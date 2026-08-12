#!/usr/bin/env python3
"""Find source-level paths from INTENT(IN) to mutable dummy arguments."""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter, defaultdict, deque
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, Iterable


DEFAULT_PATTERN = "*.fparser2.json"


@dataclass(frozen=True)
class ArgumentNode:
    routine_id: str
    position: int


@dataclass(frozen=True)
class FlowEdge:
    source: ArgumentNode
    target: ArgumentNode
    file: str
    line: int | None
    expression: str | None


@dataclass
class Routine:
    identifier: str
    name: str
    qualified_name: str
    file: str
    line: int | None
    end_line: int | None
    arguments: list[dict[str, Any]]


def _script_directory() -> Path:
    return Path(__file__).resolve().parent


def _repository_root() -> Path:
    return _script_directory().parent


def _default_targets() -> dict[str, dict[str, Any]]:
    script_dir = _script_directory()
    repo_root = _repository_root()
    return {
        "starter": {
            "roots": [repo_root / "starter", repo_root / "common_source"],
            "pattern": "*.starter.fparser2.json",
            "output": script_dir / "intent-flow-starter-report.json",
        },
        "engine": {
            "roots": [repo_root / "engine", repo_root / "common_source"],
            "pattern": "*.engine.fparser2.json",
            "output": script_dir / "intent-flow-engine-report.json",
        },
    }


def _normalise_name(value: Any) -> str:
    return str(value).casefold()


def _normalise_file(value: Any) -> str:
    return str(Path(str(value)).resolve())


def _sidecars(roots: Iterable[Path], pattern: str) -> list[Path]:
    paths = []
    for root in roots:
        if root.is_file():
            paths.append(root.resolve())
        else:
            paths.extend(path.resolve() for path in root.rglob(pattern) if path.is_file())
    return sorted(set(paths))


def _load_documents(paths: Iterable[Path]) -> tuple[list[tuple[Path, dict[str, Any]]], list[dict[str, str]]]:
    documents = []
    invalid_files = []
    for path in paths:
        try:
            document = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeError, json.JSONDecodeError) as error:
            invalid_files.append({"path": str(path), "error": f"{type(error).__name__}: {error}"})
            continue

        if not isinstance(document, dict):
            invalid_files.append({"path": str(path), "error": "JSON root is not an object"})
            continue
        if document.get("analysis_status") == "failed":
            error = document.get("error", {})
            invalid_files.append(
                {
                    "path": str(path),
                    "error": (
                        f"{error.get('type', 'ExtractionError')}: "
                        f"{error.get('message', 'source extraction failed')}"
                        if isinstance(error, dict)
                        else "source extraction failed"
                    ),
                }
            )
            continue
        if not isinstance(document.get("subroutines"), list) or not isinstance(document.get("calls"), list):
            invalid_files.append(
                {
                    "path": str(path),
                    "error": "missing compatible 'subroutines' or 'calls' array",
                }
            )
            continue
        documents.append((path, document))
    return documents, invalid_files


def _build_routines(
    documents: Iterable[tuple[Path, dict[str, Any]]],
) -> tuple[
    dict[str, Routine],
    dict[tuple[str, str], list[Routine]],
    dict[str, list[Routine]],
    list[dict[str, Any]],
]:
    routines: dict[str, Routine] = {}
    by_file_qualified: dict[tuple[str, str], list[Routine]] = defaultdict(list)
    by_name: dict[str, list[Routine]] = defaultdict(list)
    calls: list[dict[str, Any]] = []

    for sidecar_path, document in documents:
        for record in document["subroutines"]:
            if not isinstance(record, dict):
                continue
            name = _normalise_name(record.get("name", ""))
            qualified_name = _normalise_name(record.get("qualified_name", name))
            file_name = _normalise_file(record.get("file", sidecar_path))
            line = record.get("line")
            end_line = record.get("end_line")
            if not name or not isinstance(line, int):
                continue
            identifier = f"{file_name}:{line}:{qualified_name}"
            routine = Routine(
                identifier=identifier,
                name=name,
                qualified_name=qualified_name,
                file=file_name,
                line=line,
                end_line=end_line if isinstance(end_line, int) else None,
                arguments=[
                    argument
                    for argument in record.get("arguments", [])
                    if isinstance(argument, dict)
                ],
            )
            routines[identifier] = routine
            by_file_qualified[(file_name, qualified_name)].append(routine)
            by_name[name].append(routine)

        for call in document["calls"]:
            if isinstance(call, dict):
                calls.append(call)

    return routines, by_file_qualified, by_name, calls


def _call_line(call: dict[str, Any]) -> int | None:
    line = call.get("line")
    return line if isinstance(line, int) else None


def _resolve_caller(
    call: dict[str, Any],
    by_file_qualified: dict[tuple[str, str], list[Routine]],
    by_name: dict[str, list[Routine]],
) -> Routine | None:
    file_name = _normalise_file(call.get("file", ""))
    qualified_name = _normalise_name(call.get("caller_qualified_name", call.get("caller", "")))
    candidates = list(by_file_qualified.get((file_name, qualified_name), ()))
    caller_name = _normalise_name(call.get("caller", ""))
    if not candidates:
        candidates = [
            routine
            for routine in by_name.get(caller_name, ())
            if routine.file == file_name
        ]
    if not candidates:
        candidates = [
            routine
            for routine in by_name.get(caller_name, ())
            if routine.qualified_name == qualified_name
        ]
    if len(candidates) == 1:
        return candidates[0]

    call_line = _call_line(call)
    if call_line is not None and all(routine.file == file_name for routine in candidates):
        containing = [
            routine
            for routine in candidates
            if routine.line is not None
            and routine.line <= call_line
            and (routine.end_line is None or call_line <= routine.end_line)
        ]
        if len(containing) == 1:
            return containing[0]
        if containing:
            candidates = containing

    return candidates[0] if len(candidates) == 1 else None


def _resolve_callee(
    caller: Routine,
    call: dict[str, Any],
    by_name: dict[str, list[Routine]],
) -> tuple[Routine | None, str | None]:
    callee_name = _normalise_name(call.get("callee", ""))
    if not callee_name:
        return None, "missing_callee"
    if "%" in callee_name:
        return None, "type_bound"

    candidates = list(by_name.get(callee_name, ()))
    if not candidates:
        return None, "unresolved"
    if len(candidates) == 1:
        return candidates[0], None

    local_candidates = [candidate for candidate in candidates if candidate.file == caller.file]
    if len(local_candidates) == 1:
        return local_candidates[0], None

    module_prefix = caller.qualified_name.rpartition("::")[0]
    if module_prefix:
        module_candidates = [
            candidate
            for candidate in candidates
            if candidate.qualified_name.rpartition("::")[0] == module_prefix
        ]
        if len(module_candidates) == 1:
            return module_candidates[0], None
    return None, "ambiguous"


def _argument_positions(arguments: list[dict[str, Any]]) -> dict[str, int]:
    return {
        _normalise_name(argument.get("name", "")): position
        for position, argument in enumerate(arguments)
        if argument.get("name")
    }


def _source_base_name(actual: dict[str, Any]) -> str | None:
    base_name = actual.get("base_name")
    if base_name:
        return _normalise_name(base_name)
    expression = actual.get("expression")
    if not expression:
        return None
    match = re.match(r"^([A-Za-z][A-Za-z0-9_]*)", re.sub(r"\s+", "", str(expression)))
    if not match:
        return None
    return _normalise_name(match.group(1))


def _build_edges(
    calls: Iterable[dict[str, Any]],
    by_file_qualified: dict[tuple[str, str], list[Routine]],
    by_name: dict[str, list[Routine]],
) -> tuple[dict[ArgumentNode, list[FlowEdge]], dict[str, int], list[dict[str, Any]]]:
    edges: dict[ArgumentNode, list[FlowEdge]] = defaultdict(list)
    statistics: dict[str, int] = defaultdict(int)
    unresolved_calls: list[dict[str, Any]] = []

    for call in calls:
        statistics["calls"] += 1
        caller = _resolve_caller(call, by_file_qualified, by_name)
        if caller is None:
            statistics["caller_unresolved"] += 1
            unresolved_calls.append(
                {
                    "reason": "caller_unresolved",
                    "file": call.get("file"),
                    "line": call.get("line"),
                    "caller": call.get("caller"),
                    "callee": call.get("callee"),
                }
            )
            continue

        callee, reason = _resolve_callee(caller, call, by_name)
        if callee is None:
            statistics[reason or "callee_unresolved"] += 1
            unresolved_calls.append(
                {
                    "reason": reason or "callee_unresolved",
                    "file": call.get("file"),
                    "line": call.get("line"),
                    "caller": caller.qualified_name,
                    "callee": call.get("callee"),
                }
            )
            continue

        caller_positions = _argument_positions(caller.arguments)
        callee_positions = _argument_positions(callee.arguments)
        positional_index = 0
        for actual in call.get("arguments", []):
            if not isinstance(actual, dict):
                continue

            keyword = actual.get("keyword")
            if keyword is None:
                target_position = positional_index
                positional_index += 1
            else:
                target_position = callee_positions.get(_normalise_name(keyword))

            base_name = _source_base_name(actual)
            source_position = caller_positions.get(base_name) if base_name else None
            if (
                source_position is None
                or not isinstance(target_position, int)
                or target_position >= len(callee.arguments)
            ):
                continue

            edge = FlowEdge(
                source=ArgumentNode(caller.identifier, source_position),
                target=ArgumentNode(callee.identifier, target_position),
                file=_normalise_file(call.get("file", caller.file)),
                line=_call_line(call),
                expression=actual.get("expression"),
            )
            edges[edge.source].append(edge)
            statistics["dataflow_edges"] += 1
        statistics["calls_resolved"] += 1

    return edges, statistics, unresolved_calls


def _node_record(node: ArgumentNode, routines: dict[str, Routine]) -> dict[str, Any]:
    routine = routines[node.routine_id]
    argument = routine.arguments[node.position]
    return {
        "routine": routine.qualified_name,
        "file": routine.file,
        "line": routine.line,
        "argument": argument.get("name"),
        "intent": argument.get("intent"),
    }


def _intent(node: ArgumentNode, routines: dict[str, Routine]) -> str | None:
    value = routines[node.routine_id].arguments[node.position].get("intent")
    return _normalise_name(value) if value is not None else None


def _edge_record(
    edge: FlowEdge,
    routines: dict[str, Routine],
) -> dict[str, Any]:
    return {
        "caller": _node_record(edge.source, routines)["routine"],
        "caller_argument": _node_record(edge.source, routines)["argument"],
        "callee": _node_record(edge.target, routines)["routine"],
        "callee_argument": _node_record(edge.target, routines)["argument"],
        "file": edge.file,
        "line": edge.line,
        "expression": edge.expression,
    }


def _argument_effects(node: ArgumentNode, routines: dict[str, Routine]) -> dict[str, Any]:
    argument = routines[node.routine_id].arguments[node.position]
    effects = argument.get("effects")
    if isinstance(effects, dict):
        return effects
    return {
        "write_local": False,
        "write_transitive": False,
        "pointer_assoc_change": False,
        "evidence": [],
    }


def _argument_has_local_write(node: ArgumentNode, routines: dict[str, Routine]) -> bool:
    effects = _argument_effects(node, routines)
    return bool(effects.get("write_local") or effects.get("pointer_assoc_change"))


def _argument_is_pointer_like(node: ArgumentNode, routines: dict[str, Routine]) -> bool:
    argument = routines[node.routine_id].arguments[node.position]
    attributes = argument.get("attributes")
    if not isinstance(attributes, dict):
        return False
    return bool(attributes.get("pointer") or attributes.get("target"))


def _effect_evidence_records(
    node: ArgumentNode,
    routines: dict[str, Routine],
) -> list[dict[str, Any]]:
    evidence = []
    for item in _argument_effects(node, routines).get("evidence", []):
        if not isinstance(item, dict):
            continue
        evidence.append(
            {
                "kind": item.get("kind"),
                "file": item.get("file", routines[node.routine_id].file),
                "line": item.get("line"),
                "text": item.get("text"),
                "path": item.get("path"),
            }
        )
    return evidence


def _build_write_reachability(
    routines: dict[str, Routine],
    edges: dict[ArgumentNode, list[FlowEdge]],
) -> tuple[dict[ArgumentNode, FlowEdge], dict[ArgumentNode, ArgumentNode]]:
    reverse_edges: dict[ArgumentNode, list[FlowEdge]] = defaultdict(list)
    for outgoing_edges in edges.values():
        for edge in outgoing_edges:
            reverse_edges[edge.target].append(edge)

    starts = {
        ArgumentNode(routine.identifier, position)
        for routine in routines.values()
        for position, _argument in enumerate(routine.arguments)
        if _argument_has_local_write(ArgumentNode(routine.identifier, position), routines)
    }
    pending = deque(starts)
    next_to_write: dict[ArgumentNode, FlowEdge] = {}
    write_origin: dict[ArgumentNode, ArgumentNode] = {start: start for start in starts}

    while pending:
        target = pending.popleft()
        for edge in reverse_edges.get(target, ()):
            if edge.source in write_origin:
                continue
            write_origin[edge.source] = write_origin[target]
            next_to_write[edge.source] = edge
            pending.append(edge.source)

    return next_to_write, write_origin


def _path_to_write(
    start: ArgumentNode,
    next_to_write: dict[ArgumentNode, FlowEdge],
) -> list[FlowEdge]:
    path: list[FlowEdge] = []
    current = start
    seen: set[ArgumentNode] = set()
    while current in next_to_write and current not in seen:
        seen.add(current)
        edge = next_to_write[current]
        path.append(edge)
        current = edge.target
    return path


def _classify_sink(
    sink: ArgumentNode,
    routines: dict[str, Routine],
    next_to_write: dict[ArgumentNode, FlowEdge],
    write_origin: dict[ArgumentNode, ArgumentNode],
    unresolved_caller_names: set[str],
) -> dict[str, Any]:
    sink_routine = routines[sink.routine_id]
    local_evidence = _effect_evidence_records(sink, routines)
    if _argument_has_local_write(sink, routines):
        return {
            "label": "likely_caller_widen_to_mutable",
            "confidence": "high",
            "reason": "sink argument has direct write evidence in routine body",
            "local_write_evidence": local_evidence,
        }

    if sink in next_to_write:
        transitive_path = _path_to_write(sink, next_to_write)
        origin = write_origin.get(sink)
        return {
            "label": "likely_caller_widen_to_mutable",
            "confidence": "medium",
            "reason": "sink argument is forwarded to a descendant mutable argument with write evidence",
            "transitive_write_path": [
                _edge_record(edge, routines)
                for edge in transitive_path
            ],
            "transitive_write_sink": _node_record(origin, routines) if origin else None,
            "transitive_local_write_evidence": (
                _effect_evidence_records(origin, routines)
                if origin is not None
                else []
            ),
        }

    unresolved = _normalise_name(sink_routine.qualified_name) in unresolved_caller_names
    if _argument_is_pointer_like(sink, routines) or unresolved:
        reason = (
            "pointer/target dummy may be mutated indirectly"
            if _argument_is_pointer_like(sink, routines)
            else "routine has unresolved call targets; hidden writes are possible"
        )
        return {
            "label": "uncertain_alias_or_unresolved",
            "confidence": "low",
            "reason": reason,
            "local_write_evidence": local_evidence,
        }

    return {
        "label": "likely_callee_tighten_to_in",
        "confidence": "high",
        "reason": "no local or transitive write evidence found for sink argument",
        "local_write_evidence": local_evidence,
    }


def _find_intent_paths(
    routines: dict[str, Routine],
    edges: dict[ArgumentNode, list[FlowEdge]],
    sink_intent: str,
    max_depth: int,
    max_findings: int,
    next_to_write: dict[ArgumentNode, FlowEdge],
    write_origin: dict[ArgumentNode, ArgumentNode],
    unresolved_caller_names: set[str],
) -> tuple[list[dict[str, Any]], bool]:
    reverse_edges: dict[ArgumentNode, list[FlowEdge]] = defaultdict(list)
    for outgoing_edges in edges.values():
        for edge in outgoing_edges:
            reverse_edges[edge.target].append(edge)

    sinks = {
        ArgumentNode(routine.identifier, position)
        for routine in routines.values()
        for position, argument in enumerate(routine.arguments)
        if _normalise_name(argument.get("intent")) == sink_intent
    }
    ordered_sinks = sorted(sinks, key=lambda node: (node.routine_id, node.position))
    distance = {sink: 0 for sink in ordered_sinks}
    next_edge: dict[ArgumentNode, FlowEdge] = {}
    pending = deque(ordered_sinks)
    while pending:
        target = pending.popleft()
        if distance[target] >= max_depth:
            continue
        for edge in reverse_edges.get(target, ()):
            if edge.source in distance:
                continue
            distance[edge.source] = distance[target] + 1
            next_edge[edge.source] = edge
            pending.append(edge.source)

    starts = [
        ArgumentNode(routine.identifier, position)
        for routine in routines.values()
        for position, argument in enumerate(routine.arguments)
        if _normalise_name(argument.get("intent")) == "in"
    ]
    findings = []
    for start in sorted(starts, key=lambda node: (node.routine_id, node.position)):
        if start not in next_edge:
            continue

        path = []
        current = start
        while current in next_edge:
            edge = next_edge[current]
            path.append(edge)
            current = edge.target

        if current not in sinks:
            continue
        findings.append(
            {
                "kind": f"intent_in_to_intent_{sink_intent}",
                "source": _node_record(start, routines),
                "sink": _node_record(current, routines),
                "classification": _classify_sink(
                    current,
                    routines,
                    next_to_write,
                    write_origin,
                    unresolved_caller_names,
                ),
                "path": [
                    _edge_record(path_edge, routines)
                    for path_edge in path
                ],
            }
        )
        if len(findings) >= max_findings:
            return findings, True
    return findings, False


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Report paths from INTENT(IN) dummy arguments to INTENT(OUT) or INTENT(INOUT) dummies."
    )
    parser.add_argument(
        "--targets",
        nargs="+",
        choices=("starter", "engine"),
        default=["starter", "engine"],
        help="Targets to analyze (default: starter engine)",
    )
    parser.add_argument(
        "--roots",
        nargs="+",
        type=Path,
        help="Custom sidecar roots; when set, --targets are ignored",
    )
    parser.add_argument(
        "--pattern",
        default=DEFAULT_PATTERN,
        help=(
            f"Sidecar filename pattern (default: {DEFAULT_PATTERN}); "
            "used for custom roots or both targets when explicitly set"
        ),
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Output report path (with multiple targets, appends .starter/.engine)",
    )
    parser.add_argument(
        "--max-depth",
        type=int,
        default=12,
        help="Maximum call-path length to traverse (default: 12)",
    )
    parser.add_argument(
        "--max-findings",
        type=int,
        default=10_000,
        help="Stop after this many findings for each sink intent (default: 10000)",
    )
    parser.add_argument(
        "--max-unresolved",
        type=int,
        default=100,
        help="Retain at most this many unresolved-call samples (default: 100)",
    )
    parser.add_argument(
        "--fail-on-finding",
        action="store_true",
        help="Exit with status 1 when at least one mismatch is found",
    )
    return parser


def _analyze(
    roots: list[Path],
    pattern: str,
    max_depth: int,
    max_findings: int,
    max_unresolved: int,
) -> dict[str, Any]:
    sidecars = _sidecars(roots, pattern)
    if not sidecars:
        raise ValueError(f"no sidecars match '{pattern}'")
    documents, invalid_files = _load_documents(sidecars)
    routines, by_file_qualified, by_name, calls = _build_routines(documents)
    edges, edge_statistics, unresolved_calls = _build_edges(
        calls,
        by_file_qualified,
        by_name,
    )
    unresolved_caller_names = {
        _normalise_name(entry.get("caller", ""))
        for entry in unresolved_calls
        if isinstance(entry, dict) and entry.get("reason") != "caller_unresolved"
    }
    next_to_write, write_origin = _build_write_reachability(routines, edges)
    inout_findings, inout_truncated = _find_intent_paths(
        routines,
        edges,
        "inout",
        max_depth,
        max_findings,
        next_to_write,
        write_origin,
        unresolved_caller_names,
    )
    out_findings, out_truncated = _find_intent_paths(
        routines,
        edges,
        "out",
        max_depth,
        max_findings,
        next_to_write,
        write_origin,
        unresolved_caller_names,
    )
    findings = [*inout_findings, *out_findings]
    classification_counts = Counter(
        finding.get("classification", {}).get("label", "unclassified")
        for finding in findings
    )
    return {
        "schema_version": 2,
        "generated_at": datetime.now(UTC).isoformat(),
        "roots": [str(root) for root in roots],
        "pattern": pattern,
        "limitations": [
            "Only explicit CALL statements are analyzed; function references are excluded "
            "because they cannot be distinguished reliably from array references without "
            "semantic resolution.",
            "Ambiguous and type-bound procedure calls are reported as unresolved and are "
            "not used to create intent-flow edges.",
            "Write-effect detection is conservative; uncertain findings may still require "
            "manual review when aliasing or unresolved calls are involved.",
        ],
        "summary": {
            "sidecars": len(sidecars),
            "valid_sidecars": len(documents),
            "invalid_sidecars": len(invalid_files),
            "routines": len(routines),
            "calls": edge_statistics["calls"],
            "resolved_calls": edge_statistics["calls_resolved"],
            "dataflow_edges": edge_statistics["dataflow_edges"],
            "intent_in_to_intent_inout": len(inout_findings),
            "intent_in_to_intent_out": len(out_findings),
            "classification": dict(sorted(classification_counts.items())),
            "truncated": inout_truncated or out_truncated,
            "unresolved": {
                key: value
                for key, value in sorted(edge_statistics.items())
                if key not in {"calls", "calls_resolved", "dataflow_edges"}
            },
        },
        "findings": findings,
        "unresolved_call_samples": unresolved_calls[:max_unresolved],
        "invalid_files": invalid_files,
    }


def _write_report(path: Path, report: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"{json.dumps(report, indent=2)}\n", encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    args = _argument_parser().parse_args(argv)
    if args.max_depth < 1 or args.max_findings < 1 or args.max_unresolved < 0:
        print("error: limits must be positive (or zero for --max-unresolved)", file=sys.stderr)
        return 2

    exit_code = 0
    targets = _default_targets()

    if args.roots:
        roots = [root.resolve() for root in args.roots]
        missing = [root for root in roots if not root.exists()]
        if missing:
            print(
                "error: sidecar root does not exist: " + ", ".join(str(root) for root in missing),
                file=sys.stderr,
            )
            return 2
        output = args.output or (_script_directory() / "intent-flow-report.json")
        try:
            report = _analyze(
                roots,
                args.pattern,
                args.max_depth,
                args.max_findings,
                args.max_unresolved,
            )
            _write_report(output, report)
        except (OSError, ValueError) as error:
            print(f"error: {error}", file=sys.stderr)
            return 2
        print(json.dumps(report["summary"], indent=2))
        if report["invalid_files"]:
            return 1
        return 1 if args.fail_on_finding and report["findings"] else 0

    for target_name in args.targets:
        target = targets[target_name]
        roots = [root.resolve() for root in target["roots"]]
        missing = [root for root in roots if not root.exists()]
        if missing:
            print(
                "error: sidecar root does not exist: " + ", ".join(str(root) for root in missing),
                file=sys.stderr,
            )
            return 2

        pattern = args.pattern if args.pattern != DEFAULT_PATTERN else target["pattern"]
        if args.output is None:
            output = target["output"]
        elif len(args.targets) == 1:
            output = args.output
        else:
            output = args.output.with_name(f"{args.output.stem}.{target_name}{args.output.suffix}")
        try:
            report = _analyze(
                roots,
                pattern,
                args.max_depth,
                args.max_findings,
                args.max_unresolved,
            )
            report["target"] = target_name
            _write_report(output, report)
        except (OSError, ValueError) as error:
            print(f"error: target '{target_name}': {error}", file=sys.stderr)
            return 2

        print(json.dumps({"target": target_name, **report["summary"]}, indent=2))
        if report["invalid_files"]:
            exit_code = 1
        if args.fail_on_finding and report["findings"]:
            exit_code = 1

    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
