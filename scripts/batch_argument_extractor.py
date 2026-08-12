#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = ["fparser"]
# ///
"""Generate source-level Fortran contract sidecars for a source tree."""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import multiprocessing
import multiprocessing.connection
import os
import re
import shlex
import subprocess
import sys
import tempfile
import traceback
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from fparser.two.utils import FparserException

from argument_extractor import extract_file


SOURCE_SUFFIXES = {".f", ".f90"}
FORTRAN_COMPILER_HINTS = (
    "gfortran",
    "ifort",
    "ifx",
    "flang",
    "nvfortran",
    "pgfortran",
    "ftn",
)
ERROR_LINE_PATTERN = re.compile(r"\bline\s+(\d+)\b", flags=re.IGNORECASE)
CPP_LINE_MARKER = re.compile(r'^\s*#\s*(?P<line>\d+)\s+"(?P<file>[^"]+)"')


class ExtractionTimeoutError(TimeoutError):
    """Raised when a single source takes too long to extract."""


def _script_directory() -> Path:
    return Path(__file__).resolve().parent


def _repository_root() -> Path:
    return _script_directory().parent


def _default_targets() -> dict[str, dict[str, Any]]:
    repo_root = _repository_root()
    script_dir = _script_directory()
    return {
        "starter": {
            "roots": [repo_root / "starter", repo_root / "common_source"],
            "compile_log_candidates": [
                repo_root / "starter" / "log.txt",
            ],
            "output_suffix": ".starter.fparser2.json",
            "report": script_dir / "fparser2-starter-extraction-report.json",
        },
        "engine": {
            "roots": [repo_root / "engine", repo_root / "common_source"],
            "compile_log_candidates": [
                repo_root / "engine" / "log.txt",
                script_dir / "line.txt",
            ],
            "output_suffix": ".engine.fparser2.json",
            "report": script_dir / "fparser2-engine-extraction-report.json",
        },
    }


def _is_fortran_compiler(command: str) -> bool:
    executable = Path(command).name.casefold()
    return any(hint in executable for hint in FORTRAN_COMPILER_HINTS)


def _parse_compile_tokens(
    tokens: list[str],
    profile_source: Path,
) -> tuple[str, list[str], list[str]]:
    if not tokens:
        raise ValueError(f"compilation profile '{profile_source}' is empty")
    if not _is_fortran_compiler(tokens[0]):
        raise ValueError(
            f"compilation profile '{profile_source}' does not start with a Fortran compiler"
        )

    compiler = tokens[0]
    include_dirs: list[str] = []
    definitions: list[str] = []
    index = 1
    while index < len(tokens):
        token = tokens[index]
        if token == "-I":
            index += 1
            if index >= len(tokens):
                raise ValueError(f"missing include directory after -I in '{profile_source}'")
            include_dirs.append(tokens[index])
        elif token.startswith("-I"):
            include_dirs.append(token[2:])
        elif token == "-D":
            index += 1
            if index >= len(tokens):
                raise ValueError(f"missing macro definition after -D in '{profile_source}'")
            definitions.append(tokens[index])
        elif token.startswith("-D"):
            definitions.append(token[2:])
        index += 1

    return compiler, include_dirs, definitions


def _read_compile_profiles_from_log(
    path: Path,
) -> dict[str, tuple[str, list[str], list[str]]]:
    """Return one compile profile per source file found in a build log."""
    if not path.is_file():
        raise ValueError(f"compile log '{path}' does not exist")

    profiles: dict[str, tuple[str, list[str], list[str]]] = {}
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if " -c " not in line:
            continue
        try:
            tokens = shlex.split(line)
        except ValueError:
            continue
        if not tokens or not _is_fortran_compiler(tokens[0]):
            continue

        source_path: Path | None = None
        for index, token in enumerate(tokens[:-1]):
            if token == "-c":
                candidate = Path(tokens[index + 1])
                source_path = candidate if candidate.is_absolute() else (path.parent / candidate)
                break
        if source_path is None:
            continue

        source_key = str(source_path.resolve())
        profiles[source_key] = _parse_compile_tokens(tokens, path)

    return profiles


def _read_target_compile_profiles(
    target_name: str,
    candidates: list[Path],
) -> tuple[Path, dict[str, tuple[str, list[str], list[str]]], tuple[str, list[str], list[str]]]:
    chosen_log: Path | None = None
    profiles: dict[str, tuple[str, list[str], list[str]]] = {}
    for candidate in candidates:
        if not candidate.is_file():
            continue
        loaded = _read_compile_profiles_from_log(candidate)
        if loaded:
            chosen_log = candidate
            profiles = loaded
            break

    if chosen_log is None:
        raise ValueError(
            f"no compile log found for target '{target_name}' "
            f"(looked for: {', '.join(str(path) for path in candidates)})"
        )
    if not profiles:
        raise ValueError(
            f"compile log '{chosen_log}' contains no Fortran compile command "
            f"for target '{target_name}'"
        )

    default_profile = next(iter(profiles.values()))
    return chosen_log, profiles, default_profile


def _source_files(roots: list[Path], limit: int | None) -> list[Path]:
    files = [
        path.resolve()
        for root in roots
        for path in ([root] if root.is_file() else root.rglob("*"))
        if path.is_file() and path.suffix.casefold() in SOURCE_SUFFIXES
    ]
    files.sort()
    return files[:limit] if limit is not None else files


def _sidecar_path(source: Path, output_suffix: str) -> Path:
    return source.with_name(f"{source.name}{output_suffix}")


def _is_failed_sidecar(path: Path) -> bool:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return False
    return isinstance(document, dict) and document.get("analysis_status") == "failed"


def _write_json(path: Path, data: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        mode="w",
        encoding="utf-8",
        dir=path.parent,
        prefix=f".{path.name}.",
        suffix=".tmp",
        delete=False,
    ) as temporary:
        json.dump(data, temporary, indent=2)
        temporary.write("\n")
        temporary_path = Path(temporary.name)
    temporary_path.replace(path)


def _failed_result(
    source: Path,
    output: Path,
    error_type: str,
    error_message: str,
    traceback_text: str | None = None,
    compiler: str | None = None,
    include_dirs: list[str] | None = None,
    definitions: list[str] | None = None,
) -> dict[str, Any]:
    error_context = _extract_error_context(
        source,
        error_message,
        compiler=compiler,
        include_dirs=include_dirs,
        definitions=definitions,
    )
    result = {
        "status": "failed",
        "source": str(source),
        "output": str(output),
        "error": f"{error_type}: {error_message}",
    }
    if traceback_text is not None:
        result["traceback"] = traceback_text
    if error_context is not None:
        result["error_context"] = error_context

    try:
        _write_json(
            output,
            {
                "schema_version": 1,
                "analysis_status": "failed",
                "source": {"path": str(source)},
                "subroutines": [],
                "calls": [],
                "error": {
                    "type": error_type,
                    "message": error_message,
                    "context": error_context,
                },
            },
        )
    except OSError as write_error:
        result["sidecar_write_error"] = f"{type(write_error).__name__}: {write_error}"
    return result


def _extract_error_context(
    source: Path,
    error_message: str,
    compiler: str | None = None,
    include_dirs: list[str] | None = None,
    definitions: list[str] | None = None,
    context_before: int = 2,
) -> dict[str, Any] | None:
    line_match = ERROR_LINE_PATTERN.search(error_message)
    if line_match is None:
        return None

    reported_line = int(line_match.group(1))
    if reported_line < 1:
        return None

    preprocessed_context = _preprocessed_error_context(
        source,
        reported_line,
        context_before,
        compiler=compiler,
        include_dirs=include_dirs,
        definitions=definitions,
    )
    if preprocessed_context is not None:
        return preprocessed_context

    source_context = _source_error_context(source, reported_line, context_before)
    if source_context is None:
        return None
    return {
        "kind": "source",
        "reported_line": reported_line,
        "source_context": source_context,
    }


def _source_error_context(
    source: Path,
    reported_line: int,
    context_before: int,
) -> dict[str, Any] | None:
    try:
        lines = source.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return None
    if not lines:
        return None
    start_line = max(1, reported_line - context_before)
    end_line = min(len(lines), reported_line)
    return {
        "file": str(source.resolve()),
        "start_line": start_line,
        "end_line": end_line,
        "lines": [
            {
                "line": line_number,
                "text": lines[line_number - 1],
            }
            for line_number in range(start_line, end_line + 1)
        ],
    }


def _preprocess_source(
    source: Path,
    compiler: str,
    include_dirs: list[str],
    definitions: list[str],
) -> str | None:
    command = [compiler, "-E", "-cpp"]
    for include_dir in include_dirs:
        command.extend(["-I", include_dir])
    command.extend(f"-D{definition}" for definition in definitions)
    command.append(str(source))
    try:
        result = subprocess.run(command, check=True, text=True, capture_output=True)
    except (FileNotFoundError, OSError, subprocess.CalledProcessError):
        return None
    return result.stdout


def _preprocessed_source_locations(source_text: str) -> dict[int, dict[str, Any]]:
    locations: dict[int, dict[str, Any]] = {}
    source_file: str | None = None
    source_line: int | None = None
    for physical_line, text in enumerate(source_text.splitlines(), start=1):
        marker = CPP_LINE_MARKER.match(text)
        if marker:
            marker_file = marker.group("file")
            if marker_file.startswith("<") and marker_file.endswith(">"):
                source_file = None
                source_line = None
            else:
                source_file = str(Path(marker_file).resolve())
                source_line = int(marker.group("line"))
            continue
        if source_file is not None and source_line is not None:
            locations[physical_line] = {
                "file": source_file,
                "line": source_line,
            }
            source_line += 1
    return locations


def _preprocessed_error_context(
    source: Path,
    reported_line: int,
    context_before: int,
    compiler: str | None,
    include_dirs: list[str] | None,
    definitions: list[str] | None,
) -> dict[str, Any] | None:
    if not compiler:
        return None

    preprocessed = _preprocess_source(
        source,
        compiler,
        include_dirs or [],
        definitions or [],
    )
    if preprocessed is None:
        return None

    preprocessed_lines = preprocessed.splitlines()
    if not preprocessed_lines:
        return None
    if reported_line < 1 or reported_line > len(preprocessed_lines):
        return None

    start_line = max(1, reported_line - context_before)
    end_line = min(len(preprocessed_lines), reported_line)
    locations = _preprocessed_source_locations(preprocessed)
    source_cache: dict[str, list[str]] = {}
    mapped_lines: list[dict[str, Any]] = []
    for physical_line in range(start_line, end_line + 1):
        location = locations.get(physical_line)
        if location is None:
            mapped_lines.append(
                {
                    "preprocessed_line": physical_line,
                    "source_file": None,
                    "source_line": None,
                    "source_text": None,
                }
            )
            continue
        source_file = location["file"]
        source_line = location["line"]
        if source_file not in source_cache:
            try:
                source_cache[source_file] = Path(source_file).read_text(
                    encoding="utf-8",
                    errors="replace",
                ).splitlines()
            except OSError:
                source_cache[source_file] = []
        source_text = None
        source_lines = source_cache[source_file]
        if 1 <= source_line <= len(source_lines):
            source_text = source_lines[source_line - 1]
        mapped_lines.append(
            {
                "preprocessed_line": physical_line,
                "source_file": source_file,
                "source_line": source_line,
                "source_text": source_text,
            }
        )

    return {
        "kind": "preprocessed",
        "reported_line": reported_line,
        "preprocessed_context": {
            "file": str(source.resolve()),
            "start_line": start_line,
            "end_line": end_line,
            "lines": [
                {
                    "line": line_number,
                    "text": preprocessed_lines[line_number - 1],
                }
                for line_number in range(start_line, end_line + 1)
            ],
        },
        "mapped_source_lines": mapped_lines,
    }


def _format_failure_block(index: int, total: int, failure: dict[str, Any]) -> str:
    """Render a single failure as a human-readable, actionable block."""
    lines = [
        f"[{index}/{total}] FAILED: {failure['source']}",
        f"    sidecar : {failure['output']}",
        f"    error   : {failure['error']}",
    ]
    traceback_text = failure.get("traceback")
    if traceback_text:
        indented = "\n".join(f"        {line}" for line in traceback_text.splitlines())
        lines.append("    traceback:")
        lines.append(indented)
    sidecar_error = failure.get("sidecar_write_error")
    if sidecar_error:
        lines.append(f"    sidecar_write_error: {sidecar_error}")
    return "\n".join(lines)


def _print_failure_report(failures: list[dict[str, Any]], stream: Any) -> None:
    """Print one actionable block per failure so issues can be fixed in the code."""
    if not failures:
        return
    print("", file=stream)
    print(f"===== {len(failures)} extraction failure(s) =====", file=stream)
    for index, failure in enumerate(failures, start=1):
        print("", file=stream)
        print(_format_failure_block(index, len(failures), failure), file=stream)
    print("", file=stream)
    print("=" * 40, file=stream)


def _run_extraction_child(
    conn: multiprocessing.connection.Connection,
    source_name: str,
    include_dirs: list[str],
    compiler: str,
    definitions: list[str],
    output_format: str,
) -> None:
    """Run extract_file in its own process so a stuck parse can always be killed.

    A signal-based timeout in the same process cannot interrupt a call that is
    blocked inside a long C-level operation (for example catastrophic regex
    backtracking in fparser's tokenizer), because signal handlers only run
    when control returns to the Python bytecode loop. Isolating the actual
    extraction in a dedicated OS process lets the parent reclaim it with
    SIGTERM/SIGKILL regardless of what it is stuck doing.
    """
    try:
        document = extract_file(
            Path(source_name),
            include_dirs=include_dirs,
            preprocess=True,
            compiler=compiler,
            definitions=definitions,
            output_format=output_format,
        )
    except (FparserException, OSError, UnicodeError, ValueError) as error:
        conn.send(("failed", type(error).__name__, str(error), None))
    except Exception as error:
        conn.send(("failed", type(error).__name__, str(error), traceback.format_exc()))
    else:
        conn.send(("succeeded", document, None, None))
    finally:
        conn.close()


def _extract_one(
    source_name: str,
    output_suffix: str,
    skip_existing: bool,
    retry_failed: bool,
    compiler: str,
    include_dirs: list[str],
    definitions: list[str],
    output_format: str,
    timeout_seconds: int,
) -> dict[str, Any]:
    source = Path(source_name)
    output = _sidecar_path(source, output_suffix)
    if (
        skip_existing
        and output.is_file()
        and not (retry_failed and _is_failed_sidecar(output))
    ):
        return {"status": "skipped", "source": str(source), "output": str(output)}

    timeout_active = timeout_seconds > 0
    context = multiprocessing.get_context("fork")
    parent_conn, child_conn = context.Pipe(duplex=False)
    process = context.Process(
        target=_run_extraction_child,
        args=(child_conn, str(source), include_dirs, compiler, definitions, output_format),
        daemon=True,
    )
    process.start()
    child_conn.close()

    status = "crashed"
    document: Any = None
    error_type: str | None = None
    error_message: str | None = None
    traceback_text: str | None = None
    try:
        if parent_conn.poll(timeout_seconds if timeout_active else None):
            message = parent_conn.recv()
            status = message[0]
            if status == "succeeded":
                document = message[1]
            else:
                _, error_type, error_message, traceback_text = message
        else:
            status = "timeout"
    except EOFError:
        status = "crashed"
    finally:
        parent_conn.close()
        if process.is_alive():
            process.terminate()
            process.join(5)
        if process.is_alive():
            process.kill()
            process.join()

    if status == "timeout":
        return _failed_result(
            source,
            output,
            ExtractionTimeoutError.__name__,
            f"extraction exceeded {timeout_seconds} seconds",
            compiler=compiler,
            include_dirs=include_dirs,
            definitions=definitions,
        )
    if status == "crashed":
        exit_code = process.exitcode
        return _failed_result(
            source,
            output,
            "ExtractionProcessCrashed",
            f"extraction process terminated unexpectedly (exit code {exit_code})",
            compiler=compiler,
            include_dirs=include_dirs,
            definitions=definitions,
        )
    if status == "failed":
        return _failed_result(
            source,
            output,
            error_type,
            error_message,
            traceback_text,
            compiler=compiler,
            include_dirs=include_dirs,
            definitions=definitions,
        )

    # status == "succeeded": document is the extracted contract
    try:
        _write_json(output, document)
    except OSError as error:
        return _failed_result(
            source,
            output,
            type(error).__name__,
            str(error),
            compiler=compiler,
            include_dirs=include_dirs,
            definitions=definitions,
        )

    return {
        "status": "succeeded",
        "source": str(source),
        "output": str(output),
        "subroutines": len(document["subroutines"]),
        "calls": len(document["calls"]),
    }


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Generate starter/engine-specific .fparser2.json contracts with "
            "compile options auto-detected from build logs."
        )
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=4,
        help="Number of concurrent extractions (default: 4)",
    )
    parser.add_argument(
        "--targets",
        nargs="+",
        choices=("starter", "engine"),
        default=["starter", "engine"],
        help="Targets to process (default: starter engine)",
    )
    parser.add_argument(
        "--output-format",
        choices=("compatible", "detailed"),
        default="compatible",
        help="Argument extractor output format (default: compatible)",
    )
    parser.add_argument(
        "--skip-existing",
        action="store_true",
        help="Leave an existing sidecar unchanged",
    )
    parser.add_argument(
        "--retry-failed",
        action="store_true",
        help="With --skip-existing, retry sidecars marked analysis_status=failed",
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Process only the first N sorted source files",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=120,
        help="Maximum seconds per source; use 0 to disable (default: 120)",
    )
    parser.add_argument(
        "--no-failure-report",
        action="store_true",
        help="Do not print a per-failure report to stderr after the run",
    )
    return parser


def _run_target(args: argparse.Namespace, target_name: str, target: dict[str, Any]) -> dict[str, Any]:
    roots = [root.resolve() for root in target["roots"]]
    missing_roots = [root for root in roots if not root.exists()]
    if missing_roots:
        raise ValueError(
            "source root does not exist: " + ", ".join(str(root) for root in missing_roots)
        )

    compile_log, compile_profiles_by_source, default_profile = _read_target_compile_profiles(
        target_name,
        target["compile_log_candidates"],
    )
    default_compiler, default_include_dirs, default_definitions = default_profile

    sources = _source_files(roots, args.limit)
    if not sources:
        raise ValueError(f"no .F or .F90 files found for target '{target_name}'")

    output_suffix = target["output_suffix"]
    skipped_sources = [
        source
        for source in sources
        if (
            args.skip_existing
            and _sidecar_path(source, output_suffix).is_file()
            and not (
                args.retry_failed
                and _is_failed_sidecar(_sidecar_path(source, output_suffix))
            )
        )
    ]
    pending_sources = [
        source
        for source in sources
        if source not in skipped_sources
    ]

    results: list[dict[str, Any]] = []
    if pending_sources:
        with concurrent.futures.ProcessPoolExecutor(max_workers=args.jobs) as executor:
            futures = [
                executor.submit(
                    _extract_one,
                    str(source),
                    output_suffix,
                    args.skip_existing,
                    args.retry_failed,
                    *compile_profiles_by_source.get(
                        str(source.resolve()),
                        (default_compiler, default_include_dirs, default_definitions),
                    ),
                    args.output_format,
                    args.timeout,
                )
                for source in pending_sources
            ]
            for completed, future in enumerate(
                concurrent.futures.as_completed(futures),
                start=1,
            ):
                result = future.result()
                results.append(result)
                if (
                    len(pending_sources) <= 100
                    or completed % 100 == 0
                    or completed == len(pending_sources)
                ):
                    print(
                        f"[{target_name} {completed}/{len(pending_sources)}] "
                        f"{sum(item['status'] == 'succeeded' for item in results)} succeeded, "
                        f"{sum(item['status'] == 'failed' for item in results)} failed",
                        file=sys.stderr,
                    )

    failures = sorted(
        (result for result in results if result["status"] == "failed"),
        key=lambda result: result["source"],
    )
    report = {
        "schema_version": 1,
        "target": target_name,
        "generated_at": datetime.now(UTC).isoformat(),
        "roots": [str(root) for root in roots],
        "output_suffix": output_suffix,
        "output_format": args.output_format,
        "preprocessor": {
            "compiler": default_compiler,
            "include_dirs": default_include_dirs,
            "definitions": default_definitions,
            "compile_log": str(compile_log.resolve()),
            "source_overrides": len(compile_profiles_by_source),
            "timeout_seconds": args.timeout,
        },
        "summary": {
            "discovered": len(sources),
            "attempted": len(pending_sources),
            "succeeded": sum(result["status"] == "succeeded" for result in results),
            "skipped": len(skipped_sources)
            + sum(result["status"] == "skipped" for result in results),
            "failed": len(failures),
            "subroutines": sum(result.get("subroutines", 0) for result in results),
            "calls": sum(result.get("calls", 0) for result in results),
        },
        "failures": failures,
    }
    _write_json(target["report"], report)
    if not args.no_failure_report:
        _print_failure_report(failures, sys.stderr)
    return report


def main(argv: list[str] | None = None) -> int:
    args = _argument_parser().parse_args(argv)
    if args.jobs < 1:
        print("error: --jobs must be at least 1", file=sys.stderr)
        return 2
    if args.limit is not None and args.limit < 1:
        print("error: --limit must be at least 1", file=sys.stderr)
        return 2
    if args.timeout < 0:
        print("error: --timeout must be zero or positive", file=sys.stderr)
        return 2

    targets = _default_targets()
    reports: list[dict[str, Any]] = []
    for target_name in args.targets:
        try:
            report = _run_target(args, target_name, targets[target_name])
        except (OSError, ValueError) as error:
            print(f"error: {error}", file=sys.stderr)
            return 2
        reports.append(report)
        print(
            json.dumps(
                {
                    "target": target_name,
                    **report["summary"],
                },
                indent=2,
            )
        )

    return 1 if any(report["summary"]["failed"] for report in reports) else 0


if __name__ == "__main__":
    raise SystemExit(main())
