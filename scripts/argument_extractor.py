#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = ["fparser"]
# ///
"""Extract source-level Fortran routine contracts and calls as JSON.

Examples:
  uv run --script scripts/argument_extractor.py source.F90
  uv run --script scripts/argument_extractor.py source.F -o source.arguments.json
  uv run --script scripts/argument_extractor.py source.F --preprocess -I include -D USE_MPI
  uv run --script scripts/argument_extractor.py source.F90 --output-format=detailed
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path
from typing import Any, Iterable

try:
    from fparser.common.readfortran import FortranFileReader, FortranStringReader
    from fparser.common.sourceinfo import FortranFormat
    from fparser.two.parser import ParserFactory
    from fparser.two.utils import FparserException, walk
except ImportError:
    print(
        "error: fparser is required; run with "
        "'uv run --script scripts/argument_extractor.py ...' or install fparser",
        file=sys.stderr,
    )
    raise SystemExit(2)


BOOLEAN_ATTRIBUTES = {
    "ALLOCATABLE": "allocatable",
    "ASYNCHRONOUS": "asynchronous",
    "CONTIGUOUS": "contiguous",
    "EXTERNAL": "external",
    "INTRINSIC": "intrinsic",
    "OPTIONAL": "optional",
    "PARAMETER": "parameter",
    "POINTER": "pointer",
    "PRIVATE": "private",
    "PROTECTED": "protected",
    "PUBLIC": "public",
    "SAVE": "save",
    "TARGET": "target",
    "VALUE": "value",
    "VOLATILE": "volatile",
}

ROUTINE_NODES = {"Subroutine_Subprogram", "Function_Subprogram"}
STANDALONE_ATTRIBUTES = {
    "Asynchronous_Stmt": "ASYNCHRONOUS",
    "External_Stmt": "EXTERNAL",
    "Intrinsic_Stmt": "INTRINSIC",
    "Optional_Stmt": "OPTIONAL",
    "Protected_Stmt": "PROTECTED",
    "Save_Stmt": "SAVE",
    "Value_Stmt": "VALUE",
    "Volatile_Stmt": "VOLATILE",
}
FIXED_FORM_ROUTINE_PATTERN = re.compile(r"\b(?:SUBROUTINE|FUNCTION)\b", re.IGNORECASE)
FIXED_FORM_IDENTIFIER_BLANKS = re.compile(r"(?<=[A-Za-z0-9_])[ \t]+(?=[A-Za-z0-9_])")
CPP_LINE_MARKER = re.compile(r'^\s*#\s*(?P<line>\d+)\s+"(?P<file>[^"]+)"')
COMPACT_BOOLEAN_ATTRIBUTES = (
    "value",
    "allocatable",
    "asynchronous",
    "contiguous",
    "pointer",
    "target",
    "volatile",
    "external",
    "intrinsic",
    "parameter",
    "protected",
    "save",
    "public",
    "private",
)


def _source_location(node: Any) -> dict[str, int] | None:
    """Return fparser's inclusive source span, when the node has one."""
    item = getattr(node, "item", None)
    span = getattr(item, "span", None)
    if not span:
        return None
    return {"start_line": span[0], "end_line": span[1]}


def _text(value: Any) -> str | None:
    if value is None:
        return None
    return str(value).strip()


def _normalise_name(name: str) -> str:
    return name.casefold()


def _routine_parent(node: Any) -> Any | None:
    """Return the closest enclosing routine, excluding the node itself."""
    parent = getattr(node, "parent", None)
    while parent is not None:
        if type(parent).__name__ in ROUTINE_NODES:
            return parent
        parent = getattr(parent, "parent", None)
    return None


def _own_nodes(subroutine: Any) -> Iterable[Any]:
    """Yield nodes that belong to this routine, not to nested routines."""
    for node in walk(subroutine):
        if node is not subroutine and _routine_parent(node) is subroutine:
            yield node


def _first_direct_statement(unit: Any, statement_name: str) -> Any | None:
    for child in getattr(unit, "children", ()):
        if type(child).__name__ == statement_name:
            return child
    return None


def _unit_name(unit: Any) -> str | None:
    statement_names = {
        "Module": "Module_Stmt",
        "Subroutine_Subprogram": "Subroutine_Stmt",
        "Function_Subprogram": "Function_Stmt",
    }
    statement_name = statement_names.get(type(unit).__name__)
    if not statement_name:
        return None

    statement = _first_direct_statement(unit, statement_name)
    if statement is None:
        return None

    items = getattr(statement, "items", ())
    if len(items) > 1 and items[1] is not None:
        return str(items[1])
    return None


def _scope_path(subroutine: Any) -> list[str]:
    scope: list[str] = []
    parent = getattr(subroutine, "parent", None)
    while parent is not None:
        if type(parent).__name__ in {"Module", *ROUTINE_NODES}:
            name = _unit_name(parent)
            if name:
                scope.append(name)
        parent = getattr(parent, "parent", None)
    return list(reversed(scope))


def _is_interface_body(subroutine: Any) -> bool:
    parent = getattr(subroutine, "parent", None)
    while parent is not None:
        if "Interface" in type(parent).__name__:
            return True
        parent = getattr(parent, "parent", None)
    return False


def _selector_value(specification: str, selector: str) -> str | None:
    match = re.search(
        rf"\b{selector}\s*=\s*([^,\)]+)",
        specification,
        flags=re.IGNORECASE,
    )
    if match:
        return match.group(1).strip()
    return None


def _type_information(type_spec: Any, entity_length: Any = None) -> dict[str, str | None]:
    specification = str(type_spec)
    uppercase = specification.upper()
    category = "unknown"
    derived_type = None
    interface = None

    intrinsic = re.match(r"^(INTEGER|REAL|COMPLEX|LOGICAL|CHARACTER)\b", uppercase)
    derived = re.match(r"^(TYPE|CLASS)\s*\(\s*(.*?)\s*\)", specification, re.IGNORECASE)
    procedure = re.match(r"^PROCEDURE\s*\(\s*(.*?)\s*\)", specification, re.IGNORECASE)

    if intrinsic:
        category = intrinsic.group(1).lower()
    elif derived:
        category = derived.group(1).lower()
        derived_type = derived.group(2)
    elif procedure:
        category = "procedure"
        interface = procedure.group(1)

    kind = _selector_value(specification, "KIND")
    length = _selector_value(specification, "LEN")
    if category == "character" and length is None:
        old_style_length = re.search(r"CHARACTER\s*\*\s*([^\s,]+)", specification, re.IGNORECASE)
        if old_style_length:
            length = old_style_length.group(1)
    if entity_length is not None:
        length = str(entity_length)

    return {
        "category": category,
        "specification": specification,
        "kind": kind,
        "length": length,
        "derived_type": derived_type,
        "interface": interface,
    }


def _new_argument(name: str, position: int) -> dict[str, Any]:
    return {
        "name": name,
        "position": position,
        "declared": False,
        "type": None,
        "shape": None,
        "codimension": None,
        "initialization": None,
        "declaration": None,
        "declarations": [],
        "attributes": {
            "intent": None,
            "optional": False,
            "value": False,
            "allocatable": False,
            "asynchronous": False,
            "contiguous": False,
            "pointer": False,
            "target": False,
            "volatile": False,
            "external": False,
            "intrinsic": False,
            "parameter": False,
            "protected": False,
            "save": False,
            "public": False,
            "private": False,
            "dimension": None,
            "codimension": None,
            "bind": None,
            "raw": [],
            "other": [],
        },
        "effects": {
            "write_local": False,
            "write_transitive": False,
            "pointer_assoc_change": False,
            "evidence": [],
        },
    }


def _append_declaration(record: dict[str, Any], node: Any) -> None:
    entry = {"text": str(node).strip(), "location": _source_location(node)}
    if entry not in record["declarations"]:
        record["declarations"].append(entry)


def _set_shape(record: dict[str, Any], shape: Any) -> None:
    text = _text(shape)
    if text:
        record["shape"] = text


def _set_codimension(record: dict[str, Any], codimension: Any) -> None:
    text = _text(codimension)
    if text:
        record["codimension"] = text
        record["attributes"]["codimension"] = text


def _add_attribute(record: dict[str, Any], attribute: str) -> None:
    """Store both a normalized attribute and its full fparser representation."""
    attribute = attribute.strip()
    if not attribute:
        return

    attributes = record["attributes"]
    if attribute not in attributes["raw"]:
        attributes["raw"].append(attribute)

    compact = re.sub(r"\s+", "", attribute).upper()
    boolean_name = BOOLEAN_ATTRIBUTES.get(compact)
    if boolean_name:
        attributes[boolean_name] = True
        return

    intent = re.fullmatch(r"INTENT\((INOUT|IN|OUT)\)", compact)
    if intent:
        attributes["intent"] = intent.group(1).lower()
        return

    dimension = re.fullmatch(r"DIMENSION\((.*)\)", attribute, flags=re.IGNORECASE)
    if dimension:
        shape = f"({dimension.group(1).strip()})"
        attributes["dimension"] = shape
        _set_shape(record, shape)
        return

    codimension = re.fullmatch(
        r"CODIMENSION\s*(?:\((.*)\)|(\[.*\]))",
        attribute,
        flags=re.IGNORECASE,
    )
    if codimension:
        shape = codimension.group(2) or f"[{codimension.group(1).strip()}]"
        _set_codimension(record, shape)
        return

    if compact.startswith("BIND("):
        attributes["bind"] = attribute
        return

    if attribute not in attributes["other"]:
        attributes["other"].append(attribute)


def _record_for_name(
    records: dict[str, dict[str, Any]],
    name: str,
) -> dict[str, Any] | None:
    return records.get(_normalise_name(name))


def _records_mentioned(
    text: str,
    records: dict[str, dict[str, Any]],
) -> Iterable[dict[str, Any]]:
    for record in records.values():
        name = record["name"]
        if re.search(rf"(?<![A-Za-z0-9_]){re.escape(name)}(?![A-Za-z0-9_])", text, re.IGNORECASE):
            yield record


def _apply_type_declarations(
    nodes: Iterable[Any],
    records: dict[str, dict[str, Any]],
) -> None:
    for node in nodes:
        if type(node).__name__ != "Type_Declaration_Stmt":
            continue

        type_spec, attribute_list, entity_list = node.items
        attribute_specs = getattr(attribute_list, "items", ()) if attribute_list else ()
        for entity in getattr(entity_list, "items", ()):
            entity_items = getattr(entity, "items", ())
            if not entity_items:
                continue

            record = _record_for_name(records, str(entity_items[0]))
            if record is None:
                continue

            array_spec = entity_items[1] if len(entity_items) > 1 else None
            entity_detail = entity_items[2] if len(entity_items) > 2 else None
            initialization = entity_items[3] if len(entity_items) > 3 else None

            record["declared"] = True
            record["declaration"] = str(node).strip()
            _append_declaration(record, node)
            _set_shape(record, array_spec)

            entity_detail_text = _text(entity_detail)
            entity_length = None
            if entity_detail_text:
                if entity_detail_text.startswith("["):
                    _set_codimension(record, entity_detail_text)
                else:
                    entity_length = entity_detail

            record["type"] = _type_information(type_spec, entity_length)
            initialization_text = _text(initialization)
            if initialization_text:
                record["initialization"] = initialization_text

            for attribute in attribute_specs:
                _add_attribute(record, str(attribute))


def _apply_procedure_declarations(
    nodes: Iterable[Any],
    records: dict[str, dict[str, Any]],
) -> None:
    for node in nodes:
        if type(node).__name__ != "Procedure_Declaration_Stmt":
            continue

        interface, attribute_list, declaration_list = node.items
        interface_name = _text(interface)
        attribute_specs = getattr(attribute_list, "items", ()) if attribute_list else ()
        specification = "PROCEDURE"
        if interface_name:
            specification = f"PROCEDURE({interface_name})"

        for declaration in getattr(declaration_list, "items", ()):
            declaration_items = getattr(declaration, "items", ())
            if not declaration_items:
                continue

            record = _record_for_name(records, str(declaration_items[0]))
            if record is None:
                continue

            record["declared"] = True
            record["declaration"] = str(node).strip()
            record["type"] = {
                "category": "procedure",
                "specification": specification,
                "kind": None,
                "length": None,
                "derived_type": None,
                "interface": interface_name,
            }
            _append_declaration(record, node)
            for attribute in attribute_specs:
                _add_attribute(record, str(attribute))

            declaration_text = str(declaration)
            if "=>" in declaration_text:
                record["initialization"] = declaration_text.split("=>", maxsplit=1)[1].strip()


def _apply_named_shape_statement(
    node: Any,
    records: dict[str, dict[str, Any]],
    attribute: str,
    declarations: Iterable[Any],
) -> None:
    for declaration in declarations:
        items = getattr(declaration, "items", ())
        if not items:
            continue
        record = _record_for_name(records, str(items[0]))
        if record is None:
            continue
        _add_attribute(record, attribute)
        _append_declaration(record, node)
        if len(items) > 1:
            _set_shape(record, items[1])


def _apply_standalone_declarations(
    nodes: Iterable[Any],
    records: dict[str, dict[str, Any]],
) -> None:
    for node in nodes:
        node_name = type(node).__name__
        text = str(node).strip()

        if node_name == "Intent_Stmt":
            intent = _text(node.items[0])
            if intent:
                for record in _records_mentioned(text, records):
                    _add_attribute(record, f"INTENT({intent})")
                    _append_declaration(record, node)
            continue

        if node_name == "Dimension_Stmt":
            for name, shape in node.items[0]:
                record = _record_for_name(records, str(name))
                if record is None:
                    continue
                _add_attribute(record, f"DIMENSION({shape})")
                _append_declaration(record, node)
            continue

        if node_name == "Pointer_Stmt":
            _apply_named_shape_statement(node, records, "POINTER", node.items[1].items)
            continue

        if node_name == "Target_Stmt":
            _apply_named_shape_statement(node, records, "TARGET", node.items[0].items)
            continue

        if node_name == "Allocatable_Stmt":
            _apply_named_shape_statement(node, records, "ALLOCATABLE", node.items[1].items)
            continue

        attribute = STANDALONE_ATTRIBUTES.get(node_name)
        if attribute:
            for record in _records_mentioned(text, records):
                _add_attribute(record, attribute)
                _append_declaration(record, node)


def _line_number(node: Any) -> int | None:
    location = _source_location(node)
    if location is None:
        return None
    return location["start_line"]


def _lhs_root_and_path(text: str | None) -> tuple[str | None, str | None]:
    if not text:
        return None, None
    compact = re.sub(r"\s+", "", text)
    match = re.match(r"^([A-Za-z][A-Za-z0-9_]*)(.*)$", compact)
    if not match:
        return None, None
    root = _normalise_name(match.group(1))
    path = match.group(2) or None
    return root, path


def _append_effect_evidence(
    record: dict[str, Any],
    kind: str,
    node: Any,
    text: str,
    path: str | None = None,
    pointer_assoc_change: bool = False,
) -> None:
    effects = record["effects"]
    effects["write_local"] = True
    if pointer_assoc_change:
        effects["pointer_assoc_change"] = True

    evidence = {
        "kind": kind,
        "line": _line_number(node),
        "text": text.strip(),
    }
    if path:
        evidence["path"] = path
    if evidence not in effects["evidence"]:
        effects["evidence"].append(evidence)


def _mark_lhs_write(
    records: dict[str, dict[str, Any]],
    node: Any,
    lhs: Any,
    kind: str,
    pointer_assoc_change: bool = False,
) -> None:
    lhs_text = _text(lhs) or str(lhs)
    root, path = _lhs_root_and_path(lhs_text)
    if root is None:
        return
    record = records.get(root)
    if record is None:
        return
    _append_effect_evidence(
        record,
        kind=kind,
        node=node,
        text=lhs_text,
        path=path,
        pointer_assoc_change=pointer_assoc_change,
    )


def _mark_text_mentions_as_writes(
    records: dict[str, dict[str, Any]],
    node: Any,
    text: str,
    kind: str,
    pointer_assoc_change: bool = False,
) -> None:
    for record in _records_mentioned(text, records):
        _append_effect_evidence(
            record,
            kind=kind,
            node=node,
            text=text,
            pointer_assoc_change=pointer_assoc_change,
        )


def _apply_write_effects(
    nodes: Iterable[Any],
    records: dict[str, dict[str, Any]],
) -> None:
    for node in nodes:
        node_name = type(node).__name__
        if node_name == "Assignment_Stmt":
            _mark_lhs_write(records, node, node.items[0], "assignment")
            continue
        if node_name == "Pointer_Assignment_Stmt":
            _mark_lhs_write(
                records,
                node,
                node.items[0],
                "pointer_assignment",
                pointer_assoc_change=True,
            )
            continue
        if node_name == "Allocate_Stmt":
            _mark_text_mentions_as_writes(
                records,
                node,
                str(node),
                "allocate",
            )
            continue
        if node_name == "Deallocate_Stmt":
            _mark_text_mentions_as_writes(
                records,
                node,
                str(node),
                "deallocate",
            )
            continue
        if node_name == "Nullify_Stmt":
            _mark_text_mentions_as_writes(
                records,
                node,
                str(node),
                "nullify",
                pointer_assoc_change=True,
            )
            continue
        if node_name == "Read_Stmt":
            text = str(node).strip()
            # Prefer READ target list (after control list) to reduce false positives.
            target_text = text.split(")", maxsplit=1)[1] if ")" in text else text
            _mark_text_mentions_as_writes(
                records,
                node,
                target_text,
                "read_target",
            )


def _routine_statement(routine: Any) -> Any:
    statement_names = {
        "Subroutine_Subprogram": "Subroutine_Stmt",
        "Function_Subprogram": "Function_Stmt",
    }
    statement_name = statement_names.get(type(routine).__name__)
    if statement_name is None:
        raise ValueError(f"unsupported routine node '{type(routine).__name__}'")

    statement = _first_direct_statement(routine, statement_name)
    if statement is None:
        raise ValueError(f"fparser returned a routine without a {statement_name} statement")
    return statement


def _routine_location(routine: Any, nodes: Iterable[Any]) -> dict[str, int] | None:
    start = _source_location(_routine_statement(routine))
    if start is None:
        return None

    end_line = start["end_line"]
    for node in nodes:
        if type(node).__name__ in {"End_Subroutine_Stmt", "End_Function_Stmt"}:
            end = _source_location(node)
            if end:
                end_line = end["end_line"]
            break
    return {"start_line": start["start_line"], "end_line": end_line}


def _extract_subroutine(routine: Any) -> dict[str, Any]:
    statement = _routine_statement(routine)
    if len(statement.items) < 3:
        raise ValueError("fparser returned a routine statement without dummy arguments")

    prefix, name, dummy_arguments = statement.items[:3]
    binding = (
        statement.items[3]
        if type(statement).__name__ == "Subroutine_Stmt" and len(statement.items) > 3
        else None
    )
    argument_items = getattr(dummy_arguments, "items", ()) if dummy_arguments else ()
    records: dict[str, dict[str, Any]] = {}

    for position, argument in enumerate(argument_items, start=1):
        argument_name = str(argument)
        records[_normalise_name(argument_name)] = _new_argument(argument_name, position)

    nodes = list(_own_nodes(routine))
    _apply_type_declarations(nodes, records)
    _apply_procedure_declarations(nodes, records)
    _apply_standalone_declarations(nodes, records)
    _apply_write_effects(nodes, records)

    scope = _scope_path(routine)
    prefix_text = _text(prefix)
    prefixes = prefix_text.split() if prefix_text else []
    qualified_name = "::".join([*scope, str(name)])
    return {
        "name": str(name),
        "qualified_name": qualified_name,
        "scope": scope,
        "prefixes": prefixes,
        "binding": _text(binding),
        "is_interface": _is_interface_body(routine),
        "location": _routine_location(routine, nodes),
        "arguments": list(records.values()),
    }


def _extract_actual_argument(actual: Any) -> dict[str, str | None]:
    keyword = None
    value = actual
    if type(actual).__name__ == "Actual_Arg_Spec":
        keyword, value = actual.items

    expression = _text(value)
    result: dict[str, str | None] = {"expression": expression}
    if keyword is not None:
        result["keyword"] = _normalise_name(str(keyword))
    if expression:
        compact = re.sub(r"\s+", "", expression)
        base_name = re.match(r"^([A-Za-z][A-Za-z0-9_]*)(.*)$", compact)
        if base_name:
            result["base_name"] = _normalise_name(base_name.group(1))
            if base_name.group(2):
                result["access_path"] = base_name.group(2)
    return result


def _extract_calls(routine: Any, routine_info: dict[str, Any]) -> list[dict[str, Any]]:
    calls = []
    for node in _own_nodes(routine):
        if type(node).__name__ != "Call_Stmt":
            continue

        procedure, actual_arguments = node.items
        argument_items = getattr(actual_arguments, "items", ()) if actual_arguments else ()
        calls.append(
            {
                "caller": routine_info["name"],
                "caller_qualified_name": routine_info["qualified_name"],
                "callee": re.sub(r"\s+", "", str(procedure)),
                "location": _source_location(node),
                "arguments": [
                    _extract_actual_argument(argument)
                    for argument in argument_items
                ],
            }
        )
    return calls


def _compact_attributes(argument: dict[str, Any]) -> dict[str, Any]:
    attributes = argument["attributes"]
    compact = {
        name: True
        for name in COMPACT_BOOLEAN_ATTRIBUTES
        if attributes[name]
    }
    if attributes["bind"] is not None:
        compact["bind"] = attributes["bind"]
    if attributes["other"]:
        compact["other"] = attributes["other"]
    return compact


def _compatibility_argument(argument: dict[str, Any]) -> dict[str, Any]:
    type_info = argument["type"]
    attributes = argument["attributes"]
    result: dict[str, Any] = {
        "name": _normalise_name(argument["name"]),
        "type": type_info["specification"] if type_info is not None else None,
        "intent": attributes["intent"].upper() if attributes["intent"] else None,
        "optional": attributes["optional"],
    }
    if argument["shape"] is not None:
        result["shape"] = argument["shape"]
    if argument["codimension"] is not None:
        result["codimension"] = argument["codimension"]
    if not argument["declared"]:
        result["declared"] = False

    compact_attributes = _compact_attributes(argument)
    if compact_attributes:
        result["attributes"] = compact_attributes
    return result


def _compatibility_effects(
    effects: dict[str, Any],
    source: Path,
    preprocessed_locations: dict[int, dict[str, Any]],
) -> dict[str, Any]:
    result = {
        "write_local": bool(effects.get("write_local")),
        "write_transitive": bool(effects.get("write_transitive")),
        "pointer_assoc_change": bool(effects.get("pointer_assoc_change")),
        "evidence": [],
    }
    default_file = str(source.resolve())
    for evidence in effects.get("evidence", []):
        if not isinstance(evidence, dict):
            continue
        line = evidence.get("line")
        mapped_file = default_file
        mapped_line = line if isinstance(line, int) else None
        if isinstance(line, int):
            mapped_location = preprocessed_locations.get(line)
            if mapped_location is not None:
                mapped_file = mapped_location["file"]
                mapped_line = mapped_location["line"]

        compact = {
            "kind": evidence.get("kind"),
            "file": mapped_file,
            "line": mapped_line,
            "text": evidence.get("text"),
        }
        if evidence.get("path"):
            compact["path"] = evidence["path"]
        result["evidence"].append(compact)
    return result


def _compatibility_location(
    location: dict[str, int] | None,
    source: Path,
    preprocessed_locations: dict[int, dict[str, Any]],
) -> tuple[str, int | None]:
    file_name = str(source.resolve())
    line_number = location["start_line"] if location else None
    if location:
        mapped_location = preprocessed_locations.get(location["start_line"])
        if mapped_location is not None:
            file_name = mapped_location["file"]
            line_number = mapped_location["line"]
    return file_name, line_number


def _compatibility_subroutine(
    subroutine: dict[str, Any],
    source: Path,
    preprocessed_locations: dict[int, dict[str, Any]],
) -> dict[str, Any]:
    location = subroutine["location"]
    file_name, line_number = _compatibility_location(
        location,
        source,
        preprocessed_locations,
    )
    end_line = location["end_line"] if location else None
    if location:
        mapped_end = preprocessed_locations.get(location["end_line"])
        if mapped_end is not None and mapped_end["file"] == file_name:
            end_line = mapped_end["line"]

    return {
        "name": _normalise_name(subroutine["name"]),
        "qualified_name": _normalise_name(subroutine["qualified_name"]),
        "file": file_name,
        "line": line_number,
        "end_line": end_line,
        "arguments": [
            {
                **_compatibility_argument(argument),
                **(
                    {
                        "effects": _compatibility_effects(
                            argument["effects"],
                            source,
                            preprocessed_locations,
                        )
                    }
                    if isinstance(argument.get("effects"), dict)
                    else {}
                ),
            }
            for argument in subroutine["arguments"]
        ],
    }


def _compatibility_call(
    call: dict[str, Any],
    source: Path,
    preprocessed_locations: dict[int, dict[str, Any]],
) -> dict[str, Any]:
    file_name, line_number = _compatibility_location(
        call["location"],
        source,
        preprocessed_locations,
    )
    arguments = []
    for argument in call["arguments"]:
        compact_argument = {"expression": argument["expression"]}
        if argument.get("keyword") is not None:
            compact_argument["keyword"] = argument["keyword"]
        if argument.get("base_name") is not None:
            compact_argument["base_name"] = argument["base_name"]
        if argument.get("access_path") is not None:
            compact_argument["access_path"] = argument["access_path"]
        arguments.append(compact_argument)

    return {
        "caller": _normalise_name(call["caller"]),
        "caller_qualified_name": _normalise_name(call["caller_qualified_name"]),
        "callee": _normalise_name(call["callee"]),
        "file": file_name,
        "line": line_number,
        "arguments": arguments,
    }


def _preprocessor_directives(tree: Any) -> list[dict[str, Any]]:
    directives = []
    for node in walk(tree):
        node_name = type(node).__name__
        if node_name.startswith("Cpp_"):
            directives.append(
                {
                    "kind": node_name,
                    "text": str(node).strip(),
                    "location": _source_location(node),
                }
            )
    return directives


def _source_form(path: Path, requested_form: str) -> str:
    if requested_form != "auto":
        return requested_form

    suffix = path.suffix.lower()
    if suffix == ".f":
        return "fixed"
    if suffix == ".f90":
        return "free"
    raise ValueError(
        f"cannot infer source form from '{path.name}'; use --source-form=fixed or --source-form=free"
    )


def _normalise_fixed_form_dummy_argument_blanks(source: str) -> str:
    """Make legacy embedded blanks in fixed-form dummy names parseable."""
    normalised_lines: list[str] = []
    in_dummy_arguments = False
    parenthesis_depth = 0

    for line in source.splitlines(keepends=True):
        if not line or line[0] in "cC*!" or line.lstrip().startswith("#"):
            normalised_lines.append(line)
            continue

        fixed_form_prefix = line[:6]
        code = line[6:]
        segment_start = 0

        if not in_dummy_arguments:
            routine_match = FIXED_FORM_ROUTINE_PATTERN.search(code)
            if routine_match is None:
                normalised_lines.append(line)
                continue

            opening_parenthesis = code.find("(", routine_match.end())
            if opening_parenthesis == -1:
                normalised_lines.append(line)
                continue

            in_dummy_arguments = True
            parenthesis_depth = 1
            segment_start = opening_parenthesis + 1

        segment_end = len(code)
        for index in range(segment_start, len(code)):
            if code[index] == "(":
                parenthesis_depth += 1
            elif code[index] == ")":
                parenthesis_depth -= 1
                if parenthesis_depth == 0:
                    segment_end = index
                    in_dummy_arguments = False
                    break

        code = (
            code[:segment_start]
            + FIXED_FORM_IDENTIFIER_BLANKS.sub("", code[segment_start:segment_end])
            + code[segment_end:]
        )
        normalised_lines.append(f"{fixed_form_prefix}{code}")

    return "".join(normalised_lines)


def _collapse_identifier_blanks(text: str) -> str:
    """Collapse legacy blanks within identifiers while preserving strings and comments."""
    normalised: list[str] = []
    quote: str | None = None
    index = 0
    while index < len(text):
        character = text[index]
        if quote is not None:
            normalised.append(character)
            if character == quote:
                if index + 1 < len(text) and text[index + 1] == quote:
                    normalised.append(text[index + 1])
                    index += 1
                else:
                    quote = None
            index += 1
            continue
        if character in {"'", '"'}:
            quote = character
            normalised.append(character)
            index += 1
            continue
        if character == "!":
            normalised.append(text[index:])
            break
        if character in {" ", "\t"}:
            end = index
            while end < len(text) and text[end] in {" ", "\t"}:
                end += 1
            previous = text[index - 1] if index else ""
            following = text[end] if end < len(text) else ""
            if (
                (previous.isalnum() or previous == "_")
                and (following.isalnum() or following == "_")
            ):
                index = end
                continue
        normalised.append(character)
        index += 1
    return "".join(normalised)


def _normalise_fixed_form_declaration_identifier_blanks(source: str) -> str:
    """Handle embedded blanks in legacy fixed-form declaration entity names."""
    normalised_lines: list[str] = []
    in_declaration = False
    previous_declaration_line: int | None = None

    for line in source.splitlines(keepends=True):
        if not line or line[0] in "cC*!" or line.lstrip().startswith("#"):
            normalised_lines.append(line)
            continue

        fixed_form_prefix = line[:6]
        code = line[6:]
        continuation = len(fixed_form_prefix) == 6 and fixed_form_prefix[5] not in {" ", "0"}
        if not continuation:
            in_declaration = False
            previous_declaration_line = None

        if "::" in code:
            prefix, declarations = code.split("::", maxsplit=1)
            code = f"{prefix}::{_collapse_identifier_blanks(declarations)}"
            in_declaration = True
        elif in_declaration and continuation:
            code = _collapse_identifier_blanks(code)
            leading_identifier = re.match(r"(\s*)([A-Za-z][A-Za-z0-9_]*)", code)
            if leading_identifier and previous_declaration_line is not None:
                previous_line = normalised_lines[previous_declaration_line]
                previous_body = previous_line.rstrip("\r\n")
                line_ending = previous_line[len(previous_body) :]
                previous_code = previous_body[6:]
                comment_start = previous_code.find("!")
                declaration_code = (
                    previous_code
                    if comment_start == -1
                    else previous_code[:comment_start]
                )
                if declaration_code.rstrip()[-1:].isalnum() or declaration_code.rstrip()[-1:] == "_":
                    identifier = leading_identifier.group(2)
                    insertion = 6 + len(declaration_code.rstrip())
                    normalised_lines[previous_declaration_line] = (
                        f"{previous_body[:insertion]}{identifier}{previous_body[insertion:]}{line_ending}"
                    )
                    code = f"{code[:leading_identifier.start(2)]}{code[leading_identifier.end(2):]}"

        normalised_lines.append(f"{fixed_form_prefix}{code}")
        if in_declaration:
            previous_declaration_line = len(normalised_lines) - 1

    return "".join(normalised_lines)


def _normalise_type_bound_bindings(source: str) -> str:
    """Comment type-bound bindings that fparser cannot parse without semantic loss."""
    normalised_lines: list[str] = []
    in_type_definition = False
    type_start = re.compile(
        r"^\s*TYPE\s*(?:,\s*[^:]*)?::\s*[A-Za-z][A-Za-z0-9_]*",
        re.IGNORECASE,
    )
    binding = re.compile(r"^\s*(?:PROCEDURE|GENERIC|FINAL)\b", re.IGNORECASE)
    type_end = re.compile(r"^\s*END\s*TYPE\b", re.IGNORECASE)

    for line in source.splitlines(keepends=True):
        if type_end.match(line):
            in_type_definition = False
            normalised_lines.append(line)
            continue
        if in_type_definition and binding.match(line):
            indentation = line[: len(line) - len(line.lstrip())]
            line_ending = "\n" if line.endswith("\n") else ""
            normalised_lines.append(f"{indentation}! fparser2 skipped type-bound binding{line_ending}")
            continue
        if type_start.match(line):
            in_type_definition = True
        normalised_lines.append(line)

    return "".join(normalised_lines)


def _preprocessed_source_locations(source: str) -> dict[int, dict[str, Any]]:
    """Map preprocessed physical lines back to their source file and line."""
    locations: dict[int, dict[str, Any]] = {}
    source_file: str | None = None
    source_line: int | None = None

    for physical_line, text in enumerate(source.splitlines(), start=1):
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


def _preprocess(
    path: Path,
    compiler: str,
    include_dirs: list[str],
    definitions: list[str],
) -> str:
    command = [compiler, "-E", "-cpp"]
    for include_dir in include_dirs:
        command.extend(["-I", include_dir])
    command.extend(f"-D{definition}" for definition in definitions)
    command.append(str(path))

    try:
        result = subprocess.run(command, check=True, text=True, capture_output=True)
    except FileNotFoundError as error:
        raise ValueError(f"preprocessor compiler '{compiler}' was not found") from error
    except subprocess.CalledProcessError as error:
        details = error.stderr.strip() or error.stdout.strip()
        raise ValueError(f"preprocessing failed for '{path}': {details}") from error
    return result.stdout


def _parse_file(
    path: Path,
    source_form: str,
    include_dirs: list[str],
    preprocess: bool,
    compiler: str,
    definitions: list[str],
) -> tuple[Any, dict[int, dict[str, Any]]]:
    preprocessed_locations: dict[int, dict[str, Any]] = {}
    if preprocess:
        source = _preprocess(path, compiler, include_dirs, definitions)
        if source_form == "fixed":
            source = _normalise_fixed_form_dummy_argument_blanks(source)
            source = _normalise_fixed_form_declaration_identifier_blanks(source)
        source = _normalise_type_bound_bindings(source)
        preprocessed_locations = _preprocessed_source_locations(source)
        reader = FortranStringReader(
            source,
            ignore_comments=False,
        )
    else:
        reader = FortranFileReader(
            str(path),
            include_dirs=include_dirs,
            ignore_comments=False,
        )

    reader.set_format(FortranFormat(source_form == "free", is_strict=False))
    parser = ParserFactory().create(std="f2008")
    return parser(reader), preprocessed_locations


def extract_file(
    source: Path,
    source_form: str = "auto",
    include_dirs: list[str] | None = None,
    preprocess: bool = False,
    compiler: str = "gfortran",
    definitions: list[str] | None = None,
    output_format: str = "compatible",
    warn_unexpanded_includes: bool = False,
) -> dict[str, Any]:
    """Extract one source file into either compatible or detailed JSON data."""
    if not source.is_file():
        raise ValueError(f"source file '{source}' does not exist")

    include_dirs = include_dirs or []
    definitions = definitions or []
    resolved_source_form = _source_form(source, source_form)
    tree, preprocessed_locations = _parse_file(
        source,
        resolved_source_form,
        include_dirs,
        preprocess,
        compiler,
        definitions,
    )
    directives = _preprocessor_directives(tree)
    if (
        warn_unexpanded_includes
        and not preprocess
        and any(item["kind"] == "Cpp_Include_Stmt" for item in directives)
    ):
        print(
            "warning: C-preprocessor includes were not expanded; "
            "use --preprocess to analyze declarations from those files",
            file=sys.stderr,
        )

    routine_nodes = [
        node
        for node in walk(tree)
        if type(node).__name__ in ROUTINE_NODES
    ]
    extracted_routines = [
        (routine, _extract_subroutine(routine))
        for routine in routine_nodes
    ]
    calls = [
        call
        for routine, routine_info in extracted_routines
        for call in _extract_calls(routine, routine_info)
    ]

    if output_format == "compatible":
        return {
            "subroutines": [
                _compatibility_subroutine(
                    subroutine,
                    source,
                    preprocessed_locations,
                )
                for _, subroutine in extracted_routines
            ],
            "calls": [
                _compatibility_call(
                    call,
                    source,
                    preprocessed_locations,
                )
                for call in calls
            ],
        }

    if output_format == "detailed":
        return {
            "schema_version": 1,
            "source": {
                "path": str(source.resolve()),
                "source_form": resolved_source_form,
                "preprocessed": preprocess,
            },
            "preprocessor_directives": directives,
            "subroutines": [
                subroutine
                for _, subroutine in extracted_routines
            ],
            "calls": calls,
        }

    raise ValueError(f"unsupported output format '{output_format}'")


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Extract Fortran subroutine dummy-argument declarations as JSON."
    )
    parser.add_argument("source", type=Path, help="Fortran .F or .F90 source file")
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="Write JSON to this file instead of standard output",
    )
    parser.add_argument(
        "--source-form",
        choices=("auto", "fixed", "free"),
        default="auto",
        help="Fortran source form; inferred from the extension by default",
    )
    parser.add_argument(
        "-I",
        "--include-dir",
        action="append",
        default=[],
        metavar="DIR",
        help="Directory used for Fortran INCLUDE files; may be repeated",
    )
    parser.add_argument(
        "-D",
        "--define",
        action="append",
        default=[],
        metavar="NAME[=VALUE]",
        help="Macro definition passed to --preprocess; may be repeated",
    )
    parser.add_argument(
        "--preprocess",
        action="store_true",
        help="Expand C-preprocessor includes and macros with the selected compiler",
    )
    parser.add_argument(
        "--compiler",
        default="gfortran",
        help="Compiler used with --preprocess (default: gfortran)",
    )
    parser.add_argument(
        "--output-format",
        choices=("compatible", "detailed"),
        default="compatible",
        help=(
            "JSON schema: compatible mirrors the GCC plugin's subroutine records "
            "without generated declaration text; detailed preserves parser metadata"
        ),
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = _argument_parser().parse_args(argv)
    source = args.source

    try:
        result = extract_file(
            source,
            source_form=args.source_form,
            include_dirs=args.include_dir,
            preprocess=args.preprocess,
            compiler=args.compiler,
            definitions=args.define,
            output_format=args.output_format,
            warn_unexpanded_includes=True,
        )
    except (FparserException, OSError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        if isinstance(error, FparserException) and not args.preprocess:
            print(
                "hint: if this source uses C-preprocessor includes or macros, "
                "retry with --preprocess and the build's -I/-D options",
                file=sys.stderr,
            )
        return 2

    output = json.dumps(result, indent=2)

    if args.output:
        try:
            args.output.write_text(f"{output}\n", encoding="utf-8")
        except OSError as error:
            print(f"error: cannot write '{args.output}': {error}", file=sys.stderr)
            return 2
    else:
        print(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
