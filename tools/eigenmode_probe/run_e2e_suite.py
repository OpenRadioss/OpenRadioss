#!/usr/bin/env python3
"""Run public /EIG input-to-ANIM end-to-end regression cases.

Each case starts from a real OpenRadioss QA deck, runs Starter and Engine,
checks the logged spectrum against fixed reference values, then independently
decodes and cross-checks every modal ANIM file.  The output directory is a
self-contained review bundle containing generated decks, solver logs, native
outputs, JSON validation details, a Markdown report, and SHA-256 hashes.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass, field
from datetime import datetime, timezone
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
import sys
import time
from typing import Any

import numpy as np

from anim_reader import AnimationFormatError, ShellAnimation, read_shell_animation


@dataclass
class Check:
    name: str
    passed: bool
    detail: str


@dataclass
class CaseResult:
    name: str
    description: str
    expected_failure: bool
    checks: list[Check] = field(default_factory=list)
    metrics: dict[str, Any] = field(default_factory=dict)
    starter_seconds: float = 0.0
    engine_seconds: float = 0.0
    passed: bool = False

    def check(self, name: str, condition: bool, detail: str) -> bool:
        self.checks.append(Check(name, bool(condition), detail))
        return bool(condition)

    def to_json(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "description": self.description,
            "expected_failure": self.expected_failure,
            "passed": self.passed,
            "starter_seconds": self.starter_seconds,
            "engine_seconds": self.engine_seconds,
            "metrics": self.metrics,
            "checks": [check.__dict__ for check in self.checks],
        }


def parse_args() -> argparse.Namespace:
    repo = Path(__file__).resolve().parents[2]
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--cases-file",
        type=Path,
        default=Path(__file__).with_name("e2e_cases.json"),
    )
    parser.add_argument(
        "--starter",
        type=Path,
        default=repo / "starter/cbuild_starter_linux64_gf/starter_linux64_gf",
    )
    parser.add_argument(
        "--engine",
        type=Path,
        default=repo / "engine/cbuild_engine_linux64_gf_ompi/engine_linux64_gf_ompi",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=repo / f"eigenmode_e2e_results_{timestamp}",
    )
    parser.add_argument(
        "--case",
        action="append",
        default=[],
        help="run only this named case; repeat to select multiple cases",
    )
    parser.add_argument("--timeout", type=float, default=180.0)
    parser.add_argument(
        "--np",
        type=int,
        default=1,
        help="MPI ranks used by both Starter and Engine (default: 1)",
    )
    parser.add_argument(
        "--threads",
        type=int,
        default=1,
        help="OpenMP threads used by Starter and Engine (default: 1)",
    )
    parser.add_argument(
        "--mpiexec",
        type=Path,
        default=Path("/usr/lib64/openmpi/bin/mpiexec"),
        help="MPI launcher used when --np is greater than one",
    )
    parser.add_argument(
        "--backend",
        choices=("auto", "dense", "slepc"),
        default="auto",
        help="public /EIG solver backend (default: auto)",
    )
    parser.add_argument("--list", action="store_true", help="list cases and exit")
    return parser.parse_args()


def eigen_card(case: dict[str, Any], eig_id: int) -> str:
    grnd_id = int(case.get("grnd_id", 0))
    grnd_bc = int(case.get("grnd_bc", 0))
    translation = str(case.get("translation_clamp", "000"))
    rotation = str(case.get("rotation_clamp", "000"))
    nmod = int(case["nmod"])
    inorm = int(case.get("inorm", 0))
    cutfreq = float(case.get("cutfreq", 0.0))
    freqmin = float(case.get("freqmin", 0.0))
    nbloc = int(case.get("nbloc", 0))
    subspace_factor = int(case.get("subspace_factor", 2))
    maximum_iterations = int(case.get("maximum_iterations", 300))
    tolerance = float(case.get("tolerance", 1.0e-8))
    output_rigid = int(case.get("output_rigid", 1))
    additional_modes_file = int(case.get("ifile", 0))
    additional_modes_filename = str(
        case.get("additional_modes_filename", "EIG_MODES.dat")
    )
    rigid_tolerance = float(case.get("rigid_tolerance", 1.0e-12))
    upper_pivot_tolerance = float(case.get("upper_pivot_tolerance", 1.0e-12))
    positive_pivot_tolerance = float(case.get("positive_pivot_tolerance", 1.0e-12))
    return (
        f"/EIG/{eig_id}\n"
        f"End-to-end eigenmode case {case['name']}\n"
        "#  grnd_ID   grnd_bc    Trarot     Ifile    Irigid\n"
        f"{grnd_id:10d}{grnd_bc:10d}   {translation} {rotation}"
        f"{additional_modes_file:10d}"
        f"{output_rigid:10d}\n"
        "#     Nmod     Inorm             Cutfreq             Freqmin\n"
        f"{nmod:10d}{inorm:10d}{cutfreq:20.12g}{freqmin:20.12g}\n"
        "#    Nbloc      Incv     Niter      Ipri                 Tol"
        "  TolRigid   TolPivU   TolPivP\n"
        f"{nbloc:10d}{subspace_factor:10d}"
        f"{maximum_iterations:10d}{0:10d}"
        f"{tolerance:20.12g}{rigid_tolerance:10.3g}"
        f"{upper_pivot_tolerance:10.3g}{positive_pivot_tolerance:10.3g}\n"
        + (f"{additional_modes_filename}\n" if additional_modes_file else "")
    )


def normalize_ioflag(deck: str, rotation: bool) -> str:
    lines = deck.splitlines()
    for index, line in enumerate(lines):
        if line.strip().upper() != "/IOFLAG":
            continue
        for data_index in range(index + 1, len(lines)):
            stripped = lines[data_index].strip()
            if not stripped or stripped.startswith("#"):
                continue
            values = stripped.split()
            if len(values) < 6:
                raise ValueError("unable to decode /IOFLAG data line")
            fields = [int(value) for value in values[:6]]
            lines[data_index] = (
                f"{fields[0]:10d}{'':20}{fields[2]:10d}"
                f"{fields[3]:10d}{fields[4]:10d}{int(rotation):10d}"
            )
            return "\n".join(lines) + "\n"
    raise ValueError("fixture does not contain /IOFLAG")


def set_shell_formulation(deck: str, ishell: int) -> str:
    lines = deck.splitlines()
    changed = 0
    for index, line in enumerate(lines):
        if not line.strip().upper().startswith("/PROP/SHELL/"):
            continue
        header_found = False
        for data_index in range(index + 1, len(lines)):
            stripped = lines[data_index].strip()
            if stripped.startswith("/"):
                break
            if stripped.startswith("#"):
                if "ISHELL" in stripped.upper():
                    header_found = True
                continue
            if header_found and stripped:
                lines[data_index] = f"{ishell:10d}" + lines[data_index][10:]
                changed += 1
                break
    if changed == 0:
        raise ValueError("fixture does not contain /PROP/SHELL data")
    return "\n".join(lines) + "\n"


def remove_keyword_blocks(deck: str, keyword: str) -> str:
    """Remove every slash-keyword block while retaining unrelated groups."""
    prefix = keyword.upper().rstrip("/")
    lines = deck.splitlines()
    output: list[str] = []
    skipping = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("/"):
            if stripped.upper().startswith(prefix + "/") or stripped.upper() == prefix:
                skipping = True
                continue
            skipping = False
        if not skipping:
            output.append(line)
    return "\n".join(output) + "\n"


def generate_starter_deck(base: str, case: dict[str, Any]) -> str:
    marker = "/SPMD"
    if marker not in base:
        raise ValueError("fixture does not contain /SPMD insertion marker")
    if case.get("remove_bcs", False):
        base = remove_keyword_blocks(base, "/BCS")
    request_count = int(case.get("eig_requests", 1))
    cards = "".join(eigen_card(case, eig_id) for eig_id in range(1, request_count + 1))
    insert_lines = case.get("starter_insert_lines", [])
    if insert_lines:
        cards += "\n".join(str(line) for line in insert_lines) + "\n"
    deck = base.replace(marker, cards + marker, 1)
    deck = normalize_ioflag(deck, bool(case.get("rotation", False)))
    if "ishell" in case:
        deck = set_shell_formulation(deck, int(case["ishell"]))
    return deck


def node_ids_from_deck(deck: str) -> list[int]:
    """Extract user node IDs from a Starter deck's /NODE blocks."""
    node_ids: list[int] = []
    in_node_block = False
    for line in deck.splitlines():
        stripped = line.strip()
        if stripped.startswith("/"):
            in_node_block = stripped.upper() == "/NODE"
            continue
        if not in_node_block or not stripped or stripped.startswith("#"):
            continue
        try:
            node_ids.append(int(stripped.split()[0]))
        except (IndexError, ValueError) as error:
            raise ValueError(f"unable to decode /NODE line: {line!r}") from error
    return node_ids


def write_additional_modes_file(
    path: Path, node_ids: list[int], mode_count: int
) -> None:
    """Write a valid legacy text mode file for unsupported-input testing."""
    if not node_ids:
        raise ValueError("cannot write an additional modes file without nodes")
    lines = [f"{len(node_ids):8d}{mode_count:8d}"]
    for start in range(0, len(node_ids), 10):
        lines.append("".join(f"{node_id:8d}" for node_id in node_ids[start : start + 10]))
    translation_rotation_1_to_5 = "".join(f"{0.0:16.8f}" for _ in range(5))
    translation_rotation_6 = f"{0.0:16.8f}"
    for _ in range(mode_count):
        for _ in node_ids:
            lines.append(translation_rotation_1_to_5)
            lines.append(translation_rotation_6)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def generate_engine_deck(case: dict[str, Any], root_name: str) -> str:
    lines = [
        "#RADIOSS ENGINE",
        "# Generated by tools/eigenmode_probe/run_e2e_suite.py",
    ]
    if case.get("animation", False):
        lines.extend(["/ANIM/DT", "0.0 1.0", "/ANIM/VECT/DISP"])
        if case.get("rotation", False):
            lines.append("/ANIM/VECT/DROT")
        if case.get("energy", False):
            lines.extend(["/ANIM/ELEM/EINT", "/ANIM/ELEM/ENER"])
        if case.get("tensors", False):
            lines.extend(
                [
                    "/ANIM/SHELL/TENS/STRESS/MEMB",
                    "/ANIM/SHELL/TENS/STRESS/BEND",
                    "/ANIM/SHELL/TENS/STRESS/UPPER",
                    "/ANIM/SHELL/TENS/STRESS/LOWER",
                    "/ANIM/SHELL/TENS/STRAIN/MEMB",
                    "/ANIM/SHELL/TENS/STRAIN/BEND",
                    "/ANIM/SHELL/TENS/STRAIN/UPPER",
                    "/ANIM/SHELL/TENS/STRAIN/LOWER",
                ]
            )
        if case.get("solid_tensors", False):
            lines.extend(
                [
                    "/ANIM/BRICK/TENS/STRESS",
                    "/ANIM/BRICK/TENS/STRAIN",
                ]
            )
        if case.get("mass", False):
            lines.append("/ANIM/MASS")
        lines.extend(str(keyword) for keyword in case.get("animation_keywords", []))
    lines.extend(
        [
            "/DT",
            "0.9 0.0",
            "/IMPL/LINEAR",
            "/PRINT/-50/55",
            "/PROC/1/0",
            "/RFILE",
            "1000",
            f"/RUN/{root_name}/1",
            "0.02001",
            "/VERS/2019",
        ]
    )
    return "\n".join(lines) + "\n"


def solver_environment(repo: Path, threads: int) -> dict[str, str]:
    environment = os.environ.copy()
    hm_reader = repo / "extlib/hm_reader/linux64"
    mpi_candidates = [
        Path("/usr/lib64/openmpi/lib"),
        Path("/usr/lib/x86_64-linux-gnu/openmpi/lib"),
    ]
    library_paths = [str(hm_reader)]
    library_paths.extend(str(path) for path in mpi_candidates if path.is_dir())
    old_library_path = environment.get("LD_LIBRARY_PATH", "")
    if old_library_path:
        library_paths.append(old_library_path)
    environment["LD_LIBRARY_PATH"] = os.pathsep.join(library_paths)
    environment["RAD_CFG_PATH"] = str(repo / "hm_cfg_files")
    environment.setdefault("OMP_STACKSIZE", "400m")
    environment["OMP_NUM_THREADS"] = str(threads)
    environment["OPENBLAS_NUM_THREADS"] = str(threads)
    return environment


def run_command(
    command: list[str],
    cwd: Path,
    environment: dict[str, str],
    log_path: Path,
    timeout: float,
) -> tuple[int, float]:
    started = time.monotonic()
    try:
        completed = subprocess.run(
            command,
            cwd=cwd,
            env=environment,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            errors="replace",
            timeout=timeout,
            check=False,
        )
        return_code = completed.returncode
        output = completed.stdout
    except subprocess.TimeoutExpired as error:
        return_code = 124
        stdout = (
            error.stdout.decode("utf-8", errors="replace")
            if isinstance(error.stdout, bytes)
            else (error.stdout or "")
        )
        stderr = (
            error.stderr.decode("utf-8", errors="replace")
            if isinstance(error.stderr, bytes)
            else (error.stderr or "")
        )
        output = stdout + stderr
        output += f"\nTIMEOUT after {timeout:.1f} seconds\n"
    elapsed = time.monotonic() - started
    log_path.write_text(output, encoding="utf-8")
    return return_code, elapsed


MODE_LINE = re.compile(
    r"Public /EIG mode\s+(?P<mode>\d+):\s+"
    r"eigenvalue=\s*(?P<eigenvalue>[+\-0-9.EeDd]+),\s+"
    r"frequency_hz=\s*(?P<frequency>[+\-0-9.EeDd]+)"
)

RIGID_MODE_LINE = re.compile(
    r"Public /EIG exact rigid E(?P<mode>\d+):\s+frequency_hz=0"
)


def read_logged_modes(engine_log: str) -> list[dict[str, float]]:
    """Parse the public, backend-independent mode table from Engine output."""
    modes = []
    for match in MODE_LINE.finditer(engine_log):
        modes.append(
            {
                "mode": int(match.group("mode")),
                "eigenvalue": float(match.group("eigenvalue").replace("D", "E")),
                "frequency_hz": float(match.group("frequency").replace("D", "E")),
            }
        )
    return modes


def read_logged_rigid_modes(engine_log: str) -> list[dict[str, Any]]:
    return [
        {
            "mode": int(match.group("mode")),
            "kind": "rigid",
            "eigenvalue": 0.0,
            "frequency_hz": 0.0,
        }
        for match in RIGID_MODE_LINE.finditer(engine_log)
    ]


def relative_error(actual: float, expected: float) -> float:
    return abs(actual - expected) / max(abs(expected), np.finfo(float).tiny)


def validate_case(
    case: dict[str, Any],
    case_dir: Path,
    result: CaseResult,
    root_name: str,
    engine_log: str,
) -> None:
    modes = read_logged_modes(engine_log)
    rigid_modes = read_logged_rigid_modes(engine_log)
    mode_count = len(modes)
    expected_mode_count = int(
        case.get(
            "expected_positive_mode_count",
            case.get("expected_mode_count", case["nmod"]),
        )
    )
    result.metrics["mode_count"] = mode_count
    result.metrics["rigid_mode_count"] = len(rigid_modes)
    expected_rigid_count = int(case.get("expected_rigid_count", 0))
    expected_internal_rigid_count = int(
        case.get("expected_internal_rigid_count", expected_rigid_count)
    )
    result.check(
        "mode count",
        mode_count == expected_mode_count,
        f"actual={mode_count}, expected={expected_mode_count}",
    )
    mode_numbers = [int(mode["mode"]) for mode in modes]
    result.check(
        "ordered positive spectrum",
        mode_numbers == list(range(1, mode_count + 1))
        and all(
            math.isfinite(mode["eigenvalue"])
            and math.isfinite(mode["frequency_hz"])
            and mode["eigenvalue"] > 0.0
            and mode["frequency_hz"] > 0.0
            and (index == 0 or mode["eigenvalue"] >= modes[index - 1]["eigenvalue"])
            for index, mode in enumerate(modes)
        ),
        "mode numbers are contiguous; all eigenvalues are positive and ordered",
    )
    if modes:
        result.metrics["first_frequency_hz"] = modes[0]["frequency_hz"]
        result.metrics["last_frequency_hz"] = modes[-1]["frequency_hz"]
        consistency_error = max(
            abs(mode["frequency_hz"] - math.sqrt(mode["eigenvalue"]) / (2.0 * math.pi))
            / max(
                math.sqrt(mode["eigenvalue"]) / (2.0 * math.pi),
                1.0,
            )
            for mode in modes
        )
    else:
        consistency_error = float("inf")

    if "expected_rigid_count" in case:
        gate_counts = [
            int(value)
            for value in re.findall(
                r"Public /EIG component\s+\d+ rigid AND gate:.*?agreed=(\d+)",
                engine_log,
            )
        ]
        prediction_errors = [
            float(value.replace("D", "E"))
            for value in re.findall(
                r"Public /EIG component\s+\d+ physical rigid prediction:.*?"
                r"largest_backward_error=([+\-0-9.EeDd]+)",
                engine_log,
            )
        ]
        rigid_errors = [
            float(value.replace("D", "E"))
            for value in re.findall(
                r"accepted (?:component\s+\d+\s+)?exact rigid "
                r"(?:combination|mode)\s+\d+\s+with (?:projected )?stiffness "
                r"(?:backward error\s+|[+\-0-9.EeDd]+\s+and backward "
                r"error\s+)([+\-0-9.EeDd]+)",
                engine_log,
            )
        ]
        if gate_counts:
            detected_rigid_count = sum(gate_counts)
            validation_errors = prediction_errors
        else:
            detected_rigid_count = len(rigid_errors)
            validation_errors = rigid_errors
        result.metrics["rigid_maximum_backward_error"] = (
            max(validation_errors) if validation_errors else float("inf")
        )
        result.check(
            "exact rigid mode extraction",
            len(rigid_modes) == expected_rigid_count
            and [mode["mode"] for mode in rigid_modes]
            == list(range(mode_count + 1, mode_count + len(rigid_modes) + 1))
            and detected_rigid_count == expected_internal_rigid_count
            and validation_errors
            and all(
                math.isfinite(error) and error <= 1.0e-8 for error in validation_errors
            ),
            f"output={len(rigid_modes)}/{expected_rigid_count}, "
            f"detected={detected_rigid_count}/{expected_internal_rigid_count}, "
            f"component_max_residuals={validation_errors}",
        )

    tolerance_match = re.search(
        r"Public /EIG tolerances: eig=\s*([+\-0-9.EeDd]+), "
        r"rigid=\s*([+\-0-9.EeDd]+), "
        r"pivot_interval=\s*([+\-0-9.EeDd]+), "
        r"pivot_target=\s*([+\-0-9.EeDd]+)",
        engine_log,
    )
    requested_tolerances = (
        float(case.get("tolerance", 1.0e-8)),
        float(case.get("rigid_tolerance", 1.0e-12)),
        float(case.get("upper_pivot_tolerance", 1.0e-12)),
        float(case.get("positive_pivot_tolerance", 1.0e-12)),
    )
    defaults = (1.0e-8, 1.0e-12, 1.0e-12, 1.0e-12)
    expected_tolerances = tuple(
        requested if requested > 0.0 else default
        for requested, default in zip(requested_tolerances, defaults)
    )
    actual_tolerances = (
        tuple(float(value.replace("D", "E")) for value in tolerance_match.groups())
        if tolerance_match
        else ()
    )
    result.check(
        "effective /EIG tolerances",
        len(actual_tolerances) == len(expected_tolerances)
        and all(
            math.isclose(actual, expected, rel_tol=1.0e-12)
            for actual, expected in zip(actual_tolerances, expected_tolerances)
        ),
        f"actual={actual_tolerances}, expected={expected_tolerances}",
    )
    result.metrics["frequency_eigenvalue_consistency"] = consistency_error
    result.check(
        "frequency/eigenvalue consistency",
        (mode_count == 0 and expected_mode_count == 0) or consistency_error <= 2.0e-10,
        f"worst relative error={consistency_error:.3e}",
    )

    expected_frequencies = np.asarray(
        case.get("expected_frequencies_hz", []), dtype=np.float64
    )
    if expected_frequencies.size:
        actual_frequencies = np.asarray(
            [mode["frequency_hz"] for mode in modes], dtype=np.float64
        )
        reference_error = (
            float(
                np.max(
                    np.abs(actual_frequencies - expected_frequencies)
                    / np.maximum(np.abs(expected_frequencies), 1.0)
                )
            )
            if actual_frequencies.shape == expected_frequencies.shape
            else float("inf")
        )
        tolerance = float(case.get("frequency_relative_tolerance", 1.0e-7))
        result.metrics["reference_frequency_relative_error"] = reference_error
        result.check(
            "reference spectrum",
            reference_error <= tolerance,
            f"worst relative error={reference_error:.3e}, tolerance={tolerance:.3e}",
        )

    expected_frequency_bounds = np.asarray(
        case.get("expected_frequency_bounds_hz", []), dtype=np.float64
    )
    if expected_frequency_bounds.size:
        actual_frequencies = np.asarray(
            [mode["frequency_hz"] for mode in modes], dtype=np.float64
        )
        bounds_valid = (
            expected_frequency_bounds.ndim == 2
            and expected_frequency_bounds.shape[1] == 2
            and actual_frequencies.shape[0] == expected_frequency_bounds.shape[0]
            and np.all(actual_frequencies >= expected_frequency_bounds[:, 0])
            and np.all(actual_frequencies <= expected_frequency_bounds[:, 1])
        )
        result.check(
            "bounded reference spectrum",
            bool(bounds_valid),
            f"actual={actual_frequencies.tolist()}, "
            f"bounds={expected_frequency_bounds.tolist()}",
        )

    diagnostic_files = [
        path
        for suffix in (
            "K.mtx",
            "M.mtx",
            "dof_map.csv",
            "constraints.csv",
            "metadata.txt",
            "modes.csv",
        )
        for path in case_dir.glob(f"eigen_1*_{suffix}")
    ]
    result.check(
        "no diagnostic side files",
        not diagnostic_files,
        f"files={[path.name for path in diagnostic_files]}",
    )

    if int(case["nmod"]) < 0 and int(case.get("output_rigid", 1)) != 2:
        component_matches = re.findall(
            r"Public /EIG interval contains\s+(\d+)\s+positive modes and\s+"
            r"(\d+)\s+exact rigid modes; output retains\s+(\d+)\s+modes "
            r"\(Irigid=(\d+)\)",
            engine_log,
        )
        dense_matches = re.findall(
            r"Public /EIG bounded interval contains\s+(\d+)\s+eigenvalues",
            engine_log,
        )
        interval_valid = len(component_matches) == 1
        if interval_valid:
            positive, rigid, retained, output_flag = map(int, component_matches[0])
            interval_valid = (
                positive == mode_count
                and rigid == expected_internal_rigid_count
                and retained == mode_count + len(rigid_modes)
                and output_flag == int(case.get("output_rigid", 1))
            )
        elif len(dense_matches) == 1:
            interval_valid = int(dense_matches[0]) == mode_count
        result.check(
            "single bounded result accounting",
            interval_valid,
            f"component={component_matches}, dense={dense_matches}",
        )

    # Zero-mode accounting is emitted only by bounded/range extraction.  A
    # plain fixed-target request cannot recover or output zero modes, so it
    # correctly has no Irigid accounting line to validate.
    if int(case.get("output_rigid", 1)) == 2:
        output_match = re.search(
            r"Public /EIG rigid-only request retains\s+(\d+)\s+"
            r"rigid component modes",
            engine_log,
        )
        expected_output_count = len(rigid_modes)
        result.check(
            "rigid-only output controls",
            output_match is not None
            and int(output_match.group(1)) == expected_output_count,
            f"retained={output_match.group(1) if output_match else None}, "
            f"expected_count={expected_output_count}",
        )
    elif (
        case.get("cutfreq") is not None or int(case["nmod"]) < 0
    ) and "Public /EIG interval contains" in engine_log:
        output_match = re.search(
            r"Public /EIG interval contains\s+\d+\s+positive modes and\s+"
            r"\d+\s+exact rigid modes; output retains\s+(\d+)\s+modes\s+"
            r"\(Irigid=(\d+)\)",
            engine_log,
        )
        expected_output_count = mode_count + len(rigid_modes)
        expected_output_flag = int(case.get("output_rigid", 1))
        result.check(
            "rigid-mode output controls",
            output_match is not None
            and int(output_match.group(1)) == expected_output_count
            and int(output_match.group(2)) == expected_output_flag,
            f"retained={output_match.groups() if output_match else None}, "
            f"expected_count={expected_output_count}, "
            f"flag={expected_output_flag}",
        )

    _validate_animations(
        case, case_dir, result, root_name, engine_log, modes, rigid_modes
    )


def _validate_animations(
    case: dict[str, Any],
    case_dir: Path,
    result: CaseResult,
    root_name: str,
    engine_log: str,
    modes: list[dict[str, Any]],
    rigid_modes: list[dict[str, Any]],
) -> None:
    nbloc = int(case.get("nbloc", 0))
    output_modes = sorted(
        modes + rigid_modes,
        key=lambda mode: int(mode["mode"]),
    )
    output_mode_count = len(output_modes)
    selected_dense_backend = re.search(
        r"Public /EIG selected backend:\s+dense\b", engine_log
    )
    if case.get("expect_streamed_results", False) and not selected_dense_backend:
        retained = re.search(
            r"Public /EIG retained distributed mode storage:\s+"
            r"metadata_positive=(\d+), metadata_rigid=(\d+),\s+"
            r"mode_shapes=(enabled|disabled),\s+"
            r"vector_global_mib=([0-9.]+),\s+"
            r"vector_maximum_rank_mib=([0-9.]+),\s+"
            r"full_global_equivalent_mib=([0-9.]+)",
            engine_log,
        )
        result.check(
            "distributed result retention",
            retained is not None
            and int(retained.group(1)) + int(retained.group(2)) == output_mode_count,
            f"retained={retained.groups() if retained else None}, "
            f"expected_count={output_mode_count}",
        )
        transferred_batches = [
            (int(first), int(last))
            for first, last in re.findall(
                r"Public /EIG transferred retained mode batch\s+(\d+)\s+"
                r"through\s+(\d+)\s+from distributed storage",
                engine_log,
            )
        ]
        expected_transfer_batches = [
            (first, min(first + (nbloc or output_mode_count) - 1, output_mode_count))
            for first in range(1, output_mode_count + 1, nbloc or output_mode_count)
        ]
        result.check(
            "streamed result batches",
            transferred_batches == expected_transfer_batches,
            f"actual={transferred_batches}, " f"expected={expected_transfer_batches}",
        )
        result.check(
            "streamed result lifecycle completion",
            engine_log.count(
                "Public /EIG timing end: complete SLEPc eigensolver C path"
            )
            == 1,
            "expected exactly one retained-context release",
        )
    if nbloc > 0 and case.get("animation", False):
        actual_batches = [
            (int(first), int(last))
            for first, last in re.findall(
                r"Public /EIG Nbloc batch modes\s+(\d+)\s+through\s+(\d+)",
                engine_log,
            )
        ]
        expected_batches = [
            (first, min(first + nbloc - 1, output_mode_count))
            for first in range(1, output_mode_count + 1, nbloc)
        ]
        result.check(
            "Nbloc recovery batches",
            actual_batches == expected_batches,
            f"actual={actual_batches}, expected={expected_batches}",
        )

    base_animation_path = case_dir / f"{root_name}A000"
    animation_paths = sorted(case_dir.glob(f"{root_name}E[0-9][0-9][0-9]"))
    if not case.get("animation", False):
        result.check(
            "no animation requested",
            not base_animation_path.exists() and not animation_paths,
            f"base={base_animation_path.exists()}, "
            f"modal animation files={len(animation_paths)}",
        )
        return
    if not result.check(
        "no eigen base animation database",
        not base_animation_path.exists(),
        f"path={base_animation_path.name}, exists={base_animation_path.exists()}",
    ):
        return
    if not result.check(
        "animation count",
        len(animation_paths) == output_mode_count,
        f"actual={len(animation_paths)}, expected={output_mode_count}",
    ):
        return

    element_family = str(case.get("element_family", "shell"))
    if element_family not in {"shell", "solid", "oned"}:
        raise ValueError(f"unsupported element_family {element_family!r}")
    configured_vectors = case.get("expected_vector_fields")
    if configured_vectors is None:
        expected_vectors = {"Displacement"}
        if case.get("rotation", False):
            expected_vectors.add("Rotational DOF")
    else:
        expected_vectors = {str(name) for name in configured_vectors}
    if case.get("energy", False):
        expected_scalars = (
            {"Specific Energy"}
            if element_family == "oned"
            else {"Specific Energy", "Internal Energy"}
        )
    else:
        expected_scalars = set()
    if case.get("solid_tensors", False):
        expected_tensors = {"Stress", "Strain"}
    elif case.get("tensors", False):
        expected_tensors = {
            "Stress (membrane)",
            "Stress (moment/t^2)",
            "Stress (upper)",
            "Stress (lower)",
            "Strain (membrane)",
            "Strain (Curvature)",
            "Strain (upper)",
            "Strain (lower)",
        }
    else:
        expected_tensors = set()

    normalization_errors: list[float] = []
    rigid_shape_errors: list[float] = []
    reference_coordinates: list[np.ndarray] = []
    energy_sums: list[float] = []
    specific_energy_errors: list[float] = []
    parsed_animations: list[ShellAnimation] = []
    tensor_maxima: dict[str, float] = {}
    tensor_identity_errors: list[float] = []
    tensor_constitutive_errors: list[float] = []
    for mode_index, (mode, animation_path) in enumerate(
        zip(output_modes, animation_paths, strict=True), start=1
    ):
        animation = read_shell_animation(animation_path)
        parsed_animations.append(animation)
        expected_label = "Rigid mode" if mode.get("kind") == "rigid" else "Eigenmode"
        result.check(
            f"ANIM {mode_index:03d} header",
            animation.magic == 21548
            and animation.mode_label == expected_label
            and abs(animation.state_value - mode["frequency_hz"])
            / max(abs(mode["frequency_hz"]), 1.0)
            <= 2.0e-6,
            f"magic={animation.magic}, label={animation.mode_label!r}, "
            f"frequency={animation.state_value:.8g}",
        )
        result.check(
            f"ANIM {mode_index:03d} vectors",
            set(animation.vector_fields) == expected_vectors,
            f"actual={sorted(animation.vector_fields)}, expected={sorted(expected_vectors)}",
        )
        if element_family == "solid":
            scalar_fields = animation.solid_scalar_fields
            tensor_fields = animation.solid_tensor_fields
            element_masses = animation.solid_element_masses
        elif element_family == "oned":
            scalar_fields = animation.oned_scalar_fields
            tensor_fields = animation.oned_tensor_fields
            element_masses = animation.oned_element_masses
        else:
            scalar_fields = animation.scalar_fields
            tensor_fields = animation.tensor_fields
            element_masses = animation.element_masses
        result.check(
            f"ANIM {mode_index:03d} scalars",
            set(scalar_fields) == expected_scalars,
            f"actual={sorted(scalar_fields)}, expected={sorted(expected_scalars)}",
        )
        result.check(
            f"ANIM {mode_index:03d} tensors",
            set(tensor_fields) == expected_tensors,
            f"actual={sorted(tensor_fields)}, expected={sorted(expected_tensors)}",
        )
        result.check(
            f"ANIM {mode_index:03d} finite fields",
            np.all(np.isfinite(animation.coordinates))
            and all(
                np.all(np.isfinite(values))
                for values in animation.vector_fields.values()
            )
            and all(np.all(np.isfinite(values)) for values in scalar_fields.values())
            and all(np.all(np.isfinite(values)) for values in tensor_fields.values()),
            "coordinates and every requested vector/scalar/tensor value are finite",
        )
        for field_name in case.get("expected_zero_vector_fields", []):
            values = animation.vector_fields.get(str(field_name))
            result.check(
                f"ANIM {mode_index:03d} zero vector {field_name}",
                values is not None and not np.any(values),
                "field is present and identically zero",
            )
        displacement = animation.vector_fields.get("Displacement")
        rotation = animation.vector_fields.get("Rotational DOF")
        displacement_max = (
            float(np.max(np.abs(displacement)))
            if displacement is not None and displacement.size
            else 0.0
        )
        component_max = max(
            displacement_max,
            (
                float(np.max(np.abs(rotation)))
                if rotation is not None and rotation.size
                else 0.0
            ),
        )
        result.check(
            f"ANIM {mode_index:03d} nonzero mode",
            math.isfinite(component_max) and component_max > 0.0,
            f"maximum modal component={component_max:.8g}",
        )
        if mode.get("kind") == "rigid" and displacement is not None:
            values = displacement.astype(np.float64)
            coordinates = animation.coordinates.astype(np.float64)
            if displacement_max > 0.0:
                coordinates = coordinates - values / displacement_max
            parent = np.arange(len(coordinates), dtype=np.int64)

            def find(node: int) -> int:
                while parent[node] != node:
                    parent[node] = parent[parent[node]]
                    node = int(parent[node])
                return node

            def union(left: int, right: int) -> None:
                left_root = find(left)
                right_root = find(right)
                if left_root != right_root:
                    parent[right_root] = left_root

            for connectivity in (
                animation.connectivity,
                animation.solid_connectivity,
                animation.oned_connectivity,
            ):
                for element in connectivity:
                    nodes = [int(node) for node in element if node >= 0]
                    for node in nodes[1:]:
                        union(nodes[0], node)
            groups: dict[int, list[int]] = {}
            for node in range(len(coordinates)):
                groups.setdefault(find(node), []).append(node)
            fitted_values = np.zeros_like(values)
            for nodes in groups.values():
                xyz = coordinates[nodes]
                rigid_matrix = np.zeros((3 * len(nodes), 6), dtype=np.float64)
                rigid_matrix[0::3, 0] = 1.0
                rigid_matrix[1::3, 1] = 1.0
                rigid_matrix[2::3, 2] = 1.0
                rigid_matrix[0::3, 4] = xyz[:, 2]
                rigid_matrix[0::3, 5] = -xyz[:, 1]
                rigid_matrix[1::3, 3] = -xyz[:, 2]
                rigid_matrix[1::3, 5] = xyz[:, 0]
                rigid_matrix[2::3, 3] = xyz[:, 1]
                rigid_matrix[2::3, 4] = -xyz[:, 0]
                fitted, *_ = np.linalg.lstsq(
                    rigid_matrix, values[nodes].reshape(-1), rcond=None
                )
                fitted_values[nodes] = (rigid_matrix @ fitted).reshape((-1, 3))
            rigid_error = float(
                np.max(np.abs(values - fitted_values))
                / max(float(np.max(np.abs(values))), 1.0)
            )
            rigid_shape_errors.append(rigid_error)
            result.check(
                f"ANIM {mode_index:03d} exact rigid kinematics",
                rigid_error <= 2.0e-6,
                f"worst scaled rigid-fit error={rigid_error:.3e}",
            )
        if displacement_max > 0.0:
            reference_coordinates.append(
                animation.coordinates - displacement / displacement_max
            )
        else:
            reference_coordinates.append(animation.coordinates.copy())
        if int(case.get("inorm", 0)) == 1:
            normalization_errors.append(abs(component_max - 1.0))
        if expected_tensors and set(tensor_fields) == expected_tensors:
            for name, values in tensor_fields.items():
                tensor_maxima[name] = max(
                    tensor_maxima.get(name, 0.0), float(np.max(np.abs(values)))
                )
        if case.get("tensors", False) and set(tensor_fields) == expected_tensors:
            stress_membrane = tensor_fields["Stress (membrane)"]
            stress_moment = tensor_fields["Stress (moment/t^2)"]
            stress_upper = tensor_fields["Stress (upper)"]
            stress_lower = tensor_fields["Stress (lower)"]
            strain_membrane = tensor_fields["Strain (membrane)"]
            strain_curvature = tensor_fields["Strain (Curvature)"]
            strain_upper = tensor_fields["Strain (upper)"]
            strain_lower = tensor_fields["Strain (lower)"]
            tensor_scale = max(
                float(np.max(np.abs(stress_upper))),
                float(np.max(np.abs(stress_lower))),
                1.0,
            )
            strain_scale = max(
                float(np.max(np.abs(strain_upper))),
                float(np.max(np.abs(strain_lower))),
                1.0e-30,
            )
            tensor_identity_errors.extend(
                [
                    float(
                        np.max(
                            np.abs(stress_upper - stress_membrane - 6.0 * stress_moment)
                        )
                        / tensor_scale
                    ),
                    float(
                        np.max(
                            np.abs(stress_lower - stress_membrane + 6.0 * stress_moment)
                        )
                        / tensor_scale
                    ),
                    float(
                        np.max(
                            np.abs(
                                strain_upper - strain_membrane - 0.5 * strain_curvature
                            )
                        )
                        / strain_scale
                    ),
                    float(
                        np.max(
                            np.abs(
                                strain_lower - strain_membrane + 0.5 * strain_curvature
                            )
                        )
                        / strain_scale
                    ),
                ]
            )
            young = float(case.get("young_modulus", 29_000_000.0))
            poisson = float(case.get("poisson_ratio", 0.22000001))
            c11 = young / (1.0 - poisson * poisson)
            c12 = poisson * c11
            cxy = young / (1.0 + poisson)

            def plane_stress(strain: np.ndarray) -> np.ndarray:
                stress = np.empty_like(strain, dtype=np.float64)
                stress[:, 0] = c11 * strain[:, 0] + c12 * strain[:, 1]
                stress[:, 1] = c12 * strain[:, 0] + c11 * strain[:, 1]
                stress[:, 2] = cxy * strain[:, 2]
                return stress

            for stress, strain, factor in (
                (stress_membrane, strain_membrane, 1.0),
                (stress_moment, strain_curvature, 1.0 / 12.0),
                (stress_upper, strain_upper, 1.0),
                (stress_lower, strain_lower, 1.0),
            ):
                predicted = factor * plane_stress(strain)
                scale = max(float(np.max(np.abs(stress))), 1.0)
                tensor_constitutive_errors.append(
                    float(np.max(np.abs(stress - predicted)) / scale)
                )
        if case.get("energy", False):
            specific = np.asarray(scalar_fields["Specific Energy"], dtype=np.float64)
            if "Internal Energy" in scalar_fields:
                internal = np.asarray(
                    scalar_fields["Internal Energy"], dtype=np.float64
                )
            elif element_masses is not None:
                internal = specific * np.asarray(element_masses, dtype=np.float64)
            else:
                internal = np.full_like(specific, np.nan)
            if element_masses is not None:
                reconstructed_internal = specific * element_masses
                scale = np.maximum(np.abs(internal), np.finfo(np.float32).tiny)
                specific_energy_errors.append(
                    float(np.max(np.abs(reconstructed_internal - internal) / scale))
                )
            energy_sum = float(np.sum(internal))
            energy_sums.append(energy_sum)

    first_animation = parsed_animations[0]
    if element_family == "solid":
        first_element_count = first_animation.solid_element_count
        first_part_count = first_animation.solid_part_count
        first_connectivity = first_animation.solid_connectivity
        first_element_numbers = first_animation.solid_element_numbers
    elif element_family == "oned":
        first_element_count = first_animation.oned_element_count
        first_part_count = first_animation.oned_part_count
        first_connectivity = first_animation.oned_connectivity
        first_element_numbers = first_animation.oned_element_numbers
    else:
        first_element_count = first_animation.element_count
        first_part_count = first_animation.part_count
        first_connectivity = first_animation.connectivity
        first_element_numbers = first_animation.element_numbers
    topology_consistent = all(
        animation.node_count == first_animation.node_count
        and (
            animation.solid_element_count
            if element_family == "solid"
            else (
                animation.oned_element_count
                if element_family == "oned"
                else animation.element_count
            )
        )
        == first_element_count
        and (
            animation.solid_part_count
            if element_family == "solid"
            else (
                animation.oned_part_count
                if element_family == "oned"
                else animation.part_count
            )
        )
        == first_part_count
        and np.array_equal(
            (
                animation.solid_connectivity
                if element_family == "solid"
                else (
                    animation.oned_connectivity
                    if element_family == "oned"
                    else animation.connectivity
                )
            ),
            first_connectivity,
        )
        and np.array_equal(animation.node_numbers, first_animation.node_numbers)
        and np.array_equal(
            (
                animation.solid_element_numbers
                if element_family == "solid"
                else (
                    animation.oned_element_numbers
                    if element_family == "oned"
                    else animation.element_numbers
                )
            ),
            first_element_numbers,
        )
        for animation in parsed_animations
    )
    numbering_valid = (
        first_animation.node_numbers is not None
        and first_element_numbers is not None
        and np.unique(first_animation.node_numbers).size == first_animation.node_count
        and np.unique(first_element_numbers).size == first_element_count
    )
    result.metrics["anim_nodes"] = first_animation.node_count
    result.metrics[f"anim_{element_family}_elements"] = first_element_count
    result.check(
        "ANIM topology and numbering",
        topology_consistent and numbering_valid,
        f"nodes={first_animation.node_count}, "
        f"{'1-D elements' if element_family == 'oned' else element_family + 's'}="
        f"{first_element_count}, "
        f"parts={first_part_count}, identical across modes={topology_consistent}",
    )
    coordinate_scale = max(float(np.max(np.abs(reference_coordinates[0]))), 1.0)
    reference_coordinate_error = (
        max(
            float(np.max(np.abs(coordinates - reference_coordinates[0])))
            / coordinate_scale
            for coordinates in reference_coordinates[1:]
        )
        if len(reference_coordinates) > 1
        else 0.0
    )
    result.metrics["anim_reference_coordinate_error"] = reference_coordinate_error
    result.check(
        "ANIM undeformed reference geometry",
        reference_coordinate_error <= 2.0e-6,
        f"worst scaled difference across modes={reference_coordinate_error:.3e}",
    )
    masses_present = all(
        (
            animation.solid_element_masses
            if element_family == "solid"
            else (
                animation.oned_element_masses
                if element_family == "oned"
                else animation.element_masses
            )
        )
        is not None
        and animation.node_masses is not None
        and np.all(
            np.isfinite(
                animation.solid_element_masses
                if element_family == "solid"
                else (
                    animation.oned_element_masses
                    if element_family == "oned"
                    else animation.element_masses
                )
            )
        )
        and np.all(np.isfinite(animation.node_masses))
        and np.all(
            (
                animation.solid_element_masses
                if element_family == "solid"
                else (
                    animation.oned_element_masses
                    if element_family == "oned"
                    else animation.element_masses
                )
            )
            > 0.0
        )
        and (
            np.all(animation.node_masses >= 0.0)
            if element_family == "oned"
            else np.all(animation.node_masses > 0.0)
        )
        for animation in parsed_animations
    )
    masses_absent = all(
        (
            animation.solid_element_masses
            if element_family == "solid"
            else (
                animation.oned_element_masses
                if element_family == "oned"
                else animation.element_masses
            )
        )
        is None
        and animation.node_masses is None
        for animation in parsed_animations
    )
    result.check(
        "ANIM mass payload",
        masses_present if case.get("mass", False) else masses_absent,
        (
            "finite positive element/node masses present"
            if case.get("mass", False)
            else "mass arrays absent as requested"
        ),
    )
    if case.get("mass", False) and masses_present:
        first_element_masses = (
            first_animation.solid_element_masses
            if element_family == "solid"
            else (
                first_animation.oned_element_masses
                if element_family == "oned"
                else first_animation.element_masses
            )
        )
        mass_scale = max(float(np.max(np.abs(first_element_masses))), 1.0e-30)
        element_mass_error = max(
            float(
                np.max(
                    np.abs(
                        (
                            animation.solid_element_masses
                            if element_family == "solid"
                            else (
                                animation.oned_element_masses
                                if element_family == "oned"
                                else animation.element_masses
                            )
                        )
                        - first_element_masses
                    )
                )
            )
            / mass_scale
            for animation in parsed_animations
        )
        node_mass_scale = max(
            float(np.max(np.abs(first_animation.node_masses))), 1.0e-30
        )
        node_mass_error = max(
            float(np.max(np.abs(animation.node_masses - first_animation.node_masses)))
            / node_mass_scale
            for animation in parsed_animations
        )
        result.metrics["anim_element_mass_invariance_error"] = element_mass_error
        result.metrics["anim_node_mass_invariance_error"] = node_mass_error
        result.check(
            "modal mass invariance",
            element_mass_error <= 2.0e-6 and node_mass_error <= 2.0e-6,
            f"element error={element_mass_error:.3e}, "
            f"node error={node_mass_error:.3e}",
        )
    if case.get("expected_node_mass_min") and parsed_animations:
        first = parsed_animations[0]
        if first.node_numbers is None or first.node_masses is None:
            result.check("lumped nodal mass", False, "ANIM node masses absent")
        else:
            mass_by_node = {
                int(node): float(first.node_masses[index])
                for index, node in enumerate(first.node_numbers)
            }
            for node_text, minimum in case["expected_node_mass_min"].items():
                node = int(node_text)
                actual = mass_by_node.get(node, float("nan"))
                result.check(
                    f"lumped mass at node {node}",
                    math.isfinite(actual) and actual >= float(minimum),
                    f"actual={actual:.8g}, minimum={float(minimum):.8g}",
                )
    for relation in case.get("rbe2_relations", []):
        master = int(relation["master"])
        slave = int(relation["slave"])
        arm = np.asarray(relation["arm"], dtype=np.float64)
        errors: list[float] = []
        for animation in parsed_animations:
            if animation.node_numbers is None:
                continue
            node_position = {
                int(node): index for index, node in enumerate(animation.node_numbers)
            }
            displacement = animation.vector_fields.get("Displacement")
            rotation = animation.vector_fields.get("Rotational DOF")
            if displacement is None or rotation is None:
                continue
            master_position = node_position[master]
            slave_position = node_position[slave]
            expected_displacement = displacement[master_position] + np.cross(
                rotation[master_position], arm
            )
            difference = np.concatenate(
                (
                    displacement[slave_position] - expected_displacement,
                    rotation[slave_position] - rotation[master_position],
                )
            )
            scale = max(
                float(np.max(np.abs(displacement))),
                float(np.max(np.abs(rotation))),
                np.finfo(np.float32).tiny,
            )
            errors.append(float(np.max(np.abs(difference))) / scale)
        worst_relation = max(errors, default=float("inf"))
        result.metrics[f"rbe2_{master}_{slave}_kinematic_error"] = worst_relation
        result.check(
            f"RBE2 {master}->{slave} modal kinematics",
            len(errors) == output_mode_count and worst_relation <= 2.0e-6,
            f"modes={len(errors)}/{output_mode_count}, worst scaled error="
            f"{worst_relation:.3e}",
        )
    if case.get("tensors", False):
        missing_nonzero = [
            name for name, maximum in tensor_maxima.items() if maximum <= 0.0
        ]
        worst_tensor_identity = max(tensor_identity_errors, default=float("inf"))
        worst_tensor_constitutive = max(
            tensor_constitutive_errors, default=float("inf")
        )
        result.metrics["anim_tensor_max_abs"] = tensor_maxima
        result.metrics["anim_worst_tensor_identity_error"] = worst_tensor_identity
        result.metrics["anim_worst_tensor_constitutive_error"] = (
            worst_tensor_constitutive
        )
        result.check(
            "modal shell tensor recovery",
            len(tensor_maxima) == 8
            and not missing_nonzero
            and worst_tensor_identity <= 2.0e-6
            and worst_tensor_constitutive <= 2.0e-6,
            f"fields={len(tensor_maxima)}/8, zero fields={missing_nonzero}, "
            f"face identity error={worst_tensor_identity:.3e}, "
            f"plane-stress error={worst_tensor_constitutive:.3e}",
        )
    if case.get("solid_tensors", False):
        missing_nonzero = [
            name for name, maximum in tensor_maxima.items() if maximum <= 0.0
        ]
        result.metrics["anim_solid_tensor_max_abs"] = tensor_maxima
        result.check(
            "modal solid tensor recovery",
            len(tensor_maxima) == 2 and not missing_nonzero,
            f"fields={len(tensor_maxima)}/2, zero fields={missing_nonzero}",
        )

    if int(case.get("inorm", 0)) == 1:
        worst_normalization = max(normalization_errors, default=float("inf"))
        result.metrics["anim_worst_max_normalization_error"] = worst_normalization
        result.check(
            "maximum-component normalization",
            len(normalization_errors) == output_mode_count
            and worst_normalization <= 2.0e-6,
            f"modes={len(normalization_errors)}/{output_mode_count}, "
            f"worst absolute error={worst_normalization:.3e}",
        )
    if rigid_modes:
        worst_rigid_shape = max(rigid_shape_errors, default=float("inf"))
        result.metrics["anim_worst_rigid_shape_error"] = worst_rigid_shape
        result.check(
            "exact rigid animation kinematics",
            len(rigid_shape_errors) == len(rigid_modes) and worst_rigid_shape <= 2.0e-6,
            f"modes={len(rigid_shape_errors)}/{len(rigid_modes)}, "
            f"worst scaled error={worst_rigid_shape:.3e}",
        )
    if case.get("energy", False):
        worst_specific = max(specific_energy_errors, default=0.0)
        valid_energy = len(energy_sums) == output_mode_count and all(
            math.isfinite(value) and value > 0.0 for value in energy_sums
        )
        result.metrics["anim_min_internal_energy"] = min(
            energy_sums, default=float("nan")
        )
        result.metrics["anim_max_internal_energy"] = max(
            energy_sums, default=float("nan")
        )
        if case.get("mass", False):
            result.metrics["anim_worst_specific_energy_error"] = worst_specific
        result.check(
            "modal response energy payload",
            valid_energy,
            f"positive finite values={sum(math.isfinite(value) and value > 0.0 for value in energy_sums)}/{output_mode_count}; "
            f"range=[{min(energy_sums, default=float('nan')):.6e}, "
            f"{max(energy_sums, default=float('nan')):.6e}]",
        )
        if case.get("mass", False):
            result.check(
                "ANIM ENER mass conversion",
                worst_specific <= 2.0e-6,
                f"worst relative error={worst_specific:.3e}",
            )
        expected_energies = np.asarray(
            case.get("expected_internal_energies", []), dtype=np.float64
        )
        if expected_energies.size:
            actual_energies = np.asarray(energy_sums, dtype=np.float64)
            if case.get("energy_reference_order_independent", False):
                actual_energies = np.sort(actual_energies)
                expected_energies = np.sort(expected_energies)
            reference_error = (
                float(
                    np.max(
                        np.abs(actual_energies - expected_energies)
                        / np.maximum(np.abs(expected_energies), 1.0)
                    )
                )
                if actual_energies.shape == expected_energies.shape
                else float("inf")
            )
            tolerance = float(
                case.get("energy_reference_relative_tolerance", 5.0e-5)
            )
            result.metrics["anim_internal_energy_reference_error"] = reference_error
            result.check(
                "modal internal energy reference",
                reference_error <= tolerance,
                f"worst relative error={reference_error:.3e}, "
                f"tolerance={tolerance:.3e}",
            )


def write_case_validation(case_dir: Path, result: CaseResult) -> None:
    (case_dir / "validation.json").write_text(
        json.dumps(result.to_json(), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def public_eig_records(log: str) -> list[str]:
    """Return normalized public /EIG records in their emitted order."""
    records: list[str] = []
    mpi_prefix = re.compile(r"^\[\d+,\d+\]<stdout>:")
    for line in log.splitlines():
        line = mpi_prefix.sub("", line).strip()
        if "Public /EIG" in line:
            records.append(line)
    return records


def run_case(
    case: dict[str, Any],
    repo: Path,
    suite_dir: Path,
    fixture: Path,
    support_files: list[Path],
    starter: Path,
    engine: Path,
    environment: dict[str, str],
    timeout: float,
    root_name: str,
    mpi_ranks: int,
    mpiexec: Path,
) -> CaseResult:
    case_dir = suite_dir / case["name"]
    case_dir.mkdir()
    case_environment = environment.copy()
    mpi_tmp = case_dir / ".mpi_tmp"
    mpi_tmp.mkdir()
    case_environment["TMPDIR"] = str(mpi_tmp)
    result = CaseResult(
        name=case["name"],
        description=case.get("description", case["name"]),
        expected_failure=bool(
            case.get("expect_failure", False)
            or case.get("expect_starter_failure", False)
        ),
    )
    base = fixture.read_text(encoding="utf-8")
    starter_name = f"{root_name}_0000.rad"
    engine_name = f"{root_name}_0001.rad"
    (case_dir / starter_name).write_text(
        generate_starter_deck(base, case), encoding="utf-8"
    )
    (case_dir / engine_name).write_text(
        generate_engine_deck(case, root_name), encoding="utf-8"
    )
    for support_file in support_files:
        shutil.copy2(support_file, case_dir / support_file.name)
    if case.get("generate_additional_modes", False):
        mode_filename = Path(str(case["additional_modes_filename"]))
        if mode_filename.name != str(mode_filename):
            raise ValueError("additional_modes_filename must be a plain filename")
        node_ids: list[int] = []
        seen_node_ids: set[int] = set()
        for deck in [base, *(path.read_text(encoding="utf-8") for path in support_files)]:
            for node_id in node_ids_from_deck(deck):
                if node_id not in seen_node_ids:
                    seen_node_ids.add(node_id)
                    node_ids.append(node_id)
        write_additional_modes_file(
            case_dir / mode_filename,
            node_ids,
            max(3, int(case["nmod"])),
        )
    (case_dir / "case.json").write_text(
        json.dumps(case, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )

    starter_code, result.starter_seconds = run_command(
        [str(starter), "-i", starter_name, "-np", str(mpi_ranks)],
        case_dir,
        case_environment,
        case_dir / "starter.stdout.log",
        timeout,
    )
    if case.get("expect_starter_failure", False):
        starter_log = (case_dir / "starter.stdout.log").read_text(
            encoding="utf-8", errors="replace"
        )
        expected_error = str(case["expected_error"])
        result.check(
            "Starter rejected case",
            starter_code != 0,
            f"return code={starter_code}",
        )
        result.check(
            "Expected diagnostic",
            " ".join(expected_error.lower().split())
            in " ".join(starter_log.lower().split()),
            f"expected substring={expected_error!r}",
        )
        result.check(
            "No false normal termination",
            "NORMAL TERMINATION" not in starter_log,
            "normal termination marker is absent",
        )
        result.passed = all(check.passed for check in result.checks)
        write_case_validation(case_dir, result)
        return result
    if not result.check(
        "Starter return code", starter_code == 0, f"return code={starter_code}"
    ):
        result.passed = False
        write_case_validation(case_dir, result)
        return result
    restart_files = sorted(case_dir.glob(f"{root_name}_0000_*.rst"))
    if not result.check(
        "Starter restart output",
        bool(restart_files),
        f"files={[path.name for path in restart_files]}",
    ):
        result.passed = False
        write_case_validation(case_dir, result)
        return result

    engine_command = [str(engine), "-i", engine_name]
    if mpi_ranks > 1:
        engine_command = [
            str(mpiexec),
            "--tag-output",
            "-np",
            str(mpi_ranks),
            *engine_command,
        ]
    engine_code, result.engine_seconds = run_command(
        engine_command,
        case_dir,
        case_environment,
        case_dir / "engine.stdout.log",
        timeout,
    )
    engine_log = (case_dir / "engine.stdout.log").read_text(
        encoding="utf-8", errors="replace"
    )
    if result.expected_failure:
        expected_error = str(case["expected_error"])
        result.check(
            "Engine rejected case",
            engine_code != 0,
            f"return code={engine_code}",
        )
        result.check(
            "Expected diagnostic",
            " ".join(expected_error.lower().split())
            in " ".join(engine_log.lower().split()),
            f"expected substring={expected_error!r}",
        )
        result.check(
            "No false normal termination",
            "NORMAL TERMINATION" not in engine_log,
            "normal termination marker is absent",
        )
    else:
        if result.check(
            "Engine return code", engine_code == 0, f"return code={engine_code}"
        ):
            result.check(
                "Engine normal termination",
                "NORMAL TERMINATION" in engine_log,
                "normal termination marker found",
            )
            official_path = case_dir / f"{root_name}_0001.out"
            official_log = official_path.read_text(encoding="utf-8", errors="replace")
            live_records = public_eig_records(engine_log)
            official_records = public_eig_records(official_log)
            result.check(
                "official /EIG log parity",
                live_records == official_records,
                f"live={len(live_records)}, official={len(official_records)}",
            )
            if "expected_warning" in case:
                expected_warning = str(case["expected_warning"])
                result.check(
                    "Expected warning",
                    expected_warning.lower() in engine_log.lower(),
                    f"expected substring={expected_warning!r}",
                )
            try:
                validate_case(case, case_dir, result, root_name, engine_log)
            except (OSError, ValueError, KeyError, AnimationFormatError) as error:
                result.check("validator completed", False, repr(error))
            except np.linalg.LinAlgError as error:
                result.check("independent eigensolve", False, repr(error))
    result.passed = all(check.passed for check in result.checks)
    write_case_validation(case_dir, result)
    return result


def write_report(
    suite_dir: Path,
    results: list[CaseResult],
    revision: str,
    source_dirty: bool,
    starter: Path,
    engine: Path,
    mpi_ranks: int,
    threads: int,
    backend: str,
) -> None:
    passed = sum(result.passed for result in results)
    lines = [
        "# OpenRadioss public `/EIG` end-to-end review",
        "",
        f"- Generated: {datetime.now(timezone.utc).isoformat()}",
        f"- Source revision: `{revision}`",
        f"- Source state: `{'modified' if source_dirty else 'clean'}` "
        "([status](source_status.txt), [provenance](provenance.json))",
        f"- Starter: `{starter}`",
        f"- Engine: `{engine}`",
        f"- MPI ranks: `{mpi_ranks}`",
        f"- OpenMP threads: `{threads}`",
        f"- Requested backend: `{backend}`",
        f"- Result: **{passed}/{len(results)} cases passed**",
        "",
        "## Case summary",
        "",
        "| Case | Expected | Result | DOFs | Modes | Frequency range (Hz) | Engine time |",
        "|---|---:|---:|---:|---:|---:|---:|",
    ]
    for result in results:
        expectation = "reject" if result.expected_failure else "solve"
        outcome = "PASS" if result.passed else "FAIL"
        dofs = result.metrics.get("dofs", "—")
        first = result.metrics.get("first_frequency_hz")
        last = result.metrics.get("last_frequency_hz")
        frequencies = f"{first:.6g}–{last:.6g}" if first is not None else "—"
        modes = result.metrics.get("mode_count", "—")
        lines.append(
            f"| [{result.name}]({result.name}/validation.json) | {expectation} | "
            f"**{outcome}** | {dofs} | {modes} | {frequencies} | "
            f"{result.engine_seconds:.2f} s |"
        )

    lines.extend(["", "## Detailed checks", ""])
    for result in results:
        lines.extend(
            [
                f"### {result.name}: {'PASS' if result.passed else 'FAIL'}",
                "",
                result.description,
                "",
            ]
        )
        for check in result.checks:
            marker = "✅" if check.passed else "❌"
            lines.append(f"- {marker} **{check.name}** — {check.detail}")
        if result.metrics:
            lines.extend(["", "Metrics:", ""])
            for key, value in sorted(result.metrics.items()):
                formatted = f"{value:.8g}" if isinstance(value, float) else str(value)
                lines.append(f"- `{key}`: {formatted}")
        lines.append("")

    lines.extend(
        [
            "## Review guide",
            "",
            "Each case directory contains the exact generated Starter and Engine decks, "
            "captured process logs, native OpenRadioss outputs, E### eigenmode ANIM "
            "states where requested, and `validation.json`. `MANIFEST.sha256` "
            "makes the review bundle tamper-evident.",
            "",
        ]
    )
    (suite_dir / "REPORT.md").write_text("\n".join(lines), encoding="utf-8")


def write_manifest(suite_dir: Path) -> None:
    excluded = {"MANIFEST.sha256"}
    lines = []
    for path in sorted(item for item in suite_dir.rglob("*") if item.is_file()):
        if path.name in excluded:
            continue
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        lines.append(f"{digest}  {path.relative_to(suite_dir)}")
    (suite_dir / "MANIFEST.sha256").write_text(
        "\n".join(lines) + "\n", encoding="utf-8"
    )


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def write_provenance(
    suite_dir: Path,
    cases_file: Path,
    repo: Path,
    revision: str,
    source_status: str,
    starter: Path,
    engine: Path,
    mpi_ranks: int,
    threads: int,
    backend: str,
) -> None:
    harness_dir = suite_dir / "harness_snapshot"
    harness_dir.mkdir()
    harness_files = [
        Path(__file__).resolve(),
        Path(__file__).with_name("anim_reader.py").resolve(),
        cases_file.resolve(),
    ]
    for source in harness_files:
        shutil.copy2(source, harness_dir / source.name)
    (suite_dir / "source_status.txt").write_text(
        source_status or "clean\n", encoding="utf-8"
    )
    payload = {
        "generated_utc": datetime.now(timezone.utc).isoformat(),
        "platform": platform.platform(),
        "python": sys.version,
        "numpy": np.__version__,
        "source_root": str(repo),
        "source_revision": revision,
        "source_dirty": bool(source_status.strip()),
        "mpi_ranks": mpi_ranks,
        "openmp_threads": threads,
        "requested_backend": backend,
        "starter": {
            "path": str(starter),
            "sha256": file_sha256(starter),
            "size_bytes": starter.stat().st_size,
        },
        "engine": {
            "path": str(engine),
            "sha256": file_sha256(engine),
            "size_bytes": engine.stat().st_size,
        },
    }
    (suite_dir / "provenance.json").write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def select_cases(
    cases: list[dict[str, Any]], requested_names: list[str] | None
) -> list[dict[str, Any]]:
    if not requested_names:
        return cases
    selected = set(requested_names)
    known = {case["name"] for case in cases}
    unknown = sorted(selected - known)
    if unknown:
        raise ValueError(f"unknown cases: {', '.join(unknown)}")
    return [case for case in cases if case["name"] in selected]


def resolve_executables(
    args: argparse.Namespace,
) -> tuple[Path, Path, Path]:
    starter = args.starter.resolve()
    engine = args.engine.resolve()
    mpiexec = args.mpiexec.resolve()
    if not starter.is_file():
        raise FileNotFoundError(f"Starter executable not found: {starter}")
    if not engine.is_file():
        raise FileNotFoundError(f"Engine executable not found: {engine}")
    if args.np > 1 and not mpiexec.is_file():
        raise FileNotFoundError(f"MPI launcher not found: {mpiexec}")
    return starter, engine, mpiexec


def source_identity(repo: Path) -> tuple[str, str]:
    revision = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=repo,
        stdout=subprocess.PIPE,
        text=True,
        check=True,
    ).stdout.strip()
    status = subprocess.run(
        ["git", "status", "--short"],
        cwd=repo,
        stdout=subprocess.PIPE,
        text=True,
        check=True,
    ).stdout
    return revision, status


def main() -> int:
    args = parse_args()
    if args.np < 1:
        raise ValueError("--np must be at least one")
    if args.threads < 1:
        raise ValueError("--threads must be at least one")
    repo = Path(__file__).resolve().parents[2]
    configuration = json.loads(args.cases_file.read_text(encoding="utf-8"))
    cases: list[dict[str, Any]] = configuration["cases"]
    if args.list:
        for case in cases:
            print(f"{case['name']}: {case['description']}")
        return 0
    cases = select_cases(cases, args.case)
    starter, engine, mpiexec = resolve_executables(args)
    fixture = (repo / configuration["fixture"]).resolve()
    root_name = str(configuration.get("root_name", fixture.stem.rsplit("_", 1)[0]))
    support_files = [
        (repo / relative).resolve()
        for relative in configuration.get("fixture_support_files", [])
    ]
    suite_dir = args.output_dir.resolve()
    suite_dir.mkdir(parents=True, exist_ok=False)
    environment = solver_environment(repo, args.threads)
    environment["OPENRADIOSS_EIG_BACKEND"] = args.backend
    prterun = Path("/usr/lib64/openmpi/bin/prterun")
    if prterun.is_file():
        environment.setdefault("OMPI_PRTERUN", str(prterun))
    revision, source_status = source_identity(repo)
    write_provenance(
        suite_dir,
        args.cases_file,
        repo,
        revision,
        source_status,
        starter,
        engine,
        args.np,
        args.threads,
        args.backend,
    )

    results: list[CaseResult] = []
    print(f"Review bundle: {suite_dir}")
    for index, case in enumerate(cases, start=1):
        print(f"[{index}/{len(cases)}] {case['name']} ... ", end="", flush=True)
        result = run_case(
            case,
            repo,
            suite_dir,
            fixture,
            support_files,
            starter,
            engine,
            environment,
            args.timeout,
            root_name,
            args.np,
            mpiexec,
        )
        results.append(result)
        print("PASS" if result.passed else "FAIL")

    payload = {
        "revision": revision,
        "starter": str(starter),
        "engine": str(engine),
        "mpi_ranks": args.np,
        "openmp_threads": args.threads,
        "requested_backend": args.backend,
        "passed": sum(result.passed for result in results),
        "total": len(results),
        "cases": [result.to_json() for result in results],
    }
    (suite_dir / "summary.json").write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    write_report(
        suite_dir,
        results,
        revision,
        bool(source_status.strip()),
        starter,
        engine,
        args.np,
        args.threads,
        args.backend,
    )
    write_manifest(suite_dir)
    print(f"Summary: {payload['passed']}/{payload['total']} cases passed")
    print(f"Report: {suite_dir / 'REPORT.md'}")
    return 0 if payload["passed"] == payload["total"] else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(2) from error
