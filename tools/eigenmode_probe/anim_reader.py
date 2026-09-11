#!/usr/bin/env python3
"""Minimal independent reader for structural sections of Radioss ANIM files.

The end-to-end eigenmode tests intentionally do not use the Engine's output
code to validate itself.  This reader implements the stable leading 2-D block
and the immediately following 3-D and basic 1-D blocks needed to inspect modal
coordinates, vectors, structural scalar fields, tensors, masses, and numbering.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np


class AnimationFormatError(ValueError):
    """Raised when an ANIM file is truncated or structurally inconsistent."""


class _Cursor:
    def __init__(self, data: bytes, path: Path) -> None:
        self.data = memoryview(data)
        self.path = path
        self.position = 0

    def _take(self, count: int) -> memoryview:
        end = self.position + count
        if count < 0 or end > len(self.data):
            raise AnimationFormatError(
                f"{self.path}: need {count} bytes at offset {self.position}, "
                f"file size is {len(self.data)}"
            )
        result = self.data[self.position:end]
        self.position = end
        return result

    def array(self, dtype: str, count: int) -> np.ndarray:
        itemsize = np.dtype(dtype).itemsize
        return np.frombuffer(self._take(itemsize * count), dtype=dtype).copy()

    def integer(self) -> int:
        return int(self.array(">i4", 1)[0])

    def real(self) -> float:
        return float(self.array(">f4", 1)[0])

    def text(self, width: int) -> str:
        raw = bytes(self._take(width))
        return raw.replace(b"\x00", b"").replace(b"\x01", b"").decode(
            "latin-1"
        ).strip()


@dataclass(frozen=True)
class ShellAnimation:
    path: Path
    magic: int
    state_value: float
    state_label: str
    mode_label: str
    run_label: str
    flags: tuple[int, ...]
    node_count: int
    element_count: int
    part_count: int
    coordinates: np.ndarray
    connectivity: np.ndarray
    element_alive: np.ndarray
    part_boundaries: np.ndarray
    part_names: tuple[str, ...]
    scalar_fields: dict[str, np.ndarray]
    vector_fields: dict[str, np.ndarray]
    tensor_fields: dict[str, np.ndarray]
    element_masses: np.ndarray | None
    node_masses: np.ndarray | None
    node_numbers: np.ndarray | None
    element_numbers: np.ndarray | None
    solid_element_count: int
    solid_part_count: int
    solid_connectivity: np.ndarray
    solid_element_alive: np.ndarray
    solid_part_boundaries: np.ndarray
    solid_part_names: tuple[str, ...]
    solid_scalar_fields: dict[str, np.ndarray]
    solid_tensor_fields: dict[str, np.ndarray]
    solid_element_masses: np.ndarray | None
    solid_element_numbers: np.ndarray | None
    oned_element_count: int
    oned_part_count: int
    oned_connectivity: np.ndarray
    oned_element_alive: np.ndarray
    oned_part_boundaries: np.ndarray
    oned_part_names: tuple[str, ...]
    oned_scalar_fields: dict[str, np.ndarray]
    oned_tensor_fields: dict[str, np.ndarray]
    oned_skew_numbers: np.ndarray
    oned_element_masses: np.ndarray | None
    oned_element_numbers: np.ndarray | None
    parsed_bytes: int


@dataclass(frozen=True)
class _ElementSection:
    element_count: int
    part_count: int
    connectivity: np.ndarray
    element_alive: np.ndarray
    part_boundaries: np.ndarray
    part_names: tuple[str, ...]
    scalar_fields: dict[str, np.ndarray]
    tensor_fields: dict[str, np.ndarray]
    skew_numbers: np.ndarray
    element_masses: np.ndarray | None
    element_numbers: np.ndarray | None


def _empty_element_section(connectivity_width: int) -> _ElementSection:
    return _ElementSection(
        element_count=0,
        part_count=0,
        connectivity=np.empty((0, connectivity_width), dtype=">i4"),
        element_alive=np.empty(0, dtype=bool),
        part_boundaries=np.empty(0, dtype=">i4"),
        part_names=(),
        scalar_fields={},
        tensor_fields={},
        skew_numbers=np.empty(0, dtype=">i4"),
        element_masses=None,
        element_numbers=None,
    )


def _read_element_fields(
    cursor: _Cursor,
    element_count: int,
    scalar_count: int,
    tensor_count: int,
    tensor_width: int,
) -> tuple[dict[str, np.ndarray], dict[str, np.ndarray]]:
    scalar_names = tuple(cursor.text(81) for _ in range(scalar_count))
    scalars = cursor.array(">f4", element_count * scalar_count)
    if scalar_count:
        scalars = scalars.reshape(scalar_count, element_count)
    scalar_fields = {
        name: scalars[index] for index, name in enumerate(scalar_names)
    }

    tensor_names = tuple(cursor.text(81) for _ in range(tensor_count))
    tensors = cursor.array(">f4", element_count * tensor_count * tensor_width)
    if tensor_count:
        tensors = tensors.reshape(tensor_count, element_count, tensor_width)
    tensor_fields = {
        name: tensors[index] for index, name in enumerate(tensor_names)
    }
    return scalar_fields, tensor_fields


def _read_solid_section(
    cursor: _Cursor, path: Path, flags: tuple[int, ...]
) -> _ElementSection:
    if flags[2] == 0:
        return _empty_element_section(8)

    element_count = cursor.integer()
    part_count = cursor.integer()
    scalar_count = cursor.integer()
    tensor_count = cursor.integer()
    counts = (element_count, part_count, scalar_count, tensor_count)
    if any(count < 0 for count in counts):
        raise AnimationFormatError(f"{path}: negative solid count: {counts}")
    connectivity = cursor.array(">i4", element_count * 8).reshape(
        element_count, 8
    )
    element_alive = cursor.array("u1", element_count).astype(bool)
    part_boundaries = cursor.array(">i4", part_count)
    part_names = tuple(cursor.text(50) for _ in range(part_count))
    scalar_fields, tensor_fields = _read_element_fields(
        cursor, element_count, scalar_count, tensor_count, 6
    )
    element_masses = (
        cursor.array(">f4", element_count) if flags[0] == 1 else None
    )
    element_numbers = (
        cursor.array(">i4", element_count) if flags[1] != 0 else None
    )
    return _ElementSection(
        element_count=element_count,
        part_count=part_count,
        connectivity=connectivity,
        element_alive=element_alive,
        part_boundaries=part_boundaries,
        part_names=part_names,
        scalar_fields=scalar_fields,
        tensor_fields=tensor_fields,
        skew_numbers=np.empty(0, dtype=">i4"),
        element_masses=element_masses,
        element_numbers=element_numbers,
    )


def _read_oned_section(
    cursor: _Cursor, path: Path, flags: tuple[int, ...]
) -> _ElementSection:
    if flags[3] == 0:
        return _empty_element_section(2)

    element_count = cursor.integer()
    part_count = cursor.integer()
    scalar_count = cursor.integer()
    tensor_count = cursor.integer()
    has_skew = cursor.integer()
    counts = (element_count, part_count, scalar_count, tensor_count)
    if any(count < 0 for count in counts):
        raise AnimationFormatError(f"{path}: negative 1-D count: {counts}")
    connectivity = cursor.array(">i4", element_count * 2).reshape(
        element_count, 2
    )
    element_alive = cursor.array("u1", element_count).astype(bool)
    part_boundaries = cursor.array(">i4", part_count)
    part_names = tuple(cursor.text(50) for _ in range(part_count))
    scalar_fields, tensor_fields = _read_element_fields(
        cursor, element_count, scalar_count, tensor_count, 9
    )
    # Standard truss and beam records carry one skew index per displayed
    # element. Exotic two-segment springs are outside these fixtures.
    skew_numbers = (
        cursor.array(">i4", element_count)
        if has_skew
        else np.empty(0, dtype=">i4")
    )
    element_masses = (
        cursor.array(">f4", element_count) if flags[0] == 1 else None
    )
    element_numbers = (
        cursor.array(">i4", element_count) if flags[1] != 0 else None
    )
    return _ElementSection(
        element_count=element_count,
        part_count=part_count,
        connectivity=connectivity,
        element_alive=element_alive,
        part_boundaries=part_boundaries,
        part_names=part_names,
        scalar_fields=scalar_fields,
        tensor_fields=tensor_fields,
        skew_numbers=skew_numbers,
        element_masses=element_masses,
        element_numbers=element_numbers,
    )


def read_shell_animation(path: Path | str) -> ShellAnimation:
    """Read the leading shell/nodal and optional solid ANIM sections."""
    path = Path(path)
    cursor = _Cursor(path.read_bytes(), path)

    magic = cursor.integer()
    state_value = cursor.real()
    state_label = cursor.text(81)
    mode_label = cursor.text(81)
    run_label = cursor.text(81)
    flags = tuple(cursor.integer() for _ in range(10))

    node_count = cursor.integer()
    element_count = cursor.integer()
    part_count = cursor.integer()
    nodal_scalar_count = cursor.integer()
    element_scalar_count = cursor.integer()
    vector_count = cursor.integer()
    tensor_count = cursor.integer()
    skew_count = cursor.integer()
    counts = (
        node_count,
        element_count,
        part_count,
        nodal_scalar_count,
        element_scalar_count,
        vector_count,
        tensor_count,
        skew_count,
    )
    if any(count < 0 for count in counts):
        raise AnimationFormatError(f"{path}: negative leading count: {counts}")

    cursor.array(">i2", skew_count * 6)
    coordinates = cursor.array(">f4", node_count * 3).reshape(node_count, 3)
    connectivity = cursor.array(">i4", element_count * 4).reshape(
        element_count, 4
    )
    element_alive = cursor.array("u1", element_count).astype(bool)
    part_boundaries = cursor.array(">i4", part_count)
    part_names = tuple(cursor.text(50) for _ in range(part_count))
    cursor.array(">i2", node_count * 3)

    field_names = tuple(
        cursor.text(81) for _ in range(nodal_scalar_count + element_scalar_count)
    )
    nodal_scalars = cursor.array(
        ">f4", node_count * nodal_scalar_count
    ).reshape(nodal_scalar_count, node_count)
    element_scalars = cursor.array(
        ">f4", element_count * element_scalar_count
    ).reshape(element_scalar_count, element_count)
    scalar_fields: dict[str, np.ndarray] = {}
    for index, name in enumerate(field_names[:nodal_scalar_count]):
        scalar_fields[f"node:{name}"] = nodal_scalars[index]
    for index, name in enumerate(field_names[nodal_scalar_count:]):
        scalar_fields[name] = element_scalars[index]

    vector_names = tuple(cursor.text(81) for _ in range(vector_count))
    vectors = cursor.array(">f4", node_count * vector_count * 3).reshape(
        vector_count, node_count, 3
    )
    vector_fields = {
        name: vectors[index] for index, name in enumerate(vector_names)
    }

    tensor_names = tuple(cursor.text(81) for _ in range(tensor_count))
    tensors = cursor.array(">f4", element_count * tensor_count * 3).reshape(
        tensor_count, element_count, 3
    )
    tensor_fields = {
        name: tensors[index] for index, name in enumerate(tensor_names)
    }

    element_masses = None
    node_masses = None
    if flags[0] == 1:
        element_masses = cursor.array(">f4", element_count)
        node_masses = cursor.array(">f4", node_count)

    node_numbers = None
    element_numbers = None
    if flags[1] != 0:
        node_numbers = cursor.array(">i4", node_count)
        element_numbers = cursor.array(">i4", element_count)

    if flags[4] != 0:
        cursor.array(">i4", part_count * 3)

    solid = _read_solid_section(cursor, path, flags)
    oned = _read_oned_section(cursor, path, flags)

    return ShellAnimation(
        path=path,
        magic=magic,
        state_value=state_value,
        state_label=state_label,
        mode_label=mode_label,
        run_label=run_label,
        flags=flags,
        node_count=node_count,
        element_count=element_count,
        part_count=part_count,
        coordinates=coordinates,
        connectivity=connectivity,
        element_alive=element_alive,
        part_boundaries=part_boundaries,
        part_names=part_names,
        scalar_fields=scalar_fields,
        vector_fields=vector_fields,
        tensor_fields=tensor_fields,
        element_masses=element_masses,
        node_masses=node_masses,
        node_numbers=node_numbers,
        element_numbers=element_numbers,
        solid_element_count=solid.element_count,
        solid_part_count=solid.part_count,
        solid_connectivity=solid.connectivity,
        solid_element_alive=solid.element_alive,
        solid_part_boundaries=solid.part_boundaries,
        solid_part_names=solid.part_names,
        solid_scalar_fields=solid.scalar_fields,
        solid_tensor_fields=solid.tensor_fields,
        solid_element_masses=solid.element_masses,
        solid_element_numbers=solid.element_numbers,
        oned_element_count=oned.element_count,
        oned_part_count=oned.part_count,
        oned_connectivity=oned.connectivity,
        oned_element_alive=oned.element_alive,
        oned_part_boundaries=oned.part_boundaries,
        oned_part_names=oned.part_names,
        oned_scalar_fields=oned.scalar_fields,
        oned_tensor_fields=oned.tensor_fields,
        oned_skew_numbers=oned.skew_numbers,
        oned_element_masses=oned.element_masses,
        oned_element_numbers=oned.element_numbers,
        parsed_bytes=cursor.position,
    )
