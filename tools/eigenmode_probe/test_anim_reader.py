#!/usr/bin/env python3
"""Unit tests for the independent Radioss ANIM shell reader."""

from __future__ import annotations

import struct
import tempfile
from pathlib import Path
import unittest

import numpy as np

from anim_reader import AnimationFormatError, read_shell_animation


def _text(value: str, width: int) -> bytes:
    return value.encode("latin-1").ljust(width, b"\x00")


def _synthetic_animation() -> bytes:
    chunks = [
        struct.pack(">if", 21548, 12.5),
        _text("Frequency=", 81),
        _text("Eigenmode", 81),
        _text("Radioss Run=", 81),
        struct.pack(">10i", 1, 1, 0, 0, 1, 0, 1, 0, 0, 0),
        struct.pack(">8i", 4, 1, 1, 0, 2, 2, 0, 0),
        struct.pack(">12f", *range(12)),
        struct.pack(">4i", 1, 2, 3, 4),
        b"\x01",
        struct.pack(">i", 1),
        _text("part one", 50),
        struct.pack(">12h", *([0] * 12)),
        _text("Specific Energy", 81),
        _text("Internal Energy", 81),
        struct.pack(">2f", 3.0, 6.0),
        _text("Displacement", 81),
        _text("Rotational DOF", 81),
        struct.pack(">24f", *range(24)),
        struct.pack(">5f", 2.0, 0.5, 0.5, 0.5, 0.5),
        struct.pack(">5i", 101, 102, 103, 104, 201),
        struct.pack(">3i", 0, 1, 1),
    ]
    return b"".join(chunks)


class ShellAnimationReaderTests(unittest.TestCase):
    def test_reads_modal_vectors_scalars_masses_and_numbering(self) -> None:
        payload = _synthetic_animation()
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "caseA001"
            path.write_bytes(payload)
            animation = read_shell_animation(path)

        self.assertEqual(animation.magic, 21548)
        self.assertEqual(animation.state_value, 12.5)
        self.assertEqual(animation.mode_label, "Eigenmode")
        self.assertEqual(animation.node_count, 4)
        self.assertEqual(animation.element_count, 1)
        self.assertEqual(animation.part_names, ("part one",))
        np.testing.assert_array_equal(animation.connectivity, [[1, 2, 3, 4]])
        np.testing.assert_allclose(animation.scalar_fields["Specific Energy"], [3])
        np.testing.assert_allclose(animation.scalar_fields["Internal Energy"], [6])
        np.testing.assert_allclose(animation.element_masses, [2])
        np.testing.assert_array_equal(animation.node_numbers, [101, 102, 103, 104])
        np.testing.assert_array_equal(animation.element_numbers, [201])
        self.assertEqual(set(animation.vector_fields), {"Displacement", "Rotational DOF"})
        self.assertEqual(animation.parsed_bytes, len(payload))

    def test_rejects_a_truncated_file(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "truncated"
            path.write_bytes(_synthetic_animation()[:100])
            with self.assertRaisesRegex(AnimationFormatError, "file size"):
                read_shell_animation(path)


if __name__ == "__main__":
    unittest.main()
