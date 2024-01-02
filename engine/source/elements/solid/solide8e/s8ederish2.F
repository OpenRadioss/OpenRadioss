Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  S8EDERISH2                    source/elements/solid/solide8e/s8ederish2.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|        S8EA2BP8                      source/elements/solid/solide8e/s8ea2bp8.F
Chd|====================================================================
      SUBROUTINE S8EDERISH2(
     1   PXY1,    PXY2,    PXY3,    PXY4,
     2   PXY5,    PXY6,    PXY7,    PXY8,
     3   PYX1,    PYX2,    PYX3,    PYX4,
     4   PYX5,    PYX6,    PYX7,    PYX8,
     5   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     6   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     7   PZX1,    PZX2,    PZX3,    PZX4,
     8   PZX5,    PZX6,    PZX7,    PZX8,
     9   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     A   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     B   PZY1,    PZY2,    PZY3,    PZY4,
     C   PZY5,    PZY6,    PZY7,    PZY8,
     D   P0XY1,   P0XY2,   P0XY3,   P0XY4,
     E   P0XY5,   P0XY6,   P0XY7,   P0XY8,
     F   P0YX1,   P0YX2,   P0YX3,   P0YX4,
     G   P0YX5,   P0YX6,   P0YX7,   P0YX8,
     H   P0XZ1,   P0XZ2,   P0XZ3,   P0XZ4,
     I   P0XZ5,   P0XZ6,   P0XZ7,   P0XZ8,
     J   P0ZX1,   P0ZX2,   P0ZX3,   P0ZX4,
     K   P0ZX5,   P0ZX6,   P0ZX7,   P0ZX8,
     L   P0YZ1,   P0YZ2,   P0YZ3,   P0YZ4,
     M   P0YZ5,   P0YZ6,   P0YZ7,   P0YZ8,
     N   P0ZY1,   P0ZY2,   P0ZY3,   P0ZY4,
     O   P0ZY5,   P0ZY6,   P0ZY7,   P0ZY8,
     P   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   P0XY1(*),P0XY2(*),P0XY3(*),P0XY4(*),
     .   P0XY5(*),P0XY6(*),P0XY7(*),P0XY8(*),
     .   P0YX1(*),P0YX2(*),P0YX3(*),P0YX4(*),
     .   P0YX5(*),P0YX6(*),P0YX7(*),P0YX8(*),
     .   P0XZ1(*),P0XZ2(*),P0XZ3(*),P0XZ4(*),
     .   P0XZ5(*),P0XZ6(*),P0XZ7(*),P0XZ8(*),
     .   P0ZX1(*),P0ZX2(*),P0ZX3(*),P0ZX4(*),
     .   P0ZX5(*),P0ZX6(*),P0ZX7(*),P0ZX8(*),
     .   P0YZ1(*),P0YZ2(*),P0YZ3(*),P0YZ4(*),
     .   P0YZ5(*),P0YZ6(*),P0YZ7(*),P0YZ8(*),
     .   P0ZY1(*),P0ZY2(*),P0ZY3(*),P0ZY4(*),
     .   P0ZY5(*),P0ZY6(*),P0ZY7(*),P0ZY8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C                                                                     12
          CALL S8EA2BP8(
     1   PXY1,    PXY2,    PXY3,    PXY4,
     2   PXY5,    PXY6,    PXY7,    PXY8,
     3   P0XY1,   P0XY2,   P0XY3,   P0XY4,
     4   P0XY5,   P0XY6,   P0XY7,   P0XY8,
     5   NEL)
          CALL S8EA2BP8(
     1   PYX1,    PYX2,    PYX3,    PYX4,
     2   PYX5,    PYX6,    PYX7,    PYX8,
     3   P0YX1,   P0YX2,   P0YX3,   P0YX4,
     4   P0YX5,   P0YX6,   P0YX7,   P0YX8,
     5   NEL)
          CALL S8EA2BP8(
     1   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     2   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     3   P0XZ1,   P0XZ2,   P0XZ3,   P0XZ4,
     4   P0XZ5,   P0XZ6,   P0XZ7,   P0XZ8,
     5   NEL)
          CALL S8EA2BP8(
     1   PZX1,    PZX2,    PZX3,    PZX4,
     2   PZX5,    PZX6,    PZX7,    PZX8,
     3   P0ZX1,   P0ZX2,   P0ZX3,   P0ZX4,
     4   P0ZX5,   P0ZX6,   P0ZX7,   P0ZX8,
     5   NEL)
          CALL S8EA2BP8(
     1   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     2   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     3   P0YZ1,   P0YZ2,   P0YZ3,   P0YZ4,
     4   P0YZ5,   P0YZ6,   P0YZ7,   P0YZ8,
     5   NEL)
          CALL S8EA2BP8(
     1   PZY1,    PZY2,    PZY3,    PZY4,
     2   PZY5,    PZY6,    PZY7,    PZY8,
     3   P0ZY1,   P0ZY2,   P0ZY3,   P0ZY4,
     4   P0ZY5,   P0ZY6,   P0ZY7,   P0ZY8,
     5   NEL)
C
      RETURN
       END
