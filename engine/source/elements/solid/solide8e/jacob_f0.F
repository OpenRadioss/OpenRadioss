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
Chd|  JACOB_F0                      source/elements/solid/solide8e/jacob_f0.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/jacob_j33.F
Chd|        SDEFOT3                       source/elements/solid/solide/sdefot3.F
Chd|====================================================================
      SUBROUTINE JACOB_F0(
     1   DET0,    PX1,     PX2,     PX3,
     2   PX4,     PY1,     PY2,     PY3,
     3   PY4,     PZ1,     PZ2,     PZ3,
     4   PZ4,     VX1,     VX2,     VX3,
     5   VX4,     VX5,     VX6,     VX7,
     6   VX8,     VY1,     VY2,     VY3,
     7   VY4,     VY5,     VY6,     VY7,
     8   VY8,     VZ1,     VZ2,     VZ3,
     9   VZ4,     VZ5,     VZ6,     VZ7,
     A   VZ8,    DIV0,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), DET0(*)
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: DIV0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C                                                                     12
      my_real
     .   DXX(MVSIZ), DXY(MVSIZ), DXZ(MVSIZ),
     .   DYX(MVSIZ), DYY(MVSIZ), DYZ(MVSIZ),
     .   DZX(MVSIZ), DZY(MVSIZ), DZZ(MVSIZ)
C-----------------------------------------------
       CALL SDEFOT3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   VX1,     VX2,     VX3,     VX4,
     5   VX5,     VX6,     VX7,     VX8,
     6   VY1,     VY2,     VY3,     VY4,
     7   VY5,     VY6,     VY7,     VY8,
     8   VZ1,     VZ2,     VZ3,     VZ4,
     9   VZ5,     VZ6,     VZ7,     VZ8,
     A   DXX,     DXY,     DXZ,     DYX,
     B   DYY,     DYZ,     DZX,     DZY,
     C   DZZ,     NEL)
C-----------
       CALL JACOB_J33(
     1   DET0,    DXX,     DXY,     DXZ,
     2   DYX,     DYY,     DYZ,     DZX,
     3   DZY,     DZZ,     NEL)
C       
       DIV0(1:NEL) = DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL)
      RETURN
      END
