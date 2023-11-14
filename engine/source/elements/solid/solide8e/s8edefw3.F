Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  S8EDEFW3                      source/elements/solid/solide8e/s8edefw3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDEFW3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   VX1,     VX2,     VX3,     VX4,
     5   VX5,     VX6,     VX7,     VX8,
     6   VY1,     VY2,     VY3,     VY4,
     7   VY5,     VY6,     VY7,     VY8,
     8   VZ1,     VZ2,     VZ3,     VZ4,
     9   VZ5,     VZ6,     VZ7,     VZ8,
     A   WXX,     WYY,     WZZ,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
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
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   WXX(*), WYY(*), WZZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C     REAL
      my_real
     .   DXY, DYX, DXZ,
     .   DZY, DYZ, DZX,
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46, DT1D2, DT1D
C-----------------------------------------------
      DT1D2=HALF*DT1
       DO I=1,NEL
         VX17=VX1(I)-VX7(I)
         VX28=VX2(I)-VX8(I)
         VX35=VX3(I)-VX5(I)
         VX46=VX4(I)-VX6(I)
         VY17=VY1(I)-VY7(I)
         VY28=VY2(I)-VY8(I)
         VY35=VY3(I)-VY5(I)
         VY46=VY4(I)-VY6(I)
         VZ17=VZ1(I)-VZ7(I)
         VZ28=VZ2(I)-VZ8(I)
         VZ35=VZ3(I)-VZ5(I)
         VZ46=VZ4(I)-VZ6(I)
         DXY=PY1(I)*VX17+PY2(I)*VX28+
     .    PY3(I)*VX35+PY4(I)*VX46
         DXZ=PZ1(I)*VX17+PZ2(I)*VX28+
     .    PZ3(I)*VX35+PZ4(I)*VX46
         DYX=PX1(I)*VY17+PX2(I)*VY28+
     .    PX3(I)*VY35+PX4(I)*VY46
         DYZ=PZ1(I)*VY17+PZ2(I)*VY28+
     .    PZ3(I)*VY35+PZ4(I)*VY46
         DZX=PX1(I)*VZ17+PX2(I)*VZ28+
     .    PX3(I)*VZ35+PX4(I)*VZ46
         DZY=PY1(I)*VZ17+PY2(I)*VZ28+
     .    PY3(I)*VZ35+PY4(I)*VZ46
C
        WZZ(I)=DT1D2*(DYX-DXY)
        WYY(I)=DT1D2*(DXZ-DZX)
        WXX(I)=DT1D2*(DZY-DYZ)
       ENDDO
C
C
      RETURN
C
      END
