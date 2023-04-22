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
Chd|  S8EGETPIJ                     source/elements/solid/solide8e/s8egetpij.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EGETPIJ(
     .    PX1, PX2, PX3, PX4,
     .    PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4,
     .    PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4,
     .    PZ5, PZ6, PZ7, PZ8,
     .    PIJ,NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),
     .  PIJ(NEL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

C-----------------------------------------------
       DO I=1,NEL
        PX1(I) = PIJ(I,1) 
        PY1(I) = PIJ(I,2) 
        PZ1(I) = PIJ(I,3) 
        PX2(I) = PIJ(I,4) 
        PY2(I) = PIJ(I,5) 
        PZ2(I) = PIJ(I,6) 
        PX3(I) = PIJ(I,7) 
        PY3(I) = PIJ(I,8) 
        PZ3(I) = PIJ(I,9) 
        PX4(I) = PIJ(I,10) 
        PY4(I) = PIJ(I,11) 
        PZ4(I) = PIJ(I,12) 
        PX5(I) = PIJ(I,13) 
        PY5(I) = PIJ(I,14) 
        PZ5(I) = PIJ(I,15) 
        PX6(I) = PIJ(I,16) 
        PY6(I) = PIJ(I,17) 
        PZ6(I) = PIJ(I,18) 
        PX7(I) = PIJ(I,19) 
        PY7(I) = PIJ(I,20) 
        PZ7(I) = PIJ(I,21) 
        PX8(I) = PIJ(I,22) 
        PY8(I) = PIJ(I,23) 
        PZ8(I) = PIJ(I,24) 
       END DO
C-----------
      RETURN
      END
