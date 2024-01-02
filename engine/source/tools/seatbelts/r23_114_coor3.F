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
Chd|  R23_114_COOR3                 source/tools/seatbelts/r23_114_coor3.F
Chd|-- called by -----------
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R23_114_COOR3(
     1   X,       V,       IXR,     XDP,
     2   X3DP,    NC3,     VX3,     VY3,
     3   VZ3,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER IXR(NIXR,*),NC3(*)
      my_real X(3,*),V(3,*),VX3(*),VY3(*),VZ3(*)
      DOUBLE PRECISION XDP(3,*),X3DP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------------------------
      DO I=1,NEL
C--     to prevent NC3 to be zero fo seatblet outside slipring
        IF (IXR(4,I) > 0) THEN
          NC3(I)=IXR(4,I)
        ELSE
          NC3(I)=IXR(2,I)
        ENDIF
      ENDDO
C----------------------------
C     COORDONNEES NOEUD ADDITIONEL
C----------------------------
      IF (IRESP == 1) THEN
        DO I=1,NEL
          X3DP(1,I)=XDP(1,NC3(I))
          X3DP(2,I)=XDP(2,NC3(I))
          X3DP(3,I)=XDP(3,NC3(I))
        ENDDO
      ELSE
        DO I=1,NEL
          X3DP(1,I)=X(1,NC3(I))
          X3DP(2,I)=X(2,NC3(I))
          X3DP(3,I)=X(3,NC3(I))
        ENDDO
      ENDIF
C
      DO I=1,NEL
        VX3(I)=V(1,NC3(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
      ENDDO
C---
      RETURN
      END
