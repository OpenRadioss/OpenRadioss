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
Chd|  R3COOR3                       source/elements/spring/r3coor3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R3COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    X3DP,    NGL,
     3   MGN,     NC1,     NC2,     NC3,
     4   NEL)
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
      INTEGER IXR(NIXR,*),NGL(*),MGN(*),NC1(*),NC2(*),NC3(*)
C     REAL
      my_real
     .   X(3,*), VR(3,*)
      DOUBLE PRECISION XDP(3,*),
     .                 X1DP(3,*),X2DP(3,*),X3DP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------------------------
      DO I=1,NEL
        NGL(I)=IXR(NIXR,I)
        NC1(I)=IXR(2,I)
        NC2(I)=IXR(3,I)
        NC3(I)=IXR(4,I)
        MGN(I)=IXR(1,I)
      ENDDO
C----------------------------
C     COORDONNEES+ROTATIONS
C----------------------------
      IF (IRESP == 1) THEN
        DO I=1,NEL
          X1DP(1,I)=XDP(1,NC1(I))
          X1DP(2,I)=XDP(2,NC1(I))
          X1DP(3,I)=XDP(3,NC1(I))
          X2DP(1,I)=XDP(1,NC2(I))
          X2DP(2,I)=XDP(2,NC2(I))
          X2DP(3,I)=XDP(3,NC2(I))
          X3DP(1,I)=XDP(1,NC3(I))
          X3DP(2,I)=XDP(2,NC3(I))
          X3DP(3,I)=XDP(3,NC3(I))
        END DO
      ELSE
        DO I=1,NEL
          X1DP(1,I)=X(1,NC1(I))
          X1DP(2,I)=X(2,NC1(I))
          X1DP(3,I)=X(3,NC1(I))
          X2DP(1,I)=X(1,NC2(I))
          X2DP(2,I)=X(2,NC2(I))
          X2DP(3,I)=X(3,NC2(I))
          X3DP(1,I)=X(1,NC3(I))
          X3DP(2,I)=X(2,NC3(I))
          X3DP(3,I)=X(3,NC3(I))
        ENDDO
      ENDIF
C---
      RETURN
      END
