Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    r1coor3   ../engine/source/elements/spring/r1coor3.F
!||--- called by ------------------------------------------------------
!||    rforc3    ../engine/source/elements/spring/rforc3.F
!||====================================================================
      SUBROUTINE R1COOR3(
     1   X,       VR,      IXR,     XDP,
     2   X1DP,    X2DP,    NGL,     MGM,
     3   NC1,     NC2,     NEL)
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
      INTEGER IXR(NIXR,*),NGL(*),MGM(*),NC1(*),NC2(*) 
      my_real
     .   X(3,*), VR(3,*)
      DOUBLE PRECISION XDP(3,*),X1DP(3,*),X2DP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------------------------
      DO I=1,NEL
        NGL(I)=IXR(NIXR,I)
        NC1(I)=IXR(2,I)
        NC2(I)=IXR(3,I)
!        NC3(I)=IXR(4,I)  ! not used
        MGM(I)=IXR(1,I)
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
        ENDDO
      ELSE
        DO I=1,NEL
          X1DP(1,I)=X(1,NC1(I))
          X1DP(2,I)=X(2,NC1(I))
          X1DP(3,I)=X(3,NC1(I))
          X2DP(1,I)=X(1,NC2(I))
          X2DP(2,I)=X(2,NC2(I))
          X2DP(3,I)=X(3,NC2(I))
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
