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
!||    surf_area   ../engine/source/output/th/surf_area.F
!||--- called by ------------------------------------------------------
!||    hist2       ../engine/source/output/th/hist2.F
!||====================================================================
      SUBROUTINE SURF_AREA(X, NN, SURF_NODES, AREA, NUMNOD, N2D)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NN,NUMNOD
      INTEGER,INTENT(IN) :: SURF_NODES(NN,4) !< FACE NODES (NN FACES, 3D:4nodes, 2D:2nodes only are used)
      my_real,INTENT(IN) ::  X(3,NUMNOD)     !< coordinates x,y,z for each node
      my_real,INTENT(INOUT) :: AREA          !< surface area
      INTEGER,INTENT(IN) :: N2D              !0:3D, 1:2d-axi 2:2d-plane
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,NOD1,NOD2,NOD3,NOD4
      my_real :: X13, Y13, Z13, X24, Y24, Z24,N1, N2, N3
      my_real :: Y12, Z12
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      AREA = ZERO
      IF(N2D == 0)THEN
        DO I=1,NN
           NOD1=SURF_NODES(I,1)
           NOD2=SURF_NODES(I,2)
           NOD3=SURF_NODES(I,3)
           NOD4=SURF_NODES(I,4)
           X13= X(1,NOD3)-X(1,NOD1)
           Y13= X(2,NOD3)-X(2,NOD1)
           Z13= X(3,NOD3)-X(3,NOD1)
           X24= X(1,NOD4)-X(1,NOD2)
           Y24= X(2,NOD4)-X(2,NOD2)
           Z24= X(3,NOD4)-X(3,NOD2)
           N1 = Y13*Z24-Y24*Z13
           N2 = Z13*X24-Z24*X13
           N3 = X13*Y24-X24*Y13
           AREA = AREA + SQRT(N1*N1+N2*N2+N3*N3)
        ENDDO
        AREA = AREA * HALF
      ELSE
        !2D case
        DO I=1,NN
           NOD1=SURF_NODES(I,1)
           NOD2=SURF_NODES(I,2)
           !vector N1-N2 (2d:0,y,z)
           Y12= X(2,NOD1)-X(2,NOD2)
           Z12= X(3,NOD1)-X(3,NOD2)
           AREA = AREA + SQRT(Y12*Y12+Z12*Z12)
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
