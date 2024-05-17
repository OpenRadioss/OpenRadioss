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
Chd|  SURF_AREA                     source/output/th/surf_area.F  
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SURF_AREA(X, NN, SURF_NODES, AREA, NUMNOD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NN,SURF_NODES(NN,4),NUMNOD
      my_real,INTENT(IN) ::  X(3,NUMNOD)
      my_real,INTENT(INOUT) :: AREA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NOD1,NOD2,NOD3,NOD4
      my_real X13, Y13, Z13, X24, Y24, Z24,N1, N2, N3
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      AREA = ZERO
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
C-----------------------------------------------
      RETURN
      END
