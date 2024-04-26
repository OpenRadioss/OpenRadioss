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
Chd|  S8GET_X3                      source/elements/solid/solide/s8get_x3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8GET_X3(
     .   NC1,      NC2,      NC3,      NC4,
     .   NC5,      NC6,      NC7,      NC8,
     .    X1,       X2,       X3,       X4,      
     .    X5,       X6,       X7,       X8,      
     .    Y1,       Y2,       Y3,       Y4,      
     .    Y5,       Y6,       Y7,       Y8,      
     .    Z1,       Z2,       Z3,       Z4,      
     .    Z5,       Z6,       Z7,       Z8,      
     .     X,      XDP,   NUMNOD,     NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NUMNOD,NEL
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: 
     .   NC1,      NC2,      NC3,      NC4,
     .   NC5,      NC6,      NC7,      NC8
!     
      my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: X
      DOUBLE PRECISION, DIMENSION(3,NUMNOD), INTENT(IN) ::XDP
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::
     .    X1,       X2,       X3,       X4,      
     .    X5,       X6,       X7,       X8,      
     .    Y1,       Y2,       Y3,       Y4,      
     .    Y5,       Y6,       Y7,       Y8,      
     .    Z1,       Z2,       Z3,       Z4,      
     .    Z5,       Z6,       Z7,       Z8      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
C-----------------------------------------------
#ifdef MYREAL8 
        DO I=1,NEL
          X1(I)=X(1,NC1(I))
          Y1(I)=X(2,NC1(I))
          Z1(I)=X(3,NC1(I))
          X2(I)=X(1,NC2(I))
          Y2(I)=X(2,NC2(I))
          Z2(I)=X(3,NC2(I))
          X3(I)=X(1,NC3(I))
          Y3(I)=X(2,NC3(I))
          Z3(I)=X(3,NC3(I))
          X4(I)=X(1,NC4(I))
          Y4(I)=X(2,NC4(I))
          Z4(I)=X(3,NC4(I))
          X5(I)=X(1,NC5(I))
          Y5(I)=X(2,NC5(I))
          Z5(I)=X(3,NC5(I))
          X6(I)=X(1,NC6(I))
          Y6(I)=X(2,NC6(I))
          Z6(I)=X(3,NC6(I))
          X7(I)=X(1,NC7(I))
          Y7(I)=X(2,NC7(I))
          Z7(I)=X(3,NC7(I))
          X8(I)=X(1,NC8(I))
          Y8(I)=X(2,NC8(I))
          Z8(I)=X(3,NC8(I))
        ENDDO
#else
        DO I=1,NEL
          X1(I)=XDP(1,NC1(I))
          Y1(I)=XDP(2,NC1(I))
          Z1(I)=XDP(3,NC1(I))
          X2(I)=XDP(1,NC2(I))
          Y2(I)=XDP(2,NC2(I))
          Z2(I)=XDP(3,NC2(I))
          X3(I)=XDP(1,NC3(I))
          Y3(I)=XDP(2,NC3(I))
          Z3(I)=XDP(3,NC3(I))
          X4(I)=XDP(1,NC4(I))
          Y4(I)=XDP(2,NC4(I))
          Z4(I)=XDP(3,NC4(I))
          X5(I)=XDP(1,NC5(I))
          Y5(I)=XDP(2,NC5(I))
          Z5(I)=XDP(3,NC5(I))
          X6(I)=XDP(1,NC6(I))
          Y6(I)=XDP(2,NC6(I))
          Z6(I)=XDP(3,NC6(I))
          X7(I)=XDP(1,NC7(I))
          Y7(I)=XDP(2,NC7(I))
          Z7(I)=XDP(3,NC7(I))
          X8(I)=XDP(1,NC8(I))
          Y8(I)=XDP(2,NC8(I))
          Z8(I)=XDP(3,NC8(I))
        ENDDO
#endif
C-----------
      RETURN
      END
