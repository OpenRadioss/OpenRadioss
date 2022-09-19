Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  GLOA24                        source/materials/mat/mat024/gloa24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GLOA24(NEL,STRAIN,DT1,ARM1,ARM2,ARM3,
     .            D1,D2,D3,D4,D5,D6,
     .            R11,R12,R13,R21,R22,
     .            R23,R31,R32,R33,
     .            DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real DT1
      my_real, DIMENSION(NEL,6), INTENT(IN)  :: STRAIN
      my_real, DIMENSION(NEL)  , INTENT(IN)  :: D1,D2,D3,D4,D5,D6,
     .  R11,R12,R13,R21,R22,R23,R31,R32,R33
      my_real, DIMENSION(NEL)  , INTENT(INOUT) :: ARM1,ARM2,ARM3
      my_real, DIMENSION(NEL)  , INTENT(OUT) ::
     .  DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real, DIMENSION(NEL) :: H1, H2, H3,H4, H5, H6,
     .   A11, A12, A13, A21, A22, A23, A31,A32, A33
C=======================================================================
      DO 200 I=1,NEL
      DEPS1(I)=D1(I)*DT1
      DEPS2(I)=D2(I)*DT1
      DEPS3(I)=D3(I)*DT1
      DEPS4(I)=HALF*D4(I)*DT1
      DEPS5(I)=HALF*D5(I)*DT1
      DEPS6(I)=HALF*D6(I)*DT1
      A11(I)=DEPS1(I)*R11(I)+DEPS4(I)*R21(I)+DEPS6(I)*R31(I)
      A12(I)=DEPS1(I)*R12(I)+DEPS4(I)*R22(I)+DEPS6(I)*R32(I)
      A13(I)=DEPS1(I)*R13(I)+DEPS4(I)*R23(I)+DEPS6(I)*R33(I)
      A21(I)=DEPS4(I)*R11(I)+DEPS2(I)*R21(I)+DEPS5(I)*R31(I)
      A22(I)=DEPS4(I)*R12(I)+DEPS2(I)*R22(I)+DEPS5(I)*R32(I)
      A23(I)=DEPS4(I)*R13(I)+DEPS2(I)*R23(I)+DEPS5(I)*R33(I)
      A31(I)=DEPS6(I)*R11(I)+DEPS5(I)*R21(I)+DEPS3(I)*R31(I)
      A32(I)=DEPS6(I)*R12(I)+DEPS5(I)*R22(I)+DEPS3(I)*R32(I)
      A33(I)=DEPS6(I)*R13(I)+DEPS5(I)*R23(I)+DEPS3(I)*R33(I)
 200  CONTINUE
      DO 201 I=1,NEL
      DEPS1(I)=R11(I)*A11(I)+R21(I)*A21(I)+R31(I)*A31(I)
      DEPS2(I)=R12(I)*A12(I)+R22(I)*A22(I)+R32(I)*A32(I)
      DEPS3(I)=R13(I)*A13(I)+R23(I)*A23(I)+R33(I)*A33(I)
      DEPS4(I)=R11(I)*A12(I)+R21(I)*A22(I)+R31(I)*A32(I)
      DEPS5(I)=R12(I)*A13(I)+R22(I)*A23(I)+R32(I)*A33(I)
      DEPS6(I)=R11(I)*A13(I)+R21(I)*A23(I)+R31(I)*A33(I)
      DEPS4(I)=TWO*DEPS4(I)
      DEPS5(I)=TWO*DEPS5(I)
 201  DEPS6(I)=TWO*DEPS6(I)
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      DO 500 I=1,NEL
      H1(I)=STRAIN(I,1)
      H2(I)=STRAIN(I,2)
      H3(I)=STRAIN(I,3)
      H4(I)=HALF*STRAIN(I,4)
      H5(I)=HALF*STRAIN(I,5)
      H6(I)=HALF*STRAIN(I,6)
      A11(I)=H1(I)*R11(I)+H4(I)*R21(I)+H6(I)*R31(I)
      A12(I)=H1(I)*R12(I)+H4(I)*R22(I)+H6(I)*R32(I)
      A13(I)=H1(I)*R13(I)+H4(I)*R23(I)+H6(I)*R33(I)
      A21(I)=H4(I)*R11(I)+H2(I)*R21(I)+H5(I)*R31(I)
      A22(I)=H4(I)*R12(I)+H2(I)*R22(I)+H5(I)*R32(I)
      A23(I)=H4(I)*R13(I)+H2(I)*R23(I)+H5(I)*R33(I)
      A31(I)=H6(I)*R11(I)+H5(I)*R21(I)+H3(I)*R31(I)
      A32(I)=H6(I)*R12(I)+H5(I)*R22(I)+H3(I)*R32(I)
      A33(I)=H6(I)*R13(I)+H5(I)*R23(I)+H3(I)*R33(I)
 500  CONTINUE
      DO 510 I=1,NEL
      H1(I)=R11(I)*A11(I)+R21(I)*A21(I)+R31(I)*A31(I)
      H2(I)=R12(I)*A12(I)+R22(I)*A22(I)+R32(I)*A32(I)
      H3(I)=R13(I)*A13(I)+R23(I)*A23(I)+R33(I)*A33(I)
      ARM1(I)=ARM1(I) * MAX(ZERO,1.-H2(I)-H3(I))
      ARM2(I)=ARM2(I) * MAX(ZERO,1.-H1(I)-H3(I))
      ARM3(I)=ARM3(I) * MAX(ZERO,1.-H1(I)-H2(I))
 510  CONTINUE
C
      RETURN
      END
