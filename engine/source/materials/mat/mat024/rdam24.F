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
Chd|  RDAM24                        source/materials/mat/mat024/rdam24.F
Chd|-- called by -----------
Chd|        ELAS24                        source/materials/mat/mat024/elas24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RDAM24(NEL   ,NBDAMA,DAMAI ,ANG   ,DEPS1 ,
     .                  DEPS2 ,DEPS3 ,DEPS4 ,DEPS5 ,DEPS6 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBDAMA,NEL,DAMAI(NEL)
      my_real, DIMENSION(NEL)   , INTENT(INOUT)  :: DEPS1,DEPS2,DEPS3,
     .                                              DEPS4,DEPS5,DEPS6
      my_real, DIMENSION(NEL,6) , INTENT(IN)  :: ANG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K
      my_real, DIMENSION(NEL) ::  S11,S12,S13,S21,S22,
     .   S23,S31,S32,S33,A11,A12,A13,A21,A22,A23,A31,A32,A33
C=======================================================================
      DO 5 K=1,NBDAMA
      I = DAMAI(K)
      S11(I)=ANG(I,1)
      S21(I)=ANG(I,2)
      S31(I)=ANG(I,3)
      S12(I)=ANG(I,4)
      S22(I)=ANG(I,5)
      S32(I)=ANG(I,6)
      S13(I)= S21(I)*S32(I)-S31(I)*S22(I)
      S23(I)= S31(I)*S12(I)-S11(I)*S32(I)
  5   S33(I)= S11(I)*S22(I)-S21(I)*S12(I)
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     TRANSFORME INCREMENTS DE DEFORMATION
C     DANS LE REPERE DES DIRECTIONS DE DOMMAGE
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      DO 200 K=1,NBDAMA
      I= DAMAI(K)
      DEPS4(I)=HALF*DEPS4(I)
      DEPS5(I)=HALF*DEPS5(I)
      DEPS6(I)=HALF*DEPS6(I)
      A11(I)=DEPS1(I)*S11(I)+DEPS4(I)*S21(I)+DEPS6(I)*S31(I)
      A12(I)=DEPS1(I)*S12(I)+DEPS4(I)*S22(I)+DEPS6(I)*S32(I)
      A13(I)=DEPS1(I)*S13(I)+DEPS4(I)*S23(I)+DEPS6(I)*S33(I)
      A21(I)=DEPS4(I)*S11(I)+DEPS2(I)*S21(I)+DEPS5(I)*S31(I)
      A22(I)=DEPS4(I)*S12(I)+DEPS2(I)*S22(I)+DEPS5(I)*S32(I)
      A23(I)=DEPS4(I)*S13(I)+DEPS2(I)*S23(I)+DEPS5(I)*S33(I)
      A31(I)=DEPS6(I)*S11(I)+DEPS5(I)*S21(I)+DEPS3(I)*S31(I)
      A32(I)=DEPS6(I)*S12(I)+DEPS5(I)*S22(I)+DEPS3(I)*S32(I)
      A33(I)=DEPS6(I)*S13(I)+DEPS5(I)*S23(I)+DEPS3(I)*S33(I)
 200  CONTINUE
      DO 201 K = 1,NBDAMA
      I = DAMAI(K)
      DEPS1(I) = S11(I)*A11(I)+S21(I)*A21(I)+S31(I)*A31(I)
      DEPS2(I) = S12(I)*A12(I)+S22(I)*A22(I)+S32(I)*A32(I)
      DEPS3(I) = S13(I)*A13(I)+S23(I)*A23(I)+S33(I)*A33(I)
      DEPS4(I) = S11(I)*A12(I)+S21(I)*A22(I)+S31(I)*A32(I)
      DEPS5(I) = S12(I)*A13(I)+S22(I)*A23(I)+S32(I)*A33(I)
      DEPS6(I) = S11(I)*A13(I)+S21(I)*A23(I)+S31(I)*A33(I)
      DEPS4(I) = TWO*DEPS4(I)
      DEPS5(I) = TWO*DEPS5(I)
 201  DEPS6(I) = TWO*DEPS6(I)
      RETURN
      END
