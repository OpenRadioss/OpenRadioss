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
Chd|  SRORTH3                       source/elements/solid/solide/srorth3.F
Chd|-- called by -----------
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SRORTH3(JHBE, GAMA,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, NEL)
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
      INTEGER JHBE,NEL
C     REAL
      my_real
     .   GAMA(NEL,6),
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   G11(MVSIZ),G12(MVSIZ),G13(MVSIZ),
     .   G21(MVSIZ),G22(MVSIZ),G23(MVSIZ),
     .   G31(MVSIZ),G32(MVSIZ),G33(MVSIZ)
      my_real
     .   XL(MVSIZ),YL(MVSIZ),ZL(MVSIZ)
C-----------------------------------------------
C     PASSAGE AU REPERE ORTHOTROPE.
      IF (JHBE/=24 .AND. JHBE/=222 .AND. JHBE/=14)THEN
        DO I=LFT,LLT
C         Extration de G tq Xortho=Transpose(G) Xcvt
C                                 =Transpose(G) Transpose(R) Xglobal.
          G11(I)=GAMA(I,1)
          G21(I)=GAMA(I,2)
          G31(I)=GAMA(I,3)
          G12(I)=GAMA(I,4)
          G22(I)=GAMA(I,5)
          G32(I)=GAMA(I,6)
          G13(I)=G21(I)*G32(I)-G31(I)*G22(I)
          G23(I)=G31(I)*G12(I)-G11(I)*G32(I)
          G33(I)=G11(I)*G22(I)-G21(I)*G12(I)
C
          XL(I)=G11(I)*X1(I)+G21(I)*Y1(I)+G31(I)*Z1(I)
          YL(I)=G12(I)*X1(I)+G22(I)*Y1(I)+G32(I)*Z1(I)
          ZL(I)=G13(I)*X1(I)+G23(I)*Y1(I)+G33(I)*Z1(I)
          X1(I)=XL(I)
          Y1(I)=YL(I)
          Z1(I)=ZL(I)
          XL(I)=G11(I)*X2(I)+G21(I)*Y2(I)+G31(I)*Z2(I)
          YL(I)=G12(I)*X2(I)+G22(I)*Y2(I)+G32(I)*Z2(I)
          ZL(I)=G13(I)*X2(I)+G23(I)*Y2(I)+G33(I)*Z2(I)
          X2(I)=XL(I)
          Y2(I)=YL(I)
          Z2(I)=ZL(I)
          XL(I)=G11(I)*X3(I)+G21(I)*Y3(I)+G31(I)*Z3(I)
          YL(I)=G12(I)*X3(I)+G22(I)*Y3(I)+G32(I)*Z3(I)
          ZL(I)=G13(I)*X3(I)+G23(I)*Y3(I)+G33(I)*Z3(I)
          X3(I)=XL(I)
          Y3(I)=YL(I)
          Z3(I)=ZL(I)
          XL(I)=G11(I)*X4(I)+G21(I)*Y4(I)+G31(I)*Z4(I)
          YL(I)=G12(I)*X4(I)+G22(I)*Y4(I)+G32(I)*Z4(I)
          ZL(I)=G13(I)*X4(I)+G23(I)*Y4(I)+G33(I)*Z4(I)
          X4(I)=XL(I)
          Y4(I)=YL(I)
          Z4(I)=ZL(I)
          XL(I)=G11(I)*X5(I)+G21(I)*Y5(I)+G31(I)*Z5(I)
          YL(I)=G12(I)*X5(I)+G22(I)*Y5(I)+G32(I)*Z5(I)
          ZL(I)=G13(I)*X5(I)+G23(I)*Y5(I)+G33(I)*Z5(I)
          X5(I)=XL(I)
          Y5(I)=YL(I)
          Z5(I)=ZL(I)
          XL(I)=G11(I)*X6(I)+G21(I)*Y6(I)+G31(I)*Z6(I)
          YL(I)=G12(I)*X6(I)+G22(I)*Y6(I)+G32(I)*Z6(I)
          ZL(I)=G13(I)*X6(I)+G23(I)*Y6(I)+G33(I)*Z6(I)
          X6(I)=XL(I)
          Y6(I)=YL(I)
          Z6(I)=ZL(I)
          XL(I)=G11(I)*X7(I)+G21(I)*Y7(I)+G31(I)*Z7(I)
          YL(I)=G12(I)*X7(I)+G22(I)*Y7(I)+G32(I)*Z7(I)
          ZL(I)=G13(I)*X7(I)+G23(I)*Y7(I)+G33(I)*Z7(I)
          X7(I)=XL(I)
          Y7(I)=YL(I)
          Z7(I)=ZL(I)
          XL(I)=G11(I)*X8(I)+G21(I)*Y8(I)+G31(I)*Z8(I)
          YL(I)=G12(I)*X8(I)+G22(I)*Y8(I)+G32(I)*Z8(I)
          ZL(I)=G13(I)*X8(I)+G23(I)*Y8(I)+G33(I)*Z8(I)
          X8(I)=XL(I)
          Y8(I)=YL(I)
          Z8(I)=ZL(I)
C
C         MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
          R11(I)=R11(I)*G11(I)+R12(I)*G21(I)+R13(I)*G31(I)
          R12(I)=R11(I)*G12(I)+R12(I)*G22(I)+R13(I)*G32(I)
          R13(I)=R11(I)*G13(I)+R12(I)*G23(I)+R13(I)*G33(I)
          R21(I)=R21(I)*G11(I)+R22(I)*G21(I)+R23(I)*G31(I)
          R22(I)=R21(I)*G12(I)+R22(I)*G22(I)+R23(I)*G32(I)
          R23(I)=R21(I)*G13(I)+R22(I)*G23(I)+R23(I)*G33(I)
          R31(I)=R31(I)*G11(I)+R32(I)*G21(I)+R33(I)*G31(I)
          R32(I)=R31(I)*G12(I)+R32(I)*G22(I)+R33(I)*G32(I)
          R33(I)=R31(I)*G13(I)+R32(I)*G23(I)+R33(I)*G33(I)
       END DO
      END IF
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
