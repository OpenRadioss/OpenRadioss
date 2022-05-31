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
Chd|  I3CST3                        source/interfaces/inter3d/i3cst3.F
Chd|-- called by -----------
Chd|        I5KE3                         source/interfaces/inter3d/i5ke3.F
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|        UD_INT5                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I3CST3(X1    ,X2      ,X3    ,X4    ,XI ,
     1                  Y1    ,Y2      ,Y3    ,Y4    ,YI ,
     2                  Z1    ,Z2      ,Z3    ,Z4    ,ZI ,
     3                  XFACE ,N1      ,N2    ,N3    ,SSC,
     4                  TTC   ,X0      ,Y0    ,Z0    ,XX1,
     5                  XX2   ,XX3     ,XX4   ,YY1   ,YY2,
     6                  YY3   ,YY4     ,ZZ1   ,ZZ2   ,ZZ3,
     7                  ZZ4   ,XI1     ,XI2   ,XI3   ,XI4,
     8                  YI1   ,YI2     ,YI3   ,YI4   ,ZI1,
     9                  ZI2   ,ZI3     ,ZI4   ,XN1   ,XN2,
     1                  XN3   ,XN4     ,YN1   ,YN2   ,YN3,
     2                  YN4   ,ZN1     ,ZN2   ,ZN3   ,ZN4,
     3                  AREA)
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
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  XFACE
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  SSC,TTC,AREA
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::  N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XX1,XX2,XX3,XX4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: YY1,YY2,YY3,YY4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: ZZ1,ZZ2,ZZ3,ZZ4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI1,XI2,XI3,XI4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: YI1,YI2,YI3,YI4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: ZI1,ZI2,ZI3,ZI4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XN1,XN2,XN3,XN4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: YN1,YN2,YN3,YN4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: ZN1,ZN2,ZN3,ZN4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     A12(MVSIZ), A23(MVSIZ), A34(MVSIZ), A41(MVSIZ),
     .     B12(MVSIZ), B23(MVSIZ), B34(MVSIZ), B41(MVSIZ),
     .     AB1(MVSIZ), AB2(MVSIZ), AN     
C-----------------------------------------------
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (N1,BV(1,19)) ,(N2,BV(1,20)) ,(N3,BV(1,21))
!      EQUIVALENCE (SSC,BV(1,23)),(TTC,BV(1,24))
!      EQUIVALENCE (XFACE,BV(1,29))
!      EQUIVALENCE (X0,BV(1,31)) ,(Y0,BV(1,32)) ,(Z0,BV(1,33))
!      EQUIVALENCE (XX1,BV(1,34)),(YY1,BV(1,35)),(ZZ1,BV(1,36))
!      EQUIVALENCE (XX2,BV(1,37)),(YY2,BV(1,38)),(ZZ2,BV(1,39))
!      EQUIVALENCE (XX3,BV(1,40)),(YY3,BV(1,41)),(ZZ3,BV(1,42))
!      EQUIVALENCE (XX4,BV(1,43)),(YY4,BV(1,44)),(ZZ4,BV(1,45))
!      EQUIVALENCE (XI1,BV(1,46)),(YI1,BV(1,47)),(ZI1,BV(1,48))
!      EQUIVALENCE (XI2,BV(1,49)),(YI2,BV(1,50)),(ZI2,BV(1,51))
!      EQUIVALENCE (XI3,BV(1,52)),(YI3,BV(1,53)),(ZI3,BV(1,54))
!      EQUIVALENCE (XI4,BV(1,55)),(YI4,BV(1,56)),(ZI4,BV(1,57))
!      EQUIVALENCE (XN1,BV(1,58)),(YN1,BV(1,59)),(ZN1,BV(1,60))
!      EQUIVALENCE (XN2,BV(1,61)),(YN2,BV(1,62)),(ZN2,BV(1,63))
 !     EQUIVALENCE (XN3,BV(1,64)),(YN3,BV(1,65)),(ZN3,BV(1,66))
!      EQUIVALENCE (XN4,BV(1,67)),(YN4,BV(1,68)),(ZN4,BV(1,69))
!      EQUIVALENCE (AREA,BV(1,71))
C
!      EQUIVALENCE (A12,X0),(A23,Y0),(A34,Z0),(A41,XX1)
!      EQUIVALENCE (B12,YY1),(B23,ZZ1),(B34,XX2),(B41,YY2)
!      EQUIVALENCE (AB1,ZZ2),(AB2,XX3)
C=======================================================================
      DO 100 I=LFT,LLT
      X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
      Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
      Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
C
      XX1(I) = X1(I)-X0(I)
      XX2(I) = X2(I)-X0(I)
      XX3(I) = X3(I)-X0(I)
      XX4(I) = X4(I)-X0(I)
      YY1(I) = Y1(I)-Y0(I)
      YY2(I) = Y2(I)-Y0(I)
      YY3(I) = Y3(I)-Y0(I)
      YY4(I) = Y4(I)-Y0(I)
      ZZ1(I) = Z1(I)-Z0(I)
      ZZ2(I) = Z2(I)-Z0(I)
      ZZ3(I) = Z3(I)-Z0(I)
      ZZ4(I) = Z4(I)-Z0(I)
C
      XI1(I) = X1(I)-XI(I)
      XI2(I) = X2(I)-XI(I)
      XI3(I) = X3(I)-XI(I)
      XI4(I) = X4(I)-XI(I)
      YI1(I) = Y1(I)-YI(I)
      YI2(I) = Y2(I)-YI(I)
      YI3(I) = Y3(I)-YI(I)
      YI4(I) = Y4(I)-YI(I)
      ZI1(I) = Z1(I)-ZI(I)
      ZI2(I) = Z2(I)-ZI(I)
      ZI3(I) = Z3(I)-ZI(I)
      ZI4(I) = Z4(I)-ZI(I)
 100  CONTINUE
C
      DO 120 I=LFT,LLT
      XN1(I) = YY1(I)*ZZ2(I) - YY2(I)*ZZ1(I)
      YN1(I) = ZZ1(I)*XX2(I) - ZZ2(I)*XX1(I)
      ZN1(I) = XX1(I)*YY2(I) - XX2(I)*YY1(I)
      N1(I)=XN1(I)
      N2(I)=YN1(I)
      N3(I)=ZN1(I)
 120  CONTINUE
C
      DO 140 I=LFT,LLT
      XN2(I) = YY2(I)*ZZ3(I) - YY3(I)*ZZ2(I)
      YN2(I) = ZZ2(I)*XX3(I) - ZZ3(I)*XX2(I)
      ZN2(I) = XX2(I)*YY3(I) - XX3(I)*YY2(I)
      N1(I)=N1(I)+XN2(I)
      N2(I)=N2(I)+YN2(I)
      N3(I)=N3(I)+ZN2(I)
 140  CONTINUE
C
      DO 160 I=LFT,LLT
      XN3(I) = YY3(I)*ZZ4(I) - YY4(I)*ZZ3(I)
      YN3(I) = ZZ3(I)*XX4(I) - ZZ4(I)*XX3(I)
      ZN3(I) = XX3(I)*YY4(I) - XX4(I)*YY3(I)
      N1(I)=N1(I)+XN3(I)
      N2(I)=N2(I)+YN3(I)
      N3(I)=N3(I)+ZN3(I)
 160  CONTINUE
C
      DO 180 I=LFT,LLT
      XN4(I) = YY4(I)*ZZ1(I) - YY1(I)*ZZ4(I)
      YN4(I) = ZZ4(I)*XX1(I) - ZZ1(I)*XX4(I)
      ZN4(I) = XX4(I)*YY1(I) - XX1(I)*YY4(I)
      N1(I)=N1(I)+XN4(I)
      N2(I)=N2(I)+YN4(I)
      N3(I)=N3(I)+ZN4(I)
 180  CONTINUE
C
      DO 200 I=LFT,LLT
      AN= MAX(EM20,SQRT(N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I)))
      N1(I)=N1(I)/AN
      N2(I)=N2(I)/AN
      N3(I)=N3(I)/AN
      AREA(I)=HALF*AN
 200  CONTINUE
C
      DO 210 I=LFT,LLT
      X0(I)=(N1(I)*XN1(I)+N2(I)*YN1(I)+N3(I)*ZN1(I))
      Y0(I)=(N1(I)*XN2(I)+N2(I)*YN2(I)+N3(I)*ZN2(I))
      Z0(I)=(N1(I)*XN3(I)+N2(I)*YN3(I)+N3(I)*ZN3(I))
      XX1(I)=(N1(I)*XN4(I)+N2(I)*YN4(I)+N3(I)*ZN4(I))
 210  CONTINUE
C
      DO 220 I=LFT,LLT
      XN1(I) = YI1(I)*ZI2(I) - YI2(I)*ZI1(I)
      YN1(I) = ZI1(I)*XI2(I) - ZI2(I)*XI1(I)
      ZN1(I) = XI1(I)*YI2(I) - XI2(I)*YI1(I)
      YY1(I)=(N1(I)*XN1(I)+N2(I)*YN1(I)+N3(I)*ZN1(I))
 220  CONTINUE
C
      DO 240 I=LFT,LLT
      XN2(I) = YI2(I)*ZI3(I) - YI3(I)*ZI2(I)
      YN2(I) = ZI2(I)*XI3(I) - ZI3(I)*XI2(I)
      ZN2(I) = XI2(I)*YI3(I) - XI3(I)*YI2(I)
      ZZ1(I)=(N1(I)*XN2(I)+N2(I)*YN2(I)+N3(I)*ZN2(I))
 240  CONTINUE
C
      DO 260 I=LFT,LLT
      XN3(I) = YI3(I)*ZI4(I) - YI4(I)*ZI3(I)
      YN3(I) = ZI3(I)*XI4(I) - ZI4(I)*XI3(I)
      ZN3(I) = XI3(I)*YI4(I) - XI4(I)*YI3(I)
      XX2(I)=(N1(I)*XN3(I)+N2(I)*YN3(I)+N3(I)*ZN3(I))
 260  CONTINUE
C
      DO 280 I=LFT,LLT
      XN4(I) = YI4(I)*ZI1(I) - YI1(I)*ZI4(I)
      YN4(I) = ZI4(I)*XI1(I) - ZI1(I)*XI4(I)
      ZN4(I) = XI4(I)*YI1(I) - XI1(I)*YI4(I)
      YY2(I)=(N1(I)*XN4(I)+N2(I)*YN4(I)+N3(I)*ZN4(I))
 280  CONTINUE
C
      DO 300 I=LFT,LLT
      ZZ2(I)=Y0(I)*YY2(I)
      XX3(I)=ZZ1(I)*XX1(I)
 300  CONTINUE
C
      DO 320 I=LFT,LLT
      IF(XFACE(I).EQ.ZERO)GOTO 320
      IF(ZZ2(I)+XX3(I).NE.ZERO)THEN
       SSC(I)=(ZZ2(I)-XX3(I))/(ZZ2(I)+XX3(I))
      ELSE
       SSC(I)=ZERO
      ENDIF
      IF(Z0(I).NE.ZERO)THEN
       ZZ2(I)=YY1(I)*Z0(I)
       XX3(I)=XX2(I)*X0(I)
       IF(ZZ2(I)+XX3(I).NE.ZERO)THEN
        TTC(I)=(ZZ2(I)-XX3(I))/(ZZ2(I)+XX3(I))
       ELSE
        TTC(I)=ZERO
       ENDIF
      ELSE
       TTC(I)=(YY1(I)-X0(I))/X0(I)
      ENDIF
 320  CONTINUE
C
      RETURN
      END
