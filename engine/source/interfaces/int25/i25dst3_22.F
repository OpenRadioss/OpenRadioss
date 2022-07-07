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
Chd|  I25DST3_22                    source/interfaces/int25/i25dst3_22.F
Chd|-- called by -----------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|-- calls ---------------
Chd|        INTERSECA_25                  source/interfaces/int25/i25dst3_22.F
Chd|        INTERSECB_25                  source/interfaces/int25/i25dst3_22.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25DST3_22(
     1                  JLT    ,CAND_N ,CAND_E ,ISHEL  ,
     2                  XX     ,YY     ,ZZ     ,
     3                  XI     ,YI     ,ZI     ,
     4                  VX1    ,VX2    ,VX3    ,VX4    ,VXI    ,
     5                  VY1    ,VY2    ,VY3    ,VY4    ,VYI    ,
     6                  VZ1    ,VZ2    ,VZ3    ,VZ4    ,VZI    ,
     7                  NIN    ,NSN    ,IX1    ,
     9                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
     A                  INACTI ,MSEGLO ,GAPS   ,GAPM   ,
     B                  IRECT  ,IRTLM  ,TIME_S ,GAP_NM ,
     C                  PENE_OLD,STIF_OLD,ITAB ,
     D                  PENMIN,EPS0    ,ICONT_I,MARGE  ,
     E                  NAX     ,NAY   ,NAZ     ,
     E                  NBX     ,NBY   ,NBZ     ,
     J                  FAR     ,PENT  ,
     L                  SUBTRIA ,LBS   ,LCS     ,LBP    ,LCP   ,
     P                  MVOISA  ,MVOISB,GAPMXL  ,IBOUNDA,IBOUNDB,
     Q                  VTX_BISECTOR,DRAD,DGAPLOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NSN, INACTI,
     .        CAND_N(*),CAND_E(*),NSVG(MVSIZ), ISHEL(MVSIZ)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INTTH,MSEGLO(*),IRTLM(4,NSN), SUBTRIA(MVSIZ),
     .        MVOISA(MVSIZ,4), MVOISB(MVSIZ,4),IBOUNDA(4,MVSIZ),IBOUNDB(4,MVSIZ)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .     TIME_S(2,NSN), GAPS(*), GAPM(*), 
     .     PENE_OLD(5,*),STIF_OLD(2,NSN), PENMIN, EPS0, MARGE
      my_real
     .     XX(MVSIZ,5),YY(MVSIZ,5),ZZ(MVSIZ,5), 
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),VY1(MVSIZ),
     .     VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),VZ1(MVSIZ),VZ2(MVSIZ),
     .     VZ3(MVSIZ),VZ4(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),
     .     NAX(MVSIZ,5), NAY(MVSIZ,5), NAZ(MVSIZ,5),
     .     NBX(MVSIZ,5), NBY(MVSIZ,5), NBZ(MVSIZ,5),
     .     PENT(MVSIZ), 
     .     LBS(MVSIZ), LCS(MVSIZ), LBP(MVSIZ,4), LCP(MVSIZ,4), 
     .     GAP_NM(4,MVSIZ), GAPMXL(MVSIZ)
      INTEGER IRECT(4,*),ITAB(*),ICONT_I(*), FAR(MVSIZ)
      REAL*4 VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, IT, I1, I2, ITRIA(2,4),
     .        N4N, MGLOB, ITPERM(4), IA, IB, IB1, IB2, IB3, IBX, IX, IY, IZ
      INTEGER I4N(MVSIZ), INTERSECTA(MVSIZ), INTERSECTB(MVSIZ), 
     .        INTERSECA, INTERSECB, 
     .        FARA(MVSIZ,4), FARB(MVSIZ,4), INGAPA(MVSIZ,4), INGAPB(MVSIZ,4),  
     .        SUBTRIB(MVSIZ), SUBTRIX(MVSIZ)
      my_real
     .     XIJ(MVSIZ,4),  XI0V(MVSIZ),  XI5, XOI5,
     .     YIJ(MVSIZ,4),  YI0V(MVSIZ),  YI5, YOI5,
     .     ZIJ(MVSIZ,4),  ZI0V(MVSIZ),  ZI5, ZOI5,
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XO1, XO2, XO3, XO4, XO5, XOI,
     .     YO1, YO2, YO3, YO4, YO5, YOI,
     .     ZO1, ZO2, ZO3, ZO4, ZO5, ZOI,
     .     VO12, VO23, VO34, VO41, PENE, PENAX, PENBX
      my_real
     .     GAP_MM(MVSIZ), GAP, GAP21,GAP22,GAP23,GAP24,
     .     XP, YP, ZP,
     .     DX, DY, DZ, LX, LD, LAX, LBX, LCX, DMIN, PMIN,
     .     DD(MVSIZ,4), 
     .     LAP, HLA, HLB(MVSIZ,4), HLC(MVSIZ,4),
     .     LAP1,LAP2,LAP3,LAP4,
     .     AL(MVSIZ,4)
      my_real
     .     UNP,ZEROM,EPS,UNPT,ZEROMT,AAA,
     .     PENA(MVSIZ,4), PENB(MVSIZ,4), BB(MVSIZ,4), 
     .     SX1,SX2,SX3,SX4,SY1,SY2,SY3,SY4,SZ1,SZ2,SZ3,SZ4,
     .     X0N(MVSIZ,4), Y0N(MVSIZ,4), Z0N(MVSIZ,4), 
     .     XN(MVSIZ,4),YN(MVSIZ,4),ZN(MVSIZ,4),HH,
     .     LB(MVSIZ,4), LC(MVSIZ,4), 
     .     SIDA(MVSIZ,4), SIDB(MVSIZ,4), 
     .     X12, Y12, Z12, 
     .     PX, PY, PZ, PP, P1, P2, XH, YH, ZH, D1, D2, D3, VX, VY, VZ, 
     .     DIST(MVSIZ),LL,NN,PN,
     .     V12,V23,V34,V41,
     .     NX1, NX2, NX3, NX4,
     .     NY1, NY2, NY3, NY4,
     .     NZ1, NZ2, NZ3, NZ4,
     .     SOX125,SOX235,SOX345,SOX415,
     .     SOY125,SOY235,SOY345,SOY415,
     .     SOZ125,SOZ235,SOZ345,SOZ415,
     .     N1X,N1Y,N1Z,N1N,
     .     N2X,N2Y,N2Z,N2N
      DATA ITRIA/1,2,2,3,3,4,4,1/,
     .     ITPERM/1,4,3,2/
      LOGICAL :: ANY_TRIANGLE
C-----------------------------------------------------------------------
C
      UNP   = ONE + EM4
      ZEROM = ZERO - EM4
      EPS   = (TWO+HALF)/HUNDRED
      UNPT   = ONE + EPS
      ZEROMT = ZERO - EPS
C
C initialisation (cf triangles)
      FARA(1:JLT,1:4) = 0
      FARB(1:JLT,1:4) = 0
      PENA(1:JLT,1:4) = ZERO
      PENB(1:JLT,1:4) = ZERO
      DD  (1:JLT,1:4) = ZERO
C
      ANY_TRIANGLE = .FALSE. 
      DO I=1,JLT
       IF(IX3(I)==IX4(I)) THEN
         ANY_TRIANGLE = .TRUE.
       ENDIF
      ENDDO 

      DO I=1,JLT
C
C       For computing LBP, LCP, FAR=2, etc
C
C       IF(STIF(I) == ZERO)CYCLE
C    
        X0N(I,1) = XX(I,1) - XX(I,5)
        Y0N(I,1) = YY(I,1) - YY(I,5)
        Z0N(I,1) = ZZ(I,1) - ZZ(I,5)
C
        X0N(I,2) = XX(I,2) - XX(I,5)
        Y0N(I,2) = YY(I,2) - YY(I,5)
        Z0N(I,2) = ZZ(I,2) - ZZ(I,5)
C
        X0N(I,3) = XX(I,3) - XX(I,5)
        Y0N(I,3) = YY(I,3) - YY(I,5)
        Z0N(I,3) = ZZ(I,3) - ZZ(I,5)
C
        X0N(I,4) = XX(I,4) - XX(I,5)
        Y0N(I,4) = YY(I,4) - YY(I,5)
        Z0N(I,4) = ZZ(I,4) - ZZ(I,5)
C
        GAP_MM(I)=FOURTH*(GAP_NM(1,I)+GAP_NM(2,I)+GAP_NM(3,I)+GAP_NM(4,I))
      ENDDO
      
      IF (ANY_TRIANGLE) THEN
        DO I=1,JLT
          IF(IX3(I)==IX4(I)) THEN
            GAP_MM(I)=GAP_NM(3,I)
          END IF
        ENDDO
      ENDIF

C--------------------------------------------------------
C
C--------------------------------------------------------
C
C normales aux triangles (recalculees ici pour pas les stocker).
      DO I=1,JLT
C
C      IF(STIF(I) == ZERO)CYCLE
C    
       XN(I,1) = Y0N(I,1)*Z0N(I,2) - Z0N(I,1)*Y0N(I,2)
       YN(I,1) = Z0N(I,1)*X0N(I,2) - X0N(I,1)*Z0N(I,2)
       ZN(I,1) = X0N(I,1)*Y0N(I,2) - Y0N(I,1)*X0N(I,2)

       XN(I,2) = Y0N(I,2)*Z0N(I,3) - Z0N(I,2)*Y0N(I,3)
       YN(I,2) = Z0N(I,2)*X0N(I,3) - X0N(I,2)*Z0N(I,3)
       ZN(I,2) = X0N(I,2)*Y0N(I,3) - Y0N(I,2)*X0N(I,3)

       XN(I,3) = Y0N(I,3)*Z0N(I,4) - Z0N(I,3)*Y0N(I,4)
       YN(I,3) = Z0N(I,3)*X0N(I,4) - X0N(I,3)*Z0N(I,4)
       ZN(I,3) = X0N(I,3)*Y0N(I,4) - Y0N(I,3)*X0N(I,4)

       XN(I,4) = Y0N(I,4)*Z0N(I,1) - Z0N(I,4)*Y0N(I,1)
       YN(I,4) = Z0N(I,4)*X0N(I,1) - X0N(I,4)*Z0N(I,1)
       ZN(I,4) = X0N(I,4)*Y0N(I,1) - Y0N(I,4)*X0N(I,1)

      ENDDO
C
C--------------------------------------------------------
C      Search intersection
C--------------------------------------------------------
C
       INTERSECTA(1:JLT) = 0
       INTERSECTB(1:JLT) = 0

       INGAPA(1:JLT,1:4) = 0
       INGAPB(1:JLT,1:4) = 0
C
       DO I=1,JLT
C   attention triangles 
  
         XI5 = XX(I,5) - XI(I)
         YI5 = YY(I,5) - YI(I)
         ZI5 = ZZ(I,5) - ZI(I)

         V12  = XN(I,1)*XI5 + YN(I,1)*YI5 + ZN(I,1)*ZI5 
         V23  = XN(I,2)*XI5 + YN(I,2)*YI5 + ZN(I,2)*ZI5
         V34  = XN(I,3)*XI5 + YN(I,3)*YI5 + ZN(I,3)*ZI5 
         V41  = XN(I,4)*XI5 + YN(I,4)*YI5 + ZN(I,4)*ZI5      
         
         IF(ISHEL(I)==0)THEN
           IF(V12 < ZERO .and. V23 < ZERO .and. 
     .        V34 < ZERO .and. V41 < ZERO ) THEN
C            INTERSECTION IMPOSSIBLE
             CYCLE
           ENDIF
         END IF

C        POSSIBLE INTERSECTION

C--------------------------------------------------------
C  OLD COORDINATES
C--------------------------------------------------------
         XO1 = XX(I,1) - DT1*VX1(I)    
         YO1 = YY(I,1) - DT1*VY1(I)    
         ZO1 = ZZ(I,1) - DT1*VZ1(I) 
C           
         XO2 = XX(I,2) - DT1*VX2(I)     
         YO2 = YY(I,2) - DT1*VY2(I)     
         ZO2 = ZZ(I,2) - DT1*VZ2(I)     
c         
         XO3 = XX(I,3) - DT1*VX3(I)   
         YO3 = YY(I,3) - DT1*VY3(I)   
         ZO3 = ZZ(I,3) - DT1*VZ3(I)
C        
         XO4 = XX(I,4) - DT1*VX4(I) 
         YO4 = YY(I,4) - DT1*VY4(I) 
         ZO4 = ZZ(I,4) - DT1*VZ4(I)

         XOI = XI(I) - DT1*VXI(I)
         YOI = YI(I) - DT1*VYI(I)
         ZOI = ZI(I) - DT1*VZI(I) 

         IF(IX3(I) /= IX4(I))THEN
           XO5 = FOURTH*(XO1+XO2+XO3+XO4)
           YO5 = FOURTH*(YO1+YO2+YO3+YO4)
           ZO5 = FOURTH*(ZO1+ZO2+ZO3+ZO4) 
         ELSE
           XO5 = XO3
           YO5 = YO3
           ZO5 = ZO3
         ENDIF

c        compute volumes at previous time step

         X51 = XO1 - XO5
         Y51 = YO1 - YO5
         Z51 = ZO1 - ZO5
 
         X52 = XO2 - XO5
         Y52 = YO2 - YO5
         Z52 = ZO2 - ZO5
 
         X53 = XO3 - XO5
         Y53 = YO3 - YO5
         Z53 = ZO3 - ZO5
 
         X54 = XO4 - XO5
         Y54 = YO4 - YO5
         Z54 = ZO4 - ZO5
 
         XOI5 = XO5 - XOI
         YOI5 = YO5 - YOI
         ZOI5 = ZO5 - ZOI

         SOX125 = Y51*Z52 - Z51*Y52
         SOY125 = Z51*X52 - X51*Z52
         SOZ125 = X51*Y52 - Y51*X52
         VO12  = SOX125*XOI5 + SOY125*YOI5 + SOZ125*ZOI5

         SOX235 = Y52*Z53 - Z52*Y53
         SOY235 = Z52*X53 - X52*Z53
         SOZ235 = X52*Y53 - Y52*X53
         VO23  = SOX235*XOI5 + SOY235*YOI5 + SOZ235*ZOI5

         SOX345 = Y53*Z54 - Z53*Y54
         SOY345 = Z53*X54 - X53*Z54
         SOZ345 = X53*Y54 - Y53*X54
         VO34  = SOX345*XOI5 + SOY345*YOI5 + SOZ345*ZOI5

         SOX415 = Y54*Z51 - Z54*Y51
         SOY415 = Z54*X51 - X54*Z51
         SOZ415 = X54*Y51 - Y54*X51
         VO41  = SOX415*XOI5 + SOY415*YOI5 + SOZ415*ZOI5

c        compute intersection time (linear approximation)
c        compute coordinates at intersection time
c        (intersection time can be different for each sub-triangle)
         IF(ISHEL(I)==0)THEN
           CALL INTERSECA_25(
     1                      IX3(I)      ,IX4(I),INTERSECA,
     1                      V12     ,V23    ,V34    ,V41    ,
     2                      VO12    ,VO23   ,VO34   ,VO41   ,XX(I,1),
     3                      YY(I,1) ,ZZ(I,1),XX(I,2),YY(I,2),ZZ(I,2),
     4                      XX(I,3) ,YY(I,3),ZZ(I,3),XX(I,4),YY(I,4),
     5                      ZZ(I,4) ,XX(I,5),YY(I,5),ZZ(I,5),
     6                      VXI(I)  ,VYI(I) ,VZI(I) ,XO1   ,XO2    ,
     7                      XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                      YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                      ZO3     ,ZO4    ,ZO5    ,XI(I) ,YI(I)  ,
     A                      ZI(I)   ,XOI    ,YOI    ,ZOI   ,ZEROM  ,
     B                      UNP    ,ZEROMT,UNPT   )
           INTERSECTA(I)=INTERSECA
         ELSE
           CALL INTERSECB_25(
     1                      IX3(I)      ,IX4(I),INTERSECA,INTERSECB,
     1                      V12     ,V23    ,V34    ,V41   ,
     2                      VO12    ,VO23   ,VO34   ,VO41  ,XX(I,1),
     3                      YY(I,1) ,ZZ(I,1),XX(I,2),YY(I,2),ZZ(I,2),
     4                      XX(I,3) ,YY(I,3),ZZ(I,3),XX(I,4),YY(I,4),
     5                      ZZ(I,4) ,XX(I,5),YY(I,5),ZZ(I,5),
     6                      VXI(I)  ,VYI(I) ,VZI(I) ,XO1   ,XO2    ,
     7                      XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                      YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                      ZO3     ,ZO4    ,ZO5    ,XI(I) ,YI(I)  ,
     A                      ZI(I)   ,XOI    ,YOI    ,ZOI   ,ZEROM  ,
     B                      UNP    ,ZEROMT,UNPT   )
           INTERSECTA(I)=INTERSECA
           INTERSECTB(I)=INTERSECB
         END IF
C
      ENDDO
C--------------------------------------------------------
C#include  "vectorize.inc"
      DO I=1,JLT
C
C       IF(STIF(I) == ZERO)CYCLE
C
        XI0V(I) = XX(I,5) - XI(I)
        YI0V(I) = YY(I,5) - YI(I)
        ZI0V(I) = ZZ(I,5) - ZI(I)
C
        XIJ(I,1) = XX(I,1) - XI(I)
        YIJ(I,1) = YY(I,1) - YI(I)
        ZIJ(I,1) = ZZ(I,1) - ZI(I)
C
        XIJ(I,2) = XX(I,2) - XI(I)
        YIJ(I,2) = YY(I,2) - YI(I)
        ZIJ(I,2) = ZZ(I,2) - ZI(I)
C
        XIJ(I,3) = XX(I,3) - XI(I)
        YIJ(I,3) = YY(I,3) - YI(I)
        ZIJ(I,3) = ZZ(I,3) - ZI(I)
C
        XIJ(I,4) = XX(I,4) - XI(I)
        YIJ(I,4) = YY(I,4) - YI(I)
        ZIJ(I,4) = ZZ(I,4) - ZI(I)
C
        SX1 = YI0V(I)*ZIJ(I,1) - ZI0V(I)*YIJ(I,1)
        SY1 = ZI0V(I)*XIJ(I,1) - XI0V(I)*ZIJ(I,1)
        SZ1 = XI0V(I)*YIJ(I,1) - YI0V(I)*XIJ(I,1)
C
        SX2 = YI0V(I)*ZIJ(I,2) - ZI0V(I)*YIJ(I,2)
        SY2 = ZI0V(I)*XIJ(I,2) - XI0V(I)*ZIJ(I,2)
        SZ2 = XI0V(I)*YIJ(I,2) - YI0V(I)*XIJ(I,2)
C
        SX3 = YI0V(I)*ZIJ(I,3) - ZI0V(I)*YIJ(I,3)
        SY3 = ZI0V(I)*XIJ(I,3) - XI0V(I)*ZIJ(I,3)
        SZ3 = XI0V(I)*YIJ(I,3) - YI0V(I)*XIJ(I,3)
C
        SX4 = YI0V(I)*ZIJ(I,4) - ZI0V(I)*YIJ(I,4)
        SY4 = ZI0V(I)*XIJ(I,4) - XI0V(I)*ZIJ(I,4)
        SZ4 = XI0V(I)*YIJ(I,4) - YI0V(I)*XIJ(I,4)
C
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,1)*XN(I,1)+ YN(I,1)*YN(I,1)+ ZN(I,1)*ZN(I,1)))
        XN(I,1)=XN(I,1)*NN
        YN(I,1)=YN(I,1)*NN
        ZN(I,1)=ZN(I,1)*NN
        LB(I,1) = -(XN(I,1)*SX2 + YN(I,1)*SY2 + ZN(I,1)*SZ2)*NN
        LC(I,1) =  (XN(I,1)*SX1 + YN(I,1)*SY1 + ZN(I,1)*SZ1)*NN
C
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,2)*XN(I,2)+ YN(I,2)*YN(I,2)+ ZN(I,2)*ZN(I,2)))
        XN(I,2)=XN(I,2)*NN
        YN(I,2)=YN(I,2)*NN
        ZN(I,2)=ZN(I,2)*NN
        LB(I,2) = -(XN(I,2)*SX3 + YN(I,2)*SY3 + ZN(I,2)*SZ3)*NN
        LC(I,2) =  (XN(I,2)*SX2 + YN(I,2)*SY2 + ZN(I,2)*SZ2)*NN
C
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,3)*XN(I,3)+ YN(I,3)*YN(I,3)+ ZN(I,3)*ZN(I,3)))
        XN(I,3)=XN(I,3)*NN
        YN(I,3)=YN(I,3)*NN
        ZN(I,3)=ZN(I,3)*NN
        LB(I,3) = -(XN(I,3)*SX4 + YN(I,3)*SY4 + ZN(I,3)*SZ4)*NN
        LC(I,3) =  (XN(I,3)*SX3 + YN(I,3)*SY3 + ZN(I,3)*SZ3)*NN
C
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,4)*XN(I,4)+ YN(I,4)*YN(I,4)+ ZN(I,4)*ZN(I,4)))
        XN(I,4)=XN(I,4)*NN
        YN(I,4)=YN(I,4)*NN
        ZN(I,4)=ZN(I,4)*NN
        LB(I,4) = -(XN(I,4)*SX1 + YN(I,4)*SY1 + ZN(I,4)*SZ1)*NN
        LC(I,4) =  (XN(I,4)*SX4 + YN(I,4)*SY4 + ZN(I,4)*SZ4)*NN
C
      END DO
C--------------------------------------------------------
C
C#include  "vectorize.inc"
      DO I=1,JLT
C
C       IF(STIF(I) == ZERO)CYCLE
C
        AAA      = ONE/MAX(EM30,X0N(I,1)*X0N(I,1)+Y0N(I,1)*Y0N(I,1)+Z0N(I,1)*Z0N(I,1))
        HLC(I,1) = LC(I,1)*ABS(LC(I,1))*AAA
        HLB(I,4) = LB(I,4)*ABS(LB(I,4))*AAA
        AL(I,1)  = -(XI0V(I)*X0N(I,1)+YI0V(I)*Y0N(I,1)+ZI0V(I)*Z0N(I,1))*AAA
        AL(I,1)  = MAX(ZERO,MIN(ONE,AL(I,1)))
        AAA      = ONE/MAX(EM30,X0N(I,2)*X0N(I,2)+Y0N(I,2)*Y0N(I,2)+Z0N(I,2)*Z0N(I,2))
        HLC(I,2) = LC(I,2)*ABS(LC(I,2))*AAA
        HLB(I,1) = LB(I,1)*ABS(LB(I,1))*AAA
        AL(I,2)  = -(XI0V(I)*X0N(I,2)+YI0V(I)*Y0N(I,2)+ZI0V(I)*Z0N(I,2))*AAA
        AL(I,2)  = MAX(ZERO,MIN(ONE,AL(I,2)))
        AAA      = ONE/MAX(EM30,X0N(I,3)*X0N(I,3)+Y0N(I,3)*Y0N(I,3)+Z0N(I,3)*Z0N(I,3))
        HLC(I,3) = LC(I,3)*ABS(LC(I,3))*AAA
        HLB(I,2) = LB(I,2)*ABS(LB(I,2))*AAA
        AL(I,3)  = -(XI0V(I)*X0N(I,3)+YI0V(I)*Y0N(I,3)+ZI0V(I)*Z0N(I,3))*AAA
        AL(I,3)  = MAX(ZERO,MIN(ONE,AL(I,3)))
        AAA      = ONE/MAX(EM30,X0N(I,4)*X0N(I,4)+Y0N(I,4)*Y0N(I,4)+Z0N(I,4)*Z0N(I,4))
        HLC(I,4) = LC(I,4)*ABS(LC(I,4))*AAA
        HLB(I,3) = LB(I,3)*ABS(LB(I,3))*AAA
        AL(I,4)  = -(XI0V(I)*X0N(I,4)+YI0V(I)*Y0N(I,4)+ZI0V(I)*Z0N(I,4))*AAA
        AL(I,4)  = MAX(ZERO,MIN(ONE,AL(I,4)))
C
      END DO
C--------------------------------------------------------
      DO I=1,JLT
C       Projection en dehors du triangle
        IF(LB(I,1) < -EM03 .OR. LC(I,1) < -EM03 .OR.
     .      LB(I,1)+LC(I,1) > ONE+EM03) THEN
           FARA(I,1)=1
           FARB(I,1)=1
        END IF
        X12 = XX(I,2) - XX(I,1)
        Y12 = YY(I,2) - YY(I,1)
        Z12 = ZZ(I,2) - ZZ(I,1)
        LBP(I,1) = LB(I,1)
        LCP(I,1) = LC(I,1)
        LAP       = ONE-LBP(I,1)-LCP(I,1)
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LAP*ABS(LAP) * AAA
        IF(LAP.LT.ZERO.AND.
     +     HLA.LE.HLB(I,1).AND.HLA.LE.HLC(I,1))THEN
         LBP(I,1) = (XIJ(I,2)*X12+YIJ(I,2)*Y12+ZIJ(I,2)*Z12) * AAA
         LBP(I,1) = MAX(ZERO,MIN(ONE,LBP(I,1)))
         LCP(I,1) = ONE - LBP(I,1)
        ELSEIF(LBP(I,1).LT.ZERO.AND.
     +         HLB(I,1).LE.HLC(I,1).AND.HLB(I,1).LE.HLA)THEN
         LBP(I,1) = ZERO
         LCP(I,1) = AL(I,2)
        ELSEIF(LCP(I,1).LT.ZERO.AND.
     +         HLC(I,1).LE.HLA.AND.HLC(I,1).LE.HLB(I,1))THEN
         LCP(I,1) = ZERO
         LBP(I,1) = AL(I,1)
        ENDIF
C       Projection en dehors du triangle
        IF(LB(I,2) < -EM03 .OR. LC(I,2) < -EM03 .OR.
     .      LB(I,2)+LC(I,2) > ONE+EM03) THEN
           FARA(I,2)=1
           FARB(I,2)=1
        ENDIF
        X12 = XX(I,3) - XX(I,2)
        Y12 = YY(I,3) - YY(I,2)
        Z12 = ZZ(I,3) - ZZ(I,2)
        LBP(I,2) = LB(I,2)
        LCP(I,2) = LC(I,2)
        LAP       = ONE-LBP(I,2)-LCP(I,2)
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LAP*ABS(LAP) * AAA
        IF(LAP.LT.ZERO.AND.
     +     HLA.LE.HLB(I,2).AND.HLA.LE.HLC(I,2))THEN
         LBP(I,2) = (XIJ(I,3)*X12+YIJ(I,3)*Y12+ZIJ(I,3)*Z12) * AAA
         LBP(I,2) = MAX(ZERO,MIN(ONE,LBP(I,2)))
         LCP(I,2) = ONE - LBP(I,2)
        ELSEIF(LBP(I,2).LT.ZERO.AND.
     +         HLB(I,2).LE.HLC(I,2).AND.HLB(I,2).LE.HLA)THEN
         LBP(I,2) = ZERO
         LCP(I,2) = AL(I,3)
        ELSEIF(LCP(I,2).LT.ZERO.AND.
     +         HLC(I,2).LE.HLA.AND.HLC(I,2).LE.HLB(I,2))THEN
         LCP(I,2) = ZERO
         LBP(I,2) = AL(I,2)
        END IF
C       Projection en dehors du triangle
        IF(LB(I,3) < -EM03 .OR. LC(I,3) < -EM03 .OR.
     .      LB(I,3)+LC(I,3) > ONE+EM03) THEN
           FARA(I,3)=1
           FARB(I,3)=1
        END IF
        X12 = XX(I,4) - XX(I,3)
        Y12 = YY(I,4) - YY(I,3)
        Z12 = ZZ(I,4) - ZZ(I,3)
        LBP(I,3) = LB(I,3)
        LCP(I,3) = LC(I,3)
        LAP       = ONE-LBP(I,3)-LCP(I,3)
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LAP*ABS(LAP) * AAA
        IF(LAP.LT.ZERO.AND.
     +     HLA.LE.HLB(I,3).AND.HLA.LE.HLC(I,3))THEN
         LBP(I,3) = (XIJ(I,4)*X12+YIJ(I,4)*Y12+ZIJ(I,4)*Z12) * AAA
         LBP(I,3) = MAX(ZERO,MIN(ONE,LBP(I,3)))
         LCP(I,3) = ONE - LBP(I,3)
        ELSEIF(LBP(I,3).LT.ZERO.AND.
     +         HLB(I,3).LE.HLC(I,3).AND.HLB(I,3).LE.HLA)THEN
         LBP(I,3) = ZERO
         LCP(I,3) = AL(I,4)
        ELSEIF(LCP(I,3).LT.ZERO.AND.
     +         HLC(I,3).LE.HLA.AND.HLC(I,3).LE.HLB(I,3))THEN
         LCP(I,3) = ZERO
         LBP(I,3) = AL(I,3)
        ENDIF
C       Projection en dehors du triangle
        IF(LB(I,4) < -EM03 .OR. LC(I,4) < -EM03 .OR.
     .      LB(I,4)+LC(I,4) > ONE+EM03) THEN
           FARA(I,4)=1
           FARB(I,4)=1
        END IF
        X12 = XX(I,1) - XX(I,4)
        Y12 = YY(I,1) - YY(I,4)
        Z12 = ZZ(I,1) - ZZ(I,4)
        LBP(I,4) = LB(I,4)
        LCP(I,4) = LC(I,4)
        LAP       = ONE-LBP(I,4)-LCP(I,4)
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LAP*ABS(LAP) * AAA
        IF(LAP.LT.ZERO.AND.
     +     HLA.LE.HLB(I,4).AND.HLA.LE.HLC(I,4))THEN
         LBP(I,4) = (XIJ(I,1)*X12+YIJ(I,1)*Y12+ZIJ(I,1)*Z12) * AAA
         LBP(I,4) = MAX(ZERO,MIN(ONE,LBP(I,4)))
         LCP(I,4) = ONE - LBP(I,4)
        ELSEIF(LBP(I,4).LT.ZERO.AND.
     +         HLB(I,4).LE.HLC(I,4).AND.HLB(I,4).LE.HLA)THEN
         LBP(I,4) = ZERO
         LCP(I,4) = AL(I,1)
        ELSEIF(LCP(I,4).LT.ZERO.AND.
     +         HLC(I,4).LE.HLA.AND.HLC(I,4).LE.HLB(I,4))THEN
         LCP(I,4) = ZERO
         LBP(I,4) = AL(I,4)
        ENDIF
      ENDDO


        DO I = 1,JLT

          LAP1 = ONE-LBP(I,1)-LCP(I,1)
          XP  =LAP1*XX(I,5)+LBP(I,1)*XX(I,1) + LCP(I,1)*XX(I,2)
          YP  =LAP1*YY(I,5)+LBP(I,1)*YY(I,1) + LCP(I,1)*YY(I,2)
          ZP  =LAP1*ZZ(I,5)+LBP(I,1)*ZZ(I,1) + LCP(I,1)*ZZ(I,2)
          DX  =XI(I)-XP
          DY  =YI(I)-YP
          DZ  =ZI(I)-ZP
          DD(I,1) =DX*DX+DY*DY+DZ*DZ
C
          LAP2 = ONE-LBP(I,2)-LCP(I,2)
          XP  =LAP2*XX(I,5)+LBP(I,2)*XX(I,2) + LCP(I,2)*XX(I,3)
          YP  =LAP2*YY(I,5)+LBP(I,2)*YY(I,2) + LCP(I,2)*YY(I,3)
          ZP  =LAP2*ZZ(I,5)+LBP(I,2)*ZZ(I,2) + LCP(I,2)*ZZ(I,3)
          DX  =XI(I)-XP
          DY  =YI(I)-YP
          DZ  =ZI(I)-ZP
          DD(I,2) =DX*DX+DY*DY+DZ*DZ
C
          LAP3 = ONE-LBP(I,3)-LCP(I,3)
          XP  =LAP3*XX(I,5)+LBP(I,3)*XX(I,3) + LCP(I,3)*XX(I,4)
          YP  =LAP3*YY(I,5)+LBP(I,3)*YY(I,3) + LCP(I,3)*YY(I,4)
          ZP  =LAP3*ZZ(I,5)+LBP(I,3)*ZZ(I,3) + LCP(I,3)*ZZ(I,4)
          DX  =XI(I)-XP
          DY  =YI(I)-YP
          DZ  =ZI(I)-ZP
          DD(I,3) =DX*DX+DY*DY+DZ*DZ
C
          LAP4 = ONE-LBP(I,4)-LCP(I,4)
          XP  =LAP4*XX(I,5)+LBP(I,4)*XX(I,4) + LCP(I,4)*XX(I,1)
          YP  =LAP4*YY(I,5)+LBP(I,4)*YY(I,4) + LCP(I,4)*YY(I,1)
          ZP  =LAP4*ZZ(I,5)+LBP(I,4)*ZZ(I,4) + LCP(I,4)*ZZ(I,1)
          DX  =XI(I)-XP
          DY  =YI(I)-YP
          DZ  =ZI(I)-ZP
          DD(I,4) =DX*DX+DY*DY+DZ*DZ

C
          GAP  = MIN(MAX(DRAD,GAPS(I)+LAP1*GAP_MM(I)+LBP(I,1)*GAP_NM(1,I)+LCP(I,1)*GAP_NM(2,I)+DGAPLOAD),
     .                   MAX(DRAD,GAPMXL(I)+DGAPLOAD))
          GAP21 = GAP**2
          BB(I,1) =((XX(I,5)-XI(I))*XN(I,1)+(YY(I,5)-YI(I))*YN(I,1)+(ZZ(I,5)-ZI(I))*ZN(I,1))
C         
          GAP  = MIN(MAX(DRAD,GAPS(I)+LAP2*GAP_MM(I)+LBP(I,2)*GAP_NM(2,I)+LCP(I,2)*GAP_NM(3,I)+DGAPLOAD),
     .                   MAX(DRAD,GAPMXL(I)+DGAPLOAD))
          GAP22 =GAP**2
          BB(I,2) =((XX(I,5)-XI(I))*XN(I,2)+(YY(I,5)-YI(I))*YN(I,2)+(ZZ(I,5)-ZI(I))*ZN(I,2))
C
          GAP  = MIN(MAX(DRAD,GAPS(I)+LAP3*GAP_MM(I)+LBP(I,3)*GAP_NM(3,I)+LCP(I,3)*GAP_NM(4,I)+DGAPLOAD),
     .                   MAX(DRAD,GAPMXL(I)+DGAPLOAD))
          GAP23 =GAP**2
          BB(I,3) =((XX(I,5)-XI(I))*XN(I,3)+(YY(I,5)-YI(I))*YN(I,3)+(ZZ(I,5)-ZI(I))*ZN(I,3))
C
          GAP  = MIN(MAX(DRAD,GAPS(I)+LAP4*GAP_MM(I)+LBP(I,4)*GAP_NM(4,I)+LCP(I,4)*GAP_NM(1,I)+DGAPLOAD),
     .                   MAX(DRAD,GAPMXL(I)+DGAPLOAD))
          GAP24 =GAP**2
          BB(I,4) =((XX(I,5)-XI(I))*XN(I,4)+(YY(I,5)-YI(I))*YN(I,4)+(ZZ(I,5)-ZI(I))*ZN(I,4))
C
          IF(DD(I,1) <= GAP21) THEN 
            IF(BB(I,1) <= ZERO .AND. INTERSECTA(I) /= 1)THEN
              INGAPA(I,1)=1
            END IF
            IF(BB(I,1) >= ZERO .AND. INTERSECTB(I) /= 1)THEN
              INGAPB(I,1)=1
            END IF
          ENDIF
          IF(DD(I,2) <= GAP22) THEN
            IF(BB(I,2) <= ZERO .AND. INTERSECTA(I) /= 1)THEN
              INGAPA(I,2)=1
            END IF
            IF(BB(I,2) >= ZERO .AND. INTERSECTB(I) /= 1)THEN
              INGAPB(I,2)=1
            END IF
          ENDIF
          IF(DD(I,3) <= GAP23) THEN
            IF(BB(I,3) <= ZERO .AND. INTERSECTA(I) /= 1)THEN
              INGAPA(I,3)=1
            END IF
            IF(BB(I,3) >= ZERO .AND. INTERSECTB(I) /= 1)THEN
              INGAPB(I,3)=1
            END IF
          ENDIF
          IF(DD(I,4) <= GAP24) THEN
            IF(BB(I,4) <= ZERO .AND. INTERSECTA(I) /= 1)THEN
              INGAPA(I,4)=1
            END IF
            IF(BB(I,4) >= ZERO .AND. INTERSECTB(I) /= 1)THEN
              INGAPB(I,4)=1
          END IF
          END IF

        END DO
C--------------------------------------------------------
C     La determination du sous-triangle tq son cone contient le nd ne necessite pas FAR !
C--------------------------------------------------------
      SUBTRIA(1:JLT)=0
      SUBTRIB(1:JLT)=0
      SUBTRIX(1:JLT)=0
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
C
        IF(IX3(I)/=IX4(I))THEN

          DMIN=MIN(DD(I,1),DD(I,2),DD(I,3),DD(I,4))
          LD=EP20
          DO IT=1,4
            IF(DD(I,IT) <= ONEP03*DMIN)THEN
              LBX = LB(I,IT)
              LCX = LC(I,IT)
              LAX = ONE-LB(I,IT)-LC(I,IT)
C
C             Privilegier secteur dans lequel on se trouve.
              IF(LBX >= ZERO .AND. LCX >= ZERO)THEN
                LX=ZERO
              ELSE
                ! point le plus proche dans le secteur angulaire == centre
                LX  = MAX(ZERO,DD(I,IT)-BB(I,IT)*BB(I,IT))
              END IF

              IF(LX < LD) THEN
                SUBTRIX(I)= IT
                LD        = LX
              END IF
            END IF
          END DO
C          
C         !
          IT=SUBTRIX(I)  
          IF(IT > 0) THEN
            IF(INTERSECTA(I)/=0.OR.INGAPA(I,IT)/=0)SUBTRIA(I)=IT
            IF (ISHEL(I) /= 0)THEN
              IF(INTERSECTB(I)/=0.OR.INGAPB(I,IT)/=0)SUBTRIB(I)=IT
            END IF
          ENDIF
        ELSE

          IF(INTERSECTA(I)/=0.OR.INGAPA(I,1)/=0)THEN
            SUBTRIA(I)= 1
          END IF
C          
          IF (ISHEL(I) /= 0)THEN
            IF(INTERSECTB(I)/=0.OR.INGAPB(I,1)/=0)THEN
              SUBTRIB(I)= 1
            END IF
          END IF

        END IF
C
      END DO
C--------------------------------------------------------
C     Calcul de la penetration
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I =1,JLT
C          
C      IF (STIF(I) == ZERO)CYCLE
C 
       IT=SUBTRIA(I)
       IF(IT==0)CYCLE
C          
       I1=ITRIA(1,IT)
       I2=ITRIA(2,IT)
C
       LAP = ONE-LBP(I,IT)-LCP(I,IT)
       GAP  = MIN(MAX(DRAD,GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I))+DGAPLOAD,
     .                   MAX(DRAD,GAPMXL(I)+DGAPLOAD))
C
       IF(BB(I,IT) > ZERO)THEN
C
C        penetration approximee (cf zone limite interpolation des normales)
         PENA(I,IT)=MAX(ZERO,GAP+BB(I,IT))
       ELSE ! IF(BB(I,IT) > ZERO)THEN
         PENA(I,IT)=MAX(ZERO,GAP-SQRT(DD(I,IT)))
       END IF
C
      ENDDO
C--------------------------------------------------------
C     Check vs bissectors ...
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I =1,JLT
C          
C       IF (STIF(I) == ZERO)CYCLE
C 
        IT=SUBTRIA(I)
        IF(IT==0)CYCLE
C
        IF(PENA(I,IT)==ZERO) CYCLE          
C          
        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
        XH=XI(I)+BB(I,IT)*XN(I,IT)
        YH=YI(I)+BB(I,IT)*YN(I,IT)
        ZH=ZI(I)+BB(I,IT)*ZN(I,IT)
C
        IF(IX3(I) /= IX4(I))THEN
C
          IB1=IBOUNDA(I1,I)
          IB2=IBOUNDA(I2,I)
          IF(MVOISA(I,IT)==0)THEN     
C
            IF( (XH-XX(I,I1))* NAX(I,IT)+
     .          (YH-YY(I,I1))* NAY(I,IT)+
     .          (ZH-ZZ(I,I1))* NAZ(I,IT) >= GAPS(I)) FARA(I,IT) =3
C
          ELSEIF((IB1 /= 0 .AND. IB2 == 0).OR.
     .           (IB2 /= 0 .AND. IB1 == 0))THEN
C
            IBX=MAX(IB1,IB2)
C
            IBX=MAX(IB1,IB2)
            IF(IB1/=0)THEN
              IX =I1
            ELSEIF(IB2/=0)THEN
              IX =I2
            END IF
C
            XH=XI(I)+BB(I,IT)*XN(I,IT)
            YH=YI(I)+BB(I,IT)*YN(I,IT)
            ZH=ZI(I)+BB(I,IT)*ZN(I,IT)
C
            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
              P1 = (XH-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FARA(I,IT) =3
            ELSE
              VX = X0N(I,IX) ! fake bisector of angle at vertex IX
              VY = Y0N(I,IX)
              VZ = Z0N(I,IX)
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH-XX(I,IX))*VX+(YH-YY(I,IX))*VY+(ZH-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FARA(I,IT) =3
            END IF

          END IF
C
C         Cone
          IF(INGAPA(I,IT)  == 1 .AND. (FARA(I,IT)==1 .OR. BB(I,IT) <= ZERO))THEN
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
            X12=XX(I,I2)-XX(I,I1)
            Y12=YY(I,I2)-YY(I,I1)
            Z12=ZZ(I,I2)-ZZ(I,I1)
C
C           normal to the bisecting plane (pointing toward the inside)
            PX = Z12*NAY(I,IT)-Y12*NAZ(I,IT)  
            PY = X12*NAZ(I,IT)-Z12*NAX(I,IT)  
            PZ = Y12*NAX(I,IT)-X12*NAY(I,IT)  
            PP = PX*PX+PY*PY+PZ*PZ
C
            LL  = XIJ(I,I1)*XIJ(I,I1)+YIJ(I,I1)*YIJ(I,I1)+ZIJ(I,I1)*ZIJ(I,I1)
C
            SIDA(I,IT)=-(XIJ(I,I1)*PX+YIJ(I,I1)*PY+ZIJ(I,I1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))                 
            IF(SIDA(I,IT) < -ZEP01) FARA(I,IT) =2
C
          END IF
        ELSE
          IB1=IBOUNDA(1,I)
          IB2=IBOUNDA(2,I)
          IB3=IBOUNDA(3,I)
C
          D1=(XH-XX(I,1))* NAX(I,1)+
     .       (YH-YY(I,1))* NAY(I,1)+
     .       (ZH-ZZ(I,1))* NAZ(I,1)
          D2=(XH-XX(I,2))* NAX(I,2)+
     .       (YH-YY(I,2))* NAY(I,2)+
     .       (ZH-ZZ(I,2))* NAZ(I,2)
          D3=(XH-XX(I,3))* NAX(I,4)+
     .       (YH-YY(I,3))* NAY(I,4)+
     .       (ZH-ZZ(I,3))* NAZ(I,4)
C
          IF( (MVOISA(I,1) == 0 .AND. D1 >= GAPS(I)).OR.
     .        (MVOISA(I,2) == 0 .AND. D2 >= GAPS(I)).OR.
     .        (MVOISA(I,4) == 0 .AND. D3 >= GAPS(I)) )THEN
C
            FARA(I,IT) =3
C
          ELSEIF((IB1/=0 .AND. IB2==0 .AND. IB3==0).OR.
     .           (IB2/=0 .AND. IB3==0 .AND. IB1==0).OR.
     .           (IB3/=0 .AND. IB1==0 .AND. IB2==0))THEN
C
            IBX=MAX(IB1,IB2,IB3)
            IF(IB1/=0)THEN
              IX =1
              IY =2
              IZ =3
            ELSEIF(IB2/=0)THEN
              IX =2
              IY =3
              IZ =1
            ELSEIF(IB3/=0)THEN
              IX =3
              IY =1
              IZ =2
            END IF
C
            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
              P1 = (XH-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FARA(I,IT) =3
            ELSE
              VX = TWO*XX(I,IX)-(XX(I,IY)+XX(I,IZ)) ! fake bisector of angle 2,1,3
              VY = TWO*YY(I,IX)-(YY(I,IY)+YY(I,IZ))
              VZ = TWO*ZZ(I,IX)-(ZZ(I,IY)+ZZ(I,IZ))
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH-XX(I,IX))*VX+(YH-YY(I,IX))*VY+(ZH-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FARA(I,IT) =3
            END IF
C
          END IF
C
          IF(INGAPA(I,IT)  == 1 .AND. (FARA(I,IT)==1 .OR. BB(I,IT) <= ZERO))THEN
C
C            INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C          
            IF( MVOISA(I,1) /= 0 )THEN
C
              X12=XX(I,2)-XX(I,1)
              Y12=YY(I,2)-YY(I,1)
              Z12=ZZ(I,2)-ZZ(I,1)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12*NAY(I,1)-Y12*NAZ(I,1)  
              PY = X12*NAZ(I,1)-Z12*NAX(I,1)  
              PZ = Y12*NAX(I,1)-X12*NAY(I,1)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,1)*XIJ(I,1)+YIJ(I,1)*YIJ(I,1)+ZIJ(I,1)*ZIJ(I,1)
C
              SIDA(I,1)=-(XIJ(I,1)*PX+YIJ(I,1)*PY+ZIJ(I,1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDA(I,1) < -ZEP01) FARA(I,IT) =2
C
            END IF

            IF( MVOISA(I,2) /= 0 )THEN
            
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C          
              X12=XX(I,3)-XX(I,2)
              Y12=YY(I,3)-YY(I,2)
              Z12=ZZ(I,3)-ZZ(I,2)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12*NAY(I,2)-Y12*NAZ(I,2)  
              PY = X12*NAZ(I,2)-Z12*NAX(I,2)  
              PZ = Y12*NAX(I,2)-X12*NAY(I,2)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,2)*XIJ(I,2)+YIJ(I,2)*YIJ(I,2)+ZIJ(I,2)*ZIJ(I,2)
C
              SIDA(I,2)=-(XIJ(I,2)*PX+YIJ(I,2)*PY+ZIJ(I,2)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDA(I,2) < -ZEP01) FARA(I,IT) =2
C
            END IF

            IF( MVOISA(I,4) /= 0 )THEN
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C          
              X12=XX(I,1)-XX(I,3)
              Y12=YY(I,1)-YY(I,3)
              Z12=ZZ(I,1)-ZZ(I,3)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12*NAY(I,4)-Y12*NAZ(I,4)  
              PY = X12*NAZ(I,4)-Z12*NAX(I,4)  
              PZ = Y12*NAX(I,4)-X12*NAY(I,4)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  =  XIJ(I,3)*XIJ(I,3)+YIJ(I,3)*YIJ(I,3)+ZIJ(I,3)*ZIJ(I,3)
C
              SIDA(I,4)=-(XIJ(I,3)*PX+YIJ(I,3)*PY+ZIJ(I,3)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDA(I,4) < -ZEP01) FARA(I,IT) =2
C
            END IF
          END IF
       END IF
C
       IF(FARA(I,IT)==2 .AND. INTERSECTA(I)==0) PENA(I,IT)=ZERO
       IF(FARA(I,IT)==3) PENA(I,IT)=ZERO
C
      ENDDO
C--------------------------------------------------------
C     Calcul de la penetration
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I =1,JLT
C          
C      IF (STIF(I) == ZERO)CYCLE
C 
       IT=SUBTRIB(I)
       IF(IT==0)CYCLE
C          
       I1=ITRIA(1,IT)
       I2=ITRIA(2,IT)
C
       LAP = ONE-LBP(I,IT)-LCP(I,IT)
       GAP  = MIN(MAX(DRAD,GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I)+DGAPLOAD),
     .                   MAX(DRAD,GAPMXL(I)+DGAPLOAD))
C
       IF(BB(I,IT) < ZERO)THEN
C
C        penetration approximee (cf zone limite interpolation des normales)
         PENB(I,IT)=MAX(ZERO,GAP-BB(I,IT))
       ELSE ! IF(BB(I,IT) < ZERO)THEN
         PENB(I,IT)=MAX(ZERO,GAP-SQRT(DD(I,IT)))
       END IF
C
      ENDDO
C--------------------------------------------------------
C     Check vs bissectors ...
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I =1,JLT
C          
C       IF (STIF(I) == ZERO)CYCLE
C 
        IT=SUBTRIB(I)
        IF(IT==0)CYCLE
C
        IF(PENB(I,IT)==ZERO) CYCLE          
C          
        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
        XH=XI(I)+BB(I,IT)*XN(I,IT)
        YH=YI(I)+BB(I,IT)*YN(I,IT)
        ZH=ZI(I)+BB(I,IT)*ZN(I,IT)
C
        IF(IX3(I) /= IX4(I))THEN
C
C         Cone
          IB1=IBOUNDB(I1,I)
          IB2=IBOUNDB(I2,I)
          IF(MVOISB(I,IT)==0)THEN     
C
            IF( (XH-XX(I,I1))* NBX(I,IT)+
     .          (YH-YY(I,I1))* NBY(I,IT)+
     .          (ZH-ZZ(I,I1))* NBZ(I,IT) >= GAPS(I)) FARB(I,IT) =3
C
          ELSEIF((IB1 /= 0 .AND. IB2 == 0).OR.
     .           (IB2 /= 0 .AND. IB1 == 0))THEN
C
            IBX=MAX(IB1,IB2)
            IF(IB1/=0)THEN
              IX =I1
            ELSEIF(IB2/=0)THEN
              IX =I2
            END IF
C
            XH=XI(I)+BB(I,IT)*XN(I,IT)
            YH=YI(I)+BB(I,IT)*YN(I,IT)
            ZH=ZI(I)+BB(I,IT)*ZN(I,IT)
C
            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
              P1 = (XH-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FARB(I,IT) =3
            ELSE
              VX = X0N(I,IX) ! fake bisector of angle at vertex IX
              VY = Y0N(I,IX)
              VZ = Z0N(I,IX)
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH-XX(I,IX))*VX+(YH-YY(I,IX))*VY+(ZH-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FARB(I,IT) =3
            END IF

          END IF
C
C
          IF(INGAPB(I,IT)  == 1 .AND. (FARB(I,IT)==1 .OR. BB(I,IT) >= ZERO))THEN
C
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
            X12=XX(I,I2)-XX(I,I1)
            Y12=YY(I,I2)-YY(I,I1)
            Z12=ZZ(I,I2)-ZZ(I,I1)
C
C           normal to the bisecting plane (pointing toward the inside)
            PX = Z12*NBY(I,IT)-Y12*NBZ(I,IT)  
            PY = X12*NBZ(I,IT)-Z12*NBX(I,IT)  
            PZ = Y12*NBX(I,IT)-X12*NBY(I,IT)  
            PP = PX*PX+PY*PY+PZ*PZ
C
            LL  = XIJ(I,I1)*XIJ(I,I1)+YIJ(I,I1)*YIJ(I,I1)+ZIJ(I,I1)*ZIJ(I,I1)
C
            SIDB(I,IT)= (XIJ(I,I1)*PX+YIJ(I,I1)*PY+ZIJ(I,I1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))                 
            IF(SIDB(I,IT) < -ZEP01) FARB(I,IT) =2
C
          END IF
        ELSE
          IB1=IBOUNDB(1,I)
          IB2=IBOUNDB(2,I)
          IB3=IBOUNDB(3,I)
C
          D1=(XH-XX(I,1))* NBX(I,1)+
     .       (YH-YY(I,1))* NBY(I,1)+
     .       (ZH-ZZ(I,1))* NBZ(I,1)
          D2=(XH-XX(I,2))* NBX(I,2)+
     .       (YH-YY(I,2))* NBY(I,2)+
     .       (ZH-ZZ(I,2))* NBZ(I,2)
          D3=(XH-XX(I,3))* NBX(I,4)+
     .       (YH-YY(I,3))* NBY(I,4)+
     .       (ZH-ZZ(I,3))* NBZ(I,4)
C
          IF( (MVOISB(I,1) == 0 .AND. D1 >= GAPS(I)).OR.
     .        (MVOISB(I,2) == 0 .AND. D2 >= GAPS(I)).OR.
     .        (MVOISB(I,4) == 0 .AND. D3 >= GAPS(I)) )THEN
C
            FARB(I,IT) =3
C
          ELSEIF((IB1/=0 .AND. IB2==0 .AND. IB3==0).OR.
     .           (IB2/=0 .AND. IB3==0 .AND. IB1==0).OR.
     .           (IB3/=0 .AND. IB1==0 .AND. IB2==0))THEN
C
            IBX=MAX(IB1,IB2,IB3)
            IF(IB1/=0)THEN
              IX =1
              IY =2
              IZ =3
            ELSEIF(IB2/=0)THEN
              IX =2
              IY =3
              IZ =1
            ELSEIF(IB3/=0)THEN
              IX =3
              IY =1
              IZ =2
            END IF
C
            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
              P1 = (XH-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FARB(I,IT) =3
            ELSE
              VX = TWO*XX(I,IX)-(XX(I,IY)+XX(I,IZ)) ! fake bisector of angle 2,1,3
              VY = TWO*YY(I,IX)-(YY(I,IY)+YY(I,IZ))
              VZ = TWO*ZZ(I,IX)-(ZZ(I,IY)+ZZ(I,IZ))
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH-XX(I,IX))*VX+(YH-YY(I,IX))*VY+(ZH-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FARB(I,IT) =3
            END IF
C
          END IF
C
          IF(INGAPB(I,IT)  == 1 .AND. (FARB(I,IT)==1 .OR. BB(I,IT) >= ZERO))THEN
C
C           INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C          
            IF( MVOISB(I,1) /= 0 )THEN
C          
              X12=XX(I,2)-XX(I,1)
              Y12=YY(I,2)-YY(I,1)
              Z12=ZZ(I,2)-ZZ(I,1)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12*NBY(I,1)-Y12*NBZ(I,1)  
              PY = X12*NBZ(I,1)-Z12*NBX(I,1)  
              PZ = Y12*NBX(I,1)-X12*NBY(I,1)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,1)*XIJ(I,1)+YIJ(I,1)*YIJ(I,1)+ZIJ(I,1)*ZIJ(I,1)
C
              SIDB(I,1)= (XIJ(I,1)*PX+YIJ(I,1)*PY+ZIJ(I,1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDB(I,1) < -ZEP01) FARB(I,IT) =2
C
            END IF

            IF( MVOISB(I,2) /= 0 )THEN
C          
              X12=XX(I,3)-XX(I,2)
              Y12=YY(I,3)-YY(I,2)
              Z12=ZZ(I,3)-ZZ(I,2)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12*NBY(I,2)-Y12*NBZ(I,2)  
              PY = X12*NBZ(I,2)-Z12*NBX(I,2)  
              PZ = Y12*NBX(I,2)-X12*NBY(I,2)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,2)*XIJ(I,2)+YIJ(I,2)*YIJ(I,2)+ZIJ(I,2)*ZIJ(I,2)
C
              SIDB(I,2)= (XIJ(I,2)*PX+YIJ(I,2)*PY+ZIJ(I,2)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDB(I,2) < -ZEP01) FARB(I,IT) =2
C
            END IF

            IF( MVOISB(I,4) /= 0 )THEN
C          
              X12=XX(I,1)-XX(I,3)
              Y12=YY(I,1)-YY(I,3)
              Z12=ZZ(I,1)-ZZ(I,3)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12*NBY(I,4)-Y12*NBZ(I,4)  
              PY = X12*NBZ(I,4)-Z12*NBX(I,4)  
              PZ = Y12*NBX(I,4)-X12*NBY(I,4)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  =  XIJ(I,3)*XIJ(I,3)+YIJ(I,3)*YIJ(I,3)+ZIJ(I,3)*ZIJ(I,3)
C
              SIDB(I,4)= (XIJ(I,3)*PX+YIJ(I,3)*PY+ZIJ(I,3)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDB(I,4) < -ZEP01) FARB(I,IT) =2
C
            END IF
          END IF
       END IF
C
       IF(FARB(I,IT)==2 .AND. INTERSECTB(I)==0) PENB(I,IT)=ZERO
       IF(FARB(I,IT)==3) PENB(I,IT)=ZERO
C
      ENDDO
C--------------------------------------------------------
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
C
        IA    = SUBTRIA(I)
        PENAX = ZERO
        IF(IA/=0)PENAX=PENA(I,IA)
C
        IB    = SUBTRIB(I)
        PENBX = ZERO
        IF(IB/=0)PENBX=PENB(I,IB)
C
        IF(PENAX > PENBX .AND. IA > 0)THEN
          L      = CAND_E(I)
          IT     = IA
          PENT(I)= PENAX

          LBS(I)=LBP(I,IT)
          LCS(I)=LCP(I,IT)
          FAR(I)=FARA(I,IT)

        ELSEIF(PENBX > PENAX .AND. IB > 0)THEN
          L         = ISHEL(I)
          CAND_E(I) = L

          IT        = IB
          PENT(I)   = PENBX

          LBS(I)=LCP(I,IT)
          LCS(I)=LBP(I,IT)
          FAR(I)=FARB(I,IT)

          SUBTRIA(I)=ITPERM(IT) ! IT/=0
          IT = SUBTRIA(I)

        ELSE
          IT=0
          SUBTRIA(I) = 0
          PENT(I)    = ZERO
        END IF

c        if(itab(nsvg(i))==2421446.or.
c     .       itab(ix1(i))==2421446.or.itab(ix2(i))==2421446.or.itab(ix3(i))==2421446.or.itab(ix4(i))==2421446)
c     .      print *,'dst2 nat',ispmd+1,
c     .      itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),
c     .      it,ia,ib,penax,penbx,
c     .      ingapa(i,1:4),intersecta(i),ingapb(i,1:4),intersectb(i),
c     .      bb(i,1:4),dd(i,1:4)

        IF(IT == 0)CYCLE
        PENE = PENT(I)

        N = CAND_N(I)

        MGLOB=MSEGLO(L)

        IF(N.LE.NSN)THEN
#include "lockon.inc"

          IF(  TIME_S(1,N) < PENE .OR.
     *        (TIME_S(1,N) == PENE .AND. -IRTLM(1,N) < MGLOB ))THEN
            IRTLM(1,N) = -MGLOB
            IRTLM(2,N) = IT
            IRTLM(3,N) = CAND_E(I)
            IRTLM(4,N) = ISPMD+1
            TIME_S(1,N)= PENE
C            
C           PENE_OLD(5,N)=PENE 
          END IF
c      if(itab(nsvg(i))==27363)then
cc      if(itab(nsvg(i))==27952.or.
cc     .       itab(ix1(i))==27952.or.itab(ix2(i))==27952.or.itab(ix3(i))==27952.or.itab(ix4(i))==27952)then
c         print *,'dst2 nat',ispmd+1,
c     .      itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),
c     .      irtlm(1,n),cand_e(i),TIME_S(1,N),
c     .      it,pent(i),penax,penbx,intersecta(i),intersectb(i),lbs(i),lcs(i),lbs(i)+lcs(i)
c        if(ia/=0)
c     .        print *,'ia',ia,bb(i,ia),ingapa(i,ia),fara(i,ia),ibounda(1:4,i)
c        if(ib/=0)
c     .        print *,'ib',ib,bb(i,ib),ingapb(i,ib),farb(i,ib),iboundb(1:4,i)
c       end if
#include "lockoff.inc"
        ELSE
#include "lockon.inc"
          IF(  TIME_SFI(NIN)%P(2*(N-NSN-1)+1) < PENE .OR.
     *        (TIME_SFI(NIN)%P(2*(N-NSN-1)+1) == PENE .AND. -IRTLM_FI(NIN)%P(1,N-NSN) < MGLOB ))THEN
            IRTLM_FI(NIN)%P(1,N-NSN) = -MGLOB
            IRTLM_FI(NIN)%P(2,N-NSN) = IT
            IRTLM_FI(NIN)%P(3,N-NSN) = CAND_E(I)
            IRTLM_FI(NIN)%P(4,N-NSN) = ISPMD+1
            TIME_SFI(NIN)%P(2*(N-NSN-1)+1) = PENE
C            
C           PENE_OLDFI(NIN)%P(5,N-NSN)=PENE
          END IF
c          if(itafi(nin)%p(n-nsn)==3817238)
c     .      print *,'dst2 rem',ispmd+1,
c     .  itafi(nin)%p(n-nsn),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),
c     .  it,IRTLM_FI(NIN)%P(1,n-nsn),mglob,cand_e(i),pent(i,it),far(i,it),
c     .      TIME_SFI(NIN)%P(N-NSN),dd(i,it),ingap(i,it),intersect(i)
#include "lockoff.inc"
        END IF
      END DO

      RETURN
      END
Chd|====================================================================
Chd|  INTERSECA_25                  source/interfaces/int25/i25dst3_22.F
Chd|-- called by -----------
Chd|        I25DST3_22                    source/interfaces/int25/i25dst3_22.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERSECA_25(
     1                    IXX3     ,IXX4  ,INTERSECA,
     1                    V12      ,V23   ,V34    ,V41   ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3                    YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4                    XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5                    ZZ4      ,XX5   ,YY5    ,ZZ5   ,
     6                    VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,XI    ,YI     ,
     A                    ZI      ,XOI    ,YOI    ,ZOI   ,ZEROM  ,
     B                    UNP    ,ZEROMT,UNPT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXX3  ,IXX4  , INTERSECA
C     REAL
      my_real
     1     V12   ,V23    ,V34   ,V41    ,
     2     VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3     YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4     XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5     ZZ4      ,XX5   ,YY5    ,ZZ5   ,
     6     VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7     XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8     YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9     ZO3     ,ZO4    ,ZO5    ,ZEROM ,UNP    ,
     A     ZEROMT  ,UNPT   ,XI     ,YI    ,ZI     ,   
     B     XOI     ,YOI    ,ZOI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XS, YS, ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5,LA0,LB0,LC0
      my_real
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,AAA,S2
C------------------------------------------------
         INTERSECA = 0

         IF(VO12 <= ZERO .AND. V12 >= ZERO)THEN
           IF(ABS(VO12) < EM20)THEN
             AAA = ONE
           ELSEIF(ABS(V12) < EM20)THEN
             AAA = ZERO
           ELSE
             AAA = V12 / (V12-VO12)
           ENDIF

           XM1 = XX1 + AAA*(XO1-XX1)
           YM1 = YY1 + AAA*(YO1-YY1)
           ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
           XM2 = XX2 + AAA*(XO2-XX2)
           YM2 = YY2 + AAA*(YO2-YY2)
           ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
           XS = XI + AAA*(XOI-XI)
           YS = YI + AAA*(YOI-YI)
           ZS = ZI + AAA*(ZOI-ZI)

           XM5 = XX5 + AAA*(XO5-XX5)
           YM5 = YY5 + AAA*(YO5-YY5)
           ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
           X51 = XM1 - XM5
           Y51 = YM1 - YM5
           Z51 = ZM1 - ZM5
 
           X52 = XM2 - XM5
           Y52 = YM2 - YM5
           Z52 = ZM2 - ZM5
 
           XI1 = XM1 - XS
           YI1 = YM1 - YS
           ZI1 = ZM1 - ZS
 
           XI2 = XM2 - XS
           YI2 = YM2 - YS
           ZI2 = ZM2 - ZS
         
           XI5 = XM5 - XS
           YI5 = YM5 - YS
           ZI5 = ZM5 - ZS

           SX0 = Y51*Z52 - Z51*Y52
           SY0 = Z51*X52 - X51*Z52
           SZ0 = X51*Y52 - Y51*X52

           SX1 = YI5*ZI1 - ZI5*YI1
           SY1 = ZI5*XI1 - XI5*ZI1
           SZ1 = XI5*YI1 - YI5*XI1

           SX5 = YI1*ZI2 - ZI1*YI2
           SY5 = ZI1*XI2 - XI1*ZI2
           SZ5 = XI1*YI2 - YI1*XI2

           SX2 = YI2*ZI5 - ZI2*YI5
           SY2 = ZI2*XI5 - XI2*ZI5
           SZ2 = XI2*YI5 - YI2*XI5
           
           S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
           LB0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
           LC0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
           LA0 =  ONE-LB0-LC0 

          IF(IXX3 /= IXX4)THEN
           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .        LC0 >= ZEROM .and. LC0 <= UNP .and.
     .        LA0 >= ZEROM .and. LA0 <= UNP)THEN
             INTERSECA = 1
           ENDIF
C----------loose tolerance for tria           
          ELSE
           IF(LB0 >= ZEROMT .AND. LB0 <= UNPT .AND.
     .        LC0 >= ZEROMT .AND. LC0 <= UNPT .AND.
     .        LA0 >= ZEROMT .AND. LA0 <= UNPT)THEN
             INTERSECA = 1
           ENDIF
          END IF! (IXX(I,3) /= IXX(I,4))THEN
         ENDIF
         IF(IXX3 /= IXX4)THEN
           IF(VO23 <= ZERO .AND. V23 >= ZERO)THEN
             IF(ABS(VO23) < EM20)THEN
               AAA = ONE
             ELSEIF(ABS(V23) < EM20)THEN
               AAA = ZERO
             ELSE
               AAA = V23 / (V23-VO23)
             ENDIF

             XM2 = XX2 + AAA*(XO2-XX2)
             YM2 = YY2 + AAA*(YO2-YY2)
             ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
             XM3 = XX3 + AAA*(XO3-XX3)
             YM3 = YY3 + AAA*(YO3-YY3)
             ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
             XS = XI + AAA*(XOI-XI)
             YS = YI + AAA*(YOI-YI)
             ZS = ZI + AAA*(ZOI-ZI)

             XM5 = XX5 + AAA*(XO5-XX5)
             YM5 = YY5 + AAA*(YO5-YY5)
             ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
             X52 = XM2 - XM5
             Y52 = YM2 - YM5
             Z52 = ZM2 - ZM5
 
             X53 = XM3 - XM5
             Y53 = YM3 - YM5
             Z53 = ZM3 - ZM5
 
             XI2 = XM2 - XS
             YI2 = YM2 - YS
             ZI2 = ZM2 - ZS
 
             XI3 = XM3 - XS
             YI3 = YM3 - YS
             ZI3 = ZM3 - ZS
         
             XI5 = XM5 - XS
             YI5 = YM5 - YS
             ZI5 = ZM5 - ZS

             SX0 = Y52*Z53 - Z52*Y53
             SY0 = Z52*X53 - X52*Z53
             SZ0 = X52*Y53 - Y52*X53

             SX2 = YI5*ZI2 - ZI5*YI2
             SY2 = ZI5*XI2 - XI5*ZI2
             SZ2 = XI5*YI2 - YI5*XI2

             SX5 = YI2*ZI3 - ZI2*YI3
             SY5 = ZI2*XI3 - XI2*ZI3
             SZ5 = XI2*YI3 - YI2*XI3

             SX3 = YI3*ZI5 - ZI3*YI5
             SY3 = ZI3*XI5 - XI3*ZI5
             SZ3 = XI3*YI5 - YI3*XI5
             
             S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
             LB0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
             LC0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
             LA0 =  ONE-LB0-LC0 

             IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .          LC0 >= ZEROM .and. LC0 <= UNP .and.
     .          LA0 >= ZEROM .and. LA0 <= UNP)THEN
               INTERSECA = 1
             ENDIF
          ENDIF
           IF(VO34 <= ZERO .AND. V34 >= ZERO)THEN
            IF(ABS(VO34) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V34) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V34 / (V34-VO34)
            ENDIF

            XM3 = XX3 + AAA*(XO3-XX3)
            YM3 = YY3 + AAA*(YO3-YY3)
            ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4)
         
            XS = XI + AAA*(XOI-XI)
            YS = YI + AAA*(YOI-YI)
            ZS = ZI + AAA*(ZOI-ZI)

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X53 = XM3 - XM5
            Y53 = YM3 - YM5
            Z53 = ZM3 - ZM5
 
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            XI3 = XM3 - XS
            YI3 = YM3 - YS
            ZI3 = ZM3 - ZS
 
            XI4 = XM4 - XS
            YI4 = YM4 - YS
            ZI4 = ZM4 - ZS
         
            XI5 = XM5 - XS
            YI5 = YM5 - YS
            ZI5 = ZM5 - ZS

            SX0 = Y53*Z54 - Z53*Y54
            SY0 = Z53*X54 - X53*Z54
            SZ0 = X53*Y54 - Y53*X54

            SX3 = YI5*ZI3 - ZI5*YI3
            SY3 = ZI5*XI3 - XI5*ZI3
            SZ3 = XI5*YI3 - YI5*XI3

            SX5 = YI3*ZI4 - ZI3*YI4
            SY5 = ZI3*XI4 - XI3*ZI4
            SZ5 = XI3*YI4 - YI3*XI4

            SX4 = YI4*ZI5 - ZI4*YI5
            SY4 = ZI4*XI5 - XI4*ZI5
            SZ4 = XI4*YI5 - YI4*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LC0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
            LA0 =  ONE-LB0-LC0 

           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               INTERSECA = 1
            ENDIF
          ENDIF

           IF(VO41 <= ZERO .AND. V41 >= ZERO)THEN
            IF(ABS(VO41) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V41) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V41 / (V41-VO41)
            ENDIF

            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4) 
         
            XM1 = XX1 + AAA*(XO1-XX1)
            YM1 = YY1 + AAA*(YO1-YY1)
            ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
            XS = XI + AAA*(XOI-XI)
            YS = YI + AAA*(YOI-YI)
            ZS = ZI + AAA*(ZOI-ZI)

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            X51 = XM1 - XM5
            Y51 = YM1 - YM5
            Z51 = ZM1 - ZM5
 
            XI4 = XM4 - XS
            YI4 = YM4 - YS
            ZI4 = ZM4 - ZS
 
            XI1 = XM1 - XS
            YI1 = YM1 - YS
            ZI1 = ZM1 - ZS
         
            XI5 = XM5 - XS
            YI5 = YM5 - YS
            ZI5 = ZM5 - ZS

            SX0 = Y54*Z51 - Z54*Y51
            SY0 = Z54*X51 - X54*Z51
            SZ0 = X54*Y51 - Y54*X51

            SX4 = YI5*ZI4 - ZI5*YI4
            SY4 = ZI5*XI4 - XI5*ZI4
            SZ4 = XI5*YI4 - YI5*XI4

            SX5 = YI4*ZI1 - ZI4*YI1
            SY5 = ZI4*XI1 - XI4*ZI1
            SZ5 = XI4*YI1 - YI4*XI1

            SX1 = YI1*ZI5 - ZI1*YI5
            SY1 = ZI1*XI5 - XI1*ZI5
            SZ1 = XI1*YI5 - YI1*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
            LC0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LA0 =  ONE-LB0-LC0 

            IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               INTERSECA = 1
            ENDIF
          ENDIF
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  INTERSECB_25                  source/interfaces/int25/i25dst3_22.F
Chd|-- called by -----------
Chd|        I25DST3_22                    source/interfaces/int25/i25dst3_22.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERSECB_25(
     1                    IXX3     ,IXX4  ,INTERSECA,INTERSECB,
     1                    V12      ,V23   ,V34    ,V41   ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3                    YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4                    XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5                    ZZ4      ,XX5   ,YY5    ,ZZ5   ,
     6                    VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,XI    ,YI     ,
     A                    ZI      ,XOI    ,YOI    ,ZOI   ,ZEROM  ,
     B                    UNP    ,ZEROMT,UNPT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXX3  ,IXX4  , INTERSECA, INTERSECB
C     REAL
      my_real
     1     V12   ,V23    ,V34   ,V41    ,
     2     VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3     YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4     XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5     ZZ4      ,XX5   ,YY5    ,ZZ5   ,
     6     VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7     XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8     YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9     ZO3     ,ZO4    ,ZO5    ,ZEROM ,UNP    ,
     A     ZEROMT  ,UNPT   ,XI     ,YI    ,ZI     ,   
     B     XOI     ,YOI    ,ZOI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XS, YS, ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5,LA0,LB0,LC0
      my_real
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,AAA,S2
C------------------------------------------------
         INTERSECA = 0
         INTERSECB = 0

         IF(VO12 * V12 <= ZERO)THEN
           IF(ABS(VO12) < EM20)THEN
             AAA = ONE
           ELSEIF(ABS(V12) < EM20)THEN
             AAA = ZERO
           ELSE
             AAA = V12 / (V12-VO12)
           ENDIF

           XM1 = XX1 + AAA*(XO1-XX1)
           YM1 = YY1 + AAA*(YO1-YY1)
           ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
           XM2 = XX2 + AAA*(XO2-XX2)
           YM2 = YY2 + AAA*(YO2-YY2)
           ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
           XS = XI + AAA*(XOI-XI)
           YS = YI + AAA*(YOI-YI)
           ZS = ZI + AAA*(ZOI-ZI)

           XM5 = XX5 + AAA*(XO5-XX5)
           YM5 = YY5 + AAA*(YO5-YY5)
           ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
           X51 = XM1 - XM5
           Y51 = YM1 - YM5
           Z51 = ZM1 - ZM5
 
           X52 = XM2 - XM5
           Y52 = YM2 - YM5
           Z52 = ZM2 - ZM5
 
           XI1 = XM1 - XS
           YI1 = YM1 - YS
           ZI1 = ZM1 - ZS
 
           XI2 = XM2 - XS
           YI2 = YM2 - YS
           ZI2 = ZM2 - ZS
         
           XI5 = XM5 - XS
           YI5 = YM5 - YS
           ZI5 = ZM5 - ZS

           SX0 = Y51*Z52 - Z51*Y52
           SY0 = Z51*X52 - X51*Z52
           SZ0 = X51*Y52 - Y51*X52

           SX1 = YI5*ZI1 - ZI5*YI1
           SY1 = ZI5*XI1 - XI5*ZI1
           SZ1 = XI5*YI1 - YI5*XI1

           SX5 = YI1*ZI2 - ZI1*YI2
           SY5 = ZI1*XI2 - XI1*ZI2
           SZ5 = XI1*YI2 - YI1*XI2

           SX2 = YI2*ZI5 - ZI2*YI5
           SY2 = ZI2*XI5 - XI2*ZI5
           SZ2 = XI2*YI5 - YI2*XI5
           
           S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
           LB0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
           LC0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
           LA0 =  ONE-LB0-LC0 

          IF(IXX3 /= IXX4)THEN
           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .        LC0 >= ZEROM .and. LC0 <= UNP .and.
     .        LA0 >= ZEROM .and. LA0 <= UNP)THEN
             IF(VO12 <= ZERO .AND. V12 >= ZERO)THEN
               INTERSECA = 1
             ELSE ! (VO12 > ZERO .AND. V12 < ZERO)  .OR. 
                  ! (VO12 > ZERO .AND. V12 == ZERO) .OR. 
                  ! (VO12 == ZERO .AND. V12 < ZERO)
               INTERSECB = 1
             END IF
           ENDIF
C----------loose tolerance for tria           
          ELSE
           IF(LB0 >= ZEROMT .AND. LB0 <= UNPT .AND.
     .        LC0 >= ZEROMT .AND. LC0 <= UNPT .AND.
     .        LA0 >= ZEROMT .AND. LA0 <= UNPT)THEN
             IF(VO12 <= ZERO .AND. V12 >= ZERO)THEN
               INTERSECA = 1
             ELSE ! (VO12 > ZERO .AND. V12 < ZERO)  .OR. 
                  ! (VO12 > ZERO .AND. V12 == ZERO) .OR. 
                  ! (VO12 == ZERO .AND. V12 < ZERO)
               INTERSECB = 1
             ENDIF
           ENDIF
          END IF! (IXX(I,3) /= IXX(I,4))THEN
         ENDIF
         IF(IXX3 /= IXX4)THEN
           IF(VO23 * V23 <= ZERO)THEN
             IF(ABS(VO23) < EM20)THEN
               AAA = ONE
             ELSEIF(ABS(V23) < EM20)THEN
               AAA = ZERO
             ELSE
               AAA = V23 / (V23-VO23)
             ENDIF

             XM2 = XX2 + AAA*(XO2-XX2)
             YM2 = YY2 + AAA*(YO2-YY2)
             ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
             XM3 = XX3 + AAA*(XO3-XX3)
             YM3 = YY3 + AAA*(YO3-YY3)
             ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
             XS = XI + AAA*(XOI-XI)
             YS = YI + AAA*(YOI-YI)
             ZS = ZI + AAA*(ZOI-ZI)

             XM5 = XX5 + AAA*(XO5-XX5)
             YM5 = YY5 + AAA*(YO5-YY5)
             ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
             X52 = XM2 - XM5
             Y52 = YM2 - YM5
             Z52 = ZM2 - ZM5
 
             X53 = XM3 - XM5
             Y53 = YM3 - YM5
             Z53 = ZM3 - ZM5
 
             XI2 = XM2 - XS
             YI2 = YM2 - YS
             ZI2 = ZM2 - ZS
 
             XI3 = XM3 - XS
             YI3 = YM3 - YS
             ZI3 = ZM3 - ZS
         
             XI5 = XM5 - XS
             YI5 = YM5 - YS
             ZI5 = ZM5 - ZS

             SX0 = Y52*Z53 - Z52*Y53
             SY0 = Z52*X53 - X52*Z53
             SZ0 = X52*Y53 - Y52*X53

             SX2 = YI5*ZI2 - ZI5*YI2
             SY2 = ZI5*XI2 - XI5*ZI2
             SZ2 = XI5*YI2 - YI5*XI2

             SX5 = YI2*ZI3 - ZI2*YI3
             SY5 = ZI2*XI3 - XI2*ZI3
             SZ5 = XI2*YI3 - YI2*XI3

             SX3 = YI3*ZI5 - ZI3*YI5
             SY3 = ZI3*XI5 - XI3*ZI5
             SZ3 = XI3*YI5 - YI3*XI5
             
             S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
             LB0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
             LC0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
             LA0 =  ONE-LB0-LC0 

             IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .          LC0 >= ZEROM .and. LC0 <= UNP .and.
     .          LA0 >= ZEROM .and. LA0 <= UNP)THEN
               IF(VO23 <= ZERO .AND. V23 >= ZERO)THEN
                 INTERSECA = 1
               ELSE ! (VO23 > ZERO .AND. V23 < ZERO)  .OR. 
                    ! (VO23 > ZERO .AND. V23 == ZERO) .OR. 
                    ! (VO23 == ZERO .AND. V23 < ZERO)
                 INTERSECB = 1
               END IF
             ENDIF
           ENDIF
           IF(VO34 * V34 <= ZERO)THEN
            IF(ABS(VO34) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V34) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V34 / (V34-VO34)
            ENDIF

            XM3 = XX3 + AAA*(XO3-XX3)
            YM3 = YY3 + AAA*(YO3-YY3)
            ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4)
         
            XS = XI + AAA*(XOI-XI)
            YS = YI + AAA*(YOI-YI)
            ZS = ZI + AAA*(ZOI-ZI)

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X53 = XM3 - XM5
            Y53 = YM3 - YM5
            Z53 = ZM3 - ZM5
 
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            XI3 = XM3 - XS
            YI3 = YM3 - YS
            ZI3 = ZM3 - ZS
 
            XI4 = XM4 - XS
            YI4 = YM4 - YS
            ZI4 = ZM4 - ZS
         
            XI5 = XM5 - XS
            YI5 = YM5 - YS
            ZI5 = ZM5 - ZS

            SX0 = Y53*Z54 - Z53*Y54
            SY0 = Z53*X54 - X53*Z54
            SZ0 = X53*Y54 - Y53*X54

            SX3 = YI5*ZI3 - ZI5*YI3
            SY3 = ZI5*XI3 - XI5*ZI3
            SZ3 = XI5*YI3 - YI5*XI3

            SX5 = YI3*ZI4 - ZI3*YI4
            SY5 = ZI3*XI4 - XI3*ZI4
            SZ5 = XI3*YI4 - YI3*XI4

            SX4 = YI4*ZI5 - ZI4*YI5
            SY4 = ZI4*XI5 - XI4*ZI5
            SZ4 = XI4*YI5 - YI4*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LC0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
            LA0 =  ONE-LB0-LC0 

           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               IF(VO34 <= ZERO .AND. V34 >= ZERO)THEN
                 INTERSECA = 1
               ELSE ! (VO34 > ZERO .AND. V34 < ZERO)  .OR. 
                    ! (VO34 > ZERO .AND. V34 == ZERO) .OR. 
                    ! (VO34 == ZERO .AND. V34 < ZERO)
                 INTERSECB = 1
               END IF
            ENDIF
           ENDIF

           IF(VO41 * V41 <= ZERO)THEN
            IF(ABS(VO41) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V41) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V41 / (V41-VO41)
            ENDIF

            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4) 
         
            XM1 = XX1 + AAA*(XO1-XX1)
            YM1 = YY1 + AAA*(YO1-YY1)
            ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
            XS = XI + AAA*(XOI-XI)
            YS = YI + AAA*(YOI-YI)
            ZS = ZI + AAA*(ZOI-ZI)

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            X51 = XM1 - XM5
            Y51 = YM1 - YM5
            Z51 = ZM1 - ZM5
 
            XI4 = XM4 - XS
            YI4 = YM4 - YS
            ZI4 = ZM4 - ZS
 
            XI1 = XM1 - XS
            YI1 = YM1 - YS
            ZI1 = ZM1 - ZS
         
            XI5 = XM5 - XS
            YI5 = YM5 - YS
            ZI5 = ZM5 - ZS

            SX0 = Y54*Z51 - Z54*Y51
            SY0 = Z54*X51 - X54*Z51
            SZ0 = X54*Y51 - Y54*X51

            SX4 = YI5*ZI4 - ZI5*YI4
            SY4 = ZI5*XI4 - XI5*ZI4
            SZ4 = XI5*YI4 - YI5*XI4

            SX5 = YI4*ZI1 - ZI4*YI1
            SY5 = ZI4*XI1 - XI4*ZI1
            SZ5 = XI4*YI1 - YI4*XI1

            SX1 = YI1*ZI5 - ZI1*YI5
            SY1 = ZI1*XI5 - XI1*ZI5
            SZ1 = XI1*YI5 - YI1*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
            LC0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LA0 =  ONE-LB0-LC0 

            IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               IF(VO41 <= ZERO .AND. V41 >= ZERO)THEN
                 INTERSECA = 1
               ELSE ! (VO41 > ZERO .AND. V41 < ZERO)  .OR. 
                    ! (VO41 > ZERO .AND. V41 == ZERO) .OR. 
                    ! (VO41 == ZERO .AND. V41 < ZERO)
                 INTERSECB = 1
               END IF
            ENDIF
          ENDIF
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  INTERSECV_25                  source/interfaces/int25/i25dst3_22.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERSECV_25(
     1                    IXX3     ,IXX4  ,SUBTRIA,
     1                    INTERSECT,V12   ,V23    ,V34   ,V41    ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3                    YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4                    XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5                    ZZ4      ,XX5   ,YY5    ,ZZ5   ,
     6                    VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,XI    ,YI     ,
     A                    ZI      ,XOI    ,YOI    ,ZOI   ,ZEROM  ,
     B                    UNP    ,ZEROMT,UNPT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXX3  ,IXX4  ,INTERSECT, SUBTRIA
C     REAL
      my_real
     1     V12   ,V23    ,V34   ,V41    ,
     2     VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3     YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4     XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5     ZZ4      ,XX5   ,YY5    ,ZZ5   ,
     6     VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7     XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8     YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9     ZO3     ,ZO4    ,ZO5    ,ZEROM ,UNP    ,
     A     ZEROMT  ,UNPT   ,XI     ,YI    ,ZI     ,   
     B     XOI     ,YOI    ,ZOI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XS, YS, ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5,LA0,LB0,LC0
      my_real
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,AAA,S2
C------------------------------------------------
         IF(VO12 <= ZERO .AND. V12 >= ZERO)THEN
           IF(ABS(VO12) < EM20)THEN
             AAA = ONE
           ELSEIF(ABS(V12) < EM20)THEN
             AAA = ZERO
           ELSE
             AAA = V12 / (V12-VO12)
           ENDIF

           XM1 = XX1 + AAA*(XO1-XX1)
           YM1 = YY1 + AAA*(YO1-YY1)
           ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
           XM2 = XX2 + AAA*(XO2-XX2)
           YM2 = YY2 + AAA*(YO2-YY2)
           ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
           XS = XI + AAA*(XOI-XI)
           YS = YI + AAA*(YOI-YI)
           ZS = ZI + AAA*(ZOI-ZI)

           XM5 = XX5 + AAA*(XO5-XX5)
           YM5 = YY5 + AAA*(YO5-YY5)
           ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
           X51 = XM1 - XM5
           Y51 = YM1 - YM5
           Z51 = ZM1 - ZM5
 
           X52 = XM2 - XM5
           Y52 = YM2 - YM5
           Z52 = ZM2 - ZM5
 
           XI1 = XM1 - XS
           YI1 = YM1 - YS
           ZI1 = ZM1 - ZS
 
           XI2 = XM2 - XS
           YI2 = YM2 - YS
           ZI2 = ZM2 - ZS
         
           XI5 = XM5 - XS
           YI5 = YM5 - YS
           ZI5 = ZM5 - ZS

           SX0 = Y51*Z52 - Z51*Y52
           SY0 = Z51*X52 - X51*Z52
           SZ0 = X51*Y52 - Y51*X52

           SX1 = YI5*ZI1 - ZI5*YI1
           SY1 = ZI5*XI1 - XI5*ZI1
           SZ1 = XI5*YI1 - YI5*XI1

           SX5 = YI1*ZI2 - ZI1*YI2
           SY5 = ZI1*XI2 - XI1*ZI2
           SZ5 = XI1*YI2 - YI1*XI2

           SX2 = YI2*ZI5 - ZI2*YI5
           SY2 = ZI2*XI5 - XI2*ZI5
           SZ2 = XI2*YI5 - YI2*XI5
           
           S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
           LB0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
           LC0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
           LA0 =  ONE-LB0-LC0 

          IF(IXX3 /= IXX4)THEN
           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .        LC0 >= ZEROM .and. LC0 <= UNP .and.
     .        LA0 >= ZEROM .and. LA0 <= UNP)THEN
             IF(VO12 <= ZERO .AND. V12 >= ZERO)THEN
               INTERSECT = 1
             ELSE ! (VO12 > ZERO .AND. V12 < ZERO)  .OR. 
                  ! (VO12 > ZERO .AND. V12 == ZERO) .OR. 
                  ! (VO12 == ZERO .AND. V12 < ZERO)
               INTERSECT = -1
             END IF
             SUBTRIA= 1 ! subtriangle
           ENDIF
C----------loose tolerance for tria           
          ELSE
           IF(LB0 >= ZEROMT .AND. LB0 <= UNPT .AND.
     .        LC0 >= ZEROMT .AND. LC0 <= UNPT .AND.
     .        LA0 >= ZEROMT .AND. LA0 <= UNPT)THEN
             INTERSECT = 1
             SUBTRIA= 1 ! subtriangle
           ENDIF
          END IF! (IXX(I,3) /= IXX(I,4))THEN
         ENDIF
         IF(IXX3 /= IXX4)THEN
           IF(VO23 <= ZERO .AND. V23 >= ZERO)THEN
             IF(ABS(VO23) < EM20)THEN
               AAA = ONE
             ELSEIF(ABS(V23) < EM20)THEN
               AAA = ZERO
             ELSE
               AAA = V23 / (V23-VO23)
             ENDIF

             XM2 = XX2 + AAA*(XO2-XX2)
             YM2 = YY2 + AAA*(YO2-YY2)
             ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
             XM3 = XX3 + AAA*(XO3-XX3)
             YM3 = YY3 + AAA*(YO3-YY3)
             ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
             XS = XI + AAA*(XOI-XI)
             YS = YI + AAA*(YOI-YI)
             ZS = ZI + AAA*(ZOI-ZI)

             XM5 = XX5 + AAA*(XO5-XX5)
             YM5 = YY5 + AAA*(YO5-YY5)
             ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
             X52 = XM2 - XM5
             Y52 = YM2 - YM5
             Z52 = ZM2 - ZM5
 
             X53 = XM3 - XM5
             Y53 = YM3 - YM5
             Z53 = ZM3 - ZM5
 
             XI2 = XM2 - XS
             YI2 = YM2 - YS
             ZI2 = ZM2 - ZS
 
             XI3 = XM3 - XS
             YI3 = YM3 - YS
             ZI3 = ZM3 - ZS
         
             XI5 = XM5 - XS
             YI5 = YM5 - YS
             ZI5 = ZM5 - ZS

             SX0 = Y52*Z53 - Z52*Y53
             SY0 = Z52*X53 - X52*Z53
             SZ0 = X52*Y53 - Y52*X53

             SX2 = YI5*ZI2 - ZI5*YI2
             SY2 = ZI5*XI2 - XI5*ZI2
             SZ2 = XI5*YI2 - YI5*XI2

             SX5 = YI2*ZI3 - ZI2*YI3
             SY5 = ZI2*XI3 - XI2*ZI3
             SZ5 = XI2*YI3 - YI2*XI3

             SX3 = YI3*ZI5 - ZI3*YI5
             SY3 = ZI3*XI5 - XI3*ZI5
             SZ3 = XI3*YI5 - YI3*XI5
             
             S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
             LB0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
             LC0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
             LA0 =  ONE-LB0-LC0 

             IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .          LC0 >= ZEROM .and. LC0 <= UNP .and.
     .          LA0 >= ZEROM .and. LA0 <= UNP)THEN
               IF(VO23 <= ZERO .AND. V23 >= ZERO)THEN
                 INTERSECT = 1
               ELSE ! (VO23 > ZERO .AND. V23 < ZERO)  .OR. 
                    ! (VO23 > ZERO .AND. V23 == ZERO) .OR. 
                    ! (VO23 == ZERO .AND. V23 < ZERO)
                 INTERSECT = -1
               END IF

             ENDIF
          ENDIF
           IF(VO34 <= ZERO .AND. V34 >= ZERO)THEN
            IF(ABS(VO34) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V34) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V34 / (V34-VO34)
            ENDIF

            XM3 = XX3 + AAA*(XO3-XX3)
            YM3 = YY3 + AAA*(YO3-YY3)
            ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4)
         
            XS = XI + AAA*(XOI-XI)
            YS = YI + AAA*(YOI-YI)
            ZS = ZI + AAA*(ZOI-ZI)

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X53 = XM3 - XM5
            Y53 = YM3 - YM5
            Z53 = ZM3 - ZM5
 
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            XI3 = XM3 - XS
            YI3 = YM3 - YS
            ZI3 = ZM3 - ZS
 
            XI4 = XM4 - XS
            YI4 = YM4 - YS
            ZI4 = ZM4 - ZS
         
            XI5 = XM5 - XS
            YI5 = YM5 - YS
            ZI5 = ZM5 - ZS

            SX0 = Y53*Z54 - Z53*Y54
            SY0 = Z53*X54 - X53*Z54
            SZ0 = X53*Y54 - Y53*X54

            SX3 = YI5*ZI3 - ZI5*YI3
            SY3 = ZI5*XI3 - XI5*ZI3
            SZ3 = XI5*YI3 - YI5*XI3

            SX5 = YI3*ZI4 - ZI3*YI4
            SY5 = ZI3*XI4 - XI3*ZI4
            SZ5 = XI3*YI4 - YI3*XI4

            SX4 = YI4*ZI5 - ZI4*YI5
            SY4 = ZI4*XI5 - XI4*ZI5
            SZ4 = XI4*YI5 - YI4*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LC0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
            LA0 =  ONE-LB0-LC0 

           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               IF(VO34 <= ZERO .AND. V34 >= ZERO)THEN
                 INTERSECT = 1
               ELSE ! (VO34 > ZERO .AND. V34 < ZERO)  .OR. 
                    ! (VO34 > ZERO .AND. V34 == ZERO) .OR. 
                    ! (VO34 == ZERO .AND. V34 < ZERO)
                 INTERSECT = -1
               END IF
            ENDIF
          ENDIF

           IF(VO41 <= ZERO .AND. V41 >= ZERO)THEN
            IF(ABS(VO41) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V41) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V41 / (V41-VO41)
            ENDIF

            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4) 
         
            XM1 = XX1 + AAA*(XO1-XX1)
            YM1 = YY1 + AAA*(YO1-YY1)
            ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
            XS = XI + AAA*(XOI-XI)
            YS = YI + AAA*(YOI-YI)
            ZS = ZI + AAA*(ZOI-ZI)

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            X51 = XM1 - XM5
            Y51 = YM1 - YM5
            Z51 = ZM1 - ZM5
 
            XI4 = XM4 - XS
            YI4 = YM4 - YS
            ZI4 = ZM4 - ZS
 
            XI1 = XM1 - XS
            YI1 = YM1 - YS
            ZI1 = ZM1 - ZS
         
            XI5 = XM5 - XS
            YI5 = YM5 - YS
            ZI5 = ZM5 - ZS

            SX0 = Y54*Z51 - Z54*Y51
            SY0 = Z54*X51 - X54*Z51
            SZ0 = X54*Y51 - Y54*X51

            SX4 = YI5*ZI4 - ZI5*YI4
            SY4 = ZI5*XI4 - XI5*ZI4
            SZ4 = XI5*YI4 - YI5*XI4

            SX5 = YI4*ZI1 - ZI4*YI1
            SY5 = ZI4*XI1 - XI4*ZI1
            SZ5 = XI4*YI1 - YI4*XI1

            SX1 = YI1*ZI5 - ZI1*YI5
            SY1 = ZI1*XI5 - XI1*ZI5
            SZ1 = XI1*YI5 - YI1*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
            LC0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LA0 =  ONE-LB0-LC0 

            IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               IF(VO41 <= ZERO .AND. V41 >= ZERO)THEN
                 INTERSECT = 1
               ELSE ! (VO41 > ZERO .AND. V41 < ZERO)  .OR. 
                    ! (VO41 > ZERO .AND. V41 == ZERO) .OR. 
                    ! (VO41 == ZERO .AND. V41 < ZERO)
                 INTERSECT = -1
               END IF
            ENDIF
          ENDIF
         ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  INTERSECV0_25                 source/interfaces/int25/i25dst3_22.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERSECV0_25(
     1                    IXX3     ,IXX4  ,
     1                    INTERSECT,V12   ,V23    ,V34   ,V41    ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3                    YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4                    XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5                    ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6                    XOI      ,YOI   ,ZOI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,XI    ,YI     ,
     A                    ZI      ,ZEROMT , UNPT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXX3  ,IXX4  ,INTERSECT
C     REAL
      my_real
     1     V12   ,V23    ,V34   ,V41    ,
     2     VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3     YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4     XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5     ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6     XOI      ,YOI   ,ZOI    ,XO1   ,XO2    ,
     7     XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8     YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9     ZO3     ,ZO4    ,ZO5    ,ZEROMT,UNPT   ,
     A     XI      ,YI     ,ZI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XS,YS,ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5,LA0,LB0,LC0
      my_real
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,AAA,ADT,S2
C------------------------------------------------
C---------------- cas V>0 V_old>0
         IF(VO12 > ZERO .and. V12 >= ZERO )THEN
           AAA = ONE
           ADT = DT1

           XM1 = XO1
           YM1 = YO1
           ZM1 = ZO1 
         
           XM2 = XO2
           YM2 = YO2
           ZM2 = ZO2 
         
c           XOI = XI - ADT*VXI
c           YOI = YI - ADT*VYI
c           ZOI = ZI - ADT*VZI

           XM5 = XO5
           YM5 = YO5
           ZM5 = ZO5
         
           X51 = XM1 - XM5
           Y51 = YM1 - YM5
           Z51 = ZM1 - ZM5
 
           X52 = XM2 - XM5
           Y52 = YM2 - YM5
           Z52 = ZM2 - ZM5
 
           XI1 = XM1 - XOI
           YI1 = YM1 - YOI
           ZI1 = ZM1 - ZOI
 
           XI2 = XM2 - XOI
           YI2 = YM2 - YOI
           ZI2 = ZM2 - ZOI
         
           XI5 = XM5 - XOI
           YI5 = YM5 - YOI
           ZI5 = ZM5 - ZOI

           SX0 = Y51*Z52 - Z51*Y52
           SY0 = Z51*X52 - X51*Z52
           SZ0 = X51*Y52 - Y51*X52

           SX1 = YI5*ZI1 - ZI5*YI1
           SY1 = ZI5*XI1 - XI5*ZI1
           SZ1 = XI5*YI1 - YI5*XI1

           SX5 = YI1*ZI2 - ZI1*YI2
           SY5 = ZI1*XI2 - XI1*ZI2
           SZ5 = XI1*YI2 - YI1*XI2

           SX2 = YI2*ZI5 - ZI2*YI5
           SY2 = ZI2*XI5 - XI2*ZI5
           SZ2 = XI2*YI5 - YI2*XI5
           
           S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
           LB0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
           LC0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
           LA0 =  ONE-LB0-LC0 

           IF(LB0 >= ZEROMT .AND. LB0 <= UNPT .AND.
     .        LC0 >= ZEROMT .AND. LC0 <= UNPT .AND.
     .        LA0 >= ZEROMT .AND. LA0 <= UNPT)THEN
             INTERSECT = 1
             ADT0= ADT
           ENDIF
         ENDIF
         IF(IXX3 /= IXX4)THEN
           AAA = ONE
           ADT = DT1

             XM2 = XO2
             YM2 = YO2
             ZM2 = ZO2 
         
             XM3 = XO3
             YM3 = YO3
             ZM3 = ZO3 
         
c             XOI = XI - ADT*VXI
c             YOI = YI - ADT*VYI
c             ZOI = ZI - ADT*VZI

             XM5 = XO5
             YM5 = YO5
             ZM5 = ZO5
         
             X52 = XM2 - XM5
             Y52 = YM2 - YM5
             Z52 = ZM2 - ZM5
 
             X53 = XM3 - XM5
             Y53 = YM3 - YM5
             Z53 = ZM3 - ZM5
 
             XI2 = XM2 - XOI
             YI2 = YM2 - YOI
             ZI2 = ZM2 - ZOI
 
             XI3 = XM3 - XOI
             YI3 = YM3 - YOI
             ZI3 = ZM3 - ZOI
         
             XI5 = XM5 - XOI
             YI5 = YM5 - YOI
             ZI5 = ZM5 - ZOI

             SX0 = Y52*Z53 - Z52*Y53
             SY0 = Z52*X53 - X52*Z53
             SZ0 = X52*Y53 - Y52*X53

             SX2 = YI5*ZI2 - ZI5*YI2
             SY2 = ZI5*XI2 - XI5*ZI2
             SZ2 = XI5*YI2 - YI5*XI2

             SX5 = YI2*ZI3 - ZI2*YI3
             SY5 = ZI2*XI3 - XI2*ZI3
             SZ5 = XI2*YI3 - YI2*XI3

             SX3 = YI3*ZI5 - ZI3*YI5
             SY3 = ZI3*XI5 - XI3*ZI5
             SZ3 = XI3*YI5 - YI3*XI5
             
             S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
             LB0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
             LC0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
             LA0 =  ONE-LB0-LC0 

             IF(LB0 >= ZEROMT.and. LB0 <= UNPT .and.
     .          LC0 >= ZEROMT.and. LC0 <= UNPT .and.
     .          LA0 >= ZEROMT.and. LA0 <= UNPT)THEN
               INTERSECT = 1

               IF(ADT > ADT0)THEN
                 ADT0= ADT
               ENDIF
             ENDIF
          ENDIF
          IF(VO34 > ZERO .and. V34 >= ZERO )THEN
           AAA = ONE
           ADT = DT1

            XM3 = XO3
            YM3 = YO3
            ZM3 = ZO3 
         
            XM4 = XO4
            YM4 = YO4
            ZM4 = ZO4
         
c            XOI = XI - ADT*VXI
c            YOI = YI - ADT*VYI
c            ZOI = ZI - ADT*VZI

            XM5 = XO5
            YM5 = YO5
            ZM5 = ZO5
         
            X53 = XM3 - XM5
            Y53 = YM3 - YM5
            Z53 = ZM3 - ZM5
 
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            XI3 = XM3 - XOI
            YI3 = YM3 - YOI
            ZI3 = ZM3 - ZOI
 
            XI4 = XM4 - XOI
            YI4 = YM4 - YOI
            ZI4 = ZM4 - ZOI
         
            XI5 = XM5 - XOI
            YI5 = YM5 - YOI
            ZI5 = ZM5 - ZOI

            SX0 = Y53*Z54 - Z53*Y54
            SY0 = Z53*X54 - X53*Z54
            SZ0 = X53*Y54 - Y53*X54

            SX3 = YI5*ZI3 - ZI5*YI3
            SY3 = ZI5*XI3 - XI5*ZI3
            SZ3 = XI5*YI3 - YI5*XI3

            SX5 = YI3*ZI4 - ZI3*YI4
            SY5 = ZI3*XI4 - XI3*ZI4
            SZ5 = XI3*YI4 - YI3*XI4

            SX4 = YI4*ZI5 - ZI4*YI5
            SY4 = ZI4*XI5 - XI4*ZI5
            SZ4 = XI4*YI5 - YI4*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LC0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
            LA0 =  ONE-LB0-LC0 

           IF(LB0  >= ZEROMT.and. LB0 <= UNPT .and.
     .         LC0 >= ZEROMT.and. LC0 <= UNPT .and.
     .         LA0 >= ZEROMT.and. LA0 <= UNPT)THEN
               INTERSECT = 1
               IF(ADT > ADT0)THEN
                 ADT0= ADT
               ENDIF
            ENDIF
          ENDIF

          IF(VO41 > ZERO .and. V41 >= ZERO )THEN
           AAA = ONE
           ADT = DT1

            XM4 = XO4
            YM4 = YO4
            ZM4 = ZO4 
         
            XM1 = XO1
            YM1 = YO1
            ZM1 = ZO1 
         
c            XOI = XI - ADT*VXI
c            YOI = YI - ADT*VYI
c            ZOI = ZI - ADT*VZI

            XM5 = XO5
            YM5 = YO5
            ZM5 = ZO5
         
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            X51 = XM1 - XM5
            Y51 = YM1 - YM5
            Z51 = ZM1 - ZM5
 
            XI4 = XM4 - XOI
            YI4 = YM4 - YOI
            ZI4 = ZM4 - ZOI
 
            XI1 = XM1 - XOI
            YI1 = YM1 - YOI
            ZI1 = ZM1 - ZOI
         
            XI5 = XM5 - XOI
            YI5 = YM5 - YOI
            ZI5 = ZM5 - ZOI

            SX0 = Y54*Z51 - Z54*Y51
            SY0 = Z54*X51 - X54*Z51
            SZ0 = X54*Y51 - Y54*X51

            SX4 = YI5*ZI4 - ZI5*YI4
            SY4 = ZI5*XI4 - XI5*ZI4
            SZ4 = XI5*YI4 - YI5*XI4

            SX5 = YI4*ZI1 - ZI4*YI1
            SY5 = ZI4*XI1 - XI4*ZI1
            SZ5 = XI4*YI1 - YI4*XI1

            SX1 = YI1*ZI5 - ZI1*YI5
            SY1 = ZI1*XI5 - XI1*ZI5
            SZ1 = XI1*YI5 - YI1*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
            LC0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LA0 =  ONE-LB0-LC0 

            IF(LB0 >= ZEROMT.and. LB0 <= UNPT .and.
     .         LC0 >= ZEROMT.and. LC0 <= UNPT .and.
     .         LA0 >= ZEROMT.and. LA0 <= UNPT)THEN
               INTERSECT = 1
               IF(ADT > ADT0)THEN
                 ADT0= ADT
               ENDIF
            ENDIF
          ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I25GLOB_22                    source/interfaces/int25/i25dst3_22.F
Chd|-- called by -----------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25GLOB_22(
     1                  JLT    ,CAND_N_N ,CAND_E_N ,CAND_E ,
     2                  NIN    ,NSN    ,IX1    ,IX2    ,IX3    ,
     3                  IX4    ,NSVG   ,STIF   ,INACTI ,MSEGLO ,
     4                  IRTLM  ,TIME_S ,ITAB   ,SUBTRIA,
     5                  FAR    ,PENT   ,LB     ,LC     ,
     9                  INDEX  ,FARM   ,PENM   ,LBM    ,
     A                  LCM    ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "comlock.inc"
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NSN, INACTI,
     .        CAND_N_N(*), CAND_E_N(*), CAND_E(*), ITAB(*), SUBTRIA(MVSIZ)
      INTEGER NSVG(MVSIZ), IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        MSEGLO(*), IRTLM(4,NSN) ,INDEX(*), FARM(4,*),
     .        FAR(MVSIZ)
      my_real
     .     STIF(*), TIME_S(*),
     .     PENT(MVSIZ), 
     .     LB (MVSIZ), LC (MVSIZ), 
     .     PENM(4,*), LBM(4,*), LCM(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IT, I, J, L, N, MGLOB
C--------------------------------------------------------
C      Back to global arrays
C--------------------------------------------------------
      DO I=1,JLT
        J=INDEX(I)
        IT=SUBTRIA(I)

        FARM(1:4,J)=0
        PENM(1:4,J)=ZERO
        LBM(1:4,J) =ZERO
        LCM(1:4,J) =ZERO

        IF(IT/=0)THEN
          CAND_E(J)=CAND_E_N(I)

          FARM(IT,J)=FAR (I)
          PENM(IT,J)=PENT(I)
          LBM(IT,J) =LB  (I)
          LCM(IT,J) =LC  (I)
        END IF

      END DO
      RETURN
      END 
