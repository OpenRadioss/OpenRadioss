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
Chd|  I25DST3_21                    source/interfaces/int25/i25dst3_21.F
Chd|-- called by -----------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25DST3_21(
     1                  JLT    ,CAND_N ,CAND_E ,NRTM   ,XX     ,
     2                  YY     ,ZZ     ,XI     ,YI     ,ZI     ,
     3                  NIN    ,NSN    ,IX1    ,IX2    ,IX3    ,
     4                  IX4    ,NSVG   ,STIF   ,INACTI ,MSEGLO ,
     5                  GAPS   ,GAPM   ,IRECT  ,IRTLM  ,TIME_S ,
     6                  GAP_NM ,ITAB   ,NNX    ,NNY    ,NNZ    ,
     7                  FAR    ,PENT   ,DIST   ,LB     ,LC     ,
     8                  LBP    ,LCP    ,KSLIDE ,MVOISN ,GAPMXL ,
     9                  IBOUND ,VTX_BISECTOR ,ETYP ,ICODT ,ISKEW,
     A                  DRAD   )
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
      INTEGER JLT, NIN, NSN, INACTI, NRTM,
     .        CAND_N(*),
     .        CAND_E(*),NSVG(MVSIZ), ETYP(*), ICODT(*), ISKEW(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INTTH,MSEGLO(*),IRTLM(4,NSN), KSLIDE(MVSIZ,4), MVOISN(MVSIZ,4), 
     .        IBOUND(4,MVSIZ)
      my_real
     .     DRAD ,
     .     TIME_S(2,NSN), GAPS(*), GAPM(*)
      my_real
     .     XX(MVSIZ,5),YY(MVSIZ,5),ZZ(MVSIZ,5), 
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5),
     .     PENT(MVSIZ,4), DIST(MVSIZ), LB(MVSIZ,4), LC(MVSIZ,4), 
     .     LBP(MVSIZ,4), LCP(MVSIZ,4), GAP_NM(4,MVSIZ), GAPMXL(MVSIZ)
      INTEGER IRECT(4,*),ITAB(*),FAR(MVSIZ,4)
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
      INTEGER I, J, K, L, N, ITQ, N4N, IKEEP, MGLOB, IB1, IB2, IB3, IBX, IX, IY, IZ
      INTEGER I4N(MVSIZ),
     .        INGAP(MVSIZ,4),
     .        IT, JJ, I1, I2, ITRIA(2,4), SUBTRIA(MVSIZ),
     .        IBC21, IBC22, IBC23, IBCS, ISKS, IBCM(4), ISKM(4), 
     .        IBCX(MVSIZ), IBCY(MVSIZ), IBCZ(MVSIZ)
      my_real
     .     XIJ(MVSIZ,4),  XI0V(MVSIZ),  XI5,
     .     YIJ(MVSIZ,4),  YI0V(MVSIZ),  YI5,
     .     ZIJ(MVSIZ,4),  ZI0V(MVSIZ),  ZI5,
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XO1, XO2, XO3, XO4, XO5, XOI,
     .     YO1, YO2, YO3, YO4, YO5, YOI,
     .     ZO1, ZO2, ZO3, ZO4, ZO5, ZOI,
     .     VO12, VO23, VO34, VO41, PENE
      my_real
     .     GAP_MM(MVSIZ), GAP, GAP2,
     .     XP, YP, ZP,
     .     DX, DY, DZ, DMIN, DD(MVSIZ,4), 
     .     LAP, HLA, HLB(MVSIZ,4), HLC(MVSIZ,4),
     .     AL(MVSIZ,4)
      my_real
     .     UNP,ZEROM,EPS,UNPT,ZEROMT,AAA,BB(MVSIZ,4),
     .     SX1,SX2,SX3,SX4,
     .     SY1,SY2,SY3,SY4,
     .     SZ1,SZ2,SZ3,SZ4,
     .     X0N(MVSIZ,4), Y0N(MVSIZ,4), Z0N(MVSIZ,4), 
     .     XN(MVSIZ,4),YN(MVSIZ,4),ZN(MVSIZ,4),
     .     SIDE(MVSIZ,4), 
     .     X12(MVSIZ), Y12(MVSIZ), Z12(MVSIZ), 
     .     PX, PY, PZ, PP, P1, P2, XH, YH, ZH, D1, D2, D3, VX, VY, VZ,
     .     LL ,NN, PN, LD(MVSIZ), LX, LAX, LBX, LCX,
     .     PREC
      INTEGER ISHEL(MVSIZ)
      DATA ITRIA/1,2,2,3,3,4,4,1/
C-----------------------------------------------------------------------
C
C initialisation (cf triangles)
      FAR(1:JLT,1:4)  = 0
      PENT(1:JLT,1:4) = EP20
      DD  (1:JLT,1:4) = EP20
      DIST(1:JLT)= EP20
      LD(1:JLT)  = EP20
C
      DO I=1,JLT
C
C       For computing LBP, LCP, FAR=2, etc
C
        IF(STIF(I) == ZERO)CYCLE
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
        IF(IX3(I)/=IX4(I))THEN
          GAP_MM(I)=FOURTH*(GAP_NM(1,I)+GAP_NM(2,I)+GAP_NM(3,I)+GAP_NM(4,I))
        ELSE
          GAP_MM(I)=GAP_NM(3,I)
        END IF
      ENDDO
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
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
        XN(I,1) = Y0N(I,1)*Z0N(I,2) - Z0N(I,1)*Y0N(I,2)
        YN(I,1) = Z0N(I,1)*X0N(I,2) - X0N(I,1)*Z0N(I,2)
        ZN(I,1) = X0N(I,1)*Y0N(I,2) - Y0N(I,1)*X0N(I,2)
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,1)*XN(I,1)+ YN(I,1)*YN(I,1)+ ZN(I,1)*ZN(I,1)))
        XN(I,1)=XN(I,1)*NN
        YN(I,1)=YN(I,1)*NN
        ZN(I,1)=ZN(I,1)*NN
        LB(I,1) = -(XN(I,1)*SX2 + YN(I,1)*SY2 + ZN(I,1)*SZ2)*NN
        LC(I,1) =  (XN(I,1)*SX1 + YN(I,1)*SY1 + ZN(I,1)*SZ1)*NN
C
        SX3 = YI0V(I)*ZIJ(I,3) - ZI0V(I)*YIJ(I,3)
        SY3 = ZI0V(I)*XIJ(I,3) - XI0V(I)*ZIJ(I,3)
        SZ3 = XI0V(I)*YIJ(I,3) - YI0V(I)*XIJ(I,3)
C
        XN(I,2) = Y0N(I,2)*Z0N(I,3) - Z0N(I,2)*Y0N(I,3)
        YN(I,2) = Z0N(I,2)*X0N(I,3) - X0N(I,2)*Z0N(I,3)
        ZN(I,2) = X0N(I,2)*Y0N(I,3) - Y0N(I,2)*X0N(I,3)
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,2)*XN(I,2)+ YN(I,2)*YN(I,2)+ ZN(I,2)*ZN(I,2)))
        XN(I,2)=XN(I,2)*NN
        YN(I,2)=YN(I,2)*NN
        ZN(I,2)=ZN(I,2)*NN
        LB(I,2) = -(XN(I,2)*SX3 + YN(I,2)*SY3 + ZN(I,2)*SZ3)*NN
        LC(I,2) =  (XN(I,2)*SX2 + YN(I,2)*SY2 + ZN(I,2)*SZ2)*NN
C
        SX4 = YI0V(I)*ZIJ(I,4) - ZI0V(I)*YIJ(I,4)
        SY4 = ZI0V(I)*XIJ(I,4) - XI0V(I)*ZIJ(I,4)
        SZ4 = XI0V(I)*YIJ(I,4) - YI0V(I)*XIJ(I,4)
C
        XN(I,3) = Y0N(I,3)*Z0N(I,4) - Z0N(I,3)*Y0N(I,4)
        YN(I,3) = Z0N(I,3)*X0N(I,4) - X0N(I,3)*Z0N(I,4)
        ZN(I,3) = X0N(I,3)*Y0N(I,4) - Y0N(I,3)*X0N(I,4)
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,3)*XN(I,3)+ YN(I,3)*YN(I,3)+ ZN(I,3)*ZN(I,3)))
        XN(I,3)=XN(I,3)*NN
        YN(I,3)=YN(I,3)*NN
        ZN(I,3)=ZN(I,3)*NN
        LB(I,3) = -(XN(I,3)*SX4 + YN(I,3)*SY4 + ZN(I,3)*SZ4)*NN
        LC(I,3) =  (XN(I,3)*SX3 + YN(I,3)*SY3 + ZN(I,3)*SZ3)*NN
C
        XN(I,4) = Y0N(I,4)*Z0N(I,1) - Z0N(I,4)*Y0N(I,1)
        YN(I,4) = Z0N(I,4)*X0N(I,1) - X0N(I,4)*Z0N(I,1)
        ZN(I,4) = X0N(I,4)*Y0N(I,1) - Y0N(I,4)*X0N(I,1)
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
#include  "vectorize.inc"
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
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
      INGAP(1:JLT,1:4) = 0
C--------------------------------------------------------
      IT=1
      I1=ITRIA(1,IT)
      I2=ITRIA(2,IT)
#include  "vectorize.inc"
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
C
        X12(I) = XX(I,I2) - XX(I,I1)
        Y12(I) = YY(I,I2) - YY(I,I1)
        Z12(I) = ZZ(I,I2) - ZZ(I,I1)
C
        LBP(I,IT) = LB(I,IT)
        LCP(I,IT) = LC(I,IT)
        LAP       = ONE-LBP(I,IT)-LCP(I,IT)
C       HLA, HLB, HLC necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I))
        HLA= LAP*ABS(LAP) * AAA
        IF(LAP.LT.ZERO.AND.
     +     HLA.LE.HLB(I,IT).AND.HLA.LE.HLC(I,IT))THEN
         LBP(I,IT) = (XIJ(I,I2)*X12(I)+YIJ(I,I2)*Y12(I)+ZIJ(I,I2)*Z12(I)) * AAA
         LBP(I,IT) = MAX(ZERO,MIN(ONE,LBP(I,IT)))
         LCP(I,IT) = ONE - LBP(I,IT)
        ELSEIF(LBP(I,IT).LT.ZERO.AND.
     +         HLB(I,IT).LE.HLC(I,IT).AND.HLB(I,IT).LE.HLA)THEN
         LBP(I,IT) = ZERO
         LCP(I,IT) = AL(I,I2)
        ELSEIF(LCP(I,IT).LT.ZERO.AND.
     +         HLC(I,IT).LE.HLA.AND.HLC(I,IT).LE.HLB(I,IT))THEN
         LCP(I,IT) = ZERO
         LBP(I,IT) = AL(I,I1)
        ENDIF

        LAP = ONE-LBP(I,IT)-LCP(I,IT)
        XP  =LAP*XX(I,5)+LBP(I,IT)*XX(I,I1) + LCP(I,IT)*XX(I,I2)
        YP  =LAP*YY(I,5)+LBP(I,IT)*YY(I,I1) + LCP(I,IT)*YY(I,I2)
        ZP  =LAP*ZZ(I,5)+LBP(I,IT)*ZZ(I,I1) + LCP(I,IT)*ZZ(I,I2)
        DX  =XI(I)-XP
        DY  =YI(I)-YP
        DZ  =ZI(I)-ZP
        DD(I,IT)=DX*DX+DY*DY+DZ*DZ

      END DO
C--------------------------------------------------------
C     Verifie si penetre vs gap cylindrique ...
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I =1,JLT
C          
        IF (STIF(I) == ZERO)CYCLE
C
        LAP  = ONE-LBP(I,IT)-LCP(I,IT)
C
        GAP  = MIN(MAX(DRAD,GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I)),MAX(DRAD,GAPMXL(I)))
        GAP2 = GAP**2
C
        BB(I,IT) =((XX(I,5)-XI(I))*XN(I,IT)+(YY(I,5)-YI(I))*YN(I,IT)+(ZZ(I,5)-ZI(I))*ZN(I,IT))
C
        IF(DD(I,IT) <= GAP2 .AND. BB(I,IT) <= ZERO) INGAP(I,IT)=1

        IF(BB(I,IT) > ZERO)THEN
C
C         penetration approximee (cf zone limite interpolation des normales)
          PENT(I,IT)=MAX(ZERO,GAP+BB(I,IT))
        ELSE
          PENT(I,IT)=MAX(ZERO,GAP-SQRT(DD(I,IT)))
        END IF
C
      ENDDO
C--------------------------------------------------------
      N4N=0
      DO I=1,JLT
C
       IF(STIF(I) == ZERO)CYCLE
C
       IF(IX3(I).NE.IX4(I))THEN
         N4N = N4N+1
         I4N(N4N)=I
       ENDIF
      ENDDO
C--------------------------------------------------------
      DO IT=2,4

        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)

#include  "vectorize.inc"
        DO K=1,N4N
         I=I4N(K)
C
         X12(I) = XX(I,I2) - XX(I,I1)
         Y12(I) = YY(I,I2) - YY(I,I1)
         Z12(I) = ZZ(I,I2) - ZZ(I,I1)
C
         LBP(I,IT) = LB(I,IT)
         LCP(I,IT) = LC(I,IT)
         LAP       = ONE-LBP(I,IT)-LCP(I,IT)
C        HLA, HLB, HLC necessaires pour triangle angle obtu
         AAA = ONE / MAX(EM20,X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I))
         HLA= LAP*ABS(LAP) * AAA
         IF(LAP.LT.ZERO.AND.
     +      HLA.LE.HLB(I,IT).AND.HLA.LE.HLC(I,IT))THEN
          LBP(I,IT) = (XIJ(I,I2)*X12(I)+YIJ(I,I2)*Y12(I)+ZIJ(I,I2)*Z12(I)) * AAA
          LBP(I,IT) = MAX(ZERO,MIN(ONE,LBP(I,IT)))
          LCP(I,IT) = ONE - LBP(I,IT)
         ELSEIF(LBP(I,IT).LT.ZERO.AND.
     +          HLB(I,IT).LE.HLC(I,IT).AND.HLB(I,IT).LE.HLA)THEN
          LBP(I,IT) = ZERO
          LCP(I,IT) = AL(I,I2)
         ELSEIF(LCP(I,IT).LT.ZERO.AND.
     +          HLC(I,IT).LE.HLA.AND.HLC(I,IT).LE.HLB(I,IT))THEN
          LCP(I,IT) = ZERO
          LBP(I,IT) = AL(I,I1)
         ENDIF

         LAP = ONE-LBP(I,IT)-LCP(I,IT)
         XP  =LAP*XX(I,5)+LBP(I,IT)*XX(I,I1) + LCP(I,IT)*XX(I,I2)
         YP  =LAP*YY(I,5)+LBP(I,IT)*YY(I,I1) + LCP(I,IT)*YY(I,I2)
         ZP  =LAP*ZZ(I,5)+LBP(I,IT)*ZZ(I,I1) + LCP(I,IT)*ZZ(I,I2)
         DX  =XI(I)-XP
         DY  =YI(I)-YP
         DZ  =ZI(I)-ZP
         DD(I,IT)=DX*DX+DY*DY+DZ*DZ

        END DO
C--------------------------------------------------------
C       Verifie si penetre vs gap cylindrique ...
C--------------------------------------------------------
#include  "vectorize.inc"
        DO K=1,N4N
         I=I4N(K)
C
        LAP  = ONE-LBP(I,IT)-LCP(I,IT)
C
        GAP  = MIN(MAX(DRAD,GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I)),MAX(DRAD,GAPMXL(I)))
        GAP2 = GAP**2
C
        BB(I,IT) =((XX(I,5)-XI(I))*XN(I,IT)+(YY(I,5)-YI(I))*YN(I,IT)+(ZZ(I,5)-ZI(I))*ZN(I,IT))
C
        IF(DD(I,IT) <= GAP2 .AND. BB(I,IT) <= ZERO) INGAP(I,IT)=1

        IF(BB(I,IT) > ZERO)THEN
C
C         penetration approximee (cf zone limite interpolation des normales)
          PENT(I,IT)=MAX(ZERO,GAP+BB(I,IT))
        ELSE
          PENT(I,IT)=MAX(ZERO,GAP-SQRT(DD(I,IT)))
        END IF
C
       ENDDO
      END DO !  DO IT=2,4
C--------------------------------------------------------
C     Look for closest sub-triangle
C--------------------------------------------------------
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
C
C       Erase Subtria (cf intersections)
        SUBTRIA(I)=0
C
        IF(IX3(I)/=IX4(I))THEN
          DMIN=MIN(DD(I,1),DD(I,2),DD(I,3),DD(I,4))
          
          DO JJ=1,4
            IF(DD(I,JJ) <= ONEP03*DMIN)THEN
              LBX = LB(I,JJ)
              LCX = LC(I,JJ)
              LAX = ONE-LB(I,JJ)-LC(I,JJ)
C
C             Privilegier secteur dans lequel on se trouve.
              IF(LBX >= ZERO .AND. LCX >= ZERO)THEN
                LX=ZERO
              ELSE
                ! point le plus proche dans le secteur angulaire == centre
                LX  = MAX(ZERO,DD(I,JJ)-BB(I,JJ)*BB(I,JJ))
              END IF

              IF(LX < LD(I)) THEN
                SUBTRIA(I)= JJ
                DIST(I)   = DD(I,JJ)
                LD(I)     = LX
              END IF

            END IF
          END DO
        ELSE
          IF(DD(I,1) <= DIST(I))THEN
            SUBTRIA(I)= 1
            DIST(I)   = DD(I,1)
          END IF
        END IF
C
        IT = SUBTRIA(I)
        DO J=1,4
          IF(J /= IT) PENT(I,J)=ZERO
        END DO
      END DO
C--------------------------------------------------------
C     PREVENT IMPACT IN A GIVEN DIRECTION IF BOUNDARY CONDITION 
C        IN THE SAME DIRECTION FOR BOTH SECONDARY AND PRIMARY NODES
C--------------------------------------------------------
      IBCX(1:JLT)=0
      IBCY(1:JLT)=0
      IBCZ(1:JLT)=0
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
C
        IF(.NOT.(ETYP(I)==0 .OR. ETYP(I) > NRTM))CYCLE ! only solids including coating shell.
C
        IF(NSVG(I) > 0)THEN
          IBCS = ICODT(NSVG(I))
          ISKS = ISKEW(NSVG(I))
        ELSE
          N = CAND_N(I)-NSN 
          IBCS = ICODT_FI(NIN)%P(N)
          ISKS = ISKEW_FI(NIN)%P(N) 
        END IF

        IBCM(1)=ICODT(IX1(I))
        ISKM(1)=ISKEW(IX1(I))

        IBCM(2)=ICODT(IX2(I))
        ISKM(2)=ISKEW(IX2(I))

        IBCM(3)=ICODT(IX3(I))
        ISKM(3)=ISKEW(IX3(I))

        IBCM(4)=ICODT(IX4(I))
        ISKM(4)=ISKEW(IX4(I))

        IF(ISKS==1)THEN
          IF((IBCS   ==1.OR.IBCS   ==3.OR.IBCS   ==5.OR.IBCS   ==7).AND.
     .       (IBCM(1)==1.OR.IBCM(1)==3.OR.IBCM(1)==5.OR.IBCM(1)==7).AND.
     .       (IBCM(2)==1.OR.IBCM(2)==3.OR.IBCM(2)==5.OR.IBCM(2)==7).AND.
     .       (IBCM(3)==1.OR.IBCM(3)==3.OR.IBCM(3)==5.OR.IBCM(3)==7).AND.
     .       (IBCM(4)==1.OR.IBCM(4)==3.OR.IBCM(4)==5.OR.IBCM(4)==7))THEN
            IBCZ(I)=1
          END IF
          IF((IBCS   ==2.OR.IBCS   ==3.OR.IBCS   ==6.OR.IBCS   ==7).AND.
     .       (IBCM(1)==2.OR.IBCM(1)==3.OR.IBCM(1)==6.OR.IBCM(1)==7).AND.
     .       (IBCM(2)==2.OR.IBCM(2)==3.OR.IBCM(2)==6.OR.IBCM(2)==7).AND.
     .       (IBCM(3)==2.OR.IBCM(3)==3.OR.IBCM(3)==6.OR.IBCM(3)==7).AND.
     .       (IBCM(4)==2.OR.IBCM(4)==3.OR.IBCM(4)==6.OR.IBCM(4)==7))THEN
            IBCY(I)=1
          END IF
          IF((IBCS   ==4.OR.IBCS   ==5.OR.IBCS   ==6.OR.IBCS   ==7).AND.
     .       (IBCM(1)==4.OR.IBCM(1)==5.OR.IBCM(1)==6.OR.IBCM(1)==7).AND.
     .       (IBCM(2)==4.OR.IBCM(2)==5.OR.IBCM(2)==6.OR.IBCM(2)==7).AND.
     .       (IBCM(3)==4.OR.IBCM(3)==5.OR.IBCM(3)==6.OR.IBCM(3)==7).AND.
     .       (IBCM(4)==4.OR.IBCM(4)==5.OR.IBCM(4)==6.OR.IBCM(4)==7))THEN
            IBCX(I)=1
          END IF
        END IF
      END DO
C-----
      PREC=ONE-EM04
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
C
        IT = SUBTRIA(I)
        IF(PENT(I,IT) == ZERO)CYCLE
C
        IF((IBCZ(I)==1.AND.ABS(ZN(I,IT)) > PREC).OR.
     .     (IBCY(I)==1.AND.ABS(YN(I,IT)) > PREC).OR.
     .     (IBCX(I)==1.AND.ABS(XN(I,IT)) > PREC))THEN
            PENT(I,IT)=ZERO
        END IF
C
      END DO
C--------------------------------------------------------
C     Check vs bissectors ...
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I =1,JLT
C          
        IF (STIF(I) == ZERO)CYCLE
C
        IT=SUBTRIA(I)
        IF(PENT(I,IT)==ZERO) CYCLE
C
        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
C       Projection en dehors du triangle
        IF(LB(I,IT) < -EM03 .OR. LC(I,IT) < -EM03 .OR.
     .      LB(I,IT)+LC(I,IT) > ONE+EM03) FAR(I,IT)=1
C
        XH=XI(I)+BB(I,IT)*XN(I,IT)
        YH=YI(I)+BB(I,IT)*YN(I,IT)
        ZH=ZI(I)+BB(I,IT)*ZN(I,IT)
C
        IF(IX3(I) /= IX4(I))THEN
C
          IB1=IBOUND(I1,I)
          IB2=IBOUND(I2,I)
          IF(MVOISN(I,IT)==0)THEN     
C
            IF( (XH-XX(I,I1))* NNX(I,IT)+
     .          (YH-YY(I,I1))* NNY(I,IT)+
     .          (ZH-ZZ(I,I1))* NNZ(I,IT) >= GAPS(I)) FAR(I,IT) =2
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
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FAR(I,IT) =2
            ELSE
              VX = X0N(I,IX) ! fake bisector of angle at vertex IX
              VY = Y0N(I,IX)
              VZ = Z0N(I,IX)
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH-XX(I,IX))*VX+(YH-YY(I,IX))*VY+(ZH-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FAR(I,IT) =2
            END IF
C
          END IF
C
C         Cone
          IF(FAR(I,IT)==1 .OR. BB(I,IT) <= ZERO) THEN

            IF(INGAP(I,IT)  == 1 .OR.  (KSLIDE(I,I1)/=0.OR.KSLIDE(I,I2)/=0))THEN
C
C               INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
C               Glissement => utiliser le cone pour choisir entre les segments voisins
C                             mais garder le contact si le nd a glisse de plus d'un elt (cf supersonic)
C            
              X12(I)=XX(I,I2)-XX(I,I1)
              Y12(I)=YY(I,I2)-YY(I,I1)
              Z12(I)=ZZ(I,I2)-ZZ(I,I1)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12(I)*NNY(I,IT)-Y12(I)*NNZ(I,IT)  
              PY = X12(I)*NNZ(I,IT)-Z12(I)*NNX(I,IT)  
              PZ = Y12(I)*NNX(I,IT)-X12(I)*NNY(I,IT)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,I1)*XIJ(I,I1)+YIJ(I,I1)*YIJ(I,I1)+ZIJ(I,I1)*ZIJ(I,I1)
C
              SIDE(I,IT)=-(XIJ(I,I1)*PX+YIJ(I,I1)*PY+ZIJ(I,I1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))                 
              IF(SIDE(I,IT) < -ZEP01) FAR(I,IT) =2
C
            END IF

          END IF

        ELSE

          IB1=IBOUND(1,I)
          IB2=IBOUND(2,I)
          IB3=IBOUND(3,I)
C
          D1=(XH-XX(I,1))* NNX(I,1)+
     .       (YH-YY(I,1))* NNY(I,1)+
     .       (ZH-ZZ(I,1))* NNZ(I,1)
          D2=(XH-XX(I,2))* NNX(I,2)+
     .       (YH-YY(I,2))* NNY(I,2)+
     .       (ZH-ZZ(I,2))* NNZ(I,2)
          D3=(XH-XX(I,3))* NNX(I,4)+
     .       (YH-YY(I,3))* NNY(I,4)+
     .       (ZH-ZZ(I,3))* NNZ(I,4)
C
          IF( (MVOISN(I,1) == 0 .AND. D1 >= GAPS(I)).OR.
     .        (MVOISN(I,2) == 0 .AND. D2 >= GAPS(I)).OR.
     .        (MVOISN(I,4) == 0 .AND. D3 >= GAPS(I)) )THEN
C
            FAR(I,IT)=2
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
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FAR(I,IT) =2
            ELSE
              VX = TWO*XX(I,IX)-(XX(I,IY)+XX(I,IZ)) ! fake bisector of angle 2,1,3
              VY = TWO*YY(I,IX)-(YY(I,IY)+YY(I,IZ))
              VZ = TWO*ZZ(I,IX)-(ZZ(I,IY)+ZZ(I,IZ))
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH-XX(I,IX))*VX+(YH-YY(I,IX))*VY+(ZH-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FAR(I,IT) =2
            END IF
C
          END IF
C
          IF(FAR(I,IT)==1 .OR. BB(I,IT) <= ZERO) THEN

            IF(MVOISN(I,1) /= 0 .AND. (INGAP(I,IT)  == 1 .OR. (KSLIDE(I,1)/=0.OR.KSLIDE(I,2)/=0)))THEN
C
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
C             Glissement => utiliser le cone pour choisir entre les segments voisins
C                           mais garder le contact si le nd a glisse de plus d'un elt (cf supersonic)
C          
              X12(I)=XX(I,2)-XX(I,1)
              Y12(I)=YY(I,2)-YY(I,1)
              Z12(I)=ZZ(I,2)-ZZ(I,1)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12(I)*NNY(I,1)-Y12(I)*NNZ(I,1)  
              PY = X12(I)*NNZ(I,1)-Z12(I)*NNX(I,1)  
              PZ = Y12(I)*NNX(I,1)-X12(I)*NNY(I,1)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,1)*XIJ(I,1)+YIJ(I,1)*YIJ(I,1)+ZIJ(I,1)*ZIJ(I,1)
C
              SIDE(I,1)=-(XIJ(I,1)*PX+YIJ(I,1)*PY+ZIJ(I,1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDE(I,1) < -ZEP01) FAR(I,IT) =2
C
            END IF

            IF(MVOISN(I,2) /= 0 .AND. (INGAP(I,IT)  == 1 .OR. (KSLIDE(I,2)/=0.OR.KSLIDE(I,3)/=0)))THEN
C
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
C             Glissement => utiliser le cone pour choisir entre les segments voisins
C                           mais garder le contact si le nd a glisse de plus d'un elt (cf supersonic)
C            
              X12(I)=XX(I,3)-XX(I,2)
              Y12(I)=YY(I,3)-YY(I,2)
              Z12(I)=ZZ(I,3)-ZZ(I,2)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12(I)*NNY(I,2)-Y12(I)*NNZ(I,2)  
              PY = X12(I)*NNZ(I,2)-Z12(I)*NNX(I,2)  
              PZ = Y12(I)*NNX(I,2)-X12(I)*NNY(I,2)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,2)*XIJ(I,2)+YIJ(I,2)*YIJ(I,2)+ZIJ(I,2)*ZIJ(I,2)
C
              SIDE(I,2)=-(XIJ(I,2)*PX+YIJ(I,2)*PY+ZIJ(I,2)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDE(I,2) < -ZEP01) FAR(I,IT) =2
C
            END IF

            IF(MVOISN(I,4) /= 0 .AND. (INGAP(I,IT)  == 1 .OR. (KSLIDE(I,3)/=0.OR.KSLIDE(I,1)/=0)))THEN
C
C               INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
C               Glissement => utiliser le cone pour choisir entre les segments voisins
C                             mais garder le contact si le nd a glisse de plus d'un elt (cf supersonic)
C            
              X12(I)=XX(I,1)-XX(I,3)
              Y12(I)=YY(I,1)-YY(I,3)
              Z12(I)=ZZ(I,1)-ZZ(I,3)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12(I)*NNY(I,4)-Y12(I)*NNZ(I,4)  
              PY = X12(I)*NNZ(I,4)-Z12(I)*NNX(I,4)  
              PZ = Y12(I)*NNX(I,4)-X12(I)*NNY(I,4)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  =  XIJ(I,3)*XIJ(I,3)+YIJ(I,3)*YIJ(I,3)+ZIJ(I,3)*ZIJ(I,3)
C
              SIDE(I,4)=-(XIJ(I,3)*PX+YIJ(I,3)*PY+ZIJ(I,3)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDE(I,4) < -ZEP01) FAR(I,IT) =2
C
            END IF
         END IF
       END IF
C
       IF(FAR(I,IT)==2) PENT(I,IT)=ZERO
C
      ENDDO
C--------------------------------------------------------
      DO I=1,JLT
C
        IF(STIF(I) == ZERO)CYCLE
C
        IT  = SUBTRIA(I)
        IF(IT/=0.AND.PENT(I,IT)==ZERO) IT=0

        IF(IT == 0)CYCLE

        N = CAND_N(I)
        L = CAND_E(I)

        MGLOB=MSEGLO(L)

        PENE=PENT(I,IT) ! PENE /= ZERO
        IF(N.LE.NSN)THEN
#include "lockon.inc"

c       if(itab(nsvg(i))==27363)
cc      if(itab(nsvg(i))==27952.or.
cc     .       itab(ix1(i))==27952.or.itab(ix2(i))==27952.or.itab(ix3(i))==27952.or.itab(ix4(i))==27952)
c     .      print *,'dst21 nat',ispmd+1,it,irtlm(1,n),mglob,cand_e(i),pent(i,it),far(i,it),
c     .      itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),
c     .         ibound(1:4,i),lb(i,it),lc(i,it),lb(i,it)+lc(i,it),
c     .      BB(i,it),far(i,it),TIME_S(1,N),time_s(2,n),
c     .      dist(i),ld(i)    
          IF(DIST(I) < ONEP02*TIME_S(1,N) .AND.
     *      (DIST(I) < TIME_S(2,N) .OR.
     *             (DIST(I) == TIME_S(2,N) .AND. IRTLM(1,N) < MGLOB)))THEN
            IRTLM(1,N) = MGLOB
            IRTLM(2,N) = IT
            IRTLM(3,N) = CAND_E(I)
            IRTLM(4,N) = ISPMD+1
            TIME_S(2,N) = DIST(I)
          END IF
#include "lockoff.inc"
        ELSE
#include "lockon.inc"
          IF(DIST(I) < ONEP02*TIME_SFI(NIN)%P(2*(N-NSN-1)+1) .AND.
     *      (DIST(I) < TIME_SFI(NIN)%P(2*(N-NSN-1)+2)  .OR.
     *             (DIST(I) == TIME_SFI(NIN)%P(2*(N-NSN-1)+2)  .AND. IRTLM_FI(NIN)%P(1,N-NSN) < MGLOB)))THEN
            IRTLM_FI(NIN)%P(1,N-NSN) = MGLOB
            IRTLM_FI(NIN)%P(2,N-NSN) = IT
            IRTLM_FI(NIN)%P(3,N-NSN) = CAND_E(I)
            IRTLM_FI(NIN)%P(4,N-NSN) = ISPMD+1
            TIME_SFI(NIN)%P(2*(N-NSN-1)+2) = DIST(I)
          END IF
c          if(itafi(nin)%p(n-nsn)==3817238)
cc          if(itafi(nin)%p(n-nsn)==3817238.or.
cc     .       itab(ix1(i))==3817238.or.itab(ix2(i))==3817238.or.itab(ix3(i))==3817238.or.itab(ix4(i))==3817238)
c     .      print *,'dst21 rem',ispmd+1,it,IRTLM_FI(NIN)%P(1,n-nsn),mglob,cand_e(i),pent(i,it),far(i,it),
c     .      TIME_SFI(NIN)%P(N-NSN),dist(i),BB(i,it),
c     .      itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i))
#include "lockoff.inc"
        END IF
      END DO

      RETURN
      END
