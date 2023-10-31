Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  I25DST3_1                     source/interfaces/int25/i25dst3_1.F
Chd|-- called by -----------
Chd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25DST3_1(
     1                  JLT    ,CAND_N ,CAND_E ,
     2                  XX     ,YY     ,ZZ     ,
     3                  XI     ,YI     ,ZI     ,
     5                  NIN    ,NSN    ,IX1    ,
     6                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
     7                  INACTI ,MSEGLO ,GAPS   ,GAPM   ,GAPMXL ,
     8                  IRECT  ,IRTLM  ,TIME_S ,GAP_NM ,ITAB  ,
     9                  ICONT_I,NNX     ,NNY   ,NNZ     ,
     A                  FAR     ,PENT  ,DIST    ,LB    ,LC    ,
     B                  LBP     ,LCP   ,SUBTRIA ,MVOISN,IBOUND,
     C                  VTX_BISECTOR   ,DRAD    ,DGAPLOAD)
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
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NSN, INACTI,
     .        CAND_N(*),
     .        CAND_E(*),NSVG(MVSIZ)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INTTH,MSEGLO(*),IRTLM(4,NSN), SUBTRIA(MVSIZ), 
     .        MVOISN(MVSIZ,4), IBOUND(4,*)
      my_real
     .     TIME_S(2,NSN), GAPS(*), GAPM(*), GAPMXL(MVSIZ)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .     XX(MVSIZ,5),YY(MVSIZ,5),ZZ(MVSIZ,5), 
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5),
     .     PENT(MVSIZ,4), DIST(MVSIZ), LB(MVSIZ,4), LC(MVSIZ,4), 
     .     LBP(MVSIZ,4), LCP(MVSIZ,4), GAP_NM(4,MVSIZ), DD(MVSIZ,4)
      INTEGER IRECT(4,*),ITAB(*),ICONT_I(*), FAR(MVSIZ,4)
      REAL*4 VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, JJ, INTERSECT, N, ITQ, N4N, IT, IKEEP, MGLOB, NINDX, ITOLD
      INTEGER I4N(MVSIZ), INDX(MVSIZ), I1, I2, II,
     .        ITRIA(2,4), INEIGH(3,4), MARQUE(4,MVSIZ),
     .        SUBTRIA_N(MVSIZ), IT1, IB1, IB2, IB3, IBX, IX, IY, IZ
      my_real
     .     XI1(MVSIZ),  XI2(MVSIZ),  XI0V(MVSIZ),  XI5,
     .     YI1(MVSIZ),  YI2(MVSIZ),  YI0V(MVSIZ),  YI5,
     .     ZI1(MVSIZ),  ZI2(MVSIZ),  ZI0V(MVSIZ),  ZI5,
     .     XIJ(MVSIZ,4),  
     .     YIJ(MVSIZ,4),  
     .     ZIJ(MVSIZ,4),  
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
     .     DX, DY, DZ, 
     .     LAP, HLA, HLB(MVSIZ,4), HLC(MVSIZ,4),
     .     AL(MVSIZ,4)
      my_real
     .     AAA,BB(MVSIZ,4),
     .     SX1,SX2,SX3,SX4,
     .     SY1,SY2,SY3,SY4,
     .     SZ1,SZ2,SZ3,SZ4,
     .     X0N(MVSIZ,4), Y0N(MVSIZ,4), Z0N(MVSIZ,4), 
     .     XN(MVSIZ,4),YN(MVSIZ,4),ZN(MVSIZ,4),
     .     SIDE(MVSIZ,4), 
     .     X12(MVSIZ), Y12(MVSIZ), Z12(MVSIZ), 
     .     PX, PY, PZ, PP, P1, P2, XH, YH, ZH, D1, D2, D3, VX, VY, VZ, 
     .     LL,NN,PN,
     .     N1X,N1Y,N1Z,N1N,
     .     N2X,N2Y,N2Z,N2N,
     .     LX, LAX, LBX, LCX, LD(MVSIZ), DMIN
      INTEGER ISHEL(MVSIZ)
      DATA ITRIA/1,2,2,3,3,4,4,1/,
     .     INEIGH/4,3,2,1,4,3,2,1,4,3,2,1/
C-----------------------------------------------------------------------
C
C initialisation (cf triangles)
      FAR(1:JLT,1:4)    = 0
      MARQUE(1:4,1:JLT) = 0
      PENT(1:JLT,1:4)   = EP20
      DD  (1:JLT,1:4)   = EP20
      DIST(1:JLT)       = EP20
      LD(1:JLT)         = EP20
C
      DO I=1,JLT
C
        IF(STIF(I) <= ZERO)CYCLE
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
C
        SUBTRIA_N(I)= 0
C
      ENDDO
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I=1,JLT
C
        IF(STIF(I) <= ZERO)CYCLE
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
#include  "vectorize.inc"
      DO I=1,JLT
C
        IF(STIF(I) <= ZERO)CYCLE
C
        AAA	 = ONE/MAX(EM30,X0N(I,1)*X0N(I,1)+Y0N(I,1)*Y0N(I,1)+Z0N(I,1)*Z0N(I,1))
        HLC(I,1) = LC(I,1)*ABS(LC(I,1))*AAA
        HLB(I,4) = LB(I,4)*ABS(LB(I,4))*AAA
        AL(I,1)  = -(XI0V(I)*X0N(I,1)+YI0V(I)*Y0N(I,1)+ZI0V(I)*Z0N(I,1))*AAA
        AL(I,1)  = MAX(ZERO,MIN(ONE,AL(I,1)))
        AAA	 = ONE/MAX(EM30,X0N(I,2)*X0N(I,2)+Y0N(I,2)*Y0N(I,2)+Z0N(I,2)*Z0N(I,2))
        HLC(I,2) = LC(I,2)*ABS(LC(I,2))*AAA
        HLB(I,1) = LB(I,1)*ABS(LB(I,1))*AAA
        AL(I,2)  = -(XI0V(I)*X0N(I,2)+YI0V(I)*Y0N(I,2)+ZI0V(I)*Z0N(I,2))*AAA
        AL(I,2)  = MAX(ZERO,MIN(ONE,AL(I,2)))
        AAA	 = ONE/MAX(EM30,X0N(I,3)*X0N(I,3)+Y0N(I,3)*Y0N(I,3)+Z0N(I,3)*Z0N(I,3))
        HLC(I,3) = LC(I,3)*ABS(LC(I,3))*AAA
        HLB(I,2) = LB(I,2)*ABS(LB(I,2))*AAA
        AL(I,3)  = -(XI0V(I)*X0N(I,3)+YI0V(I)*Y0N(I,3)+ZI0V(I)*Z0N(I,3))*AAA
        AL(I,3)  = MAX(ZERO,MIN(ONE,AL(I,3)))
        AAA	 = ONE/MAX(EM30,X0N(I,4)*X0N(I,4)+Y0N(I,4)*Y0N(I,4)+Z0N(I,4)*Z0N(I,4))
        HLC(I,4) = LC(I,4)*ABS(LC(I,4))*AAA
        HLB(I,3) = LB(I,3)*ABS(LB(I,3))*AAA
        AL(I,4)  = -(XI0V(I)*X0N(I,4)+YI0V(I)*Y0N(I,4)+ZI0V(I)*Z0N(I,4))*AAA
        AL(I,4)  = MAX(ZERO,MIN(ONE,AL(I,4)))
C
      END DO
C--------------------------------------------------------
      IT=1
      I1=ITRIA(1,IT)
      I2=ITRIA(2,IT)
#include  "vectorize.inc"
      DO I=1,JLT
C
        IF(STIF(I) <= ZERO)CYCLE
C
        X12(I) = XX(I,I2) - XX(I,I1)
        Y12(I) = YY(I,I2) - YY(I,I1)
        Z12(I) = ZZ(I,I2) - ZZ(I,I1)
C
        LBP(I,IT) = LB(I,IT)
        LCP(I,IT) = LC(I,IT)
        LAP	  = ONE-LBP(I,IT)-LCP(I,IT)
C       HLA, HLB, HLC necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I))
        HLA= LAP*ABS(LAP) * AAA
        IF(LAP<ZERO.AND.
     +     HLA<=HLB(I,IT).AND.HLA<=HLC(I,IT))THEN
         LBP(I,IT) = (XIJ(I,I2)*X12(I)+YIJ(I,I2)*Y12(I)+ZIJ(I,I2)*Z12(I)) * AAA
         LBP(I,IT) = MAX(ZERO,MIN(ONE,LBP(I,IT)))
         LCP(I,IT) = ONE - LBP(I,IT)
        ELSEIF(LBP(I,IT)<ZERO.AND.
     +         HLB(I,IT)<=HLC(I,IT).AND.HLB(I,IT)<=HLA)THEN
         LBP(I,IT) = ZERO
         LCP(I,IT) = AL(I,I2)
        ELSEIF(LCP(I,IT)<ZERO.AND.
     +         HLC(I,IT)<=HLA.AND.HLC(I,IT)<=HLB(I,IT))THEN
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
        IF (STIF(I) <= ZERO)CYCLE
C
        LAP  = ONE-LBP(I,IT)-LCP(I,IT)
C
        GAP  = MIN(MAX(DRAD,GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I)+DGAPLOAD),
     .                 MAX(DRAD,GAPMXL(I)+DGAPLOAD))
        GAP2 = GAP**2
C
        BB(I,IT) =((XX(I,5)-XI(I))*XN(I,IT)+(YY(I,5)-YI(I))*YN(I,IT)+(ZZ(I,5)-ZI(I))*ZN(I,IT))

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
#include  "vectorize.inc"
      DO I =1,JLT
C          
        IF (STIF(I) <= ZERO)CYCLE
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

          END IF
C
C         Cone
          IF(FAR(I,IT)==1 .OR. BB(I,IT) <= ZERO)THEN     
C          
            X12(I)=XX(I,I2)-XX(I,I1)
            Y12(I)=YY(I,I2)-YY(I,I1)
            Z12(I)=ZZ(I,I2)-ZZ(I,I1)
C
C           normal to the bisecting plane (pointing toward the inside)
            PX = Z12(I)*NNY(I,IT)-Y12(I)*NNZ(I,IT)  
            PY = X12(I)*NNZ(I,IT)-Z12(I)*NNX(I,IT)  
            PZ = Y12(I)*NNX(I,IT)-X12(I)*NNY(I,IT)  
            PP = PX*PX+PY*PY+PZ*PZ
C
            LL  = XIJ(I,I1)*XIJ(I,I1)+YIJ(I,I1)*YIJ(I,I1)+ZIJ(I,I1)*ZIJ(I,I1)
C
            SIDE(I,IT)=-(XIJ(I,I1)*PX+YIJ(I,I1)*PY+ZIJ(I,I1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))		  
C
C           Projette a l'exterieur et Hors secteur angulaire (cf bissectrices) <=> Glissement
            IF(SIDE(I,IT) < -ZEP01) FAR(I,IT) =2
C
          END IF
          IF(FAR(I,IT)==2) PENT(I,IT)=ZERO
C
        ELSE
C
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
          IF    ( MVOISN(I,1)==0 .AND. D1 >= GAPS(I)) THEN 
            FAR(I,1)=2
          ELSEIF( MVOISN(I,2)==0 .AND. D2 >= GAPS(I)) THEN  
            FAR(I,2)=2
          ELSEIF( MVOISN(I,4)==0 .AND. D3 >= GAPS(I)) THEN 	 
            FAR(I,3)=2
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
          IF(FAR(I,IT)==1 .OR. BB(I,IT) <= ZERO)THEN
C
            IF(MVOISN(I,1)/=0)THEN          
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
              XI1(I) = XX(I,1) - XI(I)
              YI1(I) = YY(I,1) - YI(I)
              ZI1(I) = ZZ(I,1) - ZI(I)
              LL = XI1(I)*XI1(I)+YI1(I)*YI1(I)+ZI1(I)*ZI1(I)
C
              SIDE(I,1)=-(XI1(I)*PX+YI1(I)*PY+ZI1(I)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))   
              IF(SIDE(I,1) < -ZEP01) FAR(I,1)=2
C          
            END IF
C
            IF(MVOISN(I,2)/=0)THEN          
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
	      XI1(I) = XX(I,2) - XI(I)
	      YI1(I) = YY(I,2) - YI(I)
	      ZI1(I) = ZZ(I,2) - ZI(I)
              LL     = XI1(I)*XI1(I)+YI1(I)*YI1(I)+ZI1(I)*ZI1(I)
C
              SIDE(I,2)=-(XI1(I)*PX+YI1(I)*PY+ZI1(I)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))   
              IF(SIDE(I,2) < -ZEP01) FAR(I,2)=2
C          
            END IF
C
            IF(MVOISN(I,4)/=0)THEN          
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
	      XI1(I) = XX(I,3) - XI(I)
	      YI1(I) = YY(I,3) - YI(I)
	      ZI1(I) = ZZ(I,3) - ZI(I)
              LL     = XI1(I)*XI1(I)+YI1(I)*YI1(I)+ZI1(I)*ZI1(I)
C
              SIDE(I,4)=-(XI1(I)*PX+YI1(I)*PY+ZI1(I)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))   
              IF(SIDE(I,4) < -ZEP01) FAR(I,3)=2
C          
            END IF
C          
          END IF
C
          IF(FAR(I,1)==2.OR.FAR(I,2)==2.OR.FAR(I,3)==2) PENT(I,IT)=ZERO
        END IF
C
      ENDDO
C--------------------------------------------------------
      N4N=0
      DO I=1,JLT
C
       IF(STIF(I) <= ZERO)CYCLE
C
       IF(IX3(I)/=IX4(I))THEN
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
         LAP	   = ONE-LBP(I,IT)-LCP(I,IT)
C        HLA, HLB, HLC necessaires pour triangle angle obtu
         AAA = ONE / MAX(EM20,X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I))
         HLA= LAP*ABS(LAP) * AAA
         IF(LAP<ZERO.AND.
     +      HLA<=HLB(I,IT).AND.HLA<=HLC(I,IT))THEN
          LBP(I,IT) = (XIJ(I,I2)*X12(I)+YIJ(I,I2)*Y12(I)+ZIJ(I,I2)*Z12(I)) * AAA
          LBP(I,IT) = MAX(ZERO,MIN(ONE,LBP(I,IT)))
          LCP(I,IT) = ONE - LBP(I,IT)
         ELSEIF(LBP(I,IT)<ZERO.AND.
     +          HLB(I,IT)<=HLC(I,IT).AND.HLB(I,IT)<=HLA)THEN
          LBP(I,IT) = ZERO
          LCP(I,IT) = AL(I,I2)
         ELSEIF(LCP(I,IT)<ZERO.AND.
     +          HLC(I,IT)<=HLA.AND.HLC(I,IT)<=HLB(I,IT))THEN
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
         GAP  = MIN(MAX(GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I)+DGAPLOAD,DRAD),
     .                   MAX(DRAD,GAPMXL(I)+DGAPLOAD))
         GAP2 = GAP**2
C
         BB(I,IT) =((XX(I,5)-XI(I))*XN(I,IT)+(YY(I,5)-YI(I))*YN(I,IT)+(ZZ(I,5)-ZI(I))*ZN(I,IT))

         IF(BB(I,IT) > ZERO)THEN
C
C          penetration approximee (cf zone limite interpolation des normales)
           PENT(I,IT)=MAX(ZERO,GAP+BB(I,IT))
         ELSE
           PENT(I,IT)=MAX(ZERO,GAP-SQRT(DD(I,IT)))
         END IF
C
        ENDDO
C--------------------------------------------------------
#include  "vectorize.inc"
        DO K=1,N4N
         I=I4N(K)
C
C       Projection en dehors du triangle
        IF(LB(I,IT) < -EM03 .OR. LC(I,IT) < -EM03 .OR.
     .      LB(I,IT)+LC(I,IT) > ONE+EM03) FAR(I,IT)=1
C
        XH=XI(I)+BB(I,IT)*XN(I,IT)
        YH=YI(I)+BB(I,IT)*YN(I,IT)
        ZH=ZI(I)+BB(I,IT)*ZN(I,IT)
C
        IB1=IBOUND(I1,I)
        IB2=IBOUND(I2,I)
        IF(MVOISN(I,IT)==0)THEN     
C
          IF( (XH-XX(I,I1))* NNX(I,IT)+
     .        (YH-YY(I,I1))* NNY(I,IT)+
     .        (ZH-ZZ(I,I1))* NNZ(I,IT) >= GAPS(I)) FAR(I,IT) =2
C
	ELSEIF((IB1 /= 0 .AND. IB2 == 0).OR.
     .         (IB2 /= 0 .AND. IB1 == 0))THEN
C
          IBX=MAX(IB1,IB2)
          IF(IB1/=0)THEN
            IX =I1
          ELSEIF(IB2/=0)THEN
            IX =I2
          END IF
C
          IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .       VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .       VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .       VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .       VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .       VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
            P1 = (XH-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .           (YH-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .           (ZH-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
            P2 = (XH-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .           (YH-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .           (ZH-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
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
C       Cone
        IF(FAR(I,IT)==1 .OR. BB(I,IT) <= ZERO) THEN
C
C        tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
 	 X12(I)=XX(I,I2)-XX(I,I1)
 	 Y12(I)=YY(I,I2)-YY(I,I1)
 	 Z12(I)=ZZ(I,I2)-ZZ(I,I1)
C
C        normal to the bisecting plane (pointing toward the inside)
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

C
        IF(FAR(I,IT)==2) PENT(I,IT)=ZERO
C
       ENDDO
C--------------------------------------------------------
      END DO !  DO IT=2,4
C--------------------------------------------------------
#include  "vectorize.inc"
      DO I=1,JLT
C          
        IF (STIF(I) <= ZERO)CYCLE
C
        SUBTRIA_N(I)= SUBTRIA(I)
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
                LX  = MAX(ZERO,DD(I,JJ)-BB(I,JJ)*BB(I,JJ))
              END IF

c            if(itab(nsvg(i))==5750)
c     .  print *,'lx',jj,dmin,dd(i,jj),lx,far(i,jj),pent(i,jj)
              IF(LX < LD(I)) THEN
          	SUBTRIA_N(I)= JJ
          	DIST(I)     = DD(I,JJ)
          	LD(I)=LX
              END IF

            END IF
          END DO
        ELSE
          IF(DD(I,1) <= DIST(I))THEN
            SUBTRIA_N(I)= 1
            DIST(I)   = DD(I,1)
          END IF
        END IF
C
      END DO
C-----
#include  "vectorize.inc"
      DO I=1,JLT
C
        IF(STIF(I) <= ZERO)CYCLE
C
        ITOLD=SUBTRIA(I)
C
        IT = SUBTRIA_N(I)
        IF(IT/=0.AND.PENT(I,IT)==ZERO) IT=0
C
        DO J=1,4
          IF(J /= IT) PENT(I,J)=ZERO
        END DO
C
        N = CAND_N(I)
        IF(N <= NSN)THEN
C
C         Possible IT = Old Subtria & PENT = ZERO
          IRTLM(2,N) = IT+5*SUBTRIA(I)
C         IRTLM(3,N) = CAND_E(I)
C         IRTLM(4,N) = ISPMD+1
          TIME_S(1,N) = DIST(I)
C
C         Condition for sliding :
          IF(IX3(I)/=IX4(I))THEN
            IF(PENT(I,ITOLD)==ZERO.OR.FAR(I,ITOLD)>=2) THEN
              IRTLM(2,N) = -IRTLM(2,N)
            END IF
          ELSE
            IF(PENT(I,ITOLD)==ZERO.OR.FAR(I,1)>=2.OR.FAR(I,2)>=2.OR.FAR(I,3)>=2) THEN
              IRTLM(2,N) = -IRTLM(2,N)
            END IF
          END IF
c       it1=it
c       if(it==0)it1=1
c       if(itab(nsvg(i))==5750)
cc       if(itab(nsvg(i))==2421446.or.
cc     .       itab(ix1(i))==2421446.or.itab(ix2(i))==2421446.or.itab(ix3(i))==2421446.or.itab(ix4(i))==2421446)
c     .      print *,'dst1 nat',ispmd+1,irtlm(1,n),cand_e(i),TIME_S(1,N),
c     .         itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),
c     .         ibound(1:4,i),itold,pent(i,itold),mvoisn(i,itold),far(i,itold),
c     .        lb(i,itold),lc(i,itold),lb(i,itold)+lc(i,itold),
c     .         it,pent(i,it1),bb(i,it1),far(i,it1),dist(i),ld(i),
c     .         lb(i,it1),lc(i,it1),lb(i,it1)+lc(i,it1)
        ELSE
          IRTLM_FI(NIN)%P(2,N-NSN) = IT+5*SUBTRIA(I)
C         IRTLM_FI(NIN)%P(3,N-NSN) = CAND_E(I)
C         IRTLM_FI(NIN)%P(4,N-NSN) = ISPMD+1
          TIME_SFI(NIN)%P(2*(N-NSN-1)+1)   = DIST(I)
C
C         Condition for sliding :
          IF(IX3(I)/=IX4(I))THEN
            IF(PENT(I,ITOLD)==ZERO.OR.FAR(I,ITOLD)>=2) THEN
              IRTLM_FI(NIN)%P(2,N-NSN) = -IRTLM_FI(NIN)%P(2,N-NSN) 
            END IF
          ELSE
            IF(PENT(I,ITOLD)==ZERO.OR.FAR(I,1)>=2.OR.FAR(I,2)>=2.OR.FAR(I,3)>=2) THEN
              IRTLM_FI(NIN)%P(2,N-NSN) = -IRTLM_FI(NIN)%P(2,N-NSN) 
            END IF
          END IF
c	 it1=it
c	 if(it==0)it1=1
cc	 if(itafi(nin)%p(n-nsn)==28139)
c	  if(itafi(nin)%p(n-nsn)==2421446.or.
c     .       itab(ix1(i))==2421446.or.itab(ix2(i))==2421446.or.itab(ix3(i))==2421446.or.itab(ix4(i))==2421446)
c     .      print *,'dst1 rem',ispmd+1,IRTLM_FI(NIN)%P(1,n-nsn),cand_e(i),TIME_SFI(NIN)%P(2*(N-NSN-1)+1),
c     .  	       itafi(nin)%p(n-nsn),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),
c     .  	       itold,pent(i,itold),far(i,itold),
c     .  	       it,pent(i,it1),far(i,it1)
        END IF
      END DO
C-----
      RETURN
      END
Chd|====================================================================
Chd|  I25GLOB_1                     source/interfaces/int25/i25dst3_1.F
Chd|-- called by -----------
Chd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25GLOB_1(
     1                  JLT    ,CAND_N ,CAND_E ,
     2                  NIN    ,NSN    ,IX1    ,IX2    ,IX3    ,
     3                  IX4    ,NSVG   ,STIF   ,INACTI ,MSEGLO ,
     4                  IRTLM  ,TIME_S ,ITAB   ,
     5                  FAR    ,PENT   ,LB     ,LC     ,
     6                  FARM   ,PENM   ,LBM    ,LCM    ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NSN, INACTI,
     .        CAND_N(*),
     .        CAND_E(*),ITAB(*)
      INTEGER NSVG(MVSIZ), IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        FAR(MVSIZ,4), MSEGLO(*), IRTLM(4,NSN), FARM(4,*)
      my_real
     .     STIF(*), TIME_S(*),
     .     PENT(MVSIZ,4), LB(MVSIZ,4), LC(MVSIZ,4),
     .     PENM(4,*), LBM(4,*), LCM(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IT, I
C--------------------------------------------------------
C      Back to global arrays
C--------------------------------------------------------
      DO I=1,JLT
        DO IT=1,4
          FARM(IT,I)=FAR(I,IT)
          PENM(IT,I)=PENT(I,IT)
          LBM(IT,I) =LB(I,IT)
          LCM(IT,I) =LC(I,IT)
        END DO
      END DO
      RETURN
      END
Chd|====================================================================
Chd|  I25GLOB                       source/interfaces/int25/i25dst3_1.F
Chd|-- called by -----------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25GLOB(
     1                  JLT    ,CAND_N ,CAND_E ,
     2                  NIN    ,NSN    ,IX1    ,IX2    ,IX3    ,
     3                  IX4    ,NSVG   ,STIF   ,INACTI ,MSEGLO ,
     4                  IRTLM  ,TIME_S ,ITAB   ,
     5                  FAR    ,PENT   ,LB     ,LC     ,
     6                  INDEX  ,FARM   ,PENM   ,LBM    ,
     7                  LCM    ) 
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
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NSN, INACTI,
     .        CAND_N(*),
     .        CAND_E(*),ITAB(*)
      INTEGER NSVG(MVSIZ), IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        FAR(MVSIZ,4), MSEGLO(*), IRTLM(4,NSN) ,INDEX(*), FARM(4,*)
      my_real
     .     STIF(*), TIME_S(*),
     .     PENT(MVSIZ,4), LB(MVSIZ,4), LC(MVSIZ,4),
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
        DO IT=1,4
          FARM(IT,J)=FAR(I,IT)
          PENM(IT,J)=PENT(I,IT)
          LBM(IT,J) =LB(I,IT)
          LCM(IT,J) =LC(I,IT)
        END DO
      END DO
      RETURN
      END 
