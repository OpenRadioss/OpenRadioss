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
Chd|  I23DST3                       source/interfaces/int23/i23dst3.F
Chd|-- called by -----------
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I23DST3(
     1  		JLT    ,CAND_N_N ,CAND_E_N ,CN_LOC ,CE_LOC ,
     2  		X1     ,X2     ,X3     ,X4     ,Y1     ,
     3  		Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4  		Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     6  		IX1    ,IX2    ,IX3    ,IX4    ,NSVG   ,
     7  		GAPV   ,INACTI ,INDEX  ,
     8  		VXM    ,VYM    ,VZM    ,H1     ,H2     ,
     9  		H3     ,H4     ,IRECT  ,CAND_P ,
     A  		IFPEN  ,NX     ,NY     ,NZ     ,FTXSAV ,
     B  		FTYSAV ,FTZSAV ,FXT    ,FYT   ,FZT     ,
     C  		PENE   ,V      ,VXI    ,VYI   ,VZI     ,
     D                  MSI    ,STIF   ,JLT_NEW,NSMS  ,KINI    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, INACTI, CAND_N_N(*), CAND_E_N(*), JLT_NEW 
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ), 
     .        NSVG(MVSIZ), CN_LOC(MVSIZ) ,CE_LOC(MVSIZ) ,INDEX(MVSIZ), 
     .        IRECT(4,*), IFPEN(*), NSMS(MVSIZ), KINI(MVSIZ)
      my_real
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), 
     .     GAPV(MVSIZ), PENE(MVSIZ), CAND_P(*),
     .     NX(MVSIZ),  NY(MVSIZ),  NZ(MVSIZ),
     .     XG(MVSIZ),  YG(MVSIZ),  ZG(MVSIZ),
     .     VXM(MVSIZ), VYM(MVSIZ), VZM(MVSIZ),
     .     FTXSAV(*),  FTYSAV(*),  FTZSAV(*),
     .     FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ), 
     .     VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ), STIF(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ), V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J, II, INPROJ(4,MVSIZ)
      my_real
     .     LB1(MVSIZ), LC1(MVSIZ), P1(MVSIZ), 
     .     LB2(MVSIZ), LC2(MVSIZ), P2(MVSIZ), 
     .     LB3(MVSIZ), LC3(MVSIZ), P3(MVSIZ), 
     .     LB4(MVSIZ), LC4(MVSIZ), P4(MVSIZ),
     .     LA1, LA2, LA3, LA4,
     .     NX1(MVSIZ), NY1(MVSIZ), NZ1(MVSIZ),
     .     NX2(MVSIZ), NY2(MVSIZ), NZ2(MVSIZ),
     .     NX3(MVSIZ), NY3(MVSIZ), NZ3(MVSIZ),
     .     NX4(MVSIZ), NY4(MVSIZ), NZ4(MVSIZ),
     .     AL1(MVSIZ),  AL2(MVSIZ),  AL3(MVSIZ),  AL4(MVSIZ),  
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ),  X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ),  Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ),  Z04(MVSIZ),
     .     XI1(MVSIZ),  XI2(MVSIZ),  XI3(MVSIZ),  XI4(MVSIZ), 
     .     YI1(MVSIZ),  YI2(MVSIZ),  YI3(MVSIZ),  YI4(MVSIZ), 
     .     ZI1(MVSIZ),  ZI2(MVSIZ),  ZI3(MVSIZ),  ZI4(MVSIZ), 
     .     X0(MVSIZ),  Y0(MVSIZ),  Z0(MVSIZ),
     .     XI0V(MVSIZ),  YI0V(MVSIZ),  ZI0V(MVSIZ),
     .     LA, HLA, H0, H00,
     .     HLB1(MVSIZ), HLC1(MVSIZ),
     .     HLB2(MVSIZ), HLC2(MVSIZ),
     .     HLB3(MVSIZ), HLC3(MVSIZ),
     .     HLB4(MVSIZ), HLC4(MVSIZ),
     .     XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ),
     .     XP1(MVSIZ), YP1(MVSIZ), ZP1(MVSIZ),
     .     XP2(MVSIZ), YP2(MVSIZ), ZP2(MVSIZ),
     .     XP3(MVSIZ), YP3(MVSIZ), ZP3(MVSIZ),
     .     XP4(MVSIZ), YP4(MVSIZ), ZP4(MVSIZ),
     .     N11(MVSIZ), N21(MVSIZ), N31(MVSIZ),
     .     N12(MVSIZ), N22(MVSIZ), N32(MVSIZ),
     .     N13(MVSIZ), N23(MVSIZ), N33(MVSIZ),
     .     N14(MVSIZ), N24(MVSIZ), N34(MVSIZ),
     .     NVERS(4,MVSIZ), FIRSTIMP(4,MVSIZ),
     .     XN1(MVSIZ),YN1(MVSIZ),ZN1(MVSIZ),
     .     XN2(MVSIZ),YN2(MVSIZ),ZN2(MVSIZ),
     .     XN3(MVSIZ),YN3(MVSIZ),ZN3(MVSIZ),
     .     XN4(MVSIZ),YN4(MVSIZ),ZN4(MVSIZ),
     .     SSC(MVSIZ),TTC(MVSIZ),
     .     A12(MVSIZ),A23(MVSIZ),A34(MVSIZ),A41(MVSIZ),
     .     B12(MVSIZ),B23(MVSIZ),B34(MVSIZ),B41(MVSIZ),
     .     AB1(MVSIZ),AB2(MVSIZ),
     .     N1(MVSIZ),N2(MVSIZ),N3(MVSIZ),
     .     XX1(MVSIZ),XX2(MVSIZ),XX3(MVSIZ),XX4(MVSIZ),
     .     YY1(MVSIZ),YY2(MVSIZ),YY3(MVSIZ),YY4(MVSIZ),
     .     ZZ1(MVSIZ),ZZ2(MVSIZ),ZZ3(MVSIZ),ZZ4(MVSIZ),
     .     AREA(MVSIZ),ALP(MVSIZ),VAR
      my_real
     .     S2,NN,PS,
     .     X12,X23,X34,X41,SX1,SX2,SX3,SX4,SX0,
     .     Y12,Y23,Y34,Y41,SY1,SY2,SY3,SY4,SY0,
     .     Z12,Z23,Z34,Z41,SZ1,SZ2,SZ3,SZ4,SZ0,
     .     GAP2(MVSIZ), AAA, SIDE, DIST,
     .     LL, H
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES
C--------------------------------------------------------
       DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
         X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
         Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
         Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)) 
        ELSE
         X0(I) = X3(I)
         Y0(I) = Y3(I)
         Z0(I) = Z3(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
C
        X01(I) = X1(I) - X0(I)
        Y01(I) = Y1(I) - Y0(I)
        Z01(I) = Z1(I) - Z0(I)
C
        X02(I) = X2(I) - X0(I)
        Y02(I) = Y2(I) - Y0(I)
        Z02(I) = Z2(I) - Z0(I)
C
        X03(I) = X3(I) - X0(I)
        Y03(I) = Y3(I) - Y0(I)
        Z03(I) = Z3(I) - Z0(I)
C
        X04(I) = X4(I) - X0(I)
        Y04(I) = Y4(I) - Y0(I)
        Z04(I) = Z4(I) - Z0(I)
C
        XI0V(I) = X0(I) - XI(I)
        YI0V(I) = Y0(I) - YI(I)
        ZI0V(I) = Z0(I) - ZI(I)
C
        XI1(I) = X1(I) - XI(I)
        YI1(I) = Y1(I) - YI(I)
        ZI1(I) = Z1(I) - ZI(I)
C
        XI2(I) = X2(I) - XI(I)
        YI2(I) = Y2(I) - YI(I)
        ZI2(I) = Z2(I) - ZI(I)
C
        XI3(I) = X3(I) - XI(I)
        YI3(I) = Y3(I) - YI(I)
        ZI3(I) = Z3(I) - ZI(I)
C
        XI4(I) = X4(I) - XI(I)
        YI4(I) = Y4(I) - YI(I)
        ZI4(I) = Z4(I) - ZI(I)
C
        SX1 = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
        SY1 = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
        SZ1 = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
        SX2 = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
        SY2 = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
        SZ2 = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
        SX0 = Y01(I)*Z02(I) - Z01(I)*Y02(I)
        SY0 = Z01(I)*X02(I) - X01(I)*Z02(I)
        SZ0 = X01(I)*Y02(I) - Y01(I)*X02(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        NN=SQRT(S2)
        N11(I)=SX0*NN
        N21(I)=SY0*NN
        N31(I)=SZ0*NN
C
        AREA(I)=NN
C
        LB1(I) = -(SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
        LC1(I) =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
C
        SX3 = YI0V(I)*ZI3(I) - ZI0V(I)*YI3(I)
        SY3 = ZI0V(I)*XI3(I) - XI0V(I)*ZI3(I)
        SZ3 = XI0V(I)*YI3(I) - YI0V(I)*XI3(I)
C
        SX0 = Y02(I)*Z03(I) - Z02(I)*Y03(I)
        SY0 = Z02(I)*X03(I) - X02(I)*Z03(I)
        SZ0 = X02(I)*Y03(I) - Y02(I)*X03(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        NN=SQRT(S2)
        N12(I)=SX0*NN
        N22(I)=SY0*NN
        N32(I)=SZ0*NN
C
        AREA(I)=AREA(I)+NN
C
        LB2(I) = -(SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
        LC2(I) =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
C
        SX4 = YI0V(I)*ZI4(I) - ZI0V(I)*YI4(I)
        SY4 = ZI0V(I)*XI4(I) - XI0V(I)*ZI4(I)
        SZ4 = XI0V(I)*YI4(I) - YI0V(I)*XI4(I)
C
        SX0 = Y03(I)*Z04(I) - Z03(I)*Y04(I)
        SY0 = Z03(I)*X04(I) - X03(I)*Z04(I)
        SZ0 = X03(I)*Y04(I) - Y03(I)*X04(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        NN=SQRT(S2)
        N13(I)=SX0*NN
        N23(I)=SY0*NN
        N33(I)=SZ0*NN
C
        AREA(I)=AREA(I)+NN
C
        LB3(I) = -(SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
        LC3(I) =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
C
        SX0 = Y04(I)*Z01(I) - Z04(I)*Y01(I)
        SY0 = Z04(I)*X01(I) - X04(I)*Z01(I)
        SZ0 = X04(I)*Y01(I) - Y04(I)*X01(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        NN=SQRT(S2)
        N14(I)=SX0*NN
        N24(I)=SY0*NN
        N34(I)=SZ0*NN
C
        AREA(I)=AREA(I)+NN
        AREA(I)=HALF*AREA(I)
C
        LB4(I) = -(SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
        LC4(I) =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
       ENDDO
C---------------------------------------------------------
C
       DO I=1,JLT
        AAA    = ONE/MAX(EM30,X01(I)*X01(I)+Y01(I)*Y01(I)+Z01(I)*Z01(I))
        HLC1(I)= LC1(I)*ABS(LC1(I))*AAA
        HLB4(I)= LB4(I)*ABS(LB4(I))*AAA
        AL1(I) = -(XI0V(I)*X01(I)+YI0V(I)*Y01(I)+ZI0V(I)*Z01(I))*AAA
        AL1(I) = MAX(ZERO,MIN(ONE,AL1(I)))
        AAA    = ONE/MAX(EM30,X02(I)*X02(I)+Y02(I)*Y02(I)+Z02(I)*Z02(I))
        HLC2(I)= LC2(I)*ABS(LC2(I))*AAA
        HLB1(I)= LB1(I)*ABS(LB1(I))*AAA
        AL2(I) = -(XI0V(I)*X02(I)+YI0V(I)*Y02(I)+ZI0V(I)*Z02(I))*AAA
        AL2(I) = MAX(ZERO,MIN(ONE,AL2(I)))
        AAA    = ONE/MAX(EM30,X03(I)*X03(I)+Y03(I)*Y03(I)+Z03(I)*Z03(I))
        HLC3(I)= LC3(I)*ABS(LC3(I))*AAA
        HLB2(I)= LB2(I)*ABS(LB2(I))*AAA
        AL3(I) = -(XI0V(I)*X03(I)+YI0V(I)*Y03(I)+ZI0V(I)*Z03(I))*AAA
        AL3(I) = MAX(ZERO,MIN(ONE,AL3(I)))
        AAA    = ONE/MAX(EM30,X04(I)*X04(I)+Y04(I)*Y04(I)+Z04(I)*Z04(I))
        HLC4(I)= LC4(I)*ABS(LC4(I))*AAA
        HLB3(I)= LB3(I)*ABS(LB3(I))*AAA
        AL4(I) = -(XI0V(I)*X04(I)+YI0V(I)*Y04(I)+ZI0V(I)*Z04(I))*AAA
        AL4(I) = MAX(ZERO,MIN(ONE,AL4(I)))
       ENDDO
C
       DO I=1,JLT
        X12 = X2(I) - X1(I)
        Y12 = Y2(I) - Y1(I)
        Z12 = Z2(I) - Z1(I)
        LA = ONE - LB1(I) - LC1(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LA*ABS(LA) * AAA
        INPROJ(1,I)=0
        IF(LA<ZERO.AND.
     +     HLA<=HLB1(I).AND.HLA<=HLC1(I))THEN
         LB1(I) = (XI2(I)*X12+YI2(I)*Y12+ZI2(I)*Z12) * AAA
         LB1(I) = MAX(ZERO,MIN(ONE,LB1(I)))
         LC1(I) = ONE - LB1(I)
         INPROJ(1,I)=1
        ELSEIF(LB1(I)<ZERO.AND.
     +         HLB1(I)<=HLC1(I).AND.HLB1(I)<=HLA)THEN
         LB1(I) = ZERO
         LC1(I) = AL2(I)
         INPROJ(1,I)=1
        ELSEIF(LC1(I)<ZERO.AND.
     +         HLC1(I)<=HLA.AND.HLC1(I)<=HLB1(I))THEN
         LC1(I) = ZERO
         LB1(I) = AL1(I)
         INPROJ(1,I)=1
        ENDIF
       ENDDO
C
       DO I=1,JLT
        IF(IX3(I)==IX4(I))CYCLE
        X23 = X3(I) - X2(I)
        Y23 = Y3(I) - Y2(I)
        Z23 = Z3(I) - Z2(I)
        LA = ONE - LB2(I) - LC2(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X23*X23+Y23*Y23+Z23*Z23)
        HLA= LA*ABS(LA) * AAA
        INPROJ(2,I)=0
	IF(LA<ZERO.AND.
     +     HLA<=HLB2(I).AND.HLA<=HLC2(I))THEN
	  LB2(I) = (XI3(I)*X23+YI3(I)*Y23+ZI3(I)*Z23)*AAA
	  LB2(I) = MAX(ZERO,MIN(ONE,LB2(I)))
	  LC2(I) = ONE - LB2(I)
          INPROJ(2,I)=1
	ELSEIF(LB2(I)<ZERO.AND.
     +          HLB2(I)<=HLC2(I).AND.HLB2(I)<=HLA)THEN
         LB2(I) = ZERO
         LC2(I) = AL3(I)
         INPROJ(2,I)=1
        ELSEIF(LC2(I)<ZERO.AND.
     +         HLC2(I)<=HLA.AND.HLC2(I)<=HLB2(I))THEN
         LC2(I) = ZERO
         LB2(I) = AL2(I)
         INPROJ(2,I)=1
        ENDIF
       ENDDO
C
       DO I=1,JLT
        IF(IX3(I)==IX4(I))CYCLE
        X34 = X4(I) - X3(I)
        Y34 = Y4(I) - Y3(I)
        Z34 = Z4(I) - Z3(I)
        LA = ONE - LB3(I) - LC3(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X34*X34+Y34*Y34+Z34*Z34)
        HLA= LA*ABS(LA) * AAA
        INPROJ(3,I)=0
        IF(LA<ZERO.AND.
     +     HLA<=HLB3(I).AND.HLA<=HLC3(I))THEN
         LB3(I) = (XI4(I)*X34+YI4(I)*Y34+ZI4(I)*Z34)*AAA
         LB3(I) = MAX(ZERO,MIN(ONE,LB3(I)))
         LC3(I) = ONE - LB3(I)
         INPROJ(3,I)=1
        ELSEIF(LB3(I)<ZERO.AND.
     +         HLB3(I)<=HLC3(I).AND.HLB3(I)<=HLA)THEN
         LB3(I) = ZERO
         LC3(I) = AL4(I)
         INPROJ(3,I)=1
        ELSEIF(LC3(I)<ZERO.AND.
     +         HLC3(I)<=HLA.AND.HLC3(I)<=HLB3(I))THEN
         LC3(I) = ZERO
         LB3(I) = AL3(I)
         INPROJ(3,I)=1
        ENDIF
       ENDDO
C
       DO I=1,JLT
        IF(IX3(I)==IX4(I))CYCLE
        X41 = X1(I) - X4(I)
        Y41 = Y1(I) - Y4(I)
        Z41 = Z1(I) - Z4(I)
        LA = ONE - LB4(I) - LC4(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X41*X41+Y41*Y41+Z41*Z41)
        HLA= LA*ABS(LA) * AAA
        INPROJ(4,I)=0
        IF(LA<ZERO.AND.
     +     HLA<=HLB4(I).AND.HLA<=HLC4(I))THEN
         LB4(I) = (XI1(I)*X41+YI1(I)*Y41+ZI1(I)*Z41)*AAA
         LB4(I) = MAX(ZERO,MIN(ONE,LB4(I)))
         LC4(I) = ONE - LB4(I)
         INPROJ(4,I)=1
        ELSEIF(LB4(I)<ZERO.AND.
     +         HLB4(I)<=HLC4(I).AND.HLB4(I)<=HLA)THEN
         LB4(I) = ZERO
         LC4(I) = AL1(I)
         INPROJ(4,I)=1
        ELSEIF(LC4(I)<ZERO.AND.
     +         HLC4(I)<=HLA.AND.HLC4(I)<=HLB4(I))THEN
         LC4(I) = ZERO
         LB4(I) = AL4(I)
         INPROJ(4,I)=1
        ENDIF
       ENDDO
C---------------------------------------------------------
       DO I=1,JLT
C
         GAP2(I)=GAPV(I)*GAPV(I)
C
         LA1 = ONE - LB1(I) - LC1(I)
         XP1(I) = LB1(I)*X1(I) + LC1(I)*X2(I) + LA1*X0(I) 
         YP1(I) = LB1(I)*Y1(I) + LC1(I)*Y2(I) + LA1*Y0(I) 
         ZP1(I) = LB1(I)*Z1(I) + LC1(I)*Z2(I) + LA1*Z0(I) 
C
         NX1(I) = XI(I)-XP1(I)
         NY1(I) = YI(I)-YP1(I)
         NZ1(I) = ZI(I)-ZP1(I)
         P1(I) = NX1(I)*NX1(I) + NY1(I)*NY1(I) +NZ1(I)*NZ1(I)
C
         LA2 = ONE - LB2(I) - LC2(I)
         XP2(I) = LB2(I)*X2(I) + LC2(I)*X3(I) + LA2*X0(I) 
         YP2(I) = LB2(I)*Y2(I) + LC2(I)*Y3(I) + LA2*Y0(I) 
         ZP2(I) = LB2(I)*Z2(I) + LC2(I)*Z3(I) + LA2*Z0(I) 
C
         NX2(I) = XI(I)-XP2(I)
         NY2(I) = YI(I)-YP2(I)
         NZ2(I) = ZI(I)-ZP2(I)
         P2(I) = NX2(I)*NX2(I) + NY2(I)*NY2(I) +NZ2(I)*NZ2(I)
C
         LA3 = ONE - LB3(I) - LC3(I)
         XP3(I) = LB3(I)*X3(I) + LC3(I)*X4(I) + LA3*X0(I) 
         YP3(I) = LB3(I)*Y3(I) + LC3(I)*Y4(I) + LA3*Y0(I) 
         ZP3(I) = LB3(I)*Z3(I) + LC3(I)*Z4(I) + LA3*Z0(I) 
C
         NX3(I) = XI(I)-XP3(I)
         NY3(I) = YI(I)-YP3(I)
         NZ3(I) = ZI(I)-ZP3(I)
         P3(I) = NX3(I)*NX3(I) + NY3(I)*NY3(I) +NZ3(I)*NZ3(I)
C
         LA4 = ONE - LB4(I) - LC4(I)
         XP4(I) = LB4(I)*X4(I) + LC4(I)*X1(I) + LA4*X0(I) 
         YP4(I) = LB4(I)*Y4(I) + LC4(I)*Y1(I) + LA4*Y0(I) 
         ZP4(I) = LB4(I)*Z4(I) + LC4(I)*Z1(I) + LA4*Z0(I) 
C
         NX4(I) = XI(I)-XP4(I)
         NY4(I) = YI(I)-YP4(I)
         NZ4(I) = ZI(I)-ZP4(I)
         P4(I) = NX4(I)*NX4(I) + NY4(I)*NY4(I) +NZ4(I)*NZ4(I)
C
       ENDDO
C
      DO I=1,JLT
      END DO
C
      DO I=1,JLT
C
       IF(IX3(I)/=IX4(I))THEN
C
        H =NX1(I)*N11(I) + NY1(I)*N21(I) +NZ1(I)*N31(I)
        LL=P1(I)-H*H
        IF(INPROJ(1,I)/=0.AND.LL >= GAP2(I))THEN
          P1(I)=ZERO
        ELSE
          P1(I)=MAX(ZERO,GAPV(I)-ABS(H))
        END IF
C
        H =NX2(I)*N12(I) + NY2(I)*N22(I) +NZ2(I)*N32(I)
        LL=P2(I)-H*H
        IF(INPROJ(2,I)/=0.AND.LL >= GAP2(I))THEN
          P2(I)=ZERO
        ELSE
          P2(I)=MAX(ZERO,GAPV(I)-ABS(H))
        END IF
C
        H =NX3(I)*N13(I) + NY3(I)*N23(I) +NZ3(I)*N33(I)
        LL=P3(I)-H*H
        IF(INPROJ(3,I)/=0.AND.LL >= GAP2(I))THEN
          P3(I)=ZERO
        ELSE
          P3(I)=MAX(ZERO,GAPV(I)-ABS(H))
        END IF
C
        H =NX4(I)*N14(I) + NY4(I)*N24(I) +NZ4(I)*N34(I)
        LL=P4(I)-H*H
        IF(INPROJ(4,I)/=0.AND.LL >= GAP2(I))THEN
          P4(I)=ZERO
        ELSE
          P4(I)=MAX(ZERO,GAPV(I)-ABS(H))
        END IF
C
        PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
        N1(I) = P1(I)*N11(I)+P2(I)*N12(I)+P3(I)*N13(I)+P4(I)*N14(I)
        N2(I) = P1(I)*N21(I)+P2(I)*N22(I)+P3(I)*N23(I)+P4(I)*N24(I)
        N3(I) = P1(I)*N31(I)+P2(I)*N32(I)+P3(I)*N33(I)+P4(I)*N34(I)
        NN=SQRT(N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I))
        NN=ONE/MAX(EM20,NN)
        N1(I)=N1(I)*NN
        N2(I)=N2(I)*NN
        N3(I)=N3(I)*NN
C
        LA1 = ONE - LB1(I) - LC1(I)
        LA2 = ONE - LB2(I) - LC2(I)
        LA3 = ONE - LB3(I) - LC3(I)
        LA4 = ONE - LB4(I) - LC4(I) 
C
        H0    = FOURTH * 
     .        (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
        H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
        H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
        H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
        H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
        H00    = ONE/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
        H1(I) = H1(I) * H00
        H2(I) = H2(I) * H00
        H3(I) = H3(I) * H00
        H4(I) = H4(I) * H00
C
       ELSE ! IF(IX3(I)/=IX4(I))THEN
C
        H1(I) = LB1(I)
        H2(I) = LC1(I)
        H3(I) = ONE - LB1(I) - LC1(I)
        H4(I) = ZERO
C
        N1(I) = N11(I)
        N2(I) = N21(I)
        N3(I) = N31(I)
C
        H =NX1(I)*N11(I) + NY1(I)*N21(I) +NZ1(I)*N31(I)
        LL=P1(I)-H*H
        IF(INPROJ(1,I)/=0.AND.LL >= GAP2(I))THEN
          PENE(I)=ZERO
        ELSE
          PENE(I)=MAX(ZERO,GAPV(I)-ABS(H))
        END IF
       END IF
      ENDDO
C
      DO 410 I=1,JLT
      XP(I)=H1(I)*X1(I)+H2(I)*X2(I)+H3(I)*X3(I)+H4(I)*X4(I)
      YP(I)=H1(I)*Y1(I)+H2(I)*Y2(I)+H3(I)*Y3(I)+H4(I)*Y4(I)
      ZP(I)=H1(I)*Z1(I)+H2(I)*Z2(I)+H3(I)*Z3(I)+H4(I)*Z4(I)
 410  CONTINUE
C---------------------------------------------------------
       DO I=1,JLT
         NX1(I) = XI(I)-XP(I)
         NY1(I) = YI(I)-YP(I)
         NZ1(I) = ZI(I)-ZP(I)
       ENDDO
C---------------------
       DO I=1,JLT
C
         NVERS(1,I)    = ZERO
         FIRSTIMP(1,I) = ZERO
C
         NX(I)=ZERO
         NY(I)=ZERO
         NZ(I)=ZERO
         IF(PENE(I)==ZERO)CYCLE
C--------------------------------------------------------
         SIDE=N1(I)*NX1(I)+N2(I)*NY1(I)+N3(I)*NZ1(I)
C
         IF(IFPEN(INDEX(I))==0.OR.TT==ZERO)THEN
            FIRSTIMP(1,I) = SIGN(ONE,SIDE)
            IF(FIRSTIMP(1,I) < ZERO)THEN
              N1(I)  = -N1(I)
              N2(I)  = -N2(I)
              N3(I)  = -N3(I)
            END IF
            NVERS(1,I) = ONE
C 1st impact below gap (sorting security)
         ELSE ! IF(IFPEN(INDEX(I))==0.OR.TT==ZERO)THEN
            IF(IFPEN(INDEX(I)) < 0)THEN
              N1(I)  = -N1(I)
              N2(I)  = -N2(I)
              N3(I)  = -N3(I)
C             SIDE   = -SIDE
            END IF
            NVERS(1,I)= SIGN(ONE,SIDE*IFPEN(INDEX(I)))
         END IF
C---------------------
C
C attention a la traversee de la coque => 1E20..., 
C prendre normale = normale au triangle
         NN=ONE/
     .      MAX(EM20,SQRT(N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I)))
         N1(I)=N1(I)*NN
         N2(I)=N2(I)*NN
         N3(I)=N3(I)*NN
         IF(NVERS(1,I)<ZERO)
     .        PENE(I)=(GAPV(I)-PENE(I))+GAPV(I)
C         
         NX(I)=N1(I)
         NY(I)=N2(I)
         NZ(I)=N3(I)
C         
       END DO
C---------------------
C     PENE INITIALE
C---------------------
      IF(TT/=ZERO)THEN
        DO I=1,JLT
         J=INDEX(I)
         IF(PENE(I)==ZERO)THEN
C
           FTXSAV(J)=ZERO
           FTYSAV(J)=ZERO
           FTZSAV(J)=ZERO
           CAND_P(J)=ZERO
           IFPEN(J) =0
         ELSE
C
           IF(IFPEN(J)==0)THEN
             FTXSAV(J)=ZERO
             FTYSAV(J)=ZERO
             FTZSAV(J)=ZERO
           END IF
C
           IF(IFPEN(J)==0.AND.PENE(I)/=ZERO)THEN
             CAND_P(J) =PENE(I)
           END IF
C
           IF(IFPEN(J)==0)THEN
             IF(FIRSTIMP(1,I) > ZERO)THEN
               IFPEN(J) = 1
             ELSEIF(FIRSTIMP(1,I) < ZERO)THEN
               IFPEN(J) =-1
             END IF
           ELSEIF(PENE(I)/=ZERO)THEN
             IFPEN(J)=SIGN(ABS(IFPEN(J))+1,IFPEN(J))
           ELSE
             IFPEN(J)=0
           END IF
         END IF
C
        END DO
      END IF
C--------------------------------- 
      DO I=1,JLT
        VXM(I)=ZERO
        VYM(I)=ZERO
        VZM(I)=ZERO
C utile que si IFPEN/=0
        IF(PENE(I)/=0)THEN
          VXM(I)=H1(I)*V(1,IX1(I))+H2(I)*V(1,IX2(I))+
     .           H3(I)*V(1,IX3(I))+H4(I)*V(1,IX4(I))
          VYM(I)=H1(I)*V(2,IX1(I))+H2(I)*V(2,IX2(I))+
     .           H3(I)*V(2,IX3(I))+H4(I)*V(2,IX4(I))
          VZM(I)=H1(I)*V(3,IX1(I))+H2(I)*V(3,IX2(I))+
     .           H3(I)*V(3,IX3(I))+H4(I)*V(3,IX4(I))
        END IF
      END DO
C
      DO I=1,JLT
        J=INDEX(I)
        FXT(I)=FTXSAV(J)
        FYT(I)=FTYSAV(J)
        FZT(I)=FTZSAV(J)
      END DO
C---------------------
C     JLT_NEW=JLT
C     RETURN
C
      DO I=1,JLT
       IF(PENE(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
        JLT_NEW = JLT_NEW + 1
        CN_LOC(JLT_NEW) = CAND_N_N(I)
        CE_LOC(JLT_NEW) = CAND_E_N(I)
        IX1(JLT_NEW)  = IX1(I)
        IX2(JLT_NEW)  = IX2(I)
        IX3(JLT_NEW)  = IX3(I)
        IX4(JLT_NEW)  = IX4(I)
        NSVG(JLT_NEW) = NSVG(I)
        NX(JLT_NEW)   = NX(I)
        NY(JLT_NEW)   = NY(I)
        NZ(JLT_NEW)   = NZ(I)
        PENE(JLT_NEW) = PENE(I)
        H1(JLT_NEW)   = H1(I)
        H2(JLT_NEW)   = H2(I)
        H3(JLT_NEW)   = H3(I)
        H4(JLT_NEW)   = H4(I)
        STIF(JLT_NEW) = STIF(I)
        GAPV(JLT_NEW) = GAPV(I)
        INDEX(JLT_NEW)= INDEX(I)
C
C       KINI(JLT_NEW) = KINI(I)
        VXI(JLT_NEW)  = VXI(I)
        VYI(JLT_NEW)  = VYI(I)
        VZI(JLT_NEW)  = VZI(I)
        MSI(JLT_NEW)  = MSI(I)
C
        VXM(JLT_NEW)  = VXM(I)
        VYM(JLT_NEW)  = VYM(I)
        VZM(JLT_NEW)  = VZM(I)
C
        FXT(JLT_NEW)  = FXT(I)
        FYT(JLT_NEW)  = FYT(I)
        FZT(JLT_NEW)  = FZT(I)
C
        XI(JLT_NEW)  = XI(I)
        YI(JLT_NEW)  = YI(I)
        ZI(JLT_NEW)  = ZI(I)
C
        KINI(JLT_NEW)  = KINI(I)
C
        IF(IDTMINS==2.OR.IDTMINS_INT/=0)NSMS(JLT_NEW)=NSMS(I)
C
       ENDIF
      ENDDO
C---------------------
      RETURN
      END
C===============================================================================
