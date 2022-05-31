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
Chd|  I7DST3                        source/interfaces/int07/i7dst3.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|-- calls ---------------
Chd|        I7CMAJ                        source/interfaces/int07/i7curv.F
Chd|====================================================================
      SUBROUTINE I7DST3(
     1                  JLT    ,CAND_N ,CAND_E ,CN_LOC ,CE_LOC ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     5                  NX1    ,NX2    ,NX3    ,NX4    ,NY1    ,
     6                  NY2    ,NY3    ,NY4    ,NZ1    ,NZ2    ,
     7                  NZ3    ,NZ4    ,LB1    ,LB2    ,LB3    ,
     8                  LB4    ,LC1    ,LC2    ,LC3    ,LC4    ,
     9                  P1     ,P2     ,P3     ,P4     ,IX1    ,
     A                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
     B                  JLT_NEW,GAPV   ,INACTI ,CAND_P ,
     C                  INDEX  ,VXI    ,VYI    ,
     D                  VZI    ,MSI    ,KINI   ,ICURV  ,IRECT  ,
     E                  NNX1   ,NNX2   ,NNX3   ,NNX4   ,NNY1   ,
     F                  NNY2   ,NNY3   ,NNY4   ,NNZ1   ,NNZ2   ,
     G                  NNZ3   ,NNZ4   ,NOD_NORMAL,IADM,RCURVI ,
     H                  ANGLMI ,INTTH  ,TEMPI  ,PHI    ,AREASI ,
     I                  IELECI ,NSMS   ,CMAJ   ,DRAD2  , INTFRIC,IPARTFRICSI,
     K                  IPARTFRICMI,ITIED  ,JLT_TIED,CAND_F,IORTHFRIC,
     L                  IREP_FRICMI,DIR_FRICMI )
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
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, JLT_NEW, INACTI, INTFRIC, ITIED, JLT_TIED,IORTHFRIC,
     .        CAND_N(*), CN_LOC(MVSIZ), CAND_E(*), CE_LOC(MVSIZ), 
     .        NSVG(MVSIZ), KINI(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INDEX(MVSIZ),INTTH,IELECI(MVSIZ),
     .        NSMS(*),IPARTFRICSI(MVSIZ),IPARTFRICMI(MVSIZ),
     .        IREP_FRICMI(*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     KB1(MVSIZ), KB2(MVSIZ), KB3(MVSIZ), KB4(MVSIZ),
     .     KC1(MVSIZ), KC2(MVSIZ), KC3(MVSIZ), KC4(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), 
     .     GAPV(MVSIZ), CAND_P(*),
     .     VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .     TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ),DIR_FRICMI(MVSIZ,2)
      INTEGER IRECT(4,*),ICURV, IADM
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .    NOD_NORMAL(3,*), CAND_F(8,*)
      my_real
     .    RCURVI(MVSIZ), ANGLMI(MVSIZ),CMAJ(*),DRAD2
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J, K, KEEP(MVSIZ), IPERM,IORTHFRIC_LOC
      my_real
     .     X0(MVSIZ), Y0(MVSIZ), Z0(MVSIZ),
     .     AL1(MVSIZ),  AL2(MVSIZ),  AL3(MVSIZ), AL4(MVSIZ),
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ), X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ), Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ), Z04(MVSIZ),
     .     XI1(MVSIZ),  XI2(MVSIZ),  XI3(MVSIZ), XI4(MVSIZ),
     .     YI1(MVSIZ),  YI2(MVSIZ),  YI3(MVSIZ), YI4(MVSIZ),
     .     ZI1(MVSIZ),  ZI2(MVSIZ),  ZI3(MVSIZ), ZI4(MVSIZ),
     .     PENE2(MVSIZ),
     .     HLB1(MVSIZ), HLC1(MVSIZ), HLB2(MVSIZ),HLC2(MVSIZ),
     .     HLB3(MVSIZ), HLC3(MVSIZ), HLB4(MVSIZ),HLC4(MVSIZ)
      my_real
     .     S2,D1,D2,D3,D4,
     .     X12,X23,X34,X41,XI0,SX1,SX2,SX3,SX4,SX0,
     .     Y12,Y23,Y34,Y41,YI0,SY1,SY2,SY3,SY4,SY0,
     .     Z12,Z23,Z34,Z41,ZI0,SZ1,SZ2,SZ3,SZ4,SZ0,
C 
     .     GAP2, DS2,T1,T2,T3,XXX,YYY,ZZZ,
     .     AL1NUM,AL2NUM,AL3NUM,AL4NUM,AL1DEN,AL2DEN,AL3DEN,AL4DEN,
     .     X23D,Y23D,Z23D,X34D,Y34D,Z34D,X41D,Y41D,Z41D,
     .     X12D,Y12D,Z12D,GAP2D,XI0D,YI0D,ZI0D,S2D, LA, HLA, AAA,
     .     XI0V(MVSIZ),  YI0V(MVSIZ),  ZI0V(MVSIZ)
      my_real
     .     RPERM
      LOGICAL ITERATE
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES
C--------------------------------------------------------
       DO I=1,JLT
        IF(IX3(I).NE.IX4(I))THEN
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
C--------------------------------------------------------
C  UNIQUEMENT POUR PAQUET DE TRIANGLE
C--------------------------------------------------------
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES OU QUADRANGLE
C--------------------------------------------------------
C
       DO I=1,JLT
        CMAJ(I) = ZERO
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
        S2 = 1./MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
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
        S2 = 1./MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
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
        LB3(I) = -(SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
        LC3(I) =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
C
        SX0 = Y04(I)*Z01(I) - Z04(I)*Y01(I)
        SY0 = Z04(I)*X01(I) - X04(I)*Z01(I)
        SZ0 = X04(I)*Y01(I) - Y04(I)*X01(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB4(I) = -(SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
        LC4(I) =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
C
       ENDDO
#include   "nofusion.inc"
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
C
       ENDDO
C
#include   "nofusion.inc"
       DO I=1,JLT
        X12 = X2(I) - X1(I)
        Y12 = Y2(I) - Y1(I)
        Z12 = Z2(I) - Z1(I)
        LA = ONE - LB1(I) - LC1(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB1(I).AND.HLA.LE.HLC1(I))THEN
         LB1(I) = (XI2(I)*X12+YI2(I)*Y12+ZI2(I)*Z12) * AAA
         LB1(I) = MAX(ZERO,MIN(ONE,LB1(I)))
         LC1(I) = ONE - LB1(I)
        ELSEIF(LB1(I).LT.ZERO.AND.
     +         HLB1(I).LE.HLC1(I).AND.HLB1(I).LE.HLA)THEN
         LB1(I) = ZERO
         LC1(I) = AL2(I)
        ELSEIF(LC1(I).LT.ZERO.AND.
     +         HLC1(I).LE.HLA.AND.HLC1(I).LE.HLB1(I))THEN
         LC1(I) = ZERO
         LB1(I) = AL1(I)
        ENDIF
       ENDDO
C
#include   "nofusion.inc"
       DO I=1,JLT
        X23 = X3(I) - X2(I)
        Y23 = Y3(I) - Y2(I)
        Z23 = Z3(I) - Z2(I)
        LA = ONE - LB2(I) - LC2(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X23*X23+Y23*Y23+Z23*Z23)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB2(I).AND.HLA.LE.HLC2(I))THEN
         LB2(I) = (XI3(I)*X23+YI3(I)*Y23+ZI3(I)*Z23)*AAA
         LB2(I) = MAX(ZERO,MIN(ONE,LB2(I)))
         LC2(I) = ONE - LB2(I)
        ELSEIF(LB2(I).LT.ZERO.AND.
     +         HLB2(I).LE.HLC2(I).AND.HLB2(I).LE.HLA)THEN
         LB2(I) = ZERO
         LC2(I) = AL3(I)
        ELSEIF(LC2(I).LT.ZERO.AND.
     +         HLC2(I).LE.HLA.AND.HLC2(I).LE.HLB2(I))THEN
         LC2(I) = ZERO
         LB2(I) = AL2(I)
        ENDIF
       ENDDO
C
#include   "nofusion.inc"
       DO I=1,JLT
        X34 = X4(I) - X3(I)
        Y34 = Y4(I) - Y3(I)
        Z34 = Z4(I) - Z3(I)
        LA = ONE - LB3(I) - LC3(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X34*X34+Y34*Y34+Z34*Z34)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB3(I).AND.HLA.LE.HLC3(I))THEN
         LB3(I) = (XI4(I)*X34+YI4(I)*Y34+ZI4(I)*Z34)*AAA
         LB3(I) = MAX(ZERO,MIN(ONE,LB3(I)))
         LC3(I) = ONE - LB3(I)
        ELSEIF(LB3(I).LT.ZERO.AND.
     +         HLB3(I).LE.HLC3(I).AND.HLB3(I).LE.HLA)THEN
         LB3(I) = ZERO
         LC3(I) = AL4(I)
        ELSEIF(LC3(I).LT.ZERO.AND.
     +         HLC3(I).LE.HLA.AND.HLC3(I).LE.HLB3(I))THEN
         LC3(I) = ZERO
         LB3(I) = AL3(I)
        ENDIF
       ENDDO
C
#include   "nofusion.inc"
       DO I=1,JLT
        X41 = X1(I) - X4(I)
        Y41 = Y1(I) - Y4(I)
        Z41 = Z1(I) - Z4(I)
        LA = ONE - LB4(I) - LC4(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X41*X41+Y41*Y41+Z41*Z41)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB4(I).AND.HLA.LE.HLC4(I))THEN
         LB4(I) = (XI1(I)*X41+YI1(I)*Y41+ZI1(I)*Z41)*AAA
         LB4(I) = MAX(ZERO,MIN(ONE,LB4(I)))
         LC4(I) = ONE - LB4(I)
        ELSEIF(LB4(I).LT.ZERO.AND.
     +         HLB4(I).LE.HLC4(I).AND.HLB4(I).LE.HLA)THEN
         LB4(I) = ZERO
         LC4(I) = AL1(I)
        ELSEIF(LC4(I).LT.ZERO.AND.
     +         HLC4(I).LE.HLA.AND.HLC4(I).LE.HLB4(I))THEN
         LC4(I) = ZERO
         LB4(I) = AL4(I)
        ENDIF
       ENDDO
c---------------------------------------------------------
c      courbure cubique
c---------------------------------------------------------
       IF(ICURV == 3)THEN
         CALL I7CMAJ(JLT    ,CMAJ   ,IRECT  ,NOD_NORMAL,CAND_E,
     2               X1     ,X2     ,X3     ,X4        ,
     3               Y1     ,Y2     ,Y3     ,Y4        ,
     4               Z1     ,Z2     ,Z3     ,Z4        ,
     5               NNX1   ,NNX2   ,NNX3   ,NNX4      ,
     6               NNY1   ,NNY2   ,NNY3   ,NNY4      ,
     7               NNZ1   ,NNZ2   ,NNZ3   ,NNZ4      )
       ELSEIF(ICURV /= 0)THEN
#include   "nofusion.inc"
        DO I=1,JLT
          XXX=MAX(X1(I),X2(I),X3(I),X4(I))
     .       -MIN(X1(I),X2(I),X3(I),X4(I))
          YYY=MAX(Y1(I),Y2(I),Y3(I),Y4(I))
     .       -MIN(Y1(I),Y2(I),Y3(I),Y4(I))
          ZZZ=MAX(Z1(I),Z2(I),Z3(I),Z4(I))
     .       -MIN(Z1(I),Z2(I),Z3(I),Z4(I))
          CMAJ(I) = HALF * MAX(XXX,YYY,ZZZ)
        ENDDO
       ENDIF
c
#include   "nofusion.inc"
       DO I=1,JLT
        GAP2=(GAPV(I)+CMAJ(I))*(GAPV(I)+CMAJ(I))
        GAP2 =MAX(GAP2,DRAD2)
C
        NX1(I) = XI(I)-(X0(I) + LB1(I)*X01(I) + LC1(I)*X02(I))
        NY1(I) = YI(I)-(Y0(I) + LB1(I)*Y01(I) + LC1(I)*Y02(I))
        NZ1(I) = ZI(I)-(Z0(I) + LB1(I)*Z01(I) + LC1(I)*Z02(I))
        P1(I) = NX1(I)*NX1(I) + NY1(I)*NY1(I) +NZ1(I)*NZ1(I)
        D1 = MAX(ZERO, GAP2 - P1(I))
C
        NX2(I) = XI(I)-(X0(I) + LB2(I)*X02(I) + LC2(I)*X03(I))
        NY2(I) = YI(I)-(Y0(I) + LB2(I)*Y02(I) + LC2(I)*Y03(I))
        NZ2(I) = ZI(I)-(Z0(I) + LB2(I)*Z02(I) + LC2(I)*Z03(I))
        P2(I) = NX2(I)*NX2(I) + NY2(I)*NY2(I) +NZ2(I)*NZ2(I)
        D2 = MAX(ZERO, GAP2 - P2(I))
C
        NX3(I) = XI(I)-(X0(I) + LB3(I)*X03(I) + LC3(I)*X04(I))
        NY3(I) = YI(I)-(Y0(I) + LB3(I)*Y03(I) + LC3(I)*Y04(I))
        NZ3(I) = ZI(I)-(Z0(I) + LB3(I)*Z03(I) + LC3(I)*Z04(I))
        P3(I) = NX3(I)*NX3(I) + NY3(I)*NY3(I) +NZ3(I)*NZ3(I)
        D3 = MAX(ZERO, GAP2 - P3(I))
C
        NX4(I) = XI(I)-(X0(I) + LB4(I)*X04(I) + LC4(I)*X01(I))
        NY4(I) = YI(I)-(Y0(I) + LB4(I)*Y04(I) + LC4(I)*Y01(I))
        NZ4(I) = ZI(I)-(Z0(I) + LB4(I)*Z04(I) + LC4(I)*Z01(I))
        P4(I) = NX4(I)*NX4(I) + NY4(I)*NY4(I) +NZ4(I)*NZ4(I)
        D4 = MAX(ZERO, GAP2 - P4(I))
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE2 = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
        PENE2(I) = MAX(D1,D2,D3,D4)
C
       ENDDO
C---------------------
C      PENE INITIALE
C---------------------
      KEEP(1:JLT)=0
      IF(ITIED == 0)THEN
       IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
        DO I=1,JLT
          IF(PENE2(I).EQ.ZERO.OR.STIF(I).EQ.ZERO)THEN
            CAND_P(INDEX(I))=ZERO
          ENDIF
        ENDDO
       ENDIF
#include   "vectorize.inc"
       DO I=1,JLT
         IF( PENE2(I).NE.ZERO .AND. STIF(I).NE.ZERO)THEN
           KEEP(I) =1
         END IF
       ENDDO
      ELSE
       IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
        DO I=1,JLT
          IF((PENE2(I).EQ.ZERO.AND.CAND_F(1,INDEX(I)).EQ.ZERO).OR.
     .        STIF(I).EQ.ZERO)THEN
            CAND_P(INDEX(I))=ZERO
          ENDIF
        ENDDO
       ENDIF
#include   "vectorize.inc"
       DO I=1,JLT
         IF( (PENE2(I).NE.ZERO.OR.CAND_F(1,INDEX(I)).NE.ZERO) .AND.
     .       STIF(I).NE.ZERO)THEN
           IF( CAND_F(1,INDEX(I)).NE.ZERO )THEN
             JLT_TIED =JLT_TIED+1
             KEEP(I)  =2
           ELSE
             KEEP(I)  =1
           END IF
         END IF
       ENDDO
      END IF
C

c----copy IORTHFRIC in local flag

      IORTHFRIC_LOC = IORTHFRIC
      IF(IDTMINS < 2)THEN
C
       IF(INTFRIC == 0) THEN
        IF(INTTH == 0 ) THEN
         IF(ICURV.EQ.0.AND.IADM.EQ.0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ELSE
         IF(IADM == 0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C           
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW) = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
C
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW)   = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
C
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ENDIF 
       ELSE ! INTFRIC

        IF(INTTH == 0 ) THEN
         IF(ICURV.EQ.0.AND.IADM.EQ.0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ELSE
         IF(IADM == 0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C           
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW) = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
C
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW)   = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ENDIF 
       ENDIF

 
C
C IDTMINS=2
C ---------
      ELSE
C
       IF(INTFRIC == 0) THEN
        IF(INTTH == 0 ) THEN
         IF(ICURV.EQ.0.AND.IADM.EQ.0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C
            NSMS(JLT_NEW)  = NSMS(I)
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
C
            NSMS(JLT_NEW)  = NSMS(I)
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ELSE
         IF(IADM == 0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C           
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW) = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
C
            NSMS(JLT_NEW)  = NSMS(I)
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW)   = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
C
            NSMS(JLT_NEW)  = NSMS(I)
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ENDIF
       ELSE ! INTFRIC
        IF(INTTH == 0 ) THEN
         IF(ICURV.EQ.0.AND.IADM.EQ.0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C
            NSMS(JLT_NEW)  = NSMS(I)
C
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
C
            NSMS(JLT_NEW)  = NSMS(I)
C
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ELSE
         IF(IADM == 0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
C           
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW) = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
            NSMS(JLT_NEW)  = NSMS(I)
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include   "vectorize.inc"
          DO I=1,JLT
           IF(KEEP(I).NE.0)THEN
            JLT_NEW = JLT_NEW + 1
            CN_LOC(JLT_NEW) = CAND_N(I)
            CE_LOC(JLT_NEW) = CAND_E(I)
            IX1(JLT_NEW)  = IX1(I)
            IX2(JLT_NEW)  = IX2(I)
            IX3(JLT_NEW)  = IX3(I)
            IX4(JLT_NEW)  = IX4(I)
            NSVG(JLT_NEW) = NSVG(I)
            NX1(JLT_NEW)  = NX1(I)
            NX2(JLT_NEW)  = NX2(I)
            NX3(JLT_NEW)  = NX3(I)
            NX4(JLT_NEW)  = NX4(I)
            NY1(JLT_NEW)  = NY1(I)
            NY2(JLT_NEW)  = NY2(I)
            NY3(JLT_NEW)  = NY3(I)
            NY4(JLT_NEW)  = NY4(I)
            NZ1(JLT_NEW)  = NZ1(I)
            NZ2(JLT_NEW)  = NZ2(I)
            NZ3(JLT_NEW)  = NZ3(I)
            NZ4(JLT_NEW)  = NZ4(I)
            P1(JLT_NEW)   = P1(I)
            P2(JLT_NEW)   = P2(I)
            P3(JLT_NEW)   = P3(I)
            P4(JLT_NEW)   = P4(I)
            LB1(JLT_NEW)  = LB1(I)
            LB2(JLT_NEW)  = LB2(I)
            LB3(JLT_NEW)  = LB3(I)
            LB4(JLT_NEW)  = LB4(I)
            LC1(JLT_NEW)  = LC1(I)
            LC2(JLT_NEW)  = LC2(I)
            LC3(JLT_NEW)  = LC3(I)
            LC4(JLT_NEW)  = LC4(I)
            STIF(JLT_NEW) = STIF(I)
            GAPV(JLT_NEW) = GAPV(I)
            INDEX(JLT_NEW)= INDEX(I)
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
            XI(JLT_NEW)  = XI(I)
            YI(JLT_NEW)  = YI(I)
            ZI(JLT_NEW)  = ZI(I)
            X1(JLT_NEW)  = X1(I)
            Y1(JLT_NEW)  = Y1(I)
            Z1(JLT_NEW)  = Z1(I)
            X2(JLT_NEW)  = X2(I)
            Y2(JLT_NEW)  = Y2(I)
            Z2(JLT_NEW)  = Z2(I)
            X3(JLT_NEW)  = X3(I)
            Y3(JLT_NEW)  = Y3(I)
            Z3(JLT_NEW)  = Z3(I)
            X4(JLT_NEW)  = X4(I)
            Y4(JLT_NEW)  = Y4(I)
            Z4(JLT_NEW)  = Z4(I)
            RCURVI(JLT_NEW)  = RCURVI(I)
            ANGLMI(JLT_NEW)  = ANGLMI(I)
            TEMPI(JLT_NEW) = TEMPI(I)
            PHI(JLT_NEW)   = ZERO
            AREASI(JLT_NEW) = AREASI(I)
            IELECI(JLT_NEW) =IELECI(I)
C
            NSMS(JLT_NEW)  = NSMS(I)
C
            IPARTFRICSI(JLT_NEW)=IPARTFRICSI(I)
            IPARTFRICMI(JLT_NEW)=IPARTFRICMI(I)
            IF(IORTHFRIC_LOC >0) THEN
               IREP_FRICMI(JLT_NEW)= IREP_FRICMI(I)
               DIR_FRICMI(JLT_NEW,1:2) = DIR_FRICMI(I,1:2)
            ENDIF
C
            KEEP(JLT_NEW) = KEEP(I)
C
           ENDIF
          ENDDO
         ENDIF
        ENDIF
       ENDIF  
      END IF
C          
      IF(ITIED/=0.AND.JLT_TIED/=0)THEN
C---------------------------------------------
C       Tri a bulle
C---------------------------------------------
        ITERATE=.TRUE.
        DO WHILE (ITERATE .EQV. .TRUE.)     
           ITERATE=.FALSE.
           DO J=1,JLT_NEW-1
             IF(KEEP(J) > KEEP(J+1) )THEN

               IPERM     =KEEP(J)
               KEEP(J)   =KEEP(J+1)
               KEEP(J+1) =IPERM

               IPERM	  =CN_LOC(J)
               CN_LOC(J)  =CN_LOC(J+1)
               CN_LOC(J+1)=IPERM
               IPERM	  =CE_LOC(J)
               CE_LOC(J)  =CE_LOC(J+1)
               CE_LOC(J+1)=IPERM
               IPERM	  =IX1(J)
               IX1(J)	  =IX1(J+1)
               IX1(J+1)   =IPERM
               IPERM	  =IX2(J)
               IX2(J)	  =IX2(J+1)
               IX2(J+1)   =IPERM
               IPERM	  =IX3(J)
               IX3(J)	  =IX3(J+1)
               IX3(J+1)   =IPERM
               IPERM	  =IX4(J)
               IX4(J)	  =IX4(J+1)
               IX4(J+1)   =IPERM
               IPERM	  =NSVG(J)
               NSVG(J)    =NSVG(J+1)
               NSVG(J+1)  =IPERM
               RPERM	  =NX1(J)
               NX1(J)	  =NX1(J+1)
               NX1(J+1)   =RPERM
               RPERM	  =NX2(J)
               NX2(J)	  =NX2(J+1)
               NX2(J+1)   =RPERM
               RPERM	  =NX3(J)
               NX3(J)	  =NX3(J+1)
               NX3(J+1)   =RPERM
               RPERM	  =NX4(J)
               NX4(J)	  =NX4(J+1)
               NX4(J+1)   =RPERM
               RPERM	  =NY1(J)
               NY1(J)	  =NY1(J+1)
               NY1(J+1)   =RPERM
               RPERM	  =NY2(J)
               NY2(J)	  =NY2(J+1)
               NY2(J+1)   =RPERM
               RPERM	  =NY3(J)
               NY3(J)	  =NY3(J+1)
               NY3(J+1)   =RPERM
               RPERM	  =NY4(J)
               NY4(J)	  =NY4(J+1)
               NY4(J+1)   =RPERM
               RPERM	  =NZ1(J)
               NZ1(J)	  =NZ1(J+1)
               NZ1(J+1)   =RPERM
               RPERM	  =NZ2(J)
               NZ2(J)	  =NZ2(J+1)
               NZ2(J+1)   =RPERM
               RPERM	  =NZ3(J)
               NZ3(J)	  =NZ3(J+1)
               NZ3(J+1)   =RPERM
               RPERM	  =NZ4(J)
               NZ4(J)	  =NZ4(J+1)
               NZ4(J+1)   =RPERM
               RPERM	  =P1(J)
               P1(J)	  =P1(J+1)
               P1(J+1)    =RPERM
               RPERM	  =P2(J)
               P2(J)	  =P2(J+1)
               P2(J+1)    =RPERM
               RPERM	  =P3(J)
               P3(J)	  =P3(J+1)
               P3(J+1)    =RPERM
               RPERM	  =P4(J)
               P4(J)	  =P4(J+1)
               P4(J+1)    =RPERM
               RPERM	  =LB1(J)
               LB1(J)	  =LB1(J+1)
               LB1(J+1)   =RPERM
               RPERM	  =LB2(J)
               LB2(J)	  =LB2(J+1)
               LB2(J+1)   =RPERM
               RPERM	  =LB3(J)
               LB3(J)	  =LB3(J+1)
               LB3(J+1)   =RPERM
               RPERM	  =LB4(J)
               LB4(J)	  =LB4(J+1)
               LB4(J+1)   =RPERM
               RPERM	  =LC1(J)
               LC1(J)	  =LC1(J+1)
               LC1(J+1)   =RPERM
               RPERM	  =LC2(J)
               LC2(J)	  =LC2(J+1)
               LC2(J+1)   =RPERM
               RPERM	  =LC3(J)
               LC3(J)	  =LC3(J+1)
               LC3(J+1)   =RPERM
               RPERM	  =LC4(J)
               LC4(J)	  =LC4(J+1)
               LC4(J+1)   =RPERM

               RPERM	  =STIF(J)
               STIF(J)    =STIF(J+1)
               STIF(J+1)  =RPERM
               RPERM	  =GAPV(J)
               GAPV(J)    =GAPV(J+1)
               GAPV(J+1)  =RPERM

               IPERM	  =INDEX(J)
               INDEX(J)   =INDEX(J+1)
               INDEX(J+1) =IPERM
               IPERM	  =KINI(J)
               KINI(J)    =KINI(J+1)
               KINI(J+1)  =IPERM
               
               RPERM	  =VXI(J)
               VXI(J)	  =VXI(J+1)
               VXI(J+1)   =RPERM
               RPERM	  =VYI(J)
               VYI(J)	  =VYI(J+1)
               VYI(J+1)   =RPERM
               RPERM	  =VZI(J)
               VZI(J)	  =VZI(J+1)
               VZI(J+1)   =RPERM
               RPERM	  =MSI(J)
               MSI(J)	  =MSI(J+1)
               MSI(J+1)   =RPERM
               RPERM	  =XI(J)
               XI(J)	  =XI(J+1)
               XI(J+1)    =RPERM
               RPERM	  =YI(J)
               YI(J)	  =YI(J+1)
               YI(J+1)    =RPERM
               RPERM	  =ZI(J)
               ZI(J)	  =ZI(J+1)
               ZI(J+1)    =RPERM
               RPERM	  =X1(J)
               X1(J)	  =X1(J+1)
               X1(J+1)    =RPERM
               RPERM	  =Y1(J)
               Y1(J)	  =Y1(J+1)
               Y1(J+1)    =RPERM
               RPERM	  =Z1(J)
               Z1(J)	  =Z1(J+1)
               Z1(J+1)    =RPERM
               RPERM	  =X2(J)
               X2(J)	  =X2(J+1)
               X2(J+1)    =RPERM
               RPERM	  =Y2(J)
               Y2(J)	  =Y2(J+1)
               Y2(J+1)    =RPERM
               RPERM	  =Z2(J)
               Z2(J)	  =Z2(J+1)
               Z2(J+1)    =RPERM
               RPERM	  =X3(J)
               X3(J)	  =X3(J+1)
               X3(J+1)    =RPERM
               RPERM	  =Y3(J)
               Y3(J)	  =Y3(J+1)
               Y3(J+1)    =RPERM
               RPERM	  =Z3(J)
               Z3(J)	  =Z3(J+1)
               Z3(J+1)    =RPERM
               RPERM	  =X4(J)
               X4(J)	  =X4(J+1)
               X4(J+1)    =RPERM
               RPERM	  =Y4(J)
               Y4(J)	  =Y4(J+1)
               Y4(J+1)    =RPERM
               RPERM	  =Z4(J)
               Z4(J)	  =Z4(J+1)
               Z4(J+1)    =RPERM
C
C
               IF(ICURV/=0.OR.IADM/=0)THEN
                 RPERM	     =RCURVI(J)
                 RCURVI(J)   =RCURVI(J+1)
                 RCURVI(J+1) =RPERM
                 RPERM	     =ANGLMI(J)
                 ANGLMI(J)   =ANGLMI(J+1)
                 ANGLMI(J+1) =RPERM
               END IF

               IF(INTTH/=0)THEN
                 RPERM	     =TEMPI(J)
                 TEMPI(J)    =TEMPI(J+1)
                 TEMPI(J+1)  =RPERM
C                PHI(J)      = ZERO
                 RPERM	     =AREASI(J)
                 AREASI(J)   =AREASI(J+1)
                 AREASI(J+1) =RPERM
                 IPERM	     =IELECI(J)
                 IELECI(J)   =IELECI(J+1)
                 IELECI(J+1) =IPERM
               END IF

C
               IF(INTFRIC/=0)THEN
                 IPERM	          =IPARTFRICSI(J)
                 IPARTFRICSI(J)   =IPARTFRICSI(J+1)
                 IPARTFRICSI(J+1) =IPERM
                 IPERM	          =IPARTFRICMI(J)
                 IPARTFRICMI(J)   =IPARTFRICMI(J+1)
                 IPARTFRICMI(J+1) =IPERM
                 IF(IORTHFRIC_LOC >0) THEN
                    IPERM	  =IREP_FRICMI(J)
                    IREP_FRICMI(J) = IREP_FRICMI(J+1)
                    IREP_FRICMI(J+1)= IPERM
                    RPERM	  = DIR_FRICMI(J,1)
                    DIR_FRICMI(J,1)  = DIR_FRICMI(J+1,1)
                    DIR_FRICMI(J+1,1)= RPERM
                    RPERM	  = DIR_FRICMI(J,2)
                    DIR_FRICMI(J,2)  = DIR_FRICMI(J+1,2)
                    DIR_FRICMI(J+1,2)= RPERM
                 ENDIF
               END IF

               IF(IDTMINS==2)THEN
                 IPERM	     =NSMS(J)
                 NSMS(J)     =NSMS(J+1)
                 NSMS(J+1)   =IPERM
               END IF

               ITERATE = .TRUE.
             ENDIF
           ENDDO
        ENDDO
      END IF ! IF(ITIED/=0.AND.JLT_TIED/=0)THEN

C
      RETURN
      END
