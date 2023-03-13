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
Chd|  I20DST3                       source/interfaces/int20/i20dst3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        I20CGAP0                      source/interfaces/int20/i20dst3.F
Chd|        I20CGAP1                      source/interfaces/int20/i20dst3.F
Chd|        I20CMAJ                       source/interfaces/int20/i20curv.F
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|====================================================================
      SUBROUTINE I20DST3(
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
     B                  JLT_NEW        ,GAPV   ,INACTI ,SOLIDN_NORMAL,
     C                  INDEX  ,VXI    ,VYI    ,GAPR   ,GAP_SH ,
     D                  VZI    ,MSI    ,KINI   ,ICURV  ,IRECT  ,
     E                  NNX1   ,NNX2   ,NNX3   ,NNX4   ,NNY1   ,
     F                  NNY2   ,NNY3   ,NNY4   ,NNZ1   ,NNZ2   ,
     G                  NNZ3   ,NNZ4   ,NOD_NORMAL,IADM,RCURVI ,
     H                  ANGLMI ,INTTH  ,TEMPI  ,PHI    ,AREASI ,
     I                  IELECI ,NLN    ,NLG    ,IGAP   ,GAPMAX ,
     J                  SOLIDN_NORMAL_F,NSMS   ,NBINFLG,GAP_M  ,
     K                  CMAJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, JLT_NEW,INACTI, IGAP,CAND_N(*),CN_LOC(MVSIZ),
     .        CAND_E(*),CE_LOC(MVSIZ), NSVG(MVSIZ), KINI(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INDEX(MVSIZ),INTTH,IELECI(MVSIZ),NLN,NLG(NLN),
     .        NSMS(MVSIZ) ,NBINFLG(*)
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
     .     GAPV(MVSIZ), GAPR(MVSIZ),GAP_SH(*), GAP_M(*), 
     .     VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .     TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ),GAPMAX     
      INTEGER IRECT(4,*),ICURV, IADM
      INTEGER SOLIDN_NORMAL(3,*), SOLIDN_NORMAL_F(3,*)
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .    NOD_NORMAL(3,*)
      my_real
     .    RCURVI(MVSIZ), ANGLMI(MVSIZ),CMAJ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J,M,I1,I2,I3,I4,IB1,IB2,IB3,IB4
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
     .     S2,D1,D2,D3,D4,XXX,YYY,ZZZ,
     .     X12,X23,X34,X41,XI0,SX1,SX2,SX3,SX4,SX0,SXI,
     .     Y12,Y23,Y34,Y41,YI0,SY1,SY2,SY3,SY4,SY0,SYI,
     .     Z12,Z23,Z34,Z41,ZI0,SZ1,SZ2,SZ3,SZ4,SZ0,SZI,
     .     X10,Y10,Z10,X20,Y20,Z20,X30,Y30,Z30,X40,Y40,Z40,
     .     GAP2, DS2,T1,T2,T3,
     .     AL1NUM,AL2NUM,AL3NUM,AL4NUM,AL1DEN,AL2DEN,AL3DEN,AL4DEN,
     .     X23D,Y23D,Z23D,X34D,Y34D,Z34D,X41D,Y41D,Z41D, UNSSQR3,
     .     X12D,Y12D,Z12D,GAP2D,XI0D,YI0D,ZI0D,S2D, LA, HLA, AAA,
     .     XI0V(MVSIZ),  YI0V(MVSIZ),  ZI0V(MVSIZ)

      INTEGER BITUNSET,BITGET,BITSET
      EXTERNAL BITUNSET,BITGET,BITSET
C--------------------------------------------------------
C  SHIFT DU GAP POUR SOLIDES AVEC GAP NUL
C--------------------------------------------------------
       IF(IGAP /= 0)THEN 
         UNSSQR3 = ONE/SQR3
         DO I=1,JLT 
           M = CAND_E(I)                     
           IG = NSVG(I)
           I1 = NLG(IRECT(1,M))
           I2 = NLG(IRECT(2,M))
           I3 = NLG(IRECT(3,M))
           I4 = NLG(IRECT(4,M))

           IB1 = BITGET(NBINFLG(IRECT(1,M)),7)
           IB2 = BITGET(NBINFLG(IRECT(2,M)),7)
           IB3 = BITGET(NBINFLG(IRECT(3,M)),7)
           IB4 = BITGET(NBINFLG(IRECT(4,M)),7)

           IF(IB1+IB2+IB3+IB4 == 0)THEN
C elementary normals
             SX0=(Y3(I)-Y1(I))*(Z4(I)-Z2(I))-(Z3(I)-Z1(I))*(Y4(I)-Y2(I))
             SY0=(Z3(I)-Z1(I))*(X4(I)-X2(I))-(X3(I)-X1(I))*(Z4(I)-Z2(I))
             SZ0=(X3(I)-X1(I))*(Y4(I)-Y2(I))-(Y3(I)-Y1(I))*(X4(I)-X2(I))
             AAA = ONE / SQRT(MAX(EM20,SX0*SX0+SY0*SY0+SZ0*SZ0))
             SX0=SX0*AAA
             SY0=SY0*AAA
             SZ0=SZ0*AAA
c second normal substracted from main normal
c equiv to distribute shift over main and secondary
             IF(IG > 0) THEN
               SXI = SOLIDN_NORMAL(1,IG)
               SYI = SOLIDN_NORMAL(2,IG)
               SZI = SOLIDN_NORMAL(3,IG)
             ELSE
C remote nodes
               IG = -IG
               SXI = SOLIDN_NORMAL_F(1,IG)
               SYI = SOLIDN_NORMAL_F(2,IG)
               SZI = SOLIDN_NORMAL_F(3,IG)
             END IF
             AAA = SXI*SX0+SYI*SY0+SZI*SZ0
             IF(AAA > ZERO)THEN
               SXI = ZERO
               SYI = ZERO
               SZI = ZERO
             ENDIF
             SX1 = SOLIDN_NORMAL(1,I1)-SXI
             SY1 = SOLIDN_NORMAL(2,I1)-SYI
             SZ1 = SOLIDN_NORMAL(3,I1)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX1*SX1+SY1*SY1+SZ1*SZ1))
             SX1 = SX1*AAA
             SY1 = SY1*AAA
             SZ1 = SZ1*AAA
             SX2 = SOLIDN_NORMAL(1,I2)-SXI
             SY2 = SOLIDN_NORMAL(2,I2)-SYI
             SZ2 = SOLIDN_NORMAL(3,I2)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX2*SX2+SY2*SY2+SZ2*SZ2))
             SX2 = SX2*AAA
             SY2 = SY2*AAA
             SZ2 = SZ2*AAA
             SX3 = SOLIDN_NORMAL(1,I3)-SXI
             SY3 = SOLIDN_NORMAL(2,I3)-SYI
             SZ3 = SOLIDN_NORMAL(3,I3)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX3*SX3+SY3*SY3+SZ3*SZ3))
             SX3 = SX3*AAA
             SY3 = SY3*AAA
             SZ3 = SZ3*AAA
             SX4 = SOLIDN_NORMAL(1,I4)-SXI
             SY4 = SOLIDN_NORMAL(2,I4)-SYI
             SZ4 = SOLIDN_NORMAL(3,I4)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX4*SX4+SY4*SY4+SZ4*SZ4))
             SX4 = SX4*AAA
             SY4 = SY4*AAA
             SZ4 = SZ4*AAA
c shift nodales
             AAA = SX0*SX1 + SY0*SY1 + SZ0*SZ1
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(M)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX1 = SX1 + AAA*SX0
               SY1 = SY1 + AAA*SY0
               SZ1 = SZ1 + AAA*SZ0
               AAA = GAP_SH(M)*SQR3
             ENDIF               
             SX1 = SX1*AAA
             SY1 = SY1*AAA
             SZ1 = SZ1*AAA
             AAA = SX0*SX2 + SY0*SY2 + SZ0*SZ2
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(M)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX2 = SX2 + AAA*SX0
               SY2 = SY2 + AAA*SY0
               SZ2 = SZ2 + AAA*SZ0
               AAA = GAP_SH(M)*SQR3
             ENDIF               
             SX2 = SX2*AAA
             SY2 = SY2*AAA
             SZ2 = SZ2*AAA
             AAA = SX0*SX3 + SY0*SY3 + SZ0*SZ3
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(M)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX3 = SX3 + AAA*SX0
               SY3 = SY3 + AAA*SY0
               SZ3 = SZ3 + AAA*SZ0
               AAA = GAP_SH(M)*SQR3
             ENDIF               
             SX3 = SX3*AAA
             SY3 = SY3*AAA
             SZ3 = SZ3*AAA
             AAA = SX0*SX4 + SY0*SY4 + SZ0*SZ4
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(M)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX4 = SX4 + AAA*SX0
               SY4 = SY4 + AAA*SY0
               SZ4 = SZ4 + AAA*SZ0
               AAA = GAP_SH(M)*SQR3
             ENDIF               
             SX4 = SX4*AAA
             SY4 = SY4*AAA
             SZ4 = SZ4*AAA

             X1(I) = X1(I) - SX1
             Y1(I) = Y1(I) - SY1
             Z1(I) = Z1(I) - SZ1
             X2(I) = X2(I) - SX2
             Y2(I) = Y2(I) - SY2
             Z2(I) = Z2(I) - SZ2
             X3(I) = X3(I) - SX3
             Y3(I) = Y3(I) - SY3
             Z3(I) = Z3(I) - SZ3
             X4(I) = X4(I) - SX4
             Y4(I) = Y4(I) - SY4
             Z4(I) = Z4(I) - SZ4

           ELSEIF(I3 == I4)THEN
c            bord de coque3N
             X10 = X1(I)
             Y10 = Y1(I)
             Z10 = Z1(I)
             X20 = X2(I)
             Y20 = Y2(I)
             Z20 = Z2(I)
             X30 = X3(I)
             Y30 = Y3(I)
             Z30 = Z3(I)
             IF(IB1 == 1 .and. IB2 == 1)THEN
               CALL I20CGAP0(X10,Y10,Z10,X20,Y20,Z20,
     .                       X30,Y30,Z30,X30,Y30,Z30,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X3(I),Y3(I),Z3(I),X3(I),Y3(I),Z3(I),
     .                       GAP_M(M))
             ENDIF
             IF(IB2 == 1 .and. IB3 == 1)THEN
               CALL I20CGAP0(X20,Y20,Z20,X30,Y30,Z30,
     .                       X10,Y10,Z10,X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X1(I),Y1(I),Z1(I),X1(I),Y1(I),Z1(I),
     .                       GAP_M(M))
             ENDIF
             IF(IB3 == 1 .and. IB1 == 1)THEN
               CALL I20CGAP0(X30,Y30,Z30,X10,Y10,Z10,
     .                       X20,Y20,Z20,X20,Y20,Z20,
     .                       X3(I),Y3(I),Z3(I),X1(I),Y1(I),Z1(I),
     .                       X2(I),Y2(I),Z2(I),X2(I),Y2(I),Z2(I),
     .                       GAP_M(M))
             ENDIF
             IF(IB1 == 1 .and. IB2+IB3 == 0)THEN
               CALL I20CGAP1(X10,Y10,Z10,X20,Y20,Z20,
     .                       X30,Y30,Z30,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X3(I),Y3(I),Z3(I),GAP_M(M))
             ENDIF
             IF(IB2 == 1 .and. IB3+IB1 == 0)THEN
               CALL I20CGAP1(X20,Y20,Z20,X30,Y30,Z30,
     .                       X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X1(I),Y1(I),Z1(I),GAP_M(M))
             ENDIF
             IF(IB3 == 1 .and. IB1+IB2 == 0)THEN
               CALL I20CGAP1(X30,Y30,Z30,X10,Y10,Z10,
     .                       X20,Y20,Z20,
     .                       X3(I),Y3(I),Z3(I),X1(I),Y1(I),Z1(I),
     .                       X2(I),Y2(I),Z2(I),GAP_M(M))
             ENDIF
             X4(I)=X3(I)
             Y4(I)=Y3(I)
             Z4(I)=Z3(I)
           ELSE
c            bord de coque
             X10 = X1(I)
             Y10 = Y1(I)
             Z10 = Z1(I)
             X20 = X2(I)
             Y20 = Y2(I)
             Z20 = Z2(I)
             X30 = X3(I)
             Y30 = Y3(I)
             Z30 = Z3(I)
             X40 = X4(I)
             Y40 = Y4(I)
             Z40 = Z4(I)
             IF(IB1 == 1 .and. IB2 == 1)THEN
               CALL I20CGAP0(X10,Y10,Z10,X20,Y20,Z20,
     .                       X30,Y30,Z30,X40,Y40,Z40,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X3(I),Y3(I),Z3(I),X4(I),Y4(I),Z4(I),
     .                       GAP_M(M))
             ENDIF
             IF(IB2 == 1 .and. IB3 == 1)THEN
               CALL I20CGAP0(X20,Y20,Z20,X30,Y30,Z30,
     .                       X40,Y40,Z40,X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X4(I),Y4(I),Z4(I),X1(I),Y1(I),Z1(I),
     .                       GAP_M(M))
             ENDIF
             IF(IB3 == 1 .and. IB4 == 1)THEN
               CALL I20CGAP0(X30,Y30,Z30,X40,Y40,Z40,
     .                       X10,Y10,Z10,X20,Y20,Z20,
     .                       X3(I),Y3(I),Z3(I),X4(I),Y4(I),Z4(I),
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       GAP_M(M))
             ENDIF
             IF(IB4 == 1 .and. IB1 == 1)THEN
               CALL I20CGAP0(X40,Y40,Z40,X10,Y10,Z10,
     .                       X20,Y20,Z20,X30,Y30,Z30,
     .                       X4(I),Y4(I),Z4(I),X1(I),Y1(I),Z1(I),
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       GAP_M(M))
             ENDIF
             IF(IB1 == 1 .and. IB2+IB4 == 0)THEN
               CALL I20CGAP1(X10,Y10,Z10,X20,Y20,Z20,
     .                       X40,Y40,Z40,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X4(I),Y4(I),Z4(I),GAP_M(M))
             ENDIF
             IF(IB2 == 1 .and. IB3+IB1 == 0)THEN
               CALL I20CGAP1(X20,Y20,Z20,X30,Y30,Z30,
     .                       X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X1(I),Y1(I),Z1(I),GAP_M(M))
             ENDIF
             IF(IB3 == 1 .and. IB4+IB2 == 0)THEN
               CALL I20CGAP1(X30,Y30,Z30,X40,Y40,Z40,
     .                       X10,Y10,Z10,
     .                       X3(I),Y3(I),Z3(I),X4(I),Y4(I),Z4(I),
     .                       X2(I),Y2(I),Z2(I),GAP_M(M))
             ENDIF
             IF(IB4 == 1 .and. IB1+IB3 == 0)THEN
               CALL I20CGAP1(X40,Y40,Z40,X10,Y10,Z10,
     .                       X30,Y30,Z30,
     .                       X4(I),Y4(I),Z4(I),X1(I),Y1(I),Z1(I),
     .                       X3(I),Y3(I),Z3(I),GAP_M(M))
             ENDIF
           ENDIF
         ENDDO                           
       ENDIF
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
C
       DO I=1,JLT
        X12 = X2(I) - X1(I)
        Y12 = Y2(I) - Y1(I)
        Z12 = Z2(I) - Z1(I)
        LA = ONE - LB1(I) - LC1(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB1(I).AND.HLA<=HLC1(I))THEN
         LB1(I) = (XI2(I)*X12+YI2(I)*Y12+ZI2(I)*Z12) * AAA
         LB1(I) = MAX(ZERO,MIN(ONE,LB1(I)))
         LC1(I) = ONE - LB1(I)
        ELSEIF(LB1(I)<ZERO.AND.
     +         HLB1(I)<=HLC1(I).AND.HLB1(I)<=HLA)THEN
         LB1(I) = ZERO
         LC1(I) = AL2(I)
        ELSEIF(LC1(I)<ZERO.AND.
     +         HLC1(I)<=HLA.AND.HLC1(I)<=HLB1(I))THEN
         LC1(I) = ZERO
         LB1(I) = AL1(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
        X23 = X3(I) - X2(I)
        Y23 = Y3(I) - Y2(I)
        Z23 = Z3(I) - Z2(I)
        LA = ONE - LB2(I) - LC2(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X23*X23+Y23*Y23+Z23*Z23)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB2(I).AND.HLA<=HLC2(I))THEN
         LB2(I) = (XI3(I)*X23+YI3(I)*Y23+ZI3(I)*Z23)*AAA
         LB2(I) = MAX(ZERO,MIN(ONE,LB2(I)))
         LC2(I) = ONE - LB2(I)
        ELSEIF(LB2(I)<ZERO.AND.
     +         HLB2(I)<=HLC2(I).AND.HLB2(I)<=HLA)THEN
         LB2(I) = ZERO
         LC2(I) = AL3(I)
        ELSEIF(LC2(I)<ZERO.AND.
     +         HLC2(I)<=HLA.AND.HLC2(I)<=HLB2(I))THEN
         LC2(I) = ZERO
         LB2(I) = AL2(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
        X34 = X4(I) - X3(I)
        Y34 = Y4(I) - Y3(I)
        Z34 = Z4(I) - Z3(I)
        LA = ONE - LB3(I) - LC3(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X34*X34+Y34*Y34+Z34*Z34)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB3(I).AND.HLA<=HLC3(I))THEN
         LB3(I) = (XI4(I)*X34+YI4(I)*Y34+ZI4(I)*Z34)*AAA
         LB3(I) = MAX(ZERO,MIN(ONE,LB3(I)))
         LC3(I) = ONE - LB3(I)
        ELSEIF(LB3(I)<ZERO.AND.
     +         HLB3(I)<=HLC3(I).AND.HLB3(I)<=HLA)THEN
         LB3(I) = ZERO
         LC3(I) = AL4(I)
        ELSEIF(LC3(I)<ZERO.AND.
     +         HLC3(I)<=HLA.AND.HLC3(I)<=HLB3(I))THEN
         LC3(I) = ZERO
         LB3(I) = AL3(I)
        ENDIF
       ENDDO
C
       DO I=1,JLT
        X41 = X1(I) - X4(I)
        Y41 = Y1(I) - Y4(I)
        Z41 = Z1(I) - Z4(I)
        LA = ONE - LB4(I) - LC4(I)
Cel     HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X41*X41+Y41*Y41+Z41*Z41)
        HLA= LA*ABS(LA) * AAA
        IF(LA<ZERO.AND.
     +     HLA<=HLB4(I).AND.HLA<=HLC4(I))THEN
         LB4(I) = (XI1(I)*X41+YI1(I)*Y41+ZI1(I)*Z41)*AAA
         LB4(I) = MAX(ZERO,MIN(ONE,LB4(I)))
         LC4(I) = ONE - LB4(I)
        ELSEIF(LB4(I)<ZERO.AND.
     +         HLB4(I)<=HLC4(I).AND.HLB4(I)<=HLA)THEN
         LB4(I) = ZERO
         LC4(I) = AL1(I)
        ELSEIF(LC4(I)<ZERO.AND.
     +         HLC4(I)<=HLA.AND.HLC4(I)<=HLB4(I))THEN
         LC4(I) = ZERO
         LB4(I) = AL4(I)
        ENDIF
       ENDDO
c---------------------------------------------------------
c      courbure cubique
c---------------------------------------------------------
       IF(ICURV == 3)THEN
         CALL I20CMAJ(JLT    ,CMAJ   ,IRECT  ,NOD_NORMAL,CAND_E,
     2                X1     ,X2     ,X3     ,X4        ,NLN   ,
     3                Y1     ,Y2     ,Y3     ,Y4        ,NLG   ,
     4                Z1     ,Z2     ,Z3     ,Z4        ,
     5                NNX1   ,NNX2   ,NNX3   ,NNX4      ,
     6                NNY1   ,NNY2   ,NNY3   ,NNY4      ,
     7                NNZ1   ,NNZ2   ,NNZ3   ,NNZ4      )
       ELSEIF(ICURV /= 0)THEN
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
       DO I=1,JLT
        GAP2=(GAPV(I)+CMAJ(I))*(GAPV(I)+CMAJ(I))
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
c       IF(INACTI==5.OR.INACTI==6)THEN
c        DO I=1,JLT
c          IF(PENE2(I)==ZERO.OR.STIF(I)==ZERO)THEN
c            CAND_P(INDEX(I))=0
c          ENDIF
c        ENDDO
c       ENDIF
C
       IF(IDTMINS/=2)THEN
        IF(INTTH == 0 ) THEN
         IF(ICURV==0.AND.IADM==0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include  "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
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
           ENDIF
          ENDDO
         ENDIF
       ELSE
         IF(IADM == 0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
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
           ENDIF
          ENDDO
         ELSE    ! 
#include  "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
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
           ENDIF
          ENDDO
         ENDIF
       ENDIF  
      ELSE
C------ IDTMINS=2
        IF(INTTH == 0 ) THEN
         IF(ICURV==0.AND.IADM==0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
            INDEX(JLT_NEW)= INDEX(I)
C
            KINI(JLT_NEW) = KINI(I)
            VXI(JLT_NEW)  = VXI(I)
            VYI(JLT_NEW)  = VYI(I)
            VZI(JLT_NEW)  = VZI(I)
            MSI(JLT_NEW)  = MSI(I)
C
            NSMS(JLT_NEW) = NSMS(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include  "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
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
            NSMS(JLT_NEW) = NSMS(I)
           ENDIF
          ENDDO
         ENDIF
       ELSE
         IF(IADM == 0 )THEN
#include   "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
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
            NSMS(JLT_NEW) = NSMS(I)
C
           ENDIF
          ENDDO
         ELSE    ! 
#include  "vectorize.inc"
          DO I=1,JLT
           IF(PENE2(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
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
            GAPR(JLT_NEW) = GAPR(I)
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
            NSMS(JLT_NEW) = NSMS(I)
           ENDIF
          ENDDO
         ENDIF
       ENDIF  
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  I20CGAP0                      source/interfaces/int20/i20dst3.F
Chd|-- called by -----------
Chd|        I20DST3                       source/interfaces/int20/i20dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20CGAP0(X10,Y10,Z10,X20,Y20,Z20,
     .                    X30,Y30,Z30,X40,Y40,Z40,
     .                    X1,Y1,Z1,X2,Y2,Z2,
     .                    X3,Y3,Z3,X4,Y4,Z4,GAP_M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     . X10,Y10,Z10,X20,Y20,Z20,X30,Y30,Z30,X40,Y40,Z40,
     . X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,GAP_M
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     . L12,L12X,L12Y,L12Z,
     . L23,L23X,L23Y,L23Z,L14,L14X,L14Y,L14Z,AAA,BBB,L1214,L1223
C=======================================================================
      L12X = X20-X10
      L12Y = Y20-Y10
      L12Z = Z20-Z10
      L12 = SQRT(L12X*L12X+L12Y*L12Y+L12Z*L12Z)
      AAA = ONE/MAX(L12,EM30)
      L12X = L12X*AAA
      L12Y = L12Y*AAA
      L12Z = L12Z*AAA

      L14X = X40-X10
      L14Y = Y40-Y10
      L14Z = Z40-Z10
      L14 = SQRT(L14X*L14X+L14Y*L14Y+L14Z*L14Z)
      AAA = ONE/MAX(L14,EM30)
      L14X = L14X*AAA
      L14Y = L14Y*AAA
      L14Z = L14Z*AAA

      L23X = X30-X20
      L23Y = Y30-Y20
      L23Z = Z30-Z20
      L23 = SQRT(L23X*L23X+L23Y*L23Y+L23Z*L23Z)
      AAA = ONE/MAX(L23,EM30)
      L23X = L23X*AAA
      L23Y = L23Y*AAA
      L23Z = L23Z*AAA

      L1214 = L12X*L14X+L12Y*L14Y+L12Z*L14Z

      BBB = GAP_M*(ONE+EM5)/MAX(SQRT(ONE-L1214*L1214),EM30)
      AAA = MIN(L12/THREE,BBB)

      X1 = X1 + L14X*AAA
      Y1 = Y1 + L14Y*AAA
      Z1 = Z1 + L14Z*AAA

      L1223 = L12X*L23X+L12Y*L23Y+L12Z*L23Z

      BBB = GAP_M*(ONE+EM5)/MAX(SQRT(ONE-L1223*L1223),EM30)
      AAA = MIN(L14/THREE,BBB)

      X2 = X2 + L23X*AAA
      Y2 = Y2 + L23Y*AAA
      Z2 = Z2 + L23Z*AAA

      RETURN
      END
Chd|====================================================================
Chd|  I20CGAP1                      source/interfaces/int20/i20dst3.F
Chd|-- called by -----------
Chd|        I20DST3                       source/interfaces/int20/i20dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20CGAP1(X10,Y10,Z10,X20,Y20,Z20,
     .                    X40,Y40,Z40,
     .                    X1,Y1,Z1,X2,Y2,Z2,
     .                    X4,Y4,Z4,GAP_M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     . X10,Y10,Z10,X20,Y20,Z20,X40,Y40,Z40,
     . X1,Y1,Z1,X2,Y2,Z2,X4,Y4,Z4,GAP_M
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     . L12,L12X,L12Y,L12Z,L14,L14X,L14Y,L14Z,AAA,BBB,L1214
C=======================================================================
      L12X = X20-X10
      L12Y = Y20-Y10
      L12Z = Z20-Z10
      L12 = SQRT(L12X*L12X+L12Y*L12Y+L12Z*L12Z)
      AAA = ONE/MAX(L12,EM30)
      L12X = L12X*AAA
      L12Y = L12Y*AAA
      L12Z = L12Z*AAA

      L14X = X40-X10
      L14Y = Y40-Y10
      L14Z = Z40-Z10
      L14 = SQRT(L14X*L14X+L14Y*L14Y+L14Z*L14Z)
      AAA = ONE/MAX(L14,EM30)
      L14X = L14X*AAA
      L14Y = L14Y*AAA
      L14Z = L14Z*AAA

      L1214 = L12X*L14X+L12Y*L14Y+L12Z*L14Z

      BBB = GAP_M*(ONE+EM5)/MAX(SQRT(ONE-L1214*L1214),EM30)
      AAA = MIN(L12/THREE,BBB)

      X1 = X1 + L12X*AAA
      Y1 = Y1 + L12Y*AAA
      Z1 = Z1 + L12Z*AAA

      AAA = MIN(L14/THREE,BBB)

      X1 = X1 + L14X*AAA
      Y1 = Y1 + L14Y*AAA
      Z1 = Z1 + L14Z*AAA

      RETURN
      END
