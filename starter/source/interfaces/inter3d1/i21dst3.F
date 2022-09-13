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
Chd|  I21DST3                       source/interfaces/inter3d1/i21dst3.F
Chd|-- called by -----------
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I21DST3(
     1                  JLT    ,CAND_N ,CAND_E ,IRECT  ,NSV    ,
     2                  GAP_S  ,X      ,IRTLM  ,CSTS   ,DEPTH  ,
     3                  NOD_NORMAL,XM0 ,PENE   ,PENI   ,IFPEN  ,
     4                  IGAP   ,GAP    ,GAPMAX ,GAPMIN ,DRAD   ,
     5                  DGAPLOAD) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,  CAND_N(*), CAND_E(*), IRECT(4,*),
     .        IRTLM(2,*), NSV(*), IFPEN(*)
      INTEGER IGAP
      my_real
     .     X(3,*), CSTS(2,*), DEPTH, NOD_NORMAL(3,*), 
     .     XM0(3,*), PENE(*), PENI(*), GAP_S(*), GAP, GAPMIN, GAPMAX,
     .     DRAD 
      my_real , INTENT(IN) :: DGAPLOAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J, IS, L, N, N4N
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), FAR(MVSIZ), I4N(MVSIZ),
     .        IRTLM_L(2,MVSIZ)
      my_real
     .     GAPV(MVSIZ), 
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     X0(MVSIZ), Y0(MVSIZ), Z0(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ)
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .    NNX0(MVSIZ), NNY0(MVSIZ), NNZ0(MVSIZ)
      my_real
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ), X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ), Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ), Z04(MVSIZ),
     .     XI1(MVSIZ),  XI2(MVSIZ),
     .     YI1(MVSIZ),  YI2(MVSIZ),
     .     ZI1(MVSIZ),  ZI2(MVSIZ)
      my_real
     .     S2,D1,D2,D3,D4,
     .     X12,X23,X34,X41,XI0,
     .     Y12,Y23,Y34,Y41,YI0,
     .     Z12,Z23,Z34,Z41,ZI0,
     .     XN1(MVSIZ),YN1(MVSIZ),ZN1(MVSIZ),
     .     XN2(MVSIZ),YN2(MVSIZ),ZN2(MVSIZ),
     .     XN3(MVSIZ),YN3(MVSIZ),ZN3(MVSIZ),
     .     XN4(MVSIZ),YN4(MVSIZ),ZN4(MVSIZ),
     .     SX1(MVSIZ),SX2(MVSIZ),
     .     SY1(MVSIZ),SY2(MVSIZ),
     .     SZ1(MVSIZ),SZ2(MVSIZ),
     .     XI0V(MVSIZ),  YI0V(MVSIZ),  ZI0V(MVSIZ),
     .     XH(MVSIZ),  YH(MVSIZ),  ZH(MVSIZ),
     .     NX(MVSIZ),  NY(MVSIZ),  NZ(MVSIZ),
     .     GAP2(MVSIZ), NN, 
     .     X0H(MVSIZ), Y0H(MVSIZ), Z0H(MVSIZ),
     .     X1H(MVSIZ), Y1H(MVSIZ), Z1H(MVSIZ),
     .     X2H(MVSIZ), Y2H(MVSIZ), Z2H(MVSIZ), HH(MVSIZ), LL,
     .     HXI0, HYI0, HZI0, HXI1, HYI1, HZI1, HXI2, HYI2, HZI2,
     .     HX01, HY01, HZ01, HX02, HY02, HZ02,
     .     HXN1, HYN1, HZN1, HSX1, HSY1, HSZ1, HSX2, HSY2, HSZ2,
     .     SIDE, CRIT, CRITO, LBO, LCO, LB, LC, LA, SL,
     .     LAMBDA, XP, YP, ZP, DIST, DEPTH2, DRAD2,GAPP
      my_real
     .     CMAJ(MVSIZ), CSTS_L(2,MVSIZ)
C--------------------------------------------------------
      IF(IGAP==0)THEN
        DO I=1,JLT
          GAPV(I)=GAP
        END DO
      ELSE
C ... on pourrait utiliser le fait Depth >= max(gapv) dans le starter ...
        DO I=1,JLT
          GAPV(I)=GAP_S(CAND_N(I))
          GAPV(I)=MIN(GAPV(I),GAPMAX)
          GAPV(I)=MAX(GAPMIN,GAPV(I))
        END DO
      END IF
      DEPTH2=DEPTH*DEPTH
      DRAD2 =DRAD*DRAD
C--------------------------------------------------------
C
       DO I=1,JLT
         NSVG(I)= NSV(CAND_N(I))
         L      = CAND_E(I)
C
         IX1(I)=IRECT(1,L)
         IX2(I)=IRECT(2,L)
         IX3(I)=IRECT(3,L)
         IX4(I)=IRECT(4,L)
       END DO
C
       DO I=1,JLT
         XI(I)=X(1,NSVG(I))
         YI(I)=X(2,NSVG(I))
         ZI(I)=X(3,NSVG(I))
         X1(I)=XM0(1,IX1(I))
         Y1(I)=XM0(2,IX1(I))
         Z1(I)=XM0(3,IX1(I))
         X2(I)=XM0(1,IX2(I))
         Y2(I)=XM0(2,IX2(I))
         Z2(I)=XM0(3,IX2(I))
         X3(I)=XM0(1,IX3(I))
         Y3(I)=XM0(2,IX3(I))
         Z3(I)=XM0(3,IX3(I))
         X4(I)=XM0(1,IX4(I))
         Y4(I)=XM0(2,IX4(I))
         Z4(I)=XM0(3,IX4(I))
C
         PENE(I)=ZERO
       END DO
C
       DO I=1,JLT
         IRTLM_L(1,I)=0
         IRTLM_L(2,I)=0
         CSTS_L(1,I) =-ONE
         CSTS_L(2,I) =-ONE
       END DO
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

       DO I=1,JLT

        NNX1(I)=NOD_NORMAL(1,IX1(I))  
        NNY1(I)=NOD_NORMAL(2,IX1(I))  
        NNZ1(I)=NOD_NORMAL(3,IX1(I))  

        NNX2(I)=NOD_NORMAL(1,IX2(I))  
        NNY2(I)=NOD_NORMAL(2,IX2(I))  
        NNZ2(I)=NOD_NORMAL(3,IX2(I))  

        NNX3(I)=NOD_NORMAL(1,IX3(I))  
        NNY3(I)=NOD_NORMAL(2,IX3(I))  
        NNZ3(I)=NOD_NORMAL(3,IX3(I))  

        NNX4(I)=NOD_NORMAL(1,IX4(I))  
        NNY4(I)=NOD_NORMAL(2,IX4(I))  
        NNZ4(I)=NOD_NORMAL(3,IX4(I))  

       ENDDO

       DO I=1,JLT

        IF(IX3(I)/=IX4(I))THEN
          NNX0(I)=FOURTH*(NNX1(I)+NNX2(I)+NNX3(I)+NNX4(I))
          NNY0(I)=FOURTH*(NNY1(I)+NNY2(I)+NNY3(I)+NNY4(I))
          NNZ0(I)=FOURTH*(NNZ1(I)+NNZ2(I)+NNZ3(I)+NNZ4(I))
        ELSE
          NNX0(I)=NNX3(I)
          NNY0(I)=NNY3(I)
          NNZ0(I)=NNZ3(I)
        ENDIF

       ENDDO
C
c---------------------------------------------------------
c      courbure cubique
c---------------------------------------------------------
c       IF(ICURV == 3)THEN
c         CALL I7CMAJ(JLT    ,CMAJ   ,IRECT  ,NOD_NORMAL,CAND_E,
c     2               X1     ,X2     ,X3     ,X4        ,
c     3               Y1     ,Y2     ,Y3     ,Y4        ,
c     4               Z1     ,Z2     ,Z3     ,Z4        ,
c     5               NNX1   ,NNX2   ,NNX3   ,NNX4      ,
c     6               NNY1   ,NNY2   ,NNY3   ,NNY4      ,
c     7               NNZ1   ,NNZ2   ,NNZ3   ,NNZ4      )
c       ENDIF
c---------------------------------------------------------
       DO I=1,JLT
c       CMAJ(I) = ZERO
C       GAP2=(GAPV(I)+CMAJ(I))*(GAPV(I)+CMAJ(I))
        GAPP = GAPV(I)+DGAPLOAD
        GAP2(I)=GAPP*GAPP
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
       ENDDO
C
C normales aux triangles (recalculees ici pour pas les stocker).
       DO I=1,JLT

        XN1(I) = Y01(I)*Z02(I) - Z01(I)*Y02(I)
        YN1(I) = Z01(I)*X02(I) - X01(I)*Z02(I)
        ZN1(I) = X01(I)*Y02(I) - Y01(I)*X02(I)

        XN2(I) = Y02(I)*Z03(I) - Z02(I)*Y03(I)
        YN2(I) = Z02(I)*X03(I) - X02(I)*Z03(I)
        ZN2(I) = X02(I)*Y03(I) - Y02(I)*X03(I)

        XN3(I) = Y03(I)*Z04(I) - Z03(I)*Y04(I)
        YN3(I) = Z03(I)*X04(I) - X03(I)*Z04(I)
        ZN3(I) = X03(I)*Y04(I) - Y03(I)*X04(I)

        XN4(I) = Y04(I)*Z01(I) - Z04(I)*Y01(I)
        YN4(I) = Z04(I)*X01(I) - X04(I)*Z01(I)
        ZN4(I) = X04(I)*Y01(I) - Y04(I)*X01(I)

       ENDDO
C
C------
C 1er triangle ...
       DO I=1,JLT
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
        SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
        SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
        SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
        SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
        SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
        SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
        S2 = ONE/
     .       MAX(EM30,(XN1(I)*XN1(I)+ YN1(I)*YN1(I)+ ZN1(I)*ZN1(I)))
        LB1(I) = -(XN1(I)*SX2(I) + YN1(I)*SY2(I) + ZN1(I)*SZ2(I))*S2
        LC1(I) =  (XN1(I)*SX1(I) + YN1(I)*SY1(I) + ZN1(I)*SZ1(I))*S2
C
       ENDDO
C
        DO I=1,JLT
C
          NX(I) = XI(I)-(X0(I) + LB1(I)*X01(I) + LC1(I)*X02(I))
          NY(I) = YI(I)-(Y0(I) + LB1(I)*Y01(I) + LC1(I)*Y02(I))
          NZ(I) = ZI(I)-(Z0(I) + LB1(I)*Z01(I) + LC1(I)*Z02(I))
          HH(I) = NX(I)*XN1(I) + NY(I)*YN1(I) +NZ(I)*ZN1(I)
C
        ENDDO
C
C elevation : triangle //
        DO I=1,JLT
C
          LL = HH(I)/
     .         MAX(EM30,NNX1(I)*XN1(I)+NNY1(I)*YN1(I)+NNZ1(I)*ZN1(I))
          X1H(I)=X1(I)+LL*NNX1(I)
          Y1H(I)=Y1(I)+LL*NNY1(I)
          Z1H(I)=Z1(I)+LL*NNZ1(I)
C
          LL = HH(I)/
     .         MAX(EM30,NNX2(I)*XN1(I)+NNY2(I)*YN1(I)+NNZ2(I)*ZN1(I))
          X2H(I)=X2(I)+LL*NNX2(I)
          Y2H(I)=Y2(I)+LL*NNY2(I)
          Z2H(I)=Z2(I)+LL*NNZ2(I)
C
          LL = HH(I)/
     .         MAX(EM30,NNX0(I)*XN1(I)+NNY0(I)*YN1(I)+NNZ0(I)*ZN1(I))
          X0H(I)=X0(I)+LL*NNX0(I)
          Y0H(I)=Y0(I)+LL*NNY0(I)
          Z0H(I)=Z0(I)+LL*NNZ0(I)
C
        ENDDO
C
C coordonnees locale dans le triangle //
        DO I=1,JLT
C
          FAR(I)=0
C
          HXI0 = X0H(I) - XI(I)
          HYI0 = Y0H(I) - YI(I)
          HZI0 = Z0H(I) - ZI(I)
C
          HXI1 = X1H(I) - XI(I)
          HYI1 = Y1H(I) - YI(I)
          HZI1 = Z1H(I) - ZI(I)
C
          HXI2 = X2H(I) - XI(I)
          HYI2 = Y2H(I) - YI(I)
          HZI2 = Z2H(I) - ZI(I)
C
          HX01 = X1H(I) - X0H(I)
          HY01 = Y1H(I) - Y0H(I)
          HZ01 = Z1H(I) - Z0H(I)
C
          HX02 = X2H(I) - X0H(I)
          HY02 = Y2H(I) - Y0H(I)
          HZ02 = Z2H(I) - Z0H(I)
C
          HXN1 = HY01*HZ02 - HZ01*HY02
          HYN1 = HZ01*HX02 - HX01*HZ02
          HZN1 = HX01*HY02 - HY01*HX02
C
          IF(HXN1*XN1(I)+HYN1*YN1(I)+HZN1*ZN1(I) <= ZERO) THEN
C a optimiser
             FAR(I)=1
             CYCLE
          END IF
C
          HSX1 = HYI0*HZI1 - HZI0*HYI1
          HSY1 = HZI0*HXI1 - HXI0*HZI1
          HSZ1 = HXI0*HYI1 - HYI0*HXI1
C
          HSX2 = HYI0*HZI2 - HZI0*HYI2
          HSY2 = HZI0*HXI2 - HXI0*HZI2
          HSZ2 = HXI0*HYI2 - HYI0*HXI2
C
          S2 = ONE/
     .         MAX(EM30,(HXN1*HXN1+ HYN1*HYN1+ HZN1*HZN1))
          LB1(I) = -(HXN1*HSX2 + HYN1*HSY2 + HZN1*HSZ2)*S2
          LC1(I) =  (HXN1*HSX1 + HYN1*HSY1 + HZN1*HSZ1)*S2
C
          IF(LB1(I) < -ZEP05 .OR. LC1(I) < -ZEP05 .OR.
     .        LB1(I)+LC1(I) > ONEP05) FAR(I)=1
C
        ENDDO
C
C  necessaire au calcul de distance // normale
        DO I=1,JLT
C
         IF(FAR(I)==1)CYCLE
C
C normale interpolee dans le triangle //
         NX(I)=(ONE-LB1(I)-LC1(I))*NNX0(I)+LB1(I)*NNX1(I)+LC1(I)*NNX2(I)
         NY(I)=(ONE-LB1(I)-LC1(I))*NNY0(I)+LB1(I)*NNY1(I)+LC1(I)*NNY2(I)
         NZ(I)=(ONE-LB1(I)-LC1(I))*NNZ0(I)+LB1(I)*NNZ1(I)+LC1(I)*NNZ2(I)
C
C projection sur triangle 1
         NN = NX(I)*XN1(I)+ NY(I)*YN1(I)+ NZ(I)*ZN1(I)
         IF(NN <= ZERO) THEN
C cas limite
             FAR(I)=1
             CYCLE
         END IF
         NN = ONE/MAX(EM30,NN)
C
         LAMBDA=(XN1(I)*XI0V(I)+YN1(I)*YI0V(I)+ZN1(I)*ZI0V(I))*NN
         XP=XI(I)+LAMBDA*NX(I)
         YP=YI(I)+LAMBDA*NY(I)
         ZP=ZI(I)+LAMBDA*NZ(I)
C
         XI0V(I) = X0(I) - XP
         YI0V(I) = Y0(I) - YP
         ZI0V(I) = Z0(I) - ZP
C
         XI1(I) = X1(I) - XP
         YI1(I) = Y1(I) - YP
         ZI1(I) = Z1(I) - ZP
C
         XI2(I) = X2(I) - XP
         YI2(I) = Y2(I) - YP
         ZI2(I) = Z2(I) - ZP
C
         SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
         SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
         SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
         SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
         SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
         SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
         NN = ONE/
     .        MAX(EM30,(XN1(I)*XN1(I)+ YN1(I)*YN1(I)+ ZN1(I)*ZN1(I)))
         LB1(I) = -(XN1(I)*SX2(I) + YN1(I)*SY2(I) + ZN1(I)*SZ2(I))*NN
         LC1(I) =  (XN1(I)*SX1(I) + YN1(I)*SY1(I) + ZN1(I)*SZ1(I))*NN
C
        ENDDO
C
       DO I=1,JLT
C
         IF(FAR(I)==1)CYCLE
C
         LB = MIN(ONE,MAX(LB1(I),ZERO))
         LC = MIN(ONE,MAX(LC1(I),ZERO))
         LA = MIN(ONE,MAX(ONE-LB1(I)-LC1(I),ZERO))

         SL=ONE/MAX(EM20,LA+LB+LC)
         LB = LB*SL
         LC = LC*SL
         LA = LA*SL

         NX1(I) = XI(I)-(LA*X0(I) + LB*X1(I) + LC*X2(I))
         NY1(I) = YI(I)-(LA*Y0(I) + LB*Y1(I) + LC*Y2(I))
         NZ1(I) = ZI(I)-(LA*Z0(I) + LB*Z1(I) + LC*Z2(I))
         P1(I) = NX1(I)*NX1(I) + NY1(I)*NY1(I) +NZ1(I)*NZ1(I)
C
C        NX(I)=(1-LB1(I)-LC1(I))*NNX0(I)+LB1(I)*NNX1(I)+LC1(I)*NNX2(I)
C        NY(I)=(1-LB1(I)-LC1(I))*NNY0(I)+LB1(I)*NNY1(I)+LC1(I)*NNY2(I)
C        NZ(I)=(1-LB1(I)-LC1(I))*NNZ0(I)+LB1(I)*NNZ1(I)+LC1(I)*NNZ2(I)
C        SIDE=NX1(I)*NX(I)+NY1(I)*NY(I)+NZ1(I)*NZ(I)

         SIDE=SIGN(ONE,NX1(I)*XN1(I)+NY1(I)*YN1(I)+NZ1(I)*ZN1(I))
         IF((SIDE >= ZERO .AND. P1(I) < MAX(GAP2(I),DRAD2)) .OR.
     .      (SIDE <  ZERO .AND. P1(I) < DEPTH2))THEN
           IF(LB1(I) < -ZEP05 .OR. LC1(I) < -ZEP05 .OR.
     .        LB1(I)+LC1(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB1(I))
     .            + ABS(THIRD-LC1(I))
     .            + ABS(TWO_THIRD-LB1(I)-LC1(I))
           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB1(I)-LC1(I)
             NX(I)=LB1(I)*NNX1(I)+LC1(I)*NNX2(I)+LA*NNX0(I)
             NY(I)=LB1(I)*NNY1(I)+LC1(I)*NNY2(I)+LA*NNY0(I)
             NZ(I)=LB1(I)*NNZ1(I)+LC1(I)*NNZ2(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX1(I) = XI(I)-(X0(I) + LB1(I)*X01(I) + LC1(I)*X02(I))
             NY1(I) = YI(I)-(Y0(I) + LB1(I)*Y01(I) + LC1(I)*Y02(I))
             NZ1(I) = ZI(I)-(Z0(I) + LB1(I)*Z01(I) + LC1(I)*Z02(I))

             DIST =NX1(I)*NX(I)+NY1(I)*NY(I)+NZ1(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = CAND_E(I)
             IRTLM_L(2,I) = NINT(SIDE) 
             CSTS_L(1,I)  = LB1(I)
             CSTS_L(2,I)  = LC1(I)
           END IF
         ELSEIF(SIDE >= ZERO .AND. P1(I) < DRAD2) THEN
           IF(LB1(I) < -ZEP05 .OR. LC1(I) < -ZEP05 .OR.
     .        LB1(I)+LC1(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB1(I))
     .            + ABS(THIRD-LC1(I))
     .            + ABS(TWO_THIRD-LB1(I)-LC1(I))
           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB1(I)-LC1(I)
             NX(I)=LB1(I)*NNX1(I)+LC1(I)*NNX2(I)+LA*NNX0(I)
             NY(I)=LB1(I)*NNY1(I)+LC1(I)*NNY2(I)+LA*NNY0(I)
             NZ(I)=LB1(I)*NNZ1(I)+LC1(I)*NNZ2(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX1(I) = XI(I)-(X0(I) + LB1(I)*X01(I) + LC1(I)*X02(I))
             NY1(I) = YI(I)-(Y0(I) + LB1(I)*Y01(I) + LC1(I)*Y02(I))
             NZ1(I) = ZI(I)-(Z0(I) + LB1(I)*Z01(I) + LC1(I)*Z02(I))

             DIST =NX1(I)*NX(I)+NY1(I)*NY(I)+NZ1(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = -CAND_E(I)
             IRTLM_L(2,I) = NINT(SIDE) 
             CSTS_L(1,I)  = LB1(I)
             CSTS_L(2,I)  = LC1(I)
           END IF
         END IF
C
       ENDDO
C------
       N4N=0
       DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
          N4N = N4N+1
          I4N(N4N)=I
        ELSE
        ENDIF
       ENDDO
C------
C 2eme triangle ...
       DO N=1,N4N
C
        I=I4N(N)
C
        XI0V(I) = X0(I) - XI(I)
        YI0V(I) = Y0(I) - YI(I)
        ZI0V(I) = Z0(I) - ZI(I)
C
        XI1(I) = X2(I) - XI(I)
        YI1(I) = Y2(I) - YI(I)
        ZI1(I) = Z2(I) - ZI(I)
C
        XI2(I) = X3(I) - XI(I)
        YI2(I) = Y3(I) - YI(I)
        ZI2(I) = Z3(I) - ZI(I)
C
        SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
        SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
        SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
        SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
        SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
        SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
        S2 = ONE/
     .        MAX(EM30,(XN2(I)*XN2(I)+ YN2(I)*YN2(I)+ ZN2(I)*ZN2(I)))
        LB2(I) = -(XN2(I)*SX2(I) + YN2(I)*SY2(I) + ZN2(I)*SZ2(I))*S2
        LC2(I) =  (XN2(I)*SX1(I) + YN2(I)*SY1(I) + ZN2(I)*SZ1(I))*S2
C
       ENDDO
C
       DO N=1,N4N
C
         I=I4N(N)
C
         NX(I) = XI(I)-(X0(I) + LB2(I)*X02(I) + LC2(I)*X03(I))
         NY(I) = YI(I)-(Y0(I) + LB2(I)*Y02(I) + LC2(I)*Y03(I))
         NZ(I) = ZI(I)-(Z0(I) + LB2(I)*Z02(I) + LC2(I)*Z03(I))
         HH(I) = NX(I)*XN2(I) + NY(I)*YN2(I) +NZ(I)*ZN2(I)
C
       ENDDO
C
C elevation : triangle //
       DO N=1,N4N
C
         I=I4N(N)
C
         LL = HH(I)/
     .        MAX(EM30,NNX2(I)*XN2(I)+NNY2(I)*YN2(I)+NNZ2(I)*ZN2(I))
         X1H(I)=X2(I)+LL*NNX2(I)
         Y1H(I)=Y2(I)+LL*NNY2(I)
         Z1H(I)=Z2(I)+LL*NNZ2(I)
C
         LL = HH(I)/
     .        MAX(EM30,NNX3(I)*XN2(I)+NNY3(I)*YN2(I)+NNZ3(I)*ZN2(I))
         X2H(I)=X3(I)+LL*NNX3(I)
         Y2H(I)=Y3(I)+LL*NNY3(I)
         Z2H(I)=Z3(I)+LL*NNZ3(I)
C
         LL = HH(I)/
     .        MAX(EM30,NNX0(I)*XN2(I)+NNY0(I)*YN2(I)+NNZ0(I)*ZN2(I))
         X0H(I)=X0(I)+LL*NNX0(I)
         Y0H(I)=Y0(I)+LL*NNY0(I)
         Z0H(I)=Z0(I)+LL*NNZ0(I)
C
       ENDDO
C
C coordonnees locale dans le triangle //
       DO N=1,N4N
C
         I=I4N(N)
C
         FAR(I)=0
C
         HXI0 = X0H(I) - XI(I)
         HYI0 = Y0H(I) - YI(I)
         HZI0 = Z0H(I) - ZI(I)
C
         HXI1 = X1H(I) - XI(I)
         HYI1 = Y1H(I) - YI(I)
         HZI1 = Z1H(I) - ZI(I)
C
         HXI2 = X2H(I) - XI(I)
         HYI2 = Y2H(I) - YI(I)
         HZI2 = Z2H(I) - ZI(I)
C
         HX01 = X1H(I) - X0H(I)
         HY01 = Y1H(I) - Y0H(I)
         HZ01 = Z1H(I) - Z0H(I)
C
         HX02 = X2H(I) - X0H(I)
         HY02 = Y2H(I) - Y0H(I)
         HZ02 = Z2H(I) - Z0H(I)
C
         HXN1 = HY01*HZ02 - HZ01*HY02
         HYN1 = HZ01*HX02 - HX01*HZ02
         HZN1 = HX01*HY02 - HY01*HX02
C
         IF(HXN1*XN2(I)+HYN1*YN2(I)+HZN1*ZN2(I) <= ZERO) THEN
C a optimiser
            FAR(I)=1
            CYCLE
         END IF
C
         HSX1 = HYI0*HZI1 - HZI0*HYI1
         HSY1 = HZI0*HXI1 - HXI0*HZI1
         HSZ1 = HXI0*HYI1 - HYI0*HXI1
C
         HSX2 = HYI0*HZI2 - HZI0*HYI2
         HSY2 = HZI0*HXI2 - HXI0*HZI2
         HSZ2 = HXI0*HYI2 - HYI0*HXI2
C
         S2 = ONE/
     .        MAX(EM30,(HXN1*HXN1+ HYN1*HYN1+ HZN1*HZN1))
         LB2(I) = -(HXN1*HSX2 + HYN1*HSY2 + HZN1*HSZ2)*S2
         LC2(I) =  (HXN1*HSX1 + HYN1*HSY1 + HZN1*HSZ1)*S2
C
          IF(LB2(I) < -ZEP05 .OR. LC2(I) < -ZEP05 .OR.
     .        LB2(I)+LC2(I) > ONEP05) FAR(I)=1
       ENDDO
C
C  necessaire au calcul de distance // normale
       DO N=1,N4N
C
         I=I4N(N)
C
         IF(FAR(I)==1)CYCLE
C
C normale interpolee dans le triangle //
         NX(I)=(1-LB2(I)-LC2(I))*NNX0(I)+LB2(I)*NNX2(I)+LC2(I)*NNX3(I)
         NY(I)=(1-LB2(I)-LC2(I))*NNY0(I)+LB2(I)*NNY2(I)+LC2(I)*NNY3(I)
         NZ(I)=(1-LB2(I)-LC2(I))*NNZ0(I)+LB2(I)*NNZ2(I)+LC2(I)*NNZ3(I)
C
C projection sur triangle 2
         NN = NX(I)*XN2(I)+ NY(I)*YN2(I)+ NZ(I)*ZN2(I)
         IF(NN <= ZERO) THEN
C cas limite
             FAR(I)=1
             CYCLE
         END IF
         NN = ONE/MAX(EM30,NN)
C
         LAMBDA=(XN2(I)*XI0V(I)+YN2(I)*YI0V(I)+ZN2(I)*ZI0V(I))*NN
         XP=XI(I)+LAMBDA*NX(I)
         YP=YI(I)+LAMBDA*NY(I)
         ZP=ZI(I)+LAMBDA*NZ(I)
C
         XI0V(I) = X0(I) - XP
         YI0V(I) = Y0(I) - YP
         ZI0V(I) = Z0(I) - ZP
C
         XI1(I) = X2(I) - XP
         YI1(I) = Y2(I) - YP
         ZI1(I) = Z2(I) - ZP
C
         XI2(I) = X3(I) - XP
         YI2(I) = Y3(I) - YP
         ZI2(I) = Z3(I) - ZP
C
         SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
         SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
         SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
         SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
         SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
         SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
         NN = ONE/
     .        MAX(EM30,(XN2(I)*XN2(I)+ YN2(I)*YN2(I)+ ZN2(I)*ZN2(I)))
         LB2(I) = -(XN2(I)*SX2(I) + YN2(I)*SY2(I) + ZN2(I)*SZ2(I))*NN
         LC2(I) =  (XN2(I)*SX1(I) + YN2(I)*SY1(I) + ZN2(I)*SZ1(I))*NN
C
       ENDDO
C
       DO N=1,N4N
C
         I=I4N(N)
C
         IF(FAR(I)==1)CYCLE
C
         LB = MIN(ONE,MAX(LB2(I),ZERO))
         LC = MIN(ONE,MAX(LC2(I),ZERO))
         LA = MIN(ONE,MAX(ONE-LB2(I)-LC2(I),ZERO))

         SL=ONE/MAX(EM20,LA+LB+LC)
         LB = LB*SL
         LC = LC*SL
         LA = LA*SL

         NX2(I) = XI(I)-(LA*X0(I) + LB*X2(I) + LC*X3(I))
         NY2(I) = YI(I)-(LA*Y0(I) + LB*Y2(I) + LC*Y3(I))
         NZ2(I) = ZI(I)-(LA*Z0(I) + LB*Z2(I) + LC*Z3(I))
         P2(I) = NX2(I)*NX2(I) + NY2(I)*NY2(I) +NZ2(I)*NZ2(I)
C
C        NX(I)=(1-LB2(I)-LC2(I))*NNX0(I)+LB2(I)*NNX2(I)+LC2(I)*NNX3(I)
C        NY(I)=(1-LB2(I)-LC2(I))*NNY0(I)+LB2(I)*NNY2(I)+LC2(I)*NNY3(I)
C        NZ(I)=(1-LB2(I)-LC2(I))*NNZ0(I)+LB2(I)*NNZ2(I)+LC2(I)*NNZ3(I)
C        SIDE=NX2(I)*NX(I)+NY2(I)*NY(I)+NZ2(I)*NZ(I)

         SIDE=SIGN(ONE,NX2(I)*XN2(I)+NY2(I)*YN2(I)+NZ2(I)*ZN2(I))
         IF((SIDE >= ZERO .AND. P2(I) < MAX(GAP2(I),DRAD2)) .OR.
     .      (SIDE <  ZERO .AND. P2(I) < DEPTH2))THEN
           IF(LB2(I) < -ZEP05 .OR. LC2(I) < -ZEP05 .OR.
     .        LB2(I)+LC2(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB2(I))
     .            + ABS(THIRD-LC2(I))
     .            + ABS(TWO_THIRD-LB2(I)-LC2(I))
           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB2(I)-LC2(I)
             NX(I)=LB2(I)*NNX2(I)+LC2(I)*NNX3(I)+LA*NNX0(I)
             NY(I)=LB2(I)*NNY2(I)+LC2(I)*NNY3(I)+LA*NNY0(I)
             NZ(I)=LB2(I)*NNZ2(I)+LC2(I)*NNZ3(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX2(I) = XI(I)-(X0(I) + LB2(I)*X02(I) + LC2(I)*X03(I))
             NY2(I) = YI(I)-(Y0(I) + LB2(I)*Y02(I) + LC2(I)*Y03(I))
             NZ2(I) = ZI(I)-(Z0(I) + LB2(I)*Z02(I) + LC2(I)*Z03(I))

             DIST =NX2(I)*NX(I)+NY2(I)*NY(I)+NZ2(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = CAND_E(I)
             IRTLM_L(2,I) = NINT(TWO*SIDE) 
             CSTS_L(1,I)  = LB2(I)
             CSTS_L(2,I)  = LC2(I)
           END IF
         ELSEIF(SIDE >= ZERO .AND. P2(I) < DRAD2) THEN
           IF(LB2(I) < -ZEP05 .OR. LC2(I) < -ZEP05 .OR.
     .        LB2(I)+LC2(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB2(I))
     .            + ABS(THIRD-LC2(I))
     .            + ABS(TWO_THIRD-LB2(I)-LC2(I))
           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB2(I)-LC2(I)
             NX(I)=LB2(I)*NNX2(I)+LC2(I)*NNX3(I)+LA*NNX0(I)
             NY(I)=LB2(I)*NNY2(I)+LC2(I)*NNY3(I)+LA*NNY0(I)
             NZ(I)=LB2(I)*NNZ2(I)+LC2(I)*NNZ3(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX2(I) = XI(I)-(X0(I) + LB2(I)*X02(I) + LC2(I)*X03(I))
             NY2(I) = YI(I)-(Y0(I) + LB2(I)*Y02(I) + LC2(I)*Y03(I))
             NZ2(I) = ZI(I)-(Z0(I) + LB2(I)*Z02(I) + LC2(I)*Z03(I))

             DIST =NX2(I)*NX(I)+NY2(I)*NY(I)+NZ2(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = -CAND_E(I)
             IRTLM_L(2,I) = NINT(TWO*SIDE) 
             CSTS_L(1,I)  = LB2(I)
             CSTS_L(2,I)  = LC2(I)
           END IF
         END IF
C
       ENDDO
C------
C 3eme triangle ...
       DO N=1,N4N
C
         I=I4N(N)
C
         XI0V(I) = X0(I) - XI(I)
         YI0V(I) = Y0(I) - YI(I)
         ZI0V(I) = Z0(I) - ZI(I)
C
         XI1(I) = X3(I) - XI(I)
         YI1(I) = Y3(I) - YI(I)
         ZI1(I) = Z3(I) - ZI(I)
C
         XI2(I) = X4(I) - XI(I)
         YI2(I) = Y4(I) - YI(I)
         ZI2(I) = Z4(I) - ZI(I)
C
         SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
         SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
         SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
         SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
         SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
         SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
         S2 = ONE/
     .        MAX(EM30,(XN3(I)*XN3(I)+ YN3(I)*YN3(I)+ ZN3(I)*ZN3(I)))
         LB3(I) = -(XN3(I)*SX2(I) + YN3(I)*SY2(I) + ZN3(I)*SZ2(I))*S2
         LC3(I) =  (XN3(I)*SX1(I) + YN3(I)*SY1(I) + ZN3(I)*SZ1(I))*S2
C
       ENDDO
C
       DO N=1,N4N
C
         I=I4N(N)
C
         NX(I) = XI(I)-(X0(I) + LB3(I)*X03(I) + LC3(I)*X04(I))
         NY(I) = YI(I)-(Y0(I) + LB3(I)*Y03(I) + LC3(I)*Y04(I))
         NZ(I) = ZI(I)-(Z0(I) + LB3(I)*Z03(I) + LC3(I)*Z04(I))
         HH(I) = NX(I)*XN3(I) + NY(I)*YN3(I) +NZ(I)*ZN3(I)
C
       ENDDO
C
C elevation : triangle //
       DO N=1,N4N
C
         I=I4N(N)
C
         LL = HH(I)/
     .        MAX(EM30,NNX3(I)*XN3(I)+NNY3(I)*YN3(I)+NNZ3(I)*ZN3(I))
         X1H(I)=X3(I)+LL*NNX3(I)
         Y1H(I)=Y3(I)+LL*NNY3(I)
         Z1H(I)=Z3(I)+LL*NNZ3(I)
C
         LL = HH(I)/
     .        MAX(EM30,NNX4(I)*XN3(I)+NNY4(I)*YN3(I)+NNZ4(I)*ZN3(I))
         X2H(I)=X4(I)+LL*NNX4(I)
         Y2H(I)=Y4(I)+LL*NNY4(I)
         Z2H(I)=Z4(I)+LL*NNZ4(I)
C
         LL = HH(I)/
     .        MAX(EM30,NNX0(I)*XN3(I)+NNY0(I)*YN3(I)+NNZ0(I)*ZN3(I))
         X0H(I)=X0(I)+LL*NNX0(I)
         Y0H(I)=Y0(I)+LL*NNY0(I)
         Z0H(I)=Z0(I)+LL*NNZ0(I)
C
       ENDDO
C
C coordonnees locale dans le triangle //
       DO N=1,N4N
C
         I=I4N(N)
C
         FAR(I)=0
C
         HXI0 = X0H(I) - XI(I)
         HYI0 = Y0H(I) - YI(I)
         HZI0 = Z0H(I) - ZI(I)
C
         HXI1 = X1H(I) - XI(I)
         HYI1 = Y1H(I) - YI(I)
         HZI1 = Z1H(I) - ZI(I)
C
         HXI2 = X2H(I) - XI(I)
         HYI2 = Y2H(I) - YI(I)
         HZI2 = Z2H(I) - ZI(I)
C
         HX01 = X1H(I) - X0H(I)
         HY01 = Y1H(I) - Y0H(I)
         HZ01 = Z1H(I) - Z0H(I)
C
         HX02 = X2H(I) - X0H(I)
         HY02 = Y2H(I) - Y0H(I)
         HZ02 = Z2H(I) - Z0H(I)
C
         HXN1 = HY01*HZ02 - HZ01*HY02
         HYN1 = HZ01*HX02 - HX01*HZ02
         HZN1 = HX01*HY02 - HY01*HX02
C
         IF(HXN1*XN3(I)+HYN1*YN3(I)+HZN1*ZN3(I) <= ZERO) THEN
C a optimiser
            FAR(I)=1
            CYCLE
         END IF
C
         HSX1 = HYI0*HZI1 - HZI0*HYI1
         HSY1 = HZI0*HXI1 - HXI0*HZI1
         HSZ1 = HXI0*HYI1 - HYI0*HXI1
C
         HSX2 = HYI0*HZI2 - HZI0*HYI2
         HSY2 = HZI0*HXI2 - HXI0*HZI2
         HSZ2 = HXI0*HYI2 - HYI0*HXI2
C
         S2 = ONE/
     .        MAX(EM30,(HXN1*HXN1+ HYN1*HYN1+ HZN1*HZN1))
         LB3(I) = -(HXN1*HSX2 + HYN1*HSY2 + HZN1*HSZ2)*S2
         LC3(I) =  (HXN1*HSX1 + HYN1*HSY1 + HZN1*HSZ1)*S2
C
         IF(LB3(I) < -ZEP05 .OR. LC3(I) < -ZEP05 .OR.
     .       LB3(I)+LC3(I) > ONEP05) FAR(I)=1
       ENDDO
C
C  necessaire au calcul de distance // normale
       DO N=1,N4N
C
         I=I4N(N)
C
         IF(FAR(I)==1)CYCLE
C
C normale interpolee dans le triangle //
         NX(I)=(1-LB3(I)-LC3(I))*NNX0(I)+LB3(I)*NNX3(I)+LC3(I)*NNX4(I)
         NY(I)=(1-LB3(I)-LC3(I))*NNY0(I)+LB3(I)*NNY3(I)+LC3(I)*NNY4(I)
         NZ(I)=(1-LB3(I)-LC3(I))*NNZ0(I)+LB3(I)*NNZ3(I)+LC3(I)*NNZ4(I)
C
C projection sur triangle 3
         NN = NX(I)*XN3(I)+ NY(I)*YN3(I)+ NZ(I)*ZN3(I)
         IF(NN <= ZERO) THEN
C cas limite
             FAR(I)=1
             CYCLE
         END IF
         NN = ONE/MAX(EM30,NN)
C
         LAMBDA=(XN3(I)*XI0V(I)+YN3(I)*YI0V(I)+ZN3(I)*ZI0V(I))*NN
         XP=XI(I)+LAMBDA*NX(I)
         YP=YI(I)+LAMBDA*NY(I)
         ZP=ZI(I)+LAMBDA*NZ(I)
C
         XI0V(I) = X0(I) - XP
         YI0V(I) = Y0(I) - YP
         ZI0V(I) = Z0(I) - ZP
C
         XI1(I) = X3(I) - XP
         YI1(I) = Y3(I) - YP
         ZI1(I) = Z3(I) - ZP
C
         XI2(I) = X4(I) - XP
         YI2(I) = Y4(I) - YP
         ZI2(I) = Z4(I) - ZP
C
         SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
         SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
         SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
         SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
         SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
         SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
         NN = ONE/
     .        MAX(EM30,(XN3(I)*XN3(I)+ YN3(I)*YN3(I)+ ZN3(I)*ZN3(I)))
         LB3(I) = -(XN3(I)*SX2(I) + YN3(I)*SY2(I) + ZN3(I)*SZ2(I))*NN
         LC3(I) =  (XN3(I)*SX1(I) + YN3(I)*SY1(I) + ZN3(I)*SZ1(I))*NN
C
       ENDDO
C
       DO N=1,N4N
C
         I=I4N(N)
C
         IF(FAR(I)==1)CYCLE
C
         LB = MIN(ONE,MAX(LB3(I),ZERO))
         LC = MIN(ONE,MAX(LC3(I),ZERO))
         LA = MIN(ONE,MAX(ONE-LB3(I)-LC3(I),ZERO))

         SL=ONE/MAX(EM20,LA+LB+LC)
         LB = LB*SL
         LC = LC*SL
         LA = LA*SL

         NX3(I) = XI(I)-(LA*X0(I) + LB*X3(I) + LC*X4(I))
         NY3(I) = YI(I)-(LA*Y0(I) + LB*Y3(I) + LC*Y4(I))
         NZ3(I) = ZI(I)-(LA*Z0(I) + LB*Z3(I) + LC*Z4(I))
         P3(I) = NX3(I)*NX3(I) + NY3(I)*NY3(I) +NZ3(I)*NZ3(I)
C
C        NX(I)=(1-LB3(I)-LC3(I))*NNX0(I)+LB3(I)*NNX3(I)+LC3(I)*NNX4(I)
C        NY(I)=(1-LB3(I)-LC3(I))*NNY0(I)+LB3(I)*NNY3(I)+LC3(I)*NNY4(I)
C        NZ(I)=(1-LB3(I)-LC3(I))*NNZ0(I)+LB3(I)*NNZ3(I)+LC3(I)*NNZ4(I)
C        SIDE=NX3(I)*NX(I)+NY3(I)*NY(I)+NZ3(I)*NZ(I)

         SIDE=SIGN(ONE,NX3(I)*XN3(I)+NY3(I)*YN3(I)+NZ3(I)*ZN3(I))
         IF((SIDE >= ZERO .AND. P3(I) < MAX(GAP2(I),DRAD2)) .OR.
     .      (SIDE <  ZERO .AND. P3(I) < DEPTH2))THEN
           IF(LB3(I) < -ZEP05 .OR. LC3(I) < -ZEP05 .OR.
     .        LB3(I)+LC3(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB3(I))
     .            + ABS(THIRD-LC3(I))
     .            + ABS(TWO_THIRD-LB3(I)-LC3(I))
           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB3(I)-LC3(I)
             NX(I)=LB3(I)*NNX3(I)+LC3(I)*NNX4(I)+LA*NNX0(I)
             NY(I)=LB3(I)*NNY3(I)+LC3(I)*NNY4(I)+LA*NNY0(I)
             NZ(I)=LB3(I)*NNZ3(I)+LC3(I)*NNZ4(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX3(I) = XI(I)-(X0(I) + LB3(I)*X03(I) + LC3(I)*X04(I))
             NY3(I) = YI(I)-(Y0(I) + LB3(I)*Y03(I) + LC3(I)*Y04(I))
             NZ3(I) = ZI(I)-(Z0(I) + LB3(I)*Z03(I) + LC3(I)*Z04(I))

             DIST =NX3(I)*NX(I)+NY3(I)*NY(I)+NZ3(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = CAND_E(I)
             IRTLM_L(2,I) = NINT(THREE*SIDE) 
             CSTS_L(1,I)  = LB3(I)
             CSTS_L(2,I)  = LC3(I)
           END IF
         ELSEIF(SIDE >= ZERO .AND. P3(I) < DRAD2) THEN
           IF(LB3(I) < -ZEP05 .OR. LC3(I) < -ZEP05 .OR.
     .        LB3(I)+LC3(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB3(I))
     .            + ABS(THIRD-LC3(I))
     .            + ABS(TWO_THIRD-LB3(I)-LC3(I))
           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB3(I)-LC3(I)
             NX(I)=LB3(I)*NNX3(I)+LC3(I)*NNX4(I)+LA*NNX0(I)
             NY(I)=LB3(I)*NNY3(I)+LC3(I)*NNY4(I)+LA*NNY0(I)
             NZ(I)=LB3(I)*NNZ3(I)+LC3(I)*NNZ4(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX3(I) = XI(I)-(X0(I) + LB3(I)*X03(I) + LC3(I)*X04(I))
             NY3(I) = YI(I)-(Y0(I) + LB3(I)*Y03(I) + LC3(I)*Y04(I))
             NZ3(I) = ZI(I)-(Z0(I) + LB3(I)*Z03(I) + LC3(I)*Z04(I))

             DIST =NX3(I)*NX(I)+NY3(I)*NY(I)+NZ3(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = -CAND_E(I)
             IRTLM_L(2,I) = NINT(THREE*SIDE) 
             CSTS_L(1,I)  = LB3(I)
             CSTS_L(2,I)  = LC3(I)
           END IF
         END IF
C
       END DO
C------
C 4eme triangle ...
       DO N=1,N4N
C
         I=I4N(N)
C
         XI0V(I) = X0(I) - XI(I)
         YI0V(I) = Y0(I) - YI(I)
         ZI0V(I) = Z0(I) - ZI(I)
C
         XI1(I) = X4(I) - XI(I)
         YI1(I) = Y4(I) - YI(I)
         ZI1(I) = Z4(I) - ZI(I)
C
         XI2(I) = X1(I) - XI(I)
         YI2(I) = Y1(I) - YI(I)
         ZI2(I) = Z1(I) - ZI(I)
C
         SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
         SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
         SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
         SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
         SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
         SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
         S2 = ONE/
     .        MAX(EM30,(XN4(I)*XN4(I)+ YN4(I)*YN4(I)+ ZN4(I)*ZN4(I)))
         LB4(I) = -(XN4(I)*SX2(I) + YN4(I)*SY2(I) + ZN4(I)*SZ2(I))*S2
         LC4(I) =  (XN4(I)*SX1(I) + YN4(I)*SY1(I) + ZN4(I)*SZ1(I))*S2
C
       ENDDO
C
       DO N=1,N4N
C
         I=I4N(N)
C
         NX(I) = XI(I)-(X0(I) + LB4(I)*X04(I) + LC4(I)*X01(I))
         NY(I) = YI(I)-(Y0(I) + LB4(I)*Y04(I) + LC4(I)*Y01(I))
         NZ(I) = ZI(I)-(Z0(I) + LB4(I)*Z04(I) + LC4(I)*Z01(I))
         HH(I) = NX(I)*XN4(I) + NY(I)*YN4(I) +NZ(I)*ZN4(I)
C
       ENDDO
C
C elevation : triangle //
       DO N=1,N4N
C
         I=I4N(N)
C
         LL = HH(I)/
     .        MAX(EM30,NNX4(I)*XN4(I)+NNY4(I)*YN4(I)+NNZ4(I)*ZN4(I))
         X1H(I)=X4(I)+LL*NNX4(I)
         Y1H(I)=Y4(I)+LL*NNY4(I)
         Z1H(I)=Z4(I)+LL*NNZ4(I)
C
         LL = HH(I)/
     .        MAX(EM30,NNX1(I)*XN4(I)+NNY1(I)*YN4(I)+NNZ1(I)*ZN4(I))
         X2H(I)=X1(I)+LL*NNX1(I)
         Y2H(I)=Y1(I)+LL*NNY1(I)
         Z2H(I)=Z1(I)+LL*NNZ1(I)
C
         LL = HH(I)/
     .        MAX(EM30,NNX0(I)*XN4(I)+NNY0(I)*YN4(I)+NNZ0(I)*ZN4(I))
         X0H(I)=X0(I)+LL*NNX0(I)
         Y0H(I)=Y0(I)+LL*NNY0(I)
         Z0H(I)=Z0(I)+LL*NNZ0(I)
C
       ENDDO
C
C coordonnees locale dans le triangle //
       DO N=1,N4N
C
         I=I4N(N)
C
         FAR(I)=0
C
         HXI0 = X0H(I) - XI(I)
         HYI0 = Y0H(I) - YI(I)
         HZI0 = Z0H(I) - ZI(I)
C
         HXI1 = X1H(I) - XI(I)
         HYI1 = Y1H(I) - YI(I)
         HZI1 = Z1H(I) - ZI(I)
C
         HXI2 = X2H(I) - XI(I)
         HYI2 = Y2H(I) - YI(I)
         HZI2 = Z2H(I) - ZI(I)
C
         HX01 = X1H(I) - X0H(I)
         HY01 = Y1H(I) - Y0H(I)
         HZ01 = Z1H(I) - Z0H(I)
C
         HX02 = X2H(I) - X0H(I)
         HY02 = Y2H(I) - Y0H(I)
         HZ02 = Z2H(I) - Z0H(I)
C
         HXN1 = HY01*HZ02 - HZ01*HY02
         HYN1 = HZ01*HX02 - HX01*HZ02
         HZN1 = HX01*HY02 - HY01*HX02
C
         IF(HXN1*XN4(I)+HYN1*YN4(I)+HZN1*ZN4(I) <= ZERO) THEN
C a optimiser
            FAR(I)=1
            CYCLE
         END IF
C
         HSX1 = HYI0*HZI1 - HZI0*HYI1
         HSY1 = HZI0*HXI1 - HXI0*HZI1
         HSZ1 = HXI0*HYI1 - HYI0*HXI1
C
         HSX2 = HYI0*HZI2 - HZI0*HYI2
         HSY2 = HZI0*HXI2 - HXI0*HZI2
         HSZ2 = HXI0*HYI2 - HYI0*HXI2
C
         S2 = ONE/
     .        MAX(EM30,(HXN1*HXN1+ HYN1*HYN1+ HZN1*HZN1))
         LB4(I) = -(HXN1*HSX2 + HYN1*HSY2 + HZN1*HSZ2)*S2
         LC4(I) =  (HXN1*HSX1 + HYN1*HSY1 + HZN1*HSZ1)*S2
C
         IF(LB4(I) < -ZEP05 .OR. LC4(I) < -ZEP05 .OR.
     .       LB4(I)+LC4(I) > ONEP05) FAR(I)=1
       ENDDO
C
C  necessaire au calcul de distance // normale
       DO N=1,N4N
C
         I=I4N(N)
C
         IF(FAR(I)==1)CYCLE
C
C normale interpolee dans le triangle //
         NX(I)=(1-LB4(I)-LC4(I))*NNX0(I)+LB4(I)*NNX4(I)+LC4(I)*NNX1(I)
         NY(I)=(1-LB4(I)-LC4(I))*NNY0(I)+LB4(I)*NNY4(I)+LC4(I)*NNY1(I)
         NZ(I)=(1-LB4(I)-LC4(I))*NNZ0(I)+LB4(I)*NNZ4(I)+LC4(I)*NNZ1(I)
C
C projection sur triangle 4
         NN = NX(I)*XN4(I)+ NY(I)*YN4(I)+ NZ(I)*ZN4(I)
         IF(NN <= ZERO) THEN
C cas limite
             FAR(I)=1
             CYCLE
         END IF
         NN = ONE/MAX(EM30,NN)
C
         LAMBDA=(XN4(I)*XI0V(I)+YN4(I)*YI0V(I)+ZN4(I)*ZI0V(I))*NN
         XP=XI(I)+LAMBDA*NX(I)
         YP=YI(I)+LAMBDA*NY(I)
         ZP=ZI(I)+LAMBDA*NZ(I)
C
         XI0V(I) = X0(I) - XP
         YI0V(I) = Y0(I) - YP
         ZI0V(I) = Z0(I) - ZP
C
         XI1(I) = X4(I) - XP
         YI1(I) = Y4(I) - YP
         ZI1(I) = Z4(I) - ZP
C
         XI2(I) = X1(I) - XP
         YI2(I) = Y1(I) - YP
         ZI2(I) = Z1(I) - ZP
C
         SX1(I) = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
         SY1(I) = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
         SZ1(I) = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
C
         SX2(I) = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
         SY2(I) = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
         SZ2(I) = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
C
         NN = ONE/
     .        MAX(EM30,(XN4(I)*XN4(I)+ YN4(I)*YN4(I)+ ZN4(I)*ZN4(I)))
         LB4(I) = -(XN4(I)*SX2(I) + YN4(I)*SY2(I) + ZN4(I)*SZ2(I))*NN
         LC4(I) =  (XN4(I)*SX1(I) + YN4(I)*SY1(I) + ZN4(I)*SZ1(I))*NN
C
       ENDDO


       DO N=1,N4N
C
         I=I4N(N)
C
         IF(FAR(I)==1)CYCLE
C
         LB = MIN(ONE,MAX(LB4(I),ZERO))
         LC = MIN(ONE,MAX(LC4(I),ZERO))
         LA = MIN(ONE,MAX(ONE-LB4(I)-LC4(I),ZERO))

         SL=ONE/MAX(EM20,LA+LB+LC)
         LB = LB*SL
         LC = LC*SL
         LA = LA*SL

         NX4(I) = XI(I)-(LA*X0(I) + LB*X4(I) + LC*X1(I))
         NY4(I) = YI(I)-(LA*Y0(I) + LB*Y4(I) + LC*Y1(I))
         NZ4(I) = ZI(I)-(LA*Z0(I) + LB*Z4(I) + LC*Z1(I))
         P4(I) = NX4(I)*NX4(I) + NY4(I)*NY4(I) +NZ4(I)*NZ4(I)

         SIDE=SIGN(ONE,NX4(I)*XN4(I)+NY4(I)*YN4(I)+NZ4(I)*ZN4(I))
         IF((SIDE >= ZERO .AND. P4(I) < GAP2(I)) .OR.
     .      (SIDE <  ZERO .AND. P4(I) < DEPTH2))THEN
           IF(LB4(I) < -ZEP05 .OR. LC4(I) < -ZEP05 .OR.
     .        LB4(I)+LC4(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB4(I))
     .            + ABS(THIRD-LC4(I))
     .            + ABS(TWO_THIRD-LB4(I)-LC4(I))

           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB4(I)-LC4(I)
             NX(I)=LB4(I)*NNX4(I)+LC4(I)*NNX1(I)+LA*NNX0(I)
             NY(I)=LB4(I)*NNY4(I)+LC4(I)*NNY1(I)+LA*NNY0(I)
             NZ(I)=LB4(I)*NNZ4(I)+LC4(I)*NNZ1(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX4(I) = XI(I)-(X0(I) + LB4(I)*X04(I) + LC4(I)*X01(I))
             NY4(I) = YI(I)-(Y0(I) + LB4(I)*Y04(I) + LC4(I)*Y01(I))
             NZ4(I) = ZI(I)-(Z0(I) + LB4(I)*Z04(I) + LC4(I)*Z01(I))

             DIST =NX4(I)*NX(I)+NY4(I)*NY(I)+NZ4(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = CAND_E(I)
             IRTLM_L(2,I) = NINT(FOUR*SIDE) 
             CSTS_L(1,I)  = LB4(I)
             CSTS_L(2,I)  = LC4(I)
           END IF
         ELSEIF(SIDE >= ZERO .AND. P4(I) < DRAD2) THEN
           IF(LB4(I) < -ZEP05 .OR. LC4(I) < -ZEP05 .OR.
     .        LB4(I)+LC4(I) > ONEP05) CYCLE
           CRIT  =  ABS(THIRD-LB4(I))
     .            + ABS(THIRD-LC4(I))
     .            + ABS(TWO_THIRD-LB4(I)-LC4(I))

           LBO   = CSTS_L(1,I)
           LCO   = CSTS_L(2,I)
           CRITO =  ABS(THIRD-LBO)
     .            + ABS(THIRD-LCO)
     .            + ABS(TWO_THIRD-LBO-LCO)
           IF(CRIT < CRITO)THEN

             LA   =ONE-LB4(I)-LC4(I)
             NX(I)=LB4(I)*NNX4(I)+LC4(I)*NNX1(I)+LA*NNX0(I)
             NY(I)=LB4(I)*NNY4(I)+LC4(I)*NNY1(I)+LA*NNY0(I)
             NZ(I)=LB4(I)*NNZ4(I)+LC4(I)*NNZ1(I)+LA*NNZ0(I)
             NN=ONE/MAX(EM20,SQRT(NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)))
             NX(I)=NX(I)*NN
             NY(I)=NY(I)*NN
             NZ(I)=NZ(I)*NN

             NX4(I) = XI(I)-(X0(I) + LB4(I)*X04(I) + LC4(I)*X01(I))
             NY4(I) = YI(I)-(Y0(I) + LB4(I)*Y04(I) + LC4(I)*Y01(I))
             NZ4(I) = ZI(I)-(Z0(I) + LB4(I)*Z04(I) + LC4(I)*Z01(I))

             DIST =NX4(I)*NX(I)+NY4(I)*NY(I)+NZ4(I)*NZ(I)
             PENE(I)=GAPV(I)-DIST

             IRTLM_L(1,I) = -CAND_E(I)
             IRTLM_L(2,I) = NINT(FOUR*SIDE) 
             CSTS_L(1,I)  = LB4(I)
             CSTS_L(2,I)  = LC4(I)
           END IF
         END IF
C
       ENDDO
C---------------------
C
C---------------------
       DO I=1,JLT
          IF(IRTLM_L(1,I)/=0)THEN
             LB=CSTS_L(1,I)
             LC=CSTS_L(2,I)
             CRIT  =  ABS(THIRD-LB)
     .              + ABS(THIRD-LC)
     .              + ABS(TWO_THIRD-LB-LC)
             LBO   = CSTS(1,CAND_N(I))
             LCO   = CSTS(2,CAND_N(I))
             CRITO =  ABS(THIRD-LBO)
     .              + ABS(THIRD-LCO)
     .              + ABS(TWO_THIRD-LBO-LCO)

             IF(CRIT < CRITO)THEN

               PENI(CAND_N(I))    = MAX(PENE(I),ZERO)
               IF(IRTLM(1,CAND_N(I)) > 0) IFPEN(CAND_N(I)) = 1

               IRTLM(1,CAND_N(I)) = IRTLM_L(1,I)

               IRTLM(2,CAND_N(I)) = IRTLM_L(2,I)
               CSTS(1,CAND_N(I))  = LB
               CSTS(2,CAND_N(I))  = LC
             END IF
         END IF
C
       ENDDO

C---------------------
      RETURN
      END
