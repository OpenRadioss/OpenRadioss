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
Chd|  I15TOT1                       source/interfaces/int15/i15tot1.F
Chd|-- called by -----------
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I15TOT1(NOINT ,NDEB  ,NSC  ,X     ,KSURF ,
     2                  IGRSURF,BUFSF ,KSC  ,KSI   ,NOLD  ,
     3                  XP1   ,XP2    ,XP3  ,XP4   ,GX    ,
     4                  XTK   ,YTK    ,ZTK  ,NTX   ,NTY   ,
     5                  NTZ   ,PENET ,DEPTH ,XI    ,YI    ,
     6                  ZI    ,NXI   ,NYI   ,NZI   ,ANSMX ,
     7                  HOLD  ,IACTIV,ITAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KSURF ,KSI(4,*) ,
     .        NOINT ,NDEB, NSC, IACTIV(4,*), KSC(*), ITAB(*)
C     REAL
      my_real 
     .  X(3,*)    ,BUFSF(*)  ,
     .  NOLD(3,*) ,XTK(4,*)  ,YTK(4,*) ,ZTK(4,*) ,
     .  NTX(4,*)  ,NTY(4,*)  ,NTZ(4,*) ,
     .  PENET(4,*),DEPTH(4,*),
     .  XI(4,*)   ,YI(4,*)  ,ZI(4,*)  ,
     .  NXI(4,*)  ,NYI(4,*) ,NZI(4,*) ,
     .  XP1(3,MVSIZ), XP2(3,MVSIZ), XP3(3,MVSIZ), XP4(3,MVSIZ),
     .  GX(3,MVSIZ), ANSMX, HOLD(3,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IL, NLS, IDG,
     .        IN1, IN2, IN3, IN4,
     .        INSIDE1,INSIDE2
      my_real
     .   A, B, C, AN, BN, CN, ROT(9), DGR, EXPN,
     .   XG, YG, ZG,
     .   NTN,
     .   N1, N2, N3, N, N11, N12, N13, N21, N22, N23, NR1, NR2,
     .   XKN1, YKN1, ZKN1, SGNXKN, SGNYKN, SGNZKN, XKN, YKN, ZKN, EH,
     .   LAMBDA1, LAMBDA2, ALP, BET,
     .   XH , YH , ZH , MU, XH1, YH1, ZH1, MU1, XH2, YH2, ZH2, MU2,
     .   DX1, DY1, DZ1, DX2, DY2, DZ2, DX3, DY3, DZ3,
     .   SIDE1, SIDE2, OLDNX, OLDNY, OLDNZ, OUT, PS, NR,
     .   LAMBDA, EM, EP
      my_real
     .   X1(MVSIZ),  Y1(MVSIZ),  Z1(MVSIZ),
     .   X2(MVSIZ),  Y2(MVSIZ),  Z2(MVSIZ),
     .   X3(MVSIZ),  Y3(MVSIZ),  Z3(MVSIZ),
     .   X12(MVSIZ), Y12(MVSIZ), Z12(MVSIZ),
     .   X13(MVSIZ), Y13(MVSIZ), Z13(MVSIZ),
     .   X23(MVSIZ), Y23(MVSIZ), Z23(MVSIZ),
     .   XM(MVSIZ) , YM(MVSIZ) , ZM(MVSIZ) , 
     .   XTK2(MVSIZ), YTK2(MVSIZ) , ZTK2(MVSIZ) , 
     .   D(MVSIZ)
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
      A =BUFSF(ADRBUF+1)
      B =BUFSF(ADRBUF+2)
      C =BUFSF(ADRBUF+3)
      DGR =BUFSF(ADRBUF+36)
      IDG =NINT(DGR)
      DGR =IDG
      DGR =MAX(TWO,DGR)
      EXPN=DGR/(DGR-1.)
      AN=A**DGR
      BN=B**DGR
      CN=C**DGR
      AN=ONE/MAX(EM20,AN)
      BN=ONE/MAX(EM20,BN)
      CN=ONE/MAX(EM20,CN)
      DO I=1,9
       ROT(I)=BUFSF(ADRBUF+7+I-1)
      END DO
C-------------------------------
C     Passage au repere local :
C-------------------------------
      DO 75 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
      IN4=KSI(4,IL)
C     Passage au repere local.
      XG=X(1,IN1)-BUFSF(ADRBUF+4)
      YG=X(2,IN1)-BUFSF(ADRBUF+5)
      ZG=X(3,IN1)-BUFSF(ADRBUF+6)
      XP1(1,I)=ROT(1)*XG+ROT(2)*YG+ROT(3)*ZG
      XP1(2,I)=ROT(4)*XG+ROT(5)*YG+ROT(6)*ZG
      XP1(3,I)=ROT(7)*XG+ROT(8)*YG+ROT(9)*ZG
      XG=X(1,IN2)-BUFSF(ADRBUF+4)
      YG=X(2,IN2)-BUFSF(ADRBUF+5)
      ZG=X(3,IN2)-BUFSF(ADRBUF+6)
      XP2(1,I)=ROT(1)*XG+ROT(2)*YG+ROT(3)*ZG
      XP2(2,I)=ROT(4)*XG+ROT(5)*YG+ROT(6)*ZG
      XP2(3,I)=ROT(7)*XG+ROT(8)*YG+ROT(9)*ZG
      XG=X(1,IN3)-BUFSF(ADRBUF+4)
      YG=X(2,IN3)-BUFSF(ADRBUF+5)
      ZG=X(3,IN3)-BUFSF(ADRBUF+6)
      XP3(1,I)=ROT(1)*XG+ROT(2)*YG+ROT(3)*ZG
      XP3(2,I)=ROT(4)*XG+ROT(5)*YG+ROT(6)*ZG
      XP3(3,I)=ROT(7)*XG+ROT(8)*YG+ROT(9)*ZG
      XG=X(1,IN4)-BUFSF(ADRBUF+4)
      YG=X(2,IN4)-BUFSF(ADRBUF+5)
      ZG=X(3,IN4)-BUFSF(ADRBUF+6)
      XP4(1,I)=ROT(1)*XG+ROT(2)*YG+ROT(3)*ZG
      XP4(2,I)=ROT(4)*XG+ROT(5)*YG+ROT(6)*ZG
      XP4(3,I)=ROT(7)*XG+ROT(8)*YG+ROT(9)*ZG
      GX(1,I)=FOURTH*(XP1(1,I)+XP2(1,I)+XP3(1,I)+XP4(1,I))
      GX(2,I)=FOURTH*(XP1(2,I)+XP2(2,I)+XP3(2,I)+XP4(2,I))
      GX(3,I)=FOURTH*(XP1(3,I)+XP2(3,I)+XP3(3,I)+XP4(3,I))     
 75   CONTINUE
C-------------------------------
C     1ER TRIANGLE.
C-------------------------------
      DO 100 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      X1(I)=GX(1,I)
      Y1(I)=GX(2,I)
      Z1(I)=GX(3,I)
      X2(I)=XP1(1,I)
      Y2(I)=XP1(2,I)
      Z2(I)=XP1(3,I)
      X3(I)=XP2(1,I)
      Y3(I)=XP2(2,I)
      Z3(I)=XP2(3,I)
      X12(I)=X2(I)-X1(I)
      Y12(I)=Y2(I)-Y1(I)
      Z12(I)=Z2(I)-Z1(I)
      X13(I)=X3(I)-X1(I)
      Y13(I)=Y3(I)-Y1(I)
      Z13(I)=Z3(I)-Z1(I)
      N1=Y12(I)*Z13(I)-Z12(I)*Y13(I)
      N2=Z12(I)*X13(I)-X12(I)*Z13(I)
      N3=X12(I)*Y13(I)-Y12(I)*X13(I)
      NTN=ONE/MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      NTX(1,I)=NTN*N1
      NTY(1,I)=NTN*N2
      NTZ(1,I)=NTN*N3
      D(I)    =-NTX(1,I)*X1(I)-NTY(1,I)*Y1(I)-NTZ(1,I)*Z1(I)
C-----
      X23(I)=X3(I)-X2(I)
      Y23(I)=Y3(I)-Y2(I)
      Z23(I)=Z3(I)-Z2(I)
 100  CONTINUE
C-------------------------------
C     POINTS K,H SUR E,L / LA DISTANCE D(K,H) EST LOCALEMENT MAXIMUM
C-------------------------------
      DO 125 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      EH  =(ABS(NTX(1,I)/(DGR*AN))**EXPN)*AN
     .    +(ABS(NTY(1,I)/(DGR*BN))**EXPN)*BN
     .    +(ABS(NTZ(1,I)/(DGR*CN))**EXPN)*CN
C     X EST DU SIGNE DE LAMBDA*NTX, IDEM EN Y ET Z.
C     LAMBDA1=EXP(LOG(1./EH)/EXPN)
      LAMBDA1=(MAX(EM20,EH))**(-ONE/EXPN)
      XH1=ABS(LAMBDA1*NTX(1,I)/(DGR*AN))**(ONE/(DGR-ONE))
      IF (NTX(1,I)<ZERO) XH1=-XH1
      YH1=ABS(LAMBDA1*NTY(1,I)/(DGR*BN))**(ONE/(DGR-ONE))
      IF (NTY(1,I)<ZERO) YH1=-YH1
      ZH1=ABS(LAMBDA1*NTZ(1,I)/(DGR*CN))**(ONE/(DGR-ONE))
      IF (NTZ(1,I)<ZERO) ZH1=-ZH1
      MU1   =-NTX(1,I)*XH1-NTY(1,I)*YH1-NTZ(1,I)*ZH1-D(I)
C     LAMBDA2=-LAMBDA1
      XH2 =-XH1
      YH2 =-YH1
      ZH2 =-ZH1
C     MU2   =-NTX(1,I)*XH2-NTY(1,I)*YH2-NTZ(1,I)*ZH2-D(I)
      MU2=-MU1-TWO*D(I)
      XTK(1,I) =XH1+MU1*NTX(1,I)
      YTK(1,I) =YH1+MU1*NTY(1,I)
      ZTK(1,I) =ZH1+MU1*NTZ(1,I)
      XTK2(I)  =XH2+MU2*NTX(1,I)
      YTK2(I)  =YH2+MU2*NTY(1,I)
      ZTK2(I)  =ZH2+MU2*NTZ(1,I)
 125  CONTINUE
C-------------------------------
C     RAMENER LE PT PK SUR LE TRIANGLE.
C-------------------------------
      DO 150 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      DX1=XTK(1,I)-X1(I)
      DY1=YTK(1,I)-Y1(I)
      DZ1=ZTK(1,I)-Z1(I)
      DX2=XTK(1,I)-X2(I)
      DY2=YTK(1,I)-Y2(I)
      DZ2=ZTK(1,I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(1,I)
     .     +(DZ1*DX2-DZ2*DX1)*NTY(1,I)
     .     +(DX1*DY2-DY1*DX2)*NTZ(1,I)
      IF (OUT<0.) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK(1,I)=ALP*X1(I)+BET*X2(I)
        YTK(1,I)=ALP*Y1(I)+BET*Y2(I)
        ZTK(1,I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK(1,I)-X3(I)
      DY3=YTK(1,I)-Y3(I)
      DZ3=ZTK(1,I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(1,I)
     .     +(DZ2*DX3-DZ3*DX2)*NTY(1,I)
     .     +(DX2*DY3-DY2*DX3)*NTZ(1,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK(1,I)=ALP*X2(I)+BET*X3(I)
        YTK(1,I)=ALP*Y2(I)+BET*Y3(I)
        ZTK(1,I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(1,I)
     .     +(DZ3*DX1-DZ1*DX3)*NTY(1,I)
     .     +(DX3*DY1-DY3*DX1)*NTZ(1,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK(1,I)=ALP*X3(I)+BET*X1(I)
        YTK(1,I)=ALP*Y3(I)+BET*Y1(I)
        ZTK(1,I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-----
      DX1=XTK2(I)-X1(I)
      DY1=YTK2(I)-Y1(I)
      DZ1=ZTK2(I)-Z1(I)
      DX2=XTK2(I)-X2(I)
      DY2=YTK2(I)-Y2(I)
      DZ2=ZTK2(I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(1,I)
     .      +(DZ1*DX2-DZ2*DX1)*NTY(1,I)
     .      +(DX1*DY2-DY1*DX2)*NTZ(1,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK2(I)=ALP*X1(I)+BET*X2(I)
        YTK2(I)=ALP*Y1(I)+BET*Y2(I)
        ZTK2(I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK2(I)-X3(I)
      DY3=YTK2(I)-Y3(I)
      DZ3=ZTK2(I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(1,I)
     .      +(DZ2*DX3-DZ3*DX2)*NTY(1,I)
     .      +(DX2*DY3-DY2*DX3)*NTZ(1,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK2(I)=ALP*X2(I)+BET*X3(I)
        YTK2(I)=ALP*Y2(I)+BET*Y3(I)
        ZTK2(I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(1,I)
     .      +(DZ3*DX1-DZ1*DX3)*NTY(1,I)
     .      +(DX3*DY1-DY3*DX1)*NTZ(1,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK2(I)=ALP*X3(I)+BET*X1(I)
        YTK2(I)=ALP*Y3(I)+BET*Y1(I)
        ZTK2(I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-------------------------------
 150  CONTINUE
C-------------------------------
C     PROJECTION DE PK SUR L ET PENETRATION.
C-------------------------------
      DO 175 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      IF (IACTIV(1,IL)==-1) GOTO 175
C-----
      XH =XTK(1,I)
      YH =YTK(1,I)
      ZH =ZTK(1,I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE
      YKN1 =YH**(IDG-1)
      SGNYKN=-ONE
      IF (YKN1*YH>=ZERO) SGNYKN=ONE
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE
      N11 =SGNXKN*XKN1*AN
      N12 =SGNYKN*YKN1*BN
      N13 =SGNZKN*ZKN1*CN
      NR1 =N11*N11+N12*N12+N13*N13
      NR1 =SQRT(NR1)
      EM =N11*XTK(1,I)+N12*YTK(1,I)+N13*ZTK(1,I)
      IF (EM<=ONE) THEN
        LAMBDA1=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR1)
        INSIDE1=1
      ELSE
        INSIDE1=0
      ENDIF
C-----
      XH =XTK2(I)
      YH =YTK2(I)
      ZH =ZTK2(I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE
      YKN1 =YH**(IDG-1)
      SGNYKN=-ONE
      IF (YKN1*YH>=ZERO) SGNYKN=ONE
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE
      N21 =SGNXKN*XKN1*AN
      N22 =SGNYKN*YKN1*BN
      N23 =SGNZKN*ZKN1*CN
      NR2 =N21*N21+N22*N22+N23*N23
      NR2 =SQRT(NR2)
      EM =N21*XTK2(I)+N22*YTK2(I)+N23*ZTK2(I)
      IF (EM<=ONE) THEN
        LAMBDA2=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR2)
        INSIDE2=1
      ELSE
        INSIDE2=0
      ENDIF
C-----
      IF (INSIDE1==0.AND.INSIDE2==0) THEN
        IACTIV(1,IL)=0
      ELSE
C-----
      IF (IACTIV(1,IL)==0) THEN
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
          XM(I)=XTK(1,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(1,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(1,I)-LAMBDA1*N13/MAX(EM20,NR1)
         ELSE
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(1,I)=XTK2(I)
          YTK(1,I)=YTK2(I)
          ZTK(1,I)=ZTK2(I)
         ENDIF
        ELSEIF(INSIDE1/=0) THEN
          XM(I)=XTK(1,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(1,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(1,I)-LAMBDA1*N13/MAX(EM20,NR1)
        ELSEIF(INSIDE2/=0) THEN
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(1,I)=XTK2(I)
          YTK(1,I)=YTK2(I)
          ZTK(1,I)=ZTK2(I)
        ENDIF
C-----
      ELSE
        XH=HOLD(1,4*(IL-1)+1)
        YH=HOLD(2,4*(IL-1)+1)
        ZH=HOLD(3,4*(IL-1)+1)
        N1=NOLD(1,4*(IL-1)+1)
        N2=NOLD(2,4*(IL-1)+1)
        N3=NOLD(3,4*(IL-1)+1)
        LAMBDA1=(XH-XTK(1,I))*N1
     .         +(YH-YTK(1,I))*N2
     .         +(ZH-ZTK(1,I))*N3
        LAMBDA2=(XH-XTK2(I))*N1
     .         +(YH-YTK2(I))*N2
     .         +(ZH-ZTK2(I))*N3
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
          IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
           XM(I)=XTK(1,I)+LAMBDA1*N1
           YM(I)=YTK(1,I)+LAMBDA1*N2
           ZM(I)=ZTK(1,I)+LAMBDA1*N3
          ELSE
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(1,I)=XTK2(I)
           YTK(1,I)=YTK2(I)
           ZTK(1,I)=ZTK2(I)
          ENDIF
        ELSEIF(INSIDE1/=0) THEN
           XM(I)=XTK(1,I)+LAMBDA1*N1
           YM(I)=YTK(1,I)+LAMBDA1*N2
           ZM(I)=ZTK(1,I)+LAMBDA1*N3
        ELSEIF(INSIDE2/=0) THEN
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(1,I)=XTK2(I)
           YTK(1,I)=YTK2(I)
           ZTK(1,I)=ZTK2(I)
        ENDIF
C       one more iteration.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))        
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        NR =N1*N1+N2*N2+N3*N3
        NR =ONE/MAX(EM20,SQRT(NR))
        N1 =N1*NR
        N2 =N2*NR
        N3 =N3*NR
        LAMBDA1=(XM(I)-XTK(1,I))*N1
     .         +(YM(I)-YTK(1,I))*N2
     .         +(ZM(I)-ZTK(1,I))*N3
        XM(I)=XTK(1,I)+LAMBDA1*N1
        YM(I)=YTK(1,I)+LAMBDA1*N2
        ZM(I)=ZTK(1,I)+LAMBDA1*N3
      ENDIF
      IACTIV(1,IL)=IACTIV(1,IL)+1
      ENDIF
 175  CONTINUE
C-----
#include "vectorize.inc"
      DO 200 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(1,IL)<=0) GOTO 200
C-----
C      projection radiale de M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
C------
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))        
C      normale a l'ellipsoide en M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       NR =N1*N1+N2*N2+N3*N3
       NR =ONE/MAX(EM20,SQRT(NR))
       NXI(1,I)=N1*NR
       NYI(1,I)=N2*NR
       NZI(1,I)=N3*NR
       XI(1,I)=XM(I)
       YI(1,I)=YM(I)
       ZI(1,I)=ZM(I)
       DEPTH(1,I)=XM(I)*NXI(1,I)+YM(I)*NYI(1,I)+ZM(I)*NZI(1,I)
C------
       PENET(1,I)=(XTK(1,I)-XM(I))*NXI(1,I)
     .           +(YTK(1,I)-YM(I))*NYI(1,I)
     .           +(ZTK(1,I)-ZM(I))*NZI(1,I)
       PENET(1,I)=-PENET(1,I)
       PENET(1,I)=MAX(ZERO,PENET(1,I))
       IF (DEPTH(1,I)-PENET(1,I)<EM10*DEPTH(1,I)) THEN
         PENET(1,I)=ZERO
         IACTIV(1,IL)=-2
       ENDIF
       ANSMX=MAX(ANSMX,PENET(1,I))
 200  CONTINUE
C-------------------------------
C     MESSAGES DESACTIVATION
C-------------------------------
      DO 210 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(1,IL)==-2) THEN
         IN1=ITAB(KSI(1,IL))
         IN2=ITAB(KSI(2,IL))
         IN3=ITAB(KSI(3,IL))
         IN4=ITAB(KSI(4,IL))
#include "lockon.inc"
         WRITE(ISTDO,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(ISTDO,'(A,A,A,/,2I8,A,4I8,A)')' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN1,IN2,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
         WRITE(IOUT ,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(IOUT,'(A,A,A,/,2I8,A,4I8,A)') ' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN1,IN2,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
#include "lockoff.inc"
         IACTIV(1,IL)=-1
      ENDIF
 210  CONTINUE
C-------------------------------
C     2EME TRIANGLE.
C-------------------------------
      DO 225 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      X1(I)=GX(1,I)
      Y1(I)=GX(2,I)
      Z1(I)=GX(3,I)
      X2(I)=XP2(1,I)
      Y2(I)=XP2(2,I)
      Z2(I)=XP2(3,I)
      X3(I)=XP3(1,I)
      Y3(I)=XP3(2,I)
      Z3(I)=XP3(3,I)
      X12(I)=X2(I)-X1(I)
      Y12(I)=Y2(I)-Y1(I)
      Z12(I)=Z2(I)-Z1(I)
      X13(I)=X3(I)-X1(I)
      Y13(I)=Y3(I)-Y1(I)
      Z13(I)=Z3(I)-Z1(I)
      N1=Y12(I)*Z13(I)-Z12(I)*Y13(I)
      N2=Z12(I)*X13(I)-X12(I)*Z13(I)
      N3=X12(I)*Y13(I)-Y12(I)*X13(I)
      NTN=ONE/MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      NTX(2,I)=NTN*N1
      NTY(2,I)=NTN*N2
      NTZ(2,I)=NTN*N3
      D(I)    =-NTX(2,I)*X1(I)-NTY(2,I)*Y1(I)-NTZ(2,I)*Z1(I)
C-----
      X23(I)=X3(I)-X2(I)
      Y23(I)=Y3(I)-Y2(I)
      Z23(I)=Z3(I)-Z2(I)
 225  CONTINUE
C-------------------------------
C     POINTS K,H SUR E,L / LA DISTANCE D(K,H) EST LOCALEMENT MAXIMUM
C-------------------------------
      DO 250 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      EH  =(ABS(NTX(2,I)/(DGR*AN))**EXPN)*AN
     .    +(ABS(NTY(2,I)/(DGR*BN))**EXPN)*BN
     .    +(ABS(NTZ(2,I)/(DGR*CN))**EXPN)*CN
C     X EST DU SIGNE DE LAMBDA*NTX, IDEM EN Y ET Z.
C     LAMBDA1=EXP(LOG(1./EH)/EXPN)
      LAMBDA1=(MAX(EM20,EH))**(-ONE/EXPN)
      XH1   =ABS(LAMBDA1*NTX(2,I)/(DGR*AN))**(ONE/(DGR-ONE))
      IF (NTX(2,I)<ZERO) XH1=-XH1
      YH1   =ABS(LAMBDA1*NTY(2,I)/(DGR*BN))**(ONE/(DGR-ONE))
      IF (NTY(2,I)<ZERO) YH1=-YH1
      ZH1   =ABS(LAMBDA1*NTZ(2,I)/(DGR*CN))**(ONE/(DGR-ONE))
      IF (NTZ(2,I)<ZERO) ZH1=-ZH1
      MU1   =-NTX(2,I)*XH1-NTY(2,I)*YH1-NTZ(2,I)*ZH1-D(I)
C     LAMBDA2=-LAMBDA1
      XH2   =-XH1
      YH2   =-YH1
      ZH2   =-ZH1
C      MU2   =-NTX(2,I)*XH2-NTY(2,I)*YH2-NTZ(2,I)*ZH2-D(I)
      MU2=-MU1-TWO*D(I)
      XTK(2,I) =XH1+MU1*NTX(2,I)
      YTK(2,I) =YH1+MU1*NTY(2,I)
      ZTK(2,I) =ZH1+MU1*NTZ(2,I)
      XTK2(I)  =XH2+MU2*NTX(2,I)
      YTK2(I)  =YH2+MU2*NTY(2,I)
      ZTK2(I)  =ZH2+MU2*NTZ(2,I)
C-------------------------------
 250  CONTINUE
C-------------------------------
C     RAMENER LE PT PK SUR LE TRIANGLE.
C-------------------------------
      DO 275 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      DX1=XTK(2,I)-X1(I)
      DY1=YTK(2,I)-Y1(I)
      DZ1=ZTK(2,I)-Z1(I)
      DX2=XTK(2,I)-X2(I)
      DY2=YTK(2,I)-Y2(I)
      DZ2=ZTK(2,I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(2,I)
     .     +(DZ1*DX2-DZ2*DX1)*NTY(2,I)
     .     +(DX1*DY2-DY1*DX2)*NTZ(2,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(2,I)=ALP*X1(I)+BET*X2(I)
        YTK(2,I)=ALP*Y1(I)+BET*Y2(I)
        ZTK(2,I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK(2,I)-X3(I)
      DY3=YTK(2,I)-Y3(I)
      DZ3=ZTK(2,I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(2,I)
     .     +(DZ2*DX3-DZ3*DX2)*NTY(2,I)
     .     +(DX2*DY3-DY2*DX3)*NTZ(2,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK(2,I)=ALP*X2(I)+BET*X3(I)
        YTK(2,I)=ALP*Y2(I)+BET*Y3(I)
        ZTK(2,I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(2,I)
     .     +(DZ3*DX1-DZ1*DX3)*NTY(2,I)
     .     +(DX3*DY1-DY3*DX1)*NTZ(2,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(2,I)=ALP*X3(I)+BET*X1(I)
        YTK(2,I)=ALP*Y3(I)+BET*Y1(I)
        ZTK(2,I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-----
      DX1=XTK2(I)-X1(I)
      DY1=YTK2(I)-Y1(I)
      DZ1=ZTK2(I)-Z1(I)
      DX2=XTK2(I)-X2(I)
      DY2=YTK2(I)-Y2(I)
      DZ2=ZTK2(I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(2,I)
     .      +(DZ1*DX2-DZ2*DX1)*NTY(2,I)
     .      +(DX1*DY2-DY1*DX2)*NTZ(2,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK2(I)=ALP*X1(I)+BET*X2(I)
        YTK2(I)=ALP*Y1(I)+BET*Y2(I)
        ZTK2(I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK2(I)-X3(I)
      DY3=YTK2(I)-Y3(I)
      DZ3=ZTK2(I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(2,I)
     .      +(DZ2*DX3-DZ3*DX2)*NTY(2,I)
     .      +(DX2*DY3-DY2*DX3)*NTZ(2,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK2(I)=ALP*X2(I)+BET*X3(I)
        YTK2(I)=ALP*Y2(I)+BET*Y3(I)
        ZTK2(I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(2,I)
     .      +(DZ3*DX1-DZ1*DX3)*NTY(2,I)
     .      +(DX3*DY1-DY3*DX1)*NTZ(2,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK2(I)=ALP*X3(I)+BET*X1(I)
        YTK2(I)=ALP*Y3(I)+BET*Y1(I)
        ZTK2(I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-------------------------------
 275  CONTINUE
C-------------------------------
C     PROJECTION DE PK SUR L ET PENETRATION.
C-------------------------------
      DO 300 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      IF (IACTIV(2,IL)==-1) GOTO 300
C-----
      XH =XTK(2,I)
      YH =YTK(2,I)
      ZH =ZTK(2,I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE
      YKN1 =YH**(IDG-1)
      SGNYKN=-1.
      IF (YKN1*YH>=0.) SGNYKN=1.
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE
      N11 =SGNXKN*XKN1*AN
      N12 =SGNYKN*YKN1*BN
      N13 =SGNZKN*ZKN1*CN
      NR1 =N11*N11+N12*N12+N13*N13
      NR1 =SQRT(NR1)
      EM =N11*XTK(2,I)+N12*YTK(2,I)+N13*ZTK(2,I)
      IF (EM<=ONE) THEN
        LAMBDA1=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR1)
        INSIDE1=1
      ELSE
        INSIDE1=0
      ENDIF
C-----
      XH =XTK2(I)
      YH =YTK2(I)
      ZH =ZTK2(I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE
      YKN1 =YH**(IDG-1)
      SGNYKN=-ONE
      IF (YKN1*YH>=ZERO) SGNYKN=ONE     
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE
      N21 =SGNXKN*XKN1*AN
      N22 =SGNYKN*YKN1*BN
      N23 =SGNZKN*ZKN1*CN
      NR2  =N21*N21+N22*N22+N23*N23
      NR2 =SQRT(NR2)
      EM =N21*XTK2(I)+N22*YTK2(I)+N23*ZTK2(I)
      IF (EM<=ONE) THEN
        LAMBDA2=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR2)
        INSIDE2=1
      ELSE
        INSIDE2=0
      ENDIF
C-----
      IF (INSIDE1==0.AND.INSIDE2==0) THEN
        IACTIV(2,IL)=0
      ELSE
C-----
      IF (IACTIV(2,IL)==0) THEN
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
          XM(I)=XTK(2,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(2,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(2,I)-LAMBDA1*N13/MAX(EM20,NR1)
         ELSE
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(2,I)=XTK2(I)
          YTK(2,I)=YTK2(I)
          ZTK(2,I)=ZTK2(I)
         ENDIF
        ELSEIF(INSIDE1/=0) THEN
          XM(I)=XTK(2,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(2,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(2,I)-LAMBDA1*N13/MAX(EM20,NR1)
        ELSEIF(INSIDE2/=0) THEN
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(2,I)=XTK2(I)
          YTK(2,I)=YTK2(I)
          ZTK(2,I)=ZTK2(I)
        ENDIF
C-----
      ELSE
        XH=HOLD(1,4*(IL-1)+2)
        YH=HOLD(2,4*(IL-1)+2)
        ZH=HOLD(3,4*(IL-1)+2)
        N1=NOLD(1,4*(IL-1)+2)
        N2=NOLD(2,4*(IL-1)+2)
        N3=NOLD(3,4*(IL-1)+2)
        LAMBDA1=(XH-XTK(2,I))*N1
     .         +(YH-YTK(2,I))*N2
     .         +(ZH-ZTK(2,I))*N3
        LAMBDA2=(XH-XTK2(I))*N1
     .         +(YH-YTK2(I))*N2
     .         +(ZH-ZTK2(I))*N3
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
           XM(I)=XTK(2,I)+LAMBDA1*N1
           YM(I)=YTK(2,I)+LAMBDA1*N2
           ZM(I)=ZTK(2,I)+LAMBDA1*N3
          ELSE
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(2,I)=XTK2(I)
           YTK(2,I)=YTK2(I)
           ZTK(2,I)=ZTK2(I)
          ENDIF
        ELSEIF(INSIDE1/=0) THEN
           XM(I)=XTK(2,I)+LAMBDA1*N1
           YM(I)=YTK(2,I)+LAMBDA1*N2
           ZM(I)=ZTK(2,I)+LAMBDA1*N3
        ELSEIF(INSIDE2/=0) THEN
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(2,I)=XTK2(I)
           YTK(2,I)=YTK2(I)
           ZTK(2,I)=ZTK2(I)
        ENDIF
C       one more iteration.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))        
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        NR =N1*N1+N2*N2+N3*N3
        NR =ONE/MAX(EM20,SQRT(NR))
        N1 =N1*NR
        N2 =N2*NR
        N3 =N3*NR
        LAMBDA1=(XM(I)-XTK(2,I))*N1
     .         +(YM(I)-YTK(2,I))*N2
     .         +(ZM(I)-ZTK(2,I))*N3
        XM(I)=XTK(2,I)+LAMBDA1*N1
        YM(I)=YTK(2,I)+LAMBDA1*N2
        ZM(I)=ZTK(2,I)+LAMBDA1*N3
      ENDIF 
      IACTIV(2,IL)=IACTIV(2,IL)+1
      ENDIF
 300  CONTINUE
C-----
#include "vectorize.inc"
      DO 325 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(2,IL)<=0) GOTO 325
C-----
C      projection radiale de M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
C------
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))        
C------
C      normale a l'ellipsoide en M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
       ZKN1 =ZM(I)**(IDG-1)
       SGNZKN=-ONE
       IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       NR =N1*N1+N2*N2+N3*N3
       NR =ONE/MAX(EM20,SQRT(NR))
       NXI(2,I)=N1*NR
       NYI(2,I)=N2*NR
       NZI(2,I)=N3*NR
       XI(2,I)=XM(I)
       YI(2,I)=YM(I)
       ZI(2,I)=ZM(I)
       DEPTH(2,I)=XM(I)*NXI(2,I)+YM(I)*NYI(2,I)+ZM(I)*NZI(2,I)
C------
       PENET(2,I)=(XTK(2,I)-XM(I))*NXI(2,I)
     .          +(YTK(2,I)-YM(I))*NYI(2,I)
     .          +(ZTK(2,I)-ZM(I))*NZI(2,I)
       PENET(2,I)=-PENET(2,I)
       PENET(2,I)=MAX(ZERO,PENET(2,I))
       IF (DEPTH(2,I)-PENET(2,I)<EM10*DEPTH(2,I)) THEN
         PENET(2,I)=ZERO
         IACTIV(2,IL)=-2
       ENDIF
       ANSMX=MAX(ANSMX,PENET(2,I))
 325  CONTINUE
C-------------------------------
C     MESSAGES DESACTIVATION
C-------------------------------
      DO 335 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(2,IL)==-2) THEN
         IN1=ITAB(KSI(1,IL))
         IN2=ITAB(KSI(2,IL))
         IN3=ITAB(KSI(3,IL))
         IN4=ITAB(KSI(4,IL))
#include "lockon.inc"
         WRITE(ISTDO,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(ISTDO,'(A,A,A,/,2I8,A,4I8,A)')' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN2,IN3,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
         WRITE(IOUT ,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(IOUT,'(A,A,A,/,2I8,A,4I8,A)') ' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN2,IN3,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
#include "lockoff.inc"
         IACTIV(2,IL)=-1
      ENDIF
 335  CONTINUE
C-------------------------------
C     3EME TRIANGLE.
C-------------------------------
      DO 350 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      X1(I)=GX(1,I)
      Y1(I)=GX(2,I)
      Z1(I)=GX(3,I)
      X2(I)=XP3(1,I)
      Y2(I)=XP3(2,I)
      Z2(I)=XP3(3,I)
      X3(I)=XP4(1,I)
      Y3(I)=XP4(2,I)
      Z3(I)=XP4(3,I)
      X12(I)=X2(I)-X1(I)
      Y12(I)=Y2(I)-Y1(I)
      Z12(I)=Z2(I)-Z1(I)
      X13(I)=X3(I)-X1(I)
      Y13(I)=Y3(I)-Y1(I)
      Z13(I)=Z3(I)-Z1(I)
      N1=Y12(I)*Z13(I)-Z12(I)*Y13(I)
      N2=Z12(I)*X13(I)-X12(I)*Z13(I)
      N3=X12(I)*Y13(I)-Y12(I)*X13(I)
      NTN=ONE/MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      NTX(3,I)=NTN*N1
      NTY(3,I)=NTN*N2
      NTZ(3,I)=NTN*N3
      D(I)    =-NTX(3,I)*X1(I)-NTY(3,I)*Y1(I)-NTZ(3,I)*Z1(I)
C-----
      X23(I)=X3(I)-X2(I)
      Y23(I)=Y3(I)-Y2(I)
      Z23(I)=Z3(I)-Z2(I)
 350  CONTINUE
C-------------------------------
C     POINTS K,H SUR E,L / LA DISTANCE D(K,H) EST LOCALEMENT MAXIMUM
C-------------------------------
      DO 375 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      EH  =(ABS(NTX(3,I)/(DGR*AN))**EXPN)*AN
     .    +(ABS(NTY(3,I)/(DGR*BN))**EXPN)*BN
     .    +(ABS(NTZ(3,I)/(DGR*CN))**EXPN)*CN
C     X EST DU SIGNE DE LAMBDA*NTX, IDEM EN Y ET Z.
C     LAMBDA1=EXP(LOG(1./EH)/EXPN)
      LAMBDA1=(MAX(EM20,EH))**(-ONE/EXPN)
      XH1   =ABS(LAMBDA1*NTX(3,I)/(DGR*AN))**(ONE/(DGR-ONE))
      IF (NTX(3,I)<ZERO) XH1=-XH1
      YH1   =ABS(LAMBDA1*NTY(3,I)/(DGR*BN))**(ONE/(DGR-ONE))
      IF (NTY(3,I)<ZERO) YH1=-YH1
      ZH1   =ABS(LAMBDA1*NTZ(3,I)/(DGR*CN))**(ONE/(DGR-ONE))
      IF (NTZ(3,I)<ZERO) ZH1=-ZH1
      MU1   =-NTX(3,I)*XH1-NTY(3,I)*YH1-NTZ(3,I)*ZH1-D(I)
C     LAMBDA2=-LAMBDA1
      XH2   =-XH1
      YH2   =-YH1
      ZH2   =-ZH1
C      MU2   =-NTX(3,I)*XH2-NTY(3,I)*YH2-NTZ(3,I)*ZH2-D(I)
      MU2=-MU1-TWO*D(I)
      XTK(3,I) =XH1+MU1*NTX(3,I)
      YTK(3,I) =YH1+MU1*NTY(3,I)
      ZTK(3,I) =ZH1+MU1*NTZ(3,I)
      XTK2(I)=XH2+MU2*NTX(3,I)
      YTK2(I)=YH2+MU2*NTY(3,I)
      ZTK2(I)=ZH2+MU2*NTZ(3,I)
C-------------------------------
 375  CONTINUE
C-------------------------------
C     RAMENER LE PT PK SUR LE TRIANGLE.
C-------------------------------
      DO 400 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      DX1=XTK(3,I)-X1(I)
      DY1=YTK(3,I)-Y1(I)
      DZ1=ZTK(3,I)-Z1(I)
      DX2=XTK(3,I)-X2(I)
      DY2=YTK(3,I)-Y2(I)
      DZ2=ZTK(3,I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(3,I)
     .     +(DZ1*DX2-DZ2*DX1)*NTY(3,I)
     .     +(DX1*DY2-DY1*DX2)*NTZ(3,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(3,I)=ALP*X1(I)+BET*X2(I)
        YTK(3,I)=ALP*Y1(I)+BET*Y2(I)
        ZTK(3,I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK(3,I)-X3(I)
      DY3=YTK(3,I)-Y3(I)
      DZ3=ZTK(3,I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(3,I)
     .     +(DZ2*DX3-DZ3*DX2)*NTY(3,I)
     .     +(DX2*DY3-DY2*DX3)*NTZ(3,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(3,I)=ALP*X2(I)+BET*X3(I)
        YTK(3,I)=ALP*Y2(I)+BET*Y3(I)
        ZTK(3,I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(3,I)
     .     +(DZ3*DX1-DZ1*DX3)*NTY(3,I)
     .     +(DX3*DY1-DY3*DX1)*NTZ(3,I)
      IF (OUT<0.) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(3,I)=ALP*X3(I)+BET*X1(I)
        YTK(3,I)=ALP*Y3(I)+BET*Y1(I)
        ZTK(3,I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-----
      DX1=XTK2(I)-X1(I)
      DY1=YTK2(I)-Y1(I)
      DZ1=ZTK2(I)-Z1(I)
      DX2=XTK2(I)-X2(I)
      DY2=YTK2(I)-Y2(I)
      DZ2=ZTK2(I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(3,I)
     .      +(DZ1*DX2-DZ2*DX1)*NTY(3,I)
     .      +(DX1*DY2-DY1*DX2)*NTZ(3,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK2(I)=ALP*X1(I)+BET*X2(I)
        YTK2(I)=ALP*Y1(I)+BET*Y2(I)
        ZTK2(I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK2(I)-X3(I)
      DY3=YTK2(I)-Y3(I)
      DZ3=ZTK2(I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(3,I)
     .      +(DZ2*DX3-DZ3*DX2)*NTY(3,I)
     .      +(DX2*DY3-DY2*DX3)*NTZ(3,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK2(I)=ALP*X2(I)+BET*X3(I)
        YTK2(I)=ALP*Y2(I)+BET*Y3(I)
        ZTK2(I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(3,I)
     .      +(DZ3*DX1-DZ1*DX3)*NTY(3,I)
     .      +(DX3*DY1-DY3*DX1)*NTZ(3,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK2(I)=ALP*X3(I)+BET*X1(I)
        YTK2(I)=ALP*Y3(I)+BET*Y1(I)
        ZTK2(I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF
C-------------------------------
 400  CONTINUE
C-------------------------------
C     PROJECTION DE PK SUR L ET PENETRATION.
C-------------------------------
      DO 425 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      IF (IACTIV(3,IL)==-1) GOTO 425
C-----
      XH =XTK(3,I)
      YH =YTK(3,I)
      ZH =ZTK(3,I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE      
      YKN1 =YH**(IDG-1)
      SGNYKN=-ONE
      IF (YKN1*YH>=ZERO) SGNYKN=ONE     
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE      
      N11 =SGNXKN*XKN1*AN
      N12 =SGNYKN*YKN1*BN
      N13 =SGNZKN*ZKN1*CN
      NR1 =N11*N11+N12*N12+N13*N13
      NR1 =SQRT(NR1)
      EM =N11*XTK(3,I)+N12*YTK(3,I)+N13*ZTK(3,I)
      IF (EM<=ONE) THEN
        LAMBDA1=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR1)
        INSIDE1=1
      ELSE
        INSIDE1=0
      ENDIF
C-----
      XH =XTK2(I)
      YH =YTK2(I)
      ZH =ZTK2(I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE      
      YKN1 =YH**(IDG-1)
      SGNYKN=-ONE
      IF (YKN1*YH>=ZERO) SGNYKN=ONE     
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE      
      N21 =SGNXKN*XKN1*AN
      N22 =SGNYKN*YKN1*BN
      N23 =SGNZKN*ZKN1*CN
      NR2 =N21*N21+N22*N22+N23*N23
      NR2 =SQRT(NR2)
      EM =N21*XTK2(I)+N22*YTK2(I)+N23*ZTK2(I)
       IF (EM<=ONE) THEN
        LAMBDA2=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR2)    
        INSIDE2=1
      ELSE
        INSIDE2=0
      ENDIF
C-----
      IF (INSIDE1==0.AND.INSIDE2==0) THEN
        IACTIV(3,IL)=0
      ELSE
C-----
      IF (IACTIV(3,IL)==0) THEN
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
          XM(I)=XTK(3,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(3,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(3,I)-LAMBDA1*N13/MAX(EM20,NR1)
         ELSE
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(3,I)=XTK2(I)
          YTK(3,I)=YTK2(I)
          ZTK(3,I)=ZTK2(I)
         ENDIF
        ELSEIF(INSIDE1/=0) THEN
          XM(I)=XTK(3,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(3,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(3,I)-LAMBDA1*N13/MAX(EM20,NR1)
        ELSEIF(INSIDE2/=0) THEN
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(3,I)=XTK2(I)
          YTK(3,I)=YTK2(I)
          ZTK(3,I)=ZTK2(I)
        ENDIF
C-----
      ELSE
        XH=HOLD(1,4*(IL-1)+3)
        YH=HOLD(2,4*(IL-1)+3)
        ZH=HOLD(3,4*(IL-1)+3)
        N1=NOLD(1,4*(IL-1)+3)
        N2=NOLD(2,4*(IL-1)+3)
        N3=NOLD(3,4*(IL-1)+3)
        LAMBDA1=(XH-XTK(3,I))*N1
     .         +(YH-YTK(3,I))*N2
     .         +(ZH-ZTK(3,I))*N3
        LAMBDA2=(XH-XTK2(I))*N1
     .         +(YH-YTK2(I))*N2
     .         +(ZH-ZTK2(I))*N3
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
           XM(I)=XTK(3,I)+LAMBDA1*N1
           YM(I)=YTK(3,I)+LAMBDA1*N2
           ZM(I)=ZTK(3,I)+LAMBDA1*N3
          ELSE
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(3,I)=XTK2(I)
           YTK(3,I)=YTK2(I)
           ZTK(3,I)=ZTK2(I)
          ENDIF
        ELSEIF(INSIDE1/=0) THEN
           XM(I)=XTK(3,I)+LAMBDA1*N1
           YM(I)=YTK(3,I)+LAMBDA1*N2
           ZM(I)=ZTK(3,I)+LAMBDA1*N3
        ELSEIF(INSIDE2/=0) THEN
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(3,I)=XTK2(I)
           YTK(3,I)=YTK2(I)
           ZTK(3,I)=ZTK2(I)
        ENDIF
C       one more iteration.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))        
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        NR =N1*N1+N2*N2+N3*N3
        NR =ONE/MAX(EM20,SQRT(NR))
        N1 =N1*NR
        N2 =N2*NR
        N3 =N3*NR
        LAMBDA1=(XM(I)-XTK(3,I))*N1
     .         +(YM(I)-YTK(3,I))*N2
     .         +(ZM(I)-ZTK(3,I))*N3
        XM(I)=XTK(3,I)+LAMBDA1*N1
        YM(I)=YTK(3,I)+LAMBDA1*N2
        ZM(I)=ZTK(3,I)+LAMBDA1*N3
      ENDIF 
      IACTIV(3,IL)=IACTIV(3,IL)+1
      ENDIF
 425  CONTINUE
C-----
#include "vectorize.inc"
      DO 450 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(3,IL)<=0) GOTO 450
C-----
C      projection radiale de M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
C------
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))        
C------
C      normale a l'ellipsoide en M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE        
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE        
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE        
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       NR =N1*N1+N2*N2+N3*N3
       NR =ONE/MAX(EM20,SQRT(NR))
       NXI(3,I)=N1*NR
       NYI(3,I)=N2*NR
       NZI(3,I)=N3*NR
       XI(3,I)=XM(I)
       YI(3,I)=YM(I)
       ZI(3,I)=ZM(I)
       DEPTH(3,I)=XM(I)*NXI(3,I)+YM(I)*NYI(3,I)+ZM(I)*NZI(3,I)
C------
       PENET(3,I)=(XTK(3,I)-XM(I))*NXI(3,I)
     .          +(YTK(3,I)-YM(I))*NYI(3,I)
     .          +(ZTK(3,I)-ZM(I))*NZI(3,I)
       PENET(3,I)=-PENET(3,I)
       PENET(3,I)=MAX(ZERO,PENET(3,I))
       IF (DEPTH(3,I)-PENET(3,I)<EM10*DEPTH(3,I)) THEN
         PENET(3,I)=ZERO
         IACTIV(3,IL)=-2
       ENDIF
       ANSMX=MAX(ANSMX,PENET(3,I))
 450  CONTINUE
C-------------------------------
C     MESSAGES DESACTIVATION
C-------------------------------
      DO 460 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(3,IL)==-2) THEN
         IN1=ITAB(KSI(1,IL))
         IN2=ITAB(KSI(2,IL))
         IN3=ITAB(KSI(3,IL))
         IN4=ITAB(KSI(4,IL))
#include "lockon.inc"
         WRITE(ISTDO,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(ISTDO,'(A,A,A,/,2I8,A,4I8,A)')' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN3,IN4,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
         WRITE(IOUT ,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(IOUT,'(A,A,A,/,2I8,A,4I8,A)') ' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN3,IN4,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
#include "lockoff.inc"
         IACTIV(3,IL)=-1
      ENDIF
 460  CONTINUE
C-------------------------------
C     4EME TRIANGLE.
C-------------------------------
      DO 475 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      X1(I)=GX(1,I)
      Y1(I)=GX(2,I)
      Z1(I)=GX(3,I)
      X2(I)=XP4(1,I)
      Y2(I)=XP4(2,I)
      Z2(I)=XP4(3,I)
      X3(I)=XP1(1,I)
      Y3(I)=XP1(2,I)
      Z3(I)=XP1(3,I)
      X12(I)=X2(I)-X1(I)
      Y12(I)=Y2(I)-Y1(I)
      Z12(I)=Z2(I)-Z1(I)
      X13(I)=X3(I)-X1(I)
      Y13(I)=Y3(I)-Y1(I)
      Z13(I)=Z3(I)-Z1(I)
      N1=Y12(I)*Z13(I)-Z12(I)*Y13(I)
      N2=Z12(I)*X13(I)-X12(I)*Z13(I)
      N3=X12(I)*Y13(I)-Y12(I)*X13(I)
      NTN=ONE/MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      NTX(4,I)=NTN*N1
      NTY(4,I)=NTN*N2
      NTZ(4,I)=NTN*N3
      D(I)    =-NTX(4,I)*X1(I)-NTY(4,I)*Y1(I)-NTZ(4,I)*Z1(I)
C-----
      X23(I)=X3(I)-X2(I)
      Y23(I)=Y3(I)-Y2(I)
      Z23(I)=Z3(I)-Z2(I)
 475  CONTINUE
C-------------------------------
C     POINTS K,H SUR E,L / LA DISTANCE D(K,H) EST LOCALEMENT MAXIMUM
C-------------------------------
      DO 500 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      EH  =(ABS(NTX(4,I)/(DGR*AN))**EXPN)*AN
     .    +(ABS(NTY(4,I)/(DGR*BN))**EXPN)*BN
     .    +(ABS(NTZ(4,I)/(DGR*CN))**EXPN)*CN
C     X EST DU SIGNE DE LAMBDA*NTX, IDEM EN Y ET Z.
C     LAMBDA1=EXP(LOG(1./EH)/EXPN)
      LAMBDA1=(MAX(EM20,EH))**(-ONE/EXPN)
      XH1   =ABS(LAMBDA1*NTX(4,I)/(DGR*AN))**(ONE/(DGR-ONE))
      IF (NTX(4,I)<ZERO) XH1=-XH1
      YH1   =ABS(LAMBDA1*NTY(4,I)/(DGR*BN))**(ONE/(DGR-ONE))
      IF (NTY(4,I)<ZERO) YH1=-YH1
      ZH1   =ABS(LAMBDA1*NTZ(4,I)/(DGR*CN))**(ONE/(DGR-ONE))
      IF (NTZ(4,I)<ZERO) ZH1=-ZH1
      MU1   =-NTX(4,I)*XH1-NTY(4,I)*YH1-NTZ(4,I)*ZH1-D(I)
C     LAMBDA2=-LAMBDA1
      XH2   =-XH1
      YH2   =-YH1
      ZH2   =-ZH1
C      MU2   =-NTX(4,I)*XH2-NTY(4,I)*YH2-NTZ(4,I)*ZH2-D(I)
      MU2=-MU1-TWO*D(I)
      XTK(4,I) =XH1+MU1*NTX(4,I)
      YTK(4,I) =YH1+MU1*NTY(4,I)
      ZTK(4,I) =ZH1+MU1*NTZ(4,I)
      XTK2(I)=XH2+MU2*NTX(4,I)
      YTK2(I)=YH2+MU2*NTY(4,I)
      ZTK2(I)=ZH2+MU2*NTZ(4,I)
C-------------------------------
 500  CONTINUE
C-------------------------------
C     RAMENER LE PT PK SUR LE TRIANGLE.
C-------------------------------
      DO 525 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      DX1=XTK(4,I)-X1(I)
      DY1=YTK(4,I)-Y1(I)
      DZ1=ZTK(4,I)-Z1(I)
      DX2=XTK(4,I)-X2(I)
      DY2=YTK(4,I)-Y2(I)
      DZ2=ZTK(4,I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(4,I)
     .     +(DZ1*DX2-DZ2*DX1)*NTY(4,I)
     .     +(DX1*DY2-DY1*DX2)*NTZ(4,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(4,I)=ALP*X1(I)+BET*X2(I)
        YTK(4,I)=ALP*Y1(I)+BET*Y2(I)
        ZTK(4,I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK(4,I)-X3(I)
      DY3=YTK(4,I)-Y3(I)
      DZ3=ZTK(4,I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(4,I)
     .     +(DZ2*DX3-DZ3*DX2)*NTY(4,I)
     .     +(DX2*DY3-DY2*DX3)*NTZ(4,I)
      IF (OUT<0.) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(4,I)=ALP*X2(I)+BET*X3(I)
        YTK(4,I)=ALP*Y2(I)+BET*Y3(I)
        ZTK(4,I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(4,I)
     .     +(DZ3*DX1-DZ1*DX3)*NTY(4,I)
     .     +(DX3*DY1-DY3*DX1)*NTZ(4,I)
      IF (OUT<0.) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(4,I)=ALP*X3(I)+BET*X1(I)
        YTK(4,I)=ALP*Y3(I)+BET*Y1(I)
        ZTK(4,I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-----
      DX1=XTK2(I)-X1(I)
      DY1=YTK2(I)-Y1(I)
      DZ1=ZTK2(I)-Z1(I)
      DX2=XTK2(I)-X2(I)
      DY2=YTK2(I)-Y2(I)
      DZ2=ZTK2(I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(4,I)
     .      +(DZ1*DX2-DZ2*DX1)*NTY(4,I)
     .      +(DX1*DY2-DY1*DX2)*NTZ(4,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK2(I)=ALP*X1(I)+BET*X2(I)
        YTK2(I)=ALP*Y1(I)+BET*Y2(I)
        ZTK2(I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK2(I)-X3(I)
      DY3=YTK2(I)-Y3(I)
      DZ3=ZTK2(I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(4,I)
     .      +(DZ2*DX3-DZ3*DX2)*NTY(4,I)
     .      +(DX2*DY3-DY2*DX3)*NTZ(4,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK2(I)=ALP*X2(I)+BET*X3(I)
        YTK2(I)=ALP*Y2(I)+BET*Y3(I)
        ZTK2(I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(4,I)
     .      +(DZ3*DX1-DZ1*DX3)*NTY(4,I)
     .      +(DX3*DY1-DY3*DX1)*NTZ(4,I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=ONE-BET
        XTK2(I)=ALP*X3(I)+BET*X1(I)
        YTK2(I)=ALP*Y3(I)+BET*Y1(I)
        ZTK2(I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-------------------------------
 525  CONTINUE
C-------------------------------
C     PROJECTION DE PK SUR L ET PENETRATION.
C-------------------------------
      DO 550 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      IF (IACTIV(4,IL)==-1) GOTO 550
C-----
      XH =XTK(4,I)
      YH =YTK(4,I)
      ZH =ZTK(4,I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE
      YKN1 =YH**(IDG-1)
      SGNYKN=-ONE
      IF (YKN1*YH>=ZERO) SGNYKN=ONE
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE
      N11 =SGNXKN*XKN1*AN
      N12 =SGNYKN*YKN1*BN
      N13 =SGNZKN*ZKN1*CN
      NR1 =N11*N11+N12*N12+N13*N13
      NR1 =SQRT(NR1)
      EM =N11*XTK(4,I)+N12*YTK(4,I)+N13*ZTK(4,I)
      IF (EM<=ONE) THEN
        LAMBDA1=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR1)
        INSIDE1=1
      ELSE
        INSIDE1=0
      ENDIF
C-----
      XH =XTK2(I)
      YH =YTK2(I)
      ZH =ZTK2(I)
      XKN1 =XH**(IDG-1)
      SGNXKN=-ONE
      IF (XKN1*XH>=ZERO) SGNXKN=ONE
      YKN1 =YH**(IDG-1)
      SGNYKN=-ONE
      IF (YKN1*YH>=ONE) SGNYKN=ONE
      ZKN1 =ZH**(IDG-1)
      SGNZKN=-ONE
      IF (ZKN1*ZH>=ZERO) SGNZKN=ONE
      N21 =SGNXKN*XKN1*AN
      N22 =SGNYKN*YKN1*BN
      N23 =SGNZKN*ZKN1*CN
      NR2 =N21*N21+N22*N22+N23*N23
      NR2 =SQRT(NR2)
      EM =N21*XTK2(I)+N22*YTK2(I)+N23*ZTK2(I)
      IF (EM<=ONE) THEN
        LAMBDA2=(EM-EXP((DGR-ONE)*LOG(MAX(EM,EM20))/DGR))
     .         / MAX(EXP((DGR-ONE)*LOG(EM20)/DGR),NR2)
        INSIDE2=1
      ELSE
        INSIDE2=0
      ENDIF
C-----
      IF (INSIDE1==0.AND.INSIDE2==0) THEN
        IACTIV(4,IL)=0
      ELSE
C-----
      IF (IACTIV(4,IL)==0) THEN
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
          XM(I)=XTK(4,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(4,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(4,I)-LAMBDA1*N13/MAX(EM20,NR1)
         ELSE
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(4,I)=XTK2(I)
          YTK(4,I)=YTK2(I)
          ZTK(4,I)=ZTK2(I)
         ENDIF
        ELSEIF(INSIDE1/=0) THEN
          XM(I)=XTK(4,I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(4,I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(4,I)-LAMBDA1*N13/MAX(EM20,NR1)
        ELSEIF(INSIDE2/=0) THEN
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(4,I)=XTK2(I)
          YTK(4,I)=YTK2(I)
          ZTK(4,I)=ZTK2(I)
        ENDIF
C-----
      ELSE
        XH=HOLD(1,4*(IL-1)+4)
        YH=HOLD(2,4*(IL-1)+4)
        ZH=HOLD(3,4*(IL-1)+4)
        N1=NOLD(1,4*(IL-1)+4)
        N2=NOLD(2,4*(IL-1)+4)
        N3=NOLD(3,4*(IL-1)+4)
        LAMBDA1=(XH-XTK(4,I))*N1
     .         +(YH-YTK(4,I))*N2
     .         +(ZH-ZTK(4,I))*N3
        LAMBDA2=(XH-XTK2(I))*N1
     .         +(YH-YTK2(I))*N2
     .         +(ZH-ZTK2(I))*N3
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
           XM(I)=XTK(4,I)+LAMBDA1*N1
           YM(I)=YTK(4,I)+LAMBDA1*N2
           ZM(I)=ZTK(4,I)+LAMBDA1*N3
          ELSE
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(4,I)=XTK2(I)
           YTK(4,I)=YTK2(I)
           ZTK(4,I)=ZTK2(I)
          ENDIF
        ELSEIF(INSIDE1/=0) THEN
           XM(I)=XTK(4,I)+LAMBDA1*N1
           YM(I)=YTK(4,I)+LAMBDA1*N2
           ZM(I)=ZTK(4,I)+LAMBDA1*N3
        ELSEIF(INSIDE2/=0) THEN
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(4,I)=XTK2(I)
           YTK(4,I)=YTK2(I)
           ZTK(4,I)=ZTK2(I)
        ENDIF
C       one more iteration.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE 
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
        N1 =SGNXKN*XKN1*AN
        N2 =SGNYKN*YKN1*BN
        N3 =SGNZKN*ZKN1*CN
        NR =N1*N1+N2*N2+N3*N3
        NR =1./MAX(EM20,SQRT(NR))
        N1 =N1*NR
        N2 =N2*NR
        N3 =N3*NR
        LAMBDA1=(XM(I)-XTK(4,I))*N1
     .         +(YM(I)-YTK(4,I))*N2
     .         +(ZM(I)-ZTK(4,I))*N3
        XM(I)=XTK(4,I)+LAMBDA1*N1
        YM(I)=YTK(4,I)+LAMBDA1*N2
        ZM(I)=ZTK(4,I)+LAMBDA1*N3
      ENDIF 
      IACTIV(4,IL)=IACTIV(4,IL)+1
      ENDIF
 550  CONTINUE
C-----
#include "vectorize.inc"
      DO 575 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(4,IL)<=0) GOTO 575
C-----
C      projection radiale de M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       EM=N1*XM(I)+N2*YM(I)+N3*ZM(I)
C------
        XM(I)=XM(I)/MAX(EM20,EM**(ONE/DGR))
        YM(I)=YM(I)/MAX(EM20,EM**(ONE/DGR))
        ZM(I)=ZM(I)/MAX(EM20,EM**(ONE/DGR))
C------
C      normale a l'ellipsoide en M.
        XKN1 =XM(I)**(IDG-1)
        SGNXKN=-ONE
        IF (XKN1*XM(I)>=ZERO) SGNXKN=ONE
        YKN1 =YM(I)**(IDG-1)
        SGNYKN=-ONE
        IF (YKN1*YM(I)>=ZERO) SGNYKN=ONE
        ZKN1 =ZM(I)**(IDG-1)
        SGNZKN=-ONE
        IF (ZKN1*ZM(I)>=ZERO) SGNZKN=ONE
       N1 =SGNXKN*XKN1*AN
       N2 =SGNYKN*YKN1*BN
       N3 =SGNZKN*ZKN1*CN
       NR =N1*N1+N2*N2+N3*N3
       NR =ONE/MAX(EM20,SQRT(NR))
       NXI(4,I)=N1*NR
       NYI(4,I)=N2*NR
       NZI(4,I)=N3*NR
       XI(4,I)=XM(I)
       YI(4,I)=YM(I)
       ZI(4,I)=ZM(I)
       DEPTH(4,I)=XM(I)*NXI(4,I)+YM(I)*NYI(4,I)+ZM(I)*NZI(4,I)
C------
       PENET(4,I)=(XTK(4,I)-XM(I))*NXI(4,I)
     .          +(YTK(4,I)-YM(I))*NYI(4,I)
     .          +(ZTK(4,I)-ZM(I))*NZI(4,I)
       PENET(4,I)=-PENET(4,I)
       PENET(4,I)=MAX(ZERO,PENET(4,I))
       IF (DEPTH(4,I)-PENET(4,I)<EM10*DEPTH(4,I)) THEN
         PENET(4,I)=ZERO
         IACTIV(4,IL)=-2
       ENDIF
       ANSMX=MAX(ANSMX,PENET(4,I))
 575  CONTINUE
C-------------------------------
C     MESSAGES DESACTIVATION
C-------------------------------
      DO 585 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(4,IL)==-2) THEN
         IN1=ITAB(KSI(1,IL))
         IN2=ITAB(KSI(2,IL))
         IN3=ITAB(KSI(3,IL))
         IN4=ITAB(KSI(4,IL))
#include "lockon.inc"
         WRITE(ISTDO,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(ISTDO,'(A,A,A,/,2I8,A,4I8,A)')' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN4,IN1,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
         WRITE(IOUT ,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(IOUT,'(A,A,A,/,2I8,A,4I8,A)') ' 3NODES FACET',
     .         ' FROM 4NODES ELEMENT/SEGMENT,',                 
     .         ' DE-ACTIVATED FROM INTERFACE; FACET VERTICES:',
     .         IN4,IN1,'ISOCENTER(',IN1,IN2,IN3,IN4,')'
#include "lockoff.inc"
         IACTIV(4,IL)=-1
      ENDIF
 585  CONTINUE
C------------------------------------------------------------
      RETURN
      END
