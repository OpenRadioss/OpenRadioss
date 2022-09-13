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
Chd|  I15TOTT1                      source/interfaces/int15/i15tott1.F
Chd|-- called by -----------
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I15TOTT1(NOINT ,NDEB ,NTC   ,X     ,KSURF ,
     2                  IGRSURF  ,BUFSF ,KTC   ,KSI   ,NOLD  ,
     3                  XP1   ,XP2    ,XP3   ,XTK   ,YTK   ,
     4                  ZTK   ,NTX    ,NTY   ,NTZ   ,PENET ,
     5                  DEPTH ,XI    ,YI     ,ZI    ,NXI   ,
     6                  NYI   ,NZI   ,ANSMX  ,HOLD  ,IACTIV,
     7                  ITAB )
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KSURF ,KSI(4,*) ,
     .        NOINT ,NDEB  ,NTC, IACTIV(4,*), KTC(*), ITAB(*)
C     REAL
      my_real 
     .  X(3,*)    ,BUFSF(*) ,NOLD(3,*) ,
     .  XTK(*) ,YTK(*) ,ZTK(*) ,NTX(*) ,NTY(*) ,NTZ(*) ,
     .  PENET(*) ,DEPTH(*) ,XI(*) ,YI(*)  ,ZI(*)  ,
     .  NXI(*)   ,NYI(*) ,NZI(*) ,XP1(3,MVSIZ), XP2(3,MVSIZ),
     .  XP3(3,MVSIZ), ANSMX, HOLD(3,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IL, NLS, IDG,
     .        IN1, IN2, IN3,
     .        INSIDE1,INSIDE2
      my_real
     .   A, B, C, AN, BN, CN, ROT(9), DGR, EXPN,
     .   XG, YG, ZG,
     .   NTN,
     .   N1, N2, N3, N, N11,N12,N13,NR1,N21,N22,N23,NR2,
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
     .   XTK2(MVSIZ) , YTK2(MVSIZ) , ZTK2(MVSIZ) , 
     .   D(MVSIZ)
C-----------------------------------------------
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
      A =BUFSF(ADRBUF+1)
      B =BUFSF(ADRBUF+2)
      C =BUFSF(ADRBUF+3)
      DGR =BUFSF(ADRBUF+36)
      IDG =NINT(DGR)
      DGR =IDG
      DGR =MAX(TWO,DGR)
      EXPN=DGR/(DGR-ONE)
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
      DO 75 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      I  =NLS-NDEB
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
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
 75   CONTINUE
C-------------------------------
      DO 100 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      I  =NLS-NDEB
C-----
      X1(I)=XP1(1,I)
      Y1(I)=XP1(2,I)
      Z1(I)=XP1(3,I)
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
      NTX(I)=NTN*N1
      NTY(I)=NTN*N2
      NTZ(I)=NTN*N3
      D(I)    =-NTX(I)*X1(I)-NTY(I)*Y1(I)-NTZ(I)*Z1(I)
C-----
      X23(I)=X3(I)-X2(I)
      Y23(I)=Y3(I)-Y2(I)
      Z23(I)=Z3(I)-Z2(I)
 100  CONTINUE
C-------------------------------
C     POINTS K,H SUR E,L / LA DISTANCE D(K,H) EST LOCALEMENT MAXIMUM
C-------------------------------
      DO 125 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      I  =NLS-NDEB
C-----
      EH  =(ABS(NTX(I)/(DGR*AN))**EXPN)*AN
     .    +(ABS(NTY(I)/(DGR*BN))**EXPN)*BN
     .    +(ABS(NTZ(I)/(DGR*CN))**EXPN)*CN
C     X EST DU SIGNE DE LAMBDA*NTX, IDEM EN Y ET Z.
C     LAMBDA1=EXP(LOG(1./EH)/EXPN)
      LAMBDA1=(MAX(EM20,EH))**(-ONE/EXPN)
      XH1   =ABS(LAMBDA1*NTX(I)/(DGR*AN))**(ONE/(DGR-ONE))
      IF (NTX(I)<ZERO) XH1=-XH1
      YH1   =ABS(LAMBDA1*NTY(I)/(DGR*BN))**(ONE/(DGR-ONE))
      IF (NTY(I)<ZERO) YH1=-YH1
      ZH1   =ABS(LAMBDA1*NTZ(I)/(DGR*CN))**(ONE/(DGR-ONE))
      IF (NTZ(I)<ZERO) ZH1=-ZH1
      MU1   =-NTX(I)*XH1-NTY(I)*YH1-NTZ(I)*ZH1-D(I)
C     LAMBDA2=-LAMBDA1
      XH2   =-XH1
      YH2   =-YH1
      ZH2   =-ZH1
C      MU2   =-NTX(I)*XH2-NTY(I)*YH2-NTZ(I)*ZH2-D(I)
      MU2=-MU1-TWO*D(I)
      XTK(I) =XH1+MU1*NTX(I)
      YTK(I) =YH1+MU1*NTY(I)
      ZTK(I) =ZH1+MU1*NTZ(I)
      XTK2(I)=XH2+MU2*NTX(I)
      YTK2(I)=YH2+MU2*NTY(I)
      ZTK2(I)=ZH2+MU2*NTZ(I)
C-------------------------------
 125  CONTINUE
C-------------------------------
C     RAMENER LE PT PK SUR LE TRIANGLE.
C-------------------------------
      DO 150 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      I  =NLS-NDEB
C-----
      DX1=XTK(I)-X1(I)
      DY1=YTK(I)-Y1(I)
      DZ1=ZTK(I)-Z1(I)
      DX2=XTK(I)-X2(I)
      DY2=YTK(I)-Y2(I)
      DZ2=ZTK(I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(I)
     .     +(DZ1*DX2-DZ2*DX1)*NTY(I)
     .     +(DX1*DY2-DY1*DX2)*NTZ(I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 1,2
        PS =DX1*X12(I)+DY1*Y12(I)+DZ1*Z12(I)
        NR =X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(I)=ALP*X1(I)+BET*X2(I)
        YTK(I)=ALP*Y1(I)+BET*Y2(I)
        ZTK(I)=ALP*Z1(I)+BET*Z2(I)
      ENDIF 
      DX3=XTK(I)-X3(I)
      DY3=YTK(I)-Y3(I)
      DZ3=ZTK(I)-Z3(I)
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(I)
     .     +(DZ2*DX3-DZ3*DX2)*NTY(I)
     .     +(DX2*DY3-DY2*DX3)*NTZ(I)
      IF (OUT<ZERO) THEN
C       PROJECTION SUR 2,3
        PS =DX2*X23(I)+DY2*Y23(I)+DZ2*Z23(I)
        NR =X23(I)*X23(I)+Y23(I)*Y23(I)+Z23(I)*Z23(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(I)=ALP*X2(I)+BET*X3(I)
        YTK(I)=ALP*Y2(I)+BET*Y3(I)
        ZTK(I)=ALP*Z2(I)+BET*Z3(I)
      ENDIF 
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(I)
     .     +(DZ3*DX1-DZ1*DX3)*NTY(I)
     .     +(DX3*DY1-DY3*DX1)*NTZ(I)
      IF (OUT<0.) THEN
C       PROJECTION SUR 3,1
        PS =-DX3*X13(I)-DY3*Y13(I)-DZ3*Z13(I)
        NR =X13(I)*X13(I)+Y13(I)*Y13(I)+Z13(I)*Z13(I)
        BET=PS/MAX(EM20,NR)
        BET=MAX(BET,ZERO)
        BET=MIN(BET,ONE)
        ALP=1.-BET
        XTK(I)=ALP*X3(I)+BET*X1(I)
        YTK(I)=ALP*Y3(I)+BET*Y1(I)
        ZTK(I)=ALP*Z3(I)+BET*Z1(I)
      ENDIF 
C-----
      DX1=XTK2(I)-X1(I)
      DY1=YTK2(I)-Y1(I)
      DZ1=ZTK2(I)-Z1(I)
      DX2=XTK2(I)-X2(I)
      DY2=YTK2(I)-Y2(I)
      DZ2=ZTK2(I)-Z2(I)
      OUT = (DY1*DZ2-DY2*DZ1)*NTX(I)
     .      +(DZ1*DX2-DZ2*DX1)*NTY(I)
     .      +(DX1*DY2-DY1*DX2)*NTZ(I)
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
      OUT = (DY2*DZ3-DY3*DZ2)*NTX(I)
     .      +(DZ2*DX3-DZ3*DX2)*NTY(I)
     .      +(DX2*DY3-DY2*DX3)*NTZ(I)
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
      OUT = (DY3*DZ1-DY1*DZ3)*NTX(I)
     .      +(DZ3*DX1-DZ1*DX3)*NTY(I)
     .      +(DX3*DY1-DY3*DX1)*NTZ(I)
      IF (OUT<0.) THEN
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
      DO 175 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      I  =NLS-NDEB
      IF (IACTIV(1,IL)==-1) GOTO 175
C-----
      XH =XTK(I)
      YH =YTK(I)
      ZH =ZTK(I)
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
      EM =N11*XTK(I)+N12*YTK(I)+N13*ZTK(I)
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
          XM(I)=XTK(I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(I)-LAMBDA1*N13/MAX(EM20,NR1)
         ELSE
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(I)=XTK2(I)
          YTK(I)=YTK2(I)
          ZTK(I)=ZTK2(I)
         ENDIF
        ELSEIF(INSIDE1/=0) THEN
          XM(I)=XTK(I)-LAMBDA1*N11/MAX(EM20,NR1)
          YM(I)=YTK(I)-LAMBDA1*N12/MAX(EM20,NR1)
          ZM(I)=ZTK(I)-LAMBDA1*N13/MAX(EM20,NR1)
        ELSEIF(INSIDE2/=0) THEN
          XM(I)=XTK2(I)-LAMBDA2*N21/MAX(EM20,NR2)
          YM(I)=YTK2(I)-LAMBDA2*N22/MAX(EM20,NR2)
          ZM(I)=ZTK2(I)-LAMBDA2*N23/MAX(EM20,NR2)
          XTK(I)=XTK2(I)
          YTK(I)=YTK2(I)
          ZTK(I)=ZTK2(I)
        ENDIF
C-----
      ELSE
        XH=HOLD(1,4*(IL-1)+1)
        YH=HOLD(2,4*(IL-1)+1)
        ZH=HOLD(3,4*(IL-1)+1)
        N1=NOLD(1,4*(IL-1)+1)
        N2=NOLD(2,4*(IL-1)+1)
        N3=NOLD(3,4*(IL-1)+1)
        LAMBDA1=(XH-XTK(I))*N1
     .         +(YH-YTK(I))*N2
     .         +(ZH-ZTK(I))*N3
        LAMBDA2=(XH-XTK2(I))*N1
     .         +(YH-YTK2(I))*N2
     .         +(ZH-ZTK2(I))*N3
        IF (INSIDE1/=0.AND.INSIDE2/=0) THEN
         IF (ABS(LAMBDA1)>=ABS(LAMBDA2)) THEN
           XM(I)=XTK(I)+LAMBDA1*N1
           YM(I)=YTK(I)+LAMBDA1*N2
           ZM(I)=ZTK(I)+LAMBDA1*N3
          ELSE
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(I)=XTK2(I)
           YTK(I)=YTK2(I)
           ZTK(I)=ZTK2(I)
          ENDIF
        ELSEIF(INSIDE1/=0) THEN
           XM(I)=XTK(I)+LAMBDA1*N1
           YM(I)=YTK(I)+LAMBDA1*N2
           ZM(I)=ZTK(I)+LAMBDA1*N3
        ELSEIF(INSIDE2/=0) THEN
           XM(I)=XTK2(I)+LAMBDA2*N1
           YM(I)=YTK2(I)+LAMBDA2*N2
           ZM(I)=ZTK2(I)+LAMBDA2*N3
           XTK(I)=XTK2(I)
           YTK(I)=YTK2(I)
           ZTK(I)=ZTK2(I)
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
        LAMBDA1=(XM(I)-XTK(I))*N1
     .         +(YM(I)-YTK(I))*N2
     .         +(ZM(I)-ZTK(I))*N3
        XM(I)=XTK(I)+LAMBDA1*N1
        YM(I)=YTK(I)+LAMBDA1*N2
        ZM(I)=ZTK(I)+LAMBDA1*N3
      ENDIF 
      IACTIV(1,IL)=IACTIV(1,IL)+1
      ENDIF
 175  CONTINUE
C-----
#include "vectorize.inc"
      DO 200 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
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
C-----
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
C-----
       NXI(I)=N1*NR
       NYI(I)=N2*NR
       NZI(I)=N3*NR
       XI(I)=XM(I)
       YI(I)=YM(I)
       ZI(I)=ZM(I)
       DEPTH(I)=XM(I)*NXI(I)+YM(I)*NYI(I)+ZM(I)*NZI(I)
C------
      PENET(I)=(XTK(I)-XM(I))*NXI(I)
     .        +(YTK(I)-YM(I))*NYI(I)
     .        +(ZTK(I)-ZM(I))*NZI(I)
      PENET(I)=-PENET(I)
      PENET(I)=MAX(ZERO,PENET(I))
      IF (DEPTH(I)-PENET(I)<EM10*DEPTH(I)) THEN
         PENET(I)    =ZERO
         IACTIV(1,IL)=-2
      ENDIF
      ANSMX=MAX(ANSMX,PENET(I))
 200  CONTINUE
C-------------------------------
C     MESSAGES DESACTIVATION
C-------------------------------
      DO 210 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(1,IL)==-2) THEN
         IN1=ITAB(KSI(1,IL))
         IN2=ITAB(KSI(2,IL))
         IN3=ITAB(KSI(3,IL))
#include "lockon.inc"
         WRITE(ISTDO,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(ISTDO,'(A,A,A,3I8)')' ELEMENT/SEGMENT',
     .                   ' DE-ACTIVATED FROM INTERFACE;',                 
     .                   ' ELEMENT/SEGMENT NODES:',
     .                   IN1,IN2,IN3
         WRITE(IOUT ,'(A,I8)')' WARNING INTERFACE ',NOINT
         WRITE(IOUT,'(A,A,A,3I8)')' ELEMENT/SEGMENT',
     .                   ' DE-ACTIVATED FROM INTERFACE;',                 
     .                   ' ELEMENT/SEGMENT NODES:',
     .                   IN1,IN2,IN3
#include "lockoff.inc"
         IACTIV(1,IL)=-1
      ENDIF
 210  CONTINUE
C------------------------------------------------------------
      RETURN
      END
