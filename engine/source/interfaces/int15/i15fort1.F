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
Chd|  I15FORT1                      source/interfaces/int15/i15fort1.F
Chd|-- called by -----------
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I15FORT1(NDEB , NTC  ,STFAC ,X     ,V    ,
     2                  KSURF  ,IGRSURF ,BUFSF ,KTC   ,KSI  ,
     3                  IACTIV ,IOLD  ,HOLD  ,NOLD  ,DOLD ,
     4                  XP1   ,XP2   ,XP3   ,XTK   ,YTK  ,
     5                  ZTK   ,NTX   ,NTY   ,NTZ   ,PENET,
     6                  DEPTH ,XI    ,YI    ,ZI    ,NXI   ,
     7                  NYI  ,NZI   ,MS    ,DE    ,NPC   ,
     8                  PLD  ,WNF   ,WTF   ,WNS   ,FNORMX,
     9                  FNORMY,FNORMZ,FTANGX,FTANGY,FTANGZ  ,
     A                  DT2T  ,NOINT , NELTST ,ITYPTST ,VFRIC )
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
#include      "com08_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDEB, NTC
      INTEGER KSURF,
     .        KSI(4,*),IACTIV(4,*),NPC(*),KTC(*),
     .        NOINT,NELTST,ITYPTST
C     REAL
      my_real STFAC, BUFSF(*),X(3,*) ,  IOLD(3,*),
     .  HOLD(3,*) , NOLD(3,*) ,DOLD(3,*),MS(*)  ,  V(3,*),
     .  DE, PLD(*), XTK(*)    ,YTK(*)   ,ZTK(*) ,
     .  NTX(*) ,NTY(*) ,NTZ(*) ,
     .  PENET(*) ,DEPTH(*),
     .  XI(*) ,YI(*)  ,ZI(*)   ,NXI(*) ,NYI(*) ,NZI(*) ,
     .  WNF(3,*) ,WTF(3,*) ,WNS(*) ,XP1(3,*) ,XP2(3,*) ,XP3(3,*) ,
     .  FNORMX,FNORMY,FNORMZ,FTANGX,FTANGY,FTANGZ,
     .  DT2T,VFRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IL, IN1, IN2, IN3, IN4, NLS
      my_real
     .   A, B, C, ROT(9), X1, X2, X3, XM, YM, ZM,
     .   V1, V2, V3, VXM, VYM, VZM, VRX, VRY, VRZ,
     .   V1X2, V2X1, V1X3, V3X1, V2X3, V3X2,
     .   DELTX, DELTY, DELTZ, ND, PN, SCALE, NV,
     .   VXK, VYK, VZK, DVX, DVY, DVZ, XH, YH, ZH,
     .   DX1, DY1, DZ1, DX2, DY2, DZ2, DX3, DY3, DZ3,
     .   DX, DY, DZ,
     .   S1, S2, S3, S,
     .   F1, F11, F12, F13,
     .   FTX, FTY, FTZ, FNX, FNY, FNZ, FTN, FMAX,FN,
     .   H, DTI
      my_real
     .   FXN(MVSIZ),FYN(MVSIZ),FZN(MVSIZ),STF(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   L1(MVSIZ),L2(MVSIZ),L3(MVSIZ),
     .   VX1(MVSIZ),VY1(MVSIZ),VZ1(MVSIZ),
     .   VX2(MVSIZ),VY2(MVSIZ),VZ2(MVSIZ),
     .   VX3(MVSIZ),VY3(MVSIZ),VZ3(MVSIZ),
     .   VXH(MVSIZ),VYH(MVSIZ),VZH(MVSIZ)
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
      A =BUFSF(ADRBUF+1)
      B =BUFSF(ADRBUF+2)
      C =BUFSF(ADRBUF+3)
      DO I=1,9
       ROT(I)=BUFSF(ADRBUF+7+I-1)
      END DO
C-----------------------------------------------
C     Noeud main dans le repere de l'ellipsoide :
C-----------------------------------------------
      X1=BUFSF(ADRBUF+16)-BUFSF(ADRBUF+4)
      X2=BUFSF(ADRBUF+17)-BUFSF(ADRBUF+5)
      X3=BUFSF(ADRBUF+18)-BUFSF(ADRBUF+6)
      XM=ROT(1)*X1+ROT(2)*X2+ROT(3)*X3
      YM=ROT(4)*X1+ROT(5)*X2+ROT(6)*X3
      ZM=ROT(7)*X1+ROT(8)*X2+ROT(9)*X3
C-----------------------------------------------
C     Vitesse du noeud main en local.
C-----------------------------------------------
      V1=BUFSF(ADRBUF+19)
      V2=BUFSF(ADRBUF+20)
      V3=BUFSF(ADRBUF+21)
      VXM=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
      VYM=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
      VZM=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
C-----------------------------------------------
C     Vitesse  de rotation du noeud main en local.
C-----------------------------------------------
      V1=BUFSF(ADRBUF+22)
      V2=BUFSF(ADRBUF+23)
      V3=BUFSF(ADRBUF+24)
      VRX=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
      VRY=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
      VRZ=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
C----------------------------------------------------------
C     PASSAGE DES VITESSES EN LOCAL.
C     non optimise (plusieurs fois / noeud).
C----------------------------------------------------------
      DO 100 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 100
      I  =NLS-NDEB
C------
       IN1=KSI(1,IL)
       V1=V(1,IN1)
       V2=V(2,IN1)
       V3=V(3,IN1)
       VX1(I)=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
       VY1(I)=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
       VZ1(I)=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
C------
       IN2=KSI(2,IL)
       V1=V(1,IN2)
       V2=V(2,IN2)
       V3=V(3,IN2)
       VX2(I)=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
       VY2(I)=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
       VZ2(I)=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
C------
       IN3=KSI(3,IL)
       V1=V(1,IN3)
       V2=V(2,IN3)
       V3=V(3,IN3)
       VX3(I)=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
       VY3(I)=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
       VZ3(I)=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
 100  CONTINUE     
C----------------------------------------------------------
C     Force normale.
C----------------------------------------------------------
      DO 200 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 200
      I  =NLS-NDEB
C       STF(I)=STFAC*DEPTH(I)/MAX(DEPTH(I)-PENET(I),EM20)
      STF(I)=STFAC*DEPTH(I)**2/MAX((DEPTH(I)-PENET(I))**2,EM20)
      FXN(I)=STF(I)*PENET(I)*NXI(I)
      FYN(I)=STF(I)*PENET(I)*NYI(I)
      FZN(I)=STF(I)*PENET(I)*NZI(I)
 200  CONTINUE
C-------------------------------
C     Extraction du pt de contact au cycle precedent.
C-------------------------------
      DO 250 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 250
      I  =NLS-NDEB
C     Point sur le triangle tq D(P,L) etait maximum au cycle precedent.
      L1(I)=IOLD(1,4*(IL-1)+1)
      L2(I)=IOLD(2,4*(IL-1)+1)
      L3(I)=IOLD(3,4*(IL-1)+1)
 250  CONTINUE
C----------------------------------------------------------
C     Frottement :
C----------------------------------------------------------
#include "vectorize.inc"
      DO 275 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 275
      I  =NLS-NDEB
C------------------------------
      FXT(I)=ZERO
      FYT(I)=ZERO
      FZT(I)=ZERO     
C-------------------------------
C     TOURNER DELTA(T-1) DANS LE PLAN TANGENT.
C-------------------------------
      IF (IACTIV(1,IL)>2) THEN
       DELTX=DOLD(1,4*(IL-1)+1)
       DELTY=DOLD(2,4*(IL-1)+1)
       DELTZ=DOLD(3,4*(IL-1)+1)
       ND =SQRT(DELTX*DELTX+DELTY*DELTY+DELTZ*DELTZ)
       IF (ND/=ZERO) THEN
        PN=DELTX*NXI(I)+DELTY*NYI(I)+DELTZ*NZI(I)
        DELTX=DELTX-PN*NXI(I)
        DELTY=DELTY-PN*NYI(I)
        DELTZ=DELTZ-PN*NZI(I)
        SCALE=ND/SQRT(DELTX*DELTX+DELTY*DELTY+DELTZ*DELTZ)
        DELTX=SCALE*DELTX
        DELTY=SCALE*DELTY
        DELTZ=SCALE*DELTZ
       ENDIF
      ELSE
       DELTX=ZERO
       DELTY=ZERO
       DELTZ=ZERO    
      ENDIF
C-------------------------------
C     INCREMENT SUR DELTA (T-1 -> T) : VITESSE RELATIVE TANGENTE DE POLD * DT
C-------------------------------
      XH=HOLD(1,4*(IL-1)+1)
      YH=HOLD(2,4*(IL-1)+1)
      ZH=HOLD(3,4*(IL-1)+1)
      V1X2=VRX*(YH-YM)
      V2X1=VRY*(XH-XM)
      V2X3=VRY*(ZH-ZM)
      V3X2=VRZ*(YH-YM)
      V3X1=VRZ*(XH-XM)
      V1X3=VRX*(ZH-ZM)
      V3 =V1X2-V2X1
      V1 =V2X3-V3X2
      V2 =V3X1-V1X3
      VXH(I)=VXM+V1
      VYH(I)=VYM+V2
      VZH(I)=VZM+V3
      IF (IACTIV(1,IL)>=2) THEN
       VXK=L1(I)*VX1(I)+L2(I)*VX2(I)+L3(I)*VX3(I)
       VYK=L1(I)*VY1(I)+L2(I)*VY2(I)+L3(I)*VY3(I)
       VZK=L1(I)*VZ1(I)+L2(I)*VZ2(I)+L3(I)*VZ3(I)
       DVX=VXH(I)-VXK
       DVY=VYH(I)-VYK
       DVZ=VZH(I)-VZK
       PN=DVX*NXI(I)+DVY*NYI(I)+DVZ*NZI(I)
       DVX=DVX-PN*NXI(I)
       DVY=DVY-PN*NYI(I)
       DVZ=DVZ-PN*NZI(I)
      ELSE
       DVX=ZERO
       DVY=ZERO
       DVZ=ZERO    
      ENDIF
C-----
C     Force tangente K.DELTA, K=1. et Norme.
      DOLD(1,4*(IL-1)+1)=DELTX+DVX*DT1
      DOLD(2,4*(IL-1)+1)=DELTY+DVY*DT1
      DOLD(3,4*(IL-1)+1)=DELTZ+DVZ*DT1
      FXT(I)=STF(I)*DOLD(1,4*(IL-1)+1)
      FYT(I)=STF(I)*DOLD(2,4*(IL-1)+1)
      FZT(I)=STF(I)*DOLD(3,4*(IL-1)+1)
C-----
 275  CONTINUE
C------------------------------------------------------------
C     FRICTION (LOCAL COULOMB FRICTION).
C------------------------------------------------------------
#include "vectorize.inc"
      DO 625 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 625
      I  =NLS-NDEB
C-----
      FTX=FXT(I)
      FTY=FYT(I)
      FTZ=FZT(I)
      FNX=FXN(I)
      FNY=FYN(I)
      FNZ=FZN(I)
      FTN=SQRT(FTX*FTX+FTY*FTY+FTZ*FTZ)
      FN =SQRT(FNX*FNX+FNY*FNY+FNZ*FNZ)
      FMAX= MAX(VFRIC*FN,ZERO)
      IF (FTN>FMAX) THEN
        SCALE =FMAX/FTN
        FTX=SCALE*FTX
        FTY=SCALE*FTY
        FTZ=SCALE*FTZ
      ELSE
        SCALE=1.
      ENDIF
      IF (IACTIV(1,IL)>1) THEN
C      Glissement DELTA=scale.DELTA
       DELTX=SCALE*DOLD(1,4*(IL-1)+1)
       DELTY=SCALE*DOLD(2,4*(IL-1)+1)
       DELTZ=SCALE*DOLD(3,4*(IL-1)+1)
C      MISE EN MEMOIRE BUFFERS D'INTERFACE (-> PROCHAIN CYCLE).
       DOLD(1,4*(IL-1)+1)=DELTX
       DOLD(2,4*(IL-1)+1)=DELTY
       DOLD(3,4*(IL-1)+1)=DELTZ
      ENDIF
      FXT(I)=FTX
      FYT(I)=FTY
      FZT(I)=FTZ
 625  CONTINUE
C------------------------------------------------------------
C     LOCAL COORDINATES OF CONTACT POINT WITH RESPECT TO ELEMENT.
C------------------------------------------------------------
C#include "vectorize.inc"
      DO 650 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 650
      I  =NLS-NDEB
C-----
      DX2=XP2(1,I)-XTK(I)
      DY2=XP2(2,I)-YTK(I)
      DZ2=XP2(3,I)-ZTK(I)
      DX3=XP3(1,I)-XTK(I)
      DY3=XP3(2,I)-YTK(I)
      DZ3=XP3(3,I)-ZTK(I)
      DX=DY2*DZ3-DZ2*DY3
      DY=DX3*DZ2-DZ3*DX2
      DZ=DX2*DY3-DY2*DX3
      S1=HALF*SQRT(DX*DX+DY*DY+DZ*DZ)
C-----
      DX1=XP1(1,I) -XTK(I)
      DY1=XP1(2,I) -YTK(I)
      DZ1=XP1(3,I) -ZTK(I)
      DX=DY1*DZ3-DZ1*DY3
      DY=DX3*DZ1-DZ3*DX1
      DZ=DX1*DY3-DY1*DX3
      S2=HALF*SQRT(DX*DX+DY*DY+DZ*DZ)
C-----
      DX=DY1*DZ2-DZ1*DY2
      DY=DX2*DZ1-DZ2*DX1
      DZ=DX1*DY2-DY1*DX2
      S3=HALF*SQRT(DX*DX+DY*DY+DZ*DZ)
C-----
      S=ONE/(S1+S2+S3)
C-----
      IOLD(1,4*(IL-1)+1)=S1*S
      IOLD(2,4*(IL-1)+1)=S2*S
      IOLD(3,4*(IL-1)+1)=S3*S
 650  CONTINUE
C------------------------------------------------------------
C     MISE EN MEMOIRE BUFFERS D'INTERFACE (-> PROCHAIN CYCLE).
C------------------------------------------------------------
#include "vectorize.inc"
      DO 700 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 700
      I  =NLS-NDEB
C-----
      HOLD(1,4*(IL-1)+1)=XI(I)
      HOLD(2,4*(IL-1)+1)=YI(I)
      HOLD(3,4*(IL-1)+1)=ZI(I)
C-----
      NOLD(1,4*(IL-1)+1)=NXI(I)
      NOLD(2,4*(IL-1)+1)=NYI(I)
      NOLD(3,4*(IL-1)+1)=NZI(I)
 700  CONTINUE
C------------------------------------------------------------
C     MISE EN MEMOIRE (VECTEURS DE TRAVAIL -> ASSEMBLAGE).
C------------------------------------------------------------
C     non vectoriel.
      DO 600 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 600
      I  =NLS-NDEB
C-----
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
C-----
      WNS(IN1) =WNS(IN1)+IOLD(1,4*(IL-1)+1)*STF(I)
      WNS(IN2) =WNS(IN2)+IOLD(2,4*(IL-1)+1)*STF(I)
      WNS(IN3) =WNS(IN3)+IOLD(3,4*(IL-1)+1)*STF(I)
C-----
 600  CONTINUE
C------------------------------------------------------------
C     non vectoriel.
      DO 800 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 800
      I  =NLS-NDEB
C-----
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
C-----
      F1=FXN(I)
      FNORMX=FNORMX+F1
      F11=IOLD(1,4*(IL-1)+1)*F1
      F12=IOLD(2,4*(IL-1)+1)*F1
      F13=IOLD(3,4*(IL-1)+1)*F1
      WNF(1,IN1)=WNF(1,IN1)+F11
      WNF(1,IN2)=WNF(1,IN2)+F12
      WNF(1,IN3)=WNF(1,IN3)+F13
C-----
      F1=FYN(I)
      FNORMY=FNORMY+F1
      F11=IOLD(1,4*(IL-1)+1)*F1
      F12=IOLD(2,4*(IL-1)+1)*F1
      F13=IOLD(3,4*(IL-1)+1)*F1
      WNF(2,IN1)=WNF(2,IN1)+F11
      WNF(2,IN2)=WNF(2,IN2)+F12
      WNF(2,IN3)=WNF(2,IN3)+F13
C-----
      F1=FZN(I)
      FNORMZ=FNORMZ+F1
      F11=IOLD(1,4*(IL-1)+1)*F1
      F12=IOLD(2,4*(IL-1)+1)*F1
      F13=IOLD(3,4*(IL-1)+1)*F1
      WNF(3,IN1)=WNF(3,IN1)+F11
      WNF(3,IN2)=WNF(3,IN2)+F12
      WNF(3,IN3)=WNF(3,IN3)+F13
C-----
 800  CONTINUE
C------------------------------------------------------------
C     non vectoriel.
      DO 825 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (iactiv(1,IL)<=0) GOTO 825
      I  =NLS-NDEB
C-----
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
C-----
      F1=FXT(I)
      FTANGX=FTANGX+F1
      F11=IOLD(1,4*(IL-1)+1)*F1
      F12=IOLD(2,4*(IL-1)+1)*F1
      F13=IOLD(3,4*(IL-1)+1)*F1
      WTF(1,IN1)=WTF(1,IN1)+F11
      WTF(1,IN2)=WTF(1,IN2)+F12
      WTF(1,IN3)=WTF(1,IN3)+F13
C-----
      F1=FYT(I)
      FTANGY=FTANGY+F1
      F11=IOLD(1,4*(IL-1)+1)*F1
      F12=IOLD(2,4*(IL-1)+1)*F1
      F13=IOLD(3,4*(IL-1)+1)*F1
      WTF(2,IN1)=WTF(2,IN1)+F11
      WTF(2,IN2)=WTF(2,IN2)+F12
      WTF(2,IN3)=WTF(2,IN3)+F13
C-----
      F1=FZT(I)
      FTANGZ=FTANGZ+F1
      F11=IOLD(1,4*(IL-1)+1)*F1
      F12=IOLD(2,4*(IL-1)+1)*F1
      F13=IOLD(3,4*(IL-1)+1)*F1
      WTF(3,IN1)=WTF(3,IN1)+F11
      WTF(3,IN2)=WTF(3,IN2)+F12
      WTF(3,IN3)=WTF(3,IN3)+F13
C-----
 825  CONTINUE
C------------------------------------------------------------
C     KINEMATIC TIME STEP.
C------------------------------------------------------------
      DTI=EP20
      DO 900 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 900
      I  =NLS-NDEB
      DVX=VX1(I)-VXH(I)
      DVY=VY1(I)-VYH(I)
      DVZ=VZ1(I)-VZH(I)
      PN=DVX*NXI(I)+DVY*NYI(I)+DVZ*NZI(I)
      IF (PN<ZERO) THEN
        H  =XP1(1,I)*NXI(I)+XP1(2,I)*NYI(I)+XP1(3,I)*NZI(I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
      ENDIF
      DVX=VX2(I)-VXH(I)
      DVY=VY2(I)-VYH(I)
      DVZ=VZ2(I)-VZH(I)
      PN=DVX*NXI(I)+DVY*NYI(I)+DVZ*NZI(I)
      IF (PN<ZERO) THEN
        H  =XP2(1,I)*NXI(I)+XP2(2,I)*NYI(I)+XP2(3,I)*NZI(I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
      ENDIF
      DVX=VX3(I)-VXH(I)
      DVY=VY3(I)-VYH(I)
      DVZ=VZ3(I)-VZH(I)
      PN=DVX*NXI(I)+DVY*NYI(I)+DVZ*NZI(I)
      IF (PN<ZERO) THEN
        H  =XP3(1,I)*NXI(I)+XP3(2,I)*NYI(I)+XP3(3,I)*NZI(I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
      ENDIF
      IF(DTI<DT2T)THEN
        DT2T    = DTI
        NELTST  = NOINT
        ITYPTST = 10
      ENDIF
C-----
 900  CONTINUE
C----------------------------------
      IF (DTI<=0.) THEN
      DTI=EP20
      DO 950 NLS=NDEB+1,MIN(NDEB+MVSIZ,NTC)
      IL =KTC(NLS)
      IF (IACTIV(1,IL)<=0) GOTO 950
      I  =NLS-NDEB
      DVX=VX1(I)-VXH(I)
      DVY=VY1(I)-VYH(I)
      DVZ=VZ1(I)-VZH(I)
      PN=DVX*NXI(I)+DVY*NYI(I)+DVZ*NZI(I)
      IF (PN<ZERO) THEN
        H  =XP1(1,I)*NXI(I)+XP1(2,I)*NYI(I)+XP1(3,I)*NZI(I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
      ENDIF
      DVX=VX2(I)-VXH(I)
      DVY=VY2(I)-VYH(I)
      DVZ=VZ2(I)-VZH(I)
      PN=DVX*NXI(I)+DVY*NYI(I)+DVZ*NZI(I)
      IF (PN<ZERO) THEN
        H  =XP2(1,I)*NXI(I)+XP2(2,I)*NYI(I)+XP2(3,I)*NZI(I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
      ENDIF
      DVX=VX3(I)-VXH(I)
      DVY=VY3(I)-VYH(I)
      DVZ=VZ3(I)-VZH(I)
      PN=DVX*NXI(I)+DVY*NYI(I)+DVZ*NZI(I)
      IF (PN<ZERO) THEN
        H  =XP3(1,I)*NXI(I)+XP3(2,I)*NYI(I)+XP3(3,I)*NZI(I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
      ENDIF
      IF(DTI<=ZERO)THEN
         IN1=KSI(1,IL)
         IN2=KSI(2,IL)
         IN3=KSI(3,IL)
#include "lockon.inc"
         WRITE(ISTDO,'(A,E12.4,A,I8)')
     .   ' **WARNING MINIMUM TIME STEP ',DTI,' IN INTERFACE ',NOINT
         WRITE(IOUT ,'(A,E12.4,A,I8)')
     .   ' **WARNING MINIMUM TIME STEP ',DTI,' IN INTERFACE ',NOINT
         WRITE(IOUT,'(A,3I8)') '   ELEMENT/SEGMENT NODES :',
     .                   IN1,IN2,IN3
#include "lockoff.inc"
         TSTOP = TT
      ENDIF
C-----
 950  CONTINUE
      ENDIF
C----------------------------------
      RETURN
      END
