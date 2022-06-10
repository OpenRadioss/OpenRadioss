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
Chd|  I15FOR1                       source/interfaces/int15/i15for1.F
Chd|-- called by -----------
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I15FOR1(NDEB, NSC, STFAC,X     ,V    ,
     2                  KSURF ,IGRSURF ,BUFSF ,KSC   ,KSI  ,
     3                  IACTIV ,IOLD  ,HOLD  ,NOLD  ,DOLD ,
     4                  XP1   ,XP2   ,XP3   ,XP4   ,GX   ,
     5                  XTK   ,YTK   ,ZTK   ,NTX   ,NTY  ,
     6                  NTZ   ,PENET ,DEPTH ,XI    ,YI   ,
     7                  ZI    ,NXI   ,NYI   ,NZI   ,MS   ,
     8                  DE    ,NPC   ,PLD   ,WNF   ,WTF  ,
     9                  WNS   ,FNORMX,FNORMY,FNORMZ,FTANGX,
     A                  FTANGY,FTANGZ,DT2T ,NOINT  ,NELTST ,
     B                  ITYPTST ,VFRIC )
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
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDEB, NSC
      INTEGER KSURF,
     .        KSI(4,*),IACTIV(4,*),NPC(*),KSC(*),
     .        NOINT,NELTST,ITYPTST
C     REAL
      my_real STFAC, BUFSF(*),
     .  X(3,*) ,  IOLD(3,*), HOLD(3,*), NOLD(3,*) ,DOLD(3,*),
     .  MS(*)  ,     V(3,*),        DE,     PLD(*), XTK(4,*),
     .  YTK(4,*)  ,ZTK(4,*) ,NTX(4,*)   ,NTY(4,*)  ,NTZ(4,*) ,
     .  PENET(4,*),DEPTH(4,*),
     .  XI(4,*)  ,YI(4,*)  ,ZI(4,*)   ,NXI(4,*) ,
     .  NYI(4,*) ,NZI(4,*) ,WNF(3,*)  ,WTF(3,*)  , WNS(*),
     .  XP1(3,*) ,XP2(3,*) ,XP3(3,*)  ,XP4(3,*)  ,GX(3,*),
     .  FNORMX,FNORMY,FNORMZ,FTANGX,FTANGY,FTANGZ,
     .  DT2T,VFRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IL, IN1, IN2, IN3, IN4, NLS
      my_real
     .   ROT(9), X1, X2, X3, XM, YM, ZM,
     .   V1, V2, V3, VXM, VYM, VZM, VRX, VRY, VRZ,
     .   V1X2, V2X1, V1X3, V3X1, V2X3, V3X2,
     .   DELTX, DELTY, DELTZ, ND, PN, SCALE, NV,
     .   VXK, VYK, VZK, DVX, DVY, DVZ, XH, YH, ZH,
     .   DX1, DY1, DZ1, DX2, DY2, DZ2, DX3, DY3, DZ3,
     .   DX, DY, DZ,
     .   S1, S2, S3, S,
     .   F1, F2, F3, F4, F1G, F11, F12, F2G, F22, F23, F3G, F33, F34,
     .   F4G, F44, F41, FG,
     .   ST1, ST2, ST3, ST4, ST1G, ST11, ST12, ST2G, ST22, ST23, ST3G,
     .   ST33, ST34, ST4G, ST44, ST41, STG,
     .   FTX, FTY, FTZ, FNX, FNY, FNZ, FTN, FMAX,FN,
     .   H, DTI
      my_real
     .   FXN(4,MVSIZ),FYN(4,MVSIZ),FZN(4,MVSIZ),STF(4,MVSIZ),
     .   FXT(4,MVSIZ),FYT(4,MVSIZ),FZT(4,MVSIZ),
     .   L1(MVSIZ),L2(MVSIZ),L3(MVSIZ),
     .   VXG(MVSIZ),VYG(MVSIZ),VZG(MVSIZ),
     .   VX1(MVSIZ),VY1(MVSIZ),VZ1(MVSIZ),
     .   VX2(MVSIZ),VY2(MVSIZ),VZ2(MVSIZ),
     .   VX3(MVSIZ),VY3(MVSIZ),VZ3(MVSIZ),
     .   VX4(MVSIZ),VY4(MVSIZ),VZ4(MVSIZ),
     .   VXH(4,MVSIZ),VYH(4,MVSIZ),VZH(4,MVSIZ)
C-----------------------------------------------
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
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
Clm   non optimise (plusieurs fois / noeud).
C----------------------------------------------------------
      DO 100 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
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
C------
       IN4=KSI(4,IL)
       V1=V(1,IN4)
       V2=V(2,IN4)
       V3=V(3,IN4)
       VX4(I)=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
       VY4(I)=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
       VZ4(I)=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
C------
       VXG(I)=FOURTH*(VX1(I)+VX2(I)+VX3(I)+VX4(I))
       VYG(I)=FOURTH*(VY1(I)+VY2(I)+VY3(I)+VY4(I))
       VZG(I)=FOURTH*(VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I))      
 100  CONTINUE     
C----------------------------------------------------------
C     1ER TRIANGLE / Force normale.
C----------------------------------------------------------
      DO 200 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(1,IL).LE.0) GOTO 200
      I  =NLS-NDEB
C       STF(1,I)=STFAC*DEPTH(1,I)/MAX(DEPTH(1,I)-PENET(1,I),EM20)
      STF(1,I)=STFAC*DEPTH(1,I)**2/MAX((DEPTH(1,I)-PENET(1,I))**2,EM20)
      FXN(1,I)=STF(1,I)*PENET(1,I)*NXI(1,I)
      FYN(1,I)=STF(1,I)*PENET(1,I)*NYI(1,I)
      FZN(1,I)=STF(1,I)*PENET(1,I)*NZI(1,I)
 200  CONTINUE
C-------------------------------
C     1ER TRIANGLE / Extraction du pt de contact au cycle precedent.
C-------------------------------
      DO 250 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(1,IL).LE.0) GOTO 250
      I  =NLS-NDEB
C     Point sur le triangle tq D(P,L) etait maximum au cycle precedent.
      L1(I)=IOLD(1,4*(IL-1)+1)
      L2(I)=IOLD(2,4*(IL-1)+1)
      L3(I)=IOLD(3,4*(IL-1)+1)
 250  CONTINUE
C----------------------------------------------------------
C     1ER TRIANGLE / Frottement :
C----------------------------------------------------------
#include "vectorize.inc"
      DO 275 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(1,IL).LE.0) GOTO 275
      I  =NLS-NDEB
C------------------------------
      FXT(1,I)=ZERO
      FYT(1,I)=ZERO
      FZT(1,I)=ZERO      
C-------------------------------
C     TOURNER DELTA(T-1) DANS LE PLAN TANGENT.
C-------------------------------
      IF (IACTIV(1,IL).GT.2) THEN
       DELTX=DOLD(1,4*(IL-1)+1)
       DELTY=DOLD(2,4*(IL-1)+1)
       DELTZ=DOLD(3,4*(IL-1)+1)
       ND =SQRT(DELTX*DELTX+DELTY*DELTY+DELTZ*DELTZ)
       IF (ND.NE.ZERO) THEN
        PN=DELTX*NXI(1,I)+DELTY*NYI(1,I)+DELTZ*NZI(1,I)
        DELTX=DELTX-PN*NXI(1,I)
        DELTY=DELTY-PN*NYI(1,I)
        DELTZ=DELTZ-PN*NZI(1,I)
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
      VXH(1,I)=VXM+V1
      VYH(1,I)=VYM+V2
      VZH(1,I)=VZM+V3
      IF (IACTIV(1,IL).GE.2) THEN
       VXK=L1(I)*VXG(I)+L2(I)*VX1(I)+L3(I)*VX2(I)
       VYK=L1(I)*VYG(I)+L2(I)*VY1(I)+L3(I)*VY2(I)
       VZK=L1(I)*VZG(I)+L2(I)*VZ1(I)+L3(I)*VZ2(I)
       DVX=VXH(1,I)-VXK
       DVY=VYH(1,I)-VYK
       DVZ=VZH(1,I)-VZK
       PN=DVX*NXI(1,I)+DVY*NYI(1,I)+DVZ*NZI(1,I)
       DVX=DVX-PN*NXI(1,I)
       DVY=DVY-PN*NYI(1,I)
       DVZ=DVZ-PN*NZI(1,I)
      ELSE
       DVX=ZERO
       DVY=ZERO
       DVZ=ZERO    
      ENDIF
C-----
C     Force tangente K.DELTA
      DOLD(1,4*(IL-1)+1)=DELTX+DVX*DT1
      DOLD(2,4*(IL-1)+1)=DELTY+DVY*DT1
      DOLD(3,4*(IL-1)+1)=DELTZ+DVZ*DT1
      FXT(1,I)=STF(1,I)*DOLD(1,4*(IL-1)+1)
      FYT(1,I)=STF(1,I)*DOLD(2,4*(IL-1)+1)
      FZT(1,I)=STF(1,I)*DOLD(3,4*(IL-1)+1)
C-----
 275  CONTINUE
C-------------------------------
C     2EME TRIANGLE /  Force normale.
C-------------------------------
      DO 300 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(2,IL).LE.0) GOTO 300
      I  =NLS-NDEB
C       STF(2,I)=STFAC*DEPTH(2,I)/MAX(DEPTH(2,I)-PENET(2,I),EM20)
      STF(2,I)=STFAC*DEPTH(2,I)**2/MAX((DEPTH(2,I)-PENET(2,I))**2,EM20)
      FXN(2,I)=STF(2,I)*PENET(2,I)*NXI(2,I)
      FYN(2,I)=STF(2,I)*PENET(2,I)*NYI(2,I)
      FZN(2,I)=STF(2,I)*PENET(2,I)*NZI(2,I)
 300  CONTINUE
C-------------------------------
C     2EME TRIANGLE / Extraction du pt de contact au cycle precedent.
C-------------------------------
      DO 350 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(2,IL).LE.0) GOTO 350
      I  =NLS-NDEB
C     Point sur le triangle tq D(P,L) etait maximum au cycle precedent.
      L1(I)=IOLD(1,4*(IL-1)+2)
      L2(I)=IOLD(2,4*(IL-1)+2)
      L3(I)=IOLD(3,4*(IL-1)+2)
 350  CONTINUE
C----------------------------------------------------------
C     2EME TRIANGLE / Frottement :
C----------------------------------------------------------
#include "vectorize.inc"
      DO 375 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(2,IL).LE.0) GOTO 375
      I  =NLS-NDEB
      FXT(2,I)=ZERO
      FYT(2,I)=ZERO
      FZT(2,I)=ZERO    
C-------------------------------
C     TOURNER DELTA(T-1) DANS LE PLAN TANGENT.
C-------------------------------
      IF (IACTIV(2,IL).GT.2) THEN
       DELTX=DOLD(1,4*(IL-1)+2)
       DELTY=DOLD(2,4*(IL-1)+2)
       DELTZ=DOLD(3,4*(IL-1)+2)
       ND =SQRT(DELTX*DELTX+DELTY*DELTY+DELTZ*DELTZ)
       IF (ND.NE.0.) THEN
        PN=DELTX*NXI(2,I)+DELTY*NYI(2,I)+DELTZ*NZI(2,I)
        DELTX=DELTX-PN*NXI(2,I)
        DELTY=DELTY-PN*NYI(2,I)
        DELTZ=DELTZ-PN*NZI(2,I)
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
      XH=HOLD(1,4*(IL-1)+2)
      YH=HOLD(2,4*(IL-1)+2)
      ZH=HOLD(3,4*(IL-1)+2)
      V1X2=VRX*(YH-YM)
      V2X1=VRY*(XH-XM)
      V2X3=VRY*(ZH-ZM)
      V3X2=VRZ*(YH-YM)
      V3X1=VRZ*(XH-XM)
      V1X3=VRX*(ZH-ZM)
      V3 =V1X2-V2X1
      V1 =V2X3-V3X2
      V2 =V3X1-V1X3
      VXH(2,I)=VXM+V1
      VYH(2,I)=VYM+V2
      VZH(2,I)=VZM+V3
      IF (IACTIV(2,IL).GE.2) THEN
       VXK=L1(I)*VXG(I)+L2(I)*VX2(I)+L3(I)*VX3(I)
       VYK=L1(I)*VYG(I)+L2(I)*VY2(I)+L3(I)*VY3(I)
       VZK=L1(I)*VZG(I)+L2(I)*VZ2(I)+L3(I)*VZ3(I)
       DVX=VXH(2,I)-VXK
       DVY=VYH(2,I)-VYK
       DVZ=VZH(2,I)-VZK
       PN=DVX*NXI(2,I)+DVY*NYI(2,I)+DVZ*NZI(2,I)
       DVX=DVX-PN*NXI(2,I)
       DVY=DVY-PN*NYI(2,I)
       DVZ=DVZ-PN*NZI(2,I)
      ELSE
       DVX=ZERO
       DVY=ZERO
       DVZ=ZERO     
      ENDIF
C-----
C     Force tangente K.DELTA, K=1. et Norme.
      DOLD(1,4*(IL-1)+2)=DELTX+DVX*DT1
      DOLD(2,4*(IL-1)+2)=DELTY+DVY*DT1
      DOLD(3,4*(IL-1)+2)=DELTZ+DVZ*DT1
      FXT(2,I)=STF(2,I)*DOLD(1,4*(IL-1)+2)
      FYT(2,I)=STF(2,I)*DOLD(2,4*(IL-1)+2)
      FZT(2,I)=STF(2,I)*DOLD(3,4*(IL-1)+2)
C-----
 375  CONTINUE
C-------------------------------
C     3EME TRIANGLE /  Force normale.
C-------------------------------
      DO 400 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(3,IL).LE.0) GOTO 400
      I  =NLS-NDEB
C       STF(3,I)=STFAC*DEPTH(3,I)/MAX(DEPTH(3,I)-PENET(3,I),EM20)
      STF(3,I)=STFAC*DEPTH(3,I)**2/MAX((DEPTH(3,I)-PENET(3,I))**2,EM20)
      FXN(3,I)=STF(3,I)*PENET(3,I)*NXI(3,I)
      FYN(3,I)=STF(3,I)*PENET(3,I)*NYI(3,I)
      FZN(3,I)=STF(3,I)*PENET(3,I)*NZI(3,I)
 400  CONTINUE
C-------------------------------
C     3EME TRIANGLE / Extraction du pt de contact au cycle precedent.
C-------------------------------
      DO 450 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(3,IL).LE.0) GOTO 450
      I  =NLS-NDEB
C     Point sur le triangle tq D(P,L) etait maximum au cycle precedent.
      L1(I)=IOLD(1,4*(IL-1)+3)
      L2(I)=IOLD(2,4*(IL-1)+3)
      L3(I)=IOLD(3,4*(IL-1)+3)
 450  CONTINUE
C----------------------------------------------------------
C     3EME TRIANGLE / Frottement :
C----------------------------------------------------------
#include "vectorize.inc"
      DO 475 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(3,IL).LE.0) GOTO 475
      I  =NLS-NDEB
C------------------------------
      FXT(3,I)=ZERO
      FYT(3,I)=ZERO
      FZT(3,I)=ZERO    
C-------------------------------
C     TOURNER DELTA(T-1) DANS LE PLAN TANGENT.
C-------------------------------
      IF (IACTIV(3,IL).GT.2) THEN
       DELTX=DOLD(1,4*(IL-1)+3)
       DELTY=DOLD(2,4*(IL-1)+3)
       DELTZ=DOLD(3,4*(IL-1)+3)
       ND =SQRT(DELTX*DELTX+DELTY*DELTY+DELTZ*DELTZ)
       IF (ND.NE.ZERO) THEN
        PN=DELTX*NXI(3,I)+DELTY*NYI(3,I)+DELTZ*NZI(3,I)
        DELTX=DELTX-PN*NXI(3,I)
        DELTY=DELTY-PN*NYI(3,I)
        DELTZ=DELTZ-PN*NZI(3,I)
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
      XH=HOLD(1,4*(IL-1)+3)
      YH=HOLD(2,4*(IL-1)+3)
      ZH=HOLD(3,4*(IL-1)+3)
      V1X2=VRX*(YH-YM)
      V2X1=VRY*(XH-XM)
      V2X3=VRY*(ZH-ZM)
      V3X2=VRZ*(YH-YM)
      V3X1=VRZ*(XH-XM)
      V1X3=VRX*(ZH-ZM)
      V3 =V1X2-V2X1
      V1 =V2X3-V3X2
      V2 =V3X1-V1X3
      VXH(3,I)=VXM+V1
      VYH(3,I)=VYM+V2
      VZH(3,I)=VZM+V3
      IF (IACTIV(3,IL).GE.2) THEN
       VXK=L1(I)*VXG(I)+L2(I)*VX3(I)+L3(I)*VX4(I)
       VYK=L1(I)*VYG(I)+L2(I)*VY3(I)+L3(I)*VY4(I)
       VZK=L1(I)*VZG(I)+L2(I)*VZ3(I)+L3(I)*VZ4(I)
       DVX=VXH(3,I)-VXK
       DVY=VYH(3,I)-VYK
       DVZ=VZH(3,I)-VZK
       PN=DVX*NXI(3,I)+DVY*NYI(3,I)+DVZ*NZI(3,I)
       DVX=DVX-PN*NXI(3,I)
       DVY=DVY-PN*NYI(3,I)
       DVZ=DVZ-PN*NZI(3,I)
      ELSE
       DVX=ZERO
       DVY=ZERO
       DVZ=ZERO     
      ENDIF
C-----
C     Force tangente K.DELTA, K=1. et Norme.
      DOLD(1,4*(IL-1)+3)=DELTX+DVX*DT1
      DOLD(2,4*(IL-1)+3)=DELTY+DVY*DT1
      DOLD(3,4*(IL-1)+3)=DELTZ+DVZ*DT1
      FXT(3,I)=STF(3,I)*DOLD(1,4*(IL-1)+3)
      FYT(3,I)=STF(3,I)*DOLD(2,4*(IL-1)+3)
      FZT(3,I)=STF(3,I)*DOLD(3,4*(IL-1)+3)
C-----
 475  CONTINUE
C-------------------------------
C     4EME TRIANGLE / Force normale.
C-------------------------------
      DO 500 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(4,IL).LE.0) GOTO 500
      I  =NLS-NDEB
C       STF(4,I)=STFAC*DEPTH(4,I)/MAX(DEPTH(4,I)-PENET(4,I),EM20)
      STF(4,I)=STFAC*DEPTH(4,I)**2/MAX((DEPTH(4,I)-PENET(4,I))**2,EM20)
      FXN(4,I)=STF(4,I)*PENET(4,I)*NXI(4,I)
      FYN(4,I)=STF(4,I)*PENET(4,I)*NYI(4,I)
      FZN(4,I)=STF(4,I)*PENET(4,I)*NZI(4,I)
 500  CONTINUE
C-------------------------------
C     4EME TRIANGLE / Extraction du pt de contact au cycle precedent.
C-------------------------------
      DO 550 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(4,IL).LE.0) GOTO 550
      I  =NLS-NDEB
C     Point sur le triangle tq D(P,L) etait maximum au cycle precedent.
      L1(I)=IOLD(1,4*(IL-1)+4)
      L2(I)=IOLD(2,4*(IL-1)+4)
      L3(I)=IOLD(3,4*(IL-1)+4)
 550  CONTINUE
C----------------------------------------------------------
C     4EME TRIANGLE / Frottement :
C----------------------------------------------------------
#include "vectorize.inc"
      DO 575 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(4,IL).LE.0) GOTO 575
      I  =NLS-NDEB
C------------------------------
      FXT(4,I)=ZERO
      FYT(4,I)=ZERO
      FZT(4,I)=ZERO    
C-------------------------------
C     TOURNER DELTA(T-1) DANS LE PLAN TANGENT.
C-------------------------------
      IF (IACTIV(4,IL).GT.2) THEN
       DELTX=DOLD(1,4*(IL-1)+4)
       DELTY=DOLD(2,4*(IL-1)+4)
       DELTZ=DOLD(3,4*(IL-1)+4)
       ND =SQRT(DELTX*DELTX+DELTY*DELTY+DELTZ*DELTZ)
       IF (ND.NE.ZERO) THEN
        PN=DELTX*NXI(4,I)+DELTY*NYI(4,I)+DELTZ*NZI(4,I)
        DELTX=DELTX-PN*NXI(4,I)
        DELTY=DELTY-PN*NYI(4,I)
        DELTZ=DELTZ-PN*NZI(4,I)
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
      XH=HOLD(1,4*(IL-1)+4)
      YH=HOLD(2,4*(IL-1)+4)
      ZH=HOLD(3,4*(IL-1)+4)
      V1X2=VRX*(YH-YM)
      V2X1=VRY*(XH-XM)
      V2X3=VRY*(ZH-ZM)
      V3X2=VRZ*(YH-YM)
      V3X1=VRZ*(XH-XM)
      V1X3=VRX*(ZH-ZM)
      V3 =V1X2-V2X1
      V1 =V2X3-V3X2
      V2 =V3X1-V1X3
      VXH(4,I)=VXM+V1
      VYH(4,I)=VYM+V2
      VZH(4,I)=VZM+V3
      IF (IACTIV(4,IL).GE.2) THEN
       VXK=L1(I)*VXG(I)+L2(I)*VX4(I)+L3(I)*VX1(I)
       VYK=L1(I)*VYG(I)+L2(I)*VY4(I)+L3(I)*VY1(I)
       VZK=L1(I)*VZG(I)+L2(I)*VZ4(I)+L3(I)*VZ1(I)
       DVX=VXH(4,I)-VXK
       DVY=VYH(4,I)-VYK
       DVZ=VZH(4,I)-VZK
       PN=DVX*NXI(4,I)+DVY*NYI(4,I)+DVZ*NZI(4,I)
       DVX=DVX-PN*NXI(4,I)
       DVY=DVY-PN*NYI(4,I)
       DVZ=DVZ-PN*NZI(4,I)
      ELSE
       DVX=ZERO
       DVY=ZERO
       DVZ=ZERO    
      ENDIF
C-----
C     Force tangente K.DELTA, K=1. et Norme.
      DOLD(1,4*(IL-1)+4)=DELTX+DVX*DT1
      DOLD(2,4*(IL-1)+4)=DELTY+DVY*DT1
      DOLD(3,4*(IL-1)+4)=DELTZ+DVZ*DT1
      FXT(4,I)=STF(4,I)*DOLD(1,4*(IL-1)+4)
      FYT(4,I)=STF(4,I)*DOLD(2,4*(IL-1)+4)
      FZT(4,I)=STF(4,I)*DOLD(3,4*(IL-1)+4)
C-----
 575  CONTINUE
C------------------------------------------------------------
C     FRICTION (LOCAL COULOMB FRICTION).
C------------------------------------------------------------
C-----------------------------------------------
C     1ER TRIANGLE.
C-----------------------------------------------
#include "vectorize.inc"
      DO 580 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(1,IL).LE.0) GOTO 580
      I  =NLS-NDEB
C-----
      FTX=FXT(1,I)
      FTY=FYT(1,I)
      FTZ=FZT(1,I)
      FNX=FXN(1,I)
      FNY=FYN(1,I)
      FNZ=FZN(1,I)
      FTN=SQRT(FTX*FTX+FTY*FTY+FTZ*FTZ)
      FN =SQRT(FNX*FNX+FNY*FNY+FNZ*FNZ)
      FMAX= MAX(VFRIC*FN,ZERO)
      IF (FTN.GT.FMAX) THEN
        SCALE =FMAX/FTN
        FTX=SCALE*FTX
        FTY=SCALE*FTY
        FTZ=SCALE*FTZ
      ELSE
        SCALE=ONE
      ENDIF
      IF (IACTIV(1,IL).GT.1) THEN
C      Glissement DELTA=scale.DELTA
       DELTX=SCALE*DOLD(1,4*(IL-1)+1)
       DELTY=SCALE*DOLD(2,4*(IL-1)+1)
       DELTZ=SCALE*DOLD(3,4*(IL-1)+1)
C      MISE EN MEMOIRE BUFFERS D'INTERFACE (-> PROCHAIN CYCLE).
       DOLD(1,4*(IL-1)+1)=DELTX
       DOLD(2,4*(IL-1)+1)=DELTY
       DOLD(3,4*(IL-1)+1)=DELTZ
      ENDIF
      FXT(1,I)=FTX
      FYT(1,I)=FTY
      FZT(1,I)=FTZ
 580  CONTINUE
C-----------------------------------------------
C     2EME TRIANGLE.
C-----------------------------------------------
#include "vectorize.inc"
      DO 585 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(2,IL).LE.0) GOTO 585
      I  =NLS-NDEB
C-----
      FTX=FXT(2,I)
      FTY=FYT(2,I)
      FTZ=FZT(2,I)
      FNX=FXN(2,I)
      FNY=FYN(2,I)
      FNZ=FZN(2,I)
      FTN=SQRT(FTX*FTX+FTY*FTY+FTZ*FTZ)
      FN =SQRT(FNX*FNX+FNY*FNY+FNZ*FNZ)
      FMAX= MAX(VFRIC*FN,ZERO)
      IF (FTN.GT.FMAX) THEN
        SCALE =FMAX/FTN
        FTX=SCALE*FTX
        FTY=SCALE*FTY
        FTZ=SCALE*FTZ
      ELSE
        SCALE=ONE
      ENDIF
      IF (IACTIV(2,IL).GT.1) THEN
C      Glissement DELTA=scale.DELTA
       DELTX=SCALE*DOLD(1,4*(IL-1)+2)
       DELTY=SCALE*DOLD(2,4*(IL-1)+2)
       DELTZ=SCALE*DOLD(3,4*(IL-1)+2)
C      MISE EN MEMOIRE BUFFERS D'INTERFACE (-> PROCHAIN CYCLE).
       DOLD(1,4*(IL-1)+2)=DELTX
       DOLD(2,4*(IL-1)+2)=DELTY
       DOLD(3,4*(IL-1)+2)=DELTZ
      ENDIF
      FXT(2,I)=FTX
      FYT(2,I)=FTY
      FZT(2,I)=FTZ
 585  CONTINUE
C-----------------------------------------------
C     3EME TRIANGLE.
C-----------------------------------------------
#include "vectorize.inc"
      DO 590 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(3,IL).LE.0) GOTO 590
      I  =NLS-NDEB
C-----
      FTX=FXT(3,I)
      FTY=FYT(3,I)
      FTZ=FZT(3,I)
      FNX=FXN(3,I)
      FNY=FYN(3,I)
      FNZ=FZN(3,I)
      FTN=SQRT(FTX*FTX+FTY*FTY+FTZ*FTZ)
      FN =SQRT(FNX*FNX+FNY*FNY+FNZ*FNZ)
      FMAX= MAX(VFRIC*FN,ZERO)
      IF (FTN.GT.FMAX) THEN
        SCALE =FMAX/FTN
        FTX=SCALE*FTX
        FTY=SCALE*FTY
        FTZ=SCALE*FTZ
      ELSE
        SCALE=ONE
      ENDIF
      IF (IACTIV(3,IL).GT.1) THEN
C      Glissement DELTA=scale.DELTA
       DELTX=SCALE*DOLD(1,4*(IL-1)+3)
       DELTY=SCALE*DOLD(2,4*(IL-1)+3)
       DELTZ=SCALE*DOLD(3,4*(IL-1)+3)
C      MISE EN MEMOIRE BUFFERS D'INTERFACE (-> PROCHAIN CYCLE).
       DOLD(1,4*(IL-1)+3)=DELTX
       DOLD(2,4*(IL-1)+3)=DELTY
       DOLD(3,4*(IL-1)+3)=DELTZ
      ENDIF
      FXT(3,I)=FTX
      FYT(3,I)=FTY
      FZT(3,I)=FTZ
 590  CONTINUE
C-----------------------------------------------
C     4EME TRIANGLE.
C-----------------------------------------------
#include "vectorize.inc"
      DO 595 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(4,IL).LE.0) GOTO 595
      I  =NLS-NDEB
C-----
      FTX=FXT(4,I)
      FTY=FYT(4,I)
      FTZ=FZT(4,I)
      FNX=FXN(4,I)
      FNY=FYN(4,I)
      FNZ=FZN(4,I)
      FTN=SQRT(FTX*FTX+FTY*FTY+FTZ*FTZ)
      FN =SQRT(FNX*FNX+FNY*FNY+FNZ*FNZ)
      FMAX= MAX(VFRIC*FN,ZERO)
      IF (FTN.GT.FMAX) THEN
        SCALE =FMAX/FTN
        FTX=SCALE*FTX
        FTY=SCALE*FTY
        FTZ=SCALE*FTZ
      ELSE
        SCALE=ONE
      ENDIF
      IF (IACTIV(4,IL).GT.1) THEN
C      Glissement DELTA=scale.DELTA
       DELTX=SCALE*DOLD(1,4*(IL-1)+4)
       DELTY=SCALE*DOLD(2,4*(IL-1)+4)
       DELTZ=SCALE*DOLD(3,4*(IL-1)+4)
C      MISE EN MEMOIRE BUFFERS D'INTERFACE (-> PROCHAIN CYCLE).
       DOLD(1,4*(IL-1)+4)=DELTX
       DOLD(2,4*(IL-1)+4)=DELTY
       DOLD(3,4*(IL-1)+4)=DELTZ
      ENDIF
      FXT(4,I)=FTX
      FYT(4,I)=FTY
      FZT(4,I)=FTZ
 595  CONTINUE
C------------------------------------------------------------
C     LOCAL COORDINATES OF CONTACT POINT WITH RESPECT TO ELEMENT.
C------------------------------------------------------------
C#include "vectorize.inc"
      DO 650 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(1,IL).LE.0) GOTO 650
      I  =NLS-NDEB
C-----
      DX2=XP1(1,I)-XTK(1,I)
      DY2=XP1(2,I)-YTK(1,I)
      DZ2=XP1(3,I)-ZTK(1,I)
      DX3=XP2(1,I)-XTK(1,I)
      DY3=XP2(2,I)-YTK(1,I)
      DZ3=XP2(3,I)-ZTK(1,I)
      DX=DY2*DZ3-DZ2*DY3
      DY=DX3*DZ2-DZ3*DX2
      DZ=DX2*DY3-DY2*DX3
      S1=HALF*SQRT(DX*DX+DY*DY+DZ*DZ)
C-----
      DX1=GX(1,I) -XTK(1,I)
      DY1=GX(2,I) -YTK(1,I)
      DZ1=GX(3,I) -ZTK(1,I)
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
C#include "vectorize.inc"
      DO 660 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(2,IL).LE.0) GOTO 660
      I  =NLS-NDEB
C-----
      DX2=XP2(1,I)-XTK(2,I)
      DY2=XP2(2,I)-YTK(2,I)
      DZ2=XP2(3,I)-ZTK(2,I)
      DX3=XP3(1,I)-XTK(2,I)
      DY3=XP3(2,I)-YTK(2,I)
      DZ3=XP3(3,I)-ZTK(2,I)
      DX=DY2*DZ3-DZ2*DY3
      DY=DX3*DZ2-DZ3*DX2
      DZ=DX2*DY3-DY2*DX3
      S1=HALF*SQRT(DX*DX+DY*DY+DZ*DZ)
C-----
      DX1=GX(1,I) -XTK(2,I)
      DY1=GX(2,I) -YTK(2,I)
      DZ1=GX(3,I) -ZTK(2,I)
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
      IOLD(1,4*(IL-1)+2)=S1*S
      IOLD(2,4*(IL-1)+2)=S2*S
      IOLD(3,4*(IL-1)+2)=S3*S
 660  CONTINUE
C------------------------------------------------------------
C#include "vectorize.inc"
      DO 670 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(3,IL).LE.0) GOTO 670
      I  =NLS-NDEB
C-----
      DX2=XP3(1,I)-XTK(3,I)
      DY2=XP3(2,I)-YTK(3,I)
      DZ2=XP3(3,I)-ZTK(3,I)
      DX3=XP4(1,I)-XTK(3,I)
      DY3=XP4(2,I)-YTK(3,I)
      DZ3=XP4(3,I)-ZTK(3,I)
      DX=DY2*DZ3-DZ2*DY3
      DY=DX3*DZ2-DZ3*DX2
      DZ=DX2*DY3-DY2*DX3
      S1=HALF*SQRT(DX*DX+DY*DY+DZ*DZ)
C-----
      DX1=GX(1,I) -XTK(3,I)
      DY1=GX(2,I) -YTK(3,I)
      DZ1=GX(3,I) -ZTK(3,I)
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
      IOLD(1,4*(IL-1)+3)=S1*S
      IOLD(2,4*(IL-1)+3)=S2*S
      IOLD(3,4*(IL-1)+3)=S3*S
 670  CONTINUE
C------------------------------------------------------------
C#include "vectorize.inc"
      DO 680 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      IF (IACTIV(4,IL).LE.0) GOTO 680
      I  =NLS-NDEB
C-----
      DX2=XP4(1,I)-XTK(4,I)
      DY2=XP4(2,I)-YTK(4,I)
      DZ2=XP4(3,I)-ZTK(4,I)
      DX3=XP1(1,I)-XTK(4,I)
      DY3=XP1(2,I)-YTK(4,I)
      DZ3=XP1(3,I)-ZTK(4,I)
      DX=DY2*DZ3-DZ2*DY3
      DY=DX3*DZ2-DZ3*DX2
      DZ=DX2*DY3-DY2*DX3
      S1=HALF*SQRT(DX*DX+DY*DY+DZ*DZ)
C-----
      DX1=GX(1,I) -XTK(4,I)
      DY1=GX(2,I) -YTK(4,I)
      DZ1=GX(3,I) -ZTK(4,I)
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
      IOLD(1,4*(IL-1)+4)=S1*S
      IOLD(2,4*(IL-1)+4)=S2*S
      IOLD(3,4*(IL-1)+4)=S3*S
 680  CONTINUE
C------------------------------------------------------------
C     MISE EN MEMOIRE BUFFERS D'INTERFACE (-> PROCHAIN CYCLE).
C------------------------------------------------------------
#include "vectorize.inc"
      DO 700 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       HOLD(1,4*(IL-1)+1)=XI(1,I)
       HOLD(2,4*(IL-1)+1)=YI(1,I)
       HOLD(3,4*(IL-1)+1)=ZI(1,I)
       NOLD(1,4*(IL-1)+1)=NXI(1,I)
       NOLD(2,4*(IL-1)+1)=NYI(1,I)
       NOLD(3,4*(IL-1)+1)=NZI(1,I)
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       HOLD(1,4*(IL-1)+2)=XI(2,I)
       HOLD(2,4*(IL-1)+2)=YI(2,I)
       HOLD(3,4*(IL-1)+2)=ZI(2,I)
       NOLD(1,4*(IL-1)+2)=NXI(2,I)
       NOLD(2,4*(IL-1)+2)=NYI(2,I)
       NOLD(3,4*(IL-1)+2)=NZI(2,I)
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       HOLD(1,4*(IL-1)+3)=XI(3,I)
       HOLD(2,4*(IL-1)+3)=YI(3,I)
       HOLD(3,4*(IL-1)+3)=ZI(3,I)
       NOLD(1,4*(IL-1)+3)=NXI(3,I)
       NOLD(2,4*(IL-1)+3)=NYI(3,I)
       NOLD(3,4*(IL-1)+3)=NZI(3,I)
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       HOLD(1,4*(IL-1)+4)=XI(4,I)
       HOLD(2,4*(IL-1)+4)=YI(4,I)
       HOLD(3,4*(IL-1)+4)=ZI(4,I)
       NOLD(1,4*(IL-1)+4)=NXI(4,I)
       NOLD(2,4*(IL-1)+4)=NYI(4,I)
       NOLD(3,4*(IL-1)+4)=NZI(4,I)
      ENDIF
 700  CONTINUE
C------------------------------------------------------------
C     MISE EN MEMOIRE (VECTEURS DE TRAVAIL -> ASSEMBLAGE).
C------------------------------------------------------------
Clm   non vectoriel.
      DO 600 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
      IN4=KSI(4,IL)
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       ST1=STF(1,I)
      ELSE
       ST1=ZERO
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       ST2=STF(2,I)
      ELSE
       ST2=ZERO
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       ST3=STF(3,I)
      ELSE
       ST3=ZERO
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       ST4=STF(4,I)
      ELSE
       ST4=ZERO
      ENDIF
      ST1G=IOLD(1,4*(IL-1)+1)*ST1
      ST11=IOLD(2,4*(IL-1)+1)*ST1
      ST12=IOLD(3,4*(IL-1)+1)*ST1
      ST2G=IOLD(1,4*(IL-1)+2)*ST2
      ST22=IOLD(2,4*(IL-1)+2)*ST2
      ST23=IOLD(3,4*(IL-1)+2)*ST2
      ST3G=IOLD(1,4*(IL-1)+3)*ST3
      ST33=IOLD(2,4*(IL-1)+3)*ST3
      ST34=IOLD(3,4*(IL-1)+3)*ST3
      ST4G=IOLD(1,4*(IL-1)+4)*ST4
      ST44=IOLD(2,4*(IL-1)+4)*ST4
      ST41=IOLD(3,4*(IL-1)+4)*ST4
      STG =FOURTH*(ST1G+ST2G+ST3G+ST4G)
      WNS(IN1)=WNS(IN1)+ST11+ST41+STG
      WNS(IN2)=WNS(IN2)+ST12+ST22+STG
      WNS(IN3)=WNS(IN3)+ST23+ST33+STG
      WNS(IN4)=WNS(IN4)+ST34+ST44+STG
C-----
 600  CONTINUE
C------------------------------------------------------------
Clm   non vectoriel.
      DO 800 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
      IN4=KSI(4,IL)
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       F1=FXN(1,I)
      ELSE
       F1=ZERO
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       F2=FXN(2,I)
      ELSE
       F2=ZERO
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       F3=FXN(3,I)
      ELSE
       F3=ZERO
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       F4=FXN(4,I)
      ELSE
       F4=ZERO
      ENDIF
C-----
      FNORMX=FNORMX+F1+F2+F3+F4
C-----
      F1G=IOLD(1,4*(IL-1)+1)*F1
      F11=IOLD(2,4*(IL-1)+1)*F1
      F12=IOLD(3,4*(IL-1)+1)*F1
      F2G=IOLD(1,4*(IL-1)+2)*F2
      F22=IOLD(2,4*(IL-1)+2)*F2
      F23=IOLD(3,4*(IL-1)+2)*F2
      F3G=IOLD(1,4*(IL-1)+3)*F3
      F33=IOLD(2,4*(IL-1)+3)*F3
      F34=IOLD(3,4*(IL-1)+3)*F3
      F4G=IOLD(1,4*(IL-1)+4)*F4
      F44=IOLD(2,4*(IL-1)+4)*F4
      F41=IOLD(3,4*(IL-1)+4)*F4
      FG =FOURTH*(F1G+F2G+F3G+F4G)
      WNF(1,IN1)=WNF(1,IN1)+F11+F41+FG
      WNF(1,IN2)=WNF(1,IN2)+F12+F22+FG
      WNF(1,IN3)=WNF(1,IN3)+F23+F33+FG
      WNF(1,IN4)=WNF(1,IN4)+F34+F44+FG
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       F1=FYN(1,I)
      ELSE
       F1=ZERO
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       F2=FYN(2,I)
      ELSE
       F2=ZERO
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       F3=FYN(3,I)
      ELSE
       F3=ZERO
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       F4=FYN(4,I)
      ELSE
       F4=ZERO
      ENDIF
C-----
      FNORMY=FNORMY+F1+F2+F3+F4
C-----
      F1G=IOLD(1,4*(IL-1)+1)*F1
      F11=IOLD(2,4*(IL-1)+1)*F1
      F12=IOLD(3,4*(IL-1)+1)*F1
      F2G=IOLD(1,4*(IL-1)+2)*F2
      F22=IOLD(2,4*(IL-1)+2)*F2
      F23=IOLD(3,4*(IL-1)+2)*F2
      F3G=IOLD(1,4*(IL-1)+3)*F3
      F33=IOLD(2,4*(IL-1)+3)*F3
      F34=IOLD(3,4*(IL-1)+3)*F3
      F4G=IOLD(1,4*(IL-1)+4)*F4
      F44=IOLD(2,4*(IL-1)+4)*F4
      F41=IOLD(3,4*(IL-1)+4)*F4
      FG =FOURTH*(F1G+F2G+F3G+F4G)
      WNF(2,IN1)=WNF(2,IN1)+F11+F41+FG
      WNF(2,IN2)=WNF(2,IN2)+F12+F22+FG
      WNF(2,IN3)=WNF(2,IN3)+F23+F33+FG
      WNF(2,IN4)=WNF(2,IN4)+F34+F44+FG
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       F1=FZN(1,I)
      ELSE
       F1=ZERO
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       F2=FZN(2,I)
      ELSE
       F2=ZERO
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       F3=FZN(3,I)
      ELSE
       F3=ZERO
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       F4=FZN(4,I)
      ELSE
       F4=ZERO
      ENDIF
C-----
      FNORMZ=FNORMZ+F1+F2+F3+F4
C-----
      F1G=IOLD(1,4*(IL-1)+1)*F1
      F11=IOLD(2,4*(IL-1)+1)*F1
      F12=IOLD(3,4*(IL-1)+1)*F1
      F2G=IOLD(1,4*(IL-1)+2)*F2
      F22=IOLD(2,4*(IL-1)+2)*F2
      F23=IOLD(3,4*(IL-1)+2)*F2
      F3G=IOLD(1,4*(IL-1)+3)*F3
      F33=IOLD(2,4*(IL-1)+3)*F3
      F34=IOLD(3,4*(IL-1)+3)*F3
      F4G=IOLD(1,4*(IL-1)+4)*F4
      F44=IOLD(2,4*(IL-1)+4)*F4
      F41=IOLD(3,4*(IL-1)+4)*F4
      FG =FOURTH*(F1G+F2G+F3G+F4G)
      WNF(3,IN1)=WNF(3,IN1)+F11+F41+FG
      WNF(3,IN2)=WNF(3,IN2)+F12+F22+FG
      WNF(3,IN3)=WNF(3,IN3)+F23+F33+FG
      WNF(3,IN4)=WNF(3,IN4)+F34+F44+FG
C-----
 800  CONTINUE
C------------------------------------------------------------
Clm   non vectoriel.
      DO 825 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
C-----
      IN1=KSI(1,IL)
      IN2=KSI(2,IL)
      IN3=KSI(3,IL)
      IN4=KSI(4,IL)
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       F1=FXT(1,I)
      ELSE
       F1=ZERO
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       F2=FXT(2,I)
      ELSE
       F2=ZERO
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       F3=FXT(3,I)
      ELSE
       F3=ZERO
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       F4=FXT(4,I)
      ELSE
       F4=ZERO
      ENDIF
C-----
      FTANGX=FTANGX+F1+F2+F3+F4
C-----
      F1G=IOLD(1,4*(IL-1)+1)*F1
      F11=IOLD(2,4*(IL-1)+1)*F1
      F12=IOLD(3,4*(IL-1)+1)*F1
      F2G=IOLD(1,4*(IL-1)+2)*F2
      F22=IOLD(2,4*(IL-1)+2)*F2
      F23=IOLD(3,4*(IL-1)+2)*F2
      F3G=IOLD(1,4*(IL-1)+3)*F3
      F33=IOLD(2,4*(IL-1)+3)*F3
      F34=IOLD(3,4*(IL-1)+3)*F3
      F4G=IOLD(1,4*(IL-1)+4)*F4
      F44=IOLD(2,4*(IL-1)+4)*F4
      F41=IOLD(3,4*(IL-1)+4)*F4
      FG =FOURTH*(F1G+F2G+F3G+F4G)
      WTF(1,IN1)=WTF(1,IN1)+F11+F41+FG
      WTF(1,IN2)=WTF(1,IN2)+F12+F22+FG
      WTF(1,IN3)=WTF(1,IN3)+F23+F33+FG
      WTF(1,IN4)=WTF(1,IN4)+F34+F44+FG
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       F1=FYT(1,I)
      ELSE
       F1=ZERO
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       F2=FYT(2,I)
      ELSE
       F2=ZERO
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       F3=FYT(3,I)
      ELSE
       F3=ZERO
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       F4=FYT(4,I)
      ELSE
       F4=ZERO
      ENDIF
C-----
      FTANGY=FTANGY+F1+F2+F3+F4
C-----
      F1G=IOLD(1,4*(IL-1)+1)*F1
      F11=IOLD(2,4*(IL-1)+1)*F1
      F12=IOLD(3,4*(IL-1)+1)*F1
      F2G=IOLD(1,4*(IL-1)+2)*F2
      F22=IOLD(2,4*(IL-1)+2)*F2
      F23=IOLD(3,4*(IL-1)+2)*F2
      F3G=IOLD(1,4*(IL-1)+3)*F3
      F33=IOLD(2,4*(IL-1)+3)*F3
      F34=IOLD(3,4*(IL-1)+3)*F3
      F4G=IOLD(1,4*(IL-1)+4)*F4
      F44=IOLD(2,4*(IL-1)+4)*F4
      F41=IOLD(3,4*(IL-1)+4)*F4
      FG =FOURTH*(F1G+F2G+F3G+F4G)
      WTF(2,IN1)=WTF(2,IN1)+F11+F41+FG
      WTF(2,IN2)=WTF(2,IN2)+F12+F22+FG
      WTF(2,IN3)=WTF(2,IN3)+F23+F33+FG
      WTF(2,IN4)=WTF(2,IN4)+F34+F44+FG
C-----
      IF (IACTIV(1,IL).GT.0) THEN
       F1=FZT(1,I)
      ELSE
       F1=ZERO
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       F2=FZT(2,I)
      ELSE
       F2=ZERO
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       F3=FZT(3,I)
      ELSE
       F3=ZERO
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       F4=FZT(4,I)
      ELSE
       F4=ZERO
      ENDIF
C-----
      FTANGZ=FTANGZ+F1+F2+F3+F4
C-----
      F1G=IOLD(1,4*(IL-1)+1)*F1
      F11=IOLD(2,4*(IL-1)+1)*F1
      F12=IOLD(3,4*(IL-1)+1)*F1
      F2G=IOLD(1,4*(IL-1)+2)*F2
      F22=IOLD(2,4*(IL-1)+2)*F2
      F23=IOLD(3,4*(IL-1)+2)*F2
      F3G=IOLD(1,4*(IL-1)+3)*F3
      F33=IOLD(2,4*(IL-1)+3)*F3
      F34=IOLD(3,4*(IL-1)+3)*F3
      F4G=IOLD(1,4*(IL-1)+4)*F4
      F44=IOLD(2,4*(IL-1)+4)*F4
      F41=IOLD(3,4*(IL-1)+4)*F4
      FG =FOURTH*(F1G+F2G+F3G+F4G)
      WTF(3,IN1)=WTF(3,IN1)+F11+F41+FG
      WTF(3,IN2)=WTF(3,IN2)+F12+F22+FG
      WTF(3,IN3)=WTF(3,IN3)+F23+F33+FG
      WTF(3,IN4)=WTF(3,IN4)+F34+F44+FG
C-----
 825  CONTINUE
C------------------------------------------------------------
C     KINEMATIC TIME STEP.
C------------------------------------------------------------
      DTI=EP20
      DO 900 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      IF (IACTIV(1,IL).GT.0) THEN
       DVX=VX1(I)-VXH(1,I)
       DVY=VY1(I)-VYH(1,I)
       DVZ=VZ1(I)-VZH(1,I)
       PN=DVX*NXI(1,I)+DVY*NYI(1,I)+DVZ*NZI(1,I)
       IF (PN.LT.ZERO) THEN
        H  =XP1(1,I)*NXI(1,I)+XP1(2,I)*NYI(1,I)+XP1(3,I)*NZI(1,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX2(I)-VXH(1,I)
       DVY=VY2(I)-VYH(1,I)
       DVZ=VZ2(I)-VZH(1,I)
       PN=DVX*NXI(1,I)+DVY*NYI(1,I)+DVZ*NZI(1,I)
       IF (PN.LT.ZERO) THEN
        H  =XP2(1,I)*NXI(1,I)+XP2(2,I)*NYI(1,I)+XP2(3,I)*NZI(1,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(1,I)
       DVY=VYG(I)-VYH(1,I)
       DVZ=VZG(I)-VZH(1,I)
       PN=DVX*NXI(1,I)+DVY*NYI(1,I)+DVZ*NZI(1,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(1,I)+GX(2,I)*NYI(1,I)+GX(3,I)*NZI(1,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       DVX=VX2(I)-VXH(2,I)
       DVY=VY2(I)-VYH(2,I)
       DVZ=VZ2(I)-VZH(2,I)
       PN=DVX*NXI(2,I)+DVY*NYI(2,I)+DVZ*NZI(2,I)
       IF (PN.LT.ZERO) THEN
        H  =XP2(1,I)*NXI(2,I)+XP2(2,I)*NYI(2,I)+XP2(3,I)*NZI(2,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX3(I)-VXH(2,I)
       DVY=VY3(I)-VYH(2,I)
       DVZ=VZ3(I)-VZH(2,I)
       PN=DVX*NXI(2,I)+DVY*NYI(2,I)+DVZ*NZI(2,I)
       IF (PN.LT.ZERO) THEN
        H  =XP3(1,I)*NXI(2,I)+XP3(2,I)*NYI(2,I)+XP3(3,I)*NZI(2,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(2,I)
       DVY=VYG(I)-VYH(2,I)
       DVZ=VZG(I)-VZH(2,I)
       PN=DVX*NXI(2,I)+DVY*NYI(2,I)+DVZ*NZI(2,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(2,I)+GX(2,I)*NYI(2,I)+GX(3,I)*NZI(2,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       DVX=VX3(I)-VXH(3,I)
       DVY=VY3(I)-VYH(3,I)
       DVZ=VZ3(I)-VZH(3,I)
       PN=DVX*NXI(3,I)+DVY*NYI(3,I)+DVZ*NZI(3,I)
       IF (PN.LT.ZERO) THEN
        H  =XP3(1,I)*NXI(3,I)+XP3(2,I)*NYI(3,I)+XP3(3,I)*NZI(3,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX4(I)-VXH(3,I)
       DVY=VY4(I)-VYH(3,I)
       DVZ=VZ4(I)-VZH(3,I)
       PN=DVX*NXI(3,I)+DVY*NYI(3,I)+DVZ*NZI(3,I)
       IF (PN.LT.ZERO) THEN
        H  =XP4(1,I)*NXI(3,I)+XP4(2,I)*NYI(3,I)+XP4(3,I)*NZI(3,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(3,I)
       DVY=VYG(I)-VYH(3,I)
       DVZ=VZG(I)-VZH(3,I)
       PN=DVX*NXI(3,I)+DVY*NYI(3,I)+DVZ*NZI(3,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(3,I)+GX(2,I)*NYI(3,I)+GX(3,I)*NZI(3,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       DVX=VX1(I)-VXH(4,I)
       DVY=VY1(I)-VYH(4,I)
       DVZ=VZ1(I)-VZH(4,I)
       PN=DVX*NXI(4,I)+DVY*NYI(4,I)+DVZ*NZI(4,I)
       IF (PN.LT.ZERO) THEN
        H  =XP1(1,I)*NXI(4,I)+XP1(2,I)*NYI(4,I)+XP1(3,I)*NZI(4,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX4(I)-VXH(4,I)
       DVY=VY4(I)-VYH(4,I)
       DVZ=VZ4(I)-VZH(4,I)
       PN=DVX*NXI(4,I)+DVY*NYI(4,I)+DVZ*NZI(4,I)
       IF (PN.LT.ZERO) THEN
        H  =XP4(1,I)*NXI(4,I)+XP4(2,I)*NYI(4,I)+XP4(3,I)*NZI(4,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(4,I)
       DVY=VYG(I)-VYH(4,I)
       DVZ=VZG(I)-VZH(4,I)
       PN=DVX*NXI(4,I)+DVY*NYI(4,I)+DVZ*NZI(4,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(4,I)+GX(2,I)*NYI(4,I)+GX(3,I)*NZI(4,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF(DTI.LT.DT2T)THEN
        DT2T    = DTI
        NELTST  = NOINT
        ITYPTST = 10
      ENDIF
C-----
 900  CONTINUE
C----------------------------------
      IF (DTI.LE.ZERO) THEN
      DTI=EP20
      DO 950 NLS=NDEB+1,MIN(NDEB+MVSIZ,NSC)
      IL =KSC(NLS)
      I  =NLS-NDEB
      IF (IACTIV(1,IL).GT.0) THEN
       DVX=VX1(I)-VXH(1,I)
       DVY=VY1(I)-VYH(1,I)
       DVZ=VZ1(I)-VZH(1,I)
       PN=DVX*NXI(1,I)+DVY*NYI(1,I)+DVZ*NZI(1,I)
       IF (PN.LT.ZERO) THEN
        H  =XP1(1,I)*NXI(1,I)+XP1(2,I)*NYI(1,I)+XP1(3,I)*NZI(1,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX2(I)-VXH(1,I)
       DVY=VY2(I)-VYH(1,I)
       DVZ=VZ2(I)-VZH(1,I)
       PN=DVX*NXI(1,I)+DVY*NYI(1,I)+DVZ*NZI(1,I)
       IF (PN.LT.ZERO) THEN
        H  =XP2(1,I)*NXI(1,I)+XP2(2,I)*NYI(1,I)+XP2(3,I)*NZI(1,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(1,I)
       DVY=VYG(I)-VYH(1,I)
       DVZ=VZG(I)-VZH(1,I)
       PN=DVX*NXI(1,I)+DVY*NYI(1,I)+DVZ*NZI(1,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(1,I)+GX(2,I)*NYI(1,I)+GX(3,I)*NZI(1,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF (IACTIV(2,IL).GT.0) THEN
       DVX=VX2(I)-VXH(2,I)
       DVY=VY2(I)-VYH(2,I)
       DVZ=VZ2(I)-VZH(2,I)
       PN=DVX*NXI(2,I)+DVY*NYI(2,I)+DVZ*NZI(2,I)
       IF (PN.LT.ZERO) THEN
        H  =XP2(1,I)*NXI(2,I)+XP2(2,I)*NYI(2,I)+XP2(3,I)*NZI(2,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX3(I)-VXH(2,I)
       DVY=VY3(I)-VYH(2,I)
       DVZ=VZ3(I)-VZH(2,I)
       PN=DVX*NXI(2,I)+DVY*NYI(2,I)+DVZ*NZI(2,I)
       IF (PN.LT.ZERO) THEN
        H  =XP3(1,I)*NXI(2,I)+XP3(2,I)*NYI(2,I)+XP3(3,I)*NZI(2,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(2,I)
       DVY=VYG(I)-VYH(2,I)
       DVZ=VZG(I)-VZH(2,I)
       PN=DVX*NXI(2,I)+DVY*NYI(2,I)+DVZ*NZI(2,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(2,I)+GX(2,I)*NYI(2,I)+GX(3,I)*NZI(2,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF (IACTIV(3,IL).GT.0) THEN
       DVX=VX3(I)-VXH(3,I)
       DVY=VY3(I)-VYH(3,I)
       DVZ=VZ3(I)-VZH(3,I)
       PN=DVX*NXI(3,I)+DVY*NYI(3,I)+DVZ*NZI(3,I)
       IF (PN.LT.ZERO) THEN
        H  =XP3(1,I)*NXI(3,I)+XP3(2,I)*NYI(3,I)+XP3(3,I)*NZI(3,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX4(I)-VXH(3,I)
       DVY=VY4(I)-VYH(3,I)
       DVZ=VZ4(I)-VZH(3,I)
       PN=DVX*NXI(3,I)+DVY*NYI(3,I)+DVZ*NZI(3,I)
       IF (PN.LT.ZERO) THEN
        H  =XP4(1,I)*NXI(3,I)+XP4(2,I)*NYI(3,I)+XP4(3,I)*NZI(3,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(3,I)
       DVY=VYG(I)-VYH(3,I)
       DVZ=VZG(I)-VZH(3,I)
       PN=DVX*NXI(3,I)+DVY*NYI(3,I)+DVZ*NZI(3,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(3,I)+GX(2,I)*NYI(3,I)+GX(3,I)*NZI(3,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF (IACTIV(4,IL).GT.0) THEN
       DVX=VX1(I)-VXH(4,I)
       DVY=VY1(I)-VYH(4,I)
       DVZ=VZ1(I)-VZH(4,I)
       PN=DVX*NXI(4,I)+DVY*NYI(4,I)+DVZ*NZI(4,I)
       IF (PN.LT.ZERO) THEN
        H  =XP1(1,I)*NXI(4,I)+XP1(2,I)*NYI(4,I)+XP1(3,I)*NZI(4,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VX4(I)-VXH(4,I)
       DVY=VY4(I)-VYH(4,I)
       DVZ=VZ4(I)-VZH(4,I)
       PN=DVX*NXI(4,I)+DVY*NYI(4,I)+DVZ*NZI(4,I)
       IF (PN.LT.ZERO) THEN
        H  =XP4(1,I)*NXI(4,I)+XP4(2,I)*NYI(4,I)+XP4(3,I)*NZI(4,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
       DVX=VXG(I)-VXH(4,I)
       DVY=VYG(I)-VYH(4,I)
       DVZ=VZG(I)-VZH(4,I)
       PN=DVX*NXI(4,I)+DVY*NYI(4,I)+DVZ*NZI(4,I)
       IF (PN.LT.ZERO) THEN
        H  =GX(1,I)*NXI(4,I)+GX(2,I)*NYI(4,I)+GX(3,I)*NZI(4,I)
        DTI=MIN(DTI,HALF*H/MAX(EM20,-PN))
       ENDIF
      ENDIF
      IF(DTI.LE.ZERO)THEN
         IN1=KSI(1,IL)
         IN2=KSI(2,IL)
         IN3=KSI(3,IL)
         IN4=KSI(4,IL)
#include "lockon.inc"
         WRITE(ISTDO,'(A,E12.4,A,I8)')
     .   ' **WARNING MINIMUM TIME STEP ',DTI,' IN INTERFACE ',NOINT
         WRITE(IOUT ,'(A,E12.4,A,I8)')
     .   ' **WARNING MINIMUM TIME STEP ',DTI,' IN INTERFACE ',NOINT
         WRITE(IOUT,'(A,4I8)') '   ELEMENT/SEGMENT NODES :',
     .                   IN1,IN2,IN3,IN4
#include "lockoff.inc"
         TSTOP = TT
      ENDIF
C-----
 950  CONTINUE
      ENDIF
C----------------------------------
      RETURN
      END
