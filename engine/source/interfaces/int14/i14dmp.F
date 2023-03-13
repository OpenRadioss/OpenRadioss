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
Chd|  I14DMP                        source/interfaces/int14/i14dmp.F
Chd|-- called by -----------
Chd|        I14CMP                        source/interfaces/int14/i14cmp.F
Chd|-- calls ---------------
Chd|        NINTERP                       source/interfaces/int14/ninterp.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I14DMP(X      ,V      ,KSURF  ,IGRSURF,BUFSF ,
     2                  NSC    ,KSC    ,NSP    ,KSP    ,KSI   ,
     3                  IMPACT ,CIMP   ,NIMP   ,VISC  ,NDAMP1 ,
     4                  NDAMP2 ,GAPMIN ,NPC    ,PLD   ,MS     ,
     5                  WF    ,WST    ,STF    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSC, NSP, KSURF,KSI(*),
     .        IMPACT(*), NDAMP1, NDAMP2, NPC(*)
C     REAL
      my_real 
     .  X(3,*)  ,V(3,*)    , BUFSF(*), KSC(*), KSP(*),
     .  CIMP(3,*) ,NIMP(3,*) , VISC  , GAPMIN, PLD(*),
     .  MS(*)     ,WF(*)     , WST(*), STF
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IL, IN
      INTEGER NDEB, NREST
      INTEGER DGR
      INTEGER IPT,NPT,II,JJ
      my_real
     .   A, B, C, AN, BN, CN, ROT(9),
     .   X1, X2, X3, N1, N2, N3, N,
     .   XPVN1, YPVN1, ZPVN1, SGNXN, SGNYN, SGNZN,
     .   XM, YM, ZM, XI, YI, ZI,
     .   V1, V2, V3, VXM, VYM, VZM, VRX, VRY, VRZ,
     .   VXP, VYP, VZP,VXI, VYI, VZI,
     .   DT1INV, FF
      my_real
     .   XPV(3,MVSIZ),NV(3,MVSIZ),FNPV(MVSIZ),VIS(MVSIZ),
     .   VISC1(MVSIZ),VISC2(MVSIZ),VN(MVSIZ)
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
C-----------------------------------------------
      A =BUFSF(ADRBUF+1)
      B =BUFSF(ADRBUF+2)
      C =BUFSF(ADRBUF+3)
      DGR=BUFSF(ADRBUF+36)
      AN=A**DGR
      BN=B**DGR
      CN=C**DGR
      AN=ONE/AN
      BN=ONE/BN
      CN=ONE/CN      
      DO I=1,9
       ROT(I)=BUFSF(ADRBUF+7+I-1)
      END DO
C-----------------------------------------------
C     Noeud main dans le repere de l'ellipsoide :
C     pour calcul de l'amortissement.
C     la position (el les vitesses) du noeud main sont transmises 
C     par l'objet qui utilise la surface (Rigid Body)
C     - dans le Starter au Temps TT=0.
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
C-------------------------------
C     POINTS JUSTE IMPACTES.
C-------------------------------
      NDEB =0
      NREST=NSC
 100  CONTINUE
C-------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSC(I+NDEB)
      IN=KSI(IL)
      FNPV(I) =WF(IN)
      NV(1,I) =NIMP(1,IL)
      NV(2,I) =NIMP(2,IL)
      NV(3,I) =NIMP(3,IL)
      ENDDO
C------------------------------------------------
C     Coefficient d'amortissement :
C     F(VITESSE NORMALE) * G(FORCE NORMALE LOCALE)
C-----------------------------------------------
      IF (NDAMP1==0 .AND. NDAMP2==0) THEN
C---------------------------------
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSC(I+NDEB)
        IN=KSI(IL)
        VIS(I) = VISC*2.*SQRT(STF*MS(IN))
       ENDDO
      ELSEIF (NDAMP1==0) THEN
C---------------------------------
       CALL NINTERP(NDAMP2,NPC,PLD,MIN(MVSIZ,NREST),FNPV,VISC2)
       DO I=1,MIN(MVSIZ,NREST)
        VIS(I)=VISC*VISC2(I)
       ENDDO
      ELSEIF (NDAMP2==0) THEN
C---------------------------------
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSC(I+NDEB)
        IN=KSI(IL)
        V1 =V(1,IN)
        V2 =V(2,IN)
        V3 =V(3,IN)
        VN(I) =V(1,IN)*NV(1,I)+V(2,IN)*NV(2,I)+V(3,IN)*NV(3,I)
       ENDDO
       CALL NINTERP(NDAMP1,NPC,PLD,MIN(MVSIZ,NREST),VN,VISC1)
       DO I=1,MIN(MVSIZ,NREST)
        VIS(I)=VISC*VISC1(I)
       ENDDO
      ELSE
C---------------------------------
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSC(I+NDEB)
        IN=KSI(IL)
        V1 =V(1,IN)
        V2 =V(2,IN)
        V3 =V(3,IN)
        VN(I) =V(1,IN)*NV(1,I)+V(2,IN)*NV(2,I)+V(3,IN)*NV(3,I)
       ENDDO
       CALL NINTERP(NDAMP1,NPC,PLD,MIN(MVSIZ,NREST),VN  ,VISC1)
       CALL NINTERP(NDAMP2,NPC,PLD,MIN(MVSIZ,NREST),FNPV,VISC2)
       DO I=1,MIN(MVSIZ,NREST)
        VIS(I)=VISC*VISC1(I)*VISC2(I)
       ENDDO
      ENDIF
C------------------------------------------------
C     Amortissement :
C-----------------------------------------------
#include "vectorize.inc"
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSC(I+NDEB)
      IN=KSI(IL)
C-------------------------------
      IF (IMPACT(IL)>0) THEN
        XI=CIMP(1,IL)+GAPMIN*NIMP(1,IL)
        YI=CIMP(2,IL)+GAPMIN*NIMP(2,IL)
        ZI=CIMP(3,IL)+GAPMIN*NIMP(3,IL)
C-------------------------------
        V1 =V(1,IN)
        V2 =V(2,IN)
        V3 =V(3,IN)
        VXP=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
        VYP=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
        VZP=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
C
        VXI=VXM+(YM-YI)*VRZ-(ZM-ZI)*VRY
        VYI=VYM-(XM-XI)*VRZ+(ZM-ZI)*VRX
        VZI=VZM+(XM-XI)*VRY-(YM-YI)*VRX
C-------------------------------
        FF =-VIS(I)*(NV(1,I)*(VXP-VXI)+NV(2,I)*(VYP-VYI)
     .              +NV(3,I)*(VZP-VZI))
C-------------------------------
        WF(IN)   = FNPV(I)+FF
        WST(IN)  = VIS(I)
      ELSE
        WST(IN)=ZERO
      END IF
      ENDDO
C---------------------------------
      IF (NREST-MVSIZ>0) THEN
        NREST=NREST-MVSIZ
        NDEB =NDEB +MVSIZ
        GOTO 100
      ENDIF
C-------------------------------
C     POINTS PRECEDEMMENT IMPACTES.
C-------------------------------
      NDEB =0
      NREST=NSP
 200  CONTINUE
C-------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
      FNPV(I) =WF(IN)
      NV(1,I) =NIMP(1,IL)
      NV(2,I) =NIMP(2,IL)
      NV(3,I) =NIMP(3,IL)
      ENDDO
C------------------------------------------------
C     Coefficient d'amortissement :
C     F(VITESSE NORMALE) * G(FORCE NORMALE LOCALE)
C-----------------------------------------------
      IF (NDAMP1==0 .AND. NDAMP2==0) THEN
C---------------------------------
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSP(I+NDEB)
        IN=KSI(IL)
        VIS(I) = VISC*TWO*SQRT(STF*MS(IN))
       ENDDO
      ELSEIF (NDAMP1==0) THEN
C---------------------------------
       CALL NINTERP(NDAMP2,NPC,PLD,MIN(MVSIZ,NREST),FNPV,VISC2)
       DO I=1,MIN(MVSIZ,NREST)
        VIS(I)=VISC*VISC2(I)
       ENDDO
      ELSEIF (NDAMP2==0) THEN
C---------------------------------
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSP(I+NDEB)
        IN=KSI(IL)
        V1 =V(1,IN)
        V2 =V(2,IN)
        V3 =V(3,IN)
        VN(I) =V(1,IN)*NV(1,I)+V(2,IN)*NV(2,I)+V(3,IN)*NV(3,I)
       ENDDO
       CALL NINTERP(NDAMP1,NPC,PLD,MIN(MVSIZ,NREST),VN,VISC1)
       DO I=1,MIN(MVSIZ,NREST)
        VIS(I)=VISC*VISC1(I)
       ENDDO
      ELSE
C---------------------------------
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSP(I+NDEB)
        IN=KSI(IL)
        V1 =V(1,IN)
        V2 =V(2,IN)
        V3 =V(3,IN)
        VN(I) =V(1,IN)*NV(1,I)+V(2,IN)*NV(2,I)+V(3,IN)*NV(3,I)
       ENDDO
       CALL NINTERP(NDAMP1,NPC,PLD,MIN(MVSIZ,NREST),VN  ,VISC1)
       CALL NINTERP(NDAMP2,NPC,PLD,MIN(MVSIZ,NREST),FNPV,VISC2)
       DO I=1,MIN(MVSIZ,NREST)
        VIS(I)=VISC*VISC1(I)*VISC2(I)
       ENDDO
      ENDIF
C------------------------------------------------
C     Amortissement :
C-----------------------------------------------
#include "vectorize.inc"
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
C-------------------------------
        XI=CIMP(1,IL)+GAPMIN*NIMP(1,IL)
        YI=CIMP(2,IL)+GAPMIN*NIMP(2,IL)
        ZI=CIMP(3,IL)+GAPMIN*NIMP(3,IL)
C-------------------------------
        V1 =V(1,IN)
        V2 =V(2,IN)
        V3 =V(3,IN)
        VXP=ROT(1)*V1+ROT(2)*V2+ROT(3)*V3
        VYP=ROT(4)*V1+ROT(5)*V2+ROT(6)*V3
        VZP=ROT(7)*V1+ROT(8)*V2+ROT(9)*V3
C
        VXI=VXM+(YM-YI)*VRZ-(ZM-ZI)*VRY
        VYI=VYM-(XM-XI)*VRZ+(ZM-ZI)*VRX
        VZI=VZM+(XM-XI)*VRY-(YM-YI)*VRX
C-------------------------------
        FF =-VIS(I)*(NV(1,I)*(VXP-VXI)+NV(2,I)*(VYP-VYI)
     .              +NV(3,I)*(VZP-VZI))
C-------------------------------
        WF(IN)   = FNPV(I)+FF
        WST(IN)  = VIS(I)
      ENDDO
C---------------------------------
      IF (NREST-MVSIZ>0) THEN
        NREST=NREST-MVSIZ
        NDEB =NDEB +MVSIZ
        GOTO 200
      ENDIF
C------------------------------------------------------------
      RETURN
      END
