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
Chd|  I14ELA                        source/interfaces/int14/i14ela.F
Chd|-- called by -----------
Chd|        I14CMP                        source/interfaces/int14/i14cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I14ELA(X      ,KSURF  ,IGRSURF,BUFSF ,NSC    ,
     2                  KSC    ,NSP    ,KSP    ,KSI   ,IMPACT ,
     3                  CIMP   ,NIMP   ,STFAC  ,NLO   ,GAPMIN ,
     4                  NPC    ,PLD    ,WF     ,STF   )
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
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSC, NSP, KSURF,KSI(*),
     .        IMPACT(*), NLO, NPC(*)
C     REAL ou REAL*8
      my_real 
     .  BUFSF(*),KSC(*)    ,KSP(*)   ,STFAC , GAPMIN,
     .  X(3,*)  , CIMP(3,*),NIMP(3,*),PLD(*),WF(*)  ,
     .  STF
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
     .   EP, ANS, ANSMX, PENTE, FTOT,
     .   FNORMX, FNORMY, FNORMZ, NF
      my_real
     .   XPV(3,MVSIZ)
C-----------------------------------------------
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
      AN=UN/AN
      BN=UN/BN
      CN=UN/CN      
      DO I=1,9
       ROT(I)=BUFSF(ADRBUF+7+I-1)
      END DO
C-----------------------------------------------
      ANSMX=0.
C-------------------------------
C     POINTS JUSTE IMPACTES.
C-------------------------------
      NDEB =0
      NREST=NSC
  50  CONTINUE
C-------------------------------
C     Passage au repere local :
C-------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSC(I+NDEB)
      IN=KSI(IL)
      X1=X(1,IN)-BUFSF(ADRBUF+4)
      X2=X(2,IN)-BUFSF(ADRBUF+5)
      X3=X(3,IN)-BUFSF(ADRBUF+6)
      XPV(1,I)=ROT(1)*X1+ROT(2)*X2+ROT(3)*X3
      XPV(2,I)=ROT(4)*X1+ROT(5)*X2+ROT(6)*X3
      XPV(3,I)=ROT(7)*X1+ROT(8)*X2+ROT(9)*X3
      ENDDO
C-------------------------------
C     Normale et penetration.
C-------------------------------
#include "vectorize.inc"
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSC(I+NDEB)
      IN=KSI(IL)
C-------------------------------
      XPVN1 =XPV(1,I)**(DGR-1)
      SGNXN=-UN      
      IF (XPVN1*XPV(1,I).GE.ZERO) SGNXN=UN
      YPVN1 =XPV(2,I)**(DGR-1)
      SGNYN=-UN
      IF (YPVN1*XPV(2,I).GE.ZERO) SGNYN=UN
      ZPVN1 =XPV(3,I)**(DGR-1)
      SGNZN=-UN
      IF (ZPVN1*XPV(3,I).GE.ZERO) SGNZN=UN
C-------------------------------
      N1 =SGNXN*XPVN1*AN
      N2 =SGNYN*YPVN1*BN
      N3 =SGNZN*ZPVN1*CN
      N  =N1*N1+N2*N2+N3*N3
      N  =SQRT(N)
C-------------------------------
      EP=N1*XPV(1,I)+N2*XPV(2,I)+N3*XPV(3,I)
C-------------------------------
C     Detection exacte de l'impact.
C-------------------------------
C     Ellipsoide ::
C     ANS=(EP-SQRT(EP))/MAX(EM20,N)
C     Hyper-Ellipsoide ::
      ANS=(EP-EXP((DGR-1)*LOG(MAX(EP,EM20))/DGR))
     .      / MAX(EXP((DGR-1)*LOG(EM20)/DGR),N)
C-------------------------------
C     projection du point impacte :
Clm   projection sur tangente / hors ellips.
C-------------------------------
      IF (GAPMIN.LT.ANS) THEN
        IMPACT(IL)=0
      ELSE
        IMPACT(IL)=1
      END IF
      N1 =N1/MAX(EM20,N)
      N2 =N2/MAX(EM20,N)
      N3 =N3/MAX(EM20,N)
      CIMP(1,IL)=XPV(1,I)-ANS*N1
      CIMP(2,IL)=XPV(2,I)-ANS*N2
      CIMP(3,IL)=XPV(3,I)-ANS*N3
      NIMP(1,IL)=N1
      NIMP(2,IL)=N2
      NIMP(3,IL)=N3
C-------------------------------
      ANS=GAPMIN-ANS
      ANS=MAX(ZERO,ANS)
C-------------------------------
      IF (ANS.GT.ANSMX) ANSMX=ANS
C-------------------------------
      WF(IN)=ANS
      ENDDO
C---------------------------------
      IF (NREST-MVSIZ.GT.0) THEN
        NREST=NREST-MVSIZ
        NDEB =NDEB +MVSIZ
        GOTO 50
      ENDIF
C-------------------------------
C     POINTS PRECEDEMMENT IMPACTES.
C-------------------------------
      NDEB =0
      NREST=NSP
 100  CONTINUE
C-------------------------------
C     Penetration max :
C-------------------------------
#include "vectorize.inc"
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
C-------------------------------
C     penetration.
C-------------------------------
      ANS=GAPMIN-WF(IN)
C-------------------------------
C     IF (GAPMIN.LT.ANS) THEN
C       IMPACT(IL)=0
C     ELSE
C       IMPACT(IL)=1
C     END IF
C     ANS=MAX(ZERO,ANS)
      IF (ANS.GT.ANSMX) ANSMX=ANS
C-------------------------------
      WF(IN)=ANS
      ENDDO
C---------------------------------
      IF (NREST-MVSIZ.GT.0) THEN
        NREST=NREST-MVSIZ
        NDEB =NDEB +MVSIZ
        GOTO 100
      ENDIF
C-----------------------------------------------
C     FORCE ELASTIQUE TOTALE=FCT(PENETRATION MAX.).
C-----------------------------------------------
      IF (NLO.NE.0) THEN
        NPT = (NPC(NLO+1)-NPC(NLO))/2
        II  = NPC(NLO)
        IF (ANSMX.LE.PLD(II)) THEN
          PENTE=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
          FTOT =PLD(II+1)+PENTE*(ANSMX-PLD(II))
        ELSEIF (ANSMX.GE.PLD(II+2*(NPT-1))) THEN
          JJ=II+2*(NPT-1)
          PENTE=(PLD(JJ+1)-PLD(JJ-1))/(PLD(JJ)-PLD(JJ-2))
          FTOT =PLD(JJ+1)+MAX(-PLD(JJ+1),PENTE*(ANSMX-PLD(JJ)))
        ELSE
          DO IPT=1,NPT-1
           IF (PLD(II).LE.ANSMX
     .     .AND.ANSMX.LE.PLD(II+2)) THEN
            PENTE=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
            FTOT =PLD(II+1)+PENTE*(ANSMX-PLD(II))
            GOTO 200
           ENDIF
           II=II+2
          ENDDO
 200      CONTINUE
        ENDIF
      ENDIF
C-----------------------------------------------
C     RAIDEUR DE L'INTERFACE
C     TQ | SOMME DES FORCES | = F(PENETRATION MAX.)
C-----------------------------------------------
      IF (NLO.NE.0) THEN
       FNORMX=ZERO
       FNORMY=ZERO
       FNORMZ=ZERO     
       DO I=1,NSC
       IL=KSC(I)
       IN=KSI(IL)
       FNORMX=FNORMX+WF(IN)*NIMP(1,IL)
       FNORMY=FNORMY+WF(IN)*NIMP(2,IL)
       FNORMZ=FNORMZ+WF(IN)*NIMP(3,IL)
       ENDDO
C------
       DO I=1,NSP
       IL=KSP(I)
       IN=KSI(IL)
       FNORMX=FNORMX+WF(IN)*NIMP(1,IL)
       FNORMY=FNORMY+WF(IN)*NIMP(2,IL)
       FNORMZ=FNORMZ+WF(IN)*NIMP(3,IL)
       ENDDO
C------
       NF =SQRT(FNORMX*FNORMX+FNORMY*FNORMY+FNORMZ*FNORMZ)
       IF (NF.NE.ZERO) THEN
        STF=STFAC*FTOT/NF
       ELSE
        STF=ZERO
       ENDIF
      ELSE
       STF=STFAC
      ENDIF
C-----------------------------------------------
      DO I=1,NSC
      IL=KSC(I)
      IN=KSI(IL)
      WF(IN)=STF*WF(IN)
      ENDDO
C-----------------------------------------------
      DO I=1,NSP
      IL=KSP(I)
      IN=KSI(IL)
      WF(IN)=STF*WF(IN)
      ENDDO
C------------------------------------------------------------
      RETURN
      END
