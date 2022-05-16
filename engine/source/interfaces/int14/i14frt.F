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
Chd|  I14FRT                        source/interfaces/int14/i14frt.F
Chd|-- called by -----------
Chd|        I14CMP                        source/interfaces/int14/i14cmp.F
Chd|-- calls ---------------
Chd|        NINTERP                       source/interfaces/int14/ninterp.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I14FRT(AF    ,X     ,V     ,KSURF  ,IGRSURF,
     2                  BUFSF ,NSC   ,KSC   ,NSP    ,KSP    ,
     3                  KSI   ,IMPACT ,CIMP ,NIMP   ,FRIC   ,
     4                  NFRIC ,NPC    ,PLD  ,GAPMIN ,STF    ,
     5                  WF     ,WST   ,DE   ,MS     ,STIFN  , 
     6                  FS     ,FCONT  ,FSKYI ,ISKY ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
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
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSC,NSP, KSURF, KSI(*), IMPACT(*),
     .        NFRIC, NPC(*),ISKY(*)
C     REAL ou REAL*8
      my_real
     .  AF(*), X(3,*), V(3,*) , BUFSF(*), KSP(*),
     .  KSC(*), FRIC, CIMP(3,*),NIMP(3,*), MS(*),
     .  STIFN(*), FS(NTHVKI),FCONT(3,*),FSKYI(LSKYI,NFSKYI), PLD(*),
     .  WF(*), WST(*),GAPMIN ,  STF , DE
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IL, J3, J2, J1, IN, I3, I2, I1
      INTEGER NISKYL
      INTEGER NDEB, NREST
      INTEGER DGR
      INTEGER IPT,NPT,II,JJ
      my_real
     .   A, B, C, AN, BN, CN, ROT(9),
     .   X1, X2, X3, N1, N2, N3, N,
     .   XPVN1, YPVN1, ZPVN1, SGNXN, SGNYN, SGNZN,
     .   XI, YI, ZI,
     .   FXN, FYN, FZN, FXT, FYT, FZT, FTP, FTPA, FMAX, FN, TN,
     .   COST, DIST, PSCA, E, FX, FY, FZ,
     .   FN1, FN2, FN3, FT1, FT2, FT3, AM1, AM2, AM3,
     .   DD, DT1INV
      my_real
     .   XPV(3,MVSIZ),NV(3,MVSIZ) ,TV(3,MVSIZ),
     .   FNPV(MVSIZ) ,KFRIC(MVSIZ),XQ(3,MVSIZ),
     .   FV(3,MVSIZ) ,ST(MVSIZ)
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
      AN=1./AN
      BN=1./BN
      CN=1./CN
      DO I=1,9
       ROT(I)=BUFSF(ADRBUF+7+I-1)
      END DO
C-----------------------------------------------
      IF (DT1.EQ.ZERO) THEN
        DT1INV=ZERO
      ELSE
        DT1INV=UN/DT1
      END IF
C-----------------------------------------------
      IF (IPARIT.NE.0) THEN
#include "lockon.inc"
        NISKYL = NISKY
        NISKY  = NISKY + NSC + NSP
#include "lockoff.inc"
      END IF
C-----------------------------------------------
      DE=ZERO
C-----------------------------------------------
C     POINTS JUSTE IMPACTES.
C-----------------------------------------------
      NDEB =0
      NREST=NSC
  50  CONTINUE
C-----------------------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSC(I+NDEB)
      IN=KSI(IL)
      FXN=WF(IN)*NIMP(1,IL)
      FYN=WF(IN)*NIMP(2,IL)
      FZN=WF(IN)*NIMP(3,IL)
C------------------------------------------------------------
C     RETOUR DES FORCES EN GLOBAL
C------------------------------------------------------------
      FN1 =ROT(1)*FXN+ROT(4)*FYN+ROT(7)*FZN
      FN2 =ROT(2)*FXN+ROT(5)*FYN+ROT(8)*FZN
      FN3 =ROT(3)*FXN+ROT(6)*FYN+ROT(9)*FZN
      FV(1,I)  =FN1
      FV(2,I)  =FN2
      FV(3,I)  =FN3
C---------------------------------
      FS(1)=FS(1)-FN1*DT1
      FS(2)=FS(2)-FN2*DT1
      FS(3)=FS(3)-FN3*DT1
C---------------------------------
      ENDDO
C----------------------------------------------------------
C     MOMENTS ET STABILITE.
C----------------------------------------------------------
      IF(KDTINT.EQ.0)THEN
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSC(I+NDEB)
        IN=KSI(IL)
        IF(IMPACT(IL).GT.0)THEN
         ST(I)=STF+DEUX*WST(IN)*DT1INV
        ELSE
         ST(I)=ZERO
        ENDIF
       ENDDO
      ELSE
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSC(I+NDEB)
        IN=KSI(IL)
        IF(IMPACT(IL).GT.0)THEN
         ST(I)=STF+WST(IN)*DT1INV
        ELSE
         ST(I)=ZERO
        ENDIF
       ENDDO
      ENDIF
C----------------------------------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSC(I+NDEB)
      IN=KSI(IL)
C---------------------------------
C     CALCUL DES MOMENTS MS ^ F (REP. GLOBAL).
Clm   les forces sont transmises au (meme) point 
C     que l'on a recu !!!
C---------------------------------
      X1=X(1,IN)-BUFSF(ADRBUF+16)
      X2=X(2,IN)-BUFSF(ADRBUF+17)
      X3=X(3,IN)-BUFSF(ADRBUF+18)
      AM1=X2*FV(3,I)-X3*FV(2,I)
      AM2=X3*FV(1,I)-X1*FV(3,I)
      AM3=X1*FV(2,I)-X2*FV(1,I)
C---------------------------------
C     Assemblage des FORCES, moments et rigidites d'interface
C     au nd main.
C---------------------------------
      BUFSF(ADRBUF+25)=BUFSF(ADRBUF+25)-FV(1,I)
      BUFSF(ADRBUF+26)=BUFSF(ADRBUF+26)-FV(2,I)
      BUFSF(ADRBUF+27)=BUFSF(ADRBUF+27)-FV(3,I)
      BUFSF(ADRBUF+28)=BUFSF(ADRBUF+28)-AM1
      BUFSF(ADRBUF+29)=BUFSF(ADRBUF+29)-AM2
      BUFSF(ADRBUF+30)=BUFSF(ADRBUF+30)-AM3
      BUFSF(ADRBUF+31)=BUFSF(ADRBUF+31)+ST(I)
C        STIFRM += [ ST2 * |(CIMP-M)^N|**2 ] + [ ST1 * |(CIMP-M)^T|**2 ]
C        est majore par :
      DD = X1**2+X2**2+X3**2
      BUFSF(ADRBUF+32)=BUFSF(ADRBUF+32)+DD*ST(I)
C---------------------------------
      ENDDO
C----------------------------------------------------------
      IF(KDTINT.NE.0)THEN
       DO I=1,MIN(MVSIZ,NREST)
        IL=KSC(I+NDEB)
        IN=KSI(IL)
        IF(IMPACT(IL).GT.0)THEN
         ST(I)=STF
        ELSE
         ST(I)=ZERO
        ENDIF
       ENDDO
      ENDIF
C---------------------------------
C     Assemblage des FORCES aux noeuds seconds.
C---------------------------------
      IF (IPARIT.EQ.0) THEN
C-----------------------------------------------
#include "vectorize.inc"
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSC(I+NDEB)
      IN=KSI(IL)
       I3=3*IN
       I2=I3-1
       I1=I2-1
       AF(I1)=AF(I1)+FV(1,I)
       AF(I2)=AF(I2)+FV(2,I)
       AF(I3)=AF(I3)+FV(3,I)
C      Stabilite
       STIFN(IN)=STIFN(IN)+ST(I)
       ENDDO
      ELSE
C-----------------------------------------------
      IF(KDTINT.EQ.0)THEN
       DO I=1,MIN(MVSIZ,NREST)
       IL=KSC(I+NDEB)
       IN=KSI(IL)
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FV(1,I)
        FSKYI(NISKYL,2)=FV(2,I)
        FSKYI(NISKYL,3)=FV(3,I)
C       Stabilite
        FSKYI(NISKYL,4)=ST(I)
        ISKY(NISKYL)   =IN
        ENDDO
      ELSE
       DO I=1,MIN(MVSIZ,NREST)
       IL=KSC(I+NDEB)
       IN=KSI(IL)
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FV(1,I)
        FSKYI(NISKYL,2)=FV(2,I)
        FSKYI(NISKYL,3)=FV(3,I)
C       Stabilite
        FSKYI(NISKYL,4)=ST(I)
        FSKYI(NISKYL,5)=WST(IN)
C 2C dans modsti        FSKYI(NISKYL,5)=2.*WST(IN)
        ISKY(NISKYL)   =IN
        ENDDO
      ENDIF

      ENDIF
C------------------------------------------------------------
C     ANIM (FORCES DE CONTACT).
C------------------------------------------------------------
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT .GT.0.AND.
     .    (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D .OR.
     .   (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
#include "lockon.inc"
#include "vectorize.inc"
        DO I=1,MIN(MVSIZ,NREST)
         IL=KSC(I+NDEB)
         IN=KSI(IL)
         FCONT(1,IN) =FCONT(1,IN) + FV(1,I)
         FCONT(2,IN) =FCONT(2,IN) + FV(2,I)
         FCONT(3,IN) =FCONT(3,IN) + FV(3,I)
        ENDDO
#include "lockoff.inc"
      ENDIF
C---------------------------------
C     Pour Travail des forces sur noeuds seconds
C     1ere partie : ici
C     2eme partie : apres calcul de DT2.
C---------------------------------
      DO I=1,MIN(MVSIZ,NREST)
         IL=KSC(I+NDEB)
         IN=KSI(IL)
         DE=DE+FV(1,I)*V(1,IN)+FV(2,I)*V(2,IN)+FV(3,I)*V(3,IN)
      ENDDO
C---------------------------------
C     Groupe suivant.
C---------------------------------
      IF (NREST-MVSIZ.GT.0) THEN
        NREST=NREST-MVSIZ
        NDEB =NDEB +MVSIZ
        GOTO 50
      END IF
C-------------------------------
C     POINTS PRECEDEMMENT IMPACTES.
C-------------------------------
      NDEB =0
      NREST=NSP
 100  CONTINUE
C-------------------------------
C     Passage au repere local :
C-------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
      X1=X(1,IN)-BUFSF(ADRBUF+4)
      X2=X(2,IN)-BUFSF(ADRBUF+5)
      X3=X(3,IN)-BUFSF(ADRBUF+6)
      XPV(1,I)=ROT(1)*X1+ROT(2)*X2+ROT(3)*X3
      XPV(2,I)=ROT(4)*X1+ROT(5)*X2+ROT(6)*X3
      XPV(3,I)=ROT(7)*X1+ROT(8)*X2+ROT(9)*X3
      ENDDO
C-------------------------------
C     Extraction des infos deja calculees.
C-------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
      FNPV(I) =WF(IN)
      NV(1,I) =NIMP(1,IL)
      NV(2,I) =NIMP(2,IL)
      NV(3,I) =NIMP(3,IL)
      ENDDO
C----------------------------------------------------------
C     Coefficient de Frottement :
Clm   MU=F(FORCE NORMALE LOCALE).
C----------------------------------------------------------
      IF (NFRIC.EQ.0) THEN
       DO I=1,MIN(MVSIZ,NREST)
        KFRIC(I)=FRIC
       ENDDO
      ELSE
       CALL NINTERP(NFRIC,NPC,PLD,MIN(MVSIZ,NREST),FNPV,KFRIC)
       DO I=1,MIN(MVSIZ,NREST)
        KFRIC(I)=FRIC*KFRIC(I)
       ENDDO
      ENDIF
C----------------------------------------------------------
C     Frottement :
C----------------------------------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
C------------------------------
      FXT=ZERO
      FYT=ZERO
      FZT=ZERO      
C-------------------------------
        XI=CIMP(1,IL)
        YI=CIMP(2,IL)
        ZI=CIMP(3,IL)
C------------------------------
      FMAX= MAX(KFRIC(I)*FNPV(I),ZERO)
C     Force tangente K(XI-XP) et Norme.
      FXT=(XI-XPV(1,I))*STF
      FYT=(YI-XPV(2,I))*STF
      FZT=(ZI-XPV(3,I))*STF
      FTP=SQRT(FXT*FXT+FYT*FYT+FZT*FZT)
C-----------------------------------------------
      FXN=FNPV(I)*NV(1,I)
      FYN=FNPV(I)*NV(2,I)
      FZN=FNPV(I)*NV(3,I)
C-----------------------------------------------
      FN=FXT*NV(1,I)+FYT*NV(2,I)+FZT*NV(3,I)
      TV(1,I)=FXT-NV(1,I)*FN
      TV(2,I)=FYT-NV(2,I)*FN
      TV(3,I)=FZT-NV(3,I)*FN
      TN=SQRT(TV(1,I)*TV(1,I)+TV(2,I)*TV(2,I)+TV(3,I)*TV(3,I))
      TN=MAX(EM20,TN)
      TV(1,I)=TV(1,I)/TN
      TV(2,I)=TV(2,I)/TN
      TV(3,I)=TV(3,I)/TN
C-----------------------------------------------
C     Force de Frottement.
      FTPA=MIN(TN,FMAX)
      FXT=TV(1,I)*FTPA
      FYT=TV(2,I)*FTPA
      FZT=TV(3,I)*FTPA
C-------------------------------
C     proj. sur tangente.
      COST =(XPV(1,I)-XI)*TV(1,I)
     .     +(XPV(2,I)-YI)*TV(2,I)
     .     +(XPV(3,I)-ZI)*TV(3,I)
      XQ(1,I) =XI+COST*TV(1,I)
      XQ(2,I) =YI+COST*TV(2,I)
      XQ(3,I) =ZI+COST*TV(3,I)
C-------------------------------
C     glissement du point sur la tgte.
      XQ(1,I) =XQ(1,I)+FTPA*TV(1,I)/MAX(EM20,STF)
      XQ(2,I) =XQ(2,I)+FTPA*TV(2,I)/MAX(EM20,STF)
      XQ(3,I) =XQ(3,I)+FTPA*TV(3,I)/MAX(EM20,STF)
C---------------------------------
      FV(1,I)=FXT+FXN
      FV(2,I)=FYT+FYN
      FV(3,I)=FZT+FZN
C---------------------------------
      ENDDO
C----------------------------------------------------------
C     Frottement Suite :
C----------------------------------------------------------
#include "vectorize.inc"
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
C-------------------------------
C     nouvelle normale.
C-------------------------------
      XPVN1 =XQ(1,I)**(DGR-1)
      SGNXN=-UN
      IF (XPVN1*XQ(1,I).GE.ZERO) SGNXN=UN
      YPVN1 =XQ(2,I)**(DGR-1)
      SGNYN=-UN
      IF (YPVN1*XQ(2,I).GE.ZERO) SGNYN=UN
      ZPVN1 =XQ(3,I)**(DGR-1)
      SGNZN=-UN
      IF (ZPVN1*XQ(3,I).GE.ZERO) SGNZN=UN     
      N1 =SGNXN*XPVN1*AN
      N2 =SGNYN*YPVN1*BN
      N3 =SGNZN*ZPVN1*CN
      N =N1*N1+N2*N2+N3*N3
      N  =SQRT(N)
C-------------------------------
C     nouveau point d'impact : projection hors ellips.
C     la position de l'impact et le calcul de la normale s'affinent
C     en l'absence de glissement.
C-------------------------------
      E =N1*XQ(1,I)+N2*XQ(2,I)+N3*XQ(3,I)
      DIST =(E-EXP((DGR-1)*LOG(MAX(E,EM20))/DGR))
     .      / MAX(EXP((DGR-1)*LOG(EM20)/DGR),N)
      N1 =N1/MAX(EM20,N)
      N2 =N2/MAX(EM20,N)
      N3 =N3/MAX(EM20,N)
      CIMP(1,IL)=XQ(1,I)-DIST*N1
      CIMP(2,IL)=XQ(2,I)-DIST*N2
      CIMP(3,IL)=XQ(3,I)-DIST*N3
C     memorisation des composantes de la normale.
      NIMP(1,IL)=N1
      NIMP(2,IL)=N2
      NIMP(3,IL)=N3
C-------------------------------
C     rabattre F // PI apres glissement
Clm   ROMPT LA RELATION FTOT=F(PENETRATION MAX.)
C-------------------------------
      FX=FV(1,I)
      FY=FV(2,I)
      FZ=FV(3,I)
C-------------------------------
      XI=CIMP(1,IL)+GAPMIN*NIMP(1,IL)
      YI=CIMP(2,IL)+GAPMIN*NIMP(2,IL)
      ZI=CIMP(3,IL)+GAPMIN*NIMP(3,IL)
      PSCA = (XI-XPV(1,I))*FX
     .      +(YI-XPV(2,I))*FY
     .      +(ZI-XPV(3,I))*FZ
      DIST = (XI-XPV(1,I))**2
     .      +(YI-XPV(2,I))**2
     .      +(ZI-XPV(3,I))**2
      FX=PSCA/DIST*(XI-XPV(1,I))
      FY=PSCA/DIST*(YI-XPV(2,I))
      FZ=PSCA/DIST*(ZI-XPV(3,I))
C     decomposition pour sorties :
      PSCA=FX*NV(1,I)+FY*NV(2,I)+FZ*NV(3,I)
      FXN =PSCA*NV(1,I)
      FYN =PSCA*NV(2,I)
      FZN =PSCA*NV(3,I)
      PSCA=FX*TV(1,I)+FY*TV(2,I)+FZ*TV(3,I)
      FXT =PSCA*TV(1,I)
      FYT =PSCA*TV(2,I)
      FZT =PSCA*TV(3,I)
C------------------------------------------------------------
C     RETOUR DES FORCES EN GLOBAL
C------------------------------------------------------------
      FN1 =ROT(1)*FXN+ROT(4)*FYN+ROT(7)*FZN
      FN2 =ROT(2)*FXN+ROT(5)*FYN+ROT(8)*FZN
      FN3 =ROT(3)*FXN+ROT(6)*FYN+ROT(9)*FZN
      FT1 =ROT(1)*FXT+ROT(4)*FYT+ROT(7)*FZT
      FT2 =ROT(2)*FXT+ROT(5)*FYT+ROT(8)*FZT
      FT3 =ROT(3)*FXT+ROT(6)*FYT+ROT(9)*FZT
      FV(1,I)  =FN1+FT1
      FV(2,I)  =FN2+FT2
      FV(3,I)  =FN3+FT3
C---------------------------------
      FS(1)=FS(1)-FN1*DT1
      FS(2)=FS(2)-FN2*DT1
      FS(3)=FS(3)-FN3*DT1
      FS(4)=FS(4)-FT1*DT1
      FS(5)=FS(5)-FT2*DT1
      FS(6)=FS(6)-FT3*DT1
C---------------------------------
      ENDDO
C----------------------------------------------------------
C     MOMENTS ET STABILITE.
C----------------------------------------------------------
      IF(KDTINT.EQ.0)THEN
       DO I=1,MIN(MVSIZ,NREST)
       IL=KSP(I+NDEB)
       IN=KSI(IL)
       ST(I)=STF+DEUX*WST(IN)*DT1INV
       ENDDO
      ELSE
       DO I=1,MIN(MVSIZ,NREST)
       IL=KSP(I+NDEB)
       IN=KSI(IL)
       ST(I)=STF+WST(IN)*DT1INV
       ENDDO
      ENDIF
C----------------------------------------------------------
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
C---------------------------------
C     CALCUL DES MOMENTS MS ^ F (REP. GLOBAL).
Clm   les forces sont transmises au (meme) point 
C     que l'on a recu !!!
C---------------------------------
      X1=X(1,IN)-BUFSF(ADRBUF+16)
      X2=X(2,IN)-BUFSF(ADRBUF+17)
      X3=X(3,IN)-BUFSF(ADRBUF+18)
      AM1=X2*FV(3,I)-X3*FV(2,I)
      AM2=X3*FV(1,I)-X1*FV(3,I)
      AM3=X1*FV(2,I)-X2*FV(1,I)
C---------------------------------
C     Assemblage des FORCES, moments et rigidites d'interface
C     au nd main.
C---------------------------------
      BUFSF(ADRBUF+25)=BUFSF(ADRBUF+25)-FV(1,I)
      BUFSF(ADRBUF+26)=BUFSF(ADRBUF+26)-FV(2,I)
      BUFSF(ADRBUF+27)=BUFSF(ADRBUF+27)-FV(3,I)
      BUFSF(ADRBUF+28)=BUFSF(ADRBUF+28)-AM1
      BUFSF(ADRBUF+29)=BUFSF(ADRBUF+29)-AM2
      BUFSF(ADRBUF+30)=BUFSF(ADRBUF+30)-AM3
      BUFSF(ADRBUF+31)=BUFSF(ADRBUF+31)+ST(I)
C        STIFRM += [ ST2 * |(CIMP-M)^N|**2 ] + [ ST1 * |(CIMP-M)^T|**2 ]
C        est majore par :
      DD = X1**2+X2**2+X3**2
      BUFSF(ADRBUF+32)=BUFSF(ADRBUF+32)+DD*ST(I)
C---------------------------------
      ENDDO
C---------------------------------
C     Assemblage des FORCES aux noeuds seconds.
C---------------------------------
      IF (IPARIT.EQ.0) THEN
C-----------------------------------------------
#include "vectorize.inc"
      DO I=1,MIN(MVSIZ,NREST)
      IL=KSP(I+NDEB)
      IN=KSI(IL)
       I3=3*IN
       I2=I3-1
       I1=I2-1
       AF(I1)=AF(I1)+FV(1,I)
       AF(I2)=AF(I2)+FV(2,I)
       AF(I3)=AF(I3)+FV(3,I)
C      Stabilite
       STIFN(IN)=STIFN(IN)+ST(I)
       ENDDO
      ELSE
C-----------------------------------------------
      IF(KDTINT.EQ.0)THEN
       DO I=1,MIN(MVSIZ,NREST)
       IL=KSP(I+NDEB)
       IN=KSI(IL)
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FV(1,I)
        FSKYI(NISKYL,2)=FV(2,I)
        FSKYI(NISKYL,3)=FV(3,I)
C       Stabilite
        FSKYI(NISKYL,4)=ST(I)
        ISKY(NISKYL)   =IN
        ENDDO
      ELSE
       DO I=1,MIN(MVSIZ,NREST)
       IL=KSP(I+NDEB)
       IN=KSI(IL)
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FV(1,I)
        FSKYI(NISKYL,2)=FV(2,I)
        FSKYI(NISKYL,3)=FV(3,I)
C       Stabilite
        FSKYI(NISKYL,4)=STF
        FSKYI(NISKYL,5)=WST(IN)
C 2C dans modsti        FSKYI(NISKYL,5)=2.*WST(IN)
        ISKY(NISKYL)   =IN
        ENDDO
      ENDIF

      ENDIF
C------------------------------------------------------------
C     ANIM (FORCES DE CONTACT).
C------------------------------------------------------------
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0.AND.
     .    (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .   (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
#include "lockon.inc"
#include "vectorize.inc"
        DO I=1,MIN(MVSIZ,NREST)
         IL=KSP(I+NDEB)
         IN=KSI(IL)
         FCONT(1,IN) =FCONT(1,IN) + FV(1,I)
         FCONT(2,IN) =FCONT(2,IN) + FV(2,I)
         FCONT(3,IN) =FCONT(3,IN) + FV(3,I)
        ENDDO
#include "lockoff.inc"
      ENDIF
C---------------------------------
C     Pour Travail des forces sur noeuds seconds
C     1ere partie : ici
C     2eme partie : apres calcul de DT2.
C---------------------------------
      DO I=1,MIN(MVSIZ,NREST)
         IL=KSP(I+NDEB)
         IN=KSI(IL)
         DE=DE+FV(1,I)*V(1,IN)+FV(2,I)*V(2,IN)+FV(3,I)*V(3,IN)
      ENDDO
C---------------------------------
C     Groupe suivant.
C---------------------------------
      IF (NREST-MVSIZ.GT.0) THEN
        NREST=NREST-MVSIZ
        NDEB =NDEB +MVSIZ
        GOTO 100
      END IF
C---------------------------------
C     Travail des forces d'interface avec Madymo.
C---------------------------------
      FS(7)=FS(7)+DE*DT1*UNDEMI
      IF (IGRSURF(KSURF)%TYPE.EQ.100) THEN
C     Sorties Ellipdoides Madymo.
#include "atomic.inc"
          TFEXT=TFEXT+DE*DT1*UNDEMI
#include "atomend.inc"
      ENDIF
C------------------------------------------------------------
      RETURN
      END
