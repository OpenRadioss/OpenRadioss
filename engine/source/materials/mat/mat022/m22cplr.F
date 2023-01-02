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
Chd|  M22CPLR                       source/materials/mat/mat022/m22cplr.F
Chd|-- called by -----------
Chd|        SIGEPS22C                     source/materials/mat/mat022/sigeps22c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M22CPLR(JFT    ,JLT    ,EZZ    ,OFF    ,EPSEQ ,
     2                   IPLA   ,GS     ,YLD    ,A1     ,CA    ,
     3                   CB     ,CN     ,YMAX   ,NU     ,DPLA  ,
     4                   EPCHK  ,YOUNG  ,CC     ,EPDR   ,EPSL  ,
     5                   HL     ,YM     ,YLDL   ,ALPE   ,ICC   ,
     6                   DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     7                   SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX,
     8                   SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX,
     9                   NEL    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IPLA,NEL
C     REAL
      my_real
     .   EZZ(*),OFF(*),EPSEQ(*),GS(*),DPLA(*)
      my_real
     .   YLD(MVSIZ),A1(MVSIZ),NU(MVSIZ),YMAX(MVSIZ), 
     .   CN(MVSIZ),YOUNG(MVSIZ),EPCHK(MVSIZ),CC(MVSIZ),EPDR(MVSIZ),
     .   EPSL(MVSIZ),HL(MVSIZ),YM(MVSIZ),YLDL(MVSIZ),ALPE(MVSIZ),
     .   CA(MVSIZ),CB(MVSIZ),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),DEPSZX(MVSIZ)
      INTEGER ICC(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NINDX,INDEX(MVSIZ),NMAX
C     REAL
      my_real
     .   A(MVSIZ),B(MVSIZ),DPLA_I(MVSIZ),DPLA_J(MVSIZ),DR(MVSIZ),
     .   H(MVSIZ),NU1(MVSIZ),NU2(MVSIZ),P(MVSIZ),Q(MVSIZ),
     .   SVM(MVSIZ),DK(MVSIZ),FF(MVSIZ),G(MVSIZ),
     .   PLAEF(MVSIZ),PLAXX(MVSIZ),PLAYY(MVSIZ),PLAXY(MVSIZ),
     .   EPMAX(MVSIZ),DEPSL,ALPEI,GI,A1I,A2I,EPSP,S11,S22,S12,
     .   S1S2,S122,VM2,PLA_I,YLD_I,F,DF,P2,Q2,NNU1,
     .   NNU2,NU3,NU4,NU5,NU6,R,UMR,AA,BB,C,EE,SIGZ,PP,QQ,S1,S2,S3,
     .   SMALL
      DATA NMAX/3/
C-----------------------------------------------
      SMALL = EM7
      DO I=JFT,JLT
        SIGNXX(I)=SIGOXX(I)
        SIGNYY(I)=SIGOYY(I)
        SIGNXY(I)=SIGOXY(I)
        SIGNYZ(I)=SIGOYZ(I)
        SIGNZX(I)=SIGOZX(I)
      ENDDO
C
#include "vectorize.inc"
      DO I=JFT,JLT
        YLD(I)  = (CA(I)+CB(I)*EPSEQ(I)**CN(I))
        YLD(I)  = MIN(YLD(I),YMAX(I))
        DEPSL   = MAX(ZERO,EPSEQ(I)-EPSL(I))
        YLD(I)  = MIN(YLD(I),YLDL(I)+HL(I)*DEPSL)
        YLD(I)  = MAX(YLD(I),EM30)
        ALPEI   = MIN(ONE,YLD(I)/(YLD(I)+YM(I)*DEPSL))
        ALPEI   = MAX(EM30,ALPEI)
        YOUNG(I)= ALPEI*YM(I)
        GI      = HALF*YOUNG(I)/(ONE +NU(I))
        G(I)    = GI
        A1I     = YOUNG(I)/(ONE -NU(I)**2)
        ALPE(I) = MAX(ALPE(I),A1I/A1(I))
        A2I     = NU(I)*A1I
        SIGNXX(I)= SIGNXX(I)+A1I*DEPSXX(I)+A2I*DEPSYY(I)
        SIGNYY(I)= SIGNYY(I)+A2I*DEPSXX(I)+A1I*DEPSYY(I)
        SIGNXY(I)= SIGNXY(I)+GI *DEPSXY(I)
        SIGNYZ(I)= SIGNYZ(I)+ALPEI*GS(I)*DEPSYZ(I)
        SIGNZX(I)= SIGNZX(I)+ALPEI*GS(I)*DEPSZX(I)
      ENDDO
C-------------------
C     CONTRAINTE VM
C-------------------
#include "vectorize.inc"
      DO I=JFT,JLT
        SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I)
     .               +SIGNYY(I)*SIGNYY(I)
     .               -SIGNXX(I)*SIGNYY(I)
     .        + THREE*SIGNXY(I)*SIGNXY(I))
      ENDDO
C----------------------------
C     VITESSE DE DEFORMATION
C----------------------------
      DO I=JFT,JLT
        EPSP  = MAX( ABS(DEPSXX(I)), ABS(DEPSYY(I)), HALF*ABS(DEPSXY(I)))
        EPSP  = MAX(EPSP,EPDR(I))
        YLD(I)= YLD(I)*(ONE +CC(I) * LOG(EPSP/EPDR(I)))
        IF (ICC(I) == 2) YLD(I) = MIN(YLD(I),YMAX(I))
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
      IF (IPLA == 0) THEN
C projection radiale
#include "vectorize.inc"
        DO I=JFT,JLT
          DK(I) = MIN(ONE,YLD(I)/MAX(SVM(I),EM30))
          SIGNXX(I) = SIGNXX(I)*DK(I)
          SIGNYY(I) = SIGNYY(I)*DK(I)
          SIGNXY(I) = SIGNXY(I)*DK(I)
          DPLA(I)  = OFF(I) * MAX(ZERO,(SVM(I)-YLD(I))/YOUNG(I))
          S1 = HALF*(SIGNXX(I)+SIGNYY(I))
          EZZ(I) = DPLA(I) * S1 /YLD(I)
          EPSEQ(I) = EPSEQ(I) + DPLA(I)
          EPCHK(I) = MAX(EPSEQ(I),EPCHK(I))
        ENDDO
C------------------------------------------------------------------------
      ELSEIF (IPLA == 1) THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
#include "vectorize.inc"
        DO  I=JFT,JLT
          S1 = SIGNXX(I) + SIGNYY(I)
          S2 = SIGNXX(I) - SIGNYY(I)
          S3 = SIGNXY(I)
          A(I) = FOURTH*S1*S1
          B(I) = THREE_OVER_4*S2*S2 + THREE*S3*S3
          SVM(I) = SQRT(A(I) + B(I))  
        ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
      NINDX=0
C
      DO I=JFT,JLT
        DPLA(I) = ZERO
        IF (SVM(I) > YLD(I) .AND. OFF(I) == ONE) THEN
          NINDX = NINDX + 1
          INDEX(NINDX) = I
        ENDIF
      ENDDO
C
      IF (NINDX == 0) GOTO 800
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
#include "vectorize.inc"
      DO J=1,NINDX
        I = INDEX(J)
        NU1(I) = ONE/(ONE-NU(I))
        NU2(I) = ONE/(ONE+NU(I))
        H(I) = CN(I)*CB(I)*EXP((CN(I)-ONE)*LOG(EPSEQ(I)+SMALL))
        IF (YLD(I) >= YMAX(I)) H(I) = ZERO
        DPLA_J(I) = (SVM(I)-YLD(I))/(THREE*G(I)+H(I))
      ENDDO
C            
      DO N=1,NMAX
#include "vectorize.inc"
        DO J=1,NINDX
          I = INDEX(J)
          DPLA_I(I) = DPLA_J(I)
          DPLA(I)   = DPLA_J(I)        
          PLA_I = EPSEQ(I)+DPLA_I(I)
          YLD_I = MIN(YMAX(I),CA(I)+CB(I)*PLA_I**CN(I))
          DR(I) = HALF*YOUNG(I)*DPLA_I(I)/YLD_I
          P(I)  = ONE/(ONE+DR(I)*NU1(I))
          Q(I)  = ONE/(ONE+THREE*DR(I)*NU2(I))
          P2    = P(I)*P(I)
          Q2    = Q(I)*Q(I)
          F     = A(I)*P2+B(I)*Q2-YLD_I*YLD_I
          DF    = -(A(I)*NU1(I)*P2*P(I)+THREE*B(I)*NU2(I)*Q2*Q(I))
     .            *(YOUNG(I)-TWO*DR(I)*H(I))/YLD_I
     .            -TWO*H(I)*YLD_I
          IF (DPLA_I(I) > ZERO) THEN
            DPLA_J(I) = MAX(ZERO,DPLA_I(I)-F/DF)
          ELSE
            DPLA_J(I) = ZERO
          ENDIF        
        ENDDO
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc" 
      DO J=1,NINDX
        I = INDEX(J)
        DPLA(I) = DPLA_I(I)
        EPSEQ(I)= EPSEQ(I) + DPLA_I(I)
        EPCHK(I)= MAX(EPSEQ(I),EPCHK(I))
        S1 = (SIGNXX(I)+SIGNYY(I))*P(I)
        S2 = (SIGNXX(I)-SIGNYY(I))*Q(I)
        SIGNXX(I) = HALF*(S1+S2)
        SIGNYY(I) = HALF*(S1-S2)
        SIGNXY(I) = SIGNXY(I)*Q(I)
        EZZ(I) = DR(I)*S1/YOUNG(I)
      ENDDO
      ELSEIF (IPLA == 2) THEN
C---
C projection radial sur le deviateur sur un critere reduit
C projection elastique en z => sig33 = 0
C le coef. de reduction du critere est tel que 
C l'on se trouve sur le critere apres les 2 projections
C---
#include "vectorize.inc"
        DO I=JFT,JLT
          PP  = -(SIGNXX(I)+SIGNYY(I))*THIRD
          S11 = SIGNXX(I)+PP
          S22 = SIGNYY(I)+PP
C         s33 = p = -(S11 + S22)
          S12 = SIGNXY(I)
          P2  = PP*PP
          S1S2 = S11*S22
          S122 = S12*S12
          NNU1 = NU(I) / (ONE - NU(I))
          NNU2 = NNU1*NNU1
          NU4 = ONE + NNU2 + NNU1
          NU6 = HALF  - NNU2 + HALF*NNU1
          QQ  = (ONE - NNU1)*PP
          AA = P2*NU4 + THREE*(S122 - S1S2)
          BB = P2*NU6
          C  = QQ*QQ
          VM2= AA+BB+BB+C
          C  = C - YLD(I)*YLD(I)
          R  = MIN(ONE,(-BB+ SQRT(MAX(ZERO,BB*BB-AA*C)))/MAX(AA ,EM20))
          UMR = ONE - R
          QQ = QQ*UMR
          SIGNXX(I) = SIGNXX(I)*R - QQ
          SIGNYY(I) = SIGNYY(I)*R - QQ
          SIGNXY(I) = S12*R
          DPLA(I) = OFF(I)*SQRT(VM2)*UMR/(THREE*G(I))
          S1 = HALF*(SIGNXX(I)+SIGNYY(I))
          EZZ(I) = DPLA(I) * S1 / YLD(I)
          EPSEQ(I) = EPSEQ(I) + DPLA(I)
          EPCHK(I) = MAX(EPSEQ(I),EPCHK(I))
        ENDDO
      ENDIF  !  IF (IPLA == 0) THEN
C
 800  CONTINUE
C
      DO I=JFT,JLT
        IF (ALPE(I) < ZEP999) THEN
          R = ONE - YOUNG(I)*DPLA(I)/MAX(EM20,YLD(I))
          R = MAX(ZERO,R)
          SIGNYZ(I) = SIGNYZ(I)*R
          SIGNZX(I) = SIGNZX(I)*R
        ENDIF
      ENDDO
C
      RETURN
      END
