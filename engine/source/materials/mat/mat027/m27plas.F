Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  M27PLAS                       source/materials/mat/mat027/m27plas.F
Chd|-- called by -----------
Chd|        SIGEPS27C                     source/materials/mat/mat027/sigeps27c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M27PLAS(JFT    ,JLT    ,PM     ,OFF    ,SIG   ,
     2                   EPSEQ  ,IMAT   ,DT1C   ,IPLA   ,EZZ   ,
     3                   EPSP   ,ISRATE ,YLD    ,ETSE   ,DPLA  ,
     4                   DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     5                   NEL    )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IPLA,IMAT,ISRATE,NEL
C     REAL
      my_real, INTENT(INOUT) :: SIG(NEL,5)
      my_real
     .   PM(NPROPM,*),OFF(*),EPSEQ(*),DT1C(*),EZZ(*),
     .   EPSP(*),DPLA(*),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ)
      my_real
     .   YLD(*),ETSE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC(MVSIZ)
      INTEGER I,J,N,NINDX,INDEX(MVSIZ),NMAX
C     REAL
      my_real
     .   CA(MVSIZ),CB(MVSIZ),CN(MVSIZ),YMAX(MVSIZ),
     .   CC(MVSIZ),EPDR(MVSIZ),SVM(MVSIZ),A(MVSIZ),B(MVSIZ),
     .   DPLA_I(MVSIZ),DPLA_J(MVSIZ),DR(MVSIZ),H(MVSIZ),
     .   NU1(MVSIZ),NU2(MVSIZ),P(MVSIZ),Q(MVSIZ),NU(MVSIZ),
     .   YOUNG(MVSIZ),G(MVSIZ),ERR,F,DF,PLA_I,P2,Q2,R,S1,S2,S3,
     .   YLD_I,NNU1,NNU2,NU3,NU4,NU5,NU6,SIGZ,PP,AA,BB,C,S11,
     .   S22,S12,S1S2,S122,UMR,VM2,QQ,SMALL
      DATA NMAX/3/
C-----------------------------------------------
      SMALL = EM7     
C
#include "vectorize.inc" 
      DO I=JFT,JLT
        YOUNG(I)= PM(20,IMAT)
        NU(I)   = PM(21,IMAT)
        G(I)    = PM(22,IMAT)
        CA(I)   = PM(38,IMAT)
        CB(I)   = PM(39,IMAT)
        CN(I)   = PM(40,IMAT)
        YMAX(I) = PM(42,IMAT)
        CC(I)   = PM(43,IMAT)
        EPDR(I) = MAX(EM20,PM(44,IMAT)*DT1C(I))
        ICC(I)  = NINT(PM(49,IMAT))
        ETSE(I) = ONE
      ENDDO
C-----------------------------------
C     VITESSE DE DEFORMATION & YIELD
C-----------------------------------
      DO I=JFT,JLT
        IF (ISRATE == 0)
     .  EPSP(I) = MAX(ABS(DEPSXX(I)),ABS(DEPSYY(I)),HALF*ABS(DEPSXY(I)))
        EPSP(I) = MAX(EPSP(I),EPDR(I))
        Q(I) = (ONE + CC(I) * LOG(EPSP(I)/EPDR(I)))
      ENDDO
C
      DO I=JFT,JLT
        IF (ICC(I) == 1) YMAX(I) = YMAX(I) * Q(I)
      ENDDO
C
      DO I=JFT,JLT
        DPLA(I) = ZERO      
        YLD(I) = (CA(I)+CB(I)*EPSEQ(I)**CN(I)) * Q(I)
        YLD(I) = MIN(YLD(I),YMAX(I))
        CA(I) = CA(I)*Q(I)
        CB(I) = CB(I)*Q(I)
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
      IF (IPLA == 0) THEN
C-------------------
C     CONTRAINTE VM
C-------------------
        DO I=JFT,JLT
          SVM(I) = SQRT(SIG(I,1)*SIG(I,1)
     .                 +SIG(I,2)*SIG(I,2)
     .                 -SIG(I,1)*SIG(I,2)
     .           +THREE*SIG(I,3)*SIG(I,3))
        ENDDO
C
C projection radiale 
C
#include "vectorize.inc" 
        DO I=JFT,JLT
          R = MIN(ONE,YLD(I)/(SVM(I)+ EM15))
          SIG(I,1) = SIG(I,1)*R
          SIG(I,2) = SIG(I,2)*R
          SIG(I,3) = SIG(I,3)*R
          DPLA(I)  = OFF(I) * MAX(ZERO,(SVM(I)-YLD(I))/YOUNG(I))
          H(I) = CN(I)*CB(I)*EXP((CN(I)-ONE)*LOG(EPSEQ(I)+SMALL))
          IF (YLD(I) >= YMAX(I)) H(I) = ZERO
          ETSE(I)= H(I)/(H(I)+YOUNG(I))
          S1 = HALF*(SIG(I,1)+SIG(I,2))
          EZZ(I) = DPLA(I) * S1 /YLD(I)
          EPSEQ(I) = EPSEQ(I) + DPLA(I)          
        ENDDO
C-------------------------
      ELSEIF (IPLA == 1) THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
        DO  I=JFT,JLT
          S1 = SIG(I,1) + SIG(I,2)
          S2 = SIG(I,1) - SIG(I,2)
          S3 = SIG(I,3)
          A(I) = FOURTH*S1*S1
          B(I) = THREE_OVER_4*S2*S2+THREE*S3*S3
          SVM(I) = SQRT(A(I)+B(I))  
        ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
        NINDX=0
        DO I=JFT,JLT
          IF (SVM(I) > YLD(I) .AND. OFF(I) == ONE) THEN
            NINDX = NINDX + 1
            INDEX(NINDX) = I
          ENDIF
        ENDDO
        IF (NINDX == 0) RETURN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
#include "vectorize.inc" 
        DO  J=1,NINDX
          I = INDEX(J)
          NU1(I) = ONE/(ONE-NU(I))
          NU2(I) = ONE/(ONE+NU(I))
          H(I) = CN(I)*CB(I)*EXP((CN(I)-ONE)*LOG(EPSEQ(I)+SMALL))
          IF (YLD(I) >= YMAX(I)) H(I) = ZERO
          DPLA_J(I) = (SVM(I)-YLD(I))/(THREE*G(I)+H(I))
          ETSE(I) = H(I)/(H(I)+YOUNG(I))
        ENDDO
C            
        DO N=1,NMAX
#include "vectorize.inc" 
          DO J=1,NINDX
            I = INDEX(J)
            DPLA_I(I) = DPLA_J(I)
            DPLA(I)   = DPLA_I(I)       
            PLA_I = EPSEQ(I)+DPLA_I(I)
            YLD_I = MIN(YMAX(I),CA(I)+CB(I)*PLA_I**CN(I))
            DR(I) = HALF*YOUNG(I)*DPLA_I(I)/YLD_I
            P(I)  = ONE/(ONE+DR(I)*NU1(I))
            Q(I)  = ONE/(ONE + THREE*DR(I)*NU2(I))
            P2    = P(I)*P(I)
            Q2    = Q(I)*Q(I)
            F     = A(I)*P2+B(I)*Q2-YLD_I*YLD_I
            DF    = -(A(I)*NU1(I)*P2*P(I)+3.*B(I)*NU2(I)*Q2*Q(I))
     .              *(YOUNG(I)-TWO*DR(I)*H(I))/YLD_I
     .               -TWO*H(I)*YLD_I
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
          EPSEQ(I) = EPSEQ(I) + DPLA_I(I)
          S1 = (SIG(I,1)+SIG(I,2))*P(I)
          S2 = (SIG(I,1)-SIG(I,2))*Q(I)
          SIG(I,1) = HALF*(S1+S2)
          SIG(I,2) = HALF*(S1-S2)
          SIG(I,3) = SIG(I,3)*Q(I)
          EZZ(I) = DR(I)*S1/YOUNG(I)
        ENDDO
C-------------------------
      ELSEIF (IPLA == 2) THEN
C-------------------
C     CONTRAINTE VM
C-------------------
        DO I=JFT,JLT
          PP  = -(SIG(I,1)+SIG(I,2))*THIRD
          S11 = SIG(I,1)+PP
          S22 = SIG(I,2)+PP
          S12 = SIG(I,3)
          P2 = PP*PP
          S1S2 = S11*S22
          S122 = S12*S12
          NNU1 = NU(I) / (ONE - NU(I))
          NNU2 = NNU1*NNU1
          NU4 = ONE + NNU2 + NNU1
          NU6 = HALF  - NNU2 + HALF*NNU1
          QQ  = (ONE-NNU1)*PP
          AA = P2*NU4 + THREE*(S122 - S1S2)
          BB = P2*NU6
          C  = QQ*QQ
          VM2= AA+BB+BB+C
          C  = C - YLD(I)*YLD(I)
C
          R  = MIN(ONE,(-BB+ SQRT(MAX(ZERO,BB*BB-AA*C)))/MAX(AA ,EM20))
C
          UMR = ONE - R
          QQ = QQ*UMR
          SIG(I,1) = SIG(I,1)*R - QQ
          SIG(I,2) = SIG(I,2)*R - QQ
          SIG(I,3) = S12*R
          DPLA(I) = OFF(I)*SQRT(VM2)*UMR/(THREE*G(I))
          H(I) = CN(I)*CB(I)*EXP((CN(I)-ONE)*LOG(EPSEQ(I)+SMALL))
          IF (YLD(I) >= YMAX(I)) H(I) = ZERO
          ETSE(I) = H(I)/(H(I)+YOUNG(I))
          S1 = HALF*(SIG(I,1)+SIG(I,2))
          EZZ(I) = DPLA(I) * S1 / YLD(I)
          EPSEQ(I) = EPSEQ(I) + DPLA(I)          
        ENDDO
      ENDIF
C
      RETURN
      END
