Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    m2cplr      ../engine/source/materials/mat/mat002/m2cplr.F
!||--- called by ------------------------------------------------------
!||    sigeps02c   ../engine/source/materials/mat/mat002/sigeps02c.F
!||====================================================================
      SUBROUTINE M2CPLR(JFT    ,JLT    ,EZZ      ,OFF      ,PLA   ,
     2                  IPLA   ,TEMP   ,Z3       ,Z4       ,
     3                  IRTY   ,ETSE   ,GS       ,EPSP     ,
     4                  ISRATE ,YLD    ,G        ,A1       ,A2      ,
     5                  NU     ,CA0    ,CB0      ,CN       ,YMAX0   ,
     6                  EPCHK  ,YOUNG  ,CC       ,EPDR     ,ICC     ,
     7                  DPLA   ,TSTAR  ,FISOKIN  ,GAMA_IMP ,SIGNOR  ,
     8                  HARDM  ,NEL    ,DEPSXX   ,DEPSYY   ,DEPSXY  ,
     9                  DEPSYZ ,DEPSZX ,SIGNXX   ,SIGNYY   ,SIGNXY  ,
     A                  SIGNYZ ,SIGNZX ,SIGBAKXX ,SIGBAKYY ,SIGBAKXY,
     B                  SIGOXX ,SIGOYY ,SIGOXY   ,SIGOYZ   ,SIGOZX  ,
     C                  VP     )
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IPLA,IRTY,ISRATE,NEL
      INTEGER,INTENT(IN) :: VP
C     REAL
      my_real
     .   EZZ(*), OFF(*), PLA(*),TEMP(*),Z3,Z4,
     .   ETSE(*),GS(*),EPSP(*)
      my_real
     .   A1, A2, G, YMAX0, 
     .   CA0, CB0, CN, YOUNG, YLD(MVSIZ),
     .   CC, EPDR(MVSIZ), NU, EPCHK(MVSIZ),TSTAR(MVSIZ),
     .   DPLA(MVSIZ),FISOKIN,GAMA_IMP(*),SIGNOR(MVSIZ,5),
     .   HARDM(*),DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),SIGBAKXX(NEL),SIGBAKYY(NEL),SIGBAKXY(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC
      INTEGER I, J, N, NINDX, INDEX(MVSIZ), NMAX,IKFLG
C     REAL
      my_real
     .   A(MVSIZ), B(MVSIZ)  , DPLA_I(MVSIZ), DPLA_J(MVSIZ), DR(MVSIZ),
     .   H(MVSIZ), NU1(MVSIZ), NU2(MVSIZ)   , P(MVSIZ)     , Q(MVSIZ), 
     .   SVM(MVSIZ),CA(MVSIZ), CB(MVSIZ)    , YMAX(MVSIZ)  ,
     .   SVM2(MVSIZ),YLD2(MVSIZ),HI(MVSIZ)  ,HK(MVSIZ)     ,LOGEP(MVSIZ),
     .   NU11(MVSIZ),NU21(MVSIZ),ANU1(MVSIZ),BNU2(MVSIZ),H2(MVSIZ),
     .   ERR,F,DF,PLA_I,P2,Q2,R,S1,S2,S3,YLD_I,NNU1,NNU2,
     .   NU3,NU4,NU5,NU6,SIGZ,PP,AA,BB,C,S11,S22,S12,S1S2,S122,UMR,
     .   VM2,QQ,SMALL,MT,TM,BETA,AAA ,HKIN,ALPHA,PLAP1
         
      DATA NMAX/3/      
C------------------------------------------------------------------
      NINDX = 0
      SMALL = EM7
      DO I=JFT,JLT
        ETSE(I) = ONE
c remise aux val. initiales
        CA(I)   = CA0
        CB(I)   = CB0 
        YMAX(I) = YMAX0 
        H(I)    = ZERO
!
        SIGNXX(I)=SIGOXX(I)
        SIGNYY(I)=SIGOYY(I)
        SIGNXY(I)=SIGOXY(I)
        SIGNYZ(I)=SIGOYZ(I)
        SIGNZX(I)=SIGOZX(I)
      ENDDO
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IKFLG=0
      IF (FISOKIN > 0) THEN
        DO I=JFT,JLT
          SIGNXX(I)=SIGNXX(I)-SIGBAKXX(I)
          SIGNYY(I)=SIGNYY(I)-SIGBAKYY(I)
          SIGNXY(I)=SIGNXY(I)-SIGBAKXY(I)
        ENDDO
        IKFLG=JLT
      END IF !(FISOKIN(I) > 0) THEN
C---------------------------
C     CONTRAINTES ELASTIQUES
C---------------------------
      DO I=JFT,JLT
        SIGNXX(I)=SIGNXX(I)+A1*DEPSXX(I)+A2*DEPSYY(I)
        SIGNYY(I)=SIGNYY(I)+A2*DEPSXX(I)+A1*DEPSYY(I)
        SIGNXY(I)=SIGNXY(I)+G*DEPSXY(I)
        SIGNYZ(I)=SIGNYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I)=SIGNZX(I)+GS(I)*DEPSZX(I)
      ENDDO
      DO I=JFT,JLT
        LOGEP(I) = ZERO
      ENDDO

C-------------
C     STRAIN RATE (JOHNSON-COOK, ZERILLI-ARMSTRONG)
C-------------
      IF (CC /= ZERO) THEN
        IF (IRTY == 0) THEN 
          DO I=JFT,JLT
            IF (ISRATE == 0.AND.VP == 2) EPSP(I) = 
     .           MAX( ABS(DEPSXX(I)), ABS(DEPSYY(I)), HALF*ABS(DEPSXY(I)))
            EPSP(I)  = MAX(EPSP(I),EPDR(I))
            LOGEP(I) = LOG(EPSP(I)/EPDR(I))
          ENDDO
#include "vectorize.inc"
          DO I=JFT,JLT
            MT = MAX(EM20,Z3)
            IF (TSTAR(I) == ZERO) THEN
              Q(I) = (ONE + CC * LOGEP(I))
            ELSE
              Q(I) = (ONE + CC * LOGEP(I))*(ONE-EXP(MT*LOG(TSTAR(I))))
            ENDIF
            Q(I) = MAX(Q(I),EM20)
            CA(I) = CA(I) * Q(I)
            CB(I) = CB(I) * Q(I)
            IF (ICC == 1) YMAX(I) = YMAX(I) * Q(I)
          ENDDO
        ELSEIF (IRTY == 1) THEN   ! Zerilli
          DO I=JFT,JLT
            IF (ISRATE == 0.AND.VP == 2) EPSP(I) = MAX( ABS(DEPSXX(I)),
     .                   ABS(DEPSYY(I)), HALF*ABS(DEPSXY(I)))
            EPSP(I) = MAX(EPSP(I),EM20)
            LOGEP(I) = LOG(EPSP(I)/EPDR(I))
          ENDDO
          DO I=JFT,JLT          
            Q(I) = LOGEP(I)
            Q(I) = CC*EXP((-Z3+Z4 * Q(I))*TEMP(I))
            IF (ICC == 1) YMAX(I)= YMAX(I) + Q(I)
            CA(I) = CA(I) + Q(I)
          ENDDO
        ENDIF ! IRTY
      ELSE           ! CC = ZERO  , no strain rate effect
        IF (IRTY == 0) THEN       ! J-C
          MT = MAX(EM20,Z3)
          DO I=JFT,JLT          
            IF (TSTAR(I) /= ZERO) THEN
              Q(I)  = ONE - EXP(MT*LOG(TSTAR(I)))
              Q(I)  = MAX(Q(I),EM20)
              CA(I) = CA(I) * Q(I)
              CB(I) = CB(I) * Q(I)
            END IF
          ENDDO
        ENDIF ! IRTY
      ENDIF   ! IF (CC /= ZERO)
C-----------------------------------
C     YIELD
C-----------------------------------
      DO I=JFT,JLT
        DPLA(I) = ZERO
        IF (PLA(I) == ZERO) THEN
           YLD(I)= CA(I)
        ELSE
           BETA = CB(I)*(ONE-FISOKIN)
           YLD(I)= CA(I)+BETA*EXP(CN*LOG(PLA(I)))
        ENDIF
        YLD(I)= MIN(YLD(I),YMAX(I))
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
      IF (IPLA == 0) THEN
C-------------------
C     CONTRAINTE VM
C-------------------
        DO I=JFT,JLT
          SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)
     .               +SIGNYY(I)*SIGNYY(I)
     .               -SIGNXX(I)*SIGNYY(I)
     .         +THREE*SIGNXY(I)*SIGNXY(I))
        ENDDO
C projection radiale 
#include "vectorize.inc" 
        DO I=JFT,JLT
          R = MIN(ONE,YLD(I)/(SVM(I)+EM15))
          IF (R < ONE) THEN 
            SIGNXX(I)=SIGNXX(I)*R
            SIGNYY(I)=SIGNYY(I)*R
            SIGNXY(I)=SIGNXY(I)*R
            DPLA(I) =  OFF(I) * MAX(ZERO,(SVM(I)-YLD(I))/YOUNG)
            S1=HALF*(SIGNXX(I)+SIGNYY(I))
            EZZ(I) = DPLA(I) * S1 /YLD(I)
            PLA(I) = PLA(I) + DPLA(I)
            EPCHK(I) = MAX(PLA(I),EPCHK(I))     
            IF (YLD(I) >=YMAX(I)) THEN
              H(I)=ZERO
            ELSE
              H(I)=CN*CB(I)*EXP((CN-ONE)*LOG(PLA(I)+SMALL))
            ENDIF
            ETSE(I)= H(I)/(H(I)+YOUNG)
          ENDIF
        ENDDO
C------------------------------------------------------------------------
      ELSEIF (IPLA == 1) THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
        DO  I=JFT,JLT
          S1=SIGNXX(I)+SIGNYY(I)
          S2=SIGNXX(I)-SIGNYY(I)
          S3=SIGNXY(I)
          A(I)=FOURTH*S1*S1
          B(I)=THREE_OVER_4*S2*S2+THREE*S3*S3
          SVM(I)=SQRT(A(I)+B(I))  
        ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
        NINDX=0
        DO I=JFT,JLT
          IF (SVM(I) > YLD(I) .AND. OFF(I) == ONE) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ENDIF
        ENDDO
        IF (NINDX == 0) THEN
          IF (IKFLG > 0) THEN
            DO I=JFT,JLT
              SIGNXX(I)=SIGNXX(I) + SIGBAKXX(I)
              SIGNYY(I)=SIGNYY(I) + SIGBAKYY(I)
              SIGNXY(I)=SIGNXY(I) + SIGBAKXY(I)
            ENDDO
          END IF !(IKFLG > 0) THEN
          IF (IMPL_S > 0.AND.IKT > 0) THEN
            DO I = JFT,JLT
             GAMA_IMP(I) = ZERO
            END DO
          END IF !(IMPL_S > 0.AND.IKT > 0) THEN
          RETURN
        END IF !(NINDX==0) THEN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
#include "vectorize.inc" 
        DO  J=1,NINDX
          I=INDEX(J)
          NU1(I)=ONE/(ONE-NU)
          NU2(I)=ONE/(ONE+NU)
          IF (YLD(I) >= YMAX(I)) THEN
            H(I)=ZERO
          ELSE
            H(I)=CN*CB(I)*EXP((CN-ONE)*LOG(PLA(I)+SMALL))
          ENDIF
          DPLA_J(I)=(SVM(I)-YLD(I))/(THREE*G+H(I))
          ETSE(I)= H(I)/(H(I)+YOUNG)
          ANU1(I)  = A(I)*NU1(I)
          BNU2(I) = THREE*B(I)*NU2(I)
          H2(I)= TWO*H(I)
        ENDDO
C-----just keep at least old performance--to update H(I) inside iteration for implicit+ETSE---------
        IF (IKFLG == 0) THEN
          DO N=1,NMAX
#include "vectorize.inc" 
            DO J=1,NINDX
              I=INDEX(J)
              DPLA_I(I)=DPLA_J(I)
              PLA_I =PLA(I)+DPLA_I(I)
              DPLA(I) = DPLA_J(I)
              IF (PLA_I == ZERO) THEN
                YLD_I =MIN(YMAX(I),CA(I))
              ELSE
                YLD_I =MIN(YMAX(I),CA(I)+CB(I)*EXP(CN*LOG(PLA_I)))
              ENDIF
              DR(I) =HALF*YOUNG*DPLA_I(I)/YLD_I
              P(I)  =ONE/(ONE+DR(I)*NU1(I))
              Q(I)  =ONE/(ONE+THREE*DR(I)*NU2(I))
              P2    =P(I)*P(I)
              Q2    =Q(I)*Q(I)
              F     =A(I)*P2+B(I)*Q2-YLD_I*YLD_I
              DF    =-(ANU1(I)*P2*P(I)+BNU2(I)*Q2*Q(I))
     .               *(YOUNG-DR(I)*H2(I))/YLD_I
     .                -H2(I)*YLD_I
              IF (DPLA_I(I) > ZERO) THEN
                DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
              ELSE
                DPLA_J(I)=ZERO
              ENDIF        
            ENDDO ! DO J=1,NINDX
          ENDDO ! DO N=1,NMAX
C----------kinematic&mix hardening      
        ELSE   ! IF (IKFLG /= 0) 
#include "vectorize.inc" 
          DO  J=1,NINDX
            I=INDEX(J)
            BETA = H(I)*FISOKIN
            HI(I) = H(I)-BETA
            HK(I) = TWO_THIRD*BETA
            AAA = THREE*HK(I)/YOUNG
            NU11(I) = NU1(I) + AAA 
            NU21(I) = THREE*NU2(I) + AAA 
            ANU1(I) = A(I)*NU11(I)
            BNU2(I) = B(I)*NU21(I)
            H2(I)= TWO*HI(I)
          ENDDO
!
          DO N=1,NMAX
#include "vectorize.inc" 
            DO J=1,NINDX
              I=INDEX(J)
              DPLA_I(I)=DPLA_J(I)
              PLA_I =PLA(I)+DPLA_I(I)
              DPLA(I) = DPLA_J(I)
              BETA = ONE-FISOKIN
              IF (PLA_I == ZERO) THEN
                YLD_I =MIN(YMAX(I),CA(I))
              ELSE
                YLD_I =MIN(YMAX(I),CA(I)+BETA*CB(I)*EXP(CN*LOG(PLA_I)))
              ENDIF
              DR(I) =HALF*YOUNG*DPLA_I(I)/YLD_I
              P(I)  =ONE/(ONE+DR(I)*NU11(I))
              Q(I)  =ONE/(ONE+DR(I)*NU21(I))
              P2    =P(I)*P(I)
              Q2    =Q(I)*Q(I)
              F     =A(I)*P2+B(I)*Q2-YLD_I*YLD_I
              DF    =-(ANU1(I)*P2*P(I)+BNU2(I)*Q2*Q(I))
     .               *(YOUNG-DR(I)*H2(I))/YLD_I
     .                -H2(I)*YLD_I
              IF (DPLA_I(I) > ZERO) THEN
                DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
              ELSE
                DPLA_J(I)=ZERO
              ENDIF
            ENDDO ! DO J=1,NINDX
          ENDDO ! DO N=1,NMAX
        ENDIF !(IKFLG == 0) 
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc" 
        DO J=1,NINDX
          I=INDEX(J)
          PLA(I) = PLA(I) + DPLA_I(I)
          EPCHK(I) = MAX(PLA(I),EPCHK(I))
          S1=(SIGNXX(I)+SIGNYY(I))*P(I)
          S2=(SIGNXX(I)-SIGNYY(I))*Q(I)
          SIGNXX(I)=HALF*(S1+S2)
          SIGNYY(I)=HALF*(S1-S2)
          SIGNXY(I)=SIGNXY(I)*Q(I)
          EZZ(I) = DR(I)*S1/YOUNG
        ENDDO
C-------------------------
      ELSEIF (IPLA == 2) THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
        DO I=JFT,JLT
          SVM2(I)= SIGNXX(I)*SIGNXX(I)
     .       +SIGNYY(I)*SIGNYY(I)
     .       -SIGNXX(I)*SIGNYY(I)
     .       +THREE*SIGNXY(I)*SIGNXY(I)
          SVM(I)=SQRT(SVM2(I))  
        END DO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
        NINDX=0
        DO I=JFT,JLT
          YLD2(I)=YLD(I)*YLD(I)
          IF (SVM2(I) > YLD2(I) .AND. OFF(I) == ONE) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          END IF
        END DO
!
        IF (NINDX /= 0) THEN
C-------------
C   PROJ NORMALE AU CRITERE AVEC CALCUL APPROCHE DE LA NORMALE + RETOUR RADIAL
C-------------
#include "vectorize.inc" 
          DO J=1,NINDX
            I=INDEX(J)
            IF (YLD(I) >= YMAX(I)) THEN
              H(I)=ZERO
            ELSE
              H(I)=CN*CB(I)*EXP((CN-ONE)*LOG(PLA(I)+SMALL))
            ENDIF
            ETSE(I)= H(I)/(H(I)+YOUNG)
C
            AA=(SVM2(I)-YLD2(I))
     .        /(FIVE*SVM2(I)+THREE*(-SIGNXX(I)*SIGNYY(I)+SIGNXY(I)*SIGNXY(I)))
            S1=(ONE-TWO*AA)*SIGNXX(I)+       AA*SIGNYY(I)
            S2=AA*SIGNXX(I)+(ONE-TWO*AA)*SIGNYY(I)
            S3=(ONE-THREE*AA)*SIGNXY(I)
            SIGNXX(I)=S1
            SIGNYY(I)=S2
            SIGNXY(I)=S3
            DPLA(I) = OFF(I)*(SVM(I)-YLD(I))/(THREE*G+H(I))
            PLA(I) = PLA(I) + DPLA(I)
C
            YLD(I) =YLD(I)+H(I)*DPLA(I)
          END DO ! DO J=1,NINDX
C
#include "vectorize.inc"
          DO J=1,NINDX
            I=INDEX(J)
            SVM(I)=SQRT( SIGNXX(I)*SIGNXX(I)
     .                  +SIGNYY(I)*SIGNYY(I)
     .                  -SIGNXX(I)*SIGNYY(I)
     .            +THREE*SIGNXY(I)*SIGNXY(I))
            R  = MIN(ONE,YLD(I)/MAX(EM20,SVM(I)))
            SIGNXX(I)=SIGNXX(I)*R
            SIGNYY(I)=SIGNYY(I)*R
            SIGNXY(I)=SIGNXY(I)*R
            EZZ(I) = DPLA(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
          END DO ! DO J=1,NINDX
        END IF ! IF (NINDX /= 0)
      ENDIF ! IF (IPLA == 0)
C------------------------------------------
C     for tangent matrix
C------------------------------------------
      IF (IMPL_S > 0) THEN
        IF (IKT > 0) THEN
          DO I = JFT,JLT
            IF (DPLA(I) > ZERO) THEN
c ...... Parameter d(gama)
              PLA_I =PLA(I)
              BETA = ONE-FISOKIN
              YLD(I) =MIN(YMAX(I),CA(I)+BETA*CB(I)*EXP(CN*LOG(PLA_I)))
              GAMA_IMP(I)= THREE_HALF*DPLA(I)/YLD(I)
c ...... HK,HH---
              SIGNOR(I,4)=FISOKIN*H(I)
              SIGNOR(I,5)=(ONE-FISOKIN)*H(I)
c ...... Deviatoric stresses shifted by modified back stress ->ksi
              SIGNOR(I,1)=THIRD*(TWO*SIGNXX(I)-SIGNYY(I))
              SIGNOR(I,2)=THIRD*(TWO*SIGNYY(I)-SIGNXX(I))
              SIGNOR(I,3)=TWO*SIGNXY(I)
            ELSE
              GAMA_IMP(I) = ZERO
            END IF ! IF (DPLA(I) > ZERO)
          END DO
        END IF ! (IKT > 0) THEN
      END IF ! IF (IMPL_S > 0)
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IF (IKFLG > 0) THEN
        IF (IPLA == 1 )THEN
#include "vectorize.inc" 
          DO J=1,NINDX
            I=INDEX(J)
            PLA_I =PLA(I)
            BETA = ONE-FISOKIN
C------YLD, H(I) should not be updated----
            IF (PLA_I == ZERO) THEN
              YLD(I) =CA(I)
            ELSE
              YLD(I) =MIN(YMAX(I),CA(I)+BETA*CB(I)*EXP(CN*LOG(PLA_I)))
            ENDIF
          END DO ! DO J=1,NINDX
        END IF ! (IPLA == 1) THEN
        DO I=JFT,JLT
          HKIN = FISOKIN*H(I)
          ALPHA = HKIN*DPLA(I)/YLD(I)
          SIGBAKXX(I) = SIGBAKXX(I) + ALPHA*SIGNXX(I)
          SIGBAKYY(I) = SIGBAKYY(I) + ALPHA*SIGNYY(I) 
          SIGBAKXY(I) = SIGBAKXY(I) + ALPHA*SIGNXY(I) 
C
          SIGNXX(I)=SIGNXX(I) + SIGBAKXX(I)
          SIGNYY(I)=SIGNYY(I) + SIGBAKYY(I)
          SIGNXY(I)=SIGNXY(I) + SIGBAKXY(I)
        ENDDO

      END IF !(IKFLG > 0) THEN
C
C--------------------------------
C     HARDENING MODULUS
C--------------------------------
      DO I=JFT,JLT
        HARDM(I) = H(I)
      ENDDO
C---
      RETURN
      END
