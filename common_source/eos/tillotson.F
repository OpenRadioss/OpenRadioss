Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    tillotson_mod   ../common_source/eos/tillotson.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE TILLOTSON_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    tillotson       ../common_source/eos/tillotson.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      SUBROUTINE TILLOTSON(
     1                     IFLAG,NEL  ,PMIN ,OFF   ,EINT    ,MU    , MU2,
     2                     ESPE ,DVOL ,DF   ,VNEW  ,PSH     ,
     3                     PNEW ,DPDM ,DPDE ,VAREOS, NVAREOS, EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE CONSTANT_MOD , ONLY : ZERO, EM15, HALF, ONE, TWO
        USE EOS_PARAM_MOD , ONLY : eos_param_
        use precision_mod , only : WP
        IMPLICIT NONE
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
!----------------------------------------------------------------------------
!! \details STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
!! \details COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!! \details
!! \details  STAGGERED SCHEME
!! \details     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!! \details     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!! \details     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!! \details     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!! \details                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!! \details                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!! \details  COLLOCATED SCHEME
!! \details     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------
C
C Solver for Tillotson Equation of State.
C This equation of state depends of material state. It has several region in (P,v) diagram :
C        REGION=1 : compression
C        REGION=2 : cold expansion
C        REGION=3 : transition
C        REGION=4 : hot expansion
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) ::  IFLAG, NEL, NVAREOS
      real(kind=WP) :: PMIN,
     .           OFF(NEL) , EINT(NEL), MU(NEL)  ,
     .           MU2(NEL) , ESPE(NEL), DVOL(NEL), DF(NEL),
     .           VNEW(NEL), PNEW(NEL), DPDM(NEL), DPDE(NEL)
      real(kind=WP),INTENT(INOUT) :: VAREOS(NEL,NVAREOS)
      real(kind=WP),INTENT(INOUT) :: PSH(NEL)
      TYPE(EOS_PARAM_),INTENT(IN) :: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      real(kind=WP) :: A, B, AA, BB, dAA, dBB, DVV, ETA, ETA2, ENEW,  XX, PP,
     .                 C1,C2(NEL),EREF,
     .                 ALPHA,BETA,EIV,ECV,VIV,
     .                 REGION(NEL)
      real(kind=WP) :: W !< omega
      real(kind=WP) :: W2 !< omega**2
      real(kind=WP) :: P12,P3,P4
      real(kind=WP) :: DE_P12, DE_P3, DE_P4 ! partial derivatives dP/dE at constant mu       -> region 2 same eqns as region 1 with C2=0
      real(kind=WP) :: DM_P12, DM_P3, DM_P4 ! partial derivatives dP/d(mu) at constant E     -> region 2 same eqns as region 1 with C2=0
      real(kind=WP) :: DPDM12, DPDM3, DPDM4 ! total derivatives dP/d(mu) at constant E       -> region 2 same eqns as region 1 with C2=0
      real(kind=WP) :: DE_W  ! partial derivative dW/dE at constant mu
      real(kind=WP) :: DN_W  ! partial derivative dW/d(mu) at constant E
      real(kind=WP) :: C1EB, EAX, C1EBEAX, ABW, ABWX, ETAE, DENOM ! tmp variables for cpu cost reduction
      real(kind=WP) :: KK !< interpolation coefficient for region 3
      real(kind=WP) :: I12, I4 !< indicator functions for region 1|2 and 4
      real(kind=WP) :: TMP3
C--------------------------------------------------------------------
      ! -- USER PARAMETERS --
      C1    = EOS_STRUCT%UPARAM(1)
      C2(1:NEL) = EOS_STRUCT%UPARAM(2)
      A     = EOS_STRUCT%UPARAM(3)
      B     = EOS_STRUCT%UPARAM(4)
      EREF  = EOS_STRUCT%UPARAM(5)
      ECV   = EOS_STRUCT%UPARAM(6)
      VIV   = EOS_STRUCT%UPARAM(7)
      ALPHA = EOS_STRUCT%UPARAM(8)
      BETA  = EOS_STRUCT%UPARAM(9)
      EIV   = EOS_STRUCT%UPARAM(10)
      PSH(1:NEL) = EOS_STRUCT%PSH

      ! -- AREA CRITERIA FOR REGIONS IDENTIFICATION --
      ! ------ 1: compression
      ! ------ 2: cold expansion
      ! ------ 3: transition (currently empty but can be implemented)
      ! ------ 4: hot expansion
      IF(IFLAG /= 1)THEN
        REGION(1:NEL) = 1
        DO I=1,NEL
          IF(MU(I) < ZERO)THEN
            IF(DF(I) > VIV)THEN
              REGION(I) = 4
            ELSEIF(ESPE(I) >= ECV)THEN
              REGION(I) = 4
            ELSEIF(ESPE(I) < EIV)THEN
              REGION(I) = 2
              C2(I) = ZERO
            ELSE
              REGION(I) = 3
              C2(I) = ZERO
            ENDIF
          ENDIF
        ENDDO
        VAREOS(1:NEL,1) = REGION(1:NEL) ! output
      ELSE
        REGION(1:NEL) = VAREOS(1:NEL,1)
      ENDIF


      IF(IFLAG == 0) THEN  !P(mu[n+1],E[n]) , Frist Step staggered scheme semi-implicit integration

        DO I=1,NEL

          ETA = ONE+MU(I)
          ETA2 = ETA*ETA
          ETAE = ETA*ESPE(I)
          W = ONE+ESPE(I)/(EREF*ETA2)
          W2 = W*W
          ABW = (A+B/W)
          DE_W = ONE/(EREF*ETA2)
          DN_W = -TWO*ESPE(I)/(EREF*ETA2*ETA)
          I12 = ZERO
          I4 = ZERO
          TMP3 = ZERO
          P12 =  ZERO ; P4 = ZERO
          DPDM12 = ZERO ; DPDM4 = ZERO
          DE_P12 = ZERO ; DE_P4 = ZERO

          IF(REGION(I) <= 3)THEN
            P12 = ABW*ETAE+C1*MU(I)+C2(I)*MU2(I)
            P12 = MAX(P12,PMIN)*OFF(I)
            DE_P12 = (-b*DE_W*ETAE)/W2 + ABW*ETA
            DM_P12 = (-B*DN_W*ETAE)/W2 + ABW*ESPE(I) + C1 + TWO*C2(I)*MU(I)
            DPDM12 = DM_P12 + P12/ETA2*DE_P12
            I12 = ONE
          ENDIF

          IF(REGION(I) >= 3)THEN
            XX = MU(I)/(ONE+MU(I))
            C1EB = C1*exp(beta*xx)
            EAX = exp(-alpha*XX*XX)
            C1EBEAX = C1EB*EAX
            ABWX = (A+B*EAX/W)
            P4 = C1EB*EAX*MU(I) + ABWX*ETAE
            P4 = MAX(P4,PMIN)*OFF(I)
            DE_P4 = -B*EAX*DE_W*ETAE/W2 + ABWX*ETA
            DM_P4 = BETA/ETA2*C1EBEAX*MU(I) - TWO*C1EBEAX*ALPHA*XX*MU(I)/ETA2 + C1EBEAX
     .              +ETAE*(-TWO*B*ALPHA*XX*EAX/W/ETA2 - B*EAX*DN_W/W2) + ABWX*ESPE(I)
            DPDM4 = DM_P4 + P4/ETA2*DE_P4
            I4 = ONE
          END IF

          IF(REGION(I) == 3)THEN
            DENOM = ECV-EIV
            KK = (ESPE(I)-EIV)/DENOM
            !KK = MIN(MAX(ZERO,KK) , ONE)
            P3 = (ONE-KK)*P12 + KK*P4
            TMP3 = P4/DENOM - P12/DENOM
            I12 = (ONE-KK)
            I4 = KK
          ENDIF

          PNEW(I) = I12*P12 + I4*P4
          DPDM(I) = I12*DPDM12 + I4*DPDM4
          DPDE(I) = I12*DE_P12 + I4*DE_P4 + TMP3

          PNEW(I) = (MAX(PNEW(I), PMIN)-PSH(I))*OFF(I)
          DPDM(I) = DPDM(I) * OFF(I)
          DPDE(I) = DPDE(I) * OFF(I)

        ENDDO

       ELSEIF(IFLAG == 1) THEN !P(mu[n+1],E[n]) , Second Step staggered scheme semi-implicit integration

        DO I=1,NEL

          DVV = HALF*DVOL(I)*DF(I) / MAX(EM15, VNEW(I))

          ETA  = ONE + MU(I)
          ETA2 = ETA*ETA

          I12  = ZERO
          I4   = ZERO
          TMP3 = ZERO

          P12 = ZERO ; P4 = ZERO ; P3 = ZERO
          DE_P12 = ZERO ; DM_P12 = ZERO
          DE_P4  = ZERO ; DM_P4  = ZERO
          DPDM12 = ZERO ; DPDM4  = ZERO

          ! ==========================================================
          ! REGION 1/2/3
          ! ==========================================================
          IF (REGION(I) <= 2) THEN

            ETAE = ETA*ESPE(I)
            W    = ONE + ESPE(I)/(EREF*ETA2)
            W2   = W*W
            ABW  = (A + B/W)

            DE_W = ONE/(EREF*ETA2)
            DN_W = -TWO*ESPE(I)/(EREF*ETA2*ETA)

            ! Raw P* at (mu, E*)
            P12 = ABW*ETAE + C1*MU(I) + C2(I)*MU2(I)

            ! Partials at (mu, E*)
            DE_P12 = (-B*DE_W*ETAE)/W2 + ABW*ETA
            DM_P12 = (-B*DN_W*ETAE)/W2 + ABW*ESPE(I) + C1 + TWO*C2(I)*MU(I)

            DENOM = ONE + DVV*DE_P12
            DENOM = SIGN(MAX(ABS(DENOM),EM15), DENOM)

            ! CN closure
            P12    = P12    / DENOM
            DPDE(I)= DE_P12 / DENOM
            !DPDM12 = DM_P12 / DENOM
            DPDM12 = DM_P12 + P12/ETA2 * DE_P12

            I12 = ONE
          ENDIF

          ! ==========================================================
          ! REGION 4
          ! ==========================================================
          IF (REGION(I) == 4) THEN

            XX = MU(I)/ETA
            C1EB = C1*EXP(BETA*XX)
            EAX  = EXP(-ALPHA*XX*XX)
            C1EBEAX = C1EB*EAX

            ETAE = ETA*ESPE(I)
            W    = ONE + ESPE(I)/(EREF*ETA2)
            W2   = W*W
            ABWX = (A + B*EAX/W)

            ! Raw P* at (mu, E*)
            P4 = C1EBEAX*MU(I) + ABWX*ETAE

            DE_W = ONE/(EREF*ETA2)
            DN_W = -TWO*ESPE(I)/(EREF*ETA2*ETA)

            ! Partials at (mu, E*)
            DE_P4 = -B*EAX*DE_W*ETAE/W2 + ABWX*ETA
            DM_P4 =  BETA/ETA2*C1EBEAX*MU(I) - TWO*C1EBEAX*ALPHA*XX*MU(I)/ETA2 + C1EBEAX
     .              + ETAE*(-TWO*B*ALPHA*XX*EAX/W/ETA2 - B*EAX*DN_W/W2) + ABWX*ESPE(I)

            DENOM = ONE + DVV*DE_P4
            DENOM = SIGN(MAX(ABS(DENOM),EM15), DENOM)

            ! CN closure
            P4     = P4     / DENOM
            DPDM4  = DM_P4  + P4/ETA2  * DE_P4
            DE_P4  = DE_P4  / DENOM

            I4 = ONE
          ENDIF

          ! ==========================================================
          ! REGION 3 (transition)
          ! ==========================================================
          IF (REGION(I) == 3) THEN
            XX = MU(I)/ETA
            C1EB = C1*EXP(BETA*XX)
            EAX  = EXP(-ALPHA*XX*XX)
            C1EBEAX = C1EB*EAX
            ETAE = ETA*ESPE(I)
            W    = ONE + ESPE(I)/(EREF*ETA2)
            W2   = W*W
            ABWX = (A + B*EAX/W)
            ABW  = (A + B/W)

            DE_W = ONE/(EREF*ETA2)
            DN_W = -TWO*ESPE(I)/(EREF*ETA2*ETA)

            DENOM = ECV - EIV
            KK = (ESPE(I) - EIV) / DENOM
            !KK = MIN(MAX(ZERO,KK) , ONE)

            P12 = ABW*ETAE + C1*MU(I) + C2(I)*MU2(I)
            DE_P12 = (-B*DE_W*ETAE)/W2 + ABW*ETA
            DM_P12 = (-B*DN_W*ETAE)/W2 + ABW*ESPE(I) + C1 + TWO*C2(I)*MU(I)

            P4 = C1EBEAX*MU(I) + ABWX*ETAE
            DE_P4 = -B*EAX*DE_W*ETAE/W2 + ABWX*ETA
            DM_P4 =  BETA/ETA2*C1EBEAX*MU(I) - TWO*C1EBEAX*ALPHA*XX*MU(I)/ETA2 + C1EBEAX
     .              + ETAE*(-TWO*B*ALPHA*XX*EAX/W/ETA2 - B*EAX*DN_W/W2) + ABWX*ESPE(I)

            TMP3 = P4/DENOM - P12/DENOM
            DE_P3 = (ONE-KK)*DE_P12 + KK*DE_P4 + TMP3
            DM_P3 = (ONE-KK)*DM_P12 + KK*DM_P4
            P3    = (ONE-KK)*P12    + KK*P4

            DENOM = ONE + DVV*DE_P3
            DENOM = SIGN(MAX(ABS(DENOM),EM15), DENOM)

            P3     = P3     / DENOM
            DPDM3  = DM_P3  + P3/ETA2  * DE_P3
            DE_P3  = DE_P3  / DENOM

            ! For sound-speed derivatives, blend the CN-consistent slopes
            DPDE(I) = DE_P3
            DPDM(I) = DPDM3
            PNEW(I) = P3
          ELSE
            ! Non-transition regions
            PNEW(I) = I12*P12 + I4*P4
            DPDM(I) = I12*DPDM12 + I4*DPDM4
            DPDE(I) = I12*DE_P12 + I4*DE_P4
          ENDIF

          ! ==========================================================
          ! Apply PMIN/OFF
          ! ==========================================================
          PNEW(I) = (MAX(PNEW(I), PMIN)-PSH(I))*OFF(I)
          DPDM(I) = DPDM(I) * OFF(I)
          DPDE(I) = DPDE(I) * OFF(I)

          ! Finish CN energy update
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I) + PSH(I))

        ENDDO

      ELSEIF(IFLAG == 2) THEN ! collocated scheme, explicit integration

        DO I=1,NEL

         IF(VNEW(I) > ZERO)THEN

          ETA = ONE+MU(I)
          ETA2 = ETA*ETA
          ETAE = ETA*ESPE(I)
          W = ONE+ESPE(I)/(EREF*ETA2)
          W2 = W*W
          ABW = (A+B/W)
          DE_W = ONE/(EREF*ETA2)
          DN_W = -TWO*ESPE(I)/(EREF*ETA2*ETA)
          I12 = ZERO
          I4 = ZERO
          P12 =  ZERO ; P4 = ZERO
          DPDM12 = ZERO ; DPDM4 = ZERO
          DE_P12 = ZERO ; DE_P4 = ZERO
          TMP3 = ZERO

          IF(REGION(I) <= 3)THEN
            P12 = ABW*ETAE+C1*MU(I)+C2(I)*MU2(I)
            P12 = MAX(P12,PMIN)*OFF(I)
            DE_P12 = (-b*DE_W*ETAE)/W2 + ABW*ETA
            DM_P12 = (-B*DN_W*ETAE)/W2 + ABW*ESPE(I) + C1 + TWO*C2(I)*MU(I)
            DPDM12 = DM_P12 + P12/ETA2*DE_P12
            I12 = ONE
          ENDIF

          IF(REGION(I) >= 3)THEN
            XX = MU(I)/(ONE+MU(I))
            C1EB = C1*exp(beta*xx)
            EAX = exp(-alpha*XX*XX)
            C1EBEAX = C1EB*EAX
            ABWX = (A+B*EAX/W)
            P4 = C1EB*EAX*MU(I) + ABWX*ETAE
            P4 = MAX(P4,PMIN)*OFF(I)
            DE_P4 = -B*EAX*DE_W*ETAE/W2 + ABWX*ETA
            DM_P4 = BETA/ETA2*C1EBEAX*MU(I) - TWO*C1EBEAX*ALPHA*XX*MU(I)/ETA2 + C1EBEAX
     .              +ETAE*(-TWO*B*ALPHA*XX*EAX/W/ETA2 - B*EAX*DN_W/W2) + ABWX*ESPE(I)
            DPDM4 = DM_P4 + P4/ETA2*DE_P4
            I4 = ONE
          END IF

          IF(REGION(I) == 3)THEN
            DENOM = ECV-EIV
            KK = (ESPE(I)-EIV)/DENOM
            P3 = (ONE-KK)*P12 + KK*P4
            TMP3 = P4/DENOM - P12/DENOM
            I12 = (ONE-KK)
            I4 = KK
          ENDIF

          PNEW(I) = I12*P12 + I4*P4
          DPDM(I) = I12*DPDM12 + I4*DPDM4
          DPDE(I) = I12*DE_P12 + I4*DE_P4 + TMP3

          PNEW(I) = (MAX(PNEW(I), PMIN)-PSH(I))*OFF(I)
          DPDM(I) = DPDM(I) * OFF(I)
          DPDE(I) = DPDE(I) * OFF(I)

         ENDIF! (VNEW(I) > ZERO)

        ENDDO

      ENDIF

      RETURN
      END SUBROUTINE TILLOTSON
      END MODULE TILLOTSON_MOD
