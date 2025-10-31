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
!||    idealgas_vt_mod   ../common_source/eos/idealgas_vt.F
!||--- called by ------------------------------------------------------
!||    eosmain           ../common_source/eos/eosmain.F
!||    eosmain51         ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE IDEALGAS_VT_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    idealgas_vt     ../common_source/eos/idealgas_vt.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
      SUBROUTINE IDEALGAS_VT(
     1                       IFLAG ,NEL  ,PMIN ,OFF  ,EINT ,MU  ,MU2 ,
     2                       ESPE  ,DVOL ,DF   ,VNEW ,PSH  ,RHO0, RHO,
     3                       PNEW  ,DPDM ,DPDE ,THETA,POLD  ,EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE CONSTANT_MOD , ONLY : ZERO, EM05, HALF, ONE, TWO, THIRD, FOURTH, ONE_FIFTH
        USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of IDEAL GAS EOS with variable Cp(T) parameter
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      IMPLICIT NONE
#include    "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IFLAG, NEL
      my_real,INTENT(IN) :: PMIN,OFF(NEL),MU(NEL)
      my_real,INTENT(IN) :: MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL) ,DF(NEL), VNEW(NEL)
      my_real, INTENT(INOUT) :: PSH(NEL),PNEW(NEL),DPDM(NEL),DPDE(NEL),EINT(NEL),THETA(NEL)
      my_real,INTENT(IN) :: POLD(NEL), RHO0(NEL), RHO(NEL)
      TYPE(EOS_PARAM_),INTENT(IN) :: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, MX
        my_real :: P0,GAMMA
        my_real :: r_GAS,A0,A1,A2,A3,A4,CP(NEL),CV
        my_real :: TEMP, FUN, DFUN, TOL, ERROR, INCR
        INTEGER :: ITER, MAX_ITER
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      MAX_ITER = 50
      TOL = EM05

      CP(1:NEL)=ZERO
      CV    = EOS_STRUCT%CV
      PSH   = EOS_STRUCT%PSH
      A0    = EOS_STRUCT%UPARAM(1)
      A1    = EOS_STRUCT%UPARAM(2)
      A2    = EOS_STRUCT%UPARAM(3)
      A3    = EOS_STRUCT%UPARAM(4)
      A4    = EOS_STRUCT%UPARAM(5)
      r_GAS = EOS_STRUCT%UPARAM(6)
      P0    = EOS_STRUCT%UPARAM(7)

      IF(IFLAG == 0) THEN
        !SOLVE TEMPERATURE
        DO I=1,NEL
          ! Init newton
          TEMP = POLD(I)/RHO(I)/r_GAS
          ITER = 0
          ERROR = HUGE(ZERO)
          DO WHILE(ERROR > TOL .AND. ITER < MAX_ITER)
             ! f(x) = 0 <=> int(cv(T), T) - eint = 0
             ! <=> int(cp(T) - r_gas, T) - eint = 0
             FUN = A0 * TEMP + HALF * A1 * TEMP**2 + THIRD * A2 * TEMP**3 +
     .            FOURTH * A3 * TEMP**4 + ONE_FIFTH * A4 * TEMP**5 - r_GAS * TEMP - ESPE(I) / RHO0(I)
             IF (ABS(FUN) < TOL) EXIT
             DFUN = A0 +  A1 * TEMP + A2 * TEMP**2 + A3 * TEMP**3 + A4 * TEMP**4 - r_GAS
             INCR = - FUN / DFUN
             TEMP = TEMP + INCR
             ERROR = ABS(INCR / TEMP)
             ITER = ITER + 1
          ENDDO
          ! Store
          THETA(I) = TEMP
          CP(I) = A0 + A1 * TEMP + A2 * TEMP**2 + A3 * TEMP**3 + A4 * TEMP**4
       ENDDO
       DO I=1,NEL
         CV = CP(I) - r_GAS
         GAMMA = CP(I)/CV
         DPDM(I) = RHO0(I)*GAMMA*r_GAS*THETA(I)     !total derivative
         DPDE(I) = GAMMA*(ONE+MU(I))
         PNEW(I) = RHO0(I)*(ONE+MU(I))*r_GAS*THETA(I)    ! P(mu[n+1],E[n])
         PNEW(I) = PNEW(I)-PSH(I)
       ENDDO
         
C-----------------------------------------------
      ELSEIF(IFLAG == 1) THEN
        !SOLVE TEMPERATURE
        DO I=1,NEL
          ! Init newton
          TEMP = POLD(I)/RHO(I)/r_GAS
          ITER = 0
          ERROR = HUGE(ZERO)
          DO WHILE(ERROR > TOL .AND. ITER < MAX_ITER)
             ! f(x) = 0 <=> int(cv(T), T) - eint = 0
             ! <=> int(cp(T) - r_gas, T) - eint = 0
             FUN = A0 * TEMP + HALF * A1 * TEMP**2 + THIRD * A2 * TEMP**3 +
     .            FOURTH * A3 * TEMP**4 + ONE_FIFTH * A4 * TEMP**5 - r_GAS * TEMP - ESPE(I) / RHO0(I)
             IF (ABS(FUN) < TOL) EXIT
             DFUN = A0 +  A1 * TEMP + A2 * TEMP**2 + A3 * TEMP**3 + A4 * TEMP**4 - r_GAS
             INCR = - FUN / DFUN
             TEMP = TEMP + INCR
             ERROR = ABS(INCR / TEMP)
             ITER = ITER + 1
          ENDDO
          ! Store
          THETA(I) = TEMP
          CP(I) = A0 + A1 * TEMP + A2 * TEMP**2 + A3 * TEMP**3 + A4 * TEMP**4
        ENDDO
        DO I=1,NEL
          PNEW(I) = RHO0(I)*(ONE+MU(I))*r_GAS*THETA(I)    ! P(mu[n+1],E[n+1])
          EINT(I) = EINT(I) - HALF*DVOL(I)*PNEW(I)
          PNEW(I) = PNEW(I)-PSH(I)
          CV = CP(I) - r_GAS
          GAMMA = CP(I)/CV
          DPDE(I) = GAMMA*(ONE+MU(I))
        ENDDO

      ELSEIF (IFLAG == 2) THEN
        !SOLVE TEMPERATURE
        DO I=1,NEL
          ! Init newton
          TEMP = POLD(I)/RHO(I)/r_GAS
          ITER = 0
          ERROR = HUGE(ZERO)
          DO WHILE(ERROR > TOL .AND. ITER < MAX_ITER)
             ! f(x) = 0 <=> int(cv(T), T) - eint = 0
             ! <=> int(cp(T) - r_gas, T) - eint = 0
             FUN = A0 * TEMP + HALF * A1 * TEMP**2 + THIRD * A2 * TEMP**3 +
     .            FOURTH * A3 * TEMP**4 + ONE_FIFTH * A4 * TEMP**5 - r_GAS * TEMP - ESPE(I) / RHO0(I)
             IF (ABS(FUN) < TOL) EXIT
             DFUN = A0 +  A1 * TEMP + A2 * TEMP**2 + A3 * TEMP**3 + A4 * TEMP**4 - r_GAS
             INCR = - FUN / DFUN
             TEMP = TEMP + INCR
             ERROR = ABS(INCR / TEMP)
             ITER = ITER + 1
          ENDDO
          ! Store
          THETA(I) = TEMP
          CP(I) = A0 + A1 * TEMP + A2 * TEMP**2 + A3 * TEMP**3 + A4 * TEMP**4
        ENDDO
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
            CV = CP(I) - r_GAS
            GAMMA = CP(I)/CV
            DPDM(I) = RHO0(I)*GAMMA*r_GAS*THETA(I) !total derivative
            DPDE(I) = GAMMA*(ONE+MU(I)) !partial derivative
            PNEW(I) = RHO0(I)*(ONE+MU(I))*r_GAS*THETA(I)
          ENDIF
        ENDDO

        ENDIF
C-----------------------------------------------
        RETURN
      END SUBROUTINE IDEALGAS_VT
      END MODULE IDEALGAS_VT_MOD
