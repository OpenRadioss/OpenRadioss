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
      !||    idealgas_vt   ../common_source/eos/idealgas_vt.F
      !||--- called by ------------------------------------------------------
      !||    eosmain       ../common_source/eos/eosmain.F
      !||====================================================================
      SUBROUTINE IDEALGAS_VT(IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2 , 
     2                       ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                       PNEW ,DPDM ,DPDE ,THETA,SIG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C
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
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), 
     .        OFF(NEL)  ,EINT(NEL) ,MU(NEL)   , 
     .        MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL) ,DF(NEL)  , 
     .        VNEW(NEL) ,PNEW(NEL) ,DPDM(NEL),
     .        DPDE(NEL) ,THETA(NEL)
      my_real, INTENT(INOUT) :: PSH(NEL)
      my_real,INTENT(IN) :: SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real :: P0,GAMMA,RHO,RHO0
      my_real :: r_GAS,A0,A1,A2,A3,A4,CP(NEL),CV
      my_real :: TEMP, FUN, DFUN, TOL, ERROR, INCR
      my_real :: POLD ! pressure
      INTEGER :: ITER, MAX_ITER
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      MAX_ITER = 50
      TOL = EM05
      CP(1:NEL)=ZERO

      IF(IFLAG == 0) THEN
        MX         = MAT(1)
        RHO0       = PM(1 ,MX)
        r_GAS      = PM(106,MX)
        P0         = PM(31,MX)
        PSH(1:NEL) = PM(88,MX)
        A0         = PM(32,MX)
        A1         = PM(33,MX)
        A2         = PM(34,MX)
        A3         = PM(35,MX)
        A4         = PM(36,MX)
        !SOLVE TEMPERATURE
        DO I=1,NEL
          ! Init newton
          RHO = RHO0 * (ONE + MU(I))
          POLD=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          TEMP = POLD/RHO/r_GAS
          ITER = 0
          ERROR = HUGE(ZERO)
          DO WHILE(ERROR > TOL .AND. ITER < MAX_ITER)
             ! f(x) = 0 <=> int(cv(T), T) - eint = 0
             ! <=> int(cp(T) - r_gas, T) - eint = 0
             FUN = A0 * TEMP + HALF * A1 * TEMP**2 + THIRD * A2 * TEMP**3 +
     .            FOURTH * A3 * TEMP**4 + ONE_FIFTH * A4 * TEMP**5 - r_GAS * TEMP - ESPE(I) / RHO0
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
         DPDM(I) = RHO0*GAMMA*r_GAS*THETA(I)     !total derivative
         DPDE(I) = GAMMA*(ONE+MU(I))
       ENDDO
         
C-----------------------------------------------
      ELSEIF(IFLAG == 1) THEN
        MX         = MAT(1)
        RHO0       = PM(1 ,MX)
        r_GAS      = PM(106,MX)
        P0         = PM(31,MX)
        PSH(1:NEL) = PM(88,MX)
        A0         = PM(32,MX)
        A1         = PM(33,MX)
        A2         = PM(34,MX)
        A3         = PM(35,MX)
        A4         = PM(36,MX)
        r_GAS      = PM(106,MX)
        !SOLVE TEMPERATURE
        DO I=1,NEL
          ! Init newton
          RHO = RHO0 * (ONE + MU(I))
          POLD=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          TEMP = POLD/RHO/r_GAS
          ITER = 0
          ERROR = HUGE(ZERO)
          DO WHILE(ERROR > TOL .AND. ITER < MAX_ITER)
             ! f(x) = 0 <=> int(cv(T), T) - eint = 0
             ! <=> int(cp(T) - r_gas, T) - eint = 0
             FUN = A0 * TEMP + HALF * A1 * TEMP**2 + THIRD * A2 * TEMP**3 +
     .            FOURTH * A3 * TEMP**4 + ONE_FIFTH * A4 * TEMP**5 - r_GAS * TEMP - ESPE(I) / RHO0
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
          PNEW(I) = RHO0*(ONE+MU(I))*r_GAS*THETA(I)
          EINT(I) = EINT(I) - HALF*DVOL(I)*PNEW(I)
          PNEW(I) = PNEW(I)-PSH(I)
          CV = CP(I) - r_GAS
          GAMMA = CP(I)/CV
          DPDE(I) = GAMMA*(ONE+MU(I))
        ENDDO

      ELSEIF (IFLAG == 2) THEN
        MX           = MAT(1)
        RHO0         = PM(1 ,MX)
        r_GAS        = PM(106,MX)
        P0           = PM(31,MX)
        PSH(1:NEL)   = PM(88,MX)
        A0           = PM(32,MX)
        A1           = PM(33,MX)
        A2           = PM(34,MX)
        A3           = PM(35,MX)
        A4           = PM(36,MX)
        !SOLVE TEMPERATURE
        DO I=1,NEL
          ! Init newton
          RHO = RHO0 * (ONE + MU(I))
          POLD=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          TEMP = POLD/RHO/r_GAS
          ITER = 0
          ERROR = HUGE(ZERO)
          DO WHILE(ERROR > TOL .AND. ITER < MAX_ITER)
             ! f(x) = 0 <=> int(cv(T), T) - eint = 0
             ! <=> int(cp(T) - r_gas, T) - eint = 0
             FUN = A0 * TEMP + HALF * A1 * TEMP**2 + THIRD * A2 * TEMP**3 +
     .            FOURTH * A3 * TEMP**4 + ONE_FIFTH * A4 * TEMP**5 - r_GAS * TEMP - ESPE(I) / RHO0
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
            DPDM(I) = RHO0*GAMMA*r_GAS*THETA(I) !total derivative
            DPDE(I) = GAMMA*(ONE+MU(I)) !partial derivative
            PNEW(I) = RHO0*(ONE+MU(I))*r_GAS*THETA(I)
          ENDIF
        ENDDO

      ENDIF
C-----------------------------------------------
      RETURN
      END
