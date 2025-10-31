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
!||    osborne_mod   ../common_source/eos/osborne.F
!||--- called by ------------------------------------------------------
!||    eosmain       ../common_source/eos/eosmain.F
!||    eosmain51     ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE OSBORNE_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    osborne         ../common_source/eos/osborne.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
      SUBROUTINE OSBORNE  (
     1                     IFLAG , NEL   ,PMIN ,OFF  ,EINT ,MU   ,
     2                     ESPE  , DVOL  ,DF   ,VNEW ,RHO0 ,PSH  ,
     3                     PNEW  , DPDM  ,DPDE ,EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
      USE CONSTANT_MOD , ONLY : EM15,ZERO,ONE,HALF,TWO
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of OSBORNE EOS
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
      my_real,INTENT(IN) :: OFF(NEL) ,MU(NEL) ,DVOL(NEL) ,DF(NEL) ,VNEW(NEL), RHO0(NEL), PMIN
      my_real,INTENT(INOUT) :: PNEW(NEL), DPDM(NEL), DPDE(NEL),PSH(NEL),EINT(NEL),ESPE(NEL)
      TYPE(EOS_PARAM_),INTENT(IN):: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, MX
        my_real :: P0,E0,DVV,PP,dPdMU
        my_real :: A1,A2,B0,B1,B2,C0,C1,D0, A2_
        my_real :: DENOM
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        E0         = EOS_STRUCT%E0
        PSH(1:NEL) = EOS_STRUCT%PSH

        A1 = EOS_STRUCT%UPARAM(1)
        A2 = EOS_STRUCT%UPARAM(2)
        B0 = EOS_STRUCT%UPARAM(3)
        B1 = EOS_STRUCT%UPARAM(4)
        B2 = EOS_STRUCT%UPARAM(5)
        C0 = EOS_STRUCT%UPARAM(6)
        C1 = EOS_STRUCT%UPARAM(7)
        D0 = EOS_STRUCT%UPARAM(8)
        P0 = EOS_STRUCT%UPARAM(9)

      IF(IFLAG == 0) THEN
        DO I=1,NEL
          A2_=A2
          IF(MU(I)<ZERO)A2_=-A2
          DENOM   = (ESPE(I)+D0)
          PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
          PP      = MAX(PP,PMIN) * OFF(I)
          dPdMU   = (A1+2*A2_*MU(I)+(TWO*B2*MU(I)+B1)*ESPE(I)+C1*ESPE(I)*ESPE(I))/DENOM
          dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM
          DPDM(I) = dPdmu + dPdE(I)*DF(I)*DF(I)*(PP)   !total derivative
          PNEW(I) = PP ! P(mu[n+1],E[n])
          PNEW(I) = PNEW(I) - PSH(I)
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        DO I=1,NEL
          A2_=A2
          IF(MU(I)<ZERO)A2_=-A2
          DVV      = DVOL(I)*DF(I) / MAX(EM15,VNEW(I))   ! DVOL/V0  car  ESPE =EINT/V0
          DVV      = HALF*DVV ! car 2 iterations
          DENOM   = (ESPE(I)+D0)
          PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
          PP      = MAX(PP,PMIN) * OFF(I)
          ESPE(I) = ESPE(I) - (PP)*DVV
          DENOM   = (ESPE(I)+D0)
          PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
          PP      = MAX(PP,PMIN) * OFF(I)
          ESPE(I) = ESPE(I) - (PP)*DVV
          PNEW(I) = PP ! P(mu[n+1],E[n+1])
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I))
          PNEW(I) = PNEW(I) - PSH(I)
          dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM
        ENDDO

      ELSEIF(IFLAG == 2) THEN
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
            A2_=A2
            IF(MU(I)<ZERO)A2_=-A2
            DENOM   = (ESPE(I)+D0)
            PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))/DENOM
            PP      = MAX(PP,PMIN)*OFF(I)
            dPdMU   = (A1+2*A2_*MU(I)+(TWO*B2*MU(I)+B1)*ESPE(I)+C1*ESPE(I)*ESPE(I))/DENOM
            dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM
            DPDM(I) = dPdmu + dPdE(I)*DF(I)*DF(I)*(PP) !total derivative
            PNEW(I) = PP
            PNEW(I) = PNEW(I) - PSH(I)
          ENDIF
        ENDDO

        ENDIF

C-----------------------------------------------
        RETURN
      END SUBROUTINE OSBORNE
      END MODULE OSBORNE_MOD
