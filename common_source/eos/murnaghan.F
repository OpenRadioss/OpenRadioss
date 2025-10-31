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
!||    murnaghan_mod   ../common_source/eos/murnaghan.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE MURNAGHAN_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    murnaghan       ../common_source/eos/murnaghan.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
      SUBROUTINE MURNAGHAN(
     1                     IFLAG,NEL  ,PMIN ,OFF  ,EINT ,MU  ,
     2                     DVOL ,VNEW ,PSH ,
     3                     PNEW ,DPDM ,DPDE ,EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CONSTANT_MOD , ONLY : ZERO, EM15, HALF, ONE, TWO, THREE, THREE100
      USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of MURNAGHAN EOS
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
#include  "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) ::  IFLAG, NEL
      my_real,INTENT(IN) ::  PMIN,  OFF(NEL) ,MU(NEL), DVOL(NEL) ,VNEW(NEL)
      my_real, INTENT(INOUT) :: PSH(NEL),PNEW(NEL) ,DPDM(NEL),DPDE(NEL),EINT(NEL)
      TYPE(EOS_PARAM_),INTENT(IN) :: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
        my_real :: P0,K0,K1,PC
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K0         = EOS_STRUCT%UPARAM(1)
      P0         = EOS_STRUCT%UPARAM(2)
      PSH(1:NEL) = EOS_STRUCT%PSH
      K1         = EOS_STRUCT%UPARAM(3)

      IF(IFLAG == 0) THEN
        DO I=1,NEL
          DPDM(I) = K0*EXP( K1*LOG(1+MU(I)) )/(ONE+MU(I))     !total derivative
          DPDE(I) = ZERO                                      !partial derivative
          PNEW(I) = K0/K1 * (EXP(K1*LOG(ONE+MU(I))) - ONE )  + P0
          PNEW(I) = MAX(PNEW(I),PMIN)*OFF(I)
          PNEW(I) = PNEW(I)-PSH(I)
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        DO I=1,NEL
          PNEW(I) = K0/K1 * (EXP(K1*LOG(ONE+MU(I))) - ONE )  + P0
          PNEW(I) = MAX(PNEW(I),PMIN)*OFF(I)
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I))
          PNEW(I) = PNEW(I)-PSH(I)
          DPDE(I) = ZERO
        ENDDO

       ELSEIF(IFLAG == 2) THEN
         DO I=1,NEL
           IF (VNEW(I) > ZERO) THEN
             PNEW(I) = K0/K1 * (EXP(K1*LOG(1+MU(I))) - ONE )  + P0
             DPDM(I) = K0*EXP( K1*LOG(1+MU(I)) )/(ONE+MU(I)) !total derivative
             DPDE(I) = ZERO
             PNEW(I) = MAX(PNEW(I),PMIN)*OFF(I)
             PNEW(I) = PNEW(I)-PSH(I)
           ENDIF
         ENDDO
      ENDIF
C------------------------      
      RETURN
      END SUBROUTINE MURNAGHAN
      END MODULE MURNAGHAN_MOD
