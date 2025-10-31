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
!||    eospolyno_mod   ../common_source/eos/eospolyno.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      MODULE EOSPOLYNO_MOD
      IMPLICIT NONE
      CONTAINS
!||====================================================================
!||    eospolyno       ../common_source/eos/eospolyno.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
      SUBROUTINE EOSPOLYNO(
     1                     IFLAG,NEL  ,PMIN ,OFF  ,EINT ,MU   ,MU2 ,
     2                     ESPE ,DVOL ,DF   ,VNEW ,PSH ,
     3                     PNEW ,DPDM ,DPDE ,EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE CONSTANT_MOD , ONLY : ZERO, EM15, HALF, ONE, TWO, THREE, THREE100
        USE EOS_PARAM_MOD , ONLY : eos_param_
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
#include      "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IFLAG, NEL
      my_real,INTENT(INOUT) :: PNEW(NEL), DPDM(NEL), DPDE(NEL), PSH(NEL), EINT(NEL)
      my_real,INTENT(IN) :: OFF(NEL), MU(NEL), MU2(NEL), ESPE(NEL), DVOL(NEL), DF(NEL), VNEW(NEL), PMIN
      type(eos_param_),intent(in) :: eos_struct !< data structure for EoS parameters
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      my_real :: AA, BB, DVV, PP
      my_real :: C0,C1,C2,C3,C4,C5
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        C0         = EOS_STRUCT%UPARAM(1)
        C1         = EOS_STRUCT%UPARAM(2)
        C2         = EOS_STRUCT%UPARAM(3)
        C3         = EOS_STRUCT%UPARAM(4)
        C4         = EOS_STRUCT%UPARAM(5)
        C5         = EOS_STRUCT%UPARAM(6)
        PSH(1:NEL) = EOS_STRUCT%PSH

      IF(IFLAG == 0) THEN
        DO I=1,NEL
          PP      = C0 +(C1 +C3 *MU(I)*MU(I))*MU(I)+C2 *MU2(I) + (C4 +C5 *MU(I))*ESPE(I)
          DPDM(I) = C1 + TWO*C2 *MAX(ZERO,MU(I))+THREE*C3 *MU(I)*MU(I) + C5 *ESPE(I)+(C4 +C5 *MU(I))*DF(I)*DF(I)*(PP+PSH(I) )
          DPDE(I) = C4 +C5 *MU(I)
          PNEW(I) = MAX(PP,PMIN)*OFF(I)! P(mu[n+1],E[n])
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        DO I=1,NEL
          AA      = C0 +MU(I)*(C1 +C3 *MU(I)*MU(I))+C2 *MU2(I)
          BB      = C4 +C5 *MU(I)
          DVV     = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
          PNEW(I) = (AA+BB*(ESPE(I)-PSH(I) *DVV))/(ONE+BB*DVV)
          PNEW(I) = MAX(PNEW(I),PMIN )*OFF(I) ! P(mu[n+1],E[n+1])
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I))
          DPDE(I) = C4 +C5 *MU(I)
        ENDDO

      ELSEIF(IFLAG == 2) THEN
         DO I=1, NEL
           IF (VNEW(I) > ZERO) THEN
             PNEW(I) = C0 + (C1 +C3*MU(I)*MU(I))*MU(I) + C2*MU2(I) + (C4 +C5 *MU(I))*ESPE(I)
             DPDE(I) = C4+C5*MU(I)
             DPDM(I) = C1 + TWO*C2*MAX(ZERO,MU(I)) + THREE*C3*MU(I)*MU(I) + C5*ESPE(I)  + (DPDE(I)) * DF(I)*DF(I)*(PNEW(I)+PSH(I))
             PNEW(I) = MAX(PNEW(I),PMIN )*OFF(I)
           ENDIF
         ENDDO
       ENDIF
C------------------------
      RETURN
      END SUBROUTINE EOSPOLYNO
      END MODULE EOSPOLYNO_MOD
