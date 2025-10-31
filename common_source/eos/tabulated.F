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
!||    tabulated_mod   ../common_source/eos/tabulated.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
       MODULE TABULATED_MOD
       IMPLICIT NONE
       CONTAINS
!||====================================================================
!||    tabulated       ../common_source/eos/tabulated.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
      SUBROUTINE TABULATED(
     1                     IFLAG,NEL  ,PMIN ,OFF  ,EINT ,MU  ,
     2                     ESPE ,DVOL ,DF   ,VNEW ,PSH ,
     3                     PNEW ,DPDM ,DPDE ,
     4                     NPF  ,TF   ,SNPC,STF, EOS_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE CONSTANT_MOD , ONLY : ZERO, EM15, HALF, ONE, TWO
        USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving  of TABULATED Equation Of State (/EOS/TABULATED)
C   P(mu,E) = A(mu) + B(mu)*E
C     where A and B are two user functions (/FUNCT)
C
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
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: SNPC,STF
      INTEGER, INTENT(IN) :: IFLAG, NEL,NPF(SNPC)
      my_real, INTENT(IN) :: OFF(NEL), MU(NEL), ESPE(NEL), DVOL(NEL), DF(NEL), VNEW(NEL), TF(STF)
      my_real,INTENT(INOUT) :: PSH(NEL), EINT(NEL), DPDM(NEL), DPDE(NEL), PNEW(NEL)
      my_real,INTENT(IN) :: PMIN
      TYPE(EOS_PARAM_ ),INTENT(IN) :: EOS_STRUCT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, MX
        my_real :: E0,AA,BB,DVV,PP
        my_real :: XSCALE_A,XSCALE_B,FSCALE_A,FSCALE_B
        INTEGER :: A_fun_id, B_fun_id
        my_real :: RES_A(NEL),RES_B(NEL),DERI_A(NEL),DERI_B(NEL)
        my_real,EXTERNAL :: FINTER
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      E0 = EOS_STRUCT%E0
      PSH(1:NEL) = EOS_STRUCT%PSH
      XSCALE_A = EOS_STRUCT%UPARAM(1)
      XSCALE_B = EOS_STRUCT%UPARAM(2)
      FSCALE_A = EOS_STRUCT%UPARAM(3)
      FSCALE_B = EOS_STRUCT%UPARAM(4)
      A_fun_id = EOS_STRUCT%IPARAM(1)
      B_fun_id = EOS_STRUCT%IPARAM(2)

      IF(IFLAG == 0) THEN
         ! both A_fun_id & B_fun_id cannot be 0. This is ensured by the reader during the Starter process
         IF(A_fun_id == 0)THEN
           DO I=1,NEL
             RES_A(I) = ZERO
             DERI_A(I) = ZERO
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ELSEIF(B_fun_id == 0)THEN
           DO I=1,NEL
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = ZERO
             DERI_B(I) = ZERO
           ENDDO
         ELSE
           DO I=1,NEL
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ENDIF
         DO I=1,NEL
           PP      = RES_A(I) + RES_B(I) * ESPE(I) - PSH(I) ! A(MU(I))+B(MU(I))*ESPE(I)
           DPDM(I) = DERI_A(I)+DERI_B(I)*ESPE(I) + RES_B(I)*(PP+PSH(I))/( (ONE+MU(I))*(ONE+MU(I)) ) ! A'(MU0) + B'(MU0)*E0+B(MU0)/(ONE+MU0)/(ONE+MU0)*P0     !total derivative
           DPDE(I) = RES_B(I) ! B(MU(I))                                               !partial derivative
           PNEW(I) = MAX(PP,PMIN)*OFF(I)    ! P(mu[n+1],E[n])
         ENDDO

C-----------------------------------------------
      ELSEIF(IFLAG == 1) THEN
        ! both A_fun_id & B_fun_id cannot be 0. This is ensured by the reader during the Starter process
        IF(A_fun_id == 0)THEN
          DO I=1,NEL
            RES_A(I) = ZERO
            RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
          ENDDO
        ELSEIF(B_fun_id == 0)THEN
          DO I=1,NEL
            RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
            RES_B(I) = ZERO
          ENDDO
        ELSE
          DO I=1,NEL
            RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
            RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
          ENDDO
        ENDIF
        DO I=1,NEL
          AA      = RES_A(I)
          BB      = RES_B(I)
          DPDE(I) = BB
          DVV     = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
          PP      = AA + BB * ESPE(I)
          PNEW(I) = (AA+BB*(ESPE(I)-PSH(I)*DVV))/(ONE+BB*DVV)
          PNEW(I) = MAX(PNEW(I),PMIN )*OFF(I)    ! P(mu[n+1],E[n+1])
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I) )
        ENDDO

C-----------------------------------------------
       ELSEIF (IFLAG == 2) THEN
         ! both A_fun_id & B_fun_id cannot be 0. This is ensured by the reader during the Starter process
         IF(A_fun_id == 0)THEN
           DO I=1,NEL
             RES_A(I) = ZERO
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
             DERI_A(I) = ZERO
           ENDDO
         ELSEIF(B_fun_id == 0)THEN
           DO I=1,NEL
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = ZERO
             DERI_B(I) = ZERO
           ENDDO
         ELSE
           DO I=1,NEL
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ENDIF
         DO I=1, NEL
            IF (VNEW(I) > ZERO) THEN
               PP      = RES_A(I) + RES_B(I)*ESPE(I) - PSH(I)
               DPDM(I) = DERI_A(I)+DERI_B(I)*ESPE(I) + RES_B(I)*(PP+PSH(I))/( (ONE+MU(I))*(ONE+MU(I)) ) ! A'(MU0) + B'(MU0)*E0+B(MU0)/(ONE+MU0)/(ONE+MU0)*P0     !total derivative
               DPDE(I) = RES_B(I) ! B(MU(I))                                               !partial derivative
               PNEW(I) = PP
            ENDIF
         ENDDO

        ENDIF
C-----------------------------------------------
        RETURN
      END SUBROUTINE TABULATED
      END MODULE TABULATED_MOD
