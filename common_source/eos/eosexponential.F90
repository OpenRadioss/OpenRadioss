!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
module eosexponential_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief This subroutine is solving Exponential EoS
!! \details P = P0 exp(alpha.time) - PSH
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
      !||====================================================================
      !||    eosexponential   ../common_source/eos/eosexponential.F90
      !||--- called by ------------------------------------------------------
      !||    eosmain          ../common_source/eos/eosmain.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod     ../common_source/modules/constant_mod.F
      !||====================================================================
      subroutine eosexponential(iflag ,nel   ,pm   ,off  ,eint ,&
                                dvol  ,vnew  ,mat  ,psh  ,      &
                                pnew  ,dpdm  ,dpdE ,time ,&
                                npropm,nummat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod , only : three100, half, zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: npropm , nummat          !< size for pm array
      integer,intent(in) :: mat(nel)                 !< material identifiers for elems 1 to nel
      integer,intent(in) :: IFLAG                    !< flag mentioning what needs to be computed
      integer,intent(in) :: NEL                      !< number of elems in group (current group size)
      my_real, intent(in) :: time                    !< simulation current time
      my_real, intent(in) :: pm(npropm,nummat)       !< parameters of all materials
      my_real, intent(in) :: vnew(nel)               !< current volume of elems
      my_real, intent(in) :: off(nel)                !< state of elems (0.0 if deleted)
      my_real, intent(in) :: dvol(nel)               !< volume change of elems
      my_real, intent(inout) :: pnew(nel)            !< pressure     
      my_real, intent(inout) :: dpdm(nel)            !< total derivative : mu = rho/rho0-1
      my_real, intent(inout) :: dpdE(nel)            !< partial derivative : E=rho0.e
      my_real, intent(inout) :: psh(nel)             !< pressure shift
      my_real, intent(inout) :: eint(nel)            !< internal energy
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i, mx
      my_real :: p0,alpha
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

      if(iflag == 0) then
        mx = mat(1)
        p0 = pm(104,mx) !starter:p0-psh
        alpha = pm( 32,mx)
        psh(1:nel) = pm( 88,mx)
        dpdm(1:nel) = zero
        dpdE(1:nel) = zero
        pnew(1:nel) = p0*exp(alpha*time)
        pnew(1:nel) = pnew(1:nel)*off(1:nel)

      elseif(iflag == 1) then
        mx = mat(1)
        p0 = pm(104,mx) !starter:p0-psh
        alpha = pm( 32,mx)
        psh(1:nel) = pm( 88,mx)
        pnew(1:nel) = p0*exp(alpha*time)
        pnew(1:nel) = pnew(1:nel)*off(1:nel)
        do i=1,nel
          eint(i) = eint(i) - half*dvol(i)*(pnew(i)+psh(i))
          dpdE(i) = zero
        enddo

      elseif(iflag == 2) then
        mx  = mat(1)
        mx = mat(1)
        p0 = pm(104,mx) !starter:p0-psh
        alpha = pm( 32,mx)
        psh(1:nel) = pm( 88,mx)
          do i=1,nel
            if (vnew(i) > zero) then
              pnew(i) =  p0*exp(alpha*time)
              pnew(i) = pnew(i)*off(i)
              dpdm(i) = zero
              dpdE(i) = zero
            endif
          enddo
        endif

!------------------------
      return
      END subroutine eosexponential

end module eosexponential_mod