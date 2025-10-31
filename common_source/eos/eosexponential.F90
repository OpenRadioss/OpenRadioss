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
!||====================================================================
!||    eosexponential_mod   ../common_source/eos/eosexponential.F90
!||--- called by ------------------------------------------------------
!||    eosmain              ../common_source/eos/eosmain.F
!||    eosmain51            ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
      module eosexponential_mod
      implicit none
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
!||    eosmain51        ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod     ../common_source/modules/constant_mod.F
!||    eos_param_mod    ../common_source/modules/mat_elem/eos_param_mod.F90
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine eosexponential( &
                                iflag ,nel   ,off  ,eint ,&
                                dvol  ,vnew  ,psh  ,      &
                                pnew  ,dpdm  ,dpdE ,time ,&
                                eos_struct)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod , only : three100, half, zero
      use precision_mod , only : WP
      use eos_param_mod , only : eos_param_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: IFLAG                      !< flag mentioning what needs to be computed
      integer,intent(in) :: NEL                        !< number of elems in group (current group size)
      real(kind=WP), intent(in) :: time                !< simulation current time
      real(kind=WP), intent(in) :: vnew(nel)           !< current volume of elems
      real(kind=WP), intent(in) :: off(nel)            !< state of elems (0.0 if deleted)
      real(kind=WP), intent(in) :: dvol(nel)           !< volume change of elems
      real(kind=WP), intent(inout) :: pnew(nel)        !< pressure
      real(kind=WP), intent(inout) :: dpdm(nel)        !< total derivative : mu = rho/rho0-1
      real(kind=WP), intent(inout) :: dpdE(nel)        !< partial derivative : E=rho0.e
      real(kind=WP), intent(inout) :: psh(nel)         !< pressure shift
      real(kind=WP), intent(inout) :: eint(nel)        !< internal energy
      type(eos_param_),intent(in) :: eos_struct        !< eos parameters for each submaterials
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          real(kind=WP) :: p0,alpha
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      alpha = eos_struct%uparam(1)
      p0 = eos_struct%p0 - eos_struct%psh
      psh(1:nel) = eos_struct%psh

      if(iflag == 0) then
        dpdm(1:nel) = zero
        dpdE(1:nel) = zero
        pnew(1:nel) = p0*exp(alpha*time)
        pnew(1:nel) = pnew(1:nel)*off(1:nel)

      elseif(iflag == 1) then
        pnew(1:nel) = p0*exp(alpha*time)
        pnew(1:nel) = pnew(1:nel)*off(1:nel)
        do i=1,nel
          eint(i) = eint(i) - half*dvol(i)*(pnew(i)+psh(i))
          dpdE(i) = zero
        enddo

      elseif(iflag == 2) then
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
