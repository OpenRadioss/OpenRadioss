!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    biquad_upd_mod   ../starter/source/materials/fail/biquad/biquad_upd.F90
!||--- called by ------------------------------------------------------
!||    upfail           ../starter/source/materials/updfail.F90
!||====================================================================
      module biquad_upd_mod
      implicit none
      contains
        ! ==========================================================================================
        ! \brief generate diffuse necking curve using static hardening function from material law
        ! ==========================================================================================
!||====================================================================
!||    biquad_upd         ../starter/source/materials/fail/biquad/biquad_upd.F90
!||--- called by ------------------------------------------------------
!||    upfail             ../starter/source/materials/updfail.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine biquad_upd(fail)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use diffuse_necking_2d_mod
          use biquad_tab_mod
          use finter_1d_mod
          use constant_mod  ,only : zero,one,two,three,four,third,two_third,three_half,sqr3
          use constant_mod  ,only : em10
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Global arguments
! --------------------------------------------------------------------------------------------------
          type (fail_param_) ,target ,intent(inout) :: fail    !< failure model data structure
! --------------------------------------------------------------------------------------------------
!         Local variables
! --------------------------------------------------------------------------------------------------
          integer            :: i,ntable
          integer            :: npt_eps
          integer, parameter :: npt_eta = 201
          real(kind=WP) ,parameter :: eta13 = third
          real(kind=WP) :: epsf13                           !< plastic strain failure limit in simple tension
          real(kind=WP) :: eps_neck13                       !< plastic strain at necking pt in simple tension
          real(kind=WP) :: deps13                           
          real(kind=WP) :: deri
          real(kind=WP) ,dimension(npt_eta)    :: eta       ! table of triaxiality values = <0,2/3>
          real(kind=WP) ,dimension(npt_eta)    :: epsf      ! plastic strain at failure
          real(kind=WP) ,dimension(npt_eta)    :: eps_neck  ! plastic strain at necking points
          real(kind=WP) ,dimension(npt_eta)    :: regf_eta  ! regularization scale factors vs triaxiality
          real(kind=WP) ,dimension(:) ,pointer :: eps,sig   ! hardening stress-strain function points
! ==================================================================================================
          ntable  = fail%ntable4d
          npt_eps = size(fail%table4d(ntable)%x(1)%values)    ! absissa size of material model hardening curve
!
          eps => fail%table4d(ntable)%x(1)%values(1:npt_eps)  ! plastic strain vector of hardening curve
          sig => fail%table4d(ntable)%y1d(1:npt_eps)          ! yield stress vector of hardening curve
!          
          epsf13 = fail%uparam(9)                             ! failure strain value in uniaxial tension
!-----------------------------------------------
          ! calculate nominal failure strain from biquad equations

          call biquad_tab(npt_eta, fail%nuparam, fail%uparam, eta, epsf)
!
          ! calculate plastic strains at diffuse necking instability points vs triaxiality
          ! for the range of triaxiality values :  < 0, 2/3 >

          call diffuse_necking_2d(npt_eps,npt_eta,eps,sig,eta,eps_neck)
!
          ! necking plastic strain in uniaxial tension (triaxiality = 1/3) 
          call finter_1d(npt_eta ,eta ,eps_neck,eta13,eps_neck13,deri)
          deps13 = max(epsf13 - eps_neck13, em10)
!
          ! calculate regularization function, values normalized by uniaxual tension values
          do i= 1,npt_eta
            regf_eta(i) = (epsf(i) - eps_neck(i)) / deps13
            regf_eta(i) = min(one, max(regf_eta(i),zero))    ! limit regf_eta range to <0,1>
          end do
!-----------------------------------------------
          ! hardening function from material law is deallocated from failure model data structure
          ! replaced by regularization scale factor function
!-----------------------------------------------
          deallocate(fail%table4d(ntable)%x(1)%values)
          deallocate(fail%table4d(ntable)%y1d)
          allocate(fail%table4d(ntable)%x(1)%values(npt_eta))
          allocate(fail%table4d(ntable)%y1d(npt_eta))
!           
          fail%table4d(ntable)%ndim = 1
          fail%table4d(ntable)%x(1)%values(1:npt_eta) = eta(1:npt_eta)
          fail%table4d(ntable)%y1d(1:npt_eta)         = regf_eta(1:npt_eta)
!-----------------------------------------------
          return 
          end subroutine biquad_upd
          end module biquad_upd_mod


