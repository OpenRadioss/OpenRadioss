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
!||    diffuse_necking_2d_mod   ../starter/source/materials/fail/diffuse_necking_2d.F90
!||--- called by ------------------------------------------------------
!||    biquad_upd               ../starter/source/materials/fail/biquad/biquad_upd.F90
!||====================================================================
      module diffuse_necking_2d_mod
      implicit none
      contains
        ! ==========================================================================================
        ! \brief create diffuse necking curve based on Swift theory
        ! \details calculate intersection points between hardening curve and it's derivative 
        ! \details with scale factors on hardening depending on triaxiality       
        ! ==========================================================================================
!||====================================================================
!||    diffuse_necking_2d          ../starter/source/materials/fail/diffuse_necking_2d.F90
!||--- called by ------------------------------------------------------
!||    biquad_upd                  ../starter/source/materials/fail/biquad/biquad_upd.F90
!||--- calls      -----------------------------------------------------
!||    polyline_intersection       ../starter/source/materials/tools/polyline_intersection.F90
!||    smooth_deriv                ../starter/source/materials/tools/smooth_deriv.F90
!||--- uses       -----------------------------------------------------
!||    polyline_intersection_mod   ../starter/source/materials/tools/polyline_intersection.F90
!||    smooth_deriv_mod            ../starter/source/materials/tools/smooth_deriv.F90
!||====================================================================
        subroutine diffuse_necking_2d(npt_eps,npt_eta,eps,sig,eta,epsp_neck,idebug)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use smooth_deriv_mod
          use polyline_intersection_mod
          use constant_mod  ,only : zero,one,two,three,four,third,two_third,three_half
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Arguments
! --------------------------------------------------------------------------------------------------
          integer ,intent(in) :: npt_eps,npt_eta
          integer ,intent(in) :: idebug
          real(kind=WP) ,dimension(npt_eps) ,intent(in)  :: eps,sig       !< input hardening curve
          real(kind=WP) ,dimension(npt_eta) ,intent(out) :: eta,epsp_neck !< instability strain curve
! --------------------------------------------------------------------------------------------------
!         Local variables
! --------------------------------------------------------------------------------------------------
          integer :: i
          integer, parameter :: np_deriv = 200
          real(kind=WP) :: a,b  ! principal stress ratios
          real(kind=WP) :: x,y,dx,dydx,xint,yint
          real(kind=WP) ,dimension(npt_eta)  :: fac_eta
          real(kind=WP) ,dimension(np_deriv) :: deps,dsig
          real(kind=WP) ,dimension(npt_eps)  :: sigf
          logical :: found
! ==================================================================================================           
          ! calculate smooth derivative of tabulated hardening curve
           dx = one / np_deriv  
           x  = zero
           do i=1,np_deriv
             call smooth_deriv(npt_eps, eps, sig, x, y, dydx)
             deps(i) = x
             dsig(i) = dydx
             x = x + dx       
           end do
!
           ! calculate hardening scale factors = f(triaxiality)
!
           dx = two / (npt_eta-1)    ! b = <-1, 1> => eta = <0,2/3>
           do i=1,npt_eta
             b = (i-1) * dx - one
             a = (one+two*b) / (two+b)
             eta(i)     = third*(one+a) / sqrt(one - a + a**2)
             fac_eta(i) = (four-three*a-three*a**2+four*a**3) / (four*(one-a+a**2)**three_half)
           end do
!           
           if (idebug == 1) then
             print*,'hardening scale factor vs triaxiality'
             do i=1,npt_eta
               print*,eta(i),fac_eta(i)
             end do
           end if
!
          ! generate necking plastic strain function in triaxiality range between <0,2/3>
!
          epsp_neck(1:npt_eta) = one
          do i = 1,npt_eta
            if (fac_eta(i) > zero) then
              ! scale the hardening function with fac(eta)
              sigf(1:npt_eps) = sig(1:npt_eps) * fac_eta(i)
              ! find intersection between hardening func derivative and scaled hardening curve
!
              call polyline_intersection(npt_eps, np_deriv, eps, sigf, deps, dsig, xint  ,yint ,found)
!
              if (found) then
                epsp_neck(i) = xint
              end if 
            end if 
          end do
!           
           if (idebug == 1) then
             print*,' '
             print*,'swift necking plastic strain vs triaxiality'
             do i=1,npt_eta
               print*,eta(i),epsp_neck(i)
             end do
           end if
! ---------------------------------------
        return
! ---------------------------------------
        end subroutine diffuse_necking_2d
        end module diffuse_necking_2d_mod


