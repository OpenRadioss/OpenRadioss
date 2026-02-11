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
!||=================================================================================================
!||    smooth_deriv_mod   ../starter/source/materials/tools/smooth_deriv.F90
!||--- called by ------------------------------------------------------
!||=================================================================================================
      module smooth_deriv_mod
      implicit none
      contains
        ! ==========================================================================================
        ! \brief calculate smooth derivative value of tabulated data <x,y> at one point
        ! ==========================================================================================
!||====================================================================
!||    smooth_deriv         ../starter/source/materials/tools/smooth_deriv.F90
!||--- called by ------------------------------------------------------
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine smooth_deriv(npt, x_data, y_data, x, y, deri)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use constant_mod  ,only : zero,one,half
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Global arguments
! --------------------------------------------------------------------------------------------------
      integer ,intent(in) :: npt
      real(kind=WP), dimension(npt), intent(in) :: x_data, y_data
      real(kind=WP), intent(in)  :: x
      real(kind=WP), intent(out) :: y
      real(kind=WP), intent(out) :: deri
! --------------------------------------------------------------------------------------------------
!         Local variables
! --------------------------------------------------------------------------------------------------
      integer       :: idx,i1,i2
      real(kind=WP) :: dx,xs1,xs2,k1,k2, alpha,dydx
! ==================================================================================================           
      ! Find nearest index
      idx = minloc(abs(x_data - x), dim=1)
    
      ! Handle boundaries
      if (idx == npt) idx = npt-1
      if (x < x_data(idx)) idx = idx - 1
      
      alpha = zero
      k1  = zero
      k2  = (y_data(idx+1) - y_data(idx)) / (x_data(idx+1) - x_data(idx))
      xs2 = (x_data(idx+1) + x_data(idx)) * half
      dx  = xs2 - x
      if (idx == 1) then     ! left end
        if (dx <= zero) then
          i1 = idx+1
          i2 = idx+2
          xs1 = (x_data(i2) + x_data(i1)) * half
          k1  = (y_data(i2) - y_data(i1)) / (x_data(i2) - x_data(i1))
          alpha = dx / (xs2 - xs1)
        end if      
      else if (idx == npt-1) then  ! right end + extrapolation
        if (dx > zero) then
          i1 = idx-1
          i2 = idx
          xs1 = (x_data(i2) + x_data(i1)) * half
          k1  = (y_data(i2) - y_data(i1)) / (x_data(i2) - x_data(i1))
          alpha = dx / (xs2 - xs1)
        end if
      else  ! middle
        if (dx > zero) then
          i1 = idx-1
          i2 = idx
        else
          i1 = idx+1
          i2 = idx+2
        end if        
        xs1 = (x_data(i2) + x_data(i1)) * half
        k1  = (y_data(i2) - y_data(i1)) / (x_data(i2) - x_data(i1))
        alpha = dx / (xs2 - xs1)
      endif

      dydx = (y_data(idx+1) - y_data(idx)) / (x_data(idx+1) - x_data(idx))
      y = y_data(idx) + (x - x_data(idx)) * dydx
           
      deri  = alpha * k1 + (one - alpha) * k2
!-------------------------------------------------------------------------------
      return
!-----------
      end subroutine smooth_deriv
      end module smooth_deriv_mod
