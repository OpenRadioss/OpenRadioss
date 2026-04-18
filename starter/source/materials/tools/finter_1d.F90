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
!||    finter_1d_mod   ../starter/source/materials/tools/finter_1d.F90
!||--- called by ------------------------------------------------------
!||    biquad_upd      ../starter/source/materials/fail/biquad/biquad_upd.F90
!||====================================================================
      module finter_1d_mod
      contains
!||====================================================================
!||    finter_1d       ../starter/source/materials/tools/finter_1d.F90
!||--- called by ------------------------------------------------------
!||    biquad_upd      ../starter/source/materials/fail/biquad/biquad_upd.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      subroutine finter_1d(npt,x_data,y_data,x,y,deri)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Arguments
! --------------------------------------------------------------------------------------------------
      integer                       ,intent(in)  :: npt
      real(kind=wp), dimension(npt) ,intent(in)  :: x_data
      real(kind=wp), dimension(npt) ,intent(in)  :: y_data
      real(kind=wp)                 ,intent(in)  :: x
      real(kind=wp)                 ,intent(out) :: y
      real(kind=wp)                 ,intent(out) :: deri
! --------------------------------------------------------------------------------------------------
!         Local Variables
! --------------------------------------------------------------------------------------------------
      integer  idx
!===============================================================================    
      ! Find nearest index
      idx = minloc(abs(x_data - x), dim=1)
      idx = max(idx, 1)
      idx = min(idx, npt-1)
    
      deri = (y_data(idx+1) - y_data(idx)) / (x_data(idx+1) - x_data(idx))
      y    =  y_data(idx) + (x - x_data(idx)) * deri
!-----------
      return
!-----------
      end subroutine finter_1d
      end module finter_1d_mod
