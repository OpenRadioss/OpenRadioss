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
      !||    law163_upd_mod   ../starter/source/materials/mat/mat163/law163_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat           ../starter/source/materials/updmat.F
      !||====================================================================
      module law163_upd_mod
      contains
!! \brief update material law 190
      !||====================================================================
      !||    law163_upd         ../starter/source/materials/mat/mat163/law163_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat             ../starter/source/materials/updmat.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine law163_upd(  matparam ,pm , npropm   )
! ----------------------------------------------------------------------------------------------------------------------
!   M o d u l e s
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use matparam_def_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
          type(matparam_struct_), target :: matparam
          integer, intent(in) :: npropm
          my_real, dimension(npropm), intent(inout) :: pm
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          !< Integer variables
          integer :: i,j                                  !< iterators
          integer :: ndim                                 !< dimension
          integer :: npt                                  !< number of integration points / max number of integration points
!
          !< Real variables
          my_real :: x_i,y_i
          my_real :: ener,dx,dy,dydx,youngmax,nu,g,bulk,lam
          type(table_4d_), dimension(:) ,pointer :: table_mat
!
!--------------------------------------------------------------------------
!     copy global functions/tables to matparam data structure
!--------------------------------------------------------------------------
!
          !< Loading table
          table_mat => matparam%table(1:matparam%ntable) ! material table pointer
          ndim = table_mat(1)%ndim               ! number of dimensions
          npt  = size(table_mat(1)%x(1)%values)  ! number of points
!
          !< Compute the maximum tabulated slope
          youngmax = zero
          do i = 1,npt-1
            dx = table_mat(1)%x(1)%values(i+1) - table_mat(1)%x(1)%values(i)
            dy = zero
            if (ndim == 1) then
              dy = table_mat(1)%y1d(i+1) - table_mat(1)%y1d(i)
            elseif (ndim == 2) then
              do j = 1,ndim
                dy = max(dy,table_mat(1)%y2d(i+1,j) - table_mat(1)%y2d(i,j))
              enddo
            endif
            dydx = dy/dx
            youngmax = max(youngmax, dy/dx)
          enddo
!
          !< Update material parameters
          ! -> Young's modulus
          youngmax = max(youngmax,matparam%young)
          matparam%young = youngmax
          ! -> Recover Poisson's ratio
          nu = matparam%nu
          ! -> Bulk modulus
          bulk = youngmax/(three*(one - two*nu))
          matparam%bulk = bulk
          ! -> Shear modulus
          g = half*youngmax/(one + nu)
          matparam%shear = g
          !< Stiffness matrix components
          lam = youngmax*nu / (one+nu) / (one - two*nu) 
          matparam%uparam(1) = lam + g*two
          matparam%uparam(2) = lam
          !< Update PM array
          pm(20) = youngmax
          pm(22) = g
          pm(24) = youngmax
          pm(32) = bulk
!
        end
      end module

