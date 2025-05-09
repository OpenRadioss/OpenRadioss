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
      !||    law133_upd_mod   ../starter/source/materials/mat/mat133/law133_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat           ../starter/source/materials/updmat.F
      !||====================================================================
      module law133_upd_mod
      contains
!! \brief update material law 190
      !||====================================================================
      !||    law133_upd         ../starter/source/materials/mat/mat133/law133_upd.F90
      !||--- called by ------------------------------------------------------
      !||    updmat             ../starter/source/materials/updmat.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine law133_upd(  matparam ,pm , npropm   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use matparam_def_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(matparam_struct_), target :: matparam
          integer, intent(in) :: npropm
          my_real, dimension(npropm), intent(inout) :: pm
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ndim                                 !< dimension
          integer :: npt                                  !< number of integration points / max number of integration points
          integer :: i,j                                  !< index loops
          my_real :: shear_max, young_max,nu,bulk_max
          type(table_4d_), dimension(:) ,pointer :: table_mat
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          !< Loading table
          table_mat => matparam%table(1:matparam%ntable) ! material table pointer
          ndim = table_mat(1)%ndim               ! number of dimensions
          npt  = size(table_mat(1)%x(1)%values)  ! number of points

          !< Compute the maximum tshear modulus
          shear_max = zero
          do i = 1,npt
            if (ndim == 1) then
              shear_max = max(shear_max, table_mat(1)%y1d(i))
            elseif (ndim == 2) then
              do j = 1,ndim
                shear_max = max(shear_max,table_mat(1)%y2d(i,j))
              enddo
            endif
          enddo

          nu = matparam%nu
          young_max =  two*shear_max*(one+nu)
          bulk_max = young_max / (three*(one-two*nu))

          !< Update material parameters (if max are needed instead of initial values)
          !----------------------------
          matparam%shear = shear_max
          matparam%young = young_max
          matparam%bulk = bulk_max
          pm(20) = young_max
          pm(24) = young_max

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine law133_upd
! ----------------------------------------------------------------------------------------------------------------------
      end module law133_upd_mod

