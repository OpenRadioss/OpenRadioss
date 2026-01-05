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
!||    eikonal_init_sorting_mod       ../starter/source/initial_conditions/detonation/eikonal_init_sorting.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||====================================================================
      module eikonal_init_sorting_mod
      implicit none
      contains
!||====================================================================
!||    eikonal_init_sorting                ../starter/source/initial_conditions/detonation/eikonal_init_sorting.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method        ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine eikonal_init_sorting(neldet, numel, elem_list, uelem_list, idx_ng , idx_i, elem_list_bij, xel, vel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero, ep21
          use insertion_sort_mod , only : integer_insertion_sort_with_index
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: neldet,numel
          integer,intent(inout) :: elem_list(neldet)
          integer,intent(inout) :: uelem_list(neldet)
          integer,intent(inout) :: idx_ng(neldet)
          integer,intent(inout) :: idx_i(neldet)
          integer,intent(inout) :: elem_list_bij(1:numel)
          real(kind=WP),intent(inout) :: xel(3,neldet)
          real(kind=WP),intent(inout) :: vel(neldet)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer,allocatable,dimension(:) :: indx
          integer,allocatable,dimension(:) :: int_tmp_array
          real(kind=WP),allocatable,dimension(:) :: real_tmp_array
          integer :: kk
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(int_tmp_array(neldet))
          allocate(real_tmp_array(neldet))
          allocate(indx(neldet))

          indx(1:neldet) = [(kk, kk=1,neldet)]
          call integer_insertion_sort_with_index(uelem_list, indx, neldet)

          !sort other arrays with same order usin indx array
          int_tmp_array(:)  = elem_list(:)     ; elem_list(:)    = int_tmp_array(indx(:))
          int_tmp_array(:)  = idx_ng(:)        ; idx_ng(:)       = int_tmp_array(indx(:))
          int_tmp_array(:)  = idx_i(:)         ; idx_i(:)        = int_tmp_array(indx(:))
          real_tmp_array(:) = xel(1,:)         ; xel(1,:)        = real_tmp_array(indx(:))
          real_tmp_array(:) = xel(2,:)         ; xel(2,:)        = real_tmp_array(indx(:))
          real_tmp_array(:) = xel(3,:)         ; xel(3,:)        = real_tmp_array(indx(:))
          real_tmp_array(:) = vel(:)           ; vel(:)          = real_tmp_array(indx(:))

          ! sorting bijective array
          elem_list_bij(1:numel) = 0
          do kk=1,neldet
            elem_list_bij(elem_list(kk)) = kk
          end do

          deallocate(int_tmp_array)
          deallocate(real_tmp_array)
          deallocate(indx)

        end subroutine eikonal_init_sorting
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_init_sorting_mod


