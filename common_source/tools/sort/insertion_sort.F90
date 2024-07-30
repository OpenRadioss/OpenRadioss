!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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


 module insertion_sort_mod

    ! insertion sort for real and integer arrays
    !    index array allows to determine the bijection between unsorted and sorted arrays

   contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief This subroutine is sorting array of size n with insertion sorting algorithm
!! \details resulting index are provided
      !||====================================================================
      !||    real_insertion_sort_with_index   ../common_source/tools/sort/insertion_sort.F90
      !||--- called by ------------------------------------------------------
      !||    clipping_weiler_atherton         ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||====================================================================
          subroutine real_insertion_sort_with_index(array, index, n)
            implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            my_real, intent(inout) :: array(:)
            integer, intent(out) :: index(:)
            integer, intent(in) :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: ii, jj, temp_index
            real :: key
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            do ii=1,n
              index(ii)=ii
            end do
            do ii = 2, n
              key = array(ii)
              temp_index = index(ii)
              jj = ii - 1
              ! move (+1) elems from array(1:j) which are greather than key
              do while (array(jj) > key)
                array(jj + 1) = array(jj)
                index(jj + 1) = index(jj)
                jj = jj - 1
                if (jj == 0)exit
              end do
              array(jj + 1) = key
              index(jj + 1) = temp_index
            end do
          end subroutine real_insertion_sort_with_index



! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief This subroutine is sorting array of size n with insertion sorting algorithm
!! \details resulting index are provided
      !||====================================================================
      !||    integer_insertion_sort_with_index   ../common_source/tools/sort/insertion_sort.F90
      !||====================================================================
          subroutine integer_insertion_sort_with_index(array, index, n)
            implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            integer, intent(inout) :: array(:)
            integer, intent(out) :: index(:)
            integer, intent(in) :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: ii, jj, temp_index
            integer :: key
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            do ii=1,n
              index(ii)=ii
            end do
            do ii = 2, n
              key = array(ii)
              temp_index = index(ii)
              jj = ii - 1
              ! move (+1) elems from array(1:j) which are greather than key
              do while (array(jj) > key)
                array(jj + 1) = array(jj)
                index(jj + 1) = index(jj)
                jj = jj - 1
                if (jj == 0)exit
              end do
              array(jj + 1) = key
              index(jj + 1) = temp_index
            end do
          end subroutine integer_insertion_sort_with_index



end module insertion_sort_mod