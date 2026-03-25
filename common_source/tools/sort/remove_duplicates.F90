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
!||    remove_duplicates_mod   ../common_source/tools/sort/remove_duplicates.F90
!||====================================================================
      module remove_duplicates_mod

        ! remove duplicated values in 1d array using optionnal tolerance

      implicit none

      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief This subroutine is removing duplicated values in an ordered array
!! \details precondition : array is already sorted, complexity O(n)
!||====================================================================
!||    real_remove_duplicates   ../common_source/tools/sort/remove_duplicates.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod             ../common_source/modules/constant_mod.F
!||    precision_mod            ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine real_remove_duplicates(array, n, user_tol)
          use precision_mod , only : WP
          use constant_mod , only : zero
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP), intent(inout) :: array(:)
          real(kind=WP), intent(in), optional :: user_tol
          integer, intent(inout) :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: read_pos, write_pos
          real(kind=WP) :: tol
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (n <= 1) return

          tol = zero
          if (present(user_tol)) tol = user_tol

          !-- Two-pointer technique: read scans, write keeps unique values
          write_pos = 1
          read_pos = 2

          do while (read_pos <= n)
            !-- If current differs from last written value, keep it
            if (abs(array(write_pos) - array(read_pos)) > tol) then
              write_pos = write_pos + 1
              array(write_pos) = array(read_pos)
            end if
            read_pos = read_pos + 1
          end do

          n = write_pos

        end subroutine real_remove_duplicates




! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief This subroutine is removing duplicated values in an ordered array
!! \details precondition : array is already sorted, complexity O(n)
!||====================================================================
!||    integer_remove_duplicates   ../common_source/tools/sort/remove_duplicates.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                ../common_source/modules/constant_mod.F
!||    precision_mod               ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine integer_remove_duplicates(array, n, user_tol)
          use precision_mod , only : WP
          use constant_mod , only : zero
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(inout) :: array(:)
          real(kind=WP), intent(in), optional :: user_tol
          integer, intent(inout) :: n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: read_pos, write_pos
          real(kind=WP) :: tol
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (n <= 1) return

          tol = zero
          if (present(user_tol)) tol = user_tol

          !-- Two-pointer technique: read scans, write keeps unique values
          write_pos = 1
          read_pos = 2

          do while (read_pos <= n)
            !-- If current differs from last written value, keep it
            if (abs(array(write_pos) - array(read_pos)) > tol) then
              write_pos = write_pos + 1
              array(write_pos) = array(read_pos)
            end if
            read_pos = read_pos + 1
          end do

          n = write_pos

        end subroutine integer_remove_duplicates

      end module remove_duplicates_mod