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
!||    inter1_seg_utils_mod         ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||--- called by ------------------------------------------------------
!||    inter1_check_ale_lag_sides   ../starter/source/interfaces/int01/inter1_check_ale_lag_sides.F90
!||====================================================================
      module inter1_seg_utils_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   FUNCTION
! ======================================================================================================================
!! \brief Returns true if the input is a triangle (N4 is zero or equal to N3)
!||====================================================================
!||    is_triangle       ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||--- called by ------------------------------------------------------
!||    normalize_shape   ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||    sort_shape        ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||====================================================================
        logical function is_triangle(q)
          integer, intent(in) :: q(4)
          is_triangle = (q(4) == 0) .or. (q(4) == q(3))
        end function is_triangle

        ! Sorts the 4 nodes of a quadrilateral or triangle (with triangle normalized as N4 = N3)
!||====================================================================
!||    sort_shape                   ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||--- called by ------------------------------------------------------
!||    inter1_check_ale_lag_sides   ../starter/source/interfaces/int01/inter1_check_ale_lag_sides.F90
!||--- calls      -----------------------------------------------------
!||    is_triangle                  ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||====================================================================
        subroutine sort_shape(q, sorted)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)  :: q(4)
          integer, intent(inout) :: sorted(4)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: temp(4), i, j, tmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          temp = q
          ! If it's a triangle, replace N4 with N3 to ensure consistent shape
          if (is_triangle(temp)) temp(4) = temp(3)
          sorted = temp
          do i = 1, 3
            do j = i+1, 4
              if (sorted(j) < sorted(i)) then
                tmp = sorted(i)
                sorted(i) = sorted(j)
                sorted(j) = tmp
              end if
            end do
          end do
        end subroutine sort_shape

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Returns the canonical representation of a shape (quad or triangle)
!||====================================================================
!||    normalize_shape              ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||--- called by ------------------------------------------------------
!||    inter1_check_ale_lag_sides   ../starter/source/interfaces/int01/inter1_check_ale_lag_sides.F90
!||--- calls      -----------------------------------------------------
!||    is_triangle                  ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||    lex_less                     ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||====================================================================
        subroutine normalize_shape(q, norm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)  :: q(4)
          integer, intent(out) :: norm(4)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: perms(4,8), temp(4)
          integer :: i, j
          logical :: is_tri
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_tri = is_triangle(q)
          if (is_tri) then
            ! Normalize triangle form to N4 = N3
            temp = q
            if (temp(4) == 0) temp(4) = temp(3)
            ! 3 rotations + 3 reverse for triangle
            do i = 1, 3
              ! Forward rotation
              perms(1,i) = temp(mod(i-1,3)+1)
              perms(2,i) = temp(mod(i  ,3)+1)
              perms(3,i) = temp(mod(i+1,3)+1)
              perms(4,i) = perms(3,i)  ! Repeat last node
              ! Reverse rotation
              perms(1,i+3) = perms(3,i)
              perms(2,i+3) = perms(2,i)
              perms(3,i+3) = perms(1,i)
              perms(4,i+3) = perms(3,i+3)
            end do
            norm = perms(:,1)
            do i = 2, 6
              if (lex_less(perms(:,i), norm)) norm = perms(:,i)
            end do
          else
            ! Normal quadrilateral (8 permutations: 4 rotations + 4 reverses)
            do i = 1, 4
              do j = 1, 4
                perms(j,i)   = q(mod(i + j - 2, 4) + 1)
                perms(j,i+4) = q(mod(i - j + 4, 4) + 1)
              end do
            end do
            norm = perms(:,1)
            do i = 2, 8
              if (lex_less(perms(:,i), norm)) norm = perms(:,i)
            end do
          end if
        end subroutine normalize_shape

! ======================================================================================================================
!                                                   FUNCTION
! ======================================================================================================================
!! \brief Lexicographical comparison: returns true if a < b
!||====================================================================
!||    lex_less          ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||--- called by ------------------------------------------------------
!||    normalize_shape   ../starter/source/interfaces/int01/inter1_seg_utils.F90
!||====================================================================
        logical function lex_less(a, b)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: a(4), b(4)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          lex_less = .false.
          do i = 1, 4
            if (a(i) < b(i)) then
              lex_less = .true.
              return
            else if (a(i) > b(i)) then
              lex_less = .false.
              return
            end if
          end do
        end function lex_less

      end module inter1_seg_utils_mod

