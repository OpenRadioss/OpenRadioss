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
        module update_pon_mod
        contains
        !\brief replace old_ids with new_ids in the shell data structure
        subroutine update_pon_shells(elements, n, shell_list, new_numnod, old_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use parith_on_mod
          use connectivity_mod
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(connectivity_), intent(inout) ::  elements
          integer, intent(in) :: n
          integer, dimension(n), intent(in) :: shell_list
          integer, intent(in) :: new_numnod
          integer, intent(in) :: old_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j
          integer :: shell_id
          integer :: contributions_count
          integer, dimension(:), allocatable :: new_adsky
          integer :: new_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
            contributions_count = 0
            new_id = new_numnod ! the new id is always the last one
            do i = 1, n
              shell_id = shell_list(i)
              do j = 1, 4
                if(elements%shell%nodes(j, shell_id) == new_id) then
                    contributions_count = contributions_count + 1
                endif
              enddo
            enddo
            call my_alloc(new_adsky, new_numnod + 1)
            do i = 1, old_id
              new_adsky(i) = elements%pon%adsky(i)
            enddo
            do i = old_id + 1, new_numnod
              new_adsky(i) = elements%pon%adsky(i) - contributions_count
            enddo
            new_adsky(new_numnod + 1) = new_adsky(new_numnod) + contributions_count
            !call move_alloc to replace adsky with new_adsky
            call move_alloc(new_adsky,elements%pon%adsky)
            elements%pon%sadsky = new_numnod + 1
            contributions_count = 0
            do i = 1, n
              shell_id = shell_list(i)
              do j = 1, 4
                if(elements%shell%nodes(j, shell_id) == new_id) then
                    elements%pon%iadc(j,n) = elements%pon%adsky(new_numnod) + contributions_count                               
                    contributions_count = contributions_count + 1
                endif
              enddo
            enddo


        end subroutine update_pon_shells
        end module update_pon_mod   