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
      !||    eikonal_remove_first_mod       ../starter/source/initial_conditions/detonation/eikonal_remove_first.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||====================================================================
      module eikonal_remove_first_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
      !||====================================================================
      !||    eikonal_remove_first           ../starter/source/initial_conditions/detonation/eikonal_remove_first.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine eikonal_remove_first(priority_queue_id,priority_queue_tt,n_queue)
!! \brief Remove the first element of the list.
!! \details When first element is calculated it is removed. List is still ordered (ascending order for arrival time)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : ep21
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(inout) :: n_queue  ! size
          integer, intent(inout) :: priority_queue_id(n_queue) ! list of id (1:neltdet
          my_real, intent(inout) :: priority_queue_tt(n_queue) ! list of arrival time
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          do i = 2, n_queue
            priority_queue_id(i-1) = priority_queue_id(i)
            priority_queue_tt(i-1) = priority_queue_tt(i)
          end do
          priority_queue_id(n_queue) = 0
          priority_queue_tt(n_queue) = ep21
          n_queue = n_queue - 1
        end subroutine eikonal_remove_first
! ----------------------------------------------------------------------------------------------------------------------
        end module eikonal_remove_first_mod
