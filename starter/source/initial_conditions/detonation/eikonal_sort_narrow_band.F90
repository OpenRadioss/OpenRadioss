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
      !||    eikonal_sort_narrow_band_mod   ../starter/source/initial_conditions/detonation/eikonal_sort_narrow_band.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||====================================================================
      module eikonal_sort_narrow_band_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
      !||====================================================================
      !||    eikonal_sort_narrow_band         ../starter/source/initial_conditions/detonation/eikonal_sort_narrow_band.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method     ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine eikonal_sort_narrow_band(priority_queue_id,priority_queue_tt,n_queue)
!! \brief The narrow band is list of narrow element ordered along arrival time (ascending order)
!! \details Re-ordering required whenever a new element ios added in the list.
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use insertion_sort_mod , only : real_insertion_sort_with_index
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
          integer, intent(in) :: n_queue  ! size
          integer, intent(inout) :: priority_queue_id(n_queue) ! list of id (1:neltdet
          my_real, intent(inout) :: priority_queue_tt(n_queue) ! list of arrival time
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer,allocatable,dimension(:) :: indx  ! to retrieve new order from sorting function
          integer,allocatable,dimension(:) :: itmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate (indx(n_queue))
          allocate (itmp(n_queue))
          call real_insertion_sort_with_index(priority_queue_tt, indx, n_queue)
          !sorting also identifier :
          itmp(1:n_queue) = priority_queue_id(1:n_queue)
          do i = 1, n_queue
            priority_queue_id(i) = itmp(indx(i))
          end do
          deallocate(indx)
          deallocate(itmp)
        end subroutine eikonal_sort_narrow_band
! ----------------------------------------------------------------------------------------------------------------------
        end module eikonal_sort_narrow_band_mod
