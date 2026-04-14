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
!||    eikonal_sort_narrow_band_mod   ../starter/source/initial_conditions/detonation/eikonal_sort_narrow_band.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||====================================================================
      module eikonal_sort_narrow_band_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!||====================================================================
!||    eikonal_sort_narrow_band       ../starter/source/initial_conditions/detonation/eikonal_sort_narrow_band.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine eikonal_sort_narrow_band(priority_queue_id,priority_queue_tt,n_queue)
!! \brief The narrow band is list of narrow element ordered along arrival time (ascending order)
!! \details Re-ordering required whenever a new element is added in the list.
!! \details Insertion sort is used because the array is quasi-sorted at each FMM iteration
!! \details (only a few elements are added/updated) => O(n) average per call.
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod , only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: n_queue  ! size
          integer, intent(inout) :: priority_queue_id(n_queue) ! list of id (1:neltdet)
          real(kind=WP), intent(inout) :: priority_queue_tt(n_queue) ! list of arrival time
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii, jj, temp_id
          real(kind=WP) :: key
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Insertion sort : O(n) on quasi-sorted data (FMM context)  -  better performance than stlsort for quasi-sorted data
          do ii = 2, n_queue
            key     = priority_queue_tt(ii)
            temp_id = priority_queue_id(ii)
            jj = ii - 1
            do while (jj >= 1)
              if(priority_queue_tt(jj) <= key)exit
              priority_queue_tt(jj + 1) = priority_queue_tt(jj)
              priority_queue_id(jj + 1) = priority_queue_id(jj)
              jj = jj - 1
            end do
            priority_queue_tt(jj + 1) = key
            priority_queue_id(jj + 1) = temp_id
          end do

        end subroutine eikonal_sort_narrow_band
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_sort_narrow_band_mod
