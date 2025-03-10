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
      !||    get_segment_criteria_mod   ../engine/source/interfaces/interf/get_segment_criteria.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface      ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    update_neighbour_segment   ../engine/source/interfaces/interf/update_neighbour_segment.F90
      !||====================================================================
      module get_segment_criteria_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Ths routine computes the angle between 2 segmet's normals
      !||====================================================================
      !||    get_segment_criteria       ../engine/source/interfaces/interf/get_segment_criteria.F90
      !||--- called by ------------------------------------------------------
      !||    update_neighbour_segment   ../engine/source/interfaces/interf/update_neighbour_segment.F90
      !||====================================================================
        subroutine get_segment_criteria( criteria,local_normal,remote_normal )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          my_real, intent(inout) :: criteria
          my_real, dimension(3), intent(in) :: local_normal !< normal of the local segment
          my_real, dimension(3), intent(in) :: remote_normal !< normal of the remote segment
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          criteria = local_normal(1)*remote_normal(1) + local_normal(2)*remote_normal(2) + local_normal(3)*remote_normal(3) ! compute the angle between the 2 segments "segment_id" and the "n_segment_id"
          ! --------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_segment_criteria
      end module get_segment_criteria_mod
