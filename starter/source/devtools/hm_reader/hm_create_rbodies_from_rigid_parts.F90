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
!||    hm_create_rbodies_from_rigid_parts_mod   ../starter/source/devtools/hm_reader/hm_create_rbodies_from_rigid_parts.F90
!||====================================================================
      module hm_create_rbodies_from_rigid_parts_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief create /RBODY for RIGID parts (/PART with Irigid)
!! \details This routine create a /RBODY for each /PART with Irigid .
!||====================================================================
!||    hm_create_rbodies_from_rigid_parts    ../starter/source/devtools/hm_reader/hm_create_rbodies_from_rigid_parts.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine hm_create_rbodies_from_rigid_parts(NB_NEWRBODIES,NEW_RBODY_TO_PART,NEW_RBODY_ID)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: NB_NEWRBODIES
          integer, dimension(NB_NEWRBODIES), intent(out) :: NEW_RBODY_TO_PART
          integer, dimension(NB_NEWRBODIES), intent(out) :: NEW_RBODY_ID
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call cpp_create_rbodies_from_rigid_parts(NEW_RBODY_TO_PART,NEW_RBODY_ID)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_create_rbodies_from_rigid_parts
      end module hm_create_rbodies_from_rigid_parts_mod
