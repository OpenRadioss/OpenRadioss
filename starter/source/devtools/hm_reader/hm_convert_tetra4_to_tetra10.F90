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
!||    hm_convert_tetra4_to_tetra10_mod   ../starter/source/devtools/hm_reader/hm_convert_tetra4_to_tetra10.F90
!||====================================================================
      module hm_convert_tetra4_to_tetra10_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief convert TETRA4 elements to TETRA10 elements in case PROPERTY card with ITETRA4=1 is used
!! \details This routine reads all TETRA4 elements and convert them to TETRA10 elements by adding mid-side nodes.
!||====================================================================
!||    hm_convert_tetra4_to_tetra10    ../starter/source/devtools/hm_reader/hm_convert_tetra4_to_tetra10.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine hm_convert_tetra4_to_tetra10(itetra4toconsider)
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: itetra4toconsider
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call cpp_convert_tetra4_to_tetra10(itetra4toconsider)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_convert_tetra4_to_tetra10
      end module hm_convert_tetra4_to_tetra10_mod