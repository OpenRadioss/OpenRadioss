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
!||    ebcs_cyclic_surface_matching_mod   ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching.F90
!||--- called by ------------------------------------------------------
!||    iniebcs                            ../starter/source/boundary_conditions/ebcs/iniebcs.F
!||====================================================================
      module ebcs_cyclic_surface_matching_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief check surface 1 (nseg) and surface 2 (nseg)
!! \details storing data in linear arrays %elem_list and %node_lide with same order 1:nseg and nseg+1 : nseg+nseg
!! \details number of segment already match (checked with Reader subroutine)
!||====================================================================
!||    ebcs_cyclic_surface_matching          ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching.F90
!||--- called by ------------------------------------------------------
!||    iniebcs                               ../starter/source/boundary_conditions/ebcs/iniebcs.F
!||--- calls      -----------------------------------------------------
!||    ebcs_cyclic_surface_matching_2d       ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_2d.F90
!||    ebcs_cyclic_surface_matching_3d       ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_3d.F90
!||--- uses       -----------------------------------------------------
!||    ebcs_cyclic_surface_matching_2d_mod   ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_2d.F90
!||    ebcs_cyclic_surface_matching_3d_mod   ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_3d.F90
!||====================================================================
        subroutine ebcs_cyclic_surface_matching(ebcs_cyclic, ebcs,  n2d, numnod, X)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use ebcs_mod , only : t_ebcs_cyclic, t_ebcs
          use groupdef_mod , only : surf_
          use ebcs_cyclic_surface_matching_2d_mod , only : ebcs_cyclic_surface_matching_2d
          use ebcs_cyclic_surface_matching_3d_mod , only : ebcs_cyclic_surface_matching_3d
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(t_ebcs_cyclic), intent(in) :: ebcs_cyclic   !< ebcs data structure (specific data structure)
          type(t_ebcs), target, intent(inout) :: ebcs      !< common data structure
          integer, intent(in) :: n2d                       !< 2d/3d flag  0:3d, 1:axi, 2:plane strain
          integer,intent(in) :: numnod
          real(kind=WP),intent(in) :: X(3,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if(n2d == 0)then
            call ebcs_cyclic_surface_matching_3d(ebcs_cyclic, ebcs, numnod, X)
          else
            call ebcs_cyclic_surface_matching_2d(ebcs_cyclic, ebcs, numnod, X)
          end if
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine ebcs_cyclic_surface_matching
      end module ebcs_cyclic_surface_matching_mod
