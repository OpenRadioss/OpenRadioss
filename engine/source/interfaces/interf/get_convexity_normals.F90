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
      !||    get_convexity_normals_mod                ../engine/source/interfaces/interf/get_convexity_normals.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||====================================================================
      module get_convexity_normals_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine computes  the tangent vector to a segment around the edge n X e
      !||====================================================================
      !||    get_convexity_normals                    ../engine/source/interfaces/interf/get_convexity_normals.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                             ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine get_convexity_normals( node_id_1,node_id_2,normal,v_convexity,numnod,x )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : em20
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
          integer, intent(in) :: node_id_1,node_id_2,numnod !< number of node
          my_real, dimension(3), intent(in) :: normal !< normal of the segment
          my_real, dimension(3), intent(inout) :: v_convexity !< normal of the segment
          my_real, dimension(3,numnod), intent(in) :: x !< nodal position
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          my_real :: x12,y12,z12
          my_real :: vix,viy,viz
          my_real :: area
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! -------------------------
          ! loop over the new active segment/surface
          ! compute the tangent vector to the segment around the edge n X e

          x12= x(1,node_id_2)-x(1,node_id_1)
          y12= x(2,node_id_2)-x(2,node_id_1)
          z12= x(3,node_id_2)-x(3,node_id_1)
          vix=normal(2)*z12-normal(3)*y12
          viy=normal(3)*x12-normal(1)*z12
          viz=normal(1)*y12-normal(2)*x12
          area= max(em20,sqrt(vix*vix+viy*viy+viz*viz))
          v_convexity(1)=vix/area
          v_convexity(2)=viy/area
          v_convexity(3)=viz/area


! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_convexity_normals
      end module get_convexity_normals_mod
