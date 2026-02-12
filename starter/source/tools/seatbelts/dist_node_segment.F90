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
!||    dist_node_segment_mod   ../starter/source/tools/seatbelts/dist_node_segment.F90
!||--- called by ------------------------------------------------------
!||    find_prev_next_nodes    ../starter/source/tools/seatbelts/find_prev_next_nodes.F90
!||    hm_read_guided_cable    ../starter/source/tools/seatbelts/hm_read_guided_cable.F90
!||====================================================================
      module dist_node_segment_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \routine to search for rbodies that are referenced by slipring and fill slipring data_structure with
!||====================================================================
!||    dist_node_segment      ../starter/source/tools/seatbelts/dist_node_segment.F90
!||--- called by ------------------------------------------------------
!||    find_prev_next_nodes   ../starter/source/tools/seatbelts/find_prev_next_nodes.F90
!||    hm_read_guided_cable   ../starter/source/tools/seatbelts/hm_read_guided_cable.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine dist_node_segment(node1,node2,anchor_node,numnod,x,dist)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod , only : zero,one
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: node1                                   !< first node of segment
          integer, intent(in) :: node2                                   !< second node of segment
          integer, intent(in) :: anchor_node                             !< node to project
          integer, intent(in) :: numnod                                  !< number of nodes
          real(kind=WP), intent(out) :: dist                             !< distance from node1 to projection
          real(kind=WP), dimension(3), intent(in) :: x(3,numnod)         !< node coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP):: alpha,seg_vec(3),veca(3)
          real(kind=WP):: seg_len2
          real(kind=WP):: x_proj(3)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          seg_vec(1:3) = x(1:3,node2) - x(1:3,node1)
          veca(1:3) = x(1:3,anchor_node) - x(1:3,node1)
          seg_len2 = seg_vec(1)**2 + seg_vec(2)**2 + seg_vec(3)**2
          if (seg_len2 > zero) then
            alpha = (seg_vec(1)*veca(1)+seg_vec(2)*veca(2)+seg_vec(3)*veca(3))/seg_len2
          else
            alpha = zero
          endif
          alpha = max(min(alpha,one),zero)
!
          x_proj(1:3) = x(1:3,node1) + alpha*seg_vec(1:3)
!
          dist = (x_proj(1)-x(1,anchor_node))**2 + (x_proj(2)-x(2,anchor_node))**2 + (x_proj(3)-x(3,anchor_node))**2
          if (dist > zero) then
            dist = sqrt(dist)
          else
            dist = zero
          endif  
!          
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine dist_node_segment
      end module dist_node_segment_mod