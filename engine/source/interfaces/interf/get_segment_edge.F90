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
      !||    get_segment_edge_mod                     ../engine/source/interfaces/interf/get_segment_edge.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    get_segment_interface_id                 ../engine/source/interfaces/interf/get_segment_interface_id.F90
      !||====================================================================
      module get_segment_edge_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine finds the edge id with the 2 nodes "my_node_id_1" & "my_node_id_2"
      !||====================================================================
      !||    get_segment_edge           ../engine/source/interfaces/interf/get_segment_edge.F90
      !||--- called by ------------------------------------------------------
      !||    get_segment_interface_id   ../engine/source/interfaces/interf/get_segment_interface_id.F90
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine get_segment_edge( my_segment_id,my_node_id_1,my_node_id_2,my_iedge,intbuf_tab )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod , only : intbuf_struct_
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
          integer, intent(in) :: my_segment_id  !< id of the segment
          integer, intent(in) :: my_node_id_1 !< node id of the connected segment
          integer, intent(in) :: my_node_id_2 !< node id of the connected segment
          integer, intent(inout) :: my_iedge !< edge id 
          type(intbuf_struct_), intent(in) :: intbuf_tab    !< interface data 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: j,iedge
          integer :: edge_number,itria
          integer :: node_id_1,node_id_2,node_id_3,node_id_4
          integer, dimension(2,4) :: edge_list
          data edge_list /1,2,     &
                          2,3,     &
                          3,4,     &
                          4,1/  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! -------------------------
          ! check if the segment has 3 or 4 nodes (triangle or quadrangle)
          edge_number = 4
          itria= 0
          node_id_3 = intbuf_tab%irectm(4*(my_segment_id-1)+3)
          node_id_4 = intbuf_tab%irectm(4*(my_segment_id-1)+4)
          if( node_id_3==node_id_4 ) itria = 1
          ! ------
          ! loop over the edge of the segment
          my_iedge  = -1
          do j = 1, edge_number
            iedge = j
            if(itria==1.and.iedge==3 ) cycle
            ! get the nodes of the edge
            node_id_1 = intbuf_tab%irectm(4*(my_segment_id-1)+edge_list(1,j))
            node_id_2 = intbuf_tab%irectm(4*(my_segment_id-1)+edge_list(2,j))
            ! ------   
            ! check if the nodes of the edge are the 2 remote nodes to find the edge id

            if( (my_node_id_1==node_id_1.and.my_node_id_2==node_id_2).or.               &
                (my_node_id_1==node_id_2.and.my_node_id_2==node_id_1)        ) then
              my_iedge = iedge
            endif
          enddo
          ! -------------------------
!
          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_segment_edge
      end module get_segment_edge_mod
