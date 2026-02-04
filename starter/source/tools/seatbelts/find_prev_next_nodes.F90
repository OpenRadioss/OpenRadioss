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
!||    init_guide_mod   ../starter/source/tools/seatbelts/init_guide.F90
!||--- called by ------------------------------------------------------
!||    initia                      ../starter/source/elements/initia/initia.F
!||====================================================================
      module find_prev_next_nodes_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \routine to search for the previous and next nodes connected to a given segment
!||====================================================================
!||    init_seatbelt_rbodies   ../starter/source/tools/seatbelts/init_seatbelt_rbodies.F90
!||--- called by ------------------------------------------------------
!||    initia                  ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                  ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod             ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine find_prev_next_nodes(ig,node1,node2,anchor_node,numnod,offset_1d,                  &
                                        snod2el1d,knod2el1d,nod2el1d,numel_1d,nix,                    &
                                        ix,ipart,npart,size_part_to_guide,add_part_to_guide,          &
                                        part_to_guide,x,nodes_found,nodes_next_found,active_segment)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod , only : zero,ep20
          use dist_node_segment_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ig                                      !< if of guide
          integer, intent(in) :: node1                                   !< first node of segment
          integer, intent(in) :: node2                                   !< second node of segment
          integer, intent(in) :: anchor_node                             !< node to project
          integer, intent(in) :: numnod                                  !< number of nodes
          integer, intent(in) :: offset_1d                               !< offset for 1D elements
          integer, intent(in) :: snod2el1d                               !< size of nod2el1d array
          integer, intent(in) :: knod2el1d(numnod+1)                     !< mapping from node to 1D element
          integer, intent(in) :: nod2el1d(snod2el1d)                     !< mapping from node to 1D element   
          integer, intent(in) :: numel_1d                                !< number of 1D elements
          integer, intent(in) :: nix                                     !< number of connectivity per 1D element
          integer, intent(in) :: ix(nix,numel_1d)                        !< 1D element connectivity
          integer, intent(in) :: ipart(numel_1d)                         !< 1D element connectivity
          integer, intent(in) :: npart                                   !< number of parts
          integer, intent(in) :: size_part_to_guide                      !< size of part_to_guide array
          integer, intent(in) :: add_part_to_guide(npart+1)              !< mapping from part to guide
          integer, intent(in) :: part_to_guide(size_part_to_guide)       !< parts connected to guide
          integer, intent(out) :: nodes_found(3)                         !< previous node connected to node1
          integer, intent(out) :: nodes_next_found(2)                    !< next node connected to node2
          integer, intent(out) :: active_segment                         !< active segment index
          real(kind=WP), dimension(3), intent(in) :: x(3,numnod)         !< node coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: j,l
          integer :: elem_cur,node1p,node2p,nodtest1,nodtest2,nodtest3,seg_pos
          integer :: part_connected
          real(kind=WP) :: dist1,dist2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!  
!         check neighbouring springs on node1  
          node1p = 0
          dist1 = ep20
          do l=knod2el1d(node1)+1,knod2el1d(node1+1)              
            if (nod2el1d(l) > offset_1d) then
              elem_cur = nod2el1d(l)-offset_1d
              part_connected = 0
              do j=add_part_to_guide(ipart(elem_cur)),add_part_to_guide(ipart(elem_cur)+1)-1
                if (ig == part_to_guide(j)) part_connected = 1
              enddo
              if ((((node1==ix(2,elem_cur)).and.(node2/=ix(3,elem_cur))).or.                &
                   ((node1==ix(3,elem_cur)).and.(node2/=ix(2,elem_cur)))).and.              &
                    (part_connected==1)) then
                     node1p = ix(2,elem_cur)
                     if (node1p==node1) node1p= ix(3,elem_cur)
                     call dist_node_segment(node1,node1p,anchor_node,numnod,x,dist1) 
              endif
            endif  
          enddo 
!                
!         check neighbouring springs on node2  
          node2p = 0
          dist2 = ep20
          do l=knod2el1d(node2)+1,knod2el1d(node2+1)              
            if (nod2el1d(l) > offset_1d) then
              elem_cur = nod2el1d(l)-offset_1d
              part_connected = 0
              do j=add_part_to_guide(ipart(elem_cur)),add_part_to_guide(ipart(elem_cur)+1)-1
                if (ig == part_to_guide(j)) part_connected = 1
              enddo  
              if ((((node2==ix(2,elem_cur)).and.(node1/=ix(3,elem_cur))).or.                &
                   ((node2==ix(3,elem_cur)).and.(node1/=ix(2,elem_cur)))).and.              &
                    (part_connected==1)) then
                     node2p = ix(2,elem_cur)
                     if (node2p==node2) node2p= ix(3,elem_cur)
                     call dist_node_segment(node2,node2p,anchor_node,numnod,x,dist2)
              endif
            endif 
          enddo             
! 
!         Detrermination of second segment                 
          if (dist1 < dist2) then
!           second segment is node1p-node1            
            nodes_found(1) = node1p
            nodes_found(2) = node1
            nodes_found(3) = node2
            nodes_next_found(2) = node2p                  
            seg_pos = 1     
            nodtest1 = node1p
            nodtest2 = node1
          else
!           second segment is node2-node2p
            nodes_next_found(1) = node1p   
            nodes_found(1) = node1
            nodes_found(2) = node2
            nodes_found(3) = node2p
            seg_pos = 2    
            nodtest1 = node2p
            nodtest2 = node2
          endif 
          active_segment = seg_pos
!          
!         loop to find node_next - node before node1p of after node2p 
          nodtest3 = 0
          do l=knod2el1d(nodtest1)+1,knod2el1d(nodtest1+1)              
            if (nod2el1d(l) > offset_1d) then
              elem_cur = nod2el1d(l)-offset_1d
              part_connected = 0
              do j=add_part_to_guide(ipart(elem_cur)),add_part_to_guide(ipart(elem_cur)+1)-1
                if (ig == part_to_guide(j)) part_connected = 1
              enddo  
              if ((((nodtest1==ix(2,elem_cur)).and.(nodtest2/=ix(3,elem_cur))).or.                &
                   ((nodtest1==ix(3,elem_cur)).and.(nodtest2/=ix(2,elem_cur)))).and.              &
                    (part_connected==1)) then
                     nodtest3 = ix(2,elem_cur)
                     if (nodtest3==nodtest1) nodtest3= ix(3,elem_cur)
              endif
            endif 
          enddo 
          nodes_next_found(seg_pos) = nodtest3                             
!          
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine find_prev_next_nodes
      end module find_prev_next_nodes_mod