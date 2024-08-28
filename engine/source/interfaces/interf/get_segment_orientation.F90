!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    get_segment_orientation_mod              ../engine/source/interfaces/interf/get_segment_orientation.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||====================================================================
      module get_segment_orientation_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine computes the orientation of a new active segment and changes the order of the node if the orientation is changed
      !||====================================================================
      !||    get_segment_orientation                  ../engine/source/interfaces/interf/get_segment_orientation.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||--- calls      -----------------------------------------------------
      !||    get_segment_normal                       ../engine/source/interfaces/interf/get_segment_normal.F90
      !||    myqsort_int                              ../common_source/tools/sort/myqsort_int.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                             ../common_source/modules/constant_mod.F
      !||    get_segment_normal_mod                   ../engine/source/interfaces/interf/get_segment_normal.F90
      !||====================================================================
        subroutine get_segment_orientation( segment_id,s_elem_state,nixs,numels,numnod, &
                                            elem_state,ixs,x,intbuf_tab )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero,one
          use intbufdef_mod , only : intbuf_struct_
          use get_segment_normal_mod , only : get_segment_normal
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
          integer, intent(in) :: segment_id  !< id of the segment
          integer, intent(in) :: s_elem_state !< dim of elem_state
          integer, intent(in) :: nixs !< 1rst dim of "ixs" array
          integer, intent(in) :: numels !< number of solid element
          integer, intent(in) :: numnod !< number of node
          logical, dimension(s_elem_state), intent(in) :: elem_state !< state of the element : on or off
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid element data
          my_real, dimension(3,numnod), intent(in) :: x !< nodal position
          type(intbuf_struct_), intent(inout) :: intbuf_tab    !< interface data 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          integer :: node_id,elem_id
          integer :: real_nb_node,error
          integer, dimension(4) :: segment_node_id
          integer, dimension(8) :: list,node_id_list,perm_list
          my_real :: dds,ratio
          my_real :: xc,yc,zc
          my_real, dimension(3) :: segment_position ! coordinates of the segment barycentre
          my_real, dimension(3) :: normal ! normal of the segment
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to mimimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! -------------------------
          ! normal to the segment
          call get_segment_normal( segment_id,segment_node_id,segment_position,normal,intbuf_tab,numnod,x )

          ! orientation of the element
          elem_id = intbuf_tab%ielem_m(2*(segment_id-1)+1) ! get the element id 
          xc = zero
          yc = zero
          zc = zero

          ! ----------------
          do j=1,8
            list(j) = ixs(j+1,elem_id) ! get the node id of the element : a node can appeared several time in ixs... (tetra or degenerated element)
          enddo
          call myqsort_int(8,list,perm_list,error) ! sort the list 
          ! ----------------
          
          ! ----------------
          ! check the number of node of the element & save the list of node
          node_id = list(1)
          real_nb_node = 1
          node_id_list(real_nb_node) = node_id
          do j=2,8
            if(node_id/=list(j)) then
              real_nb_node = real_nb_node + 1
              node_id = list(j)
              node_id_list(real_nb_node) = node_id
            endif
          enddo
          ! ----------------

          ! ----------------
          ratio = one / real_nb_node
          do j=1,real_nb_node
            node_id = node_id_list(j)
            xc = xc+x(1,node_id)
            yc = yc+x(2,node_id)
            zc = zc+x(3,node_id)
          enddo
          xc=xc*ratio
          yc=yc*ratio
          zc=zc*ratio
          ! ----------------

          dds=normal(1)*(xc-segment_position(1))+normal(2)*(yc-segment_position(2))+normal(3)*(zc-segment_position(3))
          
          ! check if the element associated to the segment "segment_id" is deleted
          ! if the element is deleted --> need to consider the opposite condition for DDS
          !     c = barycentre of the element
          !     s = barycentre of the segment
          !
          !    element is on
          !     ________
          !    |        |
          !    |   c--->|     s----> 
          !    |________|
          !
          !
          !    element is off
          !     ________
          !    |        |
          !    |   c--->| <---- s 
          !    |________|
          !
          !
          if( .not.elem_state( elem_id ) ) then
            dds = -dds
          endif

          if(dds  >= zero) then
            if(segment_node_id(3)==segment_node_id(4)) then
              intbuf_tab%irectm(4*(segment_id-1)+1)=segment_node_id(2)
              intbuf_tab%irectm(4*(segment_id-1)+2)=segment_node_id(1)
            else
              do i=1,4
                intbuf_tab%irectm(4*(segment_id-1)+i)=segment_node_id(4-i+1)
              enddo
            endif
          endif
          ! -------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_segment_orientation
      end module get_segment_orientation_mod
