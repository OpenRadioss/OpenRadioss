!copyright>        openradioss
!copyright>        copyright (c) 1986-2024 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module get_segment_orientation_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine computes the orientation of a new active segment and changes the order of the node if the orientation is changed
        subroutine get_segment_orientation( segment_id,s_elem_state,nixs,numels,numnod, &
                                            elem_state,ixs,x,intbuf_tab )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero,one_over_8
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
          integer, dimension(4) :: segment_node_id
          my_real :: dds
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
          ! loop over the new active segment/surface
          ! normal to the segment
          call get_segment_normal( segment_id,segment_node_id,segment_position,normal,intbuf_tab,numnod,x )

          ! orientation of the element
          elem_id = intbuf_tab%ielem_m(2*(segment_id-1)+1) ! get the element id 
          xc = zero
          yc = zero
          zc = zero

          do j=1,8
            node_id = ixs(j+1,elem_id)
            xc = xc+x(1,node_id)
            yc = yc+x(2,node_id)
            zc = zc+x(3,node_id)
          enddo
          xc=xc*one_over_8
          yc=yc*one_over_8
          zc=zc*one_over_8

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
