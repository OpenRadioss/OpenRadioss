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
      module get_segment_normal_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine computes the normals of a segment
        subroutine get_segment_normal( segment_id,segment_node_id,segment_position,normal,intbuf_tab,numnod,x )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero,em20,fourth
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
          integer, intent(in) :: segment_id  !< id of the segment
          integer, intent(in) :: numnod !< number of node
          integer, dimension(4),intent(inout) :: segment_node_id
          my_real, dimension(3), intent(inout) :: segment_position !< coordinates of the segment barycentre
          my_real, dimension(3), intent(inout) :: normal !< normal of the segment
          type(intbuf_struct_), intent(in) :: intbuf_tab    !< interface data 
          my_real, dimension(3,numnod), intent(in) :: x !< nodal position
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          integer :: node_id,elem_id
          my_real :: xx13,yy13,zz13,xx24,yy24,zz24
          my_real :: nor1,nor2,nor3
          my_real :: area,dds
          my_real :: xc,yc,zc
          my_real, dimension(4) :: xx1,xx2,xx3
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
          segment_position(1) = zero 
          segment_position(2) = zero 
          segment_position(3) = zero
          do i=1,4
            node_id = intbuf_tab%irectm(4*(segment_id-1)+i) ! get the node id
            segment_node_id(i) = node_id
            xx1(i) = x(1,node_id)
            xx2(i) = x(2,node_id)
            xx3(i) = x(3,node_id)
            segment_position(1) = segment_position(1)+fourth*x(1,node_id)
            segment_position(2) = segment_position(2)+fourth*x(2,node_id)
            segment_position(3) = segment_position(3)+fourth*x(3,node_id)   
          enddo    

          xx13 =xx1(3)-xx1(1)
          yy13 =xx2(3)-xx2(1)
          zz13 =xx3(3)-xx3(1)
          xx24 =xx1(4)-xx1(2)
          yy24 =xx2(4)-xx2(2)
          zz24 =xx3(4)-xx3(2)

          nor1=yy13*zz24-zz13*yy24
          nor2=zz13*xx24-xx13*zz24
          nor3=xx13*yy24-yy13*xx24
          area= max(em20,sqrt(nor1*nor1+nor2*nor2+nor3*nor3))
          normal(1)=nor1/area
          normal(2)=nor2/area
          normal(3)=nor3/area
          ! -------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_segment_normal
      end module get_segment_normal_mod
