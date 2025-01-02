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
      !||    get_segment_normal_mod                   ../engine/source/interfaces/interf/get_segment_normal.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    get_segment_orientation                  ../engine/source/interfaces/interf/get_segment_orientation.F90
      !||====================================================================
      module get_segment_normal_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine computes the normals of a segment
      !||====================================================================
      !||    get_segment_normal                       ../engine/source/interfaces/interf/get_segment_normal.F90
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    get_segment_orientation                  ../engine/source/interfaces/interf/get_segment_orientation.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                             ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine get_segment_normal( segment_id,segment_node_id,segment_position,normal,intbuf_tab,numnod,x )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero,em20,fourth,third
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
          integer :: node_id_3,node_id_4
          integer :: node_number
          my_real :: xx13,yy13,zz13,xx24,yy24,zz24
          my_real :: nor1,nor2,nor3
          my_real :: area,dds
          my_real :: xc,yc,zc
          my_real :: ratio
          my_real, dimension(4) :: xx1,xx2,xx3
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
          ! normal to the segment
          segment_position(1) = zero 
          segment_position(2) = zero 
          segment_position(3) = zero
          node_number = 4
          node_id_3 = intbuf_tab%irectm(4*(segment_id-1)+3)
          node_id_4 = intbuf_tab%irectm(4*(segment_id-1)+4)
          ratio = fourth
          if(node_id_3==node_id_4) then
            node_number = 3
            ratio = third
          endif
          do i=1,node_number
            node_id = intbuf_tab%irectm(4*(segment_id-1)+i) ! get the node id
            segment_node_id(i) = node_id
            xx1(i) = x(1,node_id)
            xx2(i) = x(2,node_id)
            xx3(i) = x(3,node_id)
            segment_position(1) = segment_position(1)+ratio*x(1,node_id)
            segment_position(2) = segment_position(2)+ratio*x(2,node_id)
            segment_position(3) = segment_position(3)+ratio*x(3,node_id)   
          enddo    
          if(node_id_3==node_id_4) then
            node_id = intbuf_tab%irectm(4*(segment_id-1)+4) ! get the node id
            segment_node_id(4) = node_id
            xx1(4) = x(1,node_id)
            xx2(4) = x(2,node_id)
            xx3(4) = x(3,node_id)
          endif

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
