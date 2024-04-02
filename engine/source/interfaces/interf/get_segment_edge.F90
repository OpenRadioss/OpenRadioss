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
      module get_segment_edge_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine finds the edge id with the 2 nodes "my_node_id_1" & "my_node_id_2"
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
          integer :: edge_number
          integer :: node_id_1,node_id_2,node_id_3,node_id_4
          integer, dimension(2,4) :: edge_list
          data edge_list /1,2,     &
                          2,3,     &
                          4,1,     &
                          3,4/  
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to mimimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! -------------------------
          ! check if the segment has 3 or 4 nodes (triangle or quadrangle)
          edge_number = 4
          node_id_3 = intbuf_tab%irectm(4*(my_segment_id-1)+3)
          node_id_4 = intbuf_tab%irectm(4*(my_segment_id-1)+4)
          if( node_id_3==node_id_4 ) edge_number = 3
          ! ------
          ! loop over the edge of the segment
          my_iedge  = -1
          do j = 1, edge_number
            iedge = j
            if(j==3.and.edge_number==3) iedge = 4
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
