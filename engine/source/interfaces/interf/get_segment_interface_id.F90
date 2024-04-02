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
      module get_segment_interface_id_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine checks if some segments belong to a given interface /TYPE25 "my_interface_id" and if the segments have no neighbours
        subroutine get_segment_interface_id( ninter,nb_segment,list_segment_id, &
                                             my_interface_id,my_reduced_nb,my_reduced_list,my_reduced_neighbour, &
                                             shoot_struct,intbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use shooting_node_mod , only : shooting_node_type
          use constant_mod , only : zero
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
          integer, intent(in) :: ninter !< number of interface
          integer, intent(in) :: nb_segment !< number of connected segment (on all interfaces)
          integer, intent(in) :: my_interface_id !< interface id of the new segment
          integer, intent(inout) :: my_reduced_nb !< number of connected segment on the interface "my_interface_id"
          integer, dimension(nb_segment), intent(in) :: list_segment_id !< id of the segment
          integer, dimension(nb_segment,2), intent(inout) :: my_reduced_list !< id of the segment belonging to the interface "my_interface_id"
          integer, dimension(nb_segment,4), intent(inout) :: my_reduced_neighbour !< boolean : 1 if the segment has already a neighbour 
          type(shooting_node_type), intent(inout) :: shoot_struct !< structure for shooting node algo
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab    !< interface data 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,k,j
          integer :: id_inter
          integer :: nin,number_inter
          integer :: segment_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to mimimum ]
          integer , external :: dichotomic_search_i_asc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          my_reduced_nb = 0
          number_inter = shoot_struct%shift_interface(ninter+1,2)
          ! -------------------------
          do i=1,nb_segment
            k = list_segment_id(i)  ! get the global segment id
            id_inter = dichotomic_search_i_asc(k, shoot_struct%shift_interface(1,1), number_inter+1) ! find the interface of the segment
            nin = shoot_struct%shift_interface(id_inter,2)
            segment_id = k - shoot_struct%shift_interface(id_inter,1) + 1 ! get the segment id in the nin interface  
            my_reduced_neighbour(i,1:4) = 0
            if(nin==my_interface_id) then
              if(intbuf_tab(nin)%stfm(segment_id)>zero) then
                my_reduced_nb = my_reduced_nb + 1
                my_reduced_list(my_reduced_nb,1) = segment_id !local segment id
                my_reduced_list(my_reduced_nb,2) = list_segment_id(i) ! global segment id
                do j=1,4
                  if(intbuf_tab(nin)%mvoisin(4*(segment_id-1)+j)/=0) my_reduced_neighbour(my_reduced_nb,j) = 1
                enddo
              endif
            endif
          enddo
          ! -------------------------
!
          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_segment_interface_id
      end module get_segment_interface_id_mod
