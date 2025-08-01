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
!||    get_segment_interface_id_mod             ../engine/source/interfaces/interf/get_segment_interface_id.F90
!||--- called by ------------------------------------------------------
!||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||====================================================================
      module get_segment_interface_id_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine checks if some segments belong to a given interface /TYPE25 "my_interface_id" and if the segments have no neighbours
!||====================================================================
!||    get_segment_interface_id                 ../engine/source/interfaces/interf/get_segment_interface_id.F90
!||--- called by ------------------------------------------------------
!||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||--- calls      -----------------------------------------------------
!||    get_segment_edge                         ../engine/source/interfaces/interf/get_segment_edge.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                             ../common_source/modules/constant_mod.F
!||    get_segment_edge_mod                     ../engine/source/interfaces/interf/get_segment_edge.F90
!||    intbufdef_mod                            ../common_source/modules/interfaces/intbufdef_mod.F90
!||    shooting_node_mod                        ../engine/share/modules/shooting_node_mod.F
!||====================================================================
        subroutine get_segment_interface_id( ninter,nb_segment,list_segment_id, &
                                             my_interface_id,my_reduced_nb,my_reduced_list,my_reduced_neighbour, &
                                             shoot_struct,intbuf_tab,node_id_1,node_id_2,n_iedge)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use shooting_node_mod , only : shooting_node_type
          use constant_mod , only : zero
          use intbufdef_mod , only : intbuf_struct_
          use get_segment_edge_mod , only : get_segment_edge
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
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
          integer, intent(in) :: node_id_1,node_id_2 ! nodes ids for edge search
          integer, dimension(nb_segment), intent(inout) :: n_iedge  ! list of neighbours edges
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,k,j
          integer :: id_inter
          integer :: nin,number_inter
          integer :: segment_id,iedge
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
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
            ! find the edge id of n_segment_id
            call get_segment_edge( segment_id,node_id_1,node_id_2,iedge,intbuf_tab(nin) )
            if(nin==my_interface_id.and.iedge >0) then
              if(intbuf_tab(nin)%stfm(segment_id)>zero) then
                my_reduced_nb = my_reduced_nb + 1
                my_reduced_list(my_reduced_nb,1) = segment_id !local segment id
                my_reduced_list(my_reduced_nb,2) = list_segment_id(i) ! global segment id
                n_iedge(my_reduced_nb) = iedge
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
