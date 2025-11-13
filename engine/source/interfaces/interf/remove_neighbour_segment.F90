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
!||    remove_neighbour_segment_mod   ../engine/source/interfaces/interf/remove_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    check_remote_surface_state     ../engine/source/interfaces/interf/check_remote_surface_state.F
!||====================================================================
      module remove_neighbour_segment_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine removes the neighbourhood of a deleted segment
!||====================================================================
!||    remove_neighbour_segment     ../engine/source/interfaces/interf/remove_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    check_remote_surface_state   ../engine/source/interfaces/interf/check_remote_surface_state.F
!||--- calls      -----------------------------------------------------
!||    c_hash_find                  ../common_source/tools/container/c_hash_table.cpp
!||--- uses       -----------------------------------------------------
!||    debug_mod                    ../engine/share/modules/debug_mod.F
!||    intbufdef_mod                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    shooting_node_mod            ../engine/share/modules/shooting_node_mod.F90
!||====================================================================
        subroutine remove_neighbour_segment( nin,segment_id,intbuf_tab,shoot_struct )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod , only : intbuf_struct_
          use shooting_node_mod , only : shooting_node_type
          use debug_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nin !< interface ID
          integer, intent(in) :: segment_id !< id of the deleted segment
          type(intbuf_struct_), intent(inout) :: intbuf_tab !< interface data structure
          type(shooting_node_type), intent(in) :: shoot_struct !< structure for shooting node algo
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ijk
          integer :: total_neighbour_seg,address,my_index
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          ! ------------
          ijk = -1
          call c_hash_find( shoot_struct%neighbour(nin)%hash_id,segment_id,ijk ) ! find the address of seg_nb & seg_index arrays for the deleted segment "segment_id"
          if(ijk/=-1) then
            total_neighbour_seg = shoot_struct%neighbour(nin)%seg_nb(ijk) ! get the number of neighbouring segment
            address = shoot_struct%neighbour(nin)%seg_index(ijk) ! get the address of mvoisin_index array 
            ! loop over the number of neighbouring segment
            do i=1,total_neighbour_seg
              my_index = shoot_struct%neighbour(nin)%mvoisin_index(address+i-1) ! get the address of mvoisin
              intbuf_tab%mvoisin(my_index) = 0 ! remove the segment
            enddo
          endif
          ! ------------
          ! --------------------------

!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine remove_neighbour_segment
      end module remove_neighbour_segment_mod
