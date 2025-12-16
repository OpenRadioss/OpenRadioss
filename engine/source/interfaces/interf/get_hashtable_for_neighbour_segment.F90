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
!||    get_hashtable_for_neighbour_segment_mod   ../engine/source/interfaces/interf/get_hashtable_for_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    init_nodal_state                          ../engine/source/interfaces/interf/init_nodal_state.F
!||    update_neighbour_segment                  ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||====================================================================
      module get_hashtable_for_neighbour_segment_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine initializes a hash table. The hash table is used to remove the neighborhood of a deleted segment
!||====================================================================
!||    get_hashtable_for_neighbour_segment   ../engine/source/interfaces/interf/get_hashtable_for_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    init_nodal_state                      ../engine/source/interfaces/interf/init_nodal_state.F
!||    update_neighbour_segment              ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||--- calls      -----------------------------------------------------
!||    c_delete_hash                         ../common_source/tools/container/c_hash_table.cpp
!||    c_hash_find                           ../common_source/tools/container/c_hash_table.cpp
!||    c_hash_insert                         ../common_source/tools/container/c_hash_table.cpp
!||    c_new_hash                            ../common_source/tools/container/c_hash_table.cpp
!||--- uses       -----------------------------------------------------
!||    debug_mod                             ../engine/share/modules/debug_mod.F
!||    intbufdef_mod                         ../common_source/modules/interfaces/intbufdef_mod.F90
!||    shooting_node_mod                     ../engine/share/modules/shooting_node_mod.F90
!||====================================================================
        subroutine get_hashtable_for_neighbour_segment( nin,npari,ninter,ipari,intbuf_tab,shoot_struct )
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
          integer, intent(in) :: nin !< interface id
          integer, intent(in) :: npari !< number of parameters
          integer, intent(in) :: ninter !< number of interface
          integer, dimension(npari,ninter), intent(in) :: ipari !< interface parameters
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab !< interface data structure
          type(shooting_node_type), intent(inout) :: shoot_struct !< structure for shooting node algo
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,ijk
          integer :: nrtm
          integer :: local_seg_nb,segment_id,address
          integer, dimension(:), allocatable :: tmp_address
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          nrtm = ipari(4,nin) ! get the number of segment
          ! ------------
          ! hash initialization
          if(shoot_struct%neighbour(nin)%hash_id==-1) then
            shoot_struct%neighbour(nin)%hash_id = nin
          else
            call c_delete_hash( shoot_struct%neighbour(nin)%hash_id )
          end if
          call c_new_hash( shoot_struct%neighbour(nin)%hash_id, 4*(nrtm+1) ) ! create a new hash table
          ! ------------

          ! ------------
          ! loop over the segment's neighbour to initialize the hash table
          local_seg_nb = 0
          do i=1,nrtm
            do j=1,4
              segment_id = intbuf_tab(nin)%mvoisin(4*(i-1)+j) ! get the neighbouring segment id
              if(segment_id/=0) then
                ijk = -1
                call c_hash_find( shoot_struct%neighbour(nin)%hash_id,segment_id,ijk ) ! check if "segment_id id" is already in the hash table
                if(ijk==-1) then  ! no --> need to add it
                  local_seg_nb = local_seg_nb + 1 ! count the number of segments
                  call c_hash_insert( shoot_struct%neighbour(nin)%hash_id,segment_id,local_seg_nb )
                  shoot_struct%neighbour(nin)%seg_nb(local_seg_nb) = 1
                else
                  shoot_struct%neighbour(nin)%seg_nb(ijk) = shoot_struct%neighbour(nin)%seg_nb(ijk) + 1
                end if
              endif
            enddo
          enddo
          ! ------------

          ! ------------
          allocate(tmp_address(local_seg_nb+1))
          tmp_address(1:local_seg_nb+1) = 0
          shoot_struct%neighbour(nin)%seg_index(1:local_seg_nb+1) = 0
          shoot_struct%neighbour(nin)%seg_index(1) = 1
          do i=1,local_seg_nb
            shoot_struct%neighbour(nin)%seg_index(i+1) = shoot_struct%neighbour(nin)%seg_index(i) + &
              shoot_struct%neighbour(nin)%seg_nb(i)
          end do
          ! ------------

          ! ------------
          do i=1,nrtm
            do j=1,4
              segment_id = intbuf_tab(nin)%mvoisin(4*(i-1)+j) ! get the neighbouring segment id
              if(segment_id/=0) then
                call c_hash_find( shoot_struct%neighbour(nin)%hash_id,segment_id,ijk )
                address = shoot_struct%neighbour(nin)%seg_index(ijk) + tmp_address(ijk) ! compute the address of %mvoisin_index
                shoot_struct%neighbour(nin)%mvoisin_index(address) = 4*(i-1)+j ! save the address of %mvoisin into %mvoisin_index
                tmp_address(ijk) = tmp_address(ijk) + 1 ! local counter
              endif
            enddo
          enddo
          ! ------------

          deallocate( tmp_address )
          ! ------------
          ! --------------------------

!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_hashtable_for_neighbour_segment
      end module get_hashtable_for_neighbour_segment_mod
