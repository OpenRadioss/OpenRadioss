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
!||====================================================================
      module remove_neighbour_segment_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine initializes the shoot_struct data structure to store the neighbour segment information 
!!        (only for interface TYPE25 with erosion or deletion)
!||====================================================================
!||====================================================================
        subroutine remove_neighbour_segment( nin,segment_id,intbuf_tab,shoot_struct )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod , only : intbuf_struct_
          use shooting_node_mod , only : shooting_node_type
          use toto
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
          call c_hash_find( shoot_struct%neighbour(nin)%hash_id,segment_id,ijk )
          if(ijk/=-1) then
            total_neighbour_seg = shoot_struct%neighbour(nin)%seg_nb(ijk)
            address = shoot_struct%neighbour(nin)%seg_index(ijk)
            do i=1,total_neighbour_seg
              my_index = shoot_struct%neighbour(nin)%mvoisin_index(address+i-1)
              print*,my_ispmd,my_ncycle," j enleve ",segment_id,intbuf_tab%mvoisin(my_index)
              if(intbuf_tab%mvoisin(my_index)/=segment_id) then
                print*,my_ispmd,my_ncycle," verif 0",segment_id
                print*,my_ispmd,my_ncycle," verif 1",my_index
                print*,my_ispmd,my_ncycle," verif 2",address
                print*,my_ispmd,my_ncycle," verif 3",ijk
                print*,my_ispmd,my_ncycle," verif 4",total_neighbour_seg
                print*,my_ispmd,my_ncycle," AAAARRF "
                stop
              endif
              intbuf_tab%mvoisin(my_index) = 0
            enddo
          endif
          ! ------------
          ! --------------------------

!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine remove_neighbour_segment
      end module remove_neighbour_segment_mod
