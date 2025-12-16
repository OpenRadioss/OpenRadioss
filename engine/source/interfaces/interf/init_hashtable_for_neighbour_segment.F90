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
!||    init_hashtable_for_neighbour_segment_mod   ../engine/source/interfaces/interf/init_hashtable_for_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    init_nodal_state                           ../engine/source/interfaces/interf/init_nodal_state.F
!||====================================================================
      module init_hashtable_for_neighbour_segment_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine initializes the shoot_struct data structure to store the neighbour segment information
!!        (only for interface TYPE25 with erosion or deletion)
!||====================================================================
!||    init_hashtable_for_neighbour_segment   ../engine/source/interfaces/interf/init_hashtable_for_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    init_nodal_state                       ../engine/source/interfaces/interf/init_nodal_state.F
!||--- uses       -----------------------------------------------------
!||    shooting_node_mod                      ../engine/share/modules/shooting_node_mod.F90
!||====================================================================
        subroutine init_hashtable_for_neighbour_segment( npari,ninter,ipari,shoot_struct )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use shooting_node_mod , only : shooting_node_type
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: npari !< number of parameters
          integer, intent(in) :: ninter !< number of interface
          integer, dimension(npari,ninter), intent(in) :: ipari !< interface parameters
          type(shooting_node_type), intent(inout) :: shoot_struct !< structure for shooting node algo
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: ity,idel,erosion_state,nrtm
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          allocate(shoot_struct%neighbour(ninter))
          ! ------------
          do i=1,ninter
            shoot_struct%neighbour(i)%hash_id = -1 ! initialize hash_id with a negative value
            ity = ipari(7,i) ! get the interface id
            idel = ipari(17,i) ! get the idel option
            erosion_state = ipari(100,i) ! get the erosion state
            nrtm = ipari(4,i) ! get the number of segments
            ! ------------
            ! only for interface type25 with erosion or deletion
            if(ity==25.and.(idel/=0.or.erosion_state/=0)) then
              allocate(shoot_struct%neighbour(i)%seg_nb(4*(nrtm+1)))
              allocate(shoot_struct%neighbour(i)%seg_index(4*(nrtm+1)))
              allocate(shoot_struct%neighbour(i)%mvoisin_index(4*nrtm))
              shoot_struct%neighbour(i)%seg_nb(1:4*(nrtm+1)) = 0
              shoot_struct%neighbour(i)%seg_index(1:4*(nrtm+1)) = 0
              shoot_struct%neighbour(i)%mvoisin_index(1:4*nrtm) = 0
            else
              allocate(shoot_struct%neighbour(i)%seg_nb(0))
              allocate(shoot_struct%neighbour(i)%seg_index(0))
              allocate(shoot_struct%neighbour(i)%mvoisin_index(0))
            endif
            ! ------------
          enddo
          ! ------------
          ! --------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_hashtable_for_neighbour_segment
      end module init_hashtable_for_neighbour_segment_mod
