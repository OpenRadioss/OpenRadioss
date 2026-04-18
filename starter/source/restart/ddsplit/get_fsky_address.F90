!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    get_fsky_address_mod   ../starter/source/restart/ddsplit/get_fsky_address.F90
!||--- called by ------------------------------------------------------
!||    w_pon                  ../starter/source/restart/ddsplit/w_pon.F
!||====================================================================
      module get_fsky_address_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Assigns a local address to a node within an element if the node matches the specified global node ID and has
!!        not been assigned yet.
!! \details This subroutine searches for a node within an element (using the connectivity array) that matches the given
!!          global node ID. If the node is found and has not already been assigned (as determined by the shift array),
!!          it assigns the provided local address to the node in the address array and updates the shift array to mark
!!          the assignment. The subroutine sets the boolean flag to indicate whether the assignment was successful.
!||====================================================================
!||    get_fsky_address   ../starter/source/restart/ddsplit/get_fsky_address.F90
!||--- called by ------------------------------------------------------
!||    w_pon              ../starter/source/restart/ddsplit/w_pon.F
!||====================================================================
        subroutine get_fsky_address(bool,nixx,ixx_offset,global_elem_id,local_elem_id,local_address,node_nb,global_node_id, &
        & global_elem_nb,local_elem_nb,elm_shft,ixx,s_address,address)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, intent(inout) :: bool !< boolean to indicate if an address has been found
          integer, intent(in) :: nixx !< first dimension of connectivity array
          integer, intent(in) :: ixx_offset !< offset for connectivity array
          integer, intent(in) :: global_elem_id !< global element id
          integer, intent(in) :: local_elem_id !< local element id
          integer, intent(in) :: local_address !< local address to assign
          integer, intent(in) :: node_nb !< number of nodes per element
          integer, intent(in) :: global_node_id !< global node id to find
          integer, intent(in) :: global_elem_nb !< total number of elements
          integer, intent(in) :: local_elem_nb !< total number of elements on the current processor
          integer, intent(in) :: s_address !< first dimension of address array
          integer, dimension(global_elem_nb), intent(inout) :: elm_shft !< array to store the shifts for each element
          integer, dimension(nixx,global_elem_nb), intent(in) :: ixx !< connectivity array
          integer, dimension(s_address,local_elem_nb), intent(inout) :: address !< address array to fill

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer  :: i
          integer :: shft
          integer :: testval,myone
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          bool = .false.
          myone = 1
          do i=1,node_nb
            shft = ishft(myone,i-1)
            testval =iand(elm_shft(global_elem_id),shft)
            if( ixx(i+ixx_offset,global_elem_id)==global_node_id.and.testval==0) then
              address(i,local_elem_id) = local_address
              elm_shft(global_elem_id) = elm_shft(global_elem_id)+shft
              bool = .true.
              go to 100
            endif
          enddo
100       continue
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_fsky_address
      end module get_fsky_address_mod
