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
!||    create_element_group_mod   ../starter/source/elements/create_element_group.F90
!||--- called by ------------------------------------------------------
!||    get_element_group          ../starter/source/elements/get_element_group.F90
!||====================================================================
      module create_element_group_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Creation of entity groups based on the same keys
!! \details
!||====================================================================
!||    create_element_group   ../starter/source/elements/create_element_group.F90
!||--- called by ------------------------------------------------------
!||    get_element_group      ../starter/source/elements/get_element_group.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine create_element_group(entity_nb,nb_key,elm_group_data,sort_key,iddlevel)

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
          integer, intent(in) :: entity_nb !< number of entity
          integer, intent(in) :: nb_key !< number of keys for sorting
          integer, intent(in) :: iddlevel !< flag for the domain decomposition: 0 --> first domain decomposition without the interfaces
                                          !<                                    1 --> second domain decomposition with the interfaces
                                          !< if there are some interfaces, need to save the data to the appropiate locations (i instead of index(i))
          integer, dimension(entity_nb,2), intent(out) :: elm_group_data !< array to store the group id and the number of element in the group for each element 
          integer, dimension(nb_key,entity_nb), intent(inout) :: sort_key !< array to store the sorting keys for each element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: global_condition,condition
          integer :: i,j,my_index
          integer :: mode
          integer :: group_id
          integer, dimension(70000) :: work
          integer, dimension(:), allocatable :: index,elm_group

          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          mode = 0
          allocate(index(2*entity_nb))
          do i=1, entity_nb
            index(i) = i
          end do
          call my_orders(mode,work,sort_key,index,entity_nb,nb_key)

          allocate(elm_group(entity_nb))
          elm_group(1:entity_nb) = 0
          group_id = 1
          elm_group(group_id) = 1 ! get the number of element in the first group
          
          do i=2,entity_nb
            global_condition = .false.
            do j=1,nb_key
              condition = sort_key(j,index(i))/=sort_key(j,index(i-1))
              global_condition = global_condition.or.condition
            end do
            
            if(global_condition) then ! check if the current element belongs to the same group as the previous one
              group_id = group_id + 1 ! get the id of the current group
            end if
            elm_group(group_id) = elm_group(group_id) + 1 ! get the number of element of the current group
          end do

          group_id = 1
          if(iddlevel==0) then
            my_index = index(1)
          else
            my_index = 1
          endif
          elm_group_data(my_index,1) = group_id ! save the id of the group for the element i
          elm_group_data(my_index,2) = elm_group(group_id) ! save the number of element of the group for the element i
          do i=2,entity_nb

            global_condition = .false.
            do j=1,nb_key
              condition = sort_key(j,index(i))/=sort_key(j,index(i-1))
              global_condition = global_condition.or.condition
            end do
            
            if(global_condition) then ! check if the current element belongs to the same group as the previous one
              group_id = group_id + 1 ! get the id of the current group
            end if
            if(iddlevel==0) then
              my_index = index(i)
            else
              my_index = i
            endif            
            elm_group_data(my_index,1) = group_id ! save the id of the group for the element i
            elm_group_data(my_index,2) = elm_group(group_id) ! save the number of element of the group for the element i
          end do

          deallocate(index)
          deallocate(elm_group)

          return

        end subroutine create_element_group
      end module create_element_group_mod
