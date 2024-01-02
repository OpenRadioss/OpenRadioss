Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module consider_edge_mod
!=======================================================================================================================
!                                                  Module parameters
!=======================================================================================================================
        integer, parameter :: max_nb_nodes_per_elt = 10

 
        contains 
!=======================================================================================================================
!                                                  Subroutines
!=======================================================================================================================
!! \brief returns true if the edge between two elements should be created
        function consider_edge(connectivity,nb_nodes_mini,nelem,e1,e2) result(bool)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nelem !< total number of elements
          integer, intent(in) :: connectivity(max_nb_nodes_per_elt,nelem) !< list of nodes per element (may be zero)
          integer, intent(in) :: nb_nodes_mini(nelem) !< minimum number of shared node per element to consider the edge
          integer, intent(in) :: e1 !< index of the first element
          integer, intent(in) :: e2 !< index of the second element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: bool
          integer :: count
          integer :: i,j
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          bool = .false.
          count = 0

          i = 1
          j = 1
          do while (i <= max_nb_nodes_per_elt .and. j <= max_nb_nodes_per_elt)
             if(connectivity(i,e1) == 0 .or. connectivity(j,e2) == 0) then
                exit
             elseif (connectivity(i,e1) == connectivity(j,e2)) then
                count = count + 1
                i = i + 1
                j = j + 1
             else if (connectivity(i,e1) > connectivity(j,e2)) then
                i = i + 1
             else
                j = j + 1
             end if
          end do

            if (count >= nb_nodes_mini(e1) .or. count >= nb_nodes_mini(e2)) then
               bool = .true.
            end if
        end function

!! \brief sort array of nodes per element in descending order
        subroutine sort_descending(array)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(max_nb_nodes_per_elt), intent(inout) :: array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, temp
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          do i = 1, max_nb_nodes_per_elt-1
             do j = i+1, max_nb_nodes_per_elt
                if (array(i) < array(j)) then
                   temp = array(i)
                   array(i) = array(j)
                   array(j) = temp
                end if
             end do
          end do

        end subroutine sort_descending
        


      end module
