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
      !||    update_pon_mod            ../engine/source/engine/node_spliting/update_pon.F90
      !||--- called by ------------------------------------------------------
      !||    detach_node_from_shells   ../engine/source/engine/node_spliting/detach_node.F90
      !||    resol                     ../engine/source/engine/resol.F
      !||====================================================================
        module update_pon_mod
        contains
        !\brief replace old_ids with new_ids in the shell data structure
      !||====================================================================
      !||    update_pon_shells         ../engine/source/engine/node_spliting/update_pon.F90
      !||--- called by ------------------------------------------------------
      !||    detach_node_from_shells   ../engine/source/engine/node_spliting/detach_node.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    connectivity_mod          ../common_source/modules/connectivity.F90
      !||    my_alloc_mod              ../common_source/tools/memory/my_alloc.F90
      !||    parith_on_mod             ../common_source/modules/parith_on_mod.F90
      !||====================================================================
        subroutine update_pon_shells(elements, n, shell_list, new_numnod, old_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use parith_on_mod
          use connectivity_mod
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(connectivity_), intent(inout) ::  elements
          integer, intent(in) :: n                !< size of shell_list
          integer, dimension(n), intent(in) :: shell_list
          integer, intent(in) :: new_numnod
          integer, intent(in) :: old_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j
          integer :: shell_id
          integer :: contributions_count
          integer :: offset
          integer, dimension(:), allocatable :: new_adsky
          integer :: new_id
          integer :: numelc !< number of shell elements
          integer :: numnod

          integer :: imin_fsky, imax_fsky
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!          not tested when multiple nodes are detached at the same cycle, may not work
            numnod = elements%pon%sadsky - 1
          numelc = size(elements%shell%nodes, 2)

            !=================== debug ===================
            imin_fsky = elements%pon%iadc(1,1)                     
            imax_fsky = elements%pon%iadc(1,1)
            do i = 1, numelc
              do j = 1, 4
                if(elements%pon%iadc(j,i) < imin_fsky) then
                    imin_fsky = elements%pon%iadc(j,i)
                endif
                if(elements%pon%iadc(j,i) > imax_fsky) then
                    imax_fsky = elements%pon%iadc(j,i)
                endif
              enddo
            enddo
!           write(6,*) 'FSKY(', imin_fsky,':', imax_fsky,')'
            !=================== End debug ===================
            contributions_count = 0
            new_id = new_numnod ! the new id is always the last one
            do i = 1, n
              shell_id = shell_list(i)
              do j = 1, 4
                if(elements%shell%nodes(j, shell_id) == new_id) then
                    contributions_count = contributions_count + 1
                endif
              enddo
            enddo
            call my_alloc(new_adsky, new_numnod + 1)
            do i = 1, old_id
              new_adsky(i) = elements%pon%adsky(i)
            enddo
            do i = old_id + 1, new_numnod
              new_adsky(i) = elements%pon%adsky(i) - contributions_count
            enddo
            new_adsky(new_numnod + 1) = new_adsky(new_numnod) + contributions_count
            offset = contributions_count
!           write(6,*) 'offset', offset
            
            call move_alloc(new_adsky,elements%pon%adsky)
            elements%pon%sadsky = new_numnod + 1
            contributions_count = 0

            ! iadc corresponding to new nodes, will point to the end of FSKY
            do i = 1, n
              shell_id = shell_list(i)
              do j = 1, 4
                if(elements%shell%nodes(j, shell_id) == new_id) then
                    elements%pon%iadc(j,shell_id) = elements%pon%adsky(new_numnod) + contributions_count                               
!                   if(elements%pon%iadc(j,shell_id) < imin_fsky .or. elements%pon%iadc(j,shell_id) > imax_fsky) then
!                      write(6,*) __LINE__, 'ERROR: IADC(', shell_id, ',', j, ') = ', elements%pon%adsky(new_numnod) + contributions_count
!                   endif
!                   write(6,*) 'IADC(', shell_id, ',', j, ') = ', elements%pon%iadc(j,shell_id)
                    contributions_count = contributions_count + 1
                endif
              enddo
            enddo

            contributions_count = 0
            do i =1, numelc
              do j = 1,4
                if(elements%shell%nodes(j, i) == new_id) then
                   

                elseif(elements%shell%nodes(j,i) > old_id) then
                    ! the current node id, is greater than the old id, so we need to offset IADC 
                    ! by the number of contributions removed from the FSKY
                    elements%pon%iadc(j,i) = elements%pon%iadc(j,i) - offset
!                   if(elements%pon%iadc(j,i) < imin_fsky .or. elements%pon%iadc(j,i) > imax_fsky) then
!                       write(6,*) __LINE__, 'ERROR: IADC(', i, ',', j, ') = ', elements%pon%iadc(j,i)
!                   endif
                elseif(elements%shell%nodes(j,i) == old_id) then
                      ! if the node is the old id we rebuild the IADC 
                      ! because the contrubtions kept for that node, and the ones that are assigned to the new node
                      ! can be interleaved
                      elements%pon%iadc(j,i) = elements%pon%adsky(old_id) + contributions_count
!                     if(elements%pon%iadc(j,i) < imin_fsky .or. elements%pon%iadc(j,i) > imax_fsky) then
!                       write(6,*) __LINE__, 'ERROR: IADC(', i, ',', j, ') = ', elements%pon%iadc(j,i)
!                     endif
                      contributions_count = contributions_count + 1
                endif
              enddo
            enddo


!           do i = 1, numelc
!             do j = 1, 4
!               if(elements%pon%iadc(j,i) < imin_fsky .or. elements%pon%iadc(j,i) > imax_fsky) then
!               write(6,*) 'ERROR: IADC(', i, ',', j, ') = ', elements%pon%iadc(j,i)
!               call arret(5)
!               endif
!             enddo
!           enddo


        end subroutine update_pon_shells
        end module update_pon_mod   