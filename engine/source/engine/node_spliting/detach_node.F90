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
      !||    detach_node_mod   ../engine/source/engine/node_spliting/detach_node.F90
      !||--- called by ------------------------------------------------------
      !||    resol             ../engine/source/engine/resol.F
      !||====================================================================
      module detach_node_mod
#include "my_real.inc"
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
       !\brief This subroutine detaches a node from a list of shells
      !||====================================================================
      !||    detach_node_from_shells   ../engine/source/engine/node_spliting/detach_node.F90
      !||--- calls      -----------------------------------------------------
      !||    extend_nodal_arrays       ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||--- uses       -----------------------------------------------------
      !||    connectivity_mod          ../common_source/modules/connectivity.F90
      !||    constant_mod              ../common_source/modules/constant_mod.F
      !||    nodal_arrays_mod          ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||====================================================================
        subroutine detach_node_from_shells(nodes, node_id ,elements,shell_list,list_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod, only : TWO
          USE connectivity_mod 
          USE nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
            type(connectivity_), intent(inout) :: elements !< connectivity of elements
            integer, intent(in) :: node_id                 !< id of the node to detach  
            integer, intent(in) :: list_size               !< size of the shell list 
            integer, intent(in) :: shell_list(list_size)   !< list of local ids of shells to detach from the node 
! ---------------------------------------------------------------------------------------------------------------------- 
!                                                   Local variables
! ---------------------------------------------------------------------------------------------------------------------- 
            integer :: i,j 
            integer :: new_uid
            integer :: old_uid
            integer :: new_local_id
            integer :: numnod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            new_uid = nodes%max_uid + 1 
            nodes%max_uid = new_uid
            old_uid = nodes%itab(node_id)
            new_local_id = nodes%numnod + 1
            write(6,*) 'Detaching node ', node_id, ' from shells ', shell_list
            write(6,*) 'old_uid = ', old_uid, ' new_uid = ', new_uid
            write(6,*) 'new_local_id = ', new_local_id
            do i = 1, list_size
                do j = 1,4
                    if(elements%shell%nodes(j,shell_list(i)) == node_id) then
                        elements%shell%nodes(j,shell_list(i)) = new_local_id
                        elements%shell%ixc(j+1,shell_list(i)) = new_local_id
                        write(6,*) 'Detached node ', node_id, ' from shell ',elements%shell%user_id(shell_list(i))
                    end if
                enddo
            end do
            numnod = nodes%numnod
            call extend_nodal_arrays(nodes,numnod+1)
            i = node_id
            nodes%itab(numnod+1) = new_uid
            nodes%IKINE(numnod+1) = nodes%IKINE(i)
            nodes%V(1:3,numnod+1) = nodes%V(1:3,i)
            nodes%X(1:3,numnod+1) = nodes%X(1:3,i)
            nodes%D(1:3,numnod+1) = nodes%D(1:3,i)
            nodes%iskew(numnod+1) = nodes%iskew(i)
            nodes%ICODE(numnod+1) = nodes%ICODE(i)
            if(nodes%iroddl >0) then
              nodes%VR(1:3,numnod+1) = nodes%VR(1:3,i)
              nodes%IN(numnod+1) = nodes%IN(i)
              nodes%IN0(numnod+1) = nodes%IN0(i)
              nodes%ICODR(numnod+1) = nodes%ICODR(i)
            endif
            if(nodes%sicodt_fac >0) nodes%ICODT(numnod+1) = nodes%ICODT(i)
            if(nodes%used_dr) then
              nodes%DR(1:3,numnod+1) = nodes%DR(1:3,i)
            endif
            nodes%MS(numnod+1) = nodes%MS(i) / TWO
            nodes%MS(i) = nodes%MS(i) / TWO
            nodes%MS0(numnod+1) = nodes%MS0(i) /TWO
            nodes%MS0(i) = nodes%MS0(i) /TWO
#ifdef MYREAL4
            nodes%DDP(1:3,numnod+1) = nodes%DDP(1:3,i)
            nodes%XDP(1:3,numnod+1) = nodes%XDP(1:3,i)
#endif
            nodes%WEIGHT(numnod+1) = nodes%WEIGHT(i)
            nodes%WEIGHT_MD(numnod+1) = nodes%WEIGHT_MD(i)  
            if(nodes%iparith == 0) then
              nodes%A(1:3,numnod+1) = nodes%A(1:3,i)
              nodes%AR(1:3,numnod+1) = nodes%AR(1:3,i)
              if(nodes%iroddl > 0) nodes%STIFR(numnod+1) = nodes%STIFR(i)
              nodes%VISCN(numnod+1) = nodes%VISCN(i)
              nodes%STIFN(numnod+1) = nodes%STIFN(i)
            else                      
              nodes%A(1:3,numnod+1) = nodes%A(1:3,i)
              nodes%AR(1:3,numnod+1) = nodes%AR(1:3,i)
              nodes%STIFR(numnod+1) = nodes%STIFR(i)
              nodes%VISCN(numnod+1) = nodes%VISCN(i)
              nodes%STIFN(numnod+1) = nodes%STIFN(i)
            endif               

            nodes%ITABM1(numnod+1) = new_uid
            nodes%ITABM1(2*(numnod+1)) = new_local_id

        end subroutine detach_node_from_shells 

      end module detach_node_mod 
