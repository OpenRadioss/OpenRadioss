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
        implicit none
      contains
!||====================================================================
!||    find_segment_in_list          ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod              ../common_source/modules/connectivity.F90
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||====================================================================
        function find_segment_in_list(segment,list,size,elements) result(is_found)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE connectivity_mod
          USE constant_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: segment(4) !< segment to search for
          integer, intent(in) :: size        !< size of the list
          integer, intent(in) :: list(size)  !< list of segments
          type(connectivity_), intent(in) :: elements !< connectivity of elements
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          logical :: is_found
          integer :: count
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_found = .false.
          do i = 1, size
            count = 0
            do j = 1, 4
!                if(segment(j) == elements%shell%nodes(j,list(i))) then
!               if the jth node is found at any position in the segment
              if(any(segment == elements%shell%nodes(j,list(i)))) then
                count = count + 1
              end if
            end do
            if(count == 4) then
              is_found = .true.
              return
            end if
          end do

        end function find_segment_in_list

!||====================================================================
!||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    detach_node                   ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||    find_segment_in_list          ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod              ../common_source/modules/connectivity.F90
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    extend_array_mod              ../common_source/tools/memory/extend_array.F90
!||    interfaces_mod                ../common_source/modules/interfaces/interfaces_mod.F90
!||    nodal_arrays_mod              ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine detach_node_from_interfaces(nodes, node_id,npari,ninter, ipari, interf, elements, shell_list, list_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod
          USE connectivity_mod
          USE nodal_arrays_mod
          USE interfaces_mod
          use extend_array_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
          integer, intent(in) :: node_id                 !< id of the node to detach
          type(interfaces_), intent(inout) :: interf !< interf structure
          integer, intent(in) :: npari                   !< number of parameters
          integer, intent(in) :: ninter                  !< number of interf
          integer, intent(inout) :: ipari(npari,ninter)    !< parameters of the interf
          integer, intent(in) :: list_size               !< size of the shell list
          integer, intent(in) :: shell_list(list_size)   !< list of local ids of shells to detach from the node
          type(connectivity_), intent(in) :: elements !< connectivity of elements
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          integer :: itype
          integer :: nsn
          integer :: nmn
          integer :: nrtm
          logical :: is_found
          integer :: igap
          integer :: intth
          logical :: still_connected
          integer :: nrts
          integer :: old_secondary_node
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          still_connected = .FALSE.
          old_secondary_node = 0
          do i = 1, ninter
            itype = IPARI(INDEX_ITYPE,i)
            if(itype == 7) then
              nsn = IPARI(INDEX_NSN,i)
              nmn = IPARI(INDEX_NMN,i)
              nrtm = IPARI(INDEX_NRTM,i)
              intth  = ipari(INDEX_INTTH,i)
              IGAP   = IPARI(INDEX_IGAP,i)
              nrts   = IPARI(INDEX_NRTS,i)
              ! search if node_id is in interf%intbuf_tab(i)%MSR(1:NMN)
              is_found = any(interf%intbuf_tab(i)%MSR(1:nmn) == node_id)
              if(is_found) then
                ! extend the arrays MSR
                call extend_array(interf%intbuf_tab(i)%MSR,nmn,nmn+1)
                interf%intbuf_tab(i)%MSR(nmn+1) = nodes%numnod + 1
                IPARI(INDEX_NMN,i) = nmn + 1
                ! large value in xsav should trigger the contact search
                interf%intbuf_tab(i)%S_MSR = interf%intbuf_tab(i)%S_MSR + 1
                nmn = nmn + 1
              endif

              is_found = any(interf%intbuf_tab(i)%NSV(1:nsn) == node_id)
              if(is_found) then
                ! old_secondary_node is j such as interf%intbuf_tab(i)%NSV(j) == node_id
                do j = 1, nsn
                  if(interf%intbuf_tab(i)%NSV(j) == node_id) then
                    old_secondary_node = j
                    exit
                  endif
                enddo
                call extend_array(interf%intbuf_tab(i)%NSV,nsn,nsn+1)
                interf%intbuf_tab(i)%NSV(nsn+1) = nodes%numnod + 1
                interf%intbuf_tab(i)%S_NSV = nsn + 1
                if(intth > 0) then
                  call extend_array(interf%intbuf_tab(i)%ielec,nsn,nsn+1)
                  interf%intbuf_tab(i)%s_ielec = nsn + 1
                endif
                interf%intbuf_tab(i)%s_stfns   = nsn + 1
                call extend_array(interf%intbuf_tab(i)%stfns,nsn,nsn+1)
                interf%intbuf_tab(i)%stfns(nsn+1) = - interf%intbuf_tab(i)%stfns(old_secondary_node)
                IF(IGAP > 0) THEN
                  interf%intbuf_tab(i)%s_gap_s   = nsn + 1
                  call extend_array(interf%intbuf_tab(i)%gap_s,nsn,nsn+1)
                  interf%intbuf_tab(i)%gap_s(nsn+1) = interf%intbuf_tab(i)%gap_s(old_secondary_node)
                ENDIF
                IF (INTTH > 0 ) THEN
                  interf%intbuf_tab(i)%s_areas   = nsn + 1
                  call extend_array(interf%intbuf_tab(i)%areas,nsn,nsn+1)
                  interf%intbuf_tab(i)%areas(nsn+1) = interf%intbuf_tab(i)%areas(old_secondary_node)
                ENDIF
                IF (IGAP == 3 ) THEN
                  interf%intbuf_tab(i)%s_gap_sl  = nsn + 1
                  call extend_array(interf%intbuf_tab(i)%gap_sl,nsn,nsn+1)
                  interf%intbuf_tab(i)%gap_sl(nsn+1) = interf%intbuf_tab(i)%gap_sl(old_secondary_node)
                ENDIF
!                 if(ipari(index_intfric) > 0) then
!                      not supported ye,it
!                 endif
                nsn = nsn + 1
              endif

              if(interf%intbuf_tab(i)%s_xsav  < 3*min(nodes%numnod,nsn+nmn)) then
                call extend_array(interf%intbuf_tab(i)%xsav,interf%intbuf_tab(i)%s_xsav,3*min(nodes%numnod,nsn+nmn))
                interf%intbuf_tab(i)%s_xsav = 3*min(nodes%numnod,nsn+nmn)
                interf%intbuf_tab(i)%xsav = - HUGE(interf%intbuf_tab(i)%xsav(1))
              endif

              ! if the main segment has the same nodes as a detached shell, then the segment is detached
              do j = 1, nrtm
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 1) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 1) = nodes%numnod + 1
                endif
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 2) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 2) = nodes%numnod + 1
                endif
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 3) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 3) = nodes%numnod + 1
                endif
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 4) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 4) = nodes%numnod + 1
                endif
              enddo
              do j = 1, nrts
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 1) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 1) = nodes%numnod + 1
                endif
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 2) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 2) = nodes%numnod + 1
                endif
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 3) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 3) = nodes%numnod + 1
                endif
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 4) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 4) = nodes%numnod + 1
                endif
              enddo
            endif
          enddo

        end subroutine detach_node_from_interfaces
        !\brief This subroutine sets the values of the new node using the values of the old node
!||====================================================================
!||    set_new_node_values   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    detach_node           ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod      ../common_source/modules/connectivity.F90
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    nodal_arrays_mod      ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine set_new_node_values(nodes,i)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod, only : TWO
          USE connectivity_mod
          USE nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
          integer, intent(in) :: i                 !< id of the node to detach
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: numnod
          integer :: p
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          numnod = nodes%numnod
          nodes%itab(numnod+1) = nodes%max_uid ! -nodes%itab(i) !temporary id of the new node 
          nodes%IKINE(numnod+1) = nodes%IKINE(i)
          nodes%V(1:3,numnod+1) = nodes%V(1:3,i)
          nodes%X(1:3,numnod+1) = nodes%X(1:3,i)
          nodes%D(1:3,numnod+1) = nodes%D(1:3,i)
          nodes%iskew(numnod+1) = nodes%iskew(i)
          nodes%ICODE(numnod+1) = nodes%ICODE(i)
          nodes%TAG_S_RBY(numnod+1) = nodes%TAG_S_RBY(i)
          if(nodes%itherm_fe> 0) then
            nodes%MCP(numnod+1) = nodes%MCP(i)
            nodes%TEMP(numnod+1) = nodes%TEMP(i)
          endif


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
          nodes%MS(numnod+1) = nodes%MS(i)  / TWO
!         nodes%MS(i) = nodes%MS(i)  / TWO
          nodes%MS0(numnod+1) = nodes%MS0(i)   /TWO
!         nodes%MS0(i) = nodes%MS0(i)  /TWO

#ifdef MYREAL4
          nodes%DDP(1:3,numnod+1) = nodes%DDP(1:3,i)
          nodes%XDP(1:3,numnod+1) = nodes%XDP(1:3,i)
          if(nodes%iparith==0) then
            nodes%ACC_DP(1:3,numnod+1) = nodes%ACC_DP(1:3,i)
          endif

#endif
          nodes%WEIGHT(numnod+1) = 1 ! nodes%WEIGHT(i)
          nodes%WEIGHT_MD(numnod+1) = nodes%WEIGHT_MD(i)
          nodes%MAIN_PROC(numnod+1) = nodes%MAIN_PROC(i)

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
          p = i
          nodes%parent_node(numnod+1) = i
          do while(nodes%parent_node(p) /= p)
            p = nodes%parent_node(p)
          enddo
          nodes%parent_node(numnod+1) = p
!         nodes%nchilds(p) = nodes%nchilds(p) + 1

          nodes%ITABM1(numnod+1) = nodes%max_uid !-nodes%itab(numnod+1)
          nodes%ITABM1(2*(numnod+1)) = numnod + 1


        end subroutine set_new_node_values

! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
        !\brief This subroutine detaches a node from a list of shells
!||====================================================================
!||    detach_node_from_shells   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    detach_node               ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||    update_pon_shells         ../engine/source/engine/node_spliting/update_pon.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod          ../common_source/modules/connectivity.F90
!||    constant_mod              ../common_source/modules/constant_mod.F
!||    nodal_arrays_mod          ../common_source/modules/nodal_arrays.F90
!||    update_pon_mod            ../engine/source/engine/node_spliting/update_pon.F90
!||====================================================================
        subroutine detach_node_from_shells(nodes, node_id ,elements,shell_list,list_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod, only : TWO
          USE connectivity_mod
          USE nodal_arrays_mod
          use update_pon_mod
          implicit none
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
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          new_uid = nodes%max_uid
          old_uid = nodes%itab(node_id)
          new_local_id = nodes%numnod +1
          do i = 1, list_size
            do j = 1,4
              if(elements%shell%nodes(j,shell_list(i)) == node_id) then
                elements%shell%nodes(j,shell_list(i)) = new_local_id
                elements%shell%ixc(j+1,shell_list(i)) = new_local_id
              end if
            enddo
          end do

          if(nodes%iparith > 0) then! /PARITH/ON
            call update_pon_shells(elements,list_size,shell_list,new_local_id)
          endif


        end subroutine detach_node_from_shells
        !\brief This subroutine detaches a node from a list of shells
!||====================================================================
!||    detach_node                   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    test_jc_shell_detach          ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_from_shells       ../engine/source/engine/node_spliting/detach_node.F90
!||    extend_nodal_arrays           ../common_source/modules/nodal_arrays.F90
!||    set_new_node_values           ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod              ../common_source/modules/connectivity.F90
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    interfaces_mod                ../common_source/modules/interfaces/interfaces_mod.F90
!||    nodal_arrays_mod              ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine detach_node(nodes, node_id ,elements,shell_list,list_size,npari,ninter, ipari, interf)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod, only : TWO
          USE connectivity_mod
          USE nodal_arrays_mod
          USE interfaces_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
          type(connectivity_), intent(inout) :: elements !< connectivity of elements
          integer, intent(in) :: node_id                 !< id of the node to detach
          integer, intent(in) :: list_size               !< size of the shell list
          integer, intent(in) :: shell_list(list_size)   !< list of local ids of shells to detach from the node
          type(interfaces_), intent(inout) :: interf !< interf structure
          integer, intent(in) :: npari                   !< number of parameters
          integer, intent(in) :: ninter                  !< number of interf
          integer, intent(inout) :: ipari(npari,ninter)    !< parameters of the interf
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: new_uid
          integer :: old_uid
          integer :: new_local_id
          integer :: numnod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          !write(6,*) "detach_node",node_id,nodes%itab(node_id),"from:",shell_list(1:list_size)
          !call flush(6)
!         new_uid = nodes%max_uid + 1
!         nodes%max_uid = new_uid
          old_uid = nodes%itab(node_id)
          numnod = nodes%numnod
          new_local_id = nodes%numnod + 1
          call detach_node_from_interfaces(nodes, node_id,npari,ninter, ipari, interf, elements, shell_list,list_size)

          call extend_nodal_arrays(nodes,numnod+1) !increments nodes%numnod

          i = node_id
          call set_new_node_values(nodes,i)


          call detach_node_from_shells(nodes, node_id ,elements,shell_list,list_size)

          nodes%numnod = nodes%numnod + 1

        end subroutine detach_node

!       !\brief This subroutine detaches a node from a list of shells
        ! it is just a proof of concept to demonstrate how to detach a node from a list of shells
        ! the crack propagation is non phsyical (based on Jonhson-Cook damage)
!||====================================================================
!||    test_jc_shell_detach         ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||    detach_node                  ../engine/source/engine/node_spliting/detach_node.F90
!||    spmd_exchange_ghost_shells   ../engine/source/engine/node_spliting/ghost_shells.F90
!||    stlsort_int_int              ../common_source/tools/sort/cppsort.cpp
!||--- uses       -----------------------------------------------------
!||    connectivity_mod             ../common_source/modules/connectivity.F90
!||    constant_mod                 ../common_source/modules/constant_mod.F
!||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    extend_array_mod             ../common_source/tools/memory/extend_array.F90
!||    ghost_shells_mod             ../engine/source/engine/node_spliting/ghost_shells.F90
!||    interfaces_mod               ../common_source/modules/interfaces/interfaces_mod.F90
!||    nodal_arrays_mod             ../common_source/modules/nodal_arrays.F90
!||    precision_mod                ../common_source/modules/precision_mod.F90
!||    spmd_mod                     ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine test_jc_shell_detach(nodes, element, interf, npari, ninter, ipari, numnod, &
          numnodg, elbuf, ngroup, ngrouc, nparg, iparg, igrouc, numelc, ispmd, nspmd, &
          new_crack)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use spmd_mod
          use ghost_shells_mod
          use precision_mod, only : wp
          USE constant_mod, only : TWO
          USE connectivity_mod
          USE nodal_arrays_mod
          USE interfaces_mod
          USE elbufdef_mod
          use extend_array_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
          type(connectivity_), intent(inout) :: element !< connectivity of elements
          type(interfaces_), intent(inout) :: interf !< interf structure
          integer, intent(in) :: ngroup !< number of groups
          integer, intent(in) :: ngrouc !< number of shell groups
          integer, intent(in) :: nparg !< number of parameters per group
          integer, intent(in) :: iparg(nparg, ngroup) !< parameters of the groups
          integer, intent(in) :: numelc !< number of shell elements
          integer, intent(in) :: igrouc(ngrouc) !< group ids
          type(elbuf_struct_), intent(in) :: elbuf(ngroup)
          integer, intent(in) :: npari                   !< number of parameters
          integer, intent(in) :: ninter                  !< number of interf
          integer, intent(inout) :: ipari(npari,ninter)    !< parameters of the interf
          integer, intent(inout) :: numnod, numnodg
          integer, intent(in) :: ispmd !< rank of the processor (MPI)
          integer, intent(in) :: nspmd !< number of processors (MPI)
          integer, intent(out) :: new_crack !< flag to indicate if a new crack is created
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=wp), dimension(:), allocatable :: detach_shell
          integer :: ig,ng,numnod0,i,j,k,l,n,n1,n2,n3,n4,nel,nft,p
          integer, dimension(20) :: crack !< id of the noodes that are part of the crack
          integer :: ncrack
          integer, dimension(:), allocatable :: shell_list
          integer :: shells_to_detach
          double precision :: normal(3),  vec(3), distance
          double precision, dimension(:), allocatable :: nodal_damage
          double precision :: v(3)
          double precision, parameter :: treshold = 1.04D0
          double precision :: dmax
          integer :: nGhostShells
          real(kind=wp), dimension(:), allocatable :: ghostShellDamage
          integer, dimension(:), allocatable :: detached_nodes, detached_nodes_local
          integer, dimension(:), allocatable :: nb_detached_nodes_global
          integer, dimension(:), allocatable :: nb_detached_nodes
          integer :: nb_detached_nodes_local
          logical, dimension(:), allocatable :: is_unique 
          integer :: total_new_nodes
          integer :: displ(nspmd)
          integer :: old_max_uid
          integer :: numnodg0
          integer, dimension(:), allocatable :: permutation, processor, local_pos
          integer :: ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
          new_crack = 0        
          
          numnodg0 = numnodg
          allocate(detach_shell(0:numelc))
          detach_shell = 0.0d0
          if(.not. allocated(element%shell%damage)) then
            allocate(element%shell%damage(1:numelc))
            element%shell%damage = 0.0d0
            allocate(element%shell%dist_to_center(1:numelc))
            do i = 1, numelc
              n1 = element%shell%ixc(2,i)
              n2 = element%shell%ixc(3,i)
              n3 = element%shell%ixc(4,i)
              n4 = element%shell%ixc(5,i)
              !barycenter
              v(1) = (nodes%x(1,n1) + nodes%x(1,n2) + nodes%x(1,n3) + nodes%x(1,n4))/4.0d0
              v(2) = (nodes%x(2,n1) + nodes%x(2,n2) + nodes%x(2,n3) + nodes%x(2,n4))/4.0d0
              v(3) = (nodes%x(3,n1) + nodes%x(3,n2) + nodes%x(3,n3) + nodes%x(3,n4))/4.0d0
              distance = 0.0d0
              do j = 1,4
                distance = max(distance, sqrt((v(1) - nodes%x(1,element%shell%ixc(j+1,i)))**2 + &
                  (v(2) - nodes%x(2,element%shell%ixc(j+1,i)))**2 + &
                  (v(3) - nodes%x(3,element%shell%ixc(j+1,i)))**2))
              enddo
              element%shell%dist_to_center(i) = distance
            enddo
          endif


          !! gather the damage of the shells in the detach_shell array
          do ig = 1, ngrouc
            ng = igrouc(ig)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            ! gather jc dfmax values
            !detach_shell(nft+1: nft+nel) =  elbuf(ng)%bufly(1)%fail(1,1,1)%floc(1)%dammx(1:nel)
            do k = 1, size(elbuf(ng)%bufly,1)
              do n1 = 1,size(elbuf(ng)%bufly(k)%fail,1)
                do n2 = 1,size(elbuf(ng)%bufly(k)%fail,2)
                  do n3 = 1,size(elbuf(ng)%bufly(k)%fail,3)
                    do l = 1,size(elbuf(ng)%bufly(k)%fail(n1,n2,n3)%floc,1)
                      do n = 1, size(elbuf(ng)%bufly(k)%fail(n1,n2,n3)%floc(l)%dammx,1)
                        detach_shell(nft+n) = max(detach_shell(nft+n), elbuf(ng)%bufly(k)%fail(n1,n2,n3)%floc(l)%dammx(n))
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
            ! detach_shell(nft+1: nft+nel) = detach_shell(nft+1: nft+nel) - element%shell%damage(nft+1: nft+nel)
          enddo


          ! Exchange the detach_shell values on ghost shells
          nghostshells = size(element%ghost_shell%nodes,2)
          allocate(ghostshelldamage(nghostshells))
          ghostshelldamage = 0.0d0
          call spmd_exchange_ghost_shells(element,ispmd,nspmd,1,detach_shell,ghostshelldamage)

          numnod0 = numnod
          allocate(nodal_damage(numnod))

          ! cumulate the damage of the shells on the nodes
          nodal_damage = 0.0d0
          do i = 1, numelc
            n1 = element%shell%ixc(2,i)
            n2 = element%shell%ixc(3,i)
            n3 = element%shell%ixc(4,i)
            n4 = element%shell%ixc(5,i)
            element%shell%damage(i) = detach_shell(i)
            if(detach_shell(i) > 0.9999d0) cycle ! already broken
            nodal_damage(n1) =max(nodal_damage(n1),element%shell%damage(i))
            nodal_damage(n2) =max(nodal_damage(n2),element%shell%damage(i))
            nodal_damage(n3) =max(nodal_damage(n3),element%shell%damage(i))
            nodal_damage(n4) =max(nodal_damage(n4),element%shell%damage(i))
          enddo

          ! add the damage of the ghost shells
          do i = 1, nghostshells
            element%ghost_shell%damage(i) = ghostshelldamage(i)
            if(ghostshelldamage(i) > 0.9999d0) cycle ! already broken
            do j = 1, 4
              n1 = element%ghost_shell%nodes(j,i)
              if(n1 <= 0) cycle
              nodal_damage(n1) =max(nodal_damage(n1),element%ghost_shell%damage(i))
            enddo
          enddo

          deallocate(ghostshelldamage)

          allocate(detached_nodes_local(numnod))
          nb_detached_nodes_local = 0

          allocate(shell_list(numelc))
          shell_list = 0
          shells_to_detach = 0

          ! detach nodes based on simple 
          dmax = 0.0
          do ii = 1, numelc
            i =  element%shell%permutation(ii) ! the shells are treated in the order of their user_id, for reproducibility
            n1 = element%shell%ixc(2,i)
            n2 = element%shell%ixc(3,i)
            n3 = element%shell%ixc(4,i)
            n4 = element%shell%ixc(5,i)
            v(1) = (nodes%X(1,n1) + nodes%X(1,n2) + nodes%X(1,n3) + nodes%X(1,n4))/4.0D0
            v(2) = (nodes%X(2,n1) + nodes%X(2,n2) + nodes%X(2,n3) + nodes%X(2,n4))/4.0D0
            v(3) = (nodes%X(3,n1) + nodes%X(3,n2) + nodes%X(3,n3) + nodes%X(3,n4))/4.0D0
            distance = 0.0D0
            if(detach_shell(i) >= 0.99999d0) cycle ! deleted shell
            do j = 1,4
              distance = sqrt((v(1) - nodes%X(1,element%shell%ixc(j+1,i)))**2 + &
                (v(2) - nodes%X(2,element%shell%ixc(j+1,i)))**2 + &
                (v(3) - nodes%X(3,element%shell%ixc(j+1,i)))**2)
              if(nodes%nchilds(nodes%parent_node(element%shell%ixc(j+1,i))) < 1) then
                dmax = max(dmax,distance / element%shell%dist_to_center(i))
              endif
              if(distance > treshold * element%shell%dist_to_center(i)) then
                if(nodes%nchilds(nodes%parent_node(element%shell%ixc(j+1,i))) > 3) cycle ! this node has not been splitted more than 3 times
                if(nodal_damage(element%shell%ixc(j+1,i)) < .001 ) cycle
                crack(1) = element%shell%ixc(j+1,i)
                shell_list(1) = i
                shells_to_detach = 1
                element%shell%damage(i) = 1.0D0 ! 
                nb_detached_nodes_local = nb_detached_nodes_local + 1
                detached_nodes_local(nb_detached_nodes_local) = nodes%itab(crack(1))
                call detach_node(nodes,crack(1),element,shell_list,shells_to_detach,npari,ninter, ipari, interf)
                numnod = numnod + 1
                if(ispmd == 0) numnodg = numnodg + 1
              endif
            enddo
          enddo

          ! list nodes that are detached from the shells at this timestep
          allocate(nb_detached_nodes(nspmd))
          allocate(nb_detached_nodes_global(nspmd))
          nb_detached_nodes_global = 0
          nb_detached_nodes(1:nspmd) = 0
          nb_detached_nodes(ispmd+1) = numnod - numnod0
          ! call mpi_allreduce
          if(nspmd > 1) then
            call spmd_allreduce(nb_detached_nodes,nb_detached_nodes_global,nspmd,SPMD_SUM)
          else
            nb_detached_nodes_global = nb_detached_nodes
          endif

          total_new_nodes = sum(nb_detached_nodes_global(1:nspmd))
          if(total_new_nodes > 0) new_crack = total_new_nodes
          !allocate(detached_nodes_local(nb_detached_nodes_global(ispmd+1)))
          allocate(detached_nodes(total_new_nodes))
          if(nb_detached_nodes_global(ispmd+1) /= nb_detached_nodes_local) then
          endif

          ! reuse displ as displ
          displ(1:nspmd) = 0
          do i = 2, nspmd
            displ(i) = displ(i-1) + nb_detached_nodes_global(i-1)                             
          enddo

          if(nspmd > 1) then 
          call spmd_allgatherv(detached_nodes_local,nb_detached_nodes_global(ispmd+1), &
            detached_nodes,nb_detached_nodes_global,displ)
          else
            detached_nodes = detached_nodes_local
          endif

          !allreduce numnodg0
          if(nspmd > 1) then 
          call spmd_allreduce(numnodg0,p,1,SPMD_MAX)
            numnodg0 = p
          endif
!          old_max_uid = nodes%max_uid
          if(nspmd > 1) then
            call spmd_allreduce(nodes%max_uid,old_max_uid,1,SPMD_MAX)
          else
            old_max_uid = nodes%max_uid
          endif
        
          !write(6,*) "numnodg0",numnodg0
!         if(total_new_nodes >0) write(6,*) "MASS nb_detached_nodes_global",nb_detached_nodes_global(1:nspmd)
!         if(total_new_nodes >0) write(6,*) "MASS detached_nodes",detached_nodes(1:total_new_nodes)
          !Not finalized: new nodes may be boundary nodes (i.e. new node attached to two shells from different processors)

          k = sum(nb_detached_nodes_global(1:nspmd)) 
          allocate(processor(k))
          allocate(local_pos(k))
          k = 0
          do P = 1, nspmd
            do i = 1, nb_detached_nodes_global(P)
              k = k + 1
              processor(k) = P
              if(ispmd+1 == P) then
                 local_pos(k) = i                                                           
              else
                 local_pos(k) = 0
              endif
            enddo 
          enddo
 
          k = sum(nb_detached_nodes_global(1:nspmd)) 
          allocate(permutation(k))
          do i = 1,k 
            permutation(i) = i
          enddo
          ! sort the detached nodes in ascending order of the user id of the parent node
          CALL STLSORT_INT_INT(k,detached_nodes,permutation)

          !Loop over the detached nodes of all domains
          !the new nodes are created in the same order what the number of mpi domains is
          do ii = 1, k
            i = permutation(ii)
            P = processor(i)
            old_max_uid = old_max_uid + 1
            numnodg0 = numnodg0 + 1
            if( P == ispmd+1) then
              j = local_pos(i)
              nodes%itab(numnod0 + j) = old_max_uid
              nodes%itabm1(numnod0 + j) = old_max_uid                                
              nodes%itabm1(2*(numnod0 + j)) = numnod0 + j
              nodes%nodglob(numnod0 + j) = numnodg0
              !write(6,*) old_max_uid,"detached node ",nodes%itab(numnod0 + j),"form",nodes%itab(nodes%parent_node(numnod0+j))
            endif
            j = get_local_node_id(nodes,detached_nodes(i))
            if(j > 0) then
            ! the node is known by the current processor, so we need to update its mass
             nodes%MS(j) = nodes%MS(j) / TWO ! arbitrary division by 2
             nodes%MS0(j) = nodes%MS0(j) /TWO
             nodes%nchilds(nodes%parent_node(j)) = nodes%nchilds(nodes%parent_node(j)) + 1 
            endif
          enddo

!         if(old_max_uid /= nodes%max_uid) then
!           write(6,*) "old_max_uid",old_max_uid,"nodes%max_uid",nodes%max_uid
!         endif
          nodes%max_uid = old_max_uid
          deallocate(permutation)
          deallocate(processor)
          deallocate(local_pos)

          numnodg = numnodg0

          if (allocated(is_unique)) deallocate(is_unique)
          if (allocated(nb_detached_nodes)) deallocate(nb_detached_nodes)
          if (allocated(detached_nodes_local)) deallocate(detached_nodes_local)
          if (allocated(nb_detached_nodes_global)) deallocate(nb_detached_nodes_global)
          if (allocated(detach_shell)) deallocate(detach_shell)
          if (allocated(shell_list)) deallocate(shell_list)
          if (allocated(nodal_damage)) deallocate(nodal_damage)
        ! if (allocated(ghostShellCoordinates)) deallocate(ghostShellCoordinates)
        ! if (allocated(shellCoordinates)) deallocate(shellCoordinates)

        end subroutine test_jc_shell_detach



      end module detach_node_mod
