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
        subroutine set_new_node_values(nodes,i)
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
          integer, intent(in) :: i                 !< id of the node to detach
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: numnod
          integer :: p
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!           write(6,*) "set_new_node_values",nodes%numnod
          numnod = nodes%numnod
          nodes%itab(numnod+1) = nodes%max_uid
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
          nodes%MS(numnod+1) = nodes%MS(i) / TWO
          nodes%MS(i) = nodes%MS(i) / TWO
          nodes%MS0(numnod+1) = nodes%MS0(i) /TWO
          nodes%MS0(i) = nodes%MS0(i) /TWO
#ifdef MYREAL4
          nodes%DDP(1:3,numnod+1) = nodes%DDP(1:3,i)
          nodes%XDP(1:3,numnod+1) = nodes%XDP(1:3,i)
          if(nodes%iparith==0) then
            nodes%ACC_DP(1:3,numnod+1) = nodes%ACC_DP(1:3,i)
          endif

#endif
          nodes%WEIGHT(numnod+1) = nodes%WEIGHT(i)
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
          nodes%nchilds(p) = nodes%nchilds(p) + 1
!         write(6,*) "node",p,"has",nodes%nchilds(p),"childs"
!           if(nodes%itab(numnod+1) == 922550) write(6,*) 'stifn(',nodes%itab(i),') = ', nodes%stifn(i)
!           if(nodes%itab(i) == 907888) write(6,*) 'stifn(',nodes%itab(i),') = ', nodes%stifn(i)
!           write(6,*) "stifn numnod+1",nodes%stifn(numnod+1)  ,"            ",nodes%itab(numnod+1),nodes%itab(i)
!           write(6,*) "stifn        i",nodes%stifn(i)  ,"            ",nodes%itab(numnod+1),nodes%itab(i)

          nodes%ITABM1(numnod+1) = nodes%max_uid
          nodes%ITABM1(2*(numnod+1)) = numnod + 1


        end subroutine set_new_node_values

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
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          new_uid = nodes%max_uid
          old_uid = nodes%itab(node_id)
          new_local_id = nodes%numnod +1
!           write(6,*) 'detach_from_shells old_uid = ', old_uid, ' new_uid = ', new_uid
!           write(6,*) 'new_local_id = ', new_local_id
          do i = 1, list_size
            do j = 1,4
              if(elements%shell%nodes(j,shell_list(i)) == node_id) then
                elements%shell%nodes(j,shell_list(i)) = new_local_id
                elements%shell%ixc(j+1,shell_list(i)) = new_local_id
             !  write(6,*) '---- Detached node ', old_uid, ' from shell ',shell_list(i),elements%shell%user_id(shell_list(i))
              end if
            enddo
          end do
        end subroutine detach_node_from_shells
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
!           write(6,*) "detach_node",node_id,nodes%itab(node_id),"from:",shell_list(1:list_size)
          new_uid = nodes%max_uid + 1
          nodes%max_uid = new_uid
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


        ! this is an example on how to detach nodes, it will not give physical crack propagation
        subroutine test_jc_shell_detach(nodes, element, interf, npari, ninter, ipari, numnod, &
          numnodg, elbuf, ngroup, ngrouc, nparg, iparg, igrouc, numelc, ispmd, &
          lcnel, cnel, addcnel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod, only : TWO
          USE connectivity_mod
          USE nodal_arrays_mod
          USE interfaces_mod
          USE elbufdef_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
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
          integer, intent(in) ::lcnel
          integer, dimension(0:numnod+1), intent(in) :: addcnel ! address for the cnel array
          integer, dimension(0:lcnel), intent(in) :: cnel ! connectivity node-->element

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable :: detach_shell
          integer :: ig,ng,numnod0,i,j,k,l,n1,n2,n3,n4,nel,nft
          double precision, dimension(:,:,:), allocatable :: boxes
          double precision :: discrepancy,max_discrepancy
          integer :: crack_root, next_root
          integer, dimension(20) :: crack !< id of the noodes that are part of the crack
          integer :: ncrack
          integer, dimension(:), allocatable :: shell_list
          integer :: shells_to_detach
          integer :: shell_id
          double precision :: normal(3),  vec(3), distance
          double precision, dimension(:), allocatable :: nodal_damage
          double precision, dimension(3) :: shell_centroid
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(detach_shell(0:numelc))
          detach_shell = 0.0D0
          if(.not. allocated(element%shell%damage)) then
            allocate(element%shell%damage(1:numelc))
            element%shell%damage = 0.0D0
          endif
          do ig = 1, ngrouc
            ng = igrouc(ig)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            ! gather jc dfmax values
            detach_shell(nft+1: nft+nel) =  elbuf(ng)%bufly(1)%fail(1,1,1)%floc(1)%dammx(1:nel)
            ! detach_shell(nft+1: nft+nel) = detach_shell(nft+1: nft+nel) - element%shell%damage(nft+1: nft+nel)
          enddo

          numnod0 = numnod
          allocate(boxes(numnod,3,2))
          allocate(nodal_damage(numnod))
          nodal_damage = 0.0D0
          do i = 1,numnod
            boxes(i,1,1) = HUGE(boxes(i,1,1))
            boxes(i,2,1) = HUGE(boxes(i,2,1))
            boxes(i,3,1) = HUGE(boxes(i,3,1))
            boxes(i,1,2) = -HUGE(boxes(i,1,2))
            boxes(i,2,2) = -HUGE(boxes(i,2,2))
            boxes(i,3,2) = -HUGE(boxes(i,3,2))
          enddo

          do i = 1, size( element%shell%user_id,1)
            n1 = element%shell%ixc(2,i)
            n2 = element%shell%ixc(3,i)
            n3 = element%shell%ixc(4,i)
            n4 = element%shell%ixc(5,i)
            nodal_damage(n1) = nodal_damage(n1) + detach_shell(i)
            nodal_damage(n2) = nodal_damage(n2) + detach_shell(i)
            nodal_damage(n3) = nodal_damage(n3) + detach_shell(i)
            nodal_damage(n4) = nodal_damage(n4) + detach_shell(i)

            do k = 1,3
              boxes(n1,k,1) = min(boxes(n1,k,1),nodes%V(k,n1))
              boxes(n1,k,1) = min(boxes(n1,k,1),nodes%V(k,n2))
              boxes(n1,k,1) = min(boxes(n1,k,1),nodes%V(k,n3))
              boxes(n1,k,1) = min(boxes(n1,k,1),nodes%V(k,n4))
              boxes(n1,k,2) = max(boxes(n1,k,2),nodes%V(k,n1))
              boxes(n1,k,2) = max(boxes(n1,k,2),nodes%V(k,n2))
              boxes(n1,k,2) = max(boxes(n1,k,2),nodes%V(k,n3))
              boxes(n1,k,2) = max(boxes(n1,k,2),nodes%V(k,n4))
              boxes(n2,k,1) = min(boxes(n2,k,1),nodes%V(k,n1))
              boxes(n2,k,1) = min(boxes(n2,k,1),nodes%V(k,n2))
              boxes(n2,k,1) = min(boxes(n2,k,1),nodes%V(k,n3))
              boxes(n2,k,1) = min(boxes(n2,k,1),nodes%V(k,n4))
              boxes(n2,k,2) = max(boxes(n2,k,2),nodes%V(k,n1))
              boxes(n2,k,2) = max(boxes(n2,k,2),nodes%V(k,n2))
              boxes(n2,k,2) = max(boxes(n2,k,2),nodes%V(k,n3))
              boxes(n2,k,2) = max(boxes(n2,k,2),nodes%V(k,n4))
              boxes(n3,k,1) = min(boxes(n3,k,1),nodes%V(k,n1))
              boxes(n3,k,1) = min(boxes(n3,k,1),nodes%V(k,n2))
              boxes(n3,k,1) = min(boxes(n3,k,1),nodes%V(k,n3))
              boxes(n3,k,1) = min(boxes(n3,k,1),nodes%V(k,n4))
              boxes(n3,k,2) = max(boxes(n3,k,2),nodes%V(k,n1))
              boxes(n3,k,2) = max(boxes(n3,k,2),nodes%V(k,n2))
              boxes(n3,k,2) = max(boxes(n3,k,2),nodes%V(k,n3))
              boxes(n4,k,1) = min(boxes(n4,k,1),nodes%V(k,n1))
              boxes(n4,k,1) = min(boxes(n4,k,1),nodes%V(k,n2))
              boxes(n4,k,1) = min(boxes(n4,k,1),nodes%V(k,n3))
              boxes(n4,k,1) = min(boxes(n4,k,1),nodes%V(k,n4))
              boxes(n4,k,2) = max(boxes(n4,k,2),nodes%V(k,n1))
              boxes(n4,k,2) = max(boxes(n4,k,2),nodes%V(k,n2))
              boxes(n4,k,2) = max(boxes(n4,k,2),nodes%V(k,n3))
              boxes(n4,k,2) = max(boxes(n4,k,2),nodes%V(k,n4))
            enddo
          enddo

          allocate(shell_list(numelc))
          shell_list = 0
          shells_to_detach = 0
          max_discrepancy = -1.0D0
          crack_root = 0

          do i = 1, numnod
            if(nodal_damage(i) == 0.0D0) cycle
            if(nodes%nchilds(nodes%parent_node(i)) > 0) cycle 
            discrepancy = 0.0
            do l= 1, 3
              discrepancy = discrepancy + (boxes(i, l, 2) - boxes(i, l, 1))**2
            end do
            discrepancy = sqrt(discrepancy)
            if (discrepancy > max_discrepancy.and. discrepancy > 2.0D0) then
              max_discrepancy = discrepancy
              crack_root = i
            end if
          enddo
!         if(crack_root >0) write(6,*)  "crack_root",crack_root,max_discrepancy

          ! crack propagation : list nodes
          ncrack = 0
          do while (crack_root >0 .AND. ncrack < 20)
            ncrack = ncrack + 1
            crack(ncrack) = crack_root
            ! loop over elements connected to the nodes
            max_discrepancy = -1.0D0
            next_root = 0
            do i = addcnel(crack_root), addcnel(crack_root+1)-1
              shell_id = cnel(i) - element%shell%offset
              if(detach_shell(shell_id) > 0.999d0) cycle
              if(element%shell%damage(shell_id) > 0.0D0) cycle
              do j = 1, 4
                n1 = element%shell%ixc(j+1,shell_id)
!               if(nodes%nchilds(nodes%parent_node(n1)) > 0) cycle 
                if(n1 == crack_root) cycle
                do l = 1, ncrack
                  if(n1 == crack(l)) exit 
                enddo
                if(n1 == crack(l)) cycle
                if(nodal_damage(n1) > 0.0D0) then
                  discrepancy = 0.0
                  do  l= 1, 3
                    discrepancy = discrepancy + (boxes(n1, l, 2) - boxes(n1, l, 1))**2
                  end do
                  discrepancy = sqrt(discrepancy)
                  if (discrepancy > max_discrepancy) then
                    max_discrepancy = discrepancy
                    next_root = n1
                  end if
                endif
              enddo
            enddo
            if(next_root > 0 .and. max_discrepancy > 2.0D0) then
              crack_root = next_root
            else
              crack_root = 0
            endif
          enddo

!         if(ncrack>0) write(6,*) "ncrack=",ncrack

          ! select the shells to be detached, looking at the side of the crack
          do i = 1, numelc
            shell_centroid = 0.0
            normal = 0.0
            vec = 0.0
            ! Identify the first crack node in this shell
             if(detach_shell(i) > 0.999d0) cycle
            ! if the element has already a crack passing through one of its nodes
            ! then it cannot be detached again
!           if(element%shell%damage(i)  > 0.0D0) cycle 
            do l = 2, 5
              shell_centroid(1:3) = shell_centroid(1:3) + nodes%X(1:3,element%shell%IXC(l, i))
            end do
            shell_centroid(1:3) = shell_centroid(1:3) / 4.0  ! Average over 4 nodes
            do j = 2, 5
              do k = 1, ncrack-1
                if (element%shell%IXC(j, i) == crack(k)) then
                  ! Compute local normal using the next crack node
                  normal(1:3) = nodes%X(1:3,crack(k+1)) - nodes%X(1:3,crack(k))

                  ! Normalize the local normal
                  distance = sqrt(sum(normal**2))
                  if (distance > 0) normal = normal / distance

                  ! Compute vector from crack node to shell centroid
                  vec(1:3) = shell_centroid(1:3) - nodes%X(1:3,crack(k))

                  ! Compute signed distance using dot product
                  distance = sum(vec(1:3) * normal(1:3))

                  if (distance > 0) then
                    shells_to_detach = shells_to_detach + 1
                    shell_list(shells_to_detach) = i
                    element%shell%damage(i) = 1.0D0
                  end if
                  exit  ! Only process the first crack node found in this shell
                end if
              end do
            end do
          enddo

          do i = 1, ncrack
!           write(6,*) "crack node",i,crack(i)
            call detach_node(nodes,crack(i),element,shell_list,shells_to_detach,npari,ninter, ipari, interf)
            numnod = numnod + 1
            if(ispmd == 0) numnodg = numnodg + 1
          enddo

          deallocate(detach_shell)
          deallocate(boxes)
          deallocate(shell_list)
          deallocate(nodal_damage)

        end subroutine test_jc_shell_detach

      end module detach_node_mod
