!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    detach_node_mod     ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    apply_crack         ../engine/source/engine/node_spliting/apply_crack.F90
!||    nloc_shell_detach   ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||    resol               ../engine/source/engine/resol.F
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
!||    inter11_duplicate_edge        ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    extend_array_mod              ../common_source/tools/memory/extend_array.F90
!||    intbufdef_mod                 ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
        subroutine inter11_duplicate_edge(flag,old_edge_id, new_edge_id, intbuf_tab, ipari,npari)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use extend_array_mod
          use intbufdef_mod
          use constant_mod, only : zero
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer , intent(in) :: flag !< 1: main edge, 2:secondary edge
          integer, intent(in) :: old_edge_id !< id of the old edge
          integer, intent(in) :: new_edge_id !< id of the new edge
          type(intbuf_struct_), intent(inout) :: intbuf_tab !< interface buffer
          integer, intent(in) :: npari
          integer, intent(in) :: ipari(npari) !< interface parameters
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: igap
          integer :: intth
          integer :: intfric
          integer :: nrtm, nrts
! ----------------------------------------------------------------------------------------------------------------------
          igap = ipari(21)
          intth = ipari(47)
          intfric = ipari(72)
          nrtm = new_edge_id
          nrts = new_edge_id

          if(flag == 1) then
            call extend_array(intbuf_tab%gap_m,intbuf_tab%s_gap_m,nrtm)
            intbuf_tab%gap_m(nrtm) = intbuf_tab%gap_m(old_edge_id)
            intbuf_tab%s_gap_m   = nrtm
            call extend_array(intbuf_tab%stfm,intbuf_tab%s_stfm,nrtm)
            intbuf_tab%stfm(nrtm) = intbuf_tab%stfm(old_edge_id)
            intbuf_tab%s_stfm    = nrtm
            if (igap == 3 ) then
              call extend_array(intbuf_tab%gap_ml,intbuf_tab%s_gap_ml,nrtm)
              intbuf_tab%gap_ml(nrtm) = intbuf_tab%gap_ml(old_edge_id)
              intbuf_tab%s_gap_ml  = nrtm
            endif
            if(intth > 0 ) then
              call extend_array(intbuf_tab%ieles,intbuf_tab%s_ieles,nrtm)
              intbuf_tab%ieles(nrtm) = intbuf_tab%ieles(old_edge_id)
              intbuf_tab%s_ieles = nrtm
              call extend_array(intbuf_tab%areas,intbuf_tab%s_aream,nrtm)
              intbuf_tab%areas(nrtm) = intbuf_tab%areas(old_edge_id)
              intbuf_tab%s_aream = nrtm
            endif
            if (intfric/=0) then
              call extend_array(intbuf_tab%ipartfricm,intbuf_tab%s_ipartfricm,nrtm)
              intbuf_tab%ipartfricm(nrtm) = intbuf_tab%ipartfricm(old_edge_id)
              intbuf_tab%s_ipartfricm  = nrtm
            endif
          else if (flag == 2) then
            call extend_array(intbuf_tab%gap_s,intbuf_tab%s_gap_s,nrts)
            intbuf_tab%gap_s(nrts) = intbuf_tab%gap_s(old_edge_id)
            intbuf_tab%s_stfs    = zero! nrts
            call extend_array(intbuf_tab%stfs,intbuf_tab%s_stfs,nrts)
            intbuf_tab%stfs(nrts) = zero ! intbuf_tab%stfs(old_edge_id)
            intbuf_tab%s_gap_s   = nrts
            if (igap == 3 ) then
              call extend_array(intbuf_tab%gap_sl,intbuf_tab%s_gap_sl,nrts)
              intbuf_tab%gap_sl(nrts) = intbuf_tab%gap_sl(old_edge_id)
              intbuf_tab%s_gap_sl  = nrts
            endif
            if(intth > 0 ) then
              call extend_array(intbuf_tab%ielec,intbuf_tab%s_ielec,nrts)
              intbuf_tab%ielec(nrts) = intbuf_tab%ielec(old_edge_id)
              intbuf_tab%s_ielec = nrts
              call extend_array(intbuf_tab%areas,intbuf_tab%s_areas,nrts)
              intbuf_tab%areas(nrts) = intbuf_tab%areas(old_edge_id)
              intbuf_tab%s_areas = nrts
            endif
            if (intfric/=0) then
              call extend_array(intbuf_tab%ipartfrics,intbuf_tab%s_ipartfrics,nrts)
              intbuf_tab%ipartfrics(nrts) = intbuf_tab%ipartfrics(old_edge_id)
              intbuf_tab%s_ipartfrics  = nrts
            endif
          endif

        end subroutine inter11_duplicate_edge


!||====================================================================
!||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    detach_node                   ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||    find_segment_in_list          ../engine/source/engine/node_spliting/detach_node.F90
!||    inter11_duplicate_edge        ../engine/source/engine/node_spliting/detach_node.F90
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
          integer :: i,j,i1,i2
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
          integer :: new_edges
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          still_connected = .FALSE.
          old_secondary_node = 0
          do i = 1, ninter
            itype = IPARI(INDEX_ITYPE,i)
            nsn = IPARI(INDEX_NSN,i)
            nmn = IPARI(INDEX_NMN,i)
            nrtm = IPARI(INDEX_NRTM,i)
            intth  = ipari(INDEX_INTTH,i)
            IGAP   = IPARI(INDEX_IGAP,i)
            nrts   = IPARI(INDEX_NRTS,i)
            if(itype == 7) then
              ! search if node_id is in interf%intbuf_tab(i)%MSR(1:NMN)
              is_found = any(interf%intbuf_tab(i)%MSR(1:nmn) == node_id)
              if(is_found) then
                ! extend the arrays MSR
                call extend_array(interf%intbuf_tab(i)%MSR,nmn,nmn+1)
                interf%intbuf_tab(i)%MSR(nmn+1) = -  (nodes%numnod + 1) ! remove from contact search
                IPARI(INDEX_NMN,i) = nmn + 1
                ! large value in xsav should trigger the contact search
                interf%intbuf_tab(i)%S_MSR = interf%intbuf_tab(i)%S_MSR + 1
                nmn = nmn + 1
              end if

              is_found = any(interf%intbuf_tab(i)%NSV(1:nsn) == node_id)
              if(is_found) then
                ! old_secondary_node is j such as interf%intbuf_tab(i)%NSV(j) == node_id
                do j = 1, nsn
                  if(interf%intbuf_tab(i)%NSV(j) == node_id) then
                    old_secondary_node = j
                    exit
                  end if
                end do
                call extend_array(interf%intbuf_tab(i)%NSV,nsn,nsn+1)
                interf%intbuf_tab(i)%NSV(nsn+1) = nodes%numnod + 1
                interf%intbuf_tab(i)%S_NSV = nsn + 1
                if(intth > 0) then
                  call extend_array(interf%intbuf_tab(i)%ielec,nsn,nsn+1)
                  interf%intbuf_tab(i)%s_ielec = nsn + 1
                end if
                interf%intbuf_tab(i)%s_stfns   = nsn + 1
                call extend_array(interf%intbuf_tab(i)%stfns,nsn,nsn+1)
                ! Ignore new nodes from the interface
                interf%intbuf_tab(i)%stfns(nsn+1) = ZERO ! - interf%intbuf_tab(i)%stfns(old_secondary_node)
                IF(IGAP > 0) THEN
                  interf%intbuf_tab(i)%s_gap_s   = nsn + 1
                  call extend_array(interf%intbuf_tab(i)%gap_s,nsn,nsn+1)
                  interf%intbuf_tab(i)%gap_s(nsn+1) = interf%intbuf_tab(i)%gap_s(old_secondary_node)
                END IF
                IF (INTTH > 0 ) THEN
                  interf%intbuf_tab(i)%s_areas   = nsn + 1
                  call extend_array(interf%intbuf_tab(i)%areas,nsn,nsn+1)
                  interf%intbuf_tab(i)%areas(nsn+1) = interf%intbuf_tab(i)%areas(old_secondary_node)
                END IF
                IF (IGAP == 3 ) THEN
                  interf%intbuf_tab(i)%s_gap_sl  = nsn + 1
                  call extend_array(interf%intbuf_tab(i)%gap_sl,nsn,nsn+1)
                  interf%intbuf_tab(i)%gap_sl(nsn+1) = interf%intbuf_tab(i)%gap_sl(old_secondary_node)
                END IF
!                 if(ipari(index_intfric) > 0) then
!                      not supported yet
!                 endif
                IPARI(INDEX_NSN,i) = nsn + 1
                nsn = nsn + 1
              end if

              if(interf%intbuf_tab(i)%s_xsav  < 3*min(nodes%numnod,nsn+nmn)) then
                call extend_array(interf%intbuf_tab(i)%xsav,interf%intbuf_tab(i)%s_xsav,3*min(nodes%numnod,nsn+nmn))
                interf%intbuf_tab(i)%s_xsav = 3*min(nodes%numnod,nsn+nmn)
                interf%intbuf_tab(i)%xsav = - HUGE(interf%intbuf_tab(i)%xsav(1))
              end if

              ! if the main segment has the same nodes as a detached shell, then the segment is detached
              do j = 1, nrtm
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 1) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 1) = nodes%numnod + 1
                end if
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 2) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 2) = nodes%numnod + 1
                end if
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 3) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 3) = nodes%numnod + 1
                end if
                if(interf%intbuf_tab(i)%irectm((j-1)*4 + 4) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irectm((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irectm((j-1)*4 + 4) = nodes%numnod + 1
                end if
              end do
              do j = 1, nrts
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 1) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 1) = nodes%numnod + 1
                end if
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 2) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 2) = nodes%numnod + 1
                end if
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 3) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 3) = nodes%numnod + 1
                end if
                if(interf%intbuf_tab(i)%irects((j-1)*4 + 4) == node_id) then
                  is_found=find_segment_in_list(interf%intbuf_tab(i)%irects((j-1)*4+1:(j-1)*4+4),shell_list,list_size,elements)
                  if(is_found) interf%intbuf_tab(i)%irects((j-1)*4 + 4) = nodes%numnod + 1
                end if
              end do
            else if (itype == 11) then

              ! search if node_id is in interf%intbuf_tab(i)%MSR(1:NMN)
              is_found = any(interf%intbuf_tab(i)%MSR(1:nmn) == node_id)
              if(is_found) then
                ! extend the arrays MSR
                call extend_array(interf%intbuf_tab(i)%MSR,nmn,nmn+1)
                interf%intbuf_tab(i)%MSR(nmn+1) = nodes%numnod + 1
                IPARI(INDEX_NMN,i) = nmn + 1
                ! large value in xsav should trigger the contact search
                interf%intbuf_tab(i)%S_MSR = -(interf%intbuf_tab(i)%S_MSR + 1)
                nmn = nmn + 1
              end if
              is_found = any(interf%intbuf_tab(i)%NSV(1:nsn) == node_id)
              if(is_found) then
                ! old_secondary_node is j such as interf%intbuf_tab(i)%NSV(j) == node_id
                do j = 1, nsn
                  if(interf%intbuf_tab(i)%NSV(j) == node_id) then
                    old_secondary_node = j
                    exit
                  end if
                end do
                call extend_array(interf%intbuf_tab(i)%NSV,nsn,nsn+1)
                interf%intbuf_tab(i)%NSV(nsn+1) = nodes%numnod + 1
                interf%intbuf_tab(i)%S_NSV = nsn + 1
                IPARI(INDEX_NSN,i) = nsn + 1
                nsn = nsn + 1
              end if
              if(interf%intbuf_tab(i)%s_xsav  < 3*min(nodes%numnod,nsn+nmn)) then
                call extend_array(interf%intbuf_tab(i)%xsav,interf%intbuf_tab(i)%s_xsav,3*min(nodes%numnod,nsn+nmn))
                interf%intbuf_tab(i)%s_xsav = 3*min(nodes%numnod,nsn+nmn)
                interf%intbuf_tab(i)%xsav = - HUGE(interf%intbuf_tab(i)%xsav(1))
              end if


              !edge 2 edge interface irectm = two nodes of the edge
              new_edges = 0
              do j =1, nrtm ! loop over edges
                if(interf%intbuf_tab(i)%irectm((j-1)*2 + 1) == node_id) then
                  !if the node is split, then a new node is created, and new edges are created
                  new_edges = new_edges + 1
                else if(interf%intbuf_tab(i)%irectm((j-1)*2 + 2) == node_id) then
                  new_edges = new_edges + 1
                end if
              end do
              ! extend irect by 2*new_edges
              if(new_edges > 0) then
                call extend_array(interf%intbuf_tab(i)%irectm, &
                  interf%intbuf_tab(i)%s_irectm, &
                  interf%intbuf_tab(i)%s_irectm + 2*new_edges)
                interf%intbuf_tab(i)%s_irectm = interf%intbuf_tab(i)%s_irectm + 2*new_edges
                IPARI(INDEX_NRTM,i) = nrtm + new_edges
                new_edges = 0
                do j = 1, nrtm
                  i1 = interf%intbuf_tab(i)%irectm((j-1)*2 + 1)
                  i2 = interf%intbuf_tab(i)%irectm((j-1)*2 + 2)
                  if(interf%intbuf_tab(i)%irectm((j-1)*2 + 1) == node_id) then
                    ! a new edge is created, with the new node
                    interf%intbuf_tab(i)%irectm((nrtm+new_edges)*2 + 1) = nodes%numnod + 1
                    interf%intbuf_tab(i)%irectm((nrtm+new_edges)*2 + 2) = i2
                    new_edges = new_edges + 1
                    call inter11_duplicate_edge(1,j, nrtm+new_edges, interf%intbuf_tab(i), ipari(:,i),npari)
                  else if(interf%intbuf_tab(i)%irectm((j-1)*2 + 2) == node_id) then
                    interf%intbuf_tab(i)%irectm((nrtm+new_edges)*2 + 1) = i1
                    interf%intbuf_tab(i)%irectm((nrtm+new_edges)*2 + 2) = nodes%numnod + 1
                    new_edges = new_edges + 1
                    call inter11_duplicate_edge(1,j, nrtm+new_edges, interf%intbuf_tab(i), ipari(:,i),npari)
                  end if
                end do
              end if

              !same for secondary edges in irects
              new_edges = 0
              do j=1,nrts
                if(interf%intbuf_tab(i)%irects((j-1)*2 + 1) == node_id) then
                  new_edges = new_edges + 1
                else if(interf%intbuf_tab(i)%irects((j-1)*2 + 2) == node_id) then
                  new_edges = new_edges + 1
                end if
              end do
              if(new_edges > 0) then
                IPARI(INDEX_NRTS,i) = nrts + new_edges
                call extend_array(interf%intbuf_tab(i)%irects, &
                  interf%intbuf_tab(i)%s_irects, &
                  interf%intbuf_tab(i)%s_irects + 2*new_edges)
                interf%intbuf_tab(i)%s_irects = interf%intbuf_tab(i)%s_irects + 2*new_edges
                new_edges = 0
                do j=1,nrts
                  i1 = interf%intbuf_tab(i)%irects((j-1)*2 + 1)
                  i2 = interf%intbuf_tab(i)%irects((j-1)*2 + 2)
                  if(interf%intbuf_tab(i)%irects((j-1)*2 + 1) == node_id) then
                    interf%intbuf_tab(i)%irects((nrts+new_edges)*2 + 1) = nodes%numnod + 1
                    interf%intbuf_tab(i)%irects((nrts+new_edges)*2 + 2) = i2
                    new_edges = new_edges + 1
                    call inter11_duplicate_edge(2,j, nrts+new_edges, interf%intbuf_tab(i), ipari(:,i),npari)
                  else if(interf%intbuf_tab(i)%irects((j-1)*2 + 2) == node_id) then
                    interf%intbuf_tab(i)%irects((nrts+new_edges)*2 + 2) = nodes%numnod + 1
                    interf%intbuf_tab(i)%irects((nrts+new_edges)*2 + 1) = i1
                    new_edges = new_edges + 1
                    call inter11_duplicate_edge(2,j, nrts+new_edges, interf%intbuf_tab(i), ipari(:,i),npari)
                  end if
                end do
              end if
            end if
          end do

        end subroutine detach_node_from_interfaces

!! \brief Register split nodes with rigid-wall secondary-node lists.
!!
!! \details When a node is split, the new node must be added to every rigid wall
!!          that already tracks the parent node as a secondary node.  The rigid-wall
!!          force loop (rwall_fpen) iterates only over the pre-built lprw list, so
!!          new nodes would otherwise never receive wall forces and could penetrate
!!          the rigid surface indefinitely.
!!
!!          Called from resol.F in the new_crack > 0 block, after nloc_shell_detach.
!!          Handles no-friction walls fully.  For friction walls (ifq > 0) the rwsav
!!          extension is skipped; add it here when friction + node splitting is needed.
!||====================================================================
!||    detach_node_from_rwalls   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    resol                     ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    extend_array_mod          ../common_source/tools/memory/extend_array.F90
!||    nodal_arrays_mod          ../common_source/modules/nodal_arrays.F90
!||    rwall_mod                 ../common_source/modules/constraints/rwall_mod.F90
!||====================================================================
        subroutine detach_node_from_rwalls(nodes, numnod_old, numnod_new, rwall)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use nodal_arrays_mod
          use rwall_mod
          use extend_array_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(in)    :: nodes         !< nodal arrays (for parent_node lookup)
          integer,             intent(in)    :: numnod_old    !< node count before splits
          integer,             intent(in)    :: numnod_new    !< node count after splits
          type(rwall_),        intent(inout) :: rwall         !< rigid wall structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer        :: inew, parent_id
          integer        :: irw, j, k_start, nsn, ipen, insert_pos, n_p_start, n_p_insert
          integer        :: old_sz, old_lnspen
          real(kind=WP)  :: ZERO
          parameter      (ZERO = 0.0_WP)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (rwall%nrwall == 0) return

          do inew = numnod_old + 1, numnod_new
            parent_id = nodes%parent_node(inew)

            ! Walk wall-by-wall through lprw; track penalty offset n_p_start independently.
            k_start   = 1
            n_p_start = 1
            do irw = 1, rwall%nrwall
              nsn  = rwall%nprw(irw, 1)
              ipen = rwall%nprw(irw, 9)

              ! Search parent in this wall's secondary-node block.
              insert_pos = 0
              do j = k_start, k_start + nsn - 1
                if (rwall%lprw(j) == parent_id) then
                  insert_pos = k_start + nsn  ! append after last node of this wall
                  exit
                end if
              end do

              if (insert_pos > 0) then
                ! ---- Extend lprw and shift subsequent walls' entries right by 1 ----
                old_sz = rwall%sz_lprw
                call extend_array(rwall%lprw, old_sz, old_sz + 1)
                if (insert_pos <= old_sz) then
                  rwall%lprw(insert_pos + 1 : old_sz + 1) = rwall%lprw(insert_pos : old_sz)
                end if
                rwall%lprw(insert_pos) = inew
                rwall%nprw(irw, 1)    = nsn + 1
                rwall%sz_lprw         = old_sz + 1

                ! ---- Extend penalty arrays if this wall uses penalty ---------------
                if (ipen > 0 .and. allocated(rwall%pen%pen_old)) then
                  n_p_insert = n_p_start + nsn   ! append after this wall's penalty block
                  old_lnspen = rwall%pen%lnspen

                  call extend_array(rwall%pen%pen_old, old_lnspen, old_lnspen + 1)
                  if (n_p_insert <= old_lnspen) then
                    rwall%pen%pen_old(n_p_insert + 1 : old_lnspen + 1) = &
                      rwall%pen%pen_old(n_p_insert : old_lnspen)
                  end if
                  rwall%pen%pen_old(n_p_insert) = ZERO

                  call extend_array(rwall%pen%stif, old_lnspen, old_lnspen + 1)
                  if (n_p_insert <= old_lnspen) then
                    rwall%pen%stif(n_p_insert + 1 : old_lnspen + 1) = &
                      rwall%pen%stif(n_p_insert : old_lnspen)
                  end if
                  rwall%pen%stif(n_p_insert) = ZERO

                  call extend_array(rwall%pen%ft, 3, old_lnspen, 3, old_lnspen + 1)
                  if (n_p_insert <= old_lnspen) then
                    rwall%pen%ft(:, n_p_insert + 1 : old_lnspen + 1) = &
                      rwall%pen%ft(:, n_p_insert : old_lnspen)
                  end if
                  rwall%pen%ft(:, n_p_insert) = ZERO

                  rwall%pen%lnspen = old_lnspen + 1
                end if
                ! Note: rwsav (friction saved positions) is not extended here because
                ! the current model has no friction (fric=0 → ifq=0 → rwsav unused).
                ! Extend rwsav(3*nsn_new) when friction + node splitting is required.
              end if

              k_start = k_start + nsn
              if (ipen > 0) n_p_start = n_p_start + nsn
            end do
          end do

        end subroutine detach_node_from_rwalls
        !\brief This subroutine sets the values of the new node using the values of the old node
!||====================================================================
!||    set_new_node_values   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    apply_crack           ../engine/source/engine/node_spliting/apply_crack.F90
!||    detach_node           ../engine/source/engine/node_spliting/detach_node.F90
!||    mirror_node_split     ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod      ../common_source/modules/connectivity.F90
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    nodal_arrays_mod      ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine set_new_node_values(nodes,i,mass_fraction)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod, only : ONE, EM20
          USE connectivity_mod
          USE nodal_arrays_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
          integer, intent(in) :: i                 !< id of the node to detach
          real(kind=WP), intent(in) :: mass_fraction !< share of the parent mass/inertia/force taken by the new node
          !< (area-weighted, computed by apply_crack; identical on every rank).  The parent keeps
          !< the complement (1 - mass_fraction) so mass, inertia and assembled force are conserved.
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
          end if


          if(nodes%iroddl >0) then
            nodes%VR(1:3,numnod+1) = nodes%VR(1:3,i)
            ! Rotational inertia is split like the mass (area-weighted); previously the
            ! parent inertia was duplicated onto the child, creating inertia from nothing.
            nodes%IN(numnod+1)  = nodes%IN(i)  * mass_fraction
            nodes%IN(i)         = nodes%IN(i)  * (ONE - mass_fraction)
            nodes%IN0(numnod+1) = nodes%IN0(i) * mass_fraction
            nodes%IN0(i)        = nodes%IN0(i) * (ONE - mass_fraction)
            nodes%ICODR(numnod+1) = nodes%ICODR(i)
          end if
          if(nodes%sicodt_fac >0) nodes%ICODT(numnod+1) = nodes%ICODT(i)
          if(nodes%used_dr) then
            nodes%DR(1:3,numnod+1) = nodes%DR(1:3,i)
          end if
          ! Area-weighted mass split: the new node takes the share of the parent lumped
          ! mass corresponding to the attached-shell area that migrates with it; the
          ! parent keeps the complement.  Total mass is conserved exactly.
          nodes%MS(numnod+1) = nodes%MS(i)  * mass_fraction
          nodes%MS(i)        = nodes%MS(i)  * (ONE - mass_fraction)
          nodes%MS0(numnod+1) = nodes%MS0(i) * mass_fraction
          nodes%MS0(i)        = nodes%MS0(i) * (ONE - mass_fraction)

#ifdef MYREAL4
          nodes%DDP(1:3,numnod+1) = nodes%DDP(1:3,i)
          nodes%XDP(1:3,numnod+1) = nodes%XDP(1:3,i)
          if(nodes%iparith==0) then
            ! double-precision force accumulator: split like A below
            nodes%ACC_DP(1:3,numnod+1) = nodes%ACC_DP(1:3,i) * mass_fraction
            nodes%ACC_DP(1:3,i)        = nodes%ACC_DP(1:3,i) * (ONE - mass_fraction)
          end if

#endif
          nodes%WEIGHT(numnod+1) = 1! nodes%WEIGHT(i)
          nodes%WEIGHT_MD(numnod+1) = nodes%WEIGHT_MD(i)
          nodes%MAIN_PROC(numnod+1) = nodes%MAIN_PROC(i)

          ! The assembled force/moment (A/AR still hold forces at this point; ACCELE runs
          ! after the split) was accumulated over the full pre-split shell fan.  Splitting
          ! it with the same fraction as the mass keeps the acceleration A/MS (and AR/IN)
          ! continuous across the split cycle for both nodes, instead of amplifying it by
          ! 1/mass_fraction on the child and 1/(1-mass_fraction) on the parent.
          if(nodes%iparith == 0) then
            nodes%A(1:3,numnod+1)  = nodes%A(1:3,i)  * mass_fraction
            nodes%A(1:3,i)         = nodes%A(1:3,i)  * (ONE - mass_fraction)
            nodes%AR(1:3,numnod+1) = nodes%AR(1:3,i) * mass_fraction
            nodes%AR(1:3,i)        = nodes%AR(1:3,i) * (ONE - mass_fraction)
            if(nodes%iroddl > 0) nodes%STIFR(numnod+1) = EM20
            nodes%VISCN(numnod+1) = nodes%VISCN(i)
            nodes%STIFN(numnod+1) = EM20  ! will be reaccumulated from elements next cycle
          else
            nodes%A(1:3,numnod+1)  = nodes%A(1:3,i)  * mass_fraction
            nodes%A(1:3,i)         = nodes%A(1:3,i)  * (ONE - mass_fraction)
            nodes%AR(1:3,numnod+1) = nodes%AR(1:3,i) * mass_fraction
            nodes%AR(1:3,i)        = nodes%AR(1:3,i) * (ONE - mass_fraction)
            nodes%STIFR(numnod+1) = EM20
            nodes%VISCN(numnod+1) = nodes%VISCN(i)
            nodes%STIFN(numnod+1) = EM20  ! will be reaccumulated from elements next cycle
          end if
          p = i
          nodes%parent_node(numnod+1) = i
          do while(nodes%parent_node(p) /= p)
            p = nodes%parent_node(p)
          end do
          nodes%parent_node(numnod+1) = p
          nodes%nchilds(p) = nodes%nchilds(p) + 1

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
!||    mirror_node_split         ../engine/source/engine/node_spliting/detach_node.F90
!||--- calls      -----------------------------------------------------
!||    update_pon_shells         ../engine/source/engine/node_spliting/update_pon.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod          ../common_source/modules/connectivity.F90
!||    constant_mod              ../common_source/modules/constant_mod.F
!||    nodal_arrays_mod          ../common_source/modules/nodal_arrays.F90
!||    update_pon_mod            ../engine/source/engine/node_spliting/update_pon.F90
!||====================================================================
        subroutine detach_node_from_shells(nodes, node_id, elements, shell_list, list_size, &
          ispmd, m, row_uid, row_procne)
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
          integer, intent(in) :: ispmd                   !< 0-based local MPI rank
          integer, intent(in) :: m                       !< total FSKY rows for N' (canonical order, all ranks)
          integer, intent(in) :: row_uid(m)              !< shell user id of each canonical row (uid-sorted)
          integer, intent(in) :: row_procne(m)           !< 1-based owning rank of each canonical row
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
          integer :: old_uid
          integer :: new_local_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          old_uid = nodes%itab(node_id)
          new_local_id = nodes%numnod +1
          do i = 1, list_size
            do j = 1,4
              if(elements%shell%nodes(j,shell_list(i)) == node_id) then
                elements%shell%nodes(j,shell_list(i)) = new_local_id
                elements%shell%ixc(j+1,shell_list(i)) = new_local_id
              end if
            end do
          end do

          if(nodes%iparith > 0) then! /PARITH/ON
            call update_pon_shells(nodes%itab(node_id), elements, list_size, shell_list, new_local_id, &
              ispmd, m, row_uid, row_procne)
          end if


        end subroutine detach_node_from_shells

!! \brief Create a ghost copy of a split node on a non-owning MPI rank.
!! \details Called on a rank that has local shells going to the new node N' but does not own N'.
!!          Equivalent to detach_node but skips interface duplication and sets MAIN_PROC to
!!          owning_rank instead of copying from the parent. The UID is assigned later in the
!!          global uid-sync phase of apply_crack.
!||====================================================================
!||    mirror_node_split         ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    apply_crack               ../engine/source/engine/node_spliting/apply_crack.F90
!||--- calls      -----------------------------------------------------
!||    detach_node_from_shells   ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_nloc          ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||    extend_nodal_arrays       ../common_source/modules/nodal_arrays.F90
!||    set_new_node_values       ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod          ../common_source/modules/connectivity.F90
!||    detach_node_nloc_mod      ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||    nlocal_reg_mod            ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod          ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine mirror_node_split(nodes, node_id, elements, shell_list, list_size, &
          nloc_dmg, nthread, ispmd, nspmd, owning_rank, mass_fraction, &
          m, row_uid, row_procne)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE connectivity_mod
          USE nodal_arrays_mod
          use nlocal_reg_mod
          use detach_node_nloc_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes        !< nodal arrays
          type(connectivity_), intent(inout) :: elements     !< element connectivity
          integer,             intent(in)    :: node_id      !< local id of the parent node
          integer,             intent(in)    :: list_size    !< number of local shells to reconnect
          integer,             intent(in)    :: shell_list(list_size) !< local shell ids (positive only)
          type(nlocal_str_),   intent(inout) :: nloc_dmg     !< non-local damage structure
          integer,             intent(in)    :: nthread      !< number of OpenMP threads
          integer,             intent(in)    :: ispmd        !< local MPI rank (0-based)
          integer,             intent(in)    :: nspmd        !< number of MPI ranks
          integer,             intent(in)    :: owning_rank  !< rank that owns the new node (0-based)
          real(kind=WP),       intent(in)    :: mass_fraction !< area-weighted share of the parent mass taken by the new node
          integer,             intent(in)    :: m             !< total FSKY rows for N' (canonical order, all ranks)
          integer,             intent(in)    :: row_uid(m)    !< shell user id of each canonical row (uid-sorted)
          integer,             intent(in)    :: row_procne(m) !< 1-based owning rank of each canonical row
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: numnod
          integer :: new_local_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          numnod = nodes%numnod
          new_local_id = numnod + 1

          call extend_nodal_arrays(nodes, numnod + 1)
          call set_new_node_values(nodes, node_id, mass_fraction)
          ! Override: the new node is owned by owning_rank, not by this rank
          nodes%MAIN_PROC(new_local_id) = owning_rank + 1
          ! Mirror nodes are ghost copies; mark as not owned so output routines skip them
          nodes%WEIGHT(new_local_id) = 0

          ! N''s skyline band is built in the SAME global-uid canonical order as on the
          ! owner: this rank's local migrating shells become LOCAL rows, every other
          ! contribution (the owner's and any other rank's) becomes a RECV row (row_procne).
          call detach_node_from_shells(nodes, node_id, elements, shell_list, list_size, &
            ispmd, m, row_uid, row_procne)

          if (nloc_dmg%imod > 0) then
            call detach_node_nloc(nloc_dmg, node_id, new_local_id, &
              elements, shell_list, list_size, numnod, nthread, ispmd, nspmd, &
              m, row_uid, row_procne, mass_fraction=mass_fraction)
          end if

          nodes%numnod = nodes%numnod + 1

        end subroutine mirror_node_split

        !\brief This subroutine detaches a node from a list of shells
!||====================================================================
!||    detach_node                   ../engine/source/engine/node_spliting/detach_node.F90
!||--- called by ------------------------------------------------------
!||    apply_crack                   ../engine/source/engine/node_spliting/apply_crack.F90
!||--- calls      -----------------------------------------------------
!||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_from_shells       ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_nloc              ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||    extend_nodal_arrays           ../common_source/modules/nodal_arrays.F90
!||    set_new_node_values           ../engine/source/engine/node_spliting/detach_node.F90
!||--- uses       -----------------------------------------------------
!||    connectivity_mod              ../common_source/modules/connectivity.F90
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    detach_node_nloc_mod          ../engine/source/engine/node_spliting/detach_node_nloc.F90
!||    interfaces_mod                ../common_source/modules/interfaces/interfaces_mod.F90
!||    nlocal_reg_mod                ../common_source/modules/nlocal_reg_mod.F
!||    nodal_arrays_mod              ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine detach_node(nodes, node_id, elements, shell_list, list_size, &
          npari, ninter, ipari, interf, nloc_dmg, nthread, nspmd, ispmd, &
          mass_fraction, m, row_uid, row_procne)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          USE constant_mod, only : TWO
          USE connectivity_mod
          USE nodal_arrays_mod
          USE interfaces_mod
          use nlocal_reg_mod
          use detach_node_nloc_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(nodal_arrays_), intent(inout) :: nodes        !< nodal arrays
          type(connectivity_), intent(inout) :: elements     !< connectivity of elements
          integer,             intent(in)    :: node_id      !< local id of the node to detach
          integer,             intent(in)    :: list_size    !< size of the shell list
          integer,             intent(in)    :: shell_list(list_size) !< local ids of shells to detach
          type(interfaces_),   intent(inout) :: interf       !< interface structure
          integer,             intent(in)    :: npari        !< number of interface parameters
          integer,             intent(in)    :: ninter       !< number of interfaces
          integer,             intent(inout) :: ipari(npari, ninter) !< interface parameters
          type(nlocal_str_),   intent(inout) :: nloc_dmg     !< non-local damage structure
          integer,             intent(in)    :: nthread      !< number of OpenMP threads
          integer,             intent(in)    :: nspmd        !< number of MPI domains
          integer,             intent(in)    :: ispmd        !< local MPI rank (0-based)
          real(kind=WP),       intent(in)    :: mass_fraction !< area-weighted share of the parent mass taken by the new node
          integer,             intent(in)    :: m             !< total FSKY rows for N' (canonical order, all ranks)
          integer,             intent(in)    :: row_uid(m)    !< shell user id of each canonical row (uid-sorted)
          integer,             intent(in)    :: row_procne(m) !< 1-based owning rank of each canonical row
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
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
!          if(nspmd == 0) write(6,*) 'error'
          old_uid = nodes%itab(node_id)
          numnod = nodes%numnod
          new_local_id = nodes%numnod + 1
          call detach_node_from_interfaces(nodes, node_id, npari, ninter, ipari, interf, &
            elements, shell_list, list_size)

          call extend_nodal_arrays(nodes, numnod + 1) ! allocates space for one more node (does not increment nodes%numnod)

          i = node_id
          call set_new_node_values(nodes, i, mass_fraction)

          ! Build N''s mechanical skyline band in the global-uid canonical order: this
          ! rank's local migrating shells become LOCAL rows, every other contribution a
          ! RECV row reserved for its owning rank (row_procne).  The band is identical on
          ! every rank holding the parent, so FR_NBCC is symmetric (no MPI truncation) and
          ! ASSPAR4 sums in a decomposition-independent order (bitwise /PARITH/ON).
          call detach_node_from_shells(nodes, node_id, elements, shell_list, list_size, &
            ispmd, m, row_uid, row_procne)

          ! Update the non-local damage skyline for N' in the SAME canonical order.
          if (nloc_dmg%imod > 0) then
            call detach_node_nloc(nloc_dmg, node_id, new_local_id, &
              elements, shell_list, list_size, numnod, nthread, ispmd, nspmd, &
              m, row_uid, row_procne, mass_fraction=mass_fraction)
          end if

          nodes%numnod = nodes%numnod + 1

        end subroutine detach_node

!       !\brief This subroutine detaches a node from a list of shells
        ! it is just a proof of concept to demonstrate how to detach a node from a list of shells
        ! the crack propagation is non phsyical (based on Jonhson-Cook damage)
        !! DEAD CODE
!!        subroutine test_jc_shell_detach(nodes, element, interf, npari, ninter, ipari, numnod, &
!!          numnodg, elbuf, ngroup, ngrouc, nparg, iparg, igrouc, numelc, ispmd, nspmd, &
!!          new_crack, nloc_dmg, nthread)
!!! ----------------------------------------------------------------------------------------------------------------------
!!!                                                   Modules
!!! ----------------------------------------------------------------------------------------------------------------------
!!          use spmd_mod
!!          use ghost_shells_mod
!!          use precision_mod, only : wp
!!          USE constant_mod, only : TWO, ONE, four_over_5
!!          USE connectivity_mod
!!          USE nodal_arrays_mod
!!          USE interfaces_mod
!!          USE elbufdef_mod
!!          use extend_array_mod
!!          use nlocal_reg_mod
!!          implicit none
!!! ----------------------------------------------------------------------------------------------------------------------
!!!                                                   Arguments
!!! ----------------------------------------------------------------------------------------------------------------------
!!          type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
!!          type(connectivity_), intent(inout) :: element !< connectivity of elements
!!          type(interfaces_), intent(inout) :: interf !< interf structure
!!          integer, intent(in) :: ngroup !< number of groups
!!          integer, intent(in) :: ngrouc !< number of shell groups
!!          integer, intent(in) :: nparg !< number of parameters per group
!!          integer, intent(in) :: iparg(nparg, ngroup) !< parameters of the groups
!!          integer, intent(in) :: numelc !< number of shell elements
!!          integer, intent(in) :: igrouc(ngrouc) !< group ids
!!          type(elbuf_struct_), intent(in) :: elbuf(ngroup)
!!          integer, intent(in) :: npari                   !< number of parameters
!!          integer, intent(in) :: ninter                  !< number of interf
!!          integer, intent(inout) :: ipari(npari,ninter)    !< parameters of the interf
!!          integer, intent(inout) :: numnod, numnodg
!!          integer, intent(in) :: ispmd !< rank of the processor (MPI)
!!          integer, intent(in) :: nspmd !< number of processors (MPI)
!!          integer, intent(out) :: new_crack !< flag to indicate if a new crack is created
!!          type(nlocal_str_), intent(inout) :: nloc_dmg  !< non-local damage structure
!!          integer,           intent(in)    :: nthread   !< number of OpenMP threads
!!! ----------------------------------------------------------------------------------------------------------------------
!!!                                                   Local variables
!!! ----------------------------------------------------------------------------------------------------------------------
!!          real(kind=wp), dimension(:), allocatable :: detach_shell
!!          integer :: ig,ng,numnod0,i,j,k,l,n,n1,n2,n3,n4,nel,nft,p
!!          integer, dimension(20) :: crack !< id of the noodes that are part of the crack
!!          integer, dimension(:), allocatable :: shell_list
!!          integer :: shells_to_detach
!!          double precision :: distance
!!          double precision, dimension(:), allocatable :: nodal_damage
!!          double precision :: v(3)
!!          double precision, parameter :: treshold = 1.75D0
!!          double precision :: dmax
!!          integer :: nGhostShells
!!          real(kind=wp), dimension(:), allocatable :: ghostShellDamage
!!          integer, dimension(:), allocatable :: detached_nodes, detached_nodes_local
!!          integer, dimension(:), allocatable :: nb_detached_nodes_global
!!          integer, dimension(:), allocatable :: nb_detached_nodes
!!          integer :: nb_detached_nodes_local
!!          logical, dimension(:), allocatable :: is_unique
!!          integer :: total_new_nodes
!!          integer :: displ(nspmd)
!!          integer :: old_max_uid
!!          integer :: numnodg0
!!          integer, dimension(:), allocatable :: permutation, processor, local_pos
!!          integer :: ii
!!! ----------------------------------------------------------------------------------------------------------------------
!!!                                                   body
!!! ----------------------------------------------------------------------------------------------------------------------
!!          new_crack = 0
!!
!!          numnodg0 = numnodg
!!          allocate(detach_shell(0:numelc))
!!          detach_shell = 0.0d0
!!          if(.not. allocated(element%shell%damage)) then
!!            allocate(element%shell%damage(1:numelc))
!!            element%shell%damage = 0.0d0
!!            allocate(element%shell%dist_to_center(1:numelc))
!!            do i = 1, numelc
!!              n1 = element%shell%ixc(2,i)
!!              n2 = element%shell%ixc(3,i)
!!              n3 = element%shell%ixc(4,i)
!!              n4 = element%shell%ixc(5,i)
!!              !barycenter
!!              v(1) = (nodes%x(1,n1) + nodes%x(1,n2) + nodes%x(1,n3) + nodes%x(1,n4))/4.0d0
!!              v(2) = (nodes%x(2,n1) + nodes%x(2,n2) + nodes%x(2,n3) + nodes%x(2,n4))/4.0d0
!!              v(3) = (nodes%x(3,n1) + nodes%x(3,n2) + nodes%x(3,n3) + nodes%x(3,n4))/4.0d0
!!              distance = 0.0d0
!!              do j = 1,4
!!                distance = max(distance, sqrt((v(1) - nodes%x(1,element%shell%ixc(j+1,i)))**2 + &
!!                  (v(2) - nodes%x(2,element%shell%ixc(j+1,i)))**2 + &
!!                  (v(3) - nodes%x(3,element%shell%ixc(j+1,i)))**2))
!!              end do
!!              element%shell%dist_to_center(i) = distance
!!            end do
!!          end if
!!
!!
!!          !! gather the damage of the shells in the detach_shell array
!!          do ig = 1, ngrouc
!!            ng = igrouc(ig)
!!            nel     = iparg(2,ng)
!!            nft     = iparg(3,ng)
!!            ! gather jc dfmax values
!!            !detach_shell(nft+1: nft+nel) =  elbuf(ng)%bufly(1)%fail(1,1,1)%floc(1)%dammx(1:nel)
!!            do k = 1, size(elbuf(ng)%bufly,1)
!!              do n1 = 1,size(elbuf(ng)%bufly(k)%fail,1)
!!                do n2 = 1,size(elbuf(ng)%bufly(k)%fail,2)
!!                  do n3 = 1,size(elbuf(ng)%bufly(k)%fail,3)
!!                    do l = 1,size(elbuf(ng)%bufly(k)%fail(n1,n2,n3)%floc,1)
!!                      do n = 1, size(elbuf(ng)%bufly(k)%fail(n1,n2,n3)%floc(l)%dammx,1)
!!                        detach_shell(nft+n) = max(detach_shell(nft+n), elbuf(ng)%bufly(k)%fail(n1,n2,n3)%floc(l)%dammx(n))
!!                      end do
!!                    end do
!!                  end do
!!                end do
!!              end do
!!            end do
!!            do n = 1, size(elbuf(ng)%GBUF%OFF)
!!              detach_shell(nft+n) = max(detach_shell(nft+n), ONE - abs(elbuf(ng)%GBUF%OFF(n)))
!!              if(elbuf(ng)%GBUF%OFF(n) < -99.0 ) then
!!                write(6,*) "detaching shell ", element%shell%user_id(n), " with OFF=", elbuf(ng)%GBUF%OFF(n)
!!                detach_shell(nft+n) = 0.98d0
!!                elbuf(ng)%GBUF%OFF(n) = one
!!              end if
!!!             if(element%shell%user_id(nft+n) == 100344279) then
!!!               write(6,*) "shell 100344279 detach value=",detach_shell(nft+n)," OFF=",elbuf(ng)%GBUF%OFF(n)
!!
!!!             endif
!!            end do
!!
!!            ! detach_shell(nft+1: nft+nel) = detach_shell(nft+1: nft+nel) - element%shell%damage(nft+1: nft+nel)
!!          end do
!!
!!
!!          ! Exchange the detach_shell values on ghost shells
!!          nghostshells = size(element%ghost_shell%nodes,2)
!!          allocate(ghostshelldamage(nghostshells))
!!          ghostshelldamage = 0.0d0
!!          call spmd_exchange_ghost_shells(element,ispmd,nspmd,1,detach_shell,ghostshelldamage)
!!
!!          numnod0 = numnod
!!          allocate(nodal_damage(numnod))
!!
!!          ! cumulate the damage of the shells on the nodes
!!          nodal_damage = 0.0d0
!!          do i = 1, numelc
!!            n1 = element%shell%ixc(2,i)
!!            n2 = element%shell%ixc(3,i)
!!            n3 = element%shell%ixc(4,i)
!!            n4 = element%shell%ixc(5,i)
!!            element%shell%damage(i) = detach_shell(i)
!!            if(detach_shell(i) > 0.9999d0) cycle ! already broken
!!            nodal_damage(n1) =max(nodal_damage(n1),element%shell%damage(i))
!!            nodal_damage(n2) =max(nodal_damage(n2),element%shell%damage(i))
!!            nodal_damage(n3) =max(nodal_damage(n3),element%shell%damage(i))
!!            nodal_damage(n4) =max(nodal_damage(n4),element%shell%damage(i))
!!          end do
!!
!!          ! add the damage of the ghost shells
!!          do i = 1, nghostshells
!!            element%ghost_shell%damage(i) = ghostshelldamage(i)
!!            if(ghostshelldamage(i) > 0.9999d0) cycle ! already broken
!!            do j = 1, 4
!!              n1 = element%ghost_shell%nodes(j,i)
!!              if(n1 <= 0) cycle
!!              nodal_damage(n1) =max(nodal_damage(n1),element%ghost_shell%damage(i))
!!            end do
!!          end do
!!
!!          deallocate(ghostshelldamage)
!!
!!          allocate(detached_nodes_local(numnod))
!!          nb_detached_nodes_local = 0
!!
!!          allocate(shell_list(numelc))
!!          shell_list = 0
!!          shells_to_detach = 0
!!
!!          ! detach nodes based on simple
!!          dmax = 0.0
!!          do ii = 1, numelc
!!            i =  element%shell%permutation(ii) ! the shells are treated in the order of their user_id, for reproducibility
!!            n1 = element%shell%ixc(2,i)
!!            n2 = element%shell%ixc(3,i)
!!            n3 = element%shell%ixc(4,i)
!!            n4 = element%shell%ixc(5,i)
!!            v(1) = (nodes%X(1,n1) + nodes%X(1,n2) + nodes%X(1,n3) + nodes%X(1,n4))/4.0D0
!!            v(2) = (nodes%X(2,n1) + nodes%X(2,n2) + nodes%X(2,n3) + nodes%X(2,n4))/4.0D0
!!            v(3) = (nodes%X(3,n1) + nodes%X(3,n2) + nodes%X(3,n3) + nodes%X(3,n4))/4.0D0
!!            !if(element%shell%user_id(ii) == 100344279 .and. detach_shell(i) >0 ) write(6,*) "detach_shell(i)=",detach_shell(i)
!!            distance = 0.0D0
!!            if(detach_shell(i) >= 0.99999d0) cycle ! deleted shell
!!            do j = 1,4
!!              distance = sqrt((v(1) - nodes%X(1,element%shell%ixc(j+1,i)))**2 + &
!!                (v(2) - nodes%X(2,element%shell%ixc(j+1,i)))**2 + &
!!                (v(3) - nodes%X(3,element%shell%ixc(j+1,i)))**2)
!!              if(nodes%nchilds(nodes%parent_node(element%shell%ixc(j+1,i))) < 1) then
!!                dmax = max(dmax,distance / element%shell%dist_to_center(i))
!!              end if
!!            enddo
!!
!!            do j = 1,4
!!              distance = sqrt((v(1) - nodes%X(1,element%shell%ixc(j+1,i)))**2 + &
!!                (v(2) - nodes%X(2,element%shell%ixc(j+1,i)))**2 + &
!!                (v(3) - nodes%X(3,element%shell%ixc(j+1,i)))**2)
!!!             if(nodes%nchilds(nodes%parent_node(element%shell%ixc(j+1,i))) < 1) then
!!!               dmax = max(dmax,distance / element%shell%dist_to_center(i))
!!!             end if
!!              !if(distance > treshold * element%shell%dist_to_center(i)) then
!!              if(distance == dmax) then
!!                if(nodes%nchilds(nodes%parent_node(element%shell%ixc(j+1,i))) > 3) cycle ! this node has not been splitted more than 3 times
!!                if(nodal_damage(element%shell%ixc(j+1,i)) < 1.0D-1 ) cycle
!!                crack(1) = element%shell%ixc(j+1,i)
!!                shell_list(1) = i
!!                shells_to_detach = 1
!!                element%shell%damage(i) = 1.0D0 !
!!                nb_detached_nodes_local = nb_detached_nodes_local + 1
!!                detached_nodes_local(nb_detached_nodes_local) = nodes%itab(crack(1))
!!                !write(6,*) "detach_node",node_id,nodes%itab(node_id),"from:",shell_list(1:list_size)
!!                write(6,*) "DETACH NODE",nodes%itab(crack(1)),"damage",nodal_damage(crack(1)), &
!!                  "ratio",distance / element%shell%dist_to_center(i)
!!
!!                call detach_node(nodes,crack(1),element,shell_list,shells_to_detach,npari,ninter, ipari, interf, nloc_dmg, nthread, nspmd, ispmd)
!!                numnod = numnod + 1
!!                if(ispmd == 0) numnodg = numnodg + 1
!!              end if
!!            end do
!!          end do
!!
!!          ! list nodes that are detached from the shells at this timestep
!!          allocate(nb_detached_nodes(nspmd))
!!          allocate(nb_detached_nodes_global(nspmd))
!!          nb_detached_nodes_global = 0
!!          nb_detached_nodes(1:nspmd) = 0
!!          nb_detached_nodes(ispmd+1) = numnod - numnod0
!!          ! call mpi_allreduce
!!          if(nspmd > 1) then
!!            call spmd_allreduce(nb_detached_nodes,nb_detached_nodes_global,nspmd,SPMD_SUM)
!!          else
!!            nb_detached_nodes_global = nb_detached_nodes
!!          end if
!!
!!          total_new_nodes = sum(nb_detached_nodes_global(1:nspmd))
!!          if(total_new_nodes > 0) new_crack = total_new_nodes
!!          !allocate(detached_nodes_local(nb_detached_nodes_global(ispmd+1)))
!!          allocate(detached_nodes(total_new_nodes))
!!          if(nb_detached_nodes_global(ispmd+1) /= nb_detached_nodes_local) then
!!          end if
!!
!!          ! reuse displ as displ
!!          displ(1:nspmd) = 0
!!          do i = 2, nspmd
!!            displ(i) = displ(i-1) + nb_detached_nodes_global(i-1)
!!          end do
!!
!!          if(nspmd > 1) then
!!            call spmd_allgatherv(detached_nodes_local,nb_detached_nodes_global(ispmd+1), &
!!              detached_nodes,nb_detached_nodes_global,displ)
!!          else
!!            detached_nodes = detached_nodes_local
!!          end if
!!
!!          !allreduce numnodg0
!!          if(nspmd > 1) then
!!            call spmd_allreduce(numnodg0,p,1,SPMD_MAX)
!!            numnodg0 = p
!!          end if
!!!          old_max_uid = nodes%max_uid
!!          if(nspmd > 1) then
!!            call spmd_allreduce(nodes%max_uid,old_max_uid,1,SPMD_MAX)
!!          else
!!            old_max_uid = nodes%max_uid
!!          end if
!!
!!          !write(6,*) "numnodg0",numnodg0
!!!         if(total_new_nodes >0) write(6,*) "MASS nb_detached_nodes_global",nb_detached_nodes_global(1:nspmd)
!!!         if(total_new_nodes >0) write(6,*) "MASS detached_nodes",detached_nodes(1:total_new_nodes)
!!          !Not finalized: new nodes may be boundary nodes (i.e. new node attached to two shells from different processors)
!!
!!          k = sum(nb_detached_nodes_global(1:nspmd))
!!          allocate(processor(k))
!!          allocate(local_pos(k))
!!          k = 0
!!          do P = 1, nspmd
!!            do i = 1, nb_detached_nodes_global(P)
!!              k = k + 1
!!              processor(k) = P
!!              if(ispmd+1 == P) then
!!                local_pos(k) = i
!!              else
!!                local_pos(k) = 0
!!              end if
!!            end do
!!          end do
!!
!!          k = sum(nb_detached_nodes_global(1:nspmd))
!!          allocate(permutation(k))
!!          do i = 1,k
!!            permutation(i) = i
!!          end do
!!          ! sort the detached nodes in ascending order of the user id of the parent node
!!          CALL STLSORT_INT_INT(k,detached_nodes,permutation)
!!
!!          !Loop over the detached nodes of all domains
!!          !the new nodes are created in the same order what the number of mpi domains is
!!          do ii = 1, k
!!            i = permutation(ii)
!!            P = processor(i)
!!            old_max_uid = old_max_uid + 1
!!            numnodg0 = numnodg0 + 1
!!            if( P == ispmd+1) then
!!              j = local_pos(i)
!!              nodes%itab(numnod0 + j) = old_max_uid
!!              nodes%itabm1(numnod0 + j) = old_max_uid
!!              nodes%itabm1(2*(numnod0 + j)) = numnod0 + j
!!              nodes%nodglob(numnod0 + j) = numnodg0
!!              !write(6,*) old_max_uid,"detached node ",nodes%itab(numnod0 + j),"form",nodes%itab(nodes%parent_node(numnod0+j))
!!            end if
!!            j = get_local_node_id(nodes,detached_nodes(i))
!!            if(j > 0) then
!!              ! the node is known by the current processor, so we need to update its mass
!!              nodes%MS(j) = nodes%MS(j) / TWO ! arbitrary division by 2
!!              nodes%MS0(j) = nodes%MS0(j) /TWO
!!              nodes%nchilds(nodes%parent_node(j)) = nodes%nchilds(nodes%parent_node(j)) + 1
!!            end if
!!          end do
!!
!!!         if(old_max_uid /= nodes%max_uid) then
!!!           write(6,*) "old_max_uid",old_max_uid,"nodes%max_uid",nodes%max_uid
!!!         endif
!!          nodes%max_uid = old_max_uid
!!          deallocate(permutation)
!!          deallocate(processor)
!!          deallocate(local_pos)
!!
!!          numnodg = numnodg0
!!
!!          if (allocated(is_unique)) deallocate(is_unique)
!!          if (allocated(nb_detached_nodes)) deallocate(nb_detached_nodes)
!!          if (allocated(detached_nodes_local)) deallocate(detached_nodes_local)
!!          if (allocated(nb_detached_nodes_global)) deallocate(nb_detached_nodes_global)
!!          if (allocated(detach_shell)) deallocate(detach_shell)
!!          if (allocated(shell_list)) deallocate(shell_list)
!!          if (allocated(nodal_damage)) deallocate(nodal_damage)
!!          ! if (allocated(ghostShellCoordinates)) deallocate(ghostShellCoordinates)
!!          ! if (allocated(shellCoordinates)) deallocate(shellCoordinates)
!!
!!        end subroutine test_jc_shell_detach



      end module detach_node_mod
