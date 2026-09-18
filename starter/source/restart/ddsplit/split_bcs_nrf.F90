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
!||    split_bcs_nrf_mod   ../starter/source/restart/ddsplit/split_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    lectur              ../starter/source/starter/lectur.F
!||====================================================================
      module split_bcs_nrf_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Data structure must be updated after domain decomposition
!! \details  after domain decomposition the global data scruture must be split to keep relevant local data on each
!! domain
!
!||====================================================================
!||    split_bcs_nrf   ../starter/source/restart/ddsplit/split_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    lectur          ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine split_bcs_nrf(bcs_per_proc, cep, scep, nspmd,numnod,n2d)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs, bcs_struct_
          use array_mod
          use MY_ALLOC_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: scep !< size for array definition
          integer,intent(in) :: nspmd !< number of domains
          integer, intent(in) :: numnod !< number of nodes in the model
          integer, intent(in) :: n2d !< 1 if 2D, 0 if 3D
          integer,intent(in) :: cep(scep)
          !< indicator function for local elements [1:numel] -> [1,2,..,nspmd]
          type(bcs_struct_),dimension(nspmd),intent(inout) :: bcs_per_proc
          !< data structure to be filled with relevant local data only (on each domain)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: need_it
          integer :: ii,jj,p,j,ijk,pp,i
          integer :: proc_index(nspmd)     ! index for working array
          integer :: size_on_proc(nspmd)   ! total number of entity on each domain
          integer :: max_node,node_id,l_node_id
          integer :: my_address,my_offset,my_size,proc_id
          integer :: count_,my_new_size,my_index
          integer :: r_next,s_next,contribution_nb_max
          integer, dimension(nspmd) :: proc_list,node_per_proc,local_contribution_nb
          integer, dimension(nspmd) :: global_size_on_proc
          integer, dimension(:), allocatable :: nodal_contribution,tmp_array,tmp_array_2
          integer, dimension(:), allocatable :: proc_contribution,local_node_index
          integer, dimension(:), allocatable :: r_proc_list,s_node_list
          integer, dimension(:,:), allocatable :: tmp_array_3,elm_2_nodal_contribution
          type(array_type_int_1d), dimension(:), allocatable :: p_local_node_index
          type(array_type_int_2d), dimension(:), allocatable :: local_node_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
          bcs_per_proc(1:nspmd)%num_nrf = bcs%num_nrf
          if(bcs%num_nrf == 0)return !nothing to allocate and nothing to initialize
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(nodal_contribution(numnod+1),tmp_array(numnod+1))
          nodal_contribution(1:numnod+1) = 0
          tmp_array(1:numnod+1) = 0
          do p=1,nspmd
            allocate(bcs_per_proc(p)%nrf(bcs%num_nrf))
          end do

          ! --- filling global parameters for bcs nrf data structure on each domain
          do p=1,nspmd
            do ii=1,bcs%num_nrf
              bcs_per_proc(p)%nrf(ii)%user_id = bcs%nrf(ii)%user_id
              bcs_per_proc(p)%nrf(ii)%set_id = bcs%nrf(ii)%set_id
            end do
          end do

          ! --------------
          ! Count the number of contribution for each node
          do ii=1,bcs%num_nrf
            do jj=1,bcs%nrf(ii)%list%size
              if(n2d==0) then
                max_node = 4
                if(bcs%nrf(ii)%list%node_list(3,jj)==bcs%nrf(ii)%list%node_list(4,jj)) max_node = 3
              else
                max_node = 2
              end if
              do j=1,max_node
                node_id = bcs%nrf(ii)%list%node_list(j,jj) ! get the node id 
                nodal_contribution(node_id+1) = nodal_contribution(node_id+1) + 1 ! count the number of contribution for this node
              end do
            end do
          enddo
          ! --------------

          ! --------------
          ! Compute the address of the contribution for each node
          ! n1:c1 contribution, n2:c2 contribution, n3:c3 ...
          !   n1         n2        ...
          ! 1   c1 c1+1   c2+c1
          ! address of the first contribution for node ni: nodal_contribution(ni)
          ! number of contribution for node ni: nodal_contribution(ni+1) - nodal_contribution(ni)
          contribution_nb_max = 0
          nodal_contribution(1) = 1
          do j=2,numnod+1
            contribution_nb_max = max(contribution_nb_max,nodal_contribution(j))
            nodal_contribution(j) = nodal_contribution(j) + nodal_contribution(j-1)
          end do
          ! --------------

          ! --------------
          ! loop over the /BCS/NRF list
          !   loop over the facet of each BCS/NRF
          !     get the contribution's processor
          !     get the mapping between the facet and the local contribution of the node (local contribution: 1--> nb of contribution for the node "n")
          allocate(proc_contribution(nodal_contribution(numnod+1)-1))
          my_size = 0
          do ii=1,bcs%num_nrf
            my_size = my_size + bcs%nrf(ii)%list%size
          enddo
          allocate(elm_2_nodal_contribution(4,my_size))
          proc_contribution(:) = 0
          my_offset = 0
          do ii=1,bcs%num_nrf
            do jj=1,bcs%nrf(ii)%list%size
              p = 1 + cep(bcs%nrf(ii)%list%elem(jj)) ! get the processor id
              if(n2d==0) then
                max_node = 4
                if(bcs%nrf(ii)%list%node_list(3,jj)==bcs%nrf(ii)%list%node_list(4,jj)) max_node = 3
              else
                max_node = 2
              end if
              do j=1,max_node
                node_id = bcs%nrf(ii)%list%node_list(j,jj) ! get the node id
                tmp_array(node_id) = tmp_array(node_id) + 1
                elm_2_nodal_contribution(j,jj+my_offset) = tmp_array(node_id) ! save the local contribution id (1--> nb of contribution for the node "n")
                my_address = tmp_array(node_id) + nodal_contribution(node_id) - 1
                proc_contribution(my_address) = p ! save the processor for this contribution
              end do
            end do
            my_offset = my_offset + bcs%nrf(ii)%list%size
          enddo
          ! --------------

          ! --------------
          ! To save memory, build a mapping between the global node id and a local BCS/NRF node id
          allocate(local_node_index(numnod))
          allocate(bcs%nrf_node_ids(numnod))
          local_node_index(1:numnod) = 0
          bcs%nrf_node_ids(1:numnod) = 0
          local_contribution_nb(1:nspmd) = 0
          count_ = 0
          do ii=1,bcs%num_nrf
            do jj=1,bcs%nrf(ii)%list%size
              if(n2d==0) then
                max_node = 4
                if(bcs%nrf(ii)%list%node_list(3,jj)==bcs%nrf(ii)%list%node_list(4,jj)) max_node = 3
              else
                max_node = 2
              end if
              do j=1,max_node
                node_id = bcs%nrf(ii)%list%node_list(j,jj) ! get the node id
                if(local_node_index(node_id)==0) then
                  count_ = count_ + 1
                  local_node_index(node_id) = count_ ! mapping between the global node id and the local BCS/NRF node id
                  bcs%nrf_node_ids(count_) = node_id ! save the global node id for this local BCS/NRF node id
                end if
              end do
            end do
          enddo
          bcs%nrf_num_nodes = count_ ! save the number of unique BCS/NRF nodes
          bcs%cfl_nrf%s_nod_iadsky = count_ ! save the size of the %nod_iadsky
          allocate(bcs%cfl_nrf%nod_iadsky(count_))
          do i=1,count_
            node_id = bcs%nrf_node_ids(i)
            bcs%cfl_nrf%nod_iadsky(i) = nodal_contribution(node_id+1) - nodal_contribution(node_id)
          end do

          allocate(p_local_node_index(nspmd))
          do p=1,nspmd
            p_local_node_index(p)%size_int_array_1d = count_+1
            call alloc_1d_array(p_local_node_index(p))
          enddo
          do p=1,nspmd
            p_local_node_index(p)%int_array_1d(1:count_+1) = 0
            allocate(bcs_per_proc(p)%nrf_node_ids(count_))
          end do
          proc_index(1:nspmd) = 0
          do ii=1,bcs%num_nrf
            do jj=1,bcs%nrf(ii)%list%size
               p = 1 + cep(bcs%nrf(ii)%list%elem(jj)) ! get the processor id
              if(n2d==0) then
                max_node = 4
                if(bcs%nrf(ii)%list%node_list(3,jj)==bcs%nrf(ii)%list%node_list(4,jj)) max_node = 3
              else
                max_node = 2
              end if
              do j=1,max_node
                node_id = bcs%nrf(ii)%list%node_list(j,jj) ! get the node id
                my_address = local_node_index(node_id) ! get the local BCS/NRF node id
                if(p_local_node_index(p)%int_array_1d(my_address)==0) then                  
                  proc_index(p) = proc_index(p) + 1
                  p_local_node_index(p)%int_array_1d(my_address) = proc_index(p) ! save the local node id (local to the proc)
                  bcs_per_proc(p)%nrf_node_ids(proc_index(p)) = node_id ! save the global node id for this local BCS/NRF node id
                end if
              end do
            end do
          enddo

          do p=1,nspmd
            allocate(bcs_per_proc(p)%cfl_nrf%nod_iadsky(proc_index(p)+1))
            bcs_per_proc(p)%cfl_nrf%nod_iadsky(1) = 1
            do i=1,proc_index(p)
              node_id = bcs_per_proc(p)%nrf_node_ids(i)
              bcs_per_proc(p)%cfl_nrf%nod_iadsky(i+1) = nodal_contribution(node_id+1) - nodal_contribution(node_id)
            end do
            do i=1,proc_index(p)
              bcs_per_proc(p)%cfl_nrf%nod_iadsky(i+1) = bcs_per_proc(p)%cfl_nrf%nod_iadsky(i+1) +& 
                                                        bcs_per_proc(p)%cfl_nrf%nod_iadsky(i)
            end do
          bcs_per_proc(p)%nrf_num_nodes = proc_index(p) ! save the number of unique BCS/NRF nodes
          bcs_per_proc(p)%cfl_nrf%s_nod_iadsky = proc_index(p)+1 ! save the size of the %nod_iadsky
          enddo
          ! --------------

          ! --------------
          ! Allocate the local_node_id data structure on each domain
          allocate(local_node_id(max(1,nspmd)))
          do p=1,nspmd
            local_node_id(p)%size_int_array_2d(1) = count_+1
            local_node_id(p)%size_int_array_2d(2) = 2
            call alloc_2d_array(local_node_id(p))
            local_node_id(p)%int_array_2d(1:count_+1,1:2) = 0
          end do
          ! --------------

          ! --------------
          ! Loop over the /BCS/NRF list to fill the local_node_id data structure on each domain
          ! --> address of the first contribution of a node
          ! --> mapping between the local BCS/NRF node id and the local processor node id
          ! mapping:    global node id --> local BCS/NRF node id --> local processor node id
          node_per_proc(1:nspmd) = 0
          do ii=1,bcs%num_nrf
            do jj=1,bcs%nrf(ii)%list%size
              p = 1 + cep(bcs%nrf(ii)%list%elem(jj)) ! get the processor id
              if(n2d==0) then
                max_node = 4
                if(bcs%nrf(ii)%list%node_list(3,jj)==bcs%nrf(ii)%list%node_list(4,jj)) max_node = 3
              else
                max_node = 2
              end if
              do j=1,max_node
                node_id = bcs%nrf(ii)%list%node_list(j,jj) ! get the node id
                l_node_id = local_node_index(node_id) ! get the local BCS/NRF node id
                if(local_node_id(p)%int_array_2d(l_node_id,1)==0) then
                  local_contribution_nb(p) = local_contribution_nb(p) + nodal_contribution(node_id+1) - nodal_contribution(node_id) ! update the contribution number for this domain                  
                  node_per_proc(p) = node_per_proc(p) + 1
                  local_node_id(p)%int_array_2d(l_node_id,1) = node_per_proc(p) ! save the local node id (local to the proc)
                  local_node_id(p)%int_array_2d(node_per_proc(p),2) = node_id ! save the global node id for this local BCS/NRF node id
                end if
              end do
            end do
          end do
          ! --------------

          ! --- filling list of elems : only relevant elems on each domain
          global_size_on_proc(1:nspmd) = 0
          do ii=1,bcs%num_nrf

            proc_index(1:nspmd) = 0
            size_on_proc(1:nspmd) = 0

            !---numbering
            do jj=1,bcs%nrf(ii)%list%size
              p = 1 + cep( bcs%nrf(ii)%list%elem(jj) )
              size_on_proc(p) = size_on_proc(p) + 1
            end do

            do p=1,nspmd
              global_size_on_proc(p) = global_size_on_proc(p) + size_on_proc(p)
            enddo
            allocate(bcs%nrf(ii)%list%global_2_local(bcs%nrf(ii)%list%size))
            !---allcoation of local data structure (on each domain)
            do p=1,nspmd
              bcs_per_proc(p)%nrf(ii)%list%size = size_on_proc(p)
              call my_alloc(bcs_per_proc(p)%nrf(ii)%list%elem, size_on_proc(p), "bcs_per_proc(p)%nrf(ii)%list%elem")
              call my_alloc(bcs_per_proc(p)%nrf(ii)%list%face, size_on_proc(p), "bcs_per_proc(p)%nrf(ii)%list%face")
              call my_alloc(bcs_per_proc(p)%nrf(ii)%list%rCp, size_on_proc(p), "bcs_per_proc(p)%nrf(ii)%list%rCp")
              call my_alloc(bcs_per_proc(p)%nrf(ii)%list%rCs, size_on_proc(p), "bcs_per_proc(p)%nrf(ii)%list%rCs")
              call my_alloc(bcs_per_proc(p)%nrf(ii)%list%iadsky, 4, size_on_proc(p), &
              &"bcs_per_proc(p)%nrf(ii)%list%iadsky")
              call my_alloc(bcs_per_proc(p)%nrf(ii)%list%node_list,4,size_on_proc(p),"bcs_per_proc%nrf%node_list")
            end do
            !--filling local data structure
            do jj=1,bcs%nrf(ii)%list%size
              p = 1 + cep( bcs%nrf(ii)%list%elem(jj) )
              proc_index(p) = proc_index(p) + 1
              bcs_per_proc(p)%nrf(ii)%list%elem( proc_index(p) ) = bcs%nrf(ii)%list%elem(jj) ! global elem id
              bcs_per_proc(p)%nrf(ii)%list%face( proc_index(p) ) = bcs%nrf(ii)%list%face(jj)
              bcs_per_proc(p)%nrf(ii)%list%rCp( proc_index(p) ) = bcs%nrf(ii)%list%rCp(jj)
              bcs_per_proc(p)%nrf(ii)%list%rCs( proc_index(p) ) = bcs%nrf(ii)%list%rCs(jj)
              bcs_per_proc(p)%nrf(ii)%list%iadsky( 1:4,proc_index(p) ) = 0 ! default value, updated in w_pon
              bcs%nrf(ii)%list%global_2_local(jj) = proc_index(p) ! global to local index for the segment jj
              bcs_per_proc(p)%nrf(ii)%list%node_list(1:4, proc_index(p) ) = bcs%nrf(ii)%list%node_list(1:4,jj)
            end do

          end do


          ! --------------
          ! allocation of %iadsky data structure on each domain
          ! Compute the address of the contribution for each node AND for each domain
          ! n1:c1 contribution, n2:c2 contribution, n3:c3 ...
          !   n1         n2        ...
          ! 1   c1 c1+1   c2+c1
          ! address of the first contribution for node ni: nodal_contribution(ni)
          ! number of contribution for node ni: nodal_contribution(ni+1) - nodal_contribution(ni)          
          do p=1,nspmd
            bcs_per_proc(p)%cfl_nrf%s_iadsky = global_size_on_proc(p)
            allocate(bcs_per_proc(p)%cfl_nrf%iadsky(4,global_size_on_proc(p)))
            bcs_per_proc(p)%cfl_nrf%iadsky(1:4,1:global_size_on_proc(p)) = -1
            bcs_per_proc(p)%cfl_nrf%s_fsky = local_contribution_nb(p)          
          end do
          ! --------------

          ! --------------
          ! Loop over the /BCS/NRF list to initialize the address of each contribution into %iadsky
          proc_index(1:nspmd) = 0
          my_offset = 0
          do ii=1,bcs%num_nrf
            do jj=1,bcs%nrf(ii)%list%size
              p = 1 + cep(bcs%nrf(ii)%list%elem(jj)) ! get the processor id
              proc_index(p) = proc_index(p) + 1 ! get the local address on this domain "p"
              if(n2d==0) then
                max_node = 4
                if(bcs%nrf(ii)%list%node_list(3,jj)==bcs%nrf(ii)%list%node_list(4,jj)) max_node = 3
              else
                max_node = 2
              endif
              do j=1,max_node
                node_id = bcs%nrf(ii)%list%node_list(j,jj) ! get the node id
                l_node_id = local_node_index(node_id) ! convert the global node id to the local BCS/NRF node id
                my_address = local_node_id(p)%int_array_2d(l_node_id,1) ! get the local processor node id
                bcs_per_proc(p)%cfl_nrf%iadsky(j,proc_index(p)) = & 
                elm_2_nodal_contribution(j,jj+my_offset) +  &
                bcs_per_proc(p)%cfl_nrf%nod_iadsky(my_address) - 1 ! save the address of this contribution
              enddo
            enddo
            my_offset = my_offset + bcs%nrf(ii)%list%size
          enddo          
          deallocate(tmp_array)
          ! --------------

          ! --------------
          ! Loop over the local processor nodes to initialize the send_iadfsky and temporary rcv_iadfsky data structure on each domain
          ! send_iadfsky: list of local contribution to send to other domains for each node
          ! temporary rcv_iadfsky: list of local contribution to receive from other domains for each node
          allocate(r_proc_list(contribution_nb_max),s_node_list(contribution_nb_max))
          do p=1,nspmd
            allocate(bcs_per_proc(p)%cfl_nrf%ddm(nspmd))
            my_size = int(0.1*numnod/nspmd)+1
            do pp=1,nspmd
              bcs_per_proc(p)%cfl_nrf%ddm(pp)%s_cont_nb = 0
              bcs_per_proc(p)%cfl_nrf%ddm(pp)%r_cont_nb = 0
              if(node_per_proc(p)>0) then
                allocate(bcs_per_proc(p)%cfl_nrf%ddm(pp)%send_iadfsky(my_size))
              endif
              if(node_per_proc(pp)>0) then
                allocate(bcs_per_proc(p)%cfl_nrf%ddm(pp)%tmp_rcv_iadfsky(my_size,2))
              endif
            enddo
          enddo

          do p=1,nspmd
            do j=1,node_per_proc(p)
              node_id = local_node_id(p)%int_array_2d(j,2) ! get the global node id              
              my_address = nodal_contribution(node_id) ! get the address of the first contribution for this node
              my_size = nodal_contribution(node_id+1) - nodal_contribution(node_id) ! get the number of contribution for this node
              need_it = .false.
              r_next = 0
              s_next = 0
              do i=1,my_size
                if(proc_contribution(my_address+i-1)/=p) then ! check if the contribution is on another domain
                  r_next = r_next + 1
                  r_proc_list(r_next) = proc_contribution(my_address+i-1)
                  need_it = .true. ! send or receive data for this node on this domain
                else
                  s_next = s_next + 1
                  s_node_list(s_next) = i ! local contribution id for this node on this domain
                endif
              enddo              
              ! need to communicate for this node on this domain
              if(need_it) then
                do ijk=1,r_next
                  proc_id = r_proc_list(ijk)
                  proc_list(proc_id) = 0
                enddo
                ! S buffer : loop over R proc, s proc needs to send all the local contribution (saved into s_node_list array)
                do pp=1,r_next ! loop over the processors that need to receive data for this node
                  proc_id = r_proc_list(pp) ! get the processor id
                  if(proc_id==p) cycle
                  if(proc_list(proc_id)==0) then ! check if this proc_id has already been processed for this node
                    proc_list(proc_id) = 1 ! update the proc_list to avoid double counting for this node

                    ! check if the send_iadfsky and tmp_rcv_iadfsky arrays are large enough to store the new data
                    my_size = size(bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%send_iadfsky)
                    my_new_size = bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%s_cont_nb + s_next
                    if(my_new_size>my_size) then
                      my_new_size = int(1.05*my_new_size)
                      call my_alloc(tmp_array_2,my_new_size,"tmp_array_2")
                      tmp_array_2(1:my_size) = bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%send_iadfsky(1:my_size)
                      call my_move_alloc(tmp_array_2,bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%send_iadfsky, "send_iadfsky")
                    endif
                    ! check if the rcv_iadfsky and tmp_rcv_iadfsky arrays are large enough to store the new data
                    my_size = size(bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%tmp_rcv_iadfsky,dim=1)
                    my_new_size = bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%r_cont_nb + s_next
                    if(my_new_size>my_size) then
                      my_new_size = int(1.05*my_new_size)
                      call my_alloc(tmp_array_3,my_new_size,2,"tmp_array_3")
                      tmp_array_3(1:my_size,1:2) = bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%tmp_rcv_iadfsky(1:my_size,1:2)
                      call my_move_alloc(tmp_array_3,bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%tmp_rcv_iadfsky, "tmp_rcv_iadfsky")
                    endif

                    ! fill the send_iadfsky and tmp_rcv_iadfsky arrays
                    do ijk=1,s_next
                      bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%s_cont_nb = bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%s_cont_nb + 1 ! update the number of contribution to send for this node on this domain
                      my_index = bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%s_cont_nb
                      bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%send_iadfsky(my_index) = s_node_list(ijk) + & 
                                      bcs_per_proc(p)%cfl_nrf%nod_iadsky(j) - 1 ! save the address of the local contribution for this node on the sending domain
                      bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%r_cont_nb = bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%r_cont_nb + 1 ! update the number of contribution to receive for this node on this domain
                      my_index = bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%r_cont_nb
                      bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%tmp_rcv_iadfsky(my_index,1) = s_node_list(ijk) ! save the local contribution id for this node on the receiving domain
                      bcs_per_proc(proc_id)%cfl_nrf%ddm(p)%tmp_rcv_iadfsky(my_index,2) = node_id ! save the node id on the receiving domain
                    end do
                  endif
                enddo
              endif
            enddo
          enddo
          ! --------------

          ! --------------
          ! Loop over the local processor nodes to initialize the rcv_iadfsky data structure on each domain
          do p=1,nspmd
            do proc_id=1,nspmd
              if(proc_id/=p) then
                my_size = bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%r_cont_nb
                call my_alloc(bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%rcv_iadfsky,my_size, &
                              "bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%rcv_iadfsky")
                do ijk=1,bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%r_cont_nb
                  node_id = bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%tmp_rcv_iadfsky(ijk,2) ! get the global node id
                  l_node_id = local_node_index(node_id) ! convert the global node id to the local BCS/NRF node id
                  my_address = local_node_id(p)%int_array_2d(l_node_id,1) ! get the address to point to the good place in the local_node_id data structure
                  my_offset = bcs_per_proc(p)%cfl_nrf%nod_iadsky(my_address) - 1 ! get the address of the first contribution for this node on this domain
                  bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%rcv_iadfsky(ijk) = &
                         bcs_per_proc(p)%cfl_nrf%ddm(proc_id)%tmp_rcv_iadfsky(ijk,1) + my_offset ! save the address of the local contribution for this node on the receiving domain
                enddo
              endif
            enddo
          enddo
          ! --------------

          deallocate(elm_2_nodal_contribution)
          deallocate(r_proc_list,s_node_list)


! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine split_bcs_nrf
      end module split_bcs_nrf_mod
