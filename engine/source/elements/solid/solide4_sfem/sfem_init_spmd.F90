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
!||    sfem_init_spmd_mod   ../engine/source/elements/solid/solide4_sfem/sfem_init_spmd.F90
!||--- called by ------------------------------------------------------
!||    sfem_init            ../engine/source/elements/solid/solide4_sfem/sfem_init.F90
!||====================================================================
      module sfem_init_spmd_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initiliation of the SFEM data structure, including the initialization of the SPMD communication data structure for SFEM
!! \details 
!||====================================================================
!||    sfem_init_spmd         ../engine/source/elements/solid/solide4_sfem/sfem_init_spmd.F90
!||--- called by ------------------------------------------------------
!||    sfem_init              ../engine/source/elements/solid/solide4_sfem/sfem_init.F90
!||--- calls      -----------------------------------------------------
!||    alloc_int_1d_array     ../common_source/modules/array_mod.F
!||    dealloc_int_1d_array   ../common_source/modules/array_mod.F
!||    initbuf                ../engine/share/resol/initbuf.F
!||    myqsort_int            ../common_source/tools/sort/myqsort_int.F
!||    spmd_waitall           ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    array_mod              ../common_source/modules/array_mod.F
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    initbuf_mod            ../engine/share/resol/initbuf.F
!||    nodal_arrays_mod       ../common_source/modules/nodal_arrays.F90
!||    precision_mod          ../common_source/modules/precision_mod.F90
!||    sfem_mod               ../common_source/modules/elements/sfem_mod.F90
!||    spmd_mod               ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine sfem_init_spmd(lag_ale,numels,numnod,ngroup, &
                             nparg,ispmd,nspmd,tetra_fsky_dim,fsky_dim2, &
                             iparg,ixs,sfem,nodes,itag_nsfem,local_elm_nb,local_elm_list)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use element_mod , only : nixs
          use sfem_mod , only : sfem_
          use array_mod , only : array_type_int_1d,alloc_int_1d_array,dealloc_int_1d_array
          use nodal_arrays_mod , only : nodal_arrays_
          use initbuf_mod
          use spmd_mod , only : spmd_isend,spmd_irecv,spmd_waitall,spmd_barrier
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
          integer, intent(in) :: lag_ale !< 0 : initialization for lagrangian methoid, 1 for ale method
          integer, intent(in) :: numels !< local number of solid elements
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: ngroup !< number of element groups
          integer, intent(in) :: nparg !< number of parameters per element group
          integer, intent(in) :: ispmd !< mpi rank of the current task
          integer, intent(in) :: nspmd !< number of mpi tasks
          integer, intent(in) :: tetra_fsky_dim !w first dimension of local_elm_list array
          integer, intent(in) :: fsky_dim2 !< 2nd dimension of tetra_fsky array
          integer, dimension(numnod), intent(in) :: itag_nsfem !< 1 if the node has sfem option
          integer, dimension(2*numnod+1), intent(inout) :: local_elm_nb !< number of element connected to the node "i"
          integer, dimension(tetra_fsky_dim,3), intent(in) :: local_elm_list !< list of element connected to the node "i"
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< element group parameters
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid element data
          type(sfem_), intent(inout) :: sfem !< sfem data structure
          type(nodal_arrays_), intent(in) :: nodes !< nodes data structure

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: condition
          integer :: i,j,k,l
          integer :: nnod,icpre
          integer :: element_id
          integer :: mtn,llt,nft,iad,ity
          integer :: npt,jale,ismstr,jeul,jtur
          integer :: jthe,jlag,jmult,jhbe,jivf
          integer :: nvaux,jpor,jcvt,jclose,jplasol
          integer :: irep,iint,igtyp,israt,isrot
          integer :: icsen,isorth,isorthg,ifailure,jsms


          integer :: entity_size,entity_size_max,ierror,low_bound,up_bound
          integer :: my_address,s_size,r_size
          integer :: node_id,remote_elm_nb,next
          integer, dimension(6) :: node_id_list
          integer, dimension(nspmd) :: s_request,r_request
          integer, dimension(:), allocatable :: global_elm_nb
          integer, dimension(:), allocatable :: entity_list,perm
          integer, dimension(:,:), allocatable :: global_elm_list,entity_list_2


          integer, parameter :: tag_1 = 10001
          integer, parameter :: tag_2 = 10002          
          type(array_type_int_1d), dimension(:), allocatable :: s_buffer, r_buffer, s_buffer_2, r_buffer_2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          allocate(s_buffer(nspmd))
          allocate(r_buffer(nspmd))  
          allocate(s_buffer_2(nspmd))
          allocate(r_buffer_2(nspmd))
          allocate(global_elm_nb(2*numnod+1))
          global_elm_nb(numnod+1:2*numnod+1) = 0
          global_elm_nb(1:numnod) = local_elm_nb(1:numnod)

          ! -----------
          ! mpi exchange to get the number of elements connected to the nodes
          next = 0
          do i=1,nspmd
            if(i-1/=ispmd) then
              s_size = 0
              s_buffer(i)%size_int_array_1d = nodes%boundary_add(1,i+1) - nodes%boundary_add(1,i)
              r_buffer(i)%size_int_array_1d = nodes%boundary_add(1,i+1) - nodes%boundary_add(1,i)
              call alloc_int_1d_array(s_buffer(i))
              call alloc_int_1d_array(r_buffer(i))

              do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                node_id = nodes%boundary(j)
                s_size = s_size + 1
                s_buffer(i)%int_array_1d(s_size) = local_elm_nb(node_id)
              enddo
              next = next + 1
              call spmd_isend(s_buffer(i)%int_array_1d(:),s_buffer(i)%size_int_array_1d,i-1,tag_1,s_request(next))
              call spmd_irecv(r_buffer(i)%int_array_1d(:),r_buffer(i)%size_int_array_1d,i-1,tag_1,r_request(next))
            endif
          enddo
          ! -----------

          call spmd_waitall(next,s_request)
          call spmd_waitall(next,r_request)

          next = 0
          ! -----------
          ! allocation & exchange of the global element id list
          do i=1,nspmd
            if(i-1/=ispmd) then
              s_size = 0
              do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                node_id = nodes%boundary(j)
                s_size = s_size + local_elm_nb(node_id)
              enddo

              s_buffer_2(i)%size_int_array_1d = s_size
              call alloc_int_1d_array(s_buffer_2(i))
              s_size = 0
              do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                node_id = nodes%boundary(j)    
                do k=1,local_elm_nb(node_id)
                  my_address = k + local_elm_nb(numnod+node_id)
                  s_size = s_size + 1
                  s_buffer_2(i)%int_array_1d(s_size) = -local_elm_list(my_address,1)              
                enddo
              enddo

              r_size = 0
              do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                r_size = r_size + r_buffer(i)%int_array_1d(j-nodes%boundary_add(1,i)+1)
                global_elm_nb(nodes%boundary(j)) = global_elm_nb(nodes%boundary(j)) + &
                                                      r_buffer(i)%int_array_1d(j-nodes%boundary_add(1,i)+1) ! get the total number of elements connected to the current node by summing the number of elements received from other mpi tasks
              enddo

              r_buffer_2(i)%size_int_array_1d = r_size
              call alloc_int_1d_array(r_buffer_2(i))
              next = next + 1
              call spmd_isend(s_buffer_2(i)%int_array_1d,s_size,i-1,tag_2,s_request(next))
              call spmd_irecv(r_buffer_2(i)%int_array_1d,r_size,i-1,tag_2,r_request(next))               
            endif
          enddo
          ! -----------
          
          call spmd_waitall(next,s_request)
          call spmd_waitall(next,r_request)

          ! -----------
          ! get the total number of contributions
          sfem%tetra_fsky_dim1 = 0
          do i=1,numnod
            sfem%tetra_fsky_dim1 = sfem%tetra_fsky_dim1 + global_elm_nb(i) ! get the total number of contributions
          enddo
          sfem%tetra_fsky_dim2 = fsky_dim2
          global_elm_nb(numnod+1) = 1
          do i=2,numnod+1
            global_elm_nb(numnod+i) = global_elm_nb(numnod+i-1) + global_elm_nb(i-1) ! get the number of contributions of the node i
          enddo
          ! -----------

          allocate(sfem%tetra_fsky(sfem%tetra_fsky_dim1,sfem%tetra_fsky_dim2))
          sfem%tetra_fsky(1:sfem%tetra_fsky_dim1,1:sfem%tetra_fsky_dim2) = 0.0_WP 

          allocate(global_elm_list(sfem%tetra_fsky_dim1,4))
          global_elm_list(1:sfem%tetra_fsky_dim1,1:4) = -1000000

          ! -----------
          ! save the local data         
          do i=1,numnod
            do k=1,local_elm_nb(i)
              my_address = k + global_elm_nb(numnod+i) - 1
              global_elm_list(my_address,1) = local_elm_list(k+local_elm_nb(numnod+i),1) ! internal global element id
              global_elm_list(my_address,2) = local_elm_list(k+local_elm_nb(numnod+i),2) ! local element id
              global_elm_list(my_address,3) = local_elm_list(k+local_elm_nb(numnod+i),3) ! local node number in the element
              global_elm_list(my_address,4) = ispmd ! processor id
            enddo
          enddo
          ! -----------
                  
          ! -----------
          ! exchange of data : element id, processor id
          do i=1,nspmd
            if(i-1/=ispmd) then
              next = 0 
              do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                node_id = nodes%boundary(j)
                remote_elm_nb = r_buffer(i)%int_array_1d(j-nodes%boundary_add(1,i)+1)
                low_bound = 1
                up_bound = remote_elm_nb                           
                do k=low_bound,up_bound
                  my_address = k + local_elm_nb(node_id) + global_elm_nb(numnod+node_id) - 1
                  next = next + 1                                
                  global_elm_list(my_address,1) = r_buffer_2(i)%int_array_1d(next) ! internal global element id (remote element)
                  global_elm_list(my_address,2) = -1 ! for remote elements, we don't need the local element id
                  global_elm_list(my_address,3) = -1 ! for remote elements, we don't need the local node number in the element
                  global_elm_list(my_address,4) = i-1 ! processor id                  
                enddo
                local_elm_nb(node_id) = local_elm_nb(node_id) + remote_elm_nb ! update the number of elements connected to the current node by adding the number of elements received from other mpi tasks
              enddo
            endif
          enddo
          ! -----------

          do i=1,nspmd
            if(i-1/=ispmd) then
              call dealloc_int_1d_array(s_buffer(i))
              call dealloc_int_1d_array(r_buffer(i))
              call dealloc_int_1d_array(s_buffer_2(i))
              call dealloc_int_1d_array(r_buffer_2(i))
            endif
          enddo
          deallocate(s_buffer)
          deallocate(r_buffer)
          deallocate(s_buffer_2)
          deallocate(r_buffer_2)

          ! -----------  
          ! sort the element list for each node based on the global element id
          entity_size_max = 0
          do i=1,numnod
            low_bound = global_elm_nb(numnod+i) ! address of the first element connected to the current node
            up_bound = global_elm_nb(numnod+i+1)-1  ! address of the last element connected to the current node
            entity_size = up_bound - low_bound + 1 ! number of elements connected to the current node
            entity_size_max = max(entity_size_max,entity_size)
          enddo          
          allocate(entity_list(entity_size_max))
          allocate(entity_list_2(entity_size_max,4))
          allocate(perm(entity_size_max))
          ! loop over the node
          do i=1,numnod
            low_bound = global_elm_nb(numnod+i) ! address of the first element connected to the current node
            up_bound = global_elm_nb(numnod+i+1)-1  ! address of the last element connected to the current node
            entity_size = up_bound - low_bound + 1 ! number of elements connected to the current node
            do j=1,entity_size ! loop over the elements connected to the current node
              entity_list(j) = abs(global_elm_list(low_bound+j-1,1)) ! key : sort the elements based on their global id (the absolute value of the global element id is used as key since for remote elements, the global element id is stored as negative value)          
              entity_list_2(j,1) = global_elm_list(low_bound+j-1,1) ! save the element data
              entity_list_2(j,2) = global_elm_list(low_bound+j-1,2)
              entity_list_2(j,3) = global_elm_list(low_bound+j-1,3)     
              entity_list_2(j,4) = global_elm_list(low_bound+j-1,4)                            
            enddo
            ierror = 0
            call myqsort_int(entity_size,entity_list,perm,ierror) ! sort the element keys       
            do j=1,entity_size ! loop over the sorted elements                  
              global_elm_list(low_bound+j-1,1) = entity_list_2(perm(j),1) ! reorder the element data based on the sorted order
              global_elm_list(low_bound+j-1,2) = entity_list_2(perm(j),2)
              global_elm_list(low_bound+j-1,3) = entity_list_2(perm(j),3)
              global_elm_list(low_bound+j-1,4) = entity_list_2(perm(j),4)              
            enddo
          enddo          

          deallocate(entity_list)
          deallocate(entity_list_2)
          deallocate(perm)
          ! -----------
          
          ! -----------
          ! get the adress of the contribution of each element connected to the current node
          allocate(sfem%tetra4_iad(8,numels))
          allocate(sfem%need_it(numels))
          sfem%tetra4_iad(1:8,1:numels) = 0
          sfem%need_it(1:numels) = 0
          do i=1,ngroup
            nnod = iparg(28,i) ! get the number of node per element for the current group
            icpre = iparg(10,i) ! get the value of ICPRE for the current group
            if(lag_ale==0) then
              if (nnod /= 4 .and. nnod /= 10) cycle ! only tetrahedral elements are supported
            else
              if (nnod /= 4) cycle ! only tetrahedral elements are supported
            endif
            call initbuf(iparg,i, &
                         mtn,llt,nft,iad,ity, &
                         npt,jale,ismstr,jeul,jtur, &
                         jthe,jlag,jmult,jhbe,jivf, &
                         nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtyp,israt,isrot, &
                         icsen,isorth,isorthg,ifailure,jsms)
            if(lag_ale==0) then
              condition = (nnod==4.and.isrot==3).or.(icpre/=0.and.(nnod==10.or.(nnod==4.and.isrot==1)))
              if(jeul==1) condition = .false.
              if(jale==1) condition = .false.
            else
              condition = (nnod==4.and.isrot==3)
              if(jeul==1) condition = .false.
              if(jlag==1) condition = .false.           
            endif
            if(.not.condition) cycle ! for 4-node elements, only ISROT=3 is supported, and for both 4-node and 10-node elements, only ICPRE=1 is supported                         

            do j=1,llt
              element_id = ixs(11,j+nft)
              node_id_list(1) = ixs(2,j+nft)
              node_id_list(2) = ixs(4,j+nft)
              node_id_list(3) = ixs(7,j+nft)
              node_id_list(4) = ixs(6,j+nft)
              condition = .true.
              do l=1,4
                if(itag_nsfem(node_id_list(l))==0) condition = .false.
              enddo
              if(condition) sfem%need_it(nft+j) = 1 ! sfem is active
              do l=1,4
                node_id = node_id_list(l)
                low_bound = global_elm_nb(numnod+node_id) ! address of the first element connected to the current node
                up_bound = global_elm_nb(numnod+node_id+1) - 1 ! address of the last element connected to the current node
                entity_size = up_bound - low_bound + 1 ! number of elements connected to the current node
                condition = .true.
                do k=1,entity_size ! loop over the elements connected to the current node
                  if(element_id==global_elm_list(low_bound+k-1,1).and.global_elm_list(low_bound+k-1,3)>0) then ! find the current element in the list of elements connected to the current node
                    sfem%tetra4_iad(global_elm_list(low_bound+k-1,3),global_elm_list(low_bound+k-1,2)) = low_bound+k-1 ! store the fsky address
                    condition = .false.
                    exit
                  endif
                enddo
              enddo
            enddo

            if(lag_ale==0) then
              if(nnod==4.and.isrot==3) then
                if(ismstr==1.or.ismstr==2.or.ismstr==11.or.ismstr==12) then ! small strain formulation contribution
                  do j=1,llt
                    element_id = ixs(11,j+nft)
                    node_id_list(1) = ixs(2,j+nft)
                    node_id_list(2) = ixs(4,j+nft)
                    node_id_list(3) = ixs(7,j+nft)
                    node_id_list(4) = ixs(6,j+nft)
                    do l=1,4
                      node_id = node_id_list(l)
                      low_bound = global_elm_nb(numnod+node_id) ! address of the first element connected to the current node
                      up_bound = global_elm_nb(numnod+node_id+1) - 1 ! address of the last element connected to the current node
                      entity_size = up_bound - low_bound + 1 ! number of elements connected to the current node
                                      condition = .true.
                      do k=1,entity_size ! loop over the elements connected to the current node
                        if(element_id==global_elm_list(low_bound+k-1,1).and.global_elm_list(low_bound+k-1,3)<0) then ! find the current element in the list of elements connected to the current node
                          sfem%tetra4_iad(4+abs(global_elm_list(low_bound+k-1,3)),global_elm_list(low_bound+k-1,2)) = low_bound+k-1 ! store the fsky address
                          condition = .false.
                          exit
                        endif
                      enddo
                    enddo                   
                  enddo                
                endif
              endif
            endif
          end do
          ! -----------

          ! -----------
          ! get the mpi communication data structure for SFEM          
          allocate(sfem%spmd(nspmd))
          do i=1,nspmd
            if(i-1/=ispmd) then
              s_size = 0
              r_size = 0
              do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                node_id = nodes%boundary(j)
                low_bound = global_elm_nb(numnod+node_id) ! address of the first element connected to the current node
                up_bound = global_elm_nb(numnod+node_id+1) - 1 ! address of the last element connected to the current node
                entity_size = up_bound - low_bound + 1 ! number of elements connected to the current node
                do k=1,entity_size ! loop over the elements connected to the current node
                  if(global_elm_list(low_bound+k-1,1)>0) then
                    s_size = s_size + 1
                  elseif(global_elm_list(low_bound+k-1,4)==i-1) then
                    r_size = r_size + 1
                  endif
                enddo                
              enddo
              sfem%spmd(i)%s_size = s_size
              sfem%spmd(i)%r_size = r_size

              allocate(sfem%spmd(i)%send_iad(s_size))
              allocate(sfem%spmd(i)%rcv_iad(r_size))

              s_size = 0
              r_size = 0
              do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                node_id = nodes%boundary(j)
                low_bound = global_elm_nb(numnod+node_id) ! address of the first element connected to the current node
                up_bound = global_elm_nb(numnod+node_id+1) - 1 ! address of the last element connected to the current node
                entity_size = up_bound - low_bound + 1 ! number of elements connected to the current node
                do k=1,entity_size ! loop over the elements connected to the current node
                  if(global_elm_list(low_bound+k-1,1)>0) then
                    s_size = s_size + 1
                    sfem%spmd(i)%send_iad(s_size) = low_bound+k-1 ! store the fsky address to send
                  elseif(global_elm_list(low_bound+k-1,4)==i-1) then
                    r_size = r_size + 1
                    sfem%spmd(i)%rcv_iad(r_size) = low_bound+k-1 ! store the fsky address to receive
                  endif
                enddo                
              enddo              
            endif
          enddo
          ! -----------          
          
          ! -----------
          ! get the list of nodes connected to tetra element with SFEM option and 
          ! the number of elements connected to each of these nodes
          next = 0
          do i=1,numnod
            if(global_elm_nb(numnod+i+1)-global_elm_nb(numnod+i)>0) then
              next = next + 1 ! count the number of nodes connected to tetra element with SFEM option
            endif
          enddo
          sfem%node_nb = next ! save the number of nodes connected to tetra element with SFEM option
          allocate(sfem%node_list(sfem%node_nb)) ! list of nodes connected to tetra element with SFEM option

          allocate(sfem%node_iad(sfem%node_nb+1)) ! number of elements connected to each node in sfem%node_list
          sfem%node_list(1:sfem%node_nb) = -1
          sfem%node_iad(1:sfem%node_nb+1) = -1
          next = 0
          do i=1,numnod
            if(global_elm_nb(numnod+i+1)-global_elm_nb(numnod+i)>0) then
              next = next + 1
              sfem%node_list(next) = i ! save the node id
              sfem%node_iad(next) = global_elm_nb(numnod+i) ! save the adress of the first contribution of a node
            endif
          enddo
          next = next + 1
          sfem%node_iad(next) = global_elm_nb(2*numnod+1) ! save the adress of the first contribution of a node         
          ! ----------- 

        end subroutine sfem_init_spmd
      end module sfem_init_spmd_mod
