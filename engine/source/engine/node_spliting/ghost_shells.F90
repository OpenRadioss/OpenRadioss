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

      module ghost_shells_mod
        use precision_mod, only: wp
        implicit none
        type :: spmd_buffer_type
          integer, dimension(:), allocatable :: sendbuf
          integer, dimension(:), allocatable :: recvbuf
          integer :: send_request
          integer:: recv_request
        end type spmd_buffer_type

        type :: spmd_real_buffer_type
          real(kind=wp), dimension(:), allocatable :: sendbuf
          real(kind=wp), dimension(:), allocatable :: recvbuf
          integer :: send_request
          integer:: recv_request
        end type spmd_real_buffer_type




        interface
          !call build_ghosts(element%shell,4*nb_shells,mask,ghosts)

          function build_ghosts(shells,nb_shells,mask,nspmd,numnodes) result(c) bind(C,name="cpp_build_ghosts")
            use iso_c_binding
            implicit none
            integer(c_int), intent(in), value :: nspmd
            integer(c_int), intent(in), value :: numnodes
            integer(c_int), intent(in), value :: nb_shells
            integer(c_int), intent(in) :: shells(4,nb_shells)
            integer(c_int), intent(in) :: mask(nspmd,numnodes)
            type(c_ptr) :: c
          end function build_ghosts

          subroutine destroy_ghosts(ghosts) bind(C,name="cpp_destroy_ghosts")
            use iso_c_binding
            implicit none
            type(c_ptr), value :: ghosts
          end subroutine destroy_ghosts

          function get_shells_list(ghosts,p,n) result(cpp_ptr) bind(C,name="cpp_get_shells_list")
            use iso_c_binding
            implicit none
            type(c_ptr), value, intent(in) :: ghosts
            type(c_ptr) :: cpp_ptr
            integer(c_int), value, intent(in) :: p
            integer(c_int), intent(inout) :: n
          end function get_shells_list

          function get_shell_list_size(ghosts,p) result(n) bind(C,name="cpp_get_shells_list_size")
            use iso_c_binding
            implicit none
            type(c_ptr), value, intent(in) :: ghosts
            integer(c_int), value, intent(in) :: p
            integer(c_int) :: n
          end function get_shell_list_size
          !void cpp_copy_shells_list(void *c, int pc, int *shells)
          subroutine copy_shells_list(c,pc,shells,n) bind(C,name="cpp_copy_shells_list")
            use iso_c_binding
            implicit none
            type(c_ptr), value, intent(in) :: c
            integer(c_int), value, intent(in) :: pc
            integer(c_int), value, intent(in) :: n
            integer(c_int), intent(inout) :: shells(n)
          end subroutine copy_shells_list


        end interface

      contains
        ! \brief initializes the ghost shells
        subroutine init_ghost_shells(nodes, element,ispmd,nspmd,iad_node,sfr_node,fr_node)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use iso_c_binding
          use nodal_arrays_mod
          use connectivity_mod
          use spmd_mod
          use umap_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
          integer, intent(in) :: ispmd !< rank of the current process
          integer, intent(in) :: nspmd !< number of processes in the current MPI communicator
          integer, intent(in) :: iad_node(2,nspmd+1) !< index in bondary nodes:   J=IAD_NODE(2,I),IAD_NODE(1,I+1)-1
          integer, intent(in) :: sfr_node  !< nb nodes in the boundary
          integer, intent(in) :: fr_node(sfr_node) !<                               node = fr_node(J)
          type(nodal_arrays_),intent(in) :: nodes !< nodal arrays
          type(connectivity_),intent(inout) :: element !< connectivity arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,p,n,offset
          integer :: ierr
          type(c_ptr) :: ghosts
          type(c_ptr) :: cpp_ptr
          integer(c_int), pointer :: shells_to_send(:)
          integer :: nb_shells !< number of shells
          integer :: numnodes !< number of nodes
          integer, dimension(:,:), allocatable :: mask!< mask for the nodes
          integer :: buffer_size_out(nspmd),buffer_size_in(nspmd)
          integer :: local_id
          type(spmd_buffer_type), dimension(nspmd) :: spmd_buffer
          integer :: TAG
          integer :: bs
          integer :: node_id
          integer, dimension(:), allocatable :: connected_ghosts_shells
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          TAG = 1000
          numnodes = nodes%numnod
          nb_shells = size(element%shell%ixc,2)
          allocate(mask(nspmd,numnodes))
          mask = 0

          do p = 1, nspmd
            do j = iad_node(1,p), iad_node(1,p+1)-1
              mask(p,fr_node(j)) = 1
            enddo
          enddo

          do p = 1, nspmd
            spmd_buffer(p)%recv_request = MPI_REQUEST_NULL
            spmd_buffer(p)%send_request = MPI_REQUEST_NULL
          enddo


          ! For each node with mask == 1, we identify the corresponding shells from 1 to nb_shells
          ghosts = build_ghosts(element%shell%nodes,nb_shells,mask,nspmd,numnodes)
          deallocate(mask)

          ! count the number of shell to be exchanged for each processor
          allocate(element%ghost_shell%shells_to_send(nspmd))
          buffer_size_out = 0
          buffer_size_in = 0
          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            n = get_shell_list_size(ghosts,p)
            buffer_size_out(p) = n
            allocate(spmd_buffer(p)%sendbuf(4*n))
            allocate(element%ghost_shell%shells_to_send(p)%index(n))
            if(n > 0) then
              ! copy the list of shells to be exchanged
              !element%ghost_shell%shells_to_send(p)%index(1:n) = shells_to_send(1:n)
              call copy_shells_list(ghosts,p,element%ghost_shell%shells_to_send(p)%index,n)
            endif
          enddo
!          call MPI_Alltoall(buffer_size_out,1,MPI_INTEGER,buffer_size_in,1,MPI_INTEGER,SPMD_COMM_WORLD,ierr)
          call spmd_alltoall(buffer_size_out,1,buffer_size_in,1)

          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            ! mpi Irecv to receive the data
            allocate(spmd_buffer(p)%recvbuf(4*buffer_size_in(p)))
            bs = buffer_size_in(p)*4
            if( bs > 0 ) then
              call spmd_irecv(spmd_buffer(p)%recvbuf,bs,p-1,TAG,spmd_buffer(p)%recv_request)
            endif

            ! mpi Isend to send the data
            !cpp_ptr =get_shells_list(ghosts,p,n)
            n = size(element%ghost_shell%shells_to_send(p)%index)
            if( n > 0 ) then
              !call c_f_pointer(cpp_ptr, shells_to_send,[n])
              do i = 1, n
                j = element%ghost_shell%shells_to_send(p)%index(i)
                spmd_buffer(p)%sendbuf(1+4*(i-1)) = nodes%itab(element%shell%nodes(1,j))
                spmd_buffer(p)%sendbuf(2+4*(i-1)) = nodes%itab(element%shell%nodes(2,j))
                spmd_buffer(p)%sendbuf(3+4*(i-1)) = nodes%itab(element%shell%nodes(3,j))
                spmd_buffer(p)%sendbuf(4+4*(i-1)) = nodes%itab(element%shell%nodes(4,j))
              enddo
              bs = 4*n
              call spmd_isend(spmd_buffer(p)%sendbuf,bs,p-1,TAG,spmd_buffer(p)%send_request)
            endif
          enddo

          ! allocate element%ghost_shell%nodes
          n = sum(buffer_size_in)
          allocate(element%ghost_shell%nodes(4,n))
          allocate(element%ghost_shell%offset(nspmd+1))
          element%ghost_shell%offset = 0
          allocate(element%ghost_shell%damage(n))
          element%ghost_shell%damage = 0

          ! Wait for all the sends to complete
          offset = 0
          do p = 1, nspmd
            element%ghost_shell%offset(p) = offset+1
            if(ispmd+1 == p) cycle ! skip the current process
            n = buffer_size_in(p)
            !write(6,*) "init receiving ",n," shells from process ",p,"offset=",offset+1
            if(n > 0) then
              call spmd_Wait(spmd_buffer(p)%recv_request)
              do i = 1, n
                element%ghost_shell%nodes(1,i+offset) = spmd_buffer(p)%recvbuf(1+4*(i-1))
                element%ghost_shell%nodes(2,i+offset) = spmd_buffer(p)%recvbuf(2+4*(i-1))
                element%ghost_shell%nodes(3,i+offset) = spmd_buffer(p)%recvbuf(3+4*(i-1))
                element%ghost_shell%nodes(4,i+offset) = spmd_buffer(p)%recvbuf(4+4*(i-1))
                ! convert back user id to local id
                do j = 1,4
                  local_id = get_local_node_id(nodes,element%ghost_shell%nodes(j,i+offset))
                  if (local_id > 0) then
                    element%ghost_shell%nodes(j,i+offset) = local_id
                  else
                    element%ghost_shell%nodes(j,i+offset) = -element%ghost_shell%nodes(j,i+offset) ! negative id for nodes unknown on this processor
                  endif
                enddo
              enddo
              offset = offset + n
            endif
          enddo
          element%ghost_shell%offset(nspmd+1) = offset+1

          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            n = buffer_size_out(p)
            if( n > 0 ) then
              call spmd_Wait(spmd_buffer(p)%send_request)
              deallocate(spmd_buffer(p)%sendbuf)
            endif
            if(buffer_size_in(p) > 0) then
              deallocate(spmd_buffer(p)%recvbuf)
            endif
          enddo

          call destroy_ghosts(ghosts)

          allocate(connected_ghosts_shells(numnodes))
          connected_ghosts_shells = 0
          do i = 1, size(element%ghost_shell%nodes,2)
            do j = 1, 4
              node_id = element%ghost_shell%nodes(j,i)
              ! if node id is > 0, and is not duplicated in nodes(1:4,i)
              if(node_id > 0) then
                ! node is local to the processor
                if(j > 1) then
                  if(any(element%ghost_shell%nodes(1:j-1,i) == node_id)) cycle
                endif
                connected_ghosts_shells(node_id) = connected_ghosts_shells(node_id) + 1
              endif
            enddo
          enddo

          allocate(element%ghost_shell%addcnel(numnodes+1))
          element%ghost_shell%addcnel(1) = 1
          do n = 2, numnodes+1
            element%ghost_shell%addcnel(n) = element%ghost_shell%addcnel(n-1) + connected_ghosts_shells(n-1)
            ! look for negative value in addcnel
          enddo

          ! allocate the ghost shell connectivity
          allocate(element%ghost_shell%cnel(element%ghost_shell%addcnel(numnodes+1) - 1))

          do i = 1, size(element%ghost_shell%nodes,2)
            do j = 1, 4
              node_id = element%ghost_shell%nodes(j,i)
              ! if node id is > 0, and is not duplicated in nodes(1:4,i)
              if(node_id > 0) then
                ! node is local to the processor
                if(j > 1) then
                  if(any(element%ghost_shell%nodes(1:j-1,i) == node_id)) cycle
                endif
                ! node is a local node
                element%ghost_shell%cnel(element%ghost_shell%addcnel(node_id)) = i
                element%ghost_shell%addcnel(node_id) = element%ghost_shell%addcnel(node_id) + 1
                ! look for negative value in addcnel
              endif
            enddo
          enddo

          ! flush and reset addcnel
          element%ghost_shell%addcnel(1) = 1
          element%ghost_shell%addcnel(2:numnodes+1) = 0
          do n = 2, numnodes+1
            element%ghost_shell%addcnel(n) = element%ghost_shell%addcnel(n) + connected_ghosts_shells(n-1)
          enddo

          deallocate(connected_ghosts_shells)
        end subroutine init_ghost_shells


        subroutine spmd_exchange_ghost_shells(element,ispmd,nspmd,chunkSize,sendbuf,recvbuf)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use iso_c_binding
          use nodal_arrays_mod
          use connectivity_mod
          use spmd_mod
          use umap_mod
          use precision_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
          integer, intent(in) :: ispmd !< rank of the current process
          integer, intent(in) :: nspmd !< number of processes in the current MPI communicator
          type(connectivity_), intent(inout) :: element !< connectivity arrays
          integer, intent(in) :: chunkSize !< size of the chunk to send
          real(kind=wp), dimension(:), intent(in) :: sendbuf !< buffer to send, size = chunkSize * numelc
          real(kind=wp), dimension(:), intent(out) :: recvbuf !< buffer to receive, size = chunkSize * number of ghost shells
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          type(spmd_real_buffer_type), dimension(nspmd) :: spmd_buffer
          integer :: ierr
          integer :: p
          integer :: i,j,n,ns,nr
          integer :: offset,recv_offset,send_offset
          integer, parameter :: TAG = 1000 !< tag for the MPI messages
!-----------------------------------------------------------------------------------------------------------------------
!                                                   Body
!-----------------------------------------------------------------------------------------------------------------------
          !write(6,*) "spmd_exchange_ghost_shells: process ",ispmd+1
          do p = 1, nspmd
            spmd_buffer(p)%recv_request = MPI_REQUEST_NULL
            spmd_buffer(p)%send_request = MPI_REQUEST_NULL
            if(ispmd+1 == p) cycle ! skip the current process
            n = size(element%ghost_shell%shells_to_send(p)%index)
            allocate(spmd_buffer(p)%sendbuf(chunkSize*n))
          enddo

          ! mpi Irecv to receive the data
          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            n = element%ghost_shell%offset(p+1) - element%ghost_shell%offset(p)
            !write(*,*) "spmd_exchange_ghost_shells: process ",ispmd+1," receiving ",n," shells from process ",p

            if(n > 0) then
              nr = chunkSize*n
              allocate(spmd_buffer(p)%recvbuf(nr))
              spmd_buffer(p)%recvbuf = 0 ! initialize the buffer
              call spmd_irecv(spmd_buffer(p)%recvbuf,nr,p-1,TAG,spmd_buffer(p)%recv_request)
            endif
          enddo

          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            n = size(element%ghost_shell%shells_to_send(p)%index)
            send_offset = (element%ghost_shell%offset(p) - 1) * chunkSize
            if(n > 0) then
              ! fill the buffer: copy the contiguous chunk for process p
              !write(6,*) "sendbuf(",send_offset+1,":",send_offset+n*chunkSize,")",size(sendbuf)
              spmd_buffer(p)%sendbuf(1:n*chunkSize) = sendbuf(send_offset + 1 : send_offset + n*chunkSize)
              ns = n * chunkSize
              call spmd_isend(spmd_buffer(p)%sendbuf,ns,p-1,TAG,spmd_buffer(p)%send_request)
            endif
          enddo

          ! unpack the received data
          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            n = element%ghost_shell%offset(p+1) - element%ghost_shell%offset(p)
            recv_offset = (element%ghost_shell%offset(p) - 1) * chunkSize
            if(n > 0) then
              call spmd_Wait(spmd_buffer(p)%recv_request)
              ! copy the contiguous chunk for process p
              !write(6,*) "recvbuf(",recv_offset+1,":",recv_offset+n*chunkSize,")",size(recvbuf)
              !call flush(6)
              recvbuf(recv_offset + 1 : recv_offset + n*chunkSize) = spmd_buffer(p)%recvbuf(1:n*chunkSize)
            endif
          enddo

          ! wait for all the sends to complete

          do p = 1, nspmd
            if(ispmd+1 == p) cycle ! skip the current process
            ! Free Send Request
            if(spmd_buffer(p)%send_request /= MPI_REQUEST_NULL) then
              call spmd_Wait(spmd_buffer(p)%send_request)
            endif
            if(allocated(spmd_buffer(p)%recvbuf)) deallocate(spmd_buffer(p)%recvbuf)
            if(allocated(spmd_buffer(p)%sendbuf)) deallocate(spmd_buffer(p)%sendbuf)
          enddo
        end subroutine spmd_exchange_ghost_shells

      end module ghost_shells_mod


