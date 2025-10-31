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
!||    h3d_gather_id_val_mod    ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||--- called by ------------------------------------------------------
!||    genh3d                   ../engine/source/output/h3d/h3d_results/genh3d.F
!||    h3d_gather_id_val_test   ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||====================================================================
      module h3d_gather_id_val_mod
      implicit none
      contains
!||====================================================================
!||    h3d_gather_id_val        ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||--- called by ------------------------------------------------------
!||    genh3d                   ../engine/source/output/h3d/h3d_results/genh3d.F
!||    h3d_gather_id_val_test   ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod      ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
        subroutine h3d_gather_id_val(isend_buffer,isend_buffer_real, send_size, &
          irecv_buffer, irec_buffer_real, recv_size, &
          shell_stacksize_p0,p0_sizes,p0_offsets,nspmd,ispmd,it_spmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use spmd_comm_world_mod, only : spmd_comm_world
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                          :: send_size            !< number of elements to send
          integer, intent (in),dimension(send_size)        :: isend_buffer         !< integer buffer to send
          real, intent (in),dimension(send_size)           :: isend_buffer_real    !< float buffer to send
          integer , intent (in   )                         :: recv_size            !< total size of elements to receive
          integer, intent (inout  ),dimension(recv_size)   :: irecv_buffer         !< integer buffer to receive
          real, intent (inout  ),dimension(recv_size)      :: irec_buffer_real     !< float buffer to receive
          integer, intent(inout)                           :: shell_stacksize_p0   !< Size of stack after gather
          integer, intent (in   ),dimension(nspmd)         :: p0_sizes             !< size to receive from each mpi domain
          integer, intent (in   ),dimension(nspmd+1)       :: p0_offsets           !< offset to apply on integer buffer
          integer, intent (in   )                          :: nspmd                !< number of spmd domain
          integer, intent (in   )                          :: ispmd                !< spmd rank id
          integer, intent (in   ),dimension(nspmd)         :: it_spmd              !< spmd rank id for communications
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
          integer :: integer_size                         !< Size of integer in bytes
          integer :: real_size                            !< Size of real in bytes
          integer :: buffer_size                          !< Size of the buffer in bytes
          character, dimension(:),allocatable :: buffer   !< receive buffer for packed data
          integer :: iad_recv(nspmd+1)

          integer :: i
          integer :: l
          integer :: k
          integer :: rec_dim
          integer :: msgtag
          integer :: pos
          integer :: ierror,                     &
            status(mpi_status_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          msgtag = 7700
          integer_size = 4
          real_size = 4

          ! Domain 0 will gather data from all other processes
          ! --------------------------------------------------
          if (ispmd == 0) then
            ! buffer size is max of all processes
            buffer_size = 0
            do i=1,nspmd
              buffer_size = max(buffer_size, p0_sizes(i) )
            end do

            if (buffer_size > 0) then
              buffer_size = (buffer_size+10)*integer_size+(buffer_size+10)*real_size
              allocate(buffer(buffer_size))     ! buffer will host [integer_4][real_4] array.
              iad_recv(1) = 1
              do i=1,nspmd
                iad_recv(i+1) = iad_recv(i) + p0_sizes(i)
              end do
              do i=2,nspmd
                if (p0_sizes(i) > 0) then
                  l = iad_recv(i)
                  rec_dim = p0_sizes(i) * integer_size +  p0_sizes(i) * real_size  ! each process sends integer and real data
                  ! Prepare the receive buffer for unpacking
                  call MPI_Recv(buffer, rec_dim, MPI_PACKED, it_spmd(i), msgtag, SPMD_COMM_WORLD, status, ierror)
                  ! Unpack the received data into the receive buffers
                  pos = 0
                  call mpi_unpack(buffer, buffer_size, pos, irecv_buffer(l),  &
                  &       p0_sizes(i), MPI_INTEGER, SPMD_COMM_WORLD, ierror)
                  call mpi_unpack(buffer,  buffer_size, pos, irec_buffer_real(l), &
                  &       p0_sizes(i) , MPI_REAL, SPMD_COMM_WORLD, ierror)
                  do k=1,p0_sizes(i)
                    irecv_buffer(l+k-1) = irecv_buffer(l+k-1) + p0_offsets(i)
                  end do
                  shell_stacksize_p0 = shell_stacksize_p0 + p0_sizes(i)
                end if
              end do
            end if
          else
            ! Other processes will send their data to domain 0
            ! --------------------------------------------------
            if (send_size > 0) then
              buffer_size = send_size * integer_size + send_size * real_size
              allocate(buffer(buffer_size))
              pos = 0
              call mpi_pack(isend_buffer, send_size, MPI_INTEGER, buffer, buffer_size, pos, SPMD_COMM_WORLD, ierror)
              call mpi_pack(isend_buffer_real, send_size, MPI_REAL, buffer, buffer_size, pos, SPMD_COMM_WORLD, ierror)
              call MPI_Send(buffer, buffer_size, MPI_PACKED, 0, msgtag, SPMD_COMM_WORLD, ierror)
            end if
          end if

          if (allocated(buffer))then
            deallocate(buffer)
          end if
#endif
        end subroutine h3d_gather_id_val

      end module h3d_gather_id_val_mod

#ifdef MAIN

!||====================================================================
!||    h3d_gather_id_val_test   ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||--- calls      -----------------------------------------------------
!||    h3d_gather_id_val        ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||--- uses       -----------------------------------------------------
!||    h3d_gather_id_val_mod    ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||====================================================================
      program  h3d_gather_id_val_test
        use h3d_gather_id_val_mod
        implicit none
#include "mpif.h"
        integer,dimension(10) :: isend
        real,dimension(10) :: isend_real
        integer,dimension(:),allocatable :: irecv_buffer
        real,dimension(:),allocatable :: irec_buffer_real
        integer,dimension(:),allocatable :: p0_sizes
        integer,dimension(:),allocatable :: p0_offsets
        integer,dimension(:),allocatable :: it_spmd
        integer :: i, ierr, rank, size, recv_size,send_size


        call MPI_Init(ierr)
        call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

        if (rank /= 0)then
          send_size= 10
        else
          send_size = 0
        end if
        allocate (it_spmd(size))
        do i=1,size
          it_spmd(i) = i-1
        end do
        recv_size = 0

        if (rank ==0)then
          allocate(p0_sizes(size))
          allocate(p0_offsets(size))
          do i=1,size
            if (i /= rank+1) then
              p0_sizes(i) = 10
            else
              p0_sizes(i) = 0
            end if
            recv_size = recv_size + p0_sizes(i)
            p0_offsets(1) = 0
          end do
          do i=1,size-1
            p0_offsets(i+1) = p0_offsets(i) + p0_sizes(i)
          end do
          allocate(irecv_buffer(recv_size))
          allocate(irec_buffer_real(recv_size))
        else
          allocate(p0_sizes(1))
          allocate(p0_offsets(1))
        end if


        do i=1,send_size
          isend(i) =  i
          isend_real(i) = (rank * 1000 + i)/10.0
        end do

        call h3d_gather_id_val(isend,isend_real, send_size, &
          irecv_buffer, irec_buffer_real, recv_size, &
          p0_sizes,p0_offsets,size,rank,it_spmd)


        if (rank == 0) then
          print *, "Rank 0 received:"
          print *, irecv_buffer
          print*,"flt"
          print *, irec_buffer_real
        end if
        call mpi_barrier(MPI_COMM_WORLD,ierr)
        call mpi_finalize(ierr)

      end program  h3d_gather_id_val_test



#endif
