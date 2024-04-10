!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      module spmd_mod
        ! Define the interface for spmd_send
!#define DEBUG_SPMD
! dummy tags for MPI calls that do not have a tag
        integer, parameter :: TAG_BARRIER = -1
        integer, parameter :: TAG_WAIT = -2
        integer, parameter :: TAG_WAITALL = -3
        integer, parameter :: TAG_WAITANY = -4
        integer, parameter :: TAG_REDUCE = -5
        integer, parameter :: TAG_ALLREDUCE = -6
! MPI operators
        integer, parameter :: SPMD_MAX = 1
        integer, parameter :: SPMD_MIN = 2
        integer, parameter :: SPMD_SUM = 3
        integer, parameter :: SPMD_PROD = 4

#ifndef MPI
        integer, parameter :: MPI_COMM_WORLD = 0
        integer, parameter :: MPI_STATUS_IGNORE = 0
        integer, parameter :: MPI_STATUS_SIZE = 1
#endif
        ! \brief Interface for spmd_send, a wrapper for MPI_SEND
        interface spmd_send
          module procedure spmd_send_reals      !< Sends real numbers
          module procedure spmd_send_ints       !< Sends integers
          module procedure spmd_send_doubles    !< Sends double precision numbers
          module procedure spmd_send_real       !< Sends a single real number
          module procedure spmd_send_int        !< Sends a single integer
          module procedure spmd_send_double     !< Sends a single double precision number
        end interface spmd_send

        ! \brief Interface for spmd_recv, a wrapper for MPI_RECV
        interface spmd_recv
          module procedure spmd_recv_reals      !< Receives real numbers
          module procedure spmd_recv_reals2D      !< Receives real numbers
          module procedure spmd_recv_ints       !< Receives integers
          module procedure spmd_recv_doubles    !< Receives double precision numbers
          module procedure spmd_recv_doubles2D   !< Receives double precision numbers
          module procedure spmd_recv_real       !< Receives a single real number
          module procedure spmd_recv_int        !< Receives a single integer
          module procedure spmd_recv_double     !< Receives a single double precision number
        end interface spmd_recv

        ! \brief Interface for spmd_isend, a wrapper for MPI_ISEND
        interface spmd_isend
          module procedure spmd_isend_reals     !< Non-blocking send of real numbers
          module procedure spmd_isend_ints      !< Non-blocking send of integers
          module procedure spmd_isend_doubles   !< Non-blocking send of double precision numbers
          module procedure spmd_isend_real      !< Non-blocking send of a single real number
          module procedure spmd_isend_int       !< Non-blocking send of a single integer
          module procedure spmd_isend_double    !< Non-blocking send of a single double precision number
        end interface spmd_isend

        ! \brief Interface for spmd_irecv, a wrapper for MPI_IRECV
        interface spmd_irecv
          module procedure spmd_irecv_reals     !< Non-blocking receive of real numbers
          module procedure spmd_irecv_ints      !< Non-blocking receive of integers
          module procedure spmd_irecv_doubles   !< Non-blocking receive of double precision numbers
          module procedure spmd_irecv_real      !< Non-blocking receive of a single real number
          module procedure spmd_irecv_int       !< Non-blocking receive of a single integer
          module procedure spmd_irecv_double    !< Non-blocking receive of a single double precision number
        end interface spmd_irecv

        ! \brief Interface for spmd_reduce, a wrapper for MPI_REDUCE
        interface spmd_reduce
          module procedure spmd_reduce_reals    !< Reduces real numbers across all processes
          module procedure spmd_reduce_ints     !< Reduces integers across all processes
          module procedure spmd_reduce_doubles  !< Reduces double precision numbers across all processes
          module procedure spmd_reduce_real     !< Reduces a single real number across all processes
          module procedure spmd_reduce_int      !< Reduces a single integer across all processes
          module procedure spmd_reduce_double   !< Reduces a single double precision number across all processes
        end interface spmd_reduce

        ! \brief Interface for spmd_allreduce, a wrapper for MPI_ALLREDUCE
        interface spmd_allreduce
          module procedure spmd_allreduce_reals   !< Reduces real numbers across all processes and distributes result
          module procedure spmd_allreduce_ints    !< Reduces integers across all processes and distributes result
          module procedure spmd_allreduce_doubles !< Reduces double precision numbers across all processes and distributes result
          module procedure spmd_allreduce_real    !< Reduces a single real number across all processes and distributes result
          module procedure spmd_allreduce_int     !< Reduces a single integer across all processes and distributes result
          module procedure spmd_allreduce_double  !< Reduces a single double precision number across all processes and distributes result
        end interface spmd_allreduce


      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Handle MPI errors
        subroutine spmd_out(tag, ierr)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: tag !< Tag of the the MPI call
          integer, intent(in) :: ierr !< error of the MPI call
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
          if(ierr /= MPI_SUCCESS) then
            write(6,*) 'MPI error: ', ierr,' at ',tag
            call MPI_Abort(MPI_COMM_WORLD, ierr)
          end if
#ifdef DEBUG_SPMD
          write(6,*) 'Exiting MPI call: ', tag
#endif
#endif
        end subroutine spmd_out

!! \brief Trace Entry in MPI subroutines
        subroutine spmd_in(tag)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer :: tag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
#ifdef DEBUG_SPMD
          write(6,*) 'Entering MPI call: ', tag
#endif
        end subroutine spmd_in

!!\brief get MPI rank
        subroutine spmd_comm_rank(rank, comm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(out) :: rank !< Rank of the process
          integer, intent(in), optional :: comm !< Communicator
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call spmd_in(0)
#ifdef MPI
          if(present(comm)) then
            call MPI_Comm_rank(comm, rank, ierr)
          else
            call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
          end if
#else
          rank = 0
#endif
          call spmd_out(0,ierr)
        end subroutine spmd_comm_rank

!!\brief get MPI size
        subroutine spmd_comm_size(rank, comm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(out) :: rank !< Rank of the process
          integer, intent(in), optional :: comm !< Communicator
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call spmd_in(0)
#ifdef MPI
          if(present(comm)) then
            call MPI_Comm_size(comm, rank, ierr)
          else
            call MPI_Comm_size(MPI_COMM_WORLD, rank, ierr)
          end if
#else
          rank = 0
#endif
          call spmd_out(0,ierr)
        end subroutine spmd_comm_size


!! \brief Get the MPI operator for a given SPMD operator
        function get_mpi_operator(spmd_op) result(mpi_operator)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: spmd_op
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: mpi_operator
#ifdef MPI
          select case(spmd_op)
           case(SPMD_MAX)
            mpi_operator = MPI_MAX
           case(SPMD_MIN)
            mpi_operator = MPI_MIN
           case(SPMD_SUM)
            mpi_operator = MPI_SUM
           case(SPMD_PROD)
            mpi_operator = MPI_PROD
           case default
            mpi_operator = MPI_OP_NULL
          end select
#else
          mpi_operator = 0
#endif
        end function get_mpi_operator

! ======================================================================================================================
!                                                  WRAPPER
! ======================================================================================================================
!   The remaining subroutines are wrappers for the MPI subroutines.
!   They are not ment to be called directely, but through the interfaces defined above.
!   See MPI documentation for the meaning of the arguments.
! ======================================================================================================================
        subroutine spmd_barrier(comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, optional, intent(in) :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(TAG_BARRIER)
          if(present(comm)) then
            call MPI_Barrier(comm, ierr)
          else
            call MPI_Barrier(MPI_COMM_WORLD, ierr)
          end if
          call spmd_out(TAG_BARRIER,ierr)
#endif
        end subroutine spmd_barrier
! ======================================================================================================================
        subroutine spmd_wait(request, status)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: request
          integer, dimension(MPI_STATUS_SIZE), optional, intent(inout) :: status
#ifdef MPI
          integer :: ierr
          call spmd_in(TAG_WAIT)
          if(present(status)) then
            call MPI_Wait(request, status, ierr)
          else
            call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
          end if
          call spmd_out(TAG_WAIT,ierr)
#endif
        end subroutine spmd_wait
! ======================================================================================================================
        subroutine spmd_send_reals(buf, buf_count, dest, tag,  comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          real, dimension(buf_count), intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, MPI_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_reals
! ======================================================================================================================
        subroutine spmd_send_ints(buf, buf_count, dest, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          integer, dimension(buf_count), intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_INTEGER , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_ints
! ======================================================================================================================
        subroutine spmd_send_doubles(buf, buf_count, dest, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          double precision, dimension(buf_count), intent(in) :: buf
#ifdef MPI
          integer :: ierr
          ! the MPI datatype for double precision is MPI_DOUBLE_PRECISION
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_doubles
! ======================================================================================================================
        subroutine spmd_recv_reals(buf, buf_count, source, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count), intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_reals
! ======================================================================================================================
        subroutine spmd_recv_reals2D(buf, buf_count, source, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count,1), intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_reals2D

! ======================================================================================================================
        subroutine spmd_recv_ints(buf, buf_count, source, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(buf_count), intent(out) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
#endif
          call spmd_out(tag,ierr)
        end subroutine spmd_recv_ints
! ======================================================================================================================
        subroutine spmd_recv_doubles(buf, buf_count, source, tag,  comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count), intent(out) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_doubles
! ======================================================================================================================
        subroutine spmd_recv_doubles2D(buf, buf_count, source, tag,  comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count,1), intent(out) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_doubles2D

! ======================================================================================================================
        subroutine spmd_isend_reals(buf, buf_count, dest, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          real, dimension(buf_count), intent(in) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_reals
! ======================================================================================================================
        subroutine spmd_isend_ints(buf, buf_count, dest, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, dimension(buf_count), intent(in) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_ints
! ======================================================================================================================
        subroutine spmd_isend_doubles(buf, buf_count, dest, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          double precision, dimension(buf_count), intent(in) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_irecv_reals(buf, buf_count, source, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count), intent(out) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_reals
! ======================================================================================================================
        subroutine spmd_irecv_ints(buf, buf_count, source, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(buf_count), intent(out) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_ints
! ======================================================================================================================
        subroutine spmd_irecv_doubles(buf, buf_count, source, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count), intent(out) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_doubles
! ======================================================================================================================
        subroutine spmd_waitany(buf_count, array_of_requests, index_of_completed, status)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count
          integer, dimension(buf_count), intent(inout) :: array_of_requests
          integer, intent(inout) :: index_of_completed
          integer, dimension(MPI_STATUS_SIZE), optional, intent(inout) :: status
#ifdef MPI
          integer :: ierr
          call spmd_in(TAG_WAITANY)
          if(present(status)) then
            call MPI_Waitany(buf_count, array_of_requests, index_of_completed, status, ierr)
          else
            call MPI_Waitany(buf_count, array_of_requests, index_of_completed, MPI_STATUS_IGNORE, ierr)
          end if
          call spmd_out(TAG_WAITANY,ierr)
#endif
        end subroutine spmd_waitany
! ======================================================================================================================
        subroutine spmd_waitall(buf_count, array_of_requests, array_of_statuses)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count
          integer, dimension(buf_count), intent(inout) :: array_of_requests
          integer, dimension(MPI_STATUS_SIZE, buf_count), optional, intent(inout) :: array_of_statuses
#ifdef MPI
          integer :: ierr
          call spmd_in(TAG_WAITALL)
          if(present(array_of_statuses)) then
            call MPI_Waitall(buf_count, array_of_requests, array_of_statuses, ierr)
          else
            call MPI_Waitall(buf_count, array_of_requests, MPI_STATUSES_IGNORE, ierr)
          end if
          call spmd_out(TAG_WAITALL,ierr)
#endif
        end subroutine spmd_waitall
! ======================================================================================================================
        subroutine spmd_probe(source, tag, comm, status)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: source, tag
          integer, intent(in), optional :: comm
          integer, dimension(MPI_STATUS_SIZE), intent(out) :: status
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Probe(source, tag, comm, status, ierr)
          else
            call MPI_Probe(source, tag, MPI_COMM_WORLD, status, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_probe
! ======================================================================================================================
        subroutine spmd_reduce_reals(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          real, intent(in) :: sendbuf(*)
          real, intent(out) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_reals
! ======================================================================================================================
        subroutine spmd_reduce_ints(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: sendbuf(*)
          integer, intent(out) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_ints
! ======================================================================================================================
        subroutine spmd_reduce_doubles(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          double precision, intent(in) :: sendbuf(*)
          double precision, intent(out) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_doubles
! ======================================================================================================================
        subroutine spmd_allreduce_ints(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: sendbuf(*)
          integer, intent(out) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_allreduce_doubles(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          double precision, intent(in) :: sendbuf(*)
          double precision, intent(out) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_allreduce_reals(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          real, intent(in) :: sendbuf(*)
          real, intent(out) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_send_int(buf, buf_count, dest, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          integer, intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_INTEGER , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_send_double(buf, buf_count, dest, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          double precision,  intent(in) :: buf
          integer :: ierr
          ! the MPI datatype for double precision is MPI_DOUBLE_PRECISION
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_send_real(buf, buf_count, dest, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          real,  intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, MPI_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine

        subroutine spmd_recv_real(buf, buf_count, source, tag, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          real,  intent(out) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_real
! ======================================================================================================================
        subroutine spmd_recv_int(buf, buf_count, source, tag,  comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(out) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_int
! ======================================================================================================================
        subroutine spmd_recv_double(buf, buf_count, source, tag,  comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          double precision, intent(out) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_double
! ======================================================================================================================
        subroutine spmd_isend_real(buf, buf_count, dest, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          real,  intent(in) :: buf
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_real
! ======================================================================================================================
        subroutine spmd_isend_int(buf, buf_count, dest, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_int
! ======================================================================================================================
        subroutine spmd_isend_double(buf, buf_count, dest, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, dest, tag
          double precision, intent(in) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_irecv_real(buf, buf_count, source, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: buf_count, source, tag
          real,    intent(out) :: buf
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_real
! ======================================================================================================================
        subroutine spmd_irecv_int(buf, buf_count, source, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(out) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_int
! ======================================================================================================================
        subroutine spmd_irecv_double(buf, buf_count, source, tag, request, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          double precision, intent(out) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(out) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_double
! ======================================================================================================================
        subroutine spmd_reduce_real(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          real, intent(in) :: sendbuf
          real, intent(out) :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm

          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_real
! ======================================================================================================================
        subroutine spmd_reduce_int(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: sendbuf
          integer, intent(out) :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_int
! ======================================================================================================================
        subroutine spmd_reduce_double(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          double precision, intent(in) :: sendbuf
          double precision, intent(out) :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_double
! ======================================================================================================================
        subroutine spmd_allreduce_int(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          integer, intent(in) :: sendbuf
          integer, intent(out) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_allreduce_double(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          double precision, intent(in) :: sendbuf
          double precision, intent(out) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
! ======================================================================================================================
        subroutine spmd_allreduce_real(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#ifdef MPI
#include "mpif.h"
#endif
          real, intent(in) :: sendbuf
          real, intent(out) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = MPI_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
      end module spmd_mod
