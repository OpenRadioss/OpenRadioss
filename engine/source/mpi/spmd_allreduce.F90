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
      module spmd_allreduce_mod
        use get_mpi_operator_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        use spmd_error_mod, only: spmd_in, spmd_out
        implicit none
        interface spmd_allreduce
          module procedure spmd_allreduce_reals   !< Reduces real numbers across all processes and distributes result
          module procedure spmd_allreduce_ints    !< Reduces integers across all processes and distributes result
          module procedure spmd_allreduce_doubles !< Reduces double precision numbers across all processes and distributes result
          module procedure spmd_allreduce_real    !< Reduces a single real number across all processes and distributes result
          module procedure spmd_allreduce_int     !< Reduces a single integer across all processes and distributes result
          module procedure spmd_allreduce_double  !< Reduces a single double precision number across all processes and distributes result
        end interface spmd_allreduce
        interface spmd_reduce
          module procedure spmd_reduce_reals    !< Reduces real numbers across all processes
          module procedure spmd_reduce_ints     !< Reduces integers across all processes
          module procedure spmd_reduce_doubles  !< Reduces double precision numbers across all processes
          module procedure spmd_reduce_real     !< Reduces a single real number across all processes
          module procedure spmd_reduce_int      !< Reduces a single integer across all processes
          module procedure spmd_reduce_double   !< Reduces a single double precision number across all processes
        end interface spmd_reduce
      contains
!||====================================================================
!||    spmd_reduce_reals   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator    ../engine/source/mpi/spmd_mod.F90
!||    spmd_in             ../engine/source/mpi/spmd_error.F90
!||    spmd_out            ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod      ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_reals(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf(*)
          real, intent(inout) :: recvbuf(*)
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_ints   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator   ../engine/source/mpi/spmd_mod.F90
!||    spmd_in            ../engine/source/mpi/spmd_error.F90
!||    spmd_out           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_ints(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf(*)
          integer, intent(inout) :: recvbuf(*)
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
#endif
        end subroutine spmd_reduce_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_doubles   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_doubles(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf(*)
          double precision, intent(inout) :: recvbuf(*)
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
#endif
        end subroutine spmd_reduce_doubles

!||====================================================================
!||    spmd_allreduce_ints   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_ints(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf(*)
          integer, intent(inout) :: recvbuf(*)
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
#endif
        end subroutine spmd_allreduce_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_doubles   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator         ../engine/source/mpi/spmd_mod.F90
!||    spmd_in                  ../engine/source/mpi/spmd_error.F90
!||    spmd_out                 ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod           ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_doubles(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf(*)
          double precision, intent(inout) :: recvbuf(*)
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
#endif
        end subroutine spmd_allreduce_doubles
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_reals   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator       ../engine/source/mpi/spmd_mod.F90
!||    spmd_in                ../engine/source/mpi/spmd_error.F90
!||    spmd_out               ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod         ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_reals(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf(*)
          real, intent(inout) :: recvbuf(*)
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
#endif
        end subroutine spmd_allreduce_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_real   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator   ../engine/source/mpi/spmd_mod.F90
!||    spmd_in            ../engine/source/mpi/spmd_error.F90
!||    spmd_out           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_real(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf
          real, intent(inout) :: recvbuf
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
            used_comm = SPMD_COMM_WORLD
          end if
          if(buf_count /= 1) then
            ierr = -1
          else
            call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          end if
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_real
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_int    ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator   ../engine/source/mpi/spmd_mod.F90
!||    spmd_in            ../engine/source/mpi/spmd_error.F90
!||    spmd_out           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_int(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf
          integer, intent(inout) :: recvbuf
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
            used_comm = SPMD_COMM_WORLD
          end if
          if(buf_count /= 1) then
            ierr = -1
          else
            call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, root, used_comm, ierr)
          endif
          call spmd_out(TAG_REDUCE,ierr)
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_reduce_int
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_double   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator     ../engine/source/mpi/spmd_mod.F90
!||    spmd_in              ../engine/source/mpi/spmd_error.F90
!||    spmd_out             ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod       ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_double(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf
          double precision, intent(inout) :: recvbuf
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_reduce_double
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_int   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator     ../engine/source/mpi/spmd_mod.F90
!||    spmd_in              ../engine/source/mpi/spmd_error.F90
!||    spmd_out             ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod       ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_int(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf
          integer, intent(inout) :: recvbuf
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
            used_comm = SPMD_COMM_WORLD
          end if
          if(buf_count /= 1) then
            ierr = -1
          else
            call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          end if
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_allreduce_int
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_double   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/spmd_mod.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod          ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_double(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf
          double precision, intent(inout) :: recvbuf
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
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_allreduce_double
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_real   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_real(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf
          real, intent(inout) :: recvbuf
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
            used_comm = SPMD_COMM_WORLD
          end if
          if(buf_count /= 1) then
            ierr = -1
          else
            call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, ierr)
          end if
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_allreduce_real



      end module spmd_allreduce_mod


