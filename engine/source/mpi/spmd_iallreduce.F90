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
!||    spmd_iallreduce_mod   ../engine/source/mpi/spmd_iallreduce.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90 (via interface)
!||--- wraps       ----------------------------------------------------
!||    MPI_Iallreduce        Non-blocking Allreduce
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||    get_mpi_operator_mod  ../engine/source/mpi/get_mpi_operator.F90
!||====================================================================
      module spmd_iallreduce_mod
        use spmd_error_mod, only : spmd_in, spmd_out
        use spmd_comm_world_mod, only : SPMD_COMM_WORLD
        use get_mpi_operator_mod, only : get_mpi_operator, TAG_IALLREDUCE
        implicit none

        interface spmd_iallreduce
          module procedure spmd_iallreduce_reals
          module procedure spmd_iallreduce_ints
          module procedure spmd_iallreduce_doubles
          module procedure spmd_iallreduce_real
          module procedure spmd_iallreduce_int
          module procedure spmd_iallreduce_double
        end interface spmd_iallreduce

      contains
!||====================================================================
!||    spmd_iallreduce_reals   ../engine/source/mpi/spmd_iallreduce.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/get_mpi_operator.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_iallreduce_reals(sendbuf, recvbuf, buf_count, operation, request, comm)
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf(*)
          real, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_IALLREDUCE)
          mpi_op = get_mpi_operator(operation)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, request, ierr)
          call spmd_out(TAG_IALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
          request = MPI_REQUEST_NULL
#endif
        end subroutine spmd_iallreduce_reals
! =====================================================================================================================
!||====================================================================
!||    spmd_iallreduce_ints    ../engine/source/mpi/spmd_iallreduce.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/get_mpi_operator.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_iallreduce_ints(sendbuf, recvbuf, buf_count, operation, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf(*)
          integer, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_IALLREDUCE)
          mpi_op = get_mpi_operator(operation)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, request, ierr)
          call spmd_out(TAG_IALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
          request = MPI_REQUEST_NULL
#endif
        end subroutine spmd_iallreduce_ints
! =====================================================================================================================
!||====================================================================
!||    spmd_iallreduce_doubles ../engine/source/mpi/spmd_iallreduce.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/get_mpi_operator.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_iallreduce_doubles(sendbuf, recvbuf, buf_count, operation, request, comm)
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf(*)
          double precision, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_IALLREDUCE)
          mpi_op = get_mpi_operator(operation)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, request, ierr)
          call spmd_out(TAG_IALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
          request = MPI_REQUEST_NULL
#endif
        end subroutine spmd_iallreduce_doubles
! =====================================================================================================================
!||====================================================================
!||    spmd_iallreduce_real    ../engine/source/mpi/spmd_iallreduce.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/get_mpi_operator.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_iallreduce_real(sendbuf, recvbuf, buf_count, operation, request, comm)
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf
          real, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_IALLREDUCE)
          mpi_op = get_mpi_operator(operation)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          if (buf_count /= 1) then
            ierr = -1
            request = MPI_REQUEST_NULL
          else
            call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, request, ierr)
          end if
          call spmd_out(TAG_IALLREDUCE,ierr)
#else
          if (buf_count == 1) then
            recvbuf = sendbuf
          end if
          request = MPI_REQUEST_NULL
#endif
        end subroutine spmd_iallreduce_real
! =====================================================================================================================
!||====================================================================
!||    spmd_iallreduce_int     ../engine/source/mpi/spmd_iallreduce.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/get_mpi_operator.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_iallreduce_int(sendbuf, recvbuf, buf_count, operation, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf
          integer, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_IALLREDUCE)
          mpi_op = get_mpi_operator(operation)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          if (buf_count /= 1) then
            ierr = -1
            request = MPI_REQUEST_NULL
          else
            call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, request, ierr)
          end if
          call spmd_out(TAG_IALLREDUCE,ierr)
#else
          if (buf_count == 1) then
            recvbuf = sendbuf
          end if
          request = MPI_REQUEST_NULL
#endif
        end subroutine spmd_iallreduce_int
! =====================================================================================================================
!||====================================================================
!||    spmd_iallreduce_double  ../engine/source/mpi/spmd_iallreduce.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/get_mpi_operator.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_iallreduce_double(sendbuf, recvbuf, buf_count, operation, request, comm)
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf
          double precision, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_IALLREDUCE)
          mpi_op = get_mpi_operator(operation)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          if (buf_count /= 1) then
            ierr = -1
            request = MPI_REQUEST_NULL
          else
            call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, request, ierr)
          end if
          call spmd_out(TAG_IALLREDUCE,ierr)
#else
          if (buf_count == 1) then
            recvbuf = sendbuf
          end if
          request = MPI_REQUEST_NULL
#endif
        end subroutine spmd_iallreduce_double

      end module spmd_iallreduce_mod
