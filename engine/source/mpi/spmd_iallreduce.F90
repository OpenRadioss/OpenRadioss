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
      module spmd_iallreduce_mod
        use get_mpi_operator_mod, only: get_mpi_operator
        use spmd_operator_mod,  only: SPMD_MAX, SPMD_MIN, SPMD_SUM, SPMD_PROD
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_IALLREDUCE = -20

        ! SPMD operators provided by spmd_operator_mod

        !> \brief Interface for spmd_iallreduce, a wrapper for MPI_IALLREDUCE
        interface spmd_iallreduce
          module procedure spmd_iallreduce_reals
          module procedure spmd_iallreduce_ints
          module procedure spmd_iallreduce_doubles
          module procedure spmd_iallreduce_reals2d
          module procedure spmd_iallreduce_ints2d
          module procedure spmd_iallreduce_doubles2d
          module procedure spmd_iallreduce_real
          module procedure spmd_iallreduce_int
          module procedure spmd_iallreduce_double
        end interface spmd_iallreduce

      contains

! ======================================================================================================================
!>  \brief Non-blocking allreduce of real       array
        subroutine spmd_iallreduce_reals(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_reals

! ======================================================================================================================
!>  \brief Non-blocking allreduce of integer       array
        subroutine spmd_iallreduce_ints(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_ints

! ======================================================================================================================
!>  \brief Non-blocking allreduce of double precision       array
        subroutine spmd_iallreduce_doubles(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_doubles

! ======================================================================================================================
!>  \brief Non-blocking allreduce of real       array
        subroutine spmd_iallreduce_reals2d(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:,:), intent(in) :: sendbuf
          real, dimension(:,:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_reals2d

! ======================================================================================================================
!>  \brief Non-blocking allreduce of integer       array
        subroutine spmd_iallreduce_ints2d(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, dimension(:,:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_ints2d

! ======================================================================================================================
!>  \brief Non-blocking allreduce of double precision       array
        subroutine spmd_iallreduce_doubles2d(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:,:), intent(in) :: sendbuf
          double precision, dimension(:,:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking allreduce of real       scalar
        subroutine spmd_iallreduce_real(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real,  intent(in) :: sendbuf
          real,  intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_real

! ======================================================================================================================
!>  \brief Non-blocking allreduce of integer       scalar
        subroutine spmd_iallreduce_int(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer,  intent(in) :: sendbuf
          integer,  intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_int

! ======================================================================================================================
!>  \brief Non-blocking allreduce of double precision       scalar
        subroutine spmd_iallreduce_double(sendbuf, recvbuf, buf_count, operation, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision,  intent(in) :: sendbuf
          double precision,  intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Iallreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_iallreduce_double

      end module spmd_iallreduce_mod
