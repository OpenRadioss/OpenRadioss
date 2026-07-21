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
      module spmd_ireduce_mod
        use get_mpi_operator_mod, only: get_mpi_operator
        use spmd_operator_mod,  only: SPMD_MAX, SPMD_MIN, SPMD_SUM, SPMD_PROD
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD, SPMD_REQUEST_NULL
        implicit none

        integer, parameter, public :: TAG_IREDUCE = -37

        ! SPMD operators provided by spmd_operator_mod

        !> \brief Interface for spmd_ireduce, a wrapper for MPI_IREDUCE
        interface spmd_ireduce
          module procedure spmd_ireduce_reals
          module procedure spmd_ireduce_ints
          module procedure spmd_ireduce_doubles
          module procedure spmd_ireduce_reals2d
          module procedure spmd_ireduce_ints2d
          module procedure spmd_ireduce_doubles2d
          module procedure spmd_ireduce_real
          module procedure spmd_ireduce_int
          module procedure spmd_ireduce_double
        end interface spmd_ireduce

      contains

! ======================================================================================================================
!>  \brief Non-blocking reduce of real       array
        subroutine spmd_ireduce_reals(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          real, dimension(:), intent(in), target :: sendbuf
          real, dimension(:), intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          same_buffer = .false.
          if (buf_count .gt. 0) then
            send_ptr = c_loc(sendbuf(1))
            recv_ptr = c_loc(recvbuf(1))
            same_buffer = c_associated(send_ptr, recv_ptr)
          else
            send_ptr = c_null_ptr
            recv_ptr = c_null_ptr
          end if

          if (same_buffer) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_reals

! ======================================================================================================================
!>  \brief Non-blocking reduce of integer       array
        subroutine spmd_ireduce_ints(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          integer, dimension(:), intent(in), target :: sendbuf
          integer, dimension(:), intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          same_buffer = .false.
          if (buf_count .gt. 0) then
            send_ptr = c_loc(sendbuf(1))
            recv_ptr = c_loc(recvbuf(1))
            same_buffer = c_associated(send_ptr, recv_ptr)
          else
            send_ptr = c_null_ptr
            recv_ptr = c_null_ptr
          end if

          if (same_buffer) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_ints

! ======================================================================================================================
!>  \brief Non-blocking reduce of double precision       array
        subroutine spmd_ireduce_doubles(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          double precision, dimension(:), intent(in), target :: sendbuf
          double precision, dimension(:), intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          same_buffer = .false.
          if (buf_count .gt. 0) then
            send_ptr = c_loc(sendbuf(1))
            recv_ptr = c_loc(recvbuf(1))
            same_buffer = c_associated(send_ptr, recv_ptr)
          else
            send_ptr = c_null_ptr
            recv_ptr = c_null_ptr
          end if

          if (same_buffer) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_doubles

! ======================================================================================================================
!>  \brief Non-blocking reduce of real       array
        subroutine spmd_ireduce_reals2d(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          real, dimension(:,:), intent(in), target :: sendbuf
          real, dimension(:,:), intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          same_buffer = .false.
          if (buf_count .gt. 0) then
            send_ptr = c_loc(sendbuf(1,1))
            recv_ptr = c_loc(recvbuf(1,1))
            same_buffer = c_associated(send_ptr, recv_ptr)
          else
            send_ptr = c_null_ptr
            recv_ptr = c_null_ptr
          end if

          if (same_buffer) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_reals2d

! ======================================================================================================================
!>  \brief Non-blocking reduce of integer       array
        subroutine spmd_ireduce_ints2d(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          integer, dimension(:,:), intent(in), target :: sendbuf
          integer, dimension(:,:), intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          same_buffer = .false.
          if (buf_count .gt. 0) then
            send_ptr = c_loc(sendbuf(1,1))
            recv_ptr = c_loc(recvbuf(1,1))
            same_buffer = c_associated(send_ptr, recv_ptr)
          else
            send_ptr = c_null_ptr
            recv_ptr = c_null_ptr
          end if

          if (same_buffer) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_ints2d

! ======================================================================================================================
!>  \brief Non-blocking reduce of double precision       array
        subroutine spmd_ireduce_doubles2d(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          double precision, dimension(:,:), intent(in), target :: sendbuf
          double precision, dimension(:,:), intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          same_buffer = .false.
          if (buf_count .gt. 0) then
            send_ptr = c_loc(sendbuf(1,1))
            recv_ptr = c_loc(recvbuf(1,1))
            same_buffer = c_associated(send_ptr, recv_ptr)
          else
            send_ptr = c_null_ptr
            recv_ptr = c_null_ptr
          end if

          if (same_buffer) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking reduce of real       scalar
        subroutine spmd_ireduce_real(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          real,  intent(in), target :: sendbuf
          real,  intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            send_ptr = c_loc(sendbuf)
            recv_ptr = c_loc(recvbuf)
            same_buffer = c_associated(send_ptr, recv_ptr)

            if (same_buffer) then
              request = SPMD_REQUEST_NULL
              ierr = MPI_ERR_ARG
            else
              call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, request, ierr)
            end if
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_real

! ======================================================================================================================
!>  \brief Non-blocking reduce of integer       scalar
        subroutine spmd_ireduce_int(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          integer,  intent(in), target :: sendbuf
          integer,  intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            send_ptr = c_loc(sendbuf)
            recv_ptr = c_loc(recvbuf)
            same_buffer = c_associated(send_ptr, recv_ptr)

            if (same_buffer) then
              request = SPMD_REQUEST_NULL
              ierr = MPI_ERR_ARG
            else
              call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, root, used_comm, request, ierr)
            end if
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_int

! ======================================================================================================================
!>  \brief Non-blocking reduce of double precision       scalar
        subroutine spmd_ireduce_double(sendbuf, recvbuf, buf_count, operation, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
          implicit none
#include "spmd.inc"
          double precision,  intent(in), target :: sendbuf
          double precision,  intent(inout), target :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, mpi_op, used_comm
          type(c_ptr) :: send_ptr, recv_ptr
          logical :: same_buffer
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IREDUCE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ireduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            request = SPMD_REQUEST_NULL
            ierr = MPI_ERR_ARG
          else
            send_ptr = c_loc(sendbuf)
            recv_ptr = c_loc(recvbuf)
            same_buffer = c_associated(send_ptr, recv_ptr)

            if (same_buffer) then
              request = SPMD_REQUEST_NULL
              ierr = MPI_ERR_ARG
            else
              call MPI_Ireduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, root, used_comm, request, ierr)
            end if
          end if

          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
          request = 0
#endif
        end subroutine spmd_ireduce_double

      end module spmd_ireduce_mod
