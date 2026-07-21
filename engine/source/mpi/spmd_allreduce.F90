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
      module spmd_allreduce_mod
        use get_mpi_operator_mod, only: get_mpi_operator
        use spmd_operator_mod,  only: SPMD_MAX, SPMD_MIN, SPMD_SUM, SPMD_PROD
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_ALLREDUCE = -6

        ! SPMD operators provided by spmd_operator_mod

        !> \brief Interface for spmd_allreduce, a wrapper for MPI_ALLREDUCE
        interface spmd_allreduce
          module procedure spmd_allreduce_reals
          module procedure spmd_allreduce_ints
          module procedure spmd_allreduce_doubles
          module procedure spmd_allreduce_reals2d
          module procedure spmd_allreduce_ints2d
          module procedure spmd_allreduce_doubles2d
          module procedure spmd_allreduce_real
          module procedure spmd_allreduce_int
          module procedure spmd_allreduce_double
        end interface spmd_allreduce

      contains


! ======================================================================================================================
!>  \brief Allreduce of real       array
        subroutine spmd_allreduce_reals(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, ierr)
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_reals

! ======================================================================================================================
!>  \brief Allreduce of integer       array
        subroutine spmd_allreduce_ints(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_ints

! ======================================================================================================================
!>  \brief Allreduce of double precision       array
        subroutine spmd_allreduce_doubles(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_doubles

! ======================================================================================================================
!>  \brief Allreduce of real       array
        subroutine spmd_allreduce_reals2d(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:,:), intent(in) :: sendbuf
          real, dimension(:,:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, ierr)
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_reals2d

! ======================================================================================================================
!>  \brief Allreduce of integer       array
        subroutine spmd_allreduce_ints2d(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, dimension(:,:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_ints2d

! ======================================================================================================================
!>  \brief Allreduce of double precision       array
        subroutine spmd_allreduce_doubles2d(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:,:), intent(in) :: sendbuf
          double precision, dimension(:,:), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_doubles2d

! ======================================================================================================================
!>  \brief Allreduce of real       scalar
        subroutine spmd_allreduce_real(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real,  intent(in) :: sendbuf
          real,  intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, ierr)
          end if
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_real

! ======================================================================================================================
!>  \brief Allreduce of integer       scalar
        subroutine spmd_allreduce_int(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer,  intent(in) :: sendbuf
          integer,  intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          end if
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_int

! ======================================================================================================================
!>  \brief Allreduce of double precision       scalar
        subroutine spmd_allreduce_double(sendbuf, recvbuf, buf_count, operation, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision,  intent(in) :: sendbuf
          double precision,  intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag !added even if not used in MPI, to pass to spmd_in/out
          integer :: ierr, mpi_op, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLREDUCE
          end if
#ifdef MPI
          call spmd_in(tag_local, "MPI_Allreduce")
          mpi_op = get_mpi_operator(operation)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          end if
          call spmd_out(tag_local, ierr)
#else
          recvbuf = sendbuf
#endif
        end subroutine spmd_allreduce_double

      end module spmd_allreduce_mod
