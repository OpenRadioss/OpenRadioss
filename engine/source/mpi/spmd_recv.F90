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
      module spmd_recv_mod
        implicit none

        !> \brief Interface for spmd_recv, a wrapper for MPI_RECV
        interface spmd_recv
          module procedure spmd_recv_reals
          module procedure spmd_recv_ints
          module procedure spmd_recv_doubles
          module procedure spmd_recv_reals2d
          module procedure spmd_recv_ints2d
          module procedure spmd_recv_doubles2d
          module procedure spmd_recv_real
          module procedure spmd_recv_int
          module procedure spmd_recv_double
        end interface spmd_recv

      contains

! ======================================================================================================================
!>  \brief Blocking receive of real       array
        subroutine spmd_recv_reals(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_reals

! ======================================================================================================================
!>  \brief Blocking receive of integer       array
        subroutine spmd_recv_ints(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_ints

! ======================================================================================================================
!>  \brief Blocking receive of double precision       array
        subroutine spmd_recv_doubles(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_doubles

! ======================================================================================================================
!>  \brief Blocking receive of real       array
        subroutine spmd_recv_reals2d(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(:,:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_reals2d

! ======================================================================================================================
!>  \brief Blocking receive of integer       array
        subroutine spmd_recv_ints2d(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(:,:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_ints2d

! ======================================================================================================================
!>  \brief Blocking receive of double precision       array
        subroutine spmd_recv_doubles2d(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(:,:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_doubles2d

! ======================================================================================================================
!>  \brief Blocking receive of real       scalar
        subroutine spmd_recv_real(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real,  intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_real

! ======================================================================================================================
!>  \brief Blocking receive of integer       scalar
        subroutine spmd_recv_int(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer,  intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_int

! ======================================================================================================================
!>  \brief Blocking receive of double precision       scalar
        subroutine spmd_recv_double(buf, buf_count, source, tag, status, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision,  intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer :: ierr
          integer :: used_comm
          integer :: local_status(MPI_STATUS_SIZE)

#ifdef MPI
          call spmd_in(tag, "MPI_Recv", source)

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, used_comm, status, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_recv_double

      end module spmd_recv_mod
