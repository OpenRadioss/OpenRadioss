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
      module spmd_iallgatherv_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_IALLGATHERV = -19

        !> \brief Interface for spmd_iallgatherv, a wrapper for MPI_IALLGATHERV
        interface spmd_iallgatherv
          module procedure spmd_iallgatherv_reals
          module procedure spmd_iallgatherv_ints
          module procedure spmd_iallgatherv_doubles
          module procedure spmd_iallgatherv_reals2d
          module procedure spmd_iallgatherv_ints2d
          module procedure spmd_iallgatherv_doubles2d
          module procedure spmd_iallgatherv_real
          module procedure spmd_iallgatherv_int
          module procedure spmd_iallgatherv_double
        end interface spmd_iallgatherv

      contains

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of real       array
        subroutine spmd_iallgatherv_reals(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real, dimension(:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_reals

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of integer       array
        subroutine spmd_iallgatherv_ints(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          integer, dimension(:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_ints

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of double precision       array
        subroutine spmd_iallgatherv_doubles(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          double precision, dimension(:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION,&
          & used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_doubles

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of real       array
        subroutine spmd_iallgatherv_reals2d(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real, dimension(:,:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_reals2d

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of integer       array
        subroutine spmd_iallgatherv_ints2d(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_ints2d

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of double precision       array
        subroutine spmd_iallgatherv_doubles2d(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          double precision, dimension(:,:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION,&
          & used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of real       scalar
        subroutine spmd_iallgatherv_real(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real,  intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_real

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of integer       scalar
        subroutine spmd_iallgatherv_int(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          integer,  intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_int

! ======================================================================================================================
!>  \brief Non-blocking allgatherv of double precision       scalar
        subroutine spmd_iallgatherv_double(sendbuf, sendcount, recvbuf, recvcounts, displs, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          double precision,  intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iallgatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Iallgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION,&
          & used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          recvbuf(1:sendcount) = sendbuf
          request = 0
#endif
        end subroutine spmd_iallgatherv_double

      end module spmd_iallgatherv_mod
