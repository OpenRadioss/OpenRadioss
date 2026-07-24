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
      module spmd_ialltoall_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_IALLTOALL = -38

        !> \brief Interface for spmd_ialltoall, a wrapper for MPI_IALLTOALL
        interface spmd_ialltoall
          module procedure spmd_ialltoall_reals
          module procedure spmd_ialltoall_ints
          module procedure spmd_ialltoall_doubles
          module procedure spmd_ialltoall_reals2d
          module procedure spmd_ialltoall_ints2d
          module procedure spmd_ialltoall_doubles2d
          module procedure spmd_ialltoall_real
          module procedure spmd_ialltoall_int
          module procedure spmd_ialltoall_double
        end interface spmd_ialltoall

      contains

! ======================================================================================================================
!>  \brief Non-blocking alltoall of real       array
        subroutine spmd_ialltoall_reals(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          real, dimension(:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoall(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_reals

! ======================================================================================================================
!>  \brief Non-blocking alltoall of integer       array
        subroutine spmd_ialltoall_ints(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          integer, dimension(:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoall(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_ints

! ======================================================================================================================
!>  \brief Non-blocking alltoall of double precision       array
        subroutine spmd_ialltoall_doubles(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          double precision, dimension(:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoall(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, used_comm,&
          & request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_doubles

! ======================================================================================================================
!>  \brief Non-blocking alltoall of real       array
        subroutine spmd_ialltoall_reals2d(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          real, dimension(:,:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoall(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_reals2d

! ======================================================================================================================
!>  \brief Non-blocking alltoall of integer       array
        subroutine spmd_ialltoall_ints2d(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoall(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_ints2d

! ======================================================================================================================
!>  \brief Non-blocking alltoall of double precision       array
        subroutine spmd_ialltoall_doubles2d(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          double precision, dimension(:,:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoall(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, used_comm,&
          & request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking alltoall of real       scalar
        subroutine spmd_ialltoall_real(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          real,  intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Ialltoall(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_real

! ======================================================================================================================
!>  \brief Non-blocking alltoall of integer       scalar
        subroutine spmd_ialltoall_int(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          integer,  intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Ialltoall(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_int

! ======================================================================================================================
!>  \brief Non-blocking alltoall of double precision       scalar
        subroutine spmd_ialltoall_double(sendbuf, recvbuf, sendcount, recvcount, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          double precision,  intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALL
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoall")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Ialltoall(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, used_comm,&
            & request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoall_double

      end module spmd_ialltoall_mod
