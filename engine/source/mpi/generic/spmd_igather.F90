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
      module spmd_igather_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_IGATHER = -35

        !> \brief Interface for spmd_igather, a wrapper for MPI_IGATHER
        interface spmd_igather
          module procedure spmd_igather_reals
          module procedure spmd_igather_ints
          module procedure spmd_igather_doubles
          module procedure spmd_igather_reals2d
          module procedure spmd_igather_ints2d
          module procedure spmd_igather_doubles2d
          module procedure spmd_igather_real
          module procedure spmd_igather_int
          module procedure spmd_igather_double
        end interface spmd_igather

      contains

! ======================================================================================================================
!>  \brief Non-blocking gather of real       array
        subroutine spmd_igather_reals(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Igather(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_reals

! ======================================================================================================================
!>  \brief Non-blocking gather of integer       array
        subroutine spmd_igather_ints(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Igather(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_ints

! ======================================================================================================================
!>  \brief Non-blocking gather of double precision       array
        subroutine spmd_igather_doubles(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Igather(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root, used_comm,&
          & request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_doubles

! ======================================================================================================================
!>  \brief Non-blocking gather of real       array
        subroutine spmd_igather_reals2d(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Igather(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_reals2d

! ======================================================================================================================
!>  \brief Non-blocking gather of integer       array
        subroutine spmd_igather_ints2d(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Igather(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_ints2d

! ======================================================================================================================
!>  \brief Non-blocking gather of double precision       array
        subroutine spmd_igather_doubles2d(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Igather(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root, used_comm,&
          & request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking gather of real       scalar
        subroutine spmd_igather_real(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Igather(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_real

! ======================================================================================================================
!>  \brief Non-blocking gather of integer       scalar
        subroutine spmd_igather_int(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Igather(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_int

! ======================================================================================================================
!>  \brief Non-blocking gather of double precision       scalar
        subroutine spmd_igather_double(sendbuf, recvbuf, sendcount, recvcount, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
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
            tag_local = TAG_IGATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Igather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Igather(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root, used_comm,&
            & request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_igather_double

      end module spmd_igather_mod
