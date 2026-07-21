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
      module spmd_ibcast_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_IBCAST = -34

        !> \brief Interface for spmd_ibcast, a wrapper for MPI_IBCAST
        interface spmd_ibcast
          module procedure spmd_ibcast_reals
          module procedure spmd_ibcast_ints
          module procedure spmd_ibcast_doubles
          module procedure spmd_ibcast_reals2d
          module procedure spmd_ibcast_ints2d
          module procedure spmd_ibcast_doubles2d
          module procedure spmd_ibcast_real
          module procedure spmd_ibcast_int
          module procedure spmd_ibcast_double
        end interface spmd_ibcast

      contains

! ======================================================================================================================
!>  \brief Non-blocking broadcast of real       array
        subroutine spmd_ibcast_reals(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:), intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ibcast(buf, buf_count, MPI_REAL, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_reals

! ======================================================================================================================
!>  \brief Non-blocking broadcast of integer       array
        subroutine spmd_ibcast_ints(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:), intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ibcast(buf, buf_count, MPI_INTEGER, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_ints

! ======================================================================================================================
!>  \brief Non-blocking broadcast of double precision       array
        subroutine spmd_ibcast_doubles(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:), intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ibcast(buf, buf_count, MPI_DOUBLE_PRECISION, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_doubles

! ======================================================================================================================
!>  \brief Non-blocking broadcast of real       array
        subroutine spmd_ibcast_reals2d(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:,:), intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ibcast(buf, buf_count, MPI_REAL, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_reals2d

! ======================================================================================================================
!>  \brief Non-blocking broadcast of integer       array
        subroutine spmd_ibcast_ints2d(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:,:), intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ibcast(buf, buf_count, MPI_INTEGER, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_ints2d

! ======================================================================================================================
!>  \brief Non-blocking broadcast of double precision       array
        subroutine spmd_ibcast_doubles2d(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:,:), intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ibcast(buf, buf_count, MPI_DOUBLE_PRECISION, root, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking broadcast of real       scalar
        subroutine spmd_ibcast_real(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real,  intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Ibcast(buf, buf_count, MPI_REAL, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_real

! ======================================================================================================================
!>  \brief Non-blocking broadcast of integer       scalar
        subroutine spmd_ibcast_int(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer,  intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Ibcast(buf, buf_count, MPI_INTEGER, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_int

! ======================================================================================================================
!>  \brief Non-blocking broadcast of double precision       scalar
        subroutine spmd_ibcast_double(buf, buf_count, root, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision,  intent(inout) :: buf
          integer, intent(in) :: buf_count, root
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IBCAST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ibcast")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            call MPI_Ibcast(buf, buf_count, MPI_DOUBLE_PRECISION, root, used_comm, request, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ibcast_double

      end module spmd_ibcast_mod
