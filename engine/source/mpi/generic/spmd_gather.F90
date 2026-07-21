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
      module spmd_gather_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_GATHER = -16

        !> \brief Interface for spmd_gather, a wrapper for MPI_GATHER
        interface spmd_gather
          module procedure spmd_gather_reals
          module procedure spmd_gather_ints
          module procedure spmd_gather_doubles
          module procedure spmd_gather_reals2d
          module procedure spmd_gather_ints2d
          module procedure spmd_gather_doubles2d
          module procedure spmd_gather_real
          module procedure spmd_gather_int
          module procedure spmd_gather_double
        end interface spmd_gather

      contains

! ======================================================================================================================
!>  \brief Gather of real       array
        subroutine spmd_gather_reals(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          real, dimension(:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gather(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_reals

! ======================================================================================================================
!>  \brief Gather of integer       array
        subroutine spmd_gather_ints(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          integer, dimension(:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gather(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_ints

! ======================================================================================================================
!>  \brief Gather of double precision       array
        subroutine spmd_gather_doubles(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          double precision, dimension(:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gather(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_doubles

! ======================================================================================================================
!>  \brief Gather of real       array
        subroutine spmd_gather_reals2d(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          real, dimension(:,:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gather(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_reals2d

! ======================================================================================================================
!>  \brief Gather of integer       array
        subroutine spmd_gather_ints2d(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gather(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_ints2d

! ======================================================================================================================
!>  \brief Gather of double precision       array
        subroutine spmd_gather_doubles2d(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          double precision, dimension(:,:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gather(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_doubles2d

! ======================================================================================================================
!>  \brief Gather of real       scalar
        subroutine spmd_gather_real(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          real,  intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Gather(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_real

! ======================================================================================================================
!>  \brief Gather of integer       scalar
        subroutine spmd_gather_int(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          integer,  intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Gather(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_int

! ======================================================================================================================
!>  \brief Gather of double precision       scalar
        subroutine spmd_gather_double(sendbuf, recvbuf, sendcount, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount, root
          double precision,  intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gather")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (sendcount .ne. 1) then
            ierr = -1
          else
            call MPI_Gather(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root, used_comm,&
            & ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gather_double

      end module spmd_gather_mod
