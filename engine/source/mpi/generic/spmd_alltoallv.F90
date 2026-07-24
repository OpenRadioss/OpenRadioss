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
      module spmd_alltoallv_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_ALLTOALLV = -15

        !> \brief Interface for spmd_alltoallv, a wrapper for MPI_ALLTOALLV
        interface spmd_alltoallv
          module procedure spmd_alltoallv_reals
          module procedure spmd_alltoallv_ints
          module procedure spmd_alltoallv_doubles
          module procedure spmd_alltoallv_reals2d
          module procedure spmd_alltoallv_ints2d
          module procedure spmd_alltoallv_doubles2d
          module procedure spmd_alltoallv_real
          module procedure spmd_alltoallv_int
          module procedure spmd_alltoallv_double
        end interface spmd_alltoallv

      contains

! ======================================================================================================================
!>  \brief Alltoallv of real       array
        subroutine spmd_alltoallv_reals(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_REAL, &
            recvbuf, recvcounts, rdispls, MPI_REAL, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_reals

! ======================================================================================================================
!>  \brief Alltoallv of integer       array
        subroutine spmd_alltoallv_ints(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_INTEGER, &
            recvbuf, recvcounts, rdispls, MPI_INTEGER, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_ints

! ======================================================================================================================
!>  \brief Alltoallv of double precision       array
        subroutine spmd_alltoallv_doubles(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_DOUBLE_PRECISION, &
            recvbuf, recvcounts, rdispls, MPI_DOUBLE_PRECISION, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_doubles

! ======================================================================================================================
!>  \brief Alltoallv of real       array
        subroutine spmd_alltoallv_reals2d(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_REAL, &
            recvbuf, recvcounts, rdispls, MPI_REAL, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_reals2d

! ======================================================================================================================
!>  \brief Alltoallv of integer       array
        subroutine spmd_alltoallv_ints2d(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_INTEGER, &
            recvbuf, recvcounts, rdispls, MPI_INTEGER, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_ints2d

! ======================================================================================================================
!>  \brief Alltoallv of double precision       array
        subroutine spmd_alltoallv_doubles2d(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_DOUBLE_PRECISION, &
            recvbuf, recvcounts, rdispls, MPI_DOUBLE_PRECISION, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_doubles2d

! ======================================================================================================================
!>  \brief Alltoallv of real       scalar
        subroutine spmd_alltoallv_real(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real,  intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_REAL, &
            recvbuf, recvcounts, rdispls, MPI_REAL, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_real

! ======================================================================================================================
!>  \brief Alltoallv of integer       scalar
        subroutine spmd_alltoallv_int(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer,  intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_INTEGER, &
            recvbuf, recvcounts, rdispls, MPI_INTEGER, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_int

! ======================================================================================================================
!>  \brief Alltoallv of double precision       scalar
        subroutine spmd_alltoallv_double(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision,  intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Alltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_DOUBLE_PRECISION, &
            recvbuf, recvcounts, rdispls, MPI_DOUBLE_PRECISION, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_alltoallv_double

      end module spmd_alltoallv_mod
