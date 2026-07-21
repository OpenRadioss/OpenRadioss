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
      module spmd_allgatherv_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_ALLGATHERV = -23

        !> \\brief Interface for spmd_allgatherv, a wrapper for MPI_ALLGATHERV
        interface spmd_allgatherv
          module procedure spmd_allgatherv_reals
          module procedure spmd_allgatherv_ints
          module procedure spmd_allgatherv_doubles
          module procedure spmd_allgatherv_reals2d
          module procedure spmd_allgatherv_ints2d
          module procedure spmd_allgatherv_doubles2d
          module procedure spmd_allgatherv_real
          module procedure spmd_allgatherv_int
          module procedure spmd_allgatherv_double
        end interface spmd_allgatherv

      contains

! ======================================================================================================================
!>  \\brief Allgatherv of real       array
        subroutine spmd_allgatherv_reals(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real, dimension(:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_reals

! ======================================================================================================================
!>  \\brief Allgatherv of integer       array
        subroutine spmd_allgatherv_ints(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          integer, dimension(:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_ints

! ======================================================================================================================
!>  \\brief Allgatherv of double precision       array
        subroutine spmd_allgatherv_doubles(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          double precision, dimension(:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_doubles

! ======================================================================================================================
!>  \\brief Allgatherv of real       array
        subroutine spmd_allgatherv_reals2d(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real, dimension(:,:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_reals2d

! ======================================================================================================================
!>  \\brief Allgatherv of integer       array
        subroutine spmd_allgatherv_ints2d(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_ints2d

! ======================================================================================================================
!>  \\brief Allgatherv of double precision       array
        subroutine spmd_allgatherv_doubles2d(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          double precision, dimension(:,:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_doubles2d

! ======================================================================================================================
!>  \\brief Allgatherv of real       scalar
        subroutine spmd_allgatherv_real(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real,  intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_real

! ======================================================================================================================
!>  \\brief Allgatherv of integer       scalar
        subroutine spmd_allgatherv_int(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          integer,  intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_int

! ======================================================================================================================
!>  \\brief Allgatherv of double precision       scalar
        subroutine spmd_allgatherv_double(sendbuf, sendcount, recvbuf, recvcounts, displs, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          double precision,  intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_ALLGATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Allgatherv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (sendcount > 0) recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_double

      end module spmd_allgatherv_mod
