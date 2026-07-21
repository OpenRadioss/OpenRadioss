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
      module spmd_scatterv_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_SCATTERV = -22

        !> \brief Interface for spmd_scatterv, a wrapper for MPI_SCATTERV
        interface spmd_scatterv
          module procedure spmd_scatterv_reals
          module procedure spmd_scatterv_ints
          module procedure spmd_scatterv_doubles
          module procedure spmd_scatterv_reals2d
          module procedure spmd_scatterv_ints2d
          module procedure spmd_scatterv_doubles2d
          module procedure spmd_scatterv_real
          module procedure spmd_scatterv_int
          module procedure spmd_scatterv_double
        end interface spmd_scatterv

      contains

! ======================================================================================================================
!>  \brief Scatterv of real       array
        subroutine spmd_scatterv_reals(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_reals

! ======================================================================================================================
!>  \brief Scatterv of integer       array
        subroutine spmd_scatterv_ints(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_ints

! ======================================================================================================================
!>  \brief Scatterv of double precision       array
        subroutine spmd_scatterv_doubles(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_doubles

! ======================================================================================================================
!>  \brief Scatterv of real       array
        subroutine spmd_scatterv_reals2d(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_reals2d

! ======================================================================================================================
!>  \brief Scatterv of integer       array
        subroutine spmd_scatterv_ints2d(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_ints2d

! ======================================================================================================================
!>  \brief Scatterv of double precision       array
        subroutine spmd_scatterv_doubles2d(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_doubles2d

! ======================================================================================================================
!>  \brief Scatterv of real       scalar
        subroutine spmd_scatterv_real(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real,  intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_REAL, recvbuf, recvcount, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_real

! ======================================================================================================================
!>  \brief Scatterv of integer       scalar
        subroutine spmd_scatterv_int(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer,  intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_int

! ======================================================================================================================
!>  \brief Scatterv of double precision       scalar
        subroutine spmd_scatterv_double(sendbuf, buf_count, sendcounts, displs, recvbuf, recvcount, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision,  intent(in) :: sendbuf
          integer, intent(in) :: buf_count
          integer, intent(in) :: sendcounts(:), displs(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcount, root
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SCATTERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Scatterv")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Scatterv(sendbuf, sendcounts, displs, MPI_DOUBLE_PRECISION, recvbuf, recvcount, MPI_DOUBLE_PRECISION, root,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          if (recvcount > 0) recvbuf(1:recvcount) = sendbuf(1:recvcount)
#endif
        end subroutine spmd_scatterv_double

      end module spmd_scatterv_mod
