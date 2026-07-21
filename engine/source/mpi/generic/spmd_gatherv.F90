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
      module spmd_gatherv_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_GATHERV = -17

        !> \brief Interface for spmd_gatherv, a wrapper for MPI_GATHERV
        interface spmd_gatherv
          module procedure spmd_gatherv_reals
          module procedure spmd_gatherv_ints
          module procedure spmd_gatherv_doubles
          module procedure spmd_gatherv_reals2d
          module procedure spmd_gatherv_ints2d
          module procedure spmd_gatherv_doubles2d
          module procedure spmd_gatherv_real
          module procedure spmd_gatherv_int
          module procedure spmd_gatherv_double
        end interface spmd_gatherv

      contains

! ======================================================================================================================
!>  \brief Gatherv of real       array
        subroutine spmd_gatherv_reals(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          real, dimension(:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_reals

! ======================================================================================================================
!>  \brief Gatherv of integer       array
        subroutine spmd_gatherv_ints(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          integer, dimension(:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_ints

! ======================================================================================================================
!>  \brief Gatherv of double precision       array
        subroutine spmd_gatherv_doubles(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          double precision, dimension(:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION, root,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_doubles

! ======================================================================================================================
!>  \brief Gatherv of real       array
        subroutine spmd_gatherv_reals2d(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          real, dimension(:,:), intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_reals2d

! ======================================================================================================================
!>  \brief Gatherv of integer       array
        subroutine spmd_gatherv_ints2d(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_ints2d

! ======================================================================================================================
!>  \brief Gatherv of double precision       array
        subroutine spmd_gatherv_doubles2d(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          double precision, dimension(:,:), intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION, root,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_doubles2d

! ======================================================================================================================
!>  \brief Gatherv of real       scalar
        subroutine spmd_gatherv_real(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          real,  intent(in) :: sendbuf
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_real

! ======================================================================================================================
!>  \brief Gatherv of integer       scalar
        subroutine spmd_gatherv_int(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          integer,  intent(in) :: sendbuf
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcounts, displs, MPI_INTEGER, root, used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_int

! ======================================================================================================================
!>  \brief Gatherv of double precision       scalar
        subroutine spmd_gatherv_double(sendbuf, sendcount, recvbuf, recvcounts, displs, root, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, root
          double precision,  intent(in) :: sendbuf
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), displs(:)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GATHERV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Gatherv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Gatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, MPI_DOUBLE_PRECISION, root,&
          & used_comm, ierr)

          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_gatherv_double

      end module spmd_gatherv_mod
