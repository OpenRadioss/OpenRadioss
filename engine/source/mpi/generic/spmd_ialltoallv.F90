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
!||====================================================================
!||    spmd_ialltoallv_mod   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      module spmd_ialltoallv_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_IALLTOALLV = -39

        !> \brief Interface for spmd_ialltoallv, a wrapper for MPI_IALLTOALLV
        interface spmd_ialltoallv
          module procedure spmd_ialltoallv_reals
          module procedure spmd_ialltoallv_ints
          module procedure spmd_ialltoallv_doubles
          module procedure spmd_ialltoallv_reals2d
          module procedure spmd_ialltoallv_ints2d
          module procedure spmd_ialltoallv_doubles2d
          module procedure spmd_ialltoallv_real
          module procedure spmd_ialltoallv_int
          module procedure spmd_ialltoallv_double
        end interface spmd_ialltoallv

      contains

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of real       array
!||====================================================================
!||    spmd_ialltoallv_reals   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod          ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_reals(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_REAL, &
            recvbuf, recvcounts, rdispls, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_reals

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of integer       array
!||====================================================================
!||    spmd_ialltoallv_ints   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                ../engine/source/mpi/spmd_error.F90
!||    spmd_out               ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod         ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_ints(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_INTEGER, &
            recvbuf, recvcounts, rdispls, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_ints

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of double precision       array
!||====================================================================
!||    spmd_ialltoallv_doubles   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                   ../engine/source/mpi/spmd_error.F90
!||    spmd_out                  ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod            ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_doubles(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_DOUBLE_PRECISION, &
            recvbuf, recvcounts, rdispls, MPI_DOUBLE_PRECISION, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_doubles

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of real       array
!||====================================================================
!||    spmd_ialltoallv_reals2d   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                   ../engine/source/mpi/spmd_error.F90
!||    spmd_out                  ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod            ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_reals2d(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_REAL, &
            recvbuf, recvcounts, rdispls, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_reals2d

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of integer       array
!||====================================================================
!||    spmd_ialltoallv_ints2d   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                  ../engine/source/mpi/spmd_error.F90
!||    spmd_out                 ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod           ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_ints2d(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_INTEGER, &
            recvbuf, recvcounts, rdispls, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_ints2d

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of double precision       array
!||====================================================================
!||    spmd_ialltoallv_doubles2d   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                     ../engine/source/mpi/spmd_error.F90
!||    spmd_out                    ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod              ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_doubles2d(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(:,:), intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_DOUBLE_PRECISION, &
            recvbuf, recvcounts, rdispls, MPI_DOUBLE_PRECISION, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of real       scalar
!||====================================================================
!||    spmd_ialltoallv_real   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                ../engine/source/mpi/spmd_error.F90
!||    spmd_out               ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod         ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_real(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real,  intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          real, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_REAL, &
            recvbuf, recvcounts, rdispls, MPI_REAL, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_real

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of integer       scalar
!||====================================================================
!||    spmd_ialltoallv_int   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_int(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer,  intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          integer, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_INTEGER, &
            recvbuf, recvcounts, rdispls, MPI_INTEGER, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_int

! ======================================================================================================================
!>  \brief Non-blocking alltoallv of double precision       scalar
!||====================================================================
!||    spmd_ialltoallv_double   ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                  ../engine/source/mpi/spmd_error.F90
!||    spmd_out                 ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod           ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_ialltoallv_double(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, request, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision,  intent(in) :: sendbuf
          integer, intent(in) :: sendcounts(:), sdispls(:)
          double precision, dimension(:), intent(inout) :: recvbuf
          integer, intent(in) :: recvcounts(:), rdispls(:)
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag ! for spmd_in/out
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_IALLTOALLV
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Ialltoallv")

          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Ialltoallv(sendbuf, sendcounts, sdispls, MPI_DOUBLE_PRECISION, &
            recvbuf, recvcounts, rdispls, MPI_DOUBLE_PRECISION, used_comm, request, ierr)

          call spmd_out(tag_local, ierr)
#else
          request = 0
#endif
        end subroutine spmd_ialltoallv_double

      end module spmd_ialltoallv_mod
