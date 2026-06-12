!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    spmd_alltoallv_mod   ../engine/source/mpi/generic/spmd_alltoallv.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod             ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_alltoallv_mod
        implicit none

        ! \brief Interface for spmd_alltoallv, a wrapper for MPI_ALLTOALLV
        interface spmd_alltoallv
          module procedure spmd_alltoallv_reals    !< Alltoallv of real numbers
          module procedure spmd_alltoallv_ints     !< Alltoallv of integers
          module procedure spmd_alltoallv_doubles  !< Alltoallv of double precision numbers
        end interface spmd_alltoallv

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!||====================================================================
!||    spmd_alltoallv_reals   ../engine/source/mpi/generic/spmd_alltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoallv_reals(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm)
          use spmd_comm_world_mod, only : SPMD_COMM_WORLD
          use spmd_error_mod, only : spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          real, dimension(*), intent(in)    :: sendbuf
          integer, dimension(*), intent(in) :: sendcounts, sdispls
          real, dimension(*), intent(inout) :: recvbuf
          integer, dimension(*), intent(in) :: recvcounts, rdispls
          integer, intent(in), optional     :: comm
          integer :: ierr, tag, used_comm
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_REAL, &
            recvbuf, recvcounts, rdispls, MPI_REAL, used_comm, ierr)
          call spmd_out(tag, ierr)
#else
          recvbuf(1:sendcounts(1)) = sendbuf(1:sendcounts(1))
#endif
        end subroutine spmd_alltoallv_reals

! ======================================================================================================================
!||====================================================================
!||    spmd_alltoallv_ints    ../engine/source/mpi/generic/spmd_alltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoallv_ints(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm)
          use spmd_comm_world_mod, only : SPMD_COMM_WORLD
          use spmd_error_mod, only : spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, dimension(*), intent(in)    :: sendbuf
          integer, dimension(*), intent(in)    :: sendcounts, sdispls
          integer, dimension(*), intent(inout) :: recvbuf
          integer, dimension(*), intent(in)    :: recvcounts, rdispls
          integer, intent(in), optional        :: comm
          integer :: ierr, tag, used_comm
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_INTEGER, &
            recvbuf, recvcounts, rdispls, MPI_INTEGER, used_comm, ierr)
          call spmd_out(tag, ierr)
#else
          recvbuf(1:sendcounts(1)) = sendbuf(1:sendcounts(1))
#endif
        end subroutine spmd_alltoallv_ints

! ======================================================================================================================
!||====================================================================
!||    spmd_alltoallv_doubles   ../engine/source/mpi/generic/spmd_alltoallv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod     ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod          ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoallv_doubles(sendbuf, sendcounts, sdispls, recvbuf, recvcounts, rdispls, comm)
          use spmd_comm_world_mod, only : SPMD_COMM_WORLD
          use spmd_error_mod, only : spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, dimension(*), intent(in)    :: sendbuf
          integer, dimension(*), intent(in)             :: sendcounts, sdispls
          double precision, dimension(*), intent(inout) :: recvbuf
          integer, dimension(*), intent(in)             :: recvcounts, rdispls
          integer, intent(in), optional                 :: comm
          integer :: ierr, tag, used_comm
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Alltoallv(sendbuf, sendcounts, sdispls, MPI_DOUBLE_PRECISION, &
            recvbuf, recvcounts, rdispls, MPI_DOUBLE_PRECISION, used_comm, ierr)
          call spmd_out(tag, ierr)
#else
          recvbuf(1:sendcounts(1)) = sendbuf(1:sendcounts(1))
#endif
        end subroutine spmd_alltoallv_doubles

      end module spmd_alltoallv_mod
