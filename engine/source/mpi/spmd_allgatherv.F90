!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    spmd_allgatherv_mod   ../engine/source/mpi/spmd_allgatherv.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_allgatherv_mod
        implicit none

        ! \brief Interface for spmd_allgatherv, a wrapper for MPI_ALLGATHERV
        interface spmd_allgatherv
          module procedure spmd_allgatherv_reals     !< Allgatherv of real numbers
          module procedure spmd_allgatherv_ints      !< Allgatherv of integers
          module procedure spmd_allgatherv_doubles   !< Allgatherv of double precision numbers
          module procedure spmd_allgatherv_real      !< Allgatherv of a single real number (for legacy A(1,1) calls)
          module procedure spmd_allgatherv_int       !< Allgatherv of a single integer (for legacy A(1,1) calls)
          module procedure spmd_allgatherv_double    !< Allgatherv of a single double precision number (for legacy A(1,1) calls)
        end interface spmd_allgatherv

      contains
! ======================================================================================================================
!||====================================================================
!||    spmd_allgatherv_reals   ../engine/source/mpi/spmd_allgatherv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod     ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod          ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allgatherv_reals(sendbuf, sendcount, recvbuf, recvcounts, displs, comm)
          use spmd_comm_world_mod, only : spmd_comm_world
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real, dimension(*), intent(in) :: sendbuf
          integer, dimension(*), intent(in) :: recvcounts, displs
          real, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0  ! Allgatherv doesn't use tags, but keeping for consistency with spmd_in/out
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Allgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, comm, ierr)
          else
            call MPI_Allgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, SPMD_COMM_WORLD, ierr)
          endif
          call spmd_out(tag,ierr)
#else
          recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_allgatherv_ints   ../engine/source/mpi/spmd_allgatherv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                ../engine/source/mpi/spmd_error.F90
!||    spmd_out               ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod    ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod         ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allgatherv_ints(sendbuf, sendcount, recvbuf, recvcounts, displs, comm)
          use spmd_comm_world_mod, only : spmd_comm_world
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          integer, dimension(*), intent(in) :: sendbuf
          integer, dimension(*), intent(in) :: recvcounts, displs
          integer, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr, tag
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Allgatherv(sendbuf, sendcount, MPI_INT, recvbuf, recvcounts, displs, MPI_INT, comm, ierr)
          else
            call MPI_Allgatherv(sendbuf, sendcount, MPI_INT, recvbuf, recvcounts, displs, MPI_INT, SPMD_COMM_WORLD, ierr)
          endif
          call spmd_out(tag,ierr)
#else
          recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_allgatherv_doubles   ../engine/source/mpi/spmd_allgatherv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                   ../engine/source/mpi/spmd_error.F90
!||    spmd_out                  ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod       ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod            ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allgatherv_doubles(sendbuf, sendcount, recvbuf, recvcounts, displs, comm)
          use spmd_comm_world_mod, only : spmd_comm_world
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          double precision, dimension(*), intent(in) :: sendbuf
          integer, dimension(*), intent(in) :: recvcounts, displs
          double precision, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr, tag
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Allgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, &
                      recvbuf, recvcounts, displs,              &
                      MPI_DOUBLE_PRECISION, comm, ierr)
            else
            call MPI_Allgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, &
                      recvbuf, recvcounts, displs,              &
                      MPI_DOUBLE_PRECISION, SPMD_COMM_WORLD, ierr)
          endif
          call spmd_out(tag,ierr)
#else
            recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_allgatherv_doubles
!||====================================================================
!||    spmd_allgatherv_double   ../engine/source/mpi/spmd_allgatherv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                  ../engine/source/mpi/spmd_error.F90
!||    spmd_out                 ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod      ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod           ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allgatherv_double(sendbuf, sendcount, recvbuf, recvcounts, displs, comm)
          use spmd_comm_world_mod, only : spmd_comm_world
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf
          integer, intent(in) :: sendcount
          integer, dimension(*), intent(in) :: recvcounts, displs
          double precision, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Allgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs,&
             MPI_DOUBLE_PRECISION, comm, ierr)
          else
            call MPI_Allgatherv(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, &
            recvcounts, displs, MPI_DOUBLE_PRECISION, SPMD_COMM_WORLD, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_allgatherv_double
!||====================================================================
!||    spmd_allgatherv_int   ../engine/source/mpi/spmd_allgatherv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allgatherv_int(sendbuf, sendcount, recvbuf, recvcounts, displs, comm)
          use spmd_comm_world_mod, only : spmd_comm_world
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf
          integer, intent(in) :: sendcount
          integer, dimension(*), intent(in) :: recvcounts, displs
          integer, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Allgatherv(sendbuf, sendcount, MPI_INT, recvbuf, recvcounts, displs, MPI_INT, comm, ierr)
          else
            call MPI_Allgatherv(sendbuf, sendcount, MPI_INT, recvbuf, recvcounts, &
            displs, MPI_INT, SPMD_COMM_WORLD, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_allgatherv_int
!||====================================================================
!||    spmd_allgatherv_real   ../engine/source/mpi/spmd_allgatherv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                ../engine/source/mpi/spmd_error.F90
!||    spmd_out               ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod    ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod         ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allgatherv_real(sendbuf, sendcount, recvbuf, recvcounts, displs, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only : spmd_comm_world
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount
          real,    intent(in) :: sendbuf
          integer, dimension(*), intent(in) :: recvcounts, displs
          real, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Allgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, comm, ierr)
          else
            call MPI_Allgatherv(sendbuf, sendcount, MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, SPMD_COMM_WORLD, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_allgatherv_real



      end module spmd_allgatherv_mod