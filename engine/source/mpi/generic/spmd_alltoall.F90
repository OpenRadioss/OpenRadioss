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
!||    spmd_alltoall_mod   ../engine/source/mpi/generic/spmd_alltoall.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod            ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_alltoall_mod
        implicit none

        ! \brief Interface for spmd_alltoall, a wrapper for MPI_ALLTOALL
        interface spmd_alltoall
          module procedure spmd_alltoall_reals     !< Alltoall of real numbers
          module procedure spmd_alltoall_ints      !< Alltoall of integers
          module procedure spmd_alltoall_doubles   !< Alltoall of double precision numbers
          module procedure spmd_alltoall_real      !< Alltoall of a single real number (for legacy A(1,1) calls)
          module procedure spmd_alltoall_int       !< Alltoall of a single integer (for legacy A(1,1) calls)
          module procedure spmd_alltoall_double    !< Alltoall of a single double precision number (for legacy A(1,1) calls)
        end interface spmd_alltoall

      contains
! ======================================================================================================================
!||====================================================================
!||    spmd_alltoall_reals   ../engine/source/mpi/generic/spmd_alltoall.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoall_reals(sendbuf, sendcount, recvbuf, recvcount, comm)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          real, dimension(*), intent(in) :: sendbuf
          real, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0  ! Alltoall doesn't use tags, but keeping for consistency with spmd_in/out
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Alltoall(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, comm, ierr)
          else
            call MPI_Alltoall(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#else
          recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_alltoall_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_alltoall_ints    ../engine/source/mpi/generic/spmd_alltoall.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoall_ints(sendbuf, sendcount, recvbuf, recvcount, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          integer, dimension(*), intent(in) :: sendbuf
          integer, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr, tag
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Alltoall(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, comm, ierr)
          else
            call MPI_Alltoall(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#else
          recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_alltoall_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_alltoall_doubles   ../engine/source/mpi/generic/spmd_alltoall.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod     ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod          ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoall_doubles(sendbuf, sendcount, recvbuf, recvcount, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          double precision, dimension(*), intent(in) :: sendbuf
          double precision, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr, tag
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Alltoall(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount,&
              MPI_DOUBLE_PRECISION, comm, ierr)
          else
            call MPI_Alltoall(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf, recvcount,&
              MPI_DOUBLE_PRECISION, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#else
          recvbuf(1:sendcount) = sendbuf(1:sendcount)
#endif
        end subroutine spmd_alltoall_doubles
!||====================================================================
!||    spmd_alltoall_double   ../engine/source/mpi/generic/spmd_alltoall.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                ../engine/source/mpi/spmd_error.F90
!||    spmd_out               ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod    ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod         ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoall_double(sendbuf, sendcount, recvbuf, recvcount, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf
          integer, intent(in) :: sendcount, recvcount
          double precision, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Alltoall(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf,&
              recvcount, MPI_DOUBLE_PRECISION, comm, ierr)
          else
            call MPI_Alltoall(sendbuf, sendcount, MPI_DOUBLE_PRECISION, recvbuf,&
              recvcount, MPI_DOUBLE_PRECISION, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_alltoall_double
!||====================================================================
!||    spmd_alltoall_int     ../engine/source/mpi/generic/spmd_alltoall.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoall_int(sendbuf, sendcount, recvbuf, recvcount, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf
          integer, intent(in) :: sendcount, recvcount
          integer, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Alltoall(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount, MPI_INTEGER, comm, ierr)
          else
            call MPI_Alltoall(sendbuf, sendcount, MPI_INTEGER, recvbuf, recvcount,&
              MPI_INTEGER, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_alltoall_int
!||====================================================================
!||    spmd_alltoall_real    ../engine/source/mpi/generic/spmd_alltoall.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_alltoall_real(sendbuf, sendcount, recvbuf, recvcount, comm)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendcount, recvcount
          real,    intent(in) :: sendbuf
          real, dimension(*), intent(inout) :: recvbuf
          integer, intent(in), optional :: comm
          integer :: ierr, tag
#ifdef MPI
          tag = 0
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Alltoall(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, comm, ierr)
          else
            call MPI_Alltoall(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount,&
              MPI_REAL, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_alltoall_real



      end module spmd_alltoall_mod
