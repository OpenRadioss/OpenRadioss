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
!||    spmd_allgather_mod    ../engine/source/mpi/generic/spmd_allgather.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      module spmd_allgather_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD

        implicit none
        ! Define the interface for spmd_allgahter

        integer, parameter, public :: TAG_ALLGATHER = -7


        ! \brief Interface for spmd_allgahter, a wrapper for MPI_ALLGATHER
        ! This interface allows for gathering data from all processes in a distributed system.
        ! It supports gathering real numbers, integers, and double precision numbers.
        interface spmd_allgather
          module procedure spmd_allgather_reals   !< Allgather real numbers across all processes
          module procedure spmd_allgather_ints    !< Allgather integers across all processes
          module procedure spmd_allgather_doubles !< Allgather double precision numbers across all processes
        end interface spmd_allgather

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

! ======================================================================================================================
!||====================================================================
!||    spmd_allgather_reals   ../engine/source/mpi/generic/spmd_allgather.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine spmd_allgather_reals(sendbuf, recvbuf, buf_count,comm)
          implicit none
#include "spmd.inc"
          real, dimension(*), intent(in) :: sendbuf
          real, dimension(*), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count
          integer, intent(in), optional :: comm
          integer :: ierr,  used_comm
#ifdef MPI
          !call spmd_in(TAG_ALLGATHER)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgather(sendbuf,buf_count, MPI_REAL, recvbuf, buf_count, MPI_REAL, used_comm, ierr)
          !call spmd_out(TAG_ALLGATHER,ierr)
#endif
        end subroutine spmd_allgather_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_allgather_doubles   ../engine/source/mpi/generic/spmd_allgather.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine spmd_allgather_doubles(sendbuf, recvbuf, buf_count,comm)
          implicit none
#include "spmd.inc"
          real(kind=8), dimension(*), intent(in) :: sendbuf
          real(kind=8), dimension(*), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count
          integer, intent(in), optional :: comm
          integer :: ierr,  used_comm
#ifdef MPI
          !call spmd_in(TAG_ALLGATHER)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgather(sendbuf,buf_count, MPI_DOUBLE_PRECISION, recvbuf, buf_count, MPI_DOUBLE_PRECISION, used_comm, ierr)
          !call spmd_out(TAG_ALLGATHER,ierr)
#endif
        end subroutine spmd_allgather_doubles
! ======================================================================================================================
!||====================================================================
!||    spmd_allgather_ints   ../engine/source/mpi/generic/spmd_allgather.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine spmd_allgather_ints(sendbuf, recvbuf, buf_count,comm)
          implicit none
#include "spmd.inc"
          integer, dimension(*), intent(in) :: sendbuf
          integer, dimension(*), intent(inout) :: recvbuf
          integer, intent(in) :: buf_count
          integer, intent(in), optional :: comm
          integer :: ierr,  used_comm
#ifdef MPI
          !call spmd_in(TAG_ALLGATHER)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          call MPI_Allgather(sendbuf,buf_count, MPI_INTEGER, recvbuf, buf_count, MPI_INTEGER, used_comm, ierr)
          !call spmd_out(TAG_ALLGATHER,ierr)
#endif
        end subroutine spmd_allgather_ints
      end module spmd_allgather_mod
