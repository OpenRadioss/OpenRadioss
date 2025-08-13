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
!||    spmd_isend_mod   ../engine/source/mpi/spmd_isend.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod         ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_isend_mod
        implicit none

        ! \brief Interface for spmd_isend, a wrapper for MPI_ISEND
        interface spmd_isend
          module procedure spmd_isend_reals     !< Non-blocking send of real numbers
          module procedure spmd_isend_ints      !< Non-blocking send of integers
          module procedure spmd_isend_doubles   !< Non-blocking send of double precision numbers
          module procedure spmd_isend_real      !< Non-blocking send of a single real number
          module procedure spmd_isend_int       !< Non-blocking send of a single integer
          module procedure spmd_isend_double    !< Non-blocking send of a single double precision number
        end interface spmd_isend

      contains
!||====================================================================
!||    spmd_isend_reals      ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_reals(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real, dimension(buf_count), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_isend_ints       ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_ints(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, dimension(buf_count), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_isend_doubles    ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_doubles(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, dimension(buf_count), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_doubles

!||====================================================================
!||    spmd_isend_double     ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_double(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_double

!||====================================================================
!||    spmd_isend_int        ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_int(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_int

!||====================================================================
!||    spmd_isend_real       ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_real(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real, intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_real





      end module spmd_isend_mod

