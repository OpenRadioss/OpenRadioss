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
      module spmd_irecv_mod
        implicit none

        ! \brief Interface for spmd_irecv, a wrapper for MPI_IRECV
        interface spmd_irecv
          module procedure spmd_irecv_reals     !< Non-blocking receive of real numbers
          module procedure spmd_irecv_ints      !< Non-blocking receive of integers
          module procedure spmd_irecv_doubles   !< Non-blocking receive of double precision numbers
          module procedure spmd_irecv_real      !< Non-blocking receive of a single real number
          module procedure spmd_irecv_int       !< Non-blocking receive of a single integer
          module procedure spmd_irecv_double    !< Non-blocking receive of a single double precision number
        end interface spmd_irecv

      contains
! ======================================================================================================================
        !||====================================================================
        !||    spmd_irecv_reals   ../engine/source/mpi/spmd_mod.F90
        !||--- calls      -----------------------------------------------------
        !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_irecv_reals(buf, buf_count, source, tag, request, comm)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count), intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_reals
! ======================================================================================================================
        !||====================================================================
        !||    spmd_irecv_ints   ../engine/source/mpi/spmd_mod.F90
        !||--- calls      -----------------------------------------------------
        !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_irecv_ints(buf, buf_count, source, tag, request, comm)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(buf_count), intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_ints
! ======================================================================================================================
        !||====================================================================
        !||    spmd_irecv_doubles   ../engine/source/mpi/spmd_mod.F90
        !||--- calls      -----------------------------------------------------
        !||    spmd_in              ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out             ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_irecv_doubles(buf, buf_count, source, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count), intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_doubles
        !||====================================================================
        !||    spmd_irecv_double   ../engine/source/mpi/spmd_mod.F90
        !||--- calls      -----------------------------------------------------
        !||    spmd_in             ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out            ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_irecv_double(buf, buf_count, source, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          double precision, intent(inout) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_double
        !||====================================================================
        !||    spmd_irecv_int   ../engine/source/mpi/spmd_mod.F90
        !||--- calls      -----------------------------------------------------
        !||    spmd_in          ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out         ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_irecv_int(buf, buf_count, source, tag, request, comm)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(inout) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_int
        !||====================================================================
        !||    spmd_irecv_real   ../engine/source/mpi/spmd_mod.F90
        !||--- calls      -----------------------------------------------------
        !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_irecv_real(buf, buf_count, source, tag, request, comm)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real,    intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_real



      end module spmd_irecv_mod

