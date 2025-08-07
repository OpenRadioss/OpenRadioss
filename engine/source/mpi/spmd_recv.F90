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
!||    spmd_recv_mod              ../engine/source/mpi/spmd_recv.F90
!||--- called by ------------------------------------------------------
!||    spmd_gather_nodal_scalar   ../engine/source/mpi/nodes/spmd_gather_nodal_scalar.F
!||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_recv_mod
        implicit none

        interface spmd_recv
          module procedure spmd_recv_reals      !< Receives real numbers
          module procedure spmd_recv_reals2D      !< Receives real numbers
          module procedure spmd_recv_ints       !< Receives integers
          module procedure spmd_recv_doubles    !< Receives double precision numbers
          module procedure spmd_recv_doubles2D   !< Receives double precision numbers
          module procedure spmd_recv_real       !< Receives a single real number
          module procedure spmd_recv_int        !< Receives a single integer
          module procedure spmd_recv_double     !< Receives a single double precision number
        end interface spmd_recv

      contains

! ======================================================================================================================
!||====================================================================
!||    spmd_recv_reals       ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_reals(buf, buf_count, source, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count), intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_recv_reals2d     ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_reals2D(buf, buf_count, source, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count,1), intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_reals2D

! ======================================================================================================================
!||====================================================================
!||    spmd_recv_ints        ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_ints(buf, buf_count, source, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(buf_count), intent(inout) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_recv_doubles     ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_doubles(buf, buf_count, source, tag,  comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count), intent(inout) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_doubles
! ======================================================================================================================
!||====================================================================
!||    spmd_recv_doubles2d   ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_doubles2D(buf, buf_count, source, tag,  comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count,1), intent(inout) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_doubles2D

!||====================================================================
!||    spmd_recv_real        ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_real(buf, buf_count, source, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_real

!||====================================================================
!||    spmd_recv_int         ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_int(buf, buf_count, source, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer, intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INTEGER, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_int
!||====================================================================
!||    spmd_recv_double      ../engine/source/mpi/spmd_recv.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_recv_double(buf, buf_count, source, tag, comm)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_double
      end module spmd_recv_mod

