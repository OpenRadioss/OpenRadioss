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
!||    spmd_unpack_mod   ../engine/source/mpi/spmd_unpack.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod          ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_unpack_mod
        implicit none
        private
        interface spmd_unpack
          module procedure spmd_unpack_reals      !< Unpacks real numbers
          module procedure spmd_unpack_ints       !< Unpacks integers
          module procedure spmd_unpack_doubles    !< Unpacks double precision numbers
        end interface spmd_unpack
        public spmd_unpack
      contains
!||====================================================================
!||    spmd_unpack_reals     ../engine/source/mpi/spmd_unpack.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_unpack_reals(inbuf, insize, position, outbuf, outcount, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: insize, outcount
          integer, intent(inout) :: position
          integer, intent(in), optional :: comm
          integer, dimension(insize), intent(in) :: inbuf
          real, dimension(outcount), intent(out) :: outbuf
          integer :: ierr, tag
#ifdef MPI
          tag = 0  ! Default tag for unpack operations
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Unpack(inbuf, insize, position, outbuf, outcount, MPI_REAL, comm, ierr)
          else
            call MPI_Unpack(inbuf, insize, position, outbuf, outcount, MPI_REAL, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_unpack_reals
!||====================================================================
!||    spmd_unpack_ints      ../engine/source/mpi/spmd_unpack.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_unpack_ints(inbuf, insize, position, outbuf, outcount, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: insize, outcount
          integer, intent(inout) :: position
          integer, intent(in), optional :: comm
          integer, dimension(insize), intent(in) :: inbuf
          integer, dimension(outcount), intent(out) :: outbuf
          integer :: ierr, tag
#ifdef MPI
          tag = 0  ! Default tag for unpack operations
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Unpack(inbuf, insize, position, outbuf, outcount, MPI_INTEGER, comm, ierr)
          else
            call MPI_Unpack(inbuf, insize, position, outbuf, outcount, MPI_INTEGER, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_unpack_ints
!||====================================================================
!||    spmd_unpack_doubles   ../engine/source/mpi/spmd_unpack.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_unpack_doubles(inbuf, insize, position, outbuf, outcount, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: insize, outcount
          integer, intent(inout) :: position
          integer, intent(in), optional :: comm
          integer, dimension(insize), intent(in) :: inbuf
          double precision, dimension(outcount), intent(out) :: outbuf
#ifdef MPI
          integer :: ierr, tag
          tag = 0  ! Default tag for unpack operations
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Unpack(inbuf, insize, position, outbuf, outcount, MPI_DOUBLE_PRECISION, comm, ierr)
          else
            call MPI_Unpack(inbuf, insize, position, outbuf, outcount, MPI_DOUBLE_PRECISION, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_unpack_doubles

      end module spmd_unpack_mod
