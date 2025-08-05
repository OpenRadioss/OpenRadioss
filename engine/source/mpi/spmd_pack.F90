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


      module spmd_pack_mod

        implicit none
        private
        interface spmd_pack
          module procedure spmd_pack_reals      !< Sends real numbers
          module procedure spmd_pack_ints       !< Sends integers
          module procedure spmd_pack_doubles    !< Sends double precision numbers
        end interface spmd_pack
        public spmd_pack
      contains
!||====================================================================
!||    spmd_pack_reals       ../engine/source/mpi/spmd_pack.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_pack_reals(inbuf, incount, outbuf, outsize, position, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: incount, outsize
          integer, intent(inout) :: position
          integer, intent(in), optional :: comm
          real, dimension(incount), intent(in) :: inbuf
          integer, dimension(outsize), intent(inout) :: outbuf
          integer :: ierr, tag
#ifdef MPI
          tag = 0  ! Default tag for pack operations
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Pack(inbuf, incount, MPI_REAL, outbuf, outsize, position, comm, ierr)
          else
            call MPI_Pack(inbuf, incount, MPI_REAL, outbuf, outsize, position, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_pack_reals
!||====================================================================
!||    spmd_pack_ints        ../engine/source/mpi/spmd_pack.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_pack_ints(inbuf, incount, outbuf, outsize, position, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: incount, outsize
          integer, intent(inout) :: position
          integer, intent(in), optional :: comm
          integer, dimension(incount), intent(in) :: inbuf
          integer, dimension(outsize), intent(inout) :: outbuf
          integer :: ierr, tag
#ifdef MPI
          tag = 0  ! Default tag for pack operations
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Pack(inbuf, incount, MPI_INTEGER, outbuf, outsize, position, comm, ierr)
          else
            call MPI_Pack(inbuf, incount, MPI_INTEGER, outbuf, outsize, position, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_pack_ints
!||====================================================================
!||    spmd_pack_doubles     ../engine/source/mpi/spmd_pack.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_pack_doubles(inbuf, incount, outbuf, outsize, position, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: incount, outsize
          integer, intent(inout) :: position
          integer, intent(in), optional :: comm
          double precision, dimension(incount), intent(in) :: inbuf
          integer, dimension(outsize), intent(inout) :: outbuf
#ifdef MPI
          integer :: ierr, tag
          tag = 0  ! Default tag for pack operations
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Pack(inbuf, incount, MPI_DOUBLE_PRECISION, outbuf, outsize, position, comm, ierr)
          else
            call MPI_Pack(inbuf, incount, MPI_DOUBLE_PRECISION, outbuf, outsize, position, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_pack_doubles

      end module spmd_pack_mod