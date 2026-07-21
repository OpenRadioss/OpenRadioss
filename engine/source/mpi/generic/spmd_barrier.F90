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
      module spmd_barrier_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_BARRIER = -13

        !> \brief Interface for spmd_barrier, a wrapper for MPI_BARRIER
        interface spmd_barrier
          module procedure spmd_barrier_comm
        end interface spmd_barrier

      contains

! ======================================================================================================================
!>  \brief Barrier on communicator
        subroutine spmd_barrier_comm(comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_BARRIER
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Barrier")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Barrier(used_comm, ierr)
          call spmd_out(tag_local, ierr)
#else
          continue
#endif
        end subroutine spmd_barrier_comm

      end module spmd_barrier_mod
