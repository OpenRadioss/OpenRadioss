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
      module spmd_iprobe_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_IPROBE = -27

        !> \brief Interface for spmd_iprobe, a wrapper for MPI_IPROBE
        interface spmd_iprobe
          module procedure spmd_iprobe_basic
        end interface spmd_iprobe

      contains

! ======================================================================================================================
!>  \brief Non-blocking probe
        subroutine spmd_iprobe_basic(source, tag, flag, status, comm, trace_tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: source, tag
          logical, intent(out) :: flag
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: trace_tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(trace_tag)) then
            tag_local = trace_tag
          else
            tag_local = TAG_IPROBE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Iprobe")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Iprobe(source, tag, used_comm, flag, status, ierr)
          else
            call MPI_Iprobe(source, tag, used_comm, flag, local_status, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          flag = .false.
          if (present(status)) status = 0
#endif
        end subroutine spmd_iprobe_basic

      end module spmd_iprobe_mod
