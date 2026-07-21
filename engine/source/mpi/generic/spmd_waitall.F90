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
      module spmd_waitall_mod
        use, intrinsic :: iso_c_binding
        use spmd_profiler_mod, only: spmd_profiling_enabled
        implicit none

        integer, parameter, public :: TAG_WAITALL = -9

        interface
          subroutine spmd_profiler_complete_requests_c(requests, count, t_end) &
            bind(c, name="spmd_profiler_complete_requests")
            import :: c_int, c_double
            integer(c_int), intent(in) :: requests(*)
            integer(c_int), intent(in) :: count
            real(c_double), intent(in) :: t_end
          end subroutine spmd_profiler_complete_requests_c
        end interface

        !> \brief Interface for spmd_waitall, a wrapper for MPI_WAITALL
        interface spmd_waitall
          module procedure spmd_waitall_req
        end interface spmd_waitall

      contains

! ======================================================================================================================
!>  \brief Wait on an array of MPI requests
        subroutine spmd_waitall_req(requests, req_count, statuses, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: req_count
          integer, intent(inout) :: requests(req_count)
          integer, intent(inout), optional :: statuses(MPI_STATUS_SIZE, req_count)
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local
          integer :: local_statuses(MPI_STATUS_SIZE, req_count)
          integer(c_int) :: saved_requests(req_count)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_WAITALL
          end if

#ifdef MPI
          if (spmd_profiling_enabled) then
            saved_requests = int(requests, c_int)
          end if
          call spmd_in(tag_local, "MPI_Waitall")
          if (present(statuses)) then
            call MPI_Waitall(req_count, requests, statuses, ierr)
          else
            call MPI_Waitall(req_count, requests, local_statuses, ierr)
          end if
          call spmd_out(tag_local, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_complete_requests_c(saved_requests, int(req_count,c_int), &
              MPI_Wtime())
          end if
#else
          requests = 0
          if (present(statuses)) statuses = 0
#endif
        end subroutine spmd_waitall_req

      end module spmd_waitall_mod
