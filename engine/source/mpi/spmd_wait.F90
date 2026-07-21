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
      module spmd_wait_mod
        use, intrinsic :: iso_c_binding
        use spmd_profiler_mod, only: spmd_profiling_enabled
        implicit none

        integer, parameter, public :: TAG_WAIT = -8

        interface
          subroutine spmd_profiler_complete_request_c(request, t_end) &
            bind(c, name="spmd_profiler_complete_request")
            import :: c_int, c_double
            integer(c_int), intent(in) :: request
            real(c_double), intent(in) :: t_end
          end subroutine spmd_profiler_complete_request_c
        end interface

        !> \brief Interface for spmd_wait, a wrapper for MPI_WAIT
        interface spmd_wait
          module procedure spmd_wait_req
        end interface spmd_wait

      contains

! ======================================================================================================================
!>  \brief Wait on an MPI request
        subroutine spmd_wait_req(request, status, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(inout) :: request
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)
          integer(c_int) :: saved_request

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_WAIT
          end if

#ifdef MPI
          saved_request = int(request, c_int)
          call spmd_in(tag_local, "MPI_Wait")
          if (present(status)) then
            call MPI_Wait(request, status, ierr)
          else
            call MPI_Wait(request, local_status, ierr)
          end if
          call spmd_out(tag_local, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_complete_request_c(saved_request, MPI_Wtime())
          end if
#else
          request = 0
          if (present(status)) status = 0
#endif
        end subroutine spmd_wait_req

      end module spmd_wait_mod
