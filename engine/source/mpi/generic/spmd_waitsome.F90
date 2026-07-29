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
!||====================================================================
!||    spmd_waitsome_mod   ../engine/source/mpi/generic/spmd_waitsome.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod            ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_waitsome_mod
        implicit none

        integer, parameter, public :: TAG_WAITSOME = -33

        !> \brief Interface for spmd_waitsome, a wrapper for MPI_WAITSOME
        interface spmd_waitsome
          module procedure spmd_waitsome_req
        end interface spmd_waitsome

      contains

! ======================================================================================================================
!>  \brief Wait for completion of some requests
!||====================================================================
!||    spmd_waitsome_req   ../engine/source/mpi/generic/spmd_waitsome.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in             ../engine/source/mpi/spmd_error.F90
!||    spmd_out            ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod      ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_waitsome_req(requests, req_count, outcount, indices, statuses, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: req_count
          integer, intent(inout) :: requests(req_count)
          integer, intent(out) :: outcount
          integer, intent(out) :: indices(req_count)
#ifdef MPI
          integer, intent(inout), optional :: statuses(MPI_STATUS_SIZE, req_count)
#else
          integer, intent(inout), optional :: statuses(1, req_count)
#endif
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local
#ifdef MPI
          integer :: local_statuses(MPI_STATUS_SIZE, req_count)
#endif

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_WAITSOME
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Waitsome")
          if (present(statuses)) then
            call MPI_Waitsome(req_count, requests, outcount, indices, statuses, ierr)
          else
            call MPI_Waitsome(req_count, requests, outcount, indices, local_statuses, ierr)
          end if
          call spmd_out(tag_local, ierr)
#else
          outcount = 0
          indices = 0
          requests = 0
          if (present(statuses)) statuses = 0
#endif
        end subroutine spmd_waitsome_req

      end module spmd_waitsome_mod
