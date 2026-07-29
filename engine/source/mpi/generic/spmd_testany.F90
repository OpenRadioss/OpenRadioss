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
!||    spmd_testany_mod   ../engine/source/mpi/generic/spmd_testany.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod           ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_testany_mod
        implicit none

        integer, parameter, public :: TAG_TESTANY = -31

        !> \brief Interface for spmd_testany, a wrapper for MPI_TESTANY
        interface spmd_testany
          module procedure spmd_testany_req
        end interface spmd_testany

      contains

! ======================================================================================================================
!>  \brief Test whether any request has completed
!||====================================================================
!||    spmd_testany_req   ../engine/source/mpi/generic/spmd_testany.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in            ../engine/source/mpi/spmd_error.F90
!||    spmd_out           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_testany_req(requests, req_count, index, flag, status, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: req_count
          integer, intent(inout) :: requests(req_count)
          integer, intent(out) :: index
          logical, intent(out) :: flag
#ifdef MPI
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
#else
          integer, intent(inout), optional :: status(1)
#endif
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local
#ifdef MPI
          integer :: local_status(MPI_STATUS_SIZE)
#endif

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_TESTANY
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Testany")
          if (present(status)) then
            call MPI_Testany(req_count, requests, index, flag, status, ierr)
          else
            call MPI_Testany(req_count, requests, index, flag, local_status, ierr)
          end if
          call spmd_out(tag_local, ierr)
#else
          index = 0
          flag = .true.
          requests = 0
          if (present(status)) status = 0
#endif
        end subroutine spmd_testany_req

      end module spmd_testany_mod
