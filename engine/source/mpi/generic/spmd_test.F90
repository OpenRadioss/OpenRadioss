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
      module spmd_test_mod
        implicit none

        integer, parameter, public :: TAG_TEST = -29

        !> \brief Interface for spmd_test, a wrapper for MPI_TEST
        interface spmd_test
          module procedure spmd_test_req
        end interface spmd_test

      contains

! ======================================================================================================================
!>  \brief Test for completion of a request
        subroutine spmd_test_req(request, flag, status, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(inout) :: request
          logical, intent(out) :: flag
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_TEST
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Test")
          if (present(status)) then
            call MPI_Test(request, flag, status, ierr)
          else
            call MPI_Test(request, flag, local_status, ierr)
          end if
          call spmd_out(tag_local, ierr)
#else
          flag = .true.
          request = 0
          if (present(status)) status = 0
#endif
        end subroutine spmd_test_req

      end module spmd_test_mod
