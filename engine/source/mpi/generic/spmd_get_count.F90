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
      module spmd_get_count_mod
        implicit none

        integer, parameter, public :: TAG_GET_COUNT = -28

        ! Note: Avoid a generic interface here because all variants share the
        ! same argument list, which causes ambiguous overloading in Fortran.

      contains

! ======================================================================================================================
!>  \brief Get_count for real       scalar
        subroutine spmd_get_count_real(status, count, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: status(MPI_STATUS_SIZE)
          integer, intent(out) :: count
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GET_COUNT
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Get_count")
          call MPI_Get_count(status, MPI_REAL, count, ierr)
          call spmd_out(tag_local, ierr)
#else
          count = 0
#endif
        end subroutine spmd_get_count_real

! ======================================================================================================================
!>  \brief Get_count for integer       scalar
        subroutine spmd_get_count_int(status, count, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: status(MPI_STATUS_SIZE)
          integer, intent(out) :: count
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GET_COUNT
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Get_count")
          call MPI_Get_count(status, MPI_INTEGER, count, ierr)
          call spmd_out(tag_local, ierr)
#else
          count = 0
#endif
        end subroutine spmd_get_count_int

! ======================================================================================================================
!>  \brief Get_count for double precision       scalar
        subroutine spmd_get_count_double(status, count, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: status(MPI_STATUS_SIZE)
          integer, intent(out) :: count
          integer, intent(in), optional :: tag
          integer :: ierr
          integer :: tag_local

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_GET_COUNT
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Get_count")
          call MPI_Get_count(status, MPI_DOUBLE_PRECISION, count, ierr)
          call spmd_out(tag_local, ierr)
#else
          count = 0
#endif
        end subroutine spmd_get_count_double

      end module spmd_get_count_mod
