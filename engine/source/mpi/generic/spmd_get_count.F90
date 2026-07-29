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
!||    spmd_get_count_mod   ../engine/source/mpi/generic/spmd_get_count.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod             ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      module spmd_get_count_mod
        implicit none

        integer, parameter, public :: TAG_GET_COUNT = -28

        ! Note: Avoid a generic interface here because all variants share the
        ! same argument list, which causes ambiguous overloading in Fortran.

      contains

! ======================================================================================================================
!>  \brief Get_count for real       scalar
!||====================================================================
!||    spmd_get_count_real   ../engine/source/mpi/generic/spmd_get_count.F90
!||--- called by ------------------------------------------------------
!||    spmd_gather_sph       ../engine/source/mpi/anim/spmd_gather_sph.F
!||    spmd_gather_xyz16     ../engine/source/mpi/anim/spmd_gather_xyz16.F
!||    spmd_r4get_partn      ../engine/source/mpi/anim/spmd_r4get_partn.F
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_get_count_real(status, count, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
#ifdef MPI
          integer, intent(in) :: status(MPI_STATUS_SIZE)
#else
          integer, intent(in) :: status(1)
#endif
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
!||====================================================================
!||    spmd_get_count_int         ../engine/source/mpi/generic/spmd_get_count.F90
!||--- called by ------------------------------------------------------
!||    spmd_doutp_gath            ../engine/source/mpi/interfaces/spmd_outp.F
!||    spmd_doutp_vgath           ../engine/source/mpi/interfaces/spmd_outp.F
!||    spmd_dparrby               ../engine/source/mpi/anim/spmd_dparrby.F
!||    spmd_gather_nodal_scalar   ../engine/source/mpi/nodes/spmd_gather_nodal_scalar.F
!||    spmd_gatherf               ../engine/source/mpi/anim/spmd_gatherf.F
!||    spmd_gatheritab            ../engine/source/mpi/anim/spmd_gatheritab.F
!||    spmd_gatheritab_crk        ../engine/source/mpi/anim/spmd_gatheritab_crk.F
!||    spmd_iget_partn            ../engine/source/mpi/anim/spmd_iget_partn.F
!||    spmd_iget_partn_ply        ../engine/source/mpi/anim/spmd_iget_partn_ply.F
!||    spmd_iglob_partn           ../engine/source/mpi/anim/spmd_iglob_partn.F
!||    spmd_outpitab              ../engine/source/mpi/interfaces/spmd_outp.F
!||    spmd_velvec2               ../engine/source/mpi/anim/spmd_velvec2.F
!||    spmd_vgath                 ../engine/source/mpi/anim/spmd_vgath.F
!||    spmd_vgath_err             ../engine/source/mpi/anim/spmd_vgath_err.F
!||    spmd_wrt_crk_xyznod        ../engine/source/mpi/anim/spmd_wrt_crk_xyznod.F
!||    spmd_wrt_crk_xyznor        ../engine/source/mpi/anim/spmd_wrt_crk_xyznor.F
!||    spmd_wrt_xyznod            ../engine/source/mpi/anim/spmd_wrt_xyznod.F
!||    spmd_wrt_xyznor            ../engine/source/mpi/anim/spmd_wrt_xyznor.F
!||--- calls      -----------------------------------------------------
!||    spmd_in                    ../engine/source/mpi/spmd_error.F90
!||    spmd_out                   ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod             ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_get_count_int(status, count, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
#ifdef MPI
          integer, intent(in) :: status(MPI_STATUS_SIZE)
#else
          integer, intent(in) :: status(1)
#endif
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
!||====================================================================
!||    spmd_get_count_double   ../engine/source/mpi/generic/spmd_get_count.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod          ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_get_count_double(status, count, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
#ifdef MPI
          integer, intent(in) :: status(MPI_STATUS_SIZE)
#else
          integer, intent(in) :: status(1)
#endif
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
