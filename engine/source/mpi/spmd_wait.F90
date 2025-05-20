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
      module spmd_wait_mod
        implicit none
        integer, parameter, public :: TAG_WAIT = -2
        integer, parameter, public :: TAG_WAITALL = -3
        integer, parameter, public :: TAG_WAITANY = -4
#ifndef MPI
        integer, parameter, public :: MPI_STATUS_IGNORE = 0
        integer, parameter, public :: MPI_STATUS_SIZE = 1
#endif


      contains
        !||====================================================================
        !||    spmd_wait                       ../engine/source/mpi/spmd_mod.F90
        !||--- called by ------------------------------------------------------
        !||    spmd_e1vois                     ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_e4vois                     ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_e6vois                     ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_envois                     ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_evois                      ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_exalew                     ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_exalew_pon                 ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_exch_a_sol2sph             ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_exch_flow_tracking_data    ../engine/source/ale/grid/spmd_exch_flow_tracking_data.F90
        !||    spmd_exch_flow_tracking_data2   ../engine/source/ale/grid/spmd_exch_flow_tracking_data2.F90
        !||    spmd_exch_flow_tracking_data3   ../engine/source/ale/grid/spmd_exch_flow_tracking_data3.F90
        !||    spmd_exch_flow_tracking_data4   ../engine/source/ale/grid/spmd_exch_flow_tracking_data4.F90
        !||    spmd_exch_neighbour_segment     ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
        !||    spmd_extag                      ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_get_penis                  ../engine/source/mpi/interfaces/send_cand.F
        !||    spmd_get_penis20                ../engine/source/mpi/interfaces/send_cand.F
        !||    spmd_i21fthecom                 ../engine/source/mpi/interfaces/send_cand.F
        !||    spmd_i21tempcom                 ../engine/source/mpi/interfaces/send_cand.F
        !||    spmd_i4vois                     ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_i8vois                     ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_ifront_stamp               ../engine/source/mpi/interfaces/send_cand.F
        !||    spmd_l11vois                    ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_l51vois                    ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_sphgeta                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetd                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetdk                   ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetf                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetg                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgeth                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetimp                  ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetisph                 ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetstb                  ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgett                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetvois_off             ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetw                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetwa                   ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_sphgetx                    ../engine/source/mpi/elements/spmd_sph.F
        !||    spmd_tri10box                   ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri11vox                   ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri18_151vox               ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri24vox                   ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri25vox                   ../engine/source/mpi/interfaces/spmd_tri25vox.F
        !||    spmd_tri7vox                    ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_wvois                      ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_xv_inter_type1             ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
        !||    spmd_xvois                      ../engine/source/mpi/fluid/spmd_cfd.F
        !||--- calls      -----------------------------------------------------
        !||    spmd_in                         ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out                        ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_wait(request, status)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: request
          integer, dimension(MPI_STATUS_SIZE), optional, intent(inout) :: status
#ifdef MPI
          integer :: ierr
          call spmd_in(TAG_WAIT)
          if(present(status)) then
            call MPI_Wait(request, status, ierr)
          else
            call MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
          end if
          call spmd_out(TAG_WAIT,ierr)
#endif
        end subroutine spmd_wait
! ======================================================================================================================

! ======================================================================================================================
! ======================================================================================================================
        !||====================================================================
        !||    spmd_waitany                  ../engine/source/mpi/spmd_mod.F90
        !||--- called by ------------------------------------------------------
        !||    spmd_e1vois                   ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_e4vois                   ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_e6vois                   ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_envois                   ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_evois                    ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_exalew_pon               ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_exch_neighbour_segment   ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
        !||    spmd_i4vois                   ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_i8vois                   ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_l11vois                  ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_l51vois                  ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_segcom                   ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_tri10box                 ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri11vox                 ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri18_151vox             ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri24vox                 ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_tri25vox                 ../engine/source/mpi/interfaces/spmd_tri25vox.F
        !||    spmd_tri7vox                  ../engine/source/mpi/interfaces/spmd_int.F
        !||    spmd_wvois                    ../engine/source/mpi/fluid/spmd_cfd.F
        !||    spmd_xvois                    ../engine/source/mpi/fluid/spmd_cfd.F
        !||--- calls      -----------------------------------------------------
        !||    spmd_in                       ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out                      ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_waitany(buf_count, array_of_requests, index_of_completed, status)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count
          integer, dimension(buf_count), intent(inout) :: array_of_requests
          integer, intent(inout) :: index_of_completed
          integer, dimension(MPI_STATUS_SIZE), optional, intent(inout) :: status
#ifdef MPI
          integer :: ierr
          call spmd_in(TAG_WAITANY)
          if(present(status)) then
            call MPI_Waitany(buf_count, array_of_requests, index_of_completed, status, ierr)
          else
            call MPI_Waitany(buf_count, array_of_requests, index_of_completed, MPI_STATUS_IGNORE, ierr)
          end if
          call spmd_out(TAG_WAITANY,ierr)
#endif
        end subroutine spmd_waitany
! ======================================================================================================================
        !||====================================================================
        !||    spmd_waitall    ../engine/source/mpi/spmd_mod.F90
        !||--- called by ------------------------------------------------------
        !||    spmd_tri25vox   ../engine/source/mpi/interfaces/spmd_tri25vox.F
        !||--- calls      -----------------------------------------------------
        !||    spmd_in         ../engine/source/mpi/spmd_mod.F90
        !||    spmd_out        ../engine/source/mpi/spmd_mod.F90
        !||====================================================================
        subroutine spmd_waitall(buf_count, array_of_requests, array_of_statuses)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count
          integer, dimension(buf_count), intent(inout) :: array_of_requests
          integer, dimension(MPI_STATUS_SIZE, buf_count), optional, intent(inout) :: array_of_statuses
#ifdef MPI
          integer :: ierr
          call spmd_in(TAG_WAITALL)
          if(present(array_of_statuses)) then
            call MPI_Waitall(buf_count, array_of_requests, array_of_statuses, ierr)
          else
            call MPI_Waitall(buf_count, array_of_requests, MPI_STATUSES_IGNORE, ierr)
          end if
          call spmd_out(TAG_WAITALL,ierr)
#endif
        end subroutine spmd_waitall
      end module spmd_wait_mod
