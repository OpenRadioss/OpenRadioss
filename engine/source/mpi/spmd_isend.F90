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
!||    spmd_isend_mod      ../engine/source/mpi/spmd_isend.F90
!||--- called by ------------------------------------------------------
!||    spmd_mod            ../engine/source/mpi/spmd_mod.F90
!||--- uses       -----------------------------------------------------
!||    spmd_profiler_mod   ../engine/source/mpi/generic/spmd_profiler_mod.F90
!||====================================================================
      module spmd_isend_mod
        use, intrinsic :: iso_c_binding
        use spmd_profiler_mod, only: spmd_profiling_enabled
        implicit none

        interface
          subroutine spmd_profiler_register_request_c(request, peer_rank, msg_tag, is_recv) &
            bind(c, name="spmd_profiler_register_request")
            import :: c_int
            integer(c_int), intent(in) :: request, peer_rank, msg_tag, is_recv
          end subroutine spmd_profiler_register_request_c
        end interface

        !> \brief Interface for spmd_isend, a wrapper for MPI_ISEND
        interface spmd_isend
          module procedure spmd_isend_reals
          module procedure spmd_isend_ints
          module procedure spmd_isend_doubles
          module procedure spmd_isend_reals2d
          module procedure spmd_isend_ints2d
          module procedure spmd_isend_doubles2d
          module procedure spmd_isend_real
          module procedure spmd_isend_int
          module procedure spmd_isend_double
        end interface spmd_isend

      contains

! ======================================================================================================================
!>  \brief Non-blocking send of real       array
!||====================================================================
!||    spmd_isend_reals                   ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_reals(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real, dimension(:), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_reals

! ======================================================================================================================
!>  \brief Non-blocking send of integer       array
!||====================================================================
!||    spmd_isend_ints                    ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_ints(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, dimension(:), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_ints

! ======================================================================================================================
!>  \brief Non-blocking send of double precision       array
!||====================================================================
!||    spmd_isend_doubles                 ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_doubles(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, dimension(:), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_doubles

! ======================================================================================================================
!>  \brief Non-blocking send of real       array
!||====================================================================
!||    spmd_isend_reals2d                 ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_reals2d(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real, dimension(:,:), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_reals2d

! ======================================================================================================================
!>  \brief Non-blocking send of integer       array
!||====================================================================
!||    spmd_isend_ints2d                  ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_ints2d(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, dimension(:,:), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_ints2d

! ======================================================================================================================
!>  \brief Non-blocking send of double precision       array
!||====================================================================
!||    spmd_isend_doubles2d               ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_doubles2d(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, dimension(:,:), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_doubles2d

! ======================================================================================================================
!>  \brief Non-blocking send of real       scalar
!||====================================================================
!||    spmd_isend_real                    ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_real(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real,  intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_real

! ======================================================================================================================
!>  \brief Non-blocking send of integer       scalar
!||====================================================================
!||    spmd_isend_int                     ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_int(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer,  intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_int

! ======================================================================================================================
!>  \brief Non-blocking send of double precision       scalar
!||====================================================================
!||    spmd_isend_double                  ../engine/source/mpi/spmd_isend.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in                            ../engine/source/mpi/spmd_error.F90
!||    spmd_out                           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod                ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_error_mod                     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_isend_double(buf, buf_count, dest, tag, request, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision,  intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag, "MPI_Isend", dest)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, request, ierr)
          end if
          call spmd_out(tag, ierr)
          if (spmd_profiling_enabled) then
            call spmd_profiler_register_request_c(int(request,c_int), int(dest,c_int), &
              int(tag,c_int), 0_c_int)
          end if
#else
          request = 0
#endif
        end subroutine spmd_isend_double

      end module spmd_isend_mod
