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


      module spmd_send_mod

        implicit none
        private
        interface spmd_send
          module procedure spmd_send_reals      !< Sends real numbers
          module procedure spmd_send_ints       !< Sends integers
          module procedure spmd_send_doubles    !< Sends double precision numbers
          module procedure spmd_send_real       !< Sends a single real number
          module procedure spmd_send_int        !< Sends a single integer
          module procedure spmd_send_double     !< Sends a single double precision number
        end interface spmd_send
        public spmd_send
      contains
        subroutine spmd_send_reals(buf, buf_count, dest, tag,  comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          real, dimension(buf_count), intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_reals
        subroutine spmd_send_ints(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          integer, dimension(buf_count), intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_INTEGER , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_ints
        subroutine spmd_send_doubles(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          double precision, dimension(buf_count), intent(in) :: buf
#ifdef MPI
          integer :: ierr
          ! the MPI datatype for double precision is MPI_DOUBLE_PRECISION
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_doubles
        subroutine spmd_send_real(buf, buf_count, dest, tag,  comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          real, intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_REAL , dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_real
        subroutine spmd_send_int(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          integer, intent(in) :: buf
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_INTEGER , dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_int
        subroutine spmd_send_double(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          double precision, intent(in) :: buf
#ifdef MPI
          integer :: ierr
          ! the MPI datatype for double precision is MPI_DOUBLE_PRECISION
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_send_double

      end module spmd_send_mod
