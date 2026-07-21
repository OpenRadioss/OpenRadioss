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
      module spmd_send_mod
        implicit none

        !> \brief Interface for spmd_send, a wrapper for MPI_SEND
        interface spmd_send
          module procedure spmd_send_reals
          module procedure spmd_send_ints
          module procedure spmd_send_doubles
          module procedure spmd_send_reals2d
          module procedure spmd_send_ints2d
          module procedure spmd_send_doubles2d
          module procedure spmd_send_real
          module procedure spmd_send_int
          module procedure spmd_send_double
        end interface spmd_send

      contains

! ======================================================================================================================
!>  \brief Blocking send of real       array
        subroutine spmd_send_reals(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real, dimension(:), intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_REAL, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_reals

! ======================================================================================================================
!>  \brief Blocking send of integer       array
        subroutine spmd_send_ints(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, dimension(:), intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_ints

! ======================================================================================================================
!>  \brief Blocking send of double precision       array
        subroutine spmd_send_doubles(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, dimension(:), intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_doubles

! ======================================================================================================================
!>  \brief Blocking send of real       array
        subroutine spmd_send_reals2d(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real, dimension(:,:), intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_REAL, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_reals2d

! ======================================================================================================================
!>  \brief Blocking send of integer       array
        subroutine spmd_send_ints2d(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, dimension(:,:), intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_ints2d

! ======================================================================================================================
!>  \brief Blocking send of double precision       array
        subroutine spmd_send_doubles2d(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, dimension(:,:), intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_doubles2d

! ======================================================================================================================
!>  \brief Blocking send of real       scalar
        subroutine spmd_send_real(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real,  intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_REAL, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_real

! ======================================================================================================================
!>  \brief Blocking send of integer       scalar
        subroutine spmd_send_int(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer,  intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_INTEGER, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_int

! ======================================================================================================================
!>  \brief Blocking send of double precision       scalar
        subroutine spmd_send_double(buf, buf_count, dest, tag, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision,  intent(in) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr

#ifdef MPI
          call spmd_in(tag, "MPI_Send", dest)
          if (present(comm)) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag, ierr)
#else
          continue
#endif
        end subroutine spmd_send_double

      end module spmd_send_mod
