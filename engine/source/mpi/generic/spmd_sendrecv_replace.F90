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
      module spmd_sendrecv_replace_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        implicit none

        integer, parameter, public :: TAG_SENDRECV_REPLACE = -25

        !> \\brief Interface for spmd_sendrecv_replace, a wrapper for MPI_SENDRECV_REPLACE
        interface spmd_sendrecv_replace
          module procedure spmd_sendrecv_replace_reals
          module procedure spmd_sendrecv_replace_ints
          module procedure spmd_sendrecv_replace_doubles
          module procedure spmd_sendrecv_replace_reals2d
          module procedure spmd_sendrecv_replace_ints2d
          module procedure spmd_sendrecv_replace_doubles2d
          module procedure spmd_sendrecv_replace_real
          module procedure spmd_sendrecv_replace_int
          module procedure spmd_sendrecv_replace_double
        end interface spmd_sendrecv_replace

      contains

! ======================================================================================================================
!>  \\brief Sendrecv_replace of real       array
        subroutine spmd_sendrecv_replace_reals(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          real, dimension(:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Sendrecv_replace(buf, buf_count, MPI_REAL, dest, sendtag, source, recvtag, used_comm, status, ierr)
          else
            call MPI_Sendrecv_replace(buf, buf_count, MPI_REAL, dest, sendtag, source, recvtag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_reals

! ======================================================================================================================
!>  \\brief Sendrecv_replace of integer       array
        subroutine spmd_sendrecv_replace_ints(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          integer, dimension(:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Sendrecv_replace(buf, buf_count, MPI_INTEGER, dest, sendtag, source, recvtag, used_comm, status, ierr)
          else
            call MPI_Sendrecv_replace(buf, buf_count, MPI_INTEGER, dest, sendtag, source, recvtag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_ints

! ======================================================================================================================
!>  \\brief Sendrecv_replace of double precision       array
        subroutine spmd_sendrecv_replace_doubles(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          double precision, dimension(:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Sendrecv_replace(buf, buf_count, MPI_DOUBLE_PRECISION, dest, sendtag, source, recvtag, used_comm, status, ierr)
          else
            call MPI_Sendrecv_replace(buf, buf_count, MPI_DOUBLE_PRECISION, dest, sendtag, source, recvtag, used_comm,&
            & local_status, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_doubles

! ======================================================================================================================
!>  \\brief Sendrecv_replace of real       array
        subroutine spmd_sendrecv_replace_reals2d(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          real, dimension(:,:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Sendrecv_replace(buf, buf_count, MPI_REAL, dest, sendtag, source, recvtag, used_comm, status, ierr)
          else
            call MPI_Sendrecv_replace(buf, buf_count, MPI_REAL, dest, sendtag, source, recvtag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_reals2d

! ======================================================================================================================
!>  \\brief Sendrecv_replace of integer       array
        subroutine spmd_sendrecv_replace_ints2d(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          integer, dimension(:,:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Sendrecv_replace(buf, buf_count, MPI_INTEGER, dest, sendtag, source, recvtag, used_comm, status, ierr)
          else
            call MPI_Sendrecv_replace(buf, buf_count, MPI_INTEGER, dest, sendtag, source, recvtag, used_comm, local_status, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_ints2d

! ======================================================================================================================
!>  \\brief Sendrecv_replace of double precision       array
        subroutine spmd_sendrecv_replace_doubles2d(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          double precision, dimension(:,:), intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (present(status)) then
            call MPI_Sendrecv_replace(buf, buf_count, MPI_DOUBLE_PRECISION, dest, sendtag, source, recvtag, used_comm, status, ierr)
          else
            call MPI_Sendrecv_replace(buf, buf_count, MPI_DOUBLE_PRECISION, dest, sendtag, source, recvtag, used_comm,&
            & local_status, ierr)
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_doubles2d

! ======================================================================================================================
!>  \\brief Sendrecv_replace of real       scalar
        subroutine spmd_sendrecv_replace_real(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          real,  intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            if (present(status)) then
              call MPI_Sendrecv_replace(buf, buf_count, MPI_REAL, dest, sendtag, source, recvtag, used_comm, status, ierr)
            else
              call MPI_Sendrecv_replace(buf, buf_count, MPI_REAL, dest, sendtag, source, recvtag, used_comm, local_status, ierr)
            end if
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_real

! ======================================================================================================================
!>  \\brief Sendrecv_replace of integer       scalar
        subroutine spmd_sendrecv_replace_int(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          integer,  intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            if (present(status)) then
              call MPI_Sendrecv_replace(buf, buf_count, MPI_INTEGER, dest, sendtag, source, recvtag, used_comm, status, ierr)
            else
              call MPI_Sendrecv_replace(buf, buf_count, MPI_INTEGER, dest, sendtag, source, recvtag, used_comm, local_status, ierr)
            end if
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_int

! ======================================================================================================================
!>  \\brief Sendrecv_replace of double precision       scalar
        subroutine spmd_sendrecv_replace_double(buf, buf_count, dest, sendtag, source, recvtag, status, comm, tag)
          use spmd_error_mod, only: spmd_in, spmd_out
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, sendtag, source, recvtag
          double precision,  intent(inout) :: buf
          integer, intent(inout), optional :: status(MPI_STATUS_SIZE)
          integer, intent(in), optional :: comm
          integer, intent(in), optional :: tag
          integer :: ierr, used_comm
          integer :: tag_local
          integer :: local_status(MPI_STATUS_SIZE)

          if (present(tag)) then
            tag_local = tag
          else
            tag_local = TAG_SENDRECV_REPLACE
          end if

#ifdef MPI
          call spmd_in(tag_local, "MPI_Sendrecv_replace")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if

          if (buf_count .ne. 1) then
            ierr = -1
          else
            if (present(status)) then
              call MPI_Sendrecv_replace(buf, buf_count, MPI_DOUBLE_PRECISION, dest, sendtag, source, recvtag, used_comm, status,&
              & ierr)
            else
              call MPI_Sendrecv_replace(buf, buf_count, MPI_DOUBLE_PRECISION, dest, sendtag, source, recvtag, used_comm,&
              & local_status, ierr)
            end if
          end if

          call spmd_out(tag_local, ierr)
#else
          if (present(status)) status = 0
#endif
        end subroutine spmd_sendrecv_replace_double

      end module spmd_sendrecv_replace_mod
