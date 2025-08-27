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
!#define DEBUG_SPMD
!||====================================================================
!||    spmd_error_mod            ../engine/source/mpi/spmd_error.F90
!||--- called by ------------------------------------------------------
!||    spmd_allgatherv_double    ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles   ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_int       ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints      ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_real      ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals     ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_double     ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_doubles    ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_int        ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_ints       ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_real       ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_reals      ../engine/source/mpi/spmd_mod.F90
!||    spmd_alltoall_double      ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles     ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_int         ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints        ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_real        ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals       ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_barrier              ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_rank            ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_size            ../engine/source/mpi/spmd_mod.F90
!||    spmd_irecv_double         ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles        ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_int            ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints           ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_real           ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals          ../engine/source/mpi/spmd_irecv.F90
!||    spmd_isend_double         ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles        ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_int            ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints           ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_real           ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals          ../engine/source/mpi/spmd_isend.F90
!||    spmd_pack_doubles         ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_ints            ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_reals           ../engine/source/mpi/spmd_pack.F90
!||    spmd_probe                ../engine/source/mpi/spmd_mod.F90
!||    spmd_recv_double          ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles         ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles2d       ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_int             ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints            ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_real            ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals           ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals2d         ../engine/source/mpi/spmd_recv.F90
!||    spmd_reduce_double        ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_doubles       ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_int           ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_ints          ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_real          ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_reals         ../engine/source/mpi/spmd_mod.F90
!||    spmd_send_double          ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles         ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles2d       ../engine/source/mpi/spmd_send.F90
!||    spmd_send_int             ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints            ../engine/source/mpi/spmd_send.F90
!||    spmd_send_real            ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals           ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals2d         ../engine/source/mpi/spmd_send.F90
!||    spmd_unpack_doubles       ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_ints          ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_reals         ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait                 ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitall              ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany              ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      module spmd_error_mod
        use, intrinsic :: iso_c_binding
        implicit none

        interface
          function c_getpid() bind(c, name="getpid")
            import :: c_int
            integer(c_int) :: c_getpid
          end function c_getpid

          function c_system(command) bind(c, name="system")
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: command(*)
            integer(c_int) :: c_system
          end function c_system
        end interface

      contains

#ifdef DEBUG_SPMD
!||====================================================================
!||    print_traceback   ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine print_traceback()
          implicit none
          integer(c_int) :: pid
          character(len=200) :: command
          character(len=200, kind=c_char) :: c_command
          integer :: i, result

          write(*,*) "=== TRACEBACK START ==="

          pid = c_getpid()
          write(*,*) "Process ID:", pid

          write(command, '("gdb -batch -ex ""set confirm off"" -ex ""bt"" -p ", I0, " 2>/dev/null")') pid

          ! Convert Fortran string to C string
          do i = 1, len_trim(command)
            c_command(i:i) = command(i:i)
          end do
          c_command(len_trim(command)+1:len_trim(command)+1) = c_null_char

          !write(*,*) "Executing:", trim(command)
          result = c_system(c_command)

          if (result /= 0) then
            write(*,*) "Note: gdb command returned status", result
            write(*,*) "If no backtrace appeared, gdb might not be available"
            write(*,*) "or the process might not have debug symbols."
          end if

          write(*,*) "=== TRACEBACK END ==="
        end subroutine print_traceback
#endif



!! \brief Trace Entry in MPI subroutines
!||====================================================================
!||    spmd_in                   ../engine/source/mpi/spmd_error.F90
!||--- called by ------------------------------------------------------
!||    spmd_allgatherv_double    ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles   ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_int       ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints      ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_real      ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals     ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_double     ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_doubles    ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_int        ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_ints       ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_real       ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_reals      ../engine/source/mpi/spmd_mod.F90
!||    spmd_alltoall_double      ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles     ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_int         ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints        ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_real        ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals       ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_barrier              ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_rank            ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_size            ../engine/source/mpi/spmd_mod.F90
!||    spmd_irecv_double         ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles        ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_int            ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints           ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_real           ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals          ../engine/source/mpi/spmd_irecv.F90
!||    spmd_isend_double         ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles        ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_int            ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints           ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_real           ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals          ../engine/source/mpi/spmd_isend.F90
!||    spmd_pack_doubles         ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_ints            ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_reals           ../engine/source/mpi/spmd_pack.F90
!||    spmd_probe                ../engine/source/mpi/spmd_mod.F90
!||    spmd_recv_double          ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles         ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles2d       ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_int             ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints            ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_real            ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals           ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals2d         ../engine/source/mpi/spmd_recv.F90
!||    spmd_reduce_double        ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_doubles       ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_int           ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_ints          ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_real          ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_reals         ../engine/source/mpi/spmd_mod.F90
!||    spmd_send_double          ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles         ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles2d       ../engine/source/mpi/spmd_send.F90
!||    spmd_send_int             ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints            ../engine/source/mpi/spmd_send.F90
!||    spmd_send_real            ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals           ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals2d         ../engine/source/mpi/spmd_send.F90
!||    spmd_unpack_doubles       ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_ints          ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_reals         ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait                 ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitall              ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany              ../engine/source/mpi/spmd_wait.F90
!||====================================================================
        subroutine spmd_in(tag)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer :: tag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
#ifdef DEBUG_SPMD
          ! call print_traceback()
          write(6,*) "Entering MPI call: ", tag
#endif
        end subroutine spmd_in

!||====================================================================
!||    spmd_out                  ../engine/source/mpi/spmd_error.F90
!||--- called by ------------------------------------------------------
!||    spmd_allgatherv_double    ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_doubles   ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_int       ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_ints      ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_real      ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allgatherv_reals     ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_double     ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_doubles    ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_int        ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_ints       ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_real       ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_reals      ../engine/source/mpi/spmd_mod.F90
!||    spmd_alltoall_double      ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_doubles     ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_int         ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_ints        ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_real        ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoall_reals       ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_barrier              ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_rank            ../engine/source/mpi/spmd_mod.F90
!||    spmd_comm_size            ../engine/source/mpi/spmd_mod.F90
!||    spmd_irecv_double         ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles        ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_int            ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints           ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_real           ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals          ../engine/source/mpi/spmd_irecv.F90
!||    spmd_isend_double         ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles        ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_int            ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints           ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_real           ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals          ../engine/source/mpi/spmd_isend.F90
!||    spmd_pack_doubles         ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_ints            ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_reals           ../engine/source/mpi/spmd_pack.F90
!||    spmd_probe                ../engine/source/mpi/spmd_mod.F90
!||    spmd_recv_double          ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles         ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles2d       ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_int             ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints            ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_real            ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals           ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals2d         ../engine/source/mpi/spmd_recv.F90
!||    spmd_reduce_double        ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_doubles       ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_int           ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_ints          ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_real          ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_reals         ../engine/source/mpi/spmd_mod.F90
!||    spmd_send_double          ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles         ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles2d       ../engine/source/mpi/spmd_send.F90
!||    spmd_send_int             ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints            ../engine/source/mpi/spmd_send.F90
!||    spmd_send_real            ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals           ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals2d         ../engine/source/mpi/spmd_send.F90
!||    spmd_unpack_doubles       ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_ints          ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_reals         ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait                 ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitall              ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany              ../engine/source/mpi/spmd_wait.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod       ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
        subroutine spmd_out(tag, ierr)
          use spmd_comm_world_mod, only: SPMD_COMM_WORLD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: tag !< Tag of the the MPI call
          integer, intent(in) :: ierr !< error of the MPI call
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierror
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
          if(ierr /= MPI_SUCCESS) then
            write(6,*) "MPI error: ", ierr," at ",tag
            call MPI_Abort(SPMD_COMM_WORLD, ierr,ierror)
          end if
#ifdef DEBUG_SPMD
          write(6,*) "Exiting MPI call: ", tag
#endif
#endif
        end subroutine spmd_out

      end module spmd_error_mod
