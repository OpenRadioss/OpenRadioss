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
      !||====================================================================
      !||    spmd_mod                        ../engine/source/mpi/spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_element_init             ../engine/source/mpi/python_spmd_mod.F90
      !||    python_element_sync             ../engine/source/mpi/python_spmd_mod.F90
      !||    resol                           ../engine/source/engine/resol.F
      !||    spmd_all_dmax                   ../engine/source/mpi/elements/spmd_sph.F
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
      !||    spmd_get_inacti7                ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_penis                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_penis20                ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif                   ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif11                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif20                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif20e                ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_stif25                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_glob_dmin9                 ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_i21fthecom                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i21tempcom                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i4vois                     ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_i8vois                     ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_ifront_stamp               ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_init_ebcs                  ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_l11vois                    ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_l51vois                    ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_sd_stfa20                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn                    ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn11                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn20e                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_sd_stfn25                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_segcom                     ../engine/source/mpi/fluid/spmd_cfd.F
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
      !||    spmd_sphvox0                    ../engine/source/mpi/elements/spmd_sph.F
      !||    spmd_tri10box                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri10gat                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11gat                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11vox                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11vox0                  ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri18_151vox               ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri23vox0                  ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24gat                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24vox                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24vox0                  ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri25vox                   ../engine/source/mpi/interfaces/spmd_tri25vox.F
      !||    spmd_tri7gat                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox0                   ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_wvois                      ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_xv_inter_type1             ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
      !||    spmd_xvois                      ../engine/source/mpi/fluid/spmd_cfd.F
      !||====================================================================
      module spmd_mod
        implicit none
        ! Define the interface for spmd_send
! dummy tags for MPI calls that do not have a tag

        private
        integer, parameter, public :: TAG_BARRIER = -1
        integer, parameter, public :: TAG_WAIT = -2
        integer, parameter, public :: TAG_WAITALL = -3
        integer, parameter, public :: TAG_WAITANY = -4
        integer, parameter, public :: TAG_REDUCE = -5
        integer, parameter, public :: TAG_ALLREDUCE = -6
! MPI operators
        integer, parameter,public :: SPMD_MAX = 1
        integer, parameter,public :: SPMD_MIN = 2
        integer, parameter,public :: SPMD_SUM = 3
        integer, parameter,public :: SPMD_PROD = 4

#ifndef MPI
        integer, parameter, public :: MPI_STATUS_IGNORE = 0
        integer, parameter, public :: MPI_STATUS_SIZE = 1
#endif
        ! \brief Interface for spmd_send, a wrapper for MPI_SEND
        interface spmd_send
          module procedure spmd_send_reals      !< Sends real numbers
          module procedure spmd_send_ints       !< Sends integers
          module procedure spmd_send_doubles    !< Sends double precision numbers
          module procedure spmd_send_real       !< Sends a single real number
          module procedure spmd_send_int        !< Sends a single integer
          module procedure spmd_send_double     !< Sends a single double precision number
        end interface spmd_send

        ! \brief Interface for spmd_recv, a wrapper for MPI_RECV
        interface spmd_recv
          module procedure spmd_recv_reals      !< Receives real numbers
          module procedure spmd_recv_reals2D      !< Receives real numbers
          module procedure spmd_recv_ints       !< Receives integers
          module procedure spmd_recv_doubles    !< Receives double precision numbers
          module procedure spmd_recv_doubles2D   !< Receives double precision numbers
          module procedure spmd_recv_real       !< Receives a single real number
          module procedure spmd_recv_int        !< Receives a single integer
          module procedure spmd_recv_double     !< Receives a single double precision number
        end interface spmd_recv

        ! \brief Interface for spmd_isend, a wrapper for MPI_ISEND
        interface spmd_isend
          module procedure spmd_isend_reals     !< Non-blocking send of real numbers
          module procedure spmd_isend_ints      !< Non-blocking send of integers
          module procedure spmd_isend_doubles   !< Non-blocking send of double precision numbers
          module procedure spmd_isend_real      !< Non-blocking send of a single real number
          module procedure spmd_isend_int       !< Non-blocking send of a single integer
          module procedure spmd_isend_double    !< Non-blocking send of a single double precision number
        end interface spmd_isend

        ! \brief Interface for spmd_irecv, a wrapper for MPI_IRECV
        interface spmd_irecv
          module procedure spmd_irecv_reals     !< Non-blocking receive of real numbers
          module procedure spmd_irecv_ints      !< Non-blocking receive of integers
          module procedure spmd_irecv_doubles   !< Non-blocking receive of double precision numbers
          module procedure spmd_irecv_real      !< Non-blocking receive of a single real number
          module procedure spmd_irecv_int       !< Non-blocking receive of a single integer
          module procedure spmd_irecv_double    !< Non-blocking receive of a single double precision number
        end interface spmd_irecv

        ! \brief Interface for spmd_reduce, a wrapper for MPI_REDUCE
        interface spmd_reduce
          module procedure spmd_reduce_reals    !< Reduces real numbers across all processes
          module procedure spmd_reduce_ints     !< Reduces integers across all processes
          module procedure spmd_reduce_doubles  !< Reduces double precision numbers across all processes
          module procedure spmd_reduce_real     !< Reduces a single real number across all processes
          module procedure spmd_reduce_int      !< Reduces a single integer across all processes
          module procedure spmd_reduce_double   !< Reduces a single double precision number across all processes
        end interface spmd_reduce

        ! \brief Interface for spmd_allreduce, a wrapper for MPI_ALLREDUCE
        interface spmd_allreduce
          module procedure spmd_allreduce_reals   !< Reduces real numbers across all processes and distributes result
          module procedure spmd_allreduce_ints    !< Reduces integers across all processes and distributes result
          module procedure spmd_allreduce_doubles !< Reduces double precision numbers across all processes and distributes result
          module procedure spmd_allreduce_real    !< Reduces a single real number across all processes and distributes result
          module procedure spmd_allreduce_int     !< Reduces a single integer across all processes and distributes result
          module procedure spmd_allreduce_double  !< Reduces a single double precision number across all processes and distributes result
        end interface spmd_allreduce

        public :: spmd_send
        public :: spmd_recv
        public :: spmd_isend
        public :: spmd_irecv
        public :: spmd_reduce
        public :: spmd_allreduce 
        public :: spmd_comm_size        
        public :: spmd_comm_rank
        public :: spmd_waitall
        public :: spmd_wait
        public :: spmd_waitany
        public :: spmd_probe
        public :: spmd_barrier


      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Handle MPI errors
      !||====================================================================
      !||    spmd_out                 ../engine/source/mpi/spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    spmd_allreduce_double    ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_doubles   ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_ints      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_real      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_reals     ../engine/source/mpi/spmd_mod.F90
      !||    spmd_comm_rank           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_comm_size           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_double        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_doubles       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_int           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_ints          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_real          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_reals         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_double        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_doubles       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_int           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_ints          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_real          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_reals         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_probe               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_double         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_doubles        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_doubles2d      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_int            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_ints           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_real           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_reals          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_reals2d        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_double       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_doubles      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_int          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_ints         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_real         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_reals        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_double         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_doubles        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_int            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_ints           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_real           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_reals          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_wait                ../engine/source/mpi/spmd_mod.F90
      !||    spmd_waitall             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_waitany             ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine spmd_out(tag, ierr)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
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
            write(6,*) 'MPI error: ', ierr,' at ',tag
            call MPI_Abort(SPMD_COMM_WORLD, ierr,ierror)
          end if
#ifdef DEBUG_SPMD
          write(6,*) 'Exiting MPI call: ', tag
#endif
#endif
        end subroutine spmd_out

!! \brief Trace Entry in MPI subroutines
      !||====================================================================
      !||    spmd_in                  ../engine/source/mpi/spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    spmd_allreduce_double    ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_doubles   ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_ints      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_real      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_reals     ../engine/source/mpi/spmd_mod.F90
      !||    spmd_comm_rank           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_comm_size           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_double        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_doubles       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_int           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_ints          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_real          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_irecv_reals         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_double        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_doubles       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_int           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_ints          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_real          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_isend_reals         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_probe               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_double         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_doubles        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_doubles2d      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_int            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_ints           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_real           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_reals          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_recv_reals2d        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_double       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_doubles      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_int          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_ints         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_real         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_reals        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_double         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_doubles        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_int            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_ints           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_real           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_send_reals          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_wait                ../engine/source/mpi/spmd_mod.F90
      !||    spmd_waitall             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_waitany             ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_in(tag)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer :: tag
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
#ifdef DEBUG_SPMD
          write(6,*) 'Entering MPI call: ', tag
#endif
        end subroutine spmd_in

!!\brief get MPI rank
      !||====================================================================
      !||    spmd_comm_rank        ../engine/source/mpi/spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_element_init   ../engine/source/mpi/python_spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out              ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_comm_rank(rank, comm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(out) :: rank !< Rank of the process
          integer, intent(in), optional :: comm !< Communicator
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call spmd_in(0)
#ifdef MPI
          if(present(comm)) then
            call MPI_Comm_rank(comm, rank, ierr)
          else
            call MPI_Comm_rank(SPMD_COMM_WORLD, rank, ierr)
          end if
#else
          rank = 0
#endif
          call spmd_out(0,ierr)
        end subroutine spmd_comm_rank

!!\brief get MPI size
      !||====================================================================
      !||    spmd_comm_size        ../engine/source/mpi/spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_element_init   ../engine/source/mpi/python_spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out              ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_comm_size(rank, comm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(out) :: rank !< Rank of the process
          integer, intent(in), optional :: comm !< Communicator
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call spmd_in(0)
#ifdef MPI
          if(present(comm)) then
            call MPI_Comm_size(comm, rank, ierr)
          else
            call MPI_Comm_size(SPMD_COMM_WORLD, rank, ierr)
          end if
#else
          rank = 0
#endif
          call spmd_out(0,ierr)
        end subroutine spmd_comm_size


!! \brief Get the MPI operator for a given SPMD operator
      !||====================================================================
      !||    get_mpi_operator         ../engine/source/mpi/spmd_mod.F90
      !||--- called by ------------------------------------------------------
      !||    spmd_allreduce_double    ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_doubles   ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_int       ../engine/source/mpi/generic/spmd_allreduce_db.F
      !||    spmd_allreduce_ints      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_real      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_allreduce_reals     ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_double       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_doubles      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_int          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_ints         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_real         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_reduce_reals        ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        function get_mpi_operator(spmd_op) result(mpi_operator)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: spmd_op
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: mpi_operator
#ifdef MPI
          select case(spmd_op)
           case(SPMD_MAX)
            mpi_operator = MPI_MAX
           case(SPMD_MIN)
            mpi_operator = MPI_MIN
           case(SPMD_SUM)
            mpi_operator = MPI_SUM
           case(SPMD_PROD)
            mpi_operator = MPI_PROD
           case default
            mpi_operator = MPI_OP_NULL
          end select
#else
          mpi_operator = 0
#endif
        end function get_mpi_operator

! ======================================================================================================================
!                                                  WRAPPER
! ======================================================================================================================
!   The remaining subroutines are wrappers for the MPI subroutines.
!   They are not meant to be called directly, but through the interfaces defined above.
!   See MPI documentation for the meaning of the arguments.
! ======================================================================================================================
      !||====================================================================
      !||    spmd_barrier    ../engine/source/mpi/generic/spmd_barrier.F
      !||--- called by ------------------------------------------------------
      !||    check_nan_acc   ../engine/source/output/outfile/check_nan_acc.F
      !||    inttri          ../engine/source/interfaces/intsort/inttri.F
      !||    resol           ../engine/source/engine/resol.F
      !||    sphprep         ../engine/source/elements/sph/sphprep.F
      !||    sphtri0         ../engine/source/elements/sph/sphtri0.F
      !||    thermbilan      ../engine/source/constraints/thermic/thermbilan.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine spmd_barrier(comm)
          implicit none
#include "spmd.inc"
          integer, optional, intent(in) :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(TAG_BARRIER)
          if(present(comm)) then
            call MPI_Barrier(comm, ierr)
          else
            call MPI_Barrier(SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(TAG_BARRIER,ierr)
#endif
        end subroutine spmd_barrier
! ======================================================================================================================
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
      !||====================================================================
      !||    spmd_send_reals   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_send_reals(buf, buf_count, dest, tag,  comm)
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
! ======================================================================================================================
      !||====================================================================
      !||    spmd_send_ints   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_send_ints(buf, buf_count, dest, tag, comm)
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
! ======================================================================================================================
      !||====================================================================
      !||    spmd_send_doubles   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out            ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_send_doubles(buf, buf_count, dest, tag, comm)
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
! ======================================================================================================================
      !||====================================================================
      !||    spmd_recv_reals   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_reals(buf, buf_count, source, tag, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count), intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_reals
! ======================================================================================================================
      !||====================================================================
      !||    spmd_recv_reals2d   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out            ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_reals2D(buf, buf_count, source, tag, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count,1), intent(inout) :: buf
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_reals2D

! ======================================================================================================================
      !||====================================================================
      !||    spmd_recv_ints   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_ints(buf, buf_count, source, tag, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(buf_count), intent(inout) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
#endif
          call spmd_out(tag,ierr)
        end subroutine spmd_recv_ints
! ======================================================================================================================
      !||====================================================================
      !||    spmd_recv_doubles   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out            ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_doubles(buf, buf_count, source, tag,  comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count), intent(inout) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_doubles
! ======================================================================================================================
      !||====================================================================
      !||    spmd_recv_doubles2d   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out              ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_doubles2D(buf, buf_count, source, tag,  comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count,1), intent(inout) :: buf
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_doubles2D

! ======================================================================================================================
      !||====================================================================
      !||    spmd_isend_reals   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_isend_reals(buf, buf_count, dest, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          real, dimension(buf_count), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_reals
! ======================================================================================================================
      !||====================================================================
      !||    spmd_isend_ints   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_isend_ints(buf, buf_count, dest, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, dimension(buf_count), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_ints
! ======================================================================================================================
      !||====================================================================
      !||    spmd_isend_doubles   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in              ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out             ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_isend_doubles(buf, buf_count, dest, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, dimension(buf_count), intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_irecv_reals   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_irecv_reals(buf, buf_count, source, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real, dimension(buf_count), intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_reals
! ======================================================================================================================
      !||====================================================================
      !||    spmd_irecv_ints   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_irecv_ints(buf, buf_count, source, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          integer, dimension(buf_count), intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_ints
! ======================================================================================================================
      !||====================================================================
      !||    spmd_irecv_doubles   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in              ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out             ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_irecv_doubles(buf, buf_count, source, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          double precision, dimension(buf_count), intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
#ifdef MPI
          integer :: ierr
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_doubles
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
! ======================================================================================================================
      !||====================================================================
      !||    spmd_probe   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out     ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_probe(source, tag, comm, status)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: source, tag
          integer, intent(in), optional :: comm
          integer, dimension(MPI_STATUS_SIZE), intent(inout) :: status
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Probe(source, tag, comm, status, ierr)
          else
            call MPI_Probe(source, tag, SPMD_COMM_WORLD, status, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_probe
! ======================================================================================================================
      !||====================================================================
      !||    spmd_reduce_reals   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator    ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out            ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_reduce_reals(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf(*)
          real, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_reals
! ======================================================================================================================
      !||====================================================================
      !||    spmd_reduce_ints   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator   ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_reduce_ints(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf(*)
          integer, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_ints
! ======================================================================================================================
      !||====================================================================
      !||    spmd_reduce_doubles   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out              ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_reduce_doubles(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf(*)
          double precision, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)

#endif
        end subroutine spmd_reduce_doubles
! ======================================================================================================================
      !||====================================================================
      !||    spmd_allreduce_ints   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out              ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_allreduce_ints(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf(*)
          integer, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)

#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_allreduce_doubles   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in                  ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out                 ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_allreduce_doubles(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf(*)
          double precision, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)

#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_allreduce_reals   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator       ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in                ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out               ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_allreduce_reals(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf(*)
          real, intent(inout) :: recvbuf(*)
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_send_int   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out        ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_send_int(buf, buf_count, dest, tag, comm)
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
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_send_double   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_send_double(buf, buf_count, dest, tag, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          double precision,  intent(in) :: buf
          integer :: ierr
          ! the MPI datatype for double precision is MPI_DOUBLE_PRECISION
#ifdef MPI
          call spmd_in(tag)
          if( present(comm) ) then
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, ierr)
          else
            call MPI_Send(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, ierr)
          end if
          call spmd_out(tag,ierr)
#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_send_real   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_send_real(buf, buf_count, dest, tag, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in), optional :: comm
          real,  intent(in) :: buf
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
        end subroutine

      !||====================================================================
      !||    spmd_recv_real   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_real(buf, buf_count, source, tag, comm)
          implicit none
#include "spmd.inc"
          real,  intent(inout) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_real
! ======================================================================================================================
      !||====================================================================
      !||    spmd_recv_int   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in         ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out        ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_int(buf, buf_count, source, tag,  comm)
          implicit none
#include "spmd.inc"
          integer, intent(inout) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_INT, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_int
! ======================================================================================================================
      !||====================================================================
      !||    spmd_recv_double   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_recv_double(buf, buf_count, source, tag,  comm)
          implicit none
#include "spmd.inc"
          double precision, intent(inout) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, MPI_STATUS_IGNORE, ierr)
          else
            call MPI_Recv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_recv_double
! ======================================================================================================================
      !||====================================================================
      !||    spmd_isend_real   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_isend_real(buf, buf_count, dest, tag, request, comm)
          implicit none
#include "spmd.inc"
          real,  intent(in) :: buf
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_REAL, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_real
! ======================================================================================================================
      !||====================================================================
      !||    spmd_isend_int   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_isend_int(buf, buf_count, dest, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          integer, intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_INT, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_isend_int
! ======================================================================================================================
      !||====================================================================
      !||    spmd_isend_double   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out            ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_isend_double(buf, buf_count, dest, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, dest, tag
          double precision, intent(in) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, comm, request, ierr)
          else
            call MPI_Isend(buf, buf_count, MPI_DOUBLE_PRECISION, dest, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_irecv_real   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in           ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out          ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_irecv_real(buf, buf_count, source, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: buf_count, source, tag
          real,    intent(inout) :: buf
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_REAL, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_real
! ======================================================================================================================
      !||====================================================================
      !||    spmd_irecv_int   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in          ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_irecv_int(buf, buf_count, source, tag, request, comm)
          implicit none
#include "spmd.inc"
          integer, intent(inout) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_INT, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_int
! ======================================================================================================================
      !||====================================================================
      !||    spmd_irecv_double   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    spmd_in             ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out            ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_irecv_double(buf, buf_count, source, tag, request, comm)
          implicit none
#include "spmd.inc"
          double precision, intent(inout) :: buf
          integer, intent(in) :: buf_count, source, tag
          integer, intent(inout) :: request
          integer, intent(in), optional :: comm
          integer :: ierr
#ifdef MPI
          call spmd_in(tag)
          if (present(comm)) then
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, comm, request, ierr)
          else
            call MPI_Irecv(buf, buf_count, MPI_DOUBLE_PRECISION, source, tag, SPMD_COMM_WORLD, request, ierr)
          endif
          call spmd_out(tag,ierr)
#endif
        end subroutine spmd_irecv_double
! ======================================================================================================================
      !||====================================================================
      !||    spmd_reduce_real   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator   ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_reduce_real(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf
          real, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm

          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_real
! ======================================================================================================================
      !||====================================================================
      !||    spmd_reduce_int    ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator   ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in            ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out           ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_reduce_int(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf
          integer, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_int
! ======================================================================================================================
      !||====================================================================
      !||    spmd_reduce_double   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator     ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in              ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out             ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_reduce_double(sendbuf, recvbuf, buf_count, operation, root, comm)
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf
          double precision, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation, root
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_REDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Reduce(sendbuf, recvbuf, buf_count, MPI_REAL, mpi_op, root, used_comm, ierr)
          call spmd_out(TAG_REDUCE,ierr)
#endif
        end subroutine spmd_reduce_double
! ======================================================================================================================
      !||====================================================================
      !||    spmd_allreduce_int   ../engine/source/mpi/generic/spmd_allreduce_db.F
      !||--- called by ------------------------------------------------------
      !||    radioss2             ../engine/source/engine/radioss2.F
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator     ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_allreduce_int(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#include "spmd.inc"
          integer, intent(in) :: sendbuf
          integer, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_INTEGER, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_allreduce_double   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator        ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in                 ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out                ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_allreduce_double(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#include "spmd.inc"
          double precision, intent(in) :: sendbuf
          double precision, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
! ======================================================================================================================
      !||====================================================================
      !||    spmd_allreduce_real   ../engine/source/mpi/spmd_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
      !||    spmd_in               ../engine/source/mpi/spmd_mod.F90
      !||    spmd_out              ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine spmd_allreduce_real(sendbuf, recvbuf, buf_count, operation, comm)
          implicit none
#include "spmd.inc"
          real, intent(in) :: sendbuf
          real, intent(inout) :: recvbuf
          integer, intent(in) :: buf_count, operation
          integer, intent(in), optional :: comm
          integer :: ierr, mpi_op, used_comm
#ifdef MPI
          call spmd_in(TAG_ALLREDUCE)
          mpi_op = get_mpi_operator(operation)

          ! Determine the communicator to use
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          endif

          call MPI_Allreduce(sendbuf, recvbuf, buf_count, MPI_DOUBLE_PRECISION, mpi_op, used_comm, ierr)
          call spmd_out(TAG_ALLREDUCE,ierr)
#endif
        end subroutine
      end module spmd_mod
