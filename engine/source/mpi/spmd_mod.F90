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
!||    check_ale_comm                  ../engine/source/ale/check_ale_comm.F
!||    check_nan_acc                   ../engine/source/output/outfile/check_nan_acc.F
!||    genh3d                          ../engine/source/output/h3d/h3d_results/genh3d.F
!||    init_ghost_shells               ../engine/source/engine/node_spliting/ghost_shells.F90
!||    init_global_frontier_monvol     ../engine/source/airbag/init_global_monvol_frontier.F90
!||    init_global_node_id             ../common_source/modules/nodal_arrays.F90
!||    inter_init_component            ../engine/source/interfaces/generic/inter_init_component.F90
!||    inttri                          ../engine/source/interfaces/intsort/inttri.F
!||    python_element_init             ../engine/source/mpi/python_spmd_mod.F90
!||    python_element_sync             ../engine/source/mpi/python_spmd_mod.F90
!||    radioss2                        ../engine/source/engine/radioss2.F
!||    resol                           ../engine/source/engine/resol.F
!||    sensor_dist_surf0               ../engine/source/tools/sensor/sensor_dist_surf0.F
!||    sensor_spmd                     ../engine/source/tools/sensor/sensor_spmd.F
!||    sensor_temp0                    ../engine/source/tools/sensor/sensor_temp0.F
!||    sph_crit_voxel                  ../engine/source/elements/sph/sph_crit_voxel.F90
!||    sphprep                         ../engine/source/elements/sph/sphprep.F
!||    sphtri0                         ../engine/source/elements/sph/sphtri0.F
!||    spmd_aget_sect                  ../engine/source/mpi/anim/spmd_aget_sect.F
!||    spmd_agetmsr                    ../engine/source/mpi/anim/spmd_agetmsr.F
!||    spmd_all_dmax                   ../engine/source/mpi/elements/spmd_sph.F
!||    spmd_anim_ply_init              ../engine/source/mpi/anim/spmd_anim_ply_init.F
!||    spmd_anim_ply_xyznod            ../engine/source/mpi/anim/spmd_anim_ply_xyznod.F
!||    spmd_box_limit_reduction        ../engine/source/mpi/interfaces/spmd_box_limit_reduction.F
!||    spmd_cell_list_exchange         ../engine/source/mpi/interfaces/spmd_cell_list_exchange.F
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
!||    spmd_exch_fvstats               ../engine/source/mpi/airbags/spmd_exch_fvstats.F
!||    spmd_exch_neighbour_segment     ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||    spmd_exch_nodnx                 ../engine/source/mpi/ams/spmd_exch_nodnx.F
!||    spmd_exch_sms                   ../engine/source/mpi/ams/spmd_exch_sms.F
!||    spmd_exch_sms6                  ../engine/source/mpi/ams/spmd_exch_sms6.F
!||    spmd_exchange_component         ../engine/source/mpi/interfaces/spmd_exch_component.F90
!||    spmd_exchange_ghost_shells      ../engine/source/engine/node_spliting/ghost_shells.F90
!||    spmd_extag                      ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_fi_sms                     ../engine/source/mpi/ams/spmd_fi_sms.F
!||    spmd_fvb_switch                 ../engine/source/mpi/airbags/spmd_fvb_switch.F
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
!||    spmd_mv_ca                      ../engine/source/mpi/airbags/spmd_mv_ca.F
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
!||    spmd_vfi_sms                    ../engine/source/mpi/ams/spmd_vfi_sms.F
!||    spmd_wvois                      ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_xv_inter_type1             ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
!||    spmd_xvois                      ../engine/source/mpi/fluid/spmd_cfd.F
!||    telesc                          ../engine/source/constraints/general/cyl_joint/telesc.F
!||    test_jc_shell_detach            ../engine/source/engine/node_spliting/detach_node.F90
!||    thermbilan                      ../engine/source/constraints/thermic/thermbilan.F
!||--- uses       -----------------------------------------------------
!||    spmd_allgather_mod              ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgatherv_mod             ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_alltoall_mod               ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_comm_world_mod             ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_irecv_mod                  ../engine/source/mpi/spmd_irecv.F90
!||    spmd_isend_mod                  ../engine/source/mpi/spmd_isend.F90
!||    spmd_pack_mod                   ../engine/source/mpi/spmd_pack.F90
!||    spmd_recv_mod                   ../engine/source/mpi/spmd_recv.F90
!||    spmd_send_mod                   ../engine/source/mpi/spmd_send.F90
!||    spmd_unpack_mod                 ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait_mod                   ../engine/source/mpi/spmd_wait.F90
!||====================================================================
      module spmd_mod
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD
        use spmd_allgather_mod , only : spmd_allgather
        use spmd_send_mod, only: spmd_send
        use spmd_recv_mod, only: spmd_recv
        use spmd_isend_mod, only: spmd_isend
        use spmd_irecv_mod, only: spmd_irecv
        use spmd_wait_mod, only: spmd_wait, spmd_waitall, spmd_waitany
        use spmd_allgatherv_mod, only: spmd_allgatherv
        use spmd_alltoall_mod, only: spmd_alltoall
        use spmd_pack_mod, only: spmd_pack
        use spmd_unpack_mod, only: spmd_unpack
        implicit none
        ! Define the interface for spmd_send
! dummy tags for MPI calls that do not have a tag

        private

        integer, parameter, public :: TAG_BARRIER = -1
        integer, parameter, public :: TAG_REDUCE = -5
        integer, parameter, public :: TAG_ALLREDUCE = -6
! MPI operators
        integer, parameter,public :: SPMD_MAX = 1
        integer, parameter,public :: SPMD_MIN = 2
        integer, parameter,public :: SPMD_SUM = 3
        integer, parameter,public :: SPMD_PROD = 4
#ifdef REAL8
        integer, parameter, public :: SPMD_REAL8 = 1
#else
        integer, parameter, public :: SPMD_REAL8 = 0
#endif

#ifndef MPI
        integer, parameter, public :: MPI_STATUS_IGNORE = 0
        integer, parameter, public :: MPI_STATUS_SIZE = 1
        integer, parameter, public :: MPI_REQUEST_NULL = 0
#endif
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
        public :: spmd_allgather
        public :: SPMD_COMM_WORLD
        public :: spmd_allgatherv
        public :: spmd_alltoall
        public :: spmd_pack
        public :: spmd_unpack

      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!||====================================================================
!||    spmd_comm_rank        ../engine/source/mpi/spmd_mod.F90
!||--- called by ------------------------------------------------------
!||    python_element_init   ../engine/source/mpi/python_spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_comm_rank(rank, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_comm_size(rank, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
!||    spmd_allreduce_int       ../engine/source/mpi/spmd_mod.F90
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
!||    spmd_barrier     ../engine/source/mpi/spmd_mod.F90
!||--- called by ------------------------------------------------------
!||    check_nan_acc    ../engine/source/output/outfile/check_nan_acc.F
!||    inttri           ../engine/source/interfaces/intsort/inttri.F
!||    resol            ../engine/source/engine/resol.F
!||    sphprep          ../engine/source/elements/sph/sphprep.F
!||    sphtri0          ../engine/source/elements/sph/sphtri0.F
!||    thermbilan       ../engine/source/constraints/thermic/thermbilan.F
!||--- calls      -----------------------------------------------------
!||    spmd_in          ../engine/source/mpi/spmd_error.F90
!||    spmd_out         ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod   ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_barrier(comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
!||    spmd_probe       ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    spmd_in          ../engine/source/mpi/spmd_error.F90
!||    spmd_out         ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod   ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_probe(source, tag, comm, status)
          use spmd_error_mod, only: spmd_in, spmd_out
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
!||    spmd_in             ../engine/source/mpi/spmd_error.F90
!||    spmd_out            ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod      ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_reals(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
!||    spmd_in            ../engine/source/mpi/spmd_error.F90
!||    spmd_out           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_ints(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
#else
          recvbuf(1:buf_count) = sendbuf(1:buf_count)
#endif
        end subroutine spmd_reduce_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_doubles   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_doubles(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_ints(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
        end subroutine spmd_allreduce_ints
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_doubles   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator         ../engine/source/mpi/spmd_mod.F90
!||    spmd_in                  ../engine/source/mpi/spmd_error.F90
!||    spmd_out                 ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod           ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_doubles(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
        end subroutine spmd_allreduce_doubles
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_reals   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator       ../engine/source/mpi/spmd_mod.F90
!||    spmd_in                ../engine/source/mpi/spmd_error.F90
!||    spmd_out               ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod         ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_reals(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
        end subroutine spmd_allreduce_reals
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_real   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator   ../engine/source/mpi/spmd_mod.F90
!||    spmd_in            ../engine/source/mpi/spmd_error.F90
!||    spmd_out           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_real(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
!||    spmd_in            ../engine/source/mpi/spmd_error.F90
!||    spmd_out           ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod     ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_int(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_reduce_int
! ======================================================================================================================
!||====================================================================
!||    spmd_reduce_double   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator     ../engine/source/mpi/spmd_mod.F90
!||    spmd_in              ../engine/source/mpi/spmd_error.F90
!||    spmd_out             ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod       ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_reduce_double(sendbuf, recvbuf, buf_count, operation, root, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_reduce_double
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_int   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator     ../engine/source/mpi/spmd_mod.F90
!||    spmd_in              ../engine/source/mpi/spmd_error.F90
!||    spmd_out             ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod       ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_int(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_allreduce_int
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_double   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator        ../engine/source/mpi/spmd_mod.F90
!||    spmd_in                 ../engine/source/mpi/spmd_error.F90
!||    spmd_out                ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod          ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_double(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_allreduce_double
! ======================================================================================================================
!||====================================================================
!||    spmd_allreduce_real   ../engine/source/mpi/spmd_mod.F90
!||--- calls      -----------------------------------------------------
!||    get_mpi_operator      ../engine/source/mpi/spmd_mod.F90
!||    spmd_in               ../engine/source/mpi/spmd_error.F90
!||    spmd_out              ../engine/source/mpi/spmd_error.F90
!||--- uses       -----------------------------------------------------
!||    spmd_error_mod        ../engine/source/mpi/spmd_error.F90
!||====================================================================
        subroutine spmd_allreduce_real(sendbuf, recvbuf, buf_count, operation, comm)
          use spmd_error_mod, only: spmd_in, spmd_out
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
#else
          recvbuf = sendbuf  ! In case MPI is not defined, just copy the value
#endif
        end subroutine spmd_allreduce_real
      end module spmd_mod
