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
!||    spmd_mod                        ../engine/source/mpi/spmd_mod.F90
!||--- called by ------------------------------------------------------
!||    aconve                          ../engine/source/ale/aconve.F90
!||    apply_crack                     ../engine/source/engine/node_spliting/apply_crack.F90
!||    arezon                          ../engine/source/ale/arezon.F90
!||    check_ale_comm                  ../engine/source/ale/check_ale_comm.F
!||    check_nan_acc                   ../engine/source/output/outfile/check_nan_acc.F
!||    genh3d                          ../engine/source/output/h3d/h3d_results/genh3d.F
!||    init_ale_aconve                 ../engine/source/ale/init_ale_aconve.F90
!||    init_ale_arezon                 ../engine/source/ale/init_ale_arezon.F90
!||    init_ghost_shells               ../engine/source/engine/node_spliting/ghost_shells.F90
!||    init_global_frontier_monvol     ../engine/source/airbag/init_global_monvol_frontier.F90
!||    init_global_node_id             ../common_source/modules/nodal_arrays.F90
!||    intcrit                         ../engine/source/interfaces/intsort/intcrit.F
!||    inter_init_component            ../engine/source/interfaces/generic/inter_init_component.F90
!||    inttri                          ../engine/source/interfaces/intsort/inttri.F
!||    nloc_shell_detach               ../engine/source/engine/node_spliting/nloc_shell_detach.F90
!||    python_element_init             ../engine/source/mpi/python_spmd_mod.F90
!||    python_element_sync             ../engine/source/mpi/python_spmd_mod.F90
!||    radioss2                        ../engine/source/engine/radioss2.F
!||    resol                           ../engine/source/engine/resol.F
!||    s4alesfem                       ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
!||    s4lagsfem                       ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
!||    sensor_dist_surf0               ../engine/source/tools/sensor/sensor_dist_surf0.F
!||    sensor_spmd                     ../engine/source/tools/sensor/sensor_spmd.F
!||    sensor_temp0                    ../engine/source/tools/sensor/sensor_temp0.F
!||    sfem_init_spmd                  ../engine/source/elements/solid/solide4_sfem/sfem_init_spmd.F90
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
!||    spmd_exch_min_max               ../engine/source/mpi/ale/spmd_exch_min_max.F90
!||    spmd_exch_n_neighbor_2d         ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    spmd_exch_n_neighbor_3d         ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    spmd_exch_neighbour_segment     ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||    spmd_exch_nodnx                 ../engine/source/mpi/ams/spmd_exch_nodnx.F
!||    spmd_exch_sms                   ../engine/source/mpi/ams/spmd_exch_sms.F
!||    spmd_exch_sms6                  ../engine/source/mpi/ams/spmd_exch_sms6.F
!||    spmd_exch_vnpon                 ../engine/source/mpi/nodes/spmd_exch_vnpon.F90
!||    spmd_exch_wave                  ../engine/source/mpi/nodes/spmd_exch_wave.F
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
!||    spmd_rebuild_boundary           ../engine/source/engine/node_spliting/spmd_rebuild_boundary.F90
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
!||    thermbilan                      ../engine/source/constraints/thermic/thermbilan.F
!||    viper_mod                       ../engine/source/coupling/viper/viper_interface_mod.F90
!||--- uses       -----------------------------------------------------
!||    get_mpi_operator_mod            ../engine/source/mpi/get_mpi_operator.F90
!||    spmd_allgather_mod              ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgatherv_mod             ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_mod              ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_alltoall_mod               ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoallv_mod              ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_comm_world_mod             ../engine/source/mpi/spmd_comm_world.F90
!||    spmd_iallreduce_mod             ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_irecv_mod                  ../engine/source/mpi/spmd_irecv.F90
!||    spmd_isend_mod                  ../engine/source/mpi/spmd_isend.F90
!||    spmd_pack_mod                   ../engine/source/mpi/spmd_pack.F90
!||    spmd_recv_mod                   ../engine/source/mpi/spmd_recv.F90
!||    spmd_send_mod                   ../engine/source/mpi/spmd_send.F90
!||    spmd_unpack_mod                 ../engine/source/mpi/spmd_unpack.F90
!||    spmd_wait_mod                   ../engine/source/mpi/spmd_wait.F90
!||====================================================================
      module spmd_mod

        ! Core environment
        use spmd_comm_world_mod, only: SPMD_COMM_WORLD, SPMD_REQUEST_NULL
        use spmd_operator_mod,   only: SPMD_MAX, SPMD_MIN, SPMD_SUM, SPMD_PROD
        use get_mpi_operator_mod, only: get_mpi_operator
        use spmd_profiler_mod,   only: spmd_profiler_init, spmd_profiler_flush, &
          spmd_profile_begin, spmd_profile_end, spmd_profiling_enabled
        use spmd_error_mod,      only: spmd_in, spmd_out

        ! Point-to-point
        use spmd_send_mod,       only: spmd_send
        use spmd_recv_mod,       only: spmd_recv
        use spmd_isend_mod,      only: spmd_isend
        use spmd_irecv_mod,      only: spmd_irecv
        use spmd_sendrecv_mod,   only: spmd_sendrecv
        use spmd_sendrecv_replace_mod, only: spmd_sendrecv_replace

        ! Collectives
        use spmd_bcast_mod,      only: spmd_bcast
        ! Note: spmd_ibcast NOT re-exported here to avoid collision with legacy
        ! SPMD_IBCAST subroutine in spmd_ibcast.F (different calling convention).
        ! Use "use spmd_ibcast_mod, only: spmd_ibcast" explicitly for the MPI_Ibcast wrapper.
        use spmd_barrier_mod,    only: spmd_barrier
        use spmd_gather_mod,     only: spmd_gather
        use spmd_igather_mod,    only: spmd_igather
        use spmd_gatherv_mod,    only: spmd_gatherv
        use spmd_allgather_mod,  only: spmd_allgather
        use spmd_iallgather_mod, only: spmd_iallgather
        use spmd_allgatherv_mod,  only: spmd_allgatherv
        use spmd_iallgatherv_mod, only: spmd_iallgatherv
        use spmd_scatter_mod,    only: spmd_scatter
        use spmd_iscatter_mod,   only: spmd_iscatter
        use spmd_scatterv_mod,   only: spmd_scatterv
        use spmd_alltoall_mod,   only: spmd_alltoall
        use spmd_ialltoall_mod,  only: spmd_ialltoall
        use spmd_alltoallv_mod,  only: spmd_alltoallv
        use spmd_ialltoallv_mod, only: spmd_ialltoallv
        use spmd_reduce_mod,     only: spmd_reduce
        use spmd_ireduce_mod,    only: spmd_ireduce
        use spmd_allreduce_mod,  only: spmd_allreduce
        use spmd_iallreduce_mod, only: spmd_iallreduce

        ! Status / counting / probing
        use spmd_get_count_mod,  only: spmd_get_count_real, spmd_get_count_int, spmd_get_count_double
        use spmd_probe_mod,      only: spmd_probe
        use spmd_iprobe_mod,     only: spmd_iprobe

        ! Request/test/wait helpers
        use spmd_wait_mod,       only: spmd_wait
        use spmd_waitall_mod,    only: spmd_waitall
        use spmd_waitany_mod,    only: spmd_waitany
        use spmd_waitsome_mod,   only: spmd_waitsome
        use spmd_test_mod,       only: spmd_test
        use spmd_testall_mod,    only: spmd_testall
        use spmd_testany_mod,    only: spmd_testany
        use spmd_testsome_mod,   only: spmd_testsome

        ! Legacy OpenRadioss modules (pack/unpack, kept for compatibility)
        use spmd_pack_mod, only: spmd_pack
        use spmd_unpack_mod, only: spmd_unpack

        implicit none
        private

#ifdef REAL8
        integer, parameter, public :: SPMD_REAL8 = 1
#define MY_MPI_REAL MPI_DOUBLE_PRECISION
#else
        integer, parameter, public :: SPMD_REAL8 = 0
#define MY_MPI_REAL MPI_REAL
#endif
#ifndef MPI
        public :: MPI_STATUS_IGNORE
        public :: MPI_STATUS_SIZE
        public :: MPI_REQUEST_NULL
        public :: MPI_COMM_WORLD
        public :: SPMD_STATUS_IGNORE
        public :: SPMD_STATUS_SIZE
        public :: SPMD_REQUEST_NULL
#else
        public :: SPMD_REQUEST_NULL
#endif

        ! Core environment
        public :: SPMD_COMM_WORLD
        public :: SPMD_MAX, SPMD_MIN, SPMD_SUM, SPMD_PROD, get_mpi_operator
        public :: spmd_profiler_init, spmd_profiler_flush, spmd_profiling_enabled
        public :: spmd_profile_begin, spmd_profile_end
        public :: spmd_in, spmd_out

        ! Point-to-point
        public :: spmd_send, spmd_recv, spmd_isend, spmd_irecv
        public :: spmd_sendrecv, spmd_sendrecv_replace

        ! Collectives
        public :: spmd_bcast, spmd_barrier
        public :: spmd_gather, spmd_igather, spmd_gatherv
        public :: spmd_allgather, spmd_iallgather, spmd_allgatherv, spmd_iallgatherv
        public :: spmd_scatter, spmd_iscatter, spmd_scatterv
        public :: spmd_alltoall, spmd_ialltoall, spmd_alltoallv, spmd_ialltoallv
        public :: spmd_reduce, spmd_ireduce, spmd_allreduce, spmd_iallreduce

        ! Status / counting / probing
        public :: spmd_get_count_real, spmd_get_count_int, spmd_get_count_double
        public :: spmd_probe, spmd_iprobe

        ! Request/test/wait helpers
        public :: spmd_wait, spmd_waitall, spmd_waitany, spmd_waitsome
        public :: spmd_test, spmd_testall, spmd_testany, spmd_testsome

        ! Legacy OpenRadioss (pack/unpack)
        public :: spmd_pack, spmd_unpack

        ! Convenience wrappers
        public :: spmd_comm_size, spmd_comm_rank

      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Query the size of a communicator (defaults to SPMD_COMM_WORLD)
        subroutine spmd_comm_size(size, comm)
          implicit none
#include "spmd.inc"
          integer, intent(out) :: size
          integer, intent(in), optional :: comm
          integer :: ierr, used_comm
          integer :: tag_local

          tag_local = -1012
#ifdef MPI
          call spmd_in(tag_local, "MPI_Comm_size")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Comm_size(used_comm, size, ierr)
          call spmd_out(tag_local, ierr)
#else
          size = 1
#endif
        end subroutine spmd_comm_size

! ======================================================================================================================
!! \brief Query the rank in a communicator (defaults to SPMD_COMM_WORLD)
        subroutine spmd_comm_rank(rank, comm)
          implicit none
#include "spmd.inc"
          integer, intent(out) :: rank
          integer, intent(in), optional :: comm
          integer :: ierr, used_comm
          integer :: tag_local

          tag_local = -1011
#ifdef MPI
          call spmd_in(tag_local, "MPI_Comm_rank")
          if (present(comm)) then
            used_comm = comm
          else
            used_comm = SPMD_COMM_WORLD
          end if
          call MPI_Comm_rank(used_comm, rank, ierr)
          call spmd_out(tag_local, ierr)
#else
          rank = 0
#endif
        end subroutine spmd_comm_rank

      end module spmd_mod
