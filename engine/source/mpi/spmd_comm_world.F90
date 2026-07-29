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
!||    spmd_comm_world_mod             ../engine/source/mpi/spmd_comm_world.F90
!||--- called by ------------------------------------------------------
!||    ams_prepare_poff_assembly       ../engine/source/mpi/ams/spmd_sms.F
!||    execargcheck                    ../engine/source/engine/execargcheck.F
!||    get_global_dim                  ../engine/share/modules/linear_solver_mod.F
!||    get_solution                    ../engine/share/modules/diffusion_mod.F
!||    h3d_create_rbe2_impi            ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbe2_impi.F
!||    h3d_create_rbe3_impi            ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbe3_impi.F
!||    h3d_create_rbodies_impi         ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbodies_impi.F
!||    h3d_gather_id_val               ../engine/source/output/h3d/spmd/h3d_gather_id_val.F90
!||    imp_mumps1                      ../engine/source/implicit/imp_mumps.F
!||    imp_mumps2                      ../engine/source/implicit/imp_mumps.F
!||    inipar                          ../engine/source/mpi/init/inipar.F
!||    init_diffusion                  ../engine/share/modules/diffusion_mod.F
!||    init_solver                     ../engine/share/modules/linear_solver_mod.F
!||    init_solver_cg                  ../engine/share/modules/linear_solver_mod.F
!||    init_solver_mumps               ../engine/share/modules/linear_solver_mod.F
!||    isanargument                    ../engine/source/engine/execargcheck.F
!||    mpi_min_real_begin              ../engine/share/modules/mpi_tools_mod.F
!||    mpi_min_real_end                ../engine/share/modules/mpi_tools_mod.F
!||    mumps_set                       ../engine/source/implicit/imp_mumps.F
!||    mumps_set2                      ../engine/source/implicit/imp_mumps.F
!||    pexecinfo                       ../engine/source/engine/execargcheck.F
!||    phelpinfo                       ../engine/source/engine/execargcheck.F
!||    prexecinfo                      ../engine/source/engine/execargcheck.F
!||    prhelpinfo                      ../engine/source/engine/execargcheck.F
!||    print_stiff_mat                 ../engine/source/implicit/imp_mumps.F
!||    rad_spmd_recv                   ../engine/source/mpi/generic/rad_spmd_recv.F
!||    rad_spmd_send                   ../engine/source/mpi/generic/rad_spmd_send.F
!||    set_matrix                      ../engine/share/modules/linear_solver_mod.F
!||    set_matrix_cg                   ../engine/share/modules/linear_solver_mod.F
!||    set_matrix_mumps                ../engine/share/modules/linear_solver_mod.F
!||    set_rhs                         ../engine/share/modules/linear_solver_mod.F
!||    set_rhs_cg                      ../engine/share/modules/linear_solver_mod.F
!||    set_rhs_mumps                   ../engine/share/modules/linear_solver_mod.F
!||    solve                           ../engine/share/modules/linear_solver_mod.F
!||    solve_cg                        ../engine/share/modules/linear_solver_mod.F
!||    solve_diffusion                 ../engine/share/modules/diffusion_mod.F
!||    solve_mumps                     ../engine/share/modules/linear_solver_mod.F
!||    spmd_allgather_mod              ../engine/source/mpi/generic/spmd_allgather.F90
!||    spmd_allgatherv_mod             ../engine/source/mpi/spmd_allgatherv.F90
!||    spmd_allreduce_mod              ../engine/source/mpi/spmd_allreduce.F90
!||    spmd_alltoall_mod               ../engine/source/mpi/generic/spmd_alltoall.F90
!||    spmd_alltoallv_mod              ../engine/source/mpi/generic/spmd_alltoallv.F90
!||    spmd_barrier_mod                ../engine/source/mpi/generic/spmd_barrier.F90
!||    spmd_bcast_mod                  ../engine/source/mpi/generic/spmd_bcast.F90
!||    spmd_cddl                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_check_ale_neighbour        ../engine/source/mpi/fluid/spmd_check_ale_neighbour.F
!||    spmd_collect                    ../engine/source/mpi/output/spmd_collect.F
!||    spmd_collect_multi_fvm          ../engine/source/mpi/output/spmd_collect_multi_fvm.F
!||    spmd_collect_nlocal             ../engine/source/mpi/output/spmd_collect_nlocal.F
!||    spmd_collect_seatbelt           ../engine/source/mpi/output/spmd_collect_seatbelt.F
!||    spmd_collectm                   ../engine/source/mpi/output/spmd_collectm.F
!||    spmd_collectt                   ../engine/source/mpi/output/spmd_collectt.F
!||    spmd_comm_split                 ../engine/source/mpi/generic/spmd_comm_split.F
!||    spmd_crk_idmax                  ../engine/source/mpi/anim/spmd_crk_idmax.F
!||    spmd_dgather                    ../engine/source/mpi/output/spmd_gather.F
!||    spmd_ds_iexch                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_ds_irecv                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_ds_isend                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_ds_mexch                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_ds_rrecv                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_ds_rsend                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_ds_vdesc                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_ds_vexch                   ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_dstat_gath                 ../engine/source/mpi/output/spmd_stat.F
!||    spmd_dstat_vgath                ../engine/source/mpi/output/spmd_stat.F
!||    spmd_e_ref                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ex_cputime                 ../engine/source/mpi/output/spmd_ex_cputime.F
!||    spmd_exch_a_rb6                 ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
!||    spmd_exch_a_rb6_vrel            ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
!||    spmd_exch_a_rb6g                ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6g.F
!||    spmd_exch_a_rm6                 ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rm6.F
!||    spmd_exch_a_seatbelt            ../engine/source/mpi/seatbelts/spmd_exch_a_seatbelt.F
!||    spmd_exch_awork                 ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_exch_crkavx                ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_crkvel                ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_cut                   ../engine/source/mpi/sections/spmd_section.F
!||    spmd_exch_failwave              ../engine/source/mpi/output/spmd_exch_failwave.F
!||    spmd_exch_fr6                   ../engine/source/mpi/kinematic_conditions/spmd_exch_fr6.F
!||    spmd_exch_iedge                 ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_mult                  ../engine/source/mpi/lag_multipliers/spmd_lag.F
!||    spmd_exch_nodenr                ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_r2r                   ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_r2r_2                 ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_r2r_itag              ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_r2r_nl                ../engine/source/mpi/r2r/spmd_exch_r2r_nl.F
!||    spmd_exch_r2r_rby               ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_r2r_sph               ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_r2r_sphoff            ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exch_rbe2_pon              ../engine/source/mpi/kinematic_conditions/spmd_exch_rbe2_pon.F
!||    spmd_exch_rbe2_sms              ../engine/source/mpi/kinematic_conditions/spmd_exch_rbe2_sms.F
!||    spmd_exch_rbe3                  ../engine/source/mpi/kinematic_conditions/spmd_exch_rbe3.F
!||    spmd_exch_rbe3_a_pon            ../engine/source/mpi/kinematic_conditions/spmd_exch_rbe3_a_pon.F
!||    spmd_exch_rbe3_nodnx            ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_exch_rbe3_pon              ../engine/source/mpi/kinematic_conditions/spmd_exch_rbe3_pon.F
!||    spmd_exch_redge                 ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_sec                   ../engine/source/mpi/sections/spmd_section.F
!||    spmd_exch_smst2                 ../engine/source/mpi/ams/spmd_exch_smst2.F
!||    spmd_exch_sub_poff              ../engine/source/mpi/spmd_exch_sub.F
!||    spmd_exch_sub_pon               ../engine/source/mpi/spmd_exch_sub.F
!||    spmd_exch_tagxp                 ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_exch_userwi                ../engine/source/mpi/user_interface/spmd_exch_userwi.F
!||    spmd_exch_work                  ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_exchange_grad              ../engine/source/mpi/fluid/spmd_exchange_grad.F
!||    spmd_exchm_sms                  ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_exchseg_idel               ../engine/source/mpi/kinematic_conditions/spmd_exchseg_idel.F
!||    spmd_exci                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_exck                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_exsum_fb6                  ../engine/source/mpi/generic/spmd_exsum_fb6.F
!||    spmd_failwave_boundaries        ../engine/source/mpi/output/spmd_exch_failwave.F
!||    spmd_flush_accel                ../engine/source/mpi/output/spmd_flush_accel.F
!||    spmd_fr_poff                    ../engine/source/mpi/kinematic_conditions/spmd_fr_poff.F
!||    spmd_frwall_nn                  ../engine/source/mpi/kinematic_conditions/spmd_frwall_nn.F
!||    spmd_fvb_adim                   ../engine/source/mpi/anim/spmd_fvb_adim.F
!||    spmd_fvb_amax                   ../engine/source/mpi/anim/spmd_fvb_amax.F
!||    spmd_fxb_cin                    ../engine/source/mpi/kinematic_conditions/spmd_fxb_cin.F
!||    spmd_fxb_for                    ../engine/source/mpi/kinematic_conditions/spmd_fxb_for.F
!||    spmd_fxb_for_pon                ../engine/source/mpi/kinematic_conditions/spmd_fxb_for_pon.F
!||    spmd_gath_collective            ../engine/source/mpi/generic/spmd_gath_collective.F
!||    spmd_gather_dtnoda              ../engine/source/mpi/generic/spmd_gather_dtnoda.F
!||    spmd_gather_mod                 ../engine/source/mpi/generic/spmd_gather.F90
!||    spmd_gatherv                    ../engine/source/mpi/generic/spmd_gatherv.F
!||    spmd_gatherv_mod                ../engine/source/mpi/generic/spmd_gatherv.F90
!||    spmd_get_mult                   ../engine/source/mpi/lag_multipliers/spmd_lag.F
!||    spmd_gg_mult                    ../engine/source/mpi/lag_multipliers/spmd_lag.F
!||    spmd_glob_lmax                  ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_glob_lmin                  ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_glob_min5                  ../engine/source/mpi/generic/spmd_glob_min5.F
!||    spmd_glob_minv                  ../engine/source/mpi/generic/spmd_glob_minv.F
!||    spmd_h3d_gather_i               ../engine/source/output/h3d/spmd/spmd_h3d_gather_i.F
!||    spmd_h3d_gather_i_node          ../engine/source/output/h3d/spmd/spmd_h3d_gather_i_node.F
!||    spmd_h3d_gather_i_node_part     ../engine/source/output/h3d/spmd/spmd_h3d_gather_i_node_part.F
!||    spmd_h3d_gather_r               ../engine/source/output/h3d/spmd/spmd_h3d_gather_r.F
!||    spmd_h3d_gather_r_nodal_value   ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
!||    spmd_h3d_gather_r_node          ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_node.F
!||    spmd_h3d_gather_t_node          ../engine/source/output/h3d/spmd/spmd_h3d_gather_t_node.F
!||    spmd_h3d_getmsr                 ../engine/source/output/h3d/spmd/spmd_h3d_getmsr.F
!||    spmd_h3d_getmsr_update          ../engine/source/output/h3d/spmd/spmd_h3d_getmsr_update.F
!||    spmd_h3d_max_r_nodal_value      ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
!||    spmd_h3d_max_r_nodal_value_21   ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
!||    spmd_h3d_sum_r_nodal            ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_node.F
!||    spmd_h3d_sum_r_nodal_21         ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_node.F
!||    spmd_h3d_sum_r_nodal_value      ../engine/source/output/h3d/spmd/spmd_h3d_sum_r_nodal_value.F
!||    spmd_i25front_init              ../engine/source/mpi/interfaces/spmd_i25front.F
!||    spmd_i25front_nor               ../engine/source/mpi/interfaces/spmd_i25front.F
!||    spmd_i2d                        ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_iallgather_mod             ../engine/source/mpi/generic/spmd_iallgather.F90
!||    spmd_iallgatherv_mod            ../engine/source/mpi/generic/spmd_iallgatherv.F90
!||    spmd_iallreduce_mod             ../engine/source/mpi/spmd_iallreduce.F90
!||    spmd_ialltoall_mod              ../engine/source/mpi/generic/spmd_ialltoall.F90
!||    spmd_ialltoallv_mod             ../engine/source/mpi/generic/spmd_ialltoallv.F90
!||    spmd_ibcast                     ../engine/source/mpi/generic/spmd_ibcast.F
!||    spmd_ibcast_mod                 ../engine/source/mpi/generic/spmd_ibcast.F90
!||    spmd_icol                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifc1                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifcd                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifcf                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifrf                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifrf_gpu                   ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifri                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifru                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ifru_gpu                   ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_igather_mod                ../engine/source/mpi/generic/spmd_igather.F90
!||    spmd_iget_partn_sta             ../engine/source/mpi/output/spmd_stat.F
!||    spmd_inf_g                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_inis                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_inisl                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_int_allreduce_max          ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_iprobe_mod                 ../engine/source/mpi/generic/spmd_iprobe.F90
!||    spmd_irecv_double               ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles              ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_doubles2d            ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_int                  ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_ints2d               ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_real                 ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals                ../engine/source/mpi/spmd_irecv.F90
!||    spmd_irecv_reals2d              ../engine/source/mpi/spmd_irecv.F90
!||    spmd_ireduce_mod                ../engine/source/mpi/generic/spmd_ireduce.F90
!||    spmd_iscatter_mod               ../engine/source/mpi/generic/spmd_iscatter.F90
!||    spmd_isend_double               ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles              ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_doubles2d            ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_int                  ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_ints2d               ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_real                 ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals                ../engine/source/mpi/spmd_isend.F90
!||    spmd_isend_reals2d              ../engine/source/mpi/spmd_isend.F90
!||    spmd_isr                        ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_istat_gath                 ../engine/source/mpi/output/spmd_stat.F
!||    spmd_iwlg                       ../engine/source/mpi/implicit/spmd_dsreso.F
!||    spmd_kill                       ../engine/source/mpi/init/spmd_kill.F
!||    spmd_list_sms                   ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_max_f                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_max_i                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_max_ii                     ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_max_iv                     ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_max_s                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_max_xfe_i                  ../engine/source/mpi/elements/spmd_xfem.F
!||    spmd_mij_sms                    ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_min_i                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_min_max                    ../engine/source/mpi/anim/spmd_min_max.F
!||    spmd_min_s                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mod                        ../engine/source/mpi/spmd_mod.F90
!||    spmd_mstop                      ../engine/source/mpi/init/spmd_mstop.F
!||    spmd_mumps_count                ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mumps_deal                 ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mumps_exec                 ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mumps_flush                ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mumps_front                ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mumps_gath                 ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mumps_ini                  ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_mumps_rhs                  ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_n_ref                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_nddlig                     ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_ndof                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_nlist_sms                  ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_nndft_sms                  ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_nnz_sms                    ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_nrow                       ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_out                        ../engine/source/mpi/spmd_error.F90
!||    spmd_pack_doubles               ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_ints                  ../engine/source/mpi/spmd_pack.F90
!||    spmd_pack_reals                 ../engine/source/mpi/spmd_pack.F90
!||    spmd_printcpuinfo               ../engine/source/mpi/output/spmd_printcpuinfo.F
!||    spmd_probe_mod                  ../engine/source/mpi/generic/spmd_probe.F90
!||    spmd_r2r_idef                   ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_iget                   ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_iget2                  ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_iget4                  ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rby                    ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rget                   ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rget3                  ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rget3_dp               ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rset                   ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rset3                  ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rset3b                 ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_rset4                  ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_sync                   ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_r2r_tagel                  ../engine/source/mpi/r2r/spmd_r2r.F
!||    spmd_rbcast                     ../engine/source/mpi/generic/spmd_rbcast.F
!||    spmd_recv_double                ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles               ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_doubles2d             ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_int                   ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_ints2d                ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_real                  ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals                 ../engine/source/mpi/spmd_recv.F90
!||    spmd_recv_reals2d               ../engine/source/mpi/spmd_recv.F90
!||    spmd_reduce_db                  ../engine/source/mpi/generic/spmd_reduce_db.F
!||    spmd_reduce_mod                 ../engine/source/mpi/generic/spmd_reduce.F90
!||    spmd_rst_check                  ../engine/source/mpi/init/spmd_rst_check.F
!||    spmd_scatter_mod                ../engine/source/mpi/generic/spmd_scatter.F90
!||    spmd_scatterv_mod               ../engine/source/mpi/generic/spmd_scatterv.F90
!||    spmd_sd_acc                     ../engine/source/mpi/output/spmd_sd_acc.F
!||    spmd_sd_cj_0                    ../engine/source/mpi/kinematic_conditions/spmd_sd_cj_0.F
!||    spmd_sd_cj_1                    ../engine/source/mpi/kinematic_conditions/spmd_sd_cj_1.F
!||    spmd_sd_cj_2                    ../engine/source/mpi/kinematic_conditions/spmd_sd_cj_2.F
!||    spmd_sd_cut                     ../engine/source/mpi/sections/spmd_section.F
!||    spmd_sd_gau                     ../engine/source/mpi/output/spmd_sd_gau.F
!||    spmd_sd_gaug                    ../engine/source/mpi/output/spmd_sd_gaug.F
!||    spmd_sd_sens                    ../engine/source/mpi/output/spmd_sd_sens.F
!||    spmd_sd_skw                     ../engine/source/mpi/output/spmd_sd_skw.F
!||    spmd_sd_skw_anim                ../engine/source/mpi/output/spmd_sd_skw.F
!||    spmd_send_double                ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles               ../engine/source/mpi/spmd_send.F90
!||    spmd_send_doubles2d             ../engine/source/mpi/spmd_send.F90
!||    spmd_send_int                   ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_ints2d                ../engine/source/mpi/spmd_send.F90
!||    spmd_send_real                  ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals                 ../engine/source/mpi/spmd_send.F90
!||    spmd_send_reals2d               ../engine/source/mpi/spmd_send.F90
!||    spmd_send_vi                    ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_send_vr                    ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_sendrecv_mod               ../engine/source/mpi/generic/spmd_sendrecv.F90
!||    spmd_sendrecv_replace_mod       ../engine/source/mpi/generic/spmd_sendrecv_replace.F90
!||    spmd_sg_fani                    ../engine/source/mpi/lag_multipliers/spmd_lag.F
!||    spmd_sg_mult                    ../engine/source/mpi/lag_multipliers/spmd_lag.F
!||    spmd_sort_sms                   ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_spamaj                     ../engine/source/mpi/sph/spmd_spamaj.F
!||    spmd_sphgat                     ../engine/source/mpi/sph/spmd_sphgat.F
!||    spmd_sphgetv                    ../engine/source/mpi/sph/spmd_sphgetv.F
!||    spmd_split_comm                 ../engine/source/mpi/init/spmd_split_comm.F
!||    spmd_split_comm_inter           ../engine/source/mpi/interfaces/spmd_split_comm_inter.F
!||    spmd_split_comm_joint           ../engine/source/mpi/init/spmd_split_comm_joint.F
!||    spmd_stat_pgather               ../engine/source/mpi/output/spmd_stat.F
!||    spmd_state_inimap1d_exch_data   ../engine/source/output/sta/spmd_state_inimap1d_exch_data.F
!||    spmd_state_inimap2d_exch_data   ../engine/source/output/sta/spmd_state_inimap2d_exch_data.F
!||    spmd_state_inimap_exch_siz      ../engine/source/output/sta/spmd_state_inimap_exch_siz.F
!||    spmd_sub_boundaries             ../engine/source/mpi/spmd_exch_sub.F
!||    spmd_sum_s                      ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_sum_s2                     ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_sumf_a                     ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_sumf_k                     ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_sumf_v                     ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_sumfc_v                    ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_tri17box                   ../engine/source/mpi/interfaces/spmd_tri17box.F
!||    spmd_tri20box                   ../engine/source/mpi/interfaces/spmd_tri20box.F
!||    spmd_tri20boxe                  ../engine/source/mpi/interfaces/spmd_tri20boxe.F
!||    spmd_unpack_doubles             ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_ints                ../engine/source/mpi/spmd_unpack.F90
!||    spmd_unpack_reals               ../engine/source/mpi/spmd_unpack.F90
!||    spmd_vchgrid                    ../engine/source/mpi/implicit/imp_spmd.F
!||    spmd_wiout                      ../engine/source/mpi/generic/spmd_wiout.F
!||    spmd_wrt_cutd                   ../engine/source/mpi/sections/spmd_section.F
!||    spmd_wrt_cutf                   ../engine/source/mpi/sections/spmd_section.F
!||    spmd_x_section                  ../engine/source/mpi/sections/spmd_section.F
!||    switch_to_dtnoda                ../engine/source/time_step/switch_to_dtnoda.F
!||    terminate                       ../engine/share/modules/linear_solver_mod.F
!||    terminate_cg                    ../engine/share/modules/linear_solver_mod.F
!||    terminate_diffusion             ../engine/share/modules/diffusion_mod.F
!||    terminate_mumps                 ../engine/share/modules/linear_solver_mod.F
!||    upcase                          ../engine/source/engine/execargcheck.F
!||====================================================================
      module spmd_comm_world_mod
        implicit none
        integer :: spmd_comm_world
#ifndef MPI
        integer, parameter, public :: MPI_STATUS_IGNORE = 0
        integer, parameter, public :: MPI_STATUS_SIZE = 1
        integer, parameter, public :: MPI_REQUEST_NULL = 0
        integer, parameter, public :: MPI_COMM_WORLD = 0
        integer, parameter, public :: SPMD_STATUS_IGNORE = 0
        integer, parameter, public :: SPMD_STATUS_SIZE = 1
        integer, parameter, public :: SPMD_REQUEST_NULL = 0
#else
#include "mpif.h"
        integer, parameter, public :: SPMD_REQUEST_NULL = MPI_REQUEST_NULL
#endif
      end module spmd_comm_world_mod
