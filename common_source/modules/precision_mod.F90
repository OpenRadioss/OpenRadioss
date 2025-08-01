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
!||    precision_mod                            ../common_source/modules/precision_mod.F90
!||--- called by ------------------------------------------------------
!||    alevec                                   ../engine/source/output/anim/generate/monvol_anim.F90
!||    alevflu                                  ../engine/source/output/anim/generate/monvol_anim.F90
!||    alew8                                    ../engine/source/ale/grid/alew8.F90
!||    ams_work_mod                             ../engine/source/modules/ams_work_mod.F90
!||    anim_nodal_ssp_elems                     ../engine/source/output/anim/generate/anim_nodal_ssp_elems.F90
!||    animbale                                 ../engine/source/output/anim/generate/monvol_anim.F90
!||    animcale                                 ../engine/source/output/anim/generate/monvol_anim.F90
!||    anivflow                                 ../engine/source/output/anim/generate/monvol_anim.F90
!||    anivflowp                                ../engine/source/output/anim/generate/monvol_anim.F90
!||    arruda_boyce                             ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    arruda_boyce_dyda                        ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    bcs_mod                                  ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    bcs_wall_trigger                         ../engine/source/boundary_conditions/bcs_wall_trigger.F90
!||    brent_algo                               ../common_source/modules/root_finding_algo_mod.F90
!||    brokmann_crack_init                      ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
!||    brokmann_elem_renum                      ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
!||    brokmann_random                          ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
!||    brokmann_random_def_mod                  ../common_source/modules/brokmann_random_def_mod.F90
!||    calculp2                                 ../starter/source/materials/mat/mat057/calculp2.F90
!||    check_sorting_criteria                   ../engine/source/interfaces/intsort/check_sorting_criteria.F90
!||    chk_shell_offset                         ../starter/source/elements/shell/shell_offset/chk_shell_offset.F90
!||    compaction                               ../common_source/eos/compaction.F90
!||    compaction2                              ../common_source/eos/compaction2.F90
!||    compaction_tab                           ../common_source/eos/compaction_tab.F90
!||    compaction_tab_init                      ../common_source/eos/compaction_tab.F90
!||    compute_voxel_dimensions                 ../engine/source/interfaces/intsort/voxel_dimensions.F90
!||    connectivity_mod                         ../common_source/modules/connectivity.F90
!||    coupling_advance                         ../engine/source/coupling/coupling_adapter.F90
!||    coupling_initialize                      ../engine/source/coupling/coupling_adapter.F90
!||    coupling_read                            ../engine/source/coupling/coupling_adapter.F90
!||    coupling_sync                            ../engine/source/coupling/coupling_adapter.F90
!||    coupling_write                           ../engine/source/coupling/coupling_adapter.F90
!||    crack_depth_init                         ../starter/source/materials/fail/windshield_alter/crack_depth_init.F90
!||    create_plane_clause                      ../starter/source/model/sets/create_plane_clause.F90
!||    crityld2000                              ../starter/source/materials/mat/mat087/law87_upd.F90
!||    cross_product                            ../engine/source/constraints/general/rbody/velrot_explicit.F90
!||    damping_funct_ini                        ../engine/source/assembly/damping_funct_ini.F90
!||    damping_range_compute_param              ../starter/source/general_controls/damping/damping_range_compute_param.F90
!||    damping_range_init                       ../starter/source/general_controls/damping/damping_range_init.F90
!||    damping_range_shell                      ../engine/source/general_controls/damping/damping_range_shell.F90
!||    damping_range_shell_mom                  ../engine/source/general_controls/damping/damping_range_shell_mom.F90
!||    damping_range_solid                      ../engine/source/general_controls/damping/damping_range_solid.F90
!||    damping_rby_spmdset                      ../starter/source/general_controls/damping/damping_rby_spmdset.F90
!||    damping_vref_compute_dampa               ../engine/source/assembly/damping_vref_compute_dampa.F90
!||    damping_vref_rby                         ../engine/source/assembly/damping_vref_rby.F90
!||    damping_vref_rby_stiff                   ../engine/source/assembly/damping_vref_rby_stiff.F90
!||    damping_vref_sum6_rby                    ../engine/source/assembly/damping_vref_sum6_rby.F90
!||    defbeam_sect_new                         ../starter/source/properties/beam/defbeam_sect_new.F90
!||    detonation_times_printout                ../starter/source/initial_conditions/detonation/detonation_times_printout.F90
!||    dim_shell_offsetp                        ../starter/source/elements/shell/shell_offset/dim_shell_offsetp.F90
!||    double_to_my_real                        ../common_source/modules/cast_mod.F90
!||    dttherm                                  ../engine/source/time_step/dttherm.F90
!||    ebcs11                                   ../engine/source/boundary_conditions/ebcs/ebcs11.F90
!||    ebcs_mod                                 ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    eikonal_compute_adjacent                 ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
!||    eikonal_fast_marching_method             ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||    eikonal_godunov_operator_2d              ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_2d.F90
!||    eikonal_godunov_operator_3d              ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_3d.F90
!||    eikonal_init_mixture_vel                 ../starter/source/initial_conditions/detonation/eikonal_ini_mixture_vel.F90
!||    eikonal_init_start_list_2d               ../starter/source/initial_conditions/detonation/eikonal_init_start_list_2d.F90
!||    eikonal_remove_first                     ../starter/source/initial_conditions/detonation/eikonal_remove_first.F90
!||    eikonal_solver                           ../starter/source/initial_conditions/detonation/eikonal_solver.F90
!||    eikonal_sort_narrow_band                 ../starter/source/initial_conditions/detonation/eikonal_sort_narrow_band.F90
!||    elbufdef_mod                             ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    eos_param_mod                            ../common_source/modules/mat_elem/eos_param_mod.F90
!||    eos_table_copy                           ../starter/source/materials/tools/eos_table_copy.F90
!||    eosexponential                           ../common_source/eos/eosexponential.F90
!||    fail_gene1_b                             ../engine/source/materials/fail/gene1/fail_gene1_b.F90
!||    fail_gene1_ib                            ../engine/source/materials/fail/gene1/fail_gene1_ib.F90
!||    fail_inievo_b                            ../engine/source/materials/fail/inievo/fail_inievo_b.F90
!||    fail_inievo_ib                           ../engine/source/materials/fail/inievo/fail_inievo_ib.F90
!||    fail_lemaitre_c                          ../engine/source/materials/fail/lemaitre/fail_lemaitre_c.F90
!||    fail_lemaitre_s                          ../engine/source/materials/fail/lemaitre/fail_lemaitre_s.F90
!||    fail_param_mod                           ../common_source/modules/mat_elem/fail_param_mod.F90
!||    fail_spalling_s                          ../engine/source/materials/fail/spalling/fail_spalling_s.F90
!||    fail_tab2_b                              ../engine/source/materials/fail/tabulated/fail_tab2_b.F90
!||    fail_tab2_ib                             ../engine/source/materials/fail/tabulated/fail_tab2_ib.F90
!||    fail_visual_b                            ../engine/source/materials/fail/visual/fail_visual_b.F90
!||    fail_visual_ib                           ../engine/source/materials/fail/visual/fail_visual_ib.F90
!||    fill_surf_plane                          ../starter/source/model/sets/fill_gr_surf_plane.F90
!||    fill_voxel_local                         ../engine/source/interfaces/intsort/fill_voxel.F90
!||    fill_voxel_local_partial                 ../engine/source/interfaces/intsort/fill_voxel.F90
!||    fill_voxel_remote                        ../engine/source/interfaces/intsort/fill_voxel.F90
!||    finter_mixed_mod                         ../engine/source/tools/finter_mixed.F90
!||    force                                    ../engine/source/loads/general/force.F90
!||    fpcont2_max_output                       ../engine/source/output/h3d/h3d_results/fpcont2_max_output.F90
!||    fpcont2_min_output                       ../engine/source/output/h3d/h3d_results/fpcont2_min_output.F90
!||    fractal_dmg_init                         ../starter/source/materials/fail/fractal/fractal_dmg_init.F90
!||    fractal_elem_renum                       ../starter/source/materials/fail/fractal/fractal_elem_spmd_renum.F90
!||    func_table_copy                          ../starter/source/materials/tools/func_table_copy.F90
!||    funct_python_update_elements             ../engine/source/tools/curve/funct_python_update_elements.F90
!||    get_convexity_normals                    ../engine/source/interfaces/interf/get_convexity_normals.F90
!||    get_list_remnode                         ../starter/source/interfaces/inter3d1/get_list_remnode.F90
!||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||    get_preload_axial                        ../engine/source/elements/spring/preload_axial.F90
!||    get_segment_criteria                     ../engine/source/interfaces/interf/get_segment_criteria.F90
!||    get_segment_normal                       ../engine/source/interfaces/interf/get_segment_normal.F90
!||    get_segment_orientation                  ../engine/source/interfaces/interf/get_segment_orientation.F90
!||    get_volume_area                          ../engine/source/airbag/get_volume_area.F90
!||    ghost_shells_mod                         ../engine/source/engine/node_spliting/ghost_shells.F90
!||    glob_therm_mod                           ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    granular51                               ../engine/source/materials/mat/mat051/granular51.F90
!||    group_param_mod                          ../common_source/modules/mat_elem/group_param_mod.F90
!||    h3d_oned_scalar                          ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F90
!||    h3d_quad_scalar_1                        ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
!||    hm_pre_read_preload_axial                ../starter/source/loads/general/preload/hm_read_preload_axial.F90
!||    hm_preread_inivel                        ../starter/source/initial_conditions/general/inivel/hm_preread_inivel.F90
!||    hm_read_bcs_wall                         ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
!||    hm_read_ebcs_propergol                   ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_propergol.F90
!||    hm_read_eos_compaction                   ../starter/source/materials/eos/hm_read_eos_compaction.F90
!||    hm_read_eos_compaction2                  ../starter/source/materials/eos/hm_read_eos_compaction2.F90
!||    hm_read_eos_compaction_tab               ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
!||    hm_read_eos_exponential                  ../starter/source/materials/eos/hm_read_eos_exponential.F90
!||    hm_read_eos_powderburn                   ../starter/source/materials/eos/hm_read_eos_powderburn.F90
!||    hm_read_fail_lemaitre                    ../starter/source/materials/fail/lemaitre/hm_read_fail_lemaitre.F90
!||    hm_read_fail_spalling                    ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F90
!||    hm_read_fractal_dmg                      ../starter/source/materials/fail/fractal/hm_read_fractal_dmg.F90
!||    hm_read_funct_python                     ../starter/source/tools/curve/hm_read_funct_python.F90
!||    hm_read_inivol                           ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
!||    hm_read_mat                              ../starter/source/materials/mat/hm_read_mat.F90
!||    hm_read_mat105                           ../starter/source/materials/mat/mat105/hm_read_mat105.F90
!||    hm_read_mat125                           ../starter/source/materials/mat/mat125/hm_read_mat125.F90
!||    hm_read_mat126                           ../starter/source/materials/mat/mat126/hm_read_mat126.F90
!||    hm_read_mat127                           ../starter/source/materials/mat/mat127/hm_read_mat127.F90
!||    hm_read_mat128                           ../starter/source/materials/mat/mat128/hm_read_mat128.F90
!||    hm_read_mat133                           ../starter/source/materials/mat/mat133/hm_read_mat133.F90
!||    hm_read_mat134                           ../starter/source/materials/mat/mat134/hm_read_mat134.F90
!||    hm_read_mat163                           ../starter/source/materials/mat/mat163/hm_read_mat163.F90
!||    hm_read_mat169_arup                      ../starter/source/materials/mat/mat169/hm_read_mat169.F90
!||    hm_read_mat50                            ../starter/source/materials/mat/mat050/hm_read_mat50.F90
!||    hm_read_mat57                            ../starter/source/materials/mat/mat057/hm_read_mat57.F90
!||    hm_read_mat81                            ../starter/source/materials/mat/mat081/hm_read_mat81.F90
!||    hm_read_mat87                            ../starter/source/materials/mat/mat087/hm_read_mat87.F90
!||    hm_read_preload_axial                    ../starter/source/loads/general/preload/hm_read_preload_axial.F90
!||    hm_read_therm_stress                     ../starter/source/materials/therm/hm_read_therm_stress.F90
!||    hm_read_visc_plas                        ../starter/source/materials/visc/hm_read_visc_plas.F90
!||    i2_surfi                                 ../starter/source/interfaces/inter3d1/i2_surfi.F90
!||    i2_surfi_dim                             ../starter/source/interfaces/inter3d1/i2_surfi_dim.F90
!||    imp_intbufdef                            ../engine/share/modules/imp_mod_def.F90
!||    iniebcs_propergol                        ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
!||    iniebcs_propergol_get_cv                 ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
!||    init_global_frontier_monvol              ../engine/source/airbag/init_global_monvol_frontier.F90
!||    init_inivol                              ../starter/source/initial_conditions/inivol/init_inivol.F90
!||    init_inivol_2d_polygons                  ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
!||    initemp_shell                            ../starter/source/materials/therm/initemp_shell.F90
!||    inivel_dt2                               ../engine/source/loads/general/inivel/inivel_dt2.F90
!||    inivel_init                              ../engine/source/loads/general/inivel/inivel_init.F90
!||    inivel_mod                               ../common_source/modules/inivel_mod.F90
!||    inivel_start                             ../engine/source/loads/general/inivel/inivel_start.F90
!||    intbuf_fric_mod                          ../common_source/modules/interfaces/intbuf_fric_mod.F90
!||    intbufdef_mod                            ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter7_candidate_pairs                   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
!||    inter7_collision_detection               ../engine/source/interfaces/intsort/inter7_collision_detection.F90
!||    inter7_deserialize                       ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
!||    inter7_filter_cand                       ../engine/source/interfaces/intsort/inter7_filter_cand.F90
!||    inter7_gather_cand                       ../engine/source/interfaces/int07/inter7_gather_cand.F90
!||    inter7_penetration                       ../engine/source/interfaces/intsort/inter7_penetration.F90
!||    inter7_serialize                         ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
!||    inter_save_candidate                     ../starter/source/interfaces/inter3d1/inter_save_candidate.F90
!||    inter_sh_offset_ini                      ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
!||    inter_sh_offset_mod                      ../engine/source/modules/interfaces/sh_offset_mod.F90
!||    jcook51                                  ../engine/source/materials/mat/mat051/jcook51.F90
!||    jwl_eos_delta                            ../engine/source/materials/mat/mat041/jwl_eos_mod.F90
!||    jwl_eos_state                            ../engine/source/materials/mat/mat041/jwl_eos_mod.F90
!||    law133_upd                               ../starter/source/materials/mat/mat133/law133_upd.F90
!||    law163_upd                               ../starter/source/materials/mat/mat163/law163_upd.F90
!||    law190_upd                               ../starter/source/materials/mat/mat190/law190_upd.F90
!||    law81_upd                                ../starter/source/materials/mat/mat081/law81_upd.F90
!||    law87_upd                                ../starter/source/materials/mat/mat087/law87_upd.F90
!||    law92_guess                              ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    law92_nlsqf                              ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    mat25_crasurv_c                          ../engine/source/materials/mat/mat025/mat25_crasurv_c.F90
!||    mat25_crasurv_s                          ../engine/source/materials/mat/mat025/mat25_crasurv_s.F90
!||    mat25_tsaiwu_c                           ../engine/source/materials/mat/mat025/mat25_tsaiwu_c.F90
!||    mat25_tsaiwu_s                           ../engine/source/materials/mat/mat025/mat25_tsaiwu_s.F90
!||    mat87c_hansel                            ../engine/source/materials/mat/mat087/mat87c_hansel.F90
!||    mat87c_swift_voce                        ../engine/source/materials/mat/mat087/mat87c_swift_voce.F90
!||    mat87c_tabulated                         ../engine/source/materials/mat/mat087/mat87c_tabulated.F90
!||    mat87c_tabulated_3dir_ortho              ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
!||    mat_table_copy                           ../starter/source/materials/tools/mat_table_copy.F90
!||    matparam_def_mod                         ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mmain                                    ../engine/source/materials/mat_share/mmain.F90
!||    monvol_struct_mod                        ../engine/share/modules/monvol_struct_mod.F
!||    mrqcof_law92                             ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    mrqmin_law92                             ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    mulaw                                    ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8                                   ../engine/source/materials/mat_share/mulaw8.F90
!||    multi_fvm_mod                            ../common_source/modules/ale/multi_fvm_mod.F90
!||    multi_muscl_compute_pressure             ../engine/source/multifluid/multi_muscl_compute_pressure.F90
!||    multi_propergol_ebcs                     ../engine/source/multifluid/multi_propergol_ebcs.F90
!||    multi_solve_eint                         ../engine/source/multifluid/multi_solve_eint.F90
!||    multimat_param_mod                       ../common_source/modules/multimat_param_mod.F90
!||    newman_raju                              ../common_source/fail/newman_raju.F90
!||    nodal_arrays_mod                         ../common_source/modules/nodal_arrays.F90
!||    offset_nproj                             ../engine/source/interfaces/shell_offset/offset_nproj.F90
!||    output_mod                               ../common_source/modules/output/output_mod.F90
!||    pblast_deallocate                        ../common_source/modules/loads/pblast_mod.F90
!||    pblast_load                              ../common_source/modules/loads/pblast_mod.F90
!||    pblast_mod                               ../common_source/modules/loads/pblast_mod.F90
!||    pblast_parameters__air_burst             ../common_source/modules/loads/pblast_mod.F90
!||    pblast_parameters__free_air              ../common_source/modules/loads/pblast_mod.F90
!||    pblast_parameters__surface_burst         ../common_source/modules/loads/pblast_mod.F90
!||    pblast_write_engine                      ../common_source/modules/loads/pblast_mod.F90
!||    pblast_write_starter                     ../common_source/modules/loads/pblast_mod.F90
!||    ply_param_mod                            ../common_source/modules/mat_elem/ply_param_mod.F90
!||    polygon_clipping_mod                     ../common_source/tools/clipping/polygon_clipping_mod.F90
!||    polygon_mod                              ../common_source/tools/clipping/polygon_mod.F90
!||    preload_axial                            ../engine/source/elements/spring/preload_axial.F90
!||    prodmat                                  ../engine/source/materials/tools/prodmat.F
!||    prodmatvect                              ../starter/source/materials/mat/mat087/law87_upd.F90
!||    prop_param_mod                           ../common_source/modules/mat_elem/prop_param_mod.F90
!||    python_funct_mod                         ../common_source/modules/python_mod.F90
!||    python_monvol                            ../engine/source/coupling/python/python_monvol.F90
!||    python_monvol_mod                        ../engine/source/coupling/python/python_monvol.F90
!||    r_yld2000                                ../starter/source/materials/mat/mat087/law87_upd.F90
!||    random_walk_def_mod                      ../common_source/modules/random_walk_def_mod.F90
!||    random_walk_dmg                          ../starter/source/materials/fail/fractal/random_walk_dmg.F90
!||    rbe3_mod                                 ../common_source/modules/constraints/rbe3_mod.F90
!||    rbe3f_pen                                ../engine/source/constraints/general/rbe3/rbe3f_pen.F90
!||    rbe3fpen_ininp                           ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
!||    rbe3pen_init                             ../engine/source/constraints/general/rbe3/rbe3pen_init.F90
!||    read_ale_grid                            ../engine/source/output/restart/read_ale_grid.F90
!||    read_bcs_wall                            ../engine/source/output/restart/read_bcs_wall.F90
!||    read_eosparam                            ../engine/source/output/restart/read_eosparam.F90
!||    read_inivel                              ../engine/source/output/restart/read_inivel.F90
!||    read_mat25_crasurv                       ../starter/source/materials/mat/mat025/read_mat25_crasurv.F90
!||    read_mat25_tsaiwu                        ../starter/source/materials/mat/mat025/read_mat25_tsaiwu.F90
!||    read_thermparam                          ../engine/source/output/restart/read_thermparam.F90
!||    real_array_reindex                       ../common_source/tools/sort/array_reindex.F90
!||    real_insertion_sort_with_index           ../common_source/tools/sort/insertion_sort.F90
!||    redef3                                   ../engine/source/elements/spring/redef3.F90
!||    redef_seatbelt                           ../engine/source/tools/seatbelts/redef_seatbelt.F90
!||    retractor_table_inv                      ../engine/source/tools/seatbelts/retractor_table_inv.F90
!||    retractor_table_inv2                     ../engine/source/tools/seatbelts/retractor_table_inv2.F90
!||    s10get_x0                                ../engine/source/elements/solid/solide10/s10get_x0.F90
!||    s20temp                                  ../starter/source/elements/solid/solide20/s20temp.F90
!||    s6chour_ctl                              ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
!||    s6for_distor                             ../engine/source/elements/thickshell/solide6c/s6for_distor.F90
!||    s6get_xv                                 ../engine/source/elements/thickshell/solide6c/s6get_xv.F90
!||    sdistor_ini                              ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    select_s2s                               ../starter/source/interfaces/inter3d1/select_s2s.F90
!||    sensor_mod                               ../common_source/modules/sensor_mod.F90
!||    sfor_3n2s3                               ../engine/source/elements/solid/solide/sfor_4n2s4.F90
!||    sfor_4n2s4                               ../engine/source/elements/solid/solide/sfor_4n2s4.F90
!||    sfor_ns2s4                               ../engine/source/elements/solid/solide/sfor_ns2s4.F90
!||    sfor_visn6                               ../engine/source/elements/thickshell/solide6c/sfor_visn6.F90
!||    sh_offset_jonct_chk                      ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
!||    sh_offset_nproj                          ../starter/source/elements/shell/shell_offset/shell_offset_nproj.F90
!||    sh_offset_setn                           ../starter/source/elements/shell/shell_offset/sh_offset_setn.F90
!||    shell_offset_ini                         ../starter/source/elements/shell/shell_offset/shell_offset_ini.F90
!||    shell_offsetp                            ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||    shour_ctl                                ../engine/source/elements/solid/solidez/shour_ctl.F90
!||    sigeps01                                 ../starter/source/materials/mat/mat001/sigeps01.F90
!||    sigeps100                                ../engine/source/materials/mat/mat100/sigeps100.F90
!||    sigeps125                                ../engine/source/materials/mat/mat125/sigeps125.F90
!||    sigeps125c                               ../engine/source/materials/mat/mat125/sigeps125c.F90
!||    sigeps126                                ../engine/source/materials/mat/mat126/sigeps126.F90
!||    sigeps127                                ../engine/source/materials/mat/mat127/sigeps127.F90
!||    sigeps127c                               ../engine/source/materials/mat/mat127/sigeps127c.F90
!||    sigeps128c                               ../engine/source/materials/mat/mat128/sigeps128c.F90
!||    sigeps128s                               ../engine/source/materials/mat/mat128/sigeps128s.F90
!||    sigeps133                                ../engine/source/materials/mat/mat133/sigeps133.F90
!||    sigeps134s                               ../engine/source/materials/mat/mat134/sigeps134s.F90
!||    sigeps163                                ../engine/source/materials/mat/mat163/sigeps163.F90
!||    sigeps169_connect                        ../engine/source/materials/mat/mat169/sigeps169_connect.F90
!||    sigeps50s                                ../engine/source/materials/mat/mat050/sigeps50s.F90
!||    sigeps51                                 ../engine/source/materials/mat/mat051/sigeps51.F90
!||    sigeps51_boundary_material               ../engine/source/materials/mat/mat051/sigeps51_boundary_material.F90
!||    sigeps57c                                ../engine/source/materials/mat/mat057/sigeps57c.F90
!||    sigeps81                                 ../engine/source/materials/mat/mat081/sigeps81.F90
!||    sigeps87c                                ../engine/source/materials/mat/mat087/sigeps87c.F90
!||    skew_mod                                 ../common_source/modules/skew_mod.F90
!||    sph_work_mod                             ../common_source/modules/mat_elem/sph_work.F90
!||    spmd_exch_flow_tracking_data             ../engine/source/ale/grid/spmd_exch_flow_tracking_data.F90
!||    spmd_exch_flow_tracking_data2            ../engine/source/ale/grid/spmd_exch_flow_tracking_data2.F90
!||    spmd_exch_flow_tracking_data3            ../engine/source/ale/grid/spmd_exch_flow_tracking_data3.F90
!||    spmd_exch_flow_tracking_data4            ../engine/source/ale/grid/spmd_exch_flow_tracking_data4.F90
!||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||    spmd_exchange_ghost_shells               ../engine/source/engine/node_spliting/ghost_shells.F90
!||    spmd_xv_inter_type1                      ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
!||    spring_functions_mod                     ../common_source/modules/spring_functions_mod.F90
!||    stat_sphcel_full                         ../engine/source/output/sta/stat_sphcel_full.F90
!||    stifint_icontrol                         ../starter/source/interfaces/interf1/stifint_icontrol.F90
!||    sz_dt1                                   ../engine/source/elements/solid/solidez/sz_dt1.F90
!||    table_mat_vinterp_c1                     ../engine/source/materials/tools/table_mat_vinterp_c1.F90
!||    test_jc_shell_detach                     ../engine/source/engine/node_spliting/detach_node.F90
!||    therm_param_mod                          ../common_source/modules/mat_elem/therm_param_mod.F90
!||    update_neighbour_segment                 ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||    velrot_explicit                          ../engine/source/constraints/general/rbody/velrot_explicit.F90
!||    vinter_mixed                             ../engine/source/tools/curve/vinter_mixed.F90
!||    visc_param_mod                           ../common_source/modules/mat_elem/visc_param_mod.F90
!||    visc_plas                                ../engine/source/materials/visc/visc_plas.F90
!||    w_inivel_str                             ../starter/source/restart/ddsplit/w_inivel_str.F90
!||    write_ale_grid                           ../common_source/output/restart/write_ale_grid.F90
!||    write_bcs_wall                           ../common_source/output/restart/write_bcs_wall.F90
!||    write_eosparam                           ../engine/source/output/restart/write_eosparam.F90
!||    write_inivel                             ../engine/source/output/restart/write_inivel.F90
!||    write_thermparam                         ../engine/source/output/restart/write_thermparam.F90
!||    xyz16                                    ../engine/source/output/anim/generate/monvol_anim.F90
!||====================================================================
      MODULE precision_mod
      IMPLICIT NONE
#ifdef MYREAL8
        INTEGER, PARAMETER :: WP = 8
#else
        INTEGER, PARAMETER :: WP = 4
#endif
      END MODULE precision_mod

