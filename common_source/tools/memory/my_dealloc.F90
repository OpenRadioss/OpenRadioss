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

! ======================================================================================================================
! fypp template — generates my_dealloc.F90
! Do NOT edit the generated my_dealloc.F90 directly; edit this file and re-run fypp.
!
! Deallocation counterpart to my_alloc.fy.
!
! Each generated subroutine:
!   1. Guards against double-free (checks allocated / associated).
!   2. Resolves the pointer address with c_loc of the first element.
!   3. Calls cpp_record_dealloc_addr(addr) — the C++ side looks up the address in
!      the per-allocation map, subtracts the previously recorded byte count from the
!      per-site counter, and removes the address entry.  One site name can have many
!      live addresses simultaneously (same allocation site called from many callers).
!   4. Deallocates the array.  For pointer arrays, also nullifies after free.
!
! Axes of variation:
!   TYPES     : (fortran_type, short_name)  — same set as my_alloc.fy
!   MEM_KINDS : (fortran_attr, name_prefix) — 'allocatable' / 'pointer'
!   RANKS     : (rank, dim_var_list)        — 1D / 2D / 3D
!
! No IDX_KINDS axis: deallocation takes only the array, no dimension arguments.
!
! Subroutine naming convention:
!   my_dealloc_<mem_prefix><type_name>_<rank>d
!   e.g. my_dealloc_pdouble_3d  =>  pointer, double precision, 3D
!
! Placeholder for derived types is at the bottom of the contains section.
! ======================================================================================================================



!||====================================================================
!||    my_dealloc_mod                           ../common_source/tools/memory/my_dealloc.F90
!||--- called by ------------------------------------------------------
!||    admdiv                                   ../engine/source/model/remesh/admdiv.F
!||    admfor0                                  ../engine/source/model/remesh/admfor0.F
!||    admordr                                  ../engine/source/model/remesh/admordr.F
!||    admregul                                 ../engine/source/model/remesh/admregul.F
!||    afluxt                                   ../engine/source/ale/ale51/afluxt.F
!||    ale51_gradient_reconstruction            ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||    ale51_gradient_reconstruction2           ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
!||    alemain                                  ../engine/source/ale/alemain.F
!||    alemuscl_deallocate                      ../engine/source/ale/alemuscl/alemuscl_deallocate.F
!||    alethe                                   ../engine/source/ale/alethe.F
!||    alevec                                   ../engine/source/output/anim/generate/monvol_anim.F90
!||    alew5                                    ../engine/source/ale/grid/alew5.F
!||    alloc_group_str                          ../engine/source/groups/alloc_group_str.F
!||    alloc_line_str                           ../engine/source/groups/alloc_line_str.F
!||    alloc_subset_str                         ../engine/source/groups/alloc_subset_str.F
!||    alloc_surf_str                           ../engine/source/groups/alloc_surf_str.F
!||    allocbuf_auto                            ../engine/source/elements/elbuf/allocbuf_auto.F
!||    anim_nodal_p_elems                       ../engine/source/output/anim/generate/anim_nodal_p_elems.F
!||    anim_nodal_ssp_elems                     ../engine/source/output/anim/generate/anim_nodal_ssp_elems.F90
!||    anioff0                                  ../engine/source/output/anim/generate/anioff0.F
!||    anioffc                                  ../engine/source/output/anim/generate/anioffc.F
!||    anioffc_crk                              ../engine/source/output/anim/generate/anioffc_crk.F
!||    anioffc_ply                              ../engine/source/output/anim/generate/anioffc_ply.F
!||    aniofff                                  ../engine/source/output/anim/generate/aniofff.F
!||    anioffs                                  ../engine/source/output/anim/generate/anioff6.F
!||    assadd2                                  ../engine/source/assembly/assadd2.F
!||    asspar4                                  ../engine/source/assembly/asspar4.F
!||    asspar_crk                               ../engine/source/elements/xfem/asspar_crk.F
!||    biquad_upd                               ../starter/source/materials/fail/biquad/biquad_upd.F90
!||    brokmann_elem_renum                      ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
!||    brokmann_random                          ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
!||    build_admesh                             ../starter/source/model/remesh/build_admesh.F
!||    c3fint_reg                               ../engine/source/elements/sh3n/coque3n/c3fint_reg.F
!||    c3fint_reg_ini                           ../starter/source/elements/sh3n/coque3n/c3fint_reg_ini.F
!||    c3forc3                                  ../engine/source/elements/sh3n/coque3n/c3forc3.F
!||    c3forc3_crk                              ../engine/source/elements/xfem/c3forc3_crk.F
!||    c_front                                  ../starter/source/restart/ddsplit/c_front.F
!||    c_irbe3                                  ../starter/source/restart/ddsplit/c_irbe3.F
!||    c_ncrkxfem                               ../starter/source/restart/ddsplit/c_ncrkxfem.F
!||    c_spmd_ne_connect                        ../starter/source/ale/spmd_ne_connect.F
!||    c_tf_ne                                  ../engine/source/output/sty/c_tf_ne.F
!||    c_vois                                   ../starter/source/restart/ddsplit/c_vois.F
!||    cbafint_reg                              ../engine/source/elements/shell/coqueba/cbafint_reg.F
!||    cbafint_reg_ini                          ../starter/source/elements/shell/coqueba/cbafint_reg_ini.F
!||    cbaforc3                                 ../engine/source/elements/shell/coqueba/cbaforc3.F
!||    cdk6fint_reg                             ../engine/source/elements/sh3n/coquedk6/cdk6fint_reg.F
!||    cdk6forc3                                ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
!||    cdkfint_reg                              ../engine/source/elements/sh3n/coquedk/cdkfint_reg.F
!||    cdkfint_reg_ini                          ../starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.F
!||    cdkforc3                                 ../engine/source/elements/sh3n/coquedk/cdkforc3.F
!||    cfint_reg                                ../engine/source/elements/shell/coque/cfint_reg.F
!||    cfint_reg_ini                            ../starter/source/elements/shell/coque/cfint_reg_ini.F
!||    cforc3                                   ../engine/source/elements/shell/coque/cforc3.F
!||    cforc3_crk                               ../engine/source/elements/xfem/cforc3_crk.F
!||    check_nodal_state                        ../engine/source/interfaces/interf/check_nodal_state.F
!||    check_surface_state                      ../engine/source/interfaces/interf/check_surface_state.F
!||    chkload                                  ../engine/source/interfaces/chkload.F
!||    chkmsr3n                                 ../engine/source/interfaces/interf/chkstfn3.F
!||    chkstfn3n                                ../engine/source/interfaces/interf/chkstfn3.F
!||    cinmas                                   ../starter/source/elements/shell/coque/cinmas.F
!||    clause_init                              ../starter/source/model/sets/clause_init.F
!||    cmain3                                   ../engine/source/materials/mat_share/cmain3.F
!||    cmatc3                                   ../engine/source/elements/shell/coqueba/cmatc3.F
!||    cncoefort                                ../engine/source/elements/sh3n/coquedk/cncoef3.F
!||    compute_voxel_dimensions                 ../engine/source/interfaces/intsort/voxel_dimensions.F90
!||    count_remote_nb_elem_edge                ../engine/source/interfaces/interf/count_remote_nb_elem_edge.F
!||    coupling_read                            ../engine/source/coupling/coupling_adapter.F90
!||    coupling_read_scalar                     ../engine/source/coupling/coupling_adapter.F90
!||    coupling_write                           ../engine/source/coupling/coupling_adapter.F90
!||    coupling_write_scalar                    ../engine/source/coupling/coupling_adapter.F90
!||    create_element_group                     ../starter/source/elements/create_element_group.F90
!||    create_line_from_surface_all             ../starter/source/model/sets/create_line_from_surface_all.F
!||    create_line_from_surface_ext             ../starter/source/model/sets/create_line_from_surface_ext.F
!||    create_line_from_surface_ext_all         ../starter/source/model/sets/create_line_from_ext_surface_ext_all.F
!||    create_node_box                          ../starter/source/model/sets/create_node_box.F
!||    create_nodens_clause                     ../starter/source/model/sets/create_nodens_clause.F90
!||    create_rbody_box                         ../starter/source/model/sets/create_rbody_box.F
!||    create_subs_list                         ../starter/source/model/sets/create_subs_clause.F
!||    crk_accele                               ../engine/source/elements/xfem/accele_crk.F
!||    czforc3                                  ../engine/source/elements/shell/coquez/czforc3.F
!||    czforc3_crk                              ../engine/source/elements/xfem/czforc3_crk.F
!||    daaacc                                   ../engine/source/fluid/daaacc.F
!||    daasolvp                                 ../engine/source/fluid/daasolvp.F
!||    dam_fld_sol                              ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||    damping51                                ../engine/source/assembly/damping.F
!||    dealloc_constraint_struct                ../starter/source/modules/constraint_mod.F90
!||    dealloc_shoot_inter                      ../engine/source/interfaces/interf/dealloc_shoot_inter.F
!||    deallocate_comm_struct                   ../engine/share/modules/mpi_comm_mod.F
!||    deallocate_elbuf                         ../starter/source/elements/elbuf_init/deallocate_buffer.F
!||    deallocate_igrsurf_split                 ../starter/source/spmd/deallocate_igrsurf_split.F
!||    deallocate_joint                         ../engine/source/constraints/general/cyl_joint/deallocate_joint.F
!||    deallocate_one_element_group             ../starter/source/elements/elbuf_init/deallocate_one_element_group.F
!||    deallocate_surf_elm                      ../starter/source/groups/init_surf_elm.F
!||    delnumbf                                 ../engine/source/output/anim/generate/delnumbf.F
!||    deplafakeige                             ../engine/source/assembly/displfakeige.F
!||    dfunc0                                   ../engine/source/output/anim/generate/dfunc0.F
!||    dfuncc                                   ../engine/source/output/anim/generate/dfuncc.F
!||    dfuncc_crk                               ../engine/source/output/anim/generate/dfuncc_crk.F
!||    dfuncc_ply                               ../engine/source/output/anim/generate/dfuncc_ply.F
!||    dfuncf                                   ../engine/source/output/anim/generate/dfuncf.F
!||    dfuncs                                   ../engine/source/output/anim/generate/dfunc6.F
!||    dometis                                  ../starter/source/spmd/domain_decomposition/grid2mat.F
!||    dparrws                                  ../engine/source/output/anim/generate/dparrws.F
!||    drape_wrest                              ../engine/source/output/restart/wrrest.F
!||    dtnodams                                 ../engine/source/time_step/dtnodams.F
!||    dynain_c_strag                           ../engine/source/output/dynain/dynain_c_strag.F
!||    dynain_c_strsg                           ../engine/source/output/dynain/dynain_c_strsg.F
!||    dynain_shel_mp                           ../engine/source/output/dynain/dynain_shel_mp.F
!||    dynain_shel_spmd                         ../engine/source/output/dynain/dynain_shel_spmd.F
!||    ebcs_cyclic_surface_matching_2d          ../starter/source/boundary_conditions/ebcs/ebcs_cyclic_surface_matching_2d.F90
!||    eikonal_fast_marching_method             ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||    eikonal_init_sorting                     ../starter/source/initial_conditions/detonation/eikonal_init_sorting.F90
!||    eikonal_init_start_list                  ../starter/source/initial_conditions/detonation/eikonal_init_start_list.F90
!||    eloff                                    ../engine/source/elements/eloff.F
!||    eos_table_copy                           ../starter/source/materials/tools/eos_table_copy.F90
!||    extend_array_mod                         ../common_source/tools/memory/extend_array.F90
!||    f_nodloc2                                ../starter/source/restart/ddsplit/f_nodloc2.F
!||    fail_fld_c                               ../engine/source/materials/fail/fld/fail_fld_c.F
!||    fail_fld_tsh                             ../engine/source/materials/fail/fld/fail_fld_tsh.F
!||    fail_fld_xfem                            ../engine/source/materials/fail/fld/fail_fld_xfem.F
!||    fail_inievo_b                            ../engine/source/materials/fail/inievo/fail_inievo_b.F90
!||    fail_inievo_c                            ../engine/source/materials/fail/inievo/fail_inievo_c.F
!||    fail_inievo_ib                           ../engine/source/materials/fail/inievo/fail_inievo_ib.F90
!||    fail_inievo_s                            ../engine/source/materials/fail/inievo/fail_inievo_s.F
!||    fail_orthbiquad_c                        ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_c.F
!||    fail_orthbiquad_s                        ../engine/source/materials/fail/orthbiquad/fail_orthbiquad_s.F
!||    fcont_max_output                         ../engine/source/output/h3d/h3d_results/fcont_max_output.F
!||    fill_clause_elt_box                      ../starter/source/model/sets/fill_clause_elt_box.F
!||    fill_clause_node_box                     ../starter/source/model/sets/fill_clause_node_box.F
!||    fill_clause_rbody_box                    ../starter/source/model/sets/fill_clause_rbody_box.F
!||    fill_igr                                 ../starter/source/model/sets/fill_igr.F
!||    fillcne                                  ../starter/source/spmd/domdec2.F
!||    fillcni2                                 ../starter/source/spmd/domdec2.F
!||    find_dt_for_targeted_added_mass          ../engine/source/time_step/find_dt_for_targeted_added_mass.F
!||    find_edge_from_remote_proc               ../engine/source/interfaces/interf/find_edge_from_remote_proc.F
!||    find_edge_inter                          ../engine/source/interfaces/interf/find_edge_inter.F
!||    find_surface_from_remote_proc            ../engine/source/interfaces/interf/find_surface_from_remote_proc.F
!||    find_surface_inter                       ../engine/source/interfaces/interf/find_surface_inter.F
!||    fixfingeo                                ../engine/source/constraints/general/impvel/fixfingeo.F
!||    forint                                   ../engine/source/elements/forint.F
!||    forintp                                  ../engine/source/elements/forintp.F
!||    fractal_dmg_init                         ../starter/source/materials/fail/fractal/fractal_dmg_init.F90
!||    fractal_elem_renum                       ../starter/source/materials/fail/fractal/fractal_elem_spmd_renum.F90
!||    fractal_element_neighbor                 ../starter/source/materials/fail/fractal/fractal_element_neighbor.F90
!||    fraleonoff                               ../engine/source/input/fraleonoff.F
!||    func_table_copy                          ../starter/source/materials/tools/func_table_copy.F90
!||    funct_python_update_elements             ../engine/source/tools/curve/funct_python_update_elements.F90
!||    fv_rwl                                   ../engine/source/constraints/general/rwall/srw_imp.F
!||    fvbag1                                   ../engine/source/airbag/fvbag1.F
!||    fvbag2                                   ../engine/source/airbag/fvbag2.F
!||    fvbc_deallo                              ../engine/source/constraints/general/impvel/fv_imp0.F
!||    fvbric                                   ../engine/source/airbag/fvbric.F
!||    fvdeal                                   ../engine/source/airbag/fvdeal.F
!||    fvmesh0                                  ../engine/source/airbag/fvmesh0.F
!||    fvmesh1                                  ../engine/source/airbag/fvmesh.F
!||    fvrezone1                                ../engine/source/airbag/fvrezone.F
!||    fvupd1                                   ../engine/source/airbag/fvupd.F
!||    fxbyfor                                  ../engine/source/constraints/fxbody/fxbyfor.F
!||    genani                                   ../engine/source/output/anim/generate/genani.F
!||    genani1                                  ../starter/source/output/anim/genani1.F
!||    gendynain                                ../engine/source/output/dynain/gendynain.F
!||    genh3d                                   ../engine/source/output/h3d/h3d_results/genh3d.F
!||    genoutp                                  ../engine/source/output/sty/genoutp.F
!||    genq1np                                  ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    genstat                                  ../engine/source/output/sta/genstat.F
!||    get_element_group                        ../starter/source/elements/get_element_group.F90
!||    get_hashtable_for_neighbour_segment      ../engine/source/interfaces/interf/get_hashtable_for_neighbour_segment.F90
!||    get_list_remnode                         ../starter/source/interfaces/inter3d1/get_list_remnode.F90
!||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||    get_table_value                          ../engine/source/user_interface/utable.F
!||    get_table_value_dydx                     ../engine/source/user_interface/utable.F
!||    get_volume_area                          ../engine/source/airbag/get_volume_area.F90
!||    get_vtable_value                         ../engine/source/user_interface/utable.F
!||    gps_solid                                ../engine/source/output/outmaxsubr.F
!||    gpsstrain_skin                           ../engine/source/output/anim/generate/tensgpstrain.F
!||    gpstra_solid                             ../engine/source/output/outmaxsubr.F
!||    grpsplit                                 ../engine/source/engine/resol_init.F
!||    h3d_create_fvmbag_centroids              ../engine/source/output/h3d/h3d_build_fortran/h3d_create_fvmbag_centroids.F
!||    h3d_create_rbe2_impi                     ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbe2_impi.F
!||    h3d_create_rbe3_impi                     ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbe3_impi.F
!||    h3d_nodal_tensor                         ../engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
!||    h3d_shell_tensor                         ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
!||    h3d_skin_pre_dim                         ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
!||    h3d_skin_pre_map                         ../engine/source/output/h3d/h3d_results/h3d_skin_pre_map.F
!||    h3d_sol_skin_scalar                      ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||    h3d_sol_skin_tensor                      ../engine/source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
!||    h3d_update_fvmbag_centroids              ../engine/source/output/h3d/h3d_build_fortran/h3d_update_fvmbag_centroids.F
!||    h3d_velvecc22                            ../engine/source/output/h3d/h3d_results/h3d_velvecc22.F
!||    h3d_velvecz22                            ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
!||    hierarchy_rbody                          ../starter/source/constraints/general/rbody/hierarchy_rbody.F90
!||    hierarchy_rbody_ddm                      ../starter/source/constraints/general/rbody/hierarchy_rbody.F90
!||    hist1                                    ../engine/source/output/th/hist1.F
!||    hist13                                   ../engine/source/output/th/hist13.F
!||    hm_admlcnt                               ../starter/source/groups/hm_admlistcnt.F
!||    hm_grogro                                ../starter/source/groups/hm_grogro.F
!||    hm_grogronod                             ../starter/source/groups/hm_grogronod.F
!||    hm_lecgre                                ../starter/source/groups/hm_lecgre.F
!||    hm_read_bem                              ../starter/source/loads/bem/hm_read_bem.F
!||    hm_read_fail_orthbiquad                  ../starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
!||    hm_read_impvel                           ../starter/source/constraints/general/impvel/hm_read_impvel.F
!||    hm_read_initemp                          ../starter/source/initial_conditions/thermic/hm_read_initemp.F
!||    hm_read_inter_guided_cable               ../starter/source/tools/seatbelts/hm_read_guided_cable.F90
!||    hm_read_mat                              ../starter/source/materials/mat/hm_read_mat.F90
!||    hm_read_mat88                            ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||    hm_read_table2_1                         ../starter/source/tools/curve/hm_read_table2_1.F
!||    hm_submodgre                             ../starter/source/groups/hm_submodgr.F
!||    hm_submodgrn                             ../starter/source/groups/hm_submodgr.F
!||    hm_submodpart                            ../starter/source/groups/hm_submodpart.F
!||    hm_surfgr2                               ../starter/source/groups/hm_surfgr2.F
!||    hm_tagpart                               ../starter/source/groups/hm_tagpart.F
!||    hm_tagpart2                              ../starter/source/groups/hm_tagpart2.F
!||    hm_thvarc                                ../starter/source/output/th/hm_read_thvarc.F
!||    hm_thvarvent                             ../starter/source/output/th/hm_thvarent.F
!||    i10mainf                                 ../engine/source/interfaces/int10/i10mainf.F
!||    i11main_tri                              ../engine/source/interfaces/intsort/i11main_tri.F
!||    i11mainf                                 ../engine/source/interfaces/int11/i11mainf.F
!||    i11trivox                                ../engine/source/interfaces/intsort/i11trivox.F
!||    i11trivox1                               ../starter/source/interfaces/inter3d1/i11trivox1.F
!||    i17buce                                  ../engine/source/interfaces/int17/i17buce.F
!||    i17buce_pena                             ../engine/source/interfaces/int17/i17buce.F
!||    i17for3                                  ../engine/source/interfaces/int17/i17for3.F
!||    i18main_kine_1                           ../engine/source/interfaces/int18/i18main_kine.F
!||    i18main_kine_2                           ../engine/source/interfaces/int18/i18main_kine.F
!||    i20mainf                                 ../engine/source/interfaces/int20/i20mainf.F
!||    i21mainf                                 ../engine/source/interfaces/int21/i21mainf.F
!||    i21tri                                   ../engine/source/interfaces/intsort/i21tri.F
!||    i22buce                                  ../engine/source/interfaces/intsort/i22buce.F
!||    i22intersect                             ../engine/source/interfaces/int22/i22intersect.F
!||    i22main_tri                              ../engine/source/interfaces/intsort/i22main_tri.F
!||    i22mainf                                 ../engine/source/interfaces/int22/i22mainf.F
!||    i22trivox                                ../engine/source/interfaces/intsort/i22trivox.F
!||    i23mainf                                 ../engine/source/interfaces/int23/i23mainf.F
!||    i23trivox                                ../engine/source/interfaces/intsort/i23trivox.F
!||    i24ke3                                   ../engine/source/interfaces/int24/i24ke3.F
!||    i24main_tri                              ../engine/source/interfaces/intsort/i24main_tri.F
!||    i24mainf                                 ../engine/source/interfaces/int24/i24main.F
!||    i24trivox                                ../engine/source/interfaces/intsort/i24trivox.F
!||    i25comp_2                                ../engine/source/interfaces/int25/i25comp_2.F
!||    i25irtlm                                 ../engine/source/interfaces/int25/i25irtlm.F
!||    i25main_norm                             ../engine/source/interfaces/int25/i25main_norm.F
!||    i25main_slid                             ../engine/source/interfaces/int25/i25main_slid.F
!||    i25main_tri                              ../engine/source/interfaces/intsort/i25main_tri.F
!||    i25mainf                                 ../engine/source/interfaces/int25/i25mainf.F
!||    i25neigh                                 ../starter/source/interfaces/inter3d1/i25neigh.F
!||    i25norm                                  ../starter/source/interfaces/inter3d1/i25norm3.F
!||    i25optcd                                 ../engine/source/interfaces/intsort/i25optcd.F
!||    i25optcd_e2s                             ../engine/source/interfaces/intsort/i25optcd_e2s.F
!||    i25prep_slid_2                           ../engine/source/interfaces/int25/i25slid.F
!||    i25surfi                                 ../starter/source/interfaces/inter3d1/i25surfi.F
!||    i25tagn                                  ../engine/source/interfaces/int25/i25norm.F
!||    i25trivox                                ../engine/source/interfaces/intsort/i25trivox.F
!||    i25trivox1                               ../starter/source/interfaces/inter3d1/i25trivox1.F
!||    i25trivox_edg                            ../engine/source/interfaces/intsort/i25trivox_edg.F
!||    i2_surfi                                 ../starter/source/interfaces/inter3d1/i2_surfi.F90
!||    i2_surfi_dim                             ../starter/source/interfaces/inter3d1/i2_surfi_dim.F90
!||    i2buc1                                   ../starter/source/interfaces/inter3d1/i2buc1.F
!||    i2trivox                                 ../starter/source/interfaces/inter3d1/i2trivox.F90
!||    i7assigeo0                               ../engine/source/interfaces/int07/i7ass3.F
!||    i7main_tri                               ../engine/source/interfaces/intsort/i7main_tri.F
!||    i7mainf                                  ../engine/source/interfaces/int07/i7mainf.F
!||    i7tri                                    ../engine/source/interfaces/intsort/i7tri.F
!||    i7trivox                                 ../engine/source/interfaces/intsort/i7trivox.F
!||    i7trivox1                                ../starter/source/interfaces/inter3d1/i7trivox1.F
!||    idx_fld_sol                              ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
!||    ig3duforc3                               ../engine/source/elements/ige3d/ig3duforc3.F
!||    imp_trans0                               ../engine/source/output/restart/wrrest.F
!||    incpflow                                 ../engine/source/fluid/incpflow.F
!||    iniebcs_propellant_get_cp                ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||    inint3                                   ../starter/source/interfaces/inter3d1/inint3.F
!||    init_ale_spmd                            ../engine/source/ale/init_ale_spmd.F90
!||    init_bcs_nrf                             ../starter/source/boundary_conditions/init_bcs_nrf.F90
!||    init_bcs_wall                            ../starter/source/boundary_conditions/init_bcs_wall.F90
!||    init_global_boundary_list                ../engine/source/mpi/init/init_global_boundary_list.F90
!||    init_i25_edge                            ../engine/source/interfaces/int25/init_i25_edge.F
!||    init_inivol                              ../starter/source/initial_conditions/inivol/init_inivol.F90
!||    init_link_spmd                           ../engine/source/coupling/rad2rad/r2r_init.F
!||    init_monvol                              ../starter/source/airbag/init_monvol.F
!||    init_monvol_omp_structure                ../engine/source/airbag/init_monvol_omp_structure.F90
!||    init_rwall_penalty                       ../starter/source/constraints/general/rwall/init_rwall_penalty.F90
!||    init_th                                  ../engine/source/output/th/init_th.F
!||    initag_preload_a                         ../starter/source/loads/general/preload/hm_read_preload_axial.F90
!||    initnoise2                               ../engine/source/general_controls/computation/initnoise.F
!||    inivel_start                             ../engine/source/loads/general/inivel/inivel_start.F90
!||    insert_clause_in_set                     ../starter/source/model/sets/insert_clause_in_set.F
!||    int18_alloc                              ../engine/source/interfaces/int18/int18_alloc.F
!||    int18_law151_init                        ../engine/source/interfaces/int18/int18_law151_init.F
!||    int18_law151_update                      ../engine/source/interfaces/int18/int18_law151_update.F
!||    intcrit                                  ../engine/source/interfaces/intsort/intcrit.F
!||    inter7_candidate_pairs                   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
!||    inter7_collision_detection               ../engine/source/interfaces/intsort/inter7_collision_detection.F90
!||    inter_color_coarse_voxel                 ../engine/source/interfaces/generic/inter_color_coarse_voxel.F
!||    inter_color_voxel                        ../engine/source/interfaces/generic/inter_color_voxel.F
!||    inter_deallocate_wait                    ../engine/source/interfaces/generic/inter_deallocate_wait.F
!||    inter_init_component                     ../engine/source/interfaces/generic/inter_init_component.F90
!||    inter_init_component_list                ../engine/source/interfaces/generic/inter_init_component_list.F90
!||    inter_init_node_color                    ../engine/source/interfaces/generic/inter_init_node_color.F90
!||    inter_offset_itag                        ../starter/source/elements/shell/shell_offset/inter_offset_itag.F90
!||    inter_sh_offset_ini                      ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
!||    inter_sort_07                            ../engine/source/interfaces/int07/inter_sort_07.F
!||    inter_trc_7                              ../engine/source/interfaces/int07/inter_trc_7.F
!||    inter_voxel_creation                     ../engine/source/interfaces/generic/inter_voxel_creation.F
!||    intfop2                                  ../engine/source/interfaces/interf/intfop2.F
!||    intfop8                                  ../engine/source/interfaces/interf/intfop8.F
!||    intti1                                   ../engine/source/interfaces/interf/intti1.F
!||    inttri                                   ../engine/source/interfaces/intsort/inttri.F
!||    intvo8                                   ../engine/source/interfaces/inter3d/intvo8.F
!||    inverted_group_dealloc                   ../starter/source/model/sets/inverted_group_dealloc.F
!||    ipari_l_ini                              ../starter/source/restart/ddsplit/ipari_l_ini.F
!||    ireallocate                              ../engine/share/modules/realloc_mod.F
!||    ists_sts_bp_persist_save                 ../engine/source/interfaces/ists/ists_sts_bp_persist_mod.F90
!||    ists_sts_ensure_buffers                  ../engine/source/interfaces/ists/ists_sts_capacity_mod.F90
!||    ists_sts_skip_ensure_size                ../engine/source/interfaces/ists/ists_sts_skip_mod.F90
!||    jacobiew_v                               ../engine/source/materials/mat_share/jacobview_v.F
!||    jreallocate                              ../engine/share/modules/realloc_mod.F
!||    law190_upd                               ../starter/source/materials/mat/mat190/law190_upd.F90
!||    law76_func_comp                          ../starter/source/materials/mat/mat076/law76_func_comp.F90
!||    layini                                   ../engine/source/elements/shell/coque/layini.F
!||    lecfvbag                                 ../engine/source/input/lecfvbag.F
!||    lech3d                                   ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
!||    lectur                                   ../engine/source/input/lectur.F
!||    main_beam18                              ../engine/source/elements/beam/main_beam18.F
!||    margin_reduction                         ../starter/source/interfaces/inter3d1/margin.F90
!||    mat_func_deintersect                     ../starter/source/materials/tools/mat_func_deintersect.F90
!||    mat_table_copy                           ../starter/source/materials/tools/mat_table_copy.F90
!||    merge                                    ../starter/source/model/submodel/merge.F
!||    merge_cnod_cnod                          ../starter/source/model/submodel/merge_cnod_cnod.F
!||    min_dist_grnod_to_surface                ../starter/source/model/transformation/min_distance_grnod_to_surface.F90
!||    min_dist_grnod_to_xyzpos                 ../starter/source/model/transformation/min_distance_grnod_to_xyzpos.F90
!||    monv_fvl                                 ../engine/source/airbag/monv_imp0.F
!||    monvol_deallocate                        ../engine/share/modules/monvol_struct_mod.F
!||    monvol_triangulate_surface               ../starter/source/airbag/monvol_triangulate_surface.F
!||    mpi_min_real_end                         ../engine/share/modules/mpi_tools_mod.F
!||    mpp_init                                 ../engine/source/mpi/interfaces/spmd_i7tool.F
!||    mulaw                                    ../engine/source/materials/mat_share/mulaw.F90
!||    mulawc                                   ../engine/source/materials/mat_share/mulawc.F90
!||    multi_deallocate                         ../engine/source/multifluid/multi_deallocate.F
!||    my_alloc_mod                             ../common_source/tools/memory/my_alloc.F90
!||    nbadmesh                                 ../starter/source/model/remesh/nbadmesh.F
!||    nloc_dmg_init                            ../starter/source/materials/fail/nloc_dmg_init.F
!||    nlocal_dtnoda                            ../engine/source/time_step/nlocal_dtnoda.F
!||    nodal_schlieren                          ../engine/source/output/anim/generate/nodal_schlieren.F
!||    nodald                                   ../engine/source/output/anim/generate/nodald.F
!||    nodalt                                   ../engine/source/output/anim/generate/nodalt.F
!||    nodalvfrac                               ../engine/source/output/anim/generate/nodalvfrac.F
!||    nodnx_sms_ini                            ../engine/source/ams/sms_init.F
!||    parsor_crk                               ../engine/source/output/anim/generate/parsor_crk.F
!||    parsor_ply                               ../engine/source/output/anim/generate/parsor_ply.F
!||    parsorc                                  ../engine/source/output/anim/generate/parsorc.F
!||    parsors                                  ../engine/source/output/anim/generate/parsors.F
!||    prerbe3p0                                ../engine/source/constraints/general/rbe3/rbe3f.F
!||    printime_interf                          ../engine/source/system/timer_interf.F
!||    projecig3d                               ../engine/source/elements/ige3d/projecig3d.F
!||    q1np_build_surf_grid                     ../starter/source/elements/solid/solid_q1np/q1np_surf_grid.F90
!||    q1np_compute_volume_element              ../starter/source/elements/solid/solid_q1np/q1np_volume.F90
!||    q1np_contact_algorithms_mod              ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_export_mod                  ../engine/source/interfaces/ists_q1np/q1np_contact_export.F90
!||    q1np_dump_hist_state                     ../engine/source/elements/solid/solid_q1np/q1np_dump_hist_state.F90
!||    q1np_export_bulk_nodes_csv               ../starter/source/elements/solid/solid_q1np/q1np_export_csv.F90
!||    q1np_export_hex8_csv                     ../starter/source/elements/solid/solid_q1np/q1np_export_csv.F90
!||    q1np_fill_element_gp_volumes             ../starter/source/elements/solid/solid_q1np/q1np_init_lbuf_vol.F90
!||    q1np_fit_control_points                  ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_init_lbuf_gp_vol                    ../starter/source/elements/solid/solid_q1np/q1np_init_lbuf_vol.F90
!||    q1np_init_mod                            ../starter/source/elements/solid/solid_q1np/q1np_init.F90
!||    q1np_mass3                               ../starter/source/elements/solid/solid_q1np/q1np_mass3.F90
!||    q1np_promote_cp_to_nodes                 ../starter/source/elements/solid/solid_q1np/q1np_promote_cp.F90
!||    q1np_report_fit_error                    ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_restart_mod                         ../common_source/modules/q1np_restart_mod.F90
!||    q1np_select_global_cp_orientation        ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    qgrhead                                  ../starter/source/elements/solid_2d/quad/qgrhead.F
!||    qgrtails                                 ../starter/source/elements/solid_2d/quad/qgrtails.F
!||    r2r_init                                 ../engine/source/coupling/rad2rad/r2r_init.F
!||    r2r_split                                ../starter/source/coupling/rad2rad/r2r_split.F
!||    r2r_tagel                                ../engine/source/coupling/rad2rad/r2r_exchange.F
!||    r2r_void                                 ../starter/source/coupling/rad2rad/r2r_void.F
!||    radiossviper_inittab                     ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_receiveaccelerations        ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_sendmass                    ../engine/source/coupling/viper/viper_interface_mod.F90
!||    radiossviper_sendxve                     ../engine/source/coupling/viper/viper_interface_mod.F90
!||    random_walk_dmg                          ../starter/source/materials/fail/fractal/random_walk_dmg.F90
!||    rbe3_imp0                                ../engine/source/constraints/general/rbe3/rbe3_imp0.F
!||    rbe3_impd                                ../engine/source/constraints/general/rbe3/rbe3v.F
!||    rbe3_impi                                ../engine/source/constraints/general/rbe3/rbe3_imp0.F
!||    rbe3_impr1                               ../engine/source/constraints/general/rbe3/rbe3_imp0.F
!||    rbe3_impr2                               ../engine/source/constraints/general/rbe3/rbe3_imp0.F
!||    rbe3f                                    ../engine/source/constraints/general/rbe3/rbe3f.F
!||    rbe3v                                    ../engine/source/constraints/general/rbe3/rbe3v.F
!||    rbody_part_check                         ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    rbody_part_modif                         ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    rby_imp5                                 ../engine/source/constraints/general/rbody/rby_impd.F
!||    rbyvit                                   ../engine/source/constraints/general/rbody/rbyvit.F
!||    rdresb                                   ../engine/source/output/restart/rdresb.F
!||    read5p                                   ../engine/source/input/read5p.F
!||    read_ale_rezoning_param                  ../engine/source/output/restart/read_ale_rezoning_param.F90
!||    read_bcs_nrf                             ../engine/source/output/restart/read_bcs_nrf.F90
!||    read_cluster                             ../engine/source/output/cluster/read_cluster.F
!||    read_dynain                              ../engine/source/output/dynain/read_dynain.F
!||    read_elgroup_param                       ../engine/source/output/restart/read_elgroup_param.F
!||    read_failparam                           ../engine/source/output/restart/read_failparam.F
!||    read_failwave                            ../engine/source/output/restart/read_failwave.F
!||    read_joint                               ../engine/source/output/restart/read_joint.F
!||    read_mat_table                           ../engine/source/materials/tools/read_mat_table.F
!||    read_matparam                            ../engine/source/output/restart/read_matparam.F
!||    read_pcyl                                ../engine/source/output/restart/read_pcyl.F
!||    read_sensors                             ../engine/source/output/restart/read_sensors.F
!||    read_thermparam                          ../engine/source/output/restart/read_thermparam.F90
!||    read_viscparam                           ../engine/source/output/restart/read_viscparam.F
!||    reallocate_fi2                           ../engine/source/mpi/interfaces/spmd_i25slide.F
!||    reallocate_i_skyline                     ../engine/source/system/reallocate_skyline.F
!||    resol_init                               ../engine/source/engine/resol_init.F
!||    rforc3                                   ../engine/source/elements/spring/rforc3.F
!||    rgbodfp                                  ../engine/source/constraints/general/rbody/rgbodfp.F
!||    rgbodv                                   ../engine/source/constraints/general/rbody/rgbodv.F
!||    rmdim_imp                                ../engine/source/model/remesh/rm_imp0.F
!||    rpart_inivel_check                       ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    rwall_fpen                               ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||    s4alesfem                                ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
!||    s4fint_reg                               ../engine/source/elements/solid/solide4/s4fint_reg.F
!||    s4forc3                                  ../engine/source/elements/solid/solide4/s4forc3.F
!||    s4lagsfem                                ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
!||    s6cfint_reg                              ../engine/source/elements/thickshell/solide6c/s6cfint_reg.F
!||    s6fint_reg                               ../engine/source/elements/solid/solide6z/s6fint_reg.F90
!||    s8cfint_reg                              ../engine/source/elements/thickshell/solide8c/s8cfint_reg.F
!||    s8cforc3                                 ../engine/source/elements/thickshell/solide8c/s8cforc3.F
!||    s8eforc3                                 ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8sforc3                                 ../engine/source/elements/solid/solide8s/s8sforc3.F
!||    s8zfint_reg                              ../engine/source/elements/solid/solide8z/s8zfint_reg.F
!||    s8zforc3                                 ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    scfint_reg                               ../engine/source/elements/thickshell/solidec/scfint_reg.F
!||    scforc3                                  ../engine/source/elements/thickshell/solidec/scforc3.F
!||    section_3n                               ../engine/source/tools/sect/section_3n.F
!||    section_c                                ../engine/source/tools/sect/section_c.F
!||    section_p                                ../engine/source/tools/sect/section_p.F
!||    section_r                                ../engine/source/tools/sect/section_r.F
!||    section_s                                ../engine/source/tools/sect/section_s.F
!||    section_s4                               ../engine/source/tools/sect/section_s4.F
!||    section_s6                               ../engine/source/tools/sect/section_s6.F
!||    section_t                                ../engine/source/tools/sect/section_t.F
!||    segvar_destroy                           ../engine/share/modules/segvar_mod.F
!||    select_s2s                               ../starter/source/interfaces/inter3d1/select_s2s.F90
!||    sensor_base                              ../engine/source/tools/sensor/sensor_base.F
!||    sensor_init                              ../engine/source/tools/sensor/sensor_init.F
!||    sensor_spmd                              ../engine/source/tools/sensor/sensor_spmd.F
!||    sensor_temp0                             ../engine/source/tools/sensor/sensor_temp0.F
!||    setprojk                                 ../engine/source/elements/shell/coquez/czsumg3.F
!||    setprojkba                               ../engine/source/elements/shell/coqueba/cbasumg3.F
!||    setprojks6                               ../engine/source/elements/thickshell/solide6c/setprojks6.F
!||    setprojkz                                ../engine/source/elements/shell/coquez/czsumg3.F
!||    setprojkz1                               ../engine/source/elements/shell/coquez/czsumg3.F
!||    sfem_exclude_dim                         ../starter/source/elements/solid/solide4/sfem_exclude.F90
!||    sfem_exclude_ini                         ../starter/source/elements/solid/solide4/sfem_exclude.F90
!||    sfem_init                                ../engine/source/elements/solid/solide4_sfem/sfem_init.F90
!||    sfint_reg                                ../engine/source/elements/solid/solide/sfint_reg.F
!||    sforc3                                   ../engine/source/elements/solid/solide/sforc3.F
!||    sh2surf25                                ../starter/source/interfaces/inter3d1/i25surfi.F
!||    sh_offset_jonct_chk                      ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
!||    sh_offset_nproj                          ../starter/source/elements/shell/shell_offset/shell_offset_nproj.F90
!||    shell_offsetp                            ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
!||    sigeps25c                                ../engine/source/materials/mat/mat025/sigeps25c.F
!||    sigeps42c                                ../engine/source/materials/mat/mat042/sigeps42c.F
!||    sigeps93                                 ../engine/source/materials/mat/mat093/sigeps93.F
!||    sigeps93c                                ../engine/source/materials/mat/mat093/sigeps93c.F
!||    sinit22_fvm                              ../engine/source/interfaces/int22/sinit22_fvm.F
!||    sms_admesh_0                             ../engine/source/ams/sms_admesh.F
!||    sms_admesh_1                             ../engine/source/ams/sms_admesh.F
!||    sms_build_diag                           ../engine/source/ams/sms_build_diag.F
!||    sms_build_mat_2                          ../engine/source/ams/sms_build_mat_2.F
!||    sms_check                                ../engine/source/ams/sms_fsa_inv.F
!||    sms_encin_2                              ../engine/source/ams/sms_encin_2.F
!||    sms_fsa_invh                             ../engine/source/ams/sms_fsa_inv.F
!||    sms_ini_int                              ../engine/source/ams/sms_init.F
!||    sms_ini_jad_1                            ../engine/source/ams/sms_init.F
!||    sms_ini_jad_2                            ../engine/source/ams/sms_init.F
!||    sms_ini_jad_3                            ../engine/source/ams/sms_init.F
!||    sms_ini_kad                              ../engine/source/ams/sms_init.F
!||    sms_ini_kdi                              ../engine/source/ams/sms_init.F
!||    sms_ini_kin_1                            ../engine/source/ams/sms_init.F
!||    sms_mass_scale_2                         ../engine/source/ams/sms_mass_scale_2.F
!||    sms_mav_lt                               ../engine/source/ams/sms_pcg.F
!||    sms_rbe3_1                               ../engine/source/ams/sms_rbe3.F
!||    sms_rbe3t2                               ../engine/source/ams/sms_rbe3.F
!||    soltosph_on2                             ../engine/source/elements/sph/soltosph_on2.F
!||    soltospha                                ../engine/source/elements/sph/soltospha.F
!||    soltosphf                                ../engine/source/elements/sph/soltosph.F
!||    sort_mid_pid                             ../engine/source/system/sort_mid_pid.F
!||    sort_set                                 ../starter/source/model/sets/sort_sets.F
!||    sortie_main                              ../engine/source/output/sortie_main.F
!||    spgauge                                  ../engine/source/elements/sph/spgauge.F
!||    splissv                                  ../engine/source/elements/sph/splissv.F
!||    split_cand_i25_edge                      ../starter/source/restart/ddsplit/inter_tools.F
!||    split_interfaces                         ../starter/source/restart/ddsplit/split_interfaces.F
!||    spmd_check_tag                           ../engine/source/mpi/ams/spmd_check_tag.F
!||    spmd_coarse_cell_exchange                ../engine/source/mpi/interfaces/spmd_coarse_cell_exchange.F
!||    spmd_collect_multi_fvm                   ../engine/source/mpi/output/spmd_collect_multi_fvm.F
!||    spmd_exch_efric                          ../engine/source/mpi/interfaces/spmd_exch_efric.F
!||    spmd_exch_press                          ../engine/source/mpi/interfaces/spmd_exch_press.F
!||    spmd_exch_sorting_efric                  ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
!||    spmd_exch_wave_init                      ../engine/source/mpi/nodes/spmd_exch_wave.F
!||    spmd_exchange_component                  ../engine/source/mpi/interfaces/spmd_exch_component.F90
!||    spmd_fvb_gath                            ../engine/source/mpi/airbags/spmd_fvb_gath.F
!||    spmd_fvb_gath_end                        ../engine/source/mpi/airbags/spmd_fvb.F
!||    spmd_fvb_igath                           ../engine/source/mpi/airbags/spmd_fvb_igath.F
!||    spmd_get_penis                           ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_get_penis20                         ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_get_stif25_edg                      ../engine/source/mpi/interfaces/spmd_getstif25_edg.F
!||    spmd_h3d_max_r_nodal_value               ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
!||    spmd_h3d_max_r_nodal_value_21            ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
!||    spmd_h3d_sum_r_nodal                     ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_node.F
!||    spmd_h3d_sum_r_nodal_21                  ../engine/source/output/h3d/spmd/spmd_h3d_gather_r_node.F
!||    spmd_h3d_sum_r_nodal_value               ../engine/source/output/h3d/spmd/spmd_h3d_sum_r_nodal_value.F
!||    spmd_i21fthecom                          ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_i21tempcom                          ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_i25_prepare                         ../engine/source/interfaces/int25/i25_prepare.F
!||    spmd_i7fcom_pon                          ../engine/source/mpi/forces/spmd_i7fcom_pon.F
!||    spmd_ifront_stamp                        ../engine/source/mpi/interfaces/send_cand.F
!||    spmd_ne_connect                          ../starter/source/ale/spmd_ne_connect.F
!||    spmd_sphgetd                             ../engine/source/mpi/elements/spmd_sph.F
!||    spmd_state_inimap1d_exch_data            ../engine/source/output/sta/spmd_state_inimap1d_exch_data.F
!||    spmd_state_inimap2d_exch_data            ../engine/source/output/sta/spmd_state_inimap2d_exch_data.F
!||    spmd_tri10box                            ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri10gat                            ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri11gat                            ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri11vox                            ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri17gat                            ../engine/source/mpi/interfaces/spmd_i7crit.F
!||    spmd_tri18_151vox                        ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri20gat                            ../engine/source/mpi/interfaces/spmd_i7crit.F
!||    spmd_tri20gate                           ../engine/source/mpi/interfaces/spmd_i7crit.F
!||    spmd_tri24gat                            ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri24vox                            ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri25egat                           ../engine/source/mpi/interfaces/spmd_tri25egat.F
!||    spmd_tri25gat                            ../engine/source/mpi/interfaces/spmd_tri25gat.F
!||    spmd_tri25vox                            ../engine/source/mpi/interfaces/spmd_tri25vox.F
!||    spmd_tri7gat                             ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri7vox                             ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_update_frontier_int25               ../engine/source/mpi/interfaces/spmd_update_frontier_int25.F90
!||    sponfv                                   ../engine/source/elements/sph/sponfv.F
!||    sponof2                                  ../engine/source/elements/sph/sponof2.F
!||    sppro3                                   ../engine/source/elements/sph/sppro3.F
!||    spsym_alloc                              ../engine/source/elements/sph/spsym_alloc.F
!||    sptrivox                                 ../engine/source/elements/sph/sptrivox.F
!||    stackgroup_drape                         ../starter/source/stack/stackgroup_drape.F
!||    stat_beam_mp                             ../engine/source/output/sta/stat_beam_mp.F
!||    stat_beam_spmd                           ../engine/source/output/sta/stat_beam_spmd.F
!||    stat_brick_mp                            ../engine/source/output/sta/stat_brick_mp.F
!||    stat_brick_spmd                          ../engine/source/output/sta/stat_brick_spmd.F
!||    stat_c_auxf                              ../engine/source/output/sta/stat_c_auxf.F
!||    stat_c_epspf                             ../engine/source/output/sta/stat_c_epspf.F
!||    stat_c_fail                              ../engine/source/output/sta/stat_c_fail.F
!||    stat_c_orth_loc                          ../engine/source/output/sta/stat_c_orth_loc.F
!||    stat_c_straf                             ../engine/source/output/sta/stat_c_straf.F
!||    stat_c_strafg                            ../engine/source/output/sta/stat_c_strafg.F
!||    stat_c_strsf                             ../engine/source/output/sta/stat_c_strsf.F
!||    stat_c_strsfg                            ../engine/source/output/sta/stat_c_strsfg.F
!||    stat_inimap1d_file_spmd                  ../engine/source/output/sta/stat_inimap1d_file_spmd.F
!||    stat_inimap1d_spmd                       ../engine/source/output/sta/stat_inimap1d_spmd.F
!||    stat_inimap2d_file_spmd                  ../engine/source/output/sta/stat_inimap2d_file_spmd.F
!||    stat_inimap2d_spmd                       ../engine/source/output/sta/stat_inimap2d_spmd.F
!||    stat_n_bcs                               ../engine/source/output/sta/stat_n_bcs.F
!||    stat_n_temp                              ../engine/source/output/sta/stat_n_temp.F
!||    stat_n_vel                               ../engine/source/output/sta/state_n_vel.F
!||    stat_node                                ../engine/source/output/sta/stat_node.F
!||    stat_p_aux                               ../engine/source/output/sta/stat_p_aux.F
!||    stat_p_full                              ../engine/source/output/sta/stat_p_full.F
!||    stat_quad_mp                             ../engine/source/output/sta/stat_quad_mp.F
!||    stat_quad_spmd                           ../engine/source/output/sta/stat_quad_spmd.F
!||    stat_s_auxf                              ../engine/source/output/sta/stat_s_auxf.F
!||    stat_s_eref                              ../engine/source/output/sta/stat_s_eref.F
!||    stat_s_fail                              ../engine/source/output/sta/stat_s_fail.F
!||    stat_s_ortho                             ../engine/source/output/sta/stat_s_ortho.F
!||    stat_s_straf                             ../engine/source/output/sta/stat_s_straf.F
!||    stat_s_strsf                             ../engine/source/output/sta/stat_s_strsf.F
!||    stat_shel_mp                             ../engine/source/output/sta/stat_shel_mp.F
!||    stat_shel_spmd                           ../engine/source/output/sta/stat_shel_spmd.F
!||    stat_sphcel_spmd                         ../engine/source/output/sta/stat_sphcel_spmd.F90
!||    stat_spring_mp                           ../engine/source/output/sta/stat_spring_mp.F
!||    stat_spring_spmd                         ../engine/source/output/sta/stat_spring_spmd.F
!||    stat_t_full                              ../engine/source/output/sta/stat_t_full.F
!||    stat_truss_mp                            ../engine/source/output/sta/stat_truss_mp.F
!||    stat_truss_spmd                          ../engine/source/output/sta/stat_truss_spmd.F
!||    stifint_icontrol                         ../starter/source/interfaces/interf1/stifint_icontrol.F90
!||    stock_msg                                ../engine/source/output/message/stock_msg.F
!||    sts_broad_phase_int7_bucket_mod          ../engine/source/interfaces/ists/ists_broad_phase_int7_bucket.F90
!||    sts_broad_phase_voxel_mod                ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_build_lobatto_gp_weights             ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||    sts_contact_stiffness_mod                ../engine/source/interfaces/ists/ists_contact_stiffness.F90
!||    sts_contacts_assemble                    ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||    sts_gp_state_mod                         ../engine/source/interfaces/ists/ists_gp_state_mod.F90
!||    sts_remap_segments                       ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    switch_to_dtnoda                         ../engine/source/time_step/switch_to_dtnoda.F
!||    szforc3                                  ../engine/source/elements/solid/solidez/szforc3.F
!||    tagoff3n                                 ../engine/source/interfaces/interf/chkstfn3.F
!||    telesc                                   ../engine/source/constraints/general/cyl_joint/telesc.F
!||    tensgps3                                 ../engine/source/output/anim/generate/tensor6.F
!||    tensgps_skin                             ../engine/source/output/anim/generate/tensor6.F
!||    tensgpstrain                             ../engine/source/output/anim/generate/tensgpstrain.F
!||    tensor0                                  ../engine/source/output/anim/generate/tensor0.F
!||    tensorc                                  ../engine/source/output/anim/generate/tensorc.F
!||    tensorc_crk                              ../engine/source/output/anim/generate/tensorc_crk.F
!||    tensorc_ply                              ../engine/source/output/anim/generate/tensorc_ply.F
!||    tensors                                  ../engine/source/output/anim/generate/tensor6.F
!||    thcoq                                    ../engine/source/output/th/thcoq.F
!||    thquad                                   ../engine/source/output/th/thquad.F
!||    thsol                                    ../engine/source/output/th/thsol.F
!||    tmax_ipart                               ../engine/source/output/tmax_ipart.F
!||    torseur                                  ../engine/source/output/anim/generate/torseur.F
!||    tranqikqj                                ../engine/source/elements/shell/coquez/czsumg3.F
!||    transform_translate_in_local_skew        ../starter/source/model/transformation/transform_translate_in_local_skew.F90
!||    upd_aspc                                 ../engine/source/constraints/general/bcs/bc_imp0.F
!||    upd_aspc0                                ../engine/source/constraints/general/bcs/bc_imp0.F
!||    update_neighbour_segment                 ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||    update_slipring                          ../engine/source/tools/seatbelts/update_slipring.F
!||    update_struct_int21                      ../engine/source/interfaces/int21/update_struct_int21.F
!||    upenr_crk                                ../engine/source/elements/xfem/upenr_crk.F
!||    upgrade_rem_2ry                          ../engine/source/mpi/interfaces/spmd_i7tool.F
!||    user_windows_routine                     ../engine/source/user_interface/user_windows.F
!||    ush_init                                 ../starter/source/elements/elbuf_init/ush_init.F90
!||    velvecz22                                ../engine/source/output/anim/generate/velvecz22.F
!||    w_anim_crk                               ../starter/source/restart/ddsplit/w_anim_crk.F
!||    w_anim_ply                               ../starter/source/restart/ddsplit/w_anim_ply.F
!||    w_cluster                                ../engine/source/output/cluster/w_cluster.F
!||    w_elbuf_str                              ../engine/source/elements/elbuf/w_elbuf_str.F
!||    w_failwave                               ../engine/source/output/restart/w_failwave.F
!||    w_fi                                     ../starter/source/restart/ddsplit/w_fi.F
!||    w_front                                  ../starter/source/restart/ddsplit/w_front.F
!||    w_gr_entity                              ../engine/source/output/restart/w_gr_entity.F
!||    w_inloc                                  ../starter/source/restart/ddsplit/w_inloc.F
!||    w_iparg                                  ../starter/source/restart/ddsplit/w_iparg.F
!||    w_iskewsp                                ../starter/source/restart/ddsplit/w_iskewsp.F
!||    w_isph                                   ../starter/source/restart/ddsplit/w_isph.F
!||    w_ixaloc                                 ../starter/source/restart/ddsplit/w_ixaloc.F
!||    w_ixbloc                                 ../starter/source/restart/ddsplit/w_ixbloc.F
!||    w_ixloc                                  ../starter/source/restart/ddsplit/w_ixloc.F
!||    w_line_str                               ../engine/source/output/restart/w_line_str.F
!||    w_outmaxn                                ../starter/source/restart/ddsplit/w_outmaxn.F
!||    w_r3nloc                                 ../starter/source/restart/ddsplit/w_r3nloc.F
!||    w_reloc                                  ../starter/source/restart/ddsplit/w_reloc.F
!||    w_reloc2                                 ../starter/source/restart/ddsplit/w_reloc2.F
!||    w_rfilloc                                ../starter/source/restart/ddsplit/w_rfilloc.F
!||    w_rnloc                                  ../starter/source/restart/ddsplit/w_rnloc.F
!||    w_rnnloc                                 ../starter/source/restart/ddsplit/w_rnnloc.F
!||    w_rsph                                   ../starter/source/restart/ddsplit/w_rsph.F
!||    w_skwsph                                 ../starter/source/restart/ddsplit/w_skwsph.F
!||    w_subset_str                             ../engine/source/output/restart/w_subset_str.F
!||    w_surf_str                               ../engine/source/output/restart/w_surf_str.F
!||    write_ale_rezoning_param                 ../engine/source/output/restart/write_ale_rezoning_param.F90
!||    write_elgroup_param                      ../engine/source/output/restart/write_elgroup_param.F
!||    write_eosparam                           ../engine/source/output/restart/write_eosparam.F90
!||    write_failparam                          ../engine/source/output/restart/write_failparam.F
!||    write_intbuf                             ../engine/source/output/restart/write_intbuf.F
!||    write_mat_table                          ../engine/source/materials/tools/write_mat_table.F
!||    write_matparam                           ../engine/source/output/restart/write_matparam.F
!||    write_nloc_struct                        ../engine/source/output/restart/write_nloc_struct.F
!||    write_pcyl                               ../engine/source/output/restart/write_pcyl.F
!||    write_sensors                            ../engine/source/output/restart/write_sensors.F
!||    write_th                                 ../engine/source/output/th/write_th.F
!||    write_thermparam                         ../engine/source/output/restart/write_thermparam.F90
!||    write_viscparam                          ../engine/source/output/restart/write_viscparam.F
!||    xyznod_crk                               ../engine/source/output/anim/generate/xyznod_crk.F
!||    xyznod_ply                               ../engine/source/output/anim/generate/xyznod_ply.F
!||    xyznor_crk                               ../engine/source/output/anim/generate/xyznor_crk.F
!||    xyznor_ply                               ../engine/source/output/anim/generate/xyznor_ply.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod                             ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      module my_dealloc_mod
        use iso_c_binding, only : c_ptr, c_loc
        use elbufdef_mod, only : elbuf_struct_, g_bufel_, l_bufel_, buf_prop_, buf_nloc_, &
          buf_nlocts_, buf_damp_range_, buf_eos_, buf_poro_, buf_visc_, buf_xfem_, &
          fail_loc_, buf_fail_, buf_mat_, l_bufel_dir_, buf_intloc_, buf_intlay_, buf_lay_
        implicit none

        interface
          subroutine cpp_record_dealloc_addr(addr) bind(C, name="cpp_record_dealloc_addr")
            use iso_c_binding, only : c_ptr
            type(c_ptr), value, intent(in) :: addr
          end subroutine cpp_record_dealloc_addr
        end interface

        private :: my_dealloc_real_1d
        private :: my_dealloc_real_2d
        private :: my_dealloc_real_3d
        private :: my_dealloc_double_1d
        private :: my_dealloc_double_2d
        private :: my_dealloc_double_3d
        private :: my_dealloc_integer_1d
        private :: my_dealloc_integer_2d
        private :: my_dealloc_integer_3d
        private :: my_dealloc_logical_1d
        private :: my_dealloc_logical_2d
        private :: my_dealloc_logical_3d
        private :: my_dealloc_preal_1d
        private :: my_dealloc_preal_2d
        private :: my_dealloc_preal_3d
        private :: my_dealloc_pdouble_1d
        private :: my_dealloc_pdouble_2d
        private :: my_dealloc_pdouble_3d
        private :: my_dealloc_pinteger_1d
        private :: my_dealloc_pinteger_2d
        private :: my_dealloc_pinteger_3d
        private :: my_dealloc_plogical_1d
        private :: my_dealloc_plogical_2d
        private :: my_dealloc_plogical_3d
        private :: my_dealloc_pelbuf_1d
        private :: my_dealloc_pelbuf_2d
        private :: my_dealloc_pelbuf_3d
        private :: my_dealloc_pgbuf_1d
        private :: my_dealloc_pgbuf_2d
        private :: my_dealloc_pgbuf_3d
        private :: my_dealloc_plbuf_1d
        private :: my_dealloc_plbuf_2d
        private :: my_dealloc_plbuf_3d
        private :: my_dealloc_pbufprop_1d
        private :: my_dealloc_pbufprop_2d
        private :: my_dealloc_pbufprop_3d
        private :: my_dealloc_pbufnloc_1d
        private :: my_dealloc_pbufnloc_2d
        private :: my_dealloc_pbufnloc_3d
        private :: my_dealloc_pbufnlocts_1d
        private :: my_dealloc_pbufnlocts_2d
        private :: my_dealloc_pbufnlocts_3d
        private :: my_dealloc_pbufdamp_1d
        private :: my_dealloc_pbufdamp_2d
        private :: my_dealloc_pbufdamp_3d
        private :: my_dealloc_pbufeos_1d
        private :: my_dealloc_pbufeos_2d
        private :: my_dealloc_pbufeos_3d
        private :: my_dealloc_pbufporo_1d
        private :: my_dealloc_pbufporo_2d
        private :: my_dealloc_pbufporo_3d
        private :: my_dealloc_pbufvisc_1d
        private :: my_dealloc_pbufvisc_2d
        private :: my_dealloc_pbufvisc_3d
        private :: my_dealloc_pbufxfem_1d
        private :: my_dealloc_pbufxfem_2d
        private :: my_dealloc_pbufxfem_3d
        private :: my_dealloc_pfailloc_1d
        private :: my_dealloc_pfailloc_2d
        private :: my_dealloc_pfailloc_3d
        private :: my_dealloc_pbuffail_1d
        private :: my_dealloc_pbuffail_2d
        private :: my_dealloc_pbuffail_3d
        private :: my_dealloc_pbufmat_1d
        private :: my_dealloc_pbufmat_2d
        private :: my_dealloc_pbufmat_3d
        private :: my_dealloc_plbufdir_1d
        private :: my_dealloc_plbufdir_2d
        private :: my_dealloc_plbufdir_3d
        private :: my_dealloc_pbufintloc_1d
        private :: my_dealloc_pbufintloc_2d
        private :: my_dealloc_pbufintloc_3d
        private :: my_dealloc_pbufintlay_1d
        private :: my_dealloc_pbufintlay_2d
        private :: my_dealloc_pbufintlay_3d
        private :: my_dealloc_pbuflay_1d
        private :: my_dealloc_pbuflay_2d
        private :: my_dealloc_pbuflay_3d

        public :: my_dealloc

        interface my_dealloc
          module procedure my_dealloc_real_1d
          module procedure my_dealloc_real_2d
          module procedure my_dealloc_real_3d
          module procedure my_dealloc_double_1d
          module procedure my_dealloc_double_2d
          module procedure my_dealloc_double_3d
          module procedure my_dealloc_integer_1d
          module procedure my_dealloc_integer_2d
          module procedure my_dealloc_integer_3d
          module procedure my_dealloc_logical_1d
          module procedure my_dealloc_logical_2d
          module procedure my_dealloc_logical_3d
          module procedure my_dealloc_preal_1d
          module procedure my_dealloc_preal_2d
          module procedure my_dealloc_preal_3d
          module procedure my_dealloc_pdouble_1d
          module procedure my_dealloc_pdouble_2d
          module procedure my_dealloc_pdouble_3d
          module procedure my_dealloc_pinteger_1d
          module procedure my_dealloc_pinteger_2d
          module procedure my_dealloc_pinteger_3d
          module procedure my_dealloc_plogical_1d
          module procedure my_dealloc_plogical_2d
          module procedure my_dealloc_plogical_3d
          module procedure my_dealloc_pelbuf_1d
          module procedure my_dealloc_pelbuf_2d
          module procedure my_dealloc_pelbuf_3d
          module procedure my_dealloc_pgbuf_1d
          module procedure my_dealloc_pgbuf_2d
          module procedure my_dealloc_pgbuf_3d
          module procedure my_dealloc_plbuf_1d
          module procedure my_dealloc_plbuf_2d
          module procedure my_dealloc_plbuf_3d
          module procedure my_dealloc_pbufprop_1d
          module procedure my_dealloc_pbufprop_2d
          module procedure my_dealloc_pbufprop_3d
          module procedure my_dealloc_pbufnloc_1d
          module procedure my_dealloc_pbufnloc_2d
          module procedure my_dealloc_pbufnloc_3d
          module procedure my_dealloc_pbufnlocts_1d
          module procedure my_dealloc_pbufnlocts_2d
          module procedure my_dealloc_pbufnlocts_3d
          module procedure my_dealloc_pbufdamp_1d
          module procedure my_dealloc_pbufdamp_2d
          module procedure my_dealloc_pbufdamp_3d
          module procedure my_dealloc_pbufeos_1d
          module procedure my_dealloc_pbufeos_2d
          module procedure my_dealloc_pbufeos_3d
          module procedure my_dealloc_pbufporo_1d
          module procedure my_dealloc_pbufporo_2d
          module procedure my_dealloc_pbufporo_3d
          module procedure my_dealloc_pbufvisc_1d
          module procedure my_dealloc_pbufvisc_2d
          module procedure my_dealloc_pbufvisc_3d
          module procedure my_dealloc_pbufxfem_1d
          module procedure my_dealloc_pbufxfem_2d
          module procedure my_dealloc_pbufxfem_3d
          module procedure my_dealloc_pfailloc_1d
          module procedure my_dealloc_pfailloc_2d
          module procedure my_dealloc_pfailloc_3d
          module procedure my_dealloc_pbuffail_1d
          module procedure my_dealloc_pbuffail_2d
          module procedure my_dealloc_pbuffail_3d
          module procedure my_dealloc_pbufmat_1d
          module procedure my_dealloc_pbufmat_2d
          module procedure my_dealloc_pbufmat_3d
          module procedure my_dealloc_plbufdir_1d
          module procedure my_dealloc_plbufdir_2d
          module procedure my_dealloc_plbufdir_3d
          module procedure my_dealloc_pbufintloc_1d
          module procedure my_dealloc_pbufintloc_2d
          module procedure my_dealloc_pbufintloc_3d
          module procedure my_dealloc_pbufintlay_1d
          module procedure my_dealloc_pbufintlay_2d
          module procedure my_dealloc_pbufintlay_3d
          module procedure my_dealloc_pbuflay_1d
          module procedure my_dealloc_pbuflay_2d
          module procedure my_dealloc_pbuflay_3d
        end interface my_dealloc

      contains

!||====================================================================
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||--- called by ------------------------------------------------------
!||    my_dealloc_double_1d       ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_double_2d       ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_double_3d       ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_integer_1d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_integer_2d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_integer_3d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_logical_1d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_logical_2d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_logical_3d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufdamp_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufdamp_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufdamp_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufeos_1d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufeos_2d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufeos_3d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbuffail_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbuffail_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbuffail_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufintlay_1d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufintlay_2d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufintlay_3d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufintloc_1d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufintloc_2d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufintloc_3d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbuflay_1d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbuflay_2d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbuflay_3d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufmat_1d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufmat_2d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufmat_3d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufnloc_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufnloc_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufnloc_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufnlocts_1d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufnlocts_2d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufnlocts_3d   ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufporo_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufporo_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufporo_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufprop_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufprop_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufprop_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufvisc_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufvisc_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufvisc_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufxfem_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufxfem_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pbufxfem_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pdouble_1d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pdouble_2d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pdouble_3d      ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pelbuf_1d       ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pelbuf_2d       ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pelbuf_3d       ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pfailloc_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pfailloc_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pfailloc_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pgbuf_1d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pgbuf_2d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pgbuf_3d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pinteger_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pinteger_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_pinteger_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plbuf_1d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plbuf_2d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plbuf_3d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plbufdir_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plbufdir_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plbufdir_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plogical_1d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plogical_2d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_plogical_3d     ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_preal_1d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_preal_2d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_preal_3d        ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_real_1d         ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_real_2d         ../common_source/tools/memory/my_dealloc.F90
!||    my_dealloc_real_3d         ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine record_dealloc_addr(addr)
          use iso_c_binding, only : c_ptr
          type(c_ptr), intent(in) :: addr
          call cpp_record_dealloc_addr(addr)
        end subroutine record_dealloc_addr

! ======================================================================================================================
!                                     GENERATED DEALLOCATION ROUTINES
!   Loop order: MEM_KINDS x TYPES x RANKS
!   MEM_KINDS : allocatable (''), pointer ('p')
!   TYPES     : real, double precision, integer, logical
!   RANKS     : 1d, 2d, 3d
! ======================================================================================================================

!! \brief Deallocate a 1D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_1d    ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_1d(a)
          real, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_1d

!! \brief Deallocate a 2D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_2d    ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_2d(a)
          real, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_2d

!! \brief Deallocate a 3D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_3d    ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_3d(a)
          real, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_3d

!! \brief Deallocate a 1D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr    ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_1d(a)
          double precision, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_1d

!! \brief Deallocate a 2D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr    ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_2d(a)
          double precision, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_2d

!! \brief Deallocate a 3D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr    ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_3d(a)
          double precision, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_3d

!! \brief Deallocate a 1D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_1d(a)
          integer, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_1d

!! \brief Deallocate a 2D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_2d(a)
          integer, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_2d

!! \brief Deallocate a 3D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_3d(a)
          integer, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_3d

!! \brief Deallocate a 1D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_1d(a)
          logical, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_1d

!! \brief Deallocate a 2D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_2d(a)
          logical, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_2d

!! \brief Deallocate a 3D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_3d(a)
          logical, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_3d

!! \brief Deallocate a 1D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_1d(a)
          real, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_1d

!! \brief Deallocate a 2D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_2d(a)
          real, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_2d

!! \brief Deallocate a 3D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_3d(a)
          real, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_3d

!! \brief Deallocate a 1D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_1d(a)
          double precision, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_1d

!! \brief Deallocate a 2D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_2d(a)
          double precision, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_2d

!! \brief Deallocate a 3D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_3d(a)
          double precision, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_3d

!! \brief Deallocate a 1D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_1d(a)
          integer, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_1d

!! \brief Deallocate a 2D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_2d(a)
          integer, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_2d

!! \brief Deallocate a 3D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_3d(a)
          integer, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_3d

!! \brief Deallocate a 1D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_1d(a)
          logical, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_1d

!! \brief Deallocate a 2D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_2d(a)
          logical, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_2d

!! \brief Deallocate a 3D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_3d(a)
          logical, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_3d

!! \brief Deallocate a 1D type(elbuf_struct_) array (pointer)
!||====================================================================
!||    my_dealloc_pelbuf_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr    ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pelbuf_1d(a)
          type(elbuf_struct_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pelbuf_1d

!! \brief Deallocate a 2D type(elbuf_struct_) array (pointer)
!||====================================================================
!||    my_dealloc_pelbuf_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr    ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pelbuf_2d(a)
          type(elbuf_struct_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pelbuf_2d

!! \brief Deallocate a 3D type(elbuf_struct_) array (pointer)
!||====================================================================
!||    my_dealloc_pelbuf_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr    ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pelbuf_3d(a)
          type(elbuf_struct_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pelbuf_3d

!! \brief Deallocate a 1D type(g_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_pgbuf_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pgbuf_1d(a)
          type(g_bufel_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pgbuf_1d

!! \brief Deallocate a 2D type(g_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_pgbuf_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pgbuf_2d(a)
          type(g_bufel_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pgbuf_2d

!! \brief Deallocate a 3D type(g_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_pgbuf_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pgbuf_3d(a)
          type(g_bufel_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pgbuf_3d

!! \brief Deallocate a 1D type(l_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_plbuf_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbuf_1d(a)
          type(l_bufel_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbuf_1d

!! \brief Deallocate a 2D type(l_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_plbuf_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbuf_2d(a)
          type(l_bufel_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbuf_2d

!! \brief Deallocate a 3D type(l_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_plbuf_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbuf_3d(a)
          type(l_bufel_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbuf_3d

!! \brief Deallocate a 1D type(buf_prop_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufprop_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufprop_1d(a)
          type(buf_prop_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufprop_1d

!! \brief Deallocate a 2D type(buf_prop_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufprop_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufprop_2d(a)
          type(buf_prop_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufprop_2d

!! \brief Deallocate a 3D type(buf_prop_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufprop_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufprop_3d(a)
          type(buf_prop_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufprop_3d

!! \brief Deallocate a 1D type(buf_nloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnloc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnloc_1d(a)
          type(buf_nloc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnloc_1d

!! \brief Deallocate a 2D type(buf_nloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnloc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnloc_2d(a)
          type(buf_nloc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnloc_2d

!! \brief Deallocate a 3D type(buf_nloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnloc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnloc_3d(a)
          type(buf_nloc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnloc_3d

!! \brief Deallocate a 1D type(buf_nlocts_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnlocts_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnlocts_1d(a)
          type(buf_nlocts_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnlocts_1d

!! \brief Deallocate a 2D type(buf_nlocts_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnlocts_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnlocts_2d(a)
          type(buf_nlocts_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnlocts_2d

!! \brief Deallocate a 3D type(buf_nlocts_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnlocts_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnlocts_3d(a)
          type(buf_nlocts_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnlocts_3d

!! \brief Deallocate a 1D type(buf_damp_range_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufdamp_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufdamp_1d(a)
          type(buf_damp_range_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufdamp_1d

!! \brief Deallocate a 2D type(buf_damp_range_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufdamp_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufdamp_2d(a)
          type(buf_damp_range_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufdamp_2d

!! \brief Deallocate a 3D type(buf_damp_range_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufdamp_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufdamp_3d(a)
          type(buf_damp_range_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufdamp_3d

!! \brief Deallocate a 1D type(buf_eos_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufeos_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufeos_1d(a)
          type(buf_eos_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufeos_1d

!! \brief Deallocate a 2D type(buf_eos_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufeos_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufeos_2d(a)
          type(buf_eos_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufeos_2d

!! \brief Deallocate a 3D type(buf_eos_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufeos_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufeos_3d(a)
          type(buf_eos_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufeos_3d

!! \brief Deallocate a 1D type(buf_poro_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufporo_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufporo_1d(a)
          type(buf_poro_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufporo_1d

!! \brief Deallocate a 2D type(buf_poro_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufporo_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufporo_2d(a)
          type(buf_poro_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufporo_2d

!! \brief Deallocate a 3D type(buf_poro_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufporo_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufporo_3d(a)
          type(buf_poro_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufporo_3d

!! \brief Deallocate a 1D type(buf_visc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufvisc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufvisc_1d(a)
          type(buf_visc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufvisc_1d

!! \brief Deallocate a 2D type(buf_visc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufvisc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufvisc_2d(a)
          type(buf_visc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufvisc_2d

!! \brief Deallocate a 3D type(buf_visc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufvisc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufvisc_3d(a)
          type(buf_visc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufvisc_3d

!! \brief Deallocate a 1D type(buf_xfem_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufxfem_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufxfem_1d(a)
          type(buf_xfem_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufxfem_1d

!! \brief Deallocate a 2D type(buf_xfem_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufxfem_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufxfem_2d(a)
          type(buf_xfem_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufxfem_2d

!! \brief Deallocate a 3D type(buf_xfem_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufxfem_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufxfem_3d(a)
          type(buf_xfem_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufxfem_3d

!! \brief Deallocate a 1D type(fail_loc_) array (pointer)
!||====================================================================
!||    my_dealloc_pfailloc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pfailloc_1d(a)
          type(fail_loc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pfailloc_1d

!! \brief Deallocate a 2D type(fail_loc_) array (pointer)
!||====================================================================
!||    my_dealloc_pfailloc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pfailloc_2d(a)
          type(fail_loc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pfailloc_2d

!! \brief Deallocate a 3D type(fail_loc_) array (pointer)
!||====================================================================
!||    my_dealloc_pfailloc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pfailloc_3d(a)
          type(fail_loc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pfailloc_3d

!! \brief Deallocate a 1D type(buf_fail_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuffail_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuffail_1d(a)
          type(buf_fail_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuffail_1d

!! \brief Deallocate a 2D type(buf_fail_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuffail_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuffail_2d(a)
          type(buf_fail_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuffail_2d

!! \brief Deallocate a 3D type(buf_fail_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuffail_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuffail_3d(a)
          type(buf_fail_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuffail_3d

!! \brief Deallocate a 1D type(buf_mat_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufmat_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufmat_1d(a)
          type(buf_mat_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufmat_1d

!! \brief Deallocate a 2D type(buf_mat_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufmat_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufmat_2d(a)
          type(buf_mat_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufmat_2d

!! \brief Deallocate a 3D type(buf_mat_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufmat_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufmat_3d(a)
          type(buf_mat_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufmat_3d

!! \brief Deallocate a 1D type(l_bufel_dir_) array (pointer)
!||====================================================================
!||    my_dealloc_plbufdir_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbufdir_1d(a)
          type(l_bufel_dir_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbufdir_1d

!! \brief Deallocate a 2D type(l_bufel_dir_) array (pointer)
!||====================================================================
!||    my_dealloc_plbufdir_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbufdir_2d(a)
          type(l_bufel_dir_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbufdir_2d

!! \brief Deallocate a 3D type(l_bufel_dir_) array (pointer)
!||====================================================================
!||    my_dealloc_plbufdir_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr      ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbufdir_3d(a)
          type(l_bufel_dir_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbufdir_3d

!! \brief Deallocate a 1D type(buf_intloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintloc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintloc_1d(a)
          type(buf_intloc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintloc_1d

!! \brief Deallocate a 2D type(buf_intloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintloc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintloc_2d(a)
          type(buf_intloc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintloc_2d

!! \brief Deallocate a 3D type(buf_intloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintloc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintloc_3d(a)
          type(buf_intloc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintloc_3d

!! \brief Deallocate a 1D type(buf_intlay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintlay_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintlay_1d(a)
          type(buf_intlay_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintlay_1d

!! \brief Deallocate a 2D type(buf_intlay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintlay_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintlay_2d(a)
          type(buf_intlay_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintlay_2d

!! \brief Deallocate a 3D type(buf_intlay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintlay_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr        ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintlay_3d(a)
          type(buf_intlay_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintlay_3d

!! \brief Deallocate a 1D type(buf_lay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuflay_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuflay_1d(a)
          type(buf_lay_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuflay_1d

!! \brief Deallocate a 2D type(buf_lay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuflay_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuflay_2d(a)
          type(buf_lay_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuflay_2d

!! \brief Deallocate a 3D type(buf_lay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuflay_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuflay_3d(a)
          type(buf_lay_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuflay_3d


! ======================================================================================================================
!                            PLACEHOLDER — DERIVED TYPE DEALLOCATION ROUTINES
!
! To add derived type support, add the type to the TYPES set above, or add a dedicated
! interface block below for types that require special teardown before dealloc.
! ======================================================================================================================

      end module my_dealloc_mod
