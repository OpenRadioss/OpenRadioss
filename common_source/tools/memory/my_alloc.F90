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
      !||    my_alloc_mod                       ../common_source/tools/memory/my_alloc.F90
      !||--- called by ------------------------------------------------------
      !||    add_mass_stat                      ../starter/source/tools/admas/add_mass_stat.F
      !||    admdiv                             ../engine/source/model/remesh/admdiv.F
      !||    admfor0                            ../engine/source/model/remesh/admfor0.F
      !||    admordr                            ../engine/source/model/remesh/admordr.F
      !||    admregul                           ../engine/source/model/remesh/admregul.F
      !||    allocate_nodal_arrays              ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    allocate_rbe3                      ../common_source/modules/constraints/rbe3_mod.F90
      !||    allocate_sph_work                  ../common_source/modules/mat_elem/sph_work.F90
      !||    allocbuf_auto                      ../engine/source/elements/elbuf/allocbuf_auto.F
      !||    anioff0                            ../engine/source/output/anim/generate/anioff0.F
      !||    anioffc                            ../engine/source/output/anim/generate/anioffc.F
      !||    anioffc_crk                        ../engine/source/output/anim/generate/anioffc_crk.F
      !||    anioffc_ply                        ../engine/source/output/anim/generate/anioffc_ply.F
      !||    aniofff                            ../engine/source/output/anim/generate/aniofff.F
      !||    anioffs                            ../engine/source/output/anim/generate/anioff6.F
      !||    assadd2                            ../engine/source/assembly/assadd2.F
      !||    boxtagn                            ../starter/source/model/box/bigbox.F
      !||    c3grhead                           ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                          ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c3init3                            ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    cbainit3                           ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cgrhead                            ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                           ../starter/source/elements/shell/coque/cgrtails.F
      !||    chk_dttsh                          ../starter/source/elements/thickshell/solidec/scdtchk3.F
      !||    chkmsr3n                           ../engine/source/interfaces/interf/chkstfn3.F
      !||    cinit3                             ../starter/source/elements/shell/coque/cinit3.F
      !||    compute_voxel_dimensions           ../engine/source/interfaces/intsort/voxel_dimensions.F90
      !||    create_ellipse_clause              ../starter/source/model/sets/create_ellipse_clause.F
      !||    create_line_from_element           ../starter/source/model/sets/create_line_from_element.F
      !||    create_line_from_surface_all       ../starter/source/model/sets/create_line_from_surface_all.F
      !||    create_line_from_surface_ext       ../starter/source/model/sets/create_line_from_surface_ext.F
      !||    create_line_from_surface_ext_all   ../starter/source/model/sets/create_line_from_ext_surface_ext_all.F
      !||    create_seatbelt                    ../starter/source/tools/seatbelts/create_seatbelt.F
      !||    create_surface_from_element        ../starter/source/model/sets/create_surface_from_element.F
      !||    ddsplit                            ../starter/source/restart/ddsplit/ddsplit.F
      !||    delnumbf                           ../engine/source/output/anim/generate/delnumbf.F
      !||    dfunc0                             ../engine/source/output/anim/generate/dfunc0.F
      !||    dfuncc                             ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncc_crk                         ../engine/source/output/anim/generate/dfuncc_crk.F
      !||    dfuncc_ply                         ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    dfuncf                             ../engine/source/output/anim/generate/dfuncf.F
      !||    dfuncs                             ../engine/source/output/anim/generate/dfunc6.F
      !||    dtnodams                           ../engine/source/time_step/dtnodams.F
      !||    elbuf_ini                          ../engine/source/elements/elbuf/elbuf_ini.F
      !||    failwave_init                      ../starter/source/materials/fail/failwave_init.F
      !||    fill_gr                            ../starter/source/model/sets/fill_gr.F
      !||    fill_line                          ../starter/source/model/sets/fill_gr.F
      !||    fill_surf                          ../starter/source/model/sets/fill_gr.F
      !||    fill_surf_ellipse                  ../starter/source/model/sets/fill_gr_surf_ellipse.F
      !||    find_dt_engine                     ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    friction_parts_search              ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    fvbag1                             ../engine/source/airbag/fvbag1.F
      !||    fvmesh0                            ../engine/source/airbag/fvmesh0.F
      !||    genani                             ../engine/source/output/anim/generate/genani.F
      !||    genh3d                             ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genstat                            ../engine/source/output/sta/genstat.F
      !||    gpsstrain_skin                     ../engine/source/output/anim/generate/tensgpstrain.F
      !||    hireorbe3                          ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_grogro                          ../starter/source/groups/hm_grogro.F
      !||    hm_grogronod                       ../starter/source/groups/hm_grogronod.F
      !||    hm_lecgre                          ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                          ../starter/source/groups/hm_lecgrn.F
      !||    hm_lines_of_lines                  ../starter/source/groups/hm_lines_of_lines.F
      !||    hm_prelecgrns                      ../starter/source/groups/hm_prelecgrns.F
      !||    hm_preread_rbody                   ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    hm_read_admas                      ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_box                        ../starter/source/model/box/hm_read_box.F
      !||    hm_read_grpart                     ../starter/source/groups/hm_read_grpart.F
      !||    hm_read_inicrack                   ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_inivol                     ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
      !||    hm_read_lines                      ../starter/source/groups/hm_read_lines.F
      !||    hm_read_merge                      ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_pcyl                       ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_rbe3                       ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                      ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_retractor                  ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_sensors                    ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_slipring                   ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_sphcel                     ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_subset                     ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf                       ../starter/source/groups/hm_read_surf.F
      !||    hm_read_surfsurf                   ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_thgrou                     ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_window_user                ../starter/source/tools/userwi/hm_read_window_user.F
      !||    hm_thvarvent                       ../starter/source/output/th/hm_thvarent.F
      !||    i11mainf                           ../engine/source/interfaces/int11/i11mainf.F
      !||    i21mainf                           ../engine/source/interfaces/int21/i21mainf.F
      !||    i21tri                             ../engine/source/interfaces/intsort/i21tri.F
      !||    i24gapm                            ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i24sti3                            ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i25gapm                            ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25neigh                           ../starter/source/interfaces/inter3d1/i25neigh.F
      !||    i25sti3                            ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25trivox                          ../engine/source/interfaces/intsort/i25trivox.F
      !||    i7remnode                          ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    i7sti3                             ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    i7trivox1                          ../starter/source/interfaces/inter3d1/i7trivox1.F
      !||    ini_seatbelt                       ../starter/source/tools/seatbelts/ini_seatbelt.F
      !||    inint3                             ../starter/source/interfaces/inter3d1/inint3.F
      !||    inintr                             ../starter/source/interfaces/interf1/inintr.F
      !||    init_bcs_wall                      ../starter/source/boundary_conditions/init_bcs_wall.F90
      !||    init_monvol                        ../starter/source/airbag/init_monvol.F
      !||    insert_clause_in_set               ../starter/source/model/sets/insert_clause_in_set.F
      !||    intbuf_fric_ini_starter            ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    intbuf_ini_starter                 ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
      !||    intti1                             ../engine/source/interfaces/interf/intti1.F
      !||    lectur                             ../engine/source/input/lectur.F
      !||    monvol_check_delete_duplicated     ../starter/source/airbag/monvol_check_delete_duplicated.F
      !||    mpp_init                           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    nloc_dmg_init                      ../starter/source/materials/fail/nloc_dmg_init.F
      !||    nodnx_sms_ini                      ../engine/source/ams/sms_init.F
      !||    outri                              ../starter/source/materials/time_step/outri.F
      !||    outrin                             ../starter/source/materials/time_step/outri.F
      !||    parsor_crk                         ../engine/source/output/anim/generate/parsor_crk.F
      !||    parsor_ply                         ../engine/source/output/anim/generate/parsor_ply.F
      !||    parsorc                            ../engine/source/output/anim/generate/parsorc.F
      !||    pre_i2                             ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    prerbe3p0                          ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    presegmt                           ../starter/source/interfaces/interf1/presegmt.F
      !||    r2r_group                          ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_speedup                        ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    r2r_split                          ../starter/source/coupling/rad2rad/r2r_split.F
      !||    r2r_void                           ../starter/source/coupling/rad2rad/r2r_void.F
      !||    r2r_void_1d                        ../starter/source/coupling/rad2rad/r2r_void.F
      !||    rcheckmass                         ../starter/source/elements/spring/rcheckmass.F
      !||    read_box_box                       ../starter/source/model/box/read_box_box.F
      !||    read_eosparam                      ../engine/source/output/restart/read_eosparam.F90
      !||    read_impdisp                       ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_rwall                         ../starter/source/constraints/general/rwall/read_rwall.F
      !||    read_sensor_python                 ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||    read_viscparam                     ../engine/source/output/restart/read_viscparam.F
      !||    remn_i2_edg                        ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2_edgop                      ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op                          ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op_edg25                    ../starter/source/interfaces/int25/i25remlin.F
      !||    remn_self24                        ../starter/source/interfaces/inter3d1/remn_self24.F
      !||    resol                              ../engine/source/engine/resol.F
      !||    restalloc                          ../engine/source/output/restart/arralloc.F
      !||    retrirby                           ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    rgbodfp                            ../engine/source/constraints/general/rbody/rgbodfp.F
      !||    ri2_int24p_ini                     ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    rm_cand24                          ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    sensor_init                        ../engine/source/tools/sensor/sensor_init.F
      !||    set_user_window_nodes              ../starter/source/user_interface/user_windows_tools.F
      !||    seteloff                           ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    setrbyon                           ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    sgrhead                            ../starter/source/elements/solid/solide/sgrhead.F
      !||    sgrtails                           ../starter/source/elements/solid/solide/sgrtails.F
      !||    sms_admesh_0                       ../engine/source/ams/sms_admesh.F
      !||    sms_admesh_1                       ../engine/source/ams/sms_admesh.F
      !||    sms_build_diag                     ../engine/source/ams/sms_build_diag.F
      !||    sms_build_mat_2                    ../engine/source/ams/sms_build_mat_2.F
      !||    sms_check                          ../engine/source/ams/sms_fsa_inv.F
      !||    sms_ini_int                        ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_1                      ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_2                      ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_3                      ../engine/source/ams/sms_init.F
      !||    sms_ini_kad                        ../engine/source/ams/sms_init.F
      !||    sms_ini_kdi                        ../engine/source/ams/sms_init.F
      !||    sms_ini_kin_1                      ../engine/source/ams/sms_init.F
      !||    sms_mass_scale_2                   ../engine/source/ams/sms_mass_scale_2.F
      !||    sms_mav_lt                         ../engine/source/ams/sms_pcg.F
      !||    solid_surface_buffer               ../starter/source/model/sets/solid_surface_buffer.F
      !||    sort_mid_pid                       ../engine/source/system/sort_mid_pid.F
      !||    spgrhead                           ../starter/source/elements/sph/spgrhead.F
      !||    spgrtails                          ../starter/source/elements/sph/spgrtails.F
      !||    splissv                            ../engine/source/elements/sph/splissv.F
      !||    spmd_glob_fsum9                    ../engine/source/mpi/interfaces/spmd_th.F
      !||    spmd_sort_sms                      ../engine/source/mpi/ams/spmd_sms.F
      !||    spmd_userwi_rest                   ../starter/source/user_interface/user_windows_tools.F
      !||    st_qaprint_element                 ../starter/source/output/qaprint/st_qaprint_element.F
      !||    st_qaprint_reference_state         ../starter/source/output/qaprint/st_qaprint_reference_state.F
      !||    stackgroup                         ../starter/source/stack/stackgroup.F
      !||    stat_beam_mp                       ../engine/source/output/sta/stat_beam_mp.F
      !||    stat_beam_spmd                     ../engine/source/output/sta/stat_beam_spmd.F
      !||    stat_brick_mp                      ../engine/source/output/sta/stat_brick_mp.F
      !||    stat_brick_spmd                    ../engine/source/output/sta/stat_brick_spmd.F
      !||    stat_c_auxf                        ../engine/source/output/sta/stat_c_auxf.F
      !||    stat_c_epspf                       ../engine/source/output/sta/stat_c_epspf.F
      !||    stat_c_fail                        ../engine/source/output/sta/stat_c_fail.F
      !||    stat_c_orth_loc                    ../engine/source/output/sta/stat_c_orth_loc.F
      !||    stat_c_straf                       ../engine/source/output/sta/stat_c_straf.F
      !||    stat_c_strafg                      ../engine/source/output/sta/stat_c_strafg.F
      !||    stat_c_strsf                       ../engine/source/output/sta/stat_c_strsf.F
      !||    stat_c_strsfg                      ../engine/source/output/sta/stat_c_strsfg.F
      !||    stat_n_bcs                         ../engine/source/output/sta/stat_n_bcs.F
      !||    stat_n_temp                        ../engine/source/output/sta/stat_n_temp.F
      !||    stat_n_vel                         ../engine/source/output/sta/state_n_vel.F
      !||    stat_node                          ../engine/source/output/sta/stat_node.F
      !||    stat_p_aux                         ../engine/source/output/sta/stat_p_aux.F
      !||    stat_p_full                        ../engine/source/output/sta/stat_p_full.F
      !||    stat_quad_mp                       ../engine/source/output/sta/stat_quad_mp.F
      !||    stat_quad_spmd                     ../engine/source/output/sta/stat_quad_spmd.F
      !||    stat_s_auxf                        ../engine/source/output/sta/stat_s_auxf.F
      !||    stat_s_eref                        ../engine/source/output/sta/stat_s_eref.F
      !||    stat_s_fail                        ../engine/source/output/sta/stat_s_fail.F
      !||    stat_s_ortho                       ../engine/source/output/sta/stat_s_ortho.F
      !||    stat_s_straf                       ../engine/source/output/sta/stat_s_straf.F
      !||    stat_s_strsf                       ../engine/source/output/sta/stat_s_strsf.F
      !||    stat_shel_mp                       ../engine/source/output/sta/stat_shel_mp.F
      !||    stat_shel_spmd                     ../engine/source/output/sta/stat_shel_spmd.F
      !||    stat_sphcel_spmd                   ../engine/source/output/sta/stat_sphcel_spmd.F90
      !||    stat_spring_mp                     ../engine/source/output/sta/stat_spring_mp.F
      !||    stat_spring_spmd                   ../engine/source/output/sta/stat_spring_spmd.F
      !||    stat_t_full                        ../engine/source/output/sta/stat_t_full.F
      !||    stat_truss_mp                      ../engine/source/output/sta/stat_truss_mp.F
      !||    stat_truss_spmd                    ../engine/source/output/sta/stat_truss_spmd.F
      !||    switch_to_dtnoda                   ../engine/source/time_step/switch_to_dtnoda.F
      !||    t3grhead                           ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                          ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    tensgps3                           ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps_skin                       ../engine/source/output/anim/generate/tensor6.F
      !||    tensgpstrain                       ../engine/source/output/anim/generate/tensgpstrain.F
      !||    tensor0                            ../engine/source/output/anim/generate/tensor0.F
      !||    tensorc                            ../engine/source/output/anim/generate/tensorc.F
      !||    tensorc_crk                        ../engine/source/output/anim/generate/tensorc_crk.F
      !||    tensorc_ply                        ../engine/source/output/anim/generate/tensorc_ply.F
      !||    tensors                            ../engine/source/output/anim/generate/tensor6.F
      !||    th_surf_load_pressure              ../starter/source/output/th/th_surf_load_pressure.F
      !||    torseur                            ../engine/source/output/anim/generate/torseur.F
      !||    trirbmerge                         ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    update_pon_shells                  ../engine/source/engine/node_spliting/update_pon.F90
      !||    userwis_front                      ../starter/source/user_interface/user_windows_tools.F
      !||    velvec2                            ../engine/source/output/anim/generate/velvec.F
      !||    velvec3                            ../engine/source/output/anim/generate/velvec.F
      !||    velvecc                            ../engine/source/output/anim/generate/velvec.F
      !||    w_failwave                         ../engine/source/output/restart/w_failwave.F
      !||    w_fi                               ../starter/source/restart/ddsplit/w_fi.F
      !||    w_th_surf_loadp                    ../starter/source/restart/ddsplit/w_th_surf_loadp.F
      !||    w_th_surf_pload                    ../starter/source/restart/ddsplit/w_th_surf_pload.F
      !||    write_nloc_struct                  ../engine/source/output/restart/write_nloc_struct.F
      !||====================================================================
      module my_alloc_mod
        implicit none
        integer, parameter :: len_error_message = 100

        ! lengths : integer
        private :: my_alloc_real_1d
        private :: my_alloc_real_2d
        private :: my_alloc_real_3d
        private :: my_alloc_integer_1d
        private :: my_alloc_integer_2d
        private :: my_alloc_integer_3d
        private :: my_alloc_double_1d
        private :: my_alloc_double_2d
        private :: my_alloc_double_3d
        private :: my_alloc_logical_1d
        private :: my_alloc_logical_2d
        private :: my_alloc_logical_3d

        private :: my_alloc_preal_1d
        private :: my_alloc_preal_2d
        private :: my_alloc_preal_3d
        private :: my_alloc_pinteger_1d
        private :: my_alloc_pinteger_2d
        private :: my_alloc_pinteger_3d
        private :: my_alloc_pdouble_1d
        private :: my_alloc_pdouble_2d
        private :: my_alloc_pdouble_3d
        private :: my_alloc_plogical_1d
        private :: my_alloc_plogical_2d
        private :: my_alloc_plogical_3d


        !lengths : integer(8)
        private :: my_alloc_8_real_1d
        private :: my_alloc_8_real_2d
        private :: my_alloc_8_real_3d
        private :: my_alloc_8_integer_1d
        private :: my_alloc_8_integer_2d
        private :: my_alloc_8_integer_3d
        private :: my_alloc_8_double_1d
        private :: my_alloc_8_double_2d
        private :: my_alloc_8_double_3d
        private :: my_alloc_8_logical_1d
        private :: my_alloc_8_logical_2d
        private :: my_alloc_8_logical_3d
        private :: my_alloc_8_preal_1d
        private :: my_alloc_8_preal_2d
        private :: my_alloc_8_preal_3d
        private :: my_alloc_8_pinteger_1d
        private :: my_alloc_8_pinteger_2d
        private :: my_alloc_8_pinteger_3d
        private :: my_alloc_8_pdouble_1d
        private :: my_alloc_8_pdouble_2d
        private :: my_alloc_8_pdouble_3d
        private :: my_alloc_8_plogical_1d
        private :: my_alloc_8_plogical_2d
        private :: my_alloc_8_plogical_3d

        public :: my_alloc

        interface my_alloc
          module procedure my_alloc_real_1d
          module procedure my_alloc_real_2d
          module procedure my_alloc_real_3d
          module procedure my_alloc_integer_1d
          module procedure my_alloc_integer_2d
          module procedure my_alloc_integer_3d
          module procedure my_alloc_double_1d
          module procedure my_alloc_double_2d
          module procedure my_alloc_double_3d
          module procedure my_alloc_logical_1d
          module procedure my_alloc_logical_2d
          module procedure my_alloc_logical_3d
          module procedure my_alloc_preal_1d
          module procedure my_alloc_preal_2d
          module procedure my_alloc_preal_3d
          module procedure my_alloc_pinteger_1d
          module procedure my_alloc_pinteger_2d
          module procedure my_alloc_pinteger_3d
          module procedure my_alloc_pdouble_1d
          module procedure my_alloc_pdouble_2d
          module procedure my_alloc_pdouble_3d
          module procedure my_alloc_plogical_1d
          module procedure my_alloc_plogical_2d
          module procedure my_alloc_plogical_3d


          module procedure my_alloc_8_real_1d
          module procedure my_alloc_8_real_2d
          module procedure my_alloc_8_real_3d
          module procedure my_alloc_8_integer_1d
          module procedure my_alloc_8_integer_2d
          module procedure my_alloc_8_integer_3d
          module procedure my_alloc_8_double_1d
          module procedure my_alloc_8_double_2d
          module procedure my_alloc_8_double_3d
          module procedure my_alloc_8_logical_1d
          module procedure my_alloc_8_logical_2d
          module procedure my_alloc_8_logical_3d


          module procedure my_alloc_8_preal_1d
          module procedure my_alloc_8_preal_2d
          module procedure my_alloc_8_preal_3d
          module procedure my_alloc_8_pinteger_1d
          module procedure my_alloc_8_pinteger_2d
          module procedure my_alloc_8_pinteger_3d
          module procedure my_alloc_8_pdouble_1d
          module procedure my_alloc_8_pdouble_2d
          module procedure my_alloc_8_pdouble_3d
          module procedure my_alloc_8_plogical_1d
          module procedure my_alloc_8_plogical_2d
          module procedure my_alloc_8_plogical_3d
        end interface my_alloc

      contains

! ======================================================================================================================
!                                                     TOOLS
! ======================================================================================================================
      !||====================================================================
      !||    build_msg      ../common_source/tools/memory/my_alloc.F90
      !||--- called by ------------------------------------------------------
      !||    execargcheck   ../engine/source/engine/execargcheck.F
      !||    radioss2       ../engine/source/engine/radioss2.F
      !||    starter0       ../starter/source/starter/starter0.F
      !||====================================================================
        function build_msg(str) result(error_message)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=*), intent(in) :: str
          character(len=len_error_message) :: error_message
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(len_trim(str) > len_error_message) then
            error_message = str(1:len_error_message)
          else
            error_message = adjustl(str) // repeat(" ", len_error_message - len_trim(str))
          end if
        end function build_msg

!! \brief Check if the allocation was successful and print an error message if it was noti
      !||====================================================================
      !||    check_error_and_write         ../common_source/tools/memory/shrink_array.F90
      !||--- called by ------------------------------------------------------
      !||    extend_array_double_1d        ../common_source/tools/memory/extend_array.F90
      !||    extend_array_double_2d        ../common_source/tools/memory/extend_array.F90
      !||    extend_array_double_3d        ../common_source/tools/memory/extend_array.F90
      !||    extend_array_integer_1d       ../common_source/tools/memory/extend_array.F90
      !||    extend_array_integer_2d       ../common_source/tools/memory/extend_array.F90
      !||    extend_array_integer_3d       ../common_source/tools/memory/extend_array.F90
      !||    extend_array_real_1d          ../common_source/tools/memory/extend_array.F90
      !||    extend_array_real_2d          ../common_source/tools/memory/extend_array.F90
      !||    extend_array_real_3d          ../common_source/tools/memory/extend_array.F90
      !||    my_alloc_8_double_1d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_double_2d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_double_3d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_integer_1d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_integer_2d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_integer_3d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_logical_1d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_logical_2d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_logical_3d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pdouble_1d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pdouble_2d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pdouble_3d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pinteger_1d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pinteger_2d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pinteger_3d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_plogical_1d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_plogical_2d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_plogical_3d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_preal_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_preal_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_preal_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_real_1d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_real_2d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_real_3d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_double_1d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_double_2d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_double_3d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_integer_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_integer_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_integer_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_logical_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_logical_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_logical_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pdouble_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pdouble_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pdouble_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pinteger_1d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pinteger_2d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pinteger_3d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_plogical_1d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_plogical_2d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_plogical_3d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_preal_1d             ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_preal_2d             ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_preal_3d             ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_real_1d              ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_real_2d              ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_real_3d              ../common_source/tools/memory/my_alloc.F90
      !||    reallocate_array_integer_1d   ../common_source/tools/memory/extend_array.F90
      !||    shrink_array_double_1d        ../common_source/tools/memory/shrink_array.F90
      !||    shrink_array_integer_1d       ../common_source/tools/memory/shrink_array.F90
      !||    shrink_array_real_1d          ../common_source/tools/memory/shrink_array.F90
      !||--- calls      -----------------------------------------------------
      !||    arret                         ../engine/source/system/arret.F
      !||====================================================================
        subroutine check_error_and_write(stat,msg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: stat
          character(len=len_error_message), optional,  intent(in) :: msg

! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if (stat /= 0) then
            write(6, "(a,i10,a)") 'Error in memory allocation'
            if(present(msg)) then
              write(6, "(a)") msg
            endif
            call arret(2)
          end if
        end subroutine check_error_and_write


! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_real_1d        ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_real_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_real_2d        ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_real_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_2d

      !||====================================================================
      !||    my_alloc_real_3d        ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_real_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_double_1d      ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_double_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_double_2d      ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_double_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_2d

      !||====================================================================
      !||    my_alloc_double_3d      ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_double_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_integer_1d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_integer_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_integer_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_integer_2d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_integer_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_integer_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_integer_3d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_integer_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_integer_3d

! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_preal_1d       ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_preal_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_preal_2d       ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_preal_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_2d

      !||====================================================================
      !||    my_alloc_preal_3d       ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_preal_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_pdouble_1d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_pdouble_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_pdouble_2d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_pdouble_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_2d

      !||====================================================================
      !||    my_alloc_pdouble_3d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_pdouble_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_pinteger_1d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_pinteger_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pinteger_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_pinteger_2d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_pinteger_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pinteger_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_pinteger_3d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_pinteger_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_pinteger_3d

! ======================
! ====================== INTEGER*8
! ======================
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_8_real_1d      ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_real_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_8_real_2d      ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_real_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_2d

      !||====================================================================
      !||    my_alloc_8_real_3d      ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_real_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_8_double_1d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_double_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_8_double_2d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_double_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_2d

      !||====================================================================
      !||    my_alloc_8_double_3d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_double_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_8_integer_1d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_integer_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_integer_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_8_integer_2d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_integer_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_integer_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_8_integer_3d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_integer_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_8_integer_3d
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_8_preal_1d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_preal_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_8_preal_2d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_preal_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_2d

      !||====================================================================
      !||    my_alloc_8_preal_3d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_preal_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_8_pdouble_1d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_pdouble_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_8_pdouble_2d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_pdouble_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_2d

      !||====================================================================
      !||    my_alloc_8_pdouble_3d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_pdouble_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_8_pinteger_1d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_pinteger_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pinteger_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_8_pinteger_2d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_pinteger_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pinteger_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_8_pinteger_3d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_pinteger_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_8_pinteger_3d

! ======================================================================================================================
!                                           LOGICAL ALLOCATION ROUTINES
! ======================================================================================================================
!! \brief Allocate a 1D array of logical numbers
      !||====================================================================
      !||    my_alloc_logical_1d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_logical_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_logical_1d

!! \brief Allocate a 2D array of logical numbers
      !||====================================================================
      !||    my_alloc_logical_2d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_logical_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_logical_2d

      !||====================================================================
      !||    my_alloc_logical_3d     ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_logical_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_logical_3d
!! \brief Allocate a 1D array of logical numbers
      !||====================================================================
      !||    my_alloc_plogical_1d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_plogical_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_plogical_1d

!! \brief Allocate a 2D array of logical numbers
      !||====================================================================
      !||    my_alloc_plogical_2d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_plogical_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_plogical_2d

      !||====================================================================
      !||    my_alloc_plogical_3d    ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_plogical_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_plogical_3d
! ======================================================================================================================
!                                           LOGICAL ALLOCATION ROUTINES
! ======================================================================================================================
!! \brief Allocate a 1D array of logical numbers
      !||====================================================================
      !||    my_alloc_8_logical_1d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_logical_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_logical_1d

!! \brief Allocate a 2D array of logical numbers
      !||====================================================================
      !||    my_alloc_8_logical_2d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_logical_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_logical_2d

      !||====================================================================
      !||    my_alloc_8_logical_3d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_logical_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_logical_3d
!! \brief Allocate a 1D array of logical numbers
      !||====================================================================
      !||    my_alloc_8_plogical_1d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_plogical_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_plogical_1d

!! \brief Allocate a 2D array of logical numbers
      !||====================================================================
      !||    my_alloc_8_plogical_2d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_plogical_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_plogical_2d

      !||====================================================================
      !||    my_alloc_8_plogical_3d   ../common_source/tools/memory/my_alloc.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
        subroutine my_alloc_8_plogical_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,m,n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_plogical_3d
      end module my_alloc_mod
