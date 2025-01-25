Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    ancmsg                              ../starter/source/output/message/message.F
      !||--- called by ------------------------------------------------------
      !||    addmaspart                          ../starter/source/tools/admas/addmaspart.F
      !||    admbcs                              ../starter/source/model/remesh/admbcs.F
      !||    admlcnt                             ../starter/source/model/remesh/admlist.F
      !||    admlist                             ../starter/source/model/remesh/admlist.F
      !||    ale_check_lag                       ../starter/source/ale/ale_check_lag.F
      !||    ale_euler_init                      ../starter/source/materials/ale/ale_euler_init.F
      !||    allocbuf_auto                       ../starter/source/elements/elbuf_init/allocbuf_auto.F
      !||    arret_message                       ../starter/source/interfaces/int24/arret_message.F
      !||    arret_message_slid                  ../starter/source/interfaces/int24/arret_message.F
      !||    auto_node_merge                     ../starter/source/elements/nodes/auto_node_merge.F
      !||    bcscycmodif_nd                      ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    bcsmodif_nd                         ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    bigbox                              ../starter/source/model/box/bigbox.F
      !||    bigbox2                             ../starter/source/model/box/bigbox.F
      !||    bigsbox                             ../starter/source/model/box/bigbox.F
      !||    boxbox                              ../starter/source/model/box/boxbox.F
      !||    boxbox2                             ../starter/source/model/box/boxbox.F
      !||    boxboxs                             ../starter/source/model/box/boxbox.F
      !||    bsigini                             ../starter/source/elements/beam/bsigini.F
      !||    build_admesh                        ../starter/source/model/remesh/build_admesh.F
      !||    buserini                            ../starter/source/elements/beam/buserini.F
      !||    c3coori                             ../starter/source/elements/sh3n/coque3n/c3coori.F
      !||    c3epschk                            ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    c3grhead                            ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                           ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c3init3                             ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    c3inmas                             ../starter/source/elements/sh3n/coque3n/c3inmas.F
      !||    cbainit3                            ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    ccoori                              ../starter/source/elements/shell/coque/ccoori.F
      !||    cdk6inx                             ../starter/source/elements/sh3n/coquedk6/cdk6inx.F
      !||    cdkinit3                            ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cepschk                             ../starter/source/elements/shell/coque/cepsini.F
      !||    cgrhead                             ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                            ../starter/source/elements/shell/coque/cgrtails.F
      !||    check_dynain                        ../starter/source/starter/check_dynain.F
      !||    check_eltyp                         ../starter/source/model/sets/check_eltyp.F
      !||    check_mat_elem_prop_compatibility   ../starter/source/materials/mat/check_mat_elem_prop_compatibility.F
      !||    check_pthickfail                    ../starter/source/materials/fail/check_pthickfail.F
      !||    checkmp                             ../starter/source/elements/initia/initia.F
      !||    checkrby                            ../starter/source/constraints/general/rbody/checkrby.F
      !||    chekmp2                             ../starter/source/elements/initia/initia.F
      !||    chk_dttsh                           ../starter/source/elements/thickshell/solidec/scdtchk3.F
      !||    chkfunct                            ../starter/source/tools/curve/lecfun.F
      !||    chktyp2                             ../starter/source/interfaces/interf1/chktyp2.F
      !||    cinit3                              ../starter/source/elements/shell/coque/cinit3.F
      !||    cinmas                              ../starter/source/elements/shell/coque/cinmas.F
      !||    cm27in3                             ../starter/source/materials/mat/mat027/cm27in3.F
      !||    cmaini3                             ../starter/source/elements/sh3n/coquedk/cmaini3.F
      !||    cmatini4                            ../starter/source/materials/mat_share/cmatini4.F
      !||    contrl                              ../starter/source/starter/contrl.F
      !||    corthini                            ../starter/source/elements/shell/coque/corthini.F
      !||    create_child                        ../starter/source/coupling/rad2rad/r2r_fork.F
      !||    create_ellipse_clause               ../starter/source/model/sets/create_ellipse_clause.F
      !||    create_elt_list                     ../starter/source/model/sets/create_element_clause.F
      !||    create_map_tables                   ../starter/source/model/sets/map_tables.F
      !||    create_node_list                    ../starter/source/model/sets/create_node_clause.F
      !||    create_nodens_clause                ../starter/source/model/sets/create_nodens_clause.F90
      !||    create_part_clause                  ../starter/source/model/sets/create_part_clause.F
      !||    create_part_list                    ../starter/source/model/sets/create_part_clause.F
      !||    create_plane_clause                 ../starter/source/model/sets/create_plane_clause.F90
      !||    create_rbody_list                   ../starter/source/model/sets/create_rbody_clause.F
      !||    create_seatbelt                     ../starter/source/tools/seatbelts/create_seatbelt.F
      !||    create_seg_clause                   ../starter/source/model/sets/create_seg_clause.F
      !||    create_set_list                     ../starter/source/model/sets/create_set_clause.F
      !||    create_subm_list                    ../starter/source/model/sets/create_subm_clause.F
      !||    create_subs_list                    ../starter/source/model/sets/create_subs_clause.F
      !||    csigini                             ../starter/source/elements/shell/coque/csigini.F
      !||    csigini4                            ../starter/source/elements/shell/coqueba/scigini4.F
      !||    cstraini4                           ../starter/source/elements/shell/coqueba/cstraini4.F
      !||    cuserini4                           ../starter/source/elements/shell/coqueba/cuserini4.F
      !||    defbeam_sect                        ../starter/source/properties/beam/hm_read_prop18.F
      !||    definter                            ../starter/source/interfaces/interf1/definter.F
      !||    domdec2                             ../starter/source/spmd/domdec2.F
      !||    dometis                             ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    ecran1                              ../starter/source/initial_conditions/detonation/ecran1.F
      !||    elstagboxbox                        ../starter/source/model/box/boxbox.F
      !||    eosfun_usr2sys                      ../starter/source/materials/tools/eosfun_usr2sys.F90
      !||    error_alloc                         ../starter/source/system/error_alloc.F
      !||    facebox                             ../starter/source/model/box/bigbox.F
      !||    fail_fun2sys                        ../starter/source/materials/tools/fail_fun2sys.F
      !||    fail_tab2sys                        ../starter/source/materials/tools/fail_tab2sys.F
      !||    fill_buffer_51                      ../starter/source/materials/mat/mat051/fill_buffer_51.F
      !||    find_dt1brick_engine                ../starter/source/starter/contrl.F
      !||    find_yield_engine                   ../starter/source/general_controls/computation/hm_read_sms.F
      !||    fixmodif_nd                         ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    fredec0                             ../starter/source/starter/freform.F
      !||    fredec5                             ../starter/source/starter/freform.F
      !||    fredec_2key_4id                     ../starter/source/starter/freform.F
      !||    fredec_2key_4id_t                   ../starter/source/starter/freform.F
      !||    fredec_2key_id_or_key_id            ../starter/source/starter/freform.F
      !||    fredec_key_3id_t                    ../starter/source/starter/freform.F
      !||    freerr                              ../starter/source/starter/freform.F
      !||    fsdcod                              ../starter/source/system/fsdcod.F
      !||    fvbric0                             ../starter/source/airbag/fvbric0.F
      !||    fvbric1                             ../starter/source/airbag/fvbric1.F
      !||    fvinject                            ../starter/source/airbag/fvinject.F
      !||    fvmesh1                             ../starter/source/airbag/fvmesh.F
      !||    fvventhole                          ../starter/source/airbag/fvventhole.F
      !||    fvverif                             ../starter/source/airbag/fvmbag1.F
      !||    get_skew                            ../starter/source/elements/joint/rjoint/rini33.F
      !||    get_skew45                          ../starter/source/elements/joint/rjoint/rini45.F
      !||    get_u_func_y                        ../starter/source/user_interface/uaccess.F
      !||    get_u_geo                           ../starter/source/user_interface/uaccess.F
      !||    get_u_mat                           ../starter/source/user_interface/uaccess.F
      !||    get_u_mnu                           ../starter/source/user_interface/uaccess.F
      !||    get_u_pnu                           ../starter/source/user_interface/uaccess.F
      !||    grfind                              ../starter/source/starter/freform.F
      !||    hireorbe3                           ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_admlcnt                          ../starter/source/groups/hm_admlistcnt.F
      !||    hm_admlist                          ../starter/source/groups/hm_admlist.F
      !||    hm_bigbox                           ../starter/source/model/box/hm_bigbox.F
      !||    hm_bigbox2                          ../starter/source/model/box/hm_bigbox2.F
      !||    hm_bigsbox                          ../starter/source/groups/hm_bigsbox.F
      !||    hm_elngr                            ../starter/source/groups/hm_elngr.F
      !||    hm_elngrr                           ../starter/source/groups/hm_elngrr.F
      !||    hm_elngrs                           ../starter/source/groups/hm_elngr.F
      !||    hm_grogro                           ../starter/source/groups/hm_grogro.F
      !||    hm_grogronod                        ../starter/source/groups/hm_grogronod.F
      !||    hm_lecgre                           ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                           ../starter/source/groups/hm_lecgrn.F
      !||    hm_linengr                          ../starter/source/groups/hm_linengr.F
      !||    hm_lines_of_lines                   ../starter/source/groups/hm_lines_of_lines.F
      !||    hm_messages                         ../starter/source/devtools/hm_reader/hm_messages.F
      !||    hm_prelce16s                        ../starter/source/elements/reader/hm_read_solid.F
      !||    hm_preread_bcscyc                   ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    hm_preread_bem                      ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_preread_eig                      ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_preread_impdisp                  ../starter/source/constraints/general/impvel/hm_preread_impdisp.F
      !||    hm_preread_imptemp                  ../starter/source/constraints/thermic/hm_preread_imptemp.F
      !||    hm_preread_impvel                   ../starter/source/constraints/general/impvel/hm_preread_impvel.F
      !||    hm_preread_load_pressure            ../starter/source/loads/general/load_pressure/hm_preread_load_pressure.F
      !||    hm_preread_node                     ../starter/source/elements/reader/hm_preread_node.F
      !||    hm_preread_pfluid                   ../starter/source/loads/general/pfluid/hm_preread_pfluid.F
      !||    hm_preread_xelem                    ../starter/source/elements/reader/hm_preread_xelem.F
      !||    hm_prop_read21                      ../starter/source/properties/thickshell/hm_read_prop21.F
      !||    hm_read_activ                       ../starter/source/tools/activ/hm_read_activ.F
      !||    hm_read_admas                       ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_ale_grid                    ../starter/source/general_controls/ale_grid/hm_read_ale_grid.F
      !||    hm_read_ale_link                    ../starter/source/constraints/ale/hm_read_ale_link_vel.F
      !||    hm_read_ale_muscl                   ../starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
      !||    hm_read_ale_solver                  ../starter/source/general_controls/ale_cfd/hm_read_ale_solver.F
      !||    hm_read_alebcs                      ../starter/source/constraints/ale/hm_read_alebcs.F
      !||    hm_read_bcs                         ../starter/source/constraints/general/bcs/hm_read_bcs.F
      !||    hm_read_bcs_wall                    ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_beam                        ../starter/source/elements/reader/hm_read_beam.F
      !||    hm_read_bem                         ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_cload                       ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_cluster                     ../starter/source/output/cluster/hm_read_cluster.F
      !||    hm_read_convec                      ../starter/source/loads/thermic/hm_read_convec.F
      !||    hm_read_damp                        ../starter/source/general_controls/damping/hm_read_damp.F
      !||    hm_read_drape                       ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_ebcs_fluxout                ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
      !||    hm_read_ebcs_gradp0                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
      !||    hm_read_ebcs_inip                   ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
      !||    hm_read_ebcs_iniv                   ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
      !||    hm_read_ebcs_inlet                  ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
      !||    hm_read_ebcs_monvol                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
      !||    hm_read_ebcs_normv                  ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
      !||    hm_read_ebcs_nrf                    ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
      !||    hm_read_ebcs_pres                   ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
      !||    hm_read_ebcs_propergol              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_propergol.F90
      !||    hm_read_ebcs_valvin                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
      !||    hm_read_ebcs_valvout                ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
      !||    hm_read_ebcs_vel                    ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
      !||    hm_read_eig                         ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eos                         ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_eos_compaction              ../starter/source/materials/eos/hm_read_eos_compaction.F90
      !||    hm_read_eos_compaction2             ../starter/source/materials/eos/hm_read_eos_compaction2.F90
      !||    hm_read_eos_gruneisen               ../starter/source/materials/eos/hm_read_eos_gruneisen.F
      !||    hm_read_eos_ideal_gas               ../starter/source/materials/eos/hm_read_eos_ideal_gas.F
      !||    hm_read_eos_ideal_gas_vt            ../starter/source/materials/eos/hm_read_eos_ideal_gas_vt.F
      !||    hm_read_eos_lszk                    ../starter/source/materials/eos/hm_read_eos_lszk.F
      !||    hm_read_eos_murnaghan               ../starter/source/materials/eos/hm_read_eos_murnaghan.F
      !||    hm_read_eos_nasg                    ../starter/source/materials/eos/hm_read_eos_nasg.F
      !||    hm_read_eos_osborne                 ../starter/source/materials/eos/hm_read_eos_osborne.F
      !||    hm_read_eos_powderburn              ../starter/source/materials/eos/hm_read_eos_powderburn.F90
      !||    hm_read_eos_sesame                  ../starter/source/materials/eos/hm_read_eos_sesame.F
      !||    hm_read_eos_stiffened_gas           ../starter/source/materials/eos/hm_read_eos_stiffened_gas.F
      !||    hm_read_eos_tabulated               ../starter/source/materials/eos/hm_read_eos_tabulated.F
      !||    hm_read_eref                        ../starter/source/loads/reference_state/eref/hm_read_eref.F
      !||    hm_read_fail                        ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_fail_biquad                 ../starter/source/materials/fail/biquad/hm_read_fail_biquad.F
      !||    hm_read_fail_energy                 ../starter/source/materials/fail/energy/hm_read_fail_energy.F
      !||    hm_read_fail_fabric                 ../starter/source/materials/fail/fabric/hm_read_fail_fabric.F
      !||    hm_read_fail_fld                    ../starter/source/materials/fail/fld/hm_read_fail_fld.F
      !||    hm_read_fail_gene1                  ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
      !||    hm_read_fail_gurson                 ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
      !||    hm_read_fail_inievo                 ../starter/source/materials/fail/inievo/hm_read_fail_inievo.F
      !||    hm_read_fail_johnson                ../starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
      !||    hm_read_fail_orthbiquad             ../starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
      !||    hm_read_fail_spalling               ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F
      !||    hm_read_fail_syazwan                ../starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
      !||    hm_read_fail_tab1                   ../starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
      !||    hm_read_fail_tab2                   ../starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
      !||    hm_read_fail_tab_old                ../starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
      !||    hm_read_fail_tbutcher               ../starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
      !||    hm_read_fail_tensstrain             ../starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
      !||    hm_read_fail_user                   ../starter/source/materials/fail/failuser/hm_read_fail_user.F
      !||    hm_read_friction                    ../starter/source/interfaces/friction/reader/hm_read_friction.F
      !||    hm_read_friction_models             ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_friction_orientations       ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_frm                         ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_funct                       ../starter/source/tools/curve/hm_read_funct.F
      !||    hm_read_funct_python                ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    hm_read_fxb1                        ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_gauge                       ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_grav                        ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_impacc                      ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_impflux                     ../starter/source/constraints/thermic/hm_read_impflux.F
      !||    hm_read_imptemp                     ../starter/source/constraints/thermic/hm_read_imptemp.F
      !||    hm_read_inigrav                     ../starter/source/initial_conditions/inigrav/hm_read_inigrav.F
      !||    hm_read_inimap1d                    ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inimap2d                    ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    hm_read_inistate_d00                ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_initemp                     ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_inivel                      ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inivol                      ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
      !||    hm_read_inject1                     ../starter/source/properties/injector/hm_read_inject1.F
      !||    hm_read_inject2                     ../starter/source/properties/injector/hm_read_inject2.F
      !||    hm_read_inter_lagdt_type07          ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
      !||    hm_read_inter_lagmul                ../starter/source/interfaces/reader/hm_read_inter_lagmul.F
      !||    hm_read_inter_lagmul_type02         ../starter/source/interfaces/int02/hm_read_inter_lagmul_type02.F
      !||    hm_read_inter_lagmul_type07         ../starter/source/interfaces/int07/hm_read_inter_lagmul_type07.F
      !||    hm_read_inter_lagmul_type16         ../starter/source/interfaces/int16/hm_read_inter_lagmul_type16.F
      !||    hm_read_inter_lagmul_type17         ../starter/source/interfaces/int17/hm_read_inter_lagmul_type17.F
      !||    hm_read_inter_struct                ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_inter_type02                ../starter/source/interfaces/int02/hm_read_inter_type02.F
      !||    hm_read_inter_type05                ../starter/source/interfaces/int05/hm_read_inter_type05.F
      !||    hm_read_inter_type06                ../starter/source/interfaces/int06/hm_read_inter_type06.F
      !||    hm_read_inter_type07                ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type08                ../starter/source/interfaces/int08/hm_read_inter_type08.F
      !||    hm_read_inter_type10                ../starter/source/interfaces/int10/hm_read_inter_type10.F
      !||    hm_read_inter_type11                ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type14                ../starter/source/interfaces/int14/hm_read_inter_type14.F
      !||    hm_read_inter_type15                ../starter/source/interfaces/int15/hm_read_inter_type15.F
      !||    hm_read_inter_type18                ../starter/source/interfaces/int18/hm_read_inter_type18.F
      !||    hm_read_inter_type20                ../starter/source/interfaces/int20/hm_read_inter_type20.F
      !||    hm_read_inter_type21                ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type22                ../starter/source/interfaces/int22/hm_read_inter_type22.F
      !||    hm_read_inter_type23                ../starter/source/interfaces/int23/hm_read_inter_type23.F
      !||    hm_read_inter_type24                ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25                ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_interfaces                  ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_intsub                      ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_leak                        ../starter/source/airbag/hm_read_leak.F
      !||    hm_read_lines                       ../starter/source/groups/hm_read_lines.F
      !||    hm_read_link                        ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_load_centri                 ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure               ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_mat                         ../starter/source/materials/mat/hm_read_mat.F90
      !||    hm_read_mat01                       ../starter/source/materials/mat/mat001/hm_read_mat01.F
      !||    hm_read_mat02                       ../starter/source/materials/mat/mat002/hm_read_mat02.F
      !||    hm_read_mat03                       ../starter/source/materials/mat/mat003/hm_read_mat03.F
      !||    hm_read_mat04                       ../starter/source/materials/mat/mat004/hm_read_mat04.F
      !||    hm_read_mat10                       ../starter/source/materials/mat/mat010/hm_read_mat10.F
      !||    hm_read_mat100                      ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat102                      ../starter/source/materials/mat/mat102/hm_read_mat102.F
      !||    hm_read_mat103                      ../starter/source/materials/mat/mat103/hm_read_mat103.F
      !||    hm_read_mat104                      ../starter/source/materials/mat/mat104/hm_read_mat104.F
      !||    hm_read_mat106                      ../starter/source/materials/mat/mat106/hm_read_mat106.F
      !||    hm_read_mat107                      ../starter/source/materials/mat/mat107/hm_read_mat107.F
      !||    hm_read_mat108                      ../starter/source/materials/mat/mat108/hm_read_mat108.F
      !||    hm_read_mat11                       ../starter/source/materials/mat/mat011/hm_read_mat11.F
      !||    hm_read_mat110                      ../starter/source/materials/mat/mat110/hm_read_mat110.F
      !||    hm_read_mat111                      ../starter/source/materials/mat/mat111/hm_read_mat111.F
      !||    hm_read_mat112                      ../starter/source/materials/mat/mat112/hm_read_mat112.F
      !||    hm_read_mat113                      ../starter/source/materials/mat/mat113/hm_read_mat113.F
      !||    hm_read_mat114                      ../starter/source/materials/mat/mat114/hm_read_mat114.F
      !||    hm_read_mat115                      ../starter/source/materials/mat/mat115/hm_read_mat115.F
      !||    hm_read_mat116                      ../starter/source/materials/mat/mat116/hm_read_mat116.F
      !||    hm_read_mat117                      ../starter/source/materials/mat/mat117/hm_read_mat117.F
      !||    hm_read_mat119                      ../starter/source/materials/mat/mat119/hm_read_mat119.F
      !||    hm_read_mat11_k_eps                 ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
      !||    hm_read_mat12                       ../starter/source/materials/mat/mat012/hm_read_mat12.F
      !||    hm_read_mat121                      ../starter/source/materials/mat/mat121/hm_read_mat121.F
      !||    hm_read_mat122                      ../starter/source/materials/mat/mat122/hm_read_mat122.F
      !||    hm_read_mat124                      ../starter/source/materials/mat/mat124/hm_read_mat124.F
      !||    hm_read_mat125                      ../starter/source/materials/mat/mat125/hm_read_mat125.F90
      !||    hm_read_mat127                      ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||    hm_read_mat13                       ../starter/source/materials/mat/mat013/hm_read_mat13.F
      !||    hm_read_mat14                       ../starter/source/materials/mat/mat014/hm_read_mat14.F
      !||    hm_read_mat15                       ../starter/source/materials/mat/mat015/hm_read_mat15.F
      !||    hm_read_mat151                      ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||    hm_read_mat158                      ../starter/source/materials/mat/mat158/hm_read_mat158.F
      !||    hm_read_mat16                       ../starter/source/materials/mat/mat016/hm_read_mat16.F
      !||    hm_read_mat169_arup                 ../starter/source/materials/mat/mat169/hm_read_mat169.F90
      !||    hm_read_mat19                       ../starter/source/materials/mat/mat019/hm_read_mat19.F
      !||    hm_read_mat20                       ../starter/source/materials/mat/mat020/hm_read_mat20.F
      !||    hm_read_mat21                       ../starter/source/materials/mat/mat021/hm_read_mat21.F
      !||    hm_read_mat22                       ../starter/source/materials/mat/mat022/hm_read_mat22.F
      !||    hm_read_mat23                       ../starter/source/materials/mat/mat023/hm_read_mat23.F
      !||    hm_read_mat24                       ../starter/source/materials/mat/mat024/hm_read_mat24.F
      !||    hm_read_mat26                       ../starter/source/materials/mat/mat026/hm_read_mat26.F
      !||    hm_read_mat27                       ../starter/source/materials/mat/mat027/hm_read_mat27.F
      !||    hm_read_mat29_31                    ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat32                       ../starter/source/materials/mat/mat032/hm_read_mat32.F
      !||    hm_read_mat33                       ../starter/source/materials/mat/mat033/hm_read_mat33.F
      !||    hm_read_mat35                       ../starter/source/materials/mat/mat035/hm_read_mat35.F
      !||    hm_read_mat36                       ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat37                       ../starter/source/materials/mat/mat037/hm_read_mat37.F
      !||    hm_read_mat38                       ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat40                       ../starter/source/materials/mat/mat040/hm_read_mat40.F
      !||    hm_read_mat42                       ../starter/source/materials/mat/mat042/hm_read_mat42.F
      !||    hm_read_mat43                       ../starter/source/materials/mat/mat043/hm_read_mat43.F
      !||    hm_read_mat44                       ../starter/source/materials/mat/mat044/hm_read_mat44.F
      !||    hm_read_mat48                       ../starter/source/materials/mat/mat048/hm_read_mat48.F
      !||    hm_read_mat49                       ../starter/source/materials/mat/mat049/hm_read_mat49.F
      !||    hm_read_mat51                       ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_mat52                       ../starter/source/materials/mat/mat052/hm_read_mat52.F
      !||    hm_read_mat54                       ../starter/source/materials/mat/mat054/hm_read_mat54.F
      !||    hm_read_mat57                       ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat58                       ../starter/source/materials/mat/mat058/hm_read_mat58.F
      !||    hm_read_mat59                       ../starter/source/materials/mat/mat059/hm_read_mat59.F
      !||    hm_read_mat60                       ../starter/source/materials/mat/mat060/hm_read_mat60.F
      !||    hm_read_mat62                       ../starter/source/materials/mat/mat062/hm_read_mat62.F
      !||    hm_read_mat63                       ../starter/source/materials/mat/mat063/hm_read_mat63.F
      !||    hm_read_mat65                       ../starter/source/materials/mat/mat065/hm_read_mat65.F
      !||    hm_read_mat66                       ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat69                       ../starter/source/materials/mat/mat069/hm_read_mat69.F
      !||    hm_read_mat70                       ../starter/source/materials/mat/mat070/hm_read_mat70.F
      !||    hm_read_mat71                       ../starter/source/materials/mat/mat071/hm_read_mat71.F
      !||    hm_read_mat72                       ../starter/source/materials/mat/mat072/hm_read_mat72.F
      !||    hm_read_mat73                       ../starter/source/materials/mat/mat073/hm_read_mat73.F
      !||    hm_read_mat74                       ../starter/source/materials/mat/mat074/hm_read_mat74.F
      !||    hm_read_mat75                       ../starter/source/materials/mat/mat075/hm_read_mat75.F
      !||    hm_read_mat76                       ../starter/source/materials/mat/mat076/hm_read_mat76.F
      !||    hm_read_mat77                       ../starter/source/materials/mat/mat077/hm_read_mat77.F
      !||    hm_read_mat78                       ../starter/source/materials/mat/mat078/hm_read_mat78.F
      !||    hm_read_mat79                       ../starter/source/materials/mat/mat079/hm_read_mat79.F
      !||    hm_read_mat80                       ../starter/source/materials/mat/mat080/hm_read_mat80.F
      !||    hm_read_mat81                       ../starter/source/materials/mat/mat081/hm_read_mat81.F90
      !||    hm_read_mat82                       ../starter/source/materials/mat/mat082/hm_read_mat82.F
      !||    hm_read_mat83                       ../starter/source/materials/mat/mat083/hm_read_mat83.F
      !||    hm_read_mat87                       ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||    hm_read_mat88                       ../starter/source/materials/mat/mat088/hm_read_mat88.F
      !||    hm_read_mat90                       ../starter/source/materials/mat/mat090/hm_read_mat90.F
      !||    hm_read_mat93                       ../starter/source/materials/mat/mat093/hm_read_mat93.F
      !||    hm_read_mat97                       ../starter/source/materials/mat/mat097/hm_read_mat97.F
      !||    hm_read_mat_99                      ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_matgas                      ../starter/source/materials/mat/matgas/hm_read_matgas.F
      !||    hm_read_merge                       ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_merge_node                  ../starter/source/elements/reader/hm_read_merge_node.F
      !||    hm_read_monvol_type1                ../starter/source/airbag/hm_read_monvol_type1.F
      !||    hm_read_monvol_type10               ../starter/source/airbag/hm_read_monvol_type10.F
      !||    hm_read_monvol_type11               ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type2                ../starter/source/airbag/hm_read_monvol_type2.F
      !||    hm_read_monvol_type3                ../starter/source/airbag/hm_read_monvol_type3.F
      !||    hm_read_monvol_type4                ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5                ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6                ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7                ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8                ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9                ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_mpc                         ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_nbcs                        ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_node                        ../starter/source/elements/reader/hm_read_node.F
      !||    hm_read_nonlocal                    ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_part                        ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_pblast                      ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_pcyl                        ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_perturb_fail                ../starter/source/general_controls/computation/hm_read_perturb_fail.F
      !||    hm_read_perturb_part_shell          ../starter/source/general_controls/computation/hm_read_perturb_part_shell.F
      !||    hm_read_perturb_part_solid          ../starter/source/general_controls/computation/hm_read_perturb_part_solid.F
      !||    hm_read_pfluid                      ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_pload                       ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_preload                     ../starter/source/loads/general/preload/hm_read_preload.F
      !||    hm_read_preload_axial               ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_read_prop01                      ../starter/source/properties/shell/hm_read_prop01.F
      !||    hm_read_prop02                      ../starter/source/properties/truss/hm_read_prop02.F
      !||    hm_read_prop03                      ../starter/source/properties/beam/hm_read_prop03.F
      !||    hm_read_prop04                      ../starter/source/properties/spring/hm_read_prop04.F
      !||    hm_read_prop06                      ../starter/source/properties/solid/hm_read_prop06.F
      !||    hm_read_prop08                      ../starter/source/properties/spring/hm_read_prop08.F
      !||    hm_read_prop09                      ../starter/source/properties/shell/hm_read_prop09.F
      !||    hm_read_prop10                      ../starter/source/properties/shell/hm_read_prop10.F
      !||    hm_read_prop11                      ../starter/source/properties/shell/hm_read_prop11.F
      !||    hm_read_prop12                      ../starter/source/properties/spring/hm_read_prop12.F
      !||    hm_read_prop13                      ../starter/source/properties/spring/hm_read_prop13.F
      !||    hm_read_prop14                      ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop14f                     ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop15                      ../starter/source/properties/solid/hm_read_prop15.F
      !||    hm_read_prop16                      ../starter/source/properties/shell/hm_read_prop16.F
      !||    hm_read_prop17                      ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop18                      ../starter/source/properties/beam/hm_read_prop18.F
      !||    hm_read_prop19                      ../starter/source/properties/shell/hm_read_prop19.F
      !||    hm_read_prop20                      ../starter/source/properties/thickshell/hm_read_prop20.F
      !||    hm_read_prop22                      ../starter/source/properties/thickshell/hm_read_prop22.F
      !||    hm_read_prop23                      ../starter/source/properties/spring/hm_read_prop23.F
      !||    hm_read_prop25                      ../starter/source/properties/spring/hm_read_prop25.F
      !||    hm_read_prop26                      ../starter/source/properties/spring/hm_read_prop26.F
      !||    hm_read_prop28                      ../starter/source/properties/xelem/hm_read_prop28.F
      !||    hm_read_prop32                      ../starter/source/properties/spring/hm_read_prop32.F
      !||    hm_read_prop33                      ../starter/source/properties/spring/hm_read_prop33.F
      !||    hm_read_prop33_cyl_jnt              ../starter/source/properties/spring/hm_read_prop33_cyl_jnt.F
      !||    hm_read_prop33_fix_jnt              ../starter/source/properties/spring/hm_read_prop33_fix_jnt.F
      !||    hm_read_prop33_free_jnt             ../starter/source/properties/spring/hm_read_prop33_free_jnt.F
      !||    hm_read_prop33_old_jnt              ../starter/source/properties/spring/hm_read_prop33_old_jnt.F
      !||    hm_read_prop33_plan_jnt             ../starter/source/properties/spring/hm_read_prop33_plan_jnt.F
      !||    hm_read_prop33_rev_jnt              ../starter/source/properties/spring/hm_read_prop33_rev_jnt.F
      !||    hm_read_prop33_sph_jnt              ../starter/source/properties/spring/hm_read_prop33_sph_jnt.F
      !||    hm_read_prop33_trans_jnt            ../starter/source/properties/spring/hm_read_prop33_trans_jnt.F
      !||    hm_read_prop33_univ_jnt             ../starter/source/properties/spring/hm_read_prop33_univ_jnt.F
      !||    hm_read_prop34                      ../starter/source/properties/sph/hm_read_prop34.F
      !||    hm_read_prop36                      ../starter/source/properties/spring/hm_read_prop36.F
      !||    hm_read_prop44                      ../starter/source/properties/spring/hm_read_prop44.F
      !||    hm_read_prop45                      ../starter/source/properties/spring/hm_read_prop45.F
      !||    hm_read_prop51                      ../starter/source/properties/shell/hm_read_prop51.F
      !||    hm_read_prop_generic                ../starter/source/properties/hm_read_prop_generic.F
      !||    hm_read_prop_user                   ../starter/source/properties/user_spring_solid/hm_read_prop_user.F
      !||    hm_read_properties                  ../starter/source/properties/hm_read_properties.F
      !||    hm_read_quad                        ../starter/source/elements/reader/hm_read_quad.F
      !||    hm_read_radiation                   ../starter/source/loads/thermic/hm_read_radiation.F
      !||    hm_read_rand                        ../starter/source/general_controls/computation/hm_read_rand.F
      !||    hm_read_rbe2                        ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3                        ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                       ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul                ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_refsta                      ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    hm_read_retractor                   ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_rivet                       ../starter/source/elements/reader/hm_read_rivet.F
      !||    hm_read_rwall_cyl                   ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul                ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral                 ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane                 ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher                 ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm                 ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_sensors                     ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_sh3n                        ../starter/source/elements/reader/hm_read_sh3n.F
      !||    hm_read_shell                       ../starter/source/elements/reader/hm_read_shell.F
      !||    hm_read_skw                         ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_slipring                    ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_solid                       ../starter/source/elements/reader/hm_read_solid.F
      !||    hm_read_spcnd                       ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    hm_read_sphcel                      ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_sphio                       ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_read_spring                      ../starter/source/elements/reader/hm_read_spring.F
      !||    hm_read_stack                       ../starter/source/stack/hm_read_stack.F
      !||    hm_read_submodel                    ../starter/source/model/assembling/hm_read_submodel.F
      !||    hm_read_subset                      ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf                        ../starter/source/groups/hm_read_surf.F
      !||    hm_read_surfsurf                    ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_table1_0                    ../starter/source/tools/curve/hm_read_table1_0.F
      !||    hm_read_table1_1                    ../starter/source/tools/curve/hm_read_table1_1.F
      !||    hm_read_table2_0                    ../starter/source/tools/curve/hm_read_table2_0.F
      !||    hm_read_table2_1                    ../starter/source/tools/curve/hm_read_table2_1.F
      !||    hm_read_therm                       ../starter/source/materials/therm/hm_read_therm.F
      !||    hm_read_therm_stress                ../starter/source/materials/therm/hm_read_therm_stress.F90
      !||    hm_read_thgrki                      ../starter/source/output/th/hm_read_thgrki.F
      !||    hm_read_thgrki_rbody                ../starter/source/output/th/hm_read_thgrki_rbody.F
      !||    hm_read_thgrne                      ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrns                      ../starter/source/output/th/hm_read_thgrns.F
      !||    hm_read_thgrou                      ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrpa                      ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrpa_sub                  ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrsens                    ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf                    ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_thpart                      ../starter/source/output/thpart/hm_read_thpart.F
      !||    hm_read_tria                        ../starter/source/elements/reader/hm_read_tria.F
      !||    hm_read_truss                       ../starter/source/elements/reader/hm_read_truss.F
      !||    hm_read_unit                        ../starter/source/general_controls/computation/hm_read_unit.F
      !||    hm_read_upwind                      ../starter/source/general_controls/computation/hm_read_upwind.F
      !||    hm_read_visc                        ../starter/source/materials/visc/hm_read_visc.F
      !||    hm_read_visc_lprony                 ../starter/source/materials/visc/hm_read_visc_lprony.F
      !||    hm_read_visc_prony                  ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    hm_read_window_user                 ../starter/source/tools/userwi/hm_read_window_user.F
      !||    hm_read_xelem                       ../starter/source/elements/reader/hm_read_xelem.F
      !||    hm_read_xref                        ../starter/source/loads/reference_state/xref/hm_read_xref.F
      !||    hm_set                              ../starter/source/model/sets/hm_set.F
      !||    hm_submodgre                        ../starter/source/groups/hm_submodgr.F
      !||    hm_submodgrn                        ../starter/source/groups/hm_submodgr.F
      !||    hm_submodpart                       ../starter/source/groups/hm_submodpart.F
      !||    hm_surfgr2                          ../starter/source/groups/hm_surfgr2.F
      !||    hm_surfnod                          ../starter/source/groups/hm_surfnod.F
      !||    hm_tagpart                          ../starter/source/groups/hm_tagpart.F
      !||    hm_tagpart2                         ../starter/source/groups/hm_tagpart2.F
      !||    hm_thgrki_vent                      ../starter/source/output/th/hm_thgrki_vent.F
      !||    hm_thvarc                           ../starter/source/output/th/hm_read_thvarc.F
      !||    hm_yctrl                            ../starter/source/elements/initia/hm_yctrl.F
      !||    i11buc1                             ../starter/source/interfaces/inter3d1/i11buc1.F
      !||    i11buc_vox1                         ../starter/source/interfaces/inter3d1/i11buc1.F
      !||    i11sti3                             ../starter/source/interfaces/inter3d1/i11sti3.F
      !||    i11tri                              ../starter/source/interfaces/inter3d1/i11tri.F
      !||    i12chk3                             ../starter/source/interfaces/inter3d1/i12chk3.F
      !||    i12sol3                             ../starter/source/interfaces/inter3d1/insol3.F
      !||    i12tid3                             ../starter/source/interfaces/inter3d1/i12tid3.F
      !||    i1bcs_check                         ../starter/source/interfaces/int01/i1bcs_check.F90
      !||    i1chk3                              ../starter/source/interfaces/inter3d1/i1chk3.F
      !||    i1tid2                              ../starter/source/interfaces/inter2d1/i1tid2.F
      !||    i1tid3                              ../starter/source/interfaces/inter3d1/i1tid3.F
      !||    i20bord                             ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i20edge1                            ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i20pwr3                             ../starter/source/interfaces/inter3d1/i20pwr3.F
      !||    i20pwr3a                            ../starter/source/interfaces/inter3d1/i20pwr3.F
      !||    i20sti3                             ../starter/source/interfaces/inter3d1/i20sti3.F
      !||    i20sti3e                            ../starter/source/interfaces/inter3d1/i20sti3.F
      !||    i21buc1                             ../starter/source/interfaces/inter3d1/i21buc3.F
      !||    i21els3                             ../starter/source/interfaces/inter3d1/i21els3.F
      !||    i21gap3                             ../starter/source/interfaces/inter3d1/i21gap3.F
      !||    i21pwr3                             ../starter/source/interfaces/inter3d1/i21pwr3.F
      !||    i21tri                              ../starter/source/interfaces/inter3d1/i21tri.F
      !||    i22err3                             ../starter/source/interfaces/inter3d1/i22err3.F
      !||    i23buc1                             ../starter/source/interfaces/inter3d1/i23buc3.F
      !||    i23gap3                             ../starter/source/interfaces/inter3d1/i23gap3.F
      !||    i23tri                              ../starter/source/interfaces/inter3d1/i23tri.F
      !||    i24buc1                             ../starter/source/interfaces/inter3d1/i24buc1.F
      !||    i24edge1                            ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i24edge2                            ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i24gapm                             ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i24pen3                             ../starter/source/interfaces/inter3d1/i24pen3.F
      !||    i24pwr3                             ../starter/source/interfaces/inter3d1/i24pwr3.F
      !||    i24setnodes                         ../starter/source/interfaces/inter3d1/i24setnodes.F
      !||    i24tri                              ../starter/source/interfaces/inter3d1/i24tri.F
      !||    i25buc_vox1                         ../starter/source/interfaces/inter3d1/i25buc_vox1.F
      !||    i25gapm                             ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25neigh_msg_err                    ../starter/source/interfaces/inter3d1/i25neigh.F
      !||    i25pwr3                             ../starter/source/interfaces/inter3d1/i25pwr3.F
      !||    i25pwr3_e2s                         ../starter/source/interfaces/inter3d1/i25pwr3_e2s.F
      !||    i25pwr3e                            ../starter/source/interfaces/inter3d1/i25pwr3e.F
      !||    i25sors                             ../starter/source/interfaces/inter3d1/i25sors.F
      !||    i25sti3                             ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25surfi                            ../starter/source/interfaces/inter3d1/i25surfi.F
      !||    i2_surfi                            ../starter/source/interfaces/inter3d1/i2_surfi.F90
      !||    i2buc1                              ../starter/source/interfaces/inter3d1/i2buc1.F
      !||    i2chk3                              ../starter/source/interfaces/inter3d1/i2chk3.F
      !||    i2surfs                             ../starter/source/interfaces/inter3d1/i2surfs.F
      !||    i2tid3                              ../starter/source/interfaces/inter3d1/i2tid3.F
      !||    i2tri                               ../starter/source/interfaces/inter3d1/i2tri.F
      !||    i3pen2                              ../starter/source/interfaces/inter2d1/i3pen2.F
      !||    i3pen3                              ../starter/source/interfaces/inter3d1/i3pen3.F
      !||    i3sti2                              ../starter/source/interfaces/inter2d1/i3sti2.F
      !||    i3sti3                              ../starter/source/interfaces/inter3d1/i3sti3.F
      !||    i6pen3                              ../starter/source/interfaces/inter3d1/i6pen3.F
      !||    i6sti3                              ../starter/source/interfaces/inter3d1/i6sti3.F
      !||    i7buc1                              ../starter/source/interfaces/inter3d1/i7buc1.F
      !||    i7buc_vox1                          ../starter/source/interfaces/inter3d1/i7buc_vox1.F
      !||    i7err3                              ../starter/source/interfaces/inter3d1/i7err3.F
      !||    i7pwr3                              ../starter/source/interfaces/inter3d1/i7pwr3.F
      !||    i7sti3                              ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    i7tri                               ../starter/source/interfaces/inter3d1/i7tri.F
      !||    i9sti3                              ../starter/source/interfaces/int09/i9sti3.F
      !||    iedge_xfem                          ../starter/source/elements/xfem/iedge_xfem.F
      !||    ig3donederiv                        ../starter/source/elements/ige3d/ig3donederiv.F
      !||    ingrbric_dx                         ../starter/source/interfaces/interf1/ingrbric_dx.F
      !||    ini_bcscyc                          ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    ini_diff                            ../starter/source/tools/lagmul/ini_diff.F
      !||    ini_fxbody                          ../starter/source/constraints/fxbody/ini_fxbody.F
      !||    ini_gear                            ../starter/source/tools/lagmul/ini_gear.F
      !||    ini_inimap1d                        ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    ini_rack                            ../starter/source/tools/lagmul/ini_rack.F
      !||    ini_seatbelt                        ../starter/source/tools/seatbelts/ini_seatbelt.F
      !||    inibcs_cy                           ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    iniboltprel                         ../starter/source/loads/bolt/iniboltprel.F
      !||    iniebcs                             ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||    iniebcs_propergol_get_cv            ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||    iniend                              ../starter/source/interfaces/inter3d1/iniend.F
      !||    inigrav_load                        ../starter/source/initial_conditions/inigrav/inigrav_load.F
      !||    iniguser                            ../starter/source/system/iniguser.F
      !||    inint2                              ../starter/source/interfaces/inter2d1/inint2.F
      !||    inint3                              ../starter/source/interfaces/inter3d1/inint3.F
      !||    inint3_thkvar                       ../starter/source/interfaces/inter3d1/inint3_thkvar.F
      !||    inintr2                             ../starter/source/interfaces/inter3d1/inintr2.F
      !||    inintr_thkvar                       ../starter/source/interfaces/interf1/inintr_thkvar.F
      !||    inintsub_11                         ../starter/source/output/subinterface/inintsub_11.F
      !||    inintsub_25                         ../starter/source/output/subinterface/inintsub_25.F
      !||    inintsub_7                          ../starter/source/output/subinterface/inintsub_7.F
      !||    inirbe2                             ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    inirbe3                             ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    inirby                              ../starter/source/constraints/general/rbody/inirby.F
      !||    inirbys                             ../starter/source/constraints/general/rbody/inirby.F
      !||    inisen                              ../starter/source/tools/sensor/inisen.F
      !||    inisrf                              ../starter/source/constraints/general/rbody/inisrf.F
      !||    init_monvol                         ../starter/source/airbag/init_monvol.F
      !||    init_seatbelt_rbodies               ../starter/source/tools/seatbelts/init_seatbelt_rbodies.F90
      !||    init_skew45                         ../starter/source/elements/joint/rjoint/rini45.F
      !||    initag_preload_a                    ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    initia                              ../starter/source/elements/initia/initia.F
      !||    inivoid                             ../starter/source/elements/initia/inivoid.F
      !||    inrch2                              ../starter/source/interfaces/inter2d1/inrch2.F
      !||    insol3d                             ../starter/source/interfaces/inter3d1/insol3.F
      !||    insolt10                            ../starter/source/interfaces/inter3d1/insolt10.F
      !||    inspcnd                             ../starter/source/elements/sph/inspcnd.F
      !||    insurf                              ../starter/source/interfaces/interf1/insurf.F
      !||    int2cy_chk                          ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    int2modif_nd                        ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    inter1_check_ale_lag_sides          ../starter/source/interfaces/int01/inter1_check_ale_lag_sides.F90
      !||    inter_dcod_friction                 ../starter/source/interfaces/reader/inter_dcod_friction.F
      !||    inter_dcod_function                 ../starter/source/interfaces/reader/inter_dcod_function.F
      !||    inter_dcod_sensor                   ../starter/source/interfaces/reader/inter_dcod_sensor.F
      !||    invoi3                              ../starter/source/interfaces/inter3d1/invoi3.F
      !||    itagsl2                             ../starter/source/interfaces/inter3d1/itagsl2.F
      !||    kinchk                              ../starter/source/constraints/general/kinchk.F
      !||    kinset                              ../starter/source/constraints/general/kinset.F
      !||    lagm_ini                            ../starter/source/tools/lagmul/lagm_ini.F
      !||    law108_upd                          ../starter/source/materials/mat/mat108/law108_upd.F
      !||    law111_upd                          ../starter/source/materials/mat/mat111/law111_upd.F
      !||    law112_upd                          ../starter/source/materials/mat/mat112/law112_upd.F
      !||    law113_upd                          ../starter/source/materials/mat/mat113/law113_upd.F
      !||    law114_upd                          ../starter/source/materials/mat/mat114/law114_upd.F
      !||    law119_upd                          ../starter/source/materials/mat/mat119/law119_upd.F
      !||    law158_upd                          ../starter/source/materials/mat/mat158/law158_upd.F
      !||    law19_upd                           ../starter/source/materials/mat/mat019/law19_upd.F90
      !||    law36_upd                           ../starter/source/materials/mat/mat036/law36_upd.F
      !||    law58_upd                           ../starter/source/materials/mat/mat058/law58_upd.F
      !||    law69_nlsqf                         ../starter/source/materials/tools/nlsqf.F
      !||    law69_nlsqf_auto                    ../starter/source/materials/tools/law69_nlsqf_auto.F
      !||    law69_upd                           ../starter/source/materials/mat/mat069/law69_upd.F
      !||    law70_upd                           ../starter/source/materials/mat/mat070/law70_upd.F
      !||    law76_upd                           ../starter/source/materials/mat/mat076/law76_upd.F
      !||    law77_upd                           ../starter/source/materials/mat/mat077/law77_upd.F
      !||    law87_upd                           ../starter/source/materials/mat/mat087/law87_upd.F90
      !||    law88_upd                           ../starter/source/materials/mat/mat088/law88_upd.F
      !||    law90_upd                           ../starter/source/materials/mat/mat090/law90_upd.F
      !||    law92_nlsqf                         ../starter/source/materials/mat/mat092/law92_nlsqf.F
      !||    lcgeo19                             ../starter/source/elements/shell/coque/lcgeo19.F
      !||    lec_dof_jnt                         ../starter/source/properties/spring/hm_read_prop45.F
      !||    lec_inimap1d_file                   ../starter/source/initial_conditions/inimap/lec_inimap1d_file.F
      !||    lec_inimap2d_file                   ../starter/source/initial_conditions/inimap/lec_inimap2d_file.F
      !||    lec_inistate_d00_brick_check        ../starter/source/elements/initia/lec_inistate_d00_brick-check.F
      !||    lec_inistate_tri                    ../starter/source/elements/initia/lec_inistate_tri.F
      !||    lecacc                              ../starter/source/tools/accele/lecacc.F
      !||    lecextlnk                           ../starter/source/coupling/rad2rad/lecextlnk.F
      !||    lecfill                             ../starter/source/elements/initia/lecfill.F
      !||    lecig3d                             ../starter/source/elements/ige3d/lecig3d.F
      !||    lecins                              ../starter/source/interfaces/interf1/lecins.F
      !||    lecint                              ../starter/source/interfaces/interf1/lecint.F
      !||    lecm51__check_initial_state         ../starter/source/materials/mat/mat051/lecm51__check_initial_state.F
      !||    lecrefsta                           ../starter/source/loads/reference_state/refsta/lecrefsta.F
      !||    lecsec42                            ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                         ../starter/source/tools/sect/lecsec4bolt.F
      !||    lecstack_ply                        ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lecstamp                            ../starter/source/interfaces/interf1/lecstamp.F
      !||    lecsubmod                           ../starter/source/model/submodel/lecsubmod.F
      !||    lectrans                            ../starter/source/model/transformation/lectrans.F
      !||    lectranssub                         ../starter/source/model/submodel/lectranssub.F
      !||    lectur                              ../starter/source/starter/lectur.F
      !||    lgmini_bc                           ../starter/source/tools/lagmul/lgmini_bc.F
      !||    lgmini_fxv                          ../starter/source/tools/lagmul/lgmini_fxv.F
      !||    lgmini_gj                           ../starter/source/tools/lagmul/lgmini_gj.F
      !||    lgmini_i2                           ../starter/source/tools/lagmul/lgmini_i2.F
      !||    lgmini_i7                           ../starter/source/tools/lagmul/lgmini_i7.F
      !||    lgmini_mpc                          ../starter/source/tools/lagmul/lgmini_mpc.F
      !||    lgmini_rby                          ../starter/source/tools/lagmul/lgmini_rby.F
      !||    lgmini_rwl                          ../starter/source/tools/lagmul/lgmini_rwl.F
      !||    lm_least_square_prony               ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    lm_least_square_prony_2             ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    lslocal                             ../starter/source/elements/xfem/lslocal.F
      !||    m20dcod                             ../starter/source/system/fsdcod.F
      !||    m75init                             ../starter/source/materials/mat/mat075/m75init.F
      !||    mass_fluid_qd                       ../starter/source/fluid/mass-fluid_qd.F
      !||    mass_fluid_tg                       ../starter/source/fluid/mass-fluid_tg.F
      !||    mat11check                          ../starter/source/materials/mat/mat011/mat11check.F
      !||    matfun_usr2sys                      ../starter/source/materials/tools/matfun_usr2sys.F
      !||    matini                              ../starter/source/materials/mat_share/matini.F
      !||    mattab_usr2sys                      ../starter/source/materials/tools/mattab_usr2sys.F
      !||    merge_node                          ../starter/source/elements/nodes/merge_node.F
      !||    monvol_check_delete_duplicated      ../starter/source/airbag/monvol_check_delete_duplicated.F
      !||    monvol_check_surfclose              ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_check_venthole_surf          ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_orient_surf                  ../starter/share/modules1/monvol_struct_mod.F
      !||    msg_err                             ../starter/source/interfaces/inter3d1/i24tools.F
      !||    multi_check_eos                     ../starter/source/multifluid/multi_check_eos.F
      !||    multi_check_psh                     ../starter/source/multifluid/multi_check_psh.F
      !||    multifluid_init2                    ../starter/source/multifluid/multifluid_init2.F
      !||    multifluid_init2t                   ../starter/source/multifluid/multifluid_init2t.F
      !||    multifluid_init3                    ../starter/source/multifluid/multifluid_init3.F
      !||    multifluid_init3t                   ../starter/source/multifluid/multifluid_init3t.F
      !||    mverif                              ../starter/source/system/machine.F
      !||    nbadigemesh                         ../starter/source/elements/ige3d/nbadigemesh.F
      !||    nbadmesh                            ../starter/source/model/remesh/nbadmesh.F
      !||    nboxlst                             ../starter/source/model/box/nboxlist.F
      !||    new_seatbelt                        ../starter/source/tools/seatbelts/new_seatbelt.F
      !||    newdbl2                             ../starter/source/system/sysfus.F
      !||    ngr2usr                             ../starter/source/system/nintrr.F
      !||    nintlst                             ../starter/source/system/nintrr.F
      !||    nintlst2                            ../starter/source/system/nintrr.F
      !||    nintlstn                            ../starter/source/system/nintrr.F
      !||    nintrn                              ../starter/source/system/nintrn.F
      !||    nloc_dmg_init                       ../starter/source/materials/fail/nloc_dmg_init.F
      !||    nlocal_init_sta                     ../starter/source/materials/fail/nlocal_init_sta.F
      !||    nodgr_r2r                           ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    nodgrnr5                            ../starter/source/starter/freform.F
      !||    nodgrnr6                            ../starter/source/starter/freform.F
      !||    nrf51ini                            ../starter/source/materials/mat/mat051/nrf51ini.F
      !||    orthdir_proj                        ../starter/source/interfaces/interf1/inintr_orthdirfric.F
      !||    pcoori                              ../starter/source/elements/beam/pcoori.F
      !||    pgrtails                            ../starter/source/elements/beam/pgrtails.F
      !||    pinit3                              ../starter/source/elements/beam/pinit3.F
      !||    pornod                              ../starter/source/ale/pornod.F
      !||    pre_stackgroup                      ../starter/source/stack/pres_stackgroup.F
      !||    prelecsec                           ../starter/source/tools/sect/prelecsec.F
      !||    prepare_int25                       ../starter/source/model/mesh/build_cnel.F
      !||    printgroup                          ../starter/source/output/outfile/printgroup.F
      !||    q4deri2                             ../starter/source/elements/solid_2d/quad4/q4deri2.F
      !||    q4init2                             ../starter/source/elements/solid_2d/quad4/q4init2.F
      !||    q4voli2                             ../starter/source/elements/solid_2d/quad4/q4voli2.F
      !||    qgrhead                             ../starter/source/elements/solid_2d/quad/qgrhead.F
      !||    qgrtails                            ../starter/source/elements/solid_2d/quad/qgrtails.F
      !||    qinit2                              ../starter/source/elements/solid_2d/quad/qinit2.F
      !||    qmorth2                             ../starter/source/elements/solid_2d/quad/qmorth2.F
      !||    qvoli2                              ../starter/source/elements/solid_2d/quad/qvoli2.F
      !||    r2r_check                           ../starter/source/coupling/rad2rad/r2r_check.F
      !||    r2r_domdec                          ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    r2r_exist                           ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_fork                            ../starter/source/coupling/rad2rad/r2r_fork.F
      !||    r2r_group                           ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_prelec                          ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec_name                     ../starter/source/coupling/rad2rad/r2r_prelec_name.F
      !||    r2r_speedup                         ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    r2r_split                           ../starter/source/coupling/rad2rad/r2r_split.F
      !||    r2r_sys2                            ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_void                            ../starter/source/coupling/rad2rad/r2r_void.F
      !||    r3buf3                              ../starter/source/elements/spring/r3buf3.F
      !||    r4buf3                              ../starter/source/elements/spring/r4buf3.F
      !||    rbe2modif_nd                        ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    rcheckmass                          ../starter/source/elements/spring/rcheckmass.F
      !||    read_box_box                        ../starter/source/model/box/read_box_box.F
      !||    read_box_cyl                        ../starter/source/model/box/read_box_cyl.F
      !||    read_box_rect                       ../starter/source/model/box/read_box_rect.F
      !||    read_box_spher                      ../starter/source/model/box/read_box_spher.F
      !||    read_detonators                     ../starter/source/initial_conditions/detonation/read_detonators.F
      !||    read_dfs_detcord                    ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline                    ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan                    ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint                   ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper                ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_ebcs                           ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    read_impdisp                        ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impvel                         ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_fgeo                    ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_impvel_lagmul                  ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_mat25_crasurv                  ../starter/source/materials/mat/mat025/read_mat25_crasurv.F90
      !||    read_mat25_tsaiwu                   ../starter/source/materials/mat/mat025/read_mat25_tsaiwu.F90
      !||    read_monvol                         ../starter/source/airbag/read_monvol.F
      !||    read_msgfile                        ../starter/source/output/message/read_msgfile.F
      !||    read_pch_file                       ../starter/source/constraints/fxbody/read_pch_file.F
      !||    read_sensor_acc                     ../starter/source/tools/sensor/read_sensor_acc.F
      !||    read_sensor_contact                 ../starter/source/tools/sensor/read_sensor_contact.F
      !||    read_sensor_gauge                   ../starter/source/tools/sensor/read_sensor_gauge.F
      !||    read_sensor_hic                     ../starter/source/tools/sensor/read_sensor_hic.F
      !||    read_sensor_python                  ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||    read_sensor_rbody                   ../starter/source/tools/sensor/read_sensor_rbody.F
      !||    read_sensor_rwall                   ../starter/source/tools/sensor/read_sensor_rwall.F
      !||    read_sensor_sect                    ../starter/source/tools/sensor/read_sensor_sect.F
      !||    read_sensor_user                    ../starter/source/tools/sensor/read_sensor_user.F
      !||    realloc_iddconnect                  ../starter/source/spmd/node/ddtools.F
      !||    realloc_ifront                      ../starter/source/spmd/node/ddtools.F
      !||    remn_i2op                           ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op_edg25                     ../starter/source/interfaces/int25/i25remlin.F
      !||    remn_self24                         ../starter/source/interfaces/inter3d1/remn_self24.F
      !||    reset_u_geo                         ../starter/source/user_interface/uaccess.F
      !||    rigid_mat                           ../starter/source/materials/mat/mat019/rigid_mat.F
      !||    rigmodif_nd                         ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    rini32                              ../starter/source/properties/spring/hm_read_prop32.F
      !||    rini33_rb                           ../starter/source/elements/joint/rjoint/rini33_rb.F
      !||    rini45                              ../starter/source/elements/joint/rjoint/rini45.F
      !||    rini45_rb                           ../starter/source/elements/joint/rjoint/rini45_rb.F
      !||    rinit3                              ../starter/source/elements/spring/rinit3.F
      !||    rivet0                              ../starter/source/elements/reader/rivet0.F
      !||    rkini3                              ../starter/source/elements/spring/rkini3.F
      !||    rm_cand24                           ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    s10deri3                            ../starter/source/elements/solid/solide10/s10deri3.F
      !||    s10edg_rlink                        ../starter/source/elements/solid/solide10/s10edg_rlink.F
      !||    s10init3                            ../starter/source/elements/solid/solide10/s10init3.F
      !||    s16deri3                            ../starter/source/elements/thickshell/solide16/s16deri3.F
      !||    s16init3                            ../starter/source/elements/thickshell/solide16/s16init3.F
      !||    s20deri3                            ../starter/source/elements/solid/solide20/s20deri3.F
      !||    s20init3                            ../starter/source/elements/solid/solide20/s20init3.F
      !||    s4deri3                             ../starter/source/elements/solid/solide4/s4deri3.F
      !||    s4init3                             ../starter/source/elements/solid/solide4/s4init3.F
      !||    s4refsta3                           ../starter/source/elements/solid/solide4/s4refsta3.F
      !||    s6cderi3                            ../starter/source/elements/thickshell/solide6c/s6cderi3.F
      !||    s6cinit3                            ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    s8cinit3                            ../starter/source/elements/thickshell/solide8c/s8cinit3.F
      !||    s8ederi3                            ../starter/source/elements/solid/solide8z/s8zderi3.F
      !||    s8zderi3                            ../starter/source/elements/solid/solide8z/s8zderi3.F
      !||    s8zderic3                           ../starter/source/elements/solid/solide8z/s8zderi3.F
      !||    s8zinit3                            ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    sboxboxsurf                         ../starter/source/model/box/bigbox.F
      !||    scaleini                            ../starter/source/elements/initia/scaleini.F
      !||    scderi3                             ../starter/source/elements/thickshell/solidec/scderi3.F
      !||    scinit3                             ../starter/source/elements/thickshell/solidec/scinit3.F
      !||    scmorth3                            ../starter/source/elements/thickshell/solidec/scmorth3.F
      !||    sderi3                              ../starter/source/elements/solid/solide/sderi3.F
      !||    sderi3b                             ../starter/source/elements/solid/solid8p/sderi3b.F
      !||    set_admesh                          ../starter/source/model/remesh/set_admesh.F
      !||    set_u_geo                           ../starter/source/user_interface/uaccess.F
      !||    set_u_pnu                           ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_fpar                     ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_ipar                     ../starter/source/user_interface/uaccess.F
      !||    sgrhead                             ../starter/source/elements/solid/solide/sgrhead.F
      !||    sgrtails                            ../starter/source/elements/solid/solide/sgrtails.F
      !||    sinit3                              ../starter/source/elements/solid/solide/sinit3.F
      !||    smorth3                             ../starter/source/elements/solid/solide/smorth3.F
      !||    sms_ini_jad_3                       ../starter/source/ams/sms_init.F
      !||    sms_init                            ../starter/source/ams/sms_init.F
      !||    soltosphv4                          ../starter/source/elements/sph/soltosph.F
      !||    soltosphv8                          ../starter/source/elements/sph/soltosph.F
      !||    sort_logical_sensors                ../starter/source/tools/sensor/sort_logical_sensors.F
      !||    spgrtails                           ../starter/source/elements/sph/spgrtails.F
      !||    sphdcod                             ../starter/source/elements/sph/sphdcod.F
      !||    split_cand_i20                      ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i24                      ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i25                      ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i7                       ../starter/source/restart/ddsplit/inter_tools.F
      !||    sptri                               ../starter/source/elements/sph/sptri.F
      !||    srefderi3                           ../starter/source/elements/solid/solide/srefsta3.F
      !||    stackgroup                          ../starter/source/stack/stackgroup.F
      !||    starter0                            ../starter/source/starter/starter0.F
      !||    state_admesh                        ../starter/source/model/remesh/state_admesh.F
      !||    suinit3                             ../starter/source/elements/elbuf_init/suinit3.F
      !||    surfmas                             ../starter/source/tools/admas/surfmas.F
      !||    szderi3                             ../starter/source/elements/solid/solidez/szderi3.F
      !||    szrefderi3                          ../starter/source/elements/solid/solide/srefsta3.F
      !||    t3grhead                            ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                           ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    tagnod_r2r                          ../starter/source/coupling/rad2rad/tagnod_r2r.F
      !||    tagnods_r2r                         ../starter/source/coupling/rad2rad/tagnod_r2r.F
      !||    tinit3                              ../starter/source/elements/truss/tinit3.F
      !||    trace_print                         ../starter/source/system/trace_back.F
      !||    trirbmerge                          ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    udoubl2                             ../starter/source/system/sysfus.F
      !||    udoubl2_igr                         ../starter/source/system/sysfus.F
      !||    udoubl2_set                         ../starter/source/system/sysfus.F
      !||    udoubl2_wo_title                    ../starter/source/system/sysfus.F
      !||    udoubl3                             ../starter/source/system/sysfus.F
      !||    ulist2s                             ../starter/source/system/sysfus.F
      !||    unit_code                           ../starter/source/general_controls/computation/unit_code.F
      !||    updfail                             ../starter/source/materials/updfail.F90
      !||    updmat                              ../starter/source/materials/updmat.F
      !||    usr2sys                             ../starter/source/system/sysfus.F
      !||    usr2sys2                            ../starter/source/system/sysfus.F
      !||    vdoubl2                             ../starter/source/system/sysfus.F
      !||    w_isph                              ../starter/source/restart/ddsplit/w_isph.F
      !||    w_rsph                              ../starter/source/restart/ddsplit/w_rsph.F
      !||    xgrhead                             ../starter/source/elements/xelem/xgrhead.F
      !||    xgrtails                            ../starter/source/elements/xelem/xgrtails.F
      !||    xini28                              ../starter/source/elements/xelem/xini28.F
      !||    xini30                              ../starter/source/elements/xelem/xini30.F
      !||    xini31                              ../starter/source/elements/xelem/xini31.F
      !||    xinit3                              ../starter/source/elements/xelem/xinit3.F
      !||    yctrl                               ../starter/source/initial_conditions/inista/yctrl.F
      !||--- calls      -----------------------------------------------------
      !||    arret                               ../starter/source/system/arret.F
      !||    mess_extract_format                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod2                        ../starter/share/message_module/message_mod2.F
      !||====================================================================
      SUBROUTINE ANCMSG(MSGID ,MSGTYPE,ANMODE,
     .                  I1    ,I2    ,I3    ,I4    ,I5    ,
     .                  I6    ,I7    ,I8    ,I9    ,I10   ,
     .                  I11   ,I12   ,I13   ,I14   ,I15   ,
     .                  I16   ,I17   ,I18   ,I19   ,I20   ,
     .                  R1    ,R2    ,R3    ,R4    ,
     .                  R5    ,R6    ,R7    ,R8    ,R9   ,
     .                  C1    ,C2    ,C3    ,C4    ,
     .                  C5    ,C6    ,C7    ,C8    ,C9    ,
     .                  PRMODE )
c     .                  PRMODE,IOPTION, IOPTION_TYPE) 
      USE MESSAGE_MOD2
C-----------------------------------------------
C Usage sample :
C      USE MESSAGE_MOD
C ...
C      CALL ANCMSG(MSGID=9999,ANMODE=ANINFO_BLIND,
C     .               I1=28,C1='TIME STEP COMPUTATION',C2='SHELL')
C ...         ...         ...         ...         ...
C  ANMODE=ANINFO/ANINFO_BLIND
C         ANINFO write both title and description in standard output and listing
C         ANINFO_BLIND do not write description in standard output
C ...         ...         ...         ...         ...
C  It is recommended to use ANINFO for sensitive messages.
C ...         ...         ...         ...         ...
C  PRMODE=MSG_CUMU/MSG_PRINT
C         MSG_CUMU cumul information in a scratch file
C         MSG_PRINT print compact message from the scratch file
C ...         ...         ...         ...         ...
C  starter_message_description.txt extract :
C ...
C  /MESSAGE/9999/TITLE
C  \n ** ERROR DURING %s\n
C
C  /MESSAGE/9999/DESCRIPTION
C        DURING COMPUTATION IT APPEARS THAT
C        %s ELEMENT ID=%d HAD A TIME STEP EQUAL TO ZERO
C        ENGINE WILL STOP
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSGID,ANMODE,MSGTYPE,PRMODE
c      INTEGER MSGID,ANMODE,MSGTYPE,PRMODE,IOPTION,IOPTION_TYPE
      INTEGER
     .                 I1,I2,I3,I4,I5,
     .                 I6,I7,I8,I9,I10,
     .                 I11,I12,I13,I14,I15,
     .                 I16,I17,I18,I19,I20
      my_real
     .                 R1,R2,R3,R4,
     .                 R5,R6,R7,R8,R9
      CHARACTER(*)
     .                 C1,C2,C3,C4,
     .                 C5,C6,C7,C8,C9
      OPTIONAL ::
     .                 I1,I2,I3,I4,I5,
     .                 I6,I7,I8,I9,I10,
     .                 I11,I12,I13,I14,I15,
     .                 I16,I17,I18,I19,I20,
     .                 R1,R2,R3,R4,
     .                 R5,R6,R7,R8,R9,
     .                 C1,C2,C3,C4,
     .                 C5,C6,C7,C8,C9,
     .                 PRMODE
c     .                 PRMODE,IOPTION,IOPTION_TYPE
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER             IBUF(20)
      my_real             RBUF(10)
      CHARACTER(LEN=NCHARLINE) CBUF(10)
C
      CHARACTER(LEN=NCHARLINE):: TMPLINE,MYFMT,TMPBUF
      CHARACTER(LEN=NCHARLINE):: TMPOUT,TMPIN,TMPIN2,TMPIN3
      CHARACTER*20 CMSGTYPE
      CHARACTER*15 CTYPE
      INTEGER ITYPE,ILINE,I,I0,J,J0,SBUFMSG(2),IFILE,IOLD,
     *        INDXI,INDXR,INDXC,INDXD,INDXTMPOUT,BUFLEN,
     *        STMP,IEXIST,IEND,MSGID_R,CPT,NBREPET,NBREPET_0,
     *        INT_TMP,ITMP,INDXI_INC,INDXR_INC,INDXC_INC,INDXD_INC,
     *        NB_IBUF,NB_RBUF,NB_CBUF,MODE, WORK(70000),K,
     *        NB_MESSAGES,ITAG,NO_PRINT,ISAV
      my_real REAL_TMP
      INTEGER NCOUNT,ID_NUM,KEY_LEN
      CHARACTER(LEN=NCHARLINE) KEY, BUFMSG(2,100),BUFFMT(2,100),CHAR_TMP
      CHARACTER*2304 OPTION_NAME_DYNA,DYNA_TITLE
      INTEGER S_OPTION_NAME_DYNA,SDYNA_TITLE,DYNA_ID
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_INTEGER
      my_real, DIMENSION(:), ALLOCATABLE :: TMP_REAL
      CHARACTER(LEN=NCHARLINE), DIMENSION(:), ALLOCATABLE :: TMP_CHAR
      CHARACTER(LEN=NCHARLINE), DIMENSION(:), ALLOCATABLE :: TMP_I
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,MES_DOUBLE
C-----------------------------------------------
       CHARACTER*2 NL 
       CHARACTER*1, PARAMETER :: BACKSLASH = CHAR(92)
       EXTERNAL NL
C-----------------------------------------------

      NBREPET = 0
C
      IF(PRESENT(PRMODE))THEN

        IBUF=0
        RBUF=ZERO
        CBUF=' '
C
        INDXI=0
        INDXR=0
        INDXC=0
        INDXD=0
C
        TMPOUT=' '
        INDXTMPOUT=0
        TMPBUF=' '
C
        NB_IBUF = 0
        NB_RBUF = 0
        NB_CBUF = 0
C

C ----------------------------
C  Argument presence checking
C ----------------------------
        IF (PRESENT(I1)) THEN
         IBUF(1)=I1
         NB_IBUF = NB_IBUF + 1
         IF (PRESENT(I2)) THEN
          IBUF(2)=I2
          NB_IBUF = NB_IBUF + 1
          IF (PRESENT(I3)) THEN
            IBUF(3)=I3
            NB_IBUF = NB_IBUF + 1
            IF (PRESENT(I4)) THEN
              IBUF(4)=I4
              NB_IBUF = NB_IBUF + 1
              IF (PRESENT(I5)) THEN
                IBUF(5)=I5
                NB_IBUF = NB_IBUF + 1
                IF (PRESENT(I6)) THEN
                  IBUF(6)=I6
                  NB_IBUF = NB_IBUF + 1
                  IF (PRESENT(I7)) THEN
                    IBUF(7)=I7
                    NB_IBUF = NB_IBUF + 1
                    IF (PRESENT(I8)) THEN
                      IBUF(8)=I8
                      NB_IBUF = NB_IBUF + 1
                      IF (PRESENT(I9)) THEN
                        IBUF(9)=I9
                        NB_IBUF = NB_IBUF + 1
                        IF (PRESENT(I10)) THEN
                          IBUF(10)=I10
                          NB_IBUF = NB_IBUF + 1
          IF (PRESENT(I11)) THEN
           IBUF(11)=I11
           NB_IBUF = NB_IBUF + 1
           IF (PRESENT(I12)) THEN
            IBUF(12)=I12
             NB_IBUF = NB_IBUF + 1
            IF (PRESENT(I13)) THEN
              IBUF(13)=I13
              NB_IBUF = NB_IBUF + 1
              IF (PRESENT(I14)) THEN
                IBUF(14)=I14
                NB_IBUF = NB_IBUF + 1
                IF (PRESENT(I15)) THEN
                  IBUF(15)=I15
                  NB_IBUF = NB_IBUF + 1
                  IF (PRESENT(I16)) THEN
                    IBUF(16)=I16
                    NB_IBUF = NB_IBUF + 1
                    IF (PRESENT(I17)) THEN
                      IBUF(17)=I17
                      NB_IBUF = NB_IBUF + 1
                      IF (PRESENT(I18)) THEN
                        IBUF(18)=I18
                        NB_IBUF = NB_IBUF + 1
                        IF (PRESENT(I19)) THEN
                          IBUF(19)=I19
                          NB_IBUF = NB_IBUF + 1
                          IF (PRESENT(I20)) THEN
                            IBUF(20)=I20
                            NB_IBUF = NB_IBUF + 1
                          END IF
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
           END IF
          END IF
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
         END IF
        END IF
C
        IF (PRESENT(R1)) THEN
         RBUF(1)=R1
         NB_RBUF = NB_RBUF + 1
         IF (PRESENT(R2)) THEN
          RBUF(2)=R2
          NB_RBUF = NB_RBUF + 1
          IF (PRESENT(R3)) THEN
            RBUF(3)=R3
            NB_RBUF = NB_RBUF + 1
            IF (PRESENT(R4)) THEN
              RBUF(4)=R4
              NB_RBUF = NB_RBUF + 1
              IF (PRESENT(R5)) THEN
                RBUF(5)=R5
                NB_RBUF = NB_RBUF + 1
                IF (PRESENT(R6)) THEN
                  RBUF(6)=R6
                  NB_RBUF = NB_RBUF + 1
                  IF (PRESENT(R7)) THEN
                    RBUF(7)=R7
                    NB_RBUF = NB_RBUF + 1
                    IF (PRESENT(R8)) THEN
                      RBUF(8)=R8
                      NB_RBUF = NB_RBUF + 1
                      IF (PRESENT(R9)) THEN
                        RBUF(9)=R9
                        NB_RBUF = NB_RBUF + 1
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
         END IF
        END IF
C
        IF (PRESENT(C1)) THEN
         CBUF(1)=C1
         NB_CBUF = NB_CBUF + 1
         IF (PRESENT(C2)) THEN
          CBUF(2)=C2
          NB_CBUF = NB_CBUF + 1
          IF (PRESENT(C3)) THEN
            CBUF(3)=C3
            NB_CBUF = NB_CBUF + 1
            IF (PRESENT(C4)) THEN
              CBUF(4)=C4
              NB_CBUF = NB_CBUF + 1
              IF (PRESENT(C5)) THEN
                CBUF(5)=C5
                NB_CBUF = NB_CBUF + 1
                IF (PRESENT(C6)) THEN
                  CBUF(6)=C6
                  NB_CBUF = NB_CBUF + 1
                  IF (PRESENT(C7)) THEN
                    CBUF(7)=C7
                    NB_CBUF = NB_CBUF + 1
                    IF (PRESENT(C8)) THEN
                      CBUF(8)=C8
                      NB_CBUF = NB_CBUF + 1
                      IF (PRESENT(C9)) THEN
                        CBUF(9)=C9
                        NB_CBUF = NB_CBUF + 1
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
         END IF
        END IF


       IF (PRMODE == 0) THEN     ! PRMODE=MSG_CUMU
c
C -------------------------
C  Message is parsed 
C -------------------------
C  The results of %d %f %s 
C  are stored in RES_MES FILE
C -------------------------


        IEXIST=0
        DO ITYPE=1,SMSGTYPE
          IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
            IEXIST=1
          END IF
        END DO
        MYFMT='(A,I10,X)'
        WRITE(RES_MES,MYFMT) 'ID=',MSGID 
C
          ITYPE = 4
          IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
            DO ILINE=1,MESSAGES(ITYPE,MSGID)%SMESSAGE
              IF (ILINE>1) CTYPE=''
              TMPOUT=' '
              TMPBUF=' '
              IF (MESSAGES(ITYPE,MSGID)%SMESSAGE/=0) THEN
              TMPLINE=MESSAGES(ITYPE,MSGID)%MESSAGE(ILINE)
              BUFLEN=0
              I=2
              IOLD=1
              DO WHILE (I+1<=LEN_TRIM(TMPLINE))
                INDXTMPOUT=0
                IF (TMPLINE(I:I)=='%') THEN
                  I=I+1
                  IF (TMPLINE(I:I)=='d') THEN          ! %d (integer) replaced with its value
                    I=I+1
                    IOLD=I
                    MYFMT='(I10,X)'
                    IF (INDXI<=20) INDXI=INDXI+1
                    WRITE(TMPBUF,MYFMT)IBUF(INDXI)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)+1

                  ELSE IF (TMPLINE(I:I)=='f') THEN     ! %f (float) replaced with its value
                    I=I+1
                    IOLD=I
                    MYFMT='(1PG20.13,X)'
                    IF (INDXR<10) INDXR=INDXR+1
                    WRITE(TMPBUF,MYFMT)RBUF(INDXR)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)+1

                  ELSE IF (TMPLINE(I:I)=='s') THEN     ! %s (string) replaced with its value
                    I=I+1
                    IOLD=I
                    MYFMT='(A),X'
                    IF (INDXC<10) INDXC=INDXC+1
                    WRITE(TMPBUF,MYFMT)CBUF(INDXC)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)+1

                  ELSEIF (TMPLINE(I:I)=='i') THEN      ! New format %ixxx be computed - for test add fmt
                    ISAV=I
                    I=I+1
                    IF (DYNA_MESSAGE == 1) THEN          ! Treatment ignored when DYNA_MESSAGE=0 / Native Rd Deck
                      CALL MESS_EXTRACT_FORMAT(TMPLINE(I:ncharline),LEN_TRIM(TMPLINE(I:ncharline)),NCOUNT,ID_NUM,KEY,KEY_LEN)  ! format is %iXX="/KEY" XX=1-11
                      I=I+NCOUNT
                      OPTION_NAME_DYNA=' ' 
                      S_OPTION_NAME_DYNA=1
                    
                      CALL CPP_FIND_DYNA_MESS(KEY,KEY_LEN,IBUF(ID_NUM),OPTION_NAME_DYNA,
     *                                S_OPTION_NAME_DYNA,ISAV,DYNA_TITLE,SDYNA_TITLE)
  
                      MYFMT='(A)'
                      WRITE(TMPBUF,MYFMT) OPTION_NAME_DYNA(1:S_OPTION_NAME_DYNA)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=LEN_TRIM(TMPBUF)+1
                    END IF
                  END IF
                ELSE
                  I=I+1
                END IF
                IF (BUFLEN>0) THEN
                  TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
                IF (INDXTMPOUT>0) THEN
                  WRITE(RES_MES,'(A)')TMPOUT(1:INDXTMPOUT)
                END IF
              END DO
              END IF
            END DO
          END IF

       ELSE       ! PRMODE=MSG_PRINT
C print compacted message from tmp file
c
C
        OPEN (UNIT=RES_TMP,STATUS='SCRATCH',FORM='FORMATTED')
        REWIND(RES_MES)
        IEXIST=0
C
        DO ITYPE=1,SMSGTYPE
          IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
            IEXIST=1
          END IF
        END DO
        NBREPET = 0
        IEND = 0
c---------------------------------
c        RES_CHECK = 123499
c        OPEN (UNIT=RES_CHECK,STATUS='UNKNOWN',FORM='FORMATTED',FILE='fort.1')
        DO WHILE(IEND/=1)
          READ(RES_MES,'(A)',END=110) TMPIN
          WRITE(RES_CHECK,'(A)') TMPIN(1:LEN_TRIM(TMPIN))
        ENDDO
110     IEND = 1
        IEND = 0
        REWIND(RES_MES)
c---------------------------------
        INDXI_INC = 0
        INDXR_INC = 0
        INDXC_INC = 0
        INDXD_INC = 0
          ITYPE = 4

C         ------------------------------------------
C          Message is parsed, format tags are count
C         ------------------------------------------
          IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
            DO ILINE=1,MESSAGES(ITYPE,MSGID)%SMESSAGE
              IF (MESSAGES(ITYPE,MSGID)%SMESSAGE/=0) THEN
              TMPLINE=MESSAGES(ITYPE,MSGID)%MESSAGE(ILINE)
              I=2
              IOLD=1
              DO WHILE (I+1<=LEN_TRIM(TMPLINE))
          IF (TMPLINE(I:I)=='%') THEN
            I=I+1
            IF (TMPLINE(I:I)=='d') THEN       ! %d format
              I=I+1
              INDXI_INC=INDXI_INC+1
            ELSE IF (TMPLINE(I:I)=='f') THEN  ! %f format
              I=I+1
              INDXR_INC=INDXR_INC+1
            ELSE IF (TMPLINE(I:I)=='s') THEN  ! %s format
              I=I+1
              INDXC_INC=INDXC_INC+1
                  ELSE IF (TMPLINE(I:I)=='i') THEN  ! %ixxx format
              I=I+1
                    IF(DYNA_MESSAGE == 1) THEN
                INDXD_INC=INDXD_INC+1
                    ENDIF
            ENDIF
          ELSE
            I=I+1
          END IF
              END DO
              ENDIF
            ENDDO
          END IF
C -----------------------------------
C  Message file is read
C  catch all message info from MSGID
C -----------------------------------
        READ(RES_MES,'(A)',END=120) TMPIN
        DO WHILE(IEND/=1)

          IF(TMPIN(1:3)== 'ID=')THEN
            READ(TMPIN(4:14),'(I10)') MSGID_R 
            IF (MSGID == MSGID_R) THEN         ! Message ID was found
              NBREPET = NBREPET + 1
              INDXI = INDXI + INDXI_INC
              INDXR = INDXR + INDXR_INC
              INDXC = INDXC + INDXC_INC
              INDXD = INDXD + INDXD_INC

              READ(RES_MES,'(A)',END=120) TMPIN
              DO WHILE(IEND/=1 .AND. TMPIN(1:3) /= 'ID=')  ! skip to next message
                READ(RES_MES,'(A)',END=120) TMPIN
              ENDDO
            ELSE                                ! Message ID is different / skip to next message
              WRITE(RES_TMP,'(A)') TMPIN(1:LEN_TRIM(TMPIN))
              READ(RES_MES,'(A)',END=120) TMPIN
              DO WHILE(IEND/=1 .AND. TMPIN(1:3) /= 'ID=')
                WRITE(RES_TMP,'(A)') TMPIN(1:LEN_TRIM(TMPIN))
                READ(RES_MES,'(A)',END=120) TMPIN
              ENDDO
            ENDIF
          ENDIF
        ENDDO

120     IEND = 1

        REWIND(RES_MES)
c
C ------------------------------------------
C  All counts are done - Allocating buffers
C ------------------------------------------
        ALLOCATE(ITRI(INDXI_INC,NBREPET))
        ALLOCATE(TMP_INTEGER(INDXI+NB_IBUF))
        ALLOCATE(TMP_REAL(INDXR+NB_RBUF))
        ALLOCATE(TMP_CHAR(INDXC+NB_CBUF))
        ALLOCATE(TMP_I(INDXD))
c
        TMP_INTEGER=0
        TMP_REAL=ZERO
        TMP_I=' '
        TMP_CHAR=' '
        
        IF(NBREPET /= 0) THEN
        NBREPET_0 = NBREPET
        NBREPET = 0
        IEND = 0
        INDXI=NB_IBUF
        INDXR=NB_RBUF
        INDXC=NB_CBUF
        INDXD=0
        CPT = 0

C ----------------------
C  Re-read message file
C ----------------------
        DO WHILE(IEND/=1)
          READ(RES_MES,'(A)',END=130) TMPIN
          IF(TMPIN(1:3)== 'ID=')THEN
            READ(TMPIN(4:14),'(I10)') MSGID_R

            IF (MSGID == MSGID_R) THEN                                     ! Look for all messages bellonging to msgid number
            NBREPET = NBREPET + 1
c
              ITYPE = 4 
               IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN          ! Parsing the message and their formats
                DO ILINE=1,MESSAGES(ITYPE,MSGID)%SMESSAGE
                  IF (MESSAGES(ITYPE,MSGID)%SMESSAGE/=0) THEN
                    TMPLINE=MESSAGES(ITYPE,MSGID)%MESSAGE(ILINE)
                    I=2
                    IOLD=1
                    NO_PRINT=0
                    DO WHILE (I+1<=LEN_TRIM(TMPLINE))
                      IF (TMPLINE(I:I)=='%') THEN
          I=I+1
          IF (TMPLINE(I:I)=='d') THEN
            I=I+1
            IOLD=I
            INDXI=INDXI+1
            CPT=CPT+1
                          READ(RES_MES,'(A)',END=130) TMPIN
                          READ(TMPIN,'(I10)') INT_TMP
            TMP_INTEGER(INDXI) = INT_TMP
                          ITRI(CPT,NBREPET) = INT_TMP
            IF(CPT == INDXI_INC) CPT = 0
          ELSE IF (TMPLINE(I:I)=='f') THEN
            I=I+1
            IOLD=I
            INDXR=INDXR+1
                          READ(RES_MES,'(A)',END=130) TMPIN
                          READ(TMPIN,*) REAL_TMP
            TMP_REAL(INDXR) = REAL_TMP
          ELSE IF (TMPLINE(I:I)=='s') THEN
            I=I+1
                          IOLD=I
                          INDXC=INDXC+1
                          READ(RES_MES,'(A)',END=130) TMPIN
                          READ(TMPIN,'(A)') CHAR_TMP
                          TMP_CHAR(INDXC) = CHAR_TMP
                        ELSE IF (TMPLINE(I:I)=='i') THEN
                          I=I+1
                          IF(DYNA_MESSAGE == 1) THEN
                            IOLD=I
                            INDXD=INDXD+1
                            READ(RES_MES,'(A)',END=130) TMPIN
                            READ(RES_MES,'(A)',END=130) TMPIN2
                            READ(RES_MES,'(A)',END=130) TMPIN3

                            CHAR_TMP=TMPIN(1:LEN_TRIM(TMPIN))//NL()//TMPIN2(1:LEN_TRIM(TMPIN2))
     *                                                       //NL()//TMPIN3(1:LEN_TRIM(TMPIN3)) 
                            TMP_I(INDXD) = CHAR_TMP
                          ELSE
                              NO_PRINT=1
                          ENDIF
          ENDIF
                      ELSE
          I=I+1
                      END IF
                    END DO
                  ENDIF
                ENDDO
               END IF
            ENDIF
          ENDIF
        ENDDO
130     IEND = 1
C
        INDXI=0
        INDXR=0
        INDXC=0
        INDXD=0
        IEXIST=0
        CPT = 0
c remove duplicated messages
        MODE = 0
        ALLOCATE(INDEX(2*NBREPET_0*INDXI_INC))
        IF (NBREPET_0 . NE. 0)THEN
          ALLOCATE(MES_DOUBLE(NBREPET_0))
          MES_DOUBLE(1) = 0
          IF (INDXI_INC == 0)THEN
            MES_DOUBLE(2:NBREPET_0) = 0
          ELSE
            MES_DOUBLE(2:NBREPET_0) = 1
          ENDIF
        ELSE
          ALLOCATE(MES_DOUBLE(1))
          MES_DOUBLE(1) = 0
        ENDIF
c
        DO J=1,NBREPET_0
          INDEX(J) = J
        ENDDO
        CALL MY_ORDERS( MODE, WORK, ITRI,INDEX,NBREPET_0,INDXI_INC)
        IF (NBREPET_0 /= 0) MES_DOUBLE(INDEX(1)) = 0
        DO J=2,NBREPET_0
          ITAG = 0
          DO K=1,INDXI_INC
            IF(ITRI(K,INDEX(J)) /= 
     .         ITRI(K,INDEX(J-1)))THEN
              ITAG = ITAG + 1
            ENDIF
          ENDDO
          IF (ITAG /= 0) MES_DOUBLE(INDEX(J)) = 0
        ENDDO
        NB_MESSAGES = 0
        DO J=1,NBREPET_0
          IF(MES_DOUBLE(INDEX(J)) == 0) NB_MESSAGES = NB_MESSAGES + 1
        ENDDO

c-----------------------------------------------------------
c   MESSAGE SUMMARY
c-----------------------------------------------------------
c
        MYFMT='(A,X,A)' 
          IF(MSGTYPE == 0) THEN 
            WRITE(RES_CHECK,MYFMT) 'M_OPTION=',ERR_CATEGORY(1:LEN_TRIM(ERR_CATEGORY))
          ELSEIF(MSGTYPE == 1) THEN
            WRITE(RES_CHECK,MYFMT) 'W_OPTION=',ERR_CATEGORY(1:LEN_TRIM(ERR_CATEGORY))
          ELSEIF(MSGTYPE == 2) THEN
            WRITE(RES_CHECK,MYFMT) 'E_OPTION=',ERR_CATEGORY(1:LEN_TRIM(ERR_CATEGORY))
          ENDIF
          MYFMT='(A,I10,X)' 
          WRITE(RES_CHECK,MYFMT) 'OPTION_TYPE=',0
          IF(PRESENT(I1))THEN
            WRITE(RES_CHECK,MYFMT) 'OPTION_ID=',I1
          ELSE
            WRITE(RES_CHECK,MYFMT) 'OPTION_ID=',0
          ENDIF
          MYFMT='(A,X,A)'
          IF(PRESENT(C1))THEN
            WRITE(RES_CHECK,MYFMT) 'TITLE=',C1(1:LEN_TRIM(C1))
          ELSE
            WRITE(RES_CHECK,MYFMT) 'TITLE=',''
          ENDIF
          MYFMT='(A,I10,X)'
          WRITE(RES_CHECK,MYFMT) 'ID_MES=',MSGID
          WRITE(RES_CHECK,MYFMT) 'NB_REPET=',NBREPET
C
        CMSGTYPE=' '
        IF (MSGTYPE==0) THEN
          CMSGTYPE='MESSAGE'
          IMSG=IMSG+1
        ELSE IF (MSGTYPE==1) THEN 
          CMSGTYPE='WARNING'
          IWARN=IWARN+1
        ELSE IF (MSGTYPE==2) THEN
          CMSGTYPE='ERROR'
          IERR=IERR+1
        END IF
          IEXIST=0
          DO ITYPE=1,SMSGTYPE
            IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
              IEXIST=1
            END IF
          END DO
          IF (IEXIST == 0)  THEN
            WRITE(ISTDO,'(A,A,A,I6)')'CALL TO UNEXISTING ',
     .                            CMSGTYPE(1:LEN_TRIM(CMSGTYPE)),
     .                            ' ID = ',
     .                            MSGID
            IF (IOUT/=0) THEN
              WRITE(IOUT,'(A,A,A,I6)')'CALL TO UNEXISTING ',
     .                              CMSGTYPE(1:LEN_TRIM(CMSGTYPE)),
     .                              ' ID = ',
     .                              MSGID
            END IF
            RETURN
          END IF
        IF (ANMODE/=ANINFO_BLIND_2) THEN
            WRITE(ISTDO,'(/A,I6)')CMSGTYPE(1:LEN_TRIM(CMSGTYPE))
     *                      //' ID : ',MSGID
        END IF
        IF (ISTDO/=IOUT) THEN
          IF (IOUT/=0) THEN
          WRITE(IOUT,'(/A,I6)')CMSGTYPE(1:LEN_TRIM(CMSGTYPE))
     *                        //' ID : ',MSGID
          ELSE
          WRITE(ISTDO,'(/A,I6)')CMSGTYPE(1:LEN_TRIM(CMSGTYPE))
     *                       //' ID : ',MSGID
          END IF
        END IF
c
        DO ITYPE=1,SMSGTYPE
          IF (ITYPE==1) THEN
            CTYPE=''
          ELSE IF (ITYPE==2) THEN
C                WRITE(CTYPE,'(A,/)')"DESCRIPTION : "
            CTYPE='DESCRIPTION : '
          ELSE IF (ITYPE==3) THEN
            CTYPE='SOLUTION : '
          ELSE IF (ITYPE==4) THEN
            CTYPE=''
          END IF
          IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
           IF (ITYPE == 4)THEN
             NBREPET = NBREPET_0
           ELSE
             NBREPET = 1
           ENDIF
           DO CPT = 1,NBREPET
            DO ILINE=1,MESSAGES(ITYPE,MSGID)%SMESSAGE
              IF (ILINE>1) CTYPE=''
              TMPOUT=' '
              TMPBUF=' '
              IF (MESSAGES(ITYPE,MSGID)%SMESSAGE/=0 ) THEN
              TMPLINE=MESSAGES(ITYPE,MSGID)%MESSAGE(ILINE)
              BUFLEN=0
              INDXTMPOUT=0
              I=1
              IOLD=1
              NO_PRINT = 0
              DO WHILE (I+1<=LEN_TRIM(TMPLINE))  ! loop over all characters from TMPLINE
                IF (TMPLINE(I:I) == BACKSLASH ) THEN
                  I=I+1
                  IF (I-2>=1) THEN
                    WRITE(TMPBUF,'(A,A)')TMPLINE(IOLD:I-2),TMPLINE(I:I)
                    BUFLEN=I-2-IOLD+1+1
                  ELSE
                    WRITE(TMPBUF,'(A)')TMPLINE(I:I)
                    BUFLEN=1
                  END IF
                  I=I+1
                  IOLD=I
                ELSE IF (TMPLINE(I:I)=='%') THEN
                  I=I+1
                  IF (I-2>=1) THEN
                    WRITE(TMPBUF,'(A)')TMPLINE(IOLD:I-2)
                    BUFLEN=I-2-IOLD+1
                    IF (BUFLEN>0) THEN
                      TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                      INDXTMPOUT=INDXTMPOUT+BUFLEN
                      BUFLEN=0
                    END IF
                  END IF
                  IF (TMPLINE(I:I)=='d') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(I10)'
                    INDXI=INDXI+1
                    IF(ITYPE == 4)THEN
                      WRITE(TMPBUF,MYFMT)TMP_INTEGER(INDXI)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=10
                    ELSE
                      WRITE(TMPBUF,MYFMT)IBUF(INDXI)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=LEN_TRIM(TMPBUF)
                    ENDIF
                  ELSE IF (TMPLINE(I:I)=='f') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(1pg20.13)'
                    INDXR=INDXR+1
                    IF(ITYPE == 4)THEN
                      WRITE(TMPBUF,MYFMT)TMP_REAL(INDXR)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=20
                    ELSE
                      WRITE(TMPBUF,MYFMT)RBUF(INDXR)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=LEN_TRIM(TMPBUF)
                    ENDIF
                  ELSE IF (TMPLINE(I:I)=='s') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(A)'
                    INDXC=INDXC+1
                    IF(ITYPE == 4)THEN
                      WRITE(TMPBUF,MYFMT)TMP_CHAR(INDXC)
                    ELSE
                      WRITE(TMPBUF,MYFMT)CBUF(INDXC) 
                    ENDIF
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  ELSE IF (TMPLINE(I:I)=='r') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(I10)'
                    WRITE(TMPBUF,MYFMT)NB_MESSAGES
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  ELSE IF (TMPLINE(I:I)=='i') THEN
                    ISAV=I
                    I=I+1
                    IF(DYNA_MESSAGE == 1) THEN

                      CALL MESS_EXTRACT_FORMAT(TMPLINE(I:ncharline),LEN_TRIM(TMPLINE(I:ncharline)),NCOUNT,ID_NUM,KEY,KEY_LEN)  ! format is %iXX="/KEY" XX=1-11

                      I=I+NCOUNT
                      IOLD=I
                      MYFMT='(A)'
                      INDXD=INDXD+1
                      IF(ITYPE == 4)THEN
                         WRITE(TMPBUF,MYFMT)TMP_I(INDXD)
                      ELSE
                         OPTION_NAME_DYNA=' ' 
                         S_OPTION_NAME_DYNA=1
                    
                         CALL CPP_FIND_DYNA_MESS(KEY,KEY_LEN,IBUF(ID_NUM),OPTION_NAME_DYNA,
     *                                S_OPTION_NAME_DYNA,ISAV,DYNA_TITLE,SDYNA_TITLE)
                         WRITE(TMPBUF,MYFMT) OPTION_NAME_DYNA(1:S_OPTION_NAME_DYNA)
                      ENDIF
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=LEN_TRIM(TMPBUF)
                    ELSE
                      NO_PRINT = 1        ! Line with %i should not be print in case of native format.
                    ENDIF
                  ELSE
                    I=I+1
                  END IF
                ELSE
                  I=I+1
                END IF
                IF (BUFLEN>0) THEN
                  TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
              END DO
              IF (IOLD<=I) THEN
                WRITE(TMPBUF,'(A)')
     *          TMPLINE(IOLD:LEN_TRIM(TMPLINE))
                BUFLEN=LEN_TRIM(TMPLINE)-IOLD+1
                IF (BUFLEN>0) THEN
                  TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
              END IF
              IF (INDXTMPOUT>0.AND.
     .            MES_DOUBLE(CPT) == 0) THEN
C               do not write title on stdo in case ANINFO_BLIND_2
               IF ( NO_PRINT == 0 ) THEN       ! Line with %i should not be print in case of native format.
                IF (ITYPE==1) THEN
                  IF (ANMODE==ANINFO.OR.
     *                ANMODE==ANINFO_BLIND_1) THEN
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                  END IF
                  IF (ISTDO/=IOUT) THEN
                    IF (IOUT/=0) THEN
                      WRITE(IOUT,'(A)')TMPOUT(1:INDXTMPOUT)
                    ELSE
C                         do not lose information
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                    END IF
C                  IF (IOUTMSG/=0) THEN
C                    WRITE(IOUTMSG,'(A)')TMPOUT(1:INDXTMPOUT)
C                  ELSE
CC                       do not lose information
C                    WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
C                  END IF
                  END IF
                ELSE
C               other information written on stdo only in case ANINFO
                  IF (ILINE==1) THEN
                    IF (ANMODE==ANINFO) THEN
                      IF(ITYPE /= 4) WRITE(ISTDO,'(A)')CTYPE
                    END IF
                    IF (ISTDO/=IOUT) THEN
                      IF (IOUT/=0) THEN
                        IF(ITYPE /= 4) WRITE(IOUT,'(A)')CTYPE
                      ELSE
C                       do not lose information
                        IF(ITYPE /= 4) WRITE(ISTDO,'(A)')CTYPE
                      END IF
                    END IF
C                    IF (IOUTMSG/=0) THEN
C                      WRITE(IOUTMSG,'(A)')CTYPE
C                    ELSE
CC                       do not lose information
C                      WRITE(ISTDO,'(A)')CTYPE
C                    END IF
                  END IF

                  IF (ANMODE==ANINFO) THEN
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                  END IF
                  IF (ISTDO/=IOUT) THEN
                    IF (IOUT/=0) THEN
                      WRITE(IOUT,'(A)')TMPOUT(1:INDXTMPOUT)
                    ELSE
C                       do not lose information
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                    END IF
                  END IF
C                  IF (IOUTMSG/=0) THEN
C                    WRITE(IOUTMSG,'(A)')TMPOUT(1:INDXTMPOUT)
C                  ELSE
CC                       do not lose information
C                    WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
C                  END IF
                END IF
               END IF    ! IF NO_PRINT == 1
              END IF
C             IF (ITYPE==3.AND.LEN_TRIM(TMPLINE)==0) THEN
              END IF
            END DO
           ENDDO
          END IF
        END DO
        DEALLOCATE(TMP_INTEGER,TMP_REAL,TMP_CHAR,INDEX,ITRI,MES_DOUBLE)

        ENDIF

c        CLOSE (UNIT=RES_MES,STATUS='DELETE')
c        OPEN (UNIT=RES_MES,STATUS='SCRATCH',FORM='FORMATTED')
c        REWIND(RES_TMP)
c
c        IEND = 0
c        DO WHILE(IEND/=1)
c          READ(RES_TMP,'(A)',END=140) TMPIN
c          WRITE(RES_MES,'(A)') TMPIN
c        ENDDO
c140     CONTINUE
c        CLOSE (UNIT=RES_TMP,STATUS='DELETE')
          REWIND RES_TMP
          CLOSE (UNIT=RES_MES)
          ITMP = RES_MES
          RES_MES = RES_TMP
          RES_TMP = ITMP

       ENDIF
      ELSE                  ! PRMODE Was not set / No Message Stacking.
c not cummu/print part (old one)   
C
c-----------------------------------------------------------
c   MESSAGE SUMMARY
c-----------------------------------------------------------
          MYFMT='(A,X,A)'
          IF(MSGTYPE == 0) THEN 
            WRITE(RES_CHECK,MYFMT) 'M_OPTION=',ERR_CATEGORY(1:LEN_TRIM(ERR_CATEGORY))
          ELSEIF(MSGTYPE == 1) THEN
            WRITE(RES_CHECK,MYFMT) 'W_OPTION=',ERR_CATEGORY(1:LEN_TRIM(ERR_CATEGORY))
          ELSEIF(MSGTYPE == 2) THEN
            WRITE(RES_CHECK,MYFMT) 'E_OPTION=',ERR_CATEGORY(1:LEN_TRIM(ERR_CATEGORY))
          ENDIF
          MYFMT='(A,I10,X)' 
          WRITE(RES_CHECK,MYFMT) 'OPTION_TYPE=',0
          IF(PRESENT(I1))THEN
            WRITE(RES_CHECK,MYFMT) 'OPTION_ID=',I1
          ELSE
            WRITE(RES_CHECK,MYFMT) 'OPTION_ID=',0
          ENDIF
          MYFMT='(A,X,A)'
          IF(PRESENT(C1))THEN
            WRITE(RES_CHECK,MYFMT) 'TITLE=',C1(1:LEN_TRIM(C1))
          ELSE
            WRITE(RES_CHECK,MYFMT) 'TITLE=',''
          ENDIF
          MYFMT='(A,I10,X)'
          WRITE(RES_CHECK,MYFMT) 'ID_MES=',MSGID
          WRITE(RES_CHECK,MYFMT) 'NB_REPET=',1
C   
        CMSGTYPE=' '
        IF (MSGTYPE==0) THEN
          CMSGTYPE='MESSAGE'
          IMSG=IMSG+1
        ELSE IF (MSGTYPE==1) THEN 
          CMSGTYPE='WARNING'
          IWARN=IWARN+1
        ELSE IF (MSGTYPE==2) THEN
          CMSGTYPE='ERROR'
          IERR=IERR+1
        END IF
          IEXIST=0
          DO ITYPE=1,SMSGTYPE
            IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
              IEXIST=1
            END IF
          END DO
          IF (IEXIST == 0)  THEN
            WRITE(ISTDO,'(A,A,A,I6)')'CALL TO UNEXISTING ',
     .                            CMSGTYPE(1:LEN_TRIM(CMSGTYPE)),
     .                            ' ID = ',
     .                            MSGID
            IF (IOUT/=0) THEN
              WRITE(IOUT,'(A,A,A,I6)')'CALL TO UNEXISTING ',
     .                              CMSGTYPE(1:LEN_TRIM(CMSGTYPE)),
     .                              ' ID = ',
     .                              MSGID
            END IF
            RETURN
          END IF
        IF (ANMODE/=ANINFO_BLIND_2) THEN
            WRITE(ISTDO,'(/A,I6)')CMSGTYPE(1:LEN_TRIM(CMSGTYPE))//' ID : ',MSGID
        END IF
        IF (ISTDO/=IOUT) THEN
          IF (IOUT/=0) THEN
            WRITE(IOUT,'(/A,I6)')CMSGTYPE(1:LEN_TRIM(CMSGTYPE))//' ID : ',MSGID
          ELSE
            WRITE(ISTDO,'(/A,I6)')CMSGTYPE(1:LEN_TRIM(CMSGTYPE))//' ID : ',MSGID
          END IF
        END IF
        IBUF=0
        RBUF=ZERO
        CBUF=' '
C
        INDXI=0
        INDXR=0
        INDXC=0
C
        TMPOUT=' '
        INDXTMPOUT=0
        TMPBUF=' '
C
        IF (PRESENT(I1)) THEN
         IBUF(1)=I1
         IF (PRESENT(I2)) THEN
          IBUF(2)=I2
          IF (PRESENT(I3)) THEN
            IBUF(3)=I3
            IF (PRESENT(I4)) THEN
              IBUF(4)=I4
              IF (PRESENT(I5)) THEN
                IBUF(5)=I5
                IF (PRESENT(I6)) THEN
                  IBUF(6)=I6
                  IF (PRESENT(I7)) THEN
                    IBUF(7)=I7
                    IF (PRESENT(I8)) THEN
                      IBUF(8)=I8
                      IF (PRESENT(I9)) THEN
                        IBUF(9)=I9
                        IF (PRESENT(I10)) THEN
                          IBUF(10)=I10
          IF (PRESENT(I11)) THEN
           IBUF(11)=I11
           IF (PRESENT(I12)) THEN
            IBUF(12)=I12
            IF (PRESENT(I13)) THEN
              IBUF(13)=I13
              IF (PRESENT(I14)) THEN
                IBUF(14)=I14
                IF (PRESENT(I15)) THEN
                  IBUF(15)=I15
                  IF (PRESENT(I16)) THEN
                    IBUF(16)=I16
                    IF (PRESENT(I17)) THEN
                      IBUF(17)=I17
                      IF (PRESENT(I18)) THEN
                        IBUF(18)=I18
                        IF (PRESENT(I19)) THEN
                          IBUF(19)=I19
                          IF (PRESENT(I20)) THEN
                            IBUF(20)=I20
                          END IF
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
           END IF
          END IF
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
         END IF
        END IF
C
        IF (PRESENT(R1)) THEN
         RBUF(1)=R1
         IF (PRESENT(R2)) THEN
          RBUF(2)=R2
          IF (PRESENT(R3)) THEN
            RBUF(3)=R3
            IF (PRESENT(R4)) THEN
              RBUF(4)=R4
              IF (PRESENT(R5)) THEN
                RBUF(5)=R5
                IF (PRESENT(R6)) THEN
                  RBUF(6)=R6
                  IF (PRESENT(R7)) THEN
                    RBUF(7)=R7
                    IF (PRESENT(R8)) THEN
                      RBUF(8)=R8
                      IF (PRESENT(R9)) THEN
                        RBUF(9)=R9
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
         END IF
        END IF
C
        IF (PRESENT(C1)) THEN
         CBUF(1)=C1
         IF (PRESENT(C2)) THEN
          CBUF(2)=C2
          IF (PRESENT(C3)) THEN
            CBUF(3)=C3
            IF (PRESENT(C4)) THEN
              CBUF(4)=C4
              IF (PRESENT(C5)) THEN
                CBUF(5)=C5
                IF (PRESENT(C6)) THEN
                  CBUF(6)=C6
                  IF (PRESENT(C7)) THEN
                    CBUF(7)=C7
                    IF (PRESENT(C8)) THEN
                      CBUF(8)=C8
                      IF (PRESENT(C9)) THEN
                        CBUF(9)=C9
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
         END IF
        END IF
C
        DO ITYPE=1,SMSGTYPE
          IF (ITYPE==1) THEN
            CTYPE=''
          ELSE IF (ITYPE==2) THEN
C                WRITE(CTYPE,'(A,/)')"DESCRIPTION : "
            CTYPE='DESCRIPTION : '
          ELSE IF (ITYPE==3) THEN
            CTYPE='SOLUTION : '
          END IF
          IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
            ILINE = 0
            DO WHILE (ILINE < MESSAGES(ITYPE,MSGID)%SMESSAGE)
              ILINE = ILINE + 1
              IF (ILINE>1) CTYPE=''
              TMPOUT=' '
              TMPBUF=' '
C             IF (ITYPE==3.AND.LEN_TRIM(TMPLINE)==0) THEN
              IF (MESSAGES(ITYPE,MSGID)%SMESSAGE/=0) THEN
              TMPLINE=MESSAGES(ITYPE,MSGID)%MESSAGE(ILINE)
              BUFLEN=0
              INDXTMPOUT=0
              I=1
              IOLD=1
!               DO J0=1,LEN_TRIM(TMPLINE)
!                 IF (TMPLINE(J0:J0)=='/') THEN
!                   TMPLINE(J0:J0)=CHAR(10)
!                 END IF
!               END DO
              NO_PRINT = 0
              IF ( MSGID >= 100000 .AND. ILINE >= 3 ) THEN
c HM_READER messages ( line with error sent by reader)
                I=I+1
                IOLD=I
                MYFMT='(A)'
                IF (INDXC<10) INDXC=INDXC+1
                WRITE(TMPBUF,MYFMT)CBUF(INDXC)
                TMPBUF=ADJUSTL(TMPBUF)
                BUFLEN=LEN_TRIM(TMPBUF)
                IF (BUFLEN>0) THEN
                  TMPOUT=TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
                ILINE = MESSAGES(ITYPE,MSGID)%SMESSAGE+1
              ELSEIF ( MSGID > 200000 .AND. ITYPE /= 1 ) THEN 
c CONVERTER messages ( line with error sent by onverter)
                DO WHILE (I+1<=LEN_TRIM(TMPLINE))
                  IF (TMPLINE(I:I) == BACKSLASH) THEN
                    I=I+1
                    IF (I-2>=1) THEN
                      WRITE(TMPBUF,'(A,A)')TMPLINE(IOLD:I-2),TMPLINE(I:I)
                      BUFLEN=I-2-IOLD+1+1
                    ELSE
                      WRITE(TMPBUF,'(A)')TMPLINE(I:I)
                      BUFLEN=1
                    END IF
                    I=I+1
                    IOLD=I
                  ELSE
                    I=I+1
                    IOLD=I
                    MYFMT='(A)'
                    IF (INDXC<10) INDXC=INDXC+1
                    WRITE(TMPBUF,MYFMT)CBUF(INDXC)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  END IF
                  IF (BUFLEN>0) THEN
                    TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                    INDXTMPOUT=INDXTMPOUT+BUFLEN
                    BUFLEN=0
                  END IF
                END DO
              ELSE                                 ! IF ( (MSGID >= 100000 .AND. ILINE >= 3)) THEN  !                         !
                DO WHILE (I+1<=LEN_TRIM(TMPLINE))
                  IF (TMPLINE(I:I)==BACKSLASH) THEN
                    I=I+1
                    IF (I-2>=1) THEN
                      WRITE(TMPBUF,'(A,A)')TMPLINE(IOLD:I-2),TMPLINE(I:I)
                      BUFLEN=I-2-IOLD+1+1
                    ELSE
                      WRITE(TMPBUF,'(A)')TMPLINE(I:I)
                      BUFLEN=1
                    END IF
                    I=I+1
                    IOLD=I
                  ELSE IF (TMPLINE(I:I)=='%') THEN
                    I=I+1
                    IF (I-2>=1) THEN
                      WRITE(TMPBUF,'(A)')TMPLINE(IOLD:I-2)
                      BUFLEN=I-2-IOLD+1
                      IF (BUFLEN>0) THEN
                        TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                        INDXTMPOUT=INDXTMPOUT+BUFLEN
                        BUFLEN=0
                      END IF
                    END IF

                    IF (TMPLINE(I:I)=='d') THEN             ! %d (integer) replaced with its value
                      I=I+1
                      IOLD=I
                      MYFMT='(I10)'
                      IF (INDXI<=20) INDXI=INDXI+1
                      WRITE(TMPBUF,MYFMT)IBUF(INDXI)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=LEN_TRIM(TMPBUF)

                    ELSE IF (TMPLINE(I:I)=='f') THEN        ! %f (float) replaced with its value
                      I=I+1
                      IOLD=I
                      MYFMT='(1pg20.13)'
                      IF (INDXR<10) INDXR=INDXR+1
                      WRITE(TMPBUF,MYFMT)RBUF(INDXR)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=LEN_TRIM(TMPBUF)

                    ELSE IF (TMPLINE(I:I)=='s') THEN        ! %s (string) replaced with its value
                      I=I+1
                      IOLD=I
                      MYFMT='(A)'
                      IF (INDXC<10) INDXC=INDXC+1
                      WRITE(TMPBUF,MYFMT)CBUF(INDXC)
                      TMPBUF=ADJUSTL(TMPBUF)
                      BUFLEN=LEN_TRIM(TMPBUF)

                    ELSEIF (TMPLINE(I:I)=='i') THEN      ! New format %ixxx be computed - for test add fmt
                      ISAV=I
                      I=I+1
                      IF(DYNA_MESSAGE == 1) THEN
                        CALL MESS_EXTRACT_FORMAT(TMPLINE(I:ncharline),LEN_TRIM(TMPLINE(I:ncharline)),NCOUNT,ID_NUM,KEY,KEY_LEN)  ! format is %iXX="/KEY" XX=1-11
                        I=I+NCOUNT
                        IOLD=I
                        OPTION_NAME_DYNA=' ' 
                        S_OPTION_NAME_DYNA=1
                      
                        CALL CPP_FIND_DYNA_MESS(KEY,KEY_LEN,IBUF(ID_NUM),OPTION_NAME_DYNA,
     *                              S_OPTION_NAME_DYNA,ISAV,DYNA_TITLE,SDYNA_TITLE)
  
                        MYFMT='(A)'
                        WRITE(TMPBUF,MYFMT) OPTION_NAME_DYNA(1:S_OPTION_NAME_DYNA)
                        TMPBUF=ADJUSTL(TMPBUF)
                        BUFLEN=LEN_TRIM(TMPBUF)+1
                      ELSE 
                       NO_PRINT=1
                       I=I+1
                      END IF
                    END IF
                  ELSE
                    I=I+1
                  END IF
                  IF (BUFLEN>0) THEN
                    TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                    INDXTMPOUT=INDXTMPOUT+BUFLEN
                    BUFLEN=0
                    INDXTMPOUT=MIN(INDXTMPOUT,ncharline)
                  END IF
                END DO                
                IF (IOLD<=I) THEN
                  WRITE(TMPBUF,'(A)')
     *            TMPLINE(IOLD:LEN_TRIM(TMPLINE))
                  BUFLEN=LEN_TRIM(TMPLINE)-IOLD+1
                  IF (BUFLEN>0) THEN
                    TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                    INDXTMPOUT=INDXTMPOUT+BUFLEN
                    BUFLEN=0
                    INDXTMPOUT=MIN(INDXTMPOUT,ncharline)
                  END IF
                END IF
              ENDIF
              INDXTMPOUT=MIN(INDXTMPOUT,ncharline)
              IF (INDXTMPOUT>0) THEN
C               do not write title on stdo in case ANINFO_BLIND_2
               IF(NO_PRINT == 0)THEN                              ! NO_PRINT is set to 1 when %i is found & 
                IF (ITYPE==1) THEN
                  IF (ANMODE==ANINFO.OR.
     *                ANMODE==ANINFO_BLIND_1) THEN
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                  END IF
                  IF (ISTDO/=IOUT) THEN
                    IF (IOUT/=0) THEN
                      WRITE(IOUT,'(A)')TMPOUT(1:INDXTMPOUT)
                    ELSE
C                         do not lose information
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                    END IF
C                  IF (IOUTMSG/=0) THEN
C                    WRITE(IOUTMSG,'(A)')TMPOUT(1:INDXTMPOUT)
C                  ELSE
CC                       do not lose information
C                    WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
C                  END IF
                  END IF
                ELSE
C               other information written on stdo only in case ANINFO
                  IF (ILINE==1) THEN
                    IF (ANMODE==ANINFO) THEN
                      WRITE(ISTDO,'(A)')CTYPE
                    END IF
                    IF (ISTDO/=IOUT) THEN
                      IF (IOUT/=0) THEN
                        WRITE(IOUT,'(A)')CTYPE
                      ELSE
C                       do not lose information
                        WRITE(ISTDO,'(A)')CTYPE
                      END IF
                    END IF
C                    IF (IOUTMSG/=0) THEN
C                      WRITE(IOUTMSG,'(A)')CTYPE
C                    ELSE
CC                       do not lose information
C                      WRITE(ISTDO,'(A)')CTYPE
C                    END IF
                  END IF
                  IF (ANMODE==ANINFO) THEN
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                  END IF
                  IF (ISTDO/=IOUT) THEN
                    IF (IOUT/=0) THEN
                      WRITE(IOUT,'(A)')TMPOUT(1:INDXTMPOUT)
                    ELSE
C                       do not lose information
                      WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                    END IF
                  END IF
C                  IF (IOUTMSG/=0) THEN
C                    WRITE(IOUTMSG,'(A)')TMPOUT(1:INDXTMPOUT)
C                  ELSE
CC                       do not lose information
C                    WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
C                  END IF
                END IF
               END IF  ! NO_PRINT is set to 1 when %i is found & 
              END IF
C             IF (ITYPE==3.AND.LEN_TRIM(TMPLINE)==0) THEN
              END IF
            END DO
          END IF
        END DO
      ENDIF
c
      IF (ANMODE == ANSTOP .AND. PRESENT(PRMODE)) THEN
        IF (PRMODE == 1 .AND. NBREPET /= 0) THEN
C Care MSGID is ERROR TRAPPED -> Exit code must be 3
           IF(MSGID == 760) THEN
              CALL ARRET(3)
           ELSE
              CALL ARRET(2)
           ENDIF
        ENDIF
      ELSEIF (ANMODE == ANSTOP) THEN
C Care MSGID is ERROR TRAPPED -> Exit code must be 3
        IF(MSGID == 760) THEN
          CALL ARRET(3)
        ELSE
          CALL ARRET(2)
        ENDIF
      ENDIF
c
      RETURN
      END SUBROUTINE


      !||====================================================================
      !||    mess_extract_format    ../starter/source/output/message/message.F
      !||--- called by ------------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MESS_EXTRACT_FORMAT(TMPLINE,LINE_SIZE,NCOUNT,ID_NUM,KEY,KEY_LEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)                         :: LINE_SIZE      ! site of tmpline
      CHARACTER, INTENT(IN),DIMENSION(LINE_SIZE)  :: TMPLINE        ! input string with format

      INTEGER, INTENT(OUT)                        :: NCOUNT         ! format length
      INTEGER, INTENT(OUT)                        :: ID_NUM         ! processed ID indice
      CHARACTER, INTENT(OUT),DIMENSION(NCHARLINE) :: KEY            ! output string with option type
      INTEGER, INTENT(OUT)                        :: KEY_LEN        ! output string with option type
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,BRACKETS_COUNT,NUMB_COUNT,EQUAL_FOUND
      CHARACTER, DIMENSION(10) :: NUMB
C-----------------------------------------------
      NUMB_COUNT=0
      NUMB(1:10)=' '
      I=1
      BRACKETS_COUNT=0
      NCOUNT=0
      ID_NUM=0
      KEY_LEN=0
      EQUAL_FOUND=0

      DO WHILE( BRACKETS_COUNT /= 2 .AND. I<=LINE_SIZE)

        IF(TMPLINE(I) == '"') THEN
           BRACKETS_COUNT=BRACKETS_COUNT+1
           I=I+1 ! move one forward
           IF(I > LINE_SIZE)EXIT
        ENDIF

        IF(TMPLINE(I) == '=') THEN
           EQUAL_FOUND=1
        ENDIF

        IF(EQUAL_FOUND==0)THEN
           NUMB_COUNT=NUMB_COUNT+1
           NUMB(NUMB_COUNT)=TMPLINE(I)
        ENDIF

        IF(BRACKETS_COUNT==1)THEN   ! copying in key
          KEY_LEN=KEY_LEN+1
          KEY(KEY_LEN)=TMPLINE(I)
        ENDIF

        NCOUNT=NCOUNT+1
        I=I+1
      ENDDO
      NCOUNT=NCOUNT+1                ! move one forward to skip the brackets
      READ(NUMB,'(I10)') ID_NUM

      END


      CHARACTER*2 FUNCTION NL()
C  |====================================================================
C  | Routine to set Carriage return according to different OS flavours
C  | Linux : CR - CHAR(10)
C  | Windows : CR/LF - CHAR(13)CHAR(10)
C  |====================================================================
      CHARACTER*2 CR
#ifdef _WIN64
       CR=CHAR(13)//CHAR(10)
#else
       CR=' '//CHAR(10)
#endif
      NL=CR  
      RETURN
      END
      
