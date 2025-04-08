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
      !||    hm_option_read_mod                ../starter/share/modules1/hm_option_read_mod.F
      !||--- called by ------------------------------------------------------
      !||    check_eltyp                       ../starter/source/model/sets/check_eltyp.F
      !||    contrbe2                          ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    contrbe3                          ../starter/source/constraints/general/rbe3/contrbe3.F
      !||    contrl                            ../starter/source/starter/contrl.F
      !||    create_box_clause                 ../starter/source/model/sets/create_box_clause.F
      !||    create_ellipse_clause             ../starter/source/model/sets/create_ellipse_clause.F
      !||    create_elt_box                    ../starter/source/model/sets/create_elt_box.F
      !||    create_elt_clause                 ../starter/source/model/sets/create_element_clause.F
      !||    create_elt_list                   ../starter/source/model/sets/create_element_clause.F
      !||    create_elt_list_g                 ../starter/source/model/sets/create_element_clause.F
      !||    create_map_tables                 ../starter/source/model/sets/map_tables.F
      !||    create_node_box                   ../starter/source/model/sets/create_node_box.F
      !||    create_node_clause                ../starter/source/model/sets/create_node_clause.F
      !||    create_node_from_seg              ../starter/source/model/sets/create_node_from_seg.F
      !||    create_node_list                  ../starter/source/model/sets/create_node_clause.F
      !||    create_node_list_g                ../starter/source/model/sets/create_node_clause.F
      !||    create_nodens_clause              ../starter/source/model/sets/create_nodens_clause.F90
      !||    create_part_clause                ../starter/source/model/sets/create_part_clause.F
      !||    create_part_list                  ../starter/source/model/sets/create_part_clause.F
      !||    create_part_list_g                ../starter/source/model/sets/create_part_clause.F
      !||    create_plane_clause               ../starter/source/model/sets/create_plane_clause.F90
      !||    create_rbody_box                  ../starter/source/model/sets/create_rbody_box.F
      !||    create_rbody_clause               ../starter/source/model/sets/create_rbody_clause.F
      !||    create_rbody_list                 ../starter/source/model/sets/create_rbody_clause.F
      !||    create_rbody_list_g               ../starter/source/model/sets/create_rbody_clause.F
      !||    create_seg_clause                 ../starter/source/model/sets/create_seg_clause.F
      !||    create_set_array                  ../starter/source/model/sets/create_set_clause.F
      !||    create_set_list                   ../starter/source/model/sets/create_set_clause.F
      !||    create_set_list_g                 ../starter/source/model/sets/create_set_clause.F
      !||    create_setcol_array               ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_list                ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_list_g              ../starter/source/model/sets/create_setcol_clause.F
      !||    create_subm_clause                ../starter/source/model/sets/create_subm_clause.F
      !||    create_subm_list                  ../starter/source/model/sets/create_subm_clause.F
      !||    create_subm_list_g                ../starter/source/model/sets/create_subm_clause.F
      !||    create_subs_clause                ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list                  ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list_g                ../starter/source/model/sets/create_subs_clause.F
      !||    fill_igr                          ../starter/source/model/sets/fill_igr.F
      !||    groups_get_elem_list              ../starter/source/groups/groups_get_elem_list.F
      !||    groups_get_nentity                ../starter/source/groups/groups_get_nentity.F
      !||    hm_admlcnt                        ../starter/source/groups/hm_admlistcnt.F
      !||    hm_admlist                        ../starter/source/groups/hm_admlist.F
      !||    hm_bigsbox                        ../starter/source/groups/hm_bigsbox.F
      !||    hm_convert_2d_elements_seatbelt   ../starter/source/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
      !||    hm_convert_fail_tab               ../starter/source/devtools/hm_reader/hm_convert_fail_tab.F
      !||    hm_count_2d_element_seatbelt      ../starter/source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
      !||    hm_elngr                          ../starter/source/groups/hm_elngr.F
      !||    hm_elngrr                         ../starter/source/groups/hm_elngrr.F
      !||    hm_elngrs                         ../starter/source/groups/hm_elngr.F
      !||    hm_grogro                         ../starter/source/groups/hm_grogro.F
      !||    hm_grogronod                      ../starter/source/groups/hm_grogronod.F
      !||    hm_lecgre                         ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                         ../starter/source/groups/hm_lecgrn.F
      !||    hm_linengr                        ../starter/source/groups/hm_linengr.F
      !||    hm_lines_of_lines                 ../starter/source/groups/hm_lines_of_lines.F
      !||    hm_pre_read_link                  ../starter/source/constraints/rigidlink/hm_pre_read_rlink.F
      !||    hm_pre_read_preload               ../starter/source/loads/general/preload/hm_pre_read_preload.F
      !||    hm_pre_read_preload_axial         ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_prelecgrns                     ../starter/source/groups/hm_prelecgrns.F
      !||    hm_prelecjoi                      ../starter/source/constraints/general/cyl_joint/hm_prelecjoi.F
      !||    hm_preread_bcscyc                 ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    hm_preread_bem                    ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_preread_cload                  ../starter/source/loads/general/cload/hm_preread_cload.F
      !||    hm_preread_convec                 ../starter/source/loads/thermic/hm_preread_convec.F
      !||    hm_preread_eig                    ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_preread_grav                   ../starter/source/loads/general/grav/hm_preread_grav.F
      !||    hm_preread_impacc                 ../starter/source/constraints/general/impvel/hm_preread_impacc.F
      !||    hm_preread_impdisp                ../starter/source/constraints/general/impvel/hm_preread_impdisp.F
      !||    hm_preread_impflux                ../starter/source/constraints/thermic/hm_preread_impflux.F
      !||    hm_preread_imptemp                ../starter/source/constraints/thermic/hm_preread_imptemp.F
      !||    hm_preread_impvel                 ../starter/source/constraints/general/impvel/hm_preread_impvel.F
      !||    hm_preread_impvel0                ../starter/source/constraints/general/impvel/hm_preread_impvel0.F
      !||    hm_preread_inivel                 ../starter/source/initial_conditions/general/inivel/hm_preread_inivel.F90
      !||    hm_preread_load_centri            ../starter/source/loads/general/load_centri/hm_preread_load_centri.F
      !||    hm_preread_load_pressure          ../starter/source/loads/general/load_pressure/hm_preread_load_pressure.F
      !||    hm_preread_merge                  ../starter/source/constraints/general/merge/hm_preread_merge.F
      !||    hm_preread_node                   ../starter/source/elements/reader/hm_preread_node.F
      !||    hm_preread_part                   ../starter/source/model/assembling/hm_read_part.F
      !||    hm_preread_pblast                 ../starter/source/loads/pblast/hm_preread_pblast.F
      !||    hm_preread_pfluid                 ../starter/source/loads/general/pfluid/hm_preread_pfluid.F
      !||    hm_preread_pload                  ../starter/source/loads/general/pload/hm_preread_pload.F
      !||    hm_preread_properties             ../starter/source/properties/hm_preread_properties.F
      !||    hm_preread_radiation              ../starter/source/loads/thermic/hm_preread_radiation.F
      !||    hm_preread_rbe2                   ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_preread_rbe3                   ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
      !||    hm_preread_rbody                  ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    hm_preread_sphio                  ../starter/source/loads/sph/hm_preread_sphio.F
      !||    hm_read_activ                     ../starter/source/tools/activ/hm_read_activ.F
      !||    hm_read_admas                     ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_ale_grid                  ../starter/source/general_controls/ale_grid/hm_read_ale_grid.F
      !||    hm_read_ale_link                  ../starter/source/constraints/ale/hm_read_ale_link_vel.F
      !||    hm_read_ale_muscl                 ../starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
      !||    hm_read_ale_solver                ../starter/source/general_controls/ale_cfd/hm_read_ale_solver.F
      !||    hm_read_alebcs                    ../starter/source/constraints/ale/hm_read_alebcs.F
      !||    hm_read_analy                     ../starter/source/general_controls/computation/hm_read_analy.F
      !||    hm_read_bcs                       ../starter/source/constraints/general/bcs/hm_read_bcs.F
      !||    hm_read_bcs_wall                  ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_bem                       ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_box                       ../starter/source/model/box/hm_read_box.F
      !||    hm_read_caa                       ../starter/source/general_controls/computation/hm_read_caa.F
      !||    hm_read_cload                     ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_cluster                   ../starter/source/output/cluster/hm_read_cluster.F
      !||    hm_read_convec                    ../starter/source/loads/thermic/hm_read_convec.F
      !||    hm_read_cyljoint                  ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_damp                      ../starter/source/general_controls/damping/hm_read_damp.F
      !||    hm_read_definter                  ../starter/source/general_controls/default_values/hm_read_definter.F
      !||    hm_read_definter_type02           ../starter/source/general_controls/default_values/hm_read_definter_type02.F
      !||    hm_read_defshell                  ../starter/source/general_controls/default_values/hm_read_defshell.F
      !||    hm_read_defsolid                  ../starter/source/general_controls/default_values/hm_read_defsolid.F
      !||    hm_read_drape                     ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_eig                       ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eos                       ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_eref                      ../starter/source/loads/reference_state/eref/hm_read_eref.F
      !||    hm_read_fail                      ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_fail_alter                ../starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
      !||    hm_read_fail_biquad               ../starter/source/materials/fail/biquad/hm_read_fail_biquad.F
      !||    hm_read_fail_chang                ../starter/source/materials/fail/changchang/hm_read_fail_chang.F
      !||    hm_read_fail_connect              ../starter/source/materials/fail/connect/hm_read_fail_connect.F
      !||    hm_read_fail_emc                  ../starter/source/materials/fail/emc/hm_read_fail_emc.F
      !||    hm_read_fail_energy               ../starter/source/materials/fail/energy/hm_read_fail_energy.F
      !||    hm_read_fail_fabric               ../starter/source/materials/fail/fabric/hm_read_fail_fabric.F
      !||    hm_read_fail_fld                  ../starter/source/materials/fail/fld/hm_read_fail_fld.F
      !||    hm_read_fail_gene1                ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
      !||    hm_read_fail_gurson               ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
      !||    hm_read_fail_hashin               ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F
      !||    hm_read_fail_hc_dsse              ../starter/source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
      !||    hm_read_fail_hoffman              ../starter/source/materials/fail/hoffman/hm_read_fail_hoffman.F
      !||    hm_read_fail_inievo               ../starter/source/materials/fail/inievo/hm_read_fail_inievo.F
      !||    hm_read_fail_johnson              ../starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
      !||    hm_read_fail_ladeveze             ../starter/source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
      !||    hm_read_fail_maxstrain            ../starter/source/materials/fail/max_strain/hm_read_fail_maxstrain.F
      !||    hm_read_fail_nxt                  ../starter/source/materials/fail/nxt/hm_read_fail_nxt.F
      !||    hm_read_fail_orthbiquad           ../starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
      !||    hm_read_fail_orthenerg            ../starter/source/materials/fail/orthenerg/hm_read_fail_orthenerg.F
      !||    hm_read_fail_orthstrain           ../starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
      !||    hm_read_fail_puck                 ../starter/source/materials/fail/puck/hm_read_fail_puck.F
      !||    hm_read_fail_rtcl                 ../starter/source/materials/fail/rtcl/hm_read_fail_rtcl.F
      !||    hm_read_fail_sahraei              ../starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
      !||    hm_read_fail_snconnect            ../starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
      !||    hm_read_fail_spalling             ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F90
      !||    hm_read_fail_syazwan              ../starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
      !||    hm_read_fail_tab1                 ../starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
      !||    hm_read_fail_tab2                 ../starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
      !||    hm_read_fail_tab_old              ../starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
      !||    hm_read_fail_tbutcher             ../starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
      !||    hm_read_fail_tensstrain           ../starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
      !||    hm_read_fail_tsaihill             ../starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
      !||    hm_read_fail_tsaiwu               ../starter/source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
      !||    hm_read_fail_visual               ../starter/source/materials/fail/visual/hm_read_fail_visual.F
      !||    hm_read_fail_wierzbicki           ../starter/source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
      !||    hm_read_fail_wilkins              ../starter/source/materials/fail/wilkins/hm_read_fail_wilkins.F
      !||    hm_read_fractal_dmg               ../starter/source/materials/fail/fractal/hm_read_fractal_dmg.F90
      !||    hm_read_friction                  ../starter/source/interfaces/friction/reader/hm_read_friction.F
      !||    hm_read_friction_models           ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_friction_orientations     ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_frm                       ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_func2d                    ../starter/source/tools/curve/hm_read_func2d.F
      !||    hm_read_funct                     ../starter/source/tools/curve/hm_read_funct.F
      !||    hm_read_funct_python              ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    hm_read_fxb1                      ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_fxb2                      ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_gauge                     ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_gjoint                    ../starter/source/constraints/general/gjoint/hm_read_gjoint.F
      !||    hm_read_grav                      ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_grpart                    ../starter/source/groups/hm_read_grpart.F
      !||    hm_read_impacc                    ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_impflux                   ../starter/source/constraints/thermic/hm_read_impflux.F
      !||    hm_read_implicit                  ../starter/source/general_controls/computation/hm_read_implicit.F
      !||    hm_read_imptemp                   ../starter/source/constraints/thermic/hm_read_imptemp.F
      !||    hm_read_impvel                    ../starter/source/constraints/general/impvel/hm_read_impvel.F
      !||    hm_read_inicrack                  ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_inigrav                   ../starter/source/initial_conditions/inigrav/hm_read_inigrav.F
      !||    hm_read_inimap1d                  ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inimap2d                  ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    hm_read_inista                    ../starter/source/initial_conditions/inista/hm_read_inista.F
      !||    hm_read_inistate_d00              ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_initemp                   ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_inivel                    ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inivol                    ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
      !||    hm_read_inject1                   ../starter/source/properties/injector/hm_read_inject1.F
      !||    hm_read_inject2                   ../starter/source/properties/injector/hm_read_inject2.F
      !||    hm_read_interfaces                ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_intsub                    ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_ioflag                    ../starter/source/general_controls/inputoutput/hm_read_ioflag.F
      !||    hm_read_lagmul                    ../starter/source/tools/lagmul/hm_read_lagmul.F
      !||    hm_read_leak                      ../starter/source/airbag/hm_read_leak.F
      !||    hm_read_lines                     ../starter/source/groups/hm_read_lines.F
      !||    hm_read_link                      ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_load_centri               ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure             ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_mat                       ../starter/source/materials/mat/hm_read_mat.F90
      !||    hm_read_mat100                    ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat109                    ../starter/source/materials/mat/mat109/hm_read_mat109.F
      !||    hm_read_mat163                    ../starter/source/materials/mat/mat163/hm_read_mat163.F90
      !||    hm_read_mat36                     ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat38                     ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat40                     ../starter/source/materials/mat/mat040/hm_read_mat40.F
      !||    hm_read_mat48                     ../starter/source/materials/mat/mat048/hm_read_mat48.F
      !||    hm_read_mat63                     ../starter/source/materials/mat/mat063/hm_read_mat63.F
      !||    hm_read_mat66                     ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat71                     ../starter/source/materials/mat/mat071/hm_read_mat71.F
      !||    hm_read_mat76                     ../starter/source/materials/mat/mat076/hm_read_mat76.F
      !||    hm_read_mat87                     ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||    hm_read_mat95                     ../starter/source/materials/mat/mat095/hm_read_mat95.F
      !||    hm_read_matgas                    ../starter/source/materials/mat/matgas/hm_read_matgas.F
      !||    hm_read_merge                     ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_merge_node                ../starter/source/elements/reader/hm_read_merge_node.F
      !||    hm_read_move_funct                ../starter/source/tools/curve/hm_read_move_funct.F
      !||    hm_read_mpc                       ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_mpc0                      ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_mullins_or                ../starter/source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
      !||    hm_read_nbcs                      ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_node                      ../starter/source/elements/reader/hm_read_node.F
      !||    hm_read_nonlocal                  ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_part                      ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_pblast                    ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_pcyl                      ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_perturb                   ../starter/source/general_controls/computation/hm_read_perturb.F
      !||    hm_read_perturb_fail              ../starter/source/general_controls/computation/hm_read_perturb_fail.F
      !||    hm_read_perturb_part_shell        ../starter/source/general_controls/computation/hm_read_perturb_part_shell.F
      !||    hm_read_perturb_part_solid        ../starter/source/general_controls/computation/hm_read_perturb_part_solid.F
      !||    hm_read_pfluid                    ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_pload                     ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_prelecdrape               ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_preload                   ../starter/source/loads/general/preload/hm_read_preload.F
      !||    hm_read_preload_axial             ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_read_prethgrou                 ../starter/source/output/th/hm_read_prethgrou.F
      !||    hm_read_prop01                    ../starter/source/properties/shell/hm_read_prop01.F
      !||    hm_read_prop03                    ../starter/source/properties/beam/hm_read_prop03.F
      !||    hm_read_prop05                    ../starter/source/properties/rivet/hm_read_prop05.F
      !||    hm_read_prop08                    ../starter/source/properties/spring/hm_read_prop08.F
      !||    hm_read_prop18                    ../starter/source/properties/beam/hm_read_prop18.F
      !||    hm_read_prop27                    ../starter/source/properties/spring/hm_read_prop27.F
      !||    hm_read_properties                ../starter/source/properties/hm_read_properties.F
      !||    hm_read_radiation                 ../starter/source/loads/thermic/hm_read_radiation.F
      !||    hm_read_rand                      ../starter/source/general_controls/computation/hm_read_rand.F
      !||    hm_read_rbe2                      ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3                      ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                     ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul              ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_refsta                    ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    hm_read_retractor                 ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_rwall_cyl                 ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul              ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral               ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane               ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher               ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm               ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_sensors                   ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_skw                       ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_slipring                  ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_sms                       ../starter/source/general_controls/computation/hm_read_sms.F
      !||    hm_read_spcnd                     ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    hm_read_sphcel                    ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_sphglo                    ../starter/source/general_controls/computation/hm_read_sphglo.F
      !||    hm_read_sphio                     ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_read_spmd                      ../starter/source/general_controls/computation/hm_read_spmd.F
      !||    hm_read_submodel                  ../starter/source/model/assembling/hm_read_submodel.F
      !||    hm_read_subset                    ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf                      ../starter/source/groups/hm_read_surf.F
      !||    hm_read_surfsurf                  ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_table1                    ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_table1_0                  ../starter/source/tools/curve/hm_read_table1_0.F
      !||    hm_read_table1_1                  ../starter/source/tools/curve/hm_read_table1_1.F
      !||    hm_read_table2                    ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_table2_0                  ../starter/source/tools/curve/hm_read_table2_0.F
      !||    hm_read_table2_1                  ../starter/source/tools/curve/hm_read_table2_1.F
      !||    hm_read_therm                     ../starter/source/materials/therm/hm_read_therm.F
      !||    hm_read_therm_stress              ../starter/source/materials/therm/hm_read_therm_stress.F90
      !||    hm_read_thgrki                    ../starter/source/output/th/hm_read_thgrki.F
      !||    hm_read_thgrki_rbody              ../starter/source/output/th/hm_read_thgrki_rbody.F
      !||    hm_read_thgrne                    ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrns                    ../starter/source/output/th/hm_read_thgrns.F
      !||    hm_read_thgrou                    ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrpa                    ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrpa_sub                ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thpart                    ../starter/source/output/thpart/hm_read_thpart.F
      !||    hm_read_unit                      ../starter/source/general_controls/computation/hm_read_unit.F
      !||    hm_read_upwind                    ../starter/source/general_controls/computation/hm_read_upwind.F
      !||    hm_read_visc                      ../starter/source/materials/visc/hm_read_visc.F
      !||    hm_read_visc_lprony               ../starter/source/materials/visc/hm_read_visc_lprony.F
      !||    hm_read_visc_plas                 ../starter/source/materials/visc/hm_read_visc_plas.F90
      !||    hm_read_visc_prony                ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    hm_read_window_user               ../starter/source/tools/userwi/hm_read_window_user.F
      !||    hm_read_xref                      ../starter/source/loads/reference_state/xref/hm_read_xref.F
      !||    hm_set                            ../starter/source/model/sets/hm_set.F
      !||    hm_setfxrbyon                     ../starter/source/constraints/fxbody/hm_setfxrbyon.F
      !||    hm_submodpart                     ../starter/source/groups/hm_submodpart.F
      !||    hm_surfgr2                        ../starter/source/groups/hm_surfgr2.F
      !||    hm_surfnod                        ../starter/source/groups/hm_surfnod.F
      !||    hm_sz_r2r                         ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    hm_tagpart                        ../starter/source/groups/hm_tagpart.F
      !||    hm_tagpart2                       ../starter/source/groups/hm_tagpart2.F
      !||    hm_thgrki_vent                    ../starter/source/output/th/hm_thgrki_vent.F
      !||    hm_thvarc                         ../starter/source/output/th/hm_read_thvarc.F
      !||    hm_thvarvent                      ../starter/source/output/th/hm_thvarent.F
      !||    hm_yctrl                          ../starter/source/elements/initia/hm_yctrl.F
      !||    insert_clause_in_set              ../starter/source/model/sets/insert_clause_in_set.F
      !||    lecacc                            ../starter/source/tools/accele/lecacc.F
      !||    lecextlnk                         ../starter/source/coupling/rad2rad/lecextlnk.F
      !||    lecfill                           ../starter/source/elements/initia/lecfill.F
      !||    leclas                            ../starter/source/loads/laser/leclas.F
      !||    leclas1                           ../starter/source/loads/laser/leclas1.F
      !||    lecsec0                           ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec42                          ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                       ../starter/source/tools/sect/lecsec4bolt.F
      !||    lecstack_ply                      ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lecstamp                          ../starter/source/interfaces/interf1/lecstamp.F
      !||    lecsubmod                         ../starter/source/model/submodel/lecsubmod.F
      !||    lectrans                          ../starter/source/model/transformation/lectrans.F
      !||    lectranssub                       ../starter/source/model/submodel/lectranssub.F
      !||    lectur                            ../starter/source/starter/lectur.F
      !||    nbadigemesh                       ../starter/source/elements/ige3d/nbadigemesh.F
      !||    nbadmesh                          ../starter/source/model/remesh/nbadmesh.F
      !||    nbfunct                           ../starter/source/tools/curve/nbfunc.F
      !||    nbsph                             ../starter/source/elements/sph/nbsph.F
      !||    prelecdet                         ../starter/source/initial_conditions/detonation/prelecdet.F
      !||    preleclas                         ../starter/source/loads/laser/lpreleclas.F
      !||    prelecsec                         ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt                    ../starter/source/tools/sect/prelecsec4bolt.F
      !||    preread_rbody_lagmul              ../starter/source/constraints/general/rbody/preread_rbody_lagmul.F
      !||    preread_rbody_set                 ../starter/source/model/sets/preread_rbody_set.F
      !||    r2r_monvol                        ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec                        ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec_name                   ../starter/source/coupling/rad2rad/r2r_prelec_name.F
      !||    read_ale_mat                      ../starter/source/materials/ale/read_ale_mat.F
      !||    read_box_box                      ../starter/source/model/box/read_box_box.F
      !||    read_box_cyl                      ../starter/source/model/box/read_box_cyl.F
      !||    read_box_rect                     ../starter/source/model/box/read_box_rect.F
      !||    read_box_spher                    ../starter/source/model/box/read_box_spher.F
      !||    read_clause_box                   ../starter/source/model/sets/read_clause_box.F
      !||    read_detonators                   ../starter/source/initial_conditions/detonation/read_detonators.F
      !||    read_dfs_detcord                  ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline                  ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan                  ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint                 ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper              ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_ebcs                         ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    read_euler_mat                    ../starter/source/materials/ale/read_euler_mat.F
      !||    read_impdisp                      ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impdisp_fgeo                 ../starter/source/constraints/general/impvel/read_impdisp_fgeo.F
      !||    read_impvel                       ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_fgeo                  ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_impvel_lagmul                ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_material_models              ../starter/source/materials/read_material_models.F
      !||    read_monvol                       ../starter/source/airbag/read_monvol.F
      !||    read_rwall                        ../starter/source/constraints/general/rwall/read_rwall.F
      !||    read_sensor_acc                   ../starter/source/tools/sensor/read_sensor_acc.F
      !||    read_sensor_and                   ../starter/source/tools/sensor/read_sensor_and.F
      !||    read_sensor_contact               ../starter/source/tools/sensor/read_sensor_contact.F
      !||    read_sensor_disp                  ../starter/source/tools/sensor/read_sensor_disp.F
      !||    read_sensor_dist_surf             ../starter/source/tools/sensor/read_sensor_dist_surf.F
      !||    read_sensor_energy                ../starter/source/tools/sensor/read_sensor_energy.F
      !||    read_sensor_gauge                 ../starter/source/tools/sensor/read_sensor_gauge.F
      !||    read_sensor_hic                   ../starter/source/tools/sensor/read_sensor_hic.F
      !||    read_sensor_nic                   ../starter/source/tools/sensor/read_sensor_nic.F
      !||    read_sensor_not                   ../starter/source/tools/sensor/read_sensor_not.F
      !||    read_sensor_or                    ../starter/source/tools/sensor/read_sensor_or.F
      !||    read_sensor_python                ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||    read_sensor_rbody                 ../starter/source/tools/sensor/read_sensor_rbody.F
      !||    read_sensor_rwall                 ../starter/source/tools/sensor/read_sensor_rwall.F
      !||    read_sensor_sect                  ../starter/source/tools/sensor/read_sensor_sect.F
      !||    read_sensor_sens                  ../starter/source/tools/sensor/read_sensor_sens.F
      !||    read_sensor_temp                  ../starter/source/tools/sensor/read_sensor_temp.F
      !||    read_sensor_time                  ../starter/source/tools/sensor/read_sensor_time.F
      !||    read_sensor_vel                   ../starter/source/tools/sensor/read_sensor_vel.F
      !||    read_sensor_work                  ../starter/source/tools/sensor/read_sensor_work.F
      !||    sboxboxsurf                       ../starter/source/model/box/bigbox.F
      !||    set_admesh                        ../starter/source/model/remesh/set_admesh.F
      !||    setrb2on                          ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    setrbyon                          ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    sort_set                          ../starter/source/model/sets/sort_sets.F
      !||    split_ebcs                        ../starter/source/boundary_conditions/ebcs/split_ebcs.F
      !||    st_qaprint_set                    ../starter/source/output/qaprint/st_qaprint_set.F
      !||    state_admesh                      ../starter/source/model/remesh/state_admesh.F
      !||--- uses       -----------------------------------------------------
      !||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      MODULE HM_OPTION_READ_MOD
      INTERFACE
      SUBROUTINE HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID,UNIT_ID,SUBMODEL_INDEX,SUBMODEL_ID,OPTION_TITR,
     .                              KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4,OPT_POS)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OPTION_ID,UNIT_ID,SUBMODEL_INDEX,SUBMODEL_ID,OPT_POS
      CHARACTER*(*) KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4
      CHARACTER*(*) OPTION_TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      OPTIONAL :: OPTION_ID,UNIT_ID,SUBMODEL_INDEX,SUBMODEL_ID,KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4,OPTION_TITR,OPT_POS

      END SUBROUTINE
      END INTERFACE
C=======================================================================
      END MODULE 
