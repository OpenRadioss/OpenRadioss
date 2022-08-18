Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|-- called by -----------
Chd|        CHECK_ELTYP                   source/model/sets/check_eltyp.F
Chd|        CONTRBE2                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        CREATE_BOX_CLAUSE             source/model/sets/create_box_clause.F
Chd|        CREATE_ELT_BOX                source/model/sets/create_elt_box.F
Chd|        CREATE_ELT_CLAUSE             source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST_G             source/model/sets/create_element_clause.F
Chd|        CREATE_MAP_TABLES             source/model/sets/map_tables.F
Chd|        CREATE_NODE_BOX               source/model/sets/create_node_box.F
Chd|        CREATE_NODE_CLAUSE            source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_FROM_SEG          source/model/sets/create_node_from_seg.F
Chd|        CREATE_NODE_LIST              source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_LIST_G            source/model/sets/create_node_clause.F
Chd|        CREATE_PART_CLAUSE            source/model/sets/create_part_clause.F
Chd|        CREATE_PART_LIST              source/model/sets/create_part_clause.F
Chd|        CREATE_PART_LIST_G            source/model/sets/create_part_clause.F
Chd|        CREATE_RBODY_BOX              source/model/sets/create_rbody_box.F
Chd|        CREATE_RBODY_CLAUSE           source/model/sets/create_rbody_clause.F
Chd|        CREATE_RBODY_LIST             source/model/sets/create_rbody_clause.F
Chd|        CREATE_RBODY_LIST_G           source/model/sets/create_rbody_clause.F
Chd|        CREATE_SEG_CLAUSE             source/model/sets/create_seg_clause.F
Chd|        CREATE_SETCOL_ARRAY           source/model/sets/create_setcol_clause.F
Chd|        CREATE_SETCOL_LIST            source/model/sets/create_setcol_clause.F
Chd|        CREATE_SETCOL_LIST_G          source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_ARRAY              source/model/sets/create_set_clause.F
Chd|        CREATE_SET_LIST               source/model/sets/create_set_clause.F
Chd|        CREATE_SET_LIST_G             source/model/sets/create_set_clause.F
Chd|        CREATE_SUBM_CLAUSE            source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBM_LIST              source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBM_LIST_G            source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBS_CLAUSE            source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST              source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST_G            source/model/sets/create_subs_clause.F
Chd|        FILL_IGR                      source/model/sets/fill_igr.F  
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        GROUPS_GET_NENTITY            source/groups/groups_get_nentity.F
Chd|        HM_ADMLCNT                    source/groups/hm_admlistcnt.F 
Chd|        HM_ADMLIST                    source/groups/hm_admlist.F    
Chd|        HM_BIGSBOX                    source/groups/hm_bigsbox.F    
Chd|        HM_CONVERT_2D_ELEMENTS_SEATBELTsource/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
Chd|        HM_CONVERT_2D_ELEMENT_SEATBELTsource/devtools/hm_reader/hm_convert_2d_element_seatbelt.F
Chd|        HM_CONVERT_FAIL_TAB           source/devtools/hm_reader/hm_convert_fail_tab.F
Chd|        HM_COUNT_2D_ELEMENT_SEATBELT  source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
Chd|        HM_ELNGR                      source/groups/hm_elngr.F      
Chd|        HM_ELNGRR                     source/groups/hm_elngrr.F     
Chd|        HM_ELNGRS                     source/groups/hm_elngr.F      
Chd|        HM_GROGRO                     source/groups/hm_grogro.F     
Chd|        HM_GROGRONOD                  source/groups/hm_grogronod.F  
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_LINENGR                    source/groups/hm_linengr.F    
Chd|        HM_LINES_OF_LINES             source/groups/hm_lines_of_lines.F
Chd|        HM_PRELECGRNS                 source/groups/hm_prelecgrns.F 
Chd|        HM_PRELECJOI                  source/constraints/general/cyl_joint/hm_prelecjoi.F
Chd|        HM_PREREAD_BCSCYC             source/constraints/general/bcs/lecbcscyc.F
Chd|        HM_PREREAD_BEM                source/loads/bem/hm_read_bem.F
Chd|        HM_PREREAD_CLOAD              source/loads/general/cload/hm_preread_cload.F
Chd|        HM_PREREAD_CONVEC             source/loads/thermic/hm_preread_convec.F
Chd|        HM_PREREAD_EIG                source/general_controls/computation/hm_read_eig.F
Chd|        HM_PREREAD_GRAV               source/loads/general/grav/hm_preread_grav.F
Chd|        HM_PREREAD_IMPACC             source/constraints/general/impvel/hm_preread_impacc.F
Chd|        HM_PREREAD_IMPDISP            source/constraints/general/impvel/hm_preread_impdisp.F
Chd|        HM_PREREAD_IMPFLUX            source/constraints/thermic/hm_preread_impflux.F
Chd|        HM_PREREAD_IMPTEMP            source/constraints/thermic/hm_preread_imptemp.F
Chd|        HM_PREREAD_IMPVEL             source/constraints/general/impvel/hm_preread_impvel.F
Chd|        HM_PREREAD_IMPVEL0            source/constraints/general/impvel/hm_preread_impvel0.F
Chd|        HM_PREREAD_INIVEL             source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_PREREAD_LOAD_CENTRI        source/loads/general/load_centri/hm_preread_load_centri.F
Chd|        HM_PREREAD_LOAD_PRESSURE      source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|        HM_PREREAD_MERGE              source/constraints/general/merge/hm_preread_merge.F
Chd|        HM_PREREAD_NODE               source/elements/reader/hm_preread_node.F
Chd|        HM_PREREAD_PART               source/model/assembling/hm_read_part.F
Chd|        HM_PREREAD_PBLAST             source/loads/pblast/hm_preread_pblast.F
Chd|        HM_PREREAD_PFLUID             source/loads/general/pfluid/hm_preread_pfluid.F
Chd|        HM_PREREAD_PLOAD              source/loads/general/pload/hm_preread_pload.F
Chd|        HM_PREREAD_PROPERTIES         source/properties/hm_preread_properties.F
Chd|        HM_PREREAD_RADIATION          source/loads/thermic/hm_preread_radiation.F
Chd|        HM_PREREAD_RBE2               source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_PREREAD_RBE3               source/constraints/general/rbe3/hm_preread_rbe3.F
Chd|        HM_PREREAD_RBODY              source/constraints/general/rbody/hm_preread_rbody.F
Chd|        HM_PREREAD_SPHIO              source/loads/sph/hm_preread_sphio.F
Chd|        HM_PRE_READ_LINK              source/constraints/rigidlink/hm_pre_read_rlink.F
Chd|        HM_PRE_READ_PRELOAD           source/loads/general/preload/hm_pre_read_preload.F
Chd|        HM_READ_ACTIV                 source/tools/activ/hm_read_activ.F
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_ALEBCS                source/constraints/ale/hm_read_alebcs.F
Chd|        HM_READ_ALE_GRID              source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|        HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_ALE_MUSCL             source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_ALE_SOLVER            source/general_controls/ale_cfd/hm_read_ale_solver.F
Chd|        HM_READ_ANALY                 source/general_controls/computation/hm_read_analy.F
Chd|        HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|        HM_READ_CAA                   source/general_controls/computation/hm_read_caa.F
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|        HM_READ_DEFINTER_TYPE02       source/general_controls/default_values/hm_read_definter_type02.F
Chd|        HM_READ_DEFSHELL              source/general_controls/default_values/hm_read_defshell.F
Chd|        HM_READ_DEFSOLID              source/general_controls/default_values/hm_read_defsolid.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FAIL_ALTER            source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|        HM_READ_FAIL_BIQUAD           source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|        HM_READ_FAIL_CHANG            source/materials/fail/changchang/hm_read_fail_chang.F
Chd|        HM_READ_FAIL_CONNECT          source/materials/fail/connect/hm_read_fail_connect.F
Chd|        HM_READ_FAIL_EMC              source/materials/fail/emc/hm_read_fail_emc.F
Chd|        HM_READ_FAIL_ENERGY           source/materials/fail/energy/hm_read_fail_energy.F
Chd|        HM_READ_FAIL_FABRIC           source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|        HM_READ_FAIL_FLD              source/materials/fail/fld/hm_read_fail_fld.F
Chd|        HM_READ_FAIL_GENE1            source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|        HM_READ_FAIL_GURSON           source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_FAIL_HASHIN           source/materials/fail/hashin/hm_read_fail_hashin.F
Chd|        HM_READ_FAIL_HC_DSSE          source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
Chd|        HM_READ_FAIL_JOHNSON          source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|        HM_READ_FAIL_LADEVEZE         source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
Chd|        HM_READ_FAIL_NXT              source/materials/fail/nxt/hm_read_fail_nxt.F
Chd|        HM_READ_FAIL_ORTHBIQUAD       source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
Chd|        HM_READ_FAIL_ORTHSTRAIN       source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
Chd|        HM_READ_FAIL_PUCK             source/materials/fail/puck/hm_read_fail_puck.F
Chd|        HM_READ_FAIL_RTCL             source/materials/fail/rtcl/hm_read_fail_rtcl.F
Chd|        HM_READ_FAIL_SAHRAEI          source/materials/fail/sahraei/hm_read_fail_sahraei.F
Chd|        HM_READ_FAIL_SNCONNECT        source/materials/fail/snconnect/hm_read_fail_snconnect.F
Chd|        HM_READ_FAIL_SPALLING         source/materials/fail/spalling/hm_read_fail_spalling.F
Chd|        HM_READ_FAIL_TAB1             source/materials/fail/tabulated/hm_read_fail_tab1.F
Chd|        HM_READ_FAIL_TAB_OLD          source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|        HM_READ_FAIL_TBUTCHER         source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
Chd|        HM_READ_FAIL_TENSSTRAIN       source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
Chd|        HM_READ_FAIL_VISUAL           source/materials/fail/visual/hm_read_fail_visual.F
Chd|        HM_READ_FAIL_WIERZBICKI       source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
Chd|        HM_READ_FAIL_WILKINS          source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|        HM_READ_FRICTION              source/interfaces/friction/reader/hm_read_friction.F
Chd|        HM_READ_FRICTION_MODELS       source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FUNC2D                source/tools/curve/hm_read_func2d.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_FXB2                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPLICIT              source/general_controls/computation/hm_read_implicit.F
Chd|        HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|        HM_READ_INICRACK              source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|        HM_READ_INIGRAV               source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INISTA                source/initial_conditions/inista/hm_read_inista.F
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_IOFLAG                source/general_controls/inputoutput/hm_read_ioflag.F
Chd|        HM_READ_LAGMUL                source/tools/lagmul/hm_read_lagmul.F
Chd|        HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT109                source/materials/mat/mat109/hm_read_mat109.F
Chd|        HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT40                 source/materials/mat/mat040/hm_read_mat40.F
Chd|        HM_READ_MAT48                 source/materials/mat/mat048/hm_read_mat48.F
Chd|        HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT63                 source/materials/mat/mat063/hm_read_mat63.F
Chd|        HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT71                 source/materials/mat/mat071/hm_read_mat71.F
Chd|        HM_READ_MAT76                 source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT81                 source/materials/mat/mat081/hm_read_mat81.F
Chd|        HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT95                 source/materials/mat/mat095/hm_read_mat95.F
Chd|        HM_READ_MATGAS                source/materials/mat/matgas/hm_read_matgas.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MERGE_NODE            source/elements/reader/hm_read_merge_node.F
Chd|        HM_READ_MOVE_FUNCT            source/tools/curve/hm_read_move_funct.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_MPC0                  source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_MULLINS_OR            source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|        HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PERTURB               source/general_controls/computation/hm_read_perturb.F
Chd|        HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PERTURB_PART_SHELL    source/general_controls/computation/hm_read_perturb_part_shell.F
Chd|        HM_READ_PERTURB_PART_SOLID    source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PRELECDRAPE           source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_PRETHGROU             source/output/th/hm_read_prethgrou.F
Chd|        HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
Chd|        HM_READ_PROP03                source/properties/beam/hm_read_prop03.F
Chd|        HM_READ_PROP05                source/properties/rivet/hm_read_prop05.F
Chd|        HM_READ_PROP08                source/properties/spring/hm_read_prop08.F
Chd|        HM_READ_PROP18                source/properties/beam/hm_read_prop18.F
Chd|        HM_READ_PROP27                source/properties/spring/hm_read_prop27.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_RADIATION             source/loads/thermic/hm_read_radiation.F
Chd|        HM_READ_RAND                  source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_REFSTA                source/loads/reference_state/refsta/hm_read_refsta.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SMS                   source/general_controls/computation/hm_read_sms.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHGLO                source/general_controls/computation/hm_read_sphglo.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_SPMD                  source/general_controls/computation/hm_read_spmd.F
Chd|        HM_READ_SUBMODEL              source/model/assembling/hm_read_submodel.F
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_SURFSURF              source/groups/hm_read_surfsurf.F
Chd|        HM_READ_TABLE1                source/tools/curve/hm_read_table.F
Chd|        HM_READ_TABLE1_0              source/tools/curve/hm_read_table1_0.F
Chd|        HM_READ_TABLE1_1              source/tools/curve/hm_read_table1_1.F
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|        HM_READ_TABLE2_0              source/tools/curve/hm_read_table2_0.F
Chd|        HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|        HM_READ_THERM                 source/materials/therm/hm_read_therm.F
Chd|        HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|        HM_READ_THGRKI                source/output/th/hm_read_thgrki.F
Chd|        HM_READ_THGRKI_RBODY          source/output/th/hm_read_thgrki_rbody.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_READ_THGRPA                source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRPA_SUB            source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|        HM_READ_UPWIND                source/general_controls/computation/hm_read_upwind.F
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        HM_SUBMODPART                 source/groups/hm_submodpart.F 
Chd|        HM_SURFGR2                    source/groups/hm_surfgr2.F    
Chd|        HM_SURFNOD                    source/groups/hm_surfnod.F    
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        HM_TAGPART                    source/groups/hm_tagpart.F    
Chd|        HM_TAGPART2                   source/groups/hm_tagpart2.F   
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        HM_THVARVENT                  source/output/th/hm_thvarent.F
Chd|        HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LECLAS                        source/loads/laser/leclas.F   
Chd|        LECLAS1                       source/loads/laser/leclas1.F  
Chd|        LECSEC0                       source/tools/sect/hm_read_sect.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LECSUBMOD                     source/model/submodel/lecsubmod.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        NBFUNCT                       source/tools/curve/nbfunc.F   
Chd|        NBSPH                         source/elements/sph/nbsph.F   
Chd|        PRELECDET                     source/initial_conditions/detonation/prelecdet.F
Chd|        PRELECLAS                     source/loads/laser/lpreleclas.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|        PREREAD_RBODY_LAGMUL          source/constraints/general/rbody/preread_rbody_lagmul.F
Chd|        PREREAD_RBODY_SET             source/model/sets/preread_rbody_set.F
Chd|        R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC_NAME               source/coupling/rad2rad/r2r_prelec_name.F
Chd|        READ_BOX_BOX                  source/model/box/read_box_box.F
Chd|        READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                source/model/box/read_box_spher.F
Chd|        READ_CLAUSE_BOX               source/model/sets/read_clause_box.F
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|        READ_RWALL                    source/constraints/general/rwall/read_rwall.F
Chd|        READ_SENSOR_ACC               source/tools/sensor/read_sensor_acc.F
Chd|        READ_SENSOR_AND               source/tools/sensor/read_sensor_and.F
Chd|        READ_SENSOR_CONTACT           source/tools/sensor/read_sensor_contact.F
Chd|        READ_SENSOR_DISP              source/tools/sensor/read_sensor_disp.F
Chd|        READ_SENSOR_DIST_SURF         source/tools/sensor/read_sensor_dist_surf.F
Chd|        READ_SENSOR_ENERGY            source/tools/sensor/read_sensor_energy.F
Chd|        READ_SENSOR_GAUGE             source/tools/sensor/read_sensor_gauge.F
Chd|        READ_SENSOR_HIC               source/tools/sensor/read_sensor_hic.F
Chd|        READ_SENSOR_NIC               source/tools/sensor/read_sensor_nic.F
Chd|        READ_SENSOR_NOT               source/tools/sensor/read_sensor_not.F
Chd|        READ_SENSOR_OR                source/tools/sensor/read_sensor_or.F
Chd|        READ_SENSOR_RBODY             source/tools/sensor/read_sensor_rbody.F
Chd|        READ_SENSOR_RWALL             source/tools/sensor/read_sensor_rwall.F
Chd|        READ_SENSOR_SECT              source/tools/sensor/read_sensor_sect.F
Chd|        READ_SENSOR_SENS              source/tools/sensor/read_sensor_sens.F
Chd|        READ_SENSOR_TEMP              source/tools/sensor/read_sensor_temp.F
Chd|        READ_SENSOR_TIME              source/tools/sensor/read_sensor_time.F
Chd|        READ_SENSOR_VEL               source/tools/sensor/read_sensor_vel.F
Chd|        READ_SENSOR_WORK              source/tools/sensor/read_sensor_work.F
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|        SETRB2ON                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SETRBYON                      source/constraints/general/rbody/hm_read_rbody.F
Chd|        SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|        STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|        ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE HM_OPTION_READ_MOD
      INTERFACE
      SUBROUTINE HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID,UNIT_ID,SUBMODEL_INDEX,SUBMODEL_ID,OPTION_TITR,
     .                              KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4,OPT_POS)  
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchar_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OPTION_ID,UNIT_ID,SUBMODEL_INDEX,SUBMODEL_ID,OPT_POS
      CHARACTER*(*) KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4
      CHARACTER*(*) OPTION_TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      OPTIONAL ::
     .   OPTION_ID,UNIT_ID,SUBMODEL_INDEX,SUBMODEL_ID,KEYWORD1,KEYWORD2,KEYWORD3,KEYWORD4,OPTION_TITR,OPT_POS

      END SUBROUTINE
      END INTERFACE
C=======================================================================
      END MODULE 
