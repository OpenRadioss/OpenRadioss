Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  ANCMSG                        source/output/message/message.F
Chd|-- called by -----------
Chd|        ADDMASPART                    source/tools/admas/addmaspart.F
Chd|        ADMBCS                        source/model/remesh/admbcs.F  
Chd|        ADMLCNT                       source/model/remesh/admlist.F 
Chd|        ADMLIST                       source/model/remesh/admlist.F 
Chd|        ALE_CHECK_LAG                 source/ale/ale_check_lag.F    
Chd|        ALE_EULER_INIT                source/materials/ale/ale_euler_init.F
Chd|        ALLOCBUF_AUTO                 source/elements/elbuf_init/allocbuf_auto.F
Chd|        ARRET_MESSAGE                 source/interfaces/int24/arret_message.F
Chd|        ARRET_MESSAGE_SLID            source/interfaces/int24/arret_message.F
Chd|        AUTO_NODE_MERGE               source/elements/nodes/auto_node_merge.F
Chd|        BCSCYCMODIF_ND                source/elements/solid/solide10/dim_s10edg.F
Chd|        BCSMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|        BIGBOX                        source/model/box/bigbox.F     
Chd|        BIGBOX2                       source/model/box/bigbox.F     
Chd|        BIGSBOX                       source/model/box/bigbox.F     
Chd|        BOXBOX                        source/model/box/boxbox.F     
Chd|        BOXBOX2                       source/model/box/boxbox.F     
Chd|        BOXBOXS                       source/model/box/boxbox.F     
Chd|        BSIGINI                       source/elements/beam/bsigini.F
Chd|        BUILD_ADMESH                  source/model/remesh/build_admesh.F
Chd|        BUSERINI                      source/elements/beam/buserini.F
Chd|        C3COORI                       source/elements/sh3n/coque3n/c3coori.F
Chd|        C3EPSCHK                      source/elements/sh3n/coque3n/c3init3.F
Chd|        C3GRHEAD                      source/elements/sh3n/coque3n/c3grhead.F
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CCOORI                        source/elements/shell/coque/ccoori.F
Chd|        CDK6INX                       source/elements/sh3n/coquedk6/cdk6inx.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CEPSCHK                       source/elements/shell/coque/cepsini.F
Chd|        CGRHEAD                       source/elements/shell/coque/cgrhead.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        CHECKMP                       source/elements/initia/initia.F
Chd|        CHECKRBY                      source/constraints/general/rbody/checkrby.F
Chd|        CHECK_DYNAIN                  source/starter/check_dynain.F 
Chd|        CHECK_ELTYP                   source/model/sets/check_eltyp.F
Chd|        CHECK_SURF                    source/groups/check_surf.F    
Chd|        CHEKMP2                       source/elements/initia/initia.F
Chd|        CHKFUNCT                      source/tools/curve/lecfun.F   
Chd|        CHKTYP2                       source/interfaces/interf1/chktyp2.F
Chd|        CHK_DTTSH                     source/elements/thickshell/solidec/scdtchk3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|        CM27IN3                       source/materials/mat/mat027/cm27in3.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|        CMATINI4                      source/materials/mat_share/cmatini4.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        CORTHINI                      source/elements/shell/coque/corthini.F
Chd|        CREATE_CHILD                  source/coupling/rad2rad/r2r_fork.F
Chd|        CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|        CREATE_MAP_TABLES             source/model/sets/map_tables.F
Chd|        CREATE_NODE_LIST              source/model/sets/create_node_clause.F
Chd|        CREATE_PART_CLAUSE            source/model/sets/create_part_clause.F
Chd|        CREATE_PART_LIST              source/model/sets/create_part_clause.F
Chd|        CREATE_RBODY_LIST             source/model/sets/create_rbody_clause.F
Chd|        CREATE_SEATBELT               source/tools/seatbelts/create_seatbelt.F
Chd|        CREATE_SEG_CLAUSE             source/model/sets/create_seg_clause.F
Chd|        CREATE_SET_LIST               source/model/sets/create_set_clause.F
Chd|        CREATE_SUBM_LIST              source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBS_LIST              source/model/sets/create_subs_clause.F
Chd|        CSIGINI                       source/elements/shell/coque/csigini.F
Chd|        CSIGINI4                      source/elements/shell/coqueba/scigini4.F
Chd|        CSTRAINI4                     source/elements/shell/coqueba/cstraini4.F
Chd|        CUSERINI4                     source/elements/shell/coqueba/cuserini4.F
Chd|        DEFBEAM_SECT                  source/properties/beam/hm_read_prop18.F
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        DOMETIS2                      source/spmd/domain_decomposition/domdecs.F
Chd|        ECRAN1                        source/initial_conditions/detonation/ecran1.F
Chd|        ECRSUB2                       source/model/assembling/hm_read_subset.F
Chd|        EIGRLINE                      source/general_controls/computation/hm_read_eig.F
Chd|        ELSTAGBOXBOX                  source/model/box/boxbox.F     
Chd|        ERROR_ALLOC                   source/system/error_alloc.F   
Chd|        FACEBOX                       source/model/box/bigbox.F     
Chd|        FILL_BUFFER_51                source/materials/mat/mat051/fill_buffer_51.F
Chd|        FIND_DT1BRICK_ENGINE          source/starter/contrl.F       
Chd|        FIND_YIELD_ENGINE             source/general_controls/computation/hm_read_sms.F
Chd|        FIXMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|        FREDEC0                       source/starter/freform.F      
Chd|        FREDEC5                       source/starter/freform.F      
Chd|        FREDEC_2KEY_4ID               source/starter/freform.F      
Chd|        FREDEC_2KEY_4ID_T             source/starter/freform.F      
Chd|        FREDEC_2KEY_ID_OR_KEY_ID      source/starter/freform.F      
Chd|        FREDEC_KEY_3ID_T              source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|        FVBRIC0                       source/airbag/fvbric0.F       
Chd|        FVBRIC1                       source/airbag/fvbric1.F       
Chd|        FVINJECT                      source/airbag/fvinject.F      
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVVENTHOLE                    source/airbag/fvventhole.F    
Chd|        FVVERIF                       source/airbag/fvmbag1.F       
Chd|        FXRLINE                       source/constraints/fxbody/hm_read_fxb.F
Chd|        HIREORBE3                     source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_ADMLCNT                    source/groups/hm_admlistcnt.F 
Chd|        HM_ADMLIST                    source/groups/hm_admlist.F    
Chd|        HM_BIGBOX                     source/model/box/hm_bigbox.F  
Chd|        HM_BIGBOX2                    source/model/box/hm_bigbox2.F 
Chd|        HM_BIGSBOX                    source/groups/hm_bigsbox.F    
Chd|        HM_ELNGR                      source/groups/hm_elngr.F      
Chd|        HM_ELNGRR                     source/groups/hm_elngrr.F     
Chd|        HM_ELNGRS                     source/groups/hm_elngr.F      
Chd|        HM_GROGRO                     source/groups/hm_grogro.F     
Chd|        HM_GROGRONOD                  source/groups/hm_grogronod.F  
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_LINENGR                    source/groups/hm_linengr.F    
Chd|        HM_LINES_OF_LINES             source/groups/hm_lines_of_lines.F
Chd|        HM_MESSAGES                   source/devtools/hm_reader/hm_messages.F
Chd|        HM_PRELCE16S                  source/elements/reader/hm_read_solid.F
Chd|        HM_PREREAD_BCSCYC             source/constraints/general/bcs/lecbcscyc.F
Chd|        HM_PREREAD_BEM                source/loads/bem/hm_read_bem.F
Chd|        HM_PREREAD_EIG                source/general_controls/computation/hm_read_eig.F
Chd|        HM_PREREAD_IMPDISP            source/constraints/general/impvel/hm_preread_impdisp.F
Chd|        HM_PREREAD_IMPTEMP            source/constraints/thermic/hm_preread_imptemp.F
Chd|        HM_PREREAD_IMPVEL             source/constraints/general/impvel/hm_preread_impvel.F
Chd|        HM_PREREAD_LOAD_PRESSURE      source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|        HM_PREREAD_NODE               source/elements/reader/hm_preread_node.F
Chd|        HM_PREREAD_PFLUID             source/loads/general/pfluid/hm_preread_pfluid.F
Chd|        HM_PREREAD_XELEM              source/elements/reader/hm_preread_xelem.F
Chd|        HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
Chd|        HM_READ_ACTIV                 source/tools/activ/hm_read_activ.F
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_ALEBCS                source/constraints/ale/hm_read_alebcs.F
Chd|        HM_READ_ALE_GRID              source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|        HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_ALE_MUSCL             source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_ALE_SOLVER            source/general_controls/ale_cfd/hm_read_ale_solver.F
Chd|        HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_BEAM                  source/elements/reader/hm_read_beam.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_EBCS_FLUXOUT          source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
Chd|        HM_READ_EBCS_GRADP0           source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
Chd|        HM_READ_EBCS_INIP             source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
Chd|        HM_READ_EBCS_INIV             source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
Chd|        HM_READ_EBCS_INLET            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        HM_READ_EBCS_MONVOL           source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
Chd|        HM_READ_EBCS_NORMV            source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
Chd|        HM_READ_EBCS_NRF              source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
Chd|        HM_READ_EBCS_PRES             source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
Chd|        HM_READ_EBCS_VALVIN           source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
Chd|        HM_READ_EBCS_VALVOUT          source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
Chd|        HM_READ_EBCS_VEL              source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|        HM_READ_EOS_COMPACTION        source/materials/eos/hm_read_eos_compaction.F
Chd|        HM_READ_EOS_IDEAL_GAS         source/materials/eos/hm_read_eos_ideal_gas.F
Chd|        HM_READ_EOS_IDEAL_GAS_VT      source/materials/eos/hm_read_eos_ideal_gas_vt.F
Chd|        HM_READ_EOS_LSZK              source/materials/eos/hm_read_eos_lszk.F
Chd|        HM_READ_EOS_MURNAGHAN         source/materials/eos/hm_read_eos_murnaghan.F
Chd|        HM_READ_EOS_NASG              source/materials/eos/hm_read_eos_nasg.F
Chd|        HM_READ_EOS_OSBORNE           source/materials/eos/hm_read_eos_osborne.F
Chd|        HM_READ_EOS_SESAME            source/materials/eos/hm_read_eos_sesame.F
Chd|        HM_READ_EOS_STIFFENED_GAS     source/materials/eos/hm_read_eos_stiffened_gas.F
Chd|        HM_READ_EOS_TABULATED         source/materials/eos/hm_read_eos_tabulated.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FAIL_ENERGY           source/materials/fail/energy/hm_read_fail_energy.F
Chd|        HM_READ_FAIL_FABRIC           source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|        HM_READ_FAIL_FLD              source/materials/fail/fld/hm_read_fail_fld.F
Chd|        HM_READ_FAIL_GENE1            source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|        HM_READ_FAIL_GURSON           source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_FAIL_INIEVO           source/materials/fail/inievo/hm_read_fail_inievo.F
Chd|        HM_READ_FAIL_JOHNSON          source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|        HM_READ_FAIL_ORTHBIQUAD       source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
Chd|        HM_READ_FAIL_SPALLING         source/materials/fail/spalling/hm_read_fail_spalling.F
Chd|        HM_READ_FAIL_SYAZWAN          source/materials/fail/syazwan/hm_read_fail_syazwan.F
Chd|        HM_READ_FAIL_TAB1             source/materials/fail/tabulated/hm_read_fail_tab1.F
Chd|        HM_READ_FAIL_TAB2             source/materials/fail/tabulated/hm_read_fail_tab2.F
Chd|        HM_READ_FAIL_TAB_OLD          source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|        HM_READ_FAIL_TBUTCHER         source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
Chd|        HM_READ_FAIL_TENSSTRAIN       source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
Chd|        HM_READ_FAIL_USER             source/materials/fail/failuser/hm_read_fail_user.F
Chd|        HM_READ_FRICTION              source/interfaces/friction/reader/hm_read_friction.F
Chd|        HM_READ_FRICTION_MODELS       source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_FXB2                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_INIGRAV               source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_LAGDT_TYPE07    source/interfaces/int07/hm_read_inter_lagdt_type07.F
Chd|        HM_READ_INTER_LAGMUL          source/interfaces/reader/hm_read_inter_lagmul.F
Chd|        HM_READ_INTER_LAGMUL_TYPE02   source/interfaces/int02/hm_read_inter_lagmul_type02.F
Chd|        HM_READ_INTER_LAGMUL_TYPE07   source/interfaces/int07/hm_read_inter_lagmul_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE16   source/interfaces/int16/hm_read_inter_lagmul_type16.F
Chd|        HM_READ_INTER_LAGMUL_TYPE17   source/interfaces/int17/hm_read_inter_lagmul_type17.F
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_INTER_TYPE02          source/interfaces/int02/hm_read_inter_type02.F
Chd|        HM_READ_INTER_TYPE05          source/interfaces/int05/hm_read_inter_type05.F
Chd|        HM_READ_INTER_TYPE06          source/interfaces/int06/hm_read_inter_type06.F
Chd|        HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE08          source/interfaces/int08/hm_read_inter_type08.F
Chd|        HM_READ_INTER_TYPE10          source/interfaces/int10/hm_read_inter_type10.F
Chd|        HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE14          source/interfaces/int14/hm_read_inter_type14.F
Chd|        HM_READ_INTER_TYPE15          source/interfaces/int15/hm_read_inter_type15.F
Chd|        HM_READ_INTER_TYPE18          source/interfaces/int18/hm_read_inter_type18.F
Chd|        HM_READ_INTER_TYPE20          source/interfaces/int20/hm_read_inter_type20.F
Chd|        HM_READ_INTER_TYPE21          source/interfaces/int21/hm_read_inter_type21.F
Chd|        HM_READ_INTER_TYPE22          source/interfaces/int22/hm_read_inter_type22.F
Chd|        HM_READ_INTER_TYPE23          source/interfaces/int23/hm_read_inter_type23.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT01                 source/materials/mat/mat001/hm_read_mat01.F
Chd|        HM_READ_MAT02                 source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MAT03                 source/materials/mat/mat003/hm_read_mat03.F
Chd|        HM_READ_MAT04                 source/materials/mat/mat004/hm_read_mat04.F
Chd|        HM_READ_MAT10                 source/materials/mat/mat010/hm_read_mat10.F
Chd|        HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT102                source/materials/mat/mat102/hm_read_mat102.F
Chd|        HM_READ_MAT103                source/materials/mat/mat103/hm_read_mat103.F
Chd|        HM_READ_MAT104                source/materials/mat/mat104/hm_read_mat104.F
Chd|        HM_READ_MAT106                source/materials/mat/mat106/hm_read_mat106.F
Chd|        HM_READ_MAT107                source/materials/mat/mat107/hm_read_mat107.F
Chd|        HM_READ_MAT108                source/materials/mat/mat108/hm_read_mat108.F
Chd|        HM_READ_MAT11                 source/materials/mat/mat011/hm_read_mat11.F
Chd|        HM_READ_MAT110                source/materials/mat/mat110/hm_read_mat110.F
Chd|        HM_READ_MAT111                source/materials/mat/mat111/hm_read_mat111.F
Chd|        HM_READ_MAT112                source/materials/mat/mat112/hm_read_mat112.F
Chd|        HM_READ_MAT113                source/materials/mat/mat113/hm_read_mat113.F
Chd|        HM_READ_MAT114                source/materials/mat/mat114/hm_read_mat114.F
Chd|        HM_READ_MAT115                source/materials/mat/mat115/hm_read_mat115.F
Chd|        HM_READ_MAT116                source/materials/mat/mat116/hm_read_mat116.F
Chd|        HM_READ_MAT119                source/materials/mat/mat119/hm_read_mat119.F
Chd|        HM_READ_MAT11_K_EPS           source/materials/mat/mat011/hm_read_mat11_k_eps.F
Chd|        HM_READ_MAT12                 source/materials/mat/mat012/hm_read_mat12.F
Chd|        HM_READ_MAT121                source/materials/mat/mat121/hm_read_mat121.F
Chd|        HM_READ_MAT124                source/materials/mat/mat124/hm_read_mat124.F
Chd|        HM_READ_MAT13                 source/materials/mat/mat013/hm_read_mat13.F
Chd|        HM_READ_MAT14                 source/materials/mat/mat014/hm_read_mat14.F
Chd|        HM_READ_MAT15                 source/materials/mat/mat015/hm_read_mat15.F
Chd|        HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|        HM_READ_MAT16                 source/materials/mat/mat016/hm_read_mat16.F
Chd|        HM_READ_MAT19                 source/materials/mat/mat019/hm_read_mat19.F
Chd|        HM_READ_MAT20                 source/materials/mat/mat020/hm_read_mat20.F
Chd|        HM_READ_MAT21                 source/materials/mat/mat021/hm_read_mat21.F
Chd|        HM_READ_MAT22                 source/materials/mat/mat022/hm_read_mat22.F
Chd|        HM_READ_MAT23                 source/materials/mat/mat023/hm_read_mat23.F
Chd|        HM_READ_MAT24                 source/materials/mat/mat024/hm_read_mat24.F
Chd|        HM_READ_MAT25                 source/materials/mat/mat025/hm_read_mat25.F
Chd|        HM_READ_MAT26                 source/materials/mat/mat026/hm_read_mat26.F
Chd|        HM_READ_MAT27                 source/materials/mat/mat027/hm_read_mat27.F
Chd|        HM_READ_MAT29_31              source/materials/mat/matuser/hm_read_mat_user29_31.F
Chd|        HM_READ_MAT32                 source/materials/mat/mat032/hm_read_mat32.F
Chd|        HM_READ_MAT33                 source/materials/mat/mat033/hm_read_mat33.F
Chd|        HM_READ_MAT35                 source/materials/mat/mat035/hm_read_mat35.F
Chd|        HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT37                 source/materials/mat/mat037/hm_read_mat37.F
Chd|        HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT40                 source/materials/mat/mat040/hm_read_mat40.F
Chd|        HM_READ_MAT42                 source/materials/mat/mat042/hm_read_mat42.F
Chd|        HM_READ_MAT43                 source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT44                 source/materials/mat/mat044/hm_read_mat44.F
Chd|        HM_READ_MAT48                 source/materials/mat/mat048/hm_read_mat48.F
Chd|        HM_READ_MAT49                 source/materials/mat/mat049/hm_read_mat49.F
Chd|        HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_MAT52                 source/materials/mat/mat052/hm_read_mat52.F
Chd|        HM_READ_MAT54                 source/materials/mat/mat054/hm_read_mat54.F
Chd|        HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT58                 source/materials/mat/mat058/hm_read_mat58.F
Chd|        HM_READ_MAT59                 source/materials/mat/mat059/hm_read_mat59.F
Chd|        HM_READ_MAT60                 source/materials/mat/mat060/hm_read_mat60.F
Chd|        HM_READ_MAT62                 source/materials/mat/mat062/hm_read_mat62.F
Chd|        HM_READ_MAT63                 source/materials/mat/mat063/hm_read_mat63.F
Chd|        HM_READ_MAT65                 source/materials/mat/mat065/hm_read_mat65.F
Chd|        HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT69                 source/materials/mat/mat069/hm_read_mat69.F
Chd|        HM_READ_MAT70                 source/materials/mat/mat070/hm_read_mat70.F
Chd|        HM_READ_MAT71                 source/materials/mat/mat071/hm_read_mat71.F
Chd|        HM_READ_MAT72                 source/materials/mat/mat072/hm_read_mat72.F
Chd|        HM_READ_MAT73                 source/materials/mat/mat073/hm_read_mat73.F
Chd|        HM_READ_MAT74                 source/materials/mat/mat074/hm_read_mat74.F
Chd|        HM_READ_MAT75                 source/materials/mat/mat075/hm_read_mat75.F
Chd|        HM_READ_MAT76                 source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT77                 source/materials/mat/mat077/hm_read_mat77.F
Chd|        HM_READ_MAT78                 source/materials/mat/mat078/hm_read_mat78.F
Chd|        HM_READ_MAT79                 source/materials/mat/mat079/hm_read_mat79.F
Chd|        HM_READ_MAT80                 source/materials/mat/mat080/hm_read_mat80.F
Chd|        HM_READ_MAT81                 source/materials/mat/mat081/hm_read_mat81.F
Chd|        HM_READ_MAT82                 source/materials/mat/mat082/hm_read_mat82.F
Chd|        HM_READ_MAT83                 source/materials/mat/mat083/hm_read_law83.F
Chd|        HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT93                 source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MAT97                 source/materials/mat/mat097/hm_read_mat97.F
Chd|        HM_READ_MATGAS                source/materials/mat/matgas/hm_read_matgas.F
Chd|        HM_READ_MAT_99                source/materials/mat/matuser/hm_read_mat_user_99.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MERGE_NODE            source/elements/reader/hm_read_merge_node.F
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|        HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PERTURB_PART_SHELL    source/general_controls/computation/hm_read_perturb_part_shell.F
Chd|        HM_READ_PERTURB_PART_SOLID    source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
Chd|        HM_READ_PROP02                source/properties/truss/hm_read_prop02.F
Chd|        HM_READ_PROP03                source/properties/beam/hm_read_prop03.F
Chd|        HM_READ_PROP04                source/properties/spring/hm_read_prop04.F
Chd|        HM_READ_PROP06                source/properties/solid/hm_read_prop06.F
Chd|        HM_READ_PROP08                source/properties/spring/hm_read_prop08.F
Chd|        HM_READ_PROP09                source/properties/shell/hm_read_prop09.F
Chd|        HM_READ_PROP10                source/properties/shell/hm_read_prop10.F
Chd|        HM_READ_PROP11                source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP12                source/properties/spring/hm_read_prop12.F
Chd|        HM_READ_PROP13                source/properties/spring/hm_read_prop13.F
Chd|        HM_READ_PROP14                source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP14F               source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP15                source/properties/solid/hm_read_prop15.F
Chd|        HM_READ_PROP16                source/properties/shell/hm_read_prop16.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP18                source/properties/beam/hm_read_prop18.F
Chd|        HM_READ_PROP19                source/properties/shell/hm_read_prop19.F
Chd|        HM_READ_PROP20                source/properties/thickshell/hm_read_prop20.F
Chd|        HM_READ_PROP22                source/properties/thickshell/hm_read_prop22.F
Chd|        HM_READ_PROP23                source/properties/spring/hm_read_prop23.F
Chd|        HM_READ_PROP25                source/properties/spring/hm_read_prop25.F
Chd|        HM_READ_PROP26                source/properties/spring/hm_read_prop26.F
Chd|        HM_READ_PROP28                source/properties/xelem/hm_read_prop28.F
Chd|        HM_READ_PROP32                source/properties/spring/hm_read_prop32.F
Chd|        HM_READ_PROP33                source/properties/spring/hm_read_prop33.F
Chd|        HM_READ_PROP33_CYL_JNT        source/properties/spring/hm_read_prop33_cyl_jnt.F
Chd|        HM_READ_PROP33_FIX_JNT        source/properties/spring/hm_read_prop33_fix_jnt.F
Chd|        HM_READ_PROP33_FREE_JNT       source/properties/spring/hm_read_prop33_free_jnt.F
Chd|        HM_READ_PROP33_OLD_JNT        source/properties/spring/hm_read_prop33_old_jnt.F
Chd|        HM_READ_PROP33_PLAN_JNT       source/properties/spring/hm_read_prop33_plan_jnt.F
Chd|        HM_READ_PROP33_REV_JNT        source/properties/spring/hm_read_prop33_rev_jnt.F
Chd|        HM_READ_PROP33_SPH_JNT        source/properties/spring/hm_read_prop33_sph_jnt.F
Chd|        HM_READ_PROP33_TRANS_JNT      source/properties/spring/hm_read_prop33_trans_jnt.F
Chd|        HM_READ_PROP33_UNIV_JNT       source/properties/spring/hm_read_prop33_univ_jnt.F
Chd|        HM_READ_PROP34                source/properties/sph/hm_read_prop34.F
Chd|        HM_READ_PROP36                source/properties/spring/hm_read_prop36.F
Chd|        HM_READ_PROP44                source/properties/spring/hm_read_prop44.F
Chd|        HM_READ_PROP45                source/properties/spring/hm_read_prop45.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|        HM_READ_PROP_USER             source/properties/user_spring_solid/hm_read_prop_user.F
Chd|        HM_READ_QUAD                  source/elements/reader/hm_read_quad.F
Chd|        HM_READ_RADIATION             source/loads/thermic/hm_read_radiation.F
Chd|        HM_READ_RAND                  source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_REFSTA                source/loads/reference_state/refsta/hm_read_refsta.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RIVET                 source/elements/reader/hm_read_rivet.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_SH3N                  source/elements/reader/hm_read_sh3n.F
Chd|        HM_READ_SHELL                 source/elements/reader/hm_read_shell.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SOLID                 source/elements/reader/hm_read_solid.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_SPRING                source/elements/reader/hm_read_spring.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        HM_READ_SUBMODEL              source/model/assembling/hm_read_submodel.F
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_SURFSURF              source/groups/hm_read_surfsurf.F
Chd|        HM_READ_TABLE1_0              source/tools/curve/hm_read_table1_0.F
Chd|        HM_READ_TABLE1_1              source/tools/curve/hm_read_table1_1.F
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
Chd|        HM_READ_THGRSENS              source/output/th/hm_read_thgrsens.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_TRIA                  source/elements/reader/hm_read_tria.F
Chd|        HM_READ_TRUSS                 source/elements/reader/hm_read_truss.F
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|        HM_READ_UPWIND                source/general_controls/computation/hm_read_upwind.F
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        HM_READ_XELEM                 source/elements/reader/hm_read_xelem.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        HM_SUBMODGRE                  source/groups/hm_submodgr.F   
Chd|        HM_SUBMODGRN                  source/groups/hm_submodgr.F   
Chd|        HM_SUBMODPART                 source/groups/hm_submodpart.F 
Chd|        HM_SURFGR2                    source/groups/hm_surfgr2.F    
Chd|        HM_SURFNOD                    source/groups/hm_surfnod.F    
Chd|        HM_TAGPART                    source/groups/hm_tagpart.F    
Chd|        HM_TAGPART2                   source/groups/hm_tagpart2.F   
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|        I11BUC1                       source/interfaces/inter3d1/i11buc1.F
Chd|        I11BUC_VOX1                   source/interfaces/inter3d1/i11buc1.F
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I11TRI                        source/interfaces/inter3d1/i11tri.F
Chd|        I12CHK3                       source/interfaces/inter3d1/i12chk3.F
Chd|        I12SOL3                       source/interfaces/inter3d1/insol3.F
Chd|        I12TID3                       source/interfaces/inter3d1/i12tid3.F
Chd|        I1CHK3                        source/interfaces/inter3d1/i1chk3.F
Chd|        I1TID2                        source/interfaces/inter2d1/i1tid2.F
Chd|        I1TID3                        source/interfaces/inter3d1/i1tid3.F
Chd|        I20BORD                       source/interfaces/inter3d1/i20surfi.F
Chd|        I20EDGE1                      source/interfaces/inter3d1/i20surfi.F
Chd|        I20PWR3                       source/interfaces/inter3d1/i20pwr3.F
Chd|        I20PWR3A                      source/interfaces/inter3d1/i20pwr3.F
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|        I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|        I21BUC1                       source/interfaces/inter3d1/i21buc3.F
Chd|        I21ELS3                       source/interfaces/inter3d1/i21els3.F
Chd|        I21GAP3                       source/interfaces/inter3d1/i21gap3.F
Chd|        I21PWR3                       source/interfaces/inter3d1/i21pwr3.F
Chd|        I21TRI                        source/interfaces/inter3d1/i21tri.F
Chd|        I22ERR3                       source/interfaces/inter3d1/i22err3.F
Chd|        I23BUC1                       source/interfaces/inter3d1/i23buc3.F
Chd|        I23GAP3                       source/interfaces/inter3d1/i23gap3.F
Chd|        I23TRI                        source/interfaces/inter3d1/i23tri.F
Chd|        I24BUC1                       source/interfaces/inter3d1/i24buc1.F
Chd|        I24EDGE1                      source/interfaces/inter3d1/i24surfi.F
Chd|        I24EDGE2                      source/interfaces/inter3d1/i24surfi.F
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|        I24PEN3                       source/interfaces/inter3d1/i24pen3.F
Chd|        I24PWR3                       source/interfaces/inter3d1/i24pwr3.F
Chd|        I24SETNODES                   source/interfaces/inter3d1/i24setnodes.F
Chd|        I24TRI                        source/interfaces/inter3d1/i24tri.F
Chd|        I25BUC_VOX1                   source/interfaces/inter3d1/i25buc_vox1.F
Chd|        I25GAPM                       source/interfaces/inter3d1/i25sti3.F
Chd|        I25NEIGH_MSG_ERR              source/interfaces/inter3d1/i25neigh.F
Chd|        I25PWR3                       source/interfaces/inter3d1/i25pwr3.F
Chd|        I25PWR3E                      source/interfaces/inter3d1/i25pwr3e.F
Chd|        I25PWR3_E2S                   source/interfaces/inter3d1/i25pwr3_e2s.F
Chd|        I25SORS                       source/interfaces/inter3d1/i25sors.F
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|        I25SURFI                      source/interfaces/inter3d1/i25surfi.F
Chd|        I25TRI                        source/interfaces/inter3d1/i25tri.F
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|        I2CHK3                        source/interfaces/inter3d1/i2chk3.F
Chd|        I2SURFS                       source/interfaces/inter3d1/i2surfs.F
Chd|        I2TID3                        source/interfaces/inter3d1/i2tid3.F
Chd|        I2TRI                         source/interfaces/inter3d1/i2tri.F
Chd|        I3PEN2                        source/interfaces/inter2d1/i3pen2.F
Chd|        I3PEN3                        source/interfaces/inter3d1/i3pen3.F
Chd|        I3STI2                        source/interfaces/inter2d1/i3sti2.F
Chd|        I3STI3                        source/interfaces/inter3d1/i3sti3.F
Chd|        I6PEN3                        source/interfaces/inter3d1/i6pen3.F
Chd|        I6STI3                        source/interfaces/inter3d1/i6sti3.F
Chd|        I7BUC1                        source/interfaces/inter3d1/i7buc1.F
Chd|        I7BUC_VOX1                    source/interfaces/inter3d1/i7buc_vox1.F
Chd|        I7ERR3                        source/interfaces/inter3d1/i7err3.F
Chd|        I7PWR3                        source/interfaces/inter3d1/i7pwr3.F
Chd|        I7REMNODE                     source/interfaces/inter3d1/i7remnode.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|        I7TRI                         source/interfaces/inter3d1/i7tri.F
Chd|        I9STI3                        source/interfaces/int09/i9sti3.F
Chd|        IEDGE_XFEM                    source/elements/xfem/iedge_xfem.F
Chd|        IG3DONEDERIV                  source/elements/ige3d/ig3donederiv.F
Chd|        INGRBRIC_DX                   source/interfaces/interf1/ingrbric_dx.F
Chd|        INIBCS_CY                     source/constraints/general/bcs/lecbcscyc.F
Chd|        INIBOLTPREL                   source/loads/bolt/iniboltprel.F
Chd|        INIEBCS                       source/boundary_conditions/ebcs/iniebcs.F
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|        INIGUSER                      source/system/iniguser.F      
Chd|        ININT2                        source/interfaces/inter2d1/inint2.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|        ININTR_THKVAR                 source/interfaces/interf1/inintr_thkvar.F
Chd|        ININTSUB_11                   source/output/subinterface/inintsub_11.F
Chd|        ININTSUB_25                   source/output/subinterface/inintsub_25.F
Chd|        ININTSUB_7                    source/output/subinterface/inintsub_7.F
Chd|        INIRBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        INIRBE3                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        INIRBY                        source/constraints/general/rbody/inirby.F
Chd|        INIRBYS                       source/constraints/general/rbody/inirby.F
Chd|        INISEN                        source/tools/sensor/inisen.F  
Chd|        INISMS                        source/general_controls/computation/hm_read_sms.F
Chd|        INISRF                        source/constraints/general/rbody/inisrf.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|        INIT_SKEW45                   source/elements/joint/rjoint/rini45.F
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        INIVOL_SET                    source/initial_conditions/inivol/inivol_set.F
Chd|        INI_BCSCYC                    source/constraints/general/bcs/lecbcscyc.F
Chd|        INI_DIFF                      source/tools/lagmul/ini_diff.F
Chd|        INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|        INI_GEAR                      source/tools/lagmul/ini_gear.F
Chd|        INI_INIMAP1D                  source/initial_conditions/inimap/ini_inimap1d.F
Chd|        INI_RACK                      source/tools/lagmul/ini_rack.F
Chd|        INI_SEATBELT                  source/tools/seatbelts/ini_seatbelt.F
Chd|        INRCH2                        source/interfaces/inter2d1/inrch2.F
Chd|        INSOLT10                      source/interfaces/inter3d1/insolt10.F
Chd|        INSPCND                       source/elements/sph/inspcnd.F 
Chd|        INSURF                        source/interfaces/interf1/insurf.F
Chd|        INT2CY_CHK                    source/constraints/general/bcs/lecbcscyc.F
Chd|        INT2MODIF_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|        INTER_DCOD_FRICTION           source/interfaces/reader/inter_dcod_friction.F
Chd|        INTER_DCOD_FUNCTION           source/interfaces/reader/inter_dcod_function.F
Chd|        INTER_DCOD_SENSOR             source/interfaces/reader/inter_dcod_sensor.F
Chd|        INVOI3                        source/interfaces/inter3d1/invoi3.F
Chd|        ITAGSL2                       source/interfaces/inter3d1/itagsl2.F
Chd|        KINCHK                        source/constraints/general/kinchk.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        LAGM_INI                      source/tools/lagmul/lagm_ini.F
Chd|        LAW108_UPD                    source/materials/mat/mat108/law108_upd.F
Chd|        LAW111_UPD                    source/materials/mat/mat111/law111_upd.F
Chd|        LAW112_UPD                    source/materials/mat/mat112/law112_upd.F
Chd|        LAW113_UPD                    source/materials/mat/mat113/law113_upd.F
Chd|        LAW114_UPD                    source/materials/mat/mat114/law114_upd.F
Chd|        LAW119_UPD                    source/materials/mat/mat119/law119_upd.F
Chd|        LAW36_UPD                     source/materials/mat/mat036/law36_upd.F
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|        LAW69_NLSQF                   source/materials/tools/nlsqf.F
Chd|        LAW69_UPD                     source/materials/mat/mat069/law69_upd.F
Chd|        LAW76_UPD                     source/materials/mat/mat076/law76_upd.F
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|        LAW88_UPD                     source/materials/mat/mat088/law88_upd.F
Chd|        LAW92_NLSQF                   source/materials/mat/mat092/law92_nlsqf.F
Chd|        LCGEO19                       source/elements/shell/coque/lcgeo19.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|        LECM51__CHECK_INITIAL_STATE   source/materials/mat/mat051/lecm51__check_initial_state.F
Chd|        LECREFSTA                     source/loads/reference_state/refsta/lecrefsta.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LECSUBMOD                     source/model/submodel/lecsubmod.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        LEC_DOF_JNT                   source/properties/spring/hm_read_prop45.F
Chd|        LEC_INIMAP1D_FILE             source/initial_conditions/inimap/lec_inimap1d_file.F
Chd|        LEC_INIMAP2D_FILE             source/initial_conditions/inimap/lec_inimap2d_file.F
Chd|        LEC_INISTATE_D00_BRICK_CHECK  source/elements/initia/lec_inistate_d00_brick-check.F
Chd|        LEC_INISTATE_TRI              source/elements/initia/lec_inistate_tri.F
Chd|        LGMINI_BC                     source/tools/lagmul/lgmini_bc.F
Chd|        LGMINI_FXV                    source/tools/lagmul/lgmini_fxv.F
Chd|        LGMINI_GJ                     source/tools/lagmul/lgmini_gj.F
Chd|        LGMINI_I2                     source/tools/lagmul/lgmini_i2.F
Chd|        LGMINI_I7                     source/tools/lagmul/lgmini_i7.F
Chd|        LGMINI_MPC                    source/tools/lagmul/lgmini_mpc.F
Chd|        LGMINI_RBY                    source/tools/lagmul/lgmini_rby.F
Chd|        LGMINI_RWL                    source/tools/lagmul/lgmini_rwl.F
Chd|        LM_LEAST_SQUARE_PRONY         source/materials/visc/hm_read_visc_prony.F
Chd|        LM_LEAST_SQUARE_PRONY_2       source/materials/visc/hm_read_visc_prony.F
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|        M20DCOD                       source/system/fsdcod.F        
Chd|        M75INIT                       source/materials/mat/mat075/m75init.F
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|        MASS_FLUID_TG                 source/fluid/mass-fluid_tg.F  
Chd|        MATFUN_USR2SYS                source/materials/tools/matfun_usr2sys.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        MATTAB_USR2SYS                source/materials/tools/mattab_usr2sys.F
Chd|        MERGE_NODE                    source/elements/nodes/merge_node.F
Chd|        MONVOL_CHECK_DELETE_DUPLICATEDsource/airbag/monvol_check_delete_duplicated.F
Chd|        MONVOL_CHECK_SURFCLOSE        share/modules1/monvol_struct_mod.F
Chd|        MONVOL_CHECK_VENTHOLE_SURF    share/modules1/monvol_struct_mod.F
Chd|        MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|        MSG_ERR                       source/interfaces/inter3d1/i24tools.F
Chd|        MULTIFLUID_INIT2              source/multifluid/multifluid_init2.F
Chd|        MULTIFLUID_INIT2T             source/multifluid/multifluid_init2t.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|        MULTI_CHECK_EOS               source/multifluid/multi_check_eos.F
Chd|        MULTI_CHECK_PSH               source/multifluid/multi_check_psh.F
Chd|        MVERIF                        source/system/machine.F       
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        NEWDBL2                       source/system/sysfus.F        
Chd|        NEW_SEATBELT                  source/tools/seatbelts/new_seatbelt.F
Chd|        NLOCAL_INIT_STA               source/materials/fail/nlocal_init_sta.F
Chd|        NLOC_DMG_INIT                 source/materials/fail/nloc_dmg_init.F
Chd|        NRF51INI                      source/materials/mat/mat051/nrf51ini.F
Chd|        ORTHDIR_PROJ                  source/interfaces/interf1/inintr_orthdirfric.F
Chd|        PCOORI                        source/elements/beam/pcoori.F 
Chd|        PGRTAILS                      source/elements/beam/pgrtails.F
Chd|        PINIT3                        source/elements/beam/pinit3.F 
Chd|        PORNOD                        source/ale/pornod.F           
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PREPARE_INT25                 source/model/mesh/build_cnel.F
Chd|        PRE_STACKGROUP                source/stack/pres_stackgroup.F
Chd|        PRINTGROUP                    source/output/outfile/printgroup.F
Chd|        Q4DERI2                       source/elements/solid_2d/quad4/q4deri2.F
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        Q4VOLI2                       source/elements/solid_2d/quad4/q4voli2.F
Chd|        QGRHEAD                       source/elements/solid_2d/quad/qgrhead.F
Chd|        QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|        QMORTH2                       source/elements/solid_2d/quad/qmorth2.F
Chd|        QVOLI2                        source/elements/solid_2d/quad/qvoli2.F
Chd|        R2R_CHECK                     source/coupling/rad2rad/r2r_check.F
Chd|        R2R_DOMDEC                    source/coupling/rad2rad/r2r_domdec.F
Chd|        R2R_FORK                      source/coupling/rad2rad/r2r_fork.F
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC_NAME               source/coupling/rad2rad/r2r_prelec_name.F
Chd|        R2R_SPEEDUP                   source/coupling/rad2rad/r2r_speedup.F
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|        R2R_VOID                      source/coupling/rad2rad/r2r_void.F
Chd|        R3BUF3                        source/elements/spring/r3buf3.F
Chd|        R4BUF3                        source/elements/spring/r4buf3.F
Chd|        RBE2MODIF_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|        RCHECKMASS                    source/elements/spring/rcheckmass.F
Chd|        READ_BOX_BOX                  source/model/box/read_box_box.F
Chd|        READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                source/model/box/read_box_spher.F
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|        READ_MSGFILE                  source/output/message/read_msgfile.F
Chd|        READ_PCH_FILE                 source/constraints/fxbody/read_pch_file.F
Chd|        READ_SENSOR_ACC               source/tools/sensor/read_sensor_acc.F
Chd|        READ_SENSOR_CONTACT           source/tools/sensor/read_sensor_contact.F
Chd|        READ_SENSOR_GAUGE             source/tools/sensor/read_sensor_gauge.F
Chd|        READ_SENSOR_HIC               source/tools/sensor/read_sensor_hic.F
Chd|        READ_SENSOR_RBODY             source/tools/sensor/read_sensor_rbody.F
Chd|        READ_SENSOR_RWALL             source/tools/sensor/read_sensor_rwall.F
Chd|        READ_SENSOR_SECT              source/tools/sensor/read_sensor_sect.F
Chd|        READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|        REALLOC_IDDCONNECT            source/spmd/node/ddtools.F    
Chd|        REALLOC_IFRONT                source/spmd/node/ddtools.F    
Chd|        REMN_I2                       source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2OP_EDG25               source/interfaces/int25/i25remlin.F
Chd|        RIGID_MAT                     source/materials/mat/mat019/rigid_mat.F
Chd|        RIGMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|        RINI32                        source/properties/spring/hm_read_prop32.F
Chd|        RINI33_RB                     source/elements/joint/rjoint/rini33_rb.F
Chd|        RINI45                        source/elements/joint/rjoint/rini45.F
Chd|        RINI45_RB                     source/elements/joint/rjoint/rini45_rb.F
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|        RIVET0                        source/elements/reader/rivet0.F
Chd|        RKINI3                        source/elements/spring/rkini3.F
Chd|        RM_CAND24                     source/interfaces/inter3d1/i7remnode.F
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16DERI3                      source/elements/thickshell/solide16/s16deri3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20DERI3                      source/elements/solid/solide20/s20deri3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|        S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8EDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|        SCALEINI                      source/elements/initia/scaleini.F
Chd|        SCDERI3                       source/elements/thickshell/solidec/scderi3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SCMORTH3                      source/elements/thickshell/solidec/scmorth3.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        SDERI3B                       source/elements/solid/solid8p/sderi3b.F
Chd|        SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|        SGRHEAD                       source/elements/solid/solide/sgrhead.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SMORTH3                       source/elements/solid/solide/smorth3.F
Chd|        SMS_INIT                      source/ams/sms_init.F         
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SOLTOSPHV4                    source/elements/sph/soltosph.F
Chd|        SOLTOSPHV8                    source/elements/sph/soltosph.F
Chd|        SORT_LOGICAL_SENSORS          source/tools/sensor/sort_logical_sensors.F
Chd|        SPGRHEAD                      source/elements/sph/spgrhead.F
Chd|        SPGRTAILS                     source/elements/sph/spgrtails.F
Chd|        SPHDCOD                       source/elements/sph/sphdcod.F 
Chd|        SPLIT_CAND_I20                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I24                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I25                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I7                 source/restart/ddsplit/inter_tools.F
Chd|        SPTRI                         source/elements/sph/sptri.F   
Chd|        SREFDERI3                     source/elements/solid/solide/srefsta3.F
Chd|        STACKGROUP                    source/stack/stackgroup.F     
Chd|        STARTER0                      source/starter/starter0.F     
Chd|        STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|        SURFMAS                       source/tools/admas/surfmas.F  
Chd|        SZDERI3                       source/elements/solid/solidez/szderi3.F
Chd|        SZREFDERI3                    source/elements/solid/solide/srefsta3.F
Chd|        T3GRHEAD                      source/elements/solid_2d/tria/t3grhead.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        TAGNODS_R2R                   source/coupling/rad2rad/tagnod_r2r.F
Chd|        TAGNOD_R2R                    source/coupling/rad2rad/tagnod_r2r.F
Chd|        TINIT3                        source/elements/truss/tinit3.F
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|        TRIRBMERGE                    source/constraints/general/merge/hm_read_merge.F
Chd|        UDOUBL2                       source/system/sysfus.F        
Chd|        UDOUBL2_IGR                   source/system/sysfus.F        
Chd|        UDOUBL2_SET                   source/system/sysfus.F        
Chd|        UDOUBL2_WO_TITLE              source/system/sysfus.F        
Chd|        UDOUBL3                       source/system/sysfus.F        
Chd|        UNIT_CODE                     source/general_controls/computation/unit_code.F
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|        VDOUBL2                       source/system/sysfus.F        
Chd|        W_ISPH                        source/restart/ddsplit/w_isph.F
Chd|        W_RSPH                        source/restart/ddsplit/w_rsph.F
Chd|        XGRHEAD                       source/elements/xelem/xgrhead.F
Chd|        XGRTAILS                      source/elements/xelem/xgrtails.F
Chd|        XINI28                        source/elements/xelem/xini28.F
Chd|        XINI30                        source/elements/xelem/xini30.F
Chd|        XINI31                        source/elements/xelem/xini31.F
Chd|        XINIT3                        source/elements/xelem/xinit3.F
Chd|        YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|        GET_SKEW                      source/elements/joint/rjoint/rini33.F
Chd|        GET_SKEW45                    source/elements/joint/rjoint/rini45.F
Chd|        GET_U_FUNC_Y                  source/user_interface/uaccess.F
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        GET_U_MAT                     source/user_interface/uaccess.F
Chd|        GET_U_MNU                     source/user_interface/uaccess.F
Chd|        GET_U_NUMFUN                  source/user_interface/uaccess.F
Chd|        GET_U_PNU                     source/user_interface/uaccess.F
Chd|        GRFIND                        source/starter/freform.F      
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        NBOXLST                       source/model/box/nboxlist.F   
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        NINTLST                       source/system/nintrr.F        
Chd|        NINTLST2                      source/system/nintrr.F        
Chd|        NINTLSTN                      source/system/nintrr.F        
Chd|        NINTRN                        source/system/nintrn.F        
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        NODGRNR6                      source/starter/freform.F      
Chd|        NODGR_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_EXIST                     source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_SYS2                      source/coupling/rad2rad/routines_r2r.F
Chd|        RESET_U_GEO                   source/user_interface/uaccess.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        SET_U_SENS_FPAR               source/user_interface/uaccess.F
Chd|        SET_U_SENS_IPAR               source/user_interface/uaccess.F
Chd|        ULIST2S                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        USR2SYS2                      source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MESS_EXTRACT_FORMAT           source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        NL                            source/output/message/message.F
Chd|        MESSAGE_MOD2                  share/message_module/message_mod2.F
Chd|====================================================================
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
#include      "scr17_c.inc"
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
      CHARACTER*ncharline CBUF(10)
C
      CHARACTER*ncharline TMPLINE,MYFMT,TMPBUF
      CHARACTER*ncharline TMPOUT,TMPIN,TMPIN2,TMPIN3
      CHARACTER*20 CMSGTYPE
      CHARACTER*15 CTYPE
      INTEGER ITYPE,ILINE,I,I0,J,J0,SBUFMSG(2),IFILE,IOLD,
     *        INDXI,INDXR,INDXC,INDXD,INDXTMPOUT,BUFLEN,
     *        STMP,IEXIST,IEND,MSGID_R,CPT,NBREPET,NBREPET_0,
     *        INT_TMP,ITMP,INDXI_INC,INDXR_INC,INDXC_INC,INDXD_INC,
     *        NB_IBUF,NB_RBUF,NB_CBUF,MODE, WORK(70000),K,
     *        NB_MESSAGES,ITAG,NO_PRINT,ISAV
      my_real
     *        REAL_TMP
      INTEGER NCOUNT,ID_NUM,KEY_LEN
      CHARACTER*ncharline KEY
      CHARACTER*ncharline BUFMSG(2,100),BUFFMT(2,100),CHAR_TMP
      CHARACTER*2304 OPTION_NAME_DYNA,DYNA_TITLE
      INTEGER S_OPTION_NAME_DYNA,SDYNA_TITLE,DYNA_ID
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_INTEGER
      my_real, DIMENSION(:), ALLOCATABLE :: TMP_REAL
      CHARACTER*ncharline, DIMENSION(:), ALLOCATABLE :: TMP_CHAR
      CHARACTER*ncharline, DIMENSION(:), ALLOCATABLE :: TMP_I
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,MES_DOUBLE
C-----------------------------------------------
       CHARACTER*2 NL 
       EXTERNAL NL
C-----------------------------------------------
C
c      IF((PRESENT(IOPTION)))THEN
c        CONTINUE
c      ELSE
c        IOPTION = 0
c      ENDIF
c      IF((PRESENT(IOPTION_TYPE)))THEN
c        CONTINUE
c      ELSE
c        IOPTION_TYPE = 0
c      ENDIF
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
                IF (TMPLINE(I:I) == '\' ) THEN
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
                  IF (TMPLINE(I:I) == '\') THEN
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
                  IF (TMPLINE(I:I)=='\') THEN
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


Chd|====================================================================
Chd|  MESS_EXTRACT_FORMAT           source/output/message/message.F
Chd|-- called by -----------
Chd|        ANCMSG                        source/output/message/message.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MESS_EXTRACT_FORMAT(TMPLINE,LINE_SIZE,NCOUNT,ID_NUM,KEY,KEY_LEN) 
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
      CHARACTER, INTENT(OUT),DIMENSION(ncharline) :: KEY            ! output string with option type
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


Chd|====================================================================
Chd|  NL                            source/output/message/message.F
Chd|-- called by -----------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_READ_MAT108                source/materials/mat/mat108/hm_read_mat108.F
Chd|        HM_READ_MAT113                source/materials/mat/mat113/hm_read_mat113.F
Chd|        HM_READ_PROP04                source/properties/spring/hm_read_prop04.F
Chd|        HM_READ_PROP08                source/properties/spring/hm_read_prop08.F
Chd|        HM_READ_PROP12                source/properties/spring/hm_read_prop12.F
Chd|        HM_READ_PROP13                source/properties/spring/hm_read_prop13.F
Chd|        HM_READ_PROP25                source/properties/spring/hm_read_prop25.F
Chd|-- calls ---------------
Chd|====================================================================
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
      
