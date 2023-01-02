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
Chd|  HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|-- called by -----------
Chd|        CONTRBE2                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        GROUPS_GET_ELEM_LIST          source/groups/groups_get_elem_list.F
Chd|        GROUPS_GET_NENTITY            source/groups/groups_get_nentity.F
Chd|        HM_ADMLIST                    source/groups/hm_admlist.F    
Chd|        HM_CONVERT_2D_ELEMENTS_SEATBELTsource/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
Chd|        HM_COUNT_2D_ELEMENT_SEATBELT  source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
Chd|        HM_ELNGR                      source/groups/hm_elngr.F      
Chd|        HM_ELNGRR                     source/groups/hm_elngrr.F     
Chd|        HM_ELNGRS                     source/groups/hm_elngr.F      
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
Chd|        HM_PREREAD_LOAD_CENTRI        source/loads/general/load_centri/hm_preread_load_centri.F
Chd|        HM_PREREAD_LOAD_PRESSURE      source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|        HM_PREREAD_MERGE              source/constraints/general/merge/hm_preread_merge.F
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
Chd|        HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
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
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_DEFINTER_TYPE02       source/general_controls/default_values/hm_read_definter_type02.F
Chd|        HM_READ_DEFINTER_TYPE07       source/general_controls/default_values/hm_read_definter_type07.F
Chd|        HM_READ_DEFINTER_TYPE11       source/general_controls/default_values/hm_read_definter_type11.F
Chd|        HM_READ_DEFINTER_TYPE19       source/general_controls/default_values/hm_read_definter_type19.F
Chd|        HM_READ_DEFINTER_TYPE24       source/general_controls/default_values/hm_read_definter_type24.F
Chd|        HM_READ_DEFINTER_TYPE25       source/general_controls/default_values/hm_read_definter_type25.F
Chd|        HM_READ_DEFSHELL              source/general_controls/default_values/hm_read_defshell.F
Chd|        HM_READ_DEFSOLID              source/general_controls/default_values/hm_read_defsolid.F
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
Chd|        HM_READ_EOS_COMPACTION        source/materials/eos/hm_read_eos_compaction.F
Chd|        HM_READ_EOS_TABULATED         source/materials/eos/hm_read_eos_tabulated.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FAIL_ALTER            source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|        HM_READ_FAIL_BIQUAD           source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|        HM_READ_FAIL_CHANG            source/materials/fail/changchang/hm_read_fail_chang.F
Chd|        HM_READ_FAIL_CONNECT          source/materials/fail/connect/hm_read_fail_connect.F
Chd|        HM_READ_FAIL_ENERGY           source/materials/fail/energy/hm_read_fail_energy.F
Chd|        HM_READ_FAIL_FABRIC           source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|        HM_READ_FAIL_FLD              source/materials/fail/fld/hm_read_fail_fld.F
Chd|        HM_READ_FAIL_GENE1            source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|        HM_READ_FAIL_GURSON           source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_FAIL_HASHIN           source/materials/fail/hashin/hm_read_fail_hashin.F
Chd|        HM_READ_FAIL_HC_DSSE          source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
Chd|        HM_READ_FAIL_INIEVO           source/materials/fail/inievo/hm_read_fail_inievo.F
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
Chd|        HM_READ_FAIL_SYAZWAN          source/materials/fail/syazwan/hm_read_fail_syazwan.F
Chd|        HM_READ_FAIL_TAB1             source/materials/fail/tabulated/hm_read_fail_tab1.F
Chd|        HM_READ_FAIL_TAB2             source/materials/fail/tabulated/hm_read_fail_tab2.F
Chd|        HM_READ_FAIL_TAB_OLD          source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|        HM_READ_FAIL_TBUTCHER         source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
Chd|        HM_READ_FAIL_TENSSTRAIN       source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
Chd|        HM_READ_FAIL_USER             source/materials/fail/failuser/hm_read_fail_user.F
Chd|        HM_READ_FAIL_VISUAL           source/materials/fail/visual/hm_read_fail_visual.F
Chd|        HM_READ_FAIL_WIERZBICKI       source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
Chd|        HM_READ_FAIL_WILKINS          source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|        HM_READ_FRICTION              source/interfaces/friction/reader/hm_read_friction.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FUNC2D                source/tools/curve/hm_read_func2d.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
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
Chd|        HM_READ_INTER_HERTZ_TYPE17    source/interfaces/int17/hm_read_inter_hertz_type17.F
Chd|        HM_READ_INTER_LAGDT_TYPE07    source/interfaces/int07/hm_read_inter_lagdt_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE02   source/interfaces/int02/hm_read_inter_lagmul_type02.F
Chd|        HM_READ_INTER_LAGMUL_TYPE07   source/interfaces/int07/hm_read_inter_lagmul_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE16   source/interfaces/int16/hm_read_inter_lagmul_type16.F
Chd|        HM_READ_INTER_LAGMUL_TYPE17   source/interfaces/int17/hm_read_inter_lagmul_type17.F
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_INTER_TYPE01          source/interfaces/int01/hm_read_inter_type01.F
Chd|        HM_READ_INTER_TYPE02          source/interfaces/int02/hm_read_inter_type02.F
Chd|        HM_READ_INTER_TYPE03          source/interfaces/int03/hm_read_inter_type03.F
Chd|        HM_READ_INTER_TYPE05          source/interfaces/int05/hm_read_inter_type05.F
Chd|        HM_READ_INTER_TYPE06          source/interfaces/int06/hm_read_inter_type06.F
Chd|        HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE08          source/interfaces/int08/hm_read_inter_type08.F
Chd|        HM_READ_INTER_TYPE09          source/interfaces/int09/hm_read_inter_type09.F
Chd|        HM_READ_INTER_TYPE10          source/interfaces/int10/hm_read_inter_type10.F
Chd|        HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE12          source/interfaces/int12/hm_read_inter_type12.F
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
Chd|        HM_READ_IOFLAG                source/general_controls/inputoutput/hm_read_ioflag.F
Chd|        HM_READ_LAGMUL                source/tools/lagmul/hm_read_lagmul.F
Chd|        HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MAT02                 source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MAT03                 source/materials/mat/mat003/hm_read_mat03.F
Chd|        HM_READ_MAT04                 source/materials/mat/mat004/hm_read_mat04.F
Chd|        HM_READ_MAT05                 source/materials/mat/mat005/hm_read_mat05.F
Chd|        HM_READ_MAT06                 source/materials/mat/mat006/hm_read_mat06.F
Chd|        HM_READ_MAT06_KEPS            source/materials/mat/mat006/hm_read_mat06_keps.F
Chd|        HM_READ_MAT10                 source/materials/mat/mat010/hm_read_mat10.F
Chd|        HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT102                source/materials/mat/mat102/hm_read_mat102.F
Chd|        HM_READ_MAT103                source/materials/mat/mat103/hm_read_mat103.F
Chd|        HM_READ_MAT104                source/materials/mat/mat104/hm_read_mat104.F
Chd|        HM_READ_MAT106                source/materials/mat/mat106/hm_read_mat106.F
Chd|        HM_READ_MAT107                source/materials/mat/mat107/hm_read_mat107.F
Chd|        HM_READ_MAT108                source/materials/mat/mat108/hm_read_mat108.F
Chd|        HM_READ_MAT109                source/materials/mat/mat109/hm_read_mat109.F
Chd|        HM_READ_MAT11                 source/materials/mat/mat011/hm_read_mat11.F
Chd|        HM_READ_MAT110                source/materials/mat/mat110/hm_read_mat110.F
Chd|        HM_READ_MAT111                source/materials/mat/mat111/hm_read_mat111.F
Chd|        HM_READ_MAT112                source/materials/mat/mat112/hm_read_mat112.F
Chd|        HM_READ_MAT113                source/materials/mat/mat113/hm_read_mat113.F
Chd|        HM_READ_MAT114                source/materials/mat/mat114/hm_read_mat114.F
Chd|        HM_READ_MAT115                source/materials/mat/mat115/hm_read_mat115.F
Chd|        HM_READ_MAT116                source/materials/mat/mat116/hm_read_mat116.F
Chd|        HM_READ_MAT117                source/materials/mat/mat117/hm_read_mat117.F
Chd|        HM_READ_MAT119                source/materials/mat/mat119/hm_read_mat119.F
Chd|        HM_READ_MAT11_K_EPS           source/materials/mat/mat011/hm_read_mat11_k_eps.F
Chd|        HM_READ_MAT12                 source/materials/mat/mat012/hm_read_mat12.F
Chd|        HM_READ_MAT120                source/materials/mat/mat120/hm_read_mat120.F
Chd|        HM_READ_MAT121                source/materials/mat/mat121/hm_read_mat121.F
Chd|        HM_READ_MAT14                 source/materials/mat/mat014/hm_read_mat14.F
Chd|        HM_READ_MAT15                 source/materials/mat/mat015/hm_read_mat15.F
Chd|        HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|        HM_READ_MAT18                 source/materials/mat/mat018/hm_read_mat18.F
Chd|        HM_READ_MAT19                 source/materials/mat/mat019/hm_read_mat19.F
Chd|        HM_READ_MAT20                 source/materials/mat/mat020/hm_read_mat20.F
Chd|        HM_READ_MAT21                 source/materials/mat/mat021/hm_read_mat21.F
Chd|        HM_READ_MAT22                 source/materials/mat/mat022/hm_read_mat22.F
Chd|        HM_READ_MAT23                 source/materials/mat/mat023/hm_read_mat23.F
Chd|        HM_READ_MAT24                 source/materials/mat/mat024/hm_read_mat24.F
Chd|        HM_READ_MAT25                 source/materials/mat/mat025/hm_read_mat25.F
Chd|        HM_READ_MAT27                 source/materials/mat/mat027/hm_read_mat27.F
Chd|        HM_READ_MAT28                 source/materials/mat/mat028/hm_read_mat28.F
Chd|        HM_READ_MAT29_31              source/materials/mat/matuser/hm_read_mat_user29_31.F
Chd|        HM_READ_MAT32                 source/materials/mat/mat032/hm_read_mat32.F
Chd|        HM_READ_MAT33                 source/materials/mat/mat033/hm_read_mat33.F
Chd|        HM_READ_MAT35                 source/materials/mat/mat035/hm_read_mat35.F
Chd|        HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT41                 source/materials/mat/mat041/hm_read_mat41.F.F
Chd|        HM_READ_MAT42                 source/materials/mat/mat042/hm_read_mat42.F
Chd|        HM_READ_MAT43                 source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT44                 source/materials/mat/mat044/hm_read_mat44.F
Chd|        HM_READ_MAT46                 source/materials/mat/mat046/hm_read_mat46.F
Chd|        HM_READ_MAT49                 source/materials/mat/mat049/hm_read_mat49.F
Chd|        HM_READ_MAT50                 source/materials/mat/mat050/hm_read_mat50.F
Chd|        HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_MAT52                 source/materials/mat/mat052/hm_read_mat52.F
Chd|        HM_READ_MAT53                 source/materials/mat/mat053/hm_read_mat53.F
Chd|        HM_READ_MAT54                 source/materials/mat/mat054/hm_read_mat54.F
Chd|        HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT58                 source/materials/mat/mat058/hm_read_mat58.F
Chd|        HM_READ_MAT59                 source/materials/mat/mat059/hm_read_mat59.F
Chd|        HM_READ_MAT60                 source/materials/mat/mat060/hm_read_mat60.F
Chd|        HM_READ_MAT62                 source/materials/mat/mat062/hm_read_mat62.F
Chd|        HM_READ_MAT64                 source/materials/mat/mat064/hm_read_mat64.F
Chd|        HM_READ_MAT65                 source/materials/mat/mat065/hm_read_mat65.F
Chd|        HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT68                 source/materials/mat/mat068/hm_read_mat68.F
Chd|        HM_READ_MAT69                 source/materials/mat/mat069/hm_read_mat69.F
Chd|        HM_READ_MAT70                 source/materials/mat/mat070/hm_read_mat70.F
Chd|        HM_READ_MAT73                 source/materials/mat/mat073/hm_read_mat73.F
Chd|        HM_READ_MAT74                 source/materials/mat/mat074/hm_read_mat74.F
Chd|        HM_READ_MAT75                 source/materials/mat/mat075/hm_read_mat75.F
Chd|        HM_READ_MAT76                 source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT77                 source/materials/mat/mat077/hm_read_mat77.F
Chd|        HM_READ_MAT78                 source/materials/mat/mat078/hm_read_mat78.F
Chd|        HM_READ_MAT80                 source/materials/mat/mat080/hm_read_mat80.F
Chd|        HM_READ_MAT81                 source/materials/mat/mat081/hm_read_mat81.F
Chd|        HM_READ_MAT82                 source/materials/mat/mat082/hm_read_mat82.F
Chd|        HM_READ_MAT83                 source/materials/mat/mat083/hm_read_law83.F
Chd|        HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT92                 source/materials/mat/mat092/hm_read_mat92.F
Chd|        HM_READ_MAT93                 source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MAT97                 source/materials/mat/mat097/hm_read_mat97.F
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
Chd|        HM_READ_MPC0                  source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PERTURB_PART_SHELL    source/general_controls/computation/hm_read_perturb_part_shell.F
Chd|        HM_READ_PERTURB_PART_SOLID    source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_PRETHGROU             source/output/th/hm_read_prethgrou.F
Chd|        HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
Chd|        HM_READ_PROP03                source/properties/beam/hm_read_prop03.F
Chd|        HM_READ_PROP04                source/properties/spring/hm_read_prop04.F
Chd|        HM_READ_PROP05                source/properties/rivet/hm_read_prop05.F
Chd|        HM_READ_PROP06                source/properties/solid/hm_read_prop06.F
Chd|        HM_READ_PROP08                source/properties/spring/hm_read_prop08.F
Chd|        HM_READ_PROP09                source/properties/shell/hm_read_prop09.F
Chd|        HM_READ_PROP10                source/properties/shell/hm_read_prop10.F
Chd|        HM_READ_PROP11                source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP12                source/properties/spring/hm_read_prop12.F
Chd|        HM_READ_PROP13                source/properties/spring/hm_read_prop13.F
Chd|        HM_READ_PROP14                source/properties/solid/hm_read_prop14.F
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
Chd|        HM_READ_PROP27                source/properties/spring/hm_read_prop27.F
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
Chd|        HM_READ_PROP35                source/properties/spring/hm_read_prop35.F
Chd|        HM_READ_PROP36                source/properties/spring/hm_read_prop36.F
Chd|        HM_READ_PROP43                source/properties/solid/hm_read_prop43.F
Chd|        HM_READ_PROP44                source/properties/spring/hm_read_prop44.F
Chd|        HM_READ_PROP45                source/properties/spring/hm_read_prop45.F
Chd|        HM_READ_PROP46                source/properties/spring/hm_read_prop46.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_PROP_USER             source/properties/user_spring_solid/hm_read_prop_user.F
Chd|        HM_READ_PROP_USER4            source/properties/user_spring_solid/hm_read_prop_user4.F
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
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SMS                   source/general_controls/computation/hm_read_sms.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHGLO                source/general_controls/computation/hm_read_sphglo.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_SPMD                  source/general_controls/computation/hm_read_spmd.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
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
Chd|        HM_READ_THGRPA                source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRPA_SUB            source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRSENS              source/output/th/hm_read_thgrsens.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        HM_SURFNOD                    source/groups/hm_surfnod.F    
Chd|        HM_TAGPART2                   source/groups/hm_tagpart2.F   
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        HM_THVARVENT                  source/output/th/hm_thvarent.F
Chd|        HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|        LCGEO19                       source/elements/shell/coque/lcgeo19.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LECLAS                        source/loads/laser/leclas.F   
Chd|        LECSEC0                       source/tools/sect/hm_read_sect.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LECSUBMOD                     source/model/submodel/lecsubmod.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        LEC_DOF_JNT                   source/properties/spring/hm_read_prop45.F
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
Chd|        READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|        READ_SENSOR_VEL               source/tools/sensor/read_sensor_vel.F
Chd|        READ_SENSOR_WORK              source/tools/sensor/read_sensor_work.F
Chd|        SETRB2ON                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SETRBYON                      source/constraints/general/rbody/hm_read_rbody.F
Chd|        SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|        STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|        ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GET_INTV(NAME,IVAL,IS_AVAILABLE,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE
C   ASK (INTEGER) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   APPLY AUTOMATICALLY SUBMODEL OFFSETS USING DATATYPE DEFINED IN .cfg FILE
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     IVAL         INTEGER VALUE OF THE FIELD 
C     IS_AVAILABLE VALUE AVAILABLE IN MODEL OR NOT
C     LSUBMODEL    SUBMODEL STRUCTURE       
C     UNITAB       UNIT ARRAY       
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IVAL
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID
      LOGICAL(KIND=C_BOOL) C_IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C--------------------------------------------------
      VALUE_TYPE = 0
      SUB_ID = 0
      IVAL = 0
C--------------------------------------------------
      CALL CPP_GET_INTV(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),IVAL,SUB_ID,C_IS_AVAILABLE,VALUE_TYPE)
      IS_AVAILABLE = C_IS_AVAILABLE
C--------------------------------------------------
      RETURN
C
      END
