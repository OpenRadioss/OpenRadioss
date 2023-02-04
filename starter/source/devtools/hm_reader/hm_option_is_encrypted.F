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
Chd|  HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|-- called by -----------
Chd|        HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
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
Chd|        HM_READ_EOS_COMPACTION        source/materials/eos/hm_read_eos_compaction.F
Chd|        HM_READ_EOS_GRUNEISEN         source/materials/eos/hm_read_eos_gruneisen.F
Chd|        HM_READ_EOS_IDEAL_GAS         source/materials/eos/hm_read_eos_ideal_gas.F
Chd|        HM_READ_EOS_IDEAL_GAS_VT      source/materials/eos/hm_read_eos_ideal_gas_vt.F
Chd|        HM_READ_EOS_LINEAR            source/materials/eos/hm_read_eos_linear.F
Chd|        HM_READ_EOS_LSZK              source/materials/eos/hm_read_eos_lszk.F
Chd|        HM_READ_EOS_MURNAGHAN         source/materials/eos/hm_read_eos_murnaghan.F
Chd|        HM_READ_EOS_NASG              source/materials/eos/hm_read_eos_nasg.F
Chd|        HM_READ_EOS_NOBLE_ABEL        source/materials/eos/hm_read_eos_noble_abel.F
Chd|        HM_READ_EOS_OSBORNE           source/materials/eos/hm_read_eos_osborne.F
Chd|        HM_READ_EOS_POLYNOMIAL        source/materials/eos/hm_read_eos_polynomial.F
Chd|        HM_READ_EOS_PUFF              source/materials/eos/hm_read_eos_puff.F
Chd|        HM_READ_EOS_SESAME            source/materials/eos/hm_read_eos_sesame.F
Chd|        HM_READ_EOS_STIFFENED_GAS     source/materials/eos/hm_read_eos_stiffened_gas.F
Chd|        HM_READ_EOS_TABULATED         source/materials/eos/hm_read_eos_tabulated.F
Chd|        HM_READ_EOS_TILLOTSON         source/materials/eos/hm_read_eos_tillotson.F
Chd|        HM_READ_FAIL_ALTER            source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|        HM_READ_FAIL_BIQUAD           source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|        HM_READ_FAIL_CHANG            source/materials/fail/changchang/hm_read_fail_chang.F
Chd|        HM_READ_FAIL_COCKCROFT        source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
Chd|        HM_READ_FAIL_CONNECT          source/materials/fail/connect/hm_read_fail_connect.F
Chd|        HM_READ_FAIL_EMC              source/materials/fail/emc/hm_read_fail_emc.F
Chd|        HM_READ_FAIL_ENERGY           source/materials/fail/energy/hm_read_fail_energy.F
Chd|        HM_READ_FAIL_FABRIC           source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|        HM_READ_FAIL_FLD              source/materials/fail/fld/hm_read_fail_fld.F
Chd|        HM_READ_FAIL_GENE1            source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|        HM_READ_FAIL_GURSON           source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_FAIL_HASHIN           source/materials/fail/hashin/hm_read_fail_hashin.F
Chd|        HM_READ_FAIL_HC_DSSE          source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
Chd|        HM_READ_FAIL_HOFFMAN          source/materials/fail/hoffman/hm_read_fail_hoffman.F
Chd|        HM_READ_FAIL_INIEVO           source/materials/fail/inievo/hm_read_fail_inievo.F
Chd|        HM_READ_FAIL_JOHNSON          source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|        HM_READ_FAIL_LADEVEZE         source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
Chd|        HM_READ_FAIL_MAXSTRN          source/materials/fail/max_strain/hm_read_fail_maxstrn.F
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
Chd|        HM_READ_FAIL_TSAIHILL         source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
Chd|        HM_READ_FAIL_TSAIWU           source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
Chd|        HM_READ_FAIL_VISUAL           source/materials/fail/visual/hm_read_fail_visual.F
Chd|        HM_READ_FAIL_WIERZBICKI       source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
Chd|        HM_READ_FAIL_WILKINS          source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INTER_TYPE02          source/interfaces/int02/hm_read_inter_type02.F
Chd|        HM_READ_MAT00                 source/materials/mat/mat000/hm_read_mat00.F
Chd|        HM_READ_MAT01                 source/materials/mat/mat001/hm_read_mat01.F
Chd|        HM_READ_MAT02                 source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MAT03                 source/materials/mat/mat003/hm_read_mat03.F
Chd|        HM_READ_MAT04                 source/materials/mat/mat004/hm_read_mat04.F
Chd|        HM_READ_MAT05                 source/materials/mat/mat005/hm_read_mat05.F
Chd|        HM_READ_MAT06                 source/materials/mat/mat006/hm_read_mat06.F
Chd|        HM_READ_MAT06_KEPS            source/materials/mat/mat006/hm_read_mat06_keps.F
Chd|        HM_READ_MAT10                 source/materials/mat/mat010/hm_read_mat10.F
Chd|        HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT101                source/materials/mat/mat101/hm_read_mat101.F
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
Chd|        HM_READ_MAT13                 source/materials/mat/mat013/hm_read_mat13.F
Chd|        HM_READ_MAT14                 source/materials/mat/mat014/hm_read_mat14.F
Chd|        HM_READ_MAT15                 source/materials/mat/mat015/hm_read_mat15.F
Chd|        HM_READ_MAT16                 source/materials/mat/mat016/hm_read_mat16.F
Chd|        HM_READ_MAT18                 source/materials/mat/mat018/hm_read_mat18.F
Chd|        HM_READ_MAT19                 source/materials/mat/mat019/hm_read_mat19.F
Chd|        HM_READ_MAT20                 source/materials/mat/mat020/hm_read_mat20.F
Chd|        HM_READ_MAT21                 source/materials/mat/mat021/hm_read_mat21.F
Chd|        HM_READ_MAT22                 source/materials/mat/mat022/hm_read_mat22.F
Chd|        HM_READ_MAT23                 source/materials/mat/mat023/hm_read_mat23.F
Chd|        HM_READ_MAT24                 source/materials/mat/mat024/hm_read_mat24.F
Chd|        HM_READ_MAT25                 source/materials/mat/mat025/hm_read_mat25.F
Chd|        HM_READ_MAT26                 source/materials/mat/mat026/hm_read_mat26.F
Chd|        HM_READ_MAT27                 source/materials/mat/mat027/hm_read_mat27.F
Chd|        HM_READ_MAT28                 source/materials/mat/mat028/hm_read_mat28.F
Chd|        HM_READ_MAT32                 source/materials/mat/mat032/hm_read_mat32.F
Chd|        HM_READ_MAT33                 source/materials/mat/mat033/hm_read_mat33.F
Chd|        HM_READ_MAT34                 source/materials/mat/mat034/hm_read_mat34.F
Chd|        HM_READ_MAT35                 source/materials/mat/mat035/hm_read_mat35.F
Chd|        HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT37                 source/materials/mat/mat037/hm_read_mat37.F
Chd|        HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT40                 source/materials/mat/mat040/hm_read_mat40.F
Chd|        HM_READ_MAT41                 source/materials/mat/mat041/hm_read_mat41.F.F
Chd|        HM_READ_MAT42                 source/materials/mat/mat042/hm_read_mat42.F
Chd|        HM_READ_MAT43                 source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT44                 source/materials/mat/mat044/hm_read_mat44.F
Chd|        HM_READ_MAT46                 source/materials/mat/mat046/hm_read_mat46.F
Chd|        HM_READ_MAT48                 source/materials/mat/mat048/hm_read_mat48.F
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
Chd|        HM_READ_MAT63                 source/materials/mat/mat063/hm_read_mat63.F
Chd|        HM_READ_MAT64                 source/materials/mat/mat064/hm_read_mat64.F
Chd|        HM_READ_MAT65                 source/materials/mat/mat065/hm_read_mat65.F
Chd|        HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT68                 source/materials/mat/mat068/hm_read_mat68.F
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
Chd|        HM_READ_MAT84                 source/materials/mat/mat084/hm_read_mat84.F
Chd|        HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT92                 source/materials/mat/mat092/hm_read_mat92.F
Chd|        HM_READ_MAT93                 source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MAT94                 source/materials/mat/mat094/hm_read_mat94.F
Chd|        HM_READ_MAT95                 source/materials/mat/mat095/hm_read_mat95.F
Chd|        HM_READ_MAT97                 source/materials/mat/mat097/hm_read_mat97.F
Chd|        HM_READ_MATGAS                source/materials/mat/matgas/hm_read_matgas.F
Chd|        HM_READ_MULLINS_OR            source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
Chd|        HM_READ_PROP0                 source/properties/void/hm_read_prop0.F
Chd|        HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
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
Chd|        HM_READ_PROP27                source/properties/spring/hm_read_prop27.F
Chd|        HM_READ_PROP28                source/properties/xelem/hm_read_prop28.F
Chd|        HM_READ_PROP32                source/properties/spring/hm_read_prop32.F
Chd|        HM_READ_PROP33                source/properties/spring/hm_read_prop33.F
Chd|        HM_READ_PROP34                source/properties/sph/hm_read_prop34.F
Chd|        HM_READ_PROP35                source/properties/spring/hm_read_prop35.F
Chd|        HM_READ_PROP36                source/properties/spring/hm_read_prop36.F
Chd|        HM_READ_PROP43                source/properties/solid/hm_read_prop43.F
Chd|        HM_READ_PROP44                source/properties/spring/hm_read_prop44.F
Chd|        HM_READ_PROP45                source/properties/spring/hm_read_prop45.F
Chd|        HM_READ_PROP46                source/properties/spring/hm_read_prop46.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_REFSTA                source/loads/reference_state/refsta/hm_read_refsta.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        HM_READ_TABLE1_0              source/tools/curve/hm_read_table1_0.F
Chd|        HM_READ_TABLE1_1              source/tools/curve/hm_read_table1_1.F
Chd|        HM_READ_TABLE2_0              source/tools/curve/hm_read_table2_0.F
Chd|        HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        LAW100_UPD_AB                 source/materials/mat/mat100/law100_upd.F
Chd|        LAW69_UPD                     source/materials/mat/mat069/law69_upd.F
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|        LAW92_UPD                     source/materials/mat/mat092/law92_upd.F
Chd|        LCGEO19                       source/elements/shell/coque/lcgeo19.F
Chd|        READ_ALE_MAT                  source/materials/ale/read_ale_mat.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        READ_EULER_MAT                source/materials/ale/read_euler_mat.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   GIVE THE INFORMATION THAT AT LEAST ONE LINE OF AN OPTION IS CRYPTED
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME               DESCRIPTION                         
C
C     IS_ENCRYPTED   IS AT LEAST ONE LINE OF THE OPTION CRYPTED TRUE OR FALSE
C============================================================================
C   M o d u l e s
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C OUTPUT ARGUMENTS
      LOGICAL,INTENT(OUT)::IS_ENCRYPTED
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      CALL CPP_OPTION_IS_CRYPTED(IS_ENCRYPTED) 
      RETURN
C
      END
