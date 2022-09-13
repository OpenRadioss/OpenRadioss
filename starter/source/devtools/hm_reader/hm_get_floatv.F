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
Chd|  HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|-- called by -----------
Chd|        HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
Chd|        HM_READ_ACTIV                 source/tools/activ/hm_read_activ.F
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_ALE_CLOSE             source/ale/hm_read_ale_close.F
Chd|        HM_READ_ALE_GRID              source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|        HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_ALE_MUSCL             source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_EBCS_GRADP0           source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
Chd|        HM_READ_EBCS_INIP             source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
Chd|        HM_READ_EBCS_INIV             source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
Chd|        HM_READ_EBCS_INLET            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        HM_READ_EBCS_NORMV            source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
Chd|        HM_READ_EBCS_NRF              source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
Chd|        HM_READ_EBCS_PRES             source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
Chd|        HM_READ_EBCS_VALVIN           source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
Chd|        HM_READ_EBCS_VALVOUT          source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
Chd|        HM_READ_EBCS_VEL              source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
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
Chd|        HM_READ_FAIL_JOHNSON          source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|        HM_READ_FAIL_LADEVEZE         source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
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
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_INIGRAV               source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_INTER_HERTZ_TYPE17    source/interfaces/int17/hm_read_inter_hertz_type17.F
Chd|        HM_READ_INTER_LAGDT_TYPE07    source/interfaces/int07/hm_read_inter_lagdt_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE02   source/interfaces/int02/hm_read_inter_lagmul_type02.F
Chd|        HM_READ_INTER_LAGMUL_TYPE07   source/interfaces/int07/hm_read_inter_lagmul_type07.F
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
Chd|        HM_READ_INTER_TYPE23          source/interfaces/int23/hm_read_inter_type23.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|        HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
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
Chd|        HM_READ_MAT51_IFORM11         source/materials/mat/mat051/hm_read_mat51_iform11.F
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
Chd|        HM_READ_MERGE_NODE            source/elements/reader/hm_read_merge_node.F
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
Chd|        HM_READ_MOVE_FUNCT            source/tools/curve/hm_read_move_funct.F
Chd|        HM_READ_MULLINS_OR            source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
Chd|        HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PERTURB_PART_SHELL    source/general_controls/computation/hm_read_perturb_part_shell.F
Chd|        HM_READ_PERTURB_PART_SOLID    source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_PROP0                 source/properties/void/hm_read_prop0.F
Chd|        HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
Chd|        HM_READ_PROP02                source/properties/truss/hm_read_prop02.F
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
Chd|        HM_READ_PROP_USER4            source/properties/user_spring_solid/hm_read_prop_user4.F
Chd|        HM_READ_RADIATION             source/loads/thermic/hm_read_radiation.F
Chd|        HM_READ_RAND                  source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_THERM                 source/materials/therm/hm_read_therm.F
Chd|        HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|        HM_READ_UPWIND                source/general_controls/computation/hm_read_upwind.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        LCGEO19                       source/elements/shell/coque/lcgeo19.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECLAS                        source/loads/laser/leclas.F   
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        LEC_DOF_JNT                   source/properties/spring/hm_read_prop45.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                source/model/box/read_box_spher.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
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
Chd|        SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GET_FLOATV(NAME,RVAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE
C   ASK (REAL) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   APPLY AUTOMATICALLY UNIT SYSTEM USING DIMENSION DEFINED IN .cfg FILE
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     RVAL         REAL VALUE OF THE FIELD 
C     IS_AVAILABLE VALUE AVAILABLE IN MODEL OR NOT
C     LSUBMODEL    SUBMODEL STRUCTURE       
C     UNITAB       UNIT ARRAY       
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      my_real,
     .  INTENT(OUT)::RVAL
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,VALUE_TYPE,SUB_ID,IFLAGUNIT,UID
      my_real
     .   FAC_L,FAC_M,FAC_T,FAC
      REAL*8 DVAL,LENGTH_DIM,MASS_DIM,TIME_DIM
      LOGICAL(KIND=C_BOOL) C_IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C--------------------------------------------------
        C_IS_AVAILABLE = .FALSE.
        LENGTH_DIM = ZERO
        MASS_DIM = ZERO
        TIME_DIM = ZERO
        FAC = ONE
C--------------------------------------------------
        CALL CPP_GET_FLOATV_FLOATD(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),DVAL,C_IS_AVAILABLE,
     .                    LENGTH_DIM,MASS_DIM,TIME_DIM,UID,SUB_ID)
        IS_AVAILABLE = C_IS_AVAILABLE
C--------------------------------------------------
C      ID OFFSETS FOR //SUBMODEL 
C--------------------------------------------------
        IF(SUB_ID /= 0 .AND. UID /= 0)THEN
          UID = UID + LSUBMODEL(SUB_ID)%OFF_DEF
        ELSEIF(SUB_ID /= 0 )THEN
          IF(LSUBMODEL(SUB_ID)%UID /= 0)THEN
            UID = LSUBMODEL(SUB_ID)%UID
          ENDIF
        ENDIF
C--------------------------------------------------
c      APPLY UNIT SYSTEM
C--------------------------------------------------
        IFLAGUNIT = 0 
        FAC_M = ZERO
        FAC_L = ZERO
        FAC_T = ZERO         
        DO J=1,NUNITS                         
          IF (UNITAB%UNIT_ID(J) == UID) THEN    
            FAC_M = UNITAB%FAC_M(J)
            FAC_L = UNITAB%FAC_L(J)
            FAC_T = UNITAB%FAC_T(J)
            IFLAGUNIT = 1                      
            EXIT                              
          ENDIF        
        ENDDO
        IF (FAC_M  /= ZERO) FAC = FAC * (FAC_M ** MASS_DIM  )
        IF (FAC_L  /= ZERO) FAC = FAC * (FAC_L ** LENGTH_DIM)
        IF (FAC_T  /= ZERO) FAC = FAC * (FAC_T ** TIME_DIM  )     
C-------------------------------------------------- 
        RVAL  = DVAL *  FAC
c        print *,'real value=',NAME(1:LEN_TRIM(NAME)),RVAL,LENGTH_DIM,MASS_DIM,TIME_DIM
C--------------------------------------------------                             
      RETURN
C
      END
