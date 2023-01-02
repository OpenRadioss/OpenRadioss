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
Chd|  UNITAB_MOD                    modules/unitab_mod.F          
Chd|-- called by -----------
Chd|        READ_UNITS                    common_source/comm/write_units.F
Chd|        WRITE_UNITS                   common_source/comm/write_units.F
Chd|        DDSPLIT                       starter/priv/spmd1/ddsplit.F  
Chd|        EBCNODE                       starter/priv/ebcs1/lecebcs.F  
Chd|        HM_DEBUG_PRINT_OPTION         starter/source/devtools/hm_reader/hm_debug_print_option.F
Chd|        HM_GET_FLOATV                 starter/source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             starter/source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      starter/source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_PREREAD_CONVEC             starter/source/loads/thermic/hm_preread_convec.F
Chd|        HM_PREREAD_IMPFLUX            starter/source/constraints/thermic/hm_preread_impflux.F
Chd|        HM_PREREAD_IMPTEMP            starter/source/constraints/thermic/hm_preread_imptemp.F
Chd|        HM_PREREAD_RADIATION          starter/source/loads/thermic/hm_preread_radiation.F
Chd|        HM_PREREAD_RBE3               starter/source/constraints/general/rbe3/hm_preread_rbe3.F
Chd|        HM_PROP_READ21                starter/source/properties/thickshell/hm_read_prop21.F
Chd|        HM_READ_ADMAS                 starter/source/tools/admas/hm_read_admas.F
Chd|        HM_READ_BCS                   starter/source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_BOX                   starter/source/model/box/hm_read_box.F
Chd|        HM_READ_CLOAD                 starter/source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               starter/source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                starter/source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_DAMP                  starter/source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_EOS                   starter/source/materials/eos/hm_read_eos.F
Chd|        HM_READ_FAIL                  starter/source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FAIL_ALTER            starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|        HM_READ_FAIL_BIQUAD           starter/source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|        HM_READ_FAIL_CHANG            starter/source/materials/fail/changchang/hm_read_fail_chang.F
Chd|        HM_READ_FAIL_COCKCROFT        starter/source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
Chd|        HM_READ_FAIL_CONNECT          starter/source/materials/fail/connect/hm_read_fail_connect.F
Chd|        HM_READ_FAIL_EMC              starter/source/materials/fail/emc/hm_read_fail_emc.F
Chd|        HM_READ_FAIL_ENERGY           starter/source/materials/fail/energy/hm_read_fail_energy.F
Chd|        HM_READ_FAIL_FABRIC           starter/source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|        HM_READ_FAIL_FLD              starter/source/materials/fail/fld/hm_read_fail_fld.F
Chd|        HM_READ_FAIL_HASHIN           starter/source/materials/fail/hashin/hm_read_fail_hashin.F
Chd|        HM_READ_FAIL_HC_DSSE          starter/source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
Chd|        HM_READ_FAIL_JOHNSON          starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|        HM_READ_FAIL_LADEVEZE         starter/source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
Chd|        HM_READ_FAIL_NXT              starter/source/materials/fail/nxt/hm_read_fail_nxt.F
Chd|        HM_READ_FAIL_ORTHSTRAIN       starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
Chd|        HM_READ_FAIL_PUCK             starter/source/materials/fail/puck/hm_read_fail_puck.F
Chd|        HM_READ_FAIL_SNCONNECT        starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
Chd|        HM_READ_FAIL_SPALLING         starter/source/materials/fail/spalling/hm_read_fail_spalling.F
Chd|        HM_READ_FAIL_TAB1             starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
Chd|        HM_READ_FAIL_TAB_OLD          starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|        HM_READ_FAIL_TBUTCHER         starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
Chd|        HM_READ_FAIL_TENSSTRAIN       starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
Chd|        HM_READ_FAIL_WIERZBICKI       starter/source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
Chd|        HM_READ_FAIL_WILKINS          starter/source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|        HM_READ_FRICTION              starter/source/interfaces/friction/reader/hm_read_friction.F
Chd|        HM_READ_FRICTION_MODELS       starter/source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_FRM                   starter/source/tools/skew/hm_read_frm.F
Chd|        HM_READ_GRAV                  starter/source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_IMPACC                starter/source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               starter/source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPTEMP               starter/source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_INITEMP               starter/source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                starter/source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INJECT1               starter/source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               starter/source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_INTERFACES            starter/source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_FSI             starter/source/interfaces/reader/hm_read_inter_fsi.F
Chd|        HM_READ_INTER_STRUCT          starter/source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_INTER_TYPE07          starter/source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE10          starter/source/interfaces/int10/hm_read_inter_type10.F
Chd|        HM_READ_INTER_TYPE11          starter/source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE25          starter/source/interfaces/int25/hm_read_inter_type25.F
Chd|        HM_READ_LOAD_CENTRI           starter/source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_MAT                   starter/source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT00                 starter/source/materials/mat/mat000/hm_read_mat00.F
Chd|        HM_READ_MAT01                 starter/source/materials/mat/mat001/hm_read_mat01.F
Chd|        HM_READ_MAT02                 starter/source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MAT06                 starter/source/materials/mat/mat006/hm_read_mat06.F
Chd|        HM_READ_MAT06_KEPS            starter/source/materials/mat/mat006/hm_read_mat06_keps.F
Chd|        HM_READ_MAT10                 starter/source/materials/mat/mat010/hm_read_mat10.F
Chd|        HM_READ_MAT100                starter/source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT102                starter/source/materials/mat/mat102/hm_read_mat102.F
Chd|        HM_READ_MAT103                starter/source/materials/mat/mat103/hm_read_mat103.F
Chd|        HM_READ_MAT106                starter/source/materials/mat/mat106/hm_read_mat106.F
Chd|        HM_READ_MAT108                starter/source/materials/mat/mat108/hm_read_mat108.F
Chd|        HM_READ_MAT109                starter/source/materials/mat/mat109/hm_read_mat109.F
Chd|        HM_READ_MAT11                 starter/source/materials/mat/mat011/hm_read_mat11.F
Chd|        HM_READ_MAT110                starter/source/materials/mat/mat110/hm_read_mat110.F
Chd|        HM_READ_MAT111                starter/source/materials/mat/mat111/hm_read_mat111.F
Chd|        HM_READ_MAT112                starter/source/materials/mat/mat112/hm_read_mat112.F
Chd|        HM_READ_MAT113                starter/source/materials/mat/mat113/hm_read_mat113.F
Chd|        HM_READ_MAT116                starter/source/materials/mat/mat116/hm_read_mat116.F
Chd|        HM_READ_MAT12                 starter/source/materials/mat/mat012/hm_read_mat12.F
Chd|        HM_READ_MAT13                 starter/source/materials/mat/mat013/hm_read_mat13.F
Chd|        HM_READ_MAT14                 starter/source/materials/mat/mat014/hm_read_mat14.F
Chd|        HM_READ_MAT15                 starter/source/materials/mat/mat015/hm_read_mat15.F
Chd|        HM_READ_MAT151                starter/source/materials/mat/mat151/hm_read_mat151.F
Chd|        HM_READ_MAT19                 starter/source/materials/mat/mat019/hm_read_mat19.F
Chd|        HM_READ_MAT20                 starter/source/materials/mat/mat020/hm_read_mat20.F
Chd|        HM_READ_MAT21                 starter/source/materials/mat/mat021/hm_read_mat21.F
Chd|        HM_READ_MAT22                 starter/source/materials/mat/mat022/hm_read_mat22.F
Chd|        HM_READ_MAT23                 starter/source/materials/mat/mat023/hm_read_mat23.F
Chd|        HM_READ_MAT24                 starter/source/materials/mat/mat024/hm_read_mat24.F
Chd|        HM_READ_MAT25                 starter/source/materials/mat/mat025/hm_read_mat25.F
Chd|        HM_READ_MAT27                 starter/source/materials/mat/mat027/hm_read_mat27.F
Chd|        HM_READ_MAT28                 starter/source/materials/mat/mat028/hm_read_mat28.F
Chd|        HM_READ_MAT32                 starter/source/materials/mat/mat032/hm_read_mat32.F
Chd|        HM_READ_MAT33                 starter/source/materials/mat/mat033/hm_read_mat33.F
Chd|        HM_READ_MAT34                 starter/source/materials/mat/mat034/hm_read_mat34.F
Chd|        HM_READ_MAT35                 starter/source/materials/mat/mat035/hm_read_mat35.F
Chd|        HM_READ_MAT36                 starter/source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT37                 starter/source/materials/mat/mat037/hm_read_mat37.F
Chd|        HM_READ_MAT38                 starter/source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT40                 starter/source/materials/mat/mat040/hm_read_mat40.F
Chd|        HM_READ_MAT42                 starter/source/materials/mat/mat042/hm_read_mat42.F
Chd|        HM_READ_MAT43                 starter/source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT44                 starter/source/materials/mat/mat044/hm_read_mat44.F
Chd|        HM_READ_MAT46                 starter/source/materials/mat/mat046/hm_read_mat46.F
Chd|        HM_READ_MAT50                 starter/source/materials/mat/mat050/hm_read_mat50.F
Chd|        HM_READ_MAT51                 starter/source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_MAT51_IFORM11         starter/source/materials/mat/mat051/hm_read_mat51_iform11.F
Chd|        HM_READ_MAT52                 starter/source/materials/mat/mat052/hm_read_mat52.F
Chd|        HM_READ_MAT53                 starter/source/materials/mat/mat053/hm_read_mat53.F
Chd|        HM_READ_MAT54                 starter/source/materials/mat/mat054/hm_read_mat54.F
Chd|        HM_READ_MAT57                 starter/source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT58                 starter/source/materials/mat/mat058/hm_read_mat58.F
Chd|        HM_READ_MAT59                 starter/source/materials/mat/mat059/hm_read_mat59.F
Chd|        HM_READ_MAT60                 starter/source/materials/mat/mat060/hm_read_mat60.F
Chd|        HM_READ_MAT62                 starter/source/materials/mat/mat062/hm_read_mat62.F
Chd|        HM_READ_MAT64                 starter/source/materials/mat/mat064/hm_read_mat64.F
Chd|        HM_READ_MAT65                 starter/source/materials/mat/mat065/hm_read_mat65.F
Chd|        HM_READ_MAT66                 starter/source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT68                 starter/source/materials/mat/mat068/hm_read_mat68.F
Chd|        HM_READ_MAT69                 starter/source/materials/mat/mat069/hm_read_mat69.F
Chd|        HM_READ_MAT70                 starter/source/materials/mat/mat070/hm_read_mat70.F
Chd|        HM_READ_MAT71                 starter/source/materials/mat/mat071/hm_read_mat71.F
Chd|        HM_READ_MAT72                 starter/source/materials/mat/mat072/hm_read_mat72.F
Chd|        HM_READ_MAT73                 starter/source/materials/mat/mat073/hm_read_mat73.F
Chd|        HM_READ_MAT74                 starter/source/materials/mat/mat074/hm_read_mat74.F
Chd|        HM_READ_MAT75                 starter/source/materials/mat/mat075/hm_read_mat75.F
Chd|        HM_READ_MAT76                 starter/source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT77                 starter/source/materials/mat/mat077/hm_read_mat77.F
Chd|        HM_READ_MAT78                 starter/source/materials/mat/mat078/hm_read_mat78.F
Chd|        HM_READ_MAT79                 starter/source/materials/mat/mat079/hm_read_mat79.F
Chd|        HM_READ_MAT80                 starter/source/materials/mat/mat080/hm_read_mat80.F
Chd|        HM_READ_MAT82                 starter/source/materials/mat/mat082/hm_read_mat82.F
Chd|        HM_READ_MAT83                 starter/source/materials/mat/mat083/hm_read_law83.F
Chd|        HM_READ_MAT84                 starter/source/materials/mat/mat084/hm_read_mat84.F
Chd|        HM_READ_MAT87                 starter/source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 starter/source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 starter/source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT92                 starter/source/materials/mat/mat092/hm_read_mat92.F
Chd|        HM_READ_MAT93                 starter/source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MAT94                 starter/source/materials/mat/mat094/hm_read_mat94.F
Chd|        HM_READ_MAT95                 starter/source/materials/mat/mat095/hm_read_mat95.F
Chd|        HM_READ_MAT97                 starter/source/materials/mat/mat097/hm_read_mat97.F
Chd|        HM_READ_MATGAS                starter/source/materials/mat/matgas/hm_read_matgas.F
Chd|        HM_READ_MONVOL_TYPE2          starter/source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE4          starter/source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE7          starter/source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MOVE_FUNCT            starter/source/tools/curve/hm_read_move_funct.F
Chd|        HM_READ_MULLINS_OR            starter/source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
Chd|        HM_READ_NODE                  starter/source/elements/reader/hm_read_node.F
Chd|        HM_READ_PART                  starter/source/model/assembling/hm_read_part.F
Chd|        HM_READ_PFLUID                starter/source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 starter/source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PROP0                 starter/source/properties/void/hm_read_prop0.F
Chd|        HM_READ_PROP01                starter/source/properties/shell/hm_read_prop01.F
Chd|        HM_READ_PROP02                starter/source/properties/truss/hm_read_prop02.F
Chd|        HM_READ_PROP03                starter/source/properties/beam/hm_read_prop03.F
Chd|        HM_READ_PROP04                starter/source/properties/spring/hm_read_prop04.F
Chd|        HM_READ_PROP05                starter/source/properties/rivet/hm_read_prop05.F
Chd|        HM_READ_PROP06                starter/source/properties/solid/hm_read_prop06.F
Chd|        HM_READ_PROP08                starter/source/properties/spring/hm_read_prop08.F
Chd|        HM_READ_PROP09                starter/source/properties/shell/hm_read_prop09.F
Chd|        HM_READ_PROP10                starter/source/properties/shell/hm_read_prop10.F
Chd|        HM_READ_PROP11                starter/source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP12                starter/source/properties/spring/hm_read_prop12.F
Chd|        HM_READ_PROP13                starter/source/properties/spring/hm_read_prop13.F
Chd|        HM_READ_PROP14                starter/source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP14F               starter/source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP15                starter/source/properties/solid/hm_read_prop15.F
Chd|        HM_READ_PROP16                starter/source/properties/shell/hm_read_prop16.F
Chd|        HM_READ_PROP18                starter/source/properties/beam/hm_read_prop18.F
Chd|        HM_READ_PROP19                starter/source/properties/shell/hm_read_prop19.F
Chd|        HM_READ_PROP20                starter/source/properties/thickshell/hm_read_prop20.F
Chd|        HM_READ_PROP22                starter/source/properties/thickshell/hm_read_prop22.F
Chd|        HM_READ_PROP23                starter/source/properties/spring/hm_read_prop23.F
Chd|        HM_READ_PROP25                starter/source/properties/spring/hm_read_prop25.F
Chd|        HM_READ_PROP26                starter/source/properties/spring/hm_read_prop26.F
Chd|        HM_READ_PROP28                starter/source/properties/xelem/hm_read_prop28.F
Chd|        HM_READ_PROP32                starter/source/properties/spring/hm_read_prop32.F
Chd|        HM_READ_PROP33                starter/source/properties/spring/hm_read_prop33.F
Chd|        HM_READ_PROP33_CYL_JNT        starter/source/properties/spring/hm_read_prop33_cyl_jnt.F
Chd|        HM_READ_PROP33_FIX_JNT        starter/source/properties/spring/hm_read_prop33_fix_jnt.F
Chd|        HM_READ_PROP33_FREE_JNT       starter/source/properties/spring/hm_read_prop33_free_jnt.F
Chd|        HM_READ_PROP33_OLD_JNT        starter/source/properties/spring/hm_read_prop33_old_jnt.F
Chd|        HM_READ_PROP33_PLAN_JNT       starter/source/properties/spring/hm_read_prop33_plan_jnt.F
Chd|        HM_READ_PROP33_REV_JNT        starter/source/properties/spring/hm_read_prop33_rev_jnt.F
Chd|        HM_READ_PROP33_SPH_JNT        starter/source/properties/spring/hm_read_prop33_sph_jnt.F
Chd|        HM_READ_PROP33_TRANS_JNT      starter/source/properties/spring/hm_read_prop33_trans_jnt.F
Chd|        HM_READ_PROP33_UNIV_JNT       starter/source/properties/spring/hm_read_prop33_univ_jnt.F
Chd|        HM_READ_PROP34                starter/source/properties/sph/hm_read_prop34.F
Chd|        HM_READ_PROP36                starter/source/properties/spring/hm_read_prop36.F
Chd|        HM_READ_PROP43                starter/source/properties/solid/hm_read_prop43.F
Chd|        HM_READ_PROP44                starter/source/properties/spring/hm_read_prop44.F
Chd|        HM_READ_PROP45                starter/source/properties/spring/hm_read_prop45.F
Chd|        HM_READ_PROP46                starter/source/properties/spring/hm_read_prop46.F
Chd|        HM_READ_PROPERTIES            starter/source/properties/hm_read_properties.F
Chd|        HM_READ_PROP_GENERIC          starter/source/properties/hm_read_prop_generic.F
Chd|        HM_READ_RADIATION             starter/source/loads/thermic/hm_read_radiation.F
Chd|        HM_READ_RAND                  starter/source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBE3                  starter/source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 starter/source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RWALL_CYL             starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_PARAL           starter/source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           starter/source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           starter/source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_SH3N                  starter/source/elements/reader/hm_read_sh3n.F
Chd|        HM_READ_SHELL                 starter/source/elements/reader/hm_read_shell.F
Chd|        HM_READ_SKW                   starter/source/tools/skew/hm_read_skw.F
Chd|        HM_READ_VISC                  starter/source/materials/visc/hm_read_visc.F
Chd|        HM_READ_XREF                  starter/source/loads/reference_state/xref/hm_read_xref.F
Chd|        INIT_MONVOL                   starter/source/airbag/init_monvol.F
Chd|        INIVEL                        starter/source/initial_conditions/general/inivel/inivel.F
Chd|        LCGEO10                       starter/src/coque1/lcgeo10.F  
Chd|        LCGEO17                       starter/src/coque1/lcgeo17.F  
Chd|        LCGEO19                       starter/src/coque1/lcgeo19.F  
Chd|        LCGEO51                       starter/src/coque1/lecgeo51.F 
Chd|        LCGEO9                        starter/src/coque1/lcgeo9.F   
Chd|        LECACC                        starter/src/crash1/lecacc.F   
Chd|        LECEBCS1                      starter/priv/ebcs1/lecebcs1.F 
Chd|        LECEBCSI                      starter/priv/ebcs1/lecebcs.F  
Chd|        LECEIG2                       starter/src/eig1/leceig.F     
Chd|        LECEOS                        starter/src/eos1/leceos.F     
Chd|        LECEOS00                      starter/src/eos1/leceos00.F   
Chd|        LECEOS01                      starter/src/eos1/leceos01.F   
Chd|        LECEOS02                      starter/src/eos1/leceos02.F   
Chd|        LECEOS03                      starter/src/eos1/leceos03.F   
Chd|        LECEOS04                      starter/src/eos1/leceos04.F   
Chd|        LECEOS05                      starter/src/eos1/leceos05.F   
Chd|        LECEOS06                      starter/src/eos1/leceos06.F   
Chd|        LECEOS07                      starter/src/eos1/leceos07.F   
Chd|        LECEOS08                      starter/src/eos1/leceos08.F   
Chd|        LECEOS09                      starter/src/eos1/leceos09.F   
Chd|        LECEOS10                      starter/src/eos1/leceos10.F   
Chd|        LECEOS11                      starter/src/eos1/leceos11.F   
Chd|        LECEOS12                      starter/src/eos1/leceos12.F   
Chd|        LECEOS13                      starter/src/eos1/leceos13.F   
Chd|        LECEOS14                      starter/src/eos1/leceos14.F   
Chd|        LECEREF                       starter/src/lectur1/leceref.F 
Chd|        LECFAIL                       starter/source/materials/fail/lecfail.F
Chd|        LECFAIL_GURSON                starter/source/materials/fail/gurson/lecfail_gurson.F
Chd|        LECFAIL_VISUAL                starter/source/materials/fail/visual/lecfail_visual.F
Chd|        LECFLOW1                      starter/priv/flow1/lecflow.F  
Chd|        LECFLOW2                      starter/priv/flow1/lecflow.F  
Chd|        LECG28                        starter/src/nstrand1/lecg28.F 
Chd|        LECG35                        starter/src/user1/lecg35.F    
Chd|        LECGAU                        starter/src/crash1/lecgau.F   
Chd|        LECGEO                        starter/src/lectur1/lecgeo.F  
Chd|        LECGJOINT                     starter/src/lectur1/lecgjoint.F
Chd|        LECGRE                        starter/src/lectur1/lecgre.F  
Chd|        LECGRN                        starter/src/lectur1/lecgrn.F  
Chd|        LECGROUP                      starter/src/lectur1/lecgroup.F
Chd|        LECGUSER                      starter/src/lectur1/lecguser.F
Chd|        LECINIGRAV                    starter/src/inigrav1/lecinigrav.F
Chd|        LECINT                        starter/src/interf1/lecint.F  
Chd|        LECLEAK                       starter/priv/airbag1/lecleak.F
Chd|        LECM03                        starter/source/materials/mat/mat003/lecm03.F
Chd|        LECM04                        starter/source/materials/mat/mat004/lecm04.F
Chd|        LECM05                        starter/source/materials/mat/mat005/lecm05.F
Chd|        LECM06                        starter/source/materials/mat/mat006/lecm06.F
Chd|        LECM101                       starter/source/materials/mat/mat101/lecm101.F
Chd|        LECM104                       starter/source/materials/mat/mat104/lecm104.F
Chd|        LECM105                       starter/source/materials/mat/mat105/lecm105.F
Chd|        LECM11                        starter/source/materials/mat/mat011/lecm11.F
Chd|        LECM187                       starter/source/materials/mat/mat187/lecm187.F
Chd|        LECM41                        starter/source/materials/mat/mat041/lecm41.F
Chd|        LECM49                        starter/source/materials/mat/mat049/lecm49.F
Chd|        LECM57                        starter/src/user1/lecm57.F    
Chd|        LECM70                        starter/source/materials/mat/mat070/lecm70.F
Chd|        LECM72                        starter/source/materials/mat/mat072/lecm72.F
Chd|        LECM90                        starter/source/materials/mat/mat090/lecm90.F
Chd|        LECM91                        starter/source/materials/mat/mat091/lecm91.F
Chd|        LECM96                        starter/source/materials/mat/mat096/lecm96.F
Chd|        LECM98                        starter/source/materials/mat/mat098/lecm98.F
Chd|        LECMAT                        starter/src/mater1/lecmat.F   
Chd|        LECMODGROUP                   starter/src/crash1/lecmodgroup.F
Chd|        LECMODUNIT                    starter/src/crash1/lecmodunit.F
Chd|        LECPRE                        starter/source/loads/general/pload/lecpre.F
Chd|        LECPRELOAD                    starter/src/kinld1/lecpreload.F
Chd|        LECR13                        starter/source/materials/fail/changchang/lecfail_changchang.F
Chd|        LECR15                        starter/source/materials/fail/yamada_sun/lecfail_yamada_sun.F
Chd|        LECR17                        starter/source/materials/fail/tuler_butcher/lecfail_tbutcher_xfem.F
Chd|        LECR19                        starter/source/materials/fail/delamin_damage/lecfail_delamin_damage.F
Chd|        LECR21                        starter/source/materials/fail/johnson_cook/lecfail_jcook_xfem.F
Chd|        LECR29                        starter/source/materials/fail/saharei/lecfail_saharei.F
Chd|        LECREFSTA                     starter/src/lectur1/lecrefsta.F
Chd|        LECSEC42                      starter/source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   starter/source/tools/sect/lecsec4bolt.F
Chd|        LECSEN                        starter/src/crash1/lecsen.F   
Chd|        LECSEN4BOLT                   starter/src/bolt/lecsen4bolt.F
Chd|        LECSLIN                       starter/src/lectur1/lecslin.F 
Chd|        LECSTACK_PLY                  starter/src/stack1/lecstack_ply.F
Chd|        LECSTAMP                      starter/src/interf1/lecstamp.F
Chd|        LECSUBMOD                     starter/src/lectur1/lecsubmod.F
Chd|        LECSURF                       starter/src/lectur1/lecsurf.F 
Chd|        LECTHERM                      starter/source/materials/therm/lectherm.F
Chd|        LECTRANS                      starter/src/lectur1/lectrans.F
Chd|        LECTRANSSUB                   starter/src/lectur1/lectranssub.F
Chd|        LECUNIT                       starter/src/lectur1/lecunit.F 
Chd|        LECVEL                        starter/src/kinld1/lecvel.F   
Chd|        LECVISC                       starter/src/mater1/lecvisc.F  
Chd|        LECVISC01                     starter/src/visc1/lecvisc01.F 
Chd|        LEC_DOF_JNT                   starter/source/properties/spring/hm_read_prop45.F
Chd|        LEC_INISTATE                  starter/src/init1/lec_inistate.F
Chd|        LEC_INISTATE_D00              starter/src/init1/lec_inistate_d00.F
Chd|        READ_BOX_CYL                  starter/source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 starter/source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                starter/source/model/box/read_box_spher.F
Chd|        READ_DETONATORS               starter/source/initial_conditions/dfs/read_detonators.F
Chd|        READ_DFS_DETCORD              starter/source/initial_conditions/dfs/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              starter/source/initial_conditions/dfs/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              starter/source/initial_conditions/dfs/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             starter/source/initial_conditions/dfs/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          starter/source/initial_conditions/dfs/read_dfs_wave_shaper.F
Chd|        READ_ENGINE_DRIVER            starter/source/general_controls/engine/read_engine_driver.F
Chd|        READ_IMPDISP                  starter/source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPDISP_FGEO             starter/source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL                   starter/source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              starter/source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_INTERFACES               starter/source/interfaces/reader/read_interfaces.F
Chd|        READ_INTER_FSI                starter/source/interfaces/reader/read_inter_fsi.F
Chd|        READ_INTER_KINE_TYPE18        starter/source/interfaces/int18/read_inter_kine_type18.F
Chd|        READ_INTER_LAGDT_TYPE07       starter/source/interfaces/int07/read_inter_lagdt_type07.F
Chd|        READ_INTER_LAGMUL             starter/source/interfaces/reader/read_inter_lagmul.F
Chd|        READ_INTER_STRUCT             starter/source/interfaces/reader/read_inter_struct.F
Chd|        READ_MATERIAL_MODELS          starter/source/materials/read_material_models.F
Chd|        READ_MONVOL                   starter/source/airbag/read_monvol.F
Chd|        READ_MONVOL_TYPE1             starter/source/airbag/read_monvol_type1.F
Chd|        READ_MONVOL_TYPE10            starter/source/airbag/read_monvol_type10.F
Chd|        READ_MONVOL_TYPE11            starter/source/airbag/read_monvol_type11.F
Chd|        READ_MONVOL_TYPE3             starter/source/airbag/read_monvol_type3.F
Chd|        READ_MONVOL_TYPE5             starter/source/airbag/read_monvol_type5.F
Chd|        READ_MONVOL_TYPE6             starter/source/airbag/read_monvol_type6.F
Chd|        READ_MONVOL_TYPE8             starter/source/airbag/read_monvol_type8.F
Chd|        READ_MONVOL_TYPE9             starter/source/airbag/read_monvol_type9.F
Chd|        READ_PBLAST                   starter/source/loads/dfs/pblast/read_pblast.F
Chd|        READ_RWALL                    starter/source/constraints/general/rwall/read_rwall.F
Chd|        TRIALECEL                     starter/src/tria1/trialeclel.F
Chd|        RESTMOD                       starter/share/modules1/restart_mod.F
Chd|        UNITS_MOD                     engine/src/modules/units_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE UNITAB_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c define type NLOCAL_STRUCT_ for nodal variable regularization
c=======================================================================      
c 
      TYPE UNIT_TYPE_
        INTEGER     :: NUNITS                             ! Number of UNIT options in input deck 
        INTEGER, DIMENSION(:)   , ALLOCATABLE :: UNIT_ID  ! (NUNITS) Unit user ID
        my_real, DIMENSION(:)   , ALLOCATABLE :: FAC_M    ! (NUNITS) Mass Unit
        my_real, DIMENSION(:)   , ALLOCATABLE :: FAC_L    ! (NUNITS) Length Unit    
        my_real, DIMENSION(:)   , ALLOCATABLE :: FAC_T    ! (NUNITS) Temperature Unit
      END TYPE UNIT_TYPE_
c---------------
c     MODE = 0 => no variable regularization, no structure allocation
c     MODE = 1 => Forest non-local regularization (micromorphic) 
c     MODE = 2 => Peerlings non-local regularization
c---------------
      END MODULE  UNITAB_MOD
