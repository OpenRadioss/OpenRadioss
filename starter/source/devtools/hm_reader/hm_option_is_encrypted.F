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
      !||    hm_option_is_encrypted      ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- called by ------------------------------------------------------
      !||    hm_prop_read21              ../starter/source/properties/thickshell/hm_read_prop21.F
      !||    hm_read_ebcs_fluxout        ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
      !||    hm_read_ebcs_gradp0         ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
      !||    hm_read_ebcs_inip           ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
      !||    hm_read_ebcs_iniv           ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
      !||    hm_read_ebcs_inlet          ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
      !||    hm_read_ebcs_monvol         ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
      !||    hm_read_ebcs_normv          ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
      !||    hm_read_ebcs_nrf            ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
      !||    hm_read_ebcs_pres           ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
      !||    hm_read_ebcs_propergol      ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_propergol.F90
      !||    hm_read_ebcs_valvin         ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
      !||    hm_read_ebcs_valvout        ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
      !||    hm_read_ebcs_vel            ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
      !||    hm_read_eos_compaction      ../starter/source/materials/eos/hm_read_eos_compaction.F90
      !||    hm_read_eos_compaction2     ../starter/source/materials/eos/hm_read_eos_compaction2.F90
      !||    hm_read_eos_exponential     ../starter/source/materials/eos/hm_read_eos_exponential.F90
      !||    hm_read_eos_gruneisen       ../starter/source/materials/eos/hm_read_eos_gruneisen.F
      !||    hm_read_eos_ideal_gas       ../starter/source/materials/eos/hm_read_eos_ideal_gas.F
      !||    hm_read_eos_ideal_gas_vt    ../starter/source/materials/eos/hm_read_eos_ideal_gas_vt.F
      !||    hm_read_eos_linear          ../starter/source/materials/eos/hm_read_eos_linear.F
      !||    hm_read_eos_lszk            ../starter/source/materials/eos/hm_read_eos_lszk.F
      !||    hm_read_eos_murnaghan       ../starter/source/materials/eos/hm_read_eos_murnaghan.F
      !||    hm_read_eos_nasg            ../starter/source/materials/eos/hm_read_eos_nasg.F
      !||    hm_read_eos_noble_abel      ../starter/source/materials/eos/hm_read_eos_noble_abel.F
      !||    hm_read_eos_osborne         ../starter/source/materials/eos/hm_read_eos_osborne.F
      !||    hm_read_eos_polynomial      ../starter/source/materials/eos/hm_read_eos_polynomial.F
      !||    hm_read_eos_powderburn      ../starter/source/materials/eos/hm_read_eos_powderburn.F90
      !||    hm_read_eos_puff            ../starter/source/materials/eos/hm_read_eos_puff.F
      !||    hm_read_eos_sesame          ../starter/source/materials/eos/hm_read_eos_sesame.F
      !||    hm_read_eos_stiffened_gas   ../starter/source/materials/eos/hm_read_eos_stiffened_gas.F
      !||    hm_read_eos_tabulated       ../starter/source/materials/eos/hm_read_eos_tabulated.F
      !||    hm_read_eos_tillotson       ../starter/source/materials/eos/hm_read_eos_tillotson.F
      !||    hm_read_fail_alter          ../starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
      !||    hm_read_fail_biquad         ../starter/source/materials/fail/biquad/hm_read_fail_biquad.F
      !||    hm_read_fail_chang          ../starter/source/materials/fail/changchang/hm_read_fail_chang.F
      !||    hm_read_fail_cockcroft      ../starter/source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
      !||    hm_read_fail_connect        ../starter/source/materials/fail/connect/hm_read_fail_connect.F
      !||    hm_read_fail_emc            ../starter/source/materials/fail/emc/hm_read_fail_emc.F
      !||    hm_read_fail_energy         ../starter/source/materials/fail/energy/hm_read_fail_energy.F
      !||    hm_read_fail_fabric         ../starter/source/materials/fail/fabric/hm_read_fail_fabric.F
      !||    hm_read_fail_fld            ../starter/source/materials/fail/fld/hm_read_fail_fld.F
      !||    hm_read_fail_gene1          ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
      !||    hm_read_fail_gurson         ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
      !||    hm_read_fail_hashin         ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F
      !||    hm_read_fail_hc_dsse        ../starter/source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
      !||    hm_read_fail_hoffman        ../starter/source/materials/fail/hoffman/hm_read_fail_hoffman.F
      !||    hm_read_fail_inievo         ../starter/source/materials/fail/inievo/hm_read_fail_inievo.F
      !||    hm_read_fail_johnson        ../starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
      !||    hm_read_fail_ladeveze       ../starter/source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
      !||    hm_read_fail_maxstrain      ../starter/source/materials/fail/max_strain/hm_read_fail_maxstrain.F
      !||    hm_read_fail_nxt            ../starter/source/materials/fail/nxt/hm_read_fail_nxt.F
      !||    hm_read_fail_orthbiquad     ../starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
      !||    hm_read_fail_orthenerg      ../starter/source/materials/fail/orthenerg/hm_read_fail_orthenerg.F
      !||    hm_read_fail_orthstrain     ../starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
      !||    hm_read_fail_puck           ../starter/source/materials/fail/puck/hm_read_fail_puck.F
      !||    hm_read_fail_rtcl           ../starter/source/materials/fail/rtcl/hm_read_fail_rtcl.F
      !||    hm_read_fail_sahraei        ../starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
      !||    hm_read_fail_snconnect      ../starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
      !||    hm_read_fail_spalling       ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F90
      !||    hm_read_fail_syazwan        ../starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
      !||    hm_read_fail_tab1           ../starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
      !||    hm_read_fail_tab2           ../starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
      !||    hm_read_fail_tab_old        ../starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
      !||    hm_read_fail_tbutcher       ../starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
      !||    hm_read_fail_tensstrain     ../starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
      !||    hm_read_fail_tsaihill       ../starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
      !||    hm_read_fail_tsaiwu         ../starter/source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
      !||    hm_read_fail_visual         ../starter/source/materials/fail/visual/hm_read_fail_visual.F
      !||    hm_read_fail_wierzbicki     ../starter/source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
      !||    hm_read_fail_wilkins        ../starter/source/materials/fail/wilkins/hm_read_fail_wilkins.F
      !||    hm_read_fractal_dmg         ../starter/source/materials/fail/fractal/hm_read_fractal_dmg.F90
      !||    hm_read_funct               ../starter/source/tools/curve/hm_read_funct.F
      !||    hm_read_inivol              ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
      !||    hm_read_inter_type02        ../starter/source/interfaces/int02/hm_read_inter_type02.F
      !||    hm_read_mat00               ../starter/source/materials/mat/mat000/hm_read_mat00.F
      !||    hm_read_mat01               ../starter/source/materials/mat/mat001/hm_read_mat01.F
      !||    hm_read_mat02               ../starter/source/materials/mat/mat002/hm_read_mat02.F
      !||    hm_read_mat03               ../starter/source/materials/mat/mat003/hm_read_mat03.F
      !||    hm_read_mat04               ../starter/source/materials/mat/mat004/hm_read_mat04.F
      !||    hm_read_mat05               ../starter/source/materials/mat/mat005/hm_read_mat05.F
      !||    hm_read_mat06               ../starter/source/materials/mat/mat006/hm_read_mat06.F
      !||    hm_read_mat06_keps          ../starter/source/materials/mat/mat006/hm_read_mat06_keps.F
      !||    hm_read_mat10               ../starter/source/materials/mat/mat010/hm_read_mat10.F
      !||    hm_read_mat100              ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat101              ../starter/source/materials/mat/mat101/hm_read_mat101.F
      !||    hm_read_mat102              ../starter/source/materials/mat/mat102/hm_read_mat102.F
      !||    hm_read_mat103              ../starter/source/materials/mat/mat103/hm_read_mat103.F
      !||    hm_read_mat104              ../starter/source/materials/mat/mat104/hm_read_mat104.F
      !||    hm_read_mat106              ../starter/source/materials/mat/mat106/hm_read_mat106.F
      !||    hm_read_mat107              ../starter/source/materials/mat/mat107/hm_read_mat107.F
      !||    hm_read_mat108              ../starter/source/materials/mat/mat108/hm_read_mat108.F
      !||    hm_read_mat109              ../starter/source/materials/mat/mat109/hm_read_mat109.F
      !||    hm_read_mat11               ../starter/source/materials/mat/mat011/hm_read_mat11.F
      !||    hm_read_mat110              ../starter/source/materials/mat/mat110/hm_read_mat110.F
      !||    hm_read_mat111              ../starter/source/materials/mat/mat111/hm_read_mat111.F
      !||    hm_read_mat112              ../starter/source/materials/mat/mat112/hm_read_mat112.F
      !||    hm_read_mat113              ../starter/source/materials/mat/mat113/hm_read_mat113.F
      !||    hm_read_mat114              ../starter/source/materials/mat/mat114/hm_read_mat114.F
      !||    hm_read_mat115              ../starter/source/materials/mat/mat115/hm_read_mat115.F
      !||    hm_read_mat116              ../starter/source/materials/mat/mat116/hm_read_mat116.F
      !||    hm_read_mat117              ../starter/source/materials/mat/mat117/hm_read_mat117.F
      !||    hm_read_mat119              ../starter/source/materials/mat/mat119/hm_read_mat119.F
      !||    hm_read_mat11_k_eps         ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
      !||    hm_read_mat12               ../starter/source/materials/mat/mat012/hm_read_mat12.F
      !||    hm_read_mat120              ../starter/source/materials/mat/mat120/hm_read_mat120.F
      !||    hm_read_mat121              ../starter/source/materials/mat/mat121/hm_read_mat121.F
      !||    hm_read_mat122              ../starter/source/materials/mat/mat122/hm_read_mat122.F
      !||    hm_read_mat124              ../starter/source/materials/mat/mat124/hm_read_mat124.F
      !||    hm_read_mat125              ../starter/source/materials/mat/mat125/hm_read_mat125.F90
      !||    hm_read_mat126              ../starter/source/materials/mat/mat126/hm_read_mat126.F90
      !||    hm_read_mat127              ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||    hm_read_mat128              ../starter/source/materials/mat/mat128/hm_read_mat128.F90
      !||    hm_read_mat13               ../starter/source/materials/mat/mat013/hm_read_mat13.F
      !||    hm_read_mat133              ../starter/source/materials/mat/mat133/hm_read_mat133.F90
      !||    hm_read_mat134              ../starter/source/materials/mat/mat134/hm_read_mat134.F90
      !||    hm_read_mat14               ../starter/source/materials/mat/mat014/hm_read_mat14.F
      !||    hm_read_mat15               ../starter/source/materials/mat/mat015/hm_read_mat15.F
      !||    hm_read_mat158              ../starter/source/materials/mat/mat158/hm_read_mat158.F
      !||    hm_read_mat16               ../starter/source/materials/mat/mat016/hm_read_mat16.F
      !||    hm_read_mat163              ../starter/source/materials/mat/mat163/hm_read_mat163.F90
      !||    hm_read_mat169_arup         ../starter/source/materials/mat/mat169/hm_read_mat169.F90
      !||    hm_read_mat18               ../starter/source/materials/mat/mat018/hm_read_mat18.F
      !||    hm_read_mat19               ../starter/source/materials/mat/mat019/hm_read_mat19.F
      !||    hm_read_mat190              ../starter/source/materials/mat/mat190/hm_read_mat190.F
      !||    hm_read_mat20               ../starter/source/materials/mat/mat020/hm_read_mat20.F
      !||    hm_read_mat21               ../starter/source/materials/mat/mat021/hm_read_mat21.F
      !||    hm_read_mat22               ../starter/source/materials/mat/mat022/hm_read_mat22.F
      !||    hm_read_mat23               ../starter/source/materials/mat/mat023/hm_read_mat23.F
      !||    hm_read_mat24               ../starter/source/materials/mat/mat024/hm_read_mat24.F
      !||    hm_read_mat26               ../starter/source/materials/mat/mat026/hm_read_mat26.F
      !||    hm_read_mat27               ../starter/source/materials/mat/mat027/hm_read_mat27.F
      !||    hm_read_mat28               ../starter/source/materials/mat/mat028/hm_read_mat28.F
      !||    hm_read_mat32               ../starter/source/materials/mat/mat032/hm_read_mat32.F
      !||    hm_read_mat33               ../starter/source/materials/mat/mat033/hm_read_mat33.F
      !||    hm_read_mat34               ../starter/source/materials/mat/mat034/hm_read_mat34.F
      !||    hm_read_mat35               ../starter/source/materials/mat/mat035/hm_read_mat35.F
      !||    hm_read_mat36               ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat37               ../starter/source/materials/mat/mat037/hm_read_mat37.F
      !||    hm_read_mat38               ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat40               ../starter/source/materials/mat/mat040/hm_read_mat40.F
      !||    hm_read_mat41               ../starter/source/materials/mat/mat041/hm_read_mat41.F
      !||    hm_read_mat42               ../starter/source/materials/mat/mat042/hm_read_mat42.F
      !||    hm_read_mat43               ../starter/source/materials/mat/mat043/hm_read_mat43.F
      !||    hm_read_mat44               ../starter/source/materials/mat/mat044/hm_read_mat44.F
      !||    hm_read_mat46               ../starter/source/materials/mat/mat046/hm_read_mat46.F
      !||    hm_read_mat48               ../starter/source/materials/mat/mat048/hm_read_mat48.F
      !||    hm_read_mat49               ../starter/source/materials/mat/mat049/hm_read_mat49.F
      !||    hm_read_mat50               ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||    hm_read_mat51               ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_mat52               ../starter/source/materials/mat/mat052/hm_read_mat52.F
      !||    hm_read_mat53               ../starter/source/materials/mat/mat053/hm_read_mat53.F
      !||    hm_read_mat54               ../starter/source/materials/mat/mat054/hm_read_mat54.F
      !||    hm_read_mat57               ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat58               ../starter/source/materials/mat/mat058/hm_read_mat58.F
      !||    hm_read_mat59               ../starter/source/materials/mat/mat059/hm_read_mat59.F
      !||    hm_read_mat60               ../starter/source/materials/mat/mat060/hm_read_mat60.F
      !||    hm_read_mat62               ../starter/source/materials/mat/mat062/hm_read_mat62.F
      !||    hm_read_mat63               ../starter/source/materials/mat/mat063/hm_read_mat63.F
      !||    hm_read_mat64               ../starter/source/materials/mat/mat064/hm_read_mat64.F
      !||    hm_read_mat65               ../starter/source/materials/mat/mat065/hm_read_mat65.F
      !||    hm_read_mat66               ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat68               ../starter/source/materials/mat/mat068/hm_read_mat68.F
      !||    hm_read_mat69               ../starter/source/materials/mat/mat069/hm_read_mat69.F
      !||    hm_read_mat70               ../starter/source/materials/mat/mat070/hm_read_mat70.F
      !||    hm_read_mat71               ../starter/source/materials/mat/mat071/hm_read_mat71.F
      !||    hm_read_mat72               ../starter/source/materials/mat/mat072/hm_read_mat72.F
      !||    hm_read_mat73               ../starter/source/materials/mat/mat073/hm_read_mat73.F
      !||    hm_read_mat74               ../starter/source/materials/mat/mat074/hm_read_mat74.F
      !||    hm_read_mat75               ../starter/source/materials/mat/mat075/hm_read_mat75.F
      !||    hm_read_mat76               ../starter/source/materials/mat/mat076/hm_read_mat76.F
      !||    hm_read_mat77               ../starter/source/materials/mat/mat077/hm_read_mat77.F
      !||    hm_read_mat78               ../starter/source/materials/mat/mat078/hm_read_mat78.F
      !||    hm_read_mat79               ../starter/source/materials/mat/mat079/hm_read_mat79.F
      !||    hm_read_mat80               ../starter/source/materials/mat/mat080/hm_read_mat80.F
      !||    hm_read_mat81               ../starter/source/materials/mat/mat081/hm_read_mat81.F90
      !||    hm_read_mat82               ../starter/source/materials/mat/mat082/hm_read_mat82.F
      !||    hm_read_mat83               ../starter/source/materials/mat/mat083/hm_read_mat83.F
      !||    hm_read_mat84               ../starter/source/materials/mat/mat084/hm_read_mat84.F
      !||    hm_read_mat87               ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||    hm_read_mat88               ../starter/source/materials/mat/mat088/hm_read_mat88.F
      !||    hm_read_mat90               ../starter/source/materials/mat/mat090/hm_read_mat90.F
      !||    hm_read_mat92               ../starter/source/materials/mat/mat092/hm_read_mat92.F
      !||    hm_read_mat93               ../starter/source/materials/mat/mat093/hm_read_mat93.F
      !||    hm_read_mat94               ../starter/source/materials/mat/mat094/hm_read_mat94.F
      !||    hm_read_mat95               ../starter/source/materials/mat/mat095/hm_read_mat95.F
      !||    hm_read_mat97               ../starter/source/materials/mat/mat097/hm_read_mat97.F
      !||    hm_read_matgas              ../starter/source/materials/mat/matgas/hm_read_matgas.F
      !||    hm_read_mullins_or          ../starter/source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
      !||    hm_read_prop0               ../starter/source/properties/void/hm_read_prop0.F
      !||    hm_read_prop01              ../starter/source/properties/shell/hm_read_prop01.F
      !||    hm_read_prop03              ../starter/source/properties/beam/hm_read_prop03.F
      !||    hm_read_prop04              ../starter/source/properties/spring/hm_read_prop04.F
      !||    hm_read_prop06              ../starter/source/properties/solid/hm_read_prop06.F
      !||    hm_read_prop08              ../starter/source/properties/spring/hm_read_prop08.F
      !||    hm_read_prop09              ../starter/source/properties/shell/hm_read_prop09.F
      !||    hm_read_prop10              ../starter/source/properties/shell/hm_read_prop10.F
      !||    hm_read_prop11              ../starter/source/properties/shell/hm_read_prop11.F
      !||    hm_read_prop12              ../starter/source/properties/spring/hm_read_prop12.F
      !||    hm_read_prop13              ../starter/source/properties/spring/hm_read_prop13.F
      !||    hm_read_prop14              ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop14f             ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop15              ../starter/source/properties/solid/hm_read_prop15.F
      !||    hm_read_prop16              ../starter/source/properties/shell/hm_read_prop16.F
      !||    hm_read_prop17              ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop18              ../starter/source/properties/beam/hm_read_prop18.F
      !||    hm_read_prop19              ../starter/source/properties/shell/hm_read_prop19.F
      !||    hm_read_prop20              ../starter/source/properties/thickshell/hm_read_prop20.F
      !||    hm_read_prop22              ../starter/source/properties/thickshell/hm_read_prop22.F
      !||    hm_read_prop23              ../starter/source/properties/spring/hm_read_prop23.F
      !||    hm_read_prop25              ../starter/source/properties/spring/hm_read_prop25.F
      !||    hm_read_prop26              ../starter/source/properties/spring/hm_read_prop26.F
      !||    hm_read_prop27              ../starter/source/properties/spring/hm_read_prop27.F
      !||    hm_read_prop28              ../starter/source/properties/xelem/hm_read_prop28.F
      !||    hm_read_prop32              ../starter/source/properties/spring/hm_read_prop32.F
      !||    hm_read_prop33              ../starter/source/properties/spring/hm_read_prop33.F
      !||    hm_read_prop34              ../starter/source/properties/sph/hm_read_prop34.F
      !||    hm_read_prop35              ../starter/source/properties/spring/hm_read_prop35.F
      !||    hm_read_prop36              ../starter/source/properties/spring/hm_read_prop36.F
      !||    hm_read_prop43              ../starter/source/properties/solid/hm_read_prop43.F
      !||    hm_read_prop44              ../starter/source/properties/spring/hm_read_prop44.F
      !||    hm_read_prop45              ../starter/source/properties/spring/hm_read_prop45.F
      !||    hm_read_prop46              ../starter/source/properties/spring/hm_read_prop46.F
      !||    hm_read_prop51              ../starter/source/properties/shell/hm_read_prop51.F
      !||    hm_read_refsta              ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    hm_read_stack               ../starter/source/stack/hm_read_stack.F
      !||    hm_read_table1_0            ../starter/source/tools/curve/hm_read_table1_0.F
      !||    hm_read_table1_1            ../starter/source/tools/curve/hm_read_table1_1.F
      !||    hm_read_table2_0            ../starter/source/tools/curve/hm_read_table2_0.F
      !||    hm_read_table2_1            ../starter/source/tools/curve/hm_read_table2_1.F
      !||    hm_read_thpart              ../starter/source/output/thpart/hm_read_thpart.F
      !||    hm_read_visc_lprony         ../starter/source/materials/visc/hm_read_visc_lprony.F
      !||    hm_read_visc_plas           ../starter/source/materials/visc/hm_read_visc_plas.F90
      !||    hm_read_visc_prony          ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    law100_upd_ab               ../starter/source/materials/mat/mat100/law100_upd.F
      !||    law87_upd                   ../starter/source/materials/mat/mat087/law87_upd.F90
      !||    law92_upd                   ../starter/source/materials/mat/mat092/law92_upd.F
      !||    lcgeo19                     ../starter/source/elements/shell/coque/lcgeo19.F
      !||    read_ale_mat                ../starter/source/materials/ale/read_ale_mat.F
      !||    read_dfs_detcord            ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline            ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan            ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint           ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper        ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_euler_mat              ../starter/source/materials/ale/read_euler_mat.F
      !||    read_mat25_crasurv          ../starter/source/materials/mat/mat025/read_mat25_crasurv.F90
      !||    read_mat25_tsaiwu           ../starter/source/materials/mat/mat025/read_mat25_tsaiwu.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                 ../starter/share/message_module/message_mod.F
      !||    submodel_mod                ../starter/share/modules1/submodel_mod.F
      !||====================================================================
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C OUTPUT ARGUMENTS
      LOGICAL,INTENT(OUT)::IS_ENCRYPTED
C-----------------------------------------------
      IS_ENCRYPTED   = .FALSE.
      CALL CPP_OPTION_IS_CRYPTED(IS_ENCRYPTED) 
      RETURN
C
      END
