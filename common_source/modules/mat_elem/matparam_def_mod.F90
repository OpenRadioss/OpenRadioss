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
      !||    matparam_def_mod                    ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||--- called by ------------------------------------------------------
      !||    aeturb                              ../engine/source/ale/turbulence/aeturb.F
      !||    akturb                              ../engine/source/ale/turbulence/akturb.F
      !||    alemain                             ../engine/source/ale/alemain.F
      !||    alethe                              ../engine/source/ale/alethe.F
      !||    atherm                              ../engine/source/ale/atherm.F
      !||    binit2                              ../starter/source/ale/bimat/binit2.F
      !||    brest2                              ../engine/source/ale/bimat/brest2.F
      !||    c3grhead                            ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                           ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    cfailini                            ../starter/source/elements/shell/coque/cfailini.F
      !||    cfailini4                           ../starter/source/elements/shell/coque/cfailini.F
      !||    cgrhead                             ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                            ../starter/source/elements/shell/coque/cgrtails.F
      !||    check_mat_elem_prop_compatibility   ../starter/source/materials/mat/check_mat_elem_prop_compatibility.F
      !||    cinit3                              ../starter/source/elements/shell/coque/cinit3.F
      !||    dfunc0                              ../engine/source/output/anim/generate/dfunc0.F
      !||    dfuncc                              ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncc_ply                          ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    dfuncs                              ../engine/source/output/anim/generate/dfunc6.F
      !||    dometis                             ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    dynain_c_strsg                      ../engine/source/output/dynain/dynain_c_strsg.F
      !||    eig                                 ../engine/stub/eig.F
      !||    eig1                                ../engine/stub/eig1.F
      !||    eigp                                ../engine/stub/eigp.F
      !||    eosmain                             ../common_source/eos/eosmain.F
      !||    fail_init                           ../starter/source/materials/fail/fail_init.F
      !||    fail_lemaitre_c                     ../engine/source/materials/fail/lemaitre/fail_lemaitre_c.F90
      !||    fail_lemaitre_s                     ../engine/source/materials/fail/lemaitre/fail_lemaitre_s.F90
      !||    fail_windshield_init                ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||    failini                             ../starter/source/elements/solid/solide/failini.F
      !||    fill_buffer_51                      ../starter/source/materials/mat/mat051/fill_buffer_51.F
      !||    fill_buffer_51_0                    ../starter/source/materials/mat/mat051/fill_buffer_51_0.F
      !||    fsdcod                              ../starter/source/system/fsdcod.F
      !||    func_comp                           ../starter/source/materials/mat/mat076/law76_upd.F
      !||    funct_python_update_elements        ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    genani                              ../engine/source/output/anim/generate/genani.F
      !||    gendynain                           ../engine/source/output/dynain/gendynain.F
      !||    genh3d                              ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    granular51                          ../engine/source/materials/mat/mat051/granular51.F90
      !||    h3d_quad_scalar                     ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_quad_scalar_1                   ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    h3d_shell_scalar                    ../engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
      !||    h3d_shell_scalar_1                  ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_tensor                    ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
      !||    h3d_solid_scalar                    ../engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
      !||    h3d_solid_scalar_1                  ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    hist2                               ../engine/source/output/th/hist2.F
      !||    hm_read_eos                         ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_eos_ideal_gas               ../starter/source/materials/eos/hm_read_eos_ideal_gas.F
      !||    hm_read_eos_powderburn              ../starter/source/materials/eos/hm_read_eos_powderburn.F90
      !||    hm_read_fail                        ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_inistate_d00                ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_mat                         ../starter/source/materials/mat/hm_read_mat.F90
      !||    hm_read_mat00                       ../starter/source/materials/mat/mat000/hm_read_mat00.F
      !||    hm_read_mat01                       ../starter/source/materials/mat/mat001/hm_read_mat01.F
      !||    hm_read_mat02                       ../starter/source/materials/mat/mat002/hm_read_mat02.F
      !||    hm_read_mat03                       ../starter/source/materials/mat/mat003/hm_read_mat03.F
      !||    hm_read_mat04                       ../starter/source/materials/mat/mat004/hm_read_mat04.F
      !||    hm_read_mat05                       ../starter/source/materials/mat/mat005/hm_read_mat05.F
      !||    hm_read_mat06                       ../starter/source/materials/mat/mat006/hm_read_mat06.F
      !||    hm_read_mat06_keps                  ../starter/source/materials/mat/mat006/hm_read_mat06_keps.F
      !||    hm_read_mat10                       ../starter/source/materials/mat/mat010/hm_read_mat10.F
      !||    hm_read_mat100                      ../starter/source/materials/mat/mat100/hm_read_mat100.F
      !||    hm_read_mat101                      ../starter/source/materials/mat/mat101/hm_read_mat101.F
      !||    hm_read_mat102                      ../starter/source/materials/mat/mat102/hm_read_mat102.F
      !||    hm_read_mat103                      ../starter/source/materials/mat/mat103/hm_read_mat103.F
      !||    hm_read_mat104                      ../starter/source/materials/mat/mat104/hm_read_mat104.F
      !||    hm_read_mat105                      ../starter/source/materials/mat/mat105/hm_read_mat105.F90
      !||    hm_read_mat106                      ../starter/source/materials/mat/mat106/hm_read_mat106.F
      !||    hm_read_mat107                      ../starter/source/materials/mat/mat107/hm_read_mat107.F
      !||    hm_read_mat108                      ../starter/source/materials/mat/mat108/hm_read_mat108.F
      !||    hm_read_mat109                      ../starter/source/materials/mat/mat109/hm_read_mat109.F
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
      !||    hm_read_mat120                      ../starter/source/materials/mat/mat120/hm_read_mat120.F
      !||    hm_read_mat121                      ../starter/source/materials/mat/mat121/hm_read_mat121.F
      !||    hm_read_mat122                      ../starter/source/materials/mat/mat122/hm_read_mat122.F
      !||    hm_read_mat124                      ../starter/source/materials/mat/mat124/hm_read_mat124.F
      !||    hm_read_mat125                      ../starter/source/materials/mat/mat125/hm_read_mat125.F90
      !||    hm_read_mat126                      ../starter/source/materials/mat/mat126/hm_read_mat126.F90
      !||    hm_read_mat127                      ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||    hm_read_mat128                      ../starter/source/materials/mat/mat128/hm_read_mat128.F90
      !||    hm_read_mat129                      ../starter/source/materials/mat/mat129/hm_read_mat129.F90
      !||    hm_read_mat13                       ../starter/source/materials/mat/mat013/hm_read_mat13.F
      !||    hm_read_mat133                      ../starter/source/materials/mat/mat133/hm_read_mat133.F90
      !||    hm_read_mat134                      ../starter/source/materials/mat/mat134/hm_read_mat134.F90
      !||    hm_read_mat14                       ../starter/source/materials/mat/mat014/hm_read_mat14.F
      !||    hm_read_mat15                       ../starter/source/materials/mat/mat015/hm_read_mat15.F
      !||    hm_read_mat151                      ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||    hm_read_mat158                      ../starter/source/materials/mat/mat158/hm_read_mat158.F
      !||    hm_read_mat16                       ../starter/source/materials/mat/mat016/hm_read_mat16.F
      !||    hm_read_mat163                      ../starter/source/materials/mat/mat163/hm_read_mat163.F90
      !||    hm_read_mat169_arup                 ../starter/source/materials/mat/mat169/hm_read_mat169.F90
      !||    hm_read_mat18                       ../starter/source/materials/mat/mat018/hm_read_mat18.F
      !||    hm_read_mat19                       ../starter/source/materials/mat/mat019/hm_read_mat19.F
      !||    hm_read_mat190                      ../starter/source/materials/mat/mat190/hm_read_mat190.F
      !||    hm_read_mat20                       ../starter/source/materials/mat/mat020/hm_read_mat20.F
      !||    hm_read_mat21                       ../starter/source/materials/mat/mat021/hm_read_mat21.F
      !||    hm_read_mat22                       ../starter/source/materials/mat/mat022/hm_read_mat22.F
      !||    hm_read_mat23                       ../starter/source/materials/mat/mat023/hm_read_mat23.F
      !||    hm_read_mat24                       ../starter/source/materials/mat/mat024/hm_read_mat24.F
      !||    hm_read_mat25                       ../starter/source/materials/mat/mat025/hm_read_mat25.F
      !||    hm_read_mat26                       ../starter/source/materials/mat/mat026/hm_read_mat26.F
      !||    hm_read_mat27                       ../starter/source/materials/mat/mat027/hm_read_mat27.F
      !||    hm_read_mat28                       ../starter/source/materials/mat/mat028/hm_read_mat28.F
      !||    hm_read_mat29_31                    ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat32                       ../starter/source/materials/mat/mat032/hm_read_mat32.F
      !||    hm_read_mat33                       ../starter/source/materials/mat/mat033/hm_read_mat33.F
      !||    hm_read_mat34                       ../starter/source/materials/mat/mat034/hm_read_mat34.F
      !||    hm_read_mat35                       ../starter/source/materials/mat/mat035/hm_read_mat35.F
      !||    hm_read_mat36                       ../starter/source/materials/mat/mat036/hm_read_mat36.F
      !||    hm_read_mat37                       ../starter/source/materials/mat/mat037/hm_read_mat37.F
      !||    hm_read_mat38                       ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat40                       ../starter/source/materials/mat/mat040/hm_read_mat40.F
      !||    hm_read_mat41                       ../starter/source/materials/mat/mat041/hm_read_mat41.F
      !||    hm_read_mat42                       ../starter/source/materials/mat/mat042/hm_read_mat42.F
      !||    hm_read_mat43                       ../starter/source/materials/mat/mat043/hm_read_mat43.F
      !||    hm_read_mat44                       ../starter/source/materials/mat/mat044/hm_read_mat44.F
      !||    hm_read_mat46                       ../starter/source/materials/mat/mat046/hm_read_mat46.F
      !||    hm_read_mat48                       ../starter/source/materials/mat/mat048/hm_read_mat48.F
      !||    hm_read_mat49                       ../starter/source/materials/mat/mat049/hm_read_mat49.F
      !||    hm_read_mat50                       ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||    hm_read_mat51                       ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    hm_read_mat52                       ../starter/source/materials/mat/mat052/hm_read_mat52.F
      !||    hm_read_mat53                       ../starter/source/materials/mat/mat053/hm_read_mat53.F
      !||    hm_read_mat54                       ../starter/source/materials/mat/mat054/hm_read_mat54.F
      !||    hm_read_mat57                       ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat58                       ../starter/source/materials/mat/mat058/hm_read_mat58.F
      !||    hm_read_mat59                       ../starter/source/materials/mat/mat059/hm_read_mat59.F
      !||    hm_read_mat60                       ../starter/source/materials/mat/mat060/hm_read_mat60.F
      !||    hm_read_mat62                       ../starter/source/materials/mat/mat062/hm_read_mat62.F
      !||    hm_read_mat63                       ../starter/source/materials/mat/mat063/hm_read_mat63.F
      !||    hm_read_mat64                       ../starter/source/materials/mat/mat064/hm_read_mat64.F
      !||    hm_read_mat65                       ../starter/source/materials/mat/mat065/hm_read_mat65.F
      !||    hm_read_mat66                       ../starter/source/materials/mat/mat066/hm_read_mat66.F
      !||    hm_read_mat68                       ../starter/source/materials/mat/mat068/hm_read_mat68.F
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
      !||    hm_read_mat84                       ../starter/source/materials/mat/mat084/hm_read_mat84.F
      !||    hm_read_mat87                       ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||    hm_read_mat88                       ../starter/source/materials/mat/mat088/hm_read_mat88.F
      !||    hm_read_mat90                       ../starter/source/materials/mat/mat090/hm_read_mat90.F
      !||    hm_read_mat92                       ../starter/source/materials/mat/mat092/hm_read_mat92.F
      !||    hm_read_mat93                       ../starter/source/materials/mat/mat093/hm_read_mat93.F
      !||    hm_read_mat94                       ../starter/source/materials/mat/mat094/hm_read_mat94.F
      !||    hm_read_mat95                       ../starter/source/materials/mat/mat095/hm_read_mat95.F
      !||    hm_read_mat97                       ../starter/source/materials/mat/mat097/hm_read_mat97.F
      !||    hm_read_mat_99                      ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_nonlocal                    ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_therm                       ../starter/source/materials/therm/hm_read_therm.F
      !||    hm_read_therm_stress                ../starter/source/materials/therm/hm_read_therm_stress.F90
      !||    hm_read_visc                        ../starter/source/materials/visc/hm_read_visc.F
      !||    ig3dgrtails                         ../starter/source/elements/ige3d/ig3dgrtails.F
      !||    imp_buck                            ../engine/source/implicit/imp_buck.F
      !||    ini_inimap1d                        ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    iniebcs_nrf_tcar                    ../starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
      !||    iniebcs_propergol                   ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||    iniebcs_propergol_get_cv            ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||    iniebcsp0                           ../starter/source/boundary_conditions/ebcs/iniebcsp0.F
      !||    inigrav_eos                         ../starter/source/initial_conditions/inigrav/inigrav_eos.F
      !||    inigrav_load                        ../starter/source/initial_conditions/inigrav/inigrav_load.F
      !||    init_inivol                         ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    init_inivol_2d_polygons             ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||    init_mat_keyword                    ../starter/source/materials/mat/init_mat_keyword.F
      !||    initia                              ../starter/source/elements/initia/initia.F
      !||    initwg                              ../starter/source/spmd/domain_decomposition/initwg.F
      !||    initwg_shell                        ../starter/source/spmd/domain_decomposition/initwg_shell.F
      !||    initwg_solid                        ../starter/source/spmd/domain_decomposition/initwg_solid.F
      !||    initwg_tri                          ../starter/source/spmd/domain_decomposition/initwg_tri.F
      !||    inivol_set                          ../starter/source/initial_conditions/inivol/inivol_set.F
      !||    law104_upd                          ../starter/source/materials/mat/mat104/law104_upd.F
      !||    law129_upd                          ../starter/source/materials/mat/mat129/law129_upd.F90
      !||    law133_upd                          ../starter/source/materials/mat/mat133/law133_upd.F90
      !||    law158_upd                          ../starter/source/materials/mat/mat158/law158_upd.F
      !||    law163_upd                          ../starter/source/materials/mat/mat163/law163_upd.F90
      !||    law190_upd                          ../starter/source/materials/mat/mat190/law190_upd.F90
      !||    law19_upd                           ../starter/source/materials/mat/mat019/law19_upd.F90
      !||    law42_upd                           ../starter/source/materials/mat/mat042/law42_upd.F
      !||    law58_upd                           ../starter/source/materials/mat/mat058/law58_upd.F
      !||    law70_upd                           ../starter/source/materials/mat/mat070/law70_upd.F
      !||    law76_upd                           ../starter/source/materials/mat/mat076/law76_upd.F
      !||    law77_upd                           ../starter/source/materials/mat/mat077/law77_upd.F
      !||    law81_upd                           ../starter/source/materials/mat/mat081/law81_upd.F90
      !||    law87_upd                           ../starter/source/materials/mat/mat087/law87_upd.F90
      !||    lec_inistate                        ../starter/source/elements/initia/lec_inistate.F
      !||    lech3d                              ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    m12law                              ../engine/source/materials/mat/mat012/m12law.F
      !||    m20dcod                             ../starter/source/system/fsdcod.F
      !||    m25law                              ../engine/source/materials/mat/mat025/m25law.F
      !||    mat25_crasurv_c                     ../engine/source/materials/mat/mat025/mat25_crasurv_c.F90
      !||    mat25_crasurv_s                     ../engine/source/materials/mat/mat025/mat25_crasurv_s.F90
      !||    mat25_tsaiwu_c                      ../engine/source/materials/mat/mat025/mat25_tsaiwu_c.F90
      !||    mat25_tsaiwu_s                      ../engine/source/materials/mat/mat025/mat25_tsaiwu_s.F90
      !||    mat87c_hansel                       ../engine/source/materials/mat/mat087/mat87c_hansel.F90
      !||    mat87c_swift_voce                   ../engine/source/materials/mat/mat087/mat87c_swift_voce.F90
      !||    mat87c_tabulated                    ../engine/source/materials/mat/mat087/mat87c_tabulated.F90
      !||    mat87c_tabulated_3dir_ortho         ../engine/source/materials/mat/mat087/mat87c_tabulated_3dir_ortho.F90
      !||    mat_elem_mod                        ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||    mat_table_copy                      ../starter/source/materials/tools/mat_table_copy.F90
      !||    mmain                               ../engine/source/materials/mat_share/mmain.F90
      !||    mmodul                              ../engine/source/elements/solid/solidez/mmodul.F
      !||    mulaw                               ../engine/source/materials/mat_share/mulaw.F90
      !||    mulawc                              ../engine/source/materials/mat_share/mulawc.F90
      !||    multi_ebcs                          ../engine/source/multifluid/multi_ebcs.F
      !||    multi_inlet_ebcs                    ../engine/source/multifluid/multi_inlet_ebcs.F
      !||    multi_muscl_compute_pressure        ../engine/source/multifluid/multi_muscl_compute_pressure.F90
      !||    multi_muscl_fluxes_computation      ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||    multi_nrf_ebcs                      ../engine/source/multifluid/multi_nrf_ebcs.F
      !||    multi_pressure_equilibrium          ../engine/source/multifluid/multi_pressure_equilibrium.F
      !||    multi_propergol_ebcs                ../engine/source/multifluid/multi_propergol_ebcs.F90
      !||    multi_solve_eint                    ../engine/source/multifluid/multi_solve_eint.F90
      !||    multi_submatlaw                     ../engine/source/multifluid/multi_submatlaw.F
      !||    multi_timeevolution                 ../engine/source/multifluid/multi_timeevolution.F
      !||    multifluid_init2                    ../starter/source/multifluid/multifluid_init2.F
      !||    multifluid_init2t                   ../starter/source/multifluid/multifluid_init2t.F
      !||    multifluid_init3                    ../starter/source/multifluid/multifluid_init3.F
      !||    multifluid_init3t                   ../starter/source/multifluid/multifluid_init3t.F
      !||    pgrtails                            ../starter/source/elements/beam/pgrtails.F
      !||    powder_burn                         ../common_source/eos/powder_burn.F
      !||    prelech3d                           ../engine/source/output/h3d/h3d_build_fortran/prelech3d.F90
      !||    qgrhead                             ../starter/source/elements/solid_2d/quad/qgrhead.F
      !||    qgrtails                            ../starter/source/elements/solid_2d/quad/qgrtails.F
      !||    r2r_group                           ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_matparam_copy                   ../starter/source/elements/elbuf_init/r2r_matparam_copy.F
      !||    read_mat25_crasurv                  ../starter/source/materials/mat/mat025/read_mat25_crasurv.F90
      !||    read_mat25_tsaiwu                   ../starter/source/materials/mat/mat025/read_mat25_tsaiwu.F90
      !||    s10init3                            ../starter/source/elements/solid/solide10/s10init3.F
      !||    s16init3                            ../starter/source/elements/thickshell/solide16/s16init3.F
      !||    s20init3                            ../starter/source/elements/solid/solide20/s20init3.F
      !||    s4init3                             ../starter/source/elements/solid/solide4/s4init3.F
      !||    s4voln_m                            ../engine/source/elements/solid/solide4_sfem/s4voln_m.F
      !||    s6cinit3                            ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    s8cinit3                            ../starter/source/elements/thickshell/solide8c/s8cinit3.F
      !||    s8zinit3                            ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    sgrhead                             ../starter/source/elements/solid/solide/sgrhead.F
      !||    sgrtails                            ../starter/source/elements/solid/solide/sgrtails.F
      !||    sigeps125                           ../engine/source/materials/mat/mat125/sigeps125.F90
      !||    sigeps125c                          ../engine/source/materials/mat/mat125/sigeps125c.F90
      !||    sigeps126                           ../engine/source/materials/mat/mat126/sigeps126.F90
      !||    sigeps127                           ../engine/source/materials/mat/mat127/sigeps127.F90
      !||    sigeps127c                          ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||    sigeps128c                          ../engine/source/materials/mat/mat128/sigeps128c.F90
      !||    sigeps128s                          ../engine/source/materials/mat/mat128/sigeps128s.F90
      !||    sigeps129s                          ../engine/source/materials/mat/mat129/sigeps129s.F90
      !||    sigeps133                           ../engine/source/materials/mat/mat133/sigeps133.F90
      !||    sigeps134s                          ../engine/source/materials/mat/mat134/sigeps134s.F90
      !||    sigeps163                           ../engine/source/materials/mat/mat163/sigeps163.F90
      !||    sigeps190                           ../engine/source/materials/mat/mat190/sigeps190.F
      !||    sigeps25c                           ../engine/source/materials/mat/mat025/sigeps25c.F
      !||    sigeps25cp                          ../engine/source/materials/mat/mat025/sigeps25cp.F
      !||    sigeps42                            ../engine/source/materials/mat/mat042/sigeps42.F
      !||    sigeps50s                           ../engine/source/materials/mat/mat050/sigeps50s.F90
      !||    sigeps51                            ../engine/source/materials/mat/mat051/sigeps51.F90
      !||    sigeps57c                           ../engine/source/materials/mat/mat057/sigeps57c.F90
      !||    sigeps66c                           ../engine/source/materials/mat/mat066/sigeps66c.F
      !||    sigeps70                            ../engine/source/materials/mat/mat070/sigeps70.F
      !||    sigeps75                            ../engine/source/materials/mat/mat075/sigeps75.F
      !||    sigeps76                            ../engine/source/materials/mat/mat076/sigeps76.F
      !||    sigeps76c                           ../engine/source/materials/mat/mat076/sigeps76c.F
      !||    sigeps81                            ../engine/source/materials/mat/mat081/sigeps81.F90
      !||    sigeps87c                           ../engine/source/materials/mat/mat087/sigeps87c.F90
      !||    sinit3                              ../starter/source/elements/solid/solide/sinit3.F
      !||    spgrhead                            ../starter/source/elements/sph/spgrhead.F
      !||    spgrtails                           ../starter/source/elements/sph/spgrtails.F
      !||    stat_inimap1d_file_spmd             ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap1d_spmd                  ../engine/source/output/sta/stat_inimap1d_spmd.F
      !||    stat_inimap2d_file_spmd             ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    stat_inimap2d_spmd                  ../engine/source/output/sta/stat_inimap2d_spmd.F
      !||    suinit3                             ../starter/source/elements/elbuf_init/suinit3.F
      !||    szhour3_or                          ../engine/source/elements/solid/solidez/szhour3_or.F
      !||    t3grhead                            ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                           ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    tagnod_r2r_nl                       ../starter/source/coupling/rad2rad/tagnod_r2r_nl.F
      !||    tensorc                             ../engine/source/output/anim/generate/tensorc.F
      !||    tensorc_crk                         ../engine/source/output/anim/generate/tensorc_crk.F
      !||    tensorc_ply                         ../engine/source/output/anim/generate/tensorc_ply.F
      !||    thcoq                               ../engine/source/output/th/thcoq.F
      !||    updfail                             ../starter/source/materials/updfail.F90
      !||    updmat                              ../starter/source/materials/updmat.F
      !||    usermat_solid                       ../engine/source/materials/mat_share/usermat_solid.F
      !||--- uses       -----------------------------------------------------
      !||    ale_mod                             ../common_source/modules/ale/ale_mod.F
      !||    eos_param_mod                       ../common_source/modules/mat_elem/eos_param_mod.F90
      !||    fail_param_mod                      ../common_source/modules/mat_elem/fail_param_mod.F90
      !||    multimat_param_mod                  ../common_source/modules/multimat_param_mod.F90
      !||    names_and_titles_mod                ../common_source/modules/names_and_titles_mod.F
      !||    precision_mod                       ../common_source/modules/precision_mod.F90
      !||    table4d_mod                         ../common_source/modules/table4d_mod.F
      !||    therm_param_mod                     ../common_source/modules/mat_elem/therm_param_mod.F90
      !||    visc_param_mod                      ../common_source/modules/mat_elem/visc_param_mod.F90
      !||====================================================================
      module matparam_def_mod
!
      use table4d_mod
      use visc_param_mod
      use fail_param_mod
      use therm_param_mod
      use names_and_titles_mod
      use multimat_param_mod
      use eos_param_mod
      use precision_mod, only : WP
      use ale_mod , only : ale_rezon_
      implicit none

      private :: WP
      

!=======================================================================      
  !! \brief module to define data structure for all material model parameters 
  !! \details  allocatable dimension : nummat
!=======================================================================      
!
      type matparam_struct_
      
        character(len=nchartitle) :: title  !< material law title
        integer     :: ilaw                 !< material model (type)
        integer     :: mat_id               !< user id
        integer     :: nuparam              !< number of real value material paraameters
        integer     :: niparam              !< number of int value material parameters
        integer     :: nfunc                !< number of local functions in material
        integer     :: ntable               !< number of local function tables
        integer     :: nsubmat              !< number of submaterials (multi-mat law51)
        integer     :: nfail                !< number of failure models
        integer     :: ivisc                !< viscosity model number
        integer     :: ieos                 !< eos model (type)
        integer     :: itherm               !< thermal option activation flag (/heat/mat)
        integer     :: iexpan               !< thermal volumic expansion flag (/therm_stress)
        integer     :: iale                 !< ale formulation flag
        integer     :: iturb                !< turbulent flow flag
        integer     :: heat_flag            !< dissipated energy (heat source) is output by material law
        ! -------  material characteristics flags
        integer     :: compressibility      !< "compressible","incompressible","elasto_plastic"
        integer     :: smstr                !< "small_strain", "large_strain"
        integer     :: strain_formulation   !< "total", "incremental"
        integer     :: ipres                !< "hydrostatic",hydro_eos","hook"
        integer     :: orthotropy           !< "isotropic", "orthotropic", "anisotropic"
        ! ------- compatibility flags - not written in restart file, for starter check only
        integer     :: prop_solid           !< "solid_isotropic","solid_orthotropic","solid_composite","solid_cohesive"   ,"solid_porous","solid_all"
        integer     :: prop_shell           !< "shell_isotropic","shell_orthotropic","shell_composite","shell_anisotropic","shell_all"
        integer     :: prop_beam            !< "beam_classic"   ,"beam_integrated"  ,"beam_all"
        integer     :: prop_spring          !< "spring_predit"  ,"spring_material"  ,"spring_all"
        integer     :: prop_truss           !< "truss"
        integer     :: prop_sph             !< "sph"
        integer     :: compatibility_eos    !< "eos"
        integer     :: compatibility_visc   !< "visc"
        integer     :: compatibility_therm  !< "therm"
!        integer     :: compatibility_nloc   !< "nloc"
        ! --------------------------------- !<  
        integer     :: nloc                 !< non-local variable regularization flag
        integer     :: ifailwave            !< failwave propagation flag
        integer     :: ixfem                !< xfem flag
        ! --------------------------------- !<
        integer     :: nmod                 !< number of rupture/damage modes
        ! --------------------------------- !
        real(kind=WP) :: rho                !< reference density
        real(kind=WP) :: rho0               !< initial density        
        real(kind=WP) :: young              !< Young modulus        
        real(kind=WP) :: bulk               !< bulk modulus      
        real(kind=WP) :: shear              !< shear modulus 
        real(kind=WP) :: nu                 !< Poisson's ratio     
        real(kind=WP) :: stiff_contact      !< initial contact stiffness 
        real(kind=WP) :: stiff_hglass       !< initial hourglass stiffness      
        real(kind=WP) :: stiff_tstep        !< initial stiffness for time step stability
!
        real(kind=WP)             ,dimension(:) ,allocatable :: uparam !< real value material parameter table (nuparam)
        integer                   ,dimension(:) ,allocatable :: iparam !< integer value material parameter table (niparam)
        type (table_4d_)          ,dimension(:) ,allocatable :: table  !< local function tables
        character(len=nchartitle) ,dimension(:) ,allocatable :: mode   !< damage mode keywords
!                
        type (fail_param_),dimension(:) ,allocatable :: fail     !< failure models data structure (nfail)
        type (visc_param_)                           :: visc     !< viscosity model data structure
        type (therm_param_)                          :: therm    !< thermal model data structure (/heat/mat + /therm_stress)         
        type (eos_param_)                            :: eos      !< eos model data structure
        type (multimat_param_)                       :: multimat !< buffer scpecific to multimaterial laws (51,151) : vfrac and mat internal identifiers
        type (ale_rezon_)                            :: rezon

!        type (submat_)  ,dimension(:) ,allocatable :: submat    !< multi material data structure (to be defined) 

        contains
          procedure :: destruct => destruct_matparam
          procedure :: zeroing => zeroing_matparam

      end type matparam_struct_



      contains

      !||====================================================================
      !||    zeroing_matparam   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine zeroing_matparam(this)
          use constant_mod , only : zero
          implicit none
          class(matparam_struct_), intent(inout) :: this

            !initialize to 0 integer and real from data structure

            this%rho = zero
            this%rho0 = zero
            this%young = zero
            this%bulk = zero
            this%shear = zero
            this%nu = zero
            this%stiff_contact = zero
            this%stiff_hglass = zero
            this%stiff_tstep = zero

            !VISC
            this%visc%ilaw = 0
            this%visc%title = ''
            this%visc%nuparam = 0
            this%visc%niparam = 0
            this%visc%nuvar = 0
            this%visc%nfunc = 0
            this%visc%ntable = 0

            !THERM
            this%therm%iform = 0
            this%therm%func_thexp = 0
            this%therm%tini  = zero
            this%therm%tref  = zero
            this%therm%tmelt = zero
            this%therm%rhocp = zero
            this%therm%as = zero
            this%therm%bs = zero
            this%therm%al = zero
            this%therm%bl = zero
            this%therm%efrac = zero
            this%therm%scale_thexp = zero

            !EOS
            this%eos%title = ''
            this%eos%nuparam = 0
            this%eos%niparam = 0
            this%eos%nfunc = 0
            this%eos%ntable = 0
            this%eos%isfluid = 0
            this%eos%cv = zero
            this%eos%cp = zero

            !MULTIMAT
            this%multimat%nb = 0

            !ALE ZERONING
            this%rezon%num_nuvar_mat = 0
            this%rezon%num_nuvar_eos = 0

        end subroutine zeroing_matparam

      !||====================================================================
      !||    destruct_matparam   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
        subroutine destruct_matparam(this)
          implicit none
          class(matparam_struct_), intent(inout) :: this
          integer :: i

          if (allocated(this%uparam)) deallocate(this%uparam)
          if (allocated(this%iparam)) deallocate(this%iparam)
          if (allocated(this%table)) deallocate(this%table)
          if (allocated(this%mode)) deallocate(this%mode)

          if(this%nfail > 0)then
            do i=1,this%nfail
              call this%fail(i)%destruct
            end do
            deallocate(this%fail)
          end if

          if(this%ivisc > 0)then
            call this%visc%destruct
          end if

          if(this%nsubmat > 0)then
             call this%multimat%destruct
          end if

          if(this%ieos > 0)then
            call this%eos%destruct
          end if

        end subroutine destruct_matparam

!
!---------------
      end module matparam_def_mod
