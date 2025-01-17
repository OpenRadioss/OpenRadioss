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
      !||    sensor_mod                         ../common_source/modules/sensor_mod.F90
      !||--- called by ------------------------------------------------------
      !||    airbaga                            ../engine/source/airbag/airbag1.F
      !||    airbaga1                           ../engine/source/airbag/airbaga1.F
      !||    alefvm_grav_init                   ../engine/source/ale/alefvm/alefvm_grav_init.F
      !||    alemain                            ../engine/source/ale/alemain.F
      !||    anim_build_index_all               ../engine/source/output/anim/reader/anim_build_index_all.F
      !||    anim_set2zero_struct               ../engine/source/output/anim/reader/anim_set2zero_struct.F
      !||    bcs_wall_trigger                   ../engine/source/boundary_conditions/bcs_wall_trigger.F90
      !||    bforc2                             ../engine/source/ale/bimat/bforc2.F
      !||    boltst                             ../engine/source/elements/solid/solide/boltst.F
      !||    c3bilan                            ../engine/source/elements/sh3n/coque3n/c3bilan.F
      !||    c3forc3                            ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk                        ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3                           ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cbilan                             ../engine/source/elements/shell/coque/cbilan.F
      !||    cdk6forc3                          ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3                           ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cfield_1                           ../engine/source/loads/general/load_centri/cfield.F
      !||    cfield_imp                         ../engine/source/loads/general/load_centri/cfield_imp.F
      !||    cforc3                             ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk                         ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3                             ../engine/source/materials/mat_share/cmain3.F
      !||    convec                             ../engine/source/constraints/thermic/convec.F
      !||    create_seatbelt                    ../starter/source/tools/seatbelts/create_seatbelt.F
      !||    czforc3                            ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk                        ../engine/source/elements/xfem/czforc3_crk.F
      !||    daasolv                            ../engine/source/fluid/daasolv.F
      !||    daasolvp                           ../engine/source/fluid/daasolvp.F
      !||    ddsplit                            ../starter/source/restart/ddsplit/ddsplit.F
      !||    desacti                            ../engine/source/elements/desacti.F
      !||    domdec2                            ../starter/source/spmd/domdec2.F
      !||    dyna_ina                           ../engine/source/implicit/imp_dyna.F
      !||    dyna_wex                           ../engine/source/implicit/imp_dyna.F
      !||    ebcs11                             ../engine/source/boundary_conditions/ebcs/ebcs11.F90
      !||    ebcs_main                          ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||    ecrit                              ../engine/source/output/ecrit.F
      !||    eng_qaprint_animinput              ../engine/source/output/qaprint/eng_qaprint_animinput.F
      !||    eng_qaprint_driver                 ../engine/source/output/qaprint/eng_qaprint_driver.F
      !||    eng_qaprint_generalcontrolsinput   ../engine/source/output/qaprint/eng_qaprint_generalcontrolsinput.F
      !||    fixfingeo                          ../engine/source/constraints/general/impvel/fixfingeo.F
      !||    fixflux                            ../engine/source/constraints/thermic/fixflux.F
      !||    fixtemp                            ../engine/source/constraints/thermic/fixtemp.F
      !||    fixvel                             ../engine/source/constraints/general/impvel/fixvel.F
      !||    flow0                              ../engine/source/fluid/flow0.F
      !||    force                              ../engine/source/loads/general/force.F90
      !||    force_imp                          ../engine/source/loads/general/force_imp.F
      !||    forcefingeo                        ../engine/source/loads/general/forcefingeo.F
      !||    forcepinch                         ../engine/source/loads/general/forcepinch.F
      !||    forint                             ../engine/source/elements/forint.F
      !||    forintc                            ../engine/source/elements/forintc.F
      !||    freanim                            ../engine/source/output/anim/reader/freanim.F
      !||    freform                            ../engine/source/input/freform.F
      !||    freoutp                            ../engine/source/input/freoutp.F
      !||    frestat                            ../engine/source/input/frestat.F
      !||    fsdcod                             ../starter/source/system/fsdcod.F
      !||    fv_fint0                           ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fv_imp                             ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fv_up_switch                       ../engine/source/airbag/fv_up_switch.F
      !||    fvbag0                             ../engine/source/airbag/fvbag0.F
      !||    fvbag1                             ../engine/source/airbag/fvbag1.F
      !||    fvinjt6                            ../engine/source/airbag/fvinjt6.F
      !||    fvinjt8                            ../engine/source/airbag/fvinjt8.F
      !||    fxbodfp2                           ../engine/source/constraints/fxbody/fxbodfp.F
      !||    fxbyfor                            ../engine/source/constraints/fxbody/fxbyfor.F
      !||    genh3d                             ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    get_preload_axial                  ../engine/source/elements/spring/preload_axial.F90
      !||    get_u_numsens                      ../engine/source/user_interface/usensor.F
      !||    get_u_sens                         ../engine/source/user_interface/usensor.F
      !||    get_u_sens_acti                    ../engine/source/user_interface/usensor.F
      !||    get_u_sens_delay                   ../engine/source/user_interface/usensor.F
      !||    get_u_sens_fpar                    ../engine/source/user_interface/usensor.F
      !||    get_u_sens_id                      ../engine/source/user_interface/usensor.F
      !||    get_u_sens_ipar                    ../engine/source/user_interface/usensor.F
      !||    get_u_sens_value                   ../engine/source/user_interface/usensor.F
      !||    gravit                             ../engine/source/loads/general/grav/gravit.F
      !||    gravit_fvm_fem                     ../engine/source/loads/general/grav/gravit_fvm_fem.F
      !||    gravit_imp                         ../engine/source/loads/general/grav/gravit_imp.F
      !||    h3d_pre_skin_scalar                ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_read                           ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    h3d_skin_scalar                    ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_vector                    ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    hist2                              ../engine/source/output/th/hist2.F
      !||    hm_read_activ                      ../starter/source/tools/activ/hm_read_activ.F
      !||    hm_read_bcs_wall                   ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_grav                       ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_inivel                     ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inter_struct               ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_inter_type05               ../starter/source/interfaces/int05/hm_read_inter_type05.F
      !||    hm_read_inter_type07               ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type11               ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type21               ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type24               ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25               ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_interfaces                 ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_load_centri                ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure              ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_monvol_type11              ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type4               ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5               ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6               ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7               ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8               ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9               ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_pcyl                       ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_pfluid                     ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_preload                    ../starter/source/loads/general/preload/hm_read_preload.F
      !||    hm_read_preload_axial              ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_read_rbody                      ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_sensors                    ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_thgrou                     ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrsens                   ../starter/source/output/th/hm_read_thgrsens.F
      !||    i21_icrit                          ../engine/source/interfaces/intsort/i21_icrit.F
      !||    i25comp_1                          ../engine/source/interfaces/int25/i25comp_1.F
      !||    i25main_norm                       ../engine/source/interfaces/int25/i25main_norm.F
      !||    i25main_slid                       ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25maind_2                         ../engine/source/interfaces/int25/i25maind_2.F
      !||    ig3duforc3                         ../engine/source/elements/ige3d/ig3duforc3.F
      !||    ige3dbilan                         ../engine/source/elements/ige3d/ige3dbilan.F
      !||    imp_buck                           ../engine/source/implicit/imp_buck.F
      !||    imp_chkm                           ../engine/source/implicit/imp_solv.F
      !||    imp_compab                         ../engine/source/implicit/imp_solv.F
      !||    imp_compabp                        ../engine/source/implicit/imp_solv.F
      !||    imp_dtkin                          ../engine/source/implicit/imp_int_k.F
      !||    imp_icomcrit                       ../engine/source/implicit/imp_int_k.F
      !||    imp_int_k                          ../engine/source/implicit/imp_int_k.F
      !||    imp_intdt                          ../engine/source/implicit/imp_int_k.F
      !||    imp_inttd0                         ../engine/source/implicit/imp_int_k.F
      !||    imp_kpres                          ../engine/source/implicit/imp_glob_k.F
      !||    imp_solv                           ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                          ../engine/source/implicit/imp_int_k.F
      !||    incpflow                           ../engine/source/fluid/incpflow.F
      !||    iniebcs                            ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||    iniparsen                          ../starter/source/tools/sensor/iniparsen.F
      !||    inisen                             ../starter/source/tools/sensor/inisen.F
      !||    init_monvol                        ../starter/source/airbag/init_monvol.F
      !||    initia                             ../starter/source/elements/initia/initia.F
      !||    inivel_dt2                         ../engine/source/loads/general/inivel/inivel_dt2.F90
      !||    inivel_init                        ../engine/source/loads/general/inivel/inivel_init.F90
      !||    inivel_start                       ../engine/source/loads/general/inivel/inivel_start.F90
      !||    intcrit                            ../engine/source/interfaces/intsort/intcrit.F
      !||    inter_check_sort                   ../engine/source/interfaces/generic/inter_check_sort.F
      !||    inter_dcod_sensor                  ../starter/source/interfaces/reader/inter_dcod_sensor.F
      !||    inter_deallocate_wait              ../engine/source/interfaces/generic/inter_deallocate_wait.F
      !||    inter_sort                         ../engine/source/interfaces/generic/inter_sort.F
      !||    intfop1                            ../engine/source/interfaces/interf/intfop1.F
      !||    intfop2                            ../engine/source/interfaces/interf/intfop2.F
      !||    intfop8                            ../engine/source/interfaces/interf/intfop8.F
      !||    inttri                             ../engine/source/interfaces/intsort/inttri.F
      !||    lag_mult                           ../engine/source/tools/lagmul/lag_mult.F
      !||    law158_upd                         ../starter/source/materials/mat/mat158/law158_upd.F
      !||    law19_upd                          ../starter/source/materials/mat/mat019/law19_upd.F90
      !||    law58_upd                          ../starter/source/materials/mat/mat058/law58_upd.F
      !||    lecinp                             ../engine/source/input/lecinp.F
      !||    lectur                             ../engine/source/input/lectur.F
      !||    load_pressure                      ../engine/source/loads/general/load_pressure/load_pressure.F
      !||    manctr                             ../engine/source/input/manctr.F
      !||    material_flow                      ../engine/source/tools/seatbelts/material_flow.F
      !||    monvol0                            ../engine/source/airbag/monvol0.F
      !||    mpp_init                           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    mulawc                             ../engine/source/materials/mat_share/mulawc.F90
      !||    pbilan                             ../engine/source/elements/beam/pbilan.F
      !||    pfluid                             ../engine/source/loads/general/pfluid/pfluid.F
      !||    pforc3                             ../engine/source/elements/beam/pforc3.F
      !||    preload_axial                      ../engine/source/elements/spring/preload_axial.F90
      !||    pressure_cyl                       ../engine/source/loads/general/load_pcyl/pressure_cyl.F
      !||    q4forc2                            ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qbilan                             ../engine/source/elements/solid_2d/quad/qbilan.F
      !||    qforc2                             ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    r1sens3                            ../engine/source/elements/spring/r1sens3.F
      !||    r23forc3                           ../engine/source/elements/spring/r23forc3.F
      !||    r23l114def3                        ../engine/source/elements/spring/r23l114def3.F
      !||    r23law108                          ../engine/source/elements/spring/r23law108.F
      !||    r23law113                          ../engine/source/elements/spring/r23law113.F
      !||    r23law114                          ../engine/source/elements/spring/r23law114.F
      !||    r23sens3                           ../engine/source/elements/spring/r23sens3.F
      !||    r2sens3                            ../engine/source/elements/spring/r2sens3.F
      !||    r3bilan                            ../engine/source/elements/spring/r3bilan.F
      !||    r5bilan                            ../engine/source/elements/spring/r5bilan.F
      !||    radiation                          ../engine/source/constraints/thermic/radiation.F
      !||    radioss2                           ../engine/source/engine/radioss2.F
      !||    rbilan                             ../engine/source/elements/spring/rbilan.F
      !||    rbysens                            ../engine/source/constraints/general/rbody/rbyonf.F
      !||    rdcomi                             ../engine/source/output/restart/rdcomm.F
      !||    rdresa                             ../engine/source/output/restart/rdresa.F
      !||    rdresb                             ../engine/source/output/restart/rdresb.F
      !||    read_monvol                        ../starter/source/airbag/read_monvol.F
      !||    read_sensor_acc                    ../starter/source/tools/sensor/read_sensor_acc.F
      !||    read_sensor_and                    ../starter/source/tools/sensor/read_sensor_and.F
      !||    read_sensor_contact                ../starter/source/tools/sensor/read_sensor_contact.F
      !||    read_sensor_disp                   ../starter/source/tools/sensor/read_sensor_disp.F
      !||    read_sensor_dist_surf              ../starter/source/tools/sensor/read_sensor_dist_surf.F
      !||    read_sensor_energy                 ../starter/source/tools/sensor/read_sensor_energy.F
      !||    read_sensor_gauge                  ../starter/source/tools/sensor/read_sensor_gauge.F
      !||    read_sensor_hic                    ../starter/source/tools/sensor/read_sensor_hic.F
      !||    read_sensor_nic                    ../starter/source/tools/sensor/read_sensor_nic.F
      !||    read_sensor_not                    ../starter/source/tools/sensor/read_sensor_not.F
      !||    read_sensor_or                     ../starter/source/tools/sensor/read_sensor_or.F
      !||    read_sensor_python                 ../starter/source/tools/sensor/hm_read_sensor_python.F90
      !||    read_sensor_rbody                  ../starter/source/tools/sensor/read_sensor_rbody.F
      !||    read_sensor_rwall                  ../starter/source/tools/sensor/read_sensor_rwall.F
      !||    read_sensor_sect                   ../starter/source/tools/sensor/read_sensor_sect.F
      !||    read_sensor_sens                   ../starter/source/tools/sensor/read_sensor_sens.F
      !||    read_sensor_temp                   ../starter/source/tools/sensor/read_sensor_temp.F
      !||    read_sensor_time                   ../starter/source/tools/sensor/read_sensor_time.F
      !||    read_sensor_user                   ../starter/source/tools/sensor/read_sensor_user.F
      !||    read_sensor_vel                    ../starter/source/tools/sensor/read_sensor_vel.F
      !||    read_sensor_work                   ../starter/source/tools/sensor/read_sensor_work.F
      !||    read_sensors                       ../engine/source/output/restart/read_sensors.F
      !||    resol                              ../engine/source/engine/resol.F
      !||    resol_head                         ../engine/source/engine/resol_head.F
      !||    resol_init                         ../engine/source/engine/resol_init.F
      !||    rforc3                             ../engine/source/elements/spring/rforc3.F
      !||    rgjoint                            ../engine/source/elements/joint/rgjoint.F
      !||    rsens_nic                          ../engine/source/tools/sensor/rsens_nic.F
      !||    rskew33                            ../engine/source/elements/joint/rskew33.F
      !||    ruser32                            ../engine/source/elements/spring/ruser32.F
      !||    s10bilan                           ../engine/source/elements/solid/solide10/s10bilan.F
      !||    s10forc3                           ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16bilan                           ../engine/source/elements/thickshell/solide16/s16bilan.F
      !||    s16forc3                           ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20bilan                           ../engine/source/elements/solid/solide20/s20bilan.F
      !||    s20forc3                           ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4bilan                            ../engine/source/elements/solid/solide4/s4bilan.F
      !||    s4forc3                            ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cbilan                           ../engine/source/elements/thickshell/solide6c/s6cbilan.F
      !||    s6cforc3                           ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8bilan                            ../engine/source/elements/solid/solide8/s8bilan.F
      !||    s8cforc3                           ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3                           ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3                            ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3                           ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3                           ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    sbilan                             ../engine/source/elements/solid/solide/sbilan.F
      !||    scforc3                            ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sensor_acc                         ../engine/source/tools/sensor/sensor_acc.F
      !||    sensor_and                         ../engine/source/tools/sensor/sensor_and.F
      !||    sensor_base                        ../engine/source/tools/sensor/sensor_base.F
      !||    sensor_contact                     ../engine/source/tools/sensor/sensor_contact.F
      !||    sensor_dist                        ../engine/source/tools/sensor/sensor_dist.F
      !||    sensor_dist_surf                   ../engine/source/tools/sensor/sensor_dist_surf.F
      !||    sensor_dist_surf0                  ../engine/source/tools/sensor/sensor_dist_surf0.F
      !||    sensor_ener_sav                    ../engine/source/tools/sensor/sensor_ener_sav.F
      !||    sensor_energy                      ../engine/source/tools/sensor/sensor_energy.F
      !||    sensor_energy_bilan                ../engine/source/tools/sensor/sensor_energy_bilan.F
      !||    sensor_energy_part                 ../engine/source/tools/sensor/sensor_energy_part.F
      !||    sensor_energy_total                ../engine/source/tools/sensor/sensor_energy_total.F
      !||    sensor_gauge                       ../engine/source/tools/sensor/sensor_gauge.F
      !||    sensor_hic                         ../engine/source/tools/sensor/sensor_hic.F
      !||    sensor_init                        ../engine/source/tools/sensor/sensor_init.F
      !||    sensor_logical                     ../engine/source/tools/sensor/sensor_logical.F
      !||    sensor_nic                         ../engine/source/tools/sensor/sensor_nic.F
      !||    sensor_not                         ../engine/source/tools/sensor/sensor_not.F
      !||    sensor_or                          ../engine/source/tools/sensor/sensor_or.F
      !||    sensor_python                      ../engine/source/tools/sensor/sensor_python.F90
      !||    sensor_rbody                       ../engine/source/tools/sensor/sensor_rbody.F
      !||    sensor_rwall                       ../engine/source/tools/sensor/sensor_rwall.F
      !||    sensor_section                     ../engine/source/tools/sensor/sensor_section.F
      !||    sensor_sens                        ../engine/source/tools/sensor/sensor_sens.F
      !||    sensor_spmd                        ../engine/source/tools/sensor/sensor_spmd.F
      !||    sensor_tab_init                    ../starter/source/tools/sensor/sensor_tab_init.F
      !||    sensor_temp                        ../engine/source/tools/sensor/sensor_temp.F
      !||    sensor_temp0                       ../engine/source/tools/sensor/sensor_temp0.F
      !||    sensor_time                        ../engine/source/tools/sensor/sensor_time.F
      !||    sensor_user_alloc                  ../starter/source/tools/sensor/sensor_user_alloc.F
      !||    sensor_user_init                   ../starter/source/tools/sensor/sensor_user_init.F
      !||    sensor_vel                         ../engine/source/tools/sensor/sensor_vel.F
      !||    sensor_work                        ../engine/source/tools/sensor/sensor_work.F
      !||    set_u_sens_acti                    ../engine/source/user_interface/usensor.F
      !||    set_u_sens_deacti                  ../engine/source/user_interface/usensor.F
      !||    set_u_sens_fpar                    ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_ipar                    ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_maxvalue                ../engine/source/user_interface/usensor.F
      !||    set_u_sens_value                   ../engine/source/user_interface/usensor.F
      !||    sforc3                             ../engine/source/elements/solid/solide/sforc3.F
      !||    sigeps158c                         ../engine/source/materials/mat/mat158/sigeps158c.F
      !||    sigeps19c                          ../engine/source/materials/mat/mat019/sigeps19c.F
      !||    sigeps58c                          ../engine/source/materials/mat/mat058/sigeps58c.F
      !||    sms_encin_2                        ../engine/source/ams/sms_encin_2.F
      !||    sms_fixvel                         ../engine/source/ams/sms_fixvel.F
      !||    sms_gravit                         ../engine/source/ams/sms_gravit.F
      !||    sms_mass_scale_2                   ../engine/source/ams/sms_mass_scale_2.F
      !||    sms_pcg                            ../engine/source/ams/sms_pcg.F
      !||    sort_logical_sensors               ../starter/source/tools/sensor/sort_logical_sensors.F
      !||    sortie_main                        ../engine/source/output/sortie_main.F
      !||    spmd_i21fthecom                    ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i21tempcom                    ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i7xvcom2                      ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_ifront                        ../engine/source/mpi/interfaces/spmd_ifront.F
      !||    spmd_ifront_stamp                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_savefi                        ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    sr8bilan                           ../engine/source/elements/solid/solide8/sr8bilan.F
      !||    srbilan                            ../engine/source/elements/solid/solide/srbilan.F
      !||    st_qaprint_driver                  ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_model_tools             ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||    stop_sensor                        ../engine/source/tools/sensor/stop_sensor.F
      !||    szforc3                            ../engine/source/elements/solid/solidez/szforc3.F
      !||    tbilan                             ../engine/source/elements/truss/tbilan.F
      !||    tforc3                             ../engine/source/elements/truss/tforc3.F
      !||    th_time_output                     ../engine/source/output/th/th_time_output.F
      !||    thsens                             ../engine/source/output/th/thsens.F
      !||    updmat                             ../starter/source/materials/updmat.F
      !||    user_interface_mod                 ../engine/source/modules/user_interface_mod.F90
      !||    user_sensor_mod                    ../starter/source/modules/user_interface_mod.F90
      !||    volpre                             ../engine/source/airbag/volpres.F
      !||    volprep                            ../engine/source/airbag/volpresp.F
      !||    wfv_imp                            ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    wrcomi                             ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                            ../starter/source/restart/ddsplit/wrcommp.F
      !||    write_sensors                      ../engine/source/output/restart/write_sensors.F
      !||    wrrestp                            ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod               ../common_source/modules/names_and_titles_mod.F
      !||    python_funct_mod                   ../common_source/modules/python_mod.F90
      !||====================================================================
      module sensor_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE PYTHON_FUNCT_MOD
      USE NAMES_AND_TITLES_MOD, ONLY:NCHARTITLE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Include
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Parameters
! ----------------------------------------------------------------------------------------------------------------------
      integer ,parameter :: isenbuf  = 20
      integer ,parameter :: lsenbuf  = 101
      integer ,parameter :: nsenpari = 12
      integer ,parameter :: nsenparr = 20
      integer ,parameter :: isenpari = 3
      integer ,parameter :: isenparr = 203
      integer ,parameter :: sensor_type_python = 40
      integer, parameter :: sensor_result_size = 2
      integer ,parameter :: mx_sens  = 50000        !used only in sensor list in engine options /STOP, /OUTP...
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Types
! ----------------------------------------------------------------------------------------------------------------------
      type sensor_str_
        integer :: type        !<   sensor type 
        integer :: sens_id     !<   sensor user id
        integer :: status      !<   sensor status
                               !          = 0   : deactivated
                               !          = 1   : activated at tstart
        character(len = nchartitle) :: title
        my_real :: tcrit       !<   time when activation criterion is met
        my_real :: tmin        !<   time duration of crit value before activation
        my_real :: tdelay      !<   time delay before activation (after tmin)
        my_real :: tstart      !<   time when sensor is finally activated (for output)
        my_real :: value       !<   actual sensor value
        integer :: npari       !<   number of constant integer parameters
        integer :: nparr       !<   number of constant real value parameters
        integer :: nvar        !<   number of internal variables
        integer ,dimension(:) ,allocatable :: iparam  !<  integer parameter array
        my_real ,dimension(:) ,allocatable :: rparam  !<  real parameter array
        my_real ,dimension(:) ,allocatable :: var     !<  internal variables array
        ! user sensor buffers
        integer ,dimension(:) ,allocatable :: integer_userbuf    !<  buffer to store integer variables
        my_real ,dimension(:) ,allocatable :: float_userbuf      !<  buffer to store user variables.
        integer ,dimension(:) ,allocatable :: integer_userparam  !<  buffer to store integer variables
        my_real ,dimension(:) ,allocatable :: float_userparam    !<  buffer to store user variables.
        my_real, dimension(sensor_result_size) :: results
        integer :: python_function_id !< the python functions, if type = sensor_type_python (40)
        type(python_function)  :: python_function !< the python functions, if type = sensor_type_python (40)
      end type sensor_str_

      ! -----------------------------------
      type sub_sensor_type
        integer :: part                                                 !   id of the part
        integer :: num_group_part                                       !   number of element group per part
        real(kind=8), dimension(:,:,:), allocatable :: fbsav6_sens      !   fbsav6 double precision array (parith/on array)           
      end type sub_sensor_type
      ! -----------------------------------
      type sensor_type
        integer :: typ                                                  !   type of energy sensor : 1 --> only 1 part ; 2 --> several subparts
        integer :: part                                                 !   id of the part
        integer :: nb_sub                                               !   number of subpart only useful for typ=2
        integer :: num_group_part                                       !   number of element group per part
        type(sub_sensor_type), dimension(:), allocatable :: sub
        real(kind=8), dimension(:,:,:), allocatable :: fbsav6_sens      !   fbsav6 double precision array (parith/on array)           
      end type sensor_type
      ! -----------------------------------
      type sensor_group
        integer :: num_part                            !   number of part per element group
        integer, dimension(:,:), allocatable :: part   !   size = (num_part,3)
!       structure of sens_group%part :
!       (1:num_part,1): id of the part
!       (1:num_part,2): type of energy sensor (1=part / 2=subset)
!       (1:num_part,3): sensor linked to the current part
      end type sensor_group
      ! -----------------------------------
      type sensor_comm
        logical :: bool                                 !   boolean : true if there is one or more energy sensors
        integer :: num_sens                             !   number of sensor
        integer :: buffer_size_mean                     !   buffer size for mpi_sum reduction
        integer :: buffer_size_min_max                  !   buffer size for min/max reduction
        integer, dimension(:), allocatable :: id_sens   !   id of the sensor
      end type sensor_comm

      type sensor_user_struct_
         logical :: is_used          ! boolean, true if a user sensor is activated
         integer :: pointer_node,number_node ! nbr of node of user sensor + index 
         integer :: pointer_part,number_part ! nbr of part of user sensor + index 
         integer :: pointer_node_per_part,number_node_per_part ! nbr of node defined in a part of user sensor + index 
         integer, dimension(:), allocatable :: node_list ! list of node
         integer, dimension(:), allocatable :: part_list ! list of part
         integer, dimension(:), allocatable :: node_per_part_list ! list of node defined in a part
      end type sensor_user_struct_


      !                                  IPARAM                     RPARAM               VAR
! if type == 0 : sensor time        {}                          {}                 {} 
! if type == 1 : sensor ACCEL
! if type == 2 : sensor DISP
! if type == 3 : sensor SENS
! if type == 4 : sensor AND
! if type == 5 : sensor OR
! if type == 6 : sensor CONTACT result(1) = contact force 
! if type == 7 : sensor RWALL
! if type == 8 : sensor NOT
! if type == 9 : sensor VEL
! if type == 10 : sensor GAUGE
! if type == 11 : sensor RBODY
! if type == 12 : sensor SECT
! if type == 13 : sensor WORK
! if type == 14 : sensor ENERGY  results(1) = kinetic energy, results(2) = internal energy
! if type == 15 : sensor DIST_SURF
! if type == 16 : sensor HIC
! if type == 17 : sensor TEMP
      ! main sensor structure
      type sensors_
        integer :: nsensor 
        integer :: stabsen
        integer :: sfsav
        integer :: nstop
        integer :: nstat
        integer :: noutp
        integer :: nanim
        integer :: nreset
        integer :: anim_id
        integer :: stop_nsth      !< /stop/lsensor - write time history file
        integer :: stop_nsanim    !< /stop/lsensor - write animation file
        integer :: stop_nsstat    !< /stop/lsensor - write state file
        integer :: stop_nsoutp    !< /stop/lsensor - write state file   
        integer :: stop_nsh3d     !< /stop/lsensor - write h3d state
        integer :: stop_nsabf     !< /stop/lsensor - write abf file
        my_real :: anim_dt

        type (sensor_str_) ,dimension(:) ,allocatable :: sensor_tab

        integer            ,dimension(:) ,allocatable :: stop
        integer            ,dimension(:) ,allocatable :: stat
        integer            ,dimension(:) ,allocatable :: outp
        integer            ,dimension(:) ,allocatable :: anim
        integer            ,dimension(:) ,allocatable :: reset
        integer            ,dimension(:) ,allocatable :: stop_tmp
        integer            ,dimension(:) ,allocatable :: outp_tmp
        integer            ,dimension(:) ,allocatable :: stat_tmp
        integer            ,dimension(:) ,allocatable :: anim_tmp

        integer, dimension(:) ,allocatable :: tabsensor  

        integer,dimension(:),allocatable :: ngr_sensor
        ! logical sensors
        integer logical_sensor_count
        integer, dimension(:),allocatable :: logical_sensors_list
  
        ! Engine MPI communication buffers
        double precision ,dimension(:,:,:) ,allocatable :: fsav  ! smpd communication array for "force" sensors           
        type(sensor_comm) :: COMM_SENS14 ! structure for mpi communication : sensor typ14
        type(sensor_comm) :: COMM_SENS16 ! structure for mpi communication : sensor typ16
        type(sensor_comm) :: COMM_SENS17 ! structure for mpi communication : sensor typ17 --> sensor temperature
        type(sensor_type), DIMENSION(:), ALLOCATABLE :: SENSOR_STRUCT     !   structure of energy sensor
        type(sensor_group), DIMENSION(:), ALLOCATABLE :: SENS_GROUP       !   structure of energy sensor              
      END TYPE SENSORS_


      end module sensor_mod 
