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
      !||    output_mod                         ../common_source/modules/output/output_mod.F90
      !||--- called by ------------------------------------------------------
      !||    alemain                            ../engine/source/ale/alemain.F
      !||    bforc2                             ../engine/source/ale/bimat/bforc2.F
      !||    cforc3                             ../engine/source/elements/shell/coque/cforc3.F
      !||    contrl                             ../starter/source/starter/contrl.F
      !||    ddsplit                            ../starter/source/restart/ddsplit/ddsplit.F
      !||    ecrit                              ../engine/source/output/ecrit.F
      !||    eng_qaprint_driver                 ../engine/source/output/qaprint/eng_qaprint_driver.F
      !||    eng_qaprint_generalcontrolsinput   ../engine/source/output/qaprint/eng_qaprint_generalcontrolsinput.F
      !||    eosmain                            ../common_source/eos/eosmain.F
      !||    eospolyno                          ../common_source/eos/eospolyno.F
      !||    execargcheck                       ../engine/source/engine/execargcheck.F
      !||    forint                             ../engine/source/elements/forint.F
      !||    forintc                            ../engine/source/elements/forintc.F
      !||    forintp                            ../engine/source/elements/forintp.F
      !||    freform                            ../engine/source/input/freform.F
      !||    frestat                            ../engine/source/input/frestat.F
      !||    genoutp                            ../engine/source/output/sty/genoutp.F
      !||    genstat                            ../engine/source/output/sta/genstat.F
      !||    hist2                              ../engine/source/output/th/hist2.F
      !||    i14frt                             ../engine/source/interfaces/int14/i14frt.F
      !||    i14wfs                             ../engine/source/interfaces/int14/i14wfs.F
      !||    i15ass                             ../engine/source/interfaces/int15/i15ass.F
      !||    idealgas                           ../common_source/eos/idealgas.F
      !||    idealgas_vt                        ../common_source/eos/idealgas_vt.F
      !||    imp_buck                           ../engine/source/implicit/imp_buck.F
      !||    lecinp                             ../engine/source/input/lecinp.F
      !||    lectur                             ../engine/source/input/lectur.F
      !||    lszk                               ../common_source/eos/lszk.F
      !||    meint                              ../engine/source/materials/mat_share/meint.F
      !||    monvol0                            ../engine/source/airbag/monvol0.F
      !||    mulaw                              ../engine/source/materials/mat_share/mulaw.F90
      !||    murnaghan                          ../common_source/eos/murnaghan.F
      !||    nasg                               ../common_source/eos/nasg.F
      !||    noble_abel                         ../common_source/eos/noble_abel.F
      !||    osborne                            ../common_source/eos/osborne.F
      !||    powder_burn                        ../common_source/eos/powder_burn.F
      !||    r2r_input_init                     ../engine/source/coupling/rad2rad/r2r_input_init.F
      !||    radioss2                           ../engine/source/engine/radioss2.F
      !||    rdcomr                             ../engine/source/output/restart/rdcomm.F
      !||    rdresb                             ../engine/source/output/restart/rdresb.F
      !||    report                             ../engine/source/output/report/report.F
      !||    resol                              ../engine/source/engine/resol.F
      !||    resol_head                         ../engine/source/engine/resol_head.F
      !||    resol_init                         ../engine/source/engine/resol_init.F
      !||    sigeps41                           ../engine/source/materials/mat/mat041/sigeps41.F
      !||    sms_encin_2                        ../engine/source/ams/sms_encin_2.F
      !||    sortie_main                        ../engine/source/output/sortie_main.F
      !||    spstres                            ../engine/source/elements/sph/spstres.F
      !||    st_qaprint_driver                  ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_time_histories          ../starter/source/output/qaprint/st_qaprint_time_histories.F
      !||    starter0                           ../starter/source/starter/starter0.F
      !||    stat_size_c                        ../engine/source/output/sta/stat_size.F
      !||    stiffgas                           ../common_source/eos/stiffgas.F
      !||    stop_sensor                        ../engine/source/tools/sensor/stop_sensor.F
      !||    szforc3                            ../engine/source/elements/solid/solidez/szforc3.F
      !||    tabulated                          ../common_source/eos/tabulated.F
      !||    th_time_output                     ../engine/source/output/th/th_time_output.F
      !||    volp_lfluid                        ../engine/source/airbag/volp_lfluid.F
      !||    wrcomr                             ../engine/source/output/restart/wrcomm.F
      !||    wrrestp                            ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    state_file_mod                     ../common_source/modules/output/state_file_mod.F90
      !||    time_history_mod                   ../common_source/modules/output/time_history_mod.F
      !||====================================================================
      module output_mod
        use time_history_mod
        use state_file_mod

        type output_
           type (th_) :: th
           type (state_) :: state
           double precision :: wfext          !< external force work (global value)
           double precision :: wfext_md       !< specific to r2r method
        end type output_

        double precision, pointer :: wfext
        double precision, pointer :: wfext_md

      end module output_mod