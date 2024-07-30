!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    ddsplit                            ../starter/source/restart/ddsplit/ddsplit.F
      !||    ecrit                              ../engine/source/output/ecrit.F
      !||    eng_qaprint_driver                 ../engine/source/output/qaprint/eng_qaprint_driver.F
      !||    eng_qaprint_generalcontrolsinput   ../engine/source/output/qaprint/eng_qaprint_generalcontrolsinput.F
      !||    execargcheck                       ../engine/source/engine/execargcheck.F
      !||    forint                             ../engine/source/elements/forint.F
      !||    forintc                            ../engine/source/elements/forintc.F
      !||    forintp                            ../engine/source/elements/forintp.F
      !||    freform                            ../engine/source/input/freform.F
      !||    frestat                            ../engine/source/input/frestat.F
      !||    genstat                            ../engine/source/output/sta/genstat.F
      !||    lecinp                             ../engine/source/input/lecinp.F
      !||    lectur                             ../engine/source/input/lectur.F
      !||    r2r_input_init                     ../engine/source/coupling/rad2rad/r2r_input_init.F
      !||    radioss2                           ../engine/source/engine/radioss2.F
      !||    rdcomr                             ../engine/source/output/restart/rdcomm.F
      !||    rdresb                             ../engine/source/output/restart/rdresb.F
      !||    resol                              ../engine/source/engine/resol.F
      !||    resol_head                         ../engine/source/engine/resol_head.F
      !||    resol_init                         ../engine/source/engine/resol_init.F
      !||    sms_encin_2                        ../engine/source/ams/sms_encin_2.F
      !||    sortie_main                        ../engine/source/output/sortie_main.F
      !||    spstres                            ../engine/source/elements/sph/spstres.F
      !||    st_qaprint_driver                  ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_time_histories          ../starter/source/output/qaprint/st_qaprint_time_histories.F
      !||    starter0                           ../starter/source/starter/starter0.F
      !||    stat_size_c                        ../engine/source/output/sta/stat_size.F
      !||    stop_sensor                        ../engine/source/tools/sensor/stop_sensor.F
      !||    szforc3                            ../engine/source/elements/solid/solidez/szforc3.F
      !||    th_time_output                     ../engine/source/output/th/th_time_output.F
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
        end type output_

      end module output_mod