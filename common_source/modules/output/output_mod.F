Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  OUTPUT_MOD                    modules/output/output_mod.F   
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        EXECARGCHECK                  starter/source/starter/execargcheck.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        STARTER0                      starter/source/starter/starter0.F
Chd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_TIME_HISTORIES     starter/source/output/qaprint/st_qaprint_time_histories.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        BFORC2                        engine/source/ale/bimat/bforc2.F
Chd|        CFORC3                        engine/source/elements/shell/coque/cforc3.F
Chd|        ECRIT                         engine/source/output/ecrit.F  
Chd|        ENG_QAPRINT_DRIVER            engine/source/output/qaprint/eng_qaprint_driver.F
Chd|        FORINT                        engine/source/elements/forint.F
Chd|        FORINTC                       engine/source/elements/forintc.F
Chd|        FORINTP                       engine/source/elements/forintp.F
Chd|        FREFORM                       engine/source/input/freform.F 
Chd|        LECINP                        engine/source/input/lecinp.F  
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        R2R_INPUT_INIT                engine/source/coupling/rad2rad/r2r_input_init.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDCOMR                        engine/source/output/restart/rdcomm.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        SMS_ENCIN_2                   engine/source/ams/sms_encin_2.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SPSTRES                       engine/source/elements/sph/spstres.F
Chd|        STOP_SENSOR                   engine/source/tools/sensor/stop_sensor.F
Chd|        SZFORC3                       engine/source/elements/solid/solidez/szforc3.F
Chd|        TH_TIME_OUTPUT                engine/source/output/th/th_time_output.F
Chd|        WRCOMR                        engine/source/output/restart/wrcomm.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|        ENG_QAPRINT_GENERALCONTROLSINPUTengine/source/output/qaprint/eng_qaprint_generalcontrolsinput.F
Chd|-- calls ---------------
Chd|        TIME_HISTORY_MOD              modules/output/time_history_mod.F
Chd|====================================================================
      MODULE OUTPUT_MOD
        USE TIME_HISTORY_MOD

        TYPE OUTPUT_
           TYPE (TH_) :: TH
        END TYPE OUTPUT_

      END MODULE OUTPUT_MOD