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
C Temporary requested module to introduce TANIM_STOP and TANIM_STOP0
C All REAL VARIABLES related to ANIM were moved in this module
C       (initially located in ./engine/share/includes/com04_c.inc and engine/share/includes/scr_14.inc) 
C COMMON BLOCK are no longer maintened
      !||====================================================================
      !||    anim_mod                     ../common_source/modules/output/anim_mod.F
      !||--- called by ------------------------------------------------------
      !||    anim_set2zero_struct         ../engine/source/output/anim/reader/anim_set2zero_struct.F
      !||    ass18                        ../engine/source/interfaces/int18/ass18.F
      !||    contrl                       ../starter/source/starter/contrl.F
      !||    cutcnt                       ../engine/source/tools/sect/cutcnt.F
      !||    ecrit                        ../engine/source/output/ecrit.F
      !||    eng_qaprint_animinput        ../engine/source/output/qaprint/eng_qaprint_animinput.F
      !||    flow0                        ../engine/source/fluid/flow0.F
      !||    freanim                      ../engine/source/output/anim/reader/freanim.F
      !||    fv_up_switch                 ../engine/source/airbag/fv_up_switch.F
      !||    fvbag1                       ../engine/source/airbag/fvbag1.F
      !||    fxbyfor                      ../engine/source/constraints/fxbody/fxbyfor.F
      !||    fxbyvit                      ../engine/source/constraints/fxbody/fxbyvit.F
      !||    h3d_read                     ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    hm_read_anim                 ../starter/source/general_controls/engine/hm_read_anim.F
      !||    i10for3                      ../engine/source/interfaces/int10/i10for3.F
      !||    i14frt                       ../engine/source/interfaces/int14/i14frt.F
      !||    i15ass                       ../engine/source/interfaces/int15/i15ass.F
      !||    i17for3                      ../engine/source/interfaces/int17/i17for3.F
      !||    i17lll4_pena                 ../engine/source/interfaces/int17/i17for3.F
      !||    i17lll_pena                  ../engine/source/interfaces/int17/i17for3.F
      !||    i18for3                      ../engine/source/interfaces/int18/i18for3.F
      !||    i18kine_s                    ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_1               ../engine/source/interfaces/int18/i18main_kine.F
      !||    i20buce_crit                 ../engine/source/interfaces/intsort/i20buce_crit.F
      !||    i20for3                      ../engine/source/interfaces/int20/i20for3.F
      !||    i21ass3                      ../engine/source/interfaces/int21/i21ass3.F
      !||    i22for3                      ../engine/source/interfaces/int22/i22for3.F
      !||    i23for3                      ../engine/source/interfaces/int23/i23for3.F
      !||    i24for3                      ../engine/source/interfaces/int24/i24for3.F
      !||    i25for3                      ../engine/source/interfaces/int25/i25for3.F
      !||    i3for2                       ../engine/source/interfaces/inter2d/i3for2.F
      !||    i3for3                       ../engine/source/interfaces/inter3d/i3for3.F
      !||    i3fri3                       ../engine/source/interfaces/inter3d/i3fri3.F
      !||    i5for3                       ../engine/source/interfaces/inter3d/i5for3.F
      !||    i5fri3                       ../engine/source/interfaces/inter3d/i3fri3.F
      !||    i6ass3                       ../engine/source/interfaces/inter3d/i6ass3.F
      !||    i7for3                       ../engine/source/interfaces/int07/i7for3.F
      !||    i8for3                       ../engine/source/interfaces/inter3d/i8for3.F
      !||    imp_buck                     ../engine/source/implicit/imp_buck.F
      !||    iqela1                       ../engine/source/ale/inter/iqela1.F
      !||    lag_anith                    ../engine/source/tools/lagmul/lag_anith.F
      !||    lag_anithp                   ../engine/source/tools/lagmul/lag_anith.F
      !||    lectur                       ../engine/source/input/lectur.F
      !||    manctr                       ../engine/source/input/manctr.F
      !||    mmain                        ../engine/source/materials/mat_share/mmain.F90
      !||    rdcomi                       ../engine/source/output/restart/rdcomm.F
      !||    rdcomr                       ../engine/source/output/restart/rdcomm.F
      !||    resol                        ../engine/source/engine/resol.F
      !||    sortie_main                  ../engine/source/output/sortie_main.F
      !||    spmd_fiadd11_poff            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd11_pon             ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd17_poff            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd17_pon             ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20_poff            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20e_poff           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20f_pon            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20fe_pon           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd25e_poff           ../engine/source/mpi/interfaces/spmd_fiadd25e_poff.F
      !||    spmd_fiadd25e_pon            ../engine/source/mpi/interfaces/spmd_fiadd25e_pon.F
      !||    spmd_fiadd_poff              ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd_pon               ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_i18kine_pene_com_poff   ../engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
      !||    spmd_sd_xv                   ../engine/source/mpi/nodes/spmd_sd_xv.F
      !||    stop_sensor                  ../engine/source/tools/sensor/stop_sensor.F
      !||    wrcomi                       ../engine/source/output/restart/wrcomm.F
      !||    wrcomr                       ../engine/source/output/restart/wrcomm.F
      !||====================================================================
      MODULE ANIM_MOD
                  
#include "my_real.inc"              

        !REAL VARIABLES FOR ANIMATION FILES
        my_real TANIM, TANIM0            !start time
        my_real DTANIM, DTANIM0          !time frequency
        my_real TANIM_STOP, TANIM_STOP0  !stop time
        my_real TANIMSENS  

      END MODULE ANIM_MOD
