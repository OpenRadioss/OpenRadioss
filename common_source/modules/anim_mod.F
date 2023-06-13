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
C Temporary requested module to introduce TANIM_STOP and TANIM_STOP0
C All REAL VARIABLES related to ANIM were moved in this module
C       (intially located in ./engine/share/includes/com04_c.inc and engine/share/includes/scr_14.inc) 
C COMMON BLOCK are no longer maintened
Chd|====================================================================
Chd|  ANIM_MOD                      modules/anim_mod.F            
Chd|-- called by -----------
Chd|        CONTRL                        starter/source/starter/contrl.F
Chd|        HM_READ_ANIM                  starter/source/general_controls/engine/hm_read_anim.F
Chd|        WRCOMR                        starter/source/restart/ddsplit/wrcomm.F
Chd|        ANIM_SET2ZERO_STRUCT          engine/source/output/anim/reader/anim_set2zero_struct.F
Chd|        ASS18                         engine/source/interfaces/int18/ass18.F
Chd|        CUTCNT                        engine/source/tools/sect/cutcnt.F
Chd|        ECRIT                         engine/source/output/ecrit.F  
Chd|        ENG_QAPRINT_ANIMINPUT         engine/source/output/qaprint/eng_qaprint_animinput.F
Chd|        FLOW0                         engine/source/fluid/flow0.F   
Chd|        FREANIM                       engine/source/output/anim/reader/freanim.F
Chd|        FVBAG1                        engine/source/airbag/fvbag1.F 
Chd|        FV_UP_SWITCH                  engine/source/airbag/fv_up_switch.F
Chd|        FXBYFOR                       engine/source/constraints/fxbody/fxbyfor.F
Chd|        FXBYVIT                       engine/source/constraints/fxbody/fxbyvit.F
Chd|        H3D_READ                      engine/source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|        I10FOR3                       engine/source/interfaces/int10/i10for3.F
Chd|        I14FRT                        engine/source/interfaces/int14/i14frt.F
Chd|        I15ASS                        engine/source/interfaces/int15/i15ass.F
Chd|        I17FOR3                       engine/source/interfaces/int17/i17for3.F
Chd|        I17LLL4_PENA                  engine/source/interfaces/int17/i17for3.F
Chd|        I17LLL_PENA                   engine/source/interfaces/int17/i17for3.F
Chd|        I18FOR3                       engine/source/interfaces/int18/i18for3.F
Chd|        I18KINE_S                     engine/source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_1                engine/source/interfaces/int18/i18main_kine.F
Chd|        I20BUCE_CRIT                  engine/source/interfaces/intsort/i20buce_crit.F
Chd|        I20FOR3                       engine/source/interfaces/int20/i20for3.F
Chd|        I21ASS3                       engine/source/interfaces/int21/i21ass3.F
Chd|        I22FOR3                       engine/source/interfaces/int22/i22for3.F
Chd|        I23FOR3                       engine/source/interfaces/int23/i23for3.F
Chd|        I24FOR3                       engine/source/interfaces/int24/i24for3.F
Chd|        I25FOR3                       engine/source/interfaces/int25/i25for3.F
Chd|        I3FOR2                        engine/source/interfaces/inter2d/i3for2.F
Chd|        I3FOR3                        engine/source/interfaces/inter3d/i3for3.F
Chd|        I3FRI3                        engine/source/interfaces/inter3d/i3fri3.F
Chd|        I5FOR3                        engine/source/interfaces/inter3d/i5for3.F
Chd|        I5FRI3                        engine/source/interfaces/inter3d/i3fri3.F
Chd|        I6ASS3                        engine/source/interfaces/inter3d/i6ass3.F
Chd|        I7FOR3                        engine/source/interfaces/int07/i7for3.F
Chd|        I8FOR3                        engine/source/interfaces/inter3d/i8for3.F
Chd|        IMP_BUCK                      engine/source/implicit/imp_buck.F
Chd|        IQELA1                        engine/source/ale/inter/iqela1.F
Chd|        LAG_ANITH                     engine/source/tools/lagmul/lag_anith.F
Chd|        LAG_ANITHP                    engine/source/tools/lagmul/lag_anith.F
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        MANCTR                        engine/source/input/manctr.F  
Chd|        MMAIN                         engine/source/materials/mat_share/mmain.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDCOMR                        engine/source/output/restart/rdcomm.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SPMD_FIADD11_POFF             engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD11_PON              engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD17_POFF             engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD17_PON              engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20E_POFF            engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20FE_PON            engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20F_PON             engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20_POFF             engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD25E_POFF            engine/source/mpi/interfaces/spmd_fiadd25e_poff.F
Chd|        SPMD_FIADD25E_PON             engine/source/mpi/interfaces/spmd_fiadd25e_pon.F
Chd|        SPMD_FIADD_POFF               engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD_PON                engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_I18KINE_PENE_COM_POFF    engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|        SPMD_SD_XV                    engine/source/mpi/nodes/spmd_sd_xv.F
Chd|        STOP_SENSOR                   engine/source/tools/sensor/stop_sensor.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRCOMR                        engine/source/output/restart/wrcomm.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ANIM_MOD
                  
#include "my_real.inc"              

        !REAL VARIABLES FOR ANIMATION FILES
        my_real TANIM, TANIM0            !start time
        my_real DTANIM, DTANIM0          !time frequency
        my_real TANIM_STOP, TANIM_STOP0  !stop time
        my_real TANIMSENS  

      END MODULE ANIM_MOD
