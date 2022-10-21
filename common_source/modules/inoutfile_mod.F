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
Chd|  INOUTFILE_MOD                 modules/inoutfile_mod.F       
Chd|-- called by -----------
Chd|        CHECK_DYNAIN                  starter/source/starter/check_dynain.F
Chd|        CONTRL                        starter/source/starter/contrl.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        DD_ANI                        starter/source/output/anim/dd_ani.F
Chd|        DESOUT                        starter/source/output/outp/desout.F
Chd|        EXECARGCHECK                  starter/source/starter/execargcheck.F
Chd|        FIND_DT1BRICK_ENGINE          starter/source/starter/contrl.F
Chd|        FIND_DT_ENGINE                starter/source/coupling/rad2rad/r2r_speedup.F
Chd|        FIND_YIELD_ENGINE             starter/source/general_controls/computation/hm_read_sms.F
Chd|        FREFORM                       starter/source/starter/freform.F
Chd|        GENANI1                       starter/source/output/anim/genani1.F
Chd|        HM_READ_EIG                   starter/source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_EOS_SESAME            starter/source/materials/eos/hm_read_eos_sesame.F
Chd|        HM_READ_FXB1                  starter/source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_FXB2                  starter/source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_REFSTA                starter/source/loads/reference_state/refsta/hm_read_refsta.F
Chd|        HM_READ_SENSORS               starter/source/tools/sensor/hm_read_sensors.F
Chd|        HM_SETFXRBYON                 starter/source/constraints/fxbody/hm_setfxrbyon.F
Chd|        ININTR                        starter/source/interfaces/interf1/inintr.F
Chd|        ININTR_THKVAR                 starter/source/interfaces/interf1/inintr_thkvar.F
Chd|        INI_FXBODY                    starter/source/constraints/fxbody/ini_fxbody.F
Chd|        INI_H3DTMAX_ENGINE            starter/source/starter/contrl.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        LEC_INIMAP1D_FILE             starter/source/initial_conditions/inimap/lec_inimap1d_file.F
Chd|        LEC_INIMAP2D_FILE             starter/source/initial_conditions/inimap/lec_inimap2d_file.F
Chd|        NLOC_DMG_INIT                 starter/source/materials/fail/nloc_dmg_init.F
Chd|        R2R_DOMDEC                    starter/source/coupling/rad2rad/r2r_domdec.F
Chd|        R2R_FORK                      starter/source/coupling/rad2rad/r2r_fork.F
Chd|        R2R_GROUP                     starter/source/coupling/rad2rad/r2r_group.F
Chd|        R2R_INPUT                     starter/source/coupling/rad2rad/r2r_input.F
Chd|        READ_FLAG_ALE                 starter/source/coupling/rad2rad/r2r_fork.F
Chd|        READ_H3DTMAX_KEY              starter/source/starter/contrl.F
Chd|        READ_PCH_FILE                 starter/source/constraints/fxbody/read_pch_file.F
Chd|        STARTER0                      starter/source/starter/starter0.F
Chd|        USER_OUTPUT                   starter/source/user_interface/user_output.F
Chd|        WRITEMESH                     starter/source/airbag/writeMesh.F
Chd|        ADD_ELAPSED_TIME_MON_OFF      engine/source/system/timer.F  
Chd|        ARRET                         engine/source/system/arret.F  
Chd|        COLLECT                       engine/source/output/outfile/collect.F
Chd|        COLLECTM                      engine/source/output/outfile/collect.F
Chd|        COLLECTT                      engine/source/output/outfile/collect.F
Chd|        EIG                           engine/stub/eig.F             
Chd|        EXECARGCHECK                  engine/source/engine/execargcheck.F
Chd|        GENANI                        engine/source/output/anim/generate/genani.F
Chd|        GENDYNAIN                     engine/source/output/dynain/gendynain.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        GENOUTP                       engine/source/output/sty/genoutp.F
Chd|        GENSTAT                       engine/source/output/sta/genstat.F
Chd|        HIST1                         engine/source/output/th/hist1.F
Chd|        HIST13                        engine/source/output/th/hist13.F
Chd|        LECNOISE                      engine/source/general_controls/computation/lecnoise.F
Chd|        MANCTR                        engine/source/input/manctr.F  
Chd|        PRINTIME                      engine/source/system/timer.F  
Chd|        RADIOSS0                      engine/source/engine/radioss0.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        REPORT                        engine/source/output/report/report.F
Chd|        SECT_INI                      engine/source/tools/sect/sectio.F
Chd|        SECT_READ                     engine/source/tools/sect/sectio.F
Chd|        SECT_READP                    engine/source/tools/sect/sectio.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SORTIE_MVW                    engine/source/output/report/sortie_mvw.F
Chd|        SPMD_COLLECT                  engine/source/mpi/output/spmd_collect.F
Chd|        SPMD_COLLECTM                 engine/source/mpi/output/spmd_collectm.F
Chd|        SPMD_COLLECTT                 engine/source/mpi/output/spmd_collectt.F
Chd|        SPMD_COLLECT_MULTI_FVM        engine/source/mpi/output/spmd_collect_multi_fvm.F
Chd|        SPMD_COLLECT_NLOCAL           engine/source/mpi/output/spmd_collect_nlocal.F
Chd|        STAT_INIMAP1D_FILE_SPMD       engine/source/output/sta/stat_inimap1d_file_spmd.F
Chd|        STAT_INIMAP2D_FILE_SPMD       engine/source/output/sta/stat_inimap2d_file_spmd.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE INOUTFILE_MOD

!--------------------------------------------------------------------
!       INOUTFILE MOD :
!       if -outfile=... cdl is used (starter or engine), all the files
!       (restart files, *.out, T01, anim file...) are written in
!       an user folder
!       OUTFILE_BOOL : logical, true only if -outfile= is used
!       INOUT_BOOL : logical ; true if outfile or infile cdl are used
!--------------------------------------------------------------------
        LOGICAL :: INOUT_BOOL
        !   outfile= option
        LOGICAL :: OUTFILE_BOOL
        INTEGER :: OUTFILE_NAME_LEN
        INTEGER, PARAMETER :: OUTFILE_CHAR_LEN=2048
        CHARACTER(len=OUTFILE_CHAR_LEN) :: OUTFILE_NAME
        !   infile= option
        LOGICAL :: INFILE_BOOL
        INTEGER :: INFILE_NAME_LEN
        INTEGER, PARAMETER :: INFILE_CHAR_LEN=2048
        CHARACTER(len=INFILE_CHAR_LEN) :: INFILE_NAME

        END MODULE INOUTFILE_MOD
