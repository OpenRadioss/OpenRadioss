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
      !||====================================================================
      !||    inoutfile_mod              ../common_source/modules/inoutfile_mod.F
      !||--- called by ------------------------------------------------------
      !||    add_elapsed_time_mon_off   ../engine/source/system/timer.F
      !||    arret                      ../engine/source/system/arret.F
      !||    check_dynain               ../starter/source/starter/check_dynain.F
      !||    check_qeph_stra            ../starter/source/starter/check_qeph_stra.F
      !||    collect                    ../engine/source/output/outfile/collect.F
      !||    collectm                   ../engine/source/output/outfile/collect.F
      !||    collectt                   ../engine/source/output/outfile/collect.F
      !||    contrl                     ../starter/source/starter/contrl.F
      !||    dd_ani                     ../starter/source/output/anim/dd_ani.F
      !||    ddsplit                    ../starter/source/restart/ddsplit/ddsplit.F
      !||    desout                     ../starter/source/output/outp/desout.F
      !||    eig                        ../engine/stub/eig.F
      !||    execargcheck               ../engine/source/engine/execargcheck.F
      !||    find_dt1brick_engine       ../starter/source/starter/contrl.F
      !||    find_dt_engine             ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    find_yield_engine          ../starter/source/general_controls/computation/hm_read_sms.F
      !||    genani                     ../engine/source/output/anim/generate/genani.F
      !||    genani1                    ../starter/source/output/anim/genani1.F
      !||    gendynain                  ../engine/source/output/dynain/gendynain.F
      !||    genh3d                     ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genoutp                    ../engine/source/output/sty/genoutp.F
      !||    genstat                    ../engine/source/output/sta/genstat.F
      !||    hist1                      ../engine/source/output/th/hist1.F
      !||    hist13                     ../engine/source/output/th/hist13.F
      !||    hm_read_eig                ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eos_sesame         ../starter/source/materials/eos/hm_read_eos_sesame.F
      !||    hm_read_fxb1               ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_fxb2               ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_refsta             ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    hm_read_sensors            ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_setfxrbyon              ../starter/source/constraints/fxbody/hm_setfxrbyon.F
      !||    ini_fxbody                 ../starter/source/constraints/fxbody/ini_fxbody.F
      !||    ini_h3dtmax_engine         ../starter/source/starter/contrl.F
      !||    inintr                     ../starter/source/interfaces/interf1/inintr.F
      !||    inintr_thkvar              ../starter/source/interfaces/interf1/inintr_thkvar.F
      !||    lec_inimap1d_file          ../starter/source/initial_conditions/inimap/lec_inimap1d_file.F
      !||    lec_inimap2d_file          ../starter/source/initial_conditions/inimap/lec_inimap2d_file.F
      !||    lecnoise                   ../engine/source/general_controls/computation/lecnoise.F
      !||    lectur                     ../engine/source/input/lectur.F
      !||    manctr                     ../engine/source/input/manctr.F
      !||    nloc_dmg_init              ../starter/source/materials/fail/nloc_dmg_init.F
      !||    printime                   ../engine/source/system/timer.F
      !||    r2r_domdec                 ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    r2r_fork                   ../starter/source/coupling/rad2rad/r2r_fork.F
      !||    r2r_group                  ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_input                  ../starter/source/coupling/rad2rad/r2r_input.F
      !||    radioss0                   ../engine/source/engine/radioss0.F
      !||    radioss2                   ../engine/source/engine/radioss2.F
      !||    read_engine_dtmin          ../starter/source/general_controls/damping/read_engine_dtmin.F
      !||    read_flag_ale              ../starter/source/coupling/rad2rad/r2r_fork.F
      !||    read_h3dtmax_key           ../starter/source/starter/contrl.F
      !||    read_pch_file              ../starter/source/constraints/fxbody/read_pch_file.F
      !||    report                     ../engine/source/output/report/report.F
      !||    section_init               ../engine/source/tools/sect/section_init.F
      !||    section_read               ../engine/source/tools/sect/section_read.F
      !||    section_readp              ../engine/source/tools/sect/section_readp.F
      !||    sortie_main                ../engine/source/output/sortie_main.F
      !||    sortie_mvw                 ../engine/source/output/report/sortie_mvw.F
      !||    spmd_collect               ../engine/source/mpi/output/spmd_collect.F
      !||    spmd_collect_multi_fvm     ../engine/source/mpi/output/spmd_collect_multi_fvm.F
      !||    spmd_collect_nlocal        ../engine/source/mpi/output/spmd_collect_nlocal.F
      !||    spmd_collectm              ../engine/source/mpi/output/spmd_collectm.F
      !||    spmd_collectt              ../engine/source/mpi/output/spmd_collectt.F
      !||    starter0                   ../starter/source/starter/starter0.F
      !||    stat_inimap1d_file_spmd    ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap2d_file_spmd    ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    user_output                ../engine/source/user_interface/user_output.F
      !||    writemesh                  ../starter/source/airbag/writeMesh.F
      !||    wrrestp                    ../engine/source/output/restart/wrrestp.F
      !||====================================================================
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
