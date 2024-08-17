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
      !||    ancmsg                       ../engine/source/output/message/message.F
      !||--- called by ------------------------------------------------------
      !||    admerr                       ../engine/source/model/remesh/admerr.F
      !||    admini                       ../engine/source/model/remesh/admini.F
      !||    alero2                       ../engine/source/ale/ale2d/alero2.F
      !||    anim_build_index_all         ../engine/source/output/anim/reader/anim_build_index_all.F
      !||    anim_dcod_key_0              ../engine/source/output/anim/reader/anim_dcod_key_0.F
      !||    animx                        ../engine/source/output/anim/generate/animx.F
      !||    arret_message                ../engine/source/system/arret_message.F
      !||    arret_message_slid           ../engine/source/system/arret_message.F
      !||    bforc2                       ../engine/source/ale/bimat/bforc2.F
      !||    blero2                       ../engine/source/ale/bimat/blero2.F
      !||    chkv0_cy                     ../engine/source/constraints/general/bcs/bcscyc.F
      !||    cp_ifront                    ../engine/source/implicit/produt_v.F
      !||    create_h3d_arg_keyword       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
      !||    dim_int7                     ../engine/source/implicit/ind_glob_k.F
      !||    dtnoda                       ../engine/source/time_step/dtnoda.F
      !||    dtnodams                     ../engine/source/time_step/dtnodams.F
      !||    dyna_ina                     ../engine/source/implicit/imp_dyna.F
      !||    dyna_ini                     ../engine/source/implicit/imp_dyna.F
      !||    ecrit                        ../engine/source/output/ecrit.F
      !||    err_mem                      ../engine/source/implicit/lin_solv.F
      !||    err_thk                      ../engine/source/elements/shell/err_thk.F
      !||    errmsg                       ../engine/source/input/errmsg.F
      !||    error_alloc                  ../engine/source/system/error_alloc.F
      !||    eulro2                       ../engine/source/ale/euler2d/eulro2.F
      !||    facepoly                     ../engine/source/airbag/fvmesh.F
      !||    forint                       ../engine/source/elements/forint.F
      !||    fr_rlale                     ../engine/source/mpi/kinematic_conditions/fr_rlink1.F
      !||    fr_rlink1                    ../engine/source/mpi/kinematic_conditions/fr_rlink1.F
      !||    fralnk                       ../engine/source/input/fralnk.F
      !||    fralnk2                      ../engine/source/input/fralnk.F
      !||    freabf                       ../engine/source/input/freabf.F
      !||    freanim                      ../engine/source/output/anim/reader/freanim.F
      !||    frebcs                       ../engine/source/input/frebcs.F
      !||    frecpl                       ../engine/source/input/frecpl.F
      !||    fredamp                      ../engine/source/input/fredamp.F
      !||    fredebug                     ../engine/source/input/fredebug.F
      !||    fredli                       ../engine/source/input/fredli.F
      !||    fredli7                      ../engine/source/input/fredli7.F
      !||    fredynain                    ../engine/source/input/fredynain.F
      !||    freeig                       ../engine/source/input/freeig.F
      !||    freflw                       ../engine/source/input/freflw.F
      !||    freform                      ../engine/source/input/freform.F
      !||    frefunc                      ../engine/source/input/frefunc.F
      !||    frefvbag                     ../engine/source/input/frefvbag.F
      !||    frefxinp                     ../engine/source/input/frefxinp.F
      !||    freimpl                      ../engine/source/input/freimpl.F
      !||    freiniv                      ../engine/source/input/freiniv.F
      !||    freint                       ../engine/source/input/freint.F
      !||    frelnk                       ../engine/source/input/frelnk.F
      !||    frenois                      ../engine/source/input/frenois.F
      !||    freoutp                      ../engine/source/input/freoutp.F
      !||    frerbo                       ../engine/source/input/frerbo.F
      !||    frestat                      ../engine/source/input/frestat.F
      !||    frethermal                   ../engine/source/constraints/thermic/frethermal.F
      !||    freupwind                    ../engine/source/input/freupwind.F
      !||    freupwm                      ../engine/source/input/freupwm.F
      !||    fv_imp0                      ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fv_up_switch                 ../engine/source/airbag/fv_up_switch.F
      !||    fvbag1                       ../engine/source/airbag/fvbag1.F
      !||    fvrrest                      ../engine/source/output/restart/rdresb.F
      !||    fvvent0                      ../engine/source/airbag/fvvent0.F
      !||    genani                       ../engine/source/output/anim/generate/genani.F
      !||    gendynain                    ../engine/source/output/dynain/gendynain.F
      !||    genh3d                       ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genstat                      ../engine/source/output/sta/genstat.F
      !||    get_u_func_x                 ../engine/source/user_interface/ufunc.F
      !||    get_u_func_y                 ../engine/source/user_interface/ufunc.F
      !||    get_u_numnod                 ../engine/source/user_interface/uaccess.F
      !||    h3d_read                     ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    i10buce                      ../engine/source/interfaces/intsort/i10buce.F
      !||    i10lll                       ../engine/source/interfaces/int16/i10lagm.F
      !||    i10main_tri                  ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i10mainf                     ../engine/source/interfaces/int10/i10mainf.F
      !||    i10sms2                      ../engine/source/interfaces/int10/i10sms2.F
      !||    i11ass2                      ../engine/source/interfaces/int11/i11ass3.F
      !||    i11ass25                     ../engine/source/interfaces/int11/i11ass3.F
      !||    i11buce_vox                  ../engine/source/interfaces/intsort/i11buce.F
      !||    i11main_tri                  ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11mainf                     ../engine/source/interfaces/int11/i11mainf.F
      !||    i11sms2                      ../engine/source/interfaces/int11/i11sms2.F
      !||    i15cmp                       ../engine/source/interfaces/int15/i15cmp.F
      !||    i16lll                       ../engine/source/interfaces/int16/i16lagm.F
      !||    i16tri                       ../engine/source/interfaces/int16/i16tri.F
      !||    i17buce                      ../engine/source/interfaces/int17/i17buce.F
      !||    i17for3                      ../engine/source/interfaces/int17/i17for3.F
      !||    i17lll4                      ../engine/source/interfaces/int17/i17lagm.F
      !||    i17lll4_pena                 ../engine/source/interfaces/int17/i17for3.F
      !||    i17main_tri                  ../engine/source/interfaces/int17/i17main_pena.F
      !||    i17tri                       ../engine/source/interfaces/int17/i17tri.F
      !||    i18main_kine_i               ../engine/source/interfaces/int18/i18main_kine.F
      !||    i20ass2                      ../engine/source/interfaces/int20/i20for3.F
      !||    i20ass25                     ../engine/source/interfaces/int20/i20for3.F
      !||    i20buc_edge                  ../engine/source/interfaces/intsort/i20buce.F
      !||    i20buce                      ../engine/source/interfaces/intsort/i20buce.F
      !||    i20lll                       ../engine/source/interfaces/int16/i20lagm.F
      !||    i20main_tri                  ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                     ../engine/source/interfaces/int20/i20mainf.F
      !||    i20sms2e                     ../engine/source/interfaces/int20/i20sms2.F
      !||    i21buce                      ../engine/source/interfaces/intsort/i21buce.F
      !||    i21lll                       ../engine/source/interfaces/int17/i21lagm.F
      !||    i21mainf                     ../engine/source/interfaces/int21/i21mainf.F
      !||    i21sms2                      ../engine/source/interfaces/int21/i21sms2.F
      !||    i22ass2                      ../engine/source/interfaces/int22/i22assembly.F
      !||    i22buce                      ../engine/source/interfaces/intsort/i22buce.F
      !||    i22main_tri                  ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                     ../engine/source/interfaces/int22/i22mainf.F
      !||    i23buce                      ../engine/source/interfaces/intsort/i23buce.F
      !||    i23main_tri                  ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                     ../engine/source/interfaces/int23/i23mainf.F
      !||    i24ass2                      ../engine/source/interfaces/int24/i24for3.F
      !||    i24main_tri                  ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf                     ../engine/source/interfaces/int24/i24main.F
      !||    i24sms2                      ../engine/source/interfaces/int24/i24for3.F
      !||    i25ass2                      ../engine/source/interfaces/int25/i25ass3.F
      !||    i25asse2                     ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse25                    ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse25_e2s                ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25asse2_e2s                 ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25main_slid                 ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25main_tri                  ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                     ../engine/source/interfaces/int25/i25mainf.F
      !||    i25sms0e                     ../engine/source/interfaces/int25/i25smse.F
      !||    i25sms2                      ../engine/source/interfaces/int25/i25for3.F
      !||    i25sms2e                     ../engine/source/interfaces/int25/i25smse.F
      !||    i25sms_e2s                   ../engine/source/interfaces/int25/i25sms_e2s.F
      !||    i2rupt                       ../engine/source/interfaces/interf/int2rupt.F
      !||    i7ass2                       ../engine/source/interfaces/int07/i7ass3.F
      !||    i7ass25                      ../engine/source/interfaces/int07/i7ass3.F
      !||    i7buce_vox                   ../engine/source/interfaces/intsort/i7buce.F
      !||    i7lagm                       ../engine/source/interfaces/int07/i7lagm.F
      !||    i7main_lmult                 ../engine/source/interfaces/int07/i7main_lmult.F
      !||    i7main_tri                   ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                      ../engine/source/interfaces/int07/i7mainf.F
      !||    i7sms2                       ../engine/source/interfaces/int07/i7sms2.F
      !||    i7trivox                     ../engine/source/interfaces/intsort/i7trivox.F
      !||    i8lll                        ../engine/source/interfaces/int16/i8lagm.F
      !||    i9avel                       ../engine/source/interfaces/int09/i9avel.F
      !||    ig3duforc3                   ../engine/source/elements/ige3d/ig3duforc3.F
      !||    imp_buck                     ../engine/source/implicit/imp_buck.F
      !||    imp_fsa_inv2                 ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invh                 ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invh2                ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invp                 ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invp2                ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_kfiltr                   ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_pcgh                     ../engine/source/implicit/imp_pcg.F
      !||    imp_sol_init                 ../engine/source/implicit/imp_sol_init.F
      !||    imp_stop                     ../engine/source/implicit/imp_solv.F
      !||    ind_ktot                     ../engine/source/implicit/ind_glob_k.F
      !||    ini_k0h                      ../engine/source/implicit/imp_solv.F
      !||    ini_kisc                     ../engine/source/implicit/lin_solv.F
      !||    inter7_collision_detection   ../engine/source/interfaces/inter7_collision_detection.F
      !||    inter_count_node_curv        ../engine/source/interfaces/generic/inter_count_node_curv.F
      !||    iqela2                       ../engine/source/ale/inter/iqela2.F
      !||    ktbuf_ini                    ../engine/source/implicit/imp_init.F
      !||    l2g_kloc                     ../engine/source/implicit/ind_glob_k.F
      !||    l_dir02                      ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    lag_direct                   ../engine/source/tools/lagmul/lag_direct.F
      !||    lag_i2main                   ../engine/source/tools/lagmul/lag_i2main.F
      !||    lag_mult_h                   ../engine/source/tools/lagmul/lag_mult_h.F
      !||    lag_mult_hp                  ../engine/source/tools/lagmul/lag_mult_h.F
      !||    lag_multp                    ../engine/source/tools/lagmul/lag_mult.F
      !||    lag_rwall                    ../engine/source/tools/lagmul/lag_rwall.F
      !||    lcbcsf                       ../engine/source/constraints/general/bcs/lcbcsf.F
      !||    lecdamp                      ../engine/source/input/lecdamp.F
      !||    lecflsw                      ../engine/source/fluid/lecflsw.F
      !||    lecfun                       ../engine/source/tools/curve/lecfun.F
      !||    lech3d                       ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lecimpl                      ../engine/source/input/lectur.F
      !||    lecinv                       ../engine/source/input/lecinv.F
      !||    lecnoise                     ../engine/source/general_controls/computation/lecnoise.F
      !||    lecstat                      ../engine/source/input/lecstat.F
      !||    lectur                       ../engine/source/input/lectur.F
      !||    m13law                       ../engine/source/materials/mat/mat013/m13law.F
      !||    mat25_crasurv_c              ../engine/source/materials/mat/mat025/mat25_crasurv_c.F90
      !||    mat25_crasurv_s              ../engine/source/materials/mat/mat025/mat25_crasurv_s.F90
      !||    mat25_tsaiwu_s               ../engine/source/materials/mat/mat025/mat25_tsaiwu_s.F90
      !||    mmain                        ../engine/source/materials/mat_share/mmain.F90
      !||    mpp_init                     ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    mulaw                        ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8                       ../engine/source/materials/mat_share/mulaw8.F
      !||    mulawc                       ../engine/source/materials/mat_share/mulawc.F
      !||    multi_allocate               ../engine/source/multifluid/multi_allocate.F
      !||    multi_i18_force_pon          ../engine/source/interfaces/int18/multi_i18_force_pon.F
      !||    multi_timeevolution          ../engine/source/multifluid/multi_timeevolution.F
      !||    ndof_fv                      ../engine/source/implicit/ind_glob_k.F
      !||    projecig3d                   ../engine/source/elements/ige3d/projecig3d.F
      !||    qstat_ini                    ../engine/source/implicit/imp_dyna.F
      !||    qvolu2                       ../engine/source/elements/solid_2d/quad/qvolu2.F
      !||    radioss2                     ../engine/source/engine/radioss2.F
      !||    rbe3cl                       ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rby_decond                   ../engine/source/tools/lagmul/lag_rby_cond.F
      !||    rbyact                       ../engine/source/constraints/general/rbody/rbyact.F
      !||    rdele                        ../engine/source/input/rdele.F
      !||    rdresa                       ../engine/source/output/restart/rdresa.F
      !||    rdresb                       ../engine/source/output/restart/rdresb.F
      !||    read_dynain                  ../engine/source/output/dynain/read_dynain.F
      !||    read_msgfile                 ../engine/source/output/message/read_msgfile.F
      !||    reallocate_fi2               ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    reallocate_i_skyline         ../engine/source/system/reallocate_skyline.F
      !||    resol                        ../engine/source/engine/resol.F
      !||    restalloc                    ../engine/source/output/restart/arralloc.F
      !||    rforc3                       ../engine/source/elements/spring/rforc3.F
      !||    rgbodv                       ../engine/source/constraints/general/rbody/rgbodv.F
      !||    s10deri3                     ../engine/source/elements/solid/solide10/s10deri3.F
      !||    s16deri3                     ../engine/source/elements/thickshell/solide16/s16deri3.F
      !||    s20deri3                     ../engine/source/elements/solid/solide20/s20deri3.F
      !||    s6cderi3                     ../engine/source/elements/thickshell/solide6c/s6cderi3.F
      !||    s8deri3                      ../engine/source/elements/solid/solide8/s8deri3.F
      !||    s8ederic3                    ../engine/source/elements/solid/solide8e/s8ederic3.F
      !||    s8edericm3                   ../engine/source/elements/solid/solide8e/s8edericm3.F
      !||    s8ederict3                   ../engine/source/elements/solid/solide8e/s8ederict3.F
      !||    s8eforc3                     ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8sforc3                     ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zderic3                    ../engine/source/elements/solid/solide8z/s8zderic3.F
      !||    s8zdericm3                   ../engine/source/elements/solid/solide8z/s8zdericm3.F
      !||    s8zderict3                   ../engine/source/elements/solid/solide8z/s8zderict3.F
      !||    s8zdericto3                  ../engine/source/elements/solid/solide8z/s8zdericto3.F
      !||    s8zforc3                     ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scderi3                      ../engine/source/elements/thickshell/solidec/scderi3.F
      !||    schkjab3                     ../engine/source/elements/solid/solide/schkjab3.F
      !||    schkjabt3                    ../engine/source/elements/solid/solide4/schkjabt3.F
      !||    section_init                 ../engine/source/tools/sect/section_init.F
      !||    section_read                 ../engine/source/tools/sect/section_read.F
      !||    section_readp                ../engine/source/tools/sect/section_readp.F
      !||    sensor_base                  ../engine/source/tools/sensor/sensor_base.F
      !||    sigeps53                     ../engine/source/materials/mat/mat053/sigeps53.F
      !||    sigeps75                     ../engine/source/materials/mat/mat075/sigeps75.F
      !||    smallgeo3                    ../engine/source/elements/solid/solide/smallgeo3.F
      !||    sms_build_mat_2              ../engine/source/ams/sms_build_mat_2.F
      !||    sms_check                    ../engine/source/ams/sms_fsa_inv.F
      !||    sms_encin_2                  ../engine/source/ams/sms_encin_2.F
      !||    sms_fsa_invh                 ../engine/source/ams/sms_fsa_inv.F
      !||    sms_ini_err                  ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_3                ../engine/source/ams/sms_init.F
      !||    sms_ini_part                 ../engine/source/ams/sms_init.F
      !||    sms_mass_scale_2             ../engine/source/ams/sms_mass_scale_2.F
      !||    soltosph_on2                 ../engine/source/elements/sph/soltosph_on2.F
      !||    soltospha                    ../engine/source/elements/sph/soltospha.F
      !||    soltosphf                    ../engine/source/elements/sph/soltosph.F
      !||    spadah                       ../engine/source/elements/sph/spadah.F
      !||    spbuc3                       ../engine/source/elements/sph/spbuc3.F
      !||    sphprep                      ../engine/source/elements/sph/sphprep.F
      !||    splissv                      ../engine/source/elements/sph/splissv.F
      !||    spmd_cell_exchange           ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    spmd_cell_size_exchange      ../engine/source/mpi/interfaces/spmd_cell_size_exchange.F
      !||    spmd_check_tag               ../engine/source/mpi/ams/spmd_check_tag.F
      !||    spmd_exch_efric              ../engine/source/mpi/interfaces/spmd_exch_efric.F
      !||    spmd_exch_fa                 ../engine/source/mpi/elements/spmd_exch_dttsh.F
      !||    spmd_exch_i24                ../engine/source/mpi/interfaces/spmd_exch_i24.F
      !||    spmd_exch_i25                ../engine/source/mpi/interfaces/spmd_exch_i25.F
      !||    spmd_exch_min_max            ../engine/source/mpi/ale/spmd_exch_min_max.F
      !||    spmd_exch_press              ../engine/source/mpi/interfaces/spmd_exch_press.F
      !||    spmd_exch_smst2              ../engine/source/mpi/ams/spmd_exch_smst2.F
      !||    spmd_exch_sorting_efric      ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
      !||    spmd_exch_tagncont           ../engine/source/mpi/forces/spmd_exch_tagncont.F
      !||    spmd_exch_vmax               ../engine/source/mpi/elements/spmd_exch_dttsh.F
      !||    spmd_exchm_sms               ../engine/source/mpi/ams/spmd_sms.F
      !||    spmd_fiadd11_pon             ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20f_pon            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20fe_pon           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd25e_pon            ../engine/source/mpi/interfaces/spmd_fiadd25e_pon.F
      !||    spmd_fiadd_pon               ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_get_penis               ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_penis20             ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i17frots_pon            ../engine/source/mpi/interfaces/spmd_i17frots_pon.F
      !||    spmd_i18kine_com_a           ../engine/source/mpi/interfaces/spmd_i18kine_com_a.F
      !||    spmd_i18kine_com_acc         ../engine/source/mpi/interfaces/spmd_i18kine_com_acc.F
      !||    spmd_i18kine_com_ms          ../engine/source/mpi/interfaces/spmd_i18kine_com_ms.F
      !||    spmd_i18kine_com_v           ../engine/source/mpi/interfaces/spmd_i18kine_com_v.F
      !||    spmd_i18kine_pene_com_poff   ../engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
      !||    spmd_i20normf                ../engine/source/mpi/interfaces/spmd_i20normf.F
      !||    spmd_i21fthecom              ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i21tempcom              ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i7fcom_poff             ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon              ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7xvcom2                ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_ifront                  ../engine/source/mpi/interfaces/spmd_ifront.F
      !||    spmd_ifront_stamp            ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_inisph                  ../engine/source/mpi/sph/spmd_sptool.F
      !||    spmd_initfi                  ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_int18_law151_pon        ../engine/source/mpi/forces/spmd_int18_law151_pon.F
      !||    spmd_rst_check               ../engine/source/mpi/init/spmd_rst_check.F
      !||    spmd_sd_xv                   ../engine/source/mpi/nodes/spmd_sd_xv.F
      !||    spmd_sphgat                  ../engine/source/mpi/sph/spmd_sphgat.F
      !||    spmd_sphvox                  ../engine/source/mpi/sph/spmd_sphvox.F
      !||    spmd_tri10box                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri10gat                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11gat                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11vox                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri17box                ../engine/source/mpi/interfaces/spmd_tri17box.F
      !||    spmd_tri17gat                ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri18_151vox            ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri20box                ../engine/source/mpi/interfaces/spmd_tri20box.F
      !||    spmd_tri20boxe               ../engine/source/mpi/interfaces/spmd_tri20boxe.F
      !||    spmd_tri20gat                ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri20gate               ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri22vox                ../engine/source/mpi/interfaces/spmd_tri22vox.F
      !||    spmd_tri24gat                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24vox                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri25gat                ../engine/source/mpi/interfaces/spmd_tri25gat.F
      !||    spmd_tri25vox                ../engine/source/mpi/interfaces/spmd_tri25vox.F
      !||    spmd_tri7gat                 ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox                 ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox_optimized       ../engine/source/mpi/interfaces/spmd_tri7vox_optimized.F
      !||    sponof1                      ../engine/source/elements/sph/sponof1.F
      !||    srho3                        ../engine/source/elements/solid/solide/srho3.F
      !||    stack_ply_alloc              ../engine/share/modules/stack_mod.F
      !||    stat_inimap1d_file_spmd      ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap1d_spmd           ../engine/source/output/sta/stat_inimap1d_spmd.F
      !||    stat_inimap2d_file_spmd      ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    stat_inimap2d_spmd           ../engine/source/output/sta/stat_inimap2d_spmd.F
      !||    stop_sensor                  ../engine/source/tools/sensor/stop_sensor.F
      !||    suforc3                      ../engine/source/user_interface/suforc3.F
      !||    switch_to_dtnoda             ../engine/source/time_step/switch_to_dtnoda.F
      !||    sysfus                       ../engine/source/system/sysfus.F
      !||    table2d_vinterp_log          ../engine/source/tools/curve/table2d_vinterp_log.F
      !||    table_interp                 ../engine/source/tools/curve/table_tools.F
      !||    table_interp_dydx            ../engine/source/tools/curve/table_tools.F
      !||    table_interp_law76           ../engine/source/tools/curve/table_tools.F
      !||    table_mat_vinterp            ../engine/source/materials/tools/table_mat_vinterp.F
      !||    table_rresti                 ../engine/source/tools/curve/table_tools.F
      !||    table_vinterp                ../engine/source/tools/curve/table_tools.F
      !||    trace_print                  ../engine/source/system/trace_back.F
      !||    upd_aspc0                    ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    update_slipring              ../engine/source/tools/seatbelts/update_slipring.F
      !||    upgrade_rem_2ry              ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    user_windows_routine         ../engine/source/user_interface/user_windows.F
      !||    usermat_shell                ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid                ../engine/source/materials/mat_share/usermat_solid.F
      !||    wciusc2                      ../engine/source/input/wciusc2.F
      !||    wriusc2                      ../engine/source/input/wriusc2.F
      !||    xanim28                      ../engine/source/elements/xelem/xanim28.F
      !||    xforc28                      ../engine/source/elements/xelem/xforc28.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod2                 ../engine/share/message_module/message_mod2.F
      !||    names_and_titles_mod         ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      SUBROUTINE ANCMSG(MSGID ,MSGTYPE,ANMODE,
     .                  I1    ,I2    ,I3    ,I4    ,
     .                  I5    ,I6    ,I7    ,I8    ,I9    ,
     .                  R1    ,R2    ,R3    ,R4    ,
     .                  R5    ,R6    ,R7    ,R8    ,R9   ,
     .                  C1    ,C2    ,C3    ,C4    ,
     .                  C5    ,C6    ,C7    ,C8    ,C9    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD2
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C Usage sample :
C      USE MESSAGE_MOD 
C ...
C      CALL ANCMSG(MSGID=9999,ANMODE=ANINFO_BLIND,
C     .               I1=28,C1='TIME STEP COMPUTATION',C2='SHELL')
C ...         ...         ...         ...         ...
C  ANMODE=ANINFO/ANINFO_BLIND
C         ANINFO write both title and description in standard output and listing
C         ANINFO_BLIND do not write description in standard output
C ...         ...         ...         ...         ...
C  It is recommended to use ANINFO for sensitive messages.
C ...         ...         ...         ...         ...
C  engine_message_description.txt extract :
C ...
C  /MESSAGE/9999/TITLE
C  \n ** ERROR DURING %s\n
C
C  /MESSAGE/9999/DESCRIPTION
C        DURING COMPUTATION IT APPEARS THAT
C        %s ELEMENT ID=%d HAD A TIME STEP EQUAL TO ZERO
C        ENGINE WILL STOP
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSGID,ANMODE,MSGTYPE 
      INTEGER
     .                 I1,I2,I3,I4,
     .                 I5,I6,I7,I8,I9
      my_real
     .                 R1,R2,R3,R4,
     .                 R5,R6,R7,R8,R9
      CHARACTER(*)
     .                 C1,C2,C3,C4,
     .                 C5,C6,C7,C8,C9
      OPTIONAL ::       
     .                 MSGTYPE, ! Warning : MSGTYPE is not optional in the Starter 
     .                          ! but is is not supported in the engine
     .                 I1,I2,I3,I4,
     .                 I5,I6,I7,I8,I9,
     .                 R1,R2,R3,R4,
     .                 R5,R6,R7,R8,R9,
     .                 C1,C2,C3,C4,
     .                 C5,C6,C7,C8,C9
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER             IBUF(10)
      my_real             RBUF(10)
      CHARACTER(LEN=NCHARLINE100):: CBUF(10)
C
      CHARACTER(LEN=NCHARLINE100):: TMPLINE,MYFMT,TMPBUF
      CHARACTER(LEN=NCHAROUT) ::  TMPOUT
      INTEGER ITYPE,ILINE,I,I0,J,J0,SBUFMSG(2),IFILE,IOLD,
     .        INDXI,INDXR,INDXC,INDXTMPOUT,BUFLEN,
     .        STMP
      CHARACTER(LEN=NCHARLINE100):: BUFMSG(2,100),BUFFMT(2,100)
C
      WRITE(ISTDO,'(/A,I10)')'MESSAGE ID : ',MSGID
      IF (IOUT/=0) THEN
        WRITE(IOUT,'(/A,I10)')'MESSAGE ID : ',MSGID
      END IF
      IBUF=0
      RBUF=ZERO
      CBUF=' '
C
      INDXI=0
      INDXR=0
      INDXC=0
C
      TMPOUT=' '
      INDXTMPOUT=0
      TMPBUF=' '
C
      IF (PRESENT(I1)) THEN
        IBUF(1)=I1
        IF (PRESENT(I2)) THEN
          IBUF(2)=I2
          IF (PRESENT(I3)) THEN
            IBUF(3)=I3
            IF (PRESENT(I4)) THEN
              IBUF(4)=I4
              IF (PRESENT(I5)) THEN
                IBUF(5)=I5
                IF (PRESENT(I6)) THEN
                  IBUF(6)=I6
                  IF (PRESENT(I7)) THEN
                    IBUF(7)=I7
                    IF (PRESENT(I8)) THEN
                      IBUF(8)=I8
                      IF (PRESENT(I9)) THEN
                        IBUF(9)=I9
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
C
      IF (PRESENT(R1)) THEN
        RBUF(1)=R1
        IF (PRESENT(R2)) THEN
          RBUF(2)=R2
          IF (PRESENT(R3)) THEN
            RBUF(3)=R3
            IF (PRESENT(R4)) THEN
              RBUF(4)=R4
              IF (PRESENT(R5)) THEN
                RBUF(5)=R5
                IF (PRESENT(R6)) THEN
                  RBUF(6)=R6
                  IF (PRESENT(R7)) THEN
                    RBUF(7)=R7
                    IF (PRESENT(R8)) THEN
                      RBUF(8)=R8
                      IF (PRESENT(R9)) THEN
                        RBUF(9)=R9
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
C
      IF (PRESENT(C1)) THEN
        CBUF(1)=C1
        IF (PRESENT(C2)) THEN
          CBUF(2)=C2
          IF (PRESENT(C3)) THEN
            CBUF(3)=C3
            IF (PRESENT(C4)) THEN
              CBUF(4)=C4
              IF (PRESENT(C5)) THEN
                CBUF(5)=C5
                IF (PRESENT(C6)) THEN
                  CBUF(6)=C6
                  IF (PRESENT(C7)) THEN
                    CBUF(7)=C7
                    IF (PRESENT(C8)) THEN
                      CBUF(8)=C8
                      IF (PRESENT(C9)) THEN
                        CBUF(9)=C9
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
C
        DO ITYPE=1,2
          IF (ALLOCATED(MESSAGES(ITYPE,MSGID)%MESSAGE)) THEN
            DO ILINE=1,MESSAGES(ITYPE,MSGID)%SMESSAGE  
              TMPOUT=' '
              TMPBUF=' '
              TMPLINE=MESSAGES(ITYPE,MSGID)%MESSAGE(ILINE)
              BUFLEN=0
              INDXTMPOUT=0
              I=1
              IOLD=1
!               DO J0=1,LEN_TRIM(TMPLINE)
!                 IF (TMPLINE(J0:J0)=='/') THEN
!                   TMPLINE(J0:J0)=CHAR(10)
!                 END IF
!               END DO
              DO WHILE (I+1<=LEN_TRIM(TMPLINE))
                IF (TMPLINE(I:I)==achar(92)) then !'\') THEN
                  I=I+1
                  IF (I-2>=1) THEN
                    WRITE(TMPBUF,'(A,A)')TMPLINE(IOLD:I-2),TMPLINE(I:I)
                    BUFLEN=I-2-IOLD+1+1
                  ELSE
                    WRITE(TMPBUF,'(A)')TMPLINE(I:I)
                    BUFLEN=1
                  END IF
                  I=I+1
                  IOLD=I
                ELSE IF (TMPLINE(I:I)=='%') THEN
                  I=I+1
                  IF (I-2>=1) THEN
                    WRITE(TMPBUF,'(A)')TMPLINE(IOLD:I-2)
                    BUFLEN=I-2-IOLD+1
                    IF (BUFLEN>0) THEN
                      TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN) 
                      INDXTMPOUT=INDXTMPOUT+BUFLEN
                      BUFLEN=0
                    END IF
                  END IF
                  IF (TMPLINE(I:I)=='d') THEN 
                    I=I+1
                    IOLD=I
                    MYFMT='(I10)'
                    IF (INDXI<10) INDXI=INDXI+1
                    WRITE(TMPBUF,MYFMT)IBUF(INDXI)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  ELSE IF (TMPLINE(I:I)=='f') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(1PG20.13)'
                    IF (INDXR<10) INDXR=INDXR+1
                    WRITE(TMPBUF,MYFMT)RBUF(INDXR)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  ELSE IF (TMPLINE(I:I)=='s') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(A)'
                    IF (INDXC<10) INDXC=INDXC+1 
                    WRITE(TMPBUF,MYFMT)CBUF(INDXC)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  END IF
                ELSE
                  I=I+1
                END IF
                IF (BUFLEN>0) THEN
                  TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
              END DO
              IF (IOLD<=I) THEN
                WRITE(TMPBUF,'(A)')
     .          TMPLINE(IOLD:LEN_TRIM(TMPLINE))
                BUFLEN=LEN_TRIM(TMPLINE)-IOLD+1
                IF (BUFLEN>0) THEN
                  TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
              END IF
! #include "lockon.inc"
              IF (INDXTMPOUT>0) THEN
C               do not write description on stdout in case ANINFO_BLIND
                IF (ANMODE/=ANINFO_BLIND.OR.ITYPE==1) THEN
                  WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                END IF
                IF (IOUT/=0) THEN
                  WRITE(IOUT,'(A)')TMPOUT(1:INDXTMPOUT)
                END IF
              END IF
! #include "lockoff.inc"
            END DO
          END IF
        END DO
      RETURN
      END SUBROUTINE





