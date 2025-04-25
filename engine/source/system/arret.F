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
      !||====================================================================
      !||    arret                            ../engine/source/system/arret.F
      !||--- called by ------------------------------------------------------
      !||    admerr                           ../engine/source/model/remesh/admerr.F
      !||    admini                           ../engine/source/model/remesh/admini.F
      !||    alero2                           ../engine/source/ale/ale2d/alero2.F
      !||    anim_build_index_all             ../engine/source/output/anim/reader/anim_build_index_all.F
      !||    animx                            ../engine/source/output/anim/generate/animx.F
      !||    arret_message                    ../engine/source/system/arret_message.F
      !||    arret_message_slid               ../engine/source/system/arret_message.F
      !||    bemsolv                          ../engine/source/fluid/bemsolv.F
      !||    bforc2                           ../engine/source/ale/bimat/bforc2.F
      !||    blero2                           ../engine/source/ale/bimat/blero2.F
      !||    build_connectivity               ../engine/source/multifluid/connectivity.F
      !||    c_fortran_dgssv                  ../engine/stub/superlu_stub.F
      !||    c_freemem                        ../engine/stub/superlu_stub.F
      !||    c_tricall                        ../engine/stub/fvmbags_stub.F
      !||    check_error_and_write            ../common_source/tools/memory/shrink_array.F90
      !||    cp_ifront                        ../engine/source/implicit/produt_v.F
      !||    create_h3d_1d_scalar             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
      !||    create_h3d_1d_tensor             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
      !||    create_h3d_1d_torsor             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
      !||    create_h3d_1d_vector             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
      !||    create_h3d_input                 ../engine/source/output/h3d/h3d_build_fortran/create_h3d_input.F
      !||    create_h3d_nodal_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
      !||    create_h3d_nodal_tensor          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
      !||    create_h3d_nodal_vector          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
      !||    create_h3d_parts                 ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts.F
      !||    create_h3d_parts_all             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
      !||    create_h3d_quad_scalar           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
      !||    create_h3d_quad_tensor           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
      !||    create_h3d_quad_vector           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
      !||    create_h3d_shell_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
      !||    create_h3d_shell_tensor          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
      !||    create_h3d_shell_vector          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
      !||    create_h3d_skin_scalar           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
      !||    create_h3d_skin_tensor           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
      !||    create_h3d_skin_vector           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
      !||    create_h3d_solid_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
      !||    create_h3d_solid_tensor          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
      !||    create_h3d_solid_vector          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
      !||    create_h3d_sph_scalar            ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
      !||    create_h3d_sph_tensor            ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
      !||    crklayer3n_adv                   ../engine/source/elements/xfem/crklayer3n_adv.F
      !||    crklayer3n_ini                   ../engine/source/elements/xfem/crklayer3n_ini.F
      !||    crklayer4n_adv                   ../engine/source/elements/xfem/crklayer4n_adv.F
      !||    crklayer4n_ini                   ../engine/source/elements/xfem/crklayer4n_ini.F
      !||    crklen3n_adv                     ../engine/source/elements/xfem/crklen3n_adv.F
      !||    crklen4n_adv                     ../engine/source/elements/xfem/crklen4n_adv.F
      !||    daasolv                          ../engine/source/fluid/daasolv.F
      !||    dim_int7                         ../engine/source/implicit/ind_glob_k.F
      !||    dtnoda                           ../engine/source/time_step/dtnoda.F
      !||    dtnodams                         ../engine/source/time_step/dtnodams.F
      !||    dyna_ina                         ../engine/source/implicit/imp_dyna.F
      !||    dyna_ini                         ../engine/source/implicit/imp_dyna.F
      !||    eig                              ../engine/stub/eig.F
      !||    eigcond                          ../engine/stub/eigcond.F
      !||    enrichc_ini                      ../engine/source/elements/xfem/enrichc_ini.F
      !||    enrichtg_ini                     ../engine/source/elements/xfem/enrichtg_ini.F
      !||    err_mem                          ../engine/source/implicit/lin_solv.F
      !||    err_tpu                          ../engine/source/materials/mat/mat101/sigeps101.F
      !||    error_alloc                      ../engine/source/system/error_alloc.F
      !||    eulro2                           ../engine/source/ale/euler2d/eulro2.F
      !||    execargcheck                     ../engine/source/engine/execargcheck.F
      !||    facepoly                         ../engine/source/airbag/fvmesh.F
      !||    fixflux                          ../engine/source/constraints/thermic/fixflux.F
      !||    forint                           ../engine/source/elements/forint.F
      !||    fralnk                           ../engine/source/input/fralnk.F
      !||    fralnk2                          ../engine/source/input/fralnk.F
      !||    freabf                           ../engine/source/input/freabf.F
      !||    freanim                          ../engine/source/output/anim/reader/freanim.F
      !||    frebcs                           ../engine/source/input/frebcs.F
      !||    frecpl                           ../engine/source/input/frecpl.F
      !||    fredamp                          ../engine/source/input/fredamp.F
      !||    fredebug                         ../engine/source/input/fredebug.F
      !||    fredec2i                         ../engine/source/output/message/fredec2i.F
      !||    fredec3                          ../engine/source/output/message/fredec3.F
      !||    fredli                           ../engine/source/input/fredli.F
      !||    fredli7                          ../engine/source/input/fredli7.F
      !||    fredynain                        ../engine/source/input/fredynain.F
      !||    freeig                           ../engine/source/input/freeig.F
      !||    freflw                           ../engine/source/input/freflw.F
      !||    freform                          ../engine/source/input/freform.F
      !||    frefunc                          ../engine/source/input/frefunc.F
      !||    frefvbag                         ../engine/source/input/frefvbag.F
      !||    frefxinp                         ../engine/source/input/frefxinp.F
      !||    freimpl                          ../engine/source/input/freimpl.F
      !||    freiniv                          ../engine/source/input/freiniv.F
      !||    freint                           ../engine/source/input/freint.F
      !||    frelnk                           ../engine/source/input/frelnk.F
      !||    frenois                          ../engine/source/input/frenois.F
      !||    freoutp                          ../engine/source/input/freoutp.F
      !||    frerbo                           ../engine/source/input/frerbo.F
      !||    frestat                          ../engine/source/input/frestat.F
      !||    frethermal                       ../engine/source/constraints/thermic/frethermal.F
      !||    freupwind                        ../engine/source/input/freupwind.F
      !||    freupwm                          ../engine/source/input/freupwm.F
      !||    fv_imp0                          ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fv_up_switch                     ../engine/source/airbag/fv_up_switch.F
      !||    fvbag1                           ../engine/source/airbag/fvbag1.F
      !||    fvmesh1                          ../engine/source/airbag/fvmesh.F
      !||    fvrrest                          ../engine/source/output/restart/rdresb.F
      !||    fvvent0                          ../engine/source/airbag/fvvent0.F
      !||    fxbsys                           ../engine/source/constraints/fxbody/fxbsys.F
      !||    genani                           ../engine/source/output/anim/generate/genani.F
      !||    gendynain                        ../engine/source/output/dynain/gendynain.F
      !||    genh3d                           ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genstat                          ../engine/source/output/sta/genstat.F
      !||    get_file_name_info               ../engine/source/system/get_file_name_info.F
      !||    get_u_func_x                     ../engine/source/user_interface/ufunc.F
      !||    get_u_func_y                     ../engine/source/user_interface/ufunc.F
      !||    h3d_read                         ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    i10buce                          ../engine/source/interfaces/intsort/i10buce.F
      !||    i10lll                           ../engine/source/interfaces/int16/i10lagm.F
      !||    i10main_tri                      ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i10mainf                         ../engine/source/interfaces/int10/i10mainf.F
      !||    i10sms2                          ../engine/source/interfaces/int10/i10sms2.F
      !||    i11ass2                          ../engine/source/interfaces/int11/i11ass3.F
      !||    i11ass25                         ../engine/source/interfaces/int11/i11ass3.F
      !||    i11buce_vox                      ../engine/source/interfaces/intsort/i11buce.F
      !||    i11main_tri                      ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11mainf                         ../engine/source/interfaces/int11/i11mainf.F
      !||    i11sms2                          ../engine/source/interfaces/int11/i11sms2.F
      !||    i15cmp                           ../engine/source/interfaces/int15/i15cmp.F
      !||    i16lll                           ../engine/source/interfaces/int16/i16lagm.F
      !||    i16tri                           ../engine/source/interfaces/int16/i16tri.F
      !||    i17buce                          ../engine/source/interfaces/int17/i17buce.F
      !||    i17buce_pena                     ../engine/source/interfaces/int17/i17buce.F
      !||    i17for3                          ../engine/source/interfaces/int17/i17for3.F
      !||    i17lll4                          ../engine/source/interfaces/int17/i17lagm.F
      !||    i17lll4_pena                     ../engine/source/interfaces/int17/i17for3.F
      !||    i17main_tri                      ../engine/source/interfaces/int17/i17main_pena.F
      !||    i17tri                           ../engine/source/interfaces/int17/i17tri.F
      !||    i18main_kine_i                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i20ass2                          ../engine/source/interfaces/int20/i20for3.F
      !||    i20ass25                         ../engine/source/interfaces/int20/i20for3.F
      !||    i20buc_edge                      ../engine/source/interfaces/intsort/i20buce.F
      !||    i20buce                          ../engine/source/interfaces/intsort/i20buce.F
      !||    i20lll                           ../engine/source/interfaces/int16/i20lagm.F
      !||    i20main_tri                      ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                         ../engine/source/interfaces/int20/i20mainf.F
      !||    i20sms2e                         ../engine/source/interfaces/int20/i20sms2.F
      !||    i21buce                          ../engine/source/interfaces/intsort/i21buce.F
      !||    i21lll                           ../engine/source/interfaces/int17/i21lagm.F
      !||    i21mainf                         ../engine/source/interfaces/int21/i21mainf.F
      !||    i21sms2                          ../engine/source/interfaces/int21/i21sms2.F
      !||    i22ass2                          ../engine/source/interfaces/int22/i22assembly.F
      !||    i22buce                          ../engine/source/interfaces/intsort/i22buce.F
      !||    i22intersect                     ../engine/source/interfaces/int22/i22intersect.F
      !||    i22main_tri                      ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                         ../engine/source/interfaces/int22/i22mainf.F
      !||    i23buce                          ../engine/source/interfaces/intsort/i23buce.F
      !||    i23main_tri                      ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                         ../engine/source/interfaces/int23/i23mainf.F
      !||    i24ass2                          ../engine/source/interfaces/int24/i24for3.F
      !||    i24corp3                         ../engine/source/interfaces/int24/i24cork3.F
      !||    i24fic_getn                      ../engine/source/interfaces/int24/i24for3e.F
      !||    i24main_tri                      ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf                         ../engine/source/interfaces/int24/i24main.F
      !||    i24sms2                          ../engine/source/interfaces/int24/i24for3.F
      !||    i25ass2                          ../engine/source/interfaces/int25/i25ass3.F
      !||    i25asse2                         ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse25                        ../engine/source/interfaces/int25/i25asse.F
      !||    i25asse25_e2s                    ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25asse2_e2s                     ../engine/source/interfaces/int25/i25ass_e2s.F
      !||    i25main_slid                     ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25main_tri                      ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                         ../engine/source/interfaces/int25/i25mainf.F
      !||    i25sms0e                         ../engine/source/interfaces/int25/i25smse.F
      !||    i25sms2                          ../engine/source/interfaces/int25/i25for3.F
      !||    i25sms2e                         ../engine/source/interfaces/int25/i25smse.F
      !||    i25sms_e2s                       ../engine/source/interfaces/int25/i25sms_e2s.F
      !||    i2rupt                           ../engine/source/interfaces/interf/int2rupt.F
      !||    i7ass2                           ../engine/source/interfaces/int07/i7ass3.F
      !||    i7ass25                          ../engine/source/interfaces/int07/i7ass3.F
      !||    i7buce_vox                       ../engine/source/interfaces/intsort/i7buce.F
      !||    i7lagm                           ../engine/source/interfaces/int07/i7lagm.F
      !||    i7main_lmult                     ../engine/source/interfaces/int07/i7main_lmult.F
      !||    i7main_tri                       ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                          ../engine/source/interfaces/int07/i7mainf.F
      !||    i7sms2                           ../engine/source/interfaces/int07/i7sms2.F
      !||    i7trivox                         ../engine/source/interfaces/intsort/i7trivox.F
      !||    i8lll                            ../engine/source/interfaces/int16/i8lagm.F
      !||    i9avel                           ../engine/source/interfaces/int09/i9avel.F
      !||    ig3duforc3                       ../engine/source/elements/ige3d/ig3duforc3.F
      !||    imp_buck                         ../engine/source/implicit/imp_buck.F
      !||    imp_dsolv                        ../engine/source/implicit/imp_dsolv.F
      !||    imp_dsolv_iter                   ../engine/source/implicit/imp_dsolv.F
      !||    imp_fsa_inv2                     ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invh                     ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invh2                    ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invp                     ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invp2                    ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_inist                        ../engine/source/implicit/imp_pcg.F
      !||    imp_kfiltr                       ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_pc_inv                       ../engine/source/implicit/imp_pc_inv.F
      !||    imp_pcgh                         ../engine/source/implicit/imp_pcg.F
      !||    imp_sol_init                     ../engine/source/implicit/imp_sol_init.F
      !||    imp_solv                         ../engine/source/implicit/imp_solv.F
      !||    imp_stop                         ../engine/source/implicit/imp_solv.F
      !||    imp_updv2                        ../engine/source/implicit/imp_pcg.F
      !||    ind_ktot                         ../engine/source/implicit/ind_glob_k.F
      !||    ini_k0h                          ../engine/source/implicit/imp_solv.F
      !||    ini_kisc                         ../engine/source/implicit/lin_solv.F
      !||    inipar                           ../engine/source/mpi/init/inipar.F
      !||    intbuf_ini                       ../common_source/interf/intbuf_ini.F
      !||    inter7_collision_detection       ../engine/source/interfaces/intsort/inter7_collision_detection.F90
      !||    inter_count_node_curv            ../engine/source/interfaces/generic/inter_count_node_curv.F
      !||    joint_block_stiffness            ../engine/source/elements/joint/joint_block_stiffness.F
      !||    ktbuf_ini                        ../engine/source/implicit/imp_init.F
      !||    l2g_kloc                         ../engine/source/implicit/ind_glob_k.F
      !||    l_dir02                          ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    lag_direct                       ../engine/source/tools/lagmul/lag_direct.F
      !||    lag_i2main                       ../engine/source/tools/lagmul/lag_i2main.F
      !||    lag_mult_h                       ../engine/source/tools/lagmul/lag_mult_h.F
      !||    lag_mult_hp                      ../engine/source/tools/lagmul/lag_mult_h.F
      !||    lag_mult_sdp                     ../engine/source/tools/lagmul/lag_mult_solv.F
      !||    lag_multp                        ../engine/source/tools/lagmul/lag_mult.F
      !||    lag_rwall                        ../engine/source/tools/lagmul/lag_rwall.F
      !||    lecdamp                          ../engine/source/input/lecdamp.F
      !||    lecflsw                          ../engine/source/fluid/lecflsw.F
      !||    lecfun                           ../engine/source/tools/curve/lecfun.F
      !||    lecfvbag                         ../engine/source/input/lecfvbag.F
      !||    lecfxinp                         ../engine/source/input/lecfxinp.F
      !||    lech3d                           ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lecimpl                          ../engine/source/input/lectur.F
      !||    lecinv                           ../engine/source/input/lecinv.F
      !||    lecnoise                         ../engine/source/general_controls/computation/lecnoise.F
      !||    lecstat                          ../engine/source/input/lecstat.F
      !||    lectur                           ../engine/source/input/lectur.F
      !||    m13law                           ../engine/source/materials/mat/mat013/m13law.F
      !||    mmain                            ../engine/source/materials/mat_share/mmain.F90
      !||    mulaw                            ../engine/source/materials/mat_share/mulaw.F90
      !||    mulaw8                           ../engine/source/materials/mat_share/mulaw8.F90
      !||    mulawc                           ../engine/source/materials/mat_share/mulawc.F90
      !||    multi_allocate                   ../engine/source/multifluid/multi_allocate.F
      !||    multi_fluxes_computation         ../engine/source/multifluid/multi_fluxes_computation.F
      !||    multi_i18_force_pon              ../engine/source/interfaces/int18/multi_i18_force_pon.F
      !||    multi_inlet_ebcs                 ../engine/source/multifluid/multi_inlet_ebcs.F
      !||    multi_muscl_fluxes_computation   ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||    multi_muscl_gradients            ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_nrf_ebcs                   ../engine/source/multifluid/multi_nrf_ebcs.F
      !||    multi_propergol_ebcs             ../engine/source/multifluid/multi_propergol_ebcs.F90
      !||    multi_submatlaw                  ../engine/source/multifluid/multi_submatlaw.F
      !||    multi_timeevolution              ../engine/source/multifluid/multi_timeevolution.F
      !||    ndof_fv                          ../engine/source/implicit/ind_glob_k.F
      !||    pblast_1                         ../engine/source/loads/pblast/pblast_1.F
      !||    pblast_2                         ../engine/source/loads/pblast/pblast_2.F
      !||    pblast_3                         ../engine/source/loads/pblast/pblast_3.F
      !||    phelpinfo                        ../engine/source/engine/execargcheck.F
      !||    prec_solv                        ../engine/source/implicit/prec_solv.F
      !||    projecig3d                       ../engine/source/elements/ige3d/projecig3d.F
      !||    qnorm2t                          ../engine/source/multifluid/qnorm2t.F
      !||    qstat_ini                        ../engine/source/implicit/imp_dyna.F
      !||    qvolu2                           ../engine/source/elements/solid_2d/quad/qvolu2.F
      !||    r_bufbric_22                     ../engine/source/interfaces/int22/r_bufbric_22.F
      !||    radioss2                         ../engine/source/engine/radioss2.F
      !||    rby_decond                       ../engine/source/tools/lagmul/lag_rby_cond.F
      !||    rbyact                           ../engine/source/constraints/general/rbody/rbyact.F
      !||    rdele                            ../engine/source/input/rdele.F
      !||    rdresa                           ../engine/source/output/restart/rdresa.F
      !||    rdresb                           ../engine/source/output/restart/rdresb.F
      !||    read_dynain                      ../engine/source/output/dynain/read_dynain.F
      !||    read_msgfile                     ../engine/source/output/message/read_msgfile.F
      !||    reallocate_fi2                   ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    reallocate_i_skyline             ../engine/source/system/reallocate_skyline.F
      !||    resol                            ../engine/source/engine/resol.F
      !||    restalloc                        ../engine/source/output/restart/arralloc.F
      !||    retractor_table_inv              ../engine/source/tools/seatbelts/retractor_table_inv.F90
      !||    retractor_table_inv2             ../engine/source/tools/seatbelts/retractor_table_inv2.F90
      !||    rforc3                           ../engine/source/elements/spring/rforc3.F
      !||    rgbodv                           ../engine/source/constraints/general/rbody/rgbodv.F
      !||    s16deri3                         ../engine/source/elements/thickshell/solide16/s16deri3.F
      !||    s20deri3                         ../engine/source/elements/solid/solide20/s20deri3.F
      !||    s8deri3                          ../engine/source/elements/solid/solide8/s8deri3.F
      !||    s8ederic3                        ../engine/source/elements/solid/solide8e/s8ederic3.F
      !||    s8ederict3                       ../engine/source/elements/solid/solide8e/s8ederict3.F
      !||    s8slke3                          ../engine/source/elements/solid/solide8s/s8slke3.F
      !||    s8zderic3                        ../engine/source/elements/solid/solide8z/s8zderic3.F
      !||    s8zderict3                       ../engine/source/elements/solid/solide8z/s8zderict3.F
      !||    s8zdericto3                      ../engine/source/elements/solid/solide8z/s8zdericto3.F
      !||    section_read                     ../engine/source/tools/sect/section_read.F
      !||    section_readp                    ../engine/source/tools/sect/section_readp.F
      !||    sensor_base                      ../engine/source/tools/sensor/sensor_base.F
      !||    sigeps53                         ../engine/source/materials/mat/mat053/sigeps53.F
      !||    sigeps75                         ../engine/source/materials/mat/mat075/sigeps75.F
      !||    sms_build_mat_2                  ../engine/source/ams/sms_build_mat_2.F
      !||    sms_encin_2                      ../engine/source/ams/sms_encin_2.F
      !||    sms_fsa_invh                     ../engine/source/ams/sms_fsa_inv.F
      !||    sms_ini_err                      ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_3                    ../engine/source/ams/sms_init.F
      !||    sms_ini_part                     ../engine/source/ams/sms_init.F
      !||    sms_mass_scale_2                 ../engine/source/ams/sms_mass_scale_2.F
      !||    snorm3t                          ../engine/source/multifluid/snorm3t.F
      !||    soltosph_on2                     ../engine/source/elements/sph/soltosph_on2.F
      !||    soltospha                        ../engine/source/elements/sph/soltospha.F
      !||    soltosphf                        ../engine/source/elements/sph/soltosph.F
      !||    sp_a2                            ../engine/source/implicit/imp_fsa_inv.F
      !||    sp_static                        ../engine/source/implicit/imp_fsa_inv.F
      !||    spadah                           ../engine/source/elements/sph/spadah.F
      !||    spbuc3                           ../engine/source/elements/sph/spbuc3.F
      !||    spmd_cell_exchange               ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    spmd_cell_size_exchange          ../engine/source/mpi/interfaces/spmd_cell_size_exchange.F
      !||    spmd_check_tag                   ../engine/source/mpi/ams/spmd_check_tag.F
      !||    spmd_exch_efric                  ../engine/source/mpi/interfaces/spmd_exch_efric.F
      !||    spmd_exch_fa                     ../engine/source/mpi/elements/spmd_exch_dttsh.F
      !||    spmd_exch_i24                    ../engine/source/mpi/interfaces/spmd_exch_i24.F
      !||    spmd_exch_i25                    ../engine/source/mpi/interfaces/spmd_exch_i25.F
      !||    spmd_exch_min_max                ../engine/source/mpi/ale/spmd_exch_min_max.F
      !||    spmd_exch_press                  ../engine/source/mpi/interfaces/spmd_exch_press.F
      !||    spmd_exch_smst2                  ../engine/source/mpi/ams/spmd_exch_smst2.F
      !||    spmd_exch_sorting_efric          ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
      !||    spmd_exch_tagncont               ../engine/source/mpi/forces/spmd_exch_tagncont.F
      !||    spmd_exch_vmax                   ../engine/source/mpi/elements/spmd_exch_dttsh.F
      !||    spmd_exchm_sms                   ../engine/source/mpi/ams/spmd_sms.F
      !||    spmd_fiadd11_pon                 ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20f_pon                ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd20fe_pon               ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd25e_pon                ../engine/source/mpi/interfaces/spmd_fiadd25e_pon.F
      !||    spmd_fiadd_pon                   ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_get_penis                   ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_get_penis20                 ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i17frots_pon                ../engine/source/mpi/interfaces/spmd_i17frots_pon.F
      !||    spmd_i18kine_com_a               ../engine/source/mpi/interfaces/spmd_i18kine_com_a.F
      !||    spmd_i18kine_com_acc             ../engine/source/mpi/interfaces/spmd_i18kine_com_acc.F
      !||    spmd_i18kine_com_ms              ../engine/source/mpi/interfaces/spmd_i18kine_com_ms.F
      !||    spmd_i18kine_com_v               ../engine/source/mpi/interfaces/spmd_i18kine_com_v.F
      !||    spmd_i18kine_pene_com_poff       ../engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
      !||    spmd_i20normf                    ../engine/source/mpi/interfaces/spmd_i20normf.F
      !||    spmd_i21fthecom                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i21tempcom                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i7fcom_poff                 ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon                  ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7xvcom2                    ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_ifront                      ../engine/source/mpi/interfaces/spmd_ifront.F
      !||    spmd_ifront_stamp                ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_inisph                      ../engine/source/mpi/sph/spmd_sptool.F
      !||    spmd_initfi                      ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_int18_law151_pon            ../engine/source/mpi/forces/spmd_int18_law151_pon.F
      !||    spmd_rst_check                   ../engine/source/mpi/init/spmd_rst_check.F
      !||    spmd_sd_xv                       ../engine/source/mpi/nodes/spmd_sd_xv.F
      !||    spmd_sphgat                      ../engine/source/mpi/sph/spmd_sphgat.F
      !||    spmd_sphvox                      ../engine/source/mpi/sph/spmd_sphvox.F
      !||    spmd_tri10box                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri10gat                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11gat                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11vox                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri17box                    ../engine/source/mpi/interfaces/spmd_tri17box.F
      !||    spmd_tri17gat                    ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri18_151vox                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri20box                    ../engine/source/mpi/interfaces/spmd_tri20box.F
      !||    spmd_tri20boxe                   ../engine/source/mpi/interfaces/spmd_tri20boxe.F
      !||    spmd_tri20gat                    ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri20gate                   ../engine/source/mpi/interfaces/spmd_i7crit.F
      !||    spmd_tri22vox                    ../engine/source/mpi/interfaces/spmd_tri22vox.F
      !||    spmd_tri24gat                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri24vox                    ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri25gat                    ../engine/source/mpi/interfaces/spmd_tri25gat.F
      !||    spmd_tri25vox                    ../engine/source/mpi/interfaces/spmd_tri25vox.F
      !||    spmd_tri7gat                     ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox                     ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7vox_optimized           ../engine/source/mpi/interfaces/spmd_tri7vox_optimized.F
      !||    sponof1                          ../engine/source/elements/sph/sponof1.F
      !||    srho3                            ../engine/source/elements/solid/solide/srho3.F
      !||    stack_ply_alloc                  ../engine/share/modules/stack_mod.F
      !||    suforc3                          ../engine/source/user_interface/suforc3.F
      !||    switch_to_dtnoda                 ../engine/source/time_step/switch_to_dtnoda.F
      !||    table2d_vinterp_log              ../engine/source/tools/curve/table2d_vinterp_log.F
      !||    table_interp                     ../engine/source/tools/curve/table_tools.F
      !||    table_interp_dydx                ../engine/source/tools/curve/table_tools.F
      !||    table_interp_law76               ../engine/source/tools/curve/table_tools.F
      !||    table_mat_vinterp                ../engine/source/materials/tools/table_mat_vinterp.F
      !||    table_rresti                     ../engine/source/tools/curve/table_tools.F
      !||    table_vinterp                    ../engine/source/tools/curve/table_tools.F
      !||    trace_print                      ../engine/source/system/trace_back.F
      !||    tribox3                          ../engine/stub/fvmbags_stub.F
      !||    tritri3                          ../engine/stub/fvmbags_stub.F
      !||    upd_aspc0                        ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    update_slipring                  ../engine/source/tools/seatbelts/update_slipring.F
      !||    upgrade_cand_opt                 ../common_source/interf/upgrade_multimp.F
      !||    upgrade_lcand_e2s                ../common_source/interf/upgrade_multimp.F
      !||    upgrade_lcand_edg                ../common_source/interf/upgrade_multimp.F
      !||    upgrade_multimp                  ../common_source/interf/upgrade_multimp.F
      !||    upgrade_rem_2ry                  ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    user_windows_routine             ../engine/source/user_interface/user_windows.F
      !||    usermat_shell                    ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid                    ../engine/source/materials/mat_share/usermat_solid.F
      !||    wciusc2                          ../engine/source/input/wciusc2.F
      !||    write_intbuf                     ../engine/source/output/restart/write_intbuf.F
      !||    wriusc2                          ../engine/source/input/wriusc2.F
      !||    xanim28                          ../engine/source/elements/xelem/xanim28.F
      !||    xforc28                          ../engine/source/elements/xelem/xforc28.F
      !||--- calls      -----------------------------------------------------
      !||    c_h3d_close_file                 ../engine/source/output/h3d/h3d_build_cpp/c_h3d_close_file.cpp
      !||    cur_fil_c                        ../common_source/tools/input_output/write_routtines.c
      !||    delete_tmpfile                   ../common_source/tools/input_output/write_routtines.c
      !||    my_exit                          ../engine/source/system/my_exit.c
      !||    print_runtime                    ../engine/source/system/timer.F
      !||    qaclose                          ../common_source/qa/qa_out_mod.F
      !||    report                           ../engine/source/output/report/report.F
      !||    send_ibuf_c                      ../engine/source/coupling/rad2rad/rad2rad_c.c
      !||    sortie_mvw                       ../engine/source/output/report/sortie_mvw.F
      !||    spmd_kill                        ../engine/source/mpi/init/spmd_kill.F
      !||--- uses       -----------------------------------------------------
      !||    dynlib_mod                       ../engine/share/modules/dynlib_mod.F
      !||    h3d_mod                          ../engine/share/modules/h3d_mod.F
      !||    inoutfile_mod                    ../common_source/modules/inoutfile_mod.F
      !||    qa_out_mod                       ../common_source/qa/qa_out_mod.F
      !||    scratchfile_mod                  ../engine/share/modules/scratchfiles_mod.F
      !||====================================================================
      SUBROUTINE ARRET (N)  
      USE H3D_MOD
      USE INOUTFILE_MOD
      USE QA_OUT_MOD
      USE DYNLIB_MOD
      USE SCRATCHFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "scr20_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "rad2r_c.inc"
#include      "chara_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*2048 INAME
      INTEGER IFIL
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME 
C-----------------------------------------------
C Different End Values
C ----------------------------------------------
C N=0  : Same as N=4
C N=1  : Normal Termination - normal case
C        ARRET(1) should be called after MPI_FINALIZE only (radioss2.F)
C N=2  : Error Termination  - calls MPI_ABORT
C N=20 : Error Termination  - calls MPI_ABORT
C N=3  : Normal termination with "USER BREAK" (crontrol file...)
C        ARRET(3) should be called after  MPI_FINALIZE only (radioss2.F)
C N=4  : Stop Before Engine output creation (*_0001.out)
C N=5  : Restart files not found
C N=6  : Abort with system Error (return code=3)
C N=7  : Exiting only - errors before MPP_INIT or after MPI_FINALIZE
C ----------------------------------------------


C ----------------------------------------------
C TEMPO FILES Clean
C-----------------------------------------------
       IF(ISPMD == 0 )THEN
         IF(IS_H3D_USED) CALL C_H3D_CLOSE_FILE()
       ENDIF
C -----------------------------------------------
C ABF FILES
C -----------------------------------------------
      IF (ISPMD == 0 .AND. NABFILE /= 0 .AND. 
     .  (ABINP /= 0 .AND. ABOUT /= 0 )  ) THEN
#ifdef DNC

           CALL RELEASE_ABFPIPE(ABINP,ABOUT)

           IF(ABFILE(1) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.tmp'
             IFIL = 11
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 9
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(2) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_a.tmp'
             IFIL = 12
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(3) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_b.tmp'
             IFIL = 13
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(4) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_c.tmp'
             IFIL = 14
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(5) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_d.tmp'
             IFIL = 15
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(6) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_e.tmp'
             IFIL = 16
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(7) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_f.tmp'
             IFIL = 17
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(8) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_g.tmp'
             IFIL = 18
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(9) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_h.tmp'
             IFIL = 19
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(10) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_i.tmp'
             IFIL = 20
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
#endif
      ENDIF

      ! ----------------------
      ! user library : check in
      IF(DLIB_STRUCT(ID_ENGINE_USER_FINALIZE)%DLIB_BOOL) THEN
        ISPMD_USER = ISPMD
        CALL ENGINE_USER_FINALIZE( ISPMD_USER )
      ENDIF
      ! ----------------------
#ifdef DNC
      IF(MDS_AVAIL==1) THEN
        ISPMD_USER = ISPMD
        CALL MDS_ENGINE_USER_FINALIZE( ISPMD_USER )
      ENDIF
#endif

      ! ------------------------------------------
      ! Delete Temp. scratchfiles before exiting.
      ! ------------------------------------------
      IF(USE_IFXM == 1)THEN
        ClOSE(UNIT=IFXM)
        CALL DELETE_USER_FILE(IFXM_FN,LEN_IFXM_FN)
      ENDIF
      
      IF(USE_IFXS == 1)THEN
        CLOSE(UNIT=IFXS)
        CALL DELETE_USER_FILE(IFXS_FN,LEN_IFXS_FN)
      ENDIF
      
      IF (USE_IEIGM==1)THEN
        CLOSE(IEIGM)
        CALL DELETE_USER_FILE(IEIGM_FN,LEN_IEIGM_FN)
      ENDIF

C -----------------------------------------------
      IF (ISPMD == 0) THEN
       CALL QACLOSE()
      ENDIF
C
      SELECT CASE (N)
        CASE(0)                                  ! Stop before engine output file was written
              WRITE(ISTDO,1100)
              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)
        CASE (1)
              IF (ISPMD==0) THEN
                WRITE(IOUT,1000)          ! Normal termination 
                WRITE(ISTDO,1050)

                WRITE(IOUT,1200) NCYCLE   ! Total Number of cycles
                WRITE(ISTDO,1250) NCYCLE
                CALL PRINT_RUNTIME( )

                IF (IHTML == 1) CALL REPORT(IH3DREADER,N,IMVW)                ! Htlm report
                IF (IMVW /= 0) CALL SORTIE_MVW(IMVW,IMVW_REF)               ! MVW output
                IF ((IRAD2R==1).AND.(ISPMD==0)) CALL SEND_IBUF_C(1,1)      ! Rad2rad termination

              ENDIF

              CLOSE(UNIT=IOUT)
              IF(ISPMD /=0) THEN
                 CALL DELETE_USER_FILE(IUSC4_FNAM,LEN_IUSC4_FNAM)
              ENDIF

              CLOSE(UNIT=IUHIS)
              CALL MY_EXIT(0)
 
        CASE (2)
              WRITE(IOUT,1100)
              WRITE(ISTDO,1150)

              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)

        CASE (20)
              WRITE(IOUT,1100)
              WRITE(ISTDO,1150)

              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)

        CASE (3)
              IF (ISPMD==0) THEN
                WRITE(IOUT,1000)
                WRITE(ISTDO,1050)
                WRITE(IOUT,1300)
                WRITE(ISTDO,1300)

                WRITE(IOUT,1200) NCYCLE
                WRITE(ISTDO,1250) NCYCLE
                CALL PRINT_RUNTIME( )

                IF (IHTML == 1) CALL REPORT(IH3DREADER,N,IMVW)
                IF (IMVW /= 0) CALL SORTIE_MVW(IMVW,IMVW_REF)
              ENDIF

              CLOSE(UNIT=IOUT)
              IF(ISPMD /=0) THEN
                 CALL DELETE_USER_FILE(IUSC4_FNAM,LEN_IUSC4_FNAM)
              ENDIF

              CLOSE(UNIT=IUHIS)

              CALL MY_EXIT(0)

        CASE(4)
              WRITE(ISTDO,1400)
              IF (ISPMD==0) THEN     
              ENDIF
              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)
        CASE(5)                                  ! restart file not found
              WRITE(ISTDO,1400)
              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)
        CASE (6)                                     ! system trapped error (SIGKILL, SIGSEV etc...)
              WRITE(IOUT,1100)
              WRITE(ISTDO,1150)

              IF (ISPMD==0)THEN
              ENDIF
              CALL SPMD_KILL(3)
        CASE (7)                                     ! Exiting only - errors before MPP_INIT
             CALL MY_EXIT(2)
      END SELECT 

C
 1000 FORMAT(///'     NORMAL TERMINATION      ')
 1050 FORMAT('     NORMAL TERMINATION      ')
 1100 FORMAT(///'     ERROR TERMINATION')
 1400 FORMAT(/'     ERROR TERMINATION')
 1150 FORMAT('     ERROR TERMINATION')
 1200 FORMAT(///'     TOTAL NUMBER OF CYCLES  :', I8 / )
 1250 FORMAT('     TOTAL NUMBER OF CYCLES  :', I8)
 1300 FORMAT('     USER BREAK ')
C

      END
