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
C=======================================================================
C     // MEMOIRE PARTAGEE
C=======================================================================
      !||====================================================================
      !||    my_barrier                       ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    a22conv3                         ../engine/source/ale/alefvm/cut_cells/a22conv3.F
      !||    aconv3                           ../engine/source/ale/ale3d/aconv3.F
      !||    aconve                           ../engine/source/ale/aconve.F
      !||    admdiv                           ../engine/source/model/remesh/admdiv.F
      !||    admregul                         ../engine/source/model/remesh/admregul.F
      !||    admvit                           ../engine/source/model/remesh/admvit.F
      !||    aeturb                           ../engine/source/ale/turbulence/aeturb.F
      !||    aflux0                           ../engine/source/ale/aflux0.F
      !||    aflux3_int22_fvm                 ../engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
      !||    afluxt                           ../engine/source/ale/ale51/afluxt.F
      !||    agauge                           ../engine/source/ale/agauge.F
      !||    agrad0                           ../engine/source/ale/agrad0.F
      !||    akturb                           ../engine/source/ale/turbulence/akturb.F
      !||    al_constrainth1                  ../engine/source/implicit/nl_solv.F
      !||    al_constrainth2                  ../engine/source/implicit/nl_solv.F
      !||    ale51_finish                     ../engine/source/ale/ale51/ale51_finish.F
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    ale51_init                       ../engine/source/ale/ale51/ale51_init.F
      !||    ale51_upwind3_int22              ../engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
      !||    alefvm_main                      ../engine/source/ale/alefvm/alefvm_main.F
      !||    alefvm_sfint3_int22              ../engine/source/ale/alefvm/alefvm_sfint3_int22.F
      !||    alemain                          ../engine/source/ale/alemain.F
      !||    alesub2                          ../engine/source/ale/subcycling/alesub2.F
      !||    alethe                           ../engine/source/ale/alethe.F
      !||    alew                             ../engine/source/ale/grid/alew.F
      !||    alew5                            ../engine/source/ale/grid/alew5.F
      !||    alew6                            ../engine/source/ale/grid/alew6.F
      !||    alew7                            ../engine/source/ale/grid/alew7.F
      !||    alewdx                           ../engine/source/ale/grid/alewdx.F
      !||    arezon                           ../engine/source/ale/arezon.F
      !||    asspar2                          ../engine/source/assembly/asspar2.F
      !||    asspar3                          ../engine/source/assembly/asspar3.F
      !||    asspar4                          ../engine/source/assembly/asspar4.F
      !||    asspart                          ../engine/source/assembly/asspart.F
      !||    bfgs_h1                          ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_h1p                         ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_h2                          ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_h2p                         ../engine/source/implicit/imp_bfgs.F
      !||    cfield_1                         ../engine/source/loads/general/load_centri/cfield.F
      !||    check_surface_state              ../engine/source/interfaces/interf/check_surface_state.F
      !||    chk11msr3n                       ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk11msr3nb                      ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk20emsr3n                      ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk20emsr3nb                     ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk20msr3n                       ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk20msr3nb                      ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk23msr3n                       ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk23msr3nb                      ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk2msr3n                        ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk2msr3nb                       ../engine/source/interfaces/interf/chkstfn3.F
      !||    chk2msr3np                       ../engine/source/interfaces/interf/chkstfn3.F
      !||    chkload                          ../engine/source/interfaces/chkload.F
      !||    chkmsr3n                         ../engine/source/interfaces/interf/chkstfn3.F
      !||    chkmsr3nb                        ../engine/source/interfaces/interf/chkstfn3.F
      !||    cndint                           ../engine/source/model/remesh/cndint.F
      !||    dyna_ivfac                       ../engine/source/implicit/imp_dyna.F
      !||    eflux3_int22_fvm                 ../engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
      !||    err_thk                          ../engine/source/elements/shell/err_thk.F
      !||    forint                           ../engine/source/elements/forint.F
      !||    forintp                          ../engine/source/elements/forintp.F
      !||    gravit                           ../engine/source/loads/general/grav/gravit.F
      !||    gravit_fvm_fem                   ../engine/source/loads/general/grav/gravit_fvm_fem.F
      !||    i10main_tri                      ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i11buce_vox                      ../engine/source/interfaces/intsort/i11buce.F
      !||    i11main_tri                      ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11trivox                        ../engine/source/interfaces/intsort/i11trivox.F
      !||    i16main                          ../engine/source/interfaces/int16/i16main.F
      !||    i17buce                          ../engine/source/interfaces/int17/i17buce.F
      !||    i17buce_pena                     ../engine/source/interfaces/int17/i17buce.F
      !||    i17for3                          ../engine/source/interfaces/int17/i17for3.F
      !||    i17lagm                          ../engine/source/interfaces/int17/i17lagm.F
      !||    i17main                          ../engine/source/interfaces/int17/i17main.F
      !||    i17main_pena                     ../engine/source/interfaces/int17/i17main_pena.F
      !||    i17main_tri                      ../engine/source/interfaces/int17/i17main_pena.F
      !||    i18main_kine_1                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_2                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_i                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_s                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_v                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i20main_tri                      ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                         ../engine/source/interfaces/int20/i20mainf.F
      !||    i21main_tri                      ../engine/source/interfaces/intsort/i21main_tri.F
      !||    i21optcd                         ../engine/source/interfaces/int21/i21optcd.F
      !||    i22buce                          ../engine/source/interfaces/intsort/i22buce.F
      !||    i22ident                         ../engine/source/interfaces/int22/i22ident.F
      !||    i22intersect                     ../engine/source/interfaces/int22/i22intersect.F
      !||    i22main_tri                      ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                         ../engine/source/interfaces/int22/i22mainf.F
      !||    i22subvol                        ../engine/source/interfaces/int22/i22subvol.F
      !||    i22trivox                        ../engine/source/interfaces/intsort/i22trivox.F
      !||    i23main_tri                      ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                         ../engine/source/interfaces/int23/i23mainf.F
      !||    i23trivox                        ../engine/source/interfaces/intsort/i23trivox.F
      !||    i24main_tri                      ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24trivox                        ../engine/source/interfaces/intsort/i24trivox.F
      !||    i25comp_2                        ../engine/source/interfaces/int25/i25comp_2.F
      !||    i25edge_switch                   ../engine/source/interfaces/int25/i25edge_switch.F
      !||    i25gap3                          ../engine/source/interfaces/int25/i25gap3.F
      !||    i25main_free                     ../engine/source/interfaces/intsort/i25main_free.F
      !||    i25main_norm                     ../engine/source/interfaces/int25/i25main_norm.F
      !||    i25main_opt_tri                  ../engine/source/interfaces/intsort/i25main_opt_tri.F
      !||    i25main_slid                     ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25main_tri                      ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                         ../engine/source/interfaces/int25/i25mainf.F
      !||    i25normp                         ../engine/source/interfaces/int25/i25norm.F
      !||    i25optcd                         ../engine/source/interfaces/intsort/i25optcd.F
      !||    i25optcd_e2s                     ../engine/source/interfaces/intsort/i25optcd_e2s.F
      !||    i25tagn                          ../engine/source/interfaces/int25/i25norm.F
      !||    i25trivox                        ../engine/source/interfaces/intsort/i25trivox.F
      !||    i25trivox_edg                    ../engine/source/interfaces/intsort/i25trivox_edg.F
      !||    i7main_lmult                     ../engine/source/interfaces/int07/i7main_lmult.F
      !||    i7main_tri                       ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                          ../engine/source/interfaces/int07/i7mainf.F
      !||    i7trivox                         ../engine/source/interfaces/intsort/i7trivox.F
      !||    imp_chkm                         ../engine/source/implicit/imp_solv.F
      !||    imp_fsa_inv2hp                   ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invh                     ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invh2                    ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invhp                    ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_inisi                        ../engine/source/implicit/imp_pcg.F
      !||    imp_inist                        ../engine/source/implicit/imp_pcg.F
      !||    imp_inix                         ../engine/source/implicit/imp_pcg.F
      !||    imp_k_eig                        ../engine/stub/imp_k_eig.F
      !||    imp_pcgh                         ../engine/source/implicit/imp_pcg.F
      !||    imp_ppcgh                        ../engine/source/implicit/imp_pcg.F
      !||    imp_pro_p                        ../engine/source/implicit/imp_pcg.F
      !||    imp_solv                         ../engine/source/implicit/imp_solv.F
      !||    imp_updst                        ../engine/source/implicit/imp_pcg.F
      !||    ini_tmax                         ../engine/source/output/ini_outmax.F
      !||    int18_law151_update              ../engine/source/interfaces/int18/int18_law151_update.F
      !||    inter7_collision_detection       ../engine/source/interfaces/inter7_collision_detection.F
      !||    inter_color_coarse_voxel         ../engine/source/interfaces/generic/inter_color_coarse_voxel.F
      !||    inter_color_voxel                ../engine/source/interfaces/generic/inter_color_voxel.F
      !||    inter_count_node_curv            ../engine/source/interfaces/generic/inter_count_node_curv.F
      !||    inter_deallocate_wait            ../engine/source/interfaces/generic/inter_deallocate_wait.F
      !||    inter_minmax_node                ../engine/source/interfaces/generic/inter_minmax_node.F
      !||    inter_prepare_sort               ../engine/source/interfaces/generic/inter_prepare_sort.F
      !||    inter_sort_07                    ../engine/source/interfaces/int07/inter_sort_07.F
      !||    intfop2                          ../engine/source/interfaces/interf/intfop2.F
      !||    inttri                           ../engine/source/interfaces/intsort/inttri.F
      !||    lag_mult                         ../engine/source/tools/lagmul/lag_mult.F
      !||    lin_solv                         ../engine/source/implicit/lin_solv.F
      !||    mav_lt_h                         ../engine/source/implicit/produt_v.F
      !||    mav_ltgh                         ../engine/source/implicit/produt_v.F
      !||    mav_lth                          ../engine/source/implicit/produt_v.F
      !||    mav_lth0                         ../engine/source/implicit/produt_v.F
      !||    mmav_lth                         ../engine/source/implicit/produt_v.F
      !||    mmv_lh                           ../engine/source/implicit/produt_v.F
      !||    mmv_lth                          ../engine/source/implicit/produt_v.F
      !||    mortho_gs                        ../engine/source/implicit/produt_v.F
      !||    multi_timeevolution              ../engine/source/multifluid/multi_timeevolution.F
      !||    nlocal_dtnoda                    ../engine/source/time_step/nlocal_dtnoda.F
      !||    pblast                           ../engine/source/loads/pblast/pblast.F
      !||    pblast_1                         ../engine/source/loads/pblast/pblast_1.F
      !||    pblast_2                         ../engine/source/loads/pblast/pblast_2.F
      !||    pblast_3                         ../engine/source/loads/pblast/pblast_3.F
      !||    prec5h_solv                      ../engine/source/implicit/prec_solv.F
      !||    prec5hc_solv                     ../engine/source/implicit/prec_solv.F
      !||    prec_solvgh                      ../engine/source/implicit/prec_solv.F
      !||    prec_solvh                       ../engine/source/implicit/prec_solv.F
      !||    prelecflow                       ../engine/source/elements/solid/solide/prelecflow.F
      !||    produt_h                         ../engine/source/implicit/produt_v.F
      !||    produt_uh                        ../engine/source/implicit/produt_v.F
      !||    produt_uh2                       ../engine/source/implicit/produt_v.F
      !||    produt_vmh                       ../engine/source/implicit/produt_v.F
      !||    resol                            ../engine/source/engine/resol.F
      !||    resol_init                       ../engine/source/engine/resol_init.F
      !||    rgwal0                           ../engine/source/constraints/general/rwall/rgwal0.F
      !||    rgwalc                           ../engine/source/constraints/general/rwall/rgwalc.F
      !||    rgwall                           ../engine/source/constraints/general/rwall/rgwall.F
      !||    rgwalp                           ../engine/source/constraints/general/rwall/rgwalp.F
      !||    rgwals                           ../engine/source/constraints/general/rwall/rgwals.F
      !||    s10cndf1                         ../engine/source/elements/solid/solide10/s10cndf.F
      !||    s10cndf2                         ../engine/source/elements/solid/solide10/s10cndf.F
      !||    s10cndfnd                        ../engine/source/elements/solid/solide10/s10cndf.F
      !||    s10stfe_poff                     ../engine/source/elements/solid/solide10/s10cndf.F
      !||    seggetv                          ../engine/source/interfaces/interf/seggetv.F
      !||    sinit22_fvm                      ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    sms_admesh_2                     ../engine/source/ams/sms_admesh.F
      !||    sms_build_diag                   ../engine/source/ams/sms_build_diag.F
      !||    sms_build_mat_2                  ../engine/source/ams/sms_build_mat_2.F
      !||    sms_check                        ../engine/source/ams/sms_fsa_inv.F
      !||    sms_cjoint_0                     ../engine/source/ams/sms_cjoint.F
      !||    sms_cjoint_1                     ../engine/source/ams/sms_cjoint.F
      !||    sms_cjoint_2                     ../engine/source/ams/sms_cjoint.F
      !||    sms_encin_2                      ../engine/source/ams/sms_encin_2.F
      !||    sms_inisi                        ../engine/source/ams/sms_proj.F
      !||    sms_inist                        ../engine/source/ams/sms_proj.F
      !||    sms_inix                         ../engine/source/ams/sms_proj.F
      !||    sms_mass_scale_2                 ../engine/source/ams/sms_mass_scale_2.F
      !||    sms_mav_lt                       ../engine/source/ams/sms_pcg.F
      !||    sms_mav_lt1                      ../engine/source/ams/sms_pcg.F
      !||    sms_mav_lt2                      ../engine/source/ams/sms_pcg.F
      !||    sms_mortho_gs                    ../engine/source/ams/sms_proj.F
      !||    sms_pcg                          ../engine/source/ams/sms_pcg.F
      !||    sms_pro_p                        ../engine/source/ams/sms_proj.F
      !||    sms_produt3                      ../engine/source/ams/sms_proj.F
      !||    sms_produt_h                     ../engine/source/ams/sms_proj.F
      !||    sms_rgwal_0                      ../engine/source/ams/sms_rgwal0.F
      !||    sms_updst                        ../engine/source/ams/sms_proj.F
      !||    soltosph_on1                     ../engine/source/elements/sph/soltosph_on1.F
      !||    soltosph_on2                     ../engine/source/elements/sph/soltosph_on2.F
      !||    soltospha                        ../engine/source/elements/sph/soltospha.F
      !||    soltosphf                        ../engine/source/elements/sph/soltosph.F
      !||    spbuc3                           ../engine/source/elements/sph/spbuc3.F
      !||    spclasv                          ../engine/source/elements/sph/spclasv.F
      !||    sphprep                          ../engine/source/elements/sph/sphprep.F
      !||    sphtri0                          ../engine/source/elements/sph/sphtri0.F
      !||    splissv                          ../engine/source/elements/sph/splissv.F
      !||    spstabs                          ../engine/source/elements/sph/spstab.F
      !||    spsym_alloc                      ../engine/source/elements/sph/spsym_alloc.F
      !||    spsymp                           ../engine/source/elements/sph/spsym.F
      !||    sptrivox                         ../engine/source/elements/sph/sptrivox.F
      !||    tagoff3n                         ../engine/source/interfaces/interf/chkstfn3.F
      !||    telesc                           ../engine/source/constraints/general/cyl_joint/telesc.F
      !||    upd_tmax                         ../engine/source/output/upd_outmax.F
      !||    update_struct_int21              ../engine/source/interfaces/int21/update_struct_int21.F
      !||    write_cut_cell_buffer            ../engine/source/interfaces/int22/write_cut_cell_buffer.F
      !||====================================================================
      SUBROUTINE MY_BARRIER
#include     "implicit_f.inc"
#include     "comlock.inc"
#include     "task_c.inc"
#include     "units_c.inc"
#if defined(_OPENMP)
!$OMP BARRIER
#endif
      RETURN
      END
C
      !||====================================================================
      !||    idp_lock       ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    asspar2        ../engine/source/assembly/asspar2.F
      !||    bafil2         ../engine/source/ale/bimat/bafil2.F
      !||    balph2         ../engine/source/ale/bimat/balph2.F
      !||    befil2         ../engine/source/ale/bimat/befil2.F
      !||    qmass2         ../engine/source/elements/solid_2d/quad/qmass2.F
      !||    qmassreal2     ../engine/source/elements/solid_2d/quad/qmassreal2.F
      !||    user_lock      ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE IDP_LOCK(ID)
#include "comlock.inc"
      INTEGER ID
#if defined(_OPENMP)
        CALL OMP_SET_LOCK(LLOCK(1,ID))
#endif
      RETURN
      END
C
      !||====================================================================
      !||    user_lock   ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||    idp_lock    ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE USER_LOCK()
      CALL IDP_LOCK(1)
      RETURN
      END
C
      !||====================================================================
      !||    idp_free         ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    asspar2          ../engine/source/assembly/asspar2.F
      !||    bafil2           ../engine/source/ale/bimat/bafil2.F
      !||    balph2           ../engine/source/ale/bimat/balph2.F
      !||    befil2           ../engine/source/ale/bimat/befil2.F
      !||    qmass2           ../engine/source/elements/solid_2d/quad/qmass2.F
      !||    qmassreal2       ../engine/source/elements/solid_2d/quad/qmassreal2.F
      !||    user_free        ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE IDP_FREE(ID)
#include "comlock.inc"
      INTEGER ID
#if defined(_OPENMP)
        CALL OMP_UNSET_LOCK(LLOCK(1,ID))
#endif
      RETURN
      END
C
      !||====================================================================
      !||    user_free   ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||    idp_free    ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE USER_FREE()
      CALL IDP_FREE(1)
      RETURN
      END
C
      !||====================================================================
      !||    wait_task   ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE WAIT_TASK(ITID)
      INTEGER ITID(*),IT,JT
#if defined(_OPENMP)
#endif
      RETURN
      END

C=======================================================================
C=======================================================================
C     ROUTINES CRAY
C=======================================================================
#if CPP_mach != CPP_c90 && CPP_mach != CPP_ymp && CPP_mach != CPP_ymp_spmd && CPP_mach != CPP_t90 && CPP_mach != CPP_t90_i3e
      my_real
     .   FUNCTION CVMGN(X1,X2,X3)
#include      "implicit_f.inc"
      my_real
     .   X1, X2, X3
      IF(X3==0.) THEN
       CVMGN=X2
      ELSE
       CVMGN=X1
      ENDIF
      RETURN
      END
      my_real
     .   FUNCTION CVMGT(X1,X2,X3)
#include      "implicit_f.inc"
      my_real
     .   X1, X2
      LOGICAL X3
      IF(X3) THEN
       CVMGT=X1
      ELSE
       CVMGT=X2
      ENDIF
      RETURN
      END
#endif
C=======================================================================
C  ROUTINE DE SYNCHRO POUR LE CRAY EN SMP
C=======================================================================
C cette routine doit etre appelee pour eviter certaines
C optimisations super-scalaire incompatible avec notion
C de var. partagee.
      !||====================================================================
      !||    sync_data      ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    admregul       ../engine/source/model/remesh/admregul.F
      !||    i10optcd       ../engine/source/interfaces/intsort/i10optcd.F
      !||    i11optcd       ../engine/source/interfaces/intsort/i11optcd.F
      !||    i20optcd       ../engine/source/interfaces/intsort/i20optcd.F
      !||    i20optcde      ../engine/source/interfaces/intsort/i20optcd.F
      !||    i23optcd       ../engine/source/interfaces/int23/i23optcd.F
      !||    i24optcd       ../engine/source/interfaces/intsort/i24optcd.F
      !||    i25optcd       ../engine/source/interfaces/intsort/i25optcd.F
      !||    i25optcd_e2s   ../engine/source/interfaces/intsort/i25optcd_e2s.F
      !||    i25optcd_edg   ../engine/source/interfaces/intsort/i25optcd_edg.F
      !||    i7optcd        ../engine/source/interfaces/intsort/i7optcd.F
      !||    thnod          ../engine/source/output/th/thnod.F
      !||====================================================================
      SUBROUTINE SYNC_DATA(II)
      INTEGER II(*)
      RETURN
      END
C=======================================================================
C  ROUTINE ETIME
C=======================================================================
      !||====================================================================
      !||    my_etime    ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    startime    ../engine/source/system/timer.F
      !||    startimeg   ../engine/source/system/timer.F
      !||    stoptime    ../engine/source/system/timer.F
      !||    stoptimeg   ../engine/source/system/timer.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MY_ETIME(TIMER)
      REAL  RESULT
      REAL, DIMENSION(2) :: TIMER
#if CPP_mach == CPP_rs7 || CPP_mach == CPP_rs9 || CPP_mach == CPP_sp2  || CPP_mach == CPP_ppc || CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd    
       RESULT = ETIME_(TIMER)
#elif CPP_mach == CPP_ymp || CPP_mach == CPP_c90 || CPP_mach == CPP_ymp_spmd || CPP_mach == CPP_t90  || CPP_mach == CPP_t90_i3e
      CALL TSECND(TIMER)
#elif CPP_mach == CPP_sx4 || CPP_mach == CPP_sx4_i3e
      RESULT = ETIME(TIMER)
/* el51e9 #elif CPP_mach == CPP_w95 || CPP_mach == CPP_ant || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr */
#elif CPP_mach == CPP_w95 || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr
      TIMER(1) = 0.
#elif CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_p4win64
      CALL CPU_TIME(TIMER(1))
#elif CPP_mach == CPP_cx1_spmd || CPP_mach == CPP_cx1
      RESULT = ETIMEC(TIMER)
#elif 1
      RESULT = ETIME(TIMER)
#endif
      RETURN
      END
C=======================================================================
C  ROUTINE  VERIFICATION LIMITATIONS MACHINE
C=======================================================================
      !||====================================================================
      !||    my_flush                         ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    ecrit                            ../engine/source/output/ecrit.F
      !||    get_table_value_dydx             ../engine/source/user_interface/utable.F
      !||    imp_stop                         ../engine/source/implicit/imp_solv.F
      !||    multi_muscl_fluxes_computation   ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MY_FLUSH(IUNIT)
C appel Fortran flush
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IUNIT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C call a rendre specifique machine en cas de pb de compilation
#if CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd
C   pas de routine FLUSH sur AIX
#elif 1
      CALL FLUSH(IUNIT)
#endif
      RETURN
      END

      !||====================================================================
      !||    mkl_init          ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MKL_INIT(MKL_DYN,OMP_NEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      LOGICAL MKL_DYN,OMP_NEST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL omp_get_nested, mkl_get_dynamic
      EXTERNAL omp_get_nested, mkl_get_dynamic
C call a rendre specifique machine en cas de pb de compilation
C#if CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4linux932 || CPP_mach == CPP_il || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
#if defined mkl
         MKL_DYN = omp_get_nested()
         OMP_NEST = mkl_get_dynamic()
         call omp_set_nested(.true.)
         call mkl_set_dynamic(.false.)
#endif
      RETURN
      END

      !||====================================================================
      !||    mkl_reset         ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MKL_RESET(MKL_DYN,OMP_NEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      LOGICAL MKL_DYN,OMP_NEST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL omp_get_nested, mkl_get_dynamic
      EXTERNAL omp_get_nested, mkl_get_dynamic
C call a rendre specifique machine en cas de pb de compilation
C#if CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4linux932 || CPP_mach == CPP_il || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
#if defined mkl
         call omp_set_nested(OMP_NEST)
         call mkl_set_dynamic(MKL_DYN)
c         MKL_DYN = omp_get_nested()
c         OMP_NEST = mkl_get_dynamic()
#endif
      RETURN
      END

#if CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4win64
ccc|| CPP_mach == CPP_p4win64  || CPP_mach == CPP_p4win32 || CPP_mach==CPP_macosx64
      !||====================================================================
      !||    check_kmp_stack    ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    set_stack_values   ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE CHECK_KMP_STACK()
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
C-----------------------------------------------      
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   (KIND=KMP_SIZE_T_KIND)  NEWSIZ
      CHARACTER*256 KMP_STACKSIZE_VALUE
 
      KMP_SET = 0
 
      CALL GETENV('KMP_STACKSIZE',KMP_STACKSIZE_VALUE)
C      
         IF (KMP_STACKSIZE_VALUE(1:1) == ' ') THEN
         KMP_SET = 1
         NEWSIZ = 134217728
         CALL KMP_SET_STACKSIZE_S(NEWSIZ)
      ELSE
         KMPSTSIZ = KMP_GET_STACKSIZE()
         IF (KMPSTSIZ < 134217728)THEN
           KMP_SET = 2
         ENDIF
      ENDIF
#elif 1
#include      "implicit_f.inc"

#endif
      RETURN
      END
      !||====================================================================
      !||    get_kmp_stack   ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE GET_KMP_STACK( )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  KMPSTSIZ,NEWSIZ
C         
      KMPSTSIZ = KMP_GET_STACKSIZE()
      print*,'KMPSTSIZ=',KMPSTSIZ

#elif 1

#endif
      RETURN
      END
#elif 1
      !||====================================================================
      !||    check_kmp_stack    ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    set_stack_values   ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE CHECK_KMP_STACK( )
      RETURN
      END
C
      !||====================================================================
      !||    get_kmp_stack   ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE GET_KMP_STACK( )
      RETURN
      END
#endif

      !||====================================================================
      !||    set_stack_values   ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    radioss2           ../engine/source/engine/radioss2.F
      !||--- calls      -----------------------------------------------------
      !||    check_kmp_stack    ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE SET_STACK_VALUES( )
#include      "implicit_f.inc"
#include      "task_c.inc"
C Check & Set Stacksize value
       INTEGER Stack

       CALL SET_STACK_C(Stack)
       STACKSIZ = Stack

C Check KMP_STACKSIZE Value
       CALL  CHECK_KMP_STACK( )
      END

      !||====================================================================
      !||    write_iout   ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE WRITE_IOUT(LINE,LEN1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include  "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN1
      CHARACTER(len=LEN1)  LINE
C-----------------------------------------------
#include "lockon.inc"
      WRITE(IOUT,'(A)') LINE(1:LEN1)
#include "lockoff.inc"

      END
C-----------------------------------------------

