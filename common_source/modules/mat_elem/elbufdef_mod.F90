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
!hd|====================================================================
!hd|  ELBUFDEF_MOD                  modules/mat_elem/elbufdef_mod.F
!hd|-- called by -----------
!hd|        MAT_ELEM_MOD                  common_source/modules/mat_elem/mat_elem_mod.F
!hd|        MULTI_FVM_MOD                 common_source/modules/ale/multi_fvm_mod.F
!hd|        ALLOCBUF_AUTO                 starter/source/elements/elbuf_init/allocbuf_auto.F
!hd|        ANIOFF0                       starter/source/output/anim/anioff0.F
!hd|        ANIOFFC                       starter/source/output/anim/anioffc.F
!hd|        ANIOFFF                       starter/source/output/anim/aniofff.F
!hd|        ANIOFFS                       starter/source/output/anim/anioffs.F
!hd|        ANISKEW                       starter/source/output/anim/aniskew.F
!hd|        BINIT2                        starter/source/ale/bimat/binit2.F
!hd|        BSIGINI                       starter/source/elements/beam/bsigini.F
!hd|        BULKFAKEIGEO3                 starter/source/elements/ige3d/bulkfakeigeo3.F
!hd|        BUSERINI                      starter/source/elements/beam/buserini.F
!hd|        C3EPSINI                      starter/source/elements/sh3n/coque3n/c3epsini.F
!hd|        C3FINT_REG_INI                starter/source/elements/sh3n/coque3n/c3fint_reg_ini.F
!hd|        C3INMAS                       starter/source/elements/sh3n/coque3n/c3inmas.F
!hd|        CBAFINT_REG_INI               starter/source/elements/shell/coqueba/cbafint_reg_ini.F
!hd|        CBAINIT3                      starter/source/elements/shell/coqueba/cbainit3.F
!hd|        CBUFXFE                       starter/source/elements/xfem/cbufxfe.F
!hd|        CDKEPSINI                     starter/source/elements/sh3n/coquedk/cdkepsini.F
!hd|        CDKFINT_REG_INI               starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.F
!hd|        CDKINIT3                      starter/source/elements/sh3n/coquedk/cdkinit3.F
!hd|        CEPSINI                       starter/source/elements/shell/coque/cepsini.F
!hd|        CFAILINI                      starter/source/elements/shell/coque/cfailini.F
!hd|        CFAILINI4                     starter/source/elements/shell/coque/cfailini.F
!hd|        CFINT_REG_INI                 starter/source/elements/shell/coque/cfint_reg_ini.F
!hd|        CHK_DTTSH                     starter/source/elements/thickshell/solidec/scdtchk3.F
!hd|        CINIT3                        starter/source/elements/shell/coque/cinit3.F
!hd|        CINMAS                        starter/source/elements/shell/coque/cinmas.F
!hd|        CM27IN3                       starter/source/materials/mat/mat027/cm27in3.F
!hd|        CM35IN3                       starter/source/materials/mat/mat035/cm35in3.F
!hd|        CMAINI3                       starter/source/elements/sh3n/coquedk/cmaini3.F
!hd|        CMATINI                       starter/source/materials/mat_share/cmatini.F
!hd|        CMATINI4                      starter/source/materials/mat_share/cmatini4.F
!hd|        CMLAWI                        starter/source/elements/shell/coque/cepsini.F
!hd|        CNEPSINI                      starter/source/elements/shell/coqueba/cnepsini.F
!hd|        CNLOC_MAT104_INI              starter/source/materials/mat/mat104/cnloc_mat104_ini.F
!hd|        CNLOC_MATINI                  starter/source/materials/mat_share/cnloc_matini.F
!hd|        CORTH3                        starter/source/elements/shell/coque/corth3.F
!hd|        CORTHDIR                      starter/source/elements/shell/coque/corthdir.F
!hd|        CORTHINI                      starter/source/elements/shell/coque/corthini.F
!hd|        CSIGINI                       starter/source/elements/shell/coque/csigini.F
!hd|        CSIGINI4                      starter/source/elements/shell/coqueba/scigini4.F
!hd|        CUSERINI                      starter/source/elements/shell/coque/cuserini.F
!hd|        CUSERINI4                     starter/source/elements/shell/coqueba/cuserini4.F
!hd|        C_SEATBELTS                   starter/source/restart/ddsplit/c_seatbelts.F
!hd|        DEALLOCATE_ELBUF              starter/source/elements/elbuf_init/deallocate_buffer.F
!hd|        DEALLOCATE_ONE_ELEMENT_GROUP  starter/source/elements/elbuf_init/deallocate_one_element_group.F
!hd|        DFUNC0                        starter/source/output/anim/dfunc0.F
!hd|        DFUNCC                        starter/source/output/anim/dfuncc.F
!hd|        DFUNCF                        starter/source/output/anim/dfuncf.F
!hd|        DFUNCS                        starter/source/output/anim/dfuncs.F
!hd|        DMASANI0                      starter/source/output/anim/dmasani0.F
!hd|        DMASANIC                      starter/source/output/anim/dmasanic.F
!hd|        DMASANIF                      starter/source/output/anim/dmasanif.F
!hd|        DMASANIS                      starter/source/output/anim/dmasanis.F
!hd|        DTMAIN                        starter/source/materials/time_step/dtmain.F
!hd|        FAILINI                       starter/source/elements/solid/solide/failini.F
!hd|        GENANI1                       starter/source/output/anim/genani1.F
!hd|        IG3DINIT3                     starter/source/elements/ige3d/ig3dinit3.F
!hd|        INICRKFILL                    starter/source/elements/xfem/inicrkfill.F
!hd|        INIEBCSP                      starter/source/boundary_conditions/ebcs/iniebcsp.F
!hd|        INIEBCSP0                     starter/source/boundary_conditions/ebcs/iniebcsp0.F
!hd|        INIEBCS_DP                    starter/source/boundary_conditions/ebcs/iniebcs_dp.F
!hd|        INIEBCS_NRF_TCAR              starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
!hd|        INIFILL                       starter/source/initial_conditions/inivol/inifill.F
!hd|        INIGRAV_EOS                   starter/source/initial_conditions/inigrav/inigrav_eos.F
!hd|        INIGRAV_LOAD                  starter/source/initial_conditions/inigrav/inigrav_load.F
!hd|        INIGRAV_M37                   starter/source/initial_conditions/inigrav/inigrav_m37.F
!hd|        INIGRAV_M51                   starter/source/initial_conditions/inigrav/inigrav_m51.F
!hd|        ININTR_THKVAR                 starter/source/interfaces/interf1/inintr_thkvar.F
!hd|        INIPHASE                      starter/source/initial_conditions/inivol/iniphase.F
!hd|        INIRIG_MAT                    starter/source/elements/initia/inirig_mat.F
!hd|        INITVARS_AUTO                 starter/source/elements/elbuf_init/initvars_auto.F
!hd|        INIVOID                       starter/source/elements/initia/inivoid.F
!hd|        INIVOL_SET                    starter/source/initial_conditions/inivol/inivol_set.F
!hd|        INI_INIMAP1D                  starter/source/initial_conditions/inimap/ini_inimap1d.F
!hd|        INI_INIMAP2D                  starter/stub/ini_inimap2d.F
!hd|        INI_OUTMAX_AUTO               starter/source/elements/elbuf_init/ini_outmax_auto.F
!hd|        INI_SEATBELT                  starter/source/tools/seatbelts/ini_seatbelt.F
!hd|        LAYINI1                       starter/source/elements/shell/coqueba/layini1.F
!hd|        LAYINI_XFE                    starter/source/elements/xfem/cbufxfe.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        LSLOCAL                       starter/source/elements/xfem/lslocal.F
!hd|        M20DCOD                       starter/source/system/fsdcod.F
!hd|        M37INIT                       starter/source/materials/mat/mat037/m37init.F
!hd|        M51INIT                       starter/source/materials/mat/mat051/m51init.F
!hd|        MATINI                        starter/source/materials/mat_share/matini.F
!hd|        MULTIFLUID_GLOBAL_TDET        starter/source/multifluid/multifluid_global_tdet.F
!hd|        MULTIFLUID_INIT2              starter/source/multifluid/multifluid_init2.F
!hd|        MULTIFLUID_INIT2T             starter/source/multifluid/multifluid_init2t.F
!hd|        MULTIFLUID_INIT3              starter/source/multifluid/multifluid_init3.F
!hd|        MULTIFLUID_INIT3T             starter/source/multifluid/multifluid_init3t.F
!hd|        NLOCAL_INIT_STA               starter/source/materials/fail/nlocal_init_sta.F
!hd|        NLOC_DMG_INIT                 starter/source/materials/fail/nloc_dmg_init.F
!hd|        PARSORC                       starter/source/output/anim/parsorc.F
!hd|        PINIT3                        starter/source/elements/beam/pinit3.F
!hd|        PREINICRK3N                   starter/source/elements/xfem/preinicrk3N.F
!hd|        PREINICRK4N                   starter/source/elements/xfem/preinicrk4N.F
!hd|        Q4INIT2                       starter/source/elements/solid_2d/quad4/q4init2.F
!hd|        QINIT2                        starter/source/elements/solid_2d/quad/qinit2.F
!hd|        RINIT3                        starter/source/elements/spring/rinit3.F
!hd|        S10DERI3                      starter/source/elements/solid/solide10/s10deri3.F
!hd|        S10INIT3                      starter/source/elements/solid/solide10/s10init3.F
!hd|        S10JACI3                      starter/source/elements/solid/solide10/s10jaci3.F
!hd|        S16INIT3                      starter/source/elements/thickshell/solide16/s16init3.F
!hd|        S20INIT3                      starter/source/elements/solid/solide20/s20init3.F
!hd|        S4INIT3                       starter/source/elements/solid/solide4/s4init3.F
!hd|        S6CINIT3                      starter/source/elements/thickshell/solide6c/s6cinit3.F
!hd|        S8CINIT3                      starter/source/elements/thickshell/solide8c/s8cinit3.F
!hd|        S8E_PIJ                       starter/source/elements/solid/solide8z/s8zderi3.F
!hd|        S8ZINIT3                      starter/source/elements/solid/solide8z/s8zinit3.F
!hd|        SCALEINI                      starter/source/elements/initia/scaleini.F
!hd|        SCINIT3                       starter/source/elements/thickshell/solidec/scinit3.F
!hd|        SETELOFF                      starter/source/constraints/general/rbody/hm_read_rbody.F
!hd|        SIGIN3B                       starter/source/elements/solid/solid8p/sigin3b.F
!hd|        SINI43                        starter/source/elements/solid/sconnect/sini43.F
!hd|        SINIT3                        starter/source/elements/solid/solide/sinit3.F
!hd|        SMS_AUTO_DT                   starter/source/ams/sms_auto_dt.F
!hd|        SPINIT3                       starter/source/elements/sph/spinit3.F
!hd|        SUINIT3                       starter/source/elements/elbuf_init/suinit3.F
!hd|        TENSOR0                       starter/source/output/anim/tensor0.F
!hd|        TENSORC                       starter/source/output/anim/tensorc.F
!hd|        TENSORS                       starter/source/output/anim/tensors.F
!hd|        THICKVAR                      starter/source/interfaces/interf1/inintr_thkvar.F
!hd|        THICK_ILEV                    starter/source/elements/xfem/thick_ilev.F
!hd|        TINIT3                        starter/source/elements/truss/tinit3.F
!hd|        W_ELBUF_STR                   starter/source/restart/ddsplit/w_elbuf_str.F
!hd|        XINIT3                        starter/source/elements/xelem/xinit3.F
!hd|        ZEROVARS_AUTO                 starter/source/elements/elbuf_init/zerovars_auto.F
!hd|        A22CONV3                      engine/source/ale/alefvm/cut_cells/a22conv3.F
!hd|        ACONVE                        engine/source/ale/aconve.F
!hd|        ADMDIV                        engine/source/model/remesh/admdiv.F
!hd|        ADMERR                        engine/source/model/remesh/admerr.F
!hd|        ADMGVID                       engine/source/model/remesh/admgvid.F
!hd|        ADMINI                        engine/source/model/remesh/admini.F
!hd|        ADMMAP3                       engine/source/model/remesh/admmap3.F
!hd|        ADMMAP4                       engine/source/model/remesh/admmap4.F
!hd|        ADMREGUL                      engine/source/model/remesh/admregul.F
!hd|        ADMTHKE                       engine/source/model/remesh/admthke.F
!hd|        AETURB                        engine/source/ale/turbulence/aeturb.F
!hd|        AFLUX0                        engine/source/ale/aflux0.F
!hd|        AFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
!hd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
!hd|        AGAUGE                        engine/source/ale/agauge.F
!hd|        AGRAD0                        engine/source/ale/agrad0.F
!hd|        AIRBAGB1                      engine/source/airbag/airbagb1.F
!hd|        AKTURB                        engine/source/ale/turbulence/akturb.F
!hd|        ALE51_ANTIDIFF3_INT22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
!hd|        ALE51_FINISH                  engine/source/ale/ale51/ale51_finish.F
!hd|        ALE51_GRADIENT_RECONSTRUCTION engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!hd|        ALE51_INIT                    engine/source/ale/ale51/ale51_init.F
!hd|        ALE51_UPWIND3_INT22           engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
!hd|        ALEFLOW                       engine/source/ale/porous/aleflow.F
!hd|        ALEFVM_MAIN                   engine/source/ale/alefvm/alefvm_main.F
!hd|        ALEFVM_STRESS_INT22           engine/source/ale/alefvm/alefvm_stress_int22.F
!hd|        ALETHE                        engine/source/ale/alethe.F
!hd|        ALEW6                         engine/source/ale/grid/alew6.F
!hd|        ALEWDX                        engine/source/ale/grid/alewdx.F
!hd|        ALLOCBUF_AUTO                 engine/source/elements/elbuf/allocbuf_auto.F
!hd|        ALLOC_ELBUF_IMP               engine/source/elements/elbuf/alloc_elbuf_imp.F
!hd|        ANIMIG3D                      engine/source/output/anim/generate/animig3d.F
!hd|        ANIMX                         engine/source/output/anim/generate/animx.F
!hd|        ANIM_NODAL_P_ELEMS            engine/source/output/anim/generate/anim_nodal_p_elems.F
!hd|        ANIOFF0                       engine/source/output/anim/generate/anioff0.F
!hd|        ANIOFFC                       engine/source/output/anim/generate/anioffc.F
!hd|        ANIOFFC_CRK                   engine/source/output/anim/generate/anioffc_crk.F
!hd|        ANIOFFC_PLY                   engine/source/output/anim/generate/anioffc_ply.F
!hd|        ANIOFFF                       engine/source/output/anim/generate/aniofff.F
!hd|        ANIOFFS                       engine/source/output/anim/generate/anioff6.F
!hd|        ANISKEW                       engine/source/output/anim/generate/aniskew.F
!hd|        AREZON                        engine/source/ale/arezon.F
!hd|        ATHERM                        engine/source/ale/atherm.F
!hd|        BFORC2                        engine/source/ale/bimat/bforc2.F
!hd|        C3COORK3                      engine/source/elements/sh3n/coque3n/c3coork3.F
!hd|        C3EVEC3                       engine/source/elements/sh3n/coque3n/c3evec3.F
!hd|        C3FINT_REG                    engine/source/elements/sh3n/coque3n/c3fint_reg.F
!hd|        C3KE3                         engine/source/elements/sh3n/coque3n/c3ke3.F
!hd|        CBACOOR                       engine/source/elements/shell/coqueba/cbacoor.F
!hd|        CBACOORK                      engine/source/elements/shell/coqueba/cbacoork.F
!hd|        CBACOORT                      engine/source/elements/shell/coqueba/cbacoor.F
!hd|        CBAFINT_REG                   engine/source/elements/shell/coqueba/cbafint_reg.F
!hd|        CBAKE3                        engine/source/elements/shell/coqueba/cbake3.F
!hd|        CBAL58WARP                    engine/source/elements/shell/coqueba/cbawarpoff.F
!hd|        CBAPINCHPROJ                  engine/source/elements/shell/coqueba/cbapinchproj.F
!hd|        CDK6COOR3                     engine/source/elements/sh3n/coquedk6/cdk6coor3.F
!hd|        CDK6FINT_REG                  engine/source/elements/sh3n/coquedk6/cdk6fint_reg.F
!hd|        CDKCOOR3                      engine/source/elements/sh3n/coquedk/cdkcoor3.F
!hd|        CDKFINT_REG                   engine/source/elements/sh3n/coquedk/cdkfint_reg.F
!hd|        CEVEC3                        engine/source/elements/shell/coque/cevec3.F
!hd|        CFINT_REG                     engine/source/elements/shell/coque/cfint_reg.F
!hd|        CGSHELL3                      engine/source/implicit/cgshell.F
!hd|        CGSHELL4                      engine/source/implicit/cgshell.F
!hd|        CHECK_ALE_COMM                engine/source/ale/check_ale_comm.F
!hd|        CHKSTFN3N                     engine/source/interfaces/interf/chkstfn3.F
!hd|        CLUSTERF                      engine/source/output/cluster/clusterf.F
!hd|        CMAIN3PINCH                   engine/source/elements/shell/coqueba/cmain3pinch.F
!hd|        CMATC3                        engine/source/elements/shell/coqueba/cmatc3.F
!hd|        CNCOEFORT                     engine/source/elements/sh3n/coquedk/cncoef3.F
!hd|        CNVEC3                        engine/source/elements/shell/coque/cnvec3.F
!hd|        CONVECOFF                     engine/source/constraints/thermic/convecoff.F
!hd|        COPY_ELBUF                    engine/source/elements/elbuf/copy_elbuf.F
!hd|        COPY_ELBUF_1                  engine/source/elements/elbuf/copy_elbuf_1.F
!hd|        CORTDIR3                      engine/source/elements/shell/coque/cortdir3.F
!hd|        COUNT_ARSZ_CT                 engine/source/output/sty/outp_c_t.F
!hd|        COUNT_ARSZ_ST                 engine/source/output/sty/outp_s_t.F
!hd|        CP_IMPBUF                     engine/source/implicit/produt_v.F
!hd|        CRKLAYER4N_ADV                engine/source/elements/xfem/crklayer4n_adv.F
!hd|        CRKLAYER4N_INI                engine/source/elements/xfem/crklayer4n_ini.F
!hd|        CRKOFFC                       engine/source/elements/xfem/precrklay.F
!hd|        CRKOFFTG                      engine/source/elements/xfem/precrklay.F
!hd|        CUTFUNCE                      engine/source/tools/sect/cutfunce.F
!hd|        CZCOORK3                      engine/source/elements/shell/coquez/czcoork3.F
!hd|        CZCORC1                       engine/source/elements/shell/coquez/czcorc.F
!hd|        CZCORCHT                      engine/source/elements/shell/coquez/czcorc.F
!hd|        CZCORCT                       engine/source/elements/shell/coquez/czcorc.F
!hd|        CZKE3                         engine/source/elements/shell/coquez/czke3.F
!hd|        CZKEL3                        engine/source/elements/shell/coquez/czkel3.F
!hd|        C_TF_NE                       engine/source/output/sty/c_tf_ne.F
!hd|        DELTAX22                      engine/source/interfaces/int22/deltax22.F
!hd|        DESACTI                       engine/source/elements/desacti.F
!hd|        DFUNC0                        engine/source/output/anim/generate/dfunc0.F
!hd|        DFUNCC                        engine/source/output/anim/generate/dfuncc.F
!hd|        DFUNCC_CRK                    engine/source/output/anim/generate/dfuncc_crk.F
!hd|        DFUNCC_PLY                    engine/source/output/anim/generate/dfuncc_ply.F
!hd|        DFUNCF                        engine/source/output/anim/generate/dfuncf.F
!hd|        DFUNGPS1                      engine/source/output/anim/generate/dfuncf.F
!hd|        DFUNGPS2                      engine/source/output/anim/generate/dfuncf.F
!hd|        DIM_ELEMAX                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_ELEMS1                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_ELEMS2                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_ELEMS3                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_ELEMS4                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_ELEMSP                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_FR_K                      engine/source/mpi/implicit/imp_fri.F
!hd|        DIM_GLOB_K                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_KINMAX                    engine/source/implicit/ind_glob_k.F
!hd|        DIM_TSHEDG                    engine/source/elements/thickshell/solidec/dim_tshedg.F
!hd|        DMASANI0                      engine/source/output/anim/generate/dmasani0.F
!hd|        DMASANIC                      engine/source/output/anim/generate/dmasanic.F
!hd|        DMASANIF                      engine/source/output/anim/generate/dmasanif.F
!hd|        DMASANIS                      engine/source/output/anim/generate/dmasani6.F
!hd|        DYNAIN_C_STRAG                engine/source/output/dynain/dynain_c_strag.F
!hd|        DYNAIN_C_STRSG                engine/source/output/dynain/dynain_c_strsg.F
!hd|        DYNAIN_SHEL_MP                engine/source/output/dynain/dynain_shel_mp.F
!hd|        DYNAIN_SHEL_SPMD              engine/source/output/dynain/dynain_shel_spmd.F
!hd|        DYNAIN_SIZE_C                 engine/source/output/dynain/dynain_size.F
!hd|        EBCS0                         engine/source/boundary_conditions/ebcs/ebcs0.F
!hd|        EBCS10                        engine/source/boundary_conditions/ebcs/ebcs10.F
!hd|        EBCS_MAIN                     engine/source/boundary_conditions/ebcs/ebcs_main.F
!hd|        EFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
!hd|        EIG                           engine/stub/eig.F
!hd|        EIG1                          engine/stub/eig1.F
!hd|        EIGCOND                       engine/stub/eigcond.F
!hd|        EIGOFF                        engine/source/output/anim/generate/eigoff.F
!hd|        EIGP                          engine/stub/eigp.F
!hd|        ELOFF                         engine/source/elements/eloff.F
!hd|        ENRICHC_INI                   engine/source/elements/xfem/enrichc_ini.F
!hd|        ENRICHTG_INI                  engine/source/elements/xfem/enrichtg_ini.F
!hd|        ERR_THK                       engine/source/elements/shell/err_thk.F
!hd|        FAIL_GENE1_S                  engine/source/materials/fail/gene1/fail_gene1_s.F
!hd|        FORINTS                       engine/source/elements/forints.F
!hd|        FVBAG0                        engine/source/airbag/fvbag0.F
!hd|        FVBAG1                        engine/source/airbag/fvbag1.F
!hd|        FVBAG2                        engine/source/airbag/fvbag2.F
!hd|        FVVENT0                       engine/source/airbag/fvvent0.F
!hd|        FV_UP_SWITCH                  engine/source/airbag/fv_up_switch.F
!hd|        FXBODFP1                      engine/source/constraints/fxbody/fxbodfp.F
!hd|        FXBSGMAJ                      engine/source/constraints/fxbody/fxbsgmaj.F
!hd|        FXBYFOR                       engine/source/constraints/fxbody/fxbyfor.F
!hd|        FXBYPID                       engine/source/constraints/fxbody/fxbypid.F
!hd|        GENANI                        engine/source/output/anim/generate/genani.F
!hd|        GENDYNAIN                     engine/source/output/dynain/gendynain.F
!hd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
!hd|        GENOUTP                       engine/source/output/sty/genoutp.F
!hd|        GET_NODAL_IPART               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
!hd|        GET_Q4LSYS                    engine/source/output/sta/sta_c_get_q4lsys.F
!hd|        GET_T3LSYS                    engine/source/output/sta/sta_c_get_t3lsys.F
!hd|        GPSSTRAIN_SKIN                engine/source/output/anim/generate/tensgpstrain.F
!hd|        GPSTRA_SOLID                  engine/source/output/outmaxsubr.F
!hd|        GPS_SOLID                     engine/source/output/outmaxsubr.F
!hd|        H3D_FLD_STRAIN                engine/source/output/h3d/h3d_results/h3d_fld_strain.F
!hd|        H3D_FLD_TSH                   engine/source/output/h3d/h3d_results/h3d_fld_tsh.F
!hd|        H3D_NODAL_SCALAR              engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
!hd|        H3D_NODAL_TENSOR              engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
!hd|        H3D_NODAL_VECTOR              engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
!hd|        H3D_ONED_OFF                  engine/source/output/h3d/spmd/spmd_h3d_oned_off.F
!hd|        H3D_ONED_SCALAR               engine/source/output/h3d/h3d_results/h3d_oned_scalar.F
!hd|        H3D_ONED_TENSOR               engine/source/output/h3d/h3d_results/h3d_oned_tensor.F
!hd|        H3D_ONED_VECTOR               engine/source/output/h3d/h3d_results/h3d_oned_vector.F
!hd|        H3D_QUAD_OFF                  engine/source/output/h3d/spmd/spmd_h3d_quad_off.F
!hd|        H3D_QUAD_SCALAR               engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
!hd|        H3D_QUAD_TENSOR               engine/source/output/h3d/h3d_results/h3d_quad_tensor.F
!hd|        H3D_QUAD_VECTOR               engine/source/output/h3d/h3d_results/h3d_quad_vector.F
!hd|        H3D_SHELL_OFF                 engine/source/output/h3d/spmd/spmd_h3d_shell_off.F
!hd|        H3D_SHELL_SCALAR              engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
!hd|        H3D_SHELL_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
!hd|        H3D_SHELL_TENSOR              engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
!hd|        H3D_SHELL_VECTOR              engine/source/output/h3d/h3d_results/h3d_shell_vector.F
!hd|        H3D_SHELL_VECTOR_1            engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
!hd|        H3D_SKIN_IXSKIN               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
!hd|        H3D_SKIN_OFF                  engine/source/output/h3d/h3d_results/h3d_skin_off.F
!hd|        H3D_SKIN_TENSOR               engine/source/output/h3d/h3d_results/h3d_skin_tensor.F
!hd|        H3D_SOLID_OFF                 engine/source/output/h3d/spmd/spmd_h3d_solid_off.F
!hd|        H3D_SOLID_SCALAR              engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
!hd|        H3D_SOLID_TENSOR              engine/source/output/h3d/h3d_results/h3d_solid_tensor.F
!hd|        H3D_SOLID_TENSOR_1            engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
!hd|        H3D_SOLID_VECTOR              engine/source/output/h3d/h3d_results/h3d_solid_vector.F
!hd|        H3D_SOL_SKIN_IXSKIN           engine/source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
!hd|        H3D_SOL_SKIN_TENSOR           engine/source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
!hd|        H3D_SPH_OFF                   engine/source/output/h3d/spmd/spmd_h3d_sph_off.F
!hd|        H3D_SPH_SCALAR                engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
!hd|        H3D_SPH_TENSOR                engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
!hd|        H3D_VELVECC22                 engine/source/output/h3d/h3d_results/h3d_velvecc22.F
!hd|        H3D_VELVECZ22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.F
!hd|        HIST2                         engine/source/output/th/hist2.F
!hd|        I18FOR3                       engine/source/interfaces/int18/i18for3.F
!hd|        I22FOR3                       engine/source/interfaces/int22/i22for3.F
!hd|        I22MAINF                      engine/source/interfaces/int22/i22mainf.F
!hd|        I22SUBVOL                     engine/source/interfaces/int22/i22subvol.F
!hd|        I7MAINF                       engine/source/interfaces/int07/i7mainf.F
!hd|        I9GRD2                        engine/source/interfaces/int09/i9grd2.F
!hd|        I9GRD3                        engine/source/interfaces/int09/i9grd3.F
!hd|        I9WAL2                        engine/source/interfaces/int09/i9wal2.F
!hd|        I9WAL3                        engine/source/interfaces/int09/i9wal3.F
!hd|        I9WALE                        engine/source/interfaces/int09/i9wale.F
!hd|        IMP_BUCK                      engine/source/implicit/imp_buck.F
!hd|        IMP_CHKM                      engine/source/implicit/imp_solv.F
!hd|        IMP_GLOB_K                    engine/source/implicit/imp_glob_k.F
!hd|        IMP_GLOB_K0                   engine/source/implicit/imp_glob_k.F
!hd|        IMP_GLOB_KHP                  engine/source/implicit/imp_glob_k.F
!hd|        IMP_INIT                      engine/source/implicit/imp_init.F
!hd|        IMP_K_EIG                     engine/stub/imp_k_eig.F
!hd|        IMP_SOLV                      engine/source/implicit/imp_solv.F
!hd|        IMP_SOL_INIT                  engine/source/implicit/imp_sol_init.F
!hd|        IND_FR_K                      engine/source/mpi/implicit/imp_fri.F
!hd|        IND_GLOB_K                    engine/source/implicit/ind_glob_k.F
!hd|        IND_TSHEDG                    engine/source/elements/thickshell/solidec/ind_tshedg.F
!hd|        INIT_TH                       engine/source/output/th/init_th.F
!hd|        INIT_TH0                      engine/source/output/th/init_th0.F
!hd|        INIXFEM                       engine/source/elements/xfem/inixfem.F
!hd|        INI_FR_K                      engine/source/mpi/implicit/imp_fri.F
!hd|        INI_TMAX                      engine/source/output/ini_outmax.F
!hd|        INT18_LAW151_INIT             engine/source/interfaces/int18/int18_law151_init.F
!hd|        INT18_LAW151_UPDATE           engine/source/interfaces/int18/int18_law151_update.F
!hd|        INTAL3                        engine/source/ale/inter/intal3.F
!hd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
!hd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
!hd|        JOINT_BLOCK_STIFFNESS         engine/source/elements/joint/joint_block_stiffness.F
!hd|        JOINT_ELEM_TIMESTEP           engine/source/elements/joint/joint_elem_timestep.F
!hd|        KINE_SEATBELT_FORCE           engine/source/tools/seatbelts/kine_seatbelt_force.F
!hd|        KINE_SEATBELT_VEL             engine/source/tools/seatbelts/kine_seatbelt_vel.F
!hd|        KTBUF_INI                     engine/source/implicit/imp_init.F
!hd|        LASER1                        engine/source/loads/laser/laser1.F
!hd|        LASER2                        engine/source/loads/laser/laser2.F
!hd|        LASER3                        engine/source/loads/laser/laser2.F
!hd|        LAYINI                        engine/source/elements/shell/coque/layini.F
!hd|        LECH3D                        engine/source/output/h3d/h3d_build_fortran/lech3d.F
!hd|        M11LAW                        engine/source/materials/mat/mat011/m11law.F
!hd|        M11VS2                        engine/source/materials/mat/mat011/m11vs2.F
!hd|        M11VS3                        engine/source/materials/mat/mat011/m11vs3.F
!hd|        M1LAW8                        engine/source/materials/mat/mat001/m1law8.F
!hd|        M24LAW                        engine/source/materials/mat/mat024/m24law.F
!hd|        M2LAW8                        engine/source/materials/mat/mat002/m2law8.F
!hd|        M2LAWPI                       engine/source/materials/mat/mat002/m2lawpi.F
!hd|        M3LAW8                        engine/source/materials/mat/mat003/m3law8.F
!hd|        M51VOIS2                      engine/source/materials/mat/mat051/m51vois2.F
!hd|        M51VOIS3                      engine/source/materials/mat/mat051/m51vois3.F
!hd|        MATERIAL_FLOW                 engine/source/tools/seatbelts/material_flow.F
!hd|        MDAMA24                       engine/source/elements/solid/solidez/mdama24.F
!hd|        MEOS8                         engine/source/materials/mat_share/meos8.F
!hd|        MONVOL0                       engine/source/airbag/monvol0.F
!hd|        MULAWGLC                      engine/source/materials/mat_share/mulawglc.F
!hd|        MULAWGLCPINCH                 engine/source/elements/shell/coqueba/mulawglcpinch.F
!hd|        MULAW_IB                      engine/source/elements/beam/mulaw_ib.F
!hd|        MULTI_BUF2VAR                 engine/source/multifluid/multi_buf2var.F
!hd|        MULTI_COMPUTEVOLUME           engine/source/multifluid/multi_computevolume.F
!hd|        MULTI_COMPUTE_DT              engine/source/multifluid/multi_compute_dt.F
!hd|        MULTI_EVOLVE_GLOBAL           engine/source/multifluid/multi_evolve_global.F
!hd|        MULTI_EVOLVE_PARTIAL          engine/source/multifluid/multi_evolve_partial.F
!hd|        MULTI_FLUXES_COMPUTATION      engine/source/multifluid/multi_fluxes_computation.F
!hd|        MULTI_FVM2FEM                 engine/source/multifluid/multi_fvm2fem.F
!hd|        MULTI_GLOBALIZE               engine/source/multifluid/multi_globalize.F
!hd|        MULTI_I18_FORCE_POFF          engine/source/interfaces/int18/multi_i18_force_poff.F
!hd|        MULTI_MUSCL_GRADIENTS         engine/source/multifluid/multi_muscl_gradients.F
!hd|        MULTI_PRESSURE_EQUILIBRIUM    engine/source/multifluid/multi_pressure_equilibrium.F
!hd|        MULTI_TIMEEVOLUTION           engine/source/multifluid/multi_timeevolution.F
!hd|        MULTI_UPDATE_GLOBAL           engine/source/multifluid/multi_update_global.F
!hd|        MULTI_UPDATE_PARTIAL          engine/source/multifluid/multi_update_partial.F
!hd|        MULTI_VAR2BUF                 engine/source/multifluid/multi_var2buf.F
!hd|        NODALD                        engine/source/output/anim/generate/nodald.F
!hd|        NODALDT                       engine/source/output/anim/generate/nodaldt.F
!hd|        NODALP                        engine/source/output/anim/generate/nodalp.F
!hd|        NODALSSP                      engine/source/output/anim/generate/nodalssp.F
!hd|        NODALT                        engine/source/output/anim/generate/nodalt.F
!hd|        NODALVFRAC                    engine/source/output/anim/generate/nodalvfrac.F
!hd|        NODALVOL                      engine/source/output/anim/generate/nodalvol.F
!hd|        NODALZVOL                     engine/source/output/anim/generate/nodalzvol.F
!hd|        NODAL_SCHLIEREN               engine/source/output/anim/generate/nodal_schlieren.F
!hd|        NOISE                         engine/source/general_controls/computation/noise.F
!hd|        OUTPUT_DIV_U                  engine/source/output/anim/generate/output_div_u.F
!hd|        OUTPUT_SCHLIEREN              engine/source/output/anim/generate/output_schlieren.F
!hd|        OUTP_ARSZ_CT                  engine/source/mpi/interfaces/spmd_outp.F
!hd|        OUTP_ARSZ_ST                  engine/source/mpi/interfaces/spmd_outp.F
!hd|        OUTP_C_S                      engine/source/output/sty/outp_c_s.F
!hd|        OUTP_C_T                      engine/source/output/sty/outp_c_t.F
!hd|        OUTP_C_TF                     engine/source/output/sty/outp_c_t.F
!hd|        OUTP_R_S                      engine/source/output/sty/outp_r_s.F
!hd|        OUTP_R_T                      engine/source/output/sty/outp_r_t.F
!hd|        OUTP_SP_S                     engine/source/output/sty/outp_sp_s.F
!hd|        OUTP_SP_T                     engine/source/output/sty/outp_sp_t.F
!hd|        OUTP_SP_TT                    engine/source/output/sty/outp_sp_t.F
!hd|        OUTP_S_S                      engine/source/output/sty/outp_s_s.F
!hd|        OUTP_S_T                      engine/source/output/sty/outp_s_t.F
!hd|        OUTP_S_TT                     engine/source/output/sty/outp_s_t.F
!hd|        PARSORC                       engine/source/output/anim/generate/parsorc.F
!hd|        PARSORF                       engine/source/output/anim/generate/parsorf.F
!hd|        PKE3                          engine/source/elements/beam/pke3.F
!hd|        PNOISE                        engine/source/general_controls/computation/pnoise.F
!hd|        PORFOR5                       engine/source/airbag/porfor5.F
!hd|        PORFORM5                      engine/source/airbag/porfor5.F
!hd|        PRELECFLOW                    engine/source/elements/solid/solide/prelecflow.F
!hd|        PROJECIG3D                    engine/source/elements/ige3d/projecig3d.F
!hd|        Q4KE2                         engine/source/elements/solid_2d/quad4/q4ke2.F
!hd|        R12KE3                        engine/source/elements/spring/r12ke3.F
!hd|        R13KE3                        engine/source/elements/spring/r13ke3.F
!hd|        R23FORC3                      engine/source/elements/spring/r23forc3.F
!hd|        R23LAW108                     engine/source/elements/spring/r23law108.F
!hd|        R23LAW113                     engine/source/elements/spring/r23law113.F
!hd|        R23LAW114                     engine/source/elements/spring/r23law114.F
!hd|        R4KE3                         engine/source/elements/spring/r4ke3.F
!hd|        R8KE3                         engine/source/elements/spring/r8ke3.F
!hd|        RADIATOFF                     engine/source/constraints/thermic/radiatoff.F
!hd|        RBYONF                        engine/source/constraints/general/rbody/rbyonf.F
!hd|        RBYPID                        engine/source/constraints/general/rbody/rbypid.F
!hd|        RBYSENS                       engine/source/constraints/general/rbody/rbyonf.F
!hd|        RESOL_INIT                    engine/source/engine/resol_init.F
!hd|        RFORC3                        engine/source/elements/spring/rforc3.F
!hd|        RGWAL1                        engine/source/ale/grid/rgwal1.F
!hd|        RGWAT2                        engine/source/interfaces/int09/rgwat2.F
!hd|        RGWAT3                        engine/source/interfaces/int09/rgwat3.F
!hd|        RGWATH                        engine/source/interfaces/int09/rgwath.F
!hd|        RUSER32KE3                    engine/source/elements/spring/ruser32ke3.F
!hd|        S10DERIT3                     engine/source/elements/solid/solide10/s10derit3.F
!hd|        S10DERITO3                    engine/source/elements/solid/solide10/s10derito3.F
!hd|        S10KE3                        engine/source/elements/solid/solide10/s10ke3.F
!hd|        S10UPD11T12                   engine/source/elements/solid/solide10/s10upd11t12.F
!hd|        S10VOLNODT3                   engine/source/elements/solid/solide4_sfem/s10volnodt3.F
!hd|        S16SIGP3                      engine/source/elements/thickshell/solide16/s16sigp3.F
!hd|        S20KE3                        engine/source/elements/solid/solide20/s20ke3.F
!hd|        S4ALESFEM                     engine/source/elements/solid/solide4_sfem/s4alesfem.F
!hd|        S4KE3                         engine/source/elements/solid/solide4/s4ke3.F
!hd|        S4LAGSFEM                     engine/source/elements/solid/solide4_sfem/s4lagsfem.F
!hd|        S6CFINT_REG                   engine/source/elements/thickshell/solide6c/s6cfint_reg.F
!hd|        S6CKE3                        engine/source/elements/thickshell/solide6c/s6cke3.F
!hd|        S8CFINT_REG                   engine/source/elements/thickshell/solide8c/s8cfint_reg.F
!hd|        S8CKE3                        engine/source/elements/thickshell/solide8c/s8cke3.F
!hd|        S8E_SIGP                      engine/source/elements/solid/solide8e/s8e_sig.F
!hd|        S8FINT3                       engine/source/elements/solid/solide8/s8fint3.F
!hd|        S8FUPD11T12                   engine/source/elements/solid/solide8e/s8fupd11t12.F
!hd|        S8SKE3                        engine/source/elements/solid/solide8s/s8ske3.F
!hd|        S8ZKE3                        engine/source/elements/solid/solide8z/s8zke3.F
!hd|        S8_IS17JAC_I                  engine/source/elements/solid/solide8e/s8_is17jac_i.F
!hd|        SCFINT_REG                    engine/source/elements/thickshell/solidec/scfint_reg.F
!hd|        SCHLIEREN_BUFFER_GATHERING    engine/source/output/anim/generate/schlieren_buffer_gathering.F
!hd|        SCONNECT_OFF                  engine/source/elements/solid/sconnect/sconnect_off.F
!hd|        SEATBELT_REDUCTION_FACTOR     engine/source/tools/seatbelts/seatbelt_reduction_factor.F
!hd|        SEGGETV                       engine/source/interfaces/interf/seggetv.F
!hd|        SFINT_REG                     engine/source/elements/solid/solide/sfint_reg.F
!hd|        SHELL_LOCAL_FRAME             engine/source/output/dynain/shell_rota.F
!hd|        SHELL_ROTA                    engine/source/output/dynain/shell_rota.F
!hd|        SIGEPS02G                     engine/source/materials/mat/mat002/sigeps02g.F
!hd|        SIGEPS104                     engine/source/materials/mat/mat104/sigeps104.F
!hd|        SIGEPS104C                    engine/source/materials/mat/mat104/sigeps104c.F
!hd|        SIGEPS105                     engine/source/materials/mat/mat105/sigeps105.F
!hd|        SIGEPS107                     engine/source/materials/mat/mat107/sigeps107.F
!hd|        SIGEPS107C                    engine/source/materials/mat/mat107/sigeps107c.F
!hd|        SIGEPS112                     engine/source/materials/mat/mat112/sigeps112.F
!hd|        SIGEPS112C                    engine/source/materials/mat/mat112/sigeps112c.F
!hd|        SIGEPS121                     engine/source/materials/mat/mat121/sigeps121.F
!hd|        SIGEPS121C                    engine/source/materials/mat/mat121/sigeps121c.F
!hd|        SIGEPS122                     engine/source/materials/mat/mat122/sigeps122.F
!hd|        SIGEPS122C                    engine/source/materials/mat/mat122/sigeps122c.F
!hd|        SIGEPS22G                     engine/source/materials/mat/mat022/sigeps22g.F
!hd|        SIGEPS37_SINGLE_CELL          engine/source/interfaces/int22/sigeps37_single_cell.F
!hd|        SIGEPS51                      engine/source/materials/mat/mat051/sigeps51.F
!hd|        SIGEPS97                      engine/source/materials/mat/mat097/sigeps97.F
!hd|        SIGROTA                       engine/source/output/anim/generate/sigrota.F
!hd|        SIGROTA_XFE                   engine/source/output/anim/generate/sigrota_xfe.F
!hd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
!hd|        SMS_BUILD_MAT_2               engine/source/ams/sms_build_mat_2.F
!hd|        SOLTOSPHA                     engine/source/elements/sph/soltospha.F
!hd|        SOLTOSPHF                     engine/source/elements/sph/soltosph.F
!hd|        SOLTOSPHP                     engine/source/elements/sph/soltosph.F
!hd|        SOLTOSPH_ON1                  engine/source/elements/sph/soltosph_on1.F
!hd|        SOLTOSPH_ON12                 engine/source/elements/sph/soltosph_on1.F
!hd|        SOLTOSPH_ON2                  engine/source/elements/sph/soltosph_on2.F
!hd|        SPBRM_PRE                     engine/source/implicit/imp_solv.F
!hd|        SPECHAN                       engine/source/elements/sph/spechan.F
!hd|        SPGAUGE                       engine/source/elements/sph/spgauge.F
!hd|        SPHPREP                       engine/source/elements/sph/sphprep.F
!hd|        SPLISSV                       engine/source/elements/sph/splissv.F
!hd|        SPMD_FVB_SWITCH               engine/source/mpi/airbags/spmd_fvb_switch.F
!hd|        SPMD_L11VOIS                  engine/source/mpi/fluid/spmd_cfd.F
!hd|        SPMD_L51VOIS                  engine/source/mpi/fluid/spmd_cfd.F
!hd|        SPONFPRS                      engine/source/elements/sph/sponfprs.F
!hd|        SPONOF1                       engine/source/elements/sph/sponof1.F
!hd|        SPONOF2                       engine/source/elements/sph/sponof2.F
!hd|        SPWFVIS                       engine/source/elements/sph/spwfvis.F
!hd|        STAT_BEAM_MP                  engine/source/output/sta/stat_beam_mp.F
!hd|        STAT_BEAM_SPMD                engine/source/output/sta/stat_beam_spmd.F
!hd|        STAT_BRICK_MP                 engine/source/output/sta/stat_brick_mp.F
!hd|        STAT_BRICK_SPMD               engine/source/output/sta/stat_brick_spmd.F
!hd|        STAT_C_AUXF                   engine/source/output/sta/stat_c_auxf.F
!hd|        STAT_C_EPSPF                  engine/source/output/sta/stat_c_epspf.F
!hd|        STAT_C_OFF                    engine/source/output/sta/stat_c_off.F
!hd|        STAT_C_ORTH_LOC               engine/source/output/sta/stat_c_orth_loc.F
!hd|        STAT_C_STRAF                  engine/source/output/sta/stat_c_straf.F
!hd|        STAT_C_STRAFG                 engine/source/output/sta/stat_c_strafg.F
!hd|        STAT_C_STRSF                  engine/source/output/sta/stat_c_strsf.F
!hd|        STAT_C_STRSFG                 engine/source/output/sta/stat_c_strsfg.F
!hd|        STAT_C_THK                    engine/source/output/sta/stat_c_thk.F
!hd|        STAT_INIMAP1D_FILE_SPMD       engine/source/output/sta/stat_inimap1d_file_spmd.F
!hd|        STAT_INIMAP1D_SPMD            engine/source/output/sta/stat_inimap1d_spmd.F
!hd|        STAT_INIMAP2D_FILE_SPMD       engine/source/output/sta/stat_inimap2d_file_spmd.F
!hd|        STAT_INIMAP2D_SPMD            engine/source/output/sta/stat_inimap2d_spmd.F
!hd|        STAT_P_AUX                    engine/source/output/sta/stat_p_aux.F
!hd|        STAT_P_FULL                   engine/source/output/sta/stat_p_full.F
!hd|        STAT_QUAD_MP                  engine/source/output/sta/stat_quad_mp.F
!hd|        STAT_QUAD_SPMD                engine/source/output/sta/stat_quad_spmd.F
!hd|        STAT_R_FULL                   engine/source/output/sta/stat_r_full.F
!hd|        STAT_SHEL_MP                  engine/source/output/sta/stat_shel_mp.F
!hd|        STAT_SHEL_SPMD                engine/source/output/sta/stat_shel_spmd.F
!hd|        STAT_SIZE_C                   engine/source/output/sta/stat_size.F
!hd|        STAT_SPRING_MP                engine/source/output/sta/stat_spring_mp.F
!hd|        STAT_SPRING_SPMD              engine/source/output/sta/stat_spring_spmd.F
!hd|        STAT_S_AUXF                   engine/source/output/sta/stat_s_auxf.F
!hd|        STAT_S_EREF                   engine/source/output/sta/stat_s_eref.F
!hd|        STAT_S_ORTHO                  engine/source/output/sta/stat_s_ortho.F
!hd|        STAT_S_STRAF                  engine/source/output/sta/stat_s_straf.F
!hd|        STAT_S_STRSF                  engine/source/output/sta/stat_s_strsf.F
!hd|        STAT_TRUSS_MP                 engine/source/output/sta/stat_truss_mp.F
!hd|        STAT_TRUSS_SPMD               engine/source/output/sta/stat_truss_spmd.F
!hd|        STAT_T_FULL                   engine/source/output/sta/stat_t_full.F
!hd|        STRN_TENSCOR3                 engine/source/output/h3d/h3d_results/h3d_strn_tenscor3.F
!hd|        STRS_TENSCOR3                 engine/source/output/h3d/h3d_results/strs_tenscor3.F
!hd|        SWITCH_TO_DTNODA              engine/source/time_step/switch_to_dtnoda.F
!hd|        SZHOUR3                       engine/source/elements/solid/solidez/szhour3.F
!hd|        SZHOUR3_OR                    engine/source/elements/solid/solidez/szhour3_or.F
!hd|        S_USER                        engine/source/output/sty/s_user.F
!hd|        TAGOFF3N                      engine/source/interfaces/interf/chkstfn3.F
!hd|        TENCGPS1                      engine/source/output/anim/generate/tensorc.F
!hd|        TENCGPS2                      engine/source/output/anim/generate/tensorc.F
!hd|        TENSGPS1                      engine/source/output/anim/generate/tensor6.F
!hd|        TENSGPS2                      engine/source/output/anim/generate/tensor6.F
!hd|        TENSGPS3                      engine/source/output/anim/generate/tensor6.F
!hd|        TENSGPSTRAIN                  engine/source/output/anim/generate/tensgpstrain.F
!hd|        TENSGPS_SKIN                  engine/source/output/anim/generate/tensor6.F
!hd|        TENSOR0                       engine/source/output/anim/generate/tensor0.F
!hd|        TENSORC                       engine/source/output/anim/generate/tensorc.F
!hd|        TENSORC_CRK                   engine/source/output/anim/generate/tensorc_crk.F
!hd|        TENSORC_PLY                   engine/source/output/anim/generate/tensorc_ply.F
!hd|        TENSORS                       engine/source/output/anim/generate/tensor6.F
!hd|        TFORC3                        engine/source/elements/truss/tforc3.F
!hd|        THCLUSTER                     engine/source/output/th/thcluster.F
!hd|        THCOQ                         engine/source/output/th/thcoq.F
!hd|        THERMEXPC                     engine/source/materials/mat_share/thermexpc.F
!hd|        THERMEXPPI                    engine/source/elements/beam/thermexpp.F
!hd|        THICKVAR                      engine/source/elements/shell/coque/thickvar.F
!hd|        THNST                         engine/source/output/th/thnst.F
!hd|        THPOUT                        engine/source/output/th/thpout.F
!hd|        THQUAD                        engine/source/output/th/thquad.F
!hd|        THRES                         engine/source/output/th/thres.F
!hd|        THRES_COUNT                   engine/source/output/th/thres_count.F
!hd|        THSOL                         engine/source/output/th/thsol.F
!hd|        THSPH                         engine/source/output/th/thsph.F
!hd|        THTRUS                        engine/source/output/th/thtrus.F
!hd|        TKE3                          engine/source/elements/truss/tke3.F
!hd|        TM_DMGL25_SHELL               engine/source/output/outmaxsubr.F
!hd|        TM_DMG_SHELLS                 engine/source/output/outmaxsubr.F
!hd|        TM_DMG_SOLID                  engine/source/output/outmaxsubr.F
!hd|        TM_SEQ_SHELL                  engine/source/output/outmaxsubr.F
!hd|        TM_SEQ_SOLID                  engine/source/output/outmaxsubr.F
!hd|        TM_SIG_SHELL                  engine/source/output/outmaxsubr.F
!hd|        TM_SIG_SOLID                  engine/source/output/outmaxsubr.F
!hd|        TM_STRA_SHELL                 engine/source/output/outmaxsubr.F
!hd|        TM_STRA_SOLID                 engine/source/output/outmaxsubr.F
!hd|        UPDATE_SLIPRING               engine/source/tools/seatbelts/update_slipring.F
!hd|        UPD_TMAX                      engine/source/output/upd_outmax.F
!hd|        UPENRIC3_N3                   engine/source/elements/xfem/upenric3_nx.F
!hd|        UPENRIC3_N4                   engine/source/elements/xfem/upenric3_nx.F
!hd|        UPOFFC                        engine/source/elements/xfem/upoffc.F
!hd|        UPOFFTG                       engine/source/elements/xfem/upofftg.F
!hd|        UPXFEM1                       engine/source/elements/xfem/upxfem1.F
!hd|        UPXFEM_TAGXP                  engine/source/elements/xfem/upxfem_tagxp.F
!hd|        VELVECC22                     engine/source/output/anim/generate/velvec.F
!hd|        VELVECZ22                     engine/source/output/anim/generate/velvecz22.F
!hd|        VOLN22                        engine/source/interfaces/int22/voln22.F
!hd|        VOLPVGB                       engine/source/airbag/volpvg.F
!hd|        WRITE_BUF_LAW51               engine/source/materials/mat/mat051/write_buf_law51.F
!hd|        WRITE_CUT_CELL_BUFFER         engine/source/interfaces/int22/write_cut_cell_buffer.F
!hd|        W_ELBUF_STR                   engine/source/elements/elbuf/w_elbuf_str.F
!hd|        XFEOFF                        engine/source/elements/xfem/xfeoff.F
!hd|        XFORC3                        engine/source/elements/xelem/xforc3.F
!hd|        ALE51_GRADIENT_RECONSTRUCTION2engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
!hd|        MULTI_MUSCL_FLUXES_COMPUTATIONengine/source/multifluid/multi_muscl_fluxes_computation.F
!hd|        CHECK_MAT_ELEM_PROP_COMPATIBILITYstarter/source/materials/mat/check_mat_elem_prop_compatibility.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE ELBUFDEF_MOD
!-----------------------------------------------------------------------
#include "my_real.inc"
!=======================================================================
! define type ELBUF_STRUCT_ for element buffer structure array
!=======================================================================
!
        TYPE G_BUFEL_      ! material and property variables (mean values for each element)
          integer  NVAR_GLOB
          integer  G_NOFF
          integer  G_IERR
          integer  G_OFF
          integer  G_GAMA
          integer  G_SMSTR
          integer  G_HOURG
          integer  G_BFRAC
          integer  G_EINT
          integer  G_EINS
          integer  G_RHO
          integer  G_QVIS
          integer  G_DELTAX
          integer  G_VOL
          integer  G_EPSD
          integer  G_EPSQ
          integer  G_PLA
          integer  G_TEMP
          integer  G_TB
          integer  G_RK
          integer  G_RE
          integer  G_SIG
          integer  G_FOR
          integer  G_MOM
          integer  G_THK
          integer  G_TAG22
          integer  G_STRA
          integer  G_SIGI
          integer  G_DMG
          integer  G_FORPG
          integer  G_MOMPG
          integer  G_GAMA_R
!
          integer  G_FORPGPINCH
          integer  G_MOMPGPINCH
          integer  G_EPGPINCHXZ
          integer  G_EPGPINCHYZ
          integer  G_EPGPINCHZZ
!
          integer  G_STRPG
          integer  G_UELR
          integer  G_UELR1
          integer  G_DAMDL
          integer  G_FORTH
          integer  G_EINTTH
          integer  G_FILL
          integer  G_SEQ
          integer  G_STRW
          integer  G_STRWPG
          integer  G_THK_I
          integer  G_JAC_I
          integer  G_DT
          integer  G_ISMS
          integer  G_STRHG
          integer  G_BPRELD        ! bolt preloading
          integer  G_ABURN
          integer  G_MU
          integer  G_PLANL
          integer  G_EPSDNL
          integer  G_TEMPG
          integer  G_COR_NF        ! Nodal forces for corotational formulation
          integer  G_COR_FR        ! Local frame for corotational formulation
          integer  G_COR_XR        ! Reference local coordinates for corotational formulation
          integer  G_MAXFRAC
          integer  G_MAXEPS
          INTEGER  G_BETAORTH
          integer  G_AMU
          INTEGER  G_TSAIWU
          INTEGER  G_DMGSCL
          INTEGER  G_SH_IOFFSET
!---
!    - 1D - elem (TRUSS, BEAM, SPRING)
          integer  G_AREA
          integer  G_SKEW
          integer  G_LENGTH
          integer  G_TOTDEPL
          integer  G_TOTROT
          integer  G_FOREP
          integer  G_MOMEP
          integer  G_DEP_IN_TENS
          integer  G_DEP_IN_COMP
          integer  G_ROT_IN_TENS
          integer  G_ROT_IN_COMP
          integer  G_POSX
          integer  G_POSY
          integer  G_POSZ
          integer  G_POSXX
          integer  G_POSYY
          integer  G_POSZZ
          integer  G_YIELD
          integer  G_LENGTH_ERR
          integer  G_DV
          integer  G_DFS
          integer  G_SKEW_ERR
          integer  G_E6
          integer  G_RUPTCRIT
          integer  G_MASS
          integer  G_V_REPCVT
          integer  G_VR_REPCVT
          integer  G_NUVAR
          integer  G_NUVARN
          integer  G_DEFINI
          integer  G_FORINI
          integer  G_INIFRIC
          integer  G_ETOTSH
          integer  G_SKEW_ID
!
!    -  for seatbelt elements
          integer  G_SLIPRING_ID
          integer  G_SLIPRING_FRAM_ID
          integer  G_SLIPRING_STRAND
          integer  G_RETRACTOR_ID
          integer  G_RINGSLIP
          integer  G_ADD_NODE
          integer  G_UPDATE
          integer  G_FRAM_FACTOR
          integer  G_INTVAR
!---
          integer  G_DT_PITER
!---
          integer  G_IDT_TSH
!-------  max_historic variables
          integer  G_TM_YIELD
          integer  G_TM_SEQ
          integer  G_TM_EINT
          integer  G_TM_DMG
          integer  G_TM_SIG  ! max(p1)&min(p3) 3 for 2D, 6 for 3D
          integer  G_TM_STRA ! max(p1)&min(p3)
!---
          integer, DIMENSION(:) , POINTER ::   NOFF
          integer, DIMENSION(:) , POINTER ::   IERR
          my_real, DIMENSION(:) , POINTER ::   OFF
          my_real, DIMENSION(:) , POINTER ::   GAMA
          DOUBLE PRECISION, DIMENSION(:) , POINTER ::   SMSTR
          my_real, DIMENSION(:) , POINTER ::   HOURG
          my_real, DIMENSION(:) , POINTER ::   BFRAC
          my_real, DIMENSION(:) , POINTER ::   EINT
          my_real, DIMENSION(:) , POINTER ::   EINS
          my_real, DIMENSION(:) , POINTER ::   RHO
          my_real, DIMENSION(:) , POINTER ::   QVIS
          my_real, DIMENSION(:) , POINTER ::   DELTAX
          my_real, DIMENSION(:) , POINTER ::   VOL
          my_real, DIMENSION(:) , POINTER ::   EPSD
          my_real, DIMENSION(:) , POINTER ::   EPSQ
          my_real, DIMENSION(:) , POINTER ::   PLA
          my_real, DIMENSION(:) , POINTER ::   TEMP
          my_real, DIMENSION(:) , POINTER ::   TB
          my_real, DIMENSION(:) , POINTER ::   RK
          my_real, DIMENSION(:) , POINTER ::   RE
          my_real, DIMENSION(:) , POINTER ::   SIG
          my_real, DIMENSION(:) , POINTER ::   FOR
          my_real, DIMENSION(:) , POINTER ::   MOM
          my_real, DIMENSION(:) , POINTER ::   THK
          my_real, DIMENSION(:) , POINTER ::   TAG22
          my_real, DIMENSION(:) , POINTER ::   STRA
          my_real, DIMENSION(:) , POINTER ::   SIGI
          my_real, DIMENSION(:) , POINTER ::   DMG
          my_real, DIMENSION(:) , POINTER ::   FORPG   ! mean gauss point value
          my_real, DIMENSION(:) , POINTER ::   MOMPG
          my_real, DIMENSION(:) , POINTER ::   GAMA_R  ! Co-rotational local sys
!
          my_real, DIMENSION(:) , POINTER ::   FORPGPINCH
          my_real, DIMENSION(:) , POINTER ::   MOMPGPINCH
          my_real, DIMENSION(:) , POINTER ::   EPGPINCHXZ
          my_real, DIMENSION(:) , POINTER ::   EPGPINCHYZ
          my_real, DIMENSION(:) , POINTER ::   EPGPINCHZZ
!
          my_real, DIMENSION(:) , POINTER ::   STRPG
          my_real, DIMENSION(:) , POINTER ::   TEMPG
          my_real, DIMENSION(:) , POINTER ::   UELR   !  failure global variable
          my_real, DIMENSION(:) , POINTER ::   UELR1  !  failure global variable
          my_real, DIMENSION(:) , POINTER ::   DAMDL  !  delamination failure (law25)
          my_real, DIMENSION(:) , POINTER ::   FORTH
          my_real, DIMENSION(:) , POINTER ::   EINTTH
          my_real, DIMENSION(:) , POINTER ::   FILL
          my_real, DIMENSION(:) , POINTER ::   SEQ
          my_real, DIMENSION(:) , POINTER ::   THK_I   !---- add for Ismstr=10 shell (XFEM not done) STRWPG total anti-symme curvature
          my_real, DIMENSION(:) , POINTER ::   STRW
          my_real, DIMENSION(:) , POINTER ::   STRWPG
          my_real, DIMENSION(:) , POINTER ::   JAC_I   !--------inversed [J]
          my_real, DIMENSION(:) , POINTER ::   DT
          my_real, DIMENSION(:) , POINTER ::   ABURN
          my_real, DIMENSION(:) , POINTER ::   MU
          integer, DIMENSION(:) , POINTER ::   ISMS
          integer, DIMENSION(:) , POINTER ::   SH_IOFFSET
          my_real, DIMENSION(:) , POINTER ::   BPRELD  ! Bolt preloading
          my_real, DIMENSION(:) , POINTER ::   COR_NF  ! Corotational nodal forces
          my_real, DIMENSION(:) , POINTER ::   COR_FR  ! Corotational frame
          my_real, DIMENSION(:) , POINTER ::   COR_XR  ! Corotational reference coordinates
          my_real, DIMENSION(:) , POINTER ::   MAXFRAC
          my_real, DIMENSION(:) , POINTER ::   MAXEPS
          my_real, DIMENSION(:) , POINTER ::   BETAORTH
          my_real, DIMENSION(:) , POINTER ::   AMU
!    - 1D - elem (TRUSS, BEAM, SPRING)
          my_real, DIMENSION(:) , POINTER ::   AREA
          my_real, DIMENSION(:) , POINTER ::   SKEW
          my_real, DIMENSION(:) , POINTER ::   LENGTH
          my_real, DIMENSION(:) , POINTER ::   TOTDEPL
          my_real, DIMENSION(:) , POINTER ::   TOTROT
          my_real, DIMENSION(:) , POINTER ::   FOREP
          my_real, DIMENSION(:) , POINTER ::   MOMEP
          my_real, DIMENSION(:) , POINTER ::   DEP_IN_TENS
          my_real, DIMENSION(:) , POINTER ::   DEP_IN_COMP
          my_real, DIMENSION(:) , POINTER ::   ROT_IN_TENS
          my_real, DIMENSION(:) , POINTER ::   ROT_IN_COMP
          my_real, DIMENSION(:) , POINTER ::   POSX
          my_real, DIMENSION(:) , POINTER ::   POSY
          my_real, DIMENSION(:) , POINTER ::   POSZ
          my_real, DIMENSION(:) , POINTER ::   POSXX
          my_real, DIMENSION(:) , POINTER ::   POSYY
          my_real, DIMENSION(:) , POINTER ::   POSZZ
          my_real, DIMENSION(:) , POINTER ::   YIELD
          my_real, DIMENSION(:) , POINTER ::   LENGTH_ERR
          my_real, DIMENSION(:) , POINTER ::   DV
          my_real, DIMENSION(:) , POINTER ::   DFS
          my_real, DIMENSION(:) , POINTER ::   SKEW_ERR
          my_real, DIMENSION(:) , POINTER ::   E6
          my_real, DIMENSION(:) , POINTER ::   RUPTCRIT
          my_real, DIMENSION(:) , POINTER ::   MASS
          my_real, DIMENSION(:) , POINTER ::   V_REPCVT
          my_real, DIMENSION(:) , POINTER ::   VR_REPCVT
          my_real, DIMENSION(:) , POINTER ::   VAR
          my_real, DIMENSION(:) , POINTER ::   VARN
          my_real, DIMENSION(:) , POINTER ::   DEFINI
          my_real, DIMENSION(:) , POINTER ::   FORINI
          my_real, DIMENSION(:) , POINTER ::   INIFRIC
          my_real, DIMENSION(:) , POINTER ::   STRHG
          my_real, DIMENSION(:) , POINTER ::   ETOTSH
          integer, DIMENSION(:) , POINTER ::   SKEW_ID
          TYPE (FAIL_LOC_) , DIMENSION(:) , POINTER ::   FAIL
!
!    -  for seatbelt elements
          integer, DIMENSION(:) , POINTER ::   SLIPRING_ID
          integer, DIMENSION(:) , POINTER ::   SLIPRING_FRAM_ID
          integer, DIMENSION(:) , POINTER ::   SLIPRING_STRAND
          integer, DIMENSION(:) , POINTER ::   RETRACTOR_ID
          my_real, DIMENSION(:) , POINTER ::   RINGSLIP
          integer, DIMENSION(:) , POINTER ::   ADD_NODE
          integer, DIMENSION(:) , POINTER ::   UPDATE
          my_real, DIMENSION(:) , POINTER ::   FRAM_FACTOR
          my_real, DIMENSION(:) , POINTER ::   INTVAR
!---
          my_real, DIMENSION(:) , POINTER ::   DT_PITER ! Tetra10 Iterative Power for Time Step Computation
          integer, DIMENSION(:) , POINTER ::   IDT_TSH
!-------  max_historic variables
          my_real, DIMENSION(:) , POINTER ::   TM_YIELD
          my_real, DIMENSION(:) , POINTER ::   TM_SEQ
          my_real, DIMENSION(:) , POINTER ::   TM_EINT
          my_real, DIMENSION(:) , POINTER ::   TM_DMG
          my_real, DIMENSION(:) , POINTER ::   TM_SIG1
          my_real, DIMENSION(:) , POINTER ::   TM_STRA1
          my_real, DIMENSION(:) , POINTER ::   TM_SIG3
          my_real, DIMENSION(:) , POINTER ::   TM_STRA3
!---  work array
          my_real, DIMENSION(:) , POINTER ::   TM_PSIG
          my_real, DIMENSION(:) , POINTER ::   TM_PSTRA
!---
        END TYPE G_BUFEL_


        TYPE L_BUFEL_      ! element variables per integration point
          integer  MLAW    ! material law type
          integer  LawID   ! material law ID
          my_real, DIMENSION(:) , POINTER ::   OFF
          my_real, DIMENSION(:) , POINTER ::   GAMA
          my_real, DIMENSION(:) , POINTER ::   STRA
          my_real, DIMENSION(:) , POINTER ::   FRAC
          my_real, DIMENSION(:) , POINTER ::   BFRAC
          my_real, DIMENSION(:) , POINTER ::   EINT
          my_real, DIMENSION(:) , POINTER ::   EINS
          my_real, DIMENSION(:) , POINTER ::   RHO
          my_real, DIMENSION(:) , POINTER ::   DP_DRHO
          my_real, DIMENSION(:) , POINTER ::   QVIS
          my_real, DIMENSION(:) , POINTER ::   DELTAX
          my_real, DIMENSION(:) , POINTER ::   VOL
          my_real, DIMENSION(:) , POINTER ::   EPSA
          my_real, DIMENSION(:) , POINTER ::   EPSD
          my_real, DIMENSION(:) , POINTER ::   EPSQ
          my_real, DIMENSION(:) , POINTER ::   EPSF
          my_real, DIMENSION(:) , POINTER ::   PLA
          my_real, DIMENSION(:) , POINTER ::   TEMP
          my_real, DIMENSION(:) , POINTER ::   TB
          my_real, DIMENSION(:) , POINTER ::   RK
          my_real, DIMENSION(:) , POINTER ::   RE
          my_real, DIMENSION(:) , POINTER ::   VK
          my_real, DIMENSION(:) , POINTER ::   SF
          my_real, DIMENSION(:) , POINTER ::   ROB
          my_real, DIMENSION(:) , POINTER ::   DAM
          my_real, DIMENSION(:) , POINTER ::   DSUM
          my_real, DIMENSION(:) , POINTER ::   DGLO
          my_real, DIMENSION(:) , POINTER ::   CRAK
          my_real, DIMENSION(:) , POINTER ::   ANG
          my_real, DIMENSION(:) , POINTER ::   EPE
          my_real, DIMENSION(:) , POINTER ::   EPC
          my_real, DIMENSION(:) , POINTER ::   XST
          my_real, DIMENSION(:) , POINTER ::   SSP
          my_real, DIMENSION(:) , POINTER ::   Z
          my_real, DIMENSION(:) , POINTER ::   VISC
          my_real, DIMENSION(:) , POINTER ::   SIGL
          my_real, DIMENSION(:) , POINTER ::   SIGV
          my_real, DIMENSION(:) , POINTER ::   SIGA
          my_real, DIMENSION(:) , POINTER ::   SIGB
          my_real, DIMENSION(:) , POINTER ::   SIGC
          my_real, DIMENSION(:) , POINTER ::   SIGD
          my_real, DIMENSION(:) , POINTER ::   SIGF
          my_real, DIMENSION(:) , POINTER ::   SIG
          my_real, DIMENSION(:) , POINTER ::   SIGPLY
          my_real, DIMENSION(:) , POINTER ::   FOR
          my_real, DIMENSION(:) , POINTER ::   MOM
          my_real, DIMENSION(:) , POINTER ::   THK
          DOUBLE PRECISION, DIMENSION(:) , POINTER ::   SMSTR
          my_real, DIMENSION(:) , POINTER ::   DMG
          my_real, DIMENSION(:) , POINTER ::   FORTH
          my_real, DIMENSION(:) , POINTER ::   EINTTH
          my_real, DIMENSION(:) , POINTER ::   SEQ
          my_real, DIMENSION(:) , POINTER ::   JAC_I
          my_real, DIMENSION(:) , POINTER ::   FAC_YLD
          my_real, DIMENSION(:) , POINTER ::   ABURN
          my_real, DIMENSION(:) , POINTER ::   MU
          my_real, DIMENSION(:) , POINTER ::   PIJ   !--------[NI,J] for Imstr10
          DOUBLE PRECISION, DIMENSION(:) , POINTER ::   VOL0DP
          my_real, DIMENSION(:) , POINTER ::   PLANL
          my_real, DIMENSION(:) , POINTER ::   EPSDNL
          my_real, DIMENSION(:) , POINTER ::   DMGSCL
          my_real, DIMENSION(:) , POINTER ::   TSAIWU
        END TYPE L_BUFEL_

        TYPE BUF_PROP_
          my_real, DIMENSION(:)  , POINTER ::  VAR
          my_real, DIMENSION(:)  , POINTER ::  VARN
        END TYPE BUF_PROP_

!--------------------------------------------------------------------------------
!     Non-local buffer for regularization in the shell thickness
        TYPE BUF_NLOC_
          my_real, DIMENSION(:,:), POINTER :: MASSTH ! Embedded wire nodal masses
          my_real, DIMENSION(:,:), POINTER :: UNLTH  ! Non-local cumulated variable at nodes
          my_real, DIMENSION(:,:), POINTER :: VNLTH  ! Non-local velocities
          my_real, DIMENSION(:,:), POINTER :: FNLTH  ! Non-local forces
        END TYPE BUF_NLOC_
!     Non-local buffer for regularization in the thickshell thickness
        TYPE BUF_NLOCTS_
          my_real, DIMENSION(:,:), POINTER :: MASSTH ! Embedded wire nodal masses
          my_real, DIMENSION(:,:), POINTER :: UNLTH  ! Non-local cumulated variable at nodes
          my_real, DIMENSION(:,:), POINTER :: VNLTH  ! Non-local velocities
          my_real, DIMENSION(:,:), POINTER :: FNLTH  ! Non-local forces
        END TYPE BUF_NLOCTS_
!     Non-local buffer for brick elements geometry configuration
        TYPE BUF_NLOCS_
          INTEGER, DIMENSION(:)  , ALLOCATABLE :: NL_ISOLNOD ! Number of effective nodes (NEL)
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: NL_SOLNOD  ! Identifiers of effectives nodes (8,NEL)
        END TYPE BUF_NLOCS_
!--------------------------------------------------------------------------------

        TYPE BUF_EOS_
          my_real, DIMENSION(:)  , POINTER ::  VAR
        END TYPE BUF_EOS_

        TYPE BUF_PORO_
          my_real, DIMENSION(:)  , POINTER ::  VAR
        END TYPE BUF_PORO_

        TYPE BUF_VISC_
!        integer  ILAW    ! type de loi de viscosite
!        integer  NVAR
          my_real, DIMENSION(:)  , POINTER ::  VAR
        END TYPE BUF_VISC_

        TYPE BUF_XFEM_       ! Buffer des elements XFEM crees par la fissuration
!-------  layer variables
          integer  LY_SMSTR
          integer  LY_HOURG
          my_real, DIMENSION(:) , POINTER ::   DMG
          my_real, DIMENSION(:) , POINTER ::   GAMA
          my_real, DIMENSION(:) , POINTER ::   DIRA
          my_real, DIMENSION(:) , POINTER ::   DIRB
          my_real, DIMENSION(:) , POINTER ::   PLAPT
          my_real, DIMENSION(:) , POINTER ::   SIGPT
          my_real, DIMENSION(:) , POINTER ::   SMSTR
          my_real, DIMENSION(:) , POINTER ::   HOURG
          TYPE (G_BUFEL_)                                :: XGBUF   ! global variables
          TYPE (L_BUFEL_)  , DIMENSION(:,:,:)  , POINTER :: XLBUF   ! local variables (nptr,npts,nptt)
          TYPE (BUF_MAT_)  , DIMENSION(:,:,:)  , POINTER :: XMAT    ! material buffer
          TYPE (BUF_FAIL_) , DIMENSION(:,:,:)  , POINTER :: XFAIL   ! failure models
        END TYPE BUF_XFEM_

        TYPE FAIL_LOC_
          integer  ILAWF    ! type de loi de rupture
          integer  IDFAIL
          integer  NVAR
          integer  LF_DAM
          integer  LF_DAMMX
          integer  LF_DAMINI
          integer  LF_TDEL
          integer  LF_INDX
          integer  LF_OFF
          integer, DIMENSION(:)  , POINTER ::  INDX
          integer, DIMENSION(:)  , POINTER ::  OFF
          my_real, DIMENSION(:)  , POINTER ::  DAM
          my_real, DIMENSION(:)  , POINTER ::  VAR
          my_real, DIMENSION(:)  , POINTER ::  DAMMX
          my_real, DIMENSION(:)  , POINTER ::  DAMINI
          my_real, DIMENSION(:)  , POINTER ::  TDEL
        END TYPE FAIL_LOC_

        TYPE BUF_FAIL_
          TYPE(FAIL_LOC_), DIMENSION(:)  , POINTER ::  FLOC
        END TYPE BUF_FAIL_

        TYPE BUF_MAT_
          my_real, DIMENSION(:)  , POINTER ::  VAR
          INTEGER, DIMENSION(:)  , POINTER ::  VARTMP
        END TYPE BUF_MAT_
!
        TYPE L_BUFEL_DIR_      ! element variables per slice in each layer
          my_real, DIMENSION(:) , POINTER ::   DIRA
          my_real, DIMENSION(:) , POINTER ::   DIRB
        END TYPE L_BUFEL_DIR_

        TYPE BUF_LAY_
          integer  ILAW
          integer  IMAT
          integer  IEOS
          integer  IVISC
          integer  IPORO
          integer  NFAIL
          integer  NVAR_MAT
          integer  NVAR_EOS
          integer  NVARTMP
          integer  NVAR_VISC
          integer  NVAR_LAY   ! max nb of layer variables = 9
          integer  NVAR_LOC   ! max nb of local variables in LBUF = 51 (below)
          integer  NPTT       ! nb of integration points through layer (PID_51)
!-------
          integer  LY_DMG
          integer  LY_GAMA
          integer  LY_DIRA
          integer  LY_DIRB
          integer  LY_CRKDIR
          integer  LY_PLAPT  ! mean plastic strain value between gauss points
          integer  LY_SIGPT  ! mean stress value between gauss points
          integer  LY_HOURG
          integer  LY_UELR
          integer  LY_UELR1
          integer  LY_OFFPG
          integer  LY_OFF
!-------
          integer  L_OFF
          integer  L_GAMA
          integer  L_STRA
          integer  L_FRAC
          integer  L_BFRAC
          integer  L_EINT
          integer  L_EINS
          integer  L_RHO
          integer  L_DP_DRHO
          integer  L_QVIS
          integer  L_DELTAX
          integer  L_VOL
          integer  L_EPSA
          integer  L_EPSD
          integer  L_EPSQ
          integer  L_EPSF
          integer  L_PLA
          integer  L_TEMP
          integer  L_TB
          integer  L_RK
          integer  L_RE
          integer  L_VK
          integer  L_SF
          integer  L_ROB
          integer  L_DAM
          integer  L_DSUM
          integer  L_DGLO
          integer  L_CRAK
          integer  L_ANG
          integer  L_EPE
          integer  L_EPC
          integer  L_XST
          integer  L_SSP
          integer  L_Z
          integer  L_VISC
          integer  L_SIGL
          integer  L_SIGV
          integer  L_SIGA
          integer  L_SIGB
          integer  L_SIGC
          integer  L_SIGD
          integer  L_SIGF
          integer  L_SIG
          integer  L_SIGPLY ! PLYXFEM
          integer  L_FOR
          integer  L_MOM
          integer  L_THK
          integer  L_SMSTR
          integer  L_DMG
          integer  L_FORTH
          integer  L_EINTTH
          integer  L_SEQ
          integer  L_JAC_I
          integer  L_FAC_YLD
          integer  L_ABURN
          integer  L_MU
          integer  L_PIJ
          integer  L_VOL0DP
          integer  L_PLANL
          integer  L_EPSDNL
          integer  L_DMGSCL
          INTEGER  L_TSAIWU
!-------  layer variables
          my_real, DIMENSION(:) , POINTER ::   DMG
          my_real, DIMENSION(:) , POINTER ::   GAMA
          my_real, DIMENSION(:) , POINTER ::   DIRA
          my_real, DIMENSION(:) , POINTER ::   DIRB
          my_real, DIMENSION(:) , POINTER ::   CRKDIR
          my_real, DIMENSION(:) , POINTER ::   PLAPT
          my_real, DIMENSION(:) , POINTER ::   SIGPT
          my_real, DIMENSION(:) , POINTER ::   HOURG
          my_real, DIMENSION(:) , POINTER ::   UELR   !  failure layer variable
          my_real, DIMENSION(:) , POINTER ::   UELR1  !  failure layer variable
          INTEGER, DIMENSION(:) , POINTER ::   OFFPG  !  failure of Gauss point
          INTEGER, DIMENSION(:) , POINTER ::   OFF    !  layer failure flag
!-------
          TYPE (L_BUFEL_)  , DIMENSION(:,:,:)  , POINTER :: LBUF   ! local variables - per integration point
          TYPE (BUF_MAT_)  , DIMENSION(:,:,:)  , POINTER :: MAT    ! material buffer - per integration point
          TYPE (BUF_FAIL_) , DIMENSION(:,:,:)  , POINTER :: FAIL
          TYPE (BUF_PROP_) , DIMENSION(:,:,:)  , POINTER :: PROP
          TYPE (BUF_EOS_)  , DIMENSION(:,:,:)  , POINTER :: EOS
          TYPE (BUF_VISC_) , DIMENSION(:,:,:)  , POINTER :: VISC
          TYPE (BUF_PORO_) , DIMENSION(:,:,:)  , POINTER :: PORO
          TYPE (BUF_XFEM_) , DIMENSION(:)      , POINTER :: XFEM        ! XFEM (NXEL)
          TYPE (L_BUFEL_DIR_) , DIMENSION(:)  , POINTER :: LBUF_DIR   ! Local direction by int point in the tickness for slice)
        END TYPE BUF_LAY_
!
!--------------------
!
        TYPE BUF_INTLOC_      ! element variables per integration point
          my_real, DIMENSION(:) , POINTER ::   EPS   ! (length=3)
          my_real, DIMENSION(:) , POINTER ::   SIG   ! (length=3)
        END TYPE BUF_INTLOC_

        TYPE BUF_INTLAY_
          integer  ILAW       ! inter ply material law type
          integer  IMAT       ! inter ply material number
          integer  NFAIL
          integer  NVAR_MAT   ! number of user variables (UVAR) in the material buffer
          integer  NVARTMP    ! number of temp storage variables (VARTMP) in material laws
!------ interlayer variables par couche   (length=1)
          my_real, DIMENSION(:) , POINTER ::   EINT
          my_real, DIMENSION(:) , POINTER ::   COUNT

          TYPE (BUF_INTLOC_) , DIMENSION(:,:) , POINTER :: ILBUF
          TYPE (BUF_MAT_)    , DIMENSION(:,:) , POINTER :: MAT
          TYPE (BUF_FAIL_)   , DIMENSION(:,:) , POINTER :: FAIL
        END TYPE BUF_INTLAY_
!--------------------

        TYPE ELBUF_STRUCT_
          integer     :: IGTYP
          integer     :: NEL
          integer     :: NLAY
          integer     :: NINTLAY
          integer     :: NPTR
          integer     :: NPTS
          integer     :: NPTT
          integer     :: IXFEM
          integer     :: NXEL       ! number of xfem parts created after element crack
          integer     :: IDRAPE

          TYPE (G_BUFEL_)                              :: GBUF   ! global variables - mean element values
          TYPE (BUF_LAY_)   , DIMENSION(:)   , POINTER :: BUFLY  ! BUFLY(NLAY) layer variables
          TYPE (BUF_INTLAY_), DIMENSION(:)   , POINTER :: INTLAY ! inter-layer (NLAY-1)
          TYPE (BUF_XFEM_)  , DIMENSION(:)   , POINTER :: XFEM   ! XFEM (NXEL)
          TYPE (BUF_NLOC_)  , DIMENSION(:,:) , POINTER :: NLOC   ! Non-local thickness specific structure for shells
          TYPE (BUF_NLOCTS_), DIMENSION(:,:) , POINTER :: NLOCTS ! Non-local thickness specific structure for thickshells
          TYPE (BUF_NLOCS_)                            :: NLOCS  ! Non-local structure of brick element geometry configuration
        END TYPE ELBUF_STRUCT_
!---------------
      END MODULE ELBUFDEF_MOD
