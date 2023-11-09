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
Chd|====================================================================
Chd|  ELBUFDEF_MOD                  modules/mat_elem/elbufdef_mod.F
Chd|-- called by -----------
Chd|        MAT_ELEM_MOD                  common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MULTI_FVM_MOD                 common_source/modules/ale/multi_fvm_mod.F
Chd|        ALLOCBUF_AUTO                 starter/source/elements/elbuf_init/allocbuf_auto.F
Chd|        ANIOFF0                       starter/source/output/anim/anioff0.F
Chd|        ANIOFFC                       starter/source/output/anim/anioffc.F
Chd|        ANIOFFF                       starter/source/output/anim/aniofff.F
Chd|        ANIOFFS                       starter/source/output/anim/anioffs.F
Chd|        ANISKEW                       starter/source/output/anim/aniskew.F
Chd|        BINIT2                        starter/source/ale/bimat/binit2.F
Chd|        BSIGINI                       starter/source/elements/beam/bsigini.F
Chd|        BULKFAKEIGEO3                 starter/source/elements/ige3d/bulkfakeigeo3.F
Chd|        BUSERINI                      starter/source/elements/beam/buserini.F
Chd|        C3EPSINI                      starter/source/elements/sh3n/coque3n/c3epsini.F
Chd|        C3FINT_REG_INI                starter/source/elements/sh3n/coque3n/c3fint_reg_ini.F
Chd|        C3INIT3                       starter/source/elements/sh3n/coque3n/c3init3.F
Chd|        C3INMAS                       starter/source/elements/sh3n/coque3n/c3inmas.F
Chd|        CBAFINT_REG_INI               starter/source/elements/shell/coqueba/cbafint_reg_ini.F
Chd|        CBAINIT3                      starter/source/elements/shell/coqueba/cbainit3.F
Chd|        CBUFXFE                       starter/source/elements/xfem/cbufxfe.F
Chd|        CDKEPSINI                     starter/source/elements/sh3n/coquedk/cdkepsini.F
Chd|        CDKFINT_REG_INI               starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.F
Chd|        CDKINIT3                      starter/source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CEPSINI                       starter/source/elements/shell/coque/cepsini.F
Chd|        CFAILINI                      starter/source/elements/shell/coque/cfailini.F
Chd|        CFAILINI4                     starter/source/elements/shell/coque/cfailini.F
Chd|        CFINT_REG_INI                 starter/source/elements/shell/coque/cfint_reg_ini.F
Chd|        CHECK_PTHICKFAIL              starter/source/materials/fail/check_pthickfail.F
Chd|        CHK_DTTSH                     starter/source/elements/thickshell/solidec/scdtchk3.F
Chd|        CINIT3                        starter/source/elements/shell/coque/cinit3.F
Chd|        CINMAS                        starter/source/elements/shell/coque/cinmas.F
Chd|        CM27IN3                       starter/source/materials/mat/mat027/cm27in3.F
Chd|        CM35IN3                       starter/source/materials/mat/mat035/cm35in3.F
Chd|        CMAINI3                       starter/source/elements/sh3n/coquedk/cmaini3.F
Chd|        CMATINI                       starter/source/materials/mat_share/cmatini.F
Chd|        CMATINI4                      starter/source/materials/mat_share/cmatini4.F
Chd|        CMLAWI                        starter/source/elements/shell/coque/cepsini.F
Chd|        CNEPSINI                      starter/source/elements/shell/coqueba/cnepsini.F
Chd|        CNLOC_MAT104_INI              starter/source/materials/mat/mat104/cnloc_mat104_ini.F
Chd|        CNLOC_MATINI                  starter/source/materials/mat_share/cnloc_matini.F
Chd|        CORTH3                        starter/source/elements/shell/coque/corth3.F
Chd|        CORTHDIR                      starter/source/elements/shell/coque/corthdir.F
Chd|        CORTHINI                      starter/source/elements/shell/coque/corthini.F
Chd|        CSIGINI                       starter/source/elements/shell/coque/csigini.F
Chd|        CSIGINI4                      starter/source/elements/shell/coqueba/scigini4.F
Chd|        CUSERINI                      starter/source/elements/shell/coque/cuserini.F
Chd|        CUSERINI4                     starter/source/elements/shell/coqueba/cuserini4.F
Chd|        C_SEATBELTS                   starter/source/restart/ddsplit/c_seatbelts.F
Chd|        DEALLOCATE_ELBUF              starter/source/elements/elbuf_init/deallocate_buffer.F
Chd|        DEALLOCATE_ONE_ELEMENT_GROUP  starter/source/elements/elbuf_init/deallocate_one_element_group.F
Chd|        DFUNC0                        starter/source/output/anim/dfunc0.F
Chd|        DFUNCC                        starter/source/output/anim/dfuncc.F
Chd|        DFUNCF                        starter/source/output/anim/dfuncf.F
Chd|        DFUNCS                        starter/source/output/anim/dfuncs.F
Chd|        DMASANI0                      starter/source/output/anim/dmasani0.F
Chd|        DMASANIC                      starter/source/output/anim/dmasanic.F
Chd|        DMASANIF                      starter/source/output/anim/dmasanif.F
Chd|        DMASANIS                      starter/source/output/anim/dmasanis.F
Chd|        DTMAIN                        starter/source/materials/time_step/dtmain.F
Chd|        FAILINI                       starter/source/elements/solid/solide/failini.F
Chd|        FAIL_WINDSHIELD_INIT          starter/source/materials/fail/windshield_alter/fail_windshield_init.F
Chd|        GENANI1                       starter/source/output/anim/genani1.F
Chd|        IG3DINIT3                     starter/source/elements/ige3d/ig3dinit3.F
Chd|        INICRKFILL                    starter/source/elements/xfem/inicrkfill.F
Chd|        INIEBCSP                      starter/source/boundary_conditions/ebcs/iniebcsp.F
Chd|        INIEBCSP0                     starter/source/boundary_conditions/ebcs/iniebcsp0.F
Chd|        INIEBCS_DP                    starter/source/boundary_conditions/ebcs/iniebcs_dp.F
Chd|        INIEBCS_NRF_TCAR              starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|        INIFILL                       starter/source/initial_conditions/inivol/inifill.F
Chd|        INIGRAV_EOS                   starter/source/initial_conditions/inigrav/inigrav_eos.F
Chd|        INIGRAV_LOAD                  starter/source/initial_conditions/inigrav/inigrav_load.F
Chd|        INIGRAV_M37                   starter/source/initial_conditions/inigrav/inigrav_m37.F
Chd|        INIGRAV_M51                   starter/source/initial_conditions/inigrav/inigrav_m51.F
Chd|        ININTR_THKVAR                 starter/source/interfaces/interf1/inintr_thkvar.F
Chd|        INIPHASE                      starter/source/initial_conditions/inivol/iniphase.F
Chd|        INIRIG_MAT                    starter/source/elements/initia/inirig_mat.F
Chd|        INITVARS_AUTO                 starter/source/elements/elbuf_init/initvars_auto.F
Chd|        INIVOID                       starter/source/elements/initia/inivoid.F
Chd|        INIVOL_SET                    starter/source/initial_conditions/inivol/inivol_set.F
Chd|        INI_INIMAP1D                  starter/source/initial_conditions/inimap/ini_inimap1d.F
Chd|        INI_INIMAP2D                  starter/stub/ini_inimap2d.F   
Chd|        INI_OUTMAX_AUTO               starter/source/elements/elbuf_init/ini_outmax_auto.F
Chd|        INI_SEATBELT                  starter/source/tools/seatbelts/ini_seatbelt.F
Chd|        LAYINI1                       starter/source/elements/shell/coqueba/layini1.F
Chd|        LAYINI_XFE                    starter/source/elements/xfem/cbufxfe.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        LSLOCAL                       starter/source/elements/xfem/lslocal.F
Chd|        M20DCOD                       starter/source/system/fsdcod.F
Chd|        M37INIT                       starter/source/materials/mat/mat037/m37init.F
Chd|        M51INIT                       starter/source/materials/mat/mat051/m51init.F
Chd|        MATINI                        starter/source/materials/mat_share/matini.F
Chd|        MULTIFLUID_GLOBAL_TDET        starter/source/multifluid/multifluid_global_tdet.F
Chd|        MULTIFLUID_INIT2              starter/source/multifluid/multifluid_init2.F
Chd|        MULTIFLUID_INIT2T             starter/source/multifluid/multifluid_init2t.F
Chd|        MULTIFLUID_INIT3              starter/source/multifluid/multifluid_init3.F
Chd|        MULTIFLUID_INIT3T             starter/source/multifluid/multifluid_init3t.F
Chd|        NLOCAL_INIT_STA               starter/source/materials/fail/nlocal_init_sta.F
Chd|        NLOC_DMG_INIT                 starter/source/materials/fail/nloc_dmg_init.F
Chd|        PARSORC                       starter/source/output/anim/parsorc.F
Chd|        PINIT3                        starter/source/elements/beam/pinit3.F
Chd|        PREINICRK3N                   starter/source/elements/xfem/preinicrk3N.F
Chd|        PREINICRK4N                   starter/source/elements/xfem/preinicrk4N.F
Chd|        Q4INIT2                       starter/source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        starter/source/elements/solid_2d/quad/qinit2.F
Chd|        RINIT3                        starter/source/elements/spring/rinit3.F
Chd|        S10DERI3                      starter/source/elements/solid/solide10/s10deri3.F
Chd|        S10INIT3                      starter/source/elements/solid/solide10/s10init3.F
Chd|        S10JACI3                      starter/source/elements/solid/solide10/s10jaci3.F
Chd|        S16INIT3                      starter/source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      starter/source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       starter/source/elements/solid/solide4/s4init3.F
Chd|        S6CINIT3                      starter/source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      starter/source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8E_PIJ                       starter/source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZINIT3                      starter/source/elements/solid/solide8z/s8zinit3.F
Chd|        SCALEINI                      starter/source/elements/initia/scaleini.F
Chd|        SCINIT3                       starter/source/elements/thickshell/solidec/scinit3.F
Chd|        SETELOFF                      starter/source/constraints/general/rbody/hm_read_rbody.F
Chd|        SIGIN3B                       starter/source/elements/solid/solid8p/sigin3b.F
Chd|        SINI43                        starter/source/elements/solid/sconnect/sini43.F
Chd|        SINIT3                        starter/source/elements/solid/solide/sinit3.F
Chd|        SMS_AUTO_DT                   starter/source/ams/sms_auto_dt.F
Chd|        SPINIT3                       starter/source/elements/sph/spinit3.F
Chd|        SUINIT3                       starter/source/elements/elbuf_init/suinit3.F
Chd|        TENSOR0                       starter/source/output/anim/tensor0.F
Chd|        TENSORC                       starter/source/output/anim/tensorc.F
Chd|        TENSORS                       starter/source/output/anim/tensors.F
Chd|        THICKVAR                      starter/source/interfaces/interf1/inintr_thkvar.F
Chd|        THICK_ILEV                    starter/source/elements/xfem/thick_ilev.F
Chd|        TINIT3                        starter/source/elements/truss/tinit3.F
Chd|        W_ELBUF_STR                   starter/source/restart/ddsplit/w_elbuf_str.F
Chd|        XINIT3                        starter/source/elements/xelem/xinit3.F
Chd|        ZEROVARS_AUTO                 starter/source/elements/elbuf_init/zerovars_auto.F
Chd|        A22CONV3                      engine/source/ale/alefvm/cut_cells/a22conv3.F
Chd|        ACONVE                        engine/source/ale/aconve.F    
Chd|        ADMDIV                        engine/source/model/remesh/admdiv.F
Chd|        ADMERR                        engine/source/model/remesh/admerr.F
Chd|        ADMGVID                       engine/source/model/remesh/admgvid.F
Chd|        ADMINI                        engine/source/model/remesh/admini.F
Chd|        ADMMAP3                       engine/source/model/remesh/admmap3.F
Chd|        ADMMAP4                       engine/source/model/remesh/admmap4.F
Chd|        ADMREGUL                      engine/source/model/remesh/admregul.F
Chd|        ADMTHKE                       engine/source/model/remesh/admthke.F
Chd|        AETURB                        engine/source/ale/turbulence/aeturb.F
Chd|        AFLUX0                        engine/source/ale/aflux0.F    
Chd|        AFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
Chd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
Chd|        AGAUGE                        engine/source/ale/agauge.F    
Chd|        AGRAD0                        engine/source/ale/agrad0.F    
Chd|        AIRBAGB1                      engine/source/airbag/airbagb1.F
Chd|        AKTURB                        engine/source/ale/turbulence/akturb.F
Chd|        ALE51_ANTIDIFF3_INT22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
Chd|        ALE51_FINISH                  engine/source/ale/ale51/ale51_finish.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_INIT                    engine/source/ale/ale51/ale51_init.F
Chd|        ALE51_UPWIND3_INT22           engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
Chd|        ALEFLOW                       engine/source/ale/porous/aleflow.F
Chd|        ALEFVM_MAIN                   engine/source/ale/alefvm/alefvm_main.F
Chd|        ALEFVM_STRESS_INT22           engine/source/ale/alefvm/alefvm_stress_int22.F
Chd|        ALETHE                        engine/source/ale/alethe.F    
Chd|        ALEW6                         engine/source/ale/grid/alew6.F
Chd|        ALEWDX                        engine/source/ale/grid/alewdx.F
Chd|        ALLOCBUF_AUTO                 engine/source/elements/elbuf/allocbuf_auto.F
Chd|        ALLOC_ELBUF_IMP               engine/source/elements/elbuf/alloc_elbuf_imp.F
Chd|        ANIMIG3D                      engine/source/output/anim/generate/animig3d.F
Chd|        ANIMX                         engine/source/output/anim/generate/animx.F
Chd|        ANIM_NODAL_P_ELEMS            engine/source/output/anim/generate/anim_nodal_p_elems.F
Chd|        ANIOFF0                       engine/source/output/anim/generate/anioff0.F
Chd|        ANIOFFC                       engine/source/output/anim/generate/anioffc.F
Chd|        ANIOFFC_CRK                   engine/source/output/anim/generate/anioffc_crk.F
Chd|        ANIOFFC_PLY                   engine/source/output/anim/generate/anioffc_ply.F
Chd|        ANIOFFF                       engine/source/output/anim/generate/aniofff.F
Chd|        ANIOFFS                       engine/source/output/anim/generate/anioff6.F
Chd|        ANISKEW                       engine/source/output/anim/generate/aniskew.F
Chd|        AREZON                        engine/source/ale/arezon.F    
Chd|        ATHERM                        engine/source/ale/atherm.F    
Chd|        BFORC2                        engine/source/ale/bimat/bforc2.F
Chd|        C3COORK3                      engine/source/elements/sh3n/coque3n/c3coork3.F
Chd|        C3EVEC3                       engine/source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3FINT_REG                    engine/source/elements/sh3n/coque3n/c3fint_reg.F
Chd|        C3KE3                         engine/source/elements/sh3n/coque3n/c3ke3.F
Chd|        CBACOOR                       engine/source/elements/shell/coqueba/cbacoor.F
Chd|        CBACOORK                      engine/source/elements/shell/coqueba/cbacoork.F
Chd|        CBACOORT                      engine/source/elements/shell/coqueba/cbacoor.F
Chd|        CBAFINT_REG                   engine/source/elements/shell/coqueba/cbafint_reg.F
Chd|        CBAKE3                        engine/source/elements/shell/coqueba/cbake3.F
Chd|        CBAL58WARP                    engine/source/elements/shell/coqueba/cbawarpoff.F
Chd|        CBAPINCHPROJ                  engine/source/elements/shell/coqueba/cbapinchproj.F
Chd|        CDK6COOR3                     engine/source/elements/sh3n/coquedk6/cdk6coor3.F
Chd|        CDK6FINT_REG                  engine/source/elements/sh3n/coquedk6/cdk6fint_reg.F
Chd|        CDKCOOR3                      engine/source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CDKFINT_REG                   engine/source/elements/sh3n/coquedk/cdkfint_reg.F
Chd|        CEVEC3                        engine/source/elements/shell/coque/cevec3.F
Chd|        CFINT_REG                     engine/source/elements/shell/coque/cfint_reg.F
Chd|        CGSHELL3                      engine/source/implicit/cgshell.F
Chd|        CGSHELL4                      engine/source/implicit/cgshell.F
Chd|        CHECK_ALE_COMM                engine/source/ale/check_ale_comm.F
Chd|        CHKSTFN3N                     engine/source/interfaces/interf/chkstfn3.F
Chd|        CLUSTERF                      engine/source/output/cluster/clusterf.F
Chd|        CMAIN3PINCH                   engine/source/elements/shell/coqueba/cmain3pinch.F
Chd|        CMATC3                        engine/source/elements/shell/coqueba/cmatc3.F
Chd|        CNCOEFORT                     engine/source/elements/sh3n/coquedk/cncoef3.F
Chd|        CNVEC3                        engine/source/elements/shell/coque/cnvec3.F
Chd|        CONVECOFF                     engine/source/constraints/thermic/convecoff.F
Chd|        COPY_ELBUF                    engine/source/elements/elbuf/copy_elbuf.F
Chd|        COPY_ELBUF_1                  engine/source/elements/elbuf/copy_elbuf_1.F
Chd|        CORTDIR3                      engine/source/elements/shell/coque/cortdir3.F
Chd|        COUNT_ARSZ_CT                 engine/source/output/sty/outp_c_t.F
Chd|        COUNT_ARSZ_ST                 engine/source/output/sty/outp_s_t.F
Chd|        CP_IMPBUF                     engine/source/implicit/produt_v.F
Chd|        CRKLAYER4N_ADV                engine/source/elements/xfem/crklayer4n_adv.F
Chd|        CRKLAYER4N_INI                engine/source/elements/xfem/crklayer4n_ini.F
Chd|        CRKOFFC                       engine/source/elements/xfem/precrklay.F
Chd|        CRKOFFTG                      engine/source/elements/xfem/precrklay.F
Chd|        CUTFUNCE                      engine/source/tools/sect/cutfunce.F
Chd|        CZCOORK3                      engine/source/elements/shell/coquez/czcoork3.F
Chd|        CZCORC1                       engine/source/elements/shell/coquez/czcorc.F
Chd|        CZCORCHT                      engine/source/elements/shell/coquez/czcorc.F
Chd|        CZCORCT                       engine/source/elements/shell/coquez/czcorc.F
Chd|        CZKE3                         engine/source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        engine/source/elements/shell/coquez/czkel3.F
Chd|        C_TF_NE                       engine/source/output/sty/c_tf_ne.F
Chd|        DELAMINATION                  engine/source/properties/composite_options/stack/delamination.F
Chd|        DELTAX22                      engine/source/interfaces/int22/deltax22.F
Chd|        DESACTI                       engine/source/elements/desacti.F
Chd|        DFUNC0                        engine/source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        engine/source/output/anim/generate/dfuncc.F
Chd|        DFUNCC_CRK                    engine/source/output/anim/generate/dfuncc_crk.F
Chd|        DFUNCC_PLY                    engine/source/output/anim/generate/dfuncc_ply.F
Chd|        DFUNCF                        engine/source/output/anim/generate/dfuncf.F
Chd|        DFUNCS                        engine/source/output/anim/generate/dfunc6.F
Chd|        DFUNGPS1                      engine/source/output/anim/generate/dfuncf.F
Chd|        DFUNGPS2                      engine/source/output/anim/generate/dfuncf.F
Chd|        DIM_ELEMAX                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_ELEMS1                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_ELEMS2                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_ELEMS3                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_ELEMS4                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_ELEMSP                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_FR_K                      engine/source/mpi/implicit/imp_fri.F
Chd|        DIM_GLOB_K                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_KINMAX                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_TSHEDG                    engine/source/elements/thickshell/solidec/dim_tshedg.F
Chd|        DMASANI0                      engine/source/output/anim/generate/dmasani0.F
Chd|        DMASANIC                      engine/source/output/anim/generate/dmasanic.F
Chd|        DMASANIF                      engine/source/output/anim/generate/dmasanif.F
Chd|        DMASANIS                      engine/source/output/anim/generate/dmasani6.F
Chd|        DYNAIN_C_STRAG                engine/source/output/dynain/dynain_c_strag.F
Chd|        DYNAIN_C_STRSG                engine/source/output/dynain/dynain_c_strsg.F
Chd|        DYNAIN_SHEL_MP                engine/source/output/dynain/dynain_shel_mp.F
Chd|        DYNAIN_SHEL_SPMD              engine/source/output/dynain/dynain_shel_spmd.F
Chd|        DYNAIN_SIZE_C                 engine/source/output/dynain/dynain_size.F
Chd|        EBCS0                         engine/source/boundary_conditions/ebcs/ebcs0.F
Chd|        EBCS10                        engine/source/boundary_conditions/ebcs/ebcs10.F
Chd|        EBCS_MAIN                     engine/source/boundary_conditions/ebcs/ebcs_main.F
Chd|        EFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
Chd|        EIG                           engine/stub/eig.F             
Chd|        EIG1                          engine/stub/eig1.F            
Chd|        EIGCOND                       engine/stub/eigcond.F         
Chd|        EIGOFF                        engine/source/output/anim/generate/eigoff.F
Chd|        EIGP                          engine/stub/eigp.F            
Chd|        ELOFF                         engine/source/elements/eloff.F
Chd|        ENRICHC_INI                   engine/source/elements/xfem/enrichc_ini.F
Chd|        ENRICHTG_INI                  engine/source/elements/xfem/enrichtg_ini.F
Chd|        ERR_THK                       engine/source/elements/shell/err_thk.F
Chd|        FAIL_BEAM18                   engine/source/elements/beam/fail_beam18.F
Chd|        FAIL_BEAM3                    engine/source/elements/beam/fail_beam3.F
Chd|        FAIL_GENE1_S                  engine/source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_SETOFF_C                 engine/source/materials/fail/fail_setoff_c.F
Chd|        FAIL_SETOFF_NPG_C             engine/source/materials/fail/fail_setoff_npg_c.F
Chd|        FAIL_SETOFF_WIND_FRWAVE       engine/source/materials/fail/fail_setoff_wind_frwave.F
Chd|        FORINTS                       engine/source/elements/forints.F
Chd|        FVBAG0                        engine/source/airbag/fvbag0.F 
Chd|        FVBAG1                        engine/source/airbag/fvbag1.F 
Chd|        FVBAG2                        engine/source/airbag/fvbag2.F 
Chd|        FVVENT0                       engine/source/airbag/fvvent0.F
Chd|        FV_UP_SWITCH                  engine/source/airbag/fv_up_switch.F
Chd|        FXBODFP1                      engine/source/constraints/fxbody/fxbodfp.F
Chd|        FXBSGMAJ                      engine/source/constraints/fxbody/fxbsgmaj.F
Chd|        FXBYFOR                       engine/source/constraints/fxbody/fxbyfor.F
Chd|        FXBYPID                       engine/source/constraints/fxbody/fxbypid.F
Chd|        GENANI                        engine/source/output/anim/generate/genani.F
Chd|        GENDYNAIN                     engine/source/output/dynain/gendynain.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        GENOUTP                       engine/source/output/sty/genoutp.F
Chd|        GENSTAT                       engine/source/output/sta/genstat.F
Chd|        GET_NODAL_IPART               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        GET_Q4LSYS                    engine/source/output/sta/sta_c_get_q4lsys.F
Chd|        GET_T3LSYS                    engine/source/output/sta/sta_c_get_t3lsys.F
Chd|        GPSSTRAIN_SKIN                engine/source/output/anim/generate/tensgpstrain.F
Chd|        GPSTRA_SOLID                  engine/source/output/outmaxsubr.F
Chd|        GPS_SOLID                     engine/source/output/outmaxsubr.F
Chd|        H3D_FLD_STRAIN                engine/source/output/h3d/h3d_results/h3d_fld_strain.F
Chd|        H3D_FLD_TSH                   engine/source/output/h3d/h3d_results/h3d_fld_tsh.F
Chd|        H3D_NODAL_SCALAR              engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|        H3D_NODAL_TENSOR              engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|        H3D_NODAL_VECTOR              engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|        H3D_ONED_OFF                  engine/source/output/h3d/spmd/spmd_h3d_oned_off.F
Chd|        H3D_ONED_SCALAR               engine/source/output/h3d/h3d_results/h3d_oned_scalar.F
Chd|        H3D_ONED_TENSOR               engine/source/output/h3d/h3d_results/h3d_oned_tensor.F
Chd|        H3D_ONED_VECTOR               engine/source/output/h3d/h3d_results/h3d_oned_vector.F
Chd|        H3D_QUAD_OFF                  engine/source/output/h3d/spmd/spmd_h3d_quad_off.F
Chd|        H3D_QUAD_SCALAR               engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_QUAD_TENSOR               engine/source/output/h3d/h3d_results/h3d_quad_tensor.F
Chd|        H3D_QUAD_VECTOR               engine/source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|        H3D_SHELL_OFF                 engine/source/output/h3d/spmd/spmd_h3d_shell_off.F
Chd|        H3D_SHELL_SCALAR              engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|        H3D_SHELL_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SHELL_TENSOR              engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        H3D_SHELL_VECTOR              engine/source/output/h3d/h3d_results/h3d_shell_vector.F
Chd|        H3D_SHELL_VECTOR_1            engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
Chd|        H3D_SKIN_IXSKIN               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_OFF                  engine/source/output/h3d/h3d_results/h3d_skin_off.F
Chd|        H3D_SKIN_SCALAR               engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_TENSOR               engine/source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        H3D_SOLID_OFF                 engine/source/output/h3d/spmd/spmd_h3d_solid_off.F
Chd|        H3D_SOLID_SCALAR              engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SOLID_TENSOR              engine/source/output/h3d/h3d_results/h3d_solid_tensor.F
Chd|        H3D_SOLID_TENSOR_1            engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        H3D_SOLID_VECTOR              engine/source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        H3D_SOL_SKIN_IXSKIN           engine/source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
Chd|        H3D_SOL_SKIN_SCALAR           engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|        H3D_SOL_SKIN_SCALAR1          engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
Chd|        H3D_SOL_SKIN_TENSOR           engine/source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|        H3D_SPH_OFF                   engine/source/output/h3d/spmd/spmd_h3d_sph_off.F
Chd|        H3D_SPH_SCALAR                engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|        H3D_SPH_TENSOR                engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
Chd|        H3D_VELVECC22                 engine/source/output/h3d/h3d_results/h3d_velvecc22.F
Chd|        H3D_VELVECZ22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.F
Chd|        HIST2                         engine/source/output/th/hist2.F
Chd|        I18FOR3                       engine/source/interfaces/int18/i18for3.F
Chd|        I22FOR3                       engine/source/interfaces/int22/i22for3.F
Chd|        I22MAINF                      engine/source/interfaces/int22/i22mainf.F
Chd|        I22SUBVOL                     engine/source/interfaces/int22/i22subvol.F
Chd|        I7MAINF                       engine/source/interfaces/int07/i7mainf.F
Chd|        I9GRD2                        engine/source/interfaces/int09/i9grd2.F
Chd|        I9GRD3                        engine/source/interfaces/int09/i9grd3.F
Chd|        I9WAL2                        engine/source/interfaces/int09/i9wal2.F
Chd|        I9WAL3                        engine/source/interfaces/int09/i9wal3.F
Chd|        I9WALE                        engine/source/interfaces/int09/i9wale.F
Chd|        IMP_BUCK                      engine/source/implicit/imp_buck.F
Chd|        IMP_CHKM                      engine/source/implicit/imp_solv.F
Chd|        IMP_GLOB_K                    engine/source/implicit/imp_glob_k.F
Chd|        IMP_GLOB_K0                   engine/source/implicit/imp_glob_k.F
Chd|        IMP_GLOB_KHP                  engine/source/implicit/imp_glob_k.F
Chd|        IMP_INIT                      engine/source/implicit/imp_init.F
Chd|        IMP_K_EIG                     engine/stub/imp_k_eig.F       
Chd|        IMP_SOLV                      engine/source/implicit/imp_solv.F
Chd|        IMP_SOL_INIT                  engine/source/implicit/imp_sol_init.F
Chd|        IND_FR_K                      engine/source/mpi/implicit/imp_fri.F
Chd|        IND_GLOB_K                    engine/source/implicit/ind_glob_k.F
Chd|        IND_TSHEDG                    engine/source/elements/thickshell/solidec/ind_tshedg.F
Chd|        INIT_TH                       engine/source/output/th/init_th.F
Chd|        INIT_TH0                      engine/source/output/th/init_th0.F
Chd|        INIXFEM                       engine/source/elements/xfem/inixfem.F
Chd|        INI_FR_K                      engine/source/mpi/implicit/imp_fri.F
Chd|        INI_TMAX                      engine/source/output/ini_outmax.F
Chd|        INT18_LAW151_INIT             engine/source/interfaces/int18/int18_law151_init.F
Chd|        INT18_LAW151_UPDATE           engine/source/interfaces/int18/int18_law151_update.F
Chd|        INTAL3                        engine/source/ale/inter/intal3.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        JOINT_BLOCK_STIFFNESS         engine/source/elements/joint/joint_block_stiffness.F
Chd|        JOINT_ELEM_TIMESTEP           engine/source/elements/joint/joint_elem_timestep.F
Chd|        KINE_SEATBELT_FORCE           engine/source/tools/seatbelts/kine_seatbelt_force.F
Chd|        KINE_SEATBELT_VEL             engine/source/tools/seatbelts/kine_seatbelt_vel.F
Chd|        KTBUF_INI                     engine/source/implicit/imp_init.F
Chd|        LASER1                        engine/source/loads/laser/laser1.F
Chd|        LASER2                        engine/source/loads/laser/laser2.F
Chd|        LASER3                        engine/source/loads/laser/laser2.F
Chd|        LAYINI                        engine/source/elements/shell/coque/layini.F
Chd|        LECH3D                        engine/source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        LECSTAT                       engine/source/input/lecstat.F 
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        M11LAW                        engine/source/materials/mat/mat011/m11law.F
Chd|        M11VS2                        engine/source/materials/mat/mat011/m11vs2.F
Chd|        M11VS3                        engine/source/materials/mat/mat011/m11vs3.F
Chd|        M1LAW8                        engine/source/materials/mat/mat001/m1law8.F
Chd|        M24LAW                        engine/source/materials/mat/mat024/m24law.F
Chd|        M2LAW8                        engine/source/materials/mat/mat002/m2law8.F
Chd|        M2LAWPI                       engine/source/materials/mat/mat002/m2lawpi.F
Chd|        M3LAW8                        engine/source/materials/mat/mat003/m3law8.F
Chd|        M51VOIS2                      engine/source/materials/mat/mat051/m51vois2.F
Chd|        M51VOIS3                      engine/source/materials/mat/mat051/m51vois3.F
Chd|        MAIN_BEAM18                   engine/source/elements/beam/main_beam18.F
Chd|        MAIN_BEAM3                    engine/source/elements/beam/main_beam3.F
Chd|        MATERIAL_FLOW                 engine/source/tools/seatbelts/material_flow.F
Chd|        MDAMA24                       engine/source/elements/solid/solidez/mdama24.F
Chd|        MEOS8                         engine/source/materials/mat_share/meos8.F
Chd|        MMAIN8                        engine/source/materials/mat_share/mmain8.F
Chd|        MONVOL0                       engine/source/airbag/monvol0.F
Chd|        MULAW8                        engine/source/materials/mat_share/mulaw8.F
Chd|        MULAWGLC                      engine/source/materials/mat_share/mulawglc.F
Chd|        MULAWGLCPINCH                 engine/source/elements/shell/coqueba/mulawglcpinch.F
Chd|        MULAW_IB                      engine/source/elements/beam/mulaw_ib.F
Chd|        MULTI_BUF2VAR                 engine/source/multifluid/multi_buf2var.F
Chd|        MULTI_COMPUTEVOLUME           engine/source/multifluid/multi_computevolume.F
Chd|        MULTI_COMPUTE_DT              engine/source/multifluid/multi_compute_dt.F
Chd|        MULTI_EVOLVE_GLOBAL           engine/source/multifluid/multi_evolve_global.F
Chd|        MULTI_EVOLVE_PARTIAL          engine/source/multifluid/multi_evolve_partial.F
Chd|        MULTI_FLUXES_COMPUTATION      engine/source/multifluid/multi_fluxes_computation.F
Chd|        MULTI_FVM2FEM                 engine/source/multifluid/multi_fvm2fem.F
Chd|        MULTI_GLOBALIZE               engine/source/multifluid/multi_globalize.F
Chd|        MULTI_I18_FORCE_POFF          engine/source/interfaces/int18/multi_i18_force_poff.F
Chd|        MULTI_MUSCL_GRADIENTS         engine/source/multifluid/multi_muscl_gradients.F
Chd|        MULTI_PRESSURE_EQUILIBRIUM    engine/source/multifluid/multi_pressure_equilibrium.F
Chd|        MULTI_TIMEEVOLUTION           engine/source/multifluid/multi_timeevolution.F
Chd|        MULTI_UPDATE_GLOBAL           engine/source/multifluid/multi_update_global.F
Chd|        MULTI_UPDATE_PARTIAL          engine/source/multifluid/multi_update_partial.F
Chd|        MULTI_VAR2BUF                 engine/source/multifluid/multi_var2buf.F
Chd|        NODALD                        engine/source/output/anim/generate/nodald.F
Chd|        NODALDT                       engine/source/output/anim/generate/nodaldt.F
Chd|        NODALP                        engine/source/output/anim/generate/nodalp.F
Chd|        NODALSSP                      engine/source/output/anim/generate/nodalssp.F
Chd|        NODALT                        engine/source/output/anim/generate/nodalt.F
Chd|        NODALVFRAC                    engine/source/output/anim/generate/nodalvfrac.F
Chd|        NODALVOL                      engine/source/output/anim/generate/nodalvol.F
Chd|        NODALZVOL                     engine/source/output/anim/generate/nodalzvol.F
Chd|        NODAL_SCHLIEREN               engine/source/output/anim/generate/nodal_schlieren.F
Chd|        NOISE                         engine/source/general_controls/computation/noise.F
Chd|        OUTPUT_DIV_U                  engine/source/output/anim/generate/output_div_u.F
Chd|        OUTPUT_SCHLIEREN              engine/source/output/anim/generate/output_schlieren.F
Chd|        OUTP_ARSZ_CT                  engine/source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_ARSZ_ST                  engine/source/mpi/interfaces/spmd_outp.F
Chd|        OUTP_C_S                      engine/source/output/sty/outp_c_s.F
Chd|        OUTP_C_T                      engine/source/output/sty/outp_c_t.F
Chd|        OUTP_C_TF                     engine/source/output/sty/outp_c_t.F
Chd|        OUTP_R_S                      engine/source/output/sty/outp_r_s.F
Chd|        OUTP_R_T                      engine/source/output/sty/outp_r_t.F
Chd|        OUTP_SP_S                     engine/source/output/sty/outp_sp_s.F
Chd|        OUTP_SP_T                     engine/source/output/sty/outp_sp_t.F
Chd|        OUTP_SP_TT                    engine/source/output/sty/outp_sp_t.F
Chd|        OUTP_S_S                      engine/source/output/sty/outp_s_s.F
Chd|        OUTP_S_T                      engine/source/output/sty/outp_s_t.F
Chd|        OUTP_S_TT                     engine/source/output/sty/outp_s_t.F
Chd|        PARSORC                       engine/source/output/anim/generate/parsorc.F
Chd|        PARSORF                       engine/source/output/anim/generate/parsorf.F
Chd|        PFORC3                        engine/source/elements/beam/pforc3.F
Chd|        PKE3                          engine/source/elements/beam/pke3.F
Chd|        PNOISE                        engine/source/general_controls/computation/pnoise.F
Chd|        PORFOR5                       engine/source/airbag/porfor5.F
Chd|        PORFORM5                      engine/source/airbag/porfor5.F
Chd|        PRELECFLOW                    engine/source/elements/solid/solide/prelecflow.F
Chd|        PROJECIG3D                    engine/source/elements/ige3d/projecig3d.F
Chd|        Q4KE2                         engine/source/elements/solid_2d/quad4/q4ke2.F
Chd|        R12KE3                        engine/source/elements/spring/r12ke3.F
Chd|        R13KE3                        engine/source/elements/spring/r13ke3.F
Chd|        R23FORC3                      engine/source/elements/spring/r23forc3.F
Chd|        R23LAW108                     engine/source/elements/spring/r23law108.F
Chd|        R23LAW113                     engine/source/elements/spring/r23law113.F
Chd|        R23LAW114                     engine/source/elements/spring/r23law114.F
Chd|        R4KE3                         engine/source/elements/spring/r4ke3.F
Chd|        R8KE3                         engine/source/elements/spring/r8ke3.F
Chd|        RADIATOFF                     engine/source/constraints/thermic/radiatoff.F
Chd|        RBYONF                        engine/source/constraints/general/rbody/rbyonf.F
Chd|        RBYPID                        engine/source/constraints/general/rbody/rbypid.F
Chd|        RBYSENS                       engine/source/constraints/general/rbody/rbyonf.F
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        RFORC3                        engine/source/elements/spring/rforc3.F
Chd|        RGWAL1                        engine/source/ale/grid/rgwal1.F
Chd|        RGWAT2                        engine/source/interfaces/int09/rgwat2.F
Chd|        RGWAT3                        engine/source/interfaces/int09/rgwat3.F
Chd|        RGWATH                        engine/source/interfaces/int09/rgwath.F
Chd|        RUSER32KE3                    engine/source/elements/spring/ruser32ke3.F
Chd|        S10DERIT3                     engine/source/elements/solid/solide10/s10derit3.F
Chd|        S10DERITO3                    engine/source/elements/solid/solide10/s10derito3.F
Chd|        S10KE3                        engine/source/elements/solid/solide10/s10ke3.F
Chd|        S10UPD11T12                   engine/source/elements/solid/solide10/s10upd11t12.F
Chd|        S10VOLNODT3                   engine/source/elements/solid/solide4_sfem/s10volnodt3.F
Chd|        S16SIGP3                      engine/source/elements/thickshell/solide16/s16sigp3.F
Chd|        S20KE3                        engine/source/elements/solid/solide20/s20ke3.F
Chd|        S4ALESFEM                     engine/source/elements/solid/solide4_sfem/s4alesfem.F
Chd|        S4KE3                         engine/source/elements/solid/solide4/s4ke3.F
Chd|        S4LAGSFEM                     engine/source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|        S6CFINT_REG                   engine/source/elements/thickshell/solide6c/s6cfint_reg.F
Chd|        S6CKE3                        engine/source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CFINT_REG                   engine/source/elements/thickshell/solide8c/s8cfint_reg.F
Chd|        S8CKE3                        engine/source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8E_SIGP                      engine/source/elements/solid/solide8e/s8e_sig.F
Chd|        S8FINT3                       engine/source/elements/solid/solide8/s8fint3.F
Chd|        S8FORC3                       engine/source/elements/solid/solide8/s8forc3.F
Chd|        S8FUPD11T12                   engine/source/elements/solid/solide8e/s8fupd11t12.F
Chd|        S8SKE3                        engine/source/elements/solid/solide8s/s8ske3.F
Chd|        S8ZKE3                        engine/source/elements/solid/solide8z/s8zke3.F
Chd|        S8_IS17JAC_I                  engine/source/elements/solid/solide8e/s8_is17jac_i.F
Chd|        SCFINT_REG                    engine/source/elements/thickshell/solidec/scfint_reg.F
Chd|        SCHLIEREN_BUFFER_GATHERING    engine/source/output/anim/generate/schlieren_buffer_gathering.F
Chd|        SCONNECT_OFF                  engine/source/elements/solid/sconnect/sconnect_off.F
Chd|        SEATBELT_REDUCTION_FACTOR     engine/source/tools/seatbelts/seatbelt_reduction_factor.F
Chd|        SEGGETV                       engine/source/interfaces/interf/seggetv.F
Chd|        SHELL_LOCAL_FRAME             engine/source/output/dynain/shell_rota.F
Chd|        SHELL_ROTA                    engine/source/output/dynain/shell_rota.F
Chd|        SIGEPS02G                     engine/source/materials/mat/mat002/sigeps02g.F
Chd|        SIGEPS104                     engine/source/materials/mat/mat104/sigeps104.F
Chd|        SIGEPS104C                    engine/source/materials/mat/mat104/sigeps104c.F
Chd|        SIGEPS105                     engine/source/materials/mat/mat105/sigeps105.F
Chd|        SIGEPS107                     engine/source/materials/mat/mat107/sigeps107.F
Chd|        SIGEPS107C                    engine/source/materials/mat/mat107/sigeps107c.F
Chd|        SIGEPS112                     engine/source/materials/mat/mat112/sigeps112.F
Chd|        SIGEPS112C                    engine/source/materials/mat/mat112/sigeps112c.F
Chd|        SIGEPS121                     engine/source/materials/mat/mat121/sigeps121.F
Chd|        SIGEPS121C                    engine/source/materials/mat/mat121/sigeps121c.F
Chd|        SIGEPS122                     engine/source/materials/mat/mat122/sigeps122.F
Chd|        SIGEPS122C                    engine/source/materials/mat/mat122/sigeps122c.F
Chd|        SIGEPS22G                     engine/source/materials/mat/mat022/sigeps22g.F
Chd|        SIGEPS37_SINGLE_CELL          engine/source/interfaces/int22/sigeps37_single_cell.F
Chd|        SIGEPS51                      engine/source/materials/mat/mat051/sigeps51.F
Chd|        SIGEPS97                      engine/source/materials/mat/mat097/sigeps97.F
Chd|        SIGROTA                       engine/source/output/anim/generate/sigrota.F
Chd|        SIGROTA_XFE                   engine/source/output/anim/generate/sigrota_xfe.F
Chd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
Chd|        SMS_BUILD_MAT_2               engine/source/ams/sms_build_mat_2.F
Chd|        SOLTOSPHA                     engine/source/elements/sph/soltospha.F
Chd|        SOLTOSPHF                     engine/source/elements/sph/soltosph.F
Chd|        SOLTOSPHP                     engine/source/elements/sph/soltosph.F
Chd|        SOLTOSPH_ON1                  engine/source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON12                 engine/source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON2                  engine/source/elements/sph/soltosph_on2.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SPBRM_PRE                     engine/source/implicit/imp_solv.F
Chd|        SPECHAN                       engine/source/elements/sph/spechan.F
Chd|        SPGAUGE                       engine/source/elements/sph/spgauge.F
Chd|        SPHPREP                       engine/source/elements/sph/sphprep.F
Chd|        SPLISSV                       engine/source/elements/sph/splissv.F
Chd|        SPMD_FVB_SWITCH               engine/source/mpi/airbags/spmd_fvb_switch.F
Chd|        SPMD_L11VOIS                  engine/source/mpi/fluid/spmd_cfd.F
Chd|        SPMD_L51VOIS                  engine/source/mpi/fluid/spmd_cfd.F
Chd|        SPONFPRS                      engine/source/elements/sph/sponfprs.F
Chd|        SPONOF1                       engine/source/elements/sph/sponof1.F
Chd|        SPONOF2                       engine/source/elements/sph/sponof2.F
Chd|        SPWFVIS                       engine/source/elements/sph/spwfvis.F
Chd|        STAT_BEAM_MP                  engine/source/output/sta/stat_beam_mp.F
Chd|        STAT_BEAM_SPMD                engine/source/output/sta/stat_beam_spmd.F
Chd|        STAT_BRICK_MP                 engine/source/output/sta/stat_brick_mp.F
Chd|        STAT_BRICK_SPMD               engine/source/output/sta/stat_brick_spmd.F
Chd|        STAT_C_AUXF                   engine/source/output/sta/stat_c_auxf.F
Chd|        STAT_C_EPSPF                  engine/source/output/sta/stat_c_epspf.F
Chd|        STAT_C_FAIL                   engine/source/output/sta/stat_c_fail.F
Chd|        STAT_C_OFF                    engine/source/output/sta/stat_c_off.F
Chd|        STAT_C_ORTH_LOC               engine/source/output/sta/stat_c_orth_loc.F
Chd|        STAT_C_STRAF                  engine/source/output/sta/stat_c_straf.F
Chd|        STAT_C_STRAFG                 engine/source/output/sta/stat_c_strafg.F
Chd|        STAT_C_STRSF                  engine/source/output/sta/stat_c_strsf.F
Chd|        STAT_C_STRSFG                 engine/source/output/sta/stat_c_strsfg.F
Chd|        STAT_C_THK                    engine/source/output/sta/stat_c_thk.F
Chd|        STAT_INIMAP1D_FILE_SPMD       engine/source/output/sta/stat_inimap1d_file_spmd.F
Chd|        STAT_INIMAP1D_SPMD            engine/source/output/sta/stat_inimap1d_spmd.F
Chd|        STAT_INIMAP2D_FILE_SPMD       engine/source/output/sta/stat_inimap2d_file_spmd.F
Chd|        STAT_INIMAP2D_SPMD            engine/source/output/sta/stat_inimap2d_spmd.F
Chd|        STAT_P_AUX                    engine/source/output/sta/stat_p_aux.F
Chd|        STAT_P_FULL                   engine/source/output/sta/stat_p_full.F
Chd|        STAT_QUAD_MP                  engine/source/output/sta/stat_quad_mp.F
Chd|        STAT_QUAD_SPMD                engine/source/output/sta/stat_quad_spmd.F
Chd|        STAT_R_FULL                   engine/source/output/sta/stat_r_full.F
Chd|        STAT_SHEL_MP                  engine/source/output/sta/stat_shel_mp.F
Chd|        STAT_SHEL_SPMD                engine/source/output/sta/stat_shel_spmd.F
Chd|        STAT_SIZE_C                   engine/source/output/sta/stat_size.F
Chd|        STAT_SPRING_MP                engine/source/output/sta/stat_spring_mp.F
Chd|        STAT_SPRING_SPMD              engine/source/output/sta/stat_spring_spmd.F
Chd|        STAT_S_AUXF                   engine/source/output/sta/stat_s_auxf.F
Chd|        STAT_S_EREF                   engine/source/output/sta/stat_s_eref.F
Chd|        STAT_S_FAIL                   engine/source/output/sta/stat_s_fail.F
Chd|        STAT_S_ORTHO                  engine/source/output/sta/stat_s_ortho.F
Chd|        STAT_S_STRAF                  engine/source/output/sta/stat_s_straf.F
Chd|        STAT_S_STRSF                  engine/source/output/sta/stat_s_strsf.F
Chd|        STAT_TRUSS_MP                 engine/source/output/sta/stat_truss_mp.F
Chd|        STAT_TRUSS_SPMD               engine/source/output/sta/stat_truss_spmd.F
Chd|        STAT_T_FULL                   engine/source/output/sta/stat_t_full.F
Chd|        STRN_TENSCOR3                 engine/source/output/h3d/h3d_results/h3d_strn_tenscor3.F
Chd|        STRS_TENSCOR3                 engine/source/output/h3d/h3d_results/strs_tenscor3.F
Chd|        SUFORC3                       engine/source/user_interface/suforc3.F
Chd|        SUSER43                       engine/source/elements/solid/sconnect/suser43.F
Chd|        SWITCH_TO_DTNODA              engine/source/time_step/switch_to_dtnoda.F
Chd|        SZHOUR3                       engine/source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    engine/source/elements/solid/solidez/szhour3_or.F
Chd|        S_USER                        engine/source/output/sty/s_user.F
Chd|        TAGOFF3N                      engine/source/interfaces/interf/chkstfn3.F
Chd|        TENCGPS1                      engine/source/output/anim/generate/tensorc.F
Chd|        TENCGPS2                      engine/source/output/anim/generate/tensorc.F
Chd|        TENSGPS1                      engine/source/output/anim/generate/tensor6.F
Chd|        TENSGPS2                      engine/source/output/anim/generate/tensor6.F
Chd|        TENSGPS3                      engine/source/output/anim/generate/tensor6.F
Chd|        TENSGPSTRAIN                  engine/source/output/anim/generate/tensgpstrain.F
Chd|        TENSGPS_SKIN                  engine/source/output/anim/generate/tensor6.F
Chd|        TENSOR0                       engine/source/output/anim/generate/tensor0.F
Chd|        TENSORC                       engine/source/output/anim/generate/tensorc.F
Chd|        TENSORC_CRK                   engine/source/output/anim/generate/tensorc_crk.F
Chd|        TENSORC_PLY                   engine/source/output/anim/generate/tensorc_ply.F
Chd|        TENSORS                       engine/source/output/anim/generate/tensor6.F
Chd|        TFORC3                        engine/source/elements/truss/tforc3.F
Chd|        THCLUSTER                     engine/source/output/th/thcluster.F
Chd|        THCOQ                         engine/source/output/th/thcoq.F
Chd|        THERMEXPC                     engine/source/materials/mat_share/thermexpc.F
Chd|        THERMEXPPI                    engine/source/elements/beam/thermexpp.F
Chd|        THICKVAR                      engine/source/elements/shell/coque/thickvar.F
Chd|        THNST                         engine/source/output/th/thnst.F
Chd|        THPOUT                        engine/source/output/th/thpout.F
Chd|        THQUAD                        engine/source/output/th/thquad.F
Chd|        THRES                         engine/source/output/th/thres.F
Chd|        THRES_COUNT                   engine/source/output/th/thres_count.F
Chd|        THSOL                         engine/source/output/th/thsol.F
Chd|        THSPH                         engine/source/output/th/thsph.F
Chd|        THTRUS                        engine/source/output/th/thtrus.F
Chd|        TKE3                          engine/source/elements/truss/tke3.F
Chd|        TM_DMGL25_SHELL               engine/source/output/outmaxsubr.F
Chd|        TM_DMG_SHELLS                 engine/source/output/outmaxsubr.F
Chd|        TM_DMG_SOLID                  engine/source/output/outmaxsubr.F
Chd|        TM_SEQ_SHELL                  engine/source/output/outmaxsubr.F
Chd|        TM_SEQ_SOLID                  engine/source/output/outmaxsubr.F
Chd|        TM_SIG_SHELL                  engine/source/output/outmaxsubr.F
Chd|        TM_SIG_SOLID                  engine/source/output/outmaxsubr.F
Chd|        TM_STRA_SHELL                 engine/source/output/outmaxsubr.F
Chd|        TM_STRA_SOLID                 engine/source/output/outmaxsubr.F
Chd|        UPDATE_SLIPRING               engine/source/tools/seatbelts/update_slipring.F
Chd|        UPD_TMAX                      engine/source/output/upd_outmax.F
Chd|        UPENRIC3_N3                   engine/source/elements/xfem/upenric3_nx.F
Chd|        UPENRIC3_N4                   engine/source/elements/xfem/upenric3_nx.F
Chd|        UPOFFC                        engine/source/elements/xfem/upoffc.F
Chd|        UPOFFTG                       engine/source/elements/xfem/upofftg.F
Chd|        UPXFEM1                       engine/source/elements/xfem/upxfem1.F
Chd|        UPXFEM_TAGXP                  engine/source/elements/xfem/upxfem_tagxp.F
Chd|        USERMAT_SHELL                 engine/source/materials/mat_share/usermat_shell.F
Chd|        VELVECC22                     engine/source/output/anim/generate/velvec.F
Chd|        VELVECZ22                     engine/source/output/anim/generate/velvecz22.F
Chd|        VOLN22                        engine/source/interfaces/int22/voln22.F
Chd|        VOLPVGB                       engine/source/airbag/volpvg.F 
Chd|        WRITE_BUF_LAW51               engine/source/materials/mat/mat051/write_buf_law51.F
Chd|        WRITE_CUT_CELL_BUFFER         engine/source/interfaces/int22/write_cut_cell_buffer.F
Chd|        W_ELBUF_STR                   engine/source/elements/elbuf/w_elbuf_str.F
Chd|        XFEOFF                        engine/source/elements/xfem/xfeoff.F
Chd|        XFORC3                        engine/source/elements/xelem/xforc3.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        MULTI_MUSCL_FLUXES_COMPUTATIONengine/source/multifluid/multi_muscl_fluxes_computation.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ELBUFDEF_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c define type ELBUF_STRUCT_ for element buffer structure array
c=======================================================================      
c
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
C
        integer  G_FORPGPINCH  
        integer  G_MOMPGPINCH 
        integer  G_EPGPINCHXZ
        integer  G_EPGPINCHYZ 
        integer  G_EPGPINCHZZ
C    
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
c---
C    - 1D - elem (TRUSS, BEAM, SPRING) 
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
C
C    -  for seatbelt elements
        integer  G_SLIPRING_ID
        integer  G_SLIPRING_FRAM_ID
        integer  G_SLIPRING_STRAND
        integer  G_RETRACTOR_ID
        integer  G_RINGSLIP
        integer  G_ADD_NODE
        integer  G_UPDATE
        integer  G_FRAM_FACTOR
        integer  G_INTVAR
c---
        integer  G_DT_PITER
c---
        integer  G_IDT_TSH
c-------  max_historic variables     
        integer  G_TM_YIELD
        integer  G_TM_SEQ
        integer  G_TM_EINT
        integer  G_TM_DMG
        integer  G_TM_SIG  ! max(p1)&min(p3) 3 for 2D, 6 for 3D
        integer  G_TM_STRA ! max(p1)&min(p3)
c---
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
C
        my_real, DIMENSION(:) , POINTER ::   FORPGPINCH 
        my_real, DIMENSION(:) , POINTER ::   MOMPGPINCH
        my_real, DIMENSION(:) , POINTER ::   EPGPINCHXZ
        my_real, DIMENSION(:) , POINTER ::   EPGPINCHYZ
        my_real, DIMENSION(:) , POINTER ::   EPGPINCHZZ
C                        
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
        my_real, DIMENSION(:) , POINTER ::   BPRELD  ! Bolt preloading 
        my_real, DIMENSION(:) , POINTER ::   COR_NF  ! Corotational nodal forces 
        my_real, DIMENSION(:) , POINTER ::   COR_FR  ! Corotational frame 
        my_real, DIMENSION(:) , POINTER ::   COR_XR  ! Corotational reference coordinates 
        my_real, DIMENSION(:) , POINTER ::   MAXFRAC
        my_real, DIMENSION(:) , POINTER ::   MAXEPS 
        my_real, DIMENSION(:) , POINTER ::   BETAORTH 
        my_real, DIMENSION(:) , POINTER ::   AMU
c---
C    - 1D - elem (TRUSS, BEAM, SPRING)
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
C
C    -  for seatbelt elements
        integer, DIMENSION(:) , POINTER ::   SLIPRING_ID
        integer, DIMENSION(:) , POINTER ::   SLIPRING_FRAM_ID
        integer, DIMENSION(:) , POINTER ::   SLIPRING_STRAND
        integer, DIMENSION(:) , POINTER ::   RETRACTOR_ID
        my_real, DIMENSION(:) , POINTER ::   RINGSLIP
        integer, DIMENSION(:) , POINTER ::   ADD_NODE
        integer, DIMENSION(:) , POINTER ::   UPDATE
        my_real, DIMENSION(:) , POINTER ::   FRAM_FACTOR
        my_real, DIMENSION(:) , POINTER ::   INTVAR
c---
        my_real, DIMENSION(:) , POINTER ::   DT_PITER ! Tetra10 Iterative Power for Time Step Computation
        integer, DIMENSION(:) , POINTER ::   IDT_TSH  
c-------  max_historic variables     
        my_real, DIMENSION(:) , POINTER ::   TM_YIELD   
        my_real, DIMENSION(:) , POINTER ::   TM_SEQ   
        my_real, DIMENSION(:) , POINTER ::   TM_EINT   
        my_real, DIMENSION(:) , POINTER ::   TM_DMG   
        my_real, DIMENSION(:) , POINTER ::   TM_SIG1
        my_real, DIMENSION(:) , POINTER ::   TM_STRA1
        my_real, DIMENSION(:) , POINTER ::   TM_SIG3
        my_real, DIMENSION(:) , POINTER ::   TM_STRA3
c---  work array
        my_real, DIMENSION(:) , POINTER ::   TM_PSIG
        my_real, DIMENSION(:) , POINTER ::   TM_PSTRA
c---
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
!--------------------------------------------------------------------------------

      TYPE BUF_EOS_
        my_real, DIMENSION(:)  , POINTER ::  VAR 
      END TYPE BUF_EOS_

      TYPE BUF_PORO_
        my_real, DIMENSION(:)  , POINTER ::  VAR 
      END TYPE BUF_PORO_

      TYPE BUF_VISC_
c        integer  ILAW    ! type de loi de viscosite
c        integer  NVAR
        my_real, DIMENSION(:)  , POINTER ::  VAR 
      END TYPE BUF_VISC_

      TYPE BUF_XFEM_       ! Buffer des elements XFEM crees par la fissuration
c-------  layer variables     
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
c-------
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
c-------
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
c-------  layer variables     
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
c-------       
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
c
c--------------------       
c 
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
c------ interlayer variables par couche   (length=1) 
        my_real, DIMENSION(:) , POINTER ::   EINT         
        my_real, DIMENSION(:) , POINTER ::   COUNT
      
        TYPE (BUF_INTLOC_) , DIMENSION(:,:) , POINTER :: ILBUF
        TYPE (BUF_MAT_)    , DIMENSION(:,:) , POINTER :: MAT
        TYPE (BUF_FAIL_)   , DIMENSION(:,:) , POINTER :: FAIL  
      END TYPE BUF_INTLAY_
c--------------------       
 
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
        TYPE (BUF_NLOC_)  , DIMENSION(:,:) , POINTER :: NLOC   ! NLOC (NPTR,NPTS)
        TYPE (BUF_NLOCTS_), DIMENSION(:,:) , POINTER :: NLOCTS ! NLOC (NPTR,NPTS)
      END TYPE ELBUF_STRUCT_
c
c---------------
      END MODULE ELBUFDEF_MOD
