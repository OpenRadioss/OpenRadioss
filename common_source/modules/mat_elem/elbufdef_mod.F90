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
!Chd|====================================================================
!Chd|  elbufdef_mod                  modules/mat_elem/elbufdef_mod.f
!Chd|-- called by -----------
!Chd|        mat_elem_mod                  common_source/modules/mat_elem/mat_elem_mod.f
!Chd|        multi_fvm_mod                 common_source/modules/ale/multi_fvm_mod.f
!Chd|        allocbuf_auto                 starter/source/elements/elbuf_init/allocbuf_auto.f
!Chd|        anioff0                       starter/source/output/anim/anioff0.f
!Chd|        anioffc                       starter/source/output/anim/anioffc.f
!Chd|        aniofff                       starter/source/output/anim/aniofff.f
!Chd|        anioffs                       starter/source/output/anim/anioffs.f
!Chd|        aniskew                       starter/source/output/anim/aniskew.f
!Chd|        binit2                        starter/source/ale/bimat/binit2.f
!Chd|        bsigini                       starter/source/elements/beam/bsigini.f
!Chd|        bulkfakeigeo3                 starter/source/elements/ige3d/bulkfakeigeo3.f
!Chd|        buserini                      starter/source/elements/beam/buserini.f
!Chd|        c3epsini                      starter/source/elements/sh3n/coque3n/c3epsini.f
!Chd|        c3fint_reg_ini                starter/source/elements/sh3n/coque3n/c3fint_reg_ini.f
!Chd|        c3inmas                       starter/source/elements/sh3n/coque3n/c3inmas.f
!Chd|        cbafint_reg_ini               starter/source/elements/shell/coqueba/cbafint_reg_ini.f
!Chd|        cbainit3                      starter/source/elements/shell/coqueba/cbainit3.f
!Chd|        cbufxfe                       starter/source/elements/xfem/cbufxfe.f
!Chd|        cdkepsini                     starter/source/elements/sh3n/coquedk/cdkepsini.f
!Chd|        cdkfint_reg_ini               starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.f
!Chd|        cdkinit3                      starter/source/elements/sh3n/coquedk/cdkinit3.f
!Chd|        cepsini                       starter/source/elements/shell/coque/cepsini.f
!Chd|        cfailini                      starter/source/elements/shell/coque/cfailini.f
!Chd|        cfailini4                     starter/source/elements/shell/coque/cfailini.f
!Chd|        cfint_reg_ini                 starter/source/elements/shell/coque/cfint_reg_ini.f
!Chd|        chk_dttsh                     starter/source/elements/thickshell/solidec/scdtchk3.f
!Chd|        cinit3                        starter/source/elements/shell/coque/cinit3.f
!Chd|        cinmas                        starter/source/elements/shell/coque/cinmas.f
!Chd|        cm27in3                       starter/source/materials/mat/mat027/cm27in3.f
!Chd|        cm35in3                       starter/source/materials/mat/mat035/cm35in3.f
!Chd|        cmaini3                       starter/source/elements/sh3n/coquedk/cmaini3.f
!Chd|        cmatini                       starter/source/materials/mat_share/cmatini.f
!Chd|        cmatini4                      starter/source/materials/mat_share/cmatini4.f
!Chd|        cmlawi                        starter/source/elements/shell/coque/cepsini.f
!Chd|        cnepsini                      starter/source/elements/shell/coqueba/cnepsini.f
!Chd|        cnloc_mat104_ini              starter/source/materials/mat/mat104/cnloc_mat104_ini.f
!Chd|        cnloc_matini                  starter/source/materials/mat_share/cnloc_matini.f
!Chd|        corth3                        starter/source/elements/shell/coque/corth3.f
!Chd|        corthdir                      starter/source/elements/shell/coque/corthdir.f
!Chd|        corthini                      starter/source/elements/shell/coque/corthini.f
!Chd|        csigini                       starter/source/elements/shell/coque/csigini.f
!Chd|        csigini4                      starter/source/elements/shell/coqueba/scigini4.f
!Chd|        cuserini                      starter/source/elements/shell/coque/cuserini.f
!Chd|        cuserini4                     starter/source/elements/shell/coqueba/cuserini4.f
!Chd|        c_seatbelts                   starter/source/restart/ddsplit/c_seatbelts.f
!Chd|        deallocate_elbuf              starter/source/elements/elbuf_init/deallocate_buffer.f
!Chd|        deallocate_one_element_group  starter/source/elements/elbuf_init/deallocate_one_element_group.f
!Chd|        dfunc0                        starter/source/output/anim/dfunc0.f
!Chd|        dfuncc                        starter/source/output/anim/dfuncc.f
!Chd|        dfuncf                        starter/source/output/anim/dfuncf.f
!Chd|        dfuncs                        starter/source/output/anim/dfuncs.f
!Chd|        dmasani0                      starter/source/output/anim/dmasani0.f
!Chd|        dmasanic                      starter/source/output/anim/dmasanic.f
!Chd|        dmasanif                      starter/source/output/anim/dmasanif.f
!Chd|        dmasanis                      starter/source/output/anim/dmasanis.f
!Chd|        dtmain                        starter/source/materials/time_step/dtmain.f
!Chd|        failini                       starter/source/elements/solid/solide/failini.f
!Chd|        genani1                       starter/source/output/anim/genani1.f
!Chd|        ig3dinit3                     starter/source/elements/ige3d/ig3dinit3.f
!Chd|        inicrkfill                    starter/source/elements/xfem/inicrkfill.f
!Chd|        iniebcsp                      starter/source/boundary_conditions/ebcs/iniebcsp.f
!Chd|        iniebcsp0                     starter/source/boundary_conditions/ebcs/iniebcsp0.f
!Chd|        iniebcs_dp                    starter/source/boundary_conditions/ebcs/iniebcs_dp.f
!Chd|        iniebcs_nrf_tcar              starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.f
!Chd|        inifill                       starter/source/initial_conditions/inivol/inifill.f
!Chd|        inigrav_eos                   starter/source/initial_conditions/inigrav/inigrav_eos.f
!Chd|        inigrav_load                  starter/source/initial_conditions/inigrav/inigrav_load.f
!Chd|        inigrav_m37                   starter/source/initial_conditions/inigrav/inigrav_m37.f
!Chd|        inigrav_m51                   starter/source/initial_conditions/inigrav/inigrav_m51.f
!Chd|        inintr_thkvar                 starter/source/interfaces/interf1/inintr_thkvar.f
!Chd|        iniphase                      starter/source/initial_conditions/inivol/iniphase.f
!Chd|        inirig_mat                    starter/source/elements/initia/inirig_mat.f
!Chd|        initvars_auto                 starter/source/elements/elbuf_init/initvars_auto.f
!Chd|        inivoid                       starter/source/elements/initia/inivoid.f
!Chd|        inivol_set                    starter/source/initial_conditions/inivol/inivol_set.f
!Chd|        ini_inimap1d                  starter/source/initial_conditions/inimap/ini_inimap1d.f
!Chd|        ini_inimap2d                  starter/stub/ini_inimap2d.f   
!Chd|        ini_outmax_auto               starter/source/elements/elbuf_init/ini_outmax_auto.f
!Chd|        ini_seatbelt                  starter/source/tools/seatbelts/ini_seatbelt.f
!Chd|        layini1                       starter/source/elements/shell/coqueba/layini1.f
!Chd|        layini_xfe                    starter/source/elements/xfem/cbufxfe.f
!Chd|        lectur                        starter/source/starter/lectur.f
!Chd|        lslocal                       starter/source/elements/xfem/lslocal.f
!Chd|        m20dcod                       starter/source/system/fsdcod.f
!Chd|        m37init                       starter/source/materials/mat/mat037/m37init.f
!Chd|        m51init                       starter/source/materials/mat/mat051/m51init.f
!Chd|        matini                        starter/source/materials/mat_share/matini.f
!Chd|        multifluid_global_tdet        starter/source/multifluid/multifluid_global_tdet.f
!Chd|        multifluid_init2              starter/source/multifluid/multifluid_init2.f
!Chd|        multifluid_init2t             starter/source/multifluid/multifluid_init2t.f
!Chd|        multifluid_init3              starter/source/multifluid/multifluid_init3.f
!Chd|        multifluid_init3t             starter/source/multifluid/multifluid_init3t.f
!Chd|        nlocal_init_sta               starter/source/materials/fail/nlocal_init_sta.f
!Chd|        nloc_dmg_init                 starter/source/materials/fail/nloc_dmg_init.f
!Chd|        parsorc                       starter/source/output/anim/parsorc.f
!Chd|        pinit3                        starter/source/elements/beam/pinit3.f
!Chd|        preinicrk3n                   starter/source/elements/xfem/preinicrk3n.f
!Chd|        preinicrk4n                   starter/source/elements/xfem/preinicrk4n.f
!Chd|        q4init2                       starter/source/elements/solid_2d/quad4/q4init2.f
!Chd|        qinit2                        starter/source/elements/solid_2d/quad/qinit2.f
!Chd|        rinit3                        starter/source/elements/spring/rinit3.f
!Chd|        s10deri3                      starter/source/elements/solid/solide10/s10deri3.f
!Chd|        s10init3                      starter/source/elements/solid/solide10/s10init3.f
!Chd|        s10jaci3                      starter/source/elements/solid/solide10/s10jaci3.f
!Chd|        s16init3                      starter/source/elements/thickshell/solide16/s16init3.f
!Chd|        s20init3                      starter/source/elements/solid/solide20/s20init3.f
!Chd|        s4init3                       starter/source/elements/solid/solide4/s4init3.f
!Chd|        s6cinit3                      starter/source/elements/thickshell/solide6c/s6cinit3.f
!Chd|        s8cinit3                      starter/source/elements/thickshell/solide8c/s8cinit3.f
!Chd|        s8e_pij                       starter/source/elements/solid/solide8z/s8zderi3.f
!Chd|        s8zinit3                      starter/source/elements/solid/solide8z/s8zinit3.f
!Chd|        scaleini                      starter/source/elements/initia/scaleini.f
!Chd|        scinit3                       starter/source/elements/thickshell/solidec/scinit3.f
!Chd|        seteloff                      starter/source/constraints/general/rbody/hm_read_rbody.f
!Chd|        sigin3b                       starter/source/elements/solid/solid8p/sigin3b.f
!Chd|        sini43                        starter/source/elements/solid/sconnect/sini43.f
!Chd|        sinit3                        starter/source/elements/solid/solide/sinit3.f
!Chd|        sms_auto_dt                   starter/source/ams/sms_auto_dt.f
!Chd|        spinit3                       starter/source/elements/sph/spinit3.f
!Chd|        suinit3                       starter/source/elements/elbuf_init/suinit3.f
!Chd|        tensor0                       starter/source/output/anim/tensor0.f
!Chd|        tensorc                       starter/source/output/anim/tensorc.f
!Chd|        tensors                       starter/source/output/anim/tensors.f
!Chd|        thickvar                      starter/source/interfaces/interf1/inintr_thkvar.f
!Chd|        thick_ilev                    starter/source/elements/xfem/thick_ilev.f
!Chd|        tinit3                        starter/source/elements/truss/tinit3.f
!Chd|        w_elbuf_str                   starter/source/restart/ddsplit/w_elbuf_str.f
!Chd|        xinit3                        starter/source/elements/xelem/xinit3.f
!Chd|        zerovars_auto                 starter/source/elements/elbuf_init/zerovars_auto.f
!Chd|        a22conv3                      engine/source/ale/alefvm/cut_cells/a22conv3.f
!Chd|        aconve                        engine/source/ale/aconve.f    
!Chd|        admdiv                        engine/source/model/remesh/admdiv.f
!Chd|        admerr                        engine/source/model/remesh/admerr.f
!Chd|        admgvid                       engine/source/model/remesh/admgvid.f
!Chd|        admini                        engine/source/model/remesh/admini.f
!Chd|        admmap3                       engine/source/model/remesh/admmap3.f
!Chd|        admmap4                       engine/source/model/remesh/admmap4.f
!Chd|        admregul                      engine/source/model/remesh/admregul.f
!Chd|        admthke                       engine/source/model/remesh/admthke.f
!Chd|        aeturb                        engine/source/ale/turbulence/aeturb.f
!Chd|        aflux0                        engine/source/ale/aflux0.f    
!Chd|        aflux3_int22_fvm              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.f
!Chd|        afluxt                        engine/source/ale/ale51/afluxt.f
!Chd|        agauge                        engine/source/ale/agauge.f    
!Chd|        agrad0                        engine/source/ale/agrad0.f    
!Chd|        airbagb1                      engine/source/airbag/airbagb1.f
!Chd|        akturb                        engine/source/ale/turbulence/akturb.f
!Chd|        ale51_antidiff3_int22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.f
!Chd|        ale51_finish                  engine/source/ale/ale51/ale51_finish.f
!Chd|        ale51_gradient_reconstruction engine/source/ale/alemuscl/ale51_gradient_reconstruction.f
!Chd|        ale51_init                    engine/source/ale/ale51/ale51_init.f
!Chd|        ale51_upwind3_int22           engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.f
!Chd|        aleflow                       engine/source/ale/porous/aleflow.f
!Chd|        alefvm_main                   engine/source/ale/alefvm/alefvm_main.f
!Chd|        alefvm_stress_int22           engine/source/ale/alefvm/alefvm_stress_int22.f
!Chd|        alethe                        engine/source/ale/alethe.f    
!Chd|        alew6                         engine/source/ale/grid/alew6.f
!Chd|        alewdx                        engine/source/ale/grid/alewdx.f
!Chd|        allocbuf_auto                 engine/source/elements/elbuf/allocbuf_auto.f
!Chd|        alloc_elbuf_imp               engine/source/elements/elbuf/alloc_elbuf_imp.f
!Chd|        animig3d                      engine/source/output/anim/generate/animig3d.f
!Chd|        animx                         engine/source/output/anim/generate/animx.f
!Chd|        anim_nodal_p_elems            engine/source/output/anim/generate/anim_nodal_p_elems.f
!Chd|        anioff0                       engine/source/output/anim/generate/anioff0.f
!Chd|        anioffc                       engine/source/output/anim/generate/anioffc.f
!Chd|        anioffc_crk                   engine/source/output/anim/generate/anioffc_crk.f
!Chd|        anioffc_ply                   engine/source/output/anim/generate/anioffc_ply.f
!Chd|        aniofff                       engine/source/output/anim/generate/aniofff.f
!Chd|        anioffs                       engine/source/output/anim/generate/anioff6.f
!Chd|        aniskew                       engine/source/output/anim/generate/aniskew.f
!Chd|        arezon                        engine/source/ale/arezon.f    
!Chd|        atherm                        engine/source/ale/atherm.f    
!Chd|        bforc2                        engine/source/ale/bimat/bforc2.f
!Chd|        c3coork3                      engine/source/elements/sh3n/coque3n/c3coork3.f
!Chd|        c3evec3                       engine/source/elements/sh3n/coque3n/c3evec3.f
!Chd|        c3fint_reg                    engine/source/elements/sh3n/coque3n/c3fint_reg.f
!Chd|        c3ke3                         engine/source/elements/sh3n/coque3n/c3ke3.f
!Chd|        cbacoor                       engine/source/elements/shell/coqueba/cbacoor.f
!Chd|        cbacoork                      engine/source/elements/shell/coqueba/cbacoork.f
!Chd|        cbacoort                      engine/source/elements/shell/coqueba/cbacoor.f
!Chd|        cbafint_reg                   engine/source/elements/shell/coqueba/cbafint_reg.f
!Chd|        cbake3                        engine/source/elements/shell/coqueba/cbake3.f
!Chd|        cbal58warp                    engine/source/elements/shell/coqueba/cbawarpoff.f
!Chd|        cbapinchproj                  engine/source/elements/shell/coqueba/cbapinchproj.f
!Chd|        cdk6coor3                     engine/source/elements/sh3n/coquedk6/cdk6coor3.f
!Chd|        cdk6fint_reg                  engine/source/elements/sh3n/coquedk6/cdk6fint_reg.f
!Chd|        cdkcoor3                      engine/source/elements/sh3n/coquedk/cdkcoor3.f
!Chd|        cdkfint_reg                   engine/source/elements/sh3n/coquedk/cdkfint_reg.f
!Chd|        cevec3                        engine/source/elements/shell/coque/cevec3.f
!Chd|        cfint_reg                     engine/source/elements/shell/coque/cfint_reg.f
!Chd|        cgshell3                      engine/source/implicit/cgshell.f
!Chd|        cgshell4                      engine/source/implicit/cgshell.f
!Chd|        check_ale_comm                engine/source/ale/check_ale_comm.f
!Chd|        chkstfn3n                     engine/source/interfaces/interf/chkstfn3.f
!Chd|        clusterf                      engine/source/output/cluster/clusterf.f
!Chd|        cmain3pinch                   engine/source/elements/shell/coqueba/cmain3pinch.f
!Chd|        cmatc3                        engine/source/elements/shell/coqueba/cmatc3.f
!Chd|        cncoefort                     engine/source/elements/sh3n/coquedk/cncoef3.f
!Chd|        cnvec3                        engine/source/elements/shell/coque/cnvec3.f
!Chd|        convecoff                     engine/source/constraints/thermic/convecoff.f
!Chd|        copy_elbuf                    engine/source/elements/elbuf/copy_elbuf.f
!Chd|        copy_elbuf_1                  engine/source/elements/elbuf/copy_elbuf_1.f
!Chd|        cortdir3                      engine/source/elements/shell/coque/cortdir3.f
!Chd|        count_arsz_ct                 engine/source/output/sty/outp_c_t.f
!Chd|        count_arsz_st                 engine/source/output/sty/outp_s_t.f
!Chd|        cp_impbuf                     engine/source/implicit/produt_v.f
!Chd|        crklayer4n_adv                engine/source/elements/xfem/crklayer4n_adv.f
!Chd|        crklayer4n_ini                engine/source/elements/xfem/crklayer4n_ini.f
!Chd|        crkoffc                       engine/source/elements/xfem/precrklay.f
!Chd|        crkofftg                      engine/source/elements/xfem/precrklay.f
!Chd|        cutfunce                      engine/source/tools/sect/cutfunce.f
!Chd|        czcoork3                      engine/source/elements/shell/coquez/czcoork3.f
!Chd|        czcorc1                       engine/source/elements/shell/coquez/czcorc.f
!Chd|        czcorcht                      engine/source/elements/shell/coquez/czcorc.f
!Chd|        czcorct                       engine/source/elements/shell/coquez/czcorc.f
!Chd|        czke3                         engine/source/elements/shell/coquez/czke3.f
!Chd|        czkel3                        engine/source/elements/shell/coquez/czkel3.f
!Chd|        c_tf_ne                       engine/source/output/sty/c_tf_ne.f
!Chd|        deltax22                      engine/source/interfaces/int22/deltax22.f
!Chd|        desacti                       engine/source/elements/desacti.f
!Chd|        dfunc0                        engine/source/output/anim/generate/dfunc0.f
!Chd|        dfuncc                        engine/source/output/anim/generate/dfuncc.f
!Chd|        dfuncc_crk                    engine/source/output/anim/generate/dfuncc_crk.f
!Chd|        dfuncc_ply                    engine/source/output/anim/generate/dfuncc_ply.f
!Chd|        dfuncf                        engine/source/output/anim/generate/dfuncf.f
!Chd|        dfungps1                      engine/source/output/anim/generate/dfuncf.f
!Chd|        dfungps2                      engine/source/output/anim/generate/dfuncf.f
!Chd|        dim_elemax                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_elems1                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_elems2                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_elems3                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_elems4                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_elemsp                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_fr_k                      engine/source/mpi/implicit/imp_fri.f
!Chd|        dim_glob_k                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_kinmax                    engine/source/implicit/ind_glob_k.f
!Chd|        dim_tshedg                    engine/source/elements/thickshell/solidec/dim_tshedg.f
!Chd|        dmasani0                      engine/source/output/anim/generate/dmasani0.f
!Chd|        dmasanic                      engine/source/output/anim/generate/dmasanic.f
!Chd|        dmasanif                      engine/source/output/anim/generate/dmasanif.f
!Chd|        dmasanis                      engine/source/output/anim/generate/dmasani6.f
!Chd|        dynain_c_strag                engine/source/output/dynain/dynain_c_strag.f
!Chd|        dynain_c_strsg                engine/source/output/dynain/dynain_c_strsg.f
!Chd|        dynain_shel_mp                engine/source/output/dynain/dynain_shel_mp.f
!Chd|        dynain_shel_spmd              engine/source/output/dynain/dynain_shel_spmd.f
!Chd|        dynain_size_c                 engine/source/output/dynain/dynain_size.f
!Chd|        ebcs0                         engine/source/boundary_conditions/ebcs/ebcs0.f
!Chd|        ebcs10                        engine/source/boundary_conditions/ebcs/ebcs10.f
!Chd|        ebcs_main                     engine/source/boundary_conditions/ebcs/ebcs_main.f
!Chd|        eflux3_int22_fvm              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.f
!Chd|        eig                           engine/stub/eig.f             
!Chd|        eig1                          engine/stub/eig1.f            
!Chd|        eigcond                       engine/stub/eigcond.f         
!Chd|        eigoff                        engine/source/output/anim/generate/eigoff.f
!Chd|        eigp                          engine/stub/eigp.f            
!Chd|        eloff                         engine/source/elements/eloff.f
!Chd|        enrichc_ini                   engine/source/elements/xfem/enrichc_ini.f
!Chd|        enrichtg_ini                  engine/source/elements/xfem/enrichtg_ini.f
!Chd|        err_thk                       engine/source/elements/shell/err_thk.f
!Chd|        fail_gene1_s                  engine/source/materials/fail/gene1/fail_gene1_s.f
!Chd|        forints                       engine/source/elements/forints.f
!Chd|        fvbag0                        engine/source/airbag/fvbag0.f 
!Chd|        fvbag1                        engine/source/airbag/fvbag1.f 
!Chd|        fvbag2                        engine/source/airbag/fvbag2.f 
!Chd|        fvvent0                       engine/source/airbag/fvvent0.f
!Chd|        fv_up_switch                  engine/source/airbag/fv_up_switch.f
!Chd|        fxbodfp1                      engine/source/constraints/fxbody/fxbodfp.f
!Chd|        fxbsgmaj                      engine/source/constraints/fxbody/fxbsgmaj.f
!Chd|        fxbyfor                       engine/source/constraints/fxbody/fxbyfor.f
!Chd|        fxbypid                       engine/source/constraints/fxbody/fxbypid.f
!Chd|        genani                        engine/source/output/anim/generate/genani.f
!Chd|        gendynain                     engine/source/output/dynain/gendynain.f
!Chd|        genh3d                        engine/source/output/h3d/h3d_results/genh3d.f
!Chd|        genoutp                       engine/source/output/sty/genoutp.f
!Chd|        get_nodal_ipart               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.f
!Chd|        get_q4lsys                    engine/source/output/sta/sta_c_get_q4lsys.f
!Chd|        get_t3lsys                    engine/source/output/sta/sta_c_get_t3lsys.f
!Chd|        gpsstrain_skin                engine/source/output/anim/generate/tensgpstrain.f
!Chd|        gpstra_solid                  engine/source/output/outmaxsubr.f
!Chd|        gps_solid                     engine/source/output/outmaxsubr.f
!Chd|        h3d_fld_strain                engine/source/output/h3d/h3d_results/h3d_fld_strain.f
!Chd|        h3d_fld_tsh                   engine/source/output/h3d/h3d_results/h3d_fld_tsh.f
!Chd|        h3d_nodal_scalar              engine/source/output/h3d/h3d_results/h3d_nodal_scalar.f
!Chd|        h3d_nodal_tensor              engine/source/output/h3d/h3d_results/h3d_nodal_tensor.f
!Chd|        h3d_nodal_vector              engine/source/output/h3d/h3d_results/h3d_nodal_vector.f
!Chd|        h3d_oned_off                  engine/source/output/h3d/spmd/spmd_h3d_oned_off.f
!Chd|        h3d_oned_scalar               engine/source/output/h3d/h3d_results/h3d_oned_scalar.f
!Chd|        h3d_oned_tensor               engine/source/output/h3d/h3d_results/h3d_oned_tensor.f
!Chd|        h3d_oned_vector               engine/source/output/h3d/h3d_results/h3d_oned_vector.f
!Chd|        h3d_quad_off                  engine/source/output/h3d/spmd/spmd_h3d_quad_off.f
!Chd|        h3d_quad_scalar               engine/source/output/h3d/h3d_results/h3d_quad_scalar.f
!Chd|        h3d_quad_tensor               engine/source/output/h3d/h3d_results/h3d_quad_tensor.f
!Chd|        h3d_quad_vector               engine/source/output/h3d/h3d_results/h3d_quad_vector.f
!Chd|        h3d_shell_off                 engine/source/output/h3d/spmd/spmd_h3d_shell_off.f
!Chd|        h3d_shell_scalar              engine/source/output/h3d/h3d_results/h3d_shell_scalar.f
!Chd|        h3d_shell_scalar_1            engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.f
!Chd|        h3d_shell_tensor              engine/source/output/h3d/h3d_results/h3d_shell_tensor.f
!Chd|        h3d_shell_vector              engine/source/output/h3d/h3d_results/h3d_shell_vector.f
!Chd|        h3d_shell_vector_1            engine/source/output/h3d/h3d_results/h3d_shell_vector_1.f
!Chd|        h3d_skin_ixskin               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.f
!Chd|        h3d_skin_off                  engine/source/output/h3d/h3d_results/h3d_skin_off.f
!Chd|        h3d_skin_tensor               engine/source/output/h3d/h3d_results/h3d_skin_tensor.f
!Chd|        h3d_solid_off                 engine/source/output/h3d/spmd/spmd_h3d_solid_off.f
!Chd|        h3d_solid_scalar              engine/source/output/h3d/h3d_results/h3d_solid_scalar.f
!Chd|        h3d_solid_tensor              engine/source/output/h3d/h3d_results/h3d_solid_tensor.f
!Chd|        h3d_solid_tensor_1            engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.f
!Chd|        h3d_solid_vector              engine/source/output/h3d/h3d_results/h3d_solid_vector.f
!Chd|        h3d_sol_skin_ixskin           engine/source/output/h3d/h3d_results/h3d_sol_skin_ixskin.f
!Chd|        h3d_sol_skin_tensor           engine/source/output/h3d/h3d_results/h3d_sol_skin_tensor.f
!Chd|        h3d_sph_off                   engine/source/output/h3d/spmd/spmd_h3d_sph_off.f
!Chd|        h3d_sph_scalar                engine/source/output/h3d/h3d_results/h3d_sph_scalar.f
!Chd|        h3d_sph_tensor                engine/source/output/h3d/h3d_results/h3d_sph_tensor.f
!Chd|        h3d_velvecc22                 engine/source/output/h3d/h3d_results/h3d_velvecc22.f
!Chd|        h3d_velvecz22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.f
!Chd|        hist2                         engine/source/output/th/hist2.f
!Chd|        i18for3                       engine/source/interfaces/int18/i18for3.f
!Chd|        i22for3                       engine/source/interfaces/int22/i22for3.f
!Chd|        i22mainf                      engine/source/interfaces/int22/i22mainf.f
!Chd|        i22subvol                     engine/source/interfaces/int22/i22subvol.f
!Chd|        i7mainf                       engine/source/interfaces/int07/i7mainf.f
!Chd|        i9grd2                        engine/source/interfaces/int09/i9grd2.f
!Chd|        i9grd3                        engine/source/interfaces/int09/i9grd3.f
!Chd|        i9wal2                        engine/source/interfaces/int09/i9wal2.f
!Chd|        i9wal3                        engine/source/interfaces/int09/i9wal3.f
!Chd|        i9wale                        engine/source/interfaces/int09/i9wale.f
!Chd|        imp_buck                      engine/source/implicit/imp_buck.f
!Chd|        imp_chkm                      engine/source/implicit/imp_solv.f
!Chd|        imp_glob_k                    engine/source/implicit/imp_glob_k.f
!Chd|        imp_glob_k0                   engine/source/implicit/imp_glob_k.f
!Chd|        imp_glob_khp                  engine/source/implicit/imp_glob_k.f
!Chd|        imp_init                      engine/source/implicit/imp_init.f
!Chd|        imp_k_eig                     engine/stub/imp_k_eig.f       
!Chd|        imp_solv                      engine/source/implicit/imp_solv.f
!Chd|        imp_sol_init                  engine/source/implicit/imp_sol_init.f
!Chd|        ind_fr_k                      engine/source/mpi/implicit/imp_fri.f
!Chd|        ind_glob_k                    engine/source/implicit/ind_glob_k.f
!Chd|        ind_tshedg                    engine/source/elements/thickshell/solidec/ind_tshedg.f
!Chd|        init_th                       engine/source/output/th/init_th.f
!Chd|        init_th0                      engine/source/output/th/init_th0.f
!Chd|        inixfem                       engine/source/elements/xfem/inixfem.f
!Chd|        ini_fr_k                      engine/source/mpi/implicit/imp_fri.f
!Chd|        ini_tmax                      engine/source/output/ini_outmax.f
!Chd|        int18_law151_init             engine/source/interfaces/int18/int18_law151_init.f
!Chd|        int18_law151_update           engine/source/interfaces/int18/int18_law151_update.f
!Chd|        intal3                        engine/source/ale/inter/intal3.f
!Chd|        intfop2                       engine/source/interfaces/interf/intfop2.f
!Chd|        inttri                        engine/source/interfaces/intsort/inttri.f
!Chd|        joint_block_stiffness         engine/source/elements/joint/joint_block_stiffness.f
!Chd|        joint_elem_timestep           engine/source/elements/joint/joint_elem_timestep.f
!Chd|        kine_seatbelt_force           engine/source/tools/seatbelts/kine_seatbelt_force.f
!Chd|        kine_seatbelt_vel             engine/source/tools/seatbelts/kine_seatbelt_vel.f
!Chd|        ktbuf_ini                     engine/source/implicit/imp_init.f
!Chd|        laser1                        engine/source/loads/laser/laser1.f
!Chd|        laser2                        engine/source/loads/laser/laser2.f
!Chd|        laser3                        engine/source/loads/laser/laser2.f
!Chd|        layini                        engine/source/elements/shell/coque/layini.f
!Chd|        lech3d                        engine/source/output/h3d/h3d_build_fortran/lech3d.f
!Chd|        m11law                        engine/source/materials/mat/mat011/m11law.f
!Chd|        m11vs2                        engine/source/materials/mat/mat011/m11vs2.f
!Chd|        m11vs3                        engine/source/materials/mat/mat011/m11vs3.f
!Chd|        m1law8                        engine/source/materials/mat/mat001/m1law8.f
!Chd|        m24law                        engine/source/materials/mat/mat024/m24law.f
!Chd|        m2law8                        engine/source/materials/mat/mat002/m2law8.f
!Chd|        m2lawpi                       engine/source/materials/mat/mat002/m2lawpi.f
!Chd|        m3law8                        engine/source/materials/mat/mat003/m3law8.f
!Chd|        m51vois2                      engine/source/materials/mat/mat051/m51vois2.f
!Chd|        m51vois3                      engine/source/materials/mat/mat051/m51vois3.f
!Chd|        material_flow                 engine/source/tools/seatbelts/material_flow.f
!Chd|        mdama24                       engine/source/elements/solid/solidez/mdama24.f
!Chd|        meos8                         engine/source/materials/mat_share/meos8.f
!Chd|        monvol0                       engine/source/airbag/monvol0.f
!Chd|        mulawglc                      engine/source/materials/mat_share/mulawglc.f
!Chd|        mulawglcpinch                 engine/source/elements/shell/coqueba/mulawglcpinch.f
!Chd|        mulaw_ib                      engine/source/elements/beam/mulaw_ib.f
!Chd|        multi_buf2var                 engine/source/multifluid/multi_buf2var.f
!Chd|        multi_computevolume           engine/source/multifluid/multi_computevolume.f
!Chd|        multi_compute_dt              engine/source/multifluid/multi_compute_dt.f
!Chd|        multi_evolve_global           engine/source/multifluid/multi_evolve_global.f
!Chd|        multi_evolve_partial          engine/source/multifluid/multi_evolve_partial.f
!Chd|        multi_fluxes_computation      engine/source/multifluid/multi_fluxes_computation.f
!Chd|        multi_fvm2fem                 engine/source/multifluid/multi_fvm2fem.f
!Chd|        multi_globalize               engine/source/multifluid/multi_globalize.f
!Chd|        multi_i18_force_poff          engine/source/interfaces/int18/multi_i18_force_poff.f
!Chd|        multi_muscl_gradients         engine/source/multifluid/multi_muscl_gradients.f
!Chd|        multi_pressure_equilibrium    engine/source/multifluid/multi_pressure_equilibrium.f
!Chd|        multi_timeevolution           engine/source/multifluid/multi_timeevolution.f
!Chd|        multi_update_global           engine/source/multifluid/multi_update_global.f
!Chd|        multi_update_partial          engine/source/multifluid/multi_update_partial.f
!Chd|        multi_var2buf                 engine/source/multifluid/multi_var2buf.f
!Chd|        nodald                        engine/source/output/anim/generate/nodald.f
!Chd|        nodaldt                       engine/source/output/anim/generate/nodaldt.f
!Chd|        nodalp                        engine/source/output/anim/generate/nodalp.f
!Chd|        nodalssp                      engine/source/output/anim/generate/nodalssp.f
!Chd|        nodalt                        engine/source/output/anim/generate/nodalt.f
!Chd|        nodalvfrac                    engine/source/output/anim/generate/nodalvfrac.f
!Chd|        nodalvol                      engine/source/output/anim/generate/nodalvol.f
!Chd|        nodalzvol                     engine/source/output/anim/generate/nodalzvol.f
!Chd|        nodal_schlieren               engine/source/output/anim/generate/nodal_schlieren.f
!Chd|        noise                         engine/source/general_controls/computation/noise.f
!Chd|        output_div_u                  engine/source/output/anim/generate/output_div_u.f
!Chd|        output_schlieren              engine/source/output/anim/generate/output_schlieren.f
!Chd|        outp_arsz_ct                  engine/source/mpi/interfaces/spmd_outp.f
!Chd|        outp_arsz_st                  engine/source/mpi/interfaces/spmd_outp.f
!Chd|        outp_c_s                      engine/source/output/sty/outp_c_s.f
!Chd|        outp_c_t                      engine/source/output/sty/outp_c_t.f
!Chd|        outp_c_tf                     engine/source/output/sty/outp_c_t.f
!Chd|        outp_r_s                      engine/source/output/sty/outp_r_s.f
!Chd|        outp_r_t                      engine/source/output/sty/outp_r_t.f
!Chd|        outp_sp_s                     engine/source/output/sty/outp_sp_s.f
!Chd|        outp_sp_t                     engine/source/output/sty/outp_sp_t.f
!Chd|        outp_sp_tt                    engine/source/output/sty/outp_sp_t.f
!Chd|        outp_s_s                      engine/source/output/sty/outp_s_s.f
!Chd|        outp_s_t                      engine/source/output/sty/outp_s_t.f
!Chd|        outp_s_tt                     engine/source/output/sty/outp_s_t.f
!Chd|        parsorc                       engine/source/output/anim/generate/parsorc.f
!Chd|        parsorf                       engine/source/output/anim/generate/parsorf.f
!Chd|        pke3                          engine/source/elements/beam/pke3.f
!Chd|        pnoise                        engine/source/general_controls/computation/pnoise.f
!Chd|        porfor5                       engine/source/airbag/porfor5.f
!Chd|        porform5                      engine/source/airbag/porfor5.f
!Chd|        prelecflow                    engine/source/elements/solid/solide/prelecflow.f
!Chd|        projecig3d                    engine/source/elements/ige3d/projecig3d.f
!Chd|        q4ke2                         engine/source/elements/solid_2d/quad4/q4ke2.f
!Chd|        r12ke3                        engine/source/elements/spring/r12ke3.f
!Chd|        r13ke3                        engine/source/elements/spring/r13ke3.f
!Chd|        r23forc3                      engine/source/elements/spring/r23forc3.f
!Chd|        r23law108                     engine/source/elements/spring/r23law108.f
!Chd|        r23law113                     engine/source/elements/spring/r23law113.f
!Chd|        r23law114                     engine/source/elements/spring/r23law114.f
!Chd|        r4ke3                         engine/source/elements/spring/r4ke3.f
!Chd|        r8ke3                         engine/source/elements/spring/r8ke3.f
!Chd|        radiatoff                     engine/source/constraints/thermic/radiatoff.f
!Chd|        rbyonf                        engine/source/constraints/general/rbody/rbyonf.f
!Chd|        rbypid                        engine/source/constraints/general/rbody/rbypid.f
!Chd|        rbysens                       engine/source/constraints/general/rbody/rbyonf.f
!Chd|        resol_init                    engine/source/engine/resol_init.f
!Chd|        rforc3                        engine/source/elements/spring/rforc3.f
!Chd|        rgwal1                        engine/source/ale/grid/rgwal1.f
!Chd|        rgwat2                        engine/source/interfaces/int09/rgwat2.f
!Chd|        rgwat3                        engine/source/interfaces/int09/rgwat3.f
!Chd|        rgwath                        engine/source/interfaces/int09/rgwath.f
!Chd|        ruser32ke3                    engine/source/elements/spring/ruser32ke3.f
!Chd|        s10derit3                     engine/source/elements/solid/solide10/s10derit3.f
!Chd|        s10derito3                    engine/source/elements/solid/solide10/s10derito3.f
!Chd|        s10ke3                        engine/source/elements/solid/solide10/s10ke3.f
!Chd|        s10upd11t12                   engine/source/elements/solid/solide10/s10upd11t12.f
!Chd|        s10volnodt3                   engine/source/elements/solid/solide4_sfem/s10volnodt3.f
!Chd|        s16sigp3                      engine/source/elements/thickshell/solide16/s16sigp3.f
!Chd|        s20ke3                        engine/source/elements/solid/solide20/s20ke3.f
!Chd|        s4alesfem                     engine/source/elements/solid/solide4_sfem/s4alesfem.f
!Chd|        s4ke3                         engine/source/elements/solid/solide4/s4ke3.f
!Chd|        s4lagsfem                     engine/source/elements/solid/solide4_sfem/s4lagsfem.f
!Chd|        s6cfint_reg                   engine/source/elements/thickshell/solide6c/s6cfint_reg.f
!Chd|        s6cke3                        engine/source/elements/thickshell/solide6c/s6cke3.f
!Chd|        s8cfint_reg                   engine/source/elements/thickshell/solide8c/s8cfint_reg.f
!Chd|        s8cke3                        engine/source/elements/thickshell/solide8c/s8cke3.f
!Chd|        s8e_sigp                      engine/source/elements/solid/solide8e/s8e_sig.f
!Chd|        s8fint3                       engine/source/elements/solid/solide8/s8fint3.f
!Chd|        s8fupd11t12                   engine/source/elements/solid/solide8e/s8fupd11t12.f
!Chd|        s8ske3                        engine/source/elements/solid/solide8s/s8ske3.f
!Chd|        s8zke3                        engine/source/elements/solid/solide8z/s8zke3.f
!Chd|        s8_is17jac_i                  engine/source/elements/solid/solide8e/s8_is17jac_i.f
!Chd|        scfint_reg                    engine/source/elements/thickshell/solidec/scfint_reg.f
!Chd|        schlieren_buffer_gathering    engine/source/output/anim/generate/schlieren_buffer_gathering.f
!Chd|        sconnect_off                  engine/source/elements/solid/sconnect/sconnect_off.f
!Chd|        seatbelt_reduction_factor     engine/source/tools/seatbelts/seatbelt_reduction_factor.f
!Chd|        seggetv                       engine/source/interfaces/interf/seggetv.f
!Chd|        sfint_reg                     engine/source/elements/solid/solide/sfint_reg.f
!Chd|        shell_local_frame             engine/source/output/dynain/shell_rota.f
!Chd|        shell_rota                    engine/source/output/dynain/shell_rota.f
!Chd|        sigeps02g                     engine/source/materials/mat/mat002/sigeps02g.f
!Chd|        sigeps104                     engine/source/materials/mat/mat104/sigeps104.f
!Chd|        sigeps104c                    engine/source/materials/mat/mat104/sigeps104c.f
!Chd|        sigeps105                     engine/source/materials/mat/mat105/sigeps105.f
!Chd|        sigeps107                     engine/source/materials/mat/mat107/sigeps107.f
!Chd|        sigeps107c                    engine/source/materials/mat/mat107/sigeps107c.f
!Chd|        sigeps112                     engine/source/materials/mat/mat112/sigeps112.f
!Chd|        sigeps112c                    engine/source/materials/mat/mat112/sigeps112c.f
!Chd|        sigeps121                     engine/source/materials/mat/mat121/sigeps121.f
!Chd|        sigeps121c                    engine/source/materials/mat/mat121/sigeps121c.f
!Chd|        sigeps122                     engine/source/materials/mat/mat122/sigeps122.f
!Chd|        sigeps122c                    engine/source/materials/mat/mat122/sigeps122c.f
!Chd|        sigeps22g                     engine/source/materials/mat/mat022/sigeps22g.f
!Chd|        sigeps37_single_cell          engine/source/interfaces/int22/sigeps37_single_cell.f
!Chd|        sigeps51                      engine/source/materials/mat/mat051/sigeps51.f
!Chd|        sigeps97                      engine/source/materials/mat/mat097/sigeps97.f
!Chd|        sigrota                       engine/source/output/anim/generate/sigrota.f
!Chd|        sigrota_xfe                   engine/source/output/anim/generate/sigrota_xfe.f
!Chd|        sinit22_fvm                   engine/source/interfaces/int22/sinit22_fvm.f
!Chd|        sms_build_mat_2               engine/source/ams/sms_build_mat_2.f
!Chd|        soltospha                     engine/source/elements/sph/soltospha.f
!Chd|        soltosphf                     engine/source/elements/sph/soltosph.f
!Chd|        soltosphp                     engine/source/elements/sph/soltosph.f
!Chd|        soltosph_on1                  engine/source/elements/sph/soltosph_on1.f
!Chd|        soltosph_on12                 engine/source/elements/sph/soltosph_on1.f
!Chd|        soltosph_on2                  engine/source/elements/sph/soltosph_on2.f
!Chd|        spbrm_pre                     engine/source/implicit/imp_solv.f
!Chd|        spechan                       engine/source/elements/sph/spechan.f
!Chd|        spgauge                       engine/source/elements/sph/spgauge.f
!Chd|        sphprep                       engine/source/elements/sph/sphprep.f
!Chd|        splissv                       engine/source/elements/sph/splissv.f
!Chd|        spmd_fvb_switch               engine/source/mpi/airbags/spmd_fvb_switch.f
!Chd|        spmd_l11vois                  engine/source/mpi/fluid/spmd_cfd.f
!Chd|        spmd_l51vois                  engine/source/mpi/fluid/spmd_cfd.f
!Chd|        sponfprs                      engine/source/elements/sph/sponfprs.f
!Chd|        sponof1                       engine/source/elements/sph/sponof1.f
!Chd|        sponof2                       engine/source/elements/sph/sponof2.f
!Chd|        spwfvis                       engine/source/elements/sph/spwfvis.f
!Chd|        stat_beam_mp                  engine/source/output/sta/stat_beam_mp.f
!Chd|        stat_beam_spmd                engine/source/output/sta/stat_beam_spmd.f
!Chd|        stat_brick_mp                 engine/source/output/sta/stat_brick_mp.f
!Chd|        stat_brick_spmd               engine/source/output/sta/stat_brick_spmd.f
!Chd|        stat_c_auxf                   engine/source/output/sta/stat_c_auxf.f
!Chd|        stat_c_epspf                  engine/source/output/sta/stat_c_epspf.f
!Chd|        stat_c_off                    engine/source/output/sta/stat_c_off.f
!Chd|        stat_c_orth_loc               engine/source/output/sta/stat_c_orth_loc.f
!Chd|        stat_c_straf                  engine/source/output/sta/stat_c_straf.f
!Chd|        stat_c_strafg                 engine/source/output/sta/stat_c_strafg.f
!Chd|        stat_c_strsf                  engine/source/output/sta/stat_c_strsf.f
!Chd|        stat_c_strsfg                 engine/source/output/sta/stat_c_strsfg.f
!Chd|        stat_c_thk                    engine/source/output/sta/stat_c_thk.f
!Chd|        stat_inimap1d_file_spmd       engine/source/output/sta/stat_inimap1d_file_spmd.f
!Chd|        stat_inimap1d_spmd            engine/source/output/sta/stat_inimap1d_spmd.f
!Chd|        stat_inimap2d_file_spmd       engine/source/output/sta/stat_inimap2d_file_spmd.f
!Chd|        stat_inimap2d_spmd            engine/source/output/sta/stat_inimap2d_spmd.f
!Chd|        stat_p_aux                    engine/source/output/sta/stat_p_aux.f
!Chd|        stat_p_full                   engine/source/output/sta/stat_p_full.f
!Chd|        stat_quad_mp                  engine/source/output/sta/stat_quad_mp.f
!Chd|        stat_quad_spmd                engine/source/output/sta/stat_quad_spmd.f
!Chd|        stat_r_full                   engine/source/output/sta/stat_r_full.f
!Chd|        stat_shel_mp                  engine/source/output/sta/stat_shel_mp.f
!Chd|        stat_shel_spmd                engine/source/output/sta/stat_shel_spmd.f
!Chd|        stat_size_c                   engine/source/output/sta/stat_size.f
!Chd|        stat_spring_mp                engine/source/output/sta/stat_spring_mp.f
!Chd|        stat_spring_spmd              engine/source/output/sta/stat_spring_spmd.f
!Chd|        stat_s_auxf                   engine/source/output/sta/stat_s_auxf.f
!Chd|        stat_s_eref                   engine/source/output/sta/stat_s_eref.f
!Chd|        stat_s_ortho                  engine/source/output/sta/stat_s_ortho.f
!Chd|        stat_s_straf                  engine/source/output/sta/stat_s_straf.f
!Chd|        stat_s_strsf                  engine/source/output/sta/stat_s_strsf.f
!Chd|        stat_truss_mp                 engine/source/output/sta/stat_truss_mp.f
!Chd|        stat_truss_spmd               engine/source/output/sta/stat_truss_spmd.f
!Chd|        stat_t_full                   engine/source/output/sta/stat_t_full.f
!Chd|        strn_tenscor3                 engine/source/output/h3d/h3d_results/h3d_strn_tenscor3.f
!Chd|        strs_tenscor3                 engine/source/output/h3d/h3d_results/strs_tenscor3.f
!Chd|        switch_to_dtnoda              engine/source/time_step/switch_to_dtnoda.f
!Chd|        szhour3                       engine/source/elements/solid/solidez/szhour3.f
!Chd|        szhour3_or                    engine/source/elements/solid/solidez/szhour3_or.f
!Chd|        s_user                        engine/source/output/sty/s_user.f
!Chd|        tagoff3n                      engine/source/interfaces/interf/chkstfn3.f
!Chd|        tencgps1                      engine/source/output/anim/generate/tensorc.f
!Chd|        tencgps2                      engine/source/output/anim/generate/tensorc.f
!Chd|        tensgps1                      engine/source/output/anim/generate/tensor6.f
!Chd|        tensgps2                      engine/source/output/anim/generate/tensor6.f
!Chd|        tensgps3                      engine/source/output/anim/generate/tensor6.f
!Chd|        tensgpstrain                  engine/source/output/anim/generate/tensgpstrain.f
!Chd|        tensgps_skin                  engine/source/output/anim/generate/tensor6.f
!Chd|        tensor0                       engine/source/output/anim/generate/tensor0.f
!Chd|        tensorc                       engine/source/output/anim/generate/tensorc.f
!Chd|        tensorc_crk                   engine/source/output/anim/generate/tensorc_crk.f
!Chd|        tensorc_ply                   engine/source/output/anim/generate/tensorc_ply.f
!Chd|        tensors                       engine/source/output/anim/generate/tensor6.f
!Chd|        tforc3                        engine/source/elements/truss/tforc3.f
!Chd|        thcluster                     engine/source/output/th/thcluster.f
!Chd|        thcoq                         engine/source/output/th/thcoq.f
!Chd|        thermexpc                     engine/source/materials/mat_share/thermexpc.f
!Chd|        thermexppi                    engine/source/elements/beam/thermexpp.f
!Chd|        thickvar                      engine/source/elements/shell/coque/thickvar.f
!Chd|        thnst                         engine/source/output/th/thnst.f
!Chd|        thpout                        engine/source/output/th/thpout.f
!Chd|        thquad                        engine/source/output/th/thquad.f
!Chd|        thres                         engine/source/output/th/thres.f
!Chd|        thres_count                   engine/source/output/th/thres_count.f
!Chd|        thsol                         engine/source/output/th/thsol.f
!Chd|        thsph                         engine/source/output/th/thsph.f
!Chd|        thtrus                        engine/source/output/th/thtrus.f
!Chd|        tke3                          engine/source/elements/truss/tke3.f
!Chd|        tm_dmgl25_shell               engine/source/output/outmaxsubr.f
!Chd|        tm_dmg_shells                 engine/source/output/outmaxsubr.f
!Chd|        tm_dmg_solid                  engine/source/output/outmaxsubr.f
!Chd|        tm_seq_shell                  engine/source/output/outmaxsubr.f
!Chd|        tm_seq_solid                  engine/source/output/outmaxsubr.f
!Chd|        tm_sig_shell                  engine/source/output/outmaxsubr.f
!Chd|        tm_sig_solid                  engine/source/output/outmaxsubr.f
!Chd|        tm_stra_shell                 engine/source/output/outmaxsubr.f
!Chd|        tm_stra_solid                 engine/source/output/outmaxsubr.f
!Chd|        update_slipring               engine/source/tools/seatbelts/update_slipring.f
!Chd|        upd_tmax                      engine/source/output/upd_outmax.f
!Chd|        upenric3_n3                   engine/source/elements/xfem/upenric3_nx.f
!Chd|        upenric3_n4                   engine/source/elements/xfem/upenric3_nx.f
!Chd|        upoffc                        engine/source/elements/xfem/upoffc.f
!Chd|        upofftg                       engine/source/elements/xfem/upofftg.f
!Chd|        upxfem1                       engine/source/elements/xfem/upxfem1.f
!Chd|        upxfem_tagxp                  engine/source/elements/xfem/upxfem_tagxp.f
!Chd|        velvecc22                     engine/source/output/anim/generate/velvec.f
!Chd|        velvecz22                     engine/source/output/anim/generate/velvecz22.f
!Chd|        voln22                        engine/source/interfaces/int22/voln22.f
!Chd|        volpvgb                       engine/source/airbag/volpvg.f 
!Chd|        write_buf_law51               engine/source/materials/mat/mat051/write_buf_law51.f
!Chd|        write_cut_cell_buffer         engine/source/interfaces/int22/write_cut_cell_buffer.f
!Chd|        w_elbuf_str                   engine/source/elements/elbuf/w_elbuf_str.f
!Chd|        xfeoff                        engine/source/elements/xfem/xfeoff.f
!Chd|        xforc3                        engine/source/elements/xelem/xforc3.f
!Chd|        ale51_gradient_reconstruction2engine/source/ale/alemuscl/ale51_gradient_reconstruction2.f
!Chd|        multi_muscl_fluxes_computationengine/source/multifluid/multi_muscl_fluxes_computation.f
!Chd|        check_mat_elem_prop_compatibilitystarter/source/materials/mat/check_mat_elem_prop_compatibility.f
!Chd|-- calls ---------------
!Chd|===================================================================================

      Module elbufdef_mod

!=======================================================================================      
!! \brief  module to define data structure for internal state variables by element group
!! \details 


!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"

!=======================================================================      
!
      Type g_bufel_          ! material and property variables (mean values for each element)
        integer  nvar_glob   
        integer  g_noff   
        integer  g_ierr
        integer  g_off    
        integer  g_gama   
        integer  g_smstr  
        integer  g_hourg  
        integer  g_bfrac  
        integer  g_eint   
        integer  g_eins   
        integer  g_rho    
        integer  g_qvis   
        integer  g_deltax 
        integer  g_vol    
        integer  g_epsd   
        integer  g_epsq
        integer  g_pla    
        integer  g_temp   
        integer  g_tb     
        integer  g_rk     
        integer  g_re     
        integer  g_sig    
        integer  g_for    
        integer  g_mom    
        integer  g_thk    
        integer  g_tag22
        integer  g_stra  
        integer  g_sigi   
        integer  g_dmg   
        integer  g_forpg    
        integer  g_mompg
        integer  g_gama_r   
!
        integer  g_forpgpinch  
        integer  g_mompgpinch 
        integer  g_epgpinchxz
        integer  g_epgpinchyz 
        integer  g_epgpinchzz
!    
        integer  g_strpg
        integer  g_uelr
        integer  g_uelr1
        integer  g_damdl
        integer  g_forth    
        integer  g_eintth    
        integer  g_fill
        integer  g_seq
        integer  g_strw  
        integer  g_strwpg  
        integer  g_thk_i    
        integer  g_jac_i   
        integer  g_dt
        integer  g_isms 
        integer  g_strhg
        integer  g_bpreld        ! bolt preloading
        integer  g_aburn
        integer  g_mu
        integer  g_planl
        integer  g_epsdnl
        integer  g_tempg
        integer  g_cor_nf        ! nodal forces for corotational formulation
        integer  g_cor_fr        ! local frame for corotational formulation
        integer  g_cor_xr        ! reference local coordinates for corotational formulation
        integer  g_maxfrac 
        integer  g_maxeps 
        integer  g_betaorth
        integer  g_amu
        integer  g_tsaiwu
        integer  g_dmgscl
        integer  g_sh_ioffset
!---
!    - 1d - elem (truss, beam, spring) 
        integer  g_area
        integer  g_skew
        integer  g_length
        integer  g_totdepl
        integer  g_totrot
        integer  g_forep
        integer  g_momep
        integer  g_dep_in_tens
        integer  g_dep_in_comp
        integer  g_rot_in_tens
        integer  g_rot_in_comp
        integer  g_posx
        integer  g_posy
        integer  g_posz
        integer  g_posxx
        integer  g_posyy
        integer  g_poszz
        integer  g_yield
        integer  g_length_err
        integer  g_dv
        integer  g_dfs
        integer  g_skew_err
        integer  g_e6
        integer  g_ruptcrit
        integer  g_mass
        integer  g_v_repcvt
        integer  g_vr_repcvt
        integer  g_nuvar
        integer  g_nuvarn
        integer  g_defini
        integer  g_forini
        integer  g_inifric
        integer  g_etotsh
        integer  g_skew_id
!
!    -  for seatbelt elements
        integer  g_slipring_id
        integer  g_slipring_fram_id
        integer  g_slipring_strand
        integer  g_retractor_id
        integer  g_ringslip
        integer  g_add_node
        integer  g_update
        integer  g_fram_factor
        integer  g_intvar
!---
        integer  g_dt_piter
!---
        integer  g_idt_tsh
!-------  max_historic variables     
        integer  g_tm_yield
        integer  g_tm_seq
        integer  g_tm_eint
        integer  g_tm_dmg
        integer  g_tm_sig  ! max(p1)&min(p3) 3 for 2d, 6 for 3d
        integer  g_tm_stra ! max(p1)&min(p3)
!---
        integer, dimension(:) , pointer ::   noff  
        integer, dimension(:) , pointer ::   ierr  
        my_real, dimension(:) , pointer ::   off   
        my_real, dimension(:) , pointer ::   gama  
        double precision, dimension(:) , pointer ::   smstr 
        my_real, dimension(:) , pointer ::   hourg 
        my_real, dimension(:) , pointer ::   bfrac    
        my_real, dimension(:) , pointer ::   eint  
        my_real, dimension(:) , pointer ::   eins  
        my_real, dimension(:) , pointer ::   rho   
        my_real, dimension(:) , pointer ::   qvis       
        my_real, dimension(:) , pointer ::   deltax                 
        my_real, dimension(:) , pointer ::   vol                            
        my_real, dimension(:) , pointer ::   epsd  
        my_real, dimension(:) , pointer ::   epsq
        my_real, dimension(:) , pointer ::   pla   
        my_real, dimension(:) , pointer ::   temp  
        my_real, dimension(:) , pointer ::   tb    
        my_real, dimension(:) , pointer ::   rk    
        my_real, dimension(:) , pointer ::   re    
        my_real, dimension(:) , pointer ::   sig                          
        my_real, dimension(:) , pointer ::   for                        
        my_real, dimension(:) , pointer ::   mom                        
        my_real, dimension(:) , pointer ::   thk                        
        my_real, dimension(:) , pointer ::   tag22                    
        my_real, dimension(:) , pointer ::   stra                     
        my_real, dimension(:) , pointer ::   sigi                     
        my_real, dimension(:) , pointer ::   dmg                     
        my_real, dimension(:) , pointer ::   forpg   ! mean gauss point value    
        my_real, dimension(:) , pointer ::   mompg
        my_real, dimension(:) , pointer ::   gama_r  ! co-rotational local sys  
!
        my_real, dimension(:) , pointer ::   forpgpinch 
        my_real, dimension(:) , pointer ::   mompgpinch
        my_real, dimension(:) , pointer ::   epgpinchxz
        my_real, dimension(:) , pointer ::   epgpinchyz
        my_real, dimension(:) , pointer ::   epgpinchzz
!                        
        my_real, dimension(:) , pointer ::   strpg                  
        my_real, dimension(:) , pointer ::   tempg   
        my_real, dimension(:) , pointer ::   uelr   !  failure global variable
        my_real, dimension(:) , pointer ::   uelr1  !  failure global variable
        my_real, dimension(:) , pointer ::   damdl  !  delamination failure (law25)
        my_real, dimension(:) , pointer ::   forth
        my_real, dimension(:) , pointer ::   eintth                                                
        my_real, dimension(:) , pointer ::   fill
        my_real, dimension(:) , pointer ::   seq
        my_real, dimension(:) , pointer ::   thk_i   !---- add for ismstr=10 shell (xfem not done) strwpg total anti-symme curvature                         
        my_real, dimension(:) , pointer ::   strw 
        my_real, dimension(:) , pointer ::   strwpg
        my_real, dimension(:) , pointer ::   jac_i   !--------inversed [j]
        my_real, dimension(:) , pointer ::   dt
        my_real, dimension(:) , pointer ::   aburn 
        my_real, dimension(:) , pointer ::   mu 
        integer, dimension(:) , pointer ::   isms 
        integer, dimension(:) , pointer ::   sh_ioffset 
        my_real, dimension(:) , pointer ::   bpreld  ! bolt preloading 
        my_real, dimension(:) , pointer ::   cor_nf  ! corotational nodal forces 
        my_real, dimension(:) , pointer ::   cor_fr  ! corotational frame 
        my_real, dimension(:) , pointer ::   cor_xr  ! corotational reference coordinates 
        my_real, dimension(:) , pointer ::   maxfrac
        my_real, dimension(:) , pointer ::   maxeps 
        my_real, dimension(:) , pointer ::   betaorth 
        my_real, dimension(:) , pointer ::   amu
!---
!    - 1d - elem (truss, beam, spring)
        my_real, dimension(:) , pointer ::   area
        my_real, dimension(:) , pointer ::   skew
        my_real, dimension(:) , pointer ::   length
        my_real, dimension(:) , pointer ::   totdepl
        my_real, dimension(:) , pointer ::   totrot
        my_real, dimension(:) , pointer ::   forep
        my_real, dimension(:) , pointer ::   momep
        my_real, dimension(:) , pointer ::   dep_in_tens
        my_real, dimension(:) , pointer ::   dep_in_comp
        my_real, dimension(:) , pointer ::   rot_in_tens
        my_real, dimension(:) , pointer ::   rot_in_comp
        my_real, dimension(:) , pointer ::   posx
        my_real, dimension(:) , pointer ::   posy
        my_real, dimension(:) , pointer ::   posz
        my_real, dimension(:) , pointer ::   posxx
        my_real, dimension(:) , pointer ::   posyy
        my_real, dimension(:) , pointer ::   poszz
        my_real, dimension(:) , pointer ::   yield
        my_real, dimension(:) , pointer ::   length_err
        my_real, dimension(:) , pointer ::   dv
        my_real, dimension(:) , pointer ::   dfs
        my_real, dimension(:) , pointer ::   skew_err
        my_real, dimension(:) , pointer ::   e6
        my_real, dimension(:) , pointer ::   ruptcrit
        my_real, dimension(:) , pointer ::   mass
        my_real, dimension(:) , pointer ::   v_repcvt
        my_real, dimension(:) , pointer ::   vr_repcvt
        my_real, dimension(:) , pointer ::   var
        my_real, dimension(:) , pointer ::   varn
        my_real, dimension(:) , pointer ::   defini
        my_real, dimension(:) , pointer ::   forini
        my_real, dimension(:) , pointer ::   inifric
        my_real, dimension(:) , pointer ::   strhg
        my_real, dimension(:) , pointer ::   etotsh
        integer, dimension(:) , pointer ::   skew_id
        type (fail_loc_) , dimension(:) , pointer ::   fail
!
!    -  for seatbelt elements
        integer, dimension(:) , pointer ::   slipring_id
        integer, dimension(:) , pointer ::   slipring_fram_id
        integer, dimension(:) , pointer ::   slipring_strand
        integer, dimension(:) , pointer ::   retractor_id
        my_real, dimension(:) , pointer ::   ringslip
        integer, dimension(:) , pointer ::   add_node
        integer, dimension(:) , pointer ::   update
        my_real, dimension(:) , pointer ::   fram_factor
        my_real, dimension(:) , pointer ::   intvar
!---
        my_real, dimension(:) , pointer ::   dt_piter ! tetra10 iterative power for time step computation
        integer, dimension(:) , pointer ::   idt_tsh  
!-------  max_historic variables     
        my_real, dimension(:) , pointer ::   tm_yield   
        my_real, dimension(:) , pointer ::   tm_seq   
        my_real, dimension(:) , pointer ::   tm_eint   
        my_real, dimension(:) , pointer ::   tm_dmg   
        my_real, dimension(:) , pointer ::   tm_sig1
        my_real, dimension(:) , pointer ::   tm_stra1
        my_real, dimension(:) , pointer ::   tm_sig3
        my_real, dimension(:) , pointer ::   tm_stra3
!---  work array
        my_real, dimension(:) , pointer ::   tm_psig
        my_real, dimension(:) , pointer ::   tm_pstra
!---
      end type g_bufel_


      Type l_bufel_      ! element variables per integration point
        integer  mlaw    ! material law type          
        integer  lawid   ! material law id    
        my_real, dimension(:) , pointer ::   off    
        my_real, dimension(:) , pointer ::   gama   
        my_real, dimension(:) , pointer ::   stra   
        my_real, dimension(:) , pointer ::   frac          
        my_real, dimension(:) , pointer ::   bfrac
        my_real, dimension(:) , pointer ::   eint   
        my_real, dimension(:) , pointer ::   eins   
        my_real, dimension(:) , pointer ::   rho    
        my_real, dimension(:) , pointer ::   dp_drho
        my_real, dimension(:) , pointer ::   qvis   
        my_real, dimension(:) , pointer ::   deltax 
        my_real, dimension(:) , pointer ::   vol    
        my_real, dimension(:) , pointer ::   epsa   
        my_real, dimension(:) , pointer ::   epsd   
        my_real, dimension(:) , pointer ::   epsq   
        my_real, dimension(:) , pointer ::   epsf   
        my_real, dimension(:) , pointer ::   pla    
        my_real, dimension(:) , pointer ::   temp   
        my_real, dimension(:) , pointer ::   tb     
        my_real, dimension(:) , pointer ::   rk     
        my_real, dimension(:) , pointer ::   re         
        my_real, dimension(:) , pointer ::   vk     
        my_real, dimension(:) , pointer ::   sf     
        my_real, dimension(:) , pointer ::   rob    
        my_real, dimension(:) , pointer ::   dam    
        my_real, dimension(:) , pointer ::   dsum   
        my_real, dimension(:) , pointer ::   dglo   
        my_real, dimension(:) , pointer ::   crak   
        my_real, dimension(:) , pointer ::   ang    
        my_real, dimension(:) , pointer ::   epe    
        my_real, dimension(:) , pointer ::   epc    
        my_real, dimension(:) , pointer ::   xst    
        my_real, dimension(:) , pointer ::   ssp    
        my_real, dimension(:) , pointer ::   z      
        my_real, dimension(:) , pointer ::   visc   
        my_real, dimension(:) , pointer ::   sigl   
        my_real, dimension(:) , pointer ::   sigv   
        my_real, dimension(:) , pointer ::   siga   
        my_real, dimension(:) , pointer ::   sigb   
        my_real, dimension(:) , pointer ::   sigc   
        my_real, dimension(:) , pointer ::   sigd   
        my_real, dimension(:) , pointer ::   sigf   
        my_real, dimension(:) , pointer ::   sig    
        my_real, dimension(:) , pointer ::   sigply    
        my_real, dimension(:) , pointer ::   for    
        my_real, dimension(:) , pointer ::   mom
        my_real, dimension(:) , pointer ::   thk    
        double precision, dimension(:) , pointer ::   smstr    
        my_real, dimension(:) , pointer ::   dmg 
        my_real, dimension(:) , pointer ::   forth
        my_real, dimension(:) , pointer ::   eintth    
        my_real, dimension(:) , pointer ::   seq
        my_real, dimension(:) , pointer ::   jac_i    
        my_real, dimension(:) , pointer ::   fac_yld  
        my_real, dimension(:) , pointer ::   aburn
        my_real, dimension(:) , pointer ::   mu
        my_real, dimension(:) , pointer ::   pij   !--------[ni,j] for imstr10
        double precision, dimension(:) , pointer ::   vol0dp
        my_real, dimension(:) , pointer ::   planl
        my_real, dimension(:) , pointer ::   epsdnl            
        my_real, dimension(:) , pointer ::   dmgscl
        my_real, dimension(:) , pointer ::   tsaiwu
      end type l_bufel_                             

      Type buf_prop_
        my_real, dimension(:)  , pointer ::  var
        my_real, dimension(:)  , pointer ::  varn
      end type buf_prop_

!--------------------------------------------------------------------------------      
!     Non-local buffer for regularization in the shell thickness
      Type buf_nloc_
        my_real, dimension(:,:), pointer :: massth ! embedded wire nodal masses
        my_real, dimension(:,:), pointer :: unlth  ! non-local cumulated variable at nodes
        my_real, dimension(:,:), pointer :: vnlth  ! non-local velocities
        my_real, dimension(:,:), pointer :: fnlth  ! non-local forces
      end type buf_nloc_
!     Non-local buffer for regularization in the thickshell thickness
      Type buf_nlocts_
        my_real, dimension(:,:), pointer :: massth ! embedded wire nodal masses
        my_real, dimension(:,:), pointer :: unlth  ! non-local cumulated variable at nodes
        my_real, dimension(:,:), pointer :: vnlth  ! non-local velocities
        my_real, dimension(:,:), pointer :: fnlth  ! non-local forces
      end type buf_nlocts_
!     Non-local buffer for brick elements geometry configuration
      Type buf_nlocs_
        integer, dimension(:)  , allocatable :: nl_isolnod ! number of effective nodes (nel)
        integer, dimension(:,:), allocatable :: nl_solnod  ! identifiers of effectives nodes (8,nel)
      end type buf_nlocs_
!--------------------------------------------------------------------------------

      Type buf_eos_
        my_real, dimension(:)  , pointer ::  var 
      end type buf_eos_

      Type buf_poro_
        my_real, dimension(:)  , pointer ::  var 
      end type buf_poro_

      Type buf_visc_
!        integer  ilaw    ! type de loi de viscosite
!        integer  nvar
        my_real, dimension(:)  , pointer ::  var 
      end type buf_visc_

      Type buf_xfem_       ! buffer des elements xfem crees par la fissuration
!-------  layer variables     
        integer  ly_smstr
        integer  ly_hourg
        my_real, dimension(:) , pointer ::   dmg
        my_real, dimension(:) , pointer ::   gama
        my_real, dimension(:) , pointer ::   dira
        my_real, dimension(:) , pointer ::   dirb
        my_real, dimension(:) , pointer ::   plapt
        my_real, dimension(:) , pointer ::   sigpt
        my_real, dimension(:) , pointer ::   smstr
        my_real, dimension(:) , pointer ::   hourg
        type (g_bufel_)                                :: xgbuf   ! global variables
        type (l_bufel_)  , dimension(:,:,:)  , pointer :: xlbuf   ! local variables (nptr,npts,nptt)
        type (buf_mat_)  , dimension(:,:,:)  , pointer :: xmat    ! material buffer
        type (buf_fail_) , dimension(:,:,:)  , pointer :: xfail   ! failure models
      end type buf_xfem_
 
      Type fail_loc_
        integer  ilawf    ! type de loi de rupture
        integer  idfail
        integer  nvar
        integer  lf_dam
        integer  lf_dammx
        integer  lf_damini
        integer  lf_tdel
        integer  lf_indx
        integer  lf_off
        integer, dimension(:)  , pointer ::  indx
        integer, dimension(:)  , pointer ::  off
        my_real, dimension(:)  , pointer ::  dam
        my_real, dimension(:)  , pointer ::  var 
        my_real, dimension(:)  , pointer ::  dammx 
        my_real, dimension(:)  , pointer ::  damini
        my_real, dimension(:)  , pointer ::  tdel
      end type fail_loc_

      Type buf_fail_
        type(fail_loc_), dimension(:)  , pointer ::  floc 
      end type buf_fail_

      Type buf_mat_
        my_real, dimension(:)  , pointer ::  var 
        integer, dimension(:)  , pointer ::  vartmp
      end type buf_mat_
!     
      Type l_bufel_dir_      ! element variables per slice in each layer
        my_real, dimension(:) , pointer ::   dira
        my_real, dimension(:) , pointer ::   dirb
      end type l_bufel_dir_                             

      Type buf_lay_
        integer  ilaw     
        integer  imat 
        integer  ieos
        integer  ivisc
        integer  iporo
        integer  nfail
        integer  nvar_mat
        integer  nvar_eos        
        integer  nvartmp
        integer  nvar_visc
        integer  nvar_lay   ! max nb of layer variables = 9
        integer  nvar_loc   ! max nb of local variables in lbuf = 51 (below)
        integer  nptt       ! nb of integration points through layer (pid_51)
!-------
        integer  ly_dmg  
        integer  ly_gama   
        integer  ly_dira   
        integer  ly_dirb   
        integer  ly_crkdir   
        integer  ly_plapt  ! mean plastic strain value between gauss points 
        integer  ly_sigpt  ! mean stress value between gauss points
        integer  ly_hourg
        integer  ly_uelr
        integer  ly_uelr1
        integer  ly_offpg
        integer  ly_off
!-------
        integer  l_off    
        integer  l_gama   
        integer  l_stra   
        integer  l_frac   
        integer  l_bfrac
        integer  l_eint   
        integer  l_eins   
        integer  l_rho    
        integer  l_dp_drho
        integer  l_qvis   
        integer  l_deltax 
        integer  l_vol                        
        integer  l_epsa   
        integer  l_epsd                    
        integer  l_epsq   
        integer  l_epsf   
        integer  l_pla             
        integer  l_temp   
        integer  l_tb     
        integer  l_rk     
        integer  l_re     
        integer  l_vk     
        integer  l_sf     
        integer  l_rob    
        integer  l_dam    
        integer  l_dsum   
        integer  l_dglo   
        integer  l_crak   
        integer  l_ang    
        integer  l_epe    
        integer  l_epc    
        integer  l_xst         
        integer  l_ssp    
        integer  l_z          
        integer  l_visc   
        integer  l_sigl   
        integer  l_sigv   
        integer  l_siga   
        integer  l_sigb   
        integer  l_sigc   
        integer  l_sigd   
        integer  l_sigf   
        integer  l_sig    
        integer  l_sigply ! plyxfem
        integer  l_for    
        integer  l_mom
        integer  l_thk 
        integer  l_smstr      
        integer  l_dmg  
        integer  l_forth
        integer  l_eintth  
        integer  l_seq
        integer  l_jac_i
        integer  l_fac_yld
        integer  l_aburn
        integer  l_mu
        integer  l_pij
        integer  l_vol0dp
        integer  l_planl
        integer  l_epsdnl
        integer  l_dmgscl
        integer  l_tsaiwu
!-------  layer variables     
        my_real, dimension(:) , pointer ::   dmg
        my_real, dimension(:) , pointer ::   gama
        my_real, dimension(:) , pointer ::   dira
        my_real, dimension(:) , pointer ::   dirb
        my_real, dimension(:) , pointer ::   crkdir
        my_real, dimension(:) , pointer ::   plapt
        my_real, dimension(:) , pointer ::   sigpt
        my_real, dimension(:) , pointer ::   hourg
        my_real, dimension(:) , pointer ::   uelr   !  failure layer variable
        my_real, dimension(:) , pointer ::   uelr1  !  failure layer variable
        integer, dimension(:) , pointer ::   offpg  !  failure of gauss point
        integer, dimension(:) , pointer ::   off    !  layer failure flag
!-------       
        type (l_bufel_)  , dimension(:,:,:)  , pointer :: lbuf   ! local variables - per integration point
        type (buf_mat_)  , dimension(:,:,:)  , pointer :: mat    ! material buffer - per integration point
        type (buf_fail_) , dimension(:,:,:)  , pointer :: fail  
        type (buf_prop_) , dimension(:,:,:)  , pointer :: prop 
        type (buf_eos_)  , dimension(:,:,:)  , pointer :: eos  
        type (buf_visc_) , dimension(:,:,:)  , pointer :: visc  
        type (buf_poro_) , dimension(:,:,:)  , pointer :: poro  
        type (buf_xfem_) , dimension(:)      , pointer :: xfem        ! xfem (nxel)
        type (l_bufel_dir_) , dimension(:)  , pointer :: lbuf_dir   ! local direction by int point in the tickness for slice)
      end type buf_lay_
!
!--------------------       
! 
      Type buf_intloc_      ! element variables per integration point
        my_real, dimension(:) , pointer ::   eps   ! (length=3)
        my_real, dimension(:) , pointer ::   sig   ! (length=3)
      end type buf_intloc_                             

      Type buf_intlay_
        integer  ilaw       ! inter ply material law type     
        integer  imat       ! inter ply material number
        integer  nfail
        integer  nvar_mat   ! number of user variables (uvar) in the material buffer
        integer  nvartmp    ! number of temp storage variables (vartmp) in material laws
!------ interlayer variables par couche   (length=1) 
        my_real, dimension(:) , pointer ::   eint         
        my_real, dimension(:) , pointer ::   count
      
        type (buf_intloc_) , dimension(:,:) , pointer :: ilbuf
        type (buf_mat_)    , dimension(:,:) , pointer :: mat
        type (buf_fail_)   , dimension(:,:) , pointer :: fail  
      end type buf_intlay_
!--------------------       
 
      Type elbuf_struct_
        integer     :: igtyp    
        integer     :: nel      
        integer     :: nlay     
        integer     :: nintlay  
        integer     :: nptr     
        integer     :: npts     
        integer     :: nptt   
        integer     :: ixfem
        integer     :: nxel       ! number of xfem parts created after element crack
        integer     :: idrape
       
        type (g_bufel_)                              :: gbuf   ! global variables - mean element values
        type (buf_lay_)   , dimension(:)   , pointer :: bufly  ! bufly(nlay) layer variables 
        type (buf_intlay_), dimension(:)   , pointer :: intlay ! inter-layer (nlay-1)
        type (buf_xfem_)  , dimension(:)   , pointer :: xfem   ! xfem (nxel)
        type (buf_nloc_)  , dimension(:,:) , pointer :: nloc   ! non-local thickness specific structure for shells
        type (buf_nlocts_), dimension(:,:) , pointer :: nlocts ! non-local thickness specific structure for thickshells
        type (buf_nlocs_)                            :: nlocs  ! non-local structure of brick element geometry configuration
      end type elbuf_struct_
!
!---------------
      end module elbufdef_mod
