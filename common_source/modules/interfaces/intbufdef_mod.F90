!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!hd|  intbufdef_mod                 modules/intbufdef_mod.f
!hd|-- called by -----------
!hd|        copy_intbuf_tab               common_source/interf/copy_intbuf_tab.f
!hd|        intbuf_ini                    common_source/interf/intbuf_ini.f
!hd|        intbuf_tab_c_ini              common_source/interf/copy_intbuf_tab.f
!hd|        upgrade_cand_opt              common_source/interf/upgrade_multimp.f
!hd|        upgrade_lcand_e2s             common_source/interf/upgrade_multimp.f
!hd|        upgrade_lcand_edg             common_source/interf/upgrade_multimp.f
!hd|        upgrade_multimp               common_source/interf/upgrade_multimp.f
!hd|        build_csrect                  starter/source/model/mesh/build_cnel.f
!hd|        chktyp2                       starter/source/interfaces/interf1/chktyp2.f
!hd|        copy_ival                     starter/source/restart/ddsplit/inter_tools.f
!hd|        copy_ival_igeo                starter/source/restart/ddsplit/inter_tools.f
!hd|        copy_node_nodloc              starter/source/restart/ddsplit/inter_tools.f
!hd|        copy_rval                     starter/source/restart/ddsplit/inter_tools.f
!hd|        c_front                       starter/source/restart/ddsplit/c_front.f
!hd|        ddsplit                       starter/source/restart/ddsplit/ddsplit.f
!hd|        domdec2                       starter/source/spmd/domdec2.f
!hd|        fictivmassigeo                starter/source/groups/ssurftagigeo.f
!hd|        fillcni2                      starter/source/spmd/domdec2.f
!hd|        fill_intercep                 starter/source/spmd/node/ddtools.f
!hd|        filter_node_nodloc            starter/source/restart/ddsplit/inter_tools.f
!hd|        flush_remnode_array           starter/source/interfaces/inter3d1/flush_remnode_array.f
!hd|        fxbtagn                       starter/source/constraints/fxbody/fxbtagn.f
!hd|        hm_read_inter_type21          starter/source/interfaces/int21/hm_read_inter_type21.f
!hd|        hm_read_inter_type25          starter/source/interfaces/int25/hm_read_inter_type25.f
!hd|        i11edge                       starter/source/interfaces/inter3d1/i11edge.f
!hd|        i20ini3                       starter/source/interfaces/inter3d1/i20ini3.f
!hd|        i20sta                        starter/source/interfaces/inter3d1/inintr2.f
!hd|        i20stifn                      starter/source/interfaces/inter3d1/i20stifn.f
!hd|        i24setnodes                   starter/source/interfaces/inter3d1/i24setnodes.f
!hd|        i24stsecnd                    starter/source/interfaces/inter3d1/i24stslav.f
!hd|        i25sors                       starter/source/interfaces/inter3d1/i25sors.f
!hd|        i25stsecnd                    starter/source/interfaces/inter3d1/i25stslav.f
!hd|        i2main                        starter/source/interfaces/interf1/i2master.f
!hd|        i2tid3                        starter/source/interfaces/inter3d1/i2tid3.f
!hd|        i2_dtn                        starter/source/interfaces/inter3d1/i2_dtn.f
!hd|        i2_dtn_27                     starter/source/interfaces/inter3d1/i2_dtn_27.f
!hd|        i2_dtn_28                     starter/source/interfaces/inter3d1/i2_dtn_28.f
!hd|        i7remnode                     starter/source/interfaces/inter3d1/i7remnode.f
!hd|        i7stsecnd                     starter/source/interfaces/inter3d1/i7stslav.f
!hd|        iniend                        starter/source/interfaces/inter3d1/iniend.f
!hd|        iniend2d                      starter/source/interfaces/inter3d1/iniend.f
!hd|        inint2                        starter/source/interfaces/inter2d1/inint2.f
!hd|        inint3                        starter/source/interfaces/inter3d1/inint3.f
!hd|        inint3_thkvar                 starter/source/interfaces/inter3d1/inint3_thkvar.f
!hd|        inintmass                     starter/source/interfaces/inter3d1/inintmass.f
!hd|        inintr                        starter/source/interfaces/interf1/inintr.f
!hd|        inintr1                       starter/source/interfaces/interf1/inintr1.f
!hd|        inintr2                       starter/source/interfaces/inter3d1/inintr2.f
!hd|        inintr_orthdirfric            starter/source/interfaces/interf1/inintr_orthdirfric.f
!hd|        inintr_thkvar                 starter/source/interfaces/interf1/inintr_thkvar.f
!hd|        inintsub                      starter/source/interfaces/interf1/inintsub.f
!hd|        inintsub_11                   starter/source/output/subinterface/inintsub_11.f
!hd|        inintsub_25                   starter/source/output/subinterface/inintsub_25.f
!hd|        inintsub_7                    starter/source/output/subinterface/inintsub_7.f
!hd|        initia                        starter/source/elements/initia/initia.f
!hd|        int2cy_chk                    starter/source/constraints/general/bcs/lecbcscyc.f
!hd|        int2modif_nd                  starter/source/elements/solid/solide10/dim_s10edg.f
!hd|        int8_ini                      starter/source/interfaces/intbuf/intbuf_ini_starter.f
!hd|        intbuf_ini_starter            starter/source/interfaces/intbuf/intbuf_ini_starter.f
!hd|        ipari_l_ini                   starter/source/restart/ddsplit/ipari_l_ini.f
!hd|        itagsl12                      starter/source/interfaces/inter3d1/inintr2.f
!hd|        itagsl2                       starter/source/interfaces/inter3d1/itagsl2.f
!hd|        iwcontdd_type24               starter/source/spmd/domain_decomposition/iwcontdd_type24.f
!hd|        iwcontdd_type25               starter/source/spmd/domain_decomposition/iwcontdd_type25.f
!hd|        lagm_ini                      starter/source/tools/lagmul/lagm_ini.f
!hd|        lecins                        starter/source/interfaces/interf1/lecins.f
!hd|        lectur                        starter/source/starter/lectur.f
!hd|        lgmini_i2                     starter/source/tools/lagmul/lgmini_i2.f
!hd|        lgmini_i7                     starter/source/tools/lagmul/lgmini_i7.f
!hd|        prepare_int25                 starter/source/model/mesh/build_cnel.f
!hd|        prepare_split_cand            starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_cand_i20_edge   starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_cand_i21        starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_cand_i25_edge   starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i11             starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i17             starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i2              starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i20             starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i21             starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i24             starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i25             starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i25e2e          starter/source/spmd/prepare_split_i25e2e.f
!hd|        prepare_split_i7              starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i8              starter/source/restart/ddsplit/inter_tools.f
!hd|        prepare_split_i9              starter/source/restart/ddsplit/inter_tools.f
!hd|        prescrint                     starter/source/interfaces/interf1/prescrint.f
!hd|        pre_i2                        starter/source/interfaces/inter3d1/i7remnode.f
!hd|        r2r_clean_inter               starter/source/coupling/rad2rad/r2r_clean_inter.f
!hd|        remn_i2op                     starter/source/interfaces/inter3d1/i7remnode.f
!hd|        remn_i2op_edg25               starter/source/interfaces/int25/i25remlin.f
!hd|        remn_i2_edg                   starter/source/interfaces/inter3d1/i7remnode.f
!hd|        remn_i2_edgop                 starter/source/interfaces/inter3d1/i7remnode.f
!hd|        remn_self24                   starter/source/interfaces/inter3d1/remn_self24.f
!hd|        reset_gap                     starter/source/interfaces/interf1/reset_gap.f
!hd|        ri2_int24p_ini                starter/source/interfaces/inter3d1/i7remnode.f
!hd|        scrint                        starter/source/interfaces/interf1/scrint.f
!hd|        set_front8                    starter/source/spmd/node/ddtools.f
!hd|        set_intercep                  starter/source/spmd/node/ddtools.f
!hd|        sms_ini_jad_1                 starter/source/ams/sms_init.f
!hd|        sms_ini_jad_2                 starter/source/ams/sms_init.f
!hd|        sms_ini_jad_3                 starter/source/ams/sms_init.f
!hd|        sms_ini_kdi                   starter/source/ams/sms_init.f
!hd|        split_2ry_cand_ival_i21       starter/source/restart/ddsplit/inter_tools.f
!hd|        split_adskyn_25               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_i11                starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_i20                starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_i20_edge           starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_i24                starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_i25                starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_i25_edge           starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_i7                 starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_ival               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_ival_i21           starter/source/restart/ddsplit/inter_tools.f
!hd|        split_cand_rval               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_interfaces              starter/source/restart/ddsplit/split_interfaces.f
!hd|        split_isegpt_ival             starter/source/restart/ddsplit/inter_tools.f
!hd|        split_lbound_i25              starter/source/restart/ddsplit/inter_tools.f
!hd|        split_ledge_i25               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_nisub_i25               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_nisub_i7                starter/source/restart/ddsplit/inter_tools.f
!hd|        split_node_ival               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_node_ival2              starter/source/restart/ddsplit/inter_tools.f
!hd|        split_node_ival_i24           starter/source/restart/ddsplit/inter_tools.f
!hd|        split_node_ival_i25           starter/source/restart/ddsplit/inter_tools.f
!hd|        split_node_nodloc             starter/source/restart/ddsplit/inter_tools.f
!hd|        split_node_nodloc_p0          starter/source/restart/ddsplit/inter_tools.f
!hd|        split_node_rval               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_remnode_i11             starter/source/restart/ddsplit/inter_tools.f
!hd|        split_remnode_i24             starter/source/restart/ddsplit/inter_tools.f
!hd|        split_remnode_i25             starter/source/restart/ddsplit/inter_tools.f
!hd|        split_remnode_i25_e2s         starter/source/restart/ddsplit/inter_tools.f
!hd|        split_remnode_i25_edge        starter/source/restart/ddsplit/inter_tools.f
!hd|        split_remnode_i7              starter/source/restart/ddsplit/inter_tools.f
!hd|        split_segedge_nodloc_i24      starter/source/restart/ddsplit/inter_tools.f
!hd|        split_seg_ielem               starter/source/restart/ddsplit/split_seg_ielem.f
!hd|        split_seg_ival2               starter/source/restart/ddsplit/inter_tools.f
!hd|        split_seg_ival_i20            starter/source/restart/ddsplit/inter_tools.f
!hd|        split_seg_ival_i20_2          starter/source/restart/ddsplit/inter_tools.f
!hd|        split_seg_nodloc              starter/source/restart/ddsplit/inter_tools.f
!hd|        split_seg_nodloc_i24          starter/source/restart/ddsplit/inter_tools.f
!hd|        split_seg_rval_i20            starter/source/restart/ddsplit/inter_tools.f
!hd|        split_seg_segloc              starter/source/restart/ddsplit/inter_tools.f
!hd|        split_xsav                    starter/source/restart/ddsplit/inter_tools.f
!hd|        st_qaprint_driver             starter/source/output/qaprint/st_qaprint_driver.f
!hd|        st_qaprint_interfaces         starter/source/output/qaprint/st_qaprint_interfaces.f
!hd|        upgrade_remnode               starter/source/interfaces/interf1/upgrade_remnode.f
!hd|        upgrade_remnode2              starter/source/interfaces/interf1/upgrade_remnode.f
!hd|        upgrade_remnode_e2s           starter/source/interfaces/interf1/upgrade_remnode.f
!hd|        upgrade_remnode_edg           starter/source/interfaces/interf1/upgrade_remnode.f
!hd|        upgrade_remnode_edg2          starter/source/interfaces/interf1/upgrade_remnode.f
!hd|        wrcomip                       starter/source/restart/ddsplit/wrcommp.f
!hd|        w_fi                          starter/source/restart/ddsplit/w_fi.f
!hd|        w_front                       starter/source/restart/ddsplit/w_front.f
!hd|        w_intbuf_size                 starter/source/restart/ddsplit/inter_tools.f
!hd|        w_pon                         starter/source/restart/ddsplit/w_pon.f
!hd|        w_type8                       starter/source/restart/ddsplit/split_interfaces.f
!hd|        secnd_surface_on_domain       starter/source/interfaces/inter3d1/i24setnodes.f
!hd|        mergemod                      starter/share/modules1/merge_mod.f
!hd|        alefvm_main                   engine/source/ale/alefvm/alefvm_main.f
!hd|        alemain                       engine/source/ale/alemain.f
!hd|        alewdx                        engine/source/ale/grid/alewdx.f
!hd|        check_edge_state              engine/source/interfaces/interf/check_edge_state.f
!hd|        check_nodal_state             engine/source/interfaces/interf/check_nodal_state.f
!hd|        check_remote_surface_state    engine/source/interfaces/interf/check_remote_surface_state.f
!hd|        check_surface_state           engine/source/interfaces/interf/check_surface_state.f
!hd|        chkstfn3n                     engine/source/interfaces/interf/chkstfn3.f
!hd|        chkstifn                      engine/source/interfaces/inter2d/chkstifn.f
!hd|        cndmasi2_dim                  engine/source/elements/solid/solide10/s10cndf.f
!hd|        cndmasi2_ini                  engine/source/elements/solid/solide10/s10cndf.f
!hd|        cp_impbuf                     engine/source/implicit/produt_v.f
!hd|        deplafakeige                  engine/source/assembly/deplafakeige.f
!hd|        diag_int                      engine/source/mpi/implicit/imp_fri.f
!hd|        dim_glob_k                    engine/source/implicit/ind_glob_k.f
!hd|        dim_int7                      engine/source/implicit/ind_glob_k.f
!hd|        dim_int_k                     engine/source/implicit/ind_glob_k.f
!hd|        dim_kinefr                    engine/source/mpi/implicit/imp_fri.f
!hd|        dim_kine_i                    engine/source/implicit/ind_glob_k.f
!hd|        dim_kine_p                    engine/source/implicit/ind_glob_k.f
!hd|        dim_kine_s                    engine/source/implicit/ind_glob_k.f
!hd|        dim_kine_t                    engine/source/implicit/ind_glob_k.f
!hd|        dim_kinfrk                    engine/source/mpi/implicit/imp_fri.f
!hd|        dim_kinkn                     engine/source/implicit/imp_int_k.f
!hd|        dim_kinmax                    engine/source/implicit/ind_glob_k.f
!hd|        dim_kinmv                     engine/source/airbag/monv_imp0.f
!hd|        dim_ndof_i                    engine/source/implicit/ind_glob_k.f
!hd|        dim_ndof_ii                   engine/source/implicit/ind_glob_k.f
!hd|        eig                           engine/stub/eig.f
!hd|        eig1                          engine/stub/eig1.f
!hd|        eigcond                       engine/stub/eigcond.f
!hd|        eigp                          engine/stub/eigp.f
!hd|        find_edge_from_remote_proc    engine/source/interfaces/interf/find_edge_from_remote_proc.f
!hd|        find_edge_inter               engine/source/interfaces/interf/find_edge_inter.f
!hd|        find_surface_from_remote_proc engine/source/interfaces/interf/find_surface_from_remote_proc.f
!hd|        find_surface_inter            engine/source/interfaces/interf/find_surface_inter.f
!hd|        fr_a2bd                       engine/source/mpi/implicit/imp_fri.f
!hd|        fr_matv                       engine/source/mpi/implicit/imp_fri.f
!hd|        fr_u2dd                       engine/source/mpi/implicit/imp_fri.f
!hd|        getnddli_g                    engine/source/mpi/implicit/imp_fri.f
!hd|        i10fku3                       engine/source/interfaces/int10/i10ke3.f
!hd|        i10forcf3                     engine/source/interfaces/int10/i10ke3.f
!hd|        i10ke3                        engine/source/interfaces/int10/i10ke3.f
!hd|        i10mainf                      engine/source/interfaces/int10/i10mainf.f
!hd|        i10main_opt_tri               engine/source/interfaces/intsort/i10opt_opt_tri.f
!hd|        i10main_tri                   engine/source/interfaces/intsort/i10main_tri.f
!hd|        i11fku3                       engine/source/interfaces/int11/i11ke3.f
!hd|        i11forcf3                     engine/source/interfaces/int11/i11ke3.f
!hd|        i11ke3                        engine/source/interfaces/int11/i11ke3.f
!hd|        i11mainf                      engine/source/interfaces/int11/i11mainf.f
!hd|        i11main_crit_tri              engine/source/interfaces/intsort/i11main_crit_tri.f
!hd|        i11main_opt_tri               engine/source/interfaces/intsort/i11main_opt_tri.f
!hd|        i11main_tri                   engine/source/interfaces/intsort/i11main_tri.f
!hd|        i14cmp                        engine/source/interfaces/int14/i14cmp.f
!hd|        i14ist                        engine/source/interfaces/int14/i14ist.f
!hd|        i14wfs                        engine/source/interfaces/int14/i14wfs.f
!hd|        i15cmp                        engine/source/interfaces/int15/i15cmp.f
!hd|        i16main                       engine/source/interfaces/int16/i16main.f
!hd|        i17main                       engine/source/interfaces/int17/i17main.f
!hd|        i17main_crit_tri              engine/source/interfaces/int17/i17main_pena.f
!hd|        i17main_pena                  engine/source/interfaces/int17/i17main_pena.f
!hd|        i17main_tri                   engine/source/interfaces/int17/i17main_pena.f
!hd|        i18main_kine_1                engine/source/interfaces/int18/i18main_kine.f
!hd|        i18main_kine_2                engine/source/interfaces/int18/i18main_kine.f
!hd|        i18main_kine_f                engine/source/interfaces/int18/i18main_kine.f
!hd|        i18main_kine_i                engine/source/interfaces/int18/i18main_kine.f
!hd|        i18main_kine_s                engine/source/interfaces/int18/i18main_kine.f
!hd|        i18main_kine_v                engine/source/interfaces/int18/i18main_kine.f
!hd|        i20mainf                      engine/source/interfaces/int20/i20mainf.f
!hd|        i20main_crit_tri              engine/source/interfaces/intsort/i20main_crit_tri.f
!hd|        i20main_opt_tri               engine/source/interfaces/intsort/i20main_opt_tri.f
!hd|        i20main_tri                   engine/source/interfaces/intsort/i20main_tri.f
!hd|        i21mainf                      engine/source/interfaces/int21/i21mainf.f
!hd|        i21main_crit_tri              engine/source/interfaces/intsort/i21main_crit_tri.f
!hd|        i21main_gap                   engine/source/interfaces/int21/i21main_gap.f
!hd|        i21main_opt_tri               engine/source/interfaces/intsort/i21main_opt_tri.f
!hd|        i21main_tri                   engine/source/interfaces/intsort/i21main_tri.f
!hd|        i21reset                      engine/source/interfaces/int21/i21reset.f
!hd|        i21_icrit                     engine/source/interfaces/intsort/i21_icrit.f
!hd|        i22mainf                      engine/source/interfaces/int22/i22mainf.f
!hd|        i22main_tri                   engine/source/interfaces/intsort/i22main_tri.f
!hd|        i23mainf                      engine/source/interfaces/int23/i23mainf.f
!hd|        i23main_opt_tri               engine/source/interfaces/intsort/i23main_opt_tri.f
!hd|        i23main_tri                   engine/source/interfaces/intsort/i23main_tri.f
!hd|        i24e2e_fictive_nodes_update   engine/source/interfaces/int24/i24for3e.f
!hd|        i24ke3                        engine/source/interfaces/int24/i24ke3.f
!hd|        i24mainf                      engine/source/interfaces/int24/i24main.f
!hd|        i24main_crit_tri              engine/source/interfaces/intsort/i24main_crit_tri.f
!hd|        i24main_opt_tri               engine/source/interfaces/intsort/i24main_opt_tri.f
!hd|        i24main_tri                   engine/source/interfaces/intsort/i24main_tri.f
!hd|        i24nitschfor3                 engine/source/interfaces/int24/i24nitschfor3.f
!hd|        i24pxfem                      engine/source/interfaces/int24/i24pxfem.f
!hd|        i24xvfic_upd                  engine/source/interfaces/int24/i24for3e.f
!hd|        i25comp_1                     engine/source/interfaces/int25/i25comp_1.f
!hd|        i25comp_2                     engine/source/interfaces/int25/i25comp_2.f
!hd|        i25irtlm                      engine/source/interfaces/int25/i25irtlm.f
!hd|        i25maind_2                    engine/source/interfaces/int25/i25maind_2.f
!hd|        i25mainf                      engine/source/interfaces/int25/i25mainf.f
!hd|        i25main_crit_tri              engine/source/interfaces/intsort/i25main_crit_tri.f
!hd|        i25main_free                  engine/source/interfaces/intsort/i25main_free.f
!hd|        i25main_gap                   engine/source/interfaces/int25/i25main_gap.f
!hd|        i25main_norm                  engine/source/interfaces/int25/i25main_norm.f
!hd|        i25main_opt_tri               engine/source/interfaces/intsort/i25main_opt_tri.f
!hd|        i25main_slid                  engine/source/interfaces/int25/i25main_slid.f
!hd|        i25main_tri                   engine/source/interfaces/intsort/i25main_tri.f
!hd|        i25prep_nindex                engine/source/interfaces/int25/i25slid.f
!hd|        i25prep_send                  engine/source/interfaces/int25/i25slid.f
!hd|        i25prep_sizbufs               engine/source/interfaces/int25/i25slid.f
!hd|        i25tagn                       engine/source/interfaces/int25/i25norm.f
!hd|        i2vit27                       engine/source/interfaces/interf/i2vit27.f
!hd|        i2vit28                       engine/source/interfaces/interf/i2vit28.f
!hd|        i2_imp0                       engine/source/interfaces/interf/i2_imp0.f
!hd|        i2_imp1                       engine/source/interfaces/interf/i2_imp1.f
!hd|        i2_impd                       engine/source/interfaces/interf/i2_impd.f
!hd|        i2_impi                       engine/source/interfaces/interf/i2_imp0.f
!hd|        i2_impm                       engine/source/interfaces/interf/i2_imp1.f
!hd|        i2_impr1                      engine/source/interfaces/interf/i2_imp1.f
!hd|        i2_impr2                      engine/source/interfaces/interf/i2_imp1.f
!hd|        i5ke3                         engine/source/interfaces/inter3d/i5ke3.f
!hd|        i6main                        engine/source/interfaces/inter3d/i6main.f
!hd|        i7fku3                        engine/source/interfaces/int07/i7ke3.f
!hd|        i7forcf3                      engine/source/interfaces/int07/i7ke3.f
!hd|        i7ke3                         engine/source/interfaces/int07/i7ke3.f
!hd|        i7mainf                       engine/source/interfaces/int07/i7mainf.f
!hd|        i7mainfr                      engine/source/interfaces/int07/i7ke3.f
!hd|        i7main_crit_tri               engine/source/interfaces/intsort/i7main_crit_tri.f
!hd|        i7main_lmult                  engine/source/interfaces/int07/i7main_lmult.f
!hd|        i7main_opt_tri                engine/source/interfaces/intsort/i7main_opt_tri.f
!hd|        i7main_tri                    engine/source/interfaces/intsort/i7main_tri.f
!hd|        i9main2                       engine/source/interfaces/int09/i9main2.f
!hd|        i9main3                       engine/source/interfaces/int09/i9main3.f
!hd|        i9wale                        engine/source/interfaces/int09/i9wale.f
!hd|        iddl_int                      engine/source/mpi/implicit/imp_fri.f
!hd|        iddl_mint                     engine/source/implicit/imp_int_k.f
!hd|        idel_int                      engine/source/implicit/ind_glob_k.f
!hd|        id_mvini                      engine/source/airbag/monv_imp0.f
!hd|        ielof2                        engine/source/interfaces/interf/ielof2.f
!hd|        imp3_a2b                      engine/source/airbag/monv_imp0.f
!hd|        imp3_u2x                      engine/source/airbag/monv_imp0.f
!hd|        imp_buck                      engine/source/implicit/imp_buck.f
!hd|        imp_chkm                      engine/source/implicit/imp_solv.f
!hd|        imp_compab                    engine/source/implicit/imp_solv.f
!hd|        imp_compabp                   engine/source/implicit/imp_solv.f
!hd|        imp_diags                     engine/source/mpi/implicit/imp_fri.f
!hd|        imp_diagsn                    engine/source/mpi/implicit/imp_fri.f
!hd|        imp_dtkin                     engine/source/implicit/imp_int_k.f
!hd|        imp_dykv                      engine/source/implicit/imp_dyna.f
!hd|        imp_dykv0                     engine/source/implicit/imp_dyna.f
!hd|        imp_fr7i                      engine/source/mpi/implicit/imp_fri.f
!hd|        imp_frfv                      engine/source/mpi/implicit/imp_fri.f
!hd|        imp_fri                       engine/source/mpi/implicit/imp_fri.f
!hd|        imp_frkd                      engine/source/mpi/implicit/imp_fri.f
!hd|        imp_frki                      engine/source/mpi/implicit/imp_fri.f
!hd|        imp_frsn                      engine/source/mpi/implicit/imp_fri.f
!hd|        imp_i10mainf                  engine/source/interfaces/int10/i10ke3.f
!hd|        imp_i11mainf                  engine/source/interfaces/int11/i11ke3.f
!hd|        imp_i7mainf                   engine/source/interfaces/int07/i7ke3.f
!hd|        imp_icomcrit                  engine/source/implicit/imp_int_k.f
!hd|        imp_inisi                     engine/source/implicit/imp_pcg.f
!hd|        imp_inist                     engine/source/implicit/imp_pcg.f
!hd|        imp_intdt                     engine/source/implicit/imp_int_k.f
!hd|        imp_intfr                     engine/source/implicit/imp_solv.f
!hd|        imp_inttd0                    engine/source/implicit/imp_int_k.f
!hd|        imp_int_k                     engine/source/implicit/imp_int_k.f
!hd|        imp_k_eig                     engine/stub/imp_k_eig.f
!hd|        imp_lanzp                     engine/source/implicit/imp_lanz.f
!hd|        imp_pcgh                      engine/source/implicit/imp_pcg.f
!hd|        imp_ppcgh                     engine/source/implicit/imp_pcg.f
!hd|        imp_solv                      engine/source/implicit/imp_solv.f
!hd|        imp_sol_init                  engine/source/implicit/imp_sol_init.f
!hd|        imp_tripi                     engine/source/implicit/imp_int_k.f
!hd|        imp_updst                     engine/source/implicit/imp_pcg.f
!hd|        imp_updv2                     engine/source/implicit/imp_pcg.f
!hd|        ind_frkd                      engine/source/mpi/implicit/imp_fri.f
!hd|        ind_glob_k                    engine/source/implicit/ind_glob_k.f
!hd|        ind_int_k                     engine/source/implicit/ind_glob_k.f
!hd|        ind_kinefr                    engine/source/mpi/implicit/imp_fri.f
!hd|        ind_kine_i                    engine/source/implicit/ind_glob_k.f
!hd|        ind_kine_k                    engine/source/implicit/ind_glob_k.f
!hd|        ind_kinfrk                    engine/source/mpi/implicit/imp_fri.f
!hd|        init_i25_edge                 engine/source/interfaces/int25/init_i25_edge.f
!hd|        init_interf_sorting_strategy  engine/source/interfaces/init_interf_sorting_strategy.f
!hd|        init_nodal_state              engine/source/interfaces/interf/init_nodal_state.f
!hd|        ini_dd0                       engine/source/mpi/implicit/imp_fri.f
!hd|        ini_ddfv                      engine/source/mpi/implicit/imp_fri.f
!hd|        ini_dofspc                    engine/source/implicit/upd_glob_k.f
!hd|        ini_kinkn                     engine/source/implicit/imp_int_k.f
!hd|        ini_kinmv                     engine/source/airbag/monv_imp0.f
!hd|        int12w                        engine/source/ale/inter/int12w.f
!hd|        int18_law151_nsv_shift        engine/source/interfaces/int18/int18_law151_nsv_shift.f
!hd|        int2poff                      engine/source/interfaces/interf/int2poff.f
!hd|        int2poffh                     engine/source/interfaces/interf/int2poff.f
!hd|        int2rupt                      engine/source/interfaces/interf/int2rupt.f
!hd|        int2_imp2                     engine/source/interfaces/interf/i2_imp2.f
!hd|        intal1                        engine/source/ale/inter/intal1.f
!hd|        intal2                        engine/source/ale/inter/intal2.f
!hd|        intal3                        engine/source/ale/inter/intal3.f
!hd|        intal4                        engine/source/ale/inter/intal4.f
!hd|        intcrit                       engine/source/interfaces/intsort/intcrit.f
!hd|        inter_check_sort              engine/source/interfaces/generic/inter_check_sort.f
!hd|        inter_color_coarse_voxel      engine/source/interfaces/generic/inter_color_coarse_voxel.f
!hd|        inter_color_voxel             engine/source/interfaces/generic/inter_color_voxel.f
!hd|        inter_count_node_curv         engine/source/interfaces/generic/inter_count_node_curv.f
!hd|        inter_deallocate_wait         engine/source/interfaces/generic/inter_deallocate_wait.f
!hd|        inter_minmax_node             engine/source/interfaces/generic/inter_minmax_node.f
!hd|        inter_prepare_sort            engine/source/interfaces/generic/inter_prepare_sort.f
!hd|        inter_sort                    engine/source/interfaces/generic/inter_sort.f
!hd|        inter_sort_07                 engine/source/interfaces/int07/inter_sort_07.f
!hd|        inter_struct_init             engine/source/interfaces/generic/inter_struct_init.f
!hd|        inter_trc_7                   engine/source/interfaces/int07/inter_trc_7.f
!hd|        inter_voxel_creation          engine/source/interfaces/generic/inter_voxel_creation.f
!hd|        intfop1                       engine/source/interfaces/interf/intfop1.f
!hd|        intfop2                       engine/source/interfaces/interf/intfop2.f
!hd|        intfop8                       engine/source/interfaces/interf/intfop8.f
!hd|        intmass_update                engine/source/interfaces/interf/intmass_update.f
!hd|        intti0                        engine/source/interfaces/interf/intti0.f
!hd|        intti1                        engine/source/interfaces/interf/intti1.f
!hd|        intti12a                      engine/source/interfaces/interf/intti12.f
!hd|        intti12f                      engine/source/interfaces/interf/intti12.f
!hd|        intti12v                      engine/source/interfaces/interf/intti12.f
!hd|        intti2                        engine/source/interfaces/interf/intti2.f
!hd|        intti2f                       engine/source/interfaces/interf/intti2f.f
!hd|        intti2v                       engine/source/interfaces/interf/intti2v.f
!hd|        inttri                        engine/source/interfaces/intsort/inttri.f
!hd|        intvo2                        engine/source/interfaces/inter2d/intvo2.f
!hd|        intvo3                        engine/source/interfaces/inter3d/intvo3.f
!hd|        intvo8                        engine/source/interfaces/inter3d/intvo8.f
!hd|        int_fku3                      engine/source/implicit/imp_int_k.f
!hd|        int_matv                      engine/source/implicit/imp_int_k.f
!hd|        int_matvp                     engine/source/implicit/imp_int_k.f
!hd|        kin_kml                       engine/source/mpi/implicit/imp_fri.f
!hd|        kin_knl                       engine/source/implicit/imp_int_k.f
!hd|        kin_ksl                       engine/source/mpi/implicit/imp_fri.f
!hd|        lag_i2main                    engine/source/tools/lagmul/lag_i2main.f
!hd|        lag_mult                      engine/source/tools/lagmul/lag_mult.f
!hd|        lag_multp                     engine/source/tools/lagmul/lag_mult.f
!hd|        lectur                        engine/source/input/lectur.f
!hd|        lin_solv                      engine/source/implicit/lin_solv.f
!hd|        lin_solvh0                    engine/source/implicit/lin_solv.f
!hd|        lin_solvh1                    engine/source/implicit/lin_solv.f
!hd|        lin_solvhm                    engine/source/implicit/lin_solv.f
!hd|        lin_solvih2                   engine/source/implicit/lin_solv.f
!hd|        ltag_i2main                   engine/source/tools/lagmul/lag_ntag.f
!hd|        mav_lt2                       engine/source/implicit/produt_v.f
!hd|        mav_lth                       engine/source/implicit/produt_v.f
!hd|        mav_lth0                      engine/source/implicit/produt_v.f
!hd|        mav_ltp                       engine/source/implicit/produt_v.f
!hd|        mmav_lth                      engine/source/implicit/produt_v.f
!hd|        monv_diag                     engine/source/airbag/monv_imp0.f
!hd|        monv_imp                      engine/source/airbag/monv_imp0.f
!hd|        monv_m3                       engine/source/airbag/monv_imp0.f
!hd|        monv_prem                     engine/source/airbag/monv_imp0.f
!hd|        mpp_init                      engine/source/mpi/interfaces/spmd_i7tool.f
!hd|        mv_matv                       engine/source/airbag/monv_imp0.f
!hd|        nddli_ns                      engine/source/mpi/implicit/imp_fri.f
!hd|        nl_solv                       engine/source/implicit/nl_solv.f
!hd|        printime_interf               engine/source/system/timer_interf.f
!hd|        print_stif                    engine/source/implicit/imp_solv.f
!hd|        rdcomi                        engine/source/output/restart/rdcomm.f
!hd|        recukin                       engine/source/implicit/recudis.f
!hd|        rer02                         engine/source/implicit/upd_glob_k.f
!hd|        rer_int_v                     engine/source/implicit/upd_glob_k.f
!hd|        resol                         engine/source/engine/resol.f
!hd|        resol_head                    engine/source/engine/resol_head.f
!hd|        resol_init                    engine/source/engine/resol_init.f
!hd|        s10cndi2_ini                  engine/source/elements/solid/solide10/s10cndf.f
!hd|        sms_build_mat_2               engine/source/ams/sms_build_mat_2.f
!hd|        sms_ini_int                   engine/source/ams/sms_init.f
!hd|        sms_ini_jad_1                 engine/source/ams/sms_init.f
!hd|        sms_ini_jad_2                 engine/source/ams/sms_init.f
!hd|        sms_ini_jad_3                 engine/source/ams/sms_init.f
!hd|        sms_ini_kdi                   engine/source/ams/sms_init.f
!hd|        sms_ini_kin_1                 engine/source/ams/sms_init.f
!hd|        sortie_main                   engine/source/output/sortie_main.f
!hd|        spmd_cell_list_exchange       engine/source/mpi/interfaces/spmd_cell_list_exchange.f
!hd|        spmd_cell_size_exchange       engine/source/mpi/interfaces/spmd_cell_size_exchange.f
!hd|        spmd_check_tag                engine/source/mpi/ams/spmd_check_tag.f
!hd|        spmd_exch_da20                engine/source/mpi/interfaces/spmd_exch_da20.f
!hd|        spmd_exch_deleted_surf_edge   engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.f
!hd|        spmd_exch_efric               engine/source/mpi/interfaces/spmd_exch_efric.f
!hd|        spmd_exch_i24                 engine/source/mpi/interfaces/spmd_exch_i24.f
!hd|        spmd_exch_i25                 engine/source/mpi/interfaces/spmd_exch_i25.f
!hd|        spmd_exch_idel_seglo          engine/source/mpi/interfaces/spmd_exch_idel_seglo.f
!hd|        spmd_exch_inter_18            engine/source/mpi/interfaces/spmd_exch_inter_18.f
!hd|        spmd_exch_nor                 engine/source/mpi/interfaces/spmd_exch_nor.f
!hd|        spmd_exch_press               engine/source/mpi/interfaces/spmd_exch_press.f
!hd|        spmd_exch_smst2               engine/source/mpi/ams/spmd_exch_smst2.f
!hd|        spmd_exch_sorting_efric       engine/source/mpi/interfaces/spmd_exch_sorting_efric.f
!hd|        spmd_glob_min5                engine/source/mpi/generic/spmd_glob_min5.f
!hd|        spmd_i18kine_com_a            engine/source/mpi/interfaces/spmd_i18kine_com_a.f
!hd|        spmd_i18kine_com_acc          engine/source/mpi/interfaces/spmd_i18kine_com_acc.f
!hd|        spmd_i18kine_com_ms           engine/source/mpi/interfaces/spmd_i18kine_com_ms.f
!hd|        spmd_i18kine_com_v            engine/source/mpi/interfaces/spmd_i18kine_com_v.f
!hd|        spmd_i18kine_pene_com_poff    engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.f
!hd|        spmd_i21fthecom               engine/source/mpi/interfaces/send_cand.f
!hd|        spmd_i21tempcom               engine/source/mpi/interfaces/send_cand.f
!hd|        spmd_i24_prepare              engine/source/interfaces/int24/i24_prepare.f
!hd|        spmd_i25front_init            engine/source/mpi/interfaces/spmd_i25front.f
!hd|        spmd_i25front_nor             engine/source/mpi/interfaces/spmd_i25front.f
!hd|        spmd_i25_prepare              engine/source/interfaces/int25/i25_prepare.f
!hd|        spmd_i25_slide_gat            engine/source/mpi/interfaces/spmd_i25slide.f
!hd|        spmd_i7fcom_poff              engine/source/mpi/forces/spmd_i7fcom_poff.f
!hd|        spmd_i7fcom_pon               engine/source/mpi/forces/spmd_i7fcom_pon.f
!hd|        spmd_i7itied_cand             engine/source/mpi/interfaces/spmd_i7itied_cand.f
!hd|        spmd_i7xvcom2                 engine/source/mpi/interfaces/spmd_i7xvcom2.f
!hd|        spmd_ifront                   engine/source/mpi/interfaces/spmd_ifront.f
!hd|        spmd_ifront_stamp             engine/source/mpi/interfaces/send_cand.f
!hd|        spmd_int18_law151_pon         engine/source/mpi/forces/spmd_int18_law151_pon.f
!hd|        spmd_savefi                   engine/source/mpi/interfaces/spmd_i7tool.f
!hd|        spmd_split_comm_inter         engine/source/mpi/interfaces/spmd_split_comm_inter.f
!hd|        spmd_wait_nb                  engine/source/mpi/interfaces/spmd_wait_nb.f
!hd|        ud_int5                       engine/source/implicit/upd_glob_k.f
!hd|        updk_mv                       engine/source/airbag/monv_imp0.f
!hd|        upd_fr                        engine/source/mpi/implicit/imp_fri.f
!hd|        upd_glob_k                    engine/source/implicit/upd_glob_k.f
!hd|        upd_int_k                     engine/source/implicit/upd_glob_k.f
!hd|        upd_kml                       engine/source/mpi/implicit/imp_fri.f
!hd|        upd_ksl                       engine/source/mpi/implicit/imp_fri.f
!hd|        upd_rhs                       engine/source/implicit/upd_glob_k.f
!hd|        upd_rhs_fr                    engine/source/implicit/imp_solv.f
!hd|        wrcomi                        engine/source/output/restart/wrcomm.f
!hd|        write_intbuf                  engine/source/output/restart/write_intbuf.f
!hd|        wrrestp                       engine/source/output/restart/wrrestp.f
!hd|        imp_intbuf                    engine/share/modules/imp_mod_def.f
!hd|        intbuf_mod                    engine/share/modules/restart_mod.f
!hd|        update_weight_inter_type_24_25starter/source/spmd/domain_decomposition/update_weight_inter_type_24_25.f
!hd|-- calls ---------------
!hd|        int8_mod                      modules/interfaces/int8_mod.f
!hd|        metric_mod                    modules/metric_mod.f
!hd|====================================================================
module intbufdef_mod
!-----------------------------------------------------------------------
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
   use int8_mod
   use metric_mod


#include "my_real.inc"
!
!=======================================================================
   type intbuf_struct_
!=================================================
! define typeintbuf_struct_ for new interface buffer intbuf_tab
! module is organized as following :
!   - define integer arrays sizes
!   - define float arrays sizes
!   - define integer arrays
!   - define float arrays

!=======================================================================
! define sizes (integers arrays)
!=======================================================================
      integer ::   s_irects     !  4*nrts     :irects:connectivites faces seconds     :1,2,3,4
      integer ::   s_irectm     !  4*nrtm_fe+4*9*nrtm_ige :irectm:connectivites faces main  :1,2,3,4,5,6,7,8,20,24
      integer ::   s_nsv        !  nsn        :nsv   :noeuds seconds                  :1,2,3,4,  7
      integer ::   s_msr        !  nmn+16*nrtm_ige  :msr   :noeuds mains                   :1,2,3,4
      integer ::   s_irtlm      !  nsn        :face main la plus proche    :1,2,3,4
      integer ::   s_irupt      !  nsn        :irupt :flag rupture                     :2
      integer ::   s_inorm      !  nsn        :inorm  :main orientation flag: type2 avec rupture
      integer ::   s_ielec      !  (type20)
      integer ::   s_ieles      !  nrts       :ieles :element secnd                    :              9
      integer ::   s_lisub      !  nisub      :lisub :liste des sous-interfaces        :     7
      integer ::   s_typsub     !  nisub      :typsub :type of subinterfaces           :     25
      integer ::   s_addsubs    !  nsn+1      :addsubs:adresse ds zone de ss interf. cote second. :     7, 10, 24, 25
      integer ::   s_addsubm    !  nrtm+1     :addsubm:adresse ds zone de ss interf. cote main  :     7, 10, 24, 25
      integer ::   s_lisubs     !  nisubs     :lisubs :zone des ss interf. cote second.           :     7, 10, 24, 25
      integer ::   s_lisubm     !  nisubm     :lisubm :zone des ss interf. cote main            :     7, 10, 24, 25
      integer ::   s_inflg_subs !  nisubs     :inflg_subs: zone d appartenance a s1/s2 des ss interf. cote second. : 25
      integer ::   s_inflg_subm !  nisubs     :inflg_subs: zone d appartenance a s1/s2 des ss interf.cote main   : 25
      integer ::   s_msegtyp    !  nrtm       :msegtyp: element type                   :      2,3,5,7,10,21,23; used only in starter
      integer ::   s_cand_e     !  multimp*nsn:cand_e:facettes candidates              :7,10,11,20
      integer ::   s_cand_n     !  multimp*nsn:cand_n:noeuds candidats                 :7,10,11,20
      integer ::   s_i_stok     !             : ii_stok                                : 7    10,11,20,24
      integer ::   s_i_stok_e   !             : ii_stok_e                              : 7    10,11,20,24
      integer ::   s_ifpen      !  multimp*nsn:ifpen :flag penetr. (filtr frottement)  :           7
      integer ::   s_kremnode   !  nrtm+1     :kremnode:adress nodes removed from contact in remnode array :      7
      integer ::   s_remnode    !  nremnode   :remnode: nodes removed from contact     :      7
      integer ::   s_adccm      !  nrtm       :addcm :adresse dans chaine du 1er main:                  11
      integer ::   s_chain      !  2*multimp*nsn:chaine(1,adds) : bord second.     :                  11
!type20
      integer ::   s_daanc6     !  18*2*nln   :daanc6:contient 18*nln double precision : 20
      integer ::   s_nbinflg    !  nbinflg    :nln    :flags binaire noeuds            nbin:      20, 24, 25
      integer ::   s_mbinflg    !  mbinflg    :nrtm  :flag multiusage facettes        :      20, 24, 25
      integer ::   s_ebinflg    !  ebinflg    :nedge :flag multiusage edges           :      25
      integer ::   s_nlg        !  nln        :nlg   :local to global node             :      20
      integer ::   s_islins     !  2*nlins    :islins:surface second. et cote          :      20
      integer ::   s_islinm     !  2*nlinm    :islinm:surface main et cote           :      20
      integer ::   s_ixlins     !  2*nlins    :ixlins:connectivites bords seconds     :      20
      integer ::   s_ixlinm     !  2*nlinm    :ixlinm:connectivites bords mains      :      20
      integer ::   s_nsvl       !  nsne       :nsvl  :noeuds seconds des lignes       :      20
      integer ::   s_msrl       !  nmne       :msrl  :noeuds mains                   :      20
      integer ::   s_lcand_n    !  multimp*nmne:lcand_n:lignes mains candidates      :      20
      integer ::   s_lcand_s    !  multimp*nsne:lcand_n:lignes seconds candidates     :      20
      integer ::   s_adccm20    !  nlinm      :addcm :adresse dans chaine du 1er main:      20
      integer ::   s_chain20    !  2*multimp*nsne:chaine(1,adds) : bord second.        :      20            11                                  !                       :chaine(2,adds) : adresse suivante       :      20
!type1
      integer ::   s_ilocs      !  nsn        :ilocs :noeud main le plus proche      :1, ,3,4
      integer ::   s_nsegm      !  1+nmn      :nsegm :adresse du vecteur lmsr (main) :1, ,3,4,5
      integer ::   s_nrt        !  nrt        :lmsr  :faces connectees au noeud main :1, ,3,4,5
!type2
      integer ::   s_msegtyp2   !  :nrtm      :msegtyp: main segment type sol/shell  :2
      integer ::   s_csts_bis   !  :nrtm      :msegtyp: main segment type sol/shell  :2
!type3
      integer ::   s_irtls      !  nmn        :irtls :face second. la plus proche      :    3,4
      integer ::   s_ilocm      !  nmn        :ilocm :noeud second. le plus proche     :    3,4
      integer ::   s_irtlom     !  nsn        :irtlom:anc. face main. la plus proche  :    3,4
      integer ::   s_irtlos     !  nmn        :irtlos:anc. face secnd. la plus proche  :    3,4
      integer ::   s_nsegs      !  1+nsn      :nsegs :adresse du vecteur lnsv (second):    3,4,5
      integer ::   s_lnsv       !  nrt        :lmsr  :faces connectees au noeud main :1, ,3,4,5
      integer ::   s_lmsr       !  nrt        :lmsr  :faces connectees au noeud main :1, ,3,4,5
!type4
      integer ::   s_ielem      !  nrtm       :ielem :element main                   :              9
!type12
      integer ::   s_fcount     !  nsn: 12
!type14
      integer ::   s_ksurf      !
      integer ::   s_impact     !  nsn
!type21
      integer ::   s_msr21      !  nmng    :msr21: msr global     :      21
      integer ::   s_mndd
      integer ::   s_msr_l
!type24
      integer ::   s_mvoisin    !  4*nrtm     :mvoisin: facettes mains voisines:(at least two commun nodes) : 24
      integer ::   s_nvoisin    !  2*4*nrtm   :nvoisin: noeuds mains voisines      :24
      integer ::   s_mseglo     !  nrtm       :mseglo: global segment number (spmd)    :24, 25
      integer ::   s_msegtyp24  !  nrtm       :msegtyp: shell segment type             :24
!type25
      integer ::   s_evoisin    !  4*nrtm            :evoisin: no edge voisine
      integer ::   s_admsr      !  4*nrtm            :admsr  : adresse des normales aux noeuds mains : 25
      integer ::   s_ledge      !  4*nedge           :ledge  : description des edges : 25
      integer ::   s_lbound     !  nadmsr            :lbound : index des sommets sur les aretes libres : 25
      integer ::   s_actnor     !  nadmsr            :actnor : tag des normales actives (0/1) : 25
      integer ::   s_farm       !  4*multimp*nsn     :farm   :flag in/out wrt 4 sub-triangles : 25
      integer ::   s_adskyn     !  4*nrtm+1          :adskyn : skyline pour assemblage parith/on des normales : 25
      integer ::   s_iadnor     !  4*nrtm            :iadnor : skyline pour assemblage parith/on des normales : 25
      integer ::   s_islide     !  4*nsn             :islide : sommets (0  4) sur lesquels le noeud secnd glisse : 25
      integer ::   s_knor2msr   !  nadmsr+1          :knor2msr: adress of connected segments to normals in nor2msr : 25
      integer ::   s_nor2msr    !  ...               :nor2msr : connected segments to normals in nor2msr : 25
      integer ::   s_cand_opt_n !  multimp*nsn       :cand_opt_n : candidats apres optimisation <=> nd second. (i25optcd.f)
      integer ::   s_cand_opt_e !  multimp*nsn       :cand_opt_e : candidats apres optimisation <=> segment main (i25optcd.f)
      integer ::   s_if_adh     !  nsn               :if_adh: if adhesion spring exists (1) or not (0)  :  25
      integer ::   s_candm_e2e  !  multimp*nconte    :candm_e2e : main line : aretes candidates mains (shell & beams))
      integer ::   s_cands_e2e  !  multimp*nconte    :cands_e2e : secnd line : aretes candidates secnds
      integer ::   s_candm_e2s  !  multimp*nconte    :candm_e2s : main segment : facettes candidates mains (solid edges)
      integer ::   s_cands_e2s  !  multimp*nconte    :cands_e2s : secnd line : aretes candidates secnds
      integer ::   s_candl_max
      integer ::   s_cands_max
      integer ::   s_addsube    !  nedge+1     :addsubes:adresse ds zone edges de ss interf. cote second.             : 25
      integer ::   s_lisube     !  nisube      :lisube  :zone edges des ss interf. cote second.                       : 25
      integer ::   s_inflg_sube !  nisube      :inflg_sube :: zone d appartenance a s1/s2 des ss interf.cote main   : 25
!-------s_iseadd,s_isedge,s_cand_t will be cleaned after, for the moment with size 0
      integer ::   s_iseadd     !  iseadd     :secnd edge address                      :      24
      integer ::   s_isedge     !  l24add     :isedge:secnd edges nodes and flags      :      24
      integer ::   s_cand_t     !  multimp*nsn:cand_t:candidate type                   :      24
!            ! =0 node to surface candidate
!            ! =1 node or edge to surface candidate
!            ! =2 edge to surface candidate
      integer ::   s_iseg_pxfem !  iseg_pxfem:       :      24
      integer ::   s_iseg_ply   !  iseg_ply          :      24

      integer ::   s_icont_i    !  nsn not detected pene_ini <0 :   24, 25
      integer ::   s_ielem_m    !  2*nrtm element connected to main segment :   25
      integer ::   s_proc_mvoisin!  4*nrtm proc of neighbhoor segment :   25

!---- edge
      integer ::   s_irtse      !  (5,nrtse)
      integer ::   s_is2se      !  (2,nsne)
      integer ::   s_is2pt      !  (nsne)
      integer ::   s_ispt2      !  (nsn) - tag for irtlm on fictive node
      integer ::   s_isegpt     !  (nsn)
      integer ::   s_is2id      !  (nsne)
      integer ::   s_ifpen_e    !  (multimpe*nconte) : filter candidat friction       :25
      integer ::   s_ifpen_e2s  !  (multimpe*nconte) : filter candidat friction       :25
!---- ige
      integer ::   s_nige
!---- inter friction parts
      integer ::   s_ipartfrics!  nsn       :ipartfrics : number of part of secnd nodes
      integer ::   s_ipartfricm!  nrtm      :ipartfricm : number of part of main segments
      integer ::   s_ipartfric_e!  nedge    :ipartfric_e : number of part of edge int25
!       orthotropic friction
      integer ::   s_irep_fricm !  nrtm      :irep_fric : orthotropic system formulation flag for reference vector for friction (same as iorth/irep for prop)
!---- irem gap option for inter type25
      integer ::   s_kremnor!  nsn+1    :kremnor : skyline tab for main segment adress in remnor tab
      integer ::   s_remnor!   remnode  :remnor  : tab for main segment forbidden for each secnd node
      integer ::   s_kremnode_edg   !  nedge+1    :kremnode_edg:adress lines removed from contact in remnode array :      25
      integer ::   s_remnode_edg    !  nremnode_edg   :remnode_edg: lines removed from contact     :      25
      integer ::   s_kremnode_e2s   !  nrtm+1    :kremnode_edg:adress lines removed from contact in remnode array :      25
      integer ::   s_remnode_e2s    !  nremnode_e2s   :remnode_edg: lines removed from contact     :      25
!----nitsche method
      integer ::   s_ielnrts!  nrts      :ielnrts : number of secnd element
      integer ::   s_adrects!  4*nrts    :adrects : adress of each secnd node of the segment in ixs/ixs10/ixs20/ixs16
      integer ::   s_facnrts!  nrts      :facnrts : corresponding facet in element ielnrts
      integer ::   s_e2s_actnor!
!=======================================================================
! define sizes (float arrays)
!=======================================================================
      integer ::   s_stfac        !  1          :stfac :facteur de rigidite d'interface(manager)
      integer ::   s_variables    !   +1, ...
      integer ::   s_csts         !  2*nsn      :csts  :s et t des noeuds seconds       :   ,3,4,
      integer ::   s_dpara        !  7*nsn      :dpara :det,b1,b2,b3,c1,c2,c3            :  2
      integer ::   s_nmas         !  nmn        :mmas  :masse noeuds mains             :  2
      integer ::   s_smas         !  nsn      :smas : masse et inertie des noeuds secnd :   2
      integer ::   s_siner        !  nsn      :siner : masse et inertie des noeuds secnd :   2
      integer ::   s_areas2       !  nsn       area       : secnd area                  :  int2 with rupture
      integer ::   s_uvar         !  nsn*nuvar uvar       : user buffer for secnds      :  int2 with rupture
      integer ::   s_xm0          !  nsn*3     xm0(x,y,z) : secnd-main initial distance: int2 with rupture
      integer ::   s_spenalty     !  nsn      :
      integer ::   s_stfr_penalty !  nsn      :s
      integer ::   s_skew         !  ilev=25  : nsn*3     xm0(x,y,z) : secnd-main initial distance: int2 with rupture
      integer ::   s_dsm          !  nsn*3     dsm(x,y,z) : secnd-main displacement    :  int2 with rupture
      integer ::   s_fsm          !  nsn*3     fsm(x,y,z) : secnd-main force           :  int2 with rupture
      integer ::   s_rupt         !  6         rupt       : penality/rupture parameters  :  int2 with rupture
      integer ::   s_fini         !  ilev=25   :3*nsn  :  int2 with rupture
      integer ::   s_stfns        !  nsn        :stfns :rigidite noeuds seconds         :   ,3,4,    7
      integer ::   s_stfm         !  nrtm       :stfm  :rigidite faces main        :   ,3,4,    7  10,11
      integer ::   s_stfs         !  nrts       :stfs  :rigidite faces seconds          :   ,3,4          11
      integer ::   s_penim        !  2*nrtm     :penim :pene initiale max / main       :     20           11
      integer ::   s_penis        !  2*nrts     :penis :pene initiale max / secnd        :                  11
      integer ::   s_gap_m        !  nrtm       :gap_m :gap faces main             :(7)10,11,24
      integer ::   s_gap_s        !  nsn        :gap_s :gap noeuds seconds              :(7)10,20
      integer ::   s_xsav         !  3*min(numnod,nsn+nmn):crit  :x(xyz)                   :            7
      integer ::   s_crit         !  12         :crit  :min et max deplacement (spmd)    :            7 (10,11)
      integer ::   s_fric_p       !  10         :fric_p:friction parameters              :        5,  7     24
      integer ::   s_xfiltr       !  1          :xfiltr:coeff filtrage frottement     )  :        5,  7     24
      integer ::   s_areas        !  nsn       area       : secnd area                  :  7
      integer ::   s_aream        !  nrtm      area       : main area                  :  11
      integer ::   s_gap_sl       !  nsn        :gap_s_l :
      integer ::   s_gap_ml       !  nrtm       :gap_m_l :
      integer ::   s_cand_p       !  multimp*nsn:cand_p:penetration initiale             :           (7)
      integer ::   s_ftsavx       !  multimp*nsn:ftsavx :    sauvegarde filtrage         :            7, 8  24
      integer ::   s_ftsavy       !  multimp*nsn:ftsavy :    sauvegarde filtrage         :            7, 8  24
      integer ::   s_ftsavz       !  multimp*nsn:ftsavz :    sauvegarde filtrage         :            7, 8  24
      integer ::   s_ftsavx_e   !  multimp*nconte    :ftsavx_e : friction for saving     :            25
      integer ::   s_ftsavy_e   !  multimp*nconte    :ftsavx_e : friction for saving     :            25
      integer ::   s_ftsavz_e   !  multimp*nconte    :ftsavx_e : friction for saving     :            25
      integer ::   s_ftsavx_e2s !  multimp*nconte    :ftsavx_e2s : friction for saving   :            25
      integer ::   s_ftsavy_e2s !  multimp*nconte    :ftsavx_e2s : friction for saving   :            25
      integer ::   s_ftsavz_e2s !  multimp*nconte    :ftsavx_e2s : friction for saving   :            25
!---- ige
      integer ::   s_rige
      integer ::   s_xige
      integer ::   s_vige
      integer ::   s_massige
!type10
      integer ::   s_cand_f       !  6*4*nsn    :cand_f:ancienne forces locales +h1 a h3 :               10
!type11
      integer ::   s_cand_max
!type20
      integer ::   s_cand_fx      !
      integer ::   s_cand_fy      !
      integer ::   s_cand_fz      !
      integer ::   s_xa           !  3*nsn;3*nln:xa    :xa des noeuds                    :       20
      integer ::   s_va           !  3*nsn;3*nln:va    :va des noeuds                    :       2
      integer ::   s_stfa         !
      integer ::   s_penia        !  5*nln      :penia :pene initiale points d'ancrages  :     20
      integer ::   s_alphak       !  3*nln      :alphak:reduction de rigidite d'ancrages :     20
      integer ::   s_gap_sh       !  nrtm       :gap_sh:shift de gap pour gap nul solides:     20
      integer ::   s_avx_ancr     !  9*nln :a-v-x-ancr :accel vit deplacement point d'ancrage: 20
      integer ::   s_critx        !
      integer ::   s_gap_me       !
      integer ::   s_stf          !  nlinm      :stfm  :rigidite faces main        :       20
      integer ::   s_penime       !  2*nlinm    :penime:pene initiale max / edge main       :       20
      integer ::   s_gap_se       !
      integer ::   s_penise       !  2*nlins    :penise:pene initiale max / edge secnd   :       20
      integer ::   s_stfne        !  nsne       :stfne :rigidite noeuds seconds         :       20
!type1
      integer ::   s_n            !  3*nsn      :n     :normale                          :1,
!type3,4,5,9
      integer ::   s_cstm         !  2*nmn      :cstm  :s et t des noeuds mains        :   ,3,4,
      integer ::   s_stfnm        !  nmn        :stfnm :rigidite noeuds mains          :   ,3,4
      integer ::   s_fricos       !  3*nsn      :fricos:force de friction(second)       :   ,3,4,
      integer ::   s_fricom       !  3*nmn      :fricom:force de friction(main)        :   ,3,4,
      integer ::   s_ftsav        !  3*nsn      :ftsav :     sauvegarde filtrage         :        5
!type6
      integer ::   s_fcont        !  1          :fcont :                                 :          6
      integer ::   s_fs           !  3*nsn      :fs    :sauvegarde de la penetration     :          6
      integer ::   s_fm           !  3*nsn      :fm    :sauvegarde de la penetration     :          6
      integer ::   s_rmas         !  2          :rmas  :masse des rigid body             :          6
      integer ::   s_ansmx0
!type 8
      integer ::   s_t8          !   size of t8 structure  (1 if type8, 0 elsewhere)            :          8
      integer ::   s_gapn         !   nrtm
      integer ::   s_stf8         !   nrtm

!type 9
      integer ::   s_ee           !  ee    :energie de frottement            : 9

!type14
      integer ::   s_cimp         !  3*nsn    :
      integer ::   s_nimp         !  3*nsn    :
!type15
      integer ::   s_iold         !
      integer ::   s_hold         !
      integer ::   s_nold         !
      integer ::   s_dold         !
!type17
      integer ::   s_ks           !
      integer ::   s_km           !
      integer ::   s_frots        !
      integer ::   s_frotm        !
!type21
      integer ::   s_nod_normal   !
      integer ::   s_rcurv        !
      integer ::   s_anglm        !
      integer ::   s_frot_p       !
      integer ::   s_alpha0       !
      integer ::   s_as           !
      integer ::   s_bs           !
      integer ::   s_thknod0      !
!type22
!type23
!type24
      integer ::   s_gapn_m       !  nmn        :gapn_m:nodal main gap                 :  24, 25
      integer ::   s_secnd_fr      !  6*nsn      :secnd_fr:new(1:3) and old(4:6) friction f:  24
      integer ::   s_pene_old     !  nsn*5      :pene_old:old penetration,pen_ini(inacti=5):       24
      integer ::   s_stif_old     !  nsn*2      :stif_old:old stifness                   :       24
      integer ::   s_time_s       !  nsn        :time_s :   impact time                  :  24
      integer ::   s_gap_nm       !  12*nrtm
!---------to be cleaned later
      integer ::   s_edge8l2      !  nsn        :edge8l2: max half edge length on secnd node: 24
      integer ::   s_nod_2ry_lgth !  nsn        :nodal secnd length (to cal. kg)         :     24
      integer ::   s_nod_mas_lgth !  nmn        :nodal main length (to cal. kg)        :     24
      integer ::   s_gap_n0       !  12*nrtm        :
      integer ::   s_dgap_nm      !  4*nrtm        :
      integer ::   s_dgap_m       !  nrtm        :
      integer ::   s_delta_pmax_dgap !  1        :i
      integer ::   s_xfic         !  3*nsne
      integer ::   s_vfic         !  3*nsne
      integer ::   s_msfic        !  nsne

!type25
      integer ::   s_edge_bisector       !  3*4*nrtm
      integer ::   s_penm         !  4*multimp*nsn      :penm:penetrations wrt 4 sub-triangles :  25
      integer ::   s_distm        !  multimp*nsn        :distm:true distance to segment        :  25
      integer ::   s_lbm          !  multimp*nsn        :lbm:coordinate lb wrt 4 triangles     :  25
      integer ::   s_lcm          !  multimp*nsn        :lcm:coordinate lc wrt 4 triangles     :  25
      integer ::   s_vtx_bisector !  3*2*nadmsr         :                                      :  25
      integer ::   s_cand_ps      !  4*multimps*nconte:cand_ps:penetration initiale vs solid main edges: 25
      integer ::   s_gape         !  nedge
      integer ::   s_gap_e_l      !  nedge
      integer ::   s_stfe         !  nedge
      integer ::   s_gapmsav      ! nrtm
      integer ::   s_e2s_nod_normal ! 3*nadmsr

      integer :: number_edge_type1
      integer :: number_edge_type1_0
!       orthotropic friction
      integer ::   s_dir_fricm !  2*nrtm         : orhotropic directions for friction
!   stiffness based on mass and time step
      integer ::   s_stifmsdt_s       ! nsn : nodal mass on secondary node *stfacm
      integer ::   s_stifmsdt_m       ! nrtm : main mass *stfacm
      integer ::   s_stifmsdt_edg     ! nedge : edge mass *stfacm
!
!=======================================================================
! define arrays (integers arrays)
!=======================================================================
      integer, dimension(:) , allocatable ::  irects
      integer, dimension(:) , allocatable ::  irectm
      integer, dimension(:) , allocatable ::  nsv
      integer, dimension(:) , allocatable ::  msr
      integer, dimension(:) , allocatable ::  irtlm
      integer, dimension(:) , allocatable ::  irupt
      integer, dimension(:) , allocatable ::  inorm
      integer, dimension(:) , allocatable ::  ielec
      integer, dimension(:) , allocatable ::  ieles
      integer, dimension(:) , allocatable ::  lisub
      integer, dimension(:) , allocatable ::  typsub
      integer, dimension(:) , allocatable ::  addsubs
      integer, dimension(:) , allocatable ::  addsubm
      integer, dimension(:) , allocatable ::  lisubs
      integer, dimension(:) , allocatable ::  lisubm
      integer, dimension(:) , allocatable ::  inflg_subs
      integer, dimension(:) , allocatable ::  inflg_subm
      integer, dimension(:) , allocatable ::  addsube
      integer, dimension(:) , allocatable ::  lisube
      integer, dimension(:) , allocatable ::  inflg_sube
      integer, dimension(:) , allocatable ::  msegtyp
      integer, dimension(:) , allocatable ::  cand_e
      integer, dimension(:) , allocatable ::  cand_n
      integer, dimension(:) , allocatable ::  i_stok
      integer, dimension(:) , allocatable ::  i_stok_e
      integer, dimension(:) , allocatable ::  ifpen
      integer, dimension(:) , allocatable ::  kremnode
      integer, dimension(:) , allocatable ::  remnode
      integer, dimension(:) , allocatable ::  adccm
      integer, dimension(:) , allocatable ::  chain
      integer, dimension(:) , allocatable ::  nige
!type20
      integer, dimension(:) , allocatable ::  nbinflg
      integer, dimension(:) , allocatable ::  mbinflg
      integer, dimension(:) , allocatable ::  ebinflg
      integer, dimension(:) , allocatable ::  nlg
      integer, dimension(:) , allocatable ::  daanc6
      integer, dimension(:) , allocatable ::  islins
      integer, dimension(:) , allocatable ::  islinm
      integer, dimension(:) , allocatable ::  ixlins
      integer, dimension(:) , allocatable ::  ixlinm
      integer, dimension(:) , allocatable ::  nsvl
      integer, dimension(:) , allocatable ::  msrl
      integer, dimension(:) , allocatable ::  lcand_n
      integer, dimension(:) , allocatable ::  lcand_s
      integer, dimension(:) , allocatable ::  adccm20
      integer, dimension(:) , allocatable ::  chain20
!type1
      integer, dimension(:) , allocatable ::  ilocs
      integer, dimension(:) , allocatable ::  nsegm
      integer, dimension(:) , allocatable ::  nrt
!type2
      integer, dimension(:) , allocatable ::  msegtyp2
!type3
      integer, dimension(:) , allocatable ::  irtls
      integer, dimension(:) , allocatable ::  ilocm
      integer, dimension(:) , allocatable ::  irtlom
      integer, dimension(:) , allocatable ::  irtlos
      integer, dimension(:) , allocatable ::  nsegs
      integer, dimension(:) , allocatable ::  lnsv
      integer, dimension(:) , allocatable ::  lmsr
!type4
      integer, dimension(:) , allocatable ::  ielem
!type12
      integer, dimension(:) , allocatable ::  fcount
!type14
      integer, dimension(:) , allocatable ::  ksurf
      integer, dimension(:) , allocatable ::  impact

!type21
      integer, dimension(:) , allocatable ::  msr21
      integer, dimension(:) , allocatable ::  mndd
      integer, dimension(:) , allocatable ::  msr_l
!type24
      integer, dimension(:) , allocatable ::  mvoisin
      integer, dimension(:) , allocatable ::  nvoisin
      integer, dimension(:) , allocatable ::  mseglo
      integer, dimension(:) , allocatable ::  msegtyp24
!--------to be cleaned later
      integer, dimension(:) , allocatable ::  iseadd
      integer, dimension(:) , allocatable ::  isedge
      integer, dimension(:) , allocatable ::  cand_t
      integer, dimension(:) , allocatable ::  iseg_pxfem
      integer, dimension(:) , allocatable ::  iseg_ply
      integer, dimension(:) , allocatable ::  icont_i
      integer, dimension(:) , allocatable ::  irtse
      integer, dimension(:) , allocatable ::  is2se
      integer, dimension(:) , allocatable ::  is2pt
      integer, dimension(:) , allocatable ::  ispt2
      integer, dimension(:) , allocatable ::  isegpt
      integer, dimension(:) , allocatable ::  is2id      ! global id fictive nodes
!type25
      integer, dimension(:) , allocatable ::  nsv_on_pmain     !
      integer, dimension(:) , allocatable ::  evoisin   !
      integer, dimension(:) , allocatable ::  admsr     !
      integer, dimension(:) , allocatable ::  ledge     !
      integer, dimension(:) , allocatable ::  lbound    !
      integer, dimension(:) , allocatable ::  free_irect_id!
      integer, dimension(:) , allocatable ::  actnor    !
      integer, dimension(:) , allocatable ::  farm      !
      integer, dimension(:) , allocatable ::  adskyn    !
      integer, dimension(:) , allocatable ::  iadnor    !
      integer, dimension(:) , allocatable ::  islide    !
      integer, dimension(:) , allocatable ::  knor2msr  !
      integer, dimension(:) , allocatable ::  nor2msr   !
      integer, dimension(:) , allocatable ::  cand_opt_n!
      integer, dimension(:) , allocatable ::  cand_opt_e!
      integer, dimension(:) , allocatable ::  if_adh    ! type25 and interface adhesion
      integer, dimension(:) , allocatable ::  candm_e2e
      integer, dimension(:) , allocatable ::  cands_e2e
      integer, dimension(:) , allocatable ::  candm_e2s
      integer, dimension(:) , allocatable ::  cands_e2s
      integer, dimension(:) , allocatable ::  ifpen_e

      integer, dimension(:) , allocatable ::  ifpen_e2s
      integer, dimension(:), allocatable :: edge_type1
      integer, dimension(:), allocatable :: edge_type1_0
      integer, dimension(:) , allocatable ::  ielem_m
      integer, dimension(:) , allocatable ::  proc_mvoisin
      
!---- inter friction parts
      integer, dimension(:) , allocatable ::  ipartfrics
      integer, dimension(:) , allocatable ::  ipartfricm
      integer, dimension(:) , allocatable ::  ipartfric_e
!      orthotropic friction
      integer, dimension(:) , allocatable ::  irep_fricm
!---- irem gap option for interface type 25
      integer, dimension(:) , allocatable ::  kremnor
      integer, dimension(:) , allocatable ::  remnor
      integer, dimension(:) , allocatable ::  kremnode_edg
      integer, dimension(:) , allocatable ::  remnode_edg
      integer, dimension(:) , allocatable ::  kremnode_e2s
      integer, dimension(:) , allocatable ::  remnode_e2s
!----nitsche method
      integer, dimension(:) , allocatable ::  ielnrts
      integer, dimension(:) , allocatable ::  adrects
      integer, dimension(:) , allocatable ::  facnrts

      integer, dimension(:) , allocatable ::  e2s_actnor

!----t25 sorting buffers
      integer, dimension(:) , allocatable ::  i25_cand_a
      integer, dimension(:) , allocatable ::  i25_cand_b
!=======================================================================
! define arrays float arrays
!=======================================================================
      my_real, dimension(:) , allocatable ::   stfac
      my_real, dimension(:) , allocatable ::   variables
      my_real, dimension(:) , allocatable ::   csts
      my_real, dimension(:) , allocatable ::   dpara
      my_real, dimension(:) , allocatable ::   nmas
      my_real, dimension(:) , allocatable ::   smas
      my_real, dimension(:) , allocatable ::   siner
      my_real, dimension(:) , allocatable ::   areas2
      my_real, dimension(:) , allocatable ::   uvar
      my_real, dimension(:) , allocatable ::   xm0
      my_real, dimension(:) , allocatable ::   spenalty
      my_real, dimension(:) , allocatable ::   stfr_penalty
      my_real, dimension(:) , allocatable ::   skew
      my_real, dimension(:) , allocatable ::   dsm
      my_real, dimension(:) , allocatable ::   fsm
      my_real, dimension(:) , allocatable ::   rupt
      my_real, dimension(:) , allocatable ::   fini
      my_real, dimension(:) , allocatable ::   stfns
      my_real, dimension(:) , allocatable ::   stfm
      my_real, dimension(:) , allocatable ::   stfs
      my_real, dimension(:) , allocatable ::   penim
      my_real, dimension(:) , allocatable ::   penis
      my_real, dimension(:) , allocatable ::   gap_m
      my_real, dimension(:) , pointer     ::   gap_s    ! GAP_S is associate in Starter / Pointer is need
      my_real, dimension(:) , allocatable ::   xsav
      my_real, dimension(:) , allocatable ::   crit
      my_real, dimension(:) , allocatable ::   fric_p
      my_real, dimension(:) , allocatable ::   xfiltr
      my_real, dimension(:) , allocatable ::   areas
      my_real, dimension(:) , allocatable ::   aream
      my_real, dimension(:) , allocatable ::   gap_sl
      my_real, dimension(:) , allocatable ::   gap_ml
      my_real, dimension(:) , allocatable ::   cand_p
      my_real, dimension(:) , allocatable ::   ftsavx
      my_real, dimension(:) , allocatable ::   ftsavy
      my_real, dimension(:) , allocatable ::   ftsavz
      my_real, dimension(:) , allocatable ::   ftsavx_e
      my_real, dimension(:) , allocatable ::   ftsavy_e
      my_real, dimension(:) , allocatable ::   ftsavz_e
      my_real, dimension(:) , allocatable ::   ftsavx_e2s
      my_real, dimension(:) , allocatable ::   ftsavy_e2s
      my_real, dimension(:) , allocatable ::   ftsavz_e2s
!---- ige
      my_real, dimension(:) , allocatable ::   rige
      my_real, dimension(:) , allocatable ::   xige
      my_real, dimension(:) , allocatable ::   vige
      my_real, dimension(:) , allocatable ::   massige
!type10
      my_real, dimension(:) , allocatable ::   cand_f
!type20
      my_real, dimension(:) , allocatable ::   cand_fx
      my_real, dimension(:) , allocatable ::   cand_fy
      my_real, dimension(:) , allocatable ::   cand_fz
      my_real, dimension(:) , allocatable ::   xa
      my_real, dimension(:) , allocatable ::   va
      my_real, dimension(:) , allocatable ::   stfa
      my_real, dimension(:) , allocatable ::   penia
      my_real, dimension(:) , allocatable ::   alphak
      my_real, dimension(:) , allocatable ::   gap_sh
      my_real, dimension(:) , allocatable ::   avx_ancr
      my_real, dimension(:) , allocatable ::   critx
      my_real, dimension(:) , allocatable ::   gap_me
      my_real, dimension(:) , allocatable ::   stf
      my_real, dimension(:) , allocatable ::   penime
      my_real, dimension(:) , allocatable ::   gap_se
      my_real, dimension(:) , allocatable ::   penise
      my_real, dimension(:) , allocatable ::   stfne
!type1
      my_real, dimension(:) , allocatable ::   n
!type3,4,9
      my_real, dimension(:) , allocatable ::   cstm
      my_real, dimension(:) , allocatable ::   stfnm
      my_real, dimension(:) , allocatable ::   fricos
      my_real, dimension(:) , allocatable ::   fricom
      my_real, dimension(:) , allocatable ::   ftsav
!type6
      my_real, dimension(:) , allocatable ::   fcont
      my_real, dimension(:) , allocatable ::   fs
      my_real, dimension(:) , allocatable ::   fm
      my_real, dimension(:) , allocatable ::   rmas
      my_real, dimension(:) , allocatable ::   ansmx0
!type 8
      my_real, dimension(:) , allocatable ::   gapn
      my_real, dimension(:) , allocatable ::   stf8
      my_real, dimension(:) , allocatable ::   ee
!type14
      my_real, dimension(:) , allocatable ::   cimp
      my_real, dimension(:) , allocatable ::   nimp

!type15
      my_real, dimension(:) , allocatable ::   iold
      my_real, dimension(:) , allocatable ::   hold
      my_real, dimension(:) , allocatable ::   nold
      my_real, dimension(:) , allocatable ::   dold
!type16 + type17
      my_real :: XSLVG(7)
      my_real :: XMSRG(7)
!type17
      my_real, dimension(:) , allocatable ::   ks
      my_real, dimension(:) , allocatable ::   km
      my_real, dimension(:) , allocatable ::   frots
      my_real, dimension(:) , allocatable ::   frotm

!type21
      my_real, dimension(:) , allocatable ::   nod_normal
      my_real, dimension(:) , allocatable ::   rcurv
      my_real, dimension(:) , allocatable ::   anglm
      my_real, dimension(:) , allocatable ::   frot_p
      my_real, dimension(:) , allocatable ::   alpha0
      my_real, dimension(:) , allocatable ::   as
      my_real, dimension(:) , allocatable ::   bs
      my_real, dimension(:) , allocatable ::   thknod0

!type24
      my_real, dimension(:) , allocatable ::   gapn_m
      my_real, dimension(:) , allocatable ::   secnd_fr
      my_real, dimension(:) , allocatable ::   pene_old
      my_real, dimension(:) , allocatable ::   stif_old
      my_real, dimension(:) , allocatable ::   time_s
      my_real, dimension(:) , allocatable ::   gap_nm
!-------to be cleaned later
      my_real, dimension(:) , allocatable ::   edge8l2
      my_real, dimension(:) , allocatable ::   nod_2ry_lgth
      my_real, dimension(:) , allocatable ::   nod_mas_lgth
      my_real, dimension(:) , allocatable ::   gap_n0
      my_real, dimension(:) , allocatable ::   dgap_nm
      my_real, dimension(:) , allocatable ::   dgap_m
      my_real, dimension(:) , allocatable ::   delta_pmax_dgap
      my_real, dimension(:) , allocatable ::   xfic
      my_real, dimension(:) , allocatable ::   vfic
      my_real, dimension(:) , allocatable ::   msfic
!type25
      real(4), dimension(:) , allocatable ::   edge_bisector  !
      my_real, dimension(:) , allocatable ::   penm    !
      my_real, dimension(:) , allocatable ::   distm   !
      my_real, dimension(:) , allocatable ::   lbm     !
      my_real, dimension(:) , allocatable ::   lcm     !
      real(4), dimension(:) , allocatable ::   vtx_bisector !
      my_real, dimension(:) , allocatable ::   cand_ps
      my_real, dimension(:) , allocatable ::   gape
      my_real, dimension(:) , allocatable ::   gap_e_l
      my_real, dimension(:) , allocatable ::   stfe
      my_real, dimension(:) , allocatable ::   gapmsav
      real(4), dimension(:) , allocatable ::   e2s_nod_normal  !

      my_real, dimension(:) , allocatable ::   stifmsdt_s
      my_real, dimension(:) , allocatable ::   stifmsdt_m
      my_real, dimension(:) , allocatable ::   stifmsdt_edg
      integer :: nrtm_free

! mpi communicators
      integer :: mpi_comm
      integer :: rank
      integer :: nspmd
!
      integer :: nb_internal_edges        ! number of edges internal to the domain
      integer :: nb_boundary_edges_local  ! boundary edges treated by current domain
      integer :: nb_boundary_edges_remote ! boundary edges treated by the other domain


!type2
      my_real, dimension(:) , allocatable ::   csts_bis
!      orthotropic friction
      my_real, dimension(:) , allocatable ::   dir_fricm
!
      type(int8_struct_)             ::   t8
      type(metric_struct_)           ::   metric

      ! nodnorm
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SOLIDN_NORMAL
       my_real, DIMENSION(:,:), ALLOCATABLE :: NODNORM_NORMAL
       INTEGER, DIMENSION(:,:), ALLOCATABLE :: SOLIDN_NORMAL_F
       INTEGER, DIMENSION(:,:), ALLOCATABLE :: SOLIDN_NORMAL_FE

       ! rcurv arrays
       my_real, DIMENSION(:), ALLOCATABLE :: MODRCURV
       my_real, DIMENSION(:), ALLOCATABLE :: MODANGLM

       !Type25 / I25Norm
       REAL (KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: WNOD_NORMAL
       INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNOD
       INTEGER, DIMENSION(:), ALLOCATABLE :: TAGE
       INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAGSEG
       INTEGER:: NB_TAGSEG

       ! Implicit
       my_real, DIMENSION(6) :: BMINMA_IMP
!=======================================================================
   end type intbuf_struct_
!=======================================================================

! intbuf_size array maximum length defined as parameter
! (maximum number of different arrays composing intbuf_tab structure)
   integer, parameter :: l_intbuf_size_max = 516
   integer inter_ithknod !flag to fill thknod array  (enabled with ithick parameter from interface type 25 or 21)

   ! -------------------------
   ! index of intbuf_tab%variables
   ! -------------------------
   integer, parameter :: gap_index = 2 !< index for gap
   integer, parameter :: t_start_index = 3 ! index for start time
   integer, parameter :: distance_index = 5 !< index for dist
   integer, parameter :: bgapsmx_index = 7 !< index for bgapsmx
   integer, parameter :: tzinf_index = 8 !< index for tzinf
   integer, parameter :: maxbox_index = 9 !< index for maxbox
   integer, parameter :: t_stop_index = 11   ! index for stop time
   integer, parameter :: minbox_index = 12 !< index for minbox
   integer, parameter :: gapmin_index = 13 !< index for gapmin
   integer, parameter :: gapmax_index = 16 !< index for gapmax
   integer, parameter :: pmax_index = 23 !< index for pmax
   integer, parameter :: vmaxdt_index = 24 !< index for vmaxdt
   integer, parameter :: marge_index = 25 !< index for marge
   integer, parameter :: drad_index = 32 !< index for drad
   integer, parameter :: bgapemx_index = 40 !< index for bgapemx
   integer, parameter :: dgapload_index = 46 !< index for dgapload
!
!---------------
end module intbufdef_mod
