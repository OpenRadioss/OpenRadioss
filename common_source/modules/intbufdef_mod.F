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
Chd|  INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|-- called by -----------
Chd|        COPY_INTBUF_TAB               common_source/interf/copy_intbuf_tab.F
Chd|        INTBUF_INI                    common_source/interf/intbuf_ini.F
Chd|        INTBUF_TAB_C_INI              common_source/interf/copy_intbuf_tab.F
Chd|        UPGRADE_CAND_OPT              common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_E2S             common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_EDG             common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_MULTIMP               common_source/interf/upgrade_multimp.F
Chd|        ADDINT2M                      starter/source/interfaces/inter3d1/i7remnode.F
Chd|        ADDINT2S                      starter/source/interfaces/inter3d1/i7remnode.F
Chd|        ANOTHERN2                     starter/source/interfaces/inter3d1/i7remnode.F
Chd|        BUILD_CSRECT                  starter/source/model/mesh/build_cnel.F
Chd|        CHKTYP2                       starter/source/interfaces/interf1/chktyp2.F
Chd|        COPY_IVAL                     starter/source/restart/ddsplit/inter_tools.F
Chd|        COPY_IVAL_IGEO                starter/source/restart/ddsplit/inter_tools.F
Chd|        COPY_NODE_NODLOC              starter/source/restart/ddsplit/inter_tools.F
Chd|        COPY_RVAL                     starter/source/restart/ddsplit/inter_tools.F
Chd|        C_FRONT                       starter/source/restart/ddsplit/c_front.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        DOMDEC2                       starter/source/spmd/domdec2.F 
Chd|        FICTIVMASSIGEO                starter/source/groups/ssurftagigeo.F
Chd|        FILLCNI2                      starter/source/spmd/domdec2.F 
Chd|        FILL_INTERCEP                 starter/source/spmd/node/ddtools.F
Chd|        FILTER_NODE_NODLOC            starter/source/restart/ddsplit/inter_tools.F
Chd|        FLUSH_REMNODE_ARRAY           starter/source/interfaces/inter3d1/flush_remnode_array.F
Chd|        FXBTAGN                       starter/source/constraints/fxbody/fxbtagn.F
Chd|        HM_READ_INTER_TYPE21          starter/source/interfaces/int21/hm_read_inter_type21.F
Chd|        HM_READ_INTER_TYPE25          starter/source/interfaces/int25/hm_read_inter_type25.F
Chd|        I11EDGE                       starter/source/interfaces/inter3d1/i11edge.F
Chd|        I20INI3                       starter/source/interfaces/inter3d1/i20ini3.F
Chd|        I20STA                        starter/source/interfaces/inter3d1/inintr2.F
Chd|        I20STIFN                      starter/source/interfaces/inter3d1/i20stifn.F
Chd|        I24SETNODES                   starter/source/interfaces/inter3d1/i24setnodes.F
Chd|        I24STSECND                    starter/source/interfaces/inter3d1/i24stslav.F
Chd|        I25SORS                       starter/source/interfaces/inter3d1/i25sors.F
Chd|        I25STSECND                    starter/source/interfaces/inter3d1/i25stslav.F
Chd|        I2MAIN                        starter/source/interfaces/interf1/i2master.F
Chd|        I2TID3                        starter/source/interfaces/inter3d1/i2tid3.F
Chd|        I2_DTN                        starter/source/interfaces/inter3d1/i2_dtn.F
Chd|        I2_DTN_27                     starter/source/interfaces/inter3d1/i2_dtn_27.F
Chd|        I2_DTN_28                     starter/source/interfaces/inter3d1/i2_dtn_28.F
Chd|        I7REMNODE                     starter/source/interfaces/inter3d1/i7remnode.F
Chd|        I7STSECND                     starter/source/interfaces/inter3d1/i7stslav.F
Chd|        INIEND                        starter/source/interfaces/inter3d1/iniend.F
Chd|        INIEND2D                      starter/source/interfaces/inter3d1/iniend.F
Chd|        ININT2                        starter/source/interfaces/inter2d1/inint2.F
Chd|        ININT3                        starter/source/interfaces/inter3d1/inint3.F
Chd|        ININT3_THKVAR                 starter/source/interfaces/inter3d1/inint3_thkvar.F
Chd|        ININTMASS                     starter/source/interfaces/inter3d1/inintmass.F
Chd|        ININTR                        starter/source/interfaces/interf1/inintr.F
Chd|        ININTR1                       starter/source/interfaces/interf1/inintr1.F
Chd|        ININTR2                       starter/source/interfaces/inter3d1/inintr2.F
Chd|        ININTR_ORTHDIRFRIC            starter/source/interfaces/interf1/inintr_orthdirfric.F
Chd|        ININTR_THKVAR                 starter/source/interfaces/interf1/inintr_thkvar.F
Chd|        ININTSUB                      starter/source/interfaces/interf1/inintsub.F
Chd|        ININTSUB_11                   starter/source/output/subinterface/inintsub_11.F
Chd|        ININTSUB_25                   starter/source/output/subinterface/inintsub_25.F
Chd|        ININTSUB_7                    starter/source/output/subinterface/inintsub_7.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        INT2CY_CHK                    starter/source/constraints/general/bcs/lecbcscyc.F
Chd|        INT2MODIF_ND                  starter/source/elements/solid/solide10/dim_s10edg.F
Chd|        INT8_INI                      starter/source/interfaces/intbuf/intbuf_ini_starter.F
Chd|        INTBUF_INI_STARTER            starter/source/interfaces/intbuf/intbuf_ini_starter.F
Chd|        IPARI_L_INI                   starter/source/restart/ddsplit/ipari_l_ini.F
Chd|        ITAGSL12                      starter/source/interfaces/inter3d1/inintr2.F
Chd|        ITAGSL2                       starter/source/interfaces/inter3d1/itagsl2.F
Chd|        LAGM_INI                      starter/source/tools/lagmul/lagm_ini.F
Chd|        LECINS                        starter/source/interfaces/interf1/lecins.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        LGMINI_I2                     starter/source/tools/lagmul/lgmini_i2.F
Chd|        LGMINI_I7                     starter/source/tools/lagmul/lgmini_i7.F
Chd|        PREPARE_INT25                 starter/source/model/mesh/build_cnel.F
Chd|        PREPARE_SPLIT_CAND            starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_CAND_I20_EDGE   starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_CAND_I21        starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_CAND_I25_EDGE   starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I11             starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I17             starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I2              starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I20             starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I21             starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I24             starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I25             starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I25E2E          starter/source/spmd/prepare_split_i25e2e.F
Chd|        PREPARE_SPLIT_I7              starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I8              starter/source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I9              starter/source/restart/ddsplit/inter_tools.F
Chd|        PRESCRINT                     starter/source/interfaces/interf1/prescrint.F
Chd|        PRE_I2                        starter/source/interfaces/inter3d1/i7remnode.F
Chd|        R2R_CLEAN_INTER               starter/source/coupling/rad2rad/r2r_clean_inter.F
Chd|        REMN_I2OP                     starter/source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2OP_EDG25               starter/source/interfaces/int25/i25remlin.F
Chd|        REMN_I2_EDG                   starter/source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2_EDGOP                 starter/source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_SELF24                   starter/source/interfaces/inter3d1/remn_self24.F
Chd|        RESET_GAP                     starter/source/interfaces/interf1/reset_gap.F
Chd|        RI2_INT24P_INI                starter/source/interfaces/inter3d1/i7remnode.F
Chd|        SCRINT                        starter/source/interfaces/interf1/scrint.F
Chd|        SET_FRONT8                    starter/source/spmd/node/ddtools.F
Chd|        SET_INTERCEP                  starter/source/spmd/node/ddtools.F
Chd|        SMS_INI_JAD_1                 starter/source/ams/sms_init.F 
Chd|        SMS_INI_JAD_2                 starter/source/ams/sms_init.F 
Chd|        SMS_INI_JAD_3                 starter/source/ams/sms_init.F 
Chd|        SMS_INI_KDI                   starter/source/ams/sms_init.F 
Chd|        SPLIT_2RY_CAND_IVAL_I21       starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_ADSKYN_25               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I11                starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I20                starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I20_EDGE           starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I24                starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I25                starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I25_EDGE           starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I7                 starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_IVAL               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_IVAL_I21           starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_RVAL               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_INTERFACES              starter/source/restart/ddsplit/split_interfaces.F
Chd|        SPLIT_ISEGPT_IVAL             starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_LBOUND_I25              starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_LEDGE_I25               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NISUB_I25               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NISUB_I7                starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL2              starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL_I24           starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL_I25           starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_NODLOC             starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_NODLOC_P0          starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_RVAL               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I11             starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I24             starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I25             starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I25_E2S         starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I25_EDGE        starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I7              starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEGEDGE_NODLOC_I24      starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_IVAL2               starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_IVAL_I20            starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_IVAL_I20_2          starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_NODLOC              starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_NODLOC_I24          starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_RVAL_I20            starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_SEGLOC              starter/source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_XSAV                    starter/source/restart/ddsplit/inter_tools.F
Chd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_INTERFACES         starter/source/output/qaprint/st_qaprint_interfaces.F
Chd|        UPGRADE_REMNODE               starter/source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE2              starter/source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_E2S           starter/source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_EDG           starter/source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_EDG2          starter/source/interfaces/interf1/upgrade_remnode.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        W_FI                          starter/source/restart/ddsplit/w_fi.F
Chd|        W_FRONT                       starter/source/restart/ddsplit/w_front.F
Chd|        W_INTBUF_SIZE                 starter/source/restart/ddsplit/inter_tools.F
Chd|        W_PON                         starter/source/restart/ddsplit/w_pon.F
Chd|        W_TYPE8                       starter/source/restart/ddsplit/split_interfaces.F
Chd|        SECND_SURFACE_ON_DOMAIN       starter/source/interfaces/inter3d1/i24setnodes.F
Chd|        MERGEMOD                      starter/share/modules1/merge_mod.F
Chd|        ALEFVM_MAIN                   engine/source/ale/alefvm/alefvm_main.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        ALEWDX                        engine/source/ale/grid/alewdx.F
Chd|        CHECK_EDGE_STATE              engine/source/interfaces/interf/check_edge_state.F
Chd|        CHECK_NODAL_STATE             engine/source/interfaces/interf/check_nodal_state.F
Chd|        CHECK_REMOTE_SURFACE_STATE    engine/source/interfaces/interf/check_remote_surface_state.F
Chd|        CHECK_SURFACE_STATE           engine/source/interfaces/interf/check_surface_state.F
Chd|        CHKSTFN3N                     engine/source/interfaces/interf/chkstfn3.F
Chd|        CHKSTIFN                      engine/source/interfaces/inter2d/chkstifn.F
Chd|        CNDMASI2_DIM                  engine/source/elements/solid/solide10/s10cndf.F
Chd|        CNDMASI2_INI                  engine/source/elements/solid/solide10/s10cndf.F
Chd|        CP_IMPBUF                     engine/source/implicit/produt_v.F
Chd|        DEPLAFAKEIGE                  engine/source/assembly/deplafakeige.F
Chd|        DIAG_INT                      engine/source/mpi/implicit/imp_fri.F
Chd|        DIM_GLOB_K                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_INT7                      engine/source/implicit/ind_glob_k.F
Chd|        DIM_INT_K                     engine/source/implicit/ind_glob_k.F
Chd|        DIM_KINEFR                    engine/source/mpi/implicit/imp_fri.F
Chd|        DIM_KINE_I                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_KINE_P                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_KINE_S                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_KINE_T                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_KINFRK                    engine/source/mpi/implicit/imp_fri.F
Chd|        DIM_KINKN                     engine/source/implicit/imp_int_k.F
Chd|        DIM_KINMAX                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_KINMV                     engine/source/airbag/monv_imp0.F
Chd|        DIM_NDOF_I                    engine/source/implicit/ind_glob_k.F
Chd|        DIM_NDOF_II                   engine/source/implicit/ind_glob_k.F
Chd|        EIG                           engine/stub/eig.F             
Chd|        EIG1                          engine/stub/eig1.F            
Chd|        EIGCOND                       engine/stub/eigcond.F         
Chd|        EIGP                          engine/stub/eigp.F            
Chd|        FIND_EDGE_FROM_REMOTE_PROC    engine/source/interfaces/interf/find_edge_from_remote_proc.F
Chd|        FIND_EDGE_INTER               engine/source/interfaces/interf/find_edge_inter.F
Chd|        FIND_SURFACE_FROM_REMOTE_PROC engine/source/interfaces/interf/find_surface_from_remote_proc.F
Chd|        FIND_SURFACE_INTER            engine/source/interfaces/interf/find_surface_inter.F
Chd|        FR_A2BD                       engine/source/mpi/implicit/imp_fri.F
Chd|        FR_MATV                       engine/source/mpi/implicit/imp_fri.F
Chd|        FR_U2DD                       engine/source/mpi/implicit/imp_fri.F
Chd|        GETNDDLI_G                    engine/source/mpi/implicit/imp_fri.F
Chd|        I10FKU3                       engine/source/interfaces/int10/i10ke3.F
Chd|        I10FORCF3                     engine/source/interfaces/int10/i10ke3.F
Chd|        I10KE3                        engine/source/interfaces/int10/i10ke3.F
Chd|        I10MAINF                      engine/source/interfaces/int10/i10mainf.F
Chd|        I10MAIN_OPT_TRI               engine/source/interfaces/intsort/i10opt_opt_tri.F
Chd|        I10MAIN_TRI                   engine/source/interfaces/intsort/i10main_tri.F
Chd|        I11FKU3                       engine/source/interfaces/int11/i11ke3.F
Chd|        I11FORCF3                     engine/source/interfaces/int11/i11ke3.F
Chd|        I11KE3                        engine/source/interfaces/int11/i11ke3.F
Chd|        I11MAINF                      engine/source/interfaces/int11/i11mainf.F
Chd|        I11MAIN_CRIT_TRI              engine/source/interfaces/intsort/i11main_crit_tri.F
Chd|        I11MAIN_OPT_TRI               engine/source/interfaces/intsort/i11main_opt_tri.F
Chd|        I11MAIN_TRI                   engine/source/interfaces/intsort/i11main_tri.F
Chd|        I14CMP                        engine/source/interfaces/int14/i14cmp.F
Chd|        I14IST                        engine/source/interfaces/int14/i14ist.F
Chd|        I14WFS                        engine/source/interfaces/int14/i14wfs.F
Chd|        I15CMP                        engine/source/interfaces/int15/i15cmp.F
Chd|        I16MAIN                       engine/source/interfaces/int16/i16main.F
Chd|        I17MAIN                       engine/source/interfaces/int17/i17main.F
Chd|        I17MAIN_CRIT_TRI              engine/source/interfaces/int17/i17main_pena.F
Chd|        I17MAIN_PENA                  engine/source/interfaces/int17/i17main_pena.F
Chd|        I17MAIN_TRI                   engine/source/interfaces/int17/i17main_pena.F
Chd|        I18MAIN_KINE_1                engine/source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_2                engine/source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_F                engine/source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_I                engine/source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_S                engine/source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_V                engine/source/interfaces/int18/i18main_kine.F
Chd|        I20MAINF                      engine/source/interfaces/int20/i20mainf.F
Chd|        I20MAIN_CRIT_TRI              engine/source/interfaces/intsort/i20main_crit_tri.F
Chd|        I20MAIN_OPT_TRI               engine/source/interfaces/intsort/i20main_opt_tri.F
Chd|        I20MAIN_TRI                   engine/source/interfaces/intsort/i20main_tri.F
Chd|        I21MAINF                      engine/source/interfaces/int21/i21mainf.F
Chd|        I21MAIN_CRIT_TRI              engine/source/interfaces/intsort/i21main_crit_tri.F
Chd|        I21MAIN_GAP                   engine/source/interfaces/int21/i21main_gap.F
Chd|        I21MAIN_OPT_TRI               engine/source/interfaces/intsort/i21main_opt_tri.F
Chd|        I21MAIN_TRI                   engine/source/interfaces/intsort/i21main_tri.F
Chd|        I21RESET                      engine/source/interfaces/int21/i21reset.F
Chd|        I21_ICRIT                     engine/source/interfaces/intsort/i21_icrit.F
Chd|        I22MAINF                      engine/source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
Chd|        I23MAINF                      engine/source/interfaces/int23/i23mainf.F
Chd|        I23MAIN_OPT_TRI               engine/source/interfaces/intsort/i23main_opt_tri.F
Chd|        I23MAIN_TRI                   engine/source/interfaces/intsort/i23main_tri.F
Chd|        I24E2E_FICTIVE_NODES_UPDATE   engine/source/interfaces/int24/i24for3e.F
Chd|        I24KE3                        engine/source/interfaces/int24/i24ke3.F
Chd|        I24MAINF                      engine/source/interfaces/int24/i24main.F
Chd|        I24MAIN_CRIT_TRI              engine/source/interfaces/intsort/i24main_crit_tri.F
Chd|        I24MAIN_OPT_TRI               engine/source/interfaces/intsort/i24main_opt_tri.F
Chd|        I24MAIN_TRI                   engine/source/interfaces/intsort/i24main_tri.F
Chd|        I24NITSCHFOR3                 engine/source/interfaces/int24/i24nitschfor3.F
Chd|        I24PXFEM                      engine/source/interfaces/int24/i24pxfem.F
Chd|        I24XVFIC_UPD                  engine/source/interfaces/int24/i24for3e.F
Chd|        I25COMP_1                     engine/source/interfaces/int25/i25comp_1.F
Chd|        I25COMP_2                     engine/source/interfaces/int25/i25comp_2.F
Chd|        I25IRTLM                      engine/source/interfaces/int25/i25irtlm.F
Chd|        I25MAIND_2                    engine/source/interfaces/int25/i25maind_2.F
Chd|        I25MAINF                      engine/source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_CRIT_TRI              engine/source/interfaces/intsort/i25main_crit_tri.F
Chd|        I25MAIN_FREE                  engine/source/interfaces/intsort/i25main_free.F
Chd|        I25MAIN_GAP                   engine/source/interfaces/int25/i25main_gap.F
Chd|        I25MAIN_NORM                  engine/source/interfaces/int25/i25main_norm.F
Chd|        I25MAIN_OPT_TRI               engine/source/interfaces/intsort/i25main_opt_tri.F
Chd|        I25MAIN_SLID                  engine/source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   engine/source/interfaces/intsort/i25main_tri.F
Chd|        I25PREP_NINDEX                engine/source/interfaces/int25/i25slid.F
Chd|        I25PREP_SEND                  engine/source/interfaces/int25/i25slid.F
Chd|        I25PREP_SIZBUFS               engine/source/interfaces/int25/i25slid.F
Chd|        I25TAGN                       engine/source/interfaces/int25/i25norm.F
Chd|        I2VIT27                       engine/source/interfaces/interf/i2vit27.F
Chd|        I2VIT28                       engine/source/interfaces/interf/i2vit28.F
Chd|        I2_IMP0                       engine/source/interfaces/interf/i2_imp0.F
Chd|        I2_IMP1                       engine/source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPD                       engine/source/interfaces/interf/i2_impd.F
Chd|        I2_IMPI                       engine/source/interfaces/interf/i2_imp0.F
Chd|        I2_IMPM                       engine/source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPR1                      engine/source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPR2                      engine/source/interfaces/interf/i2_imp1.F
Chd|        I5KE3                         engine/source/interfaces/inter3d/i5ke3.F
Chd|        I6MAIN                        engine/source/interfaces/inter3d/i6main.F
Chd|        I7FKU3                        engine/source/interfaces/int07/i7ke3.F
Chd|        I7FORCF3                      engine/source/interfaces/int07/i7ke3.F
Chd|        I7KE3                         engine/source/interfaces/int07/i7ke3.F
Chd|        I7MAINF                       engine/source/interfaces/int07/i7mainf.F
Chd|        I7MAINFR                      engine/source/interfaces/int07/i7ke3.F
Chd|        I7MAIN_CRIT_TRI               engine/source/interfaces/intsort/i7main_crit_tri.F
Chd|        I7MAIN_LMULT                  engine/source/interfaces/int07/i7main_lmult.F
Chd|        I7MAIN_OPT_TRI                engine/source/interfaces/intsort/i7main_opt_tri.F
Chd|        I7MAIN_TRI                    engine/source/interfaces/intsort/i7main_tri.F
Chd|        I9MAIN2                       engine/source/interfaces/int09/i9main2.F
Chd|        I9MAIN3                       engine/source/interfaces/int09/i9main3.F
Chd|        I9WALE                        engine/source/interfaces/int09/i9wale.F
Chd|        IDDL_INT                      engine/source/mpi/implicit/imp_fri.F
Chd|        IDDL_MINT                     engine/source/implicit/imp_int_k.F
Chd|        IDEL_INT                      engine/source/implicit/ind_glob_k.F
Chd|        ID_MVINI                      engine/source/airbag/monv_imp0.F
Chd|        IELOF2                        engine/source/interfaces/interf/ielof2.F
Chd|        IMP3_A2B                      engine/source/airbag/monv_imp0.F
Chd|        IMP3_U2X                      engine/source/airbag/monv_imp0.F
Chd|        IMP_BUCK                      engine/source/implicit/imp_buck.F
Chd|        IMP_CHKM                      engine/source/implicit/imp_solv.F
Chd|        IMP_COMPAB                    engine/source/implicit/imp_solv.F
Chd|        IMP_COMPABP                   engine/source/implicit/imp_solv.F
Chd|        IMP_DIAGS                     engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_DIAGSN                    engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_DTKIN                     engine/source/implicit/imp_int_k.F
Chd|        IMP_DYKV                      engine/source/implicit/imp_dyna.F
Chd|        IMP_DYKV0                     engine/source/implicit/imp_dyna.F
Chd|        IMP_FR7I                      engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_FRFV                      engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_FRI                       engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_FRKD                      engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_FRKI                      engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_FRSN                      engine/source/mpi/implicit/imp_fri.F
Chd|        IMP_I10MAINF                  engine/source/interfaces/int10/i10ke3.F
Chd|        IMP_I11MAINF                  engine/source/interfaces/int11/i11ke3.F
Chd|        IMP_I7MAINF                   engine/source/interfaces/int07/i7ke3.F
Chd|        IMP_ICOMCRIT                  engine/source/implicit/imp_int_k.F
Chd|        IMP_INISI                     engine/source/implicit/imp_pcg.F
Chd|        IMP_INIST                     engine/source/implicit/imp_pcg.F
Chd|        IMP_INTDT                     engine/source/implicit/imp_int_k.F
Chd|        IMP_INTFR                     engine/source/implicit/imp_solv.F
Chd|        IMP_INTTD0                    engine/source/implicit/imp_int_k.F
Chd|        IMP_INT_K                     engine/source/implicit/imp_int_k.F
Chd|        IMP_K_EIG                     engine/stub/imp_k_eig.F       
Chd|        IMP_LANZP                     engine/source/implicit/imp_lanz.F
Chd|        IMP_PCGH                      engine/source/implicit/imp_pcg.F
Chd|        IMP_PPCGH                     engine/source/implicit/imp_pcg.F
Chd|        IMP_SOLV                      engine/source/implicit/imp_solv.F
Chd|        IMP_SOL_INIT                  engine/source/implicit/imp_sol_init.F
Chd|        IMP_TRIPI                     engine/source/implicit/imp_int_k.F
Chd|        IMP_UPDST                     engine/source/implicit/imp_pcg.F
Chd|        IMP_UPDV2                     engine/source/implicit/imp_pcg.F
Chd|        IND_FRKD                      engine/source/mpi/implicit/imp_fri.F
Chd|        IND_GLOB_K                    engine/source/implicit/ind_glob_k.F
Chd|        IND_INT_K                     engine/source/implicit/ind_glob_k.F
Chd|        IND_KINEFR                    engine/source/mpi/implicit/imp_fri.F
Chd|        IND_KINE_I                    engine/source/implicit/ind_glob_k.F
Chd|        IND_KINE_K                    engine/source/implicit/ind_glob_k.F
Chd|        IND_KINFRK                    engine/source/mpi/implicit/imp_fri.F
Chd|        INIT_INTERF_SORTING_STRATEGY  engine/source/interfaces/init_interf_sorting_strategy.F
Chd|        INIT_NODAL_STATE              engine/source/interfaces/interf/init_nodal_state.F
Chd|        INI_DD0                       engine/source/mpi/implicit/imp_fri.F
Chd|        INI_DDFV                      engine/source/mpi/implicit/imp_fri.F
Chd|        INI_DOFSPC                    engine/source/implicit/upd_glob_k.F
Chd|        INI_KINKN                     engine/source/implicit/imp_int_k.F
Chd|        INI_KINMV                     engine/source/airbag/monv_imp0.F
Chd|        INT12W                        engine/source/ale/inter/int12w.F
Chd|        INT18_LAW151_NSV_SHIFT        engine/source/interfaces/int18/int18_law151_nsv_shift.F
Chd|        INT2POFF                      engine/source/interfaces/interf/int2poff.F
Chd|        INT2POFFH                     engine/source/interfaces/interf/int2poff.F
Chd|        INT2RUPT                      engine/source/interfaces/interf/int2rupt.F
Chd|        INT2_IMP2                     engine/source/interfaces/interf/i2_imp2.F
Chd|        INTAL1                        engine/source/ale/inter/intal1.F
Chd|        INTAL2                        engine/source/ale/inter/intal2.F
Chd|        INTAL3                        engine/source/ale/inter/intal3.F
Chd|        INTAL4                        engine/source/ale/inter/intal4.F
Chd|        INTCRIT                       engine/source/interfaces/intsort/intcrit.F
Chd|        INTER_CHECK_SORT              engine/source/interfaces/generic/inter_check_sort.F
Chd|        INTER_COLOR_COARSE_VOXEL      engine/source/interfaces/generic/inter_color_coarse_voxel.F
Chd|        INTER_COLOR_VOXEL             engine/source/interfaces/generic/inter_color_voxel.F
Chd|        INTER_COUNT_NODE_CURV         engine/source/interfaces/generic/inter_count_node_curv.F
Chd|        INTER_DEALLOCATE_WAIT         engine/source/interfaces/generic/inter_deallocate_wait.F
Chd|        INTER_MINMAX_NODE             engine/source/interfaces/generic/inter_minmax_node.F
Chd|        INTER_PREPARE_SORT            engine/source/interfaces/generic/inter_prepare_sort.F
Chd|        INTER_SORT                    engine/source/interfaces/generic/inter_sort.F
Chd|        INTER_SORT_07                 engine/source/interfaces/int07/inter_sort_07.F
Chd|        INTER_STRUCT_INIT             engine/source/interfaces/generic/inter_struct_init.F
Chd|        INTER_TRC_7                   engine/source/interfaces/int07/inter_trc_7.F
Chd|        INTER_VOXEL_CREATION          engine/source/interfaces/generic/inter_voxel_creation.F
Chd|        INTFOP1                       engine/source/interfaces/interf/intfop1.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        INTFOP8                       engine/source/interfaces/interf/intfop8.F
Chd|        INTMASS_UPDATE                engine/source/interfaces/interf/intmass_update.F
Chd|        INTTI0                        engine/source/interfaces/interf/intti0.F
Chd|        INTTI1                        engine/source/interfaces/interf/intti1.F
Chd|        INTTI12A                      engine/source/interfaces/interf/intti12.F
Chd|        INTTI12F                      engine/source/interfaces/interf/intti12.F
Chd|        INTTI12V                      engine/source/interfaces/interf/intti12.F
Chd|        INTTI2                        engine/source/interfaces/interf/intti2.F
Chd|        INTTI2F                       engine/source/interfaces/interf/intti2f.F
Chd|        INTTI2V                       engine/source/interfaces/interf/intti2v.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        INTVO2                        engine/source/interfaces/inter2d/intvo2.F
Chd|        INTVO3                        engine/source/interfaces/inter3d/intvo3.F
Chd|        INTVO8                        engine/source/interfaces/inter3d/intvo8.F
Chd|        INT_FKU3                      engine/source/implicit/imp_int_k.F
Chd|        INT_MATV                      engine/source/implicit/imp_int_k.F
Chd|        INT_MATVP                     engine/source/implicit/imp_int_k.F
Chd|        KIN_KML                       engine/source/mpi/implicit/imp_fri.F
Chd|        KIN_KNL                       engine/source/implicit/imp_int_k.F
Chd|        KIN_KSL                       engine/source/mpi/implicit/imp_fri.F
Chd|        LAG_I2MAIN                    engine/source/tools/lagmul/lag_i2main.F
Chd|        LAG_MULT                      engine/source/tools/lagmul/lag_mult.F
Chd|        LAG_MULTP                     engine/source/tools/lagmul/lag_mult.F
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        LIN_SOLV                      engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVH0                    engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVH1                    engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVHM                    engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVIH2                   engine/source/implicit/lin_solv.F
Chd|        LTAG_I2MAIN                   engine/source/tools/lagmul/lag_ntag.F
Chd|        MAV_LT2                       engine/source/implicit/produt_v.F
Chd|        MAV_LTH                       engine/source/implicit/produt_v.F
Chd|        MAV_LTH0                      engine/source/implicit/produt_v.F
Chd|        MAV_LTP                       engine/source/implicit/produt_v.F
Chd|        MMAV_LTH                      engine/source/implicit/produt_v.F
Chd|        MONV_DIAG                     engine/source/airbag/monv_imp0.F
Chd|        MONV_IMP                      engine/source/airbag/monv_imp0.F
Chd|        MONV_M3                       engine/source/airbag/monv_imp0.F
Chd|        MONV_PREM                     engine/source/airbag/monv_imp0.F
Chd|        MPP_INIT                      engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        MV_MATV                       engine/source/airbag/monv_imp0.F
Chd|        NDDLI_NS                      engine/source/mpi/implicit/imp_fri.F
Chd|        NL_SOLV                       engine/source/implicit/nl_solv.F
Chd|        PRINTIME_INTERF               engine/source/system/timer_interf.F
Chd|        PRINT_STIF                    engine/source/implicit/imp_solv.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RECUKIN                       engine/source/implicit/recudis.F
Chd|        RER02                         engine/source/implicit/upd_glob_k.F
Chd|        RER_INT_V                     engine/source/implicit/upd_glob_k.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        S10CNDI2_INI                  engine/source/elements/solid/solide10/s10cndf.F
Chd|        SMS_BUILD_MAT_2               engine/source/ams/sms_build_mat_2.F
Chd|        SMS_INI_INT                   engine/source/ams/sms_init.F  
Chd|        SMS_INI_JAD_1                 engine/source/ams/sms_init.F  
Chd|        SMS_INI_JAD_2                 engine/source/ams/sms_init.F  
Chd|        SMS_INI_JAD_3                 engine/source/ams/sms_init.F  
Chd|        SMS_INI_KDI                   engine/source/ams/sms_init.F  
Chd|        SMS_INI_KIN_1                 engine/source/ams/sms_init.F  
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SPMD_CELL_LIST_EXCHANGE       engine/source/mpi/interfaces/spmd_cell_list_exchange.F
Chd|        SPMD_CELL_SIZE_EXCHANGE       engine/source/mpi/interfaces/spmd_cell_size_exchange.F
Chd|        SPMD_CHECK_TAG                engine/source/mpi/ams/spmd_check_tag.F
Chd|        SPMD_EXCH_DA20                engine/source/mpi/interfaces/spmd_exch_da20.F
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|        SPMD_EXCH_EFRIC               engine/source/mpi/interfaces/spmd_exch_efric.F
Chd|        SPMD_EXCH_I24                 engine/source/mpi/interfaces/spmd_exch_i24.F
Chd|        SPMD_EXCH_I25                 engine/source/mpi/interfaces/spmd_exch_i25.F
Chd|        SPMD_EXCH_IDEL_SEGLO          engine/source/mpi/interfaces/spmd_exch_idel_seglo.F
Chd|        SPMD_EXCH_NOR                 engine/source/mpi/interfaces/spmd_exch_nor.F
Chd|        SPMD_EXCH_PRESS               engine/source/mpi/interfaces/spmd_exch_press.F
Chd|        SPMD_EXCH_SMST2               engine/source/mpi/ams/spmd_exch_smst2.F
Chd|        SPMD_EXCH_SORTING_EFRIC       engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
Chd|        SPMD_GLOB_MIN5                engine/source/mpi/generic/spmd_glob_min5.F
Chd|        SPMD_I18KINE_COM_A            engine/source/mpi/interfaces/spmd_i18kine_com_a.F
Chd|        SPMD_I18KINE_COM_ACC          engine/source/mpi/interfaces/spmd_i18kine_com_acc.F
Chd|        SPMD_I18KINE_COM_MS           engine/source/mpi/interfaces/spmd_i18kine_com_ms.F
Chd|        SPMD_I18KINE_COM_V            engine/source/mpi/interfaces/spmd_i18kine_com_v.F
Chd|        SPMD_I18KINE_PENE_COM_POFF    engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|        SPMD_I21FTHECOM               engine/source/mpi/interfaces/send_cand.F
Chd|        SPMD_I21TEMPCOM               engine/source/mpi/interfaces/send_cand.F
Chd|        SPMD_I24_PREPARE              engine/source/interfaces/int24/i24_prepare.F
Chd|        SPMD_I25FRONT_INIT            engine/source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I25FRONT_NOR             engine/source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I25_PREPARE              engine/source/interfaces/int25/i25_prepare.F
Chd|        SPMD_I25_SLIDE_GAT            engine/source/mpi/interfaces/spmd_i25slide.F
Chd|        SPMD_I7FCOM_POFF              engine/source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               engine/source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7ITIED_CAND             engine/source/mpi/interfaces/spmd_i7itied_cand.F
Chd|        SPMD_I7XVCOM2                 engine/source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPMD_IFRONT                   engine/source/mpi/interfaces/spmd_ifront.F
Chd|        SPMD_IFRONT_STAMP             engine/source/mpi/interfaces/send_cand.F
Chd|        SPMD_INT18_LAW151_PON         engine/source/mpi/forces/spmd_int18_law151_pon.F
Chd|        SPMD_SAVEFI                   engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_SPLIT_COMM_INTER         engine/source/mpi/interfaces/spmd_split_comm_inter.F
Chd|        SPMD_WAIT_NB                  engine/source/mpi/interfaces/spmd_wait_nb.F
Chd|        UD_INT5                       engine/source/implicit/upd_glob_k.F
Chd|        UPDK_MV                       engine/source/airbag/monv_imp0.F
Chd|        UPD_FR                        engine/source/mpi/implicit/imp_fri.F
Chd|        UPD_GLOB_K                    engine/source/implicit/upd_glob_k.F
Chd|        UPD_INT_K                     engine/source/implicit/upd_glob_k.F
Chd|        UPD_KML                       engine/source/mpi/implicit/imp_fri.F
Chd|        UPD_KSL                       engine/source/mpi/implicit/imp_fri.F
Chd|        UPD_RHS                       engine/source/implicit/upd_glob_k.F
Chd|        UPD_RHS_FR                    engine/source/implicit/imp_solv.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRITE_INTBUF                  engine/source/output/restart/write_intbuf.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|        IMP_INTBUF                    engine/share/modules/imp_mod_def.F
Chd|        INTBUF_MOD                    engine/share/modules/restart_mod.F
Chd|        UPDATE_WEIGHT_INTER_TYPE_24_25starter/source/spmd/domain_decomposition/update_weight_inter_type_24_25.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      modules/interfaces/int8_mod.F 
Chd|        METRIC_MOD                    modules/metric_mod.F          
Chd|====================================================================
      MODULE INTBUFDEF_MOD
C----------------------------------------------------------------------- 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
      USE METRIC_MOD


#include "my_real.inc"      
c
C=======================================================================
      TYPE INTBUF_STRUCT_
C=================================================  
c define typeINTBUF_STRUCT_ for new interface buffer INTBUF_TAB
c module is organized as following :
c   - define integer arrays sizes
c   - define float arrays sizes
c   - define integer arrays
c   - define float arrays

C=======================================================================
c DEFINE SIZES (integers arrays)
C=======================================================================
        integer ::   S_IRECTS     !  4*NRTS     :IRECTS:CONNECTIVITES FACES SECONDS     :1,2,3,4
        integer ::   S_IRECTM     !  4*NRTM_FE+4*9*NRTM_IGE :IRECTM:CONNECTIVITES FACES MAIN  :1,2,3,4,5,6,7,8,20,24
        integer ::   S_NSV        !  NSN        :NSV   :NOEUDS SECONDS                  :1,2,3,4,  7 
        integer ::   S_MSR        !  NMN+16*NRTM_IGE  :MSR   :NOEUDS MAINS                   :1,2,3,4
        integer ::   S_IRTLM      !  NSN        :FACE MAIN LA PLUS PROCHE    :1,2,3,4   
        integer ::   S_IRUPT      !  NSN        :IRUPT :Flag rupture                     :2
        integer ::   S_INORM      !  NSN        :INORM  :main orientation flag: type2 avec rupture
        integer ::   S_IELEC      !  (type20)
        integer ::   S_IELES      !  NRTS       :IELES :ELEMENT SECND                    :              9
        integer ::   S_LISUB      !  NISUB      :LISUB :LISTE DES SOUS-INTERFACES        :     7
        integer ::   S_TYPSUB     !  NISUB      :TYPSUB :TYPE OF SUBINTERFACES           :     25
        integer ::   S_ADDSUBS    !  NSN+1      :ADDSUBS:ADRESSE DS ZONE DE SS INTERF. COTE SECOND. :     7, 10, 24, 25  
        integer ::   S_ADDSUBM    !  NRTM+1     :ADDSUBM:ADRESSE DS ZONE DE SS INTERF. COTE MAIN  :     7, 10, 24, 25
        integer ::   S_LISUBS     !  NISUBS     :LISUBS :ZONE DES SS INTERF. COTE SECOND.           :     7, 10, 24, 25
        integer ::   S_LISUBM     !  NISUBM     :LISUBM :ZONE DES SS INTERF. COTE MAIN            :     7, 10, 24, 25
        integer ::   S_INFLG_SUBS !  NISUBS     :INFLG_SUBS: ZONE D APPARTENANCE A S1/S2 DES SS INTERF. COTE SECOND. : 25
        integer ::   S_INFLG_SUBM !  NISUBS     :INFLG_SUBS: ZONE D APPARTENANCE A S1/S2 DES SS INTERF.COTE MAIN   : 25
        integer ::   S_MSEGTYP    !  NRTM       :MSEGTYP: element type                   :      2,3,5,7,10,21,23; used only in Starter
        integer ::   S_CAND_E     !  MULTIMP*NSN:CAND_E:FACETTES CANDIDATES              :7,10,11,20   
        integer ::   S_CAND_N     !  MULTIMP*NSN:CAND_N:NOEUDS CANDIDATS                 :7,10,11,20  
        integer ::   S_I_STOK     !             : II_STOK                                : 7    10,11,20,24
        integer ::   S_I_STOK_E   !             : II_STOK_E                              : 7    10,11,20,24
        integer ::   S_IFPEN      !  MULTIMP*NSN:IFPEN :FLAG PENETR. (filtr frottement)  :           7
        integer ::   S_KREMNODE   !  NRTM+1     :KREMNODE:ADRESS NODES REMOVED FROM CONTACT IN REMNODE ARRAY :      7
        integer ::   S_REMNODE    !  NREMNODE   :REMNODE: NODES REMOVED FROM CONTACT     :      7
        integer ::   S_ADCCM      !  NRTM       :ADDCM :adresse dans CHAINE du 1er main:                  11
        integer ::   S_CHAIN      !  2*MULTIMP*NSN:CHAINE(1,adds) : bord second.     :                  11
!type20
        integer ::   S_DAANC6     !  18*2*NLN   :DAANC6:contient 18*NLN double precision : 20       
        integer ::   S_NBINFLG    !  NBINFLG    :NLN    :Flags binaire noeuds            NBIN:      20, 24, 25
        integer ::   S_MBINFLG    !  MBINFLG    :NRTM  :Flag multiusage facettes        :      20, 24, 25
        integer ::   S_EBINFLG    !  EBINFLG    :NEDGE :Flag multiusage edges           :      25
        integer ::   S_NLG        !  NLN        :NLG   :LOCAL TO GLOBAL NODE             :      20
        integer ::   S_ISLINS     !  2*NLINS    :ISLINS:SURFACE SECOND. et COTE          :      20
        integer ::   S_ISLINM     !  2*NLINM    :ISLINM:SURFACE MAIN et COTE           :      20
        integer ::   S_IXLINS     !  2*NLINS    :IXLINS:CONNECTIVITES BORDS SECONDS     :      20
        integer ::   S_IXLINM     !  2*NLINM    :IXLINM:CONNECTIVITES BORDS MAINS      :      20
        integer ::   S_NSVL       !  NSNE       :NSVL  :NOEUDS SECONDS DES LIGNES       :      20
        integer ::   S_MSRL       !  NMNE       :MSRL  :NOEUDS MAINS                   :      20
        integer ::   S_LCAND_N    !  MULTIMP*NMNE:LCAND_N:LIGNES MAINS CANDIDATES      :      20
        integer ::   S_LCAND_S    !  MULTIMP*NSNE:LCAND_N:LIGNES SECONDS CANDIDATES     :      20
        integer ::   S_ADCCM20    !  NLINM      :ADDCM :adresse dans CHAINE du 1er main:      20
        integer ::   S_CHAIN20    !  2*MULTIMP*NSNE:CHAINE(1,adds) : bord second.        :      20            11                                  !                       :CHAINE(2,adds) : adresse suivante       :      20  
!type1
        integer ::   S_ILOCS      !  NSN        :ILOCS :NOEUD MAIN LE PLUS PROCHE      :1, ,3,4
        integer ::   S_NSEGM      !  1+NMN      :NSEGM :ADRESSE DU VECTEUR LMSR (MAIN) :1, ,3,4,5
        integer ::   S_NRT        !  NRT        :LMSR  :FACES CONNECTEES AU NOEUD MAIN :1, ,3,4,5
!type2
        integer ::   S_MSEGTYP2   !  :NRTM      :MSEGTYP: main segment type sol/shell  :2
        integer ::   S_CSTS_BIS   !  :NRTM      :MSEGTYP: main segment type sol/shell  :2
!type3
        integer ::   S_IRTLS      !  NMN        :IRTLS :FACE SECOND. LA PLUS PROCHE      :    3,4
        integer ::   S_ILOCM      !  NMN        :ILOCM :NOEUD SECOND. LE PLUS PROCHE     :    3,4
        integer ::   S_IRTLOM     !  NSN        :IRTLOM:ANC. FACE MAIN. LA PLUS PROCHE  :    3,4
        integer ::   S_IRTLOS     !  NMN        :IRTLOS:ANC. FACE SECND. LA PLUS PROCHE  :    3,4
        integer ::   S_NSEGS      !  1+NSN      :NSEGS :ADRESSE DU VECTEUR LNSV (SECOND):    3,4,5
        integer ::   S_LNSV       !  NRT        :LMSR  :FACES CONNECTEES AU NOEUD MAIN :1, ,3,4,5
        integer ::   S_LMSR       !  NRT        :LMSR  :FACES CONNECTEES AU NOEUD MAIN :1, ,3,4,5
!type4
        integer ::   S_IELEM      !  NRTM       :IELEM :ELEMENT main                   :              9
!type12
        integer ::   S_FCOUNT     !  NSN: 12
!type14
        integer ::   S_KSURF      !  
        integer ::   S_IMPACT     !  NSN 
!type21
        integer ::   S_MSR21      !  NMNG    :MSR21: MSR global     :      21
        integer ::   S_MNDD       
        integer ::   S_MSR_L      
!type24
        integer ::   S_MVOISIN    !  4*NRTM     :MVOISIN: facettes mains voisines:(at least two commun nodes) : 24
        integer ::   S_NVOISIN    !  2*4*NRTM   :NVOISIN: Noeuds mains voisines      :24
        integer ::   S_MSEGLO     !  NRTM       :MSEGLO: global segment number (spmd)    :24, 25
        integer ::   S_MSEGTYP24  !  NRTM       :MSEGTYP: shell segment type             :24
!type25
        integer ::   S_EVOISIN    !  4*NRTM            :EVOISIN: No edge voisine
        integer ::   S_ADMSR      !  4*NRTM            :ADMSR  : adresse des normales aux noeuds mains : 25
        integer ::   S_LEDGE      !  4*NEDGE           :LEDGE  : description des edges : 25
        integer ::   S_LBOUND     !  NADMSR            :LBOUND : Index des sommets sur les aretes libres : 25
        integer ::   S_ACTNOR     !  NADMSR            :ACTNOR : tag des normales actives (0/1) : 25
        integer ::   S_FARM       !  4*MULTIMP*NSN     :FARM   :Flag In/out wrt 4 sub-triangles : 25
        integer ::   S_ADSKYN     !  4*NRTM+1          :ADSKYN : skyline pour assemblage parith/on des normales : 25
        integer ::   S_IADNOR     !  4*NRTM            :IADNOR : skyline pour assemblage parith/on des normales : 25
        integer ::   S_ISLIDE     !  4*NSN             :ISLIDE : sommets (0  4) sur lesquels le noeud secnd glisse : 25
        integer ::   S_KNOR2MSR   !  NADMSR+1          :KNOR2MSR: adress of connected segments to normals in NOR2MSR : 25
        integer ::   S_NOR2MSR    !  ...               :NOR2MSR : connected segments to normals in NOR2MSR : 25
        integer ::   S_CAND_OPT_N !  MULTIMP*NSN       :CAND_OPT_N : Candidats apres optimisation <=> nd second. (i25optcd.F)
        integer ::   S_CAND_OPT_E !  MULTIMP*NSN       :CAND_OPT_E : Candidats apres optimisation <=> segment main (i25optcd.F)
        integer ::   S_IF_ADH     !  NSN               :IF_ADH: if adhesion spring exists (1) or not (0)  :  25
        integer ::   S_CANDM_E2E  !  MULTIMP*NCONTE    :CANDM_E2E : Main line : Aretes candidates mains (shell & beams))
        integer ::   S_CANDS_E2E  !  MULTIMP*NCONTE    :CANDS_E2E : Secnd line : Aretes candidates secnds 
        integer ::   S_CANDM_E2S  !  MULTIMP*NCONTE    :CANDM_E2S : Main segment : Facettes candidates mains (solid edges)
        integer ::   S_CANDS_E2S  !  MULTIMP*NCONTE    :CANDS_E2S : Secnd line : Aretes candidates secnds 
        integer ::   S_CANDL_MAX 
        integer ::   S_CANDS_MAX 
        integer ::   S_ADDSUBE    !  NEDGE+1     :ADDSUBES:ADRESSE DS ZONE EDGES DE SS INTERF. COTE SECOND.             : 25   
        integer ::   S_LISUBE     !  NISUBE      :LISUBE  :ZONE EDGES DES SS INTERF. COTE SECOND.                       : 25
        integer ::   S_INFLG_SUBE !  NISUBE      :INFLG_SUBE :: ZONE D APPARTENANCE A S1/S2 DES SS INTERF.COTE MAIN   : 25
C-------S_ISEADD,S_ISEDGE,S_CAND_T will be cleaned after, for the moment with size 0
        integer ::   S_ISEADD     !  ISEADD     :SECND EDGE ADDRESS                      :      24
        integer ::   S_ISEDGE     !  L24ADD     :ISEDGE:SECND EDGES NODES AND FLAGS      :      24
        integer ::   S_CAND_T     !  MULTIMP*NSN:CAND_T:Candidate type                   :      24
c            ! =0 node to surface candidate
c            ! =1 node or edge to surface candidate
c            ! =2 edge to surface candidate
        integer ::   S_ISEG_PXFEM !  ISEG_PXFEM:       :      24
        integer ::   S_ISEG_PLY   !  ISEG_PLY          :      24

        integer ::   S_ICONT_I    !  NSN not detected pene_ini <0 :   24
C---- edge        
        integer ::   S_IRTSE      !  (5,NRTSE)
        integer ::   S_IS2SE      !  (2,NSNE)
        integer ::   S_IS2PT      !  (NSNE)
        integer ::   S_ISPT2      !  (NSN) - tag for irtlm on Fictive node
        integer ::   S_ISEGPT     !  (NSN)
        integer ::   S_IS2ID      !  (NSNE)
        integer ::   S_IFPEN_E    !  (MULTIMPE*NCONTE) : filter candidat friction       :25
        integer ::   S_IFPEN_E2S  !  (MULTIMPE*NCONTE) : filter candidat friction       :25
C---- ige
        integer ::   S_NIGE
C---- INTER FRICTION PARTS
        integer ::   S_IPARTFRICS!  NSN       :IPARTFRICS : number of part of secnd nodes 
        integer ::   S_IPARTFRICM!  NRTM      :IPARTFRICM : number of part of main segments
        integer ::   S_IPARTFRIC_E!  NEDGE    :IPARTFRIC_E : number of part of edge int25
C       Orthotropic friction 
        integer ::   S_IREP_FRICM !  NRTM      :IREP_FRIC : Orthotropic system formulation flag for reference vector for friction (same as IORTH/IREP for prop) 
C---- Irem Gap option for inter type25
        integer ::   S_KREMNOR!  NSN+1    :KREMNOR : skyline tab for main segment adress in REMNOR tab
        integer ::   S_REMNOR!   REMNODE  :REMNOR  : tab for main segment forbidden for each secnd node
        integer ::   S_KREMNODE_EDG   !  NEDGE+1    :KREMNODE_EDG:ADRESS LINES REMOVED FROM CONTACT IN REMNODE ARRAY :      25
        integer ::   S_REMNODE_EDG    !  NREMNODE_EDG   :REMNODE_EDG: LINES REMOVED FROM CONTACT     :      25
        integer ::   S_KREMNODE_E2S   !  NRTM+1    :KREMNODE_EDG:ADRESS LINES REMOVED FROM CONTACT IN REMNODE ARRAY :      25
        integer ::   S_REMNODE_E2S    !  NREMNODE_E2S   :REMNODE_EDG: LINES REMOVED FROM CONTACT     :      25
C----Nitsche Method
        integer ::   S_IELNRTS!  NRTS      :IELNRTS : number of secnd element 
        integer ::   S_ADRECTS!  4*NRTS    :ADRECTS : Adress of each secnd node of the segment in IXS/IXS10/IXS20/IXS16 
        integer ::   S_FACNRTS!  NRTS      :FACNRTS : Corresponding facet in element IELNRTS 
        integer ::   S_E2S_ACTNOR!
C=======================================================================
c DEFINE SIZES (float arrays)
C=======================================================================
        integer ::   S_STFAC        !  1          :STFAC :FACTEUR DE RIGIDITE D'INTERFACE(MANAGER)
        integer ::   S_VARIABLES    !   +1, ...
        integer ::   S_CSTS         !  2*NSN      :CSTS  :S ET T DES NOEUDS SECONDS       :   ,3,4,    
        integer ::   S_DPARA        !  7*NSN      :DPARA :DET,B1,B2,B3,C1,C2,C3            :  2
        integer ::   S_NMAS         !  NMN        :MMAS  :MASSE NOEUDS MAINS             :  2
        integer ::   S_SMAS         !  NSN      :SMAS : MASSE ET INERTIE DES NOEUDS SECND :   2  
        integer ::   S_SINER        !  NSN      :SINER : MASSE ET INERTIE DES NOEUDS SECND :   2
        integer ::   S_AREAS2       !  NSN       AREA       : secnd area                  :  int2 with rupture  
        integer ::   S_UVAR         !  NSN*NUVAR UVAR       : user buffer for secnds      :  int2 with rupture  
        integer ::   S_XM0          !  NSN*3     XM0(x,y,z) : secnd-main initial distance: int2 with rupture
        integer ::   S_SPENALTY     !  NSN      :
        integer ::   S_STFR_PENALTY !  NSN      :S
        integer ::   S_SKEW         !  ILEV=25  : NSN*3     XM0(x,y,z) : secnd-main initial distance: int2 with rupture
        integer ::   S_DSM          !  NSN*3     DSM(x,y,z) : secnd-main displacement    :  int2 with rupture
        integer ::   S_FSM          !  NSN*3     FSM(x,y,z) : secnd-main force           :  int2 with rupture
        integer ::   S_RUPT         !  6         RUPT       : penality/rupture parameters  :  int2 with rupture
        integer ::   S_FINI         !  ILEV=25   :3*NSN  :  int2 with rupture
        integer ::   S_STFNS        !  NSN        :STFNS :RIGIDITE NOEUDS SECONDS         :   ,3,4,    7
        integer ::   S_STFM         !  NRTM       :STFM  :RIGIDITE FACES MAIN        :   ,3,4,    7  10,11
        integer ::   S_STFS         !  NRTS       :STFS  :RIGIDITE FACES SECONDS          :   ,3,4          11
        integer ::   S_PENIM        !  2*NRTM     :PENIM :pene initiale max / main       :     20           11
        integer ::   S_PENIS        !  2*NRTS     :PENIS :pene initiale max / secnd        :                  11
        integer ::   S_GAP_M        !  NRTM       :GAP_M :GAP FACES MAIN             :(7)10,11,24
        integer ::   S_GAP_S        !  NSN        :GAP_S :GAP NOEUDS SECONDS              :(7)10,20
        integer ::   S_XSAV         !  3*MIN(NUMNOD,NSN+NMN):CRIT  :X(XYZ)                   :            7
        integer ::   S_CRIT         !  12         :CRIT  :MIN et MAX DEPLACEMENT (SPMD)    :            7 (10,11)
        integer ::   S_FRIC_P       !  10         :FRIC_P:FRICTION PARAMETERS              :        5,  7     24
        integer ::   S_XFILTR       !  1          :XFILTR:COEFF FILTRAGE FROTTEMENT     )  :        5,  7     24
        integer ::   S_AREAS        !  NSN       AREA       : secnd area                  :  7
        integer ::   S_AREAM        !  NRTM      AREA       : main area                  :  11
        integer ::   S_GAP_SL       !  NSN        :GAP_S_L :
        integer ::   S_GAP_ML       !  NRTM       :GAP_M_L :           
        integer ::   S_CAND_P       !  MULTIMP*NSN:CAND_P:PENETRATION INITIALE             :           (7)   
        integer ::   S_FTSAVX       !  MULTIMP*NSN:FTSAVX :    SAUVEGARDE FILTRAGE         :            7, 8  24
        integer ::   S_FTSAVY       !  MULTIMP*NSN:FTSAVY :    SAUVEGARDE FILTRAGE         :            7, 8  24
        integer ::   S_FTSAVZ       !  MULTIMP*NSN:FTSAVZ :    SAUVEGARDE FILTRAGE         :            7, 8  24
        integer ::   S_FTSAVX_E   !  MULTIMP*NCONTE    :FTSAVX_E : Friction for saving     :            25
        integer ::   S_FTSAVY_E   !  MULTIMP*NCONTE    :FTSAVX_E : Friction for saving     :            25 
        integer ::   S_FTSAVZ_E   !  MULTIMP*NCONTE    :FTSAVX_E : Friction for saving     :            25 
        integer ::   S_FTSAVX_E2S !  MULTIMP*NCONTE    :FTSAVX_E2S : Friction for saving   :            25 
        integer ::   S_FTSAVY_E2S !  MULTIMP*NCONTE    :FTSAVX_E2S : Friction for saving   :            25 
        integer ::   S_FTSAVZ_E2S !  MULTIMP*NCONTE    :FTSAVX_E2S : Friction for saving   :            25 
C---- ige
        integer ::   S_RIGE          
        integer ::   S_XIGE          
        integer ::   S_VIGE 
        integer ::   S_MASSIGE         
!type10
        integer ::   S_CAND_F       !  6*4*NSN    :CAND_F:ANCIENNE FORCES LOCALES +H1 A H3 :               10    
!type11
        integer ::   S_CAND_MAX 
!type20
        integer ::   S_CAND_FX      !  
        integer ::   S_CAND_FY      !  
        integer ::   S_CAND_FZ      !  
        integer ::   S_XA           !  3*NSN;3*NLN:XA    :XA DES NOEUDS                    :       20
        integer ::   S_VA           !  3*NSN;3*NLN:VA    :VA DES NOEUDS                    :       2
        integer ::   S_STFA         !  
        integer ::   S_PENIA        !  5*NLN      :PENIA :pene initiale points d'ancrages  :     20 
        integer ::   S_ALPHAK       !  3*NLN      :ALPHAK:reduction de rigidite d'ancrages :     20
        integer ::   S_GAP_SH       !  NRTM       :GAP_SH:Shift de gap pour gap nul solides:     20
        integer ::   S_AVX_ANCR     !  9*NLN :A-V-X-ANCR :Accel Vit deplacement POINT D'ANCRAGE: 20
        integer ::   S_CRITX        !  
        integer ::   S_GAP_ME       !  
        integer ::   S_STF          !  NLINM      :STFM  :RIGIDITE FACES MAIN        :       20
        integer ::   S_PENIME       !  2*NLINM    :PENIME:pene initiale max / edge main       :       20
        integer ::   S_GAP_SE       !   
        integer ::   S_PENISE       !  2*NLINS    :PENISE:pene initiale max / edge secnd   :       20
        integer ::   S_STFNE        !  NSNE       :STFNE :RIGIDITE NOEUDS SECONDS         :       20
!type1
        integer ::   S_N            !  3*NSN      :N     :NORMALE                          :1,
!type3,4,5,9
        integer ::   S_CSTM         !  2*NMN      :CSTM  :S ET T DES NOEUDS MAINS        :   ,3,4,
        integer ::   S_STFNM        !  NMN        :STFNM :RIGIDITE NOEUDS MAINS          :   ,3,4
        integer ::   S_FRICOS       !  3*NSN      :FRICOS:FORCE DE FRICTION(SECOND)       :   ,3,4,    
        integer ::   S_FRICOM       !  3*NMN      :FRICOM:FORCE DE FRICTION(MAIN)        :   ,3,4,   
        integer ::   S_FTSAV        !  3*NSN      :FTSAV :     SAUVEGARDE FILTRAGE         :        5         
!type6
        integer ::   S_FCONT        !  1          :FCONT :                                 :          6  
        integer ::   S_FS           !  3*NSN      :FS    :SAUVEGARDE DE LA PENETRATION     :          6
        integer ::   S_FM           !  3*NSN      :FM    :SAUVEGARDE DE LA PENETRATION     :          6
        integer ::   S_RMAS         !  2          :RMAS  :MASSE DES RIGID BODY             :          6
        integer ::   S_ANSMX0       
!type 8
        integer ::   S_T8          !   SIZE OF T8 Structure  (1 if Type8, 0 elsewhere)            :          8 
        integer ::   S_GAPN         !   NRTM
        integer ::   S_STF8         !   NRTM

!type 9
        integer ::   S_EE           !  EE    :ENERGIE DE FROTTEMENT            : 9

!type14
        integer ::   S_CIMP         !  3*NSN    :
        integer ::   S_NIMP         !  3*NSN    :
!type15
        integer ::   S_IOLD         !  
        integer ::   S_HOLD         !  
        integer ::   S_NOLD         !  
        integer ::   S_DOLD         !  
!type17
        integer ::   S_KS           !  
        integer ::   S_KM           !  
        integer ::   S_FROTS        !  
        integer ::   S_FROTM        !  
!type21
        integer ::   S_NOD_NORMAL   !  
        integer ::   S_RCURV        !  
        integer ::   S_ANGLM        !  
        integer ::   S_FROT_P       !  
        integer ::   S_ALPHA0       !  
        integer ::   S_AS           !  
        integer ::   S_BS           !  
        integer ::   S_THKNOD0      !  
!type22
!type23
!type24
        integer ::   S_GAPN_M       !  NMN        :GAPN_M:Nodal main gap                 :  24, 25 
        integer ::   S_SECND_FR      !  6*NSN      :SECND_FR:new(1:3) and old(4:6) friction F:  24  
        integer ::   S_PENE_OLD     !  NSN*5      :PENE_OLD:Old penetration,PEN_ini(Inacti=5):       24
        integer ::   S_STIF_OLD     !  NSN*2      :STIF_OLD:Old stifness                   :       24
        integer ::   S_TIME_S       !  NSN        :TIME_S :   impact TIME                  :  24
        integer ::   S_GAP_NM       !  12*NRTM          
C---------to be cleaned later
        integer ::   S_EDGE8L2      !  NSN        :EDGE8L2: max half edge length on secnd node: 24
        integer ::   S_NOD_2RY_LGTH !  NSN        :NODAL SECND LENGTH (TO CAL. Kg)         :     24  
        integer ::   S_NOD_MAS_LGTH !  NMN        :NODAL main LENGTH (TO CAL. Kg)        :     24  
        integer ::   S_GAP_N0       !  12*NRTM        :
        integer ::   S_DGAP_NM      !  4*NRTM        :
        integer ::   S_DGAP_M       !  NRTM        :
        integer ::   S_DELTA_PMAX_DGAP !  1        :I
        integer ::   S_XFIC         !  3*NSNE
        integer ::   S_VFIC         !  3*NSNE
        integer ::   S_MSFIC        !  NSNE

!type25
        integer ::   S_EDGE_BISECTOR       !  3*4*NRTM
        integer ::   S_PENM         !  4*MULTIMP*NSN      :PENM:Penetrations wrt 4 sub-triangles :  25 
        integer ::   S_DISTM        !  MULTIMP*NSN        :DISTM:True distance to segment        :  25 
        integer ::   S_LBM          !  MULTIMP*NSN        :LBM:Coordinate LB wrt 4 triangles     :  25 
        integer ::   S_LCM          !  MULTIMP*NSN        :LCM:Coordinate LC wrt 4 triangles     :  25 
        integer ::   S_VTX_BISECTOR !  3*2*NADMSR         :                                      :  25
        integer ::   S_CAND_PS      !  4*MULTIMPS*NCONTE:CAND_PS:PENETRATION INITIALE vs Solid Main Edges: 25
        integer ::   S_GAPE         !  NEDGE 
        integer ::   S_GAP_E_L      !  NEDGE 
        integer ::   S_STFE         !  NEDGE 
        integer ::   S_GAPMSAV      ! NRTM
        integer ::   S_E2S_NOD_NORMAL ! 3*NADMSR

        integer :: NUMBER_EDGE_TYPE1
        integer :: NUMBER_EDGE_TYPE1_0
C       Orthotropic friction 
        integer ::   S_DIR_FRICM !  2*NRTM         : orhotropic directions for friction 
C   Stiffness based on mass and time step
        integer ::   S_STIFMSDT_S       ! NSN : nodal mass on secondary node *STFACM
        integer ::   S_STIFMSDT_M       ! NRTM : main mass *STFACM
        integer ::   S_STIFMSDT_EDG     ! NEDGE : edge mass *STFACM
C
C=======================================================================
c DEFINE ARRAYS (integers arrays)
C=======================================================================
        integer, DIMENSION(:) , POINTER ::  IRECTS    
        integer, DIMENSION(:) , POINTER ::  IRECTM    
        integer, DIMENSION(:) , POINTER ::  NSV       
        integer, DIMENSION(:) , POINTER ::  MSR       
        integer, DIMENSION(:) , POINTER ::  IRTLM     
        integer, DIMENSION(:) , POINTER ::  IRUPT     
        integer, DIMENSION(:) , POINTER ::  INORM     
        integer, DIMENSION(:) , POINTER ::  IELEC     
        integer, DIMENSION(:) , POINTER ::  IELES     
        integer, DIMENSION(:) , POINTER ::  LISUB  
        integer, DIMENSION(:) , POINTER ::  TYPSUB        
        integer, DIMENSION(:) , POINTER ::  ADDSUBS   
        integer, DIMENSION(:) , POINTER ::  ADDSUBM   
        integer, DIMENSION(:) , POINTER ::  LISUBS    
        integer, DIMENSION(:) , POINTER ::  LISUBM    
        integer, DIMENSION(:) , POINTER ::  INFLG_SUBS
        integer, DIMENSION(:) , POINTER ::  INFLG_SUBM
        integer, DIMENSION(:) , POINTER ::  ADDSUBE   
        integer, DIMENSION(:) , POINTER ::  LISUBE    
        integer, DIMENSION(:) , POINTER ::  INFLG_SUBE
        integer, DIMENSION(:) , POINTER ::  MSEGTYP   
        integer, DIMENSION(:) , POINTER ::  CAND_E    
        integer, DIMENSION(:) , POINTER ::  CAND_N    
        integer, DIMENSION(:) , POINTER ::  I_STOK    
        integer, DIMENSION(:) , POINTER ::  I_STOK_E  
        integer, DIMENSION(:) , POINTER ::  IFPEN     
        integer, DIMENSION(:) , POINTER ::  KREMNODE  
        integer, DIMENSION(:) , POINTER ::  REMNODE   
        integer, DIMENSION(:) , POINTER ::  ADCCM     
        integer, DIMENSION(:) , POINTER ::  CHAIN     
        integer, DIMENSION(:) , POINTER ::  NIGE
!type20
        integer, DIMENSION(:) , POINTER ::  NBINFLG   
        integer, DIMENSION(:) , POINTER ::  MBINFLG   
        integer, DIMENSION(:) , POINTER ::  EBINFLG   
        integer, DIMENSION(:) , POINTER ::  NLG       
        integer, DIMENSION(:) , POINTER ::  DAANC6    
        integer, DIMENSION(:) , POINTER ::  ISLINS    
        integer, DIMENSION(:) , POINTER ::  ISLINM    
        integer, DIMENSION(:) , POINTER ::  IXLINS    
        integer, DIMENSION(:) , POINTER ::  IXLINM    
        integer, DIMENSION(:) , POINTER ::  NSVL      
        integer, DIMENSION(:) , POINTER ::  MSRL      
        integer, DIMENSION(:) , POINTER ::  LCAND_N   
        integer, DIMENSION(:) , POINTER ::  LCAND_S   
        integer, DIMENSION(:) , POINTER ::  ADCCM20   
        integer, DIMENSION(:) , POINTER ::  CHAIN20   
!type1
        integer, DIMENSION(:) , POINTER ::  ILOCS     
        integer, DIMENSION(:) , POINTER ::  NSEGM     
        integer, DIMENSION(:) , POINTER ::  NRT       
!type2
        integer, DIMENSION(:) , POINTER ::  MSEGTYP2
!type3
        integer, DIMENSION(:) , POINTER ::  IRTLS     
        integer, DIMENSION(:) , POINTER ::  ILOCM     
        integer, DIMENSION(:) , POINTER ::  IRTLOM    
        integer, DIMENSION(:) , POINTER ::  IRTLOS    
        integer, DIMENSION(:) , POINTER ::  NSEGS     
        integer, DIMENSION(:) , POINTER ::  LNSV      
        integer, DIMENSION(:) , POINTER ::  LMSR      
!type4
        integer, DIMENSION(:) , POINTER ::  IELEM     
!type12
        integer, DIMENSION(:) , POINTER ::  FCOUNT    
!type14
        integer, DIMENSION(:) , POINTER ::  KSURF     
        integer, DIMENSION(:) , POINTER ::  IMPACT    

!type21
        integer, DIMENSION(:) , POINTER ::  MSR21     
        integer, DIMENSION(:) , POINTER ::  MNDD      
        integer, DIMENSION(:) , POINTER ::  MSR_L     
!type24
        integer, DIMENSION(:) , POINTER ::  MVOISIN   
        integer, DIMENSION(:) , POINTER ::  NVOISIN   
        integer, DIMENSION(:) , POINTER ::  MSEGLO    
        integer, DIMENSION(:) , POINTER ::  MSEGTYP24 
C--------to be cleaned later        
        integer, DIMENSION(:) , POINTER ::  ISEADD    
        integer, DIMENSION(:) , POINTER ::  ISEDGE    
        integer, DIMENSION(:) , POINTER ::  CAND_T    
        integer, DIMENSION(:) , POINTER ::  ISEG_PXFEM
        integer, DIMENSION(:) , POINTER ::  ISEG_PLY  
        integer, DIMENSION(:) , POINTER ::  ICONT_I   
        integer, DIMENSION(:) , POINTER ::  IRTSE     
        integer, DIMENSION(:) , POINTER ::  IS2SE     
        integer, DIMENSION(:) , POINTER ::  IS2PT     
        integer, DIMENSION(:) , POINTER ::  ISPT2     
        integer, DIMENSION(:) , POINTER ::  ISEGPT     
        integer, DIMENSION(:) , POINTER ::  IS2ID      ! Global ID Fictive Nodes   
!type25
        integer, DIMENSION(:) , POINTER ::  NSV_ON_PMAIN     !  
        integer, DIMENSION(:) , POINTER ::  EVOISIN   !  
        integer, DIMENSION(:) , POINTER ::  ADMSR     !  
        integer, DIMENSION(:) , POINTER ::  LEDGE     !  
        integer, DIMENSION(:) , POINTER ::  LBOUND    !  
        integer, DIMENSION(:) , POINTER ::  FREE_IRECT_ID!
        integer, DIMENSION(:) , POINTER ::  ACTNOR    !  
        integer, DIMENSION(:) , POINTER ::  FARM      !  
        integer, DIMENSION(:) , POINTER ::  ADSKYN    !  
        integer, DIMENSION(:) , POINTER ::  IADNOR    !  
        integer, DIMENSION(:) , POINTER ::  ISLIDE    !  
        integer, DIMENSION(:) , POINTER ::  KNOR2MSR  !  
        integer, DIMENSION(:) , POINTER ::  NOR2MSR   !  
        integer, DIMENSION(:) , POINTER ::  CAND_OPT_N!  
        integer, DIMENSION(:) , POINTER ::  CAND_OPT_E!
        integer, DIMENSION(:) , POINTER ::  IF_ADH    ! Type25 and interface adhesion
        integer, DIMENSION(:) , POINTER ::  CANDM_E2E  
        integer, DIMENSION(:) , POINTER ::  CANDS_E2E
        integer, DIMENSION(:) , POINTER ::  CANDM_E2S  
        integer, DIMENSION(:) , POINTER ::  CANDS_E2S
        integer, DIMENSION(:) , POINTER ::  IFPEN_E   
        integer, DIMENSION(:) , POINTER ::  IFPEN_E2S      
        integer, DIMENSION(:), ALLOCATABLE :: EDGE_TYPE1
        integer, DIMENSION(:), ALLOCATABLE :: EDGE_TYPE1_0
C---- INTER FRICTION PARTS
        integer, DIMENSION(:) , POINTER ::  IPARTFRICS 
        integer, DIMENSION(:) , POINTER ::  IPARTFRICM
        integer, DIMENSION(:) , POINTER ::  IPARTFRIC_E
C      Orthotropic friction
        integer, DIMENSION(:) , POINTER ::  IREP_FRICM        
C---- Irem Gap option for interface type 25
        integer, DIMENSION(:) , POINTER ::  KREMNOR
        integer, DIMENSION(:) , POINTER ::  REMNOR 
        integer, DIMENSION(:) , POINTER ::  KREMNODE_EDG  
        integer, DIMENSION(:) , POINTER ::  REMNODE _EDG 
        integer, DIMENSION(:) , POINTER ::  KREMNODE_E2S  
        integer, DIMENSION(:) , POINTER ::  REMNODE _E2S 
C----Nitsche Method
        integer, DIMENSION(:) , POINTER ::  IELNRTS
        integer, DIMENSION(:) , POINTER ::  ADRECTS 
        integer, DIMENSION(:) , POINTER ::  FACNRTS

        integer, DIMENSION(:) , POINTER ::  E2S_ACTNOR

C=======================================================================
c DEFINE ARRAYS (float arrays)
C=======================================================================
        my_real, DIMENSION(:) , POINTER ::   STFAC        
        my_real, DIMENSION(:) , POINTER ::   VARIABLES    
        my_real, DIMENSION(:) , POINTER ::   CSTS         
        my_real, DIMENSION(:) , POINTER ::   DPARA        
        my_real, DIMENSION(:) , POINTER ::   NMAS         
        my_real, DIMENSION(:) , POINTER ::   SMAS         
        my_real, DIMENSION(:) , POINTER ::   SINER        
        my_real, DIMENSION(:) , POINTER ::   AREAS2       
        my_real, DIMENSION(:) , POINTER ::   UVAR         
        my_real, DIMENSION(:) , POINTER ::   XM0          
        my_real, DIMENSION(:) , POINTER ::   SPENALTY     
        my_real, DIMENSION(:) , POINTER ::   STFR_PENALTY 
        my_real, DIMENSION(:) , POINTER ::   SKEW         
        my_real, DIMENSION(:) , POINTER ::   DSM          
        my_real, DIMENSION(:) , POINTER ::   FSM          
        my_real, DIMENSION(:) , POINTER ::   RUPT         
        my_real, DIMENSION(:) , POINTER ::   FINI         
        my_real, DIMENSION(:) , POINTER ::   STFNS        
        my_real, DIMENSION(:) , POINTER ::   STFM         
        my_real, DIMENSION(:) , POINTER ::   STFS         
        my_real, DIMENSION(:) , POINTER ::   PENIM        
        my_real, DIMENSION(:) , POINTER ::   PENIS        
        my_real, DIMENSION(:) , POINTER ::   GAP_M        
        my_real, DIMENSION(:) , POINTER ::   GAP_S        
        my_real, DIMENSION(:) , POINTER ::   XSAV         
        my_real, DIMENSION(:) , POINTER ::   CRIT         
        my_real, DIMENSION(:) , POINTER ::   FRIC_P       
        my_real, DIMENSION(:) , POINTER ::   XFILTR       
        my_real, DIMENSION(:) , POINTER ::   AREAS        
        my_real, DIMENSION(:) , POINTER ::   AREAM        
        my_real, DIMENSION(:) , POINTER ::   GAP_SL       
        my_real, DIMENSION(:) , POINTER ::   GAP_ML       
        my_real, DIMENSION(:) , POINTER ::   CAND_P       
        my_real, DIMENSION(:) , POINTER ::   FTSAVX       
        my_real, DIMENSION(:) , POINTER ::   FTSAVY       
        my_real, DIMENSION(:) , POINTER ::   FTSAVZ  
        my_real, DIMENSION(:) , POINTER ::   FTSAVX_E      
        my_real, DIMENSION(:) , POINTER ::   FTSAVY_E       
        my_real, DIMENSION(:) , POINTER ::   FTSAVZ_E
        my_real, DIMENSION(:) , POINTER ::   FTSAVX_E2S       
        my_real, DIMENSION(:) , POINTER ::   FTSAVY_E2S       
        my_real, DIMENSION(:) , POINTER ::   FTSAVZ_E2S       
C---- ige
        my_real, DIMENSION(:) , POINTER ::   RIGE
        my_real, DIMENSION(:) , POINTER ::   XIGE
        my_real, DIMENSION(:) , POINTER ::   VIGE
        my_real, DIMENSION(:) , POINTER ::   MASSIGE
!type10
        my_real, DIMENSION(:) , POINTER ::   CAND_F    
!type20
        my_real, DIMENSION(:) , POINTER ::   CAND_FX   
        my_real, DIMENSION(:) , POINTER ::   CAND_FY   
        my_real, DIMENSION(:) , POINTER ::   CAND_FZ   
        my_real, DIMENSION(:) , POINTER ::   XA        
        my_real, DIMENSION(:) , POINTER ::   VA        
        my_real, DIMENSION(:) , POINTER ::   STFA      
        my_real, DIMENSION(:) , POINTER ::   PENIA     
        my_real, DIMENSION(:) , POINTER ::   ALPHAK    
        my_real, DIMENSION(:) , POINTER ::   GAP_SH    
        my_real, DIMENSION(:) , POINTER ::   AVX_ANCR  
        my_real, DIMENSION(:) , POINTER ::   CRITX     
        my_real, DIMENSION(:) , POINTER ::   GAP_ME    
        my_real, DIMENSION(:) , POINTER ::   STF       
        my_real, DIMENSION(:) , POINTER ::   PENIME    
        my_real, DIMENSION(:) , POINTER ::   GAP_SE    
        my_real, DIMENSION(:) , POINTER ::   PENISE    
        my_real, DIMENSION(:) , POINTER ::   STFNE     
!type1
        my_real, DIMENSION(:) , POINTER ::   N       
!type3,4,9
        my_real, DIMENSION(:) , POINTER ::   CSTM    
        my_real, DIMENSION(:) , POINTER ::   STFNM   
        my_real, DIMENSION(:) , POINTER ::   FRICOS  
        my_real, DIMENSION(:) , POINTER ::   FRICOM  
        my_real, DIMENSION(:) , POINTER ::   FTSAV   
!type6
        my_real, DIMENSION(:) , POINTER ::   FCONT   
        my_real, DIMENSION(:) , POINTER ::   FS      
        my_real, DIMENSION(:) , POINTER ::   FM      
        my_real, DIMENSION(:) , POINTER ::   RMAS    
        my_real, DIMENSION(:) , POINTER ::   ANSMX0  
!type 8
        my_real, DIMENSION(:) , POINTER ::   GAPN    
        my_real, DIMENSION(:) , POINTER ::   STF8    
        my_real, DIMENSION(:) , POINTER ::   EE      
!type14
        my_real, DIMENSION(:) , POINTER ::   CIMP   
        my_real, DIMENSION(:) , POINTER ::   NIMP   

!type15
        my_real, DIMENSION(:) , POINTER ::   IOLD   
        my_real, DIMENSION(:) , POINTER ::   HOLD   
        my_real, DIMENSION(:) , POINTER ::   NOLD   
        my_real, DIMENSION(:) , POINTER ::   DOLD   
!type17
        my_real, DIMENSION(:) , POINTER ::   KS     
        my_real, DIMENSION(:) , POINTER ::   KM     
        my_real, DIMENSION(:) , POINTER ::   FROTS  
        my_real, DIMENSION(:) , POINTER ::   FROTM  

!type21
        my_real, DIMENSION(:) , POINTER ::   NOD_NORMAL 
        my_real, DIMENSION(:) , POINTER ::   RCURV      
        my_real, DIMENSION(:) , POINTER ::   ANGLM      
        my_real, DIMENSION(:) , POINTER ::   FROT_P     
        my_real, DIMENSION(:) , POINTER ::   ALPHA0     
        my_real, DIMENSION(:) , POINTER ::   AS         
        my_real, DIMENSION(:) , POINTER ::   BS         
        my_real, DIMENSION(:) , POINTER ::   THKNOD0    
!type24
        my_real, DIMENSION(:) , POINTER ::   GAPN_M   
        my_real, DIMENSION(:) , POINTER ::   SECND_FR  
        my_real, DIMENSION(:) , POINTER ::   PENE_OLD 
        my_real, DIMENSION(:) , POINTER ::   STIF_OLD 
        my_real, DIMENSION(:) , POINTER ::   TIME_S   
        my_real, DIMENSION(:) , POINTER ::   GAP_NM   
C-------to be cleaned later        
        my_real, DIMENSION(:) , POINTER ::   EDGE8L2  
        my_real, DIMENSION(:) , POINTER ::   NOD_2RY_LGTH 
        my_real, DIMENSION(:) , POINTER ::   NOD_MAS_LGTH 
        my_real, DIMENSION(:) , POINTER ::   GAP_N0  
        my_real, DIMENSION(:) , POINTER ::   DGAP_NM 
        my_real, DIMENSION(:) , POINTER ::   DGAP_M  
        my_real, DIMENSION(:) , POINTER ::   DELTA_PMAX_DGAP  
        my_real, DIMENSION(:) , POINTER ::   XFIC     
        my_real, DIMENSION(:) , POINTER ::   VFIC     
        my_real, DIMENSION(:) , POINTER ::   MSFIC     
!type25
        REAL(4), DIMENSION(:) , POINTER ::   EDGE_BISECTOR  !
        my_real, DIMENSION(:) , POINTER ::   PENM    !
        my_real, DIMENSION(:) , POINTER ::   DISTM   !
        my_real, DIMENSION(:) , POINTER ::   LBM     !
        my_real, DIMENSION(:) , POINTER ::   LCM     !
        REAL(4), DIMENSION(:) , POINTER ::   VTX_BISECTOR ! 
        my_real, DIMENSION(:) , POINTER ::   CAND_PS
        my_real, DIMENSION(:) , POINTER ::   GAPE
        my_real, DIMENSION(:) , POINTER ::   GAP_E_L
        my_real, DIMENSION(:) , POINTER ::   STFE
        my_real, DIMENSION(:) , POINTER ::   GAPMSAV
        REAL(4), DIMENSION(:) , POINTER ::   E2S_NOD_NORMAL  !

        my_real, DIMENSION(:) , POINTER ::   STIFMSDT_S
        my_real, DIMENSION(:) , POINTER ::   STIFMSDT_M
        my_real, DIMENSION(:) , POINTER ::   STIFMSDT_EDG
        INTEGER :: NRTM_FREE

C MPI Communicators
        INTEGER :: MPI_COMM
        INTEGER :: RANK
        INTEGER :: NSPMD
C
        INTEGER :: NB_INTERNAL_EDGES        ! number of edges internal to the domain
        INTEGER :: NB_BOUNDARY_EDGES_LOCAL  ! boundary edges treated by current domain 
        INTEGER :: NB_BOUNDARY_EDGES_REMOTE ! boundary edges treated by the other domain 


!type2
        my_real, DIMENSION(:) , POINTER ::   CSTS_BIS
C      Orthotropic friction
        my_real, DIMENSION(:) , POINTER ::   DIR_FRICM
C
        TYPE(INT8_STRUCT_)             ::   T8
        TYPE(METRIC_STRUCT_)           ::   METRIC

C=======================================================================
      END TYPE INTBUF_STRUCT_
C=======================================================================

c INTBUF_SIZE array maximum length defined as parameter
c (maximum number of different arrays composing INTBUF_TAB structure)
      INTEGER, PARAMETER :: L_INTBUF_SIZE_MAX = 516
      INTEGER INTER_ITHKNOD !Flag to fill THKNOD array  (enabled with Ithick parameter from interface type 25 or 21)


c
c---------------
      END MODULE INTBUFDEF_MOD
