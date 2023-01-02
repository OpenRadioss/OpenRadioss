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
Chd|  ARRET                         source/system/arret.F         
Chd|-- called by -----------
Chd|        INTBUF_INI                    ../common_source/interf/intbuf_ini.F
Chd|        PBLAST_LOAD                   ../common_source/modules/pblast_mod.F
Chd|        UPGRADE_CAND_OPT              ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_E2S             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_EDG             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        ADMERR                        source/model/remesh/admerr.F  
Chd|        ADMINI                        source/model/remesh/admini.F  
Chd|        ALERO2                        source/ale/ale2d/alero2.F     
Chd|        ANIMX                         source/output/anim/generate/animx.F
Chd|        ANIM_BUILD_INDEX_ALL          source/output/anim/reader/anim_build_index_all.F
Chd|        ARRET_MESSAGE                 source/system/arret_message.F 
Chd|        ARRET_MESSAGE_SLID            source/system/arret_message.F 
Chd|        BEMSOLV                       source/fluid/bemsolv.F        
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        BLERO2                        source/ale/bimat/blero2.F     
Chd|        BUILD_CONNECTIVITY            source/multifluid/connectivity.F
Chd|        CP_IFRONT                     source/implicit/produt_v.F    
Chd|        CREATE_H3D_1D_SCALAR          source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
Chd|        CREATE_H3D_1D_TENSOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
Chd|        CREATE_H3D_1D_TORSOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
Chd|        CREATE_H3D_1D_VECTOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
Chd|        CREATE_H3D_INPUT              source/output/h3d/h3d_build_fortran/create_h3d_input.F
Chd|        CREATE_H3D_NODAL_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
Chd|        CREATE_H3D_NODAL_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
Chd|        CREATE_H3D_NODAL_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
Chd|        CREATE_H3D_PARTS              source/output/h3d/h3d_build_fortran/create_h3d_parts.F
Chd|        CREATE_H3D_PARTS_ALL          source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
Chd|        CREATE_H3D_QUAD_SCALAR        source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
Chd|        CREATE_H3D_QUAD_TENSOR        source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
Chd|        CREATE_H3D_QUAD_VECTOR        source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
Chd|        CREATE_H3D_SHELL_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
Chd|        CREATE_H3D_SHELL_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
Chd|        CREATE_H3D_SHELL_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
Chd|        CREATE_H3D_SKIN_SCALAR        source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
Chd|        CREATE_H3D_SKIN_TENSOR        source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
Chd|        CREATE_H3D_SKIN_VECTOR        source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
Chd|        CREATE_H3D_SOLID_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
Chd|        CREATE_H3D_SOLID_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
Chd|        CREATE_H3D_SOLID_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
Chd|        CREATE_H3D_SPH_SCALAR         source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
Chd|        CREATE_H3D_SPH_TENSOR         source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
Chd|        CRKLAYER3N_ADV                source/elements/xfem/crklayer3n_adv.F
Chd|        CRKLAYER3N_INI                source/elements/xfem/crklayer3n_ini.F
Chd|        CRKLAYER4N_ADV                source/elements/xfem/crklayer4n_adv.F
Chd|        CRKLAYER4N_INI                source/elements/xfem/crklayer4n_ini.F
Chd|        CRKLEN3N_ADV                  source/elements/xfem/crklen3n_adv.F
Chd|        CRKLEN4N_ADV                  source/elements/xfem/crklen4n_adv.F
Chd|        C_FORTRAN_DGSSV               stub/superlu_stub.F           
Chd|        C_FREEMEM                     stub/superlu_stub.F           
Chd|        C_TRICALL                     stub/fvmbags_stub.F           
Chd|        DAASOLV                       source/fluid/daasolv.F        
Chd|        DIM_INT7                      source/implicit/ind_glob_k.F  
Chd|        DTNODA                        source/time_step/dtnoda.F     
Chd|        DTNODAMS                      source/time_step/dtnodams.F   
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_INI                      source/implicit/imp_dyna.F    
Chd|        EIG                           stub/eig.F                    
Chd|        EIGCOND                       stub/eigcond.F                
Chd|        ENRICHC_INI                   source/elements/xfem/enrichc_ini.F
Chd|        ENRICHTG_INI                  source/elements/xfem/enrichtg_ini.F
Chd|        ERROR_ALLOC                   source/system/error_alloc.F   
Chd|        ERR_MEM                       source/implicit/lin_solv.F    
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|        ERR_TPU                       source/materials/mat/mat101/sigeps101.F
Chd|        EULRO2                        source/ale/euler2d/eulro2.F   
Chd|        EXECARGCHECK                  source/engine/execargcheck.F  
Chd|        FACEPOLY                      source/airbag/fvmesh.F        
Chd|        FIXFLUX                       source/constraints/thermic/fixflux.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FRALNK                        source/input/fralnk.F         
Chd|        FRALNK2                       source/input/fralnk.F         
Chd|        FREABF                        source/input/freabf.F         
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|        FREBCS                        source/input/frebcs.F         
Chd|        FRECPL                        source/input/frecpl.F         
Chd|        FREDAMP                       source/input/fredamp.F        
Chd|        FREDEBUG                      source/input/fredebug.F       
Chd|        FREDEC2I                      source/output/message/fredec2i.F
Chd|        FREDEC3                       source/output/message/fredec3.F
Chd|        FREDLI                        source/input/fredli.F         
Chd|        FREDLI7                       source/input/fredli7.F        
Chd|        FREDYNAIN                     source/input/fredynain.F      
Chd|        FREEIG                        source/input/freeig.F         
Chd|        FREFLW                        source/input/freflw.F         
Chd|        FREFORM                       source/input/freform.F        
Chd|        FREFUNC                       source/input/frefunc.F        
Chd|        FREFVBAG                      source/input/frefvbag.F       
Chd|        FREFXINP                      source/input/frefxinp.F       
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|        FREINIV                       source/input/freiniv.F        
Chd|        FREINT                        source/input/freint.F         
Chd|        FRELNK                        source/input/frelnk.F         
Chd|        FRENOIS                       source/input/frenois.F        
Chd|        FREOUTP                       source/input/freoutp.F        
Chd|        FRERBO                        source/input/frerbo.F         
Chd|        FRESTAT                       source/input/frestat.F        
Chd|        FRETHERMAL                    source/constraints/thermic/frethermal.F
Chd|        FREUPWIND                     source/input/freupwind.F      
Chd|        FREUPWM                       source/input/freupwm.F        
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVRREST                       source/output/restart/rdresb.F
Chd|        FVVENT0                       source/airbag/fvvent0.F       
Chd|        FV_IMP0                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|        FXBSYS                        source/constraints/fxbody/fxbsys.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|        GET_FILE_NAME_INFO            source/system/get_file_name_info.F
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|        I10BUCE                       source/interfaces/intsort/i10buce.F
Chd|        I10LLL                        source/interfaces/int16/i10lagm.F
Chd|        I10MAINF                      source/interfaces/int10/i10mainf.F
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I10SMS2                       source/interfaces/int10/i10sms2.F
Chd|        I11ASS2                       source/interfaces/int11/i11ass3.F
Chd|        I11ASS25                      source/interfaces/int11/i11ass3.F
Chd|        I11BUCE_VOX                   source/interfaces/intsort/i11buce.F
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|        I11SMS2                       source/interfaces/int11/i11sms2.F
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|        I16LLL                        source/interfaces/int16/i16lagm.F
Chd|        I16TRI                        source/interfaces/int16/i16tri.F
Chd|        I17BUCE                       source/interfaces/int17/i17buce.F
Chd|        I17BUCE_PENA                  source/interfaces/int17/i17buce.F
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|        I17LLL4                       source/interfaces/int17/i17lagm.F
Chd|        I17LLL4_PENA                  source/interfaces/int17/i17for3.F
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|        I17TRI                        source/interfaces/int17/i17tri.F
Chd|        I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|        I20ASS2                       source/interfaces/int20/i20for3.F
Chd|        I20ASS25                      source/interfaces/int20/i20for3.F
Chd|        I20BUCE                       source/interfaces/intsort/i20buce.F
Chd|        I20BUC_EDGE                   source/interfaces/intsort/i20buce.F
Chd|        I20LLL                        source/interfaces/int16/i20lagm.F
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I20SMS2E                      source/interfaces/int20/i20sms2.F
Chd|        I21BUCE                       source/interfaces/intsort/i21buce.F
Chd|        I21LLL                        source/interfaces/int17/i21lagm.F
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|        I21SMS2                       source/interfaces/int21/i21sms2.F
Chd|        I22ASS2                       source/interfaces/int22/i22assembly.F
Chd|        I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|        I22INTERSECT                  source/interfaces/int22/i22intersect.F
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23BUCE                       source/interfaces/intsort/i23buce.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24ASS2                       source/interfaces/int24/i24for3.F
Chd|        I24CORP3                      source/interfaces/int24/i24cork3.F
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I24SMS2                       source/interfaces/int24/i24for3.F
Chd|        I25ASS2                       source/interfaces/int25/i25ass3.F
Chd|        I25ASSE2                      source/interfaces/int25/i25asse.F
Chd|        I25ASSE25                     source/interfaces/int25/i25asse.F
Chd|        I25ASSE25_E2S                 source/interfaces/int25/i25ass_e2s.F
Chd|        I25ASSE2_E2S                  source/interfaces/int25/i25ass_e2s.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I25SMS0E                      source/interfaces/int25/i25smse.F
Chd|        I25SMS2                       source/interfaces/int25/i25for3.F
Chd|        I25SMS2E                      source/interfaces/int25/i25smse.F
Chd|        I25SMS_E2S                    source/interfaces/int25/i25sms_e2s.F
Chd|        I2RUPT                        source/interfaces/interf/int2rupt.F
Chd|        I7ASS2                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS25                       source/interfaces/int07/i7ass3.F
Chd|        I7BUCE_VOX                    source/interfaces/intsort/i7buce.F
Chd|        I7LAGM                        source/interfaces/int07/i7lagm.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        I7SMS2                        source/interfaces/int07/i7sms2.F
Chd|        I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|        I8LLL                         source/interfaces/int16/i8lagm.F
Chd|        I9AVEL                        source/interfaces/int09/i9avel.F
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|        IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_INIST                     source/implicit/imp_pcg.F     
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PC_INV                    source/implicit/imp_pc_inv.F  
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|        IMP_UPDV2                     source/implicit/imp_pcg.F     
Chd|        IND_KTOT                      source/implicit/ind_glob_k.F  
Chd|        INIPAR                        source/mpi/init/inipar.F      
Chd|        INI_K0H                       source/implicit/imp_solv.F    
Chd|        INI_KISC                      source/implicit/lin_solv.F    
Chd|        INTER_COUNT_NODE_CURV         source/interfaces/generic/inter_count_node_curv.F
Chd|        JOINT_BLOCK_STIFFNESS         source/elements/joint/joint_block_stiffness.F
Chd|        KTBUF_INI                     source/implicit/imp_init.F    
Chd|        L2G_KLOC                      source/implicit/ind_glob_k.F  
Chd|        LAG_DIRECT                    source/tools/lagmul/lag_direct.F
Chd|        LAG_I2MAIN                    source/tools/lagmul/lag_i2main.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|        LAG_MULT_H                    source/tools/lagmul/lag_mult_h.F
Chd|        LAG_MULT_HP                   source/tools/lagmul/lag_mult_h.F
Chd|        LAG_MULT_SDP                  source/tools/lagmul/lag_mult_solv.F
Chd|        LAG_RWALL                     source/tools/lagmul/lag_rwall.F
Chd|        LECDAMP                       source/input/lecdamp.F        
Chd|        LECFLSW                       source/fluid/lecflsw.F        
Chd|        LECFUN                        source/tools/curve/lecfun.F   
Chd|        LECFVBAG                      source/input/lecfvbag.F       
Chd|        LECFXINP                      source/input/lecfxinp.F       
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        LECIMPL                       source/input/lectur.F         
Chd|        LECINV                        source/input/lecinv.F         
Chd|        LECNOISE                      source/general_controls/computation/lecnoise.F
Chd|        LECSTAT                       source/input/lecstat.F        
Chd|        LECTUR                        source/input/lectur.F         
Chd|        L_DIR02                       source/constraints/general/impvel/fv_imp0.F
Chd|        M13LAW                        source/materials/mat/mat013/m13law.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        MULTI_ALLOCATE                source/multifluid/multi_allocate.F
Chd|        MULTI_FLUXES_COMPUTATION      source/multifluid/multi_fluxes_computation.F
Chd|        MULTI_I18_FORCE_PON           source/interfaces/int18/multi_i18_force_pon.F
Chd|        MULTI_INLET_EBCS              source/multifluid/multi_inlet_ebcs.F
Chd|        MULTI_MUSCL_FLUXES_COMPUTATIONsource/multifluid/multi_muscl_fluxes_computation.F
Chd|        MULTI_MUSCL_GRADIENTS         source/multifluid/multi_muscl_gradients.F
Chd|        MULTI_NRF_EBCS                source/multifluid/multi_nrf_ebcs.F
Chd|        MULTI_SUBMATLAW               source/multifluid/multi_submatlaw.F
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        NDOF_FV                       source/implicit/ind_glob_k.F  
Chd|        PBLAST_1                      source/loads/pblast/pblast_1.F
Chd|        PBLAST_2                      source/loads/pblast/pblast_2.F
Chd|        PBLAST_3                      source/loads/pblast/pblast_3.F
Chd|        PHELPINFO                     source/engine/execargcheck.F  
Chd|        PREC_SOLV                     source/implicit/prec_solv.F   
Chd|        PROJECIG3D                    source/elements/ige3d/projecig3d.F
Chd|        QNORM2T                       source/multifluid/qnorm2t.F   
Chd|        QSTAT_INI                     source/implicit/imp_dyna.F    
Chd|        QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RBYACT                        source/constraints/general/rbody/rbyact.F
Chd|        RBY_DECOND                    source/tools/lagmul/lag_rby_cond.F
Chd|        RDELE                         source/input/rdele.F          
Chd|        RDRESA                        source/output/restart/rdresa.F
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_DYNAIN                   source/output/dynain/read_dynain.F
Chd|        READ_MSGFILE                  source/output/message/read_msgfile.F
Chd|        REALLOCATE_FI2                source/mpi/interfaces/spmd_i25slide.F
Chd|        REALLOCATE_I_SKYLINE          source/system/reallocate_skyline.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|        RGBODV                        source/constraints/general/rbody/rgbodv.F
Chd|        R_BUFBRIC_22                  source/interfaces/int22/r_bufbric_22.F
Chd|        S16DERI3                      source/elements/thickshell/solide16/s16deri3.F
Chd|        S20DERI3                      source/elements/solid/solide20/s20deri3.F
Chd|        S8DERI3                       source/elements/solid/solide8/s8deri3.F
Chd|        S8EDERIC3                     source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERICT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8SLKE3                       source/elements/solid/solide8s/s8slke3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERICT3                    source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERICTO3                   source/elements/solid/solide8z/s8zderi3.F
Chd|        SECT_READ                     source/tools/sect/sectio.F    
Chd|        SECT_READP                    source/tools/sect/sectio.F    
Chd|        SENSOR2                       source/tools/sensor/sensor2.F 
Chd|        SIGEPS53                      source/materials/mat/mat053/sigeps53.F
Chd|        SIGEPS75                      source/materials/mat/mat075/sigeps75.F
Chd|        SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F  
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|        SMS_INI_ERR                   source/ams/sms_init.F         
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SMS_INI_PART                  source/ams/sms_init.F         
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SNORM3T                       source/multifluid/snorm3t.F   
Chd|        SOLTOSPHA                     source/elements/sph/soltospha.F
Chd|        SOLTOSPHF                     source/elements/sph/soltosph.F
Chd|        SOLTOSPH_ON2                  source/elements/sph/soltosph_on2.F
Chd|        SPADAH                        source/elements/sph/spadah.F  
Chd|        SPBUC3                        source/elements/sph/spbuc3.F  
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPMD_CELL_EXCHANGE            source/mpi/generic/spmd_cell_exchange.F
Chd|        SPMD_CELL_SIZE_EXCHANGE       source/mpi/interfaces/spmd_cell_size_exchange.F
Chd|        SPMD_CHECK_TAG                source/mpi/ams/spmd_check_tag.F
Chd|        SPMD_EXCHM_SMS                source/mpi/ams/spmd_sms.F     
Chd|        SPMD_EXCH_FA                  source/mpi/elements/spmd_exch_dttsh.F
Chd|        SPMD_EXCH_I24                 source/mpi/interfaces/spmd_exch_i24.F
Chd|        SPMD_EXCH_I25                 source/mpi/interfaces/spmd_exch_i25.F
Chd|        SPMD_EXCH_MIN_MAX             source/mpi/ale/spmd_exch_min_max.F
Chd|        SPMD_EXCH_PRESS               source/mpi/interfaces/spmd_exch_press.F
Chd|        SPMD_EXCH_SMST2               source/mpi/ams/spmd_exch_smst2.F
Chd|        SPMD_EXCH_TAGNCONT            source/mpi/forces/spmd_exch_tagncont.F
Chd|        SPMD_EXCH_VMAX                source/mpi/elements/spmd_exch_dttsh.F
Chd|        SPMD_FIADD11_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20FE_PON            source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20F_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD25E_PON             source/mpi/interfaces/spmd_fiadd25e_pon.F
Chd|        SPMD_FIADD_PON                source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_GET_PENIS                source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_PENIS20              source/mpi/interfaces/send_cand.F
Chd|        SPMD_I17FROTS_PON             source/mpi/interfaces/spmd_i17frots_pon.F
Chd|        SPMD_I18KINE_COM_A            source/mpi/interfaces/spmd_i18kine_com_a.F
Chd|        SPMD_I18KINE_COM_ACC          source/mpi/interfaces/spmd_i18kine_com_acc.F
Chd|        SPMD_I18KINE_COM_MS           source/mpi/interfaces/spmd_i18kine_com_ms.F
Chd|        SPMD_I18KINE_COM_V            source/mpi/interfaces/spmd_i18kine_com_v.F
Chd|        SPMD_I18KINE_PENE_COM_POFF    source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|        SPMD_I20NORMF                 source/mpi/interfaces/spmd_i20normf.F
Chd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I21TEMPCOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
Chd|        SPMD_IFRONT_STAMP             source/mpi/interfaces/send_cand.F
Chd|        SPMD_INISPH                   source/mpi/sph/spmd_sptool.F  
Chd|        SPMD_INITFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_INT18_LAW151_PON         source/mpi/forces/spmd_int18_law151_pon.F
Chd|        SPMD_RST_CHECK                source/mpi/init/spmd_rst_check.F
Chd|        SPMD_SD_XV                    source/mpi/nodes/spmd_sd_xv.F 
Chd|        SPMD_SPHBOX                   source/mpi/elements/spmd_sph.F
Chd|        SPMD_SPHGAT                   source/mpi/sph/spmd_sphgat.F  
Chd|        SPMD_SPHGAT_old               source/mpi/sph/spmd_sphgat.F  
Chd|        SPMD_SPHVOX                   source/mpi/sph/spmd_sphvox.F  
Chd|        SPMD_SPHVOX_OLD               source/mpi/sph/spmd_sphvox.F  
Chd|        SPMD_TAGNCONT_STAMP           source/mpi/forces/spmd_exch_tagncont.F
Chd|        SPMD_TRI10BOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI10GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI17BOX                 source/mpi/interfaces/spmd_tri17box.F
Chd|        SPMD_TRI17GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|        SPMD_TRI18_151VOX             source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI20BOX                 source/mpi/interfaces/spmd_tri20box.F
Chd|        SPMD_TRI20BOXE                source/mpi/interfaces/spmd_tri20boxe.F
Chd|        SPMD_TRI20GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|        SPMD_TRI20GATE                source/mpi/interfaces/spmd_i7crit.F
Chd|        SPMD_TRI22VOX                 source/mpi/interfaces/spmd_tri22vox.F
Chd|        SPMD_TRI24GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI24VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI25GAT                 source/mpi/interfaces/spmd_tri25gat.F
Chd|        SPMD_TRI25VOX                 source/mpi/interfaces/spmd_tri25vox.F
Chd|        SPMD_TRI7GAT                  source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX                  source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|        SPONOF1                       source/elements/sph/sponof1.F 
Chd|        SP_A2                         source/implicit/imp_fsa_inv.F 
Chd|        SP_STATIC                     source/implicit/imp_fsa_inv.F 
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|        SWITCH_TO_DTNODA              source/time_step/switch_to_dtnoda.F
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_INTERP_LAW76            source/tools/curve/table_tools.F
Chd|        TABLE_RRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_RRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|        TRIBOX3                       stub/fvmbags_stub.F           
Chd|        TRITRI3                       stub/fvmbags_stub.F           
Chd|        UPDATE_SLIPRING               source/tools/seatbelts/update_slipring.F
Chd|        UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|        UPGRADE_REM_2RY               source/mpi/interfaces/spmd_i7tool.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|        WCIUSC2                       source/input/wciusc2.F        
Chd|        WRITE_INTBUF                  source/output/restart/write_intbuf.F
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        XANIM28                       source/elements/xelem/xanim28.F
Chd|        XFORC28                       source/elements/xelem/xforc28.F
Chd|        GET_U_FUNC_X                  source/user_interface/ufunc.F 
Chd|        GET_U_FUNC_Y                  source/user_interface/ufunc.F 
Chd|-- calls ---------------
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        DELETE_TMPFILE                source/output/tools/sortie_c.c
Chd|        DELETE_USER_FILE              source/user_interface/dyn_userlib.c
Chd|        ENGINE_USER_FINALIZE          source/user_interface/dyn_userlib.c
Chd|        MY_EXIT                       source/system/my_exit.c       
Chd|        PRINT_RUNTIME                 source/system/timer.F         
Chd|        REPORT                        source/output/report/report.F 
Chd|        SEND_IBUF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SORTIE_MVW                    source/output/report/sortie_mvw.F
Chd|        SPMD_KILL                     source/mpi/init/spmd_kill.F   
Chd|        DYNLIB_MOD                    share/modules/dynlib_mod.F    
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        SCRATCHFILE_MOD               share/modules/scratchfiles_mod.F
Chd|====================================================================
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
#include      "tlock_c.inc"
#include      "comlock.inc"
#include      "chara_c.inc"
#include      "scr20_c.inc"

#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "rad2r_c.inc"
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
                IF ((IRAD2R==1).AND.(ISPMD==0)) CALL SEND_IBUF_C(1,1)	    ! Rad2rad termination

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
