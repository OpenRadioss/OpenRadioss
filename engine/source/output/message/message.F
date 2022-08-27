Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  ANCMSG                        source/output/message/message.F
Chd|-- called by -----------
Chd|        ADMERR                        source/model/remesh/admerr.F  
Chd|        ADMINI                        source/model/remesh/admini.F  
Chd|        ALERO2                        source/ale/ale2d/alero2.F     
Chd|        ANIMX                         source/output/anim/generate/animx.F
Chd|        ANIM_BUILD_INDEX_ALL          source/output/anim/reader/anim_build_index_all.F
Chd|        ANIM_DCOD_KEY_0               source/output/anim/reader/anim_dcod_key_0.F
Chd|        ARRET_MESSAGE                 source/system/arret_message.F 
Chd|        ARRET_MESSAGE_SLID            source/system/arret_message.F 
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        BLERO2                        source/ale/bimat/blero2.F     
Chd|        CHKV0_CY                      source/constraints/general/bcs/bcscyc.F
Chd|        CP_IFRONT                     source/implicit/produt_v.F    
Chd|        CREATE_H3D_ARG_KEYWORD        source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
Chd|        DIM_INT7                      source/implicit/ind_glob_k.F  
Chd|        DSACTI1                       source/implicit/dsolve/dsacti.F
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|        DSRENUM                       source/implicit/dsolve/dsrenum.F
Chd|        DSRESOL                       source/implicit/dsolve/dsresol.F
Chd|        DSRESOL_ITER                  source/implicit/dsolve/dsresol.F
Chd|        DTNODA                        source/time_step/dtnoda.F     
Chd|        DTNODAMS                      source/time_step/dtnodams.F   
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_INI                      source/implicit/imp_dyna.F    
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        ERRMSG                        source/input/errmsg.F         
Chd|        ERROR_ALLOC                   source/system/error_alloc.F   
Chd|        ERR_MEM                       source/implicit/lin_solv.F    
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|        EULRO2                        source/ale/euler2d/eulro2.F   
Chd|        FACEPOLY                      source/airbag/fvmesh.F        
Chd|        FORINT                        source/elements/forint.F      
Chd|        FRALNK                        source/input/fralnk.F         
Chd|        FRALNK2                       source/input/fralnk.F         
Chd|        FREABF                        source/input/freabf.F         
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|        FREBCS                        source/input/frebcs.F         
Chd|        FRECPL                        source/input/frecpl.F         
Chd|        FREDAMP                       source/input/fredamp.F        
Chd|        FREDEBUG                      source/input/fredebug.F       
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
Chd|        FR_RLALE                      source/mpi/kinematic_conditions/fr_rlink1.F
Chd|        FR_RLINK1                     source/mpi/kinematic_conditions/fr_rlink1.F
Chd|        FVBAG1                        source/airbag/fvbag.F         
Chd|        FVRREST                       source/output/restart/rdresb.F
Chd|        FVVENT0                       source/airbag/fvvent0.F       
Chd|        FV_IMP0                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        GENSTAT                       source/output/sta/genstat.F   
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
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23BUCE                       source/interfaces/intsort/i23buce.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24ASS2                       source/interfaces/int24/i24for3.F
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
Chd|        IMP_FSA_INV2                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVP2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_KFILTR                    source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|        IND_KTOT                      source/implicit/ind_glob_k.F  
Chd|        INI_K0H                       source/implicit/imp_solv.F    
Chd|        INI_KISC                      source/implicit/lin_solv.F    
Chd|        INTER_COUNT_NODE_CURV         source/interfaces/generic/inter_count_node_curv.F
Chd|        IQELA2                        source/ale/iqela2.F           
Chd|        KTBUF_INI                     source/implicit/imp_init.F    
Chd|        L2G_KLOC                      source/implicit/ind_glob_k.F  
Chd|        LAG_DIRECT                    source/tools/lagmul/lag_direct.F
Chd|        LAG_I2MAIN                    source/tools/lagmul/lag_i2main.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|        LAG_MULT_H                    source/tools/lagmul/lag_mult_h.F
Chd|        LAG_MULT_HP                   source/tools/lagmul/lag_mult_h.F
Chd|        LAG_RWALL                     source/tools/lagmul/lag_rwall.F
Chd|        LCBCSF                        source/constraints/general/bcs/lcbcsf.F
Chd|        LECDAMP                       source/input/lecdamp.F        
Chd|        LECFLSW                       source/fluid/lecflsw.F        
Chd|        LECFUN                        source/tools/curve/lecfun.F   
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        LECIMPL                       source/input/lectur.F         
Chd|        LECINV                        source/input/lecinv.F         
Chd|        LECNOISE                      source/general_controls/computation/lecnoise.F
Chd|        LECSTAT                       source/input/lecstat.F        
Chd|        LECTUR                        source/input/lectur.F         
Chd|        L_DIR02                       source/constraints/general/impvel/fv_imp0.F
Chd|        M13LAW                        source/materials/mat/mat013/m13law.F
Chd|        M25CPLRC                      source/materials/mat/mat025/m25cplrc.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        MULTI_ALLOCATE                source/multifluid/multi_allocate.F
Chd|        MULTI_I18_FORCE_PON           source/interfaces/int18/multi_i18_force_pon.F
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|        NDOF_FV                       source/implicit/ind_glob_k.F  
Chd|        PROJECIG3D                    source/elements/ige3d/projecig3d.F
Chd|        QSTAT_INI                     source/implicit/imp_dyna.F    
Chd|        QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
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
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|        S16DERI3                      source/elements/thickshell/solide16/s16deri3.F
Chd|        S20DERI3                      source/elements/solid/solide20/s20deri3.F
Chd|        S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|        S8DERI3                       source/elements/solid/solide8/s8deri3.F
Chd|        S8EDERIC3                     source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERICM3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERICT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERICM3                    source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERICT3                    source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERICTO3                   source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCDERI3                       source/elements/thickshell/solidec/scderi3.F
Chd|        SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|        SCHKJABT3                     source/elements/solid/solide4/s4deri3.F
Chd|        SECT_INI                      source/tools/sect/sectio.F    
Chd|        SECT_READ                     source/tools/sect/sectio.F    
Chd|        SECT_READP                    source/tools/sect/sectio.F    
Chd|        SENSOR2                       source/tools/sensor/sensor2.F 
Chd|        SIGEPS53                      source/materials/mat/mat053/sigeps53.F
Chd|        SIGEPS75                      source/materials/mat/mat075/sigeps75.F
Chd|        SMALLGEO3                     source/elements/solid/solide/small3.F
Chd|        SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F  
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_FSA_INVH                  source/ams/sms_fsa_inv.F      
Chd|        SMS_INI_ERR                   source/ams/sms_init.F         
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SMS_INI_PART                  source/ams/sms_init.F         
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
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
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        STAT_INIMAP1D_FILE_SPMD       source/output/sta/stat_inimap1d_file_spmd.F
Chd|        STAT_INIMAP1D_SPMD            source/output/sta/stat_inimap1d_spmd.F
Chd|        STAT_INIMAP2D_FILE_SPMD       source/output/sta/stat_inimap2d_file_spmd.F
Chd|        STAT_INIMAP2D_SPMD            source/output/sta/stat_inimap2d_spmd.F
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|        SWITCH_TO_DTNODA              source/time_step/switch_to_dtnoda.F
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_INTERP_LAW76            source/tools/curve/table_tools.F
Chd|        TABLE_RRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_RRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        TRACE_PRINT                   source/system/trace_back.F    
Chd|        UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|        UPGRADE_REM_2RY               source/mpi/interfaces/spmd_i7tool.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|        WCIUSC2                       source/input/wciusc2.F        
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        XANIM28                       source/elements/xelem/xanim28.F
Chd|        XFORC28                       source/elements/xelem/xforc28.F
Chd|        GET_U_FUNC_X                  source/user_interface/ufunc.F 
Chd|        GET_U_FUNC_Y                  source/user_interface/ufunc.F 
Chd|        GET_U_NUMNOD                  source/user_interface/uaccess.F
Chd|        SYSFUS                        source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        MESSAGE_MOD2                  share/message_module/message_mod2.F
Chd|====================================================================
      SUBROUTINE ANCMSG(MSGID ,MSGTYPE,ANMODE,
     .                  I1    ,I2    ,I3    ,I4    ,
     .                  I5    ,I6    ,I7    ,I8    ,I9    ,
     .                  R1    ,R2    ,R3    ,R4    ,
     .                  R5    ,R6    ,R7    ,R8    ,R9   ,
     .                  C1    ,C2    ,C3    ,C4    ,
     .                  C5    ,C6    ,C7    ,C8    ,C9    )
      USE MESSAGE_MOD2 
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
#include      "nchara_c.inc"
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER             IBUF(10)
      my_real             RBUF(10)
      CHARACTER*ncharline CBUF(10)
C
      CHARACTER*ncharline TMPLINE,MYFMT,TMPBUF
      CHARACTER(LEN=NCHAROUT) ::  TMPOUT
      INTEGER ITYPE,ILINE,I,I0,J,J0,SBUFMSG(2),IFILE,IOLD,
     .        INDXI,INDXR,INDXC,INDXTMPOUT,BUFLEN,
     .        STMP
      CHARACTER*ncharline BUFMSG(2,100),BUFFMT(2,100)
C
      WRITE(ISTDO,'(/A,I10)')'MESSAGE ID : ',MSGID
      IF (IOUT.NE.0) THEN
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
!                 IF (TMPLINE(J0:J0).EQ.'/') THEN
!                   TMPLINE(J0:J0)=CHAR(10)
!                 END IF
!               END DO
              DO WHILE (I+1.LE.LEN_TRIM(TMPLINE))
                IF (TMPLINE(I:I).EQ.achar(92)) then !'\') THEN
                  I=I+1
                  IF (I-2.GE.1) THEN
                    WRITE(TMPBUF,'(A,A)')TMPLINE(IOLD:I-2),TMPLINE(I:I)
                    BUFLEN=I-2-IOLD+1+1
                  ELSE
                    WRITE(TMPBUF,'(A)')TMPLINE(I:I)
                    BUFLEN=1
                  END IF
                  I=I+1
                  IOLD=I
                ELSE IF (TMPLINE(I:I).EQ.'%') THEN
                  I=I+1
                  IF (I-2.GE.1) THEN
                    WRITE(TMPBUF,'(A)')TMPLINE(IOLD:I-2)
                    BUFLEN=I-2-IOLD+1
                    IF (BUFLEN.GT.0) THEN
                      TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN) 
                      INDXTMPOUT=INDXTMPOUT+BUFLEN
                      BUFLEN=0
                    END IF
                  END IF
                  IF (TMPLINE(I:I).EQ.'d') THEN 
                    I=I+1
                    IOLD=I
                    MYFMT='(I10)'
                    IF (INDXI.LT.10) INDXI=INDXI+1
                    WRITE(TMPBUF,MYFMT)IBUF(INDXI)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  ELSE IF (TMPLINE(I:I).EQ.'f') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(1PG20.13)'
                    IF (INDXR.LT.10) INDXR=INDXR+1
                    WRITE(TMPBUF,MYFMT)RBUF(INDXR)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  ELSE IF (TMPLINE(I:I).EQ.'s') THEN
                    I=I+1
                    IOLD=I
                    MYFMT='(A)'
                    IF (INDXC.LT.10) INDXC=INDXC+1 
                    WRITE(TMPBUF,MYFMT)CBUF(INDXC)
                    TMPBUF=ADJUSTL(TMPBUF)
                    BUFLEN=LEN_TRIM(TMPBUF)
                  END IF
                ELSE
                  I=I+1
                END IF
                IF (BUFLEN.GT.0) THEN
                  TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
              END DO
              IF (IOLD.LE.I) THEN
                WRITE(TMPBUF,'(A)')
     .          TMPLINE(IOLD:LEN_TRIM(TMPLINE))
                BUFLEN=LEN_TRIM(TMPLINE)-IOLD+1
                IF (BUFLEN.GT.0) THEN
                  TMPOUT=TMPOUT(1:INDXTMPOUT)//TMPBUF(1:BUFLEN)
                  INDXTMPOUT=INDXTMPOUT+BUFLEN
                  BUFLEN=0
                END IF
              END IF
! #include "lockon.inc"
              IF (INDXTMPOUT.GT.0) THEN
C               do not write description on stdout in case ANINFO_BLIND
                IF (ANMODE.NE.ANINFO_BLIND.OR.ITYPE.EQ.1) THEN
                  WRITE(ISTDO,'(A)')TMPOUT(1:INDXTMPOUT)
                END IF
                IF (IOUT.NE.0) THEN
                  WRITE(IOUT,'(A)')TMPOUT(1:INDXTMPOUT)
                END IF
              END IF
! #include "lockoff.inc"
            END DO
          END IF
        END DO
      RETURN
      END SUBROUTINE





