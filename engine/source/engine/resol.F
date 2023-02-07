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
Chd|  RESOL                         source/engine/resol.F         
Chd|-- called by -----------
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|-- calls ---------------
Chd|        ACCDTDC                       source/elements/thickshell/solidec/scdtdc3.F
Chd|        ACCEL1                        source/tools/accele/accel1.F  
Chd|        ACCELE                        source/assembly/accele.F      
Chd|        ACCELEPINCH                   source/assembly/accelepinch.F 
Chd|        ADD_ELAPSED_TIME_MON_OFF      source/system/timer.F         
Chd|        ADMDIV                        source/model/remesh/admdiv.F  
Chd|        ADMERR                        source/model/remesh/admerr.F  
Chd|        ADMFOR0                       source/model/remesh/admfor0.F 
Chd|        ADMGVID                       source/model/remesh/admgvid.F 
Chd|        ADMORDR                       source/model/remesh/admordr.F 
Chd|        ADMREGUL                      source/model/remesh/admregul.F
Chd|        ADMVIT                        source/model/remesh/admvit.F  
Chd|        AGAUGE0                       source/ale/agauge.F           
Chd|        ALEFVM_ACCELE                 source/ale/alefvm/alefvm_accele.F
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|        ALELAG_WREST                  source/output/restart/wrrest.F
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ALEMUSCL_DEALLOCATE           source/ale/alemuscl/alemuscl_deallocate.F
Chd|        ALESUB2                       source/ale/subcycling/alesub2.F
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|        ALLOC_ELBUF_IMP               source/elements/elbuf/alloc_elbuf_imp.F
Chd|        AMS_PREPARE_POFF_ASSEMBLY     source/mpi/ams/spmd_sms.F     
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ASSINIT                       source/assembly/assadd2.F     
Chd|        ASSINIT_CRKXFEM               source/assembly/assadd2.F     
Chd|        ASSINIT_PXFEM                 source/assembly/assadd2.F     
Chd|        ASSPAR                        source/assembly/asspar.F      
Chd|        ASSPAR3                       source/assembly/asspar3.F     
Chd|        ASSPAR4                       source/assembly/asspar4.F     
Chd|        ASSPAR5                       source/assembly/asspar5.F     
Chd|        ASSPART                       source/assembly/asspart.F     
Chd|        ASSPARXX                      source/assembly/assparxx.F    
Chd|        ASSPAR_CRK                    source/elements/xfem/asspar_crk.F
Chd|        ASSPAR_SUB                    source/assembly/asspar_sub.F  
Chd|        ASSPAR_SUB_POFF               source/assembly/asspar_sub_poff.F
Chd|        BCS10                         source/constraints/general/bcs/bcs10.F
Chd|        BCSCYC                        source/constraints/general/bcs/bcscyc.F
Chd|        BCSDTTH_COPY                  source/constraints/general/bcs/bcsdtth.F
Chd|        BCSN                          source/constraints/general/bcs/bcsn.F
Chd|        BMULTN                        source/ale/bimat/bmultn.F     
Chd|        CFIELD_1                      source/loads/general/load_centri/cfield.F
Chd|        CHECK_EDGE_STATE              source/interfaces/interf/check_edge_state.F
Chd|        CHECK_NAN_ACC                 source/output/outfile/check_nan_acc.F
Chd|        CHECK_NODAL_STATE             source/interfaces/interf/check_nodal_state.F
Chd|        CHECK_SURFACE_STATE           source/interfaces/interf/check_surface_state.F
Chd|        CHKLOAD                       source/interfaces/chkload.F   
Chd|        CHKMSIN                       source/output/outfile/chkmsin.F
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|        CHKSTIFN                      source/interfaces/inter2d/chkstifn.F
Chd|        CJOINT                        source/constraints/general/cyl_joint/cjoint.F
Chd|        CLOSE_C                       source/output/tools/sortie_c.c
Chd|        CLOSE_SOCK_C                  source/coupling/rad2rad/rad2rad_c.c
Chd|        CLUSTERF                      source/output/cluster/clusterf.F
Chd|        CNDINT                        source/model/remesh/cndint.F  
Chd|        CNDORDR                       source/model/remesh/cndordr.F 
Chd|        CND_DMASI2                    source/elements/solid/solide10/s10cndf.F
Chd|        COLLECT                       source/output/outfile/collect.F
Chd|        COLLECTM                      source/output/outfile/collect.F
Chd|        COLLECTT                      source/output/outfile/collect.F
Chd|        CONVEC                        source/constraints/thermic/convec.F
Chd|        CONVWREST                     source/output/restart/wrrest.F
Chd|        CP_DM                         source/implicit/produt_v.F    
Chd|        CRKXFEM_WREST                 source/output/restart/wrrest.F
Chd|        CRKXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|        CRK_ACCELE                    source/elements/xfem/accele_crk.F
Chd|        CRK_COORD_INI                 source/elements/xfem/inixfem.F
Chd|        CRK_VITESSE                   source/elements/xfem/crk_vitesse.F
Chd|        CRK_VITESSE2                  source/elements/xfem/crk_vitesse2.F
Chd|        CRK_ZERO_ACCEL                source/elements/xfem/crk_vitesse.F
Chd|        CRK_ZERO_FSKY                 source/elements/xfem/crk_vitesse.F
Chd|        CUMULTIME_MP                  source/system/timer.F         
Chd|        DAMPING                       source/assembly/damping.F     
Chd|        DAMPING44                     source/assembly/damping.F     
Chd|        DAMPING51                     source/assembly/damping.F     
Chd|        DEALLOCATE_JOINT              source/constraints/general/cyl_joint/deallocate_joint.F
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        DEALLOC_IMPBUF                source/implicit/imp_solv.F    
Chd|        DEALLOC_SHOOT_INTER           source/interfaces/interf/dealloc_shoot_inter.F
Chd|        DEPLA                         source/assembly/depla.F       
Chd|        DEPLAFAKEIGE                  source/assembly/deplafakeige.F
Chd|        DEPLAPINCH                    source/assembly/deplapinch.F  
Chd|        DESACTI                       source/elements/desacti.F     
Chd|        DRAPE_WREST                   source/output/restart/wrrest.F
Chd|        DTNODA                        source/time_step/dtnoda.F     
Chd|        DTNODAMP                      source/time_step/dtnodamp.F   
Chd|        DTNODAMS                      source/time_step/dtnodams.F   
Chd|        DTNODARAYL                    source/time_step/dtnodarayl.F 
Chd|        EBCCLAP                       source/boundary_conditions/ebcs/ebcclap.F
Chd|        EBCS_EXTRAPOL                 source/boundary_conditions/ebcs/ebcs_extrapol.F
Chd|        ELAPSTIME                     source/system/timer.F         
Chd|        ENER_W0                       source/general_controls/damping/static.F
Chd|        ENGINE_USER_CHECK             source/user_interface/dyn_userlib.c
Chd|        ENGINE_USER_INITIALIZE        source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_USERWI            source/user_interface/dyn_userlib.c
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|        EULDX                         source/ale/grid/euldx.F       
Chd|        FAIL_WIND_FRWAVE_INIT         source/materials/fail/alter/fail_wind_frwave_init.F
Chd|        FEQUILIBRE                    source/elements/shell/fequilibre.F
Chd|        FINALIZE_REACTIONS            source/output/reactions.F     
Chd|        FIND_DT_FOR_TARGETED_ADDED_MASSsource/time_step/find_dt_for_targeted_added_mass.F
Chd|        FIND_EDGE_INTER               source/interfaces/interf/find_edge_inter.F
Chd|        FIND_SURFACE_INTER            source/interfaces/interf/find_surface_inter.F
Chd|        FIXFINGEO                     source/constraints/general/impvel/fixfingeo.F
Chd|        FIXFLUX                       source/constraints/thermic/fixflux.F
Chd|        FIXTEMP                       source/constraints/thermic/fixtemp.F
Chd|        FIXVEL                        source/constraints/general/impvel/fixvel.F
Chd|        FLOW0                         source/fluid/flow0.F          
Chd|        FLOW1                         source/fluid/flow1.F          
Chd|        FLOW_ACCELE                   source/assembly/flow_accele.F 
Chd|        FLOW_DEPLA                    source/assembly/flow_depla.F  
Chd|        FLOW_VITESSE                  source/assembly/flow_vitesse.F
Chd|        FORANI1                       source/output/anim/generate/forani1.F
Chd|        FORANI2                       source/output/anim/generate/forani2.F
Chd|        FORANI3                       source/output/anim/generate/forani3.F
Chd|        FORCE                         source/loads/general/force.F  
Chd|        FORCEFINGEO                   source/loads/general/forcefingeo.F
Chd|        FORCEPINCH                    source/loads/general/forcepinch.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        FORINTS                       source/elements/forints.F     
Chd|        FREE_PINCH                    ../common_source/modules/pinchtype_mod.F
Chd|        FVBAG0                        source/airbag/fvbag0.F        
Chd|        FVCOPY                        source/airbag/fvcopy.F        
Chd|        FVDIM                         source/airbag/fvdim.F         
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        FVREZONE0                     source/airbag/fvrezone.F      
Chd|        FVSTATS                       source/airbag/fvstats.F       
Chd|        FVUPD0                        source/airbag/fvupd.F         
Chd|        FVWREST                       source/output/restart/wrrest.F
Chd|        FV_SWITCH_CRIT                source/airbag/fvdim.F         
Chd|        FXBYFOR                       source/constraints/fxbody/fxbyfor.F
Chd|        FXBYPID                       source/constraints/fxbody/fxbypid.F
Chd|        FXBYVIT                       source/constraints/fxbody/fxbyvit.F
Chd|        FXFLUXWREST                   source/constraints/thermic/fxfluxwrest.F
Chd|        FXGRVCOR                      source/constraints/fxbody/fxgrvcor.F
Chd|        FXTEMPWREST                   source/output/restart/wrrest.F
Chd|        GET_FBUFDP_C                  source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_FBUF_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_IBUF_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|        GET_SHMBUF_C                  source/coupling/rad2rad/rad2rad_c.c
Chd|        GRAVIT                        source/loads/general/grav/gravit.F
Chd|        GRAVIT_FVM_FEM                source/loads/general/grav/gravit_fvm_fem.F
Chd|        I14IST                        source/interfaces/int14/i14ist.F
Chd|        I14WFS                        source/interfaces/int14/i14wfs.F
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|        I24E2E_FICTIVE_NODES_UPDATE   source/interfaces/int24/i24for3e.F
Chd|        I24NITSCHFOR3                 source/interfaces/int24/i24nitschfor3.F
Chd|        I24PXFEM                      source/interfaces/int24/i24pxfem.F
Chd|        IMPWREST                      source/output/restart/wrrest.F
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_DT2                       source/implicit/imp_dt.F      
Chd|        IMP_FANIE                     source/implicit/imp_solv.F    
Chd|        IMP_FANII                     source/implicit/imp_solv.F    
Chd|        IMP_FOUT                      source/implicit/imp_solv.F    
Chd|        IMP_RESTARCP                  source/implicit/imp_sol_init.F
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        IMP_TRANS                     source/output/restart/wrrest.F
Chd|        INIT_INTERF_SORTING_STRATEGY  source/interfaces/init_interf_sorting_strategy.F
Chd|        INIT_NODAL_STATE              source/interfaces/interf/init_nodal_state.F
Chd|        INIT_TRIM                     source/interfaces/intsort/init_trim.F
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|        INT18_LAW151_ALLOC            source/interfaces/int18/int18_law151_alloc.F
Chd|        INT18_LAW151_INIT             source/interfaces/int18/int18_law151_init.F
Chd|        INT18_LAW151_OMP_ACCUMULATION source/interfaces/int18/int18_law151_omp_accumulation.F
Chd|        INT18_LAW151_UPDATE           source/interfaces/int18/int18_law151_update.F
Chd|        INTAL1                        source/ale/inter/intal1.F     
Chd|        INTER_STRUCT_INIT             source/interfaces/generic/inter_struct_init.F
Chd|        INTFOP1                       source/interfaces/interf/intfop1.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTFOP8                       source/interfaces/interf/intfop8.F
Chd|        INTSTAMP_ASS                  source/interfaces/int21/intstamp_ass.F
Chd|        INTSTAMP_DT                   source/interfaces/int21/intstamp_dt.F
Chd|        INTSTAMP_INIT                 source/interfaces/int21/intstamp_init.F
Chd|        INTSTAMP_MOVE                 source/interfaces/int21/intstamp_move.F
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|        INTTI2                        source/interfaces/interf/intti2.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        JOINT_BLOCK_STIFFNESS         source/elements/joint/joint_block_stiffness.F
Chd|        JOINT_ELEM_TIMESTEP           source/elements/joint/joint_elem_timestep.F
Chd|        KINE_SEATBELT_FORCE           source/tools/seatbelts/kine_seatbelt_force.F
Chd|        KINE_SEATBELT_VEL             source/tools/seatbelts/kine_seatbelt_vel.F
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|        LOAD_PRESSURE                 source/loads/general/load_pressure/load_pressure.F
Chd|        MANCTR                        source/input/manctr.F         
Chd|        MODSTI                        source/time_step/modsti.F     
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|        MOVFRA1                       source/tools/skew/movfram.F   
Chd|        MOVFRA2                       source/tools/skew/movfram.F   
Chd|        MOVFRA_IMP                    source/tools/skew/movfram.F   
Chd|        MPI_MIN_REAL_BEGIN            share/modules/mpi_tools_mod.F 
Chd|        MPI_MIN_REAL_END              share/modules/mpi_tools_mod.F 
Chd|        MULTI_DEALLOCATE              source/multifluid/multi_deallocate.F
Chd|        MVOLUDT                       source/airbag/mvoludt.F       
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        NEED_FOR_REACTIONS            source/output/reactions.F     
Chd|        NEWSKW                        source/tools/skew/newskw.F    
Chd|        NEWSKW_INIT                   source/tools/skew/newskw.F    
Chd|        NFWREST                       source/output/restart/wrrest.F
Chd|        NITSCHEWREST                  source/output/restart/wrrest.F
Chd|        NLOCAL_ACC                    source/assembly/nlocal_acc.F  
Chd|        NLOCAL_DTNODA                 source/time_step/nlocal_dtnoda.F
Chd|        NLOCAL_INCR                   source/assembly/nlocal_incr.F 
Chd|        NLOCAL_VEL                    source/assembly/nlocal_vel.F  
Chd|        PBLAST                        source/loads/pblast/pblast.F  
Chd|        PBLAST_DEALLOCATE             ../common_source/modules/pblast_mod.F
Chd|        PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|        PLYXFEM_WRAVUPLY              source/output/restart/wrrest.F
Chd|        PLYXFEM_WREST                 source/output/restart/wrrest.F
Chd|        PLYXFEM_WRESTANIM             source/output/restart/wrrest.F
Chd|        PLY_ACCELE                    source/assembly/ply_accele.F  
Chd|        PLY_BCS                       source/constraints/general/bcs/ply_bcs.F
Chd|        PLY_INFO_WREST                source/output/restart/wrrest.F
Chd|        PLY_VITESSE                   source/assembly/ply_vitesse.F 
Chd|        PORO                          source/ale/porous/poro.F      
Chd|        PREPARE_DEBUG                 share/modules/debug_mod.F     
Chd|        PRERBE3P0                     source/constraints/general/rbe3/rbe3f.F
Chd|        PRINTIME                      source/system/timer.F         
Chd|        PRINTIMEG                     source/system/timer.F         
Chd|        PRINTIME_INTERF               source/system/timer_interf.F  
Chd|        R2R_BLOCK_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        R2R_EXCHANGE                  source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|        R2R_SEM_C                     source/coupling/rad2rad/rad2rad_c.c
Chd|        R2R_SENDKINE                  source/coupling/rad2rad/r2r_getdata.F
Chd|        R2R_UNLOCK_THREADS_C          source/coupling/rad2rad/rad2rad_c.c
Chd|        RADIATION                     source/constraints/thermic/radiation.F
Chd|        RADIAWREST                    source/output/restart/wrrest.F
Chd|        RBAGDT                        source/airbag/rbagdt.F        
Chd|        RBE2COR                       source/constraints/general/rbody/rgbcor.F
Chd|        RBE2T1                        source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2V                         source/constraints/general/rbe2/rbe2v.F
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|        RBE3V                         source/constraints/general/rbe3/rbe3v.F
Chd|        RBYCOR                        source/constraints/general/rbody/rbycor.F
Chd|        RBYFOR                        source/constraints/general/rbody/rbyfor.F
Chd|        RBYSENS                       source/constraints/general/rbody/rbyonf.F
Chd|        RBYVIT                        source/constraints/general/rbody/rbyvit.F
Chd|        RE2INT5                       source/implicit/imp_solv.F    
Chd|        RE2INT7                       source/implicit/imp_solv.F    
Chd|        REALLOCATE_I_SKYLINE          source/system/reallocate_skyline.F
Chd|        RENUM_SIZ                     source/interfaces/interf/renum_siz.F
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        RGWAL0                        source/constraints/general/rwall/rgwal0.F
Chd|        RGWALF                        source/constraints/general/rwall/rgwal0.F
Chd|        RIGMATWREST                   source/output/restart/wrrest.F
Chd|        RIVET1                        source/elements/rivet/rivet1.F
Chd|        RLINK10                       source/constraints/general/rlink/rlink10.F
Chd|        RLINK11                       source/constraints/general/rlink/rlink10.F
Chd|        RMATACCE                      source/materials/mat/mat013/rmatacce.F
Chd|        RMATFORP                      source/materials/mat/mat013/rmatforp.F
Chd|        RTHBUFWREST                   source/output/restart/wrrest.F
Chd|        S10CNDF1                      source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDF2                      source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDFND                     source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDI2A                     source/elements/solid/solide10/s10cndv.F
Chd|        S10CNDI2A1                    source/elements/solid/solide10/s10cndv.F
Chd|        S10CNDV                       source/elements/solid/solide10/s10cndv.F
Chd|        S10GETVDM                     source/elements/solid/solide10/s10cndv.F
Chd|        S10STFE_POFF                  source/elements/solid/solide10/s10cndf.F
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|        SAV_BUF_POINT                 source/user_interface/eng_callback_c.c
Chd|        SECT_FIO                      source/tools/sect/sectio.F    
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|        SEND_FBUFDP_C                 source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_FBUF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SEND_IBUF_C                   source/coupling/rad2rad/rad2rad_c.c
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|        SENSOR_DIST_SURF0             source/tools/sensor/sensor_dist_surf0.F
Chd|        SENSOR_ENER_SAV               source/tools/sensor/sensor_ener_sav.F
Chd|        SENSOR_INIT                   source/tools/sensor/sensor_init.F
Chd|        SENSOR_LOGICAL                source/tools/sensor/sensor_logical.F
Chd|        SENSOR_SPMD                   source/tools/sensor/sensor_spmd.F
Chd|        SENSOR_TEMP0                  source/tools/sensor/sensor_temp0.F
Chd|        SHSUB0                        source/elements/shell/subcycling/shsub0.F
Chd|        SHSUB1                        source/elements/shell/subcycling/shsub1.F
Chd|        SHSUB2                        source/elements/shell/subcycling/shsub2.F
Chd|        SHSUB3                        source/elements/shell/subcycling/shsub3.F
Chd|        SMP_INIT                      source/engine/resol_init.F    
Chd|        SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F  
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_INI_ERR                   source/ams/sms_init.F         
Chd|        SMS_INI_INT                   source/ams/sms_init.F         
Chd|        SMS_INI_JAD_1                 source/ams/sms_init.F         
Chd|        SMS_INI_JAD_2                 source/ams/sms_init.F         
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SMS_INI_KAD                   source/ams/sms_init.F         
Chd|        SMS_INI_KDI                   source/ams/sms_init.F         
Chd|        SMS_INI_KIN_2                 source/ams/sms_init.F         
Chd|        SMS_INI_PART                  source/ams/sms_init.F         
Chd|        SMS_INI_RBY                   source/ams/sms_init.F         
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SOLTOSPHA                     source/elements/sph/soltospha.F
Chd|        SOLTOSPHF                     source/elements/sph/soltosph.F
Chd|        SORTIE_ERROR                  source/output/sortie_error.F  
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        SPGAUGE_F                     source/elements/sph/spgauge.F 
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|        SPHRES44B                     source/elements/sph/sphres44b.F
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPLIT_ASSPAR4                 source/assembly/split_asspar4.F
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_BARRIER                  source/mpi/generic/spmd_barrier.F
Chd|        SPMD_CHKW                     source/mpi/generic/spmd_chkw.F
Chd|        SPMD_COLLECT                  source/mpi/output/spmd_collect.F
Chd|        SPMD_COLLECTM                 source/mpi/output/spmd_collectm.F
Chd|        SPMD_COLLECTT                 source/mpi/output/spmd_collectt.F
Chd|        SPMD_COLLECT_MULTI_FVM        source/mpi/output/spmd_collect_multi_fvm.F
Chd|        SPMD_COLLECT_NLOCAL           source/mpi/output/spmd_collect_nlocal.F
Chd|        SPMD_CRK_ADV                  source/elements/xfem/xfemfsky.F
Chd|        SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|        SPMD_EXCH_A                   source/mpi/forces/spmd_exch_a.F
Chd|        SPMD_EXCH_A_AMS_POFF          source/mpi/forces/spmd_exch_a_ams_poff.F
Chd|        SPMD_EXCH_CRKVEL              source/mpi/elements/spmd_xfem.F
Chd|        SPMD_EXCH_DA20                source/mpi/interfaces/spmd_exch_da20.F
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|        SPMD_EXCH_FA                  source/mpi/elements/spmd_exch_dttsh.F
Chd|        SPMD_EXCH_I24                 source/mpi/interfaces/spmd_exch_i24.F
Chd|        SPMD_EXCH_I25                 source/mpi/interfaces/spmd_exch_i25.F
Chd|        SPMD_EXCH_ICODT               source/mpi/nodes/spmd_exch_icodt.F
Chd|        SPMD_EXCH_ICONT               source/mpi/nodes/spmd_exch_icont.F
Chd|        SPMD_EXCH_PRESS               source/mpi/interfaces/spmd_exch_press.F
Chd|        SPMD_EXCH_R2R_SPH             source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_EXCH_R2R_SPHOFF          source/mpi/r2r/spmd_r2r.F     
Chd|        SPMD_EXCH_SEC                 source/mpi/sections/spmd_section.F
Chd|        SPMD_EXCH_SUB_POFF            source/mpi/spmd_exch_sub.F    
Chd|        SPMD_EXCH_SUB_PON             source/mpi/spmd_exch_sub.F    
Chd|        SPMD_EXCH_TAGNCONT            source/mpi/forces/spmd_exch_tagncont.F
Chd|        SPMD_EXCH_THKNOD              source/mpi/nodes/spmd_exch_thknod.F
Chd|        SPMD_EXCH_USERWI              source/mpi/user_interface/spmd_exch_userwi.F
Chd|        SPMD_EXCH_VMAX                source/mpi/elements/spmd_exch_dttsh.F
Chd|        SPMD_EXCH_WAVE                source/mpi/nodes/spmd_exch_wave.F
Chd|        SPMD_EXSUM_FB6                source/mpi/generic/spmd_exsum_fb6.F
Chd|        SPMD_FLUSH_ACCEL              source/mpi/output/spmd_flush_accel.F
Chd|        SPMD_FVB_SWITCH               source/mpi/airbags/spmd_fvb_switch.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_MIN5                source/mpi/generic/spmd_glob_min5.F
Chd|        SPMD_GLOB_MINV                source/mpi/generic/spmd_glob_minv.F
Chd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I21TEMPCOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I24_PREPARE              source/interfaces/int24/i24_prepare.F
Chd|        SPMD_I25FRONT_INIT            source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I25_PREPARE              source/interfaces/int25/i25_prepare.F
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_INT18_LAW151_PON         source/mpi/forces/spmd_int18_law151_pon.F
Chd|        SPMD_MAX_XFE_I                source/mpi/elements/spmd_xfem.F
Chd|        SPMD_MUMPS_INI                source/mpi/implicit/imp_spmd.F
Chd|        SPMD_NLIST_SMS                source/mpi/ams/spmd_sms.F     
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        SPMD_SD_XV                    source/mpi/nodes/spmd_sd_xv.F 
Chd|        SPMD_SORT_SMS                 source/mpi/ams/spmd_sms.F     
Chd|        SPMD_WIOUT                    source/mpi/generic/spmd_wiout.F
Chd|        SPONFV                        source/elements/sph/sponfv.F  
Chd|        SPWFVIS                       source/elements/sph/spwfvis.F 
Chd|        SRFVIT                        source/constraints/general/rbody/srfvit.F
Chd|        STACK_WREST                   source/output/restart/wrrest.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STATIC                        source/general_controls/damping/static.F
Chd|        STOPTIME                      source/system/timer.F         
Chd|        STORE_ACCEL_FIELD             source/output/reactions.F     
Chd|        STORE_ACCEL_FIELD_TH          source/output/store_accel_field_th.F
Chd|        STORE_FORCES                  source/output/reactions.F     
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|        TEMPUR                        source/constraints/thermic/tempur.F
Chd|        THBCS                         source/output/th/thbcs.F      
Chd|        THBCS_IMP                     source/output/th/thbcs_imp.F  
Chd|        THCWREST                      source/output/restart/wrrest.F
Chd|        THERMBILAN                    source/constraints/thermic/thermbilan.F
Chd|        THICKVAR                      source/elements/shell/coque/thickvar.F
Chd|        TIMFUN                        source/tools/curve/timfun.F   
Chd|        TRACE_IN                      source/system/trace_back.F    
Chd|        TRACE_OUT                     source/system/trace_back.F    
Chd|        UPDATE_FAILWAVE               source/materials/fail/failwave/update_failwave.F
Chd|        UPDATE_SLIPRING               source/tools/seatbelts/update_slipring.F
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|        UPENR_CRK                     source/elements/xfem/upenr_crk.F
Chd|        UPGRADE_REM_2RY               source/mpi/interfaces/spmd_i7tool.F
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|        VITESSE                       source/assembly/vitesse.F     
Chd|        VITESSEPINCH                  source/assembly/vitessepinch.F
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_DPDB                    source/output/tools/write_db.F
Chd|        WRITE_UNITS                   ../common_source/comm/write_units.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|        ZERO1                         source/system/zero.F          
Chd|        ZEROR                         source/system/zero.F          
Chd|        ACCELERATION_DP_MOD           share/modules/acceleration_dp_mod.F
Chd|        ALEFLOW_MOD                   share/modules/aleflow_mod.F   
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALEMUSCL_MOD                  ../common_source/modules/alemuscl_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DIFFUSION_MOD                 share/modules/diffusion_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        DTDC_MOD                      share/modules/dtdc_mod.F      
Chd|        DYNLIB_MOD                    share/modules/dynlib_mod.F    
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ECND_MOD                      share/modules/ecdn_mod.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        ERROR_MOD                     share/modules/error_mod.F     
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        FSKYI_MOD                     share/modules/restart_mod.F   
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        HEAT_MOD                      share/modules/heat_mod.F      
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        ICONTACT_MOD                  share/modules/icontact_mod.F  
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MDS_RESTART_MOD               ../common_source/modules/mds_rest.mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|        MPI_TOOLS_MOD                 share/modules/mpi_tools_mod.F 
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|        RESOLSAV_MOD                  share/modules/resol_mod.F     
Chd|        RIGMAT_MOD                    share/modules/rigmat_mod.F    
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|        SMS_MOD                       share/modules/sms_mod.F       
Chd|        SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|        THGRELEM_MOD                  share/modules/thgrelem_mod.F  
Chd|        THK_MOD                       share/modules/thk_mod.F       
Chd|        THREAC_MOD                    share/modules/threac_mod.F    
Chd|        TIME_MOD                      share/modules/time_mod.F      
Chd|        UNITS_MOD                     share/modules/units_mod.F     
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|        XFEM2VARS_MOD                 share/modules/xfem2vars_mod.F 
Chd|        i22EDGE_MOD                   ../common_source/modules/cut-cell-buffer_mod.F
Chd|====================================================================
      SUBROUTINE RESOL(AF,IAF,
     1  ICODE  ,ISKEW  ,ISKWN   ,NETH   ,IPART  ,NOM_OPT,KXX    ,IXX    ,
     2  IXUR   ,IXTG   ,IXS     ,IXQ    ,IXC    ,IXT    ,IXP    ,IXR    ,
     3          ITAB   ,ITABM1  ,IFILL  ,
     4  IMS    ,NPC    ,IBCL    ,MATPARAM_TAB   ,GROUP_PARAM_TAB,
     5  IBFV   ,IDUM   ,LAS     ,LACCELM,NNLINK ,LNLINK ,
     6  IUSER  ,IPARG  ,DD_IAD  ,IGRV   ,IEXLNK ,KINET  ,
     7  IPARI  ,        NPRW    ,ICONX  ,NPBY   ,
     8  LPBY   ,LRIVET ,NSTRF   ,LJOINT ,NODPOR ,MONVOL ,ITHGRP,IAD_ELEM,
     9  FR_ELEM,WEIGHT,MAIN_PROC,ICODT,ICODR, ADSKY  ,IADS   ,ILINK  ,
     A  LLINK  ,LINALE ,NEFLSW  ,NNFLSW ,ICUT   ,CLUSTER,
     B          ITASK  ,INOISE  ,
     C  X      ,D      ,V       ,VR     ,DR     ,THKE   ,DAMP   ,MS     ,
     D  IN     ,PM     ,SKEW    ,GEO    ,EANI   ,BUFMAT ,BUFGEO ,BUFSF  ,
     E  W      ,VEUL   ,FILL    ,DFILL  ,ALPH   ,WB     ,DSAVE  ,ASAVE  ,
     .  MSNF   ,
     F  TF     ,FORC   ,VEL     ,FSAV   ,FZERO  ,XLAS   ,ACCELM ,
     G  AGRV   ,FR_WAVE,FAILWAVE,PARTS0 ,USER   ,ELBUF  ,RWBUF  ,SENSOR_TAB,
     H  RWSAV  ,RBY     ,RIVET  ,SECBUF ,VOLMON ,LAMBDA ,
     I  WA     ,FV     ,A       ,AR     ,STIFN  ,STIFR  ,PARTSAV,FSKY
     J         ,UWA    ,VAL2    ,PHI    ,SEGVAR ,R      ,CRFLSW ,
     K  FLSW   ,FANI   ,XCUT    ,ANIN   ,TANI   ,SECFCUM,BUFNOIS,
     L  IDATA  ,RDATA   ,
     M  IFRAME ,KXSP   ,IXSP    ,NOD2SP ,ISPSYM ,ISPCOND,
     N  XFRAME ,SPBUF  ,XSPSYM  ,VSPSYM ,PV     ,
     O  FSAVD  ,IBVEL  ,LBVEL   ,WASPH  ,XDP    ,W16    ,
     P  ISPHIO ,LPRTSPH,LONFSPH ,VSPHIO ,FBVEL  ,LAGBUF ,IBCSLAG,
     Q  IACTIV ,DAMPR  ,GJBUFI  ,GJBUFR , RBMPC , IBMPC ,SPHVELN,
     R  ITHGRPA ,ITHGRPB ,ITHGRPC ,ITHGRPD ,ITHGRPE ,
     S  ITHGRPF ,ITHGRPG ,ITHGRPH ,ITHGRPI ,
     T  NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,NERCVOIS,NESDVOIS,LERCVOIS,
     U  LESDVOIS,NPSEGCOM,LSEGCOM ,NPORGEO ,FSKYM   ,
     V  IXTG1   ,NPBYL   ,LPBYL   ,RBYL    ,IGEO    ,IPM     ,
     W  VISCN   ,MADPRT  ,MADSH4  ,MADSH3  ,MADSOL  ,MADNOD  ,MADFAIL ,
     X  IAD_RBY ,FR_RBY  ,FR_WALL ,PROCNE  ,IAD_RBY2,
     Y  FR_RBY2 ,IAD_I2M ,FR_I2M  ,ADDCNI2 ,PROCNI2 ,IADI2   ,FR_MV   ,
     Z  IADMV2  ,FR_LL   ,FR_RL   ,IADCJ   ,
     a  FR_CJ   ,FR_SEC  ,
     b  IAD_SEC ,IAD_CUT ,FR_CUT  ,RG_CUT  ,NEWFRONT,FR_MAD  ,NODGLOB ,
     c  FXBIPM  ,FXBRPM  ,FXBNOD  ,FXBMOD  ,FXBGLM  ,FXBCPM  ,FXBCPS  ,
     d  FXBLM   ,FXBFLS  ,FXBDLS  ,FXBDEP  ,FXBVIT  ,FXBACC  ,FXBELM  ,
     e  FXBSIG  ,FXBGRVI ,FXBGRVR ,EIGIPM  ,EIGIBUF ,EIGRPM  ,LSHSUB  ,
     f  DRSAVE  ,ARSAVE  ,STSAVE  ,STRSAVE ,FSAVSAV ,LNODPOR ,
     g  FR_I18  ,IADSDP  ,IADRCP  ,GRAPHE  ,IFLOW   ,RFLOW   ,
     h  ITHBUF  ,ITHBUFA ,ITHBUFB ,ITHBUFC ,ITHBUFD ,ITHBUFE ,ITHBUFF ,
     i  ITHBUFG ,ITHBUFH ,ITHBUFI ,LGRAV   ,DD_R2R  ,FASOLFR ,FR_LAGF ,
     j  LLAGF   ,LPRW    ,ICONTACT,RCONTACT,SH4TREE ,SH3TREE ,
     k  IPADMESH,PADMESH ,MSC     ,MSTG    ,INC     ,INTG    ,PTG     ,
     l  ISKWP   ,NSKWP   ,ISENSP  ,NSENSP  ,IACCP   ,NACCP,IPART_STATE,
     m  ACONTACT,PCONTACT,FACTIV  ,MCP     ,TEMP
     p                   ,SH4TRIM ,SH3TRIM ,MSCND   ,INCND   ,
     q  IBFFLUX ,FBFFLUX ,RBYM    ,IRBYM   ,LNRBYM  ,ICODRBYM,
     r  IBCV    ,FCONV   ,IBFTEMP ,FBFTEMP ,IRBE3   ,LRBE3   ,FRBE3   ,
     s  IAD_RBE3M,FR_RBE3M,FR_RBE3MP,IAD_RBYM,FR_RBYM,
     t  WEIGHT_RM,MS_PLY,ZI_PLY,INOD_PXFEM,IEL_PXFEM,IADC_PXFEM,
     u  ADSKY_PXFEM,ICODE_PLY,ICODT_PLY,ISKEW_PLY  ,MS0     ,ADMSMS  ,
     v  MADCLNOD,NOM_SECT,MCPC    ,MCPTG   ,DMELC   ,DMELTG  ,MSSA    ,
     w  DMELS   ,MSTR    ,DMELTR  ,MSP     ,DMELP   ,MSRT    ,DMELRT  ,
     x  IBCR    ,FRADIA  ,RES_SMS ,TABLE   ,IRBE2   ,LRBE2  ,IAD_RBE2 ,
     y  FR_RBE2 ,PHIE    ,MSF     ,
     z  PROCNE_PXFEM  ,IADSDP_PXFEM,IADRCP_PXFEM,ICFIELD,LCFIELD,CFIELD,
     1  IADUSERW, FR_USERWI, MSZ2 ,DIAG_SMS,ILOADP  ,LLOADP  ,LOADP   ,
     2  INOD_CRK,IEL_CRK ,IADC_CRK,ADSKY_CRK,
     3  CNE_CRK,PROCNE_CRK,IADSDP_CRK,IADRCP_CRK, IBUFSSG_IO,
     4  IBC_PLY ,DMINT2  ,IBORDNODE,
     5  ELBUF_TAB,POR    ,NODEDGE  ,IAD_EDGE, 
     6  FR_EDGE  ,FR_NBEDGE,CRKNODIAD,LGAUGE ,GAUGE  ,
     7  IGAUP    ,NGAUP  ,NODLEVXF,DD_R2R_ELEM, WEIGHT_MD,
     8  NODGLOBXFE,SPH2SOL ,SOL2SPH ,IRST ,FSKYD  ,
     9  DMSPH     ,WAGAP   ,XFEM_TAB,ELCUTC  ,NODENR  ,
     A  KXFENOD2ELC,ENRTAG,INTBUF_TAB, 
     B  RTHBU    F,KXIG3D ,IXIG3D ,KNOT, WIGE,
     C  TABSENSOR ,FBSAV6,WSMCOMP,STACK,
     D  CPUTIME_MP_GLOB, CPUTIME_MP ,TAB_UMP,POIN_UMP ,SOL2SPH_TYP,
     E  IRUNN_BIS,ADDCSRECT, IAD_FRNOR,FR_NOR  ,PROCNOR,
     F  IAD_FREDG,FR_EDG   ,DRAPE_Q4  , DRAPE_T3 ,TAB_MAT, 
     G  NATIV0_SMS,MULTI_FVM,DDP      ,SEGQUADFR,MS_2D  ,
     H  H3D_DATA   ,INTBUF_FRIC_TAB,SUBSETS,IGRNOD,IGRBRIC,
     I  IGRQUAD,IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,
     J  IGRSPRING,IGRPART,IGRSURF,FORNEQS,
     K  NLOC_DMG ,ISKWP_L,KNOTLOCPC,KNOTLOCEL,PINCH_DATA,TAG_SKINS6, ALE_CONNECTIVITY,
     L  XCELL, XFACE, NE_NERCVOIS, NE_NESDVOIS, NE_LERCVOIS, NE_LESDVOIS,IBCSCYC  ,LBCSCYC,
     M  T_MONVOL,ID_GLOBAL_VOIS,FACE_VOIS,TAGSLV_RBY,IPART_DYNAIN,FCONT_MAX,EBCS_TAB,DIFFUSION,
     N  KLOADPINTER,LOADPINTER,IN0 ,DGAPLOADINT)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE DSGRAPH_MOD
      USE ERROR_MOD
      USE RESOLSAV_MOD
      USE ICONTACT_MOD
      USE REMESH_MOD
      USE HEAT_MOD
      USE UNITS_MOD
      USE SMS_MOD
      USE SMS_PCG_PROJ
      USE THK_MOD
      USE INTSTAMP_GLOB_MOD
      USE RIGMAT_MOD
      USE PLYXFEM_MOD
      USE THREAC_MOD
      USE THGRELEM_MOD
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE ALEFLOW_MOD
      USE RAD2R_MOD
      USE INTBUFMOD
      USE FSKYI_MOD
      USE IMP_INTBUF
      USE MESSAGE_MOD
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD            
      USE I22BUFBRIC_MOD
      USE i22EDGE_MOD
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD
      USE XFEM2VARS_MOD
      USE STACK_MOD
      USE ALEFVM_MOD
      USE I22TRI_MOD
      USE FVBAG_MOD
      USE MPI_TOOLS_MOD
      USE ALEMUSCL_MOD
      USE ECND_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
      USE PBLAST_MOD
      USE GROUPDEF_MOD
      USE USER_INTERFACE_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE MATPARAM_DEF_MOD
      USE TIME_MOD
      USE PINCHTYPE_MOD
      USE CHECK_MOD
      USE ACCELERATION_DP_MOD   !   --> only useful for SP+parith/off
      USE ALE_CONNECTIVITY_MOD
      USE DYNLIB_MOD
      USE MONVOL_STRUCT_MOD
      USE DRAPE_MOD
      USE SENSOR_MOD
      USE MDS_RESTART_MOD
      USE EBCS_MOD
      USE SEATBELT_MOD
      USE DIFFUSION_MOD
      USE SEGVAR_MOD
      USE DTDC_MOD  
      USE IMPBUFDEF_MOD
      USE INTER_STRUCT_MOD
      USE INTER_SORTING_MOD
      USE SHOOTING_NODE_MOD
      USE ANIM_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "my_allocate.inc"
#include      "macro.inc"
#include      "assert.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "com10_c.inc"
#include      "intstamp_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr12_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "scr23_c.inc"
#include      "secur_c.inc"
#include      "units_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "cong1_c.inc"
#include      "cong2_c.inc"
#include      "scrfs_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "rnur_c.inc"
#include      "timeri_c.inc"
#include      "couple_c.inc"
#include      "rad2r_c.inc"
#include      "scr24_c.inc"
#include      "chara_c.inc"
#include      "bucki_c.inc"
#include      "buckr_c.inc"
#include      "nsvis_c.inc"
#include      "scr01_c.inc"
#include      "scr04_c.inc"
#include      "vecfx_c.inc"
#include      "vecpx_c.inc"
#include      "vect01_c.inc"
#include      "scrupw_c.inc"
#include      "lagmult.inc"
#include      "check.inc"
#include      "warn_c.inc"
#include      "vale_c.inc"
#include      "impl1_c.inc"
#include      "fxbcom.inc"
#include      "eigcom.inc"
#include      "scr20_c.inc"
#include      "subc_c.inc"
#include      "spmd_c.inc"
#include      "buckcom.inc"
#include      "impl2_c.inc"
#include      "remesh_c.inc"
#include      "com_xfem1.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
#include      "thknod_c.inc"
#include      "sms_c.inc"
#include      "filescount_c.inc"
#include      "inter22.inc"
#include      "userlib.inc"
#include      "alefvm.inc"
#include      "r4r8_p.inc"
#include      "drape_c.inc"
#include      "thermal_c.inc"
#include      "alemuscl.inc"


      COMMON /VGLOB/DMAS,DINER
      my_real
     .       DMAS,DINER
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK
      INTEGER  ICODE(*), ISKEW(*), ISKWN(LISKN,*), NETH(*),
     .   IPART(*),NOM_OPT(LNOPT1,*),IXS(*),
     .   IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXUR(*)  ,IXTG(NIXTG,*), IXTG1(4,*),
     .   ITAB(*), 
     .   KINET(*),
     .   IFILL(NUMNOD,*), IMS(*), NPC(*), IBCL(*), IBFV(*),
     .   IDUM(*), LAS(*),IPARG(NPARG,*),IPARI(NPARI,*),
     .   NPRW(*), LPRW(*), ICONX(*), NPBY(NNPBY,*),
     .   LPBY(*),
     .   LRIVET(*), NSTRF(*), LJOINT(*), ICODT(*), ICODR(*), ILINK(*),
     .   LLINK(*), LINALE(*), NEFLSW(*), NNFLSW(*),
     .   NODPOR(*),ICUT(*) , INOISE(*),MONVOL(*),
     .   ADSKY(*),IADS(*),LACCELM(3,*),DD_IAD(NSPMD+1,*),FR_ELEM(*),
     .   IAD_ELEM(2,*),IAD_RBY(*),FR_RBY(*),NNLINK(10,*),LNLINK(*),
     .   IAF(*),ITHGRP(*),IGRV(*),WEIGHT(*),MAIN_PROC(*),WEIGHT_MD(*),
     .   KXX(NIXX,*),IXX(*),IEXLNK(NR2R,*),ITABM1(*),IUSER(*),
     .   IFRAME(LISKN,*),KXSP(NISP,*),IXSP(*),NOD2SP(*),
     .   ISPCOND(NISPCOND,*),ISPSYM(NSPCOND,*),IBVEL(NBVELP,*),LBVEL(*),
     .   ISPHIO(NISPHIO,*),LPRTSPH(*),LONFSPH(*),LAGBUF(*),IBCSLAG(*),
     .   IACTIV(*),GJBUFI(LKJNI,*) ,IBMPC(*),NPBYL(NNPBY,*), LPBYL(*),
     .   ITHGRPA(*), ITHGRPB(*), ITHGRPC(*), ITHGRPD(*), ITHGRPE(*),
     .   ITHGRPF(*), ITHGRPG(*), ITHGRPH(*), ITHGRPI(*), NBRCVOIS(*),
     .   NBSDVOIS(*),LNRCVOIS(*),LNSDVOIS(*),
     .   NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*), NPORGEO(*),
     .   NPSEGCOM(*), LSEGCOM(*), IGEO(NPROPGI,*),
     .   IPM(NPROPMI,*),MADPRT(*), MADSH4(*), MADSH3(*), MADSOL(*),
     .   MADNOD(*),MADFAIL(*), IADSDP(*), IADRCP(*),
     .   PROCNE(*), FR_WALL(*),IAD_RBY2(*),
     .   FR_RBY2(*),IAD_I2M(*),FR_I2M(*),ADDCNI2(*),PROCNI2(*),IADI2(*),
     .   FR_MV(*), IADMV2(*),  FR_LL(*),  FR_RL(*),
     .   IADCJ(*), FR_CJ(*),  
     .   FR_SEC(*), IAD_SEC(4,*),
     .   IAD_CUT(NSPMD+2,*), FR_CUT(*), RG_CUT(*), NEWFRONT(*),
     .   FR_MAD(5,*), NODGLOB(*), LNODPOR(*), FR_I18(*), 
     .   FXBIPM(NBIPM,*),FXBNOD(*),FXBELM(*), FXBGRVI(*),
     .   EIGIPM(NEIPM,*), EIGIBUF(*), LSHSUB(*), IFLOW(*), FASOLFR(*),
     .   DD_R2R(NSPMD+1,*), ITHBUF(*), ITHBUFA(*), ITHBUFB(*),
     .   ITHBUFC(*), ITHBUFD(*), ITHBUFE(*),  ITHBUFF(*),  ITHBUFG(*),
     .   ITHBUFH(*), ITHBUFI(*), LGRAV(*), FR_LAGF(3,*), LLAGF(*),
     .   ICONTACT(*), SH4TREE(*), SH3TREE(*), IPADMESH(*),
     .   ISKWP(*), NSKWP(*), ISENSP(2,*), NSENSP(*), IACCP(*), NACCP(*),
     .   IPART_STATE(*),SH4TRIM(*), SH3TRIM(*),IRBYM(*) ,LNRBYM(*),
     .   ICODRBYM(*),IRBE3(NRBE3L,*),LRBE3(*),IAD_RBE3M(*),FR_RBE3M(*),
     .   FRBE3(*), FR_RBE3MP(*),IAD_RBYM(*),
     .   FR_RBYM(*),NOM_SECT(*), IBCR(*),IRBE2(*),LRBE2(*),
     .   IAD_RBE2(*),FR_RBE2(*),IADSDP_PXFEM(*),
     .   IADRCP_PXFEM(*),ICFIELD(*),LCFIELD(*),ILOADP(*),LLOADP(*),
     .   IADSDP_CRK(*),IADRCP_CRK(*),INOD_CRK(*),IEL_CRK(*),IADC_CRK(*),
     .   ADSKY_CRK(*),PROCNE_CRK(*),CNE_CRK(*),IBORDNODE(*),
     .   NODEDGE(*),IAD_EDGE(*),FR_EDGE(*),FR_NBEDGE(*),
     .   CRKNODIAD(*), LGAUGE(3,*), IGAUP(*), NGAUP(*),NODLEVXF(*),
     .   NODGLOBXFE(*),ELCUTC(2,*),NODENR(*),KXFENOD2ELC(*),
     .   ENRTAG(NUMNOD,*),KXIG3D(*),IXIG3D(*),TABSENSOR(STABSEN),
     .   NATIV0_SMS(*), SEGQUADFR(*), TAGSLV_RBY(*),IPART_DYNAIN(*),
     .   KLOADPINTER(NINTER+1)  ,LOADPINTER(NINTER*NLOADP_HYD) 
      INTEGER IDATA(*), IBFTEMP(*), IBCV(*), IBFFLUX(*), WEIGHT_RM(*),
     .        ICODT_PLY(*), ISKEW_PLY(*),INOD_PXFEM(*),IEL_PXFEM(*),
     .        IADC_PXFEM(*),ADSKY_PXFEM(*),ICODE_PLY(*),MADCLNOD(*),
     .        PROCNE_PXFEM(*),IADUSERW(*), FR_USERWI(*),
     .        IBUFSSG_IO(*),IBC_PLY(*),DD_R2R_ELEM(*),
     .        SPH2SOL(*), SOL2SPH(2,*), IRST(3,*), SOL2SPH_TYP(*),IRUNN_BIS,
     .        ADDCSRECT(*), IAD_FRNOR(*), FR_NOR(*), PROCNOR(*), 
     .        IAD_FREDG(*), FR_EDG(*),TAG_SKINS6(*),IBCSCYC(*),LBCSCYC(*)
      INTEGER, DIMENSION(NUMSKW), INTENT(IN) :: ISKWP_L
      INTEGER, DIMENSION(*), INTENT(in) :: ID_GLOBAL_VOIS,FACE_VOIS
      my_real, DIMENSION(3*NUMNOD), TARGET :: X
      my_real
     .   D(3,*)      ,V(3,*)   ,VR(3,*),DAMP(*),
c     .   X(3,*)    ,D(3,*)      ,V(3,*)   ,VR(3,*),DAMP(*),
     .   MS(*)   ,IN(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),GEO(NPROPG,*),
     .   BUFMAT(*) ,W(3,*)    ,VEUL(*),FILL(NUMNOD,*),DFILL(NUMNOD,*),
     .   ALPH(*)   ,WB(3,*)   ,TF(*)       ,FORC(*)  ,VEL(*),
     .   FSAV(NTHVKI,*) ,FZERO(3,*),XLAS(*)     ,ELBUF(*) ,
     .   RWBUF(NRWLP,*),RWSAV(*),RBY(NRBY,*),RIVET(*),WA(*),
     .   FV(*)     ,A(3,*)    ,AR(3,*)     ,VAL2(*)  ,PHI(*),
     .   R(3,*)    ,CRFLSW(*),FLSW(*),
     .   FANI(3,*) ,UWA(*)    ,PARTSAV(*)    ,STIFN(*) ,STIFR(*),
     .   DSAVE(3,*),ASAVE(3,*),XCUT(*)   ,ANIN(*)    ,BUFNOIS(*),
     .   FSKY(8,*),DR(3,*),ACCELM(LLACCELM,*),
     .   TANI(*),VOLMON(*),EANI(*),AGRV(*), THKE(*), BUFSF(*),AF(*),
     .   SECBUF(*),SECFCUM(7,NUMNOD,NSECT),LAMBDA(*), 
     .   FR_WAVE(*),PARTS0(*),BUFGEO(*),
     .   USER(*),SPBUF(NSPBUF,*),XFRAME(NXFRAME,*),
     .   WASPH(*),W16(*),VSPHIO(*),FBVEL(*),DAMPR(NRDAMP,*),
     .   RDATA(*),PV(*),FSAVD(NTHVKI,*),GJBUFR(LKJNR,*),RBMPC(*),
     .   SPHVELN(*),RBYL(NRBY,*),FSKYM(*), MSNF(*),VISCN(*),
     .   FXBRPM(*), FXBMOD(*), FXBGLM(*), FXBCPM(*), FXBCPS(*),
     .   FXBLM(*),  FXBFLS(*), FXBDLS(*), FXBDEP(*), FXBVIT(*),
     .   FXBACC(*), FXBSIG(*), FXBGRVR(*), EIGRPM(NERPM,*),
     .   DRSAVE(3,*), ARSAVE(3,*), STSAVE(*), STRSAVE(*), 
     .   FSKYD(*), DMSPH(*),KNOT(*),WIGE(*),MS_2D(*),
     .   KNOTLOCPC(*),KNOTLOCEL(*),XCELL(*),XFACE(*),FCONT_MAX(3,*)
      my_real
     .   FSAVSAV(NTHVKI,*), RFLOW(*), RCONTACT(*),
     .   PADMESH(*), MSC(*), MSTG(*), INC(*) , INTG(*), PTG(3,*),
     .   ACONTACT(*), PCONTACT(*), FACTIV(*), TEMP(*), MCP(*),
     .   MSCND(*), INCND(*), RBYM(*), FBFFLUX(*),
     .   FCONV(*), FBFTEMP(*), MS_PLY(*), ZI_PLY(*), MS0(*), ADMSMS(*),
     .   MCPC(*), MCPTG(*), DMELC(*), DMELTG(*), MSSA(*), DMELS(*),
     .   MSTR(*), DMELTR(*), MSP(*), DMELP(*), MSRT(*), DMELRT(*),
     .   FRADIA(*), RES_SMS(*), PHIE(*),MSF(*),
     .   CFIELD(*),MSZ2(*), DIAG_SMS(*),LOADP(*), DMINT2(*),POR(*),
     .   GAUGE(LLGAUGE,*),WAGAP(2,*),RTHBUF(*),FORNEQS(3,*),IN0(*)
      my_real , INTENT(IN) :: DGAPLOADINT(NINTER*NLOADP_HYD)
      my_real, DIMENSION(2) :: XX
      my_real yy
      TYPE (CLUSTER_) ,DIMENSION(*) :: CLUSTER
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(TTABLE) :: TABLE(*)
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB
      TYPE(ELBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: ELBUF_IMP
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE SPSYM_STRUCT
            my_real, DIMENSION(:)  , POINTER :: BUF 
      END TYPE SPSYM_STRUCT
      TYPE (SPSYM_STRUCT) :: XSPSYM,VSPSYM,WSMCOMP
      TYPE (STACK_PLY) :: STACK
      TYPE (MPI_MIN_REAL_STRUCT) :: MPI_BUF
      INTEGER MIN_TAB(4)
      my_real :: DT2R
C
      DOUBLE PRECISION XDP(3,*),DDP(3,*)
      DOUBLE PRECISION, DIMENSION(12,6,SFSAV6) :: FBSAV6
C     Mat + Prop timers     
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      my_real, DIMENSION(NBR_GPMP,NSPMD+1) ::  CPUTIME_MP_GLOB
      my_real, DIMENSION(TAILLE) ::  CPUTIME_MP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
      my_real TAB_MAT(NGROUP)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(INTBUF_FRIC_STRUCT_), DIMENSION(NINTERFRIC) :: INTBUF_FRIC_TAB
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
!
      TYPE(SUBSET_) ,DIMENSION(NSUBS)   :: SUBSETS
      TYPE(GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(GROUP_)  ,DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE(GROUP_)  ,DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE(GROUP_)  ,DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE(GROUP_)  ,DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE(GROUP_)  ,DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE(GROUP_)  ,DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE(GROUP_)  ,DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE(GROUP_)  ,DIMENSION(NGRPART) :: IGRPART
      TYPE(SURF_)   ,DIMENSION(NSURF)   :: IGRSURF
c
      TYPE(NLOCAL_STR_)  :: NLOC_DMG
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE(GRP_PARAM_STRUCT_) , DIMENSION(NGROUP) :: GROUP_PARAM_TAB
      TYPE(PINCH) :: PINCH_DATA
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER, INTENT(IN) :: NE_NERCVOIS(*), NE_NESDVOIS(*), NE_LERCVOIS(*), NE_LESDVOIS(*)
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      TYPE (DRAPE_) :: DRAPE_Q4  , DRAPE_T3 
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE(t_ebcs_tab), INTENT(INOUT) :: EBCS_TAB
      TYPE(T_DIFFUSION), INTENT(INOUT) :: DIFFUSION
      TYPE(t_segvar) :: SEGVAR
      TYPE(inter_struct_type), DIMENSION(:), ALLOCATABLE :: INTER_STRUCT   !   structure for interface
      TYPE(sorting_comm_type), DIMENSION(:), ALLOCATABLE :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL LOUT
      CHARACTER FILNAM*100,OPTION*256
      INTEGER NODFT, NODLT,  I, N,  
     .   K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, K, ISK,
     .   N0, N1, NN,  NNOD, 
     .   ISYNC, J,
     .   NELTST,ITYPTST,NWAFT, NBNCL, NBIKL,
     .   NBNODL, NBNODLR,GREFTSK,GRELTSK,
     .   ISTOP,  NFIA, NFEA, NFOA, NDMA, NDIN,
     .   NFNCA, NFTCA,NDMA2, NUVAR,NUVARI,NFNCA2, NFTCA2,
     .   L1,L2,L3,LL1,LL2,LL3,NFT2,LISENDP_PXFEM,
     .   LIRECVP_PXFEM,NDAMA2,FLG_KJ2,NCONT,BID,K12,IVAD,IAD_GREL,
     .   FLG_KJ2_RAZ,I_EXCH_FLG_RAZ,SIZE_OPT
      INTEGER :: LISENDP_CRK,LIRECVP_CRK
      INTEGER :: I13A,I13B,I13C,I13D,I13E,I13F,I13G,I13H,I13I
      INTEGER :: I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,I15K
      INTEGER :: I15ATH,I35ATH
      INTEGER :: I87A,I87B,I87C,I87D,I87E,I87F,I87G,I87H,I87I,I87J,I87K,
     .        I87L,I87M,I87N,IMSCH,IAD1B,IAD1,IAD2,I2MSCH, ISMSCH, ONOFP
      INTEGER CPLXIT,  ADRNOD, ONOF, ONFELT, ICH,IFLGADM, MADENDREQUEST
      CHARACTER*13 CDUM
      INTEGER IDUM1, KRNFRM, KPNODE, KN, DIM6, DIM_EXCH
      
      my_real
     .        RDUM1,MADUF,XN,RBUF(10)
      INTEGER LLT1,IWIOUT
      INTEGER KSPH1,KSPH21,KSPH22,KSPH23,IUN,
     .        KSPACTIV,KSP2SORT,NELTSA,ITYPTSA,IDTNOD
      INTEGER NEGMAS
      INTEGER NBINTC, SIZE, LENR, LENS, LENI, SIZI, ISIZXV ,ILENXV,
     .        I2SIZE, LSEND1, LRECV1, LSEND2, LRECV2, NPARTL,
     .        ISLEN7, IRLEN7, ISLEN11, IRLEN11, LISENDP, LIRECVP,
     .        ISLEN17, IRLEN17,IRLEN7T,ISLEN7T,LINDIDEL,LBUFIDEL,LBUFSEGLO,
     .        ISLEN20, IRLEN20, ISLEN20T, IRLEN20T, NBINT20,
     .        ISLEN20E, IRLEN20E,R3SIZE,LAG_SEC,LENS1,LENR1,INT18KINE,
     .        NRBYKIN_L, INT24USE, NELEML,I24MAXNSNE,INT24E2EUSE
      INTEGER :: INT7ITIED
      INTEGER, DIMENSION(:,:),ALLOCATABLE ::
     .        ISENDTO,IRCVFROM,FR_NBCC,FR_NBCCI2,FR_NBCC1
      INTEGER, DIMENSION(:),ALLOCATABLE ::
     .        INTLIST,NISKYFI,ISENDP,IRECVP, IRBKIN_L,
     .        IPARTL, NISKYFIE, TAGEL,
     .        ISENDP_PXFEM,IRECVP_PXFEM,COUNT_REMSLV,COUNT_REMSLVE,
     .        INTLIST25
      INTEGER, DIMENSION(:), ALLOCATABLE, TARGET :: CNEL,ADDCNEL,ADDTMPL
      INTEGER, DIMENSION(:),ALLOCATABLE ::
     .        ISENDP_CRK,IRECVP_CRK
      INTEGER NSPHACTG,nd
      INTEGER, DIMENSION(1), TARGET :: IMP_DUM
C----
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DRETRI, XSEC,QFRICINT,ICODR0,ICODT0,
     .                                     STIFN_TMP,STIFR_TMP
      SAVE ISENDTO, IRCVFROM, INTLIST, FR_NBCC, NISKYFI, FR_NBCCI2,
     .     NBINTC, I2SIZE, ISLEN7   ,IRLEN7   ,ISLEN11  ,IRLEN11,
     .     ILENXV, ISIZXV, ISLEN17, IRLEN17,IRLEN7T,ISLEN7T,
     .     ISLEN20, IRLEN20, ISLEN20T, IRLEN20T, NBINT20, NISKYFIE,
     .     ISLEN20E, IRLEN20E,R3SIZE, NRBYKIN_L, IRBKIN_L, QFRICINT,
     .     INTLIST25
      SAVE DRETRI, XSEC, LISENDP, LIRECVP, ISENDP, IRECVP,
     .     CNEL, ADDCNEL, ADDTMPL, IPARTL, NPARTL, LINDIDEL, LBUFIDEL,
     .     LBUFSEGLO
      my_real
     .   DT2T,
     .   DT2SAVE,  MAS, BB, TFEXC,TREST,DTREST,
     .   DMAST, DINERT,DTU, RFACTOR, RFMAX,FACTB,DAMPT,
     .   DTFNOD, DTMNOD, XSENS(12,NSENSOR),DAMPA3
      my_real
     .   FXBMVN(LENCP),FXBSV(LENLM),FXBSE(15*NFXBODY),DXANCG(3,NUMNOD),
     .   FXBMCD(LENMCD),FXBFP(LENVAR),FXBFC(LENLM),FXBEFW(NFXBODY),
     .   FXBEDP(NFXBODY),FXBGRP(LENVAR),FXBGRW(NFXBODY)
      my_real
     .   DT2PREV, DTMIN1_SAVE, TARGET_DT
      SAVE IMSCH, I2MSCH, ISMSCH
      SAVE DT2PREV
      SAVE
     .     NFIA, NFEA, NFOA, NDMA, NDIN, NDMA2,
     .     I13A,I13B,I13C,I13D,I13E,I13F,I13G,I13H,I13I,
     .     I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,I15K,
     .     I87A,I87B,I87C,I87D,I87E,I87F,I87G,I87H,I87I,I87J,I87K,I87L,
     .     I87M,I87N,I15ATH,I35ATH,LAG_SEC,NFT2,NDAMA2
      INTEGER NDDL0,NNZK0,LENQMV,NV46,NUM_IMP(NINTER),NINT7,NT_IMP,NT_RW,
     .        LI1,LI2,LI3,LI4,LI5,LI6,
     .        LIF,NNDL,IT,NMC2,LI13,DIRUL(NFXVEL),NUM_IMP1(NINTER),
     .        FR_RBE2M(SFR_RBE2),R2SIZE,
     .        NUM_IMPL(NINTER,NTHREAD), I_OPT_STOK(NINTER),IT_T,NTMP
      INTEGER
     .        IER1,NDS,IBUCK,NMRBE2,NTHOLD,
     .        NN1
      SAVE
     .        NMRBE2,R2SIZE,NINT7
      INTEGER, DIMENSION(:),POINTER :: NS_IMP,NE_IMP,IND_IMP
      TYPE (IMPBUF_STRUCT_), TARGET :: IMPBUF_TAB
      my_real, DIMENSION(:)  , POINTER :: X_A,FEXT_IMP,R_IMP 
      my_real
     .   TTMP,DMCP(NUMGEO)
      data TTMP /0.0/
      INTEGER LWIBEM, LWRBEM, IERROR, IERROR2
      INTEGER, DIMENSION(:), ALLOCATABLE :: WIBEM
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: WRBEM
      SAVE LWIBEM, LWRBEM, WIBEM, WRBEM
      INTEGER NKCOND, NDDLG, NRP, NCP, NKCOND_INI, NT_IMP_OLD
      INTEGER LWIFLOW, LWRFLOW,IPARSENS
      INTEGER, DIMENSION(:), ALLOCATABLE :: WIFLOW
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: WRFLOW
      SAVE LWIFLOW, LWRFLOW, WIFLOW, WRFLOW
      INTEGER SIZ, R2R_ON
      INTEGER NDDLG0, NNZKG0, NNMAXG, NNZKG, NNMAX, NDDL0P(NSPMD),
     .        NNZK0P(NSPMD), NDDLP(NSPMD), NNZKP(NSPMD),
     .        NNMAXP(NSPMD), NDDLPMAX
      INTEGER, DIMENSION(:), ALLOCATABLE :: CDDLP
#if defined(MUMPS5)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
#endif
      INTEGER IFVMESH
C
      my_real ,DIMENSION(:,:), ALLOCATABLE :: MADCLFRECV,PARTSAV2
C
      INTEGER MADYMO_DEL,MADYMO_DEL_GLOBAL
      INTEGER, DIMENSION(:),  ALLOCATABLE ::MAD_TAG_SOL, MAD_TAG_SH,MAD_TAG_TG,MAD_FAIL_ELEMENTS
C
C OpenMP specific
      INTEGER ITSK, NODFTSK, NODLTSK, NUMNTSK, NDTSK, IPMTSK,
     .        PARTFTSK, PARTLTSK, NWAFTSK, I16TSK,
     .        NELTSTT, ITYPTSTT,IGMTSK,NGROUC, NGROUNC,
     .        IGROUC(NGROUP), IGROUNC(NGROUP),
     .        NSNFIOLD(NSPMD),ADSKYI(0:NUMNOD+1),
     .        KINDRBY(NRBYKIN), NINDEX1(NINTER), NINDEX2(NINTER),
     .        NINDEX3(NINTER), NINDEX4(NINTER), KINDRBYM(NRBYM),
     .        OMP_GET_THREAD_NUM,IADISK,RNUM_SIZ,RNS,NINDEXP,NI,LENC
      EXTERNAL OMP_GET_THREAD_NUM
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFIDEL, INDIDEL,RENUM,IBUFSEGLO,
     .                                      INDSEGLO
      my_real
     .        DT2TT,D_TSTART,D_TSTOP
      my_real
     .        XSLV(18,NINTER),XMSR(12,NINTER),
     .        VSLV(6,NINTER),VMSR(6,NINTER),
     .        SIZE_T(NINTER)
C End OpenMP specific
C Parith/ON specific
      INTEGER FR_RBY6(SFR_RBY)
      DOUBLE PRECISION
     .        RBY6(8,6,NRBYKIN),FRL6(15,6,NRLINK),FNL6(15,6,NLINK),
     .        FRWL6(7,6,NRWALL),RBYM6(6,6,NRBYM),SPHG_F6(4,6,NBGAUGE)
      my_real
     .        FRL(4,NRLINK), FNL(4,NLINK)
      INTEGER NGRTH,NELEM
      INTEGER RESTSIZE
C End Parith/ON specific
      INTEGER FR_SMS(NSPMD+1), FR_RMS(NSPMD+1)
      INTEGER, POINTER, DIMENSION(:) :: PTR_SMS
      INTEGER NMT0,IROTG,NMT,NMP,NTH0
      my_real
     .      , DIMENSION(:), ALLOCATABLE :: RRBE3
      DOUBLE PRECISION
     .      , DIMENSION(:), ALLOCATABLE :: RRBE3_PON
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKINE
      my_real
     .      , DIMENSION(:), ALLOCATABLE :: STK_SN,STK_SR,FEXT,
     .         FCLUSTER,MCLUSTER
      INTEGER LNZM
      INTEGER, DIMENSION(:),ALLOCATABLE :: INT18ADD,TAGPENE
      INTEGER, DIMENSION(:),ALLOCATABLE :: IDAMP_RDOF_TAB
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: MTF, CAND_SAV
      INTEGER FLG_DTNODAMP
      my_real
     .       DT3
      INTEGER  IGROUPFLG(2)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IGROUPC,IGROUPTG,IGROUPS
      INTEGER  IOLDSECT
      my_real
     .      ,  DIMENSION(:),ALLOCATABLE :: VARNOD
C-----------------------------------------------------------------------------------
C Parith/OFF + AMS
      INTEGER, DIMENSION(:),ALLOCATABLE :: UNCOMP_FR,FR_LOC, 
     *                                     UNCOMP_FRI2M,FR_LOCI2M
      INTEGER NB_FR,NB_FRI2M
C-----------------------------------------------------------------------------------
C ADDITIVE MANUFACTURING 
      my_real, DIMENSION(:), ALLOCATABLE :: MCP_OFF
C-----------------------------------------------------------------------------------
C TEMPI I24
      INTEGER SFR_I24
      INTEGER, DIMENSION(:), ALLOCATABLE :: FR_I24
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_I24
      my_real
     .        DELTA_PMAX_GAP(NINTER)
      INTEGER DELTA_PMAX_GAP_NODE(NINTER)
      INTEGER S_LOADPINTER, NPRESLOAD
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAGNCONT
      INTEGER, DIMENSION(:), ALLOCATABLE :: LOADP_HYD_INTER, LOADP_TAGDEL
C-----------------------------------------------------------------------------------
C Debug CAND INTERFACE TYPE25
      INTEGER NB25_CANDT(PARASIZ),NB25_IMPCT(PARASIZ),
     .        NB25_DST1(PARASIZ),NB25_DST2(PARASIZ)
      SAVE NB25_CANDT,NB25_IMPCT,NB25_DST1,NB25_DST2
C Communication INTERFACE TYPE25
      INTEGER SFR_I25
      INTEGER, DIMENSION(:), ALLOCATABLE :: FR_I25
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_I25
C Assembling normals INTERFACE TYPE25
      REAL*4
     .       , DIMENSION(:,:), ALLOCATABLE :: FSKYN25
C time varying gap T25
      my_real MAXDGAP(NINTER)
      INTEGER :: FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE
C-----------------------------------------------------------------------------------
C Multidomains
      INTEGER IDEL7NOK_R2R,IDEL7NOK_SAV,NUMSPH_GLO_R2R,FLG_SPHINOUT_R2R
      INTEGER, DIMENSION(:),ALLOCATABLE :: OFF_SPH_R2R
C-----------------------------------------------------------------------------------
C Sensors inter
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ISENSINT
      INTEGER NISUBMAX

      INTEGER, ALLOCATABLE, DIMENSION(:) :: ICONTACT_OLD
C-----------------------------------------------------------------------------------
      my_real
     . ,  DIMENSION(:),ALLOCATABLE :: WASPSYM
C AMS
      INTEGER LSKYI_SMS_NEW
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: T2MAIN_SMS
      my_real, DIMENSION(:),ALLOCATABLE :: T2FAC_SMS         
C-----------------------------------------------------------------------------------
C     FVMBAG: switch to UP
      INTEGER :: NFVBAG0 
      LOGICAL :: CHECK_NPOLH
C---ADMESH + TRIMING----      
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGTRIMC,TAGTRIMTG
      

      INTEGER :: IFTHE, ICONDN, IDX_FTHE, IDX_CONDN, IDX_PINCH,MSTOP_DT_THERM
C---Nitsche Method----
      INTEGER NFACNIT      
      my_real, DIMENSION(:,:),ALLOCATABLE :: STRESSMEAN    
      my_real, DIMENSION(:),ALLOCATABLE :: FORNEQSKY  

      INTEGER :: NUMSKW_L,NUMSKW_L_SEND,NODFTSK_2,NODLTSK_2
      INTEGER, DIMENSION(NUMSKW) :: ISKWP_L_SEND
      INTEGER, DIMENSION(NSPMD) :: RECVCOUNT
      INTEGER, DIMENSION(NTHREAD) :: NODFT_ASSPAR, NODLT_ASSPAR
      INTEGER NODFT_NL,NODLT_NL
      REAL(kind=8) :: SECS
      REAL(kind=8) :: TT_DOUBLE
      my_real
     .         DTNOD_NLOCAL
      INTEGER, DIMENSION(:),ALLOCATABLE ::IBUFPDEL, NINDEXPDEL
C----------------------------------------------------------------------------------
C IMPVEL/FGEO
      INTEGER FXVEL_FGEO
C----------------------------------------------------------------------------------
C MDS Table
      INTEGER, DIMENSION(:), ALLOCATABLE :: MDS_OUTPUT_TABLE
      INTEGER MAX_DEPVAR
C-----------------------------------------------------------------------------------
      INTEGER IFLAG, COMPTREAC ! Flag for computing reaction forces
!     1. Storing Freac=Fint+Fext+Fcont
!     2. Add acceleration fields (gravity, etc) to Freac
!        Downloading velocities (cf so-called main/secnd formulation)
!        In RD, accelerations are computed so that the resulting velocity after integration 
!                                                      will correspond to the expected velocity
!               because sortie_main (output) is done before integration of accelerations
!     3. Finalize Freac=MS*A-Freac
C----------------------------------------------------------------------------------
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NSKWP : integer ; dimension = NSPMD
!               number of skew per processor
!       ISKWP : integer ; dimension=NUMSKW+1
!               gives the ID processir of the current i SKEW
!               ISKWP < 0 --> the SKEW is local on a processor
!               and we don't need to communicate the data
!               ISKWP > 0 --> the SKEW is global and the data must be
!               communicated
!       NUMSKW_L : integer 
!                  number of local SKEW
!       NUMSKW_L_SEND : integer 
!                       number of sent SKEW 
!       ISKWP_L_SEND : integer ; dimension=NUMSKW_L_SEND
!                      index of sent SKEW
!       RECVCOUNT : integer ; dimension=NSPMD
!                   number of received SKEW
!       NODFT_ASSPAR : integer ; dimension=NTHREADS
!                      lower bound for asspar4 splitting
!       NODLT_ASSPAR : integer ; dimension=NTHREADS
!                      upper bound for asspar4 splitting
!       TT_DOUBLE : real(kind8)
!                   current time in double precision --> with simple precision version,
!                   the current time can be wrong (due to error's accumulation) if the
!                   number of cycle is huge AND the time step is small 
!                   (ie. when NCYCLE > 300 000 and DT = 10^-7)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER :: STATE_H3D, STATE_ANIM
        LOGICAL :: BOOL_RESTART

        INTEGER :: NATIV_SMS_SIZ ! size of NATIV_SMS array
        INTEGER :: TEMP_SIZ      ! size of TEMP

        TYPE(shooting_node_type) :: SHOOT_STRUCT
        INTEGER :: S_ELEM_STATE     ! size of ELEM_STATE array
        LOGICAL, DIMENSION(:), ALLOCATABLE :: ELEM_STATE    ! boolean : true if element is ON, false if element is OFF
        INTEGER :: SIZE_ADDCNEL ! size of addcnel array
        INTEGER :: SIZE_CNEL ! size of cnel array
      DATA IUN/1/
C-----------------------------------------------------------------------------------       
C Uncomment for Debug - ITAB is copied in Debug Module.
C-----------------------------------------------------------------------------------
       CALL PREPARE_DEBUG()
C-----------------------------------------------------------------------------------
      BOOL_RESTART = .TRUE.
      STATE_H3D = 0 
      STATE_ANIM = 0
      IF(IPARIT==0.AND.IRESP==1) THEN
        ALLOCATE( ACC_DP(3,NUMNOD) )
        ACC_DP(1:3,1:NUMNOD) = 0.d+00
      ENDIF
      ALLOCATE(INT18ADD(NINTER+1))
      INT18ADD(1) = 1
      INT18ADD(NINTER+1) = 0
      ALLOCATE (IDAMP_RDOF_TAB(SICONTACT))
      ALLOCATE( ICONTACT_OLD(SICONTACT))
      IDAMP_RDOF_TAB = 0
      FLG_DTNODAMP = 0
      IF ((NODADT==1).AND.
     .       (IDAMP_RDOF==NDAMP .OR. NDAMP>0 .OR. ISTAT==3)) THEN
        FLG_DTNODAMP = 1
      ENDIF
      FLG_KJ2 = 0
      I_EXCH_FLG_RAZ = 0
      
      NELTST  = 0
      ITYPTST = 0
      DT2T    = 0
      MSTOP_DT_THERM = 0

      IF(ITHERM_FE>0) THEN
        TEMP_SIZ = NUMNOD
      ELSE
        TEMP_SIZ = 0
      ENDIF

C initialize int24 presence flag
      INT24USE = 0
      INT24E2EUSE = 0
C-----------------------------------------------
C exit flag for MaDyMo coupling
      CPLXIT=0
      ILASTANIM=0
      ILASTH3D=0
      IOLDSECT = -1
      ILASTDYNAIN=0
      NODFT   = 1
      NODLT   = NUMNOD
      NWAFT   = 1
      LOUT = ISPMD==0
      IWIOUT  = 0
      LMPC    = 0
      TT_DOUBLE = TT
      LSKYI_COUNT = 0
      FLAG_SLIPRING_UPDATE = 0
      FLAG_RETRACTOR_UPDATE = 0
c
      IF(ISPMD==0)THEN
          DO I = 1,NUMMPC
            LMPC = LMPC + IBMPC(I)
          ENDDO
        END IF
C-----------------------------------------------
C     I N I T I A L I S A T I O N S
C----------------------------------------------
      ALLOCATE(IKINE(NUMNOD))
C -------------------------------------------------------------------        
C User Libraries get the possibility to use GET_U_NOD_X & GET_U_NOD_V in user elements properties (Solids & Springs)
        GETUNOD_NOCOM=0
        
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C initialisation timer resol
        IF (IMON>0) THEN
          CALL STARTIME(1,1)
        ENDIF
C----------------------------------------------
C       Allocations Phase1
C----------------------------------------------
        IMSCH  = 0
        I2MSCH = 0
        DT2PREV= ZERO
        LISENDP_PXFEM = 0
        LIRECVP_PXFEM = 0
C
        LISENDP_CRK = 0
        LIRECVP_CRK = 0
C
        T0INF_PBLAST = EP20
        IDT_PBLAST   = 0
C
        NUMSPH_GLO_R2R = 0
        FLG_SPHINOUT_R2R = 0  

        CALL ASSINIT(ADSKY,IAD_ELEM,FR_ELEM,PROCNE,LISENDP,LIRECVP)
        IF(IPLYXFEM > 0 )
     .    CALL ASSINIT_PXFEM(ADSKY_PXFEM,INOD_PXFEM,IAD_ELEM,FR_ELEM,
     .                       PROCNE_PXFEM,LISENDP_PXFEM,LIRECVP_PXFEM)
        IF (ICRACK3D > 0 .AND. NSPMD > 1)
     .    CALL ASSINIT_CRKXFEM(ADSKY_CRK,INOD_CRK,IAD_ELEM,
     .                         FR_ELEM,PROCNE_CRK,LISENDP_CRK,
     .                         LIRECVP_CRK)
        ALLOCATE(ISENDTO (NINTER+1,NSPMD+1))
        ALLOCATE(IRCVFROM(NINTER+1,NSPMD+1))
        ISENDTO(1:NINTER+1, 1:NSPMD+1) = 0
        IRCVFROM(1:NINTER+1, 1:NSPMD+1) = 0

        ALLOCATE(INTLIST(2*NINTER))
        ALLOCATE(INTLIST25(NINTER25))
        ALLOCATE(NISKYFI(NINTER))
        ALLOCATE(NISKYFIE(NINTER))
        ALLOCATE(COUNT_REMSLV(NINTER))
        ALLOCATE(COUNT_REMSLVE(NINTER))
        COUNT_REMSLV(1:NINTER)=0
        COUNT_REMSLVE(1:NINTER)=0
        ALLOCATE(FR_NBCC  (2,NSPMD+1))
        ALLOCATE(FR_NBCCI2(2,NSPMD+1))
        ALLOCATE(DRETRI(5*NINTER))
        ALLOCATE(XSEC(3*4*NSECT))
        ALLOCATE(IRBKIN_L(NRBYKIN))       
        ALLOCATE(ISENDP(MIN(LISENDP,1):LISENDP))
        ALLOCATE(IRECVP(MIN(LIRECVP,1):LIRECVP))
        ALLOCATE(ISENDP_PXFEM(MIN(LISENDP_PXFEM,1):LISENDP_PXFEM))
        ALLOCATE(IRECVP_PXFEM(MIN(LIRECVP_PXFEM,1):LIRECVP_PXFEM))
        ALLOCATE(ISENDP_CRK(MIN(LISENDP_CRK,1):LISENDP_CRK))
        ALLOCATE(IRECVP_CRK(MIN(LIRECVP_CRK,1):LIRECVP_CRK))
        ALLOCATE(FR_NBCC1  (2,NSPMD+1))
C
        NLOADP_HYD_INTER = 0  
        IERROR = 0
        IF(NINTLOADP > 0 ) THEN
           ALLOCATE(LOADP_HYD_INTER(NLOADP_HYD),STAT=IERROR2)
           IERROR = IERROR + IERROR2
           DO K=1,NLOADP_HYD
              IF(ILOADP(SIZLOADP*(K-1)+5) > 0 ) THEN
                 NLOADP_HYD_INTER = NLOADP_HYD_INTER + 1
                 LOADP_HYD_INTER(K) = NLOADP_HYD_INTER
              ENDIF
           ENDDO
        ENDIF

        IF(NLOADP_HYD_INTER > 0) THEN
            ALLOCATE(TAGNCONT(NLOADP_HYD_INTER,NUMNOD),STAT=IERROR2)
            IERROR = IERROR + IERROR2
            IF(IERROR/=0)THEN
              CALL ANCMSG(MSGID=158,ANMODE=ANINFO,
     .            I1=IERROR)
              CALL ARRET(2)
            END IF
            TAGNCONT = 0
        ELSE
             ALLOCATE(TAGNCONT(0,0))
        ENDIF
C
        S_LOADPINTER = NINTER*NLOADP_HYD
C
        NPRESLOAD = 0 
        IF(NLOADP_HYD > 0 ) THEN
           DO  K=1,NLOADP_HYD
              NPRESLOAD = NPRESLOAD + ILOADP(SIZLOADP*(K-1)+1)/4 
           ENDDO
           ALLOCATE(LOADP_TAGDEL(NPRESLOAD),STAT=IERROR2)  
           LOADP_TAGDEL(1:NPRESLOAD) =0
        ELSE
           ALLOCATE(LOADP_TAGDEL(0))       
        ENDIF
C
          IF((IDEL7NG>0).OR.(IRAD2R/=0).OR.(IALEMUSCL>0).OR.(PDEL>0))THEN
            SIZE_ADDCNEL = NUMNOD+1
            SIZE_CNEL = LCNEL
            NELEML = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .               NUMELR+NUMELTG+NUMELUR
            S_ELEM_STATE = NELEML
          ELSE
            SIZE_ADDCNEL = 0
            SIZE_CNEL = 0
            S_ELEM_STATE = 0
          ENDIF
          IERROR = 0
          ALLOCATE(CNEL(0:SIZE_CNEL),STAT=IERROR2)
          IERROR = IERROR + IERROR2
          ALLOCATE(ADDCNEL(0:SIZE_ADDCNEL),STAT=IERROR2)
          IERROR = IERROR + IERROR2
          IF((IDEL7NG>0).OR.(IRAD2R/=0).OR.(IALEMUSCL>0).OR.(PDEL>0))THEN
C Allocation of inverse connectivity array
            ALLOCATE(ADDTMPL(0:NUMNOD+1),STAT=IERROR2)
            IERROR = IERROR + IERROR2
            NELEML = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .               NUMELR+NUMELTG+NUMELUR
            ALLOCATE(TAGEL(1:NELEML),STAT=IERROR2)
            TAGEL(:) = 0
            IERROR = IERROR + IERROR2
            IF(IERROR/=0)THEN
              CALL ANCMSG(MSGID=158,ANMODE=ANINFO,
     .            I1=IERROR)
              CALL ARRET(2)
            END IF
            pCNEL => CNEL
            pADDCNEL => ADDCNEL
            pADDTMPL => ADDTMPL
          ELSE
            ALLOCATE(ADDTMPL(0),TAGEL(0))
          END IF
          ALLOCATE(IPARTL(NPART))
          ALLOCATE(PARTSAV2(2,NPART))
          ALLOCATE( ELEM_STATE(S_ELEM_STATE) )
          ELEM_STATE(1:S_ELEM_STATE) = .TRUE.
C
C   Working arrays for thermal analysis                                      
C
          NODADT_THERM = 0  
          IF(IDT_THERM == 1) THEN
            IF(NINTER>0.OR.NODADT>0) NODADT_THERM = 1 ! Flag for nodal thermal time step
          ENDIF
          IF(IDT_THERM == 1) THEN
             ALLOCATE(ICODT0(NUMNOD)) ! Tabs for storing initial kinimatic when thermal time step
             ALLOCATE(ICODR0(NUMNOD))
          ELSE
             ALLOCATE(ICODT0(0))
             ALLOCATE(ICODR0(0))
          ENDIF
        
          ALLOCATE(MCP_OFF(NUMNOD))
          MCP_OFF(1:NUMNOD) = 1.0

          IFTHE  = 1
          ICONDN = 1
          IF(ITHERM_FE > 0 ) THEN
            IF(IPARIT == 3 ) THEN
              IFTHE = NUMNOD+3*NUMNOD*NTHREAD
              ALLOCATE(FTHE(IFTHE), FTHESKY(LSKY))
            ELSEIF(IPARIT /= 0 ) THEN
              IFTHE = NUMNOD
              ALLOCATE(FTHE(IFTHE), FTHESKY(LSKY))
            ELSE
              IFTHE = NUMNOD*NTHREAD
              ALLOCATE(FTHE(IFTHE), FTHESKY(0))
            ENDIF
            ALLOCATE(QFRICINT(NINTER))
            QFRICINT(1:NINTER) = ZERO
            IF(NODADT_THERM == 1) THEN
               IF(IPARIT == 0 ) THEN
                  ICONDN = NUMNOD*NTHREAD
                  ALLOCATE (CONDN(ICONDN), CONDNSKY(0))
               ELSE
                  ICONDN = NUMNOD
                  ALLOCATE (CONDN(ICONDN),STAT=IERR)
                  ALLOCATE (CONDNSKY(LSKY),STAT=IERR)
               ENDIF                  
            ENDIF
          ELSE
            IFTHE  = 1
            ICONDN = 1
            ALLOCATE(FTHE(IFTHE), FTHESKY(0))
            ALLOCATE(QFRICINT(0))
            ALLOCATE(CONDN(ICONDN),CONDNSKY(0))
          ENDIF
C
          IF(INTHEAT > 0  ) THEN
            IF(IPARIT /= 0 )THEN
              ALLOCATE(FTHESKYI(LSKYI))
              FTHESKYI(1:LSKYI) = 0 
            ELSE
              ALLOCATE(FTHESKYI(0))
            ENDIF
            IF(NODADT_THERM == 1) THEN  
              IF(IPARIT /= 0 )THEN
               ALLOCATE(CONDNSKYI(LSKYI))
              ELSE
               ALLOCATE(CONDNSKYI(0))
              ENDIF
            ELSE
              ALLOCATE(CONDNSKYI(0))
            ENDIF
          ELSE
            ALLOCATE(FTHESKYI(0))
            ALLOCATE(CONDNSKYI(0))
          ENDIF
C
C  ply xfem for composite shell
C
             IF(IPLYXFEM > 0 ) THEN
               IF(IPARIT /= 0 ) THEN
                 ALLOCATE(PLYSKY(NPLYMAX))
cc                 ALLOCATE(PLYSKYI)
                 DO I=1,NPLYMAX
                   NULLIFY(PLY(I)%A,PLYSKY(I)%FSKY)
                   ALLOCATE(PLY(I)%A(4,NPLYXFE),
     .                      PLYSKY(I)%FSKY(4,LSKYPXFEM))
                   PLYSKY(I)%FSKY = ZERO
                   PLY(I)%A = ZERO 
                   NULLIFY(PLY(I)%ITAG)
                   ALLOCATE(PLY(I)%ITAG(NPLYXFE))
                   PLY(I)%ITAG = 0
                 ENDDO
               ELSE
C is not available iparit = 0
                 DO I=1,NPLYMAX
                     NULLIFY(PLY(I)%A)
                     ALLOCATE(PLY(I)%A(4,NPLYXFE*NTHREAD))
                     PLY(I)%A  = ZERO
                 ENDDO 
                 ALLOCATE(PLYSKY(0))
               ENDIF
               IF(ANIM_PLY > 0) THEN
                 ALLOCATE(VN_NOD(3,NPLYXFE))
                 VN_NOD = ZERO
               ELSE
                 ALLOCATE(VN_NOD(0,0))
               ENDIF
             ELSE
               ALLOCATE(PLY(0),PLYSKY(0))
             ENDIF
C for interface type 24 + pxfem  
             IF(INTPLYXFEM > 0 ) THEN
                 IF(IPARIT /= 0 ) THEN
                   ALLOCATE(PLYSKYI)
                   NULLIFY(PLYSKYI%FSKYI)
                   ALLOCATE(PLYSKYI%FSKYI(LSKYI,5))
                    PLYSKYI%FSKYI = ZERO
                 ELSE
                   ALLOCATE(PLYSKYI)
                 ENDIF
                 ELSE
              ENDIF
C
        IF(IRIGID_MAT > 0 ) THEN
            ALLOCATE(VRBYM(3*NRBYM),VRRBYM(3*NRBYM),
     .            ARBYM(3*NRBYM),ARRBYM(3*NRBYM))
            VRBYM  = ZERO
            VRRBYM = ZERO
            ARBYM  = ZERO
            ARRBYM = ZERO
        ELSE
          ALLOCATE(VRBYM(0),VRRBYM(0),ARBYM(0),ARRBYM(0))
        ENDIF

C ----------------------------------------------
C    SIMPLIFIED ALE FORMULATION
C ----------------------------------------------
           IF( IALELAG > 0) THEN
              IF(IPARIT == 0) THEN
                  ALLOCATE (AFLOW(3*NUMNOD*NTHREAD))
                  ALLOCATE(FFSKY(0))
                  ALLOCATE(IFOAM(NUMNOD*NTHREAD))
                  ALLOCATE(IFOAM_CONT(NUMNOD*NTHREAD))
               ELSE
                  ALLOCATE (AFLOW(3*NUMNOD))
                  ALLOCATE(FFSKY(3*LSKY))
                   ALLOCATE(IFOAM(NUMNOD))
                   ALLOCATE(IFOAM_CONT(NUMNOD))
                   FFSKY = ZERO
              ENDIF
              AFLOW = ZERO
              IFOAM = 0
              IFOAM_CONT =0
              DO I=1,NUMNOD
                MSF(I) =  MSNF(I)
              ENDDO
           ELSE
             ALLOCATE(AFLOW(0),VFLOW(0),
     .                     DFLOW(0),WFLOW(0),
     .                     FFSKY(0),IFOAM(0),
     .                     IFOAM_CONT(0) )
           ENDIF
C
        IF(NADMESH/=0)THEN
            ALLOCATE(LSH4ACT(NUMELC),LSH4KIN(NUMELC),
     .               PSH4ACT(0:LEVELMAX+1),PSH4KIN(0:LEVELMAX+1),
     .               LSH3ACT(NUMELTG),LSH3KIN(NUMELTG),
     .               PSH3ACT(0:LEVELMAX+1),PSH3KIN(0:LEVELMAX+1),
     .               STAT=IERROR)
C
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=153,ANMODE=ANINFO,
     .            I1=IERROR)
              CALL ARRET(2)
            END IF

            IF (IPARIT/=0) THEN
              ALLOCATE(MSH4SKY(NUMELC),MSH3SKY(NUMELTG),
     .               STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=153,ANMODE=ANINFO,
     .            I1=IERROR)
                CALL ARRET(2)
              END IF
            ELSE
              ALLOCATE(MSH4SKY(0),MSH3SKY(0))
            END IF
            ALLOCATE(ILEVNOD(0:NUMNOD),STAT=IERR)
            IF(IERR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            END IF
          END IF
C
        IF(ISTATCND /= 0)THEN
            ALLOCATE(LSH4UPL(NUMELC),LSH3UPL(NUMELTG),
     .               PSH4UPL(0:LEVELMAX),PSH3UPL(0:LEVELMAX),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=159,ANMODE=ANINFO,
     .            I1=IERROR)
              CALL ARRET(2)
            END IF
            ALLOCATE(ACND(3,NUMNOD),ARCND(3,NUMNOD),
     .               STCND(NTHREAD*NUMNOD) ,STRCND(NUMNOD),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=159,ANMODE=ANINFO,
     .            I1=IERROR)
              CALL ARRET(2)
            END IF
        ELSE
            ALLOCATE(LSH4UPL(0),LSH3UPL(0),PSH4UPL(0),PSH3UPL(0))
            ALLOCATE(ACND(0,0),ARCND(0,0),
     .               STCND(0) ,STRCND(0) )
        END IF
c
        IF( (ANIM_N(18) /= 0 .OR. H3D_DATA%N_SCAL_STIFR /= 0) .AND. IRODDL /= 0) THEN
          ALLOCATE(STIFR_TMP(NUMNOD)) 
        ELSE
          ALLOCATE(STIFR_TMP(0)) 
        ENDIF
        IF( ANIM_N(19) /= 0 .OR. H3D_DATA%N_SCAL_STIFN /= 0) THEN
          ALLOCATE(STIFN_TMP(NUMNOD))
        ELSE
          ALLOCATE(STIFN_TMP(0))
        ENDIF
C--------------------------------------------
C       Error estimation / not on Restart file.
C--------------------------------------------
          IF(ANIM_CE(2156)/=0 .OR. IADMERRT/=0)THEN
             ALLOCATE(ERR_THK_SH4 (NUMELC))
             ALLOCATE(ERR_THK_SH3 (NUMELTG))
          ELSE
             ALLOCATE(ERR_THK_SH4 (0))
             ALLOCATE(ERR_THK_SH3 (0))
          END IF
C
          IF(ITHKNOD/=0)THEN
            ALLOCATE(THKSH4 (NUMELC),THKSH3 (NUMELTG),
     .               THKNOD (NUMNOD),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            END IF
          ELSE
             ALLOCATE(THKSH4 (0))
             ALLOCATE(THKSH3 (0))
             ALLOCATE(THKNOD (0))
          END IF
C----------------------------------------------------------
C    SELECTIVE MASS SCALING
C----------------------------------------------------------
C for chkpt :: ISMSCH=1
          ISMSCH  =1
          IF(IDTMINS /= 0)THEN
            ALLOCATE(TAGNOD_SMS(NUMNOD),NATIV_SMS(NUMNOD),
     .               TAGPRT_SMS(NPART),TAGREL_SMS(NGROUP),
     .               INDX1_SMS(NUMNOD),INDX2_SMS(NUMNOD),
     .               TAGSLV_RBY_SMS(NUMNOD),TAGMSR_RBY_SMS(NUMNOD),
     .               KAD_SMS(NUMNOD+1), JAD_SMS(NUMNOD+1), IAD_SMS(NUMNOD+1), LAD_SMS(NUMNOD+1),
     .                  JADC_SMS(4*NUMELC),
     .                  JADS_SMS(8*NUMELS), JADS10_SMS(6*NUMELS10),
     .                  JADT_SMS(2*NUMELT),
     .                  JADP_SMS(2*NUMELP),
     .                  JADR_SMS(3*NUMELR),
     .                  JADTG_SMS(3*NUMELTG), 
     .               X_SMS(3,NUMNOD), P_SMS(3,NUMNOD),
     .               Y_SMS(3,NUMNOD), Z_SMS(3,NUMNOD),
     .               PREC_SMS(NUMNOD), XMOM_SMS(3,NUMNOD),
     .               PREC_SMS3(3,NUMNOD),DIAG_SMS3(3,NUMNOD),
     .               T2MAIN_SMS(6,NUMNOD),
     .               T2FAC_SMS(NUMNOD),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF
            FR_RMS(1:NSPMD+1)=0
            FR_SMS(1:NSPMD+1)=0
            T2MAIN_SMS = 0
            T2FAC_SMS = ONE
          ELSEIF(IDTMINS_INT/=0)THEN
            ALLOCATE(TAGNOD_SMS(NUMNOD),NATIV_SMS(0),
     .               TAGPRT_SMS(0),TAGREL_SMS(0),
     .               INDX1_SMS(NUMNOD),INDX2_SMS(NUMNOD),
     .               TAGSLV_RBY_SMS(NUMNOD),TAGMSR_RBY_SMS(NUMNOD),
     .               KAD_SMS(0), JAD_SMS(0), LAD_SMS(0),
     .                  JADC_SMS(0),
     .                  JADS_SMS(0), JADS10_SMS(0),
     .                  JADT_SMS(0),
     .                  JADP_SMS(0),
     .                  JADR_SMS(0),
     .                  JADTG_SMS(0), 
     .               X_SMS(3,NUMNOD), P_SMS(3,NUMNOD),
     .               Y_SMS(3,NUMNOD), Z_SMS(3,NUMNOD),
     .               PREC_SMS(NUMNOD), XMOM_SMS(3,NUMNOD),
     .               PREC_SMS3(3,NUMNOD),DIAG_SMS3(3,NUMNOD),
     .               T2MAIN_SMS(6,NUMNOD),
     .               T2FAC_SMS(NUMNOD),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF
            TAGNOD_SMS(1:NUMNOD)=0
            FR_RMS(1:NSPMD+1)=0
            FR_SMS(1:NSPMD+1)=0
            T2MAIN_SMS = 0
            T2FAC_SMS = ONE
          ELSE
            ALLOCATE(TAGNOD_SMS(0),NATIV_SMS(0),
     .               TAGPRT_SMS(0),TAGREL_SMS(0),
     .               INDX1_SMS(0),INDX2_SMS(0),
     .               TAGSLV_RBY_SMS(0),TAGMSR_RBY_SMS(0),
     .               KAD_SMS(0), JAD_SMS(0), LAD_SMS(0),
     .               JADC_SMS(0),
     .               JADS_SMS(0), JADS10_SMS(0),
     .               JADT_SMS(0),
     .               JADP_SMS(0),
     .               JADR_SMS(0),
     .               JADTG_SMS(0), 
     .               X_SMS(0,0), P_SMS(0,0),
     .               Y_SMS(0,0), Z_SMS(0,0), PREC_SMS(0),
     .               XMOM_SMS(0,0), PREC_SMS3(0,0),DIAG_SMS3(0,0),
     .               T2MAIN_SMS(0,0),
     .               T2FAC_SMS(0))
C init pointer
                     PTR_SMS => NATIV_SMS
          END IF
        IF(IDTMINS == 2 .AND. NINTSTAMP /=0)THEN
            ALLOCATE(JADI21_SMS(NINTSTAMP),
     .               TAGSLV_I21_SMS(NUMNOD),TAGMSR_I21_SMS(NUMNOD),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF
        ELSE
            ALLOCATE(JADI21_SMS(0),
     .               TAGSLV_I21_SMS(0),TAGMSR_I21_SMS(0))
        END IF
C --
        IF(IDTMINS /= 0 .OR. IDTMINS_INT /= 0)THEN
          ALLOCATE(NODXI_SMS(NUMNOD),NODII_SMS(NUMNOD),
     .             STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
        ELSE
          ALLOCATE(NODXI_SMS(0),NODII_SMS(0))
        END IF
C --
        IF(IDTMINS == 2 .OR. IDTMINS_INT /= 0)THEN
          ALLOCATE(MSKYI_SMS(LSKYI_SMS),ISKYI_SMS(LSKYI_SMS,3),
     .         JADI_SMS(NUMNOD+1),JDII_SMS(2*LSKYI_SMS),
     .         LTI_SMS(2*LSKYI_SMS),
     .         STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
        ELSE
          ALLOCATE(MSKYI_SMS(0),ISKYI_SMS(0,0),
     .             JADI_SMS(0),JDII_SMS(0),LTI_SMS(0))
        END IF

         IF (IREAC == 1 ) THEN
            ALLOCATE(NODREAC(NUMNOD))
          ELSE
            ALLOCATE(NODREAC(0))
          ENDIF

          IF (IGRELEM == 1) THEN
            NGPE = NTHPART
            NGRTH = NTHPART
              NELEM=NUMELSG+3*NUMELS16G+NUMSPHG+
     .              NUMELCG+NUMELTGG+NUMELQG+NUMELURG+
     .              NUMELTG + NUMELPG  + 2*NUMELRG
          ELSE
            NGPE = 0
            NGRTH = 0
            NELEM = 0
          ENDIF
C
          IF (IGRELEM == 1 ) THEN
            ALLOCATE(GRTH(NELEM+NGRTH+1))
            ALLOCATE(IGRTH(NELEM+1))
          ELSE
            ALLOCATE(GRTH(1))
            ALLOCATE(IGRTH(1))
          ENDIF
         IGRTH = 0
         GRTH = 0
         DXANCG = ZERO
C------------------------------------------
C     ALLOCATION IGROUPC AND IGROUPTG 
C     TABLE GIVING GROUP NUMBER FOR SHELLS
C     ALLOCATION IGROUPS FOR BRICKS
C------------------------------------------
      IGROUPFLG(1:2)=0
      IF(NVOLU > 0) IGROUPFLG(1) = 1
      DO I=1,NUMMAT
        IF(IPM(2,I)/=19.AND.IPM(2,I)/=58) CYCLE
        IF(IPM(4,I) >= 4) IGROUPFLG(1)=1
      ENDDO
      IF(IGROUPFLG(1) == 1) THEN
        ALLOCATE(IGROUPC(NUMELC))
        ALLOCATE(IGROUPTG(NUMELTG))
      ELSE
        ALLOCATE(IGROUPC(0))
        ALLOCATE(IGROUPTG(0))
      ENDIF
      IGROUPFLG(2)=1
      ALLOCATE(IGROUPS(NUMELS))
C-----------------------------------------------
C /IMPDISP/FGEO
      FXVEL_FGEO = 0
C-----------------------------------------------
C     USER WINDOWS
C-----------------------------------------------
      IF(IUSERWI /= 0)THEN
        ALLOCATE(A_SAV(3,NUMNOD))
        A_SAV(1:3,1:NUMNOD)=ZERO
        ALLOCATE(AR_SAV(3,NUMNOD))
        AR_SAV(1:3,1:NUMNOD)=ZERO
      ELSE
        ALLOCATE(A_SAV(0,0))
        ALLOCATE(AR_SAV(0,0))
      ENDIF
C-----------------------------------------------
C     End Allocations Phase 1
C-----------------------------------------------
C INTERFACES RENUM
      RNUM_SIZ=NUMNOD
      ALLOCATE(RENUM(RNUM_SIZ))
C-----------------------------------------------
C     Sensor Inter
C-----------------------------------------------
      NISUBMAX = 0
      DO I=1,NINTER
        NISUBMAX = MAX(NISUBMAX,IPARI(36,I))
      ENDDO
      ALLOCATE(ISENSINT(NISUBMAX+1,NINTER))
      ISENSINT(1:NISUBMAX+1,1:NINTER) = 0
C-----------------------------------------------
      IDEL7NOK_SAV=IDEL7NOK
C-----------------------------------------------

      CALL TRACE_IN(6,0,ZERO)

C-----------------------------------------------
C     Initialisations
C-----------------------------------------------

      L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
      L2 = L1+6*NUMELS10
      L3 = L2+12*NUMELS20
      LL1 = 1+8*NUMELS
      LL2 = LL1+6*NUMELS10
      LL3 = LL2+12*NUMELS20
      IF(NADMESH/=0.AND.IDEL7NG>=1)THEN
        ALLOCATE(TAGTRIMC(NUMELC))
        ALLOCATE(TAGTRIMTG(NUMELTG))
      ELSE
        ALLOCATE(TAGTRIMC(0))
        ALLOCATE(TAGTRIMTG(0))
      ENDIF

C NITSCHE METHOD
       NFACNIT =0
       IF (NITSCHE > 0 ) THEN

c  Element mean stress 
           ALLOCATE(STRESSMEAN(6,NUMELS))

c  Equivalent nodal force  
           IF(IPARIT /= 0 ) THEN
             IF(NUMELS10G ==0) THEN
               NFACNIT = 6 
               ALLOCATE(FORNEQSKY(18*LSKY))
               FORNEQSKY(1:18*LSKY) = ZERO
             ELSE
               NFACNIT = 16 
               ALLOCATE(FORNEQSKY(48*LSKY))
               FORNEQSKY(1:48*LSKY) = ZERO
             ENDIF
           ELSE
             ALLOCATE(FORNEQSKY(0))
           ENDIF
           STRESSMEAN(1:6,1:NUMELS)=ZERO
       ELSE
           ALLOCATE(STRESSMEAN(0,0))
           ALLOCATE( FORNEQSKY(0))
       ENDIF

       CALL NEWSKW_INIT(ISKWP,NUMSKW_L,NSKWP,NUMSKW_L_SEND,ISKWP_L_SEND,RECVCOUNT)
       PARTSAV2(1:2,1:NPART) = ZERO

       ! Initialization of INTER_STRUCT structure + allocation
       ALLOCATE( INTER_STRUCT(NINTER) )
       ALLOCATE( SORT_COMM(NINTER) )
       CALL INTER_STRUCT_INIT(INTER_STRUCT,SORT_COMM)
       


C========================================================================================
C Initialize SENSORS & communication buffers.
C Play Time & Logical sensors.


       K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
       K2=K1+NUMELS
       K3=K2+NUMELQ
       K4=K3+NUMELC
       K5=K4+NUMELT
       K6=K5+NUMELP
       K7=K6+NUMELR
       K8=K7+NUMELUR
       K9=K8+NUMELTG
       K10=K9+NUMELX
       K11=K10+NUMSPH
       K12=K11+NUMELIG3D

       CALL SENSOR_INIT(SUBSETS,IPARG,NGROUC,
     1                  IPART(K3),IPART(K8),IPART(K1),IPART(K2),IPART(K4),
     2                  IPART(K5) ,IPART(K6),SENSOR_TAB)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,N1,GREFTSK,GRELTSK)
C Init var parallel SMP
        CALL SMP_INIT(
     1   ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK,
     2   IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)
C
        N1 =  1 + NUMNOD + ITSK*NUMNOD*3
        CALL RESOL_INIT(
     1   ITSK     ,IADS(LL1),IADS(LL2),IADS(LL3),FR_NBCC  ,
     2   ISENDTO  ,IRCVFROM ,IAD_ELEM ,FR_ELEM  ,ITABM1   ,
     3   IPARI    ,IPARG    ,ITAB     ,IXS(L1)  ,IXS(L2)  ,
     4   I13A     ,I13B     ,I13C     ,I13D     ,I13E     ,
     5   I13F     ,I13G     ,I13H     ,I13I     ,I15A     ,
     6   I15B     ,I15C     ,I15D     ,I15E     ,I15F     ,
     7   I15G     ,I15H     ,I15I     ,I87A     ,I87B     ,
     8   I87C     ,I87D     ,I87E     ,I87F     ,I87G     ,
     9   NFIA     ,NFEA     ,NFOA     ,NDMA     ,NDMA2    ,
     A   NODFTSK  ,NODLTSK  ,NDTSK    ,NUMNTSK  ,IXS(L3)  ,
     B   IXS      ,IXQ      ,IXC      ,IXT      ,IXP      ,
     C   IXR      ,IXTG     ,ADSKY    ,IADS     ,IKINE    ,
     D   FSKY     ,A        ,AR       ,V        ,VR       ,
     E   X        ,D        ,MS       ,IN       ,STIFN    ,
     F   STIFR    ,DMAS     ,DINER    ,FANI     ,ANIN     ,
     G             WA       ,UWA      ,PM       ,GEO      ,
     H   PARTSAV  ,PARTS0   ,MONVOL   ,
     I   I87H     ,I87I     ,I87J     ,I87K     ,IXUR     ,
     J   I15J     ,KXX      ,
     K   SECBUF   ,SECFCUM  ,NSTRF    ,IGRNOD   ,IEXLNK   ,
     L   XFRAME   ,A(1,N1)  ,AR(1,N1) ,STIFN(N1),STIFR(N1),
     M   FSKYM    ,IXTG1    ,IBCL     ,VISCN    ,DD_R2R   ,
     O   VISCN(N1),ELBUF    ,IPART    ,MADPRT   ,MADSH4   ,
     P   MADSH3   ,MADSOL   ,MADNOD   ,MADFAIL  ,IGEO     ,
     Q   INTLIST  ,NBINTC   ,PROCNE   ,NISKYFI  ,WEIGHT   ,
     R   ISIZXV   ,ILENXV   ,ADDCNI2  ,PROCNI2  ,IAD_I2M  ,
     S   FR_I2M   ,FR_NBCCI2,I2SIZE   ,FR_MAD   ,LWIBEM   ,
     T   LWRBEM   ,FXBFP    ,FXBEFW   ,FXBEDP   ,FXBGRP   ,
     U   FXBGRW   ,NDIN     ,IADSDP   ,IADRCP   ,ISENDP   ,
     V   IRECVP   ,ISLEN7   ,IRLEN7   ,ISLEN11  ,IRLEN11  ,
     W   LWIFLOW  ,LWRFLOW  ,IFLOW    ,ADDCNEL  ,CNEL     ,
     X   ADDTMPL  ,IPARTL   ,NPARTL   ,NFNCA    ,NFTCA    ,
     Y   I15ATH   ,I35ATH   ,IPM      ,SH4TREE  ,IPADMESH ,
     Z   MSC      ,INC      ,SH3TREE  ,MSTG     ,INTG     ,
     a   PTG      ,FTHE     ,FTHESKY  ,FTHESKYI ,NME17    ,
     b   ISLEN17  ,IRLEN17  ,IRLEN7T  ,ISLEN7T  ,LINDIDEL ,
     c   LBUFIDEL ,SH4TRIM  ,SH3TRIM  ,MSCND    ,INCND    ,
     d   IRLEN20  ,ISLEN20  ,IRLEN20T ,ISLEN20T ,NBINT20  ,
     e   IRLEN20E ,ISLEN20E ,NISKYFIE ,IRBE3    ,R3SIZE   ,
     f   MCP      ,MS0      ,INOD_PXFEM,IEL_PXFEM,IADC_PXFEM,
     g   ADSKY_PXFEM,ICODT  ,ICODR    ,IBFV     ,ADMSMS    ,
     h   ITHGRP   ,ITHBUF   ,NODREAC  ,IGROUC   ,NGROUC    ,
     i   IGROUNC  ,NGROUNC  ,FR_RBY   ,FR_RBY6  ,NPBY      ,
     j   NOM_SECT ,MCPC     ,MCPTG    ,GRTH     ,IGRTH     ,
     k   NELEM    ,LAG_SEC  ,NPRW     ,DIAG_SMS ,DMELC     ,
     l   DMELTG   ,NGRTH    ,NFT2     ,DMELS    ,DMELTR    ,
     m   DMELP    ,DMELRT   ,RES_SMS  ,I87L     ,IRBE2     ,
     n   LRBE2    ,NMRBE2   ,IAD_RBE2 ,FR_RBE2  ,FR_RBE2M  ,
     o   R2SIZE   ,LPBY     ,PROCNE_PXFEM,ISENDP_PXFEM,IRECVP_PXFEM ,
     p   IADSDP_PXFEM,IADRCP_PXFEM,FR_NBCC1,RBY ,INT18KINE ,
     q   XDP      ,I87M     ,INOD_CRK  ,IEL_CRK ,IADC_CRK,
     r   ADSKY_CRK,PROCNE_CRK,ISENDP_CRK,IRECVP_CRK,
     s   IADSDP_CRK,IADRCP_CRK ,INT24USE,NDAMA2    ,
     t   IGROUPC  ,IGROUPTG ,IGROUPS   ,IGROUPFLG ,DMINT2  ,IRBKIN_L ,
     u   NRBYKIN_L,KINDRBY  ,ELBUF_TAB ,SENSOR_TAB,DD_R2R_ELEM,
     v   SDD_R2R_ELEM,KINET, WEIGHT_MD ,DMSPH   ,IOLDSECT,LBUFSEGLO,
     w   INTBUF_TAB  ,NUMSPH_GLO_R2R, FLG_SPHINOUT_R2R,I15K,
     y   CONDN       ,CONDNSKY,KXFENOD2ELC  ,ELCUTC ,NODEDGE,
     z   IAD_EDGE ,CRKNODIAD,FR_EDGE     ,FR_NBEDGE ,NODLEVXF,
     x   CRKEDGE  ,XFEM_TAB  ,ISENSINT ,NISUBMAX  ,
     1   STABSEN  ,TABSENSOR, INTLIST25 ,INT24E2EUSE,TABMP_L ,
     2   I87N ,TAB_MAT,H3D_DATA,TAGTRIMC,TAGTRIMTG ,
     3   IGRBRIC ,IGRQUAD  ,IGRSH4N  ,IGRSH3N  ,IGRTRUSS   ,
     4   IGRBEAM ,IGRSPRING,IGRPART  ,FORNEQS  ,INT7ITIED,
     5   FXVEL_FGEO,FAILWAVE,NLOC_DMG,PINCH_DATA ,SLLOADP,
     6   TAGSLV_RBY,NFNCA2  ,NFTCA2  ,IN0 ,SORT_COMM,STACK )


!$OMP END PARALLEL

C========================================================================================
C                                  NON PARALLEL SECTION (SMP)
C========================================================================================
       CALL SPLIT_ASSPAR4(ADSKY,NUMNOD,NTHREAD,NODFT_ASSPAR,NODLT_ASSPAR,SADSKY)

       ! Allocation & initialization of /INT18 + LAW151
       CALL INT18_LAW151_ALLOC(MULTI_FVM,IPARI)
       CALL INT18_LAW151_INIT(MULTI_FVM,IGRBRIC,IPARI    ,IXS,
     1                        IGROUPS  ,IPARG  ,ELBUF_TAB,MULTI_FVM%FORCE_INT         ,
     2            X       ,          V       ,          MS         ,          KINET       ,
     3  MULTI_FVM%X_APPEND,MULTI_FVM%V_APPEND,MULTI_FVM%MASS_APPEND,MULTI_FVM%KINET_APPEND,
     4  MULTI_FVM%FORCE_INT_PON)
       ! allocation & initialization for /FAIL/ALTER
       CALL FAIL_WIND_FRWAVE_INIT(NGROUP)

C     End Initialisations //
       IF(NADMESH/=0.AND.IDEL7NG>=1) IDEL7NOK = 0
C     THERMAL TIME STEP : we don't check incompatible kinematic conditions anymore
c      we constraint all DDLs
       IF(IDT_THERM == 1) THEN
          CALL BCSDTTH_COPY(ICODT, ICODR, ICODT0, ICODR0 ,1 )
       ENDIF
       
       I24MAXNSNE=0
       IF (INT24USE == 1)THEN
         ALLOCATE(IAD_I24(NBINTC+1,NSPMD))
         SFR_I24=0
         CALL  SPMD_I24_PREPARE(1,IPARI, INTBUF_TAB, 
     *                         IAD_ELEM, FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I24 , SFR_I24, IDUM,I24MAXNSNE)
         ALLOCATE(FR_I24(SFR_I24))
         CALL  SPMD_I24_PREPARE(2,IPARI, INTBUF_TAB, 
     *                         IAD_ELEM, FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I24 , SFR_I24, FR_I24,I24MAXNSNE)
C E2E Fictive Node Position, Velocity, Mass
         CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTBUF_TAB,
     *                                     X,V,MS,ITAB)
 
       ELSE
         ALLOCATE(IAD_I24(1,1))
       ENDIF

      IF(NINTER25 /= 0)THEN
         ALLOCATE(IAD_I25(NBINTC+1,NSPMD))
         CALL  SPMD_I25_PREPARE(1,IPARI, INTBUF_TAB, 
     *                         IAD_ELEM, FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I25 , SFR_I25, IDUM)

         ALLOCATE(FR_I25(SFR_I25))
         CALL  SPMD_I25_PREPARE(2,IPARI, INTBUF_TAB, 
     *                         IAD_ELEM, FR_ELEM,INTLIST,NBINTC,
     *                         IAD_I25 , SFR_I25, FR_I25)
 
      ELSE
         ALLOCATE(IAD_I25(1,1))
      ENDIF
      ALLOCATE(FSKYN25 (3,NBCCNOR))
      FSKYN25=0.
C     Multidomains + SPH
      IF ((NUMSPH_GLO_R2R>0).AND.(FLG_SPHINOUT_R2R==1)) THEN
        ALLOCATE(OFF_SPH_R2R(NUMNOD))
        OFF_SPH_R2R(:) = 0
      ENDIF

C     THERMAL EXPANSION and IMPTEMP : Initialisation of nodal temperatures
      IF(NFXTEMP  > 0 .AND. ITHERM_FE > 0.AND.TT==ZERO) THEN
        CALL FIXTEMP(IBFTEMP  ,FBFTEMP    ,TEMP    ,NPC    ,TF  ,
     .      SENSOR_TAB )
      ENDIF

C ---------------------
C Allocating FEXT Array
C ---------------------
      IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .   ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
        ALLOCATE(FEXT(3*NUMNOD))
        FEXT(1:3*NUMNOD)=ZERO
      ELSE
        ALLOCATE(FEXT(3))
        FEXT(1:3)=ZERO
      ENDIF
C ---------------------
      IF (ANIM_V(19) + H3D_DATA%N_VECT_CLUST_FORCE > 0) THEN
        ALLOCATE(FCLUSTER(3*NUMNOD))
        FCLUSTER(1:3*NUMNOD)=ZERO
      ELSE
        ALLOCATE(FCLUSTER(3))
        FCLUSTER(1:3)=ZERO
      ENDIF
      IF (ANIM_V(20) + H3D_DATA%N_VECT_CLUST_MOM > 0) THEN
        ALLOCATE(MCLUSTER(3*NUMNOD))
        MCLUSTER(1:3*NUMNOD)=0
      ELSE
        ALLOCATE(MCLUSTER(3))
        MCLUSTER(1:3)=0
      ENDIF
      
      ! -------------------------
      ! initialization of SHOOT_STRUCT for 
      ! the deactivation node algo
      CALL INIT_NODAL_STATE( IPARI,SHOOT_STRUCT,INTBUF_TAB,IAD_ELEM,FR_ELEM,
     .                       ITAB,ITABM1,GEO,ADDCNEL,CNEL,
     .                       IXS,IXC,IXT,IXP,IXR,IXTG,
     .                       SIZE_ADDCNEL,SIZE_CNEL )
      ! -------------------------
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

        CALL TRACE_OUT(6)
c-----------------------------------------------
c     INITIALISATION XFEM 
c-----------------------------------------------
      IF (ICRACK3D > 0 .and. TT == ZERO) THEN                                            
        CALL INIXFEM(ELBUF_TAB   ,XFEM_TAB   ,                                      
     .    IPARG     ,IXC         ,IXTG       ,NGROUC      ,IGROUC       , 
     .    ELCUTC    ,IADC_CRK    ,IEL_CRK    ,INOD_CRK    ,ADSKY_CRK    , 
     .    X         ,KXFENOD2ELC ,NODEDGE    ,CRKNODIAD   ,IAD_EDGE     ,
     .    FR_EDGE   ,FR_NBEDGE   ,NODLEVXF   ,CRKEDGE     ,XEDGE4N      ,
     .    XEDGE3N   )                                                  
      END IF                                                            
C-----------------------------------------------
C       Allocations Phase 2
C-----------------------------------------------
        ALLOCATE(FTHREAC(6*CPTREAC))
        IF (IMPL_S==1) THEN
          ALLOCATE(FTHDTM(6*CPTREAC))
        ELSE
          ALLOCATE(FTHDTM(0))
        ENDIF
C
        COMPTREAC = 0
        CALL NEED_FOR_REACTIONS(NPBY,H3D_DATA,COMPTREAC) ! Look for options for anim, ... and rbody with failure
        IF(COMPTREAC == 1)THEN
          MY_ALLOCATE(FREAC,6*NUMNOD) 
          FREAC(1:6*NUMNOD)=ZERO
        ELSE
          ALLOCATE(FREAC(0))
        ENDIF
C
        FTHREAC = ZERO
        FTHDTM  = ZERO
        IF (NTHPART > 0) THEN
          ALLOCATE(GRESAV(NPSAV*NGPE*NTHREAD))
        ELSE
          ALLOCATE(GRESAV(1))
        ENDIF
        GRESAV = ZERO
C       ALLOCATION Flying Nodes if IDEL 
        IF(IDEL7NG>0)THEN
          IERROR = 0
          ALLOCATE(IBUFIDEL(LBUFIDEL),STAT=IERROR2)
          IERROR = IERROR + IERROR2
          ALLOCATE(INDIDEL(LINDIDEL),STAT=IERROR2)
          IERROR = IERROR + IERROR2
          IF(INT24USE==1.OR.NINTER25/=0)THEN
             ALLOCATE (IBUFSEGLO(LBUFSEGLO),STAT=IERROR2)
             ALLOCATE (INDSEGLO(NINTER+1),STAT=IERROR2)
          ENDIF
          IF(IERROR/=0)THEN
            CALL ANCMSG(MSGID=158,ANMODE=ANINFO,
     .            I1=IERROR)
            CALL ARRET(2)
          ENDIF
        ELSE
          ALLOCATE(IBUFIDEL(0)  ,STAT=IERROR2)
          ALLOCATE(INDIDEL(0)   ,STAT=IERROR2)         
      END IF

      IF(NSPMD > 1.AND.PDEL > 0)  THEN
        ALLOCATE(IBUFPDEL(4*NCONLD+4*NPRESLOAD),STAT=IERR) 
        ALLOCATE(NINDEXPDEL(NCONLD+NPRESLOAD),STAT=IERR)
      ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C------------IF adaptive Mesh : check if there are elements eroded after trimming to remove nodes from interface------
       IF(NADMESH/=0.AND.IDEL7NG>=1)THEN

          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
          IF((INT24USE==1.OR.NINTER25/=0).AND.IDEL7NOK==1)THEN
              INDSEGLO(2:NINTER+1)=0
              INDSEGLO(1)=1
          ENDIF

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,N1,GREFTSK,GRELTSK)
C Init var parallel SMP
        CALL SMP_INIT(
     1   ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK,
     2   IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

          CALL TAGOFF3N(
     1       GEO        ,IXS       ,IXS(L1)   ,IXS(L1)   ,IXS(L3)  ,IXQ    ,
     2       IXC         ,IXT      ,IXP       ,IXR       ,IXTG     ,IXUR   ,
     3       WA(NUMNOD+1),NODFTSK  ,NODLTSK   ,IPARG     ,ELBUF    ,ITSK   ,
     4       WA(NWAFTSK) ,IXTG1    ,IAD_ELEM  ,FR_ELEM   ,ITAB     ,ITABM1 ,
     5       ADDCNEL     ,CNEL     ,KXSP      ,ELBUF_TAB ,TAGEL    ,IEXLNK , 
     6       IGRNOD      ,DD_R2R   ,DD_R2R_ELEM,SDD_R2R_ELEM,IDEL7NOK_SAV ,
     7       IDEL7NOK_R2R,TAGTRIMC,TAGTRIMTG,S_ELEM_STATE,ELEM_STATE,
     8       SHOOT_STRUCT,SHOOT_STRUCT%GLOBAL_NB_ELEM_OFF)
          ! ---------------------
          ! check if a node is deactivated and deactivate all the corresponding secondary nodes
          CALL CHECK_NODAL_STATE( ITSK,WA(NUMNOD+1),NEWFRONT,INTBUF_TAB,SHOOT_STRUCT%SIZE_SEC_NODE,
     .           SHOOT_STRUCT%SHIFT_S_NODE,SHOOT_STRUCT%INTER_SEC_NODE,SHOOT_STRUCT%SEC_NODE_ID)
          ! ---------------------

          ! ---------------------
          ! check if a surface/edge must be deactivated and save the surface/edge id 
          IF(ITSK==0) THEN 
                CALL FIND_SURFACE_INTER( ITSK,ITAB,SHOOT_STRUCT,IXS,IXS(L1),IXC,IXTG )
                CALL FIND_EDGE_INTER( ITSK,ITAB,SHOOT_STRUCT,IXS,IXS(L1),
     1                                IXC,IXTG,IXQ,IXT,IXP,
     2                                IXR,GEO )
          ENDIF
          CALL MY_BARRIER( )
          ! ---------------------

          ! ---------------------
          ! exchange of surfaces (ie. 4 nodes) to deactivate and deactivation
          ! ONLY FOR LOCAL SURFACE / REMOTE ELEMENT
          IF(NSPMD>1) THEN 
            IF(ITSK==0) CALL SPMD_EXCH_DELETED_SURF_EDGE( IAD_ELEM,ITABM1,SHOOT_STRUCT,INTBUF_TAB,NEWFRONT,
     .                           IPARI,GEO,
     .                           IXS,IXC,IXT,IXP,IXR,IXTG,
     .                           ADDCNEL,CNEL,WA(NWAFTSK),TAGEL )
            CALL MY_BARRIER()
          ENDIF
          ! ---------------------

          ! ---------------------
          ! loop over the surface id and deactivate the surface
          ! ONLY FOR LOCAL SURFACE / LOCAL ELEMENT
          CALL CHECK_SURFACE_STATE( ITSK,SHOOT_STRUCT%SAVE_SURFACE_NB,SHOOT_STRUCT%SAVE_SURFACE,
     .                              SHOOT_STRUCT%SHIFT_INTERFACE,INTBUF_TAB )

          ! loop over the edge id and deactivate the edge
          ! ONLY FOR LOCAL EDGE / LOCAL ELEMENT
          CALL CHECK_EDGE_STATE( ITSK,SHOOT_STRUCT%SAVE_M_EDGE_NB,SHOOT_STRUCT%SAVE_S_EDGE_NB,
     .                           SHOOT_STRUCT%SAVE_M_EDGE,SHOOT_STRUCT%SAVE_S_EDGE,
     .                           SHOOT_STRUCT%SHIFT_INTERFACE,INTBUF_TAB,NEWFRONT,IPARI,GEO,
     .                           IXS,IXC,IXT,IXP,IXR,IXTG,
     .                           ADDCNEL,ITABM1,CNEL,WA(NWAFTSK),TAGEL,SHOOT_STRUCT )
          ! ---------------------

          CALL CHKSTFN3N(
     1       IPARI   ,GEO         ,IXS      ,IXQ         ,IXC     ,IXT       ,
     2       IXP     ,IXR         ,IXTG     ,WA(NUMNOD+1),IPARG   ,ITSK      ,
     3       NEWFRONT,WA(NWAFTSK) ,MS       ,IN          ,ANIN(NDMA+1),ITAB  ,
     4       ITABM1  ,ADDCNEL     , CNEL    ,INDIDEL     ,NINDEX1 ,NINDEX2   ,
     5       NINDEX3 ,NINDEX4     ,TAGEL    ,INT24USE    ,IBUFSEGLO ,INDSEGLO,
     6       IBUFIDEL ,INTBUF_TAB,IAD_ELEM)


!$OMP END PARALLEL
          CALL DEALLOC_SHOOT_INTER( SHOOT_STRUCT )
        ENDIF
      IF(ISFEM >= 1) THEN
        SVARNOD = 2*NUMNOD
      ELSE
        SVARNOD = 1
      ENDIF
      ALLOCATE(VARNOD(SVARNOD))

C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------------------------------------
C    SELECTIVE MASS SCALING
C----------------------------------------------------------
          IF(IDTMINS_INT /= 0)THEN
C
C           /DT/INTER/AMS
            CALL SMS_INI_ERR(NPRW ,LPRW ,KINET )
C
          END IF
C----------------------------------------------------------
          IF(IDTMINS /= 0)THEN
            L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
            L2 = L1+6*NUMELS10
            L3 = L2+12*NUMELS20

C NATIV_SMS read from starter state    
            NATIV_SMS(1:NUMNOD) = NATIV0_SMS(1:NUMNOD)
C
            CALL SMS_INI_PART(IGRPART ,TAGPRT_SMS)

            IF(IDTMINS_INT==0)
     .      CALL SMS_INI_RBY(
     1      KINET   ,NPRW     ,LPRW    ,NPBY    , LPBY     ,
     2      TAGMSR_RBY_SMS,TAGSLV_RBY_SMS)

            CALL SMS_INI_KAD(
     1      IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2      IXR     ,IXTG     ,IXTG1   ,IXS(L1) ,IXS(L3)   ,
     3      IXS(L2) ,IPARG    ,MS      ,MS0     ,TAGNOD_SMS,
     4      ICODT    ,ICODR   ,KINET   ,INDX1_SMS,
     5      KAD_SMS ,IPART(I15A),IPART(I15B),
     6     IPART(I15C),IPART(I15D),IPART(I15E),IPART(I15F),IPART(I15G),
     7     IPART(I15H),IPART(I15I),TAGPRT_SMS ,TAGREL_SMS ,ITAB       ,
     8     WEIGHT     ,IRBE2      ,IRBE3      ,LRBE2      ,LRBE3      ,
     9     IAD_ELEM   ,FR_ELEM    ,NPRW       ,LPRW       ,IPART      ,
     A     IGEO       ,NATIV_SMS  )

            ALLOCATE(KDI_SMS(KNZ_SMS),PK_SMS(KNZ_SMS),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF

            NSGDONE=1
            CALL SMS_INI_KDI(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,KAD_SMS ,
     4             KDI_SMS  ,JADC_SMS,JADS_SMS ,JADS10_SMS ,
     5             JADT_SMS ,JADP_SMS,
     6             JADR_SMS,JADTG_SMS,INDX1_SMS,TAGPRT_SMS,IAD_SMS ,
     7       TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     8      IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     9             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     A             TAGSLV_RBY_SMS,IPARI  ,INTBUF_TAB,IADI2,
     B             LAD_SMS   ,IPART,IGEO     ,WEIGHT    ,
     C             NATIV_SMS)

            ALLOCATE(IDI_SMS(NNZ_SMS),JDI_SMS(NNZ_SMS),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF

            NSGDONE=1
            CALL SMS_INI_JAD_1(
     2             IXC      ,IPARG   ,IXS      ,IXT       ,IXP      ,
     3             IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,JADC_SMS ,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS,JADP_SMS ,JADR_SMS ,
     5             JADTG_SMS,INDX1_SMS,TAGPRT_SMS,
     6             KAD_SMS,KDI_SMS    ,PK_SMS     ,
     7       TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     8      IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     9             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     A             TAGSLV_RBY_SMS,IPARI  ,INTBUF_TAB,IADI2,
     B             LAD_SMS   ,IPART      ,IGEO      ,WEIGHT  ,NATIV_SMS,
     C             IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS   ,T2MAIN_SMS)
            DEALLOCATE(JDI_SMS)

            ALLOCATE(JDI_SMS(NNZ_SMS),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF

            NSGDONE=1
            CALL SMS_INI_JAD_2(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,JADC_SMS ,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS,INDX1_SMS,TAGPRT_SMS,KAD_SMS ,KDI_SMS  ,
     6       TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     7      IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     8             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     9             TAGSLV_RBY_SMS,IPARI  ,INTBUF_TAB,IADI2    ,
     A             LAD_SMS   ,NPRW ,LPRW,TAGMSR_RBY_SMS,
     B         TAGSLV_I21_SMS ,TAGMSR_I21_SMS, JADI21_SMS,INTSTAMP  ,
     .                                                    IPART     ,
     C         IGEO    ,WEIGHT  ,NATIV_SMS,IRBE2      ,LRBE2     ,
     D         IAD_SMS ,IDI_SMS ,JAD_SMS  ,JDI_SMS    ,T2MAIN_SMS)
            DEALLOCATE(JDI_SMS)
            ALLOCATE(JDI_SMS(NNZ_SMS),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF
            ALLOCATE(JSM_SMS(NNZ_SMS),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF

            NSGDONE=1
            CALL SMS_INI_JAD_3(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP      ,
     3             IXR      ,IXTG    ,IXS(L1)  ,TAGNOD_SMS,JADC_SMS,
     4             JADS_SMS ,JADS10_SMS,JADT_SMS,JADP_SMS,JADR_SMS ,
     5             JADTG_SMS,INDX1_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,
     6       TAGREL_SMS,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     7      IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     8             IAD_ELEM  ,FR_ELEM,NPBY     ,LPBY     ,KINET    ,
     9             TAGSLV_RBY_SMS,IPARI  ,INTBUF_TAB,IADI2    ,
     A             LAD_SMS ,JSM_SMS ,TAGSLV_I21_SMS,INTSTAMP  ,IPART    ,
     B             IGEO    ,TAGMSR_RBY_SMS,WEIGHT,NATIV_SMS,
     C             IAD_SMS ,IDI_SMS ,JAD_SMS ,JDI_SMS    ,T2MAIN_SMS)

            DEALLOCATE(IAD_SMS,IDI_SMS)

            ALLOCATE(LTK_SMS(KNZ_SMS),LT_SMS(NNZ_SMS),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF

            ALLOCATE(TAG_LNK_SMS(NRLINK+NLINK+NJOINT),
     .               NRWL_SMS(SLPRW),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF

            IF(IDTMINS==1)THEN
C             Obsolete
            ELSE
              CALL SMS_INI_KIN_2(
     1  ILINK     ,LLINK    ,NNLINK    ,LNLINK    ,TAG_LNK_SMS,
     2  FR_LL     ,FR_RL    ,WEIGHT    ,ITAB      ,LJOINT     ,
     3  IADCJ     ,FR_CJ    ,NPRW      ,LPRW      ,FR_WALL    ,
     4  NRWL_SMS  ,IAD_ELEM ,FR_ELEM   )
            END IF

            IF(IDTMINS_INT/=0)THEN
C re-tag rigid bodies (all nodes)
              CALL SMS_INI_RBY(
     1        KINET   ,NPRW     ,LPRW    ,NPBY    , LPBY     ,
     2        TAGMSR_RBY_SMS,TAGSLV_RBY_SMS)
            END IF

            CALL SMS_INI_INT(
     1  IPARI    ,INTBUF_TAB     ,IAD_ELEM   ,FR_ELEM  ,INTLIST,
     2  NBINTC)

          ELSEIF(IDTMINS_INT/=0)THEN
C
C           /DT/INTER/AMS
            ALLOCATE(KDI_SMS(0),JDI_SMS(0),PK_SMS(0),LT_SMS(0),LTK_SMS(0),
     .               JSM_SMS(0),
     .               TAG_LNK_SMS(NRLINK+NLINK+NJOINT),NRWL_SMS(SLPRW),
     .               STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
              CALL ARRET(2)
            ENDIF

            NINDX1_SMS=0
            NSMSPCG=MIN(NSMSPCG,NUMNODG)

            CALL SMS_INI_KIN_2(
     1  ILINK     ,LLINK    ,NNLINK    ,LNLINK    ,TAG_LNK_SMS,
     2  FR_LL     ,FR_RL    ,WEIGHT    ,ITAB      ,LJOINT     ,
     3  IADCJ     ,FR_CJ    ,NPRW      ,LPRW      ,FR_WALL    ,
     4  NRWL_SMS  ,IAD_ELEM ,FR_ELEM   )

            CALL SMS_INI_RBY(
     1      KINET   ,NPRW     ,LPRW    ,NPBY    , LPBY     ,
     2      TAGMSR_RBY_SMS,TAGSLV_RBY_SMS)

            CALL SMS_INI_INT(
     1  IPARI    ,INTBUF_TAB     ,IAD_ELEM   ,FR_ELEM  ,INTLIST,
     2  NBINTC)
     
          ELSE
            ALLOCATE(KDI_SMS(0),JDI_SMS(0),PK_SMS(0),LT_SMS(0),LTK_SMS(0),
     .               JSM_SMS(0),
     .               TAG_LNK_SMS(0),NRWL_SMS(0))
          END IF

C
        IF (M_VS_SMS>0) THEN
          LNZM=M_VS_SMS+3
          ALLOCATE(PROJ_S(NUMNOD,LNZM),PROJ_T(NUMNOD,LNZM),
     .             PROJ_LA_1(LNZM),
     .                    PROJ_W(3*LNZM),PROJ_K(LNZM,LNZM),
     .             STAT=IERR)
          IF (IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
          PROJ_S = ZERO
          NCG_RUN_SMS = 0
        ELSE
          ALLOCATE(PROJ_S(0,0),PROJ_T(0,0),
     .             PROJ_LA_1(0),
     .                    PROJ_W(0),PROJ_K(0,0))
        END IF
C---------------------------------------------------------------------
C DEBUG MATRIX AMS
C---------------------------------------------------------------------
      IF (IDTMINS/=0.AND.DEBUG(MACRO_DEBUG_AMS)==1) THEN
          IF(NSPMD > 1) THEN
              IF (ISPMD==0) THEN
                 SIZ = NUMNODG
              ELSE
                 SIZ = 0
              END IF
              CALL SPMD_COLLECTM(TAGNOD_SMS,ITAB,WEIGHT,NODGLOB,SIZ)
          ELSE
            CALL COLLECTM(TAGNOD_SMS,ITAB,WEIGHT,NODGLOB)
          END IF
      END IF
C---------------------------------------------------------------------
C AMS + POFF
C---------------------------------------------------------------------
      IF(IDTMINS/=0.AND.IPARIT==0)THEN
         ALLOCATE(UNCOMP_FR(NUMNOD))
         ALLOCATE(UNCOMP_FRI2M(NUMNOD))

         CALL AMS_PREPARE_POFF_ASSEMBLY(
     *              IAD_ELEM,FR_ELEM,NB_FR,UNCOMP_FR,
     *               IAD_I2M,FR_I2M,NB_FRI2M,UNCOMP_FRI2M)

         ALLOCATE(FR_LOC(NB_FR))
         FR_LOC(1:NB_FR) = UNCOMP_FR(1:NB_FR)

         ALLOCATE(FR_LOCI2M(NB_FRI2M))
         FR_LOCI2M(1:NB_FRI2M) = UNCOMP_FRI2M(1:NB_FRI2M)


         DEALLOCATE(UNCOMP_FR)
         DEALLOCATE(UNCOMP_FRI2M)

      ELSE
         ALLOCATE(FR_LOC(1))
         ALLOCATE(FR_LOCI2M(1))
      ENDIF
      
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C ALLOCATION OF WORKING ARRAYS FOR AIRBAG BEM
C
          ALLOCATE(WIBEM(LWIBEM), WRBEM(LWRBEM), STAT=IERROR)
          IF (IERROR/=0) THEN
             CALL ANCMSG(MSGID=160,ANMODE=ANINFO,
     .            I1=IERROR)
             CALL ARRET(2)
          ENDIF
          DO I=1,LWIBEM
            WIBEM(I)=0
          ENDDO
          ALLOCATE(WIFLOW(LWIFLOW), WRFLOW(LWRFLOW), STAT=IERROR)
          IF (IERROR/=0) THEN
             CALL ANCMSG(MSGID=160,ANMODE=ANINFO,
     .            I1=IERROR)
             CALL ARRET(2)
          ENDIF
          DO I=1,LWIFLOW
            WIFLOW(I)=0
          ENDDO
C
C         Allocation interface 17
C
          ALLOCATE(EMINX(NME17*6))
C
        IF (NFXBODY>0) THEN
           DO N=1,NFXBODY
              ADRNOD=FXBIPM(6,N)
              ONOF=1
              ONFELT=0
              CALL FXBYPID(
     .            IPARG         ,         IXS , IXQ    , IXC        ,
     .            IXT           , IXP   , IXR , IXTG   , FXBIPM(1,N),
     .            FXBNOD(ADRNOD), ONOF  , WA  , ONFELT ,ELBUF_TAB   )
           ENDDO
         END IF
C

C     if no subcycling + subcycling next to restart :
C       dt2s=0 => shells computation at 1st cycle next to restart
        IF(ISHSUB==0 .AND. MCHECK==0) DT2S=0.0
        IF(MCHECK/=0) DT2SAVE = DT2
        DT2SOLD=DT2S
        LASTSH =0
C------------RBE3----allocation-------------
      IF(NRBE3>0)THEN
       IF(ITASK==0)THEN
        NMT0 = SLRBE3/2
        IROTG=0
        DO N=1,NRBE3
         IROTG=MAX(IROTG,IRBE3(6,N))
        END DO
        IF (R3SIZE>5)IROTG = 1
        IF (NMT0>0) THEN
         CALL PRERBE3P0(IRBE3 ,LRBE3 ,NMT   )
         LI1=1
         LI2=LI1+3*NMT
         LI3=LI2+NMT
         IF (IROTG>0) THEN
          LI4=LI3+NMT
          LI5=LI4+3*NMT
          LI6=LI5+NMT
         ELSE
          LI4=LI3
          LI5=LI4
          LI6=LI5
         ENDIF
         LIF=LI6+NMT
         ALLOCATE(RRBE3(LIF),STAT=IER1)
         RRBE3 = ZERO
          NMP = 6*NMT
          LI1=1
          LI2=LI1+3*NMP
          LI3=LI2+NMP
          IF (IROTG>0) THEN
           LI4=LI3+NMP
           LI5=LI4+3*NMP
           LI6=LI5+NMP
          ELSE
           LI4=LI3
           LI5=LI4
           LI6=LI5
          ENDIF
          LIF=LI6+NMP
C         spmd p/on
          ALLOCATE(RRBE3_PON(LIF),STAT=IER1)
          RRBE3_PON=ZERO
        END IF
       END IF
       CALL MY_BARRIER
      END IF
C----------------------------------
C       IMPLICIT SIZES [MONO THREAD]
C       Double precision only
C----------------------------------
      IF (IMPL_S==1.OR.NEIG>0) THEN
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
         ALLOCATE (ELBUF_IMP(NGROUP))
         CALL ALLOC_ELBUF_IMP(ELBUF_TAB,ELBUF_IMP,NGROUP, IPARG)  
          IF (IMON>0) CALL STARTIME(34,1)
          IF (IMON>0) CALL STARTIME(31,1)
          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
          CALL IMP_SOL_INIT(
     1    GEO       ,NPBY      ,LPBY      ,ITAB      ,
     2    IPARI     ,IXS       ,IXQ       ,IXC       ,IXT       ,
     4    IXP       ,IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,
     5    IXS(L2)   ,IXS(L3)   ,IXUR      ,IPARG     ,     
     6    ELBUF     ,NINT7     ,NBINTC    ,X         ,DMCP      ,
     7    FR_ELEM   ,IAD_ELEM  ,FR_I2M    ,IAD_I2M   ,
     8    NPRW      ,NUM_IMP1  ,NUM_IMPL  ,MONVOL    ,IGRSURF   ,
     9    FR_MV     ,IPM       ,IGEO      ,IAD_RBY   ,
     A    FR_RBY    ,SH4TREE   ,SH3TREE   ,IRBE3     ,LRBE3     ,
     B    FR_RBE3M  ,IAD_RBE3M ,IRBE2     ,LRBE2     ,IBFV      ,
     C    VEL       ,ELBUF_TAB ,IFRAME    ,INTBUF_TAB,
     D    NDDL0     ,NNZK0     ,IMPBUF_TAB)
          IF (IMON>0) CALL STOPTIME(31,1)
          IF (IMON>0) CALL STOPTIME(34,1)
C        
            NS_IMP=>IMPBUF_TAB%CAND_N 
            NE_IMP=>IMPBUF_TAB%CAND_E 
           IND_IMP=>IMPBUF_TAB%INDSUBT
               X_A=>X
                   FEXT_IMP=>IMPBUF_TAB%AC
                   R_IMP=>IMPBUF_TAB%R_IMP
           ALLOCATE(FAC_K(0), IPIV_K(0))
         IF (IMUMPSV >0.OR.(ISOLV==7.AND.NSPMD>1)) THEN
#if defined(MUMPS5) 
          CALL SPMD_MUMPS_INI(MUMPS_PAR, 1)
#else
          WRITE(6,*) __LINE__,"Fatal error: MUMPS required"
          CALL FLUSH(6)
          CALL ARRET(5)
#endif
          ALLOCATE(CDDLP(NDDL0))
         ENDIF
         IBUCK= IBUCKL
C----------------------------------
C       EIGENSOLVER  [MONO THREAD]
C----------------------------------
C------ one routine for EIG
       IF (NEIG>0) THEN
         CALL TRACE_IN(3,NCYCLE,ZERO)
#ifdef DNC
         CALL IMP_EIGSOL(
     1                   EIGIPM      , EIGRPM      , MS        ,IN         , EIGIBUF   ,
     2                   X           ,IXTG1        ,TF         , NPC       , FR_WAVE   ,
     3                   W16         , WA        ,
     4                   ICODT       , ICODR       , ISKEW     ,IBFV       , VEL       ,
     4                   V           , A           , ELBUF    , IXS       , IXQ     ,
     5                   IXC         , IXT         , IXP      , IXR       , IXTG    ,
     6                   PM          , GEO         , FANI     , ICUT      , SKEW    ,
     7                   XCUT        ,FANI(1,1+NFIA), ITAB    ,FANI(1,1+NFEA),FANI(1,1+NFOA),
     8                   ANIN        , LPBY        , NPBY     , NSTRF     , RWBUF   ,
     9                   NPRW        , IXUR        , TANI     , ELBUF_TAB , DD_IAD  ,
     A                   IAD_ELEM    , FR_ELEM     , WEIGHT   , EANI      , IPART   ,
     B                   RBY         , NOM_OPT     , IGRSURF  ,
     C                   BUFSF       , IDATA       , RDATA    , BUFMAT    , BUFGEO  ,
     D                   KXX         , IXX         , KXSP     , IXSP      , NOD2SP  ,
     E                   SPBUF       , IXS(L1)     , IXS(L2)  , IXS(L3)   , VR      ,
     F                   MONVOL      , VOLMON      , IPM      , IGEO      , IPARG   ,
     G                   NODGLOB     , IAD_ELEM    , FR_ELEM  , FR_SEC    , FR_RBY2 ,
     H                   IAD_RBY2    , FR_WALL     ,  IPARI   ,
     I                   INTBUF_TAB  , D           ,PARTSAV   ,
     J                   FSAV(1,NFNCA+1),FSAV(1,NFTCA+1), TEMP      , THKE    ,
     K                   ERR_THK_SH4 , ERR_THK_SH3 , IRBE2    , IRBE3     ,LRBE2    ,
     L                   LRBE3       , FRBE3    ,FR_RBE2  , FR_RBE3M  , IAD_RBE2,
     M                   WEIGHT_MD   , CLUSTER     , FCLUSTER , MCLUSTER  , XFEM_TAB,
     O                   W           , NV46     , NERCVOIS  , NESDVOIS,
     P                   LERCVOIS    , LESDVOIS    ,CRKEDGE   , INDX_CRK  , XEDGE4N ,
     Q                   XEDGE3N     ,STACK        ,SPH2SOL   ,STIFN      ,STIFR    ,
     R                   DRAPE_Q4    , DRAPE_T3    ,H3D_DATA  ,SUBSETS    ,IGRNOD   ,
     S                   FCONT_MAX   ,FANI(1,NFNCA2+1),FANI(1,NFTCA2+1), ALE_CONNECTIVITY ,
     T                   ITASK       ,NDDL0        ,NNZK0     ,IMPBUF_TAB )
#endif
        DT2 = MAX(EM20,TSTOP-TT)
        MSTOP=2
        GOTO 300
       END IF !IF (NEIG>0) THEN
#endif
      ELSE
         IMPL_S0 = 0
         X_A=>X
                 IMP_DUM(1)=0
         NS_IMP => IMP_DUM 
         NE_IMP => IMP_DUM 
         IND_IMP=> IMP_DUM
         NUM_IMP=0
      END IF !IF (IMPL_S==1.OR.NEIG>0) THEN
C------------------------------------------------------
C  Fin Implicit [MONO THREAD]
C------------------------------------------------------
C------------------------------------------------------
C  other initializations [MONO THREAD]
C------------------------------------------------------
      ONOFP=0
      IAD1 = 0
      IAD2 = 0
      IAD1B= 0
      IF (IMACH==3.AND.IPARIT>0) THEN
         IAD1 = 1
         IF(IVECTOR==1)THEN
           IAD1B = IAD1+NUMNOD+1
           IAD2 = IAD1B+NUMNOD
         ELSE
           IAD1B = IAD1
           IAD2 = IAD1B+NUMNOD+1
         ENDIF
      ENDIF
      IF(NUMSPH/=0)THEN
        IF(SCODVER>=44.AND.SMINVER<3)
     .   CALL SPHRES44B(KXSP ,IXSP ,NOD2SP ,IPARG ,SPBUF )
      ENDIF
C------------------------------------------------------
C SPH pointers
C------------------------------------------------------
      KSPH1 =1+3*NUMSPH
      KSPH21 =NUMSPH+1
      KSPH22 =KSPH21+16*NUMSPH
      KSPH23 =KSPH22+MIN(IUN,NSPHIO)*3*NUMSPH
      KSPACTIV=1
      IF(NSPHSOL==0)THEN
C     pointer to list of paticles to be sorted (to be sorted == active)
        KSP2SORT=1
      ELSE
C     to be sorted > active
        KSP2SORT=KSPH23
      END IF
C-------------------------------
C       POINTERS FOR USER ROUTINES
C-------------------------------
        CALL SAV_BUF_POINT(PM,1)
        CALL SAV_BUF_POINT(BUFMAT,2)
        CALL SAV_BUF_POINT(GEO,3)
        CALL SAV_BUF_POINT(BUFGEO,4)
        CALL SAV_BUF_POINT(NPC,5)
        CALL SAV_BUF_POINT(TF,6)
        CALL SAV_BUF_POINT(ISKWN,9)
        CALL SAV_BUF_POINT(SKEW,10)
        CALL SAV_BUF_POINT(LACCELM,11)
        CALL SAV_BUF_POINT(ACCELM,12)
        CALL SAV_BUF_POINT(ITABM1,13)
        CALL SAV_BUF_POINT(X,14)
        CALL SAV_BUF_POINT(D,15)
        CALL SAV_BUF_POINT(V,16)
        CALL SAV_BUF_POINT(A,17)
        CALL SAV_BUF_POINT(V,16)
        CALL SAV_BUF_POINT(A,17)
        CALL SAV_BUF_POINT(WEIGHT,18)
        CALL SAV_BUF_POINT(IPM,19)
        CALL SAV_BUF_POINT(IGEO,20)

C ----------------------------------------------
C Specific Initializations
C ----------------------------------------------

#ifdef DNC
      MADYMO_DEL_GLOBAL=0
      MADYMO_DEL=0
      IF(IMADCPL==1)THEN

          ALLOCATE(MAD_TAG_SOL(NUMELS))
          ALLOCATE(MAD_TAG_SH (NUMELC))
          ALLOCATE(MAD_TAG_TG (NUMELTG))
          ALLOCATE(MAD_FAIL_ELEMENTS (NMADSOL+NMADSH4+NMADSH3))
          MAD_TAG_SOL(1:NUMELS)=0
          MAD_TAG_SH(1:NUMELC)=0
          MAD_TAG_TG(1:NUMELTG)=0
          MAD_FAIL_ELEMENTS(1:NMADSOL+NMADSH4+NMADSH3) = 0

          ALLOCATE(MADCLFRECV(3,MADCLNODS))

          CALL INITIAL_DATA_EXCH_MADCPL(X,A,V,MS,MADCLNOD)

C Implementing a dummy Madymo cycle
          CALL DUMMY_CYCLE_MADCPL(X,MADCLNOD)

      ENDIF
#endif

      IF(NINTER25 /= 0) THEN
        CALL SPMD_I25FRONT_INIT(ITAB,MAIN_PROC,INTBUF_TAB,IPARI)
      ELSE
        NINTER25E = 0
      ENDIF

      NFVBAG = 0
      CHECK_NPOLH = .FALSE. 
      IF(NVOLU>0)THEN
        CALL FVDIM(MONVOL)
        CALL FV_SWITCH_CRIT(MONVOL,CHECK_NPOLH)
      ENDIF
C Save the number of FVMBAG before switches to UP
      NFVBAG0 = NFVBAG


      IF( NINTER /= 0 ) THEN
C Bucket or voxel, depending on /PERF/ in 1.rad
        CALL INIT_INTERF_SORTING_STRATEGY(INTBUF_TAB,NINTER)
C First: try with reduced bounding box
        CALL INIT_TRIM(NINTER)
      ENDIF 
      
      ! ----------------------
      ! user library : initialization
      IF(DLIB_STRUCT(ID_ENGINE_USER_INITIALIZE)%DLIB_BOOL) THEN
        NSPMD_USER = NSPMD
        NTASK_USER = NTHREAD
        ISPMD_USER = ISPMD
        CALL ENGINE_USER_INITIALIZE(NSPMD_USER,NTASK_USER,ISPMD_USER)
      ENDIF
      ! ----------------------

C=======================================================================

C----------------------------------------
C     BEGINNING OF EXPLICIT ITERATION LOOP
C----------------------------------------

 100  CONTINUE

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C INTERFACE
C Reallocate RENUM array
      RNS = 0
      CALL RENUM_SIZ(IPARI,RNS)
      IF (RNS > RNUM_SIZ)THEN
        DEALLOCATE(RENUM)
        RNUM_SIZ=RNS
        ALLOCATE(RENUM(RNUM_SIZ))
      ENDIF

#ifdef DNC
      IF (IMADCPL>0)THEN

        CALL DATA_SEND_MADCPL(X,MADCLNOD,
     *                        MADYMO_DEL_GLOBAL,MAD_FAIL_ELEMENTS)
      ENDIF
#endif

C-----------------------------------------------
C     TRACE BACK
C-----------------------------------------------
      CALL TRACE_IN(3,NCYCLE,ZERO)

      IF(IMON>0) CALL STARTIME(6,1)
      IF(IMONM > 0) CALL STARTIME(47,1)
C
      CALL MANCTR(SENSOR_TAB,H3D_DATA)
C----------------------------
C       MOVING SKEW  [MONO THREAD]
C----------------------------------
       IF(NUMSKW/=0) CALL NEWSKW(SKEW    ,ISKWN        ,X           ,ISKWP_L  ,NSKWP,
     1                           NUMSKW_L,NUMSKW_L_SEND,ISKWP_L_SEND,RECVCOUNT,ISKWP)
C----------------------------------
        ECONT=ZERO
        EDAMP=ZERO
        ENINT=ZERO
        XMASS=ZERO
        XMOMT=ZERO
        YMOMT=ZERO
        ZMOMT=ZERO
        DEF=ZERO
        DMF=ZERO
C
        DT1=DT2
        DT2=EP06
        NT_IMP=0
        DT_THERM=EP06
      IF(IMPL_S>0) THEN
       IF(NCYCLE>0) THEN
                IF(ISMDISP==1) X_A=>IMPBUF_TAB%X_A
        IF (IMON>0) CALL STARTIME(5,1)
        CALL IMP_FANIE(FANI  ,FEXT_IMP,NFIA  ,NFEA  ,NODFT ,NODLT,
     .                 H3D_DATA  )
        IF (IMON>0) CALL STOPTIME(5,1)
       ENDIF
      END IF
C----------------------------------
      IF(IMONM > 0) CALL STOPTIME(47,1)
      IF(IMON>0) CALL STOPTIME(6,1)

      IMADFSH4=0

C----------------------------------
C       Gather actual thickness of shells
C----------------------------------
      IF(ITHKNOD/=0)THEN
        NSGDONE=1
        THKNOD(NODFT:NODLT)=ZERO
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
        IF(ITASK==0)THEN
          CALL THICKVAR(IPARG,ELBUF_TAB,IXC,IXTG,THKSH4,
     .                  THKSH3,THKNOD,THKE,SH4TREE,SH3TREE)
        END IF

        IF(NSPMD>1) THEN
          SIZE = 1
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXCH_THKNOD(
     +      THKNOD,IAD_ELEM ,FR_ELEM,SIZE,LENR)
        ENDIF
      END IF

C-----------------------------
C    Not pure thermal case                          
C-----------------------------

      IF(ILAG+IALE+IEULER/=0)THEN
C--- // N/3 -------------------------------------
C         CANCEL Mass matrix in 2D
C------------------------------------------------
          ISYNC = 0
          IF(N2D/=0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
            CALL SMP_INIT(
     1        ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2        IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)
            CALL ZERO1(MS(NODFTSK),NUMNTSK)
!$OMP END PARALLEL

          ENDIF
      ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
          DO I=1+NFOA,NFOA+2*(NSECT+NRBODY+NRWALL+NUMELUR)
             FANI(1,I) = ZERO
             FANI(2,I) = ZERO
             FANI(3,I) = ZERO
          ENDDO
          IF(ANIM_V(13)+H3D_DATA%N_VECT_CONT2==1)THEN
              DO I=1+NFT2,NFT2 + NUMNOD*MIN(1,ANIM_V(13)+H3D_DATA%N_VECT_CONT2)
                 FANI(1,I) = ZERO
                 FANI(2,I) = ZERO
                 FANI(3,I) = ZERO
              ENDDO
          ENDIF

        NGDONE = 1
        NSGDONE = 1
C------------------------
C       INTERFACES 14 & 15 :
C         Initialisation buffers: forces, moments, stifness of surfaces.
C--- //0 ----------------
        IF (ISPMD==0) THEN
          IF (NINTER/=0) THEN
          IF (IMON>0) CALL STARTIME(2,1)
            CALL I14IST(IPARI,INTBUF_TAB,IGRSURF,BUFSF)
          IF (IMON>0) CALL STOPTIME(2,1)
          ENDIF
        ENDIF
C
        IF(ISHSUB==1)THEN
          IF(TT==ZERO .OR. TT+1.1*DT2OLD>T1SH+DT2S)THEN
C kshsub=-1 shell at this cycle 
            KSHSUB = -1
          ELSE
C kshsub=1  No shell at this cycle.
            KSHSUB = 1
            LASTSH = 0
          ENDIF
          IF(KSHSUB==-1)THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()
          CALL SHSUB0(FSKY, FSKY, ITSK)
!$OMP END PARALLEL
          ENDIF
        ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------------------------------------
        DT2T    = DT2
C--------------------------------------------------------
        K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
        K2=K1+NUMELS
        K3=K2+NUMELQ
        K4=K3+NUMELC
        K5=K4+NUMELT
        K6=K5+NUMELP
        K7=K6+NUMELR
        K8=K7+NUMELUR
        K9=K8+NUMELTG
        K10=K9+NUMELX
        K11=K10+NUMSPH
        K12=K11+NUMELIG3D
C-----------------------------------------------
C      gauges de pression, calcul de la position
C-----------------------------------------------
      IF (IALE+IEULER+ITHERM+NUMSPH/=0) THEN
        IF(NBGAUGE/=0)CALL AGAUGE0(LGAUGE,GAUGE,X,IXC,IGAUP,NGAUP)
      END IF
C----------------------------------------
C
      IF (IMON>0) CALL STARTIME(6,1)
      IF (IMONM > 0) CALL STARTIME(49,1)
C----------------------------
        IF(NUMGEO>0.AND.NODADT==0)THEN
         DO I=1,NUMGEO
           IF(GEO(5,I)>ZERO.AND.DTFAC1(3)*GEO(5,I)<DT2T)THEN
             DT2T= DTFAC1(3)*GEO(5,I)
             NELTST = 0
             ITYPTST= 3
           ENDIF
         ENDDO
        ENDIF
C------------------------
C       USER WINDOW
C------------------------
        IF (IUSERWI /= 0) THEN
          CALL TRACE_IN(9,2,ZERO)
          IF (ISPMD == 0)THEN
           DTU = EP30
           NUVAR  = SUSER
           NUVARI = SIUSER - 100
           IF (USERL_AVAIL>0)THEN
             CALL ENG_USERLIB_USERWI(RAD_INPUTNAME,LEN_RAD_INPUTNAME,
     1                              NUVAR    ,NUVARI    ,NUMNOD   ,
     2         NCYCLE    ,LENWA    ,IUSER    ,ITAB     ,TT       ,
     3         DT1       ,DTU      ,USREINT  ,TFEXT    ,USER     ,
     4         D         ,X        ,V        ,VR       ,MS       ,
     5         IN        ,STIFN    ,STIFR    ,A        ,AR       ,
     6         WA        )

           ELSE
             ! ----------------
             ! ERROR to be printed & exit
             OPTION='USER WINDOWS'
             SIZE_OPT=LEN_TRIM(OPTION)
             CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE_OPT),ANMODE=ANINFO)
             CALL ARRET(2)
             ! ----------------
           ENDIF
           DT2 = MIN(DT2,DTU)
          ENDIF
          IF(NSPMD> 1)THEN
            CALL SPMD_EXCH_USERWI(A,AR,IADUSERW,FR_USERWI)
          ENDIF
          CALL TRACE_OUT(9)
        ENDIF
C----------------------------------
C       FUNCTIONS
C----------------------------------
        IF(NFUNCT /= 0.AND.IALE+IEULER+ITHERM+NEBCS>0) THEN
            CALL TIMFUN(FV, NPC, TF)
            IF(EBCS_TAB%nebcs_loc/=0) THEN
                !!! Need to "extrapolate values" whenever current time
                !!! is lower than minimum defined time function or greater than
                !!! maximum defined time function
                CALL EBCS_EXTRAPOL(FV, NPC, TF, EBCS_TAB)
            ENDIF
        ENDIF
C----------------------------------

C-------------------------------------------------------------------
C       ACTIVATION-DEACTIVATION of groups of elements
C-------------------------------------------------------------------
        IF (NACTIV>0) THEN
            IF(ITHERM_FE > 0 .AND. NSPMD > 1 ) THEN
              DO I = 1,NUMNOD
                MCP(I)   = MCP(I)   * WEIGHT(I)
                STIFN(I) = STIFN(I) * WEIGHT(I)
              ENDDO
            ENDIF
            CALL DESACTI(IXS     ,IXQ     ,IXC     ,IXP     ,IXT      ,
     .                   IXR     ,IXTG     ,IPARG   ,
     .                   IACTIV  ,SENSOR_TAB,FSKY    ,X       ,ELBUF_TAB,
     .                   IBCV    ,FCONV   ,IBCR    ,FRADIA  ,IGROUPS  ,
     .                   FACTIV  ,TEMP     ,MCP     ,PM      ,MCP_OFF  ,
     .                   IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N ,IGRTRUSS ,
     .                   IGRBEAM ,IGRSPRING)
        ELSE
            IF(ITHERM_FE > 0 .AND. NSPMD > 1 .AND. IPARIT == 0) THEN
              DO I = 1,NUMNOD
                MCP(I)   = MCP(I)   * WEIGHT(I)
              ENDDO
            ENDIF
        ENDIF
      IF (IMONM > 0) CALL STOPTIME(49,1)
      
C-------------------------------------------------------------------
C       EXTERNAL FORCES
C-------------------------------------------------------------------

       IF (NCONLD/=0 .AND. IMPL_S/=1) THEN
          CALL TRACE_IN(10,0,ZERO)
        IF (IMON>0) CALL STARTIME(4,1)
        IF (IMONM > 0) CALL STARTIME(41,1)
          CALL FORCE(IBCL   ,FORC    ,NPC    ,TF      ,A         ,
     2               V      ,X       ,SKEW   ,AR      ,VR        ,
     3               SENSOR_TAB ,WEIGHT ,TFEXC   ,IADS(I87J),
     4               FSKY   , FSKY   ,FEXT   ,H3D_DATA,
     5               CPTREAC,FTHREAC,NODREAC)
C
        IF (NPINCH > 0) THEN
          CALL FORCEPINCH(IBCL   ,FORC    ,NPC    ,TF      ,A         ,
     2                    V      ,X       ,SKEW   ,AR      ,VR        ,
     3                    SENSOR_TAB ,WEIGHT ,TFEXC   ,IADS(I87J),
     4                    FSKY   , FSKY   ,FEXT   ,H3D_DATA,
     5                    PINCH_DATA%APINCH, PINCH_DATA%VPINCH)  
        ENDIF
C
        IF (IMONM > 0) CALL STOPTIME(41,1)
        IF (IMON>0) CALL STOPTIME(4,1)
          CALL TRACE_OUT(10)
        ENDIF
C
        IF(NFXVEL/=0.AND.IMPL_S/=1) THEN
          IF(IMON>0) THEN
            CALL STARTIME(6,1)
            CALL STARTIME(4,1)
            IF(IMONM > 0) CALL STARTIME(44,1)
          ENDIF
          CALL FORCEFINGEO(IBFV ,NPC    ,TF      ,A    ,V    ,X     ,
     2                     VEL  ,SENSOR_TAB ,FSKY ,FEXT ,ITABM1,
     3                     H3D_DATA)
          IF(IMON>0) THEN
            IF(IMONM > 0) CALL STOPTIME(44,1)
            CALL STOPTIME(4,1)
            CALL STOPTIME(6,1)
          ENDIF
        ENDIF
C-------------------------------------------------------------------
C       LOAD PFLUID
C-------------------------------------------------------------------
        IF(NLOADP_F/=0.AND.IMPL_S/=1) THEN
          CALL TRACE_IN(10,0,ZERO)
        IF (IMON>0) CALL STARTIME(4,1)
        IF (IMONM > 0) CALL STARTIME(41,1)
!$OMP PARALLEL
          CALL PFLUID(ILOADP  ,LOADP   ,NPC   ,TF   ,A         ,
     2                V     ,X      ,XFRAME  ,AR   ,VR        ,
     3                SENSOR_TAB,WEIGHT,TFEXC,IADS(I87M),
     4                FSKY    ,FSKY    ,LLOADP ,FEXT  ,H3D_DATA
     5                )
!$OMP END PARALLEL
        IF (IMONM > 0) CALL STOPTIME(41,1)
        IF (IMON>0) CALL STOPTIME(4,1)
          CALL TRACE_OUT(10)
        ENDIF

C-------------------------------------------------------------------
C       LOAD PBLAST
C----------------------------------
        IF(NLOADP_B/=0.AND.IMPL_S/=1) THEN
          CALL TRACE_IN(10,0,ZERO)
          IF (IMON>0) CALL STARTIME(4,1)
          IF (IMONM > 0) CALL STARTIME(41,1)
          CALL PBLAST(ILOADP     ,LOADP      ,A          ,V       ,X      ,
     1                IADS(I87M) ,FSKY       ,FSKY       ,LLOADP  ,FEXT   ,
     2                ITAB       ,H3D_DATA)
          IF (IMONM > 0) CALL STOPTIME(41,1)
          IF (IMON>0) CALL STOPTIME(4,1)
          CALL TRACE_OUT(10)
          IF(DT_PBLAST<DT2T)THEN
            !inter22 kinematic time step
              DT2T         = DT_PBLAST  
              ITYPTST      = 12
              NELTST       = IDT_PBLAST
              DT_PBLAST    = EP20
              T0INF_PBLAST = EP20       
         ENDIF    
        ENDIF    

  
        IF( NUMCONV + NUMRADIA > 0 .AND. ITHERM_FE > 0 )THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
C-------------------------------------------------------------------
C   BC -- CONVECTION  for heat_transfert by FEM
C----------------------------------------------
          IF(NUMCONV > 0 .AND. ITHERM_FE > 0) THEN
            IF (IMON>0) CALL STARTIME(4,1)
            IF (IMONM > 0) CALL STARTIME(41,1)
              CALL CONVEC(IBCV  ,FCONV   ,NPC   ,TF   , X     ,
     1                  TEMP    ,SENSOR_TAB,FTHE, IADS(I87K),
     2                  FTHESKY)
            IF (IMONM > 0) CALL STOPTIME(41,1)
            IF (IMON>0) CALL STOPTIME(4,1)
          ENDIF
C-------------------------------------------------------------------
C   BC -- RADIATION to environment for heat_transfert by FEM
C-----------------------------------------------------------
          IF(NUMRADIA > 0 .AND. ITHERM_FE > 0) THEN
            IF (IMON>0) CALL STARTIME(4,1)
            IF (IMONM > 0) CALL STARTIME(41,1)
            CALL RADIATION(IBCR,   FRADIA, NPC,     TF,   X         ,
     1                     TEMP,   SENSOR_TAB, FTHE, IADS(I87L),
     2                     FTHESKY)
            IF (IMONM > 0) CALL STOPTIME(41,1)
            IF (IMON>0) CALL STOPTIME(4,1)
          ENDIF
!$OMP END PARALLEL
        ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C-------------------------------------------------------------------
C   BC -- THERMAL FLUX  for heat_transfert by FEM
C-------------------------------------------------
        IF(NFXFLUX > 0 .AND. ITHERM_FE > 0) THEN
          IF (IMON>0) CALL STARTIME(4,1)
          IF (IMONM > 0) CALL STARTIME(41,1)
          CALL FIXFLUX(IBFFLUX, FBFFLUX,  NPC,  TF,     X,     IXS,
     1                 SENSOR_TAB, FTHE, IADS(I87N), FTHESKY)
          IF (IMONM > 0) CALL STOPTIME(41,1)
          IF (IMON>0) CALL STOPTIME(4,1)
        ENDIF


        ICONTACT_OLD(1:SICONTACT) = ICONTACT(1:SICONTACT) 
        IF(NVOLU/=0)THEN
        IF (IMONM > 0) CALL STARTIME(50,1)
          CALL TRACE_IN(11,0,ZERO)
          NN = NUMELC+NUMELTG+IBAGSURF
          N0 = 1 + 3*NN
          IF(INTBAG/=0)THEN
            N1 = N0+ NN
          ELSE
            N1 = N0
          ENDIF
C
          N=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
           CALL MONVOL0(
     1      MONVOL     ,VOLMON     ,X_A            ,A         ,
     2      NPC        ,TF         ,V              ,WA        ,
     3      FSAV(1,N)  ,SENSOR_TAB ,IGRSURF   ,
     4      FR_MV      ,IADS(I87I) ,
     5      FSKY       ,ICONTACT   ,WA(N0)         ,IPARG     ,
     6      ELBUF_TAB  ,GEO        ,IGEO           ,
     7      PM         ,IPM        ,IPART          ,IPART(K3) ,
     8      IPART(K8)  ,IGROUPC    ,IGROUPTG       ,FEXT      ,
     9      1          ,H3D_DATA   ,T_MONVOL)
          CALL TRACE_OUT(11)
        IF (IMONM > 0) CALL STOPTIME(50,1)
        ENDIF
      IF (IMON>0) CALL STOPTIME(6,1)
C
      IF (NFLOW>0) THEN
         CALL FLOW0(IFLOW, RFLOW,   WIFLOW, WRFLOW, X,
     .              V,     A,       NPC,    TF,     SENSOR_TAB,
     .              NBGAUGE,LGAUGE,  GAUGE , NSENSOR,
     .              IGRV,   AGRV  ,NFUNCT  )
      ENDIF

C----------------------------------------
C      MPI COMMUNICATION BEFORE SORTING
C----------------------------------------
      IF (IMON>0) CALL STARTIME(13,1)
      IF (IMONM > 0) CALL STARTIME(24,1)
      IF(NSPMD>1)THEN
          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
          CALL SPMD_I7XVCOM2(
     1       IPARI  ,X       ,V       ,MS      ,
     2       IMSCH  ,I2MSCH   ,DT2PREV ,INTLIST ,NBINTC  ,
     3       ISLEN7 ,IRLEN7   ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4       IRLEN17 ,IXS     ,IXS(L3) ,
     5       IGRBRIC ,TEMP    ,2       ,IRLEN7T ,ISLEN7T ,
     6       IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T,IRLEN20E,
     7       ISLEN20E,IKINE   ,DIAG_SMS,SENSOR_TAB,INTBUF_TAB ,INT24E2EUSE,
     8       FORNEQS ,MULTI_FVM)
        END IF
      IF (IMONM > 0) CALL STOPTIME(24,1)
      IF (IMON>0) CALL STOPTIME(13,1)


C--------------------------------------------------------
C INTERFACE 24 - Communication part 4 / 4
C--------------------------------------------------------

          IF (INT24USE == 1)THEN
            IF (IMON>0) CALL STARTIME(8,1)        
              CALL SPMD_EXCH_I24(IPARI    ,INTBUF_TAB       ,ITAB  ,
     *                           IAD_ELEM ,FR_ELEM ,INTLIST ,NBINTC,
     *                           IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,4,
     *                           INT24E2EUSE)
            IF (IMON>0) CALL STOPTIME(8,1)
          ENDIF
      
C
C Traitement des Section en SPMD comm avant FORINT
C
      IF(NSECT>0.AND.NSPMD>1) THEN
        LSEND1 = IAD_SEC(1,NSPMD+1)
        LRECV1 = IAD_SEC(2,NSPMD+1)
        LSEND2 = IAD_SEC(3,NSPMD+1)
        LRECV2 = IAD_SEC(4,NSPMD+1)
          CALL SPMD_EXCH_SEC(NSTRF ,X      ,MS    ,WEIGHT,XSEC  ,
     2                       FR_SEC,IAD_SEC,LSEND1,LRECV1,LSEND2,
     3                       LRECV2,WEIGHT_MD)
        END IF
C----------------------------------------------------------
C       INTER/TYPE21 ROTATION
C----------------------------------------------------------
      IF(NINTSTAMP/=0)THEN
        CALL INTSTAMP_INIT(INTSTAMP,ICODR)
      END IF
C---------------------------------------------
C TETRA4 : SMOOTH FINITE ELEMENT FORMULATIONS
C---------------------------------------------
      IF(ISFEM >= 1) THEN
        IF(IALE+IEULER+ITHERM==0) THEN
          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          CALL S4LAGSFEM(IPARG,IXS,X,V,ELBUF_TAB,VARNOD,
     .                   IAD_ELEM,FR_ELEM,IXS(L1),XDP)
        ENDIF
      ENDIF

C----------------------------------------
C      COLLISION DETECTION FOR INTERFACES 7 (CALL BARRIER DANS I7BUCE_CRIT)
C----------------------------------------
      IF(NINTER/=0) THEN
        CALL TRACE_IN(8,2,ZERO)
        IF (IMON > 0) CALL STARTIME(2,1)
        L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
        L2 = L1+6*NUMELS10
        L3 = L2+12*NUMELS20
        LSKYI_COUNT = 0
        LSKYI_SMS_NEW = 0

        IF(IDTMINS/=0) THEN
            NATIV_SMS_SIZ = NUMNOD
        ELSE
            NATIV_SMS_SIZ = 0
        ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,DT2TT,NELTSTT,ITYPTSTT)

        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST
        ITSK = OMP_GET_THREAD_NUM()
           CALL INTTRI(
     1     IPARI   ,X_A     ,W     ,
     2     V       ,MS      ,IN      ,IAD_ELEM  ,
     3     FR_ELEM ,VR      ,ISENDTO ,IRCVFROM  ,
     4     NEWFRONT  ,ITSK    ,WA      ,DT2TT     ,
     5     ITAB      ,NELTSTT ,ITYPTSTT,WEIGHT    ,
     6     INTLIST ,NBINTC  ,KINET   ,DRETRI    ,
     7     ISLEN7  ,IRLEN7  ,ISLEN11 ,IRLEN11   ,
     8     TEMP    ,IGRBRIC ,IGRSH3N ,EMINX     ,
     9     IXS     ,IXS(L3) ,IXS(L2) ,ISLEN17   ,
     A     IRLEN17 ,IRLEN7T ,ISLEN7T ,NUM_IMP1  ,
     B     IND_IMP ,INTSTAMP,THKNOD  ,IRLEN20   ,
     C     ISLEN20 ,IRLEN20T,ISLEN20T,IRLEN20E  ,
     D     ISLEN20E  ,RENUM   ,NSNFIOLD,XSLV      ,
     E     XMSR      ,VSLV    ,VMSR    ,SIZE_T    ,
     F     NATIV_SMS ,DXANCG  ,IKINE   ,DIAG_SMS  ,
     G     COUNT_REMSLV,COUNT_REMSLVE  ,ALE_CONNECTIVITY,
     H     IXTG     ,SENSOR_TAB,DELTA_PMAX_GAP,INTBUF_TAB,
     I     DELTA_PMAX_GAP_NODE,IAD_FRNOR,FR_NOR,
     J     NB25_CANDT,NB25_IMPCT,NB25_DST1,NB25_DST2,INTLIST25,
     K     IAD_FREDG,FR_EDG,MAIN_PROC,NATIV_SMS,I_OPT_STOK ,
     L     MULTI_FVM,IPARG  ,ELBUF_TAB, H3D_DATA, T2MAIN_SMS,
     M     LSKYI_SMS_NEW    ,FORNEQS  ,INT7ITIED,IDEL7NOK_SAV,MAXDGAP,
     N     T2FAC_SMS,ICODT,ISKEW ,FSKYN25  ,ADDCSRECT,PROCNOR,
     O     INTER_STRUCT,SORT_COMM,RNUM_SIZ,NATIV_SMS_SIZ,TEMP_SIZ)
C
#include "lockon.inc"
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
          ENDIF
#include "lockoff.inc"

!$OMP END PARALLEL

C========================================================================================

C ----------------------------------------------------
C Check if ISKY & FSKYI are sufficiently allocate
C If not reallocate them
C ----------------------------------------------------

      IF(IPARIT >0)THEN
       IF ( NISKY+LSKYI_COUNT > SISKY) THEN
         CALL REALLOCATE_I_SKYLINE(LSKYI_COUNT,1)
       ENDIF   

C
C Remote Secnd Nodes
       IF (NSPMD >1)THEN
         CALL UPGRADE_REM_2RY(IPARI,COUNT_REMSLV,COUNT_REMSLVE)
         COUNT_REMSLV(1:NINTER)= 0
         COUNT_REMSLVE(1:NINTER)= 0
       ENDIF

      ENDIF

        IF (IMON > 0) CALL STOPTIME(2,1)
        CALL TRACE_OUT(8)
      ENDIF

C ----------------------------------------------------
C SMS - Check if ISKYI_SMS & MSKYI_SMS are sufficiently allocate
C If not reallocate them
C ----------------------------------------------------

      IF ((IDTMINS == 2.OR.IDTMINS_INT/=0).AND.(NINTER > 0)) THEN
C 
        IF (LSKYI_SMS_NEW > LSKYI_SMS) THEN
C
          DEALLOCATE(ISKYI_SMS,MSKYI_SMS,JDII_SMS,LTI_SMS)
C
          LSKYI_SMS = NINT(LSKYI_SMS_NEW*1.2)
          ALLOCATE(MSKYI_SMS(LSKYI_SMS),ISKYI_SMS(LSKYI_SMS,3),
     .             JDII_SMS(2*LSKYI_SMS),LTI_SMS(2*LSKYI_SMS),
     .             STAT=IERROR)
C
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='LSKYI_SMS RESIZE')
            CALL ARRET(2)
          ENDIF
C
        ENDIF   
      ENDIF      

C--------------------------------------------------------
C INTERFACE 25 - Communication IRTLM, TIME_S, etc :: send
C--------------------------------------------------------
       IF(NINTER25 /= 0)THEN

         IF(NSPMD > 1)THEN
           IF (IMON>0) CALL STARTIME(10,1)        
           CALL SPMD_EXCH_I25(IPARI    ,INTBUF_TAB       ,ITAB  ,
     *                        IAD_ELEM ,FR_ELEM ,INTLIST ,NBINTC,
     *                        IAD_I25  ,FR_I25  ,SFR_I25 ,1     )
           CALL SPMD_EXCH_I25(IPARI    ,INTBUF_TAB       ,ITAB  ,
     *                        IAD_ELEM ,FR_ELEM ,INTLIST ,NBINTC,
     *                        IAD_I25  ,FR_I25  ,SFR_I25 ,2     )
           IF (IMON>0) CALL STOPTIME(10,1)
         END IF

       END IF

C-------------------------------------------------------
C     ADAPTIVE MESHING
C   Get ICONTACT After sort, After Contact forces if SPMD
C                              Before Contact forces if SMP
C-------------------------------------------------------
      IF(NADMESH > 0.AND.IMPL_S==0)THEN
        IF (IMON>0) CALL STARTIME(36,1)
C
        NSH4OLD=NSH4ACT
        NSH3OLD=NSH3ACT
        IADMESH=0
          ICHKADM=0
          IF(TT >= TADMESH+DTADMESH)THEN
            TADMESH=TADMESH+DTADMESH
            ICHKADM=1
          END IF
C
        IF(ICHKADM/=0 .AND. IADMERRT/=0)THEN
C
            CALL ADMERR(
     .     IXC     ,IXTG     ,X        ,IPARG       ,ELBUF_TAB   ,  
     .       IPART   ,IPART(K3),IPART(K8),ERR_THK_SH4 ,ERR_THK_SH3 ,
     .       IAD_ELEM ,FR_ELEM ,WEIGHT   ,SH4TREE     ,SH3TREE     )
          END IF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

        CALL ADMDIV(IXC  ,IPART(K3),IXTG ,IPART(K8),IPART,
     .              ITSK ,ICONTACT ,IPARG,X        ,MS     ,
     .              IN   ,RCONTACT ,ELBUF_TAB,NODFTSK  ,NODLTSK,
     .               IGEO ,IPM      ,SH4TREE,PADMESH,MSC  ,
     .               INC  ,SH3TREE  ,MSTG   ,INTG   ,PTG  ,
     .            ACONTACT,PCONTACT,ERR_THK_SH4,ERR_THK_SH3,MSCND,
     .               INCND,PM      ,MCP     ,MCPC   ,MCPTG)
        NGDONE = 1       ! ne sert plus a virer
C       /---------------/
        CALL MY_BARRIER
C       /---------------/
        IF(IADMRULE /= 0)THEN
          IF(IADMESH > 0)THEN
            CALL ADMREGUL(IXC  ,IPART(K3),IXTG ,IPART(K8),IPART,
     .                    ITSK   ,IPARG    ,X      ,MS       ,IN   ,
     .                    ELBUF_TAB,NODFTSK  ,NODLTSK,IGEO     ,IPM  ,
     .                    SH4TREE,MSC  ,INC   ,SH3TREE,MSTG    ,
     .                    INTG   ,PTG  ,MSCND ,INCND  ,PM      ,
     .                    MCP    ,MCPC ,MCPTG )
C           /---------------/
            CALL MY_BARRIER
C           /---------------/
          END IF
        END IF

        IF(IADMESH > 0)THEN
          IF(ITSK==0) THEN
            CALL ADMORDR(SH4TREE,SH3TREE,IXC,IXTG)
            IF(ISTATCND /= 0) CALL CNDORDR(IPART,IPART(K3),IPART(K8),
     .                                     SH4TREE,SH3TREE)
          END IF
          IFLGADM=1
          CALL ADMGVID(
     1      IPARG     ,ELBUF_TAB ,FSKY   ,FSKY  ,FTHESKY,
     2      IADS(I87C),IADS(I87G),IFLGADM,IGROUC,NGROUC ,
     3      CONDNSKY  )
          CALL MY_BARRIER
          NGDONE = 1    
        END IF
        RCONTACT(NODFTSK:NODLTSK)=EP30
        ACONTACT(NODFTSK:NODLTSK)=EP30
        PCONTACT(NODFTSK:NODLTSK)=ZERO
!$OMP END PARALLEL

        IF (IMON>0) CALL STOPTIME(36,1)
      END IF
C--- // N/3 --------------------------------------------------------
C       EXTERNAL AND INTERNAL FORCES (ANIM)
C--------------------------------

        IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .   ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT>0
     .    .AND.IMPL_S==0)THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

        CALL FORANI1(FANI,A ,NFIA,NFEA,NFOA,NODFTSK,NODLTSK,FEXT,H3D_DATA)
!$OMP END PARALLEL
C
        ENDIF
C--------------------------------------------------------
C       ALE flux + forces
C--------------------------------------------------------
        IF (IALE+IEULER+ITHERM/=0) THEN
           CALL STARTIME(MACRO_TIMER_ALEMAIN,1) 
C-----------------------------
C        SPMD : MS=0 boundary nodes if  weight/=1  (pour amas03)
C        only for PARITH/OFF 
C-----------------------------
           LENQMV = 1
           IF(TRIMAT>0)LENQMV = MIN(1,TRIMAT)*(NUMELS+NUMELQ)
           NV46   = 6
           IF(N2D /= 0) NV46 = 4
           CALL TRACE_IN(12,0,ZERO)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)

C     Init var parallel SMP
           CALL SMP_INIT(
     1          ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2          IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3          GREFTSK,GRELTSK)
           DT2TT    = DT2T
           NELTSTT  = NELTST
           ITYPTSTT = ITYPTST
C     
           CALL ALEMAIN(
     1          PM         ,GEO      ,X             ,A(1,NDTSK)   ,AR(1,NDTSK)    ,V               ,
     2          MS         ,WA       ,ELBUF_TAB     ,BUFMAT       ,PARTSAV(IPMTSK),TF              ,
     3          VAL2       ,VEUL     ,FV            ,STIFN(NDTSK) ,FSKY           ,EANI            ,
     4          PHI        ,FILL     ,DFILL         ,ALPH         ,SKEW           ,W               ,
     5          D          ,DSAVE    ,ASAVE         ,DT2TT        ,DT2SAVE        ,XCELL           ,
     6          IPARG      ,NPC      ,IXS           ,IXQ          ,IXTG           ,IADS            ,IADS(I87B),
     7          IFILL      ,NODPOR   ,ICODT         ,ISKEW        ,IMS            ,
     8          NELTSTT    ,ITYPTSTT ,IPART         ,IPART(K1)    ,IPART(K2)      ,ITSK            ,
     A          NODFTSK    ,NODLTSK  ,NBRCVOIS      ,TEMP         ,
     B          NBSDVOIS   ,LNRCVOIS ,LNSDVOIS      ,NERCVOIS     ,NESDVOIS       ,LERCVOIS        ,
     C          LESDVOIS   ,ISIZXV   ,IAD_ELEM      ,FR_ELEM      ,FSKYM          ,MSNF            ,
     D          IPARI      ,SEGVAR   ,ITAB          ,ISKWN        ,
     E          VOLMON     ,FSAV     ,IGRSURF       ,NELTSA       ,
     F          ITYPTSA    ,WEIGHT   ,NPSEGCOM      ,LSEGCOM      ,IPM            ,IGEO            ,
     G          ITABM1     ,LENQMV   ,NV46          ,A            ,GRESAV         ,
     H          GRTH       ,IGRTH    ,LGAUGE        ,GAUGE        ,MSSA           ,
     I          DMELS      ,IGAUP    ,NGAUP         ,TABLE        ,MS0            ,
     J          XDP        ,IGRNOD   ,VARNOD                      ,FSKYI          ,ISKY            ,
     K          INTBUF_TAB ,IXT      ,IGRV          ,AGRV         ,SENSOR_TAB     ,
     L          LGRAV      ,CONDNSKY ,CONDN         ,MS_2D        ,MULTI_FVM      ,IGRTRUSS        ,
     M          IGRBRIC    ,NLOC_DMG ,ID_GLOBAL_VOIS,FACE_VOIS    ,EBCS_TAB       ,ALE_CONNECTIVITY, DIFFUSION,
     N          MATPARAM_TAB,H3D_DATA)

           IF(INT22 /=0)  call my_barrier !INTER22in input files  - get also IDT_INT22
#include "lockon.inc"
           IF(INT22 == 0)THEN
              IF(DT2TT<DT2T)THEN
                 DT2T   = DT2TT  
                 ITYPTST= ITYPTSTT
                 NELTST = NELTSTT   
              ENDIF         
           ELSE    
              IF(IDT_INT22 /= 0)THEN
!inter22 kinematic time step
                 DT2T   = dt22_min  
                 ITYPTST= 10
                 NELTST = 1        
              ELSE
!cell time step
                 IF(DT2TT<DT2T)THEN
                    DT2T   = DT2TT  
                    ITYPTST= ITYPTSTT
                    NELTST = NELTSTT   
                 ENDIF       
              ENDIF
           ENDIF       
#include "lockoff.inc"

!$OMP END PARALLEL

           CALL TRACE_OUT(12)
           CALL STOPTIME(MACRO_TIMER_ALEMAIN,1) 
        ELSE
           IFSUB=0
           IFSUBM=0
           T1S=TT
        ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

      IF(IALESUB ==2 .AND. IFSUB==2)GOTO 22
 21   CONTINUE
C
      IF (IMON>0) CALL STARTIME(6,1)
      IF (IMONM > 0) CALL STARTIME(51,1)
C----------------------------------
C       INTERNAL FORCES: RNUR S8FORC3 
C----------------------------------
        CALL FORINTS(
     1    PM        ,GEO       ,X         ,A         ,AR        ,
     2    V         ,VR        ,MS        ,IN        ,W         ,
     3    ELBUF                ,VAL2      ,VEUL      ,FV        ,
     4    STIFN     ,STIFR     ,FSKY      ,TF        ,BUFMAT    ,
     5    PARTSAV   ,FANI(1,NFOA+1),FSAV  ,
     6    SKEW      ,DT2T      ,
     7    IADS      ,IPARG     ,NPC       ,IXS       ,
     8    NELTST    ,ITYPTST   ,IPART     ,IPART(K1) ,ITAB      ,
     9    FSKYI     ,BUFGEO    ,KXX       ,IXX       ,ISKY      ,
     A    IPART(K9) ,GRESAV    ,GRTH      ,
     B    IGRTH     ,ELBUF_TAB )
      IF (IMONM > 0) CALL STOPTIME(51,1)
      IF (IMON>0) CALL STOPTIME(6,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
      CALL SMP_INIT(
     1  ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2  IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

C-------------------------------------------------------
C     RESET ICONTACT(1:NUMNOD)
C-------------------------------------------------------
      IF(KCONTACT/=0)THEN
        IF(IALELAG >0) 
     .  IFOAM_CONT(NODFTSK:NODLTSK)= ICONTACT(NODFTSK:NODLTSK)
C     
        ICONTACT(NODFTSK:NODLTSK)=0
      END IF

      IF(ISTATCND /= 0)THEN
        IF(IPARIT==0)THEN
          DO N=1,NUMNOD
            STCND(NDTSK+N-1) = ZERO
          END DO
        ELSE
          DO N=NODFTSK,NODLTSK
            STCND(N) = ZERO
          END DO
        END IF
      END IF
C int 24+pxfem
      IF(INTPLYXFEM > 0) THEN
          DO N=NODFTSK,NODLTSK
            WAGAP(1,N) = ZERO
            WAGAP(2,N) = ZERO
          END DO
      ENDIF
C      
!$OMP END PARALLEL

      IF(ANIM_PLY > 0) VN_NOD = ZERO


C--------------move to here to keep TAGNCONT for output
      IF(NLOADP_HYD_INTER > 0 )THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        DO K=1,NLOADP_HYD_INTER
           TAGNCONT(K,NODFTSK:NODLTSK) = 0
        ENDDO

!$OMP END PARALLEL

      ENDIF
C--- // 3N ------------------------------
C       INTERFACES AVEC OUVERTURE DE VIDE
C----------------------------------------
      IF(NINTER/=0) THEN

        CALL TRACE_IN(4,1,ZERO)
        IF (IMON>0) CALL STARTIME(2,1)

        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST
        IF (IMPL_S>0) THEN
         IADISK   = 1
        ELSE
         IADISK   = 1
        ENDIF
        CALL INTFOP8(
     1      IPARI  ,X       ,A(1,1) ,
     2      ICODT  ,FSAV    ,WA(1),V         ,MS             ,
     3      DT2TT  ,NELTSTT ,ITYPTSTT   ,ITAB      ,STIFN(1)   ,
     4      NPC    ,TF      ,FSKYI     ,ISKY      ,VR              ,
     5      FANI   ,IN      ,BUFSF     ,FANI(1,NFNCA+1) ,
     6      FANI(1,NFTCA+1) ,ICONTACT  ,RCONTACT  ,NUM_IMPL(1,1),
     7      NS_IMP(IADISK),NE_IMP(IADISK),NT_IMP  ,SENSOR_TAB,INTBUF_TAB ,
     8      H3D_DATA      ,PSKIDS      ,TAGNCONT,KLOADPINTER,LOADPINTER,
     9      LOADP_HYD_INTER)

        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        ENDIF


C=======================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,IADISK)
!$OMP+ PRIVATE(DT2TT,NELTSTT,ITYPTSTT,GREFTSK,GRELTSK)

C Init var parallel SMP
        CALL SMP_INIT(
     1    ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2    IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST


        IF(ISTATCND /= 0 .AND. IPARIT == 0)THEN
          DO N=1,NUMNOD
            STCND (NDTSK+N-1)=-STIFN (NDTSK+N-1)
          END DO
        END IF

        IF (IMPL_S>0) THEN
         IADISK   = 1+ITSK*NINT7
        ELSE
         IADISK   = 1
        ENDIF

        CALL INTFOP1(
     1      IPARI  ,X       ,A(1,NDTSK) ,
     2      ICODT  ,FSAV    ,WA(NWAFTSK),V         ,MS             ,
     3      DT2TT  ,NELTSTT ,ITYPTSTT   ,ITAB      ,STIFN(NDTSK)   ,
     4      NPC    ,TF      ,FSKYI     ,ISKY      ,VR              ,
     6      FANI   ,IN      ,IGRSURF   ,BUFSF     ,FANI(1,NFNCA+1) ,
     7      FANI(1,NFTCA+1) ,ICONTACT  ,RCONTACT  ,NUM_IMPL(1,ITSK+1),
     8      NS_IMP(IADISK),NE_IMP(IADISK),NT_IMP  ,SENSOR_TAB,INTBUF_TAB,
     9      H3D_DATA   )

#include "lockon.inc"
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        ENDIF
#include "lockoff.inc"

!$OMP END PARALLEL

        IF(IMPL_S>0) THEN
         CALL RE2INT5(NT_IMP,NUM_IMP,NS_IMP,NE_IMP,NUM_IMPL,IPARI,NINT7)
         NT_IMP5=NT_IMP
        END IF
C
        IF (IMON>0) CALL STOPTIME(2,1)
        CALL TRACE_OUT(4)

C----------------------------------------
C       INTERFACES: computation of forces
C----------------------------------------
        CALL TRACE_IN(4,2,ZERO)
        IF (IMON>0) CALL STARTIME(8,1)

          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20

C--------------------------------------------------------
C INTERFACE 25 - Communication IRTLM, TIME_S, etc :: Reception
C--------------------------------------------------------
        IF(NSPMD > 1)THEN
         IF(NINTER25 /= 0)THEN
          IF (IMON>0) CALL STARTIME(10,1)        
            CALL SPMD_EXCH_I25(IPARI    ,INTBUF_TAB       ,ITAB  ,
     *                         IAD_ELEM ,FR_ELEM ,INTLIST ,NBINTC,
     *                         IAD_I25  ,FR_I25  ,SFR_I25 ,3     )
            CALL SPMD_EXCH_I25(IPARI    ,INTBUF_TAB       ,ITAB  ,
     *                         IAD_ELEM ,FR_ELEM ,INTLIST ,NBINTC,
     *                         IAD_I25  ,FR_I25  ,SFR_I25 ,4     )
          IF (IMON>0) CALL STOPTIME(10,1)
         ENDIF
        END IF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

        NCONT = 0 ! Initialisation number of nodes tagged for FCONT storage
        IF (IMPL_S>0) THEN
         NTMP = NT_IMP5 + NT_IMP1
        ENDIF

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IADISK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)
!$OMP+ PRIVATE(IDX_FTHE,IDX_CONDN,IDX_PINCH)
C Init var parallel SMP
        CALL SMP_INIT(
     1    ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2    IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)
        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST

         IF (IMPL_S>0) THEN
          IADISK   = 1+NTMP+ITSK*(NINT7-NTMP)
         ELSE
          IADISK   = 1
         ENDIF
         IDX_FTHE = NDTSK
         IF(NDTSK>IFTHE) IDX_FTHE  = 1
         IDX_CONDN = NDTSK
         IF(NDTSK>ICONDN)IDX_CONDN = 1
         IDX_PINCH = NDTSK
         IF(NPINCH == 0 )IDX_PINCH = 1
       
           CALL INTFOP2(
     1     IPARI             ,X_A            ,A(1,NDTSK)     ,IGROUPS         ,ALE_CONNECTIVITY,
     2     ICODT             ,FSAV           ,V              ,MS              ,DT2TT           ,
     3     NELTSTT           ,ITYPTSTT       ,ITAB           ,STIFN(NDTSK)    ,TF              ,
     4     FSKYI             ,ISKY           ,VR             ,FANI            ,SECFCUM         ,
     5     ITSK+1            ,NISKYFI        ,KINET          ,NEWFRONT        ,NSTRF           ,
     6     ICONTACT          ,VISCN(NDTSK)   ,XCELL          ,
     8     NUM_IMPL(1,ITSK+1),NS_IMP(IADISK) ,NE_IMP(IADISK) ,IND_IMP(IADISK) ,NT_IMP          ,
     9     FR_I18            ,IGRBRIC        ,EMINX           ,
     A     IXS               ,IXS(L3)        ,IXS(L2)        ,FANI(1,NFNCA+1) ,FANI(1,NFTCA+1) ,
     B     IAD_ELEM          ,FR_ELEM        ,RCONTACT       ,ACONTACT        ,PCONTACT        ,
     C     TEMP              ,FTHE(IDX_FTHE) ,FTHESKYI       ,IPARG           ,  
     D     PM                ,INTSTAMP       ,WEIGHT         ,NISKYFIE        ,IRLEN20         , 
     E     ISLEN20           ,IRLEN20T       ,ISLEN20T       ,IRLEN20E        ,ISLEN20E        , 
     F     MSKYI_SMS         ,ISKYI_SMS      ,NATIV_SMS      ,INT18ADD        ,FCONTG          , 
     G     FNCONTG           ,FTCONTG        ,NODGLOB        ,MS0             ,NPC             , 
     H     WA                ,SENSOR_TAB     ,QFRICINT       ,NCONT           ,INDEXCONT       , 
     I     TAGCONT           ,INOD_PXFEM     ,MS_PLY         ,WAGAP           ,ELBUF_TAB       ,
     J     CONDN(IDX_CONDN)  ,CONDNSKYI      ,INTBUF_TAB     ,NV46            ,
     K     SFSAV6         ,FBSAV6          ,STABSEN        ,
     L     TABSENSOR         ,ISENSINT       ,NISUBMAX       ,NB25_CANDT      ,NB25_IMPCT      ,
     M     NB25_DST1         ,NB25_DST2      ,IXIG3D         ,KXIG3D          ,WIGE            ,
     N     KNOT              ,IGEO           ,MULTI_FVM      ,H3D_DATA        ,INTBUF_FRIC_TAB ,
     P     PSKIDS            ,T2MAIN_SMS   ,FORNEQS        ,KNOTLOCPC       ,KNOTLOCEL      ,
     Q     PINCH_DATA%APINCH(1,IDX_PINCH),PINCH_DATA%STIFPINCH(IDX_PINCH),T2FAC_SMS,TAGNCONT   ,
     R     KLOADPINTER       ,LOADPINTER     ,LOADP_HYD_INTER ,DGAPLOADINT    ,S_LOADPINTER   )

         IF(NINTSTAMP/=0.AND.(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0.OR.
     .          ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0))THEN
            CALL MY_BARRIER
            IF(NCONT/=0) THEN
              DO I=ITSK+1,NCONT,NTHREAD
                TAGCONT(INDEXCONT(I)) = 0
              ENDDO
            ENDIF
         ENDIF  

#include "lockon.inc"
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        END IF
#include "lockoff.inc"

         IF(ISTATCND /= 0 .AND. IPARIT == 0)THEN
          DO N=1,NUMNOD
            STCND (NDTSK+N-1) = STCND (NDTSK+N-1) + STIFN (NDTSK+N-1)
          END DO
         ENDIF
         IF (IPARIT == 0 .AND. NSPMD > 1 .AND. NTHREAD > 1) THEN
           CALL MY_BARRIER()
           CALL ASSPARXX(ITSK, INTLIST,NBINTC,IPARI)
         ENDIF
!$OMP END PARALLEL

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

        IF(IMPL_S>0) CALL RE2INT7(NT_IMP,NUM_IMP,NS_IMP,NE_IMP,
     1                              IND_IMP,NUM_IMPL,IPARI,NINT7 )
C
          IF (IMON>0) THEN
          CALL STOPTIME(8,1)
          CALL STARTIME(10,1)
          ENDIF
        IF (IMONM > 0) CALL STARTIME(21,1)

C--------------------------------------------------------
C Communication contact forces (SPMD), Part1 : Send
C--------------------------------------------------------

        IF(NSPMD>1)THEN
C
           IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0.AND.
     .      ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D .OR.
     .      (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .      .OR.H3D_DATA%N_VECT_PCONT_MAX >0)THEN        ! comm a faire avant appel i7fcom
            CALL SPMD_EXCH_PRESS( 
     1        IPARI          ,INTLIST ,NBINTC  ,FANI(1,NFNCA+1),
     2        FANI(1,NFTCA+1),ISLEN7 ,IRLEN7  ,IRLEN7T ,ISLEN7T        ,
     3        IRLEN20        ,ISLEN20,IRLEN20T,ISLEN20T,INTBUF_TAB    )
           END IF
C
           L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
           L2 = L1+6*NUMELS10
           L3 = L2+12*NUMELS20
           IF(IPARIT==0) THEN
             CALL SPMD_I7FCOM_POFF(
     1         IPARI     ,A      ,STIFN   ,VISCN   ,
     2         INTLIST   ,NBINTC  ,ICODT  ,SECFCUM ,NSTRF   ,
     3         ICONTACT  ,FANI    ,ISLEN7 ,IRLEN7  ,ISLEN11 ,
     4         IRLEN11   ,ISLEN17 ,IRLEN17,IGRBRIC ,
     5         IXS       ,IXS(L3) ,FTHE    ,IRLEN7T ,
     6         ISLEN7T   ,IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,
     7         IRLEN20E  ,ISLEN20E,CONDN  ,1, INTBUF_TAB    ,
     8         H3D_DATA, MULTI_FVM,TAGNCONT      ,
     9         KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
           ELSE

             CALL SPMD_I7FCOM_PON(
     1         IPARI     ,INTLIST,
     2         NBINTC    ,NISKYFI,ICODT   ,SECFCUM ,NSTRF  ,
     3         ICONTACT  ,FANI   ,IGRBRIC ,IXS     ,
     4         IXS(L3)   ,NISKYFIE,NBINT20,
     5         1, INTBUF_TAB  , SFSKYI   ,
     6         SISKY ,H3D_DATA,MULTI_FVM ,TAGNCONT  ,
     7         KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)

             IF(MULTI_FVM%IS_INT18_LAW151) THEN
              IF(NTHREAD>1) CALL INT18_LAW151_OMP_ACCUMULATION( MULTI_FVM )
              CALL SPMD_INT18_LAW151_PON( IPARI,ISLEN7,IRLEN7,1,INTBUF_TAB,
     1                                 MULTI_FVM )
             ENDIF
           END IF
          ENDIF
        IF (IMON>0) CALL STOPTIME(10,1)
        IF (IMONM > 0) CALL STOPTIME(21,1)

          CALL TRACE_OUT(4)
      ENDIF   ! fin NINTER > 0
C--------------------------------------------------------
C INTERFACE 24 - Communication part 1 / 4
C--------------------------------------------------------
      IF (INT24USE == 1)THEN
        IF (IMON>0) CALL STARTIME(8,1)        
          CALL SPMD_EXCH_I24(IPARI    ,INTBUF_TAB       ,ITAB  ,
     *                       IAD_ELEM ,FR_ELEM ,INTLIST ,NBINTC,
     *                       IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,1,
     *                       INT24E2EUSE )
   
        IF (IMON>0) CALL STOPTIME(8,1)
      ENDIF
C--------------------------------------------------------
C INTERFACE 21 - Communication of heat flux
C--------------------------------------------------------
        IF(NSPMD>1)THEN
          IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1) THEN
             CALL SPMD_I21FTHECOM(IPARI  ,FTHE  ,INTBUF_TAB  ,SENSOR_TAB,NISKYFI ,
     .                            FTHESKYI,ISKY ,FSKYI       ,CONDNSKYI) 
          ENDIF
         ENDIF
C--------------------------------------------------------
C Itet=2 STIFND <- STIFN part of interface only
C--------------------------------------------------------
      IF (NS10E > 0.AND.IPARIT == 0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        GREFTSK   = 1+ITSK*NS10E/ NTHREAD
        GRELTSK   = (ITSK+1)*NS10E/NTHREAD
       CALL S10CNDFND(ICNDS10,WEIGHT ,IAD_CNDS,FR_CNDS,ITAB   ,
     2                NODFTSK,NODLTSK,GREFTSK,GRELTSK,ITSK   ,
     3                STIFN ,STIFND)
!$OMP END PARALLEL
      END IF

C--------------------------------------------------------
C     SHELLS SUB-CYCLING
C--------------------------------------------------------

      IF(ISHSUB==1)THEN
C kshsub=-1 on assemble ce qui a ete calcule jusqu'a present,
C           avant de calculer les coques
        IF(KSHSUB==-1) GOTO 221
      ENDIF

 20   CONTINUE

      IF(ISHSUB==1)THEN

          IF(KSHSUB==-1)THEN
C kshsub=-1 on a assemble ce qui a ete calcule jusqu'a present,
C           on va calculer les coques
            KSHSUB = 0
            DT1SAVC= DT1
            DT1    = TT-T1SH
            T1SH   = TT
            DT2S   = EP30
          ENDIF

        IF(KSHSUB==0)THEN
C         preparation avant de calculer les coques
          NELTSA  = NELTST
          ITYPTSA = ITYPTST
          DT2SAVE = DT2T
          DT2T    = EP30

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK)
          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

          CALL SHSUB1(LSHSUB,V       ,VR      ,DSAVE   ,
     2           D        ,ITSK   ,NODFTSK   ,NODLTSK   ,
     3      FSKY   ,FSKY    ,DR      ,DRSAVE  ,
     4      A   ,ASAVE   ,AR      ,ARSAVE  ,
     5      STIFN    ,STSAVE  ,STIFR   ,STRSAVE ,
     6      FSAVSAV )

!$OMP END PARALLEL

        END IF
      ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------------------------------------
C       VOLUMES MONITORES
C--------------------------------------------------------

        IF(NVOLU/=0)THEN
        IF (IMONM > 0) CALL STARTIME(50,1)
          CALL TRACE_IN(11,0,ZERO)
          NN = NUMELC+NUMELTG+IBAGSURF
          N0 = 1 + 3*NN
          IF(INTBAG/=0)THEN
            N1 = N0+ NN
          ELSE
            N1 = N0
          ENDIF
C
          CALL FVDIM(MONVOL)
          CALL FVCOPY(MONVOL)
          CALL FVMESH0(MONVOL,  X, VOLMON, IXS)
          CALL FVREZONE0(MONVOL, X)
          CALL FVUPD0(MONVOL, X, V, VOLMON)
          N=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
          CALL FVBAG0(MONVOL,    VOLMON,  X,         SENSOR_TAB,   V,
     .                A,         NPC,     TF,
     .                FSAV(1,N), IFVMESH, ICONTACT_OLD,  LGAUGE,
     .                GAUGE    , IGEO,    GEO,       PM,       IPM,
     .                IPARG    , IGROUPTG,IGROUPC,   ELBUF_TAB,FEXT,
     .                1        , H3D_DATA )
C
          CALL TRACE_OUT(11)
        IF (IMONM > 0) CALL STOPTIME(50,1)
        ENDIF
  

      IF(NSPMD > 1 .AND. NVOLU > 0 .AND. NFVBAG0 > 0) THEN
        ! DT2R : value to be minimized over the processor
        DT2R = DT2

        ! Fill MIN_TAB with integer
        MIN_TAB(1) = NELTS 
        MIN_TAB(2) = ITYPTS 
        MIN_TAB(3) = 0 
        MIN_TAB(4) = ISPMD 
        !
        ! Begin Asynchronous communication
        !
        CALL MPI_MIN_REAL_BEGIN(DT2R,MIN_TAB,4,MPI_BUF) 
        !                        |     |     |    |
        !Value to be minimized --*     |     |    |
        !Integers array ---------------*     |    |
        !Size of Integers array -------------*    |
        !Internal Structure ----------------------*
        !
        ! After this call DT2R and MIN_TAB are unchanged
        ! The minimum value of DT2R and the corresponding MIN_TAB
        ! Will be received after a call to  MPI_MIN_REAL_END
      ENDIF

      IF(ISHSUB==0)THEN
          T1SH=TT
      ENDIF
C
C-----------------------------------------------------
C     UPDATE OF SLIPRING AND RETRACTOR
C-----------------------------------------------------

      IF (NSLIPRING + NRETRACTOR> 0) CALL UPDATE_SLIPRING(IXR,IXC,IPARG,ELBUF_TAB,FLAG_SLIPRING_UPDATE,
     .                                                    FLAG_RETRACTOR_UPDATE,X,NPBY)
C
C---- // GROUPS ----------------
C     FORCES INTERNES DES COQUES, COQUES 3 NOEUDS
C-------------------------------
C   no subcycling, or subcycling and KSHSUB==0 :
      IF(KSHSUB==0)THEN
        CALL TRACE_IN(14,0,ZERO)
        IF (IMON>0) CALL STARTIME(3,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C -------------------------------------------------------------------        
C   User Libraries get the possibility to use GET_U_NOD_X & GET_U_NOD_V in user elements properties (Solids & Springs)
        GETUNOD_NOCOM=1
C -------------------------------------------------------------------        
        LLT1 = I87G+3*NUMELTG
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)
!$OMP+ PRIVATE(IDX_FTHE,IDX_CONDN,IDX_PINCH)
C Init var parallel SMP
        CALL SMP_INIT(
     1   ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK,
     2   IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)
        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST
        
        IAD_GREL = 1
        IF(IGRELEM == 1)IAD_GREL = K3-K1+1

        IDX_FTHE = NDTSK
        IF(NDTSK>IFTHE)IDX_FTHE=1
        IDX_CONDN = NDTSK
        IF(NDTSK>ICONDN)IDX_CONDN=1
        IDX_PINCH = NDTSK
        IF(NPINCH == 0 )IDX_PINCH = 1
        CALL FORINTC(
     1   PM           ,GEO             ,X               ,A(1,NDTSK)     ,AR(1,NDTSK)  ,
     2   V            ,VR              ,MS              ,IN             ,NLOC_DMG     , 
     3   WA(NWAFTSK)  ,STIFN(NDTSK)    ,STIFR(NDTSK)    ,FSKY           ,CRKSKY       , 
     4   TF           ,BUFMAT          ,PARTSAV(IPMTSK) ,D              ,MATPARAM_TAB , 
     5   DR           ,EANI            ,TANI            ,FANI(1,NFOA+1) ,       
     6   FSAV         ,SENSOR_TAB      ,SKEW            ,ANIN(NDMA2+1)  ,FAILWAVE     ,               
     7   DT2TT        ,THKE            ,BUFGEO          ,IADS(I87C)     ,IADS(I87G)   , 
     8   IPARG        ,NPC             ,IXC             ,IXTG           ,NELTSTT      , 
     9   IPARI        ,ITYPTSTT        ,NSTRF          ,          
     A   IPART        ,IPART(K3)       ,IPART(K8)       ,SECFCUM        ,                  
     B   FSAVD        ,GROUP_PARAM_TAB , 
     E   FZERO        ,IXTG1           ,IADS(LLT1)      ,IGEO           ,IPM          ,
     F   MADFAIL      ,XSEC            ,FSAVSAV         ,ITSK           ,MCP          ,
     G   TEMP         ,FTHE(IDX_FTHE)       ,FTHESKY         ,
     H   MS_PLY       ,ZI_PLY          ,INOD_PXFEM      ,XEDGE4N       ,XEDGE3N       ,
     I   IEL_PXFEM    ,IADC_PXFEM      ,IGROUC          ,NGROUC        ,GRESAV(IGMTSK),
     J   GRTH         ,IGRTH(IAD_GREL) ,MSTG            ,DMELTG        ,MSC           ,
     K   DMELC        ,TABLE           ,KXFENOD2ELC     ,PTG           ,MSZ2          ,
     L   INOD_CRK     ,IEL_CRK         ,IADC_CRK        ,ELCUTC        ,NODENR        ,
     M   IBORDNODE    ,NODEDGE         ,CRKNODIAD       ,ELBUF_TAB     ,
     N   XFEM_TAB     ,CONDN(IDX_CONDN),CONDNSKY        ,CRKEDGE       ,
     O   SFSAV6       ,FBSAV6          ,STABSEN         ,TABSENSOR     ,STACK         ,ITAB  ,
     Q   DRAPE_Q4     ,DRAPE_T3        ,SUBSETS, XDP    ,PINCH_DATA%VPINCH   ,
     R   PINCH_DATA%APINCH(1,IDX_PINCH)    ,PINCH_DATA%STIFPINCH(IDX_PINCH)    )     
#include "lockon.inc"
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        END IF
#include "lockoff.inc"

!$OMP END PARALLEL
C
        CALL TRACE_OUT(14)
        IF (IMON>0) CALL STOPTIME(3,1)
      END IF

C--------------------------------------------------------
C     SHELLS SUB-CYCLING
C--------------------------------------------------------

      IF(ISHSUB==1)THEN
          IF(KSHSUB==0) THEN
            KSHSUB=2
          ENDIF
        IF(KSHSUB==1) THEN
C kshsub=1 shells are not computed this cycle, retrieve internal forces

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK)
          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

          CALL SHSUB2(ITSK, NODFTSK, NODLTSK,
     .               A, ASAVE, AR, ARSAVE,
     .                  FSAV(1,NINTER+NRWALL+NRBODY+1),
     .               FSAVSAV)

!$OMP END PARALLEL
      ENDIF

      ENDIF

      IF(ISHSUB ==1 .AND. KSHSUB==2)GOTO 22
C  implicit barrier before asspar

C---- // GROUPS ----------------
C     INTERNAL FORCES OF  TRUSSES, POUTRES, RESSORTS,
C                         SOLIDES, QUAD
C-------------------------------

 211  CONTINUE
      CALL TRACE_IN(14,0,ZERO)
      IF (IMON>0) CALL STARTIME(3,1)
      L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
      L2 = L1+6*NUMELS10
      L3 = L2+12*NUMELS20
      LL1 = 1+8*NUMELS
      LL2 = LL1+6*NUMELS10
      LL3 = LL2+12*NUMELS20
C
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(I16TSK,IGMTSK,GREFTSK,GRELTSK)
!$OMP+ PRIVATE(IDX_FTHE,IDX_CONDN)
C Init var parallel SMP
      CALL SMP_INIT(
     1   ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK,
     2   IPMTSK,PARTFTSK ,PARTLTSK ,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)
      DT2TT = DT2T
      NELTSTT = NELTST
      ITYPTSTT= ITYPTST
      I16TSK = 1+ITSK*(SW16/NTHREAD)
      IDX_FTHE = NDTSK
      IF(NDTSK>IFTHE) IDX_FTHE  = 1
      IDX_CONDN = NDTSK
      IF(NDTSK>ICONDN)IDX_CONDN = 1
C
      CALL FORINT(
     1 PM             ,GEO           ,X           ,A(1,NDTSK) ,AR(1,NDTSK)     ,     
     2 V              ,VR            ,MS          ,IN         ,W               ,      
     3 ELBUF          ,WA(NWAFTSK)   ,VAL2        ,VEUL       ,FV              ,     
     4 STIFN(NDTSK)   ,STIFR(NDTSK)  ,FSKY        ,TF         ,BUFMAT          ,     
     5 PARTSAV(IPMTSK),D             ,DR          ,EANI       ,ELBUF_TAB       ,
     6 TANI           ,FANI(1,NFOA+1),FSAV        ,SENSOR_TAB ,NLOC_DMG        ,
     7 SKEW           ,ANIN(NDMA2+1) ,DT2TT       ,BUFGEO     ,ITAB            ,
     8 IADS           ,IADS(I87B)    ,IADS(I87D)  ,IADS(I87E) ,MATPARAM_TAB    ,
     9 IADS(I87F)     ,IPARG         ,ALE_CONNECTIVITY,NPC    ,
     A IXS            ,IXQ           ,IXT         ,IXP        ,
     B IXR            ,NELTSTT       ,IPARI       ,
     C ITYPTSTT       ,NSTRF         ,IPART       ,
     D IPART(K1)      ,IPART(K2)     ,IPART(K4)   ,IPART(K5)  ,
     E IPART(K6)      ,IPART(K7)     ,FR_WAVE     ,IXUR       ,RBY           ,
     F IADS(I87H)     ,SECFCUM       ,AGRV        ,IGRV       ,LGRAV           ,
     G IXS(L1)        ,
     H IXS(L2)        ,IADS(LL1)     ,IADS(LL2)   ,IXS(L3)    ,IADS(LL3)       ,
     I W16(I16TSK)    ,FSKYM         ,MSNF        ,IGEO       ,IPM             ,
     J XSEC           ,ITSK          ,TEMP           ,
     K FTHE(IDX_FTHE) ,FTHESKY                    ,IGROUNC    ,NGROUNC         ,
     M GRESAV(IGMTSK) ,GRTH          ,IGRTH       ,XDP        ,MSSA            ,
     N DMELS          ,MSTR          ,DMELTR      ,MSP        ,DMELP           ,
     O MSRT           ,DMELRT        ,TABLE       ,VFLOW      ,AFLOW           ,
     P DFLOW          ,WFLOW         ,FFSKY       ,AFLOW      ,NBSDVOIS        ,
     Q NERCVOIS       ,NESDVOIS      ,LERCVOIS    ,LESDVOIS   ,PHI             ,
     R PHIE           ,MSF           ,NODFTSK     ,NODLTSK    ,
     S FLG_KJ2        ,POR           ,IFOAM_CONT  ,IFOAM      ,VARNOD          ,
     T KXIG3D         ,IXIG3D        ,KNOT        ,WIGE       ,CONDN(IDX_CONDN),
     U CONDNSKY       ,SFSAV6        ,FBSAV6      ,STABSEN    ,TABSENSOR       ,
     V TAGPRT_SMS     ,ITAGND        ,MS_2D       ,ALE_CONNECTIVITY%NALE       ,STRESSMEAN      ,
     W KNOTLOCPC      ,KNOTLOCEL     ,SUBSETS     ,FLAG_SLIPRING_UPDATE, FLAG_RETRACTOR_UPDATE  ,
     Y H3D_DATA)
C     
#include "lockon.inc"
      IF(DT2TT<DT2T)THEN
        DT2T = DT2TT
        NELTST = NELTSTT
        ITYPTST= ITYPTSTT
      END IF
#include "lockoff.inc"
!$OMP END PARALLEL
      CALL TRACE_OUT(14)

      IF (IMON>0) CALL STOPTIME(3,1)
C-----
      IF(NUMSPHG/=0)THEN
        IF (IMONM > 0) CALL STARTIME(48,1)
        IF (IMONM > 0) CALL STARTIME(87,1)
        CALL TRACE_IN(13,0,ZERO)
C-----------------------------------------------
C     SPH SORT, SYMMETRIZATION AND CSPH PREPARATION & SPH INLETS/OUTLETS (After forint)
C-----------------------------------------------
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)
C Init var parallel SMP
        CALL SMP_INIT(
     1   ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK,
     2   IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

        CALL SPHPREP(
     1    PM      ,GEO     ,X       ,V       ,MS      ,
     2    ELBUF_TAB,WA      ,TF      ,BUFMAT  ,PARTSAV ,
     3    IPARG   ,NPC     ,IPART   ,ITAB    ,BUFGEO  ,
     4    XFRAME  ,KXSP    ,IXSP    ,NOD2SP  ,IPART(K10),
     5    SPBUF   ,ISPCOND ,ISPSYM  ,XSPSYM  ,VSPSYM    ,
     6    WASPH(KSPH21) ,LPRTSPH ,LONFSPH ,WASPH(KSP2SORT) ,
     7    ISPHIO  ,VSPHIO  ,IGRSURF ,D        ,
     8    SPHVELN ,ITSK    ,XDP     ,IBUFSSG_IO,LGAUGE  ,
     9    GAUGE   ,NGROUNC ,IGROUNC ,SOL2SPH ,SPH2SOL   ,
     A    IXS     ,IADS    ,ADSKY   ,FSKYD   ,DMSPH(NDTSK),
     B    WASPH(KSPACTIV),ICONTACT_OLD,OFF_SPH_R2R,WSMCOMP,IRUNN_BIS)
!$OMP END PARALLEL

        IF (IMONM > 0) CALL STOPTIME(87,1)
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
        IF (IMONM > 0) CALL STARTIME(88,1)

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK)
!$OMP+ PRIVATE(DT2TT,NELTSTT,ITYPTSTT,IGMTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
        CALL SMP_INIT(
     1    ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2    IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST
        
        IAD_GREL = 1
        IF(IGRELEM == 1)IAD_GREL = K10-K1+1       

C----------------------------------
C       SPH: Internal forces 
C----------------------------------
        CALL FORINTP(
     1    PM            ,GEO            ,X          ,A(1,NDTSK)      ,V             ,   
     2    MS            ,W              ,ELBUF_TAB  ,WA              ,FV            , 
     3    STIFN(NDTSK)  ,TF             ,BUFMAT     ,PARTSAV(IPMTSK) ,NLOC_DMG      ,    
     4    FSAV          ,DT2TT          ,IADS       ,IPARG           ,NPC           ,  
     5    NELTSTT       ,ITYPTSTT       ,IPART      ,ITAB            ,ISKY          ,  
     6    BUFGEO        ,FSKYI          ,XFRAME     ,KXSP            ,IXSP          ,  
     7    NOD2SP        ,IPART(K10)     ,SPBUF      ,ISPCOND         ,ISPSYM        ,  
     8    XSPSYM%BUF    ,VSPSYM%BUF     ,                 
     9    WASPH(KSPH21) ,LPRTSPH        ,LONFSPH    ,WASPH(KSPACTIV) ,ISPHIO        ,
     A    VSPHIO        ,SPHVELN        ,ITSK       ,IPM             ,GRESAV(IGMTSK),
     B    GRTH          ,IGRTH(IAD_GREL),TABLE      ,LGAUGE          ,GAUGE         ,
     C    NGROUNC       ,IGROUNC        ,IXS        ,IRST            ,SOL2SPH       ,
     D    SPH2SOL       ,FSKY           ,FSKY       ,IGEO            ,TEMP          ,
     E    FTHE          ,FTHESKYI       ,SPHG_F6    ,WSMCOMP%BUF     ,SOL2SPH_TYP   ,
     F    MATPARAM_TAB  )
#include "lockon.inc"
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        END IF
#include "lockoff.inc"

!$OMP END PARALLEL

C -------------------------------------------------------------------        
C   User Libraries get the possibility to use GET_U_NOD_X & GET_U_NOD_V in user elements properties (Solids & Springs)
        GETUNOD_NOCOM=0
C -------------------------------------------------------------------        

C=======================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

        CALL TRACE_OUT(13)
        IF (IMONM > 0) CALL STOPTIME(88,1)
        IF (IMONM > 0) CALL STOPTIME(48,1)
      ENDIF

C-----------------------------------------------
C Multidomains : synchro proc of flag_activation
C----------------------------------------------
      IF (NSPMD>1) THEN
        IF ((SDD_R2R_ELEM>0).AND.(FLG_SPHINOUT_R2R>0)) THEN
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          CALL SPMD_EXCH_R2R_SPHOFF(OFF_SPH_R2R,IAD_ELEM,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
          CALL SPMD_EXCH_R2R_SPH(X,IAD_ELEM,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
          CALL SPMD_EXCH_R2R_SPH(D,IAD_ELEM,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
          CALL SPMD_EXCH_R2R_SPH(V,IAD_ELEM,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
        ENDIF
      ENDIF


        IF(NVOLU/=0)THEN
        IF (IMONM > 0) CALL STARTIME(50,1)
          CALL TRACE_IN(11,0,ZERO)
          NN = NUMELC+NUMELTG+IBAGSURF
          N0 = 1 + 3*NN
          IF(INTBAG/=0)THEN
            N1 = N0+ NN
          ELSE
            N1 = N0
          ENDIF
C
          N=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
          CALL FVBAG0(MONVOL,    VOLMON,  X,         SENSOR_TAB,   V,
     .                A,         NPC,     TF,
     .                FSAV(1,N), IFVMESH, ICONTACT_OLD,  LGAUGE,
     .                GAUGE    , IGEO,    GEO,       PM,       IPM,
     .                IPARG    , IGROUPTG,IGROUPC,   ELBUF_TAB,FEXT,
     .                2        , H3D_DATA )
           CALL MONVOL0(
     1      MONVOL     ,VOLMON     ,X_A            ,A         ,
     2      NPC        ,TF         ,V              ,WA        ,
     3      FSAV(1,N)  ,SENSOR_TAB ,IGRSURF   ,
     4      FR_MV      ,IADS(I87I) ,
     5      FSKY       ,ICONTACT   ,WA(N0)         ,IPARG     ,
     6      ELBUF_TAB  ,GEO        ,IGEO           ,
     7      PM         ,IPM        ,IPART          ,IPART(K3) ,
     8      IPART(K8)  ,IGROUPC    ,IGROUPTG       ,FEXT      ,
     9      2          ,H3D_DATA   ,T_MONVOL)


          CALL TRACE_OUT(11)
        IF (IMONM > 0) CALL STOPTIME(50,1)
        ENDIF
        
C     ----------------------
C     subcycling ISHSUB==1
C     ----------------------
C     KSHSUB=-1 <=> Shell at this cycle  and 1st phase assembly
C                   (contact force)                      
C     KSHSUB=1  <=> No shell computation at this cycle 
      IF(ISHSUB==1.AND.KSHSUB/=-1.AND.KSHSUB/=1) GOTO 22

 221  CONTINUE

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (NS10E > 0 .AND. IPARIT==0) THEN
C must be done before the reception of remote contact stif
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
             ITSK = OMP_GET_THREAD_NUM()
             NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
             NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
             GREFTSK   = 1+ITSK*NS10E/ NTHREAD
             GRELTSK   = (ITSK+1)*NS10E/NTHREAD
             CALL S10STFE_POFF(ICNDS10,WEIGHT ,IAD_CNDS,FR_CNDS,ITAB   ,
     2                NODFTSK,NODLTSK,GREFTSK,GRELTSK,ITSK   ,
     3                STIFN ,STIFND)
!$OMP END PARALLEL
      END IF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------------------
C Communication of interface forces (SPMD), Part2 : Reception
C----------------------------------------
       IF(NINTER/=0) THEN
        IF(NSPMD>1)THEN
          IF(IMONM == 2)THEN
            CALL STARTIME(59,1)
            CALL SPMD_BARRIER()
            CALL STOPTIME(59,1)
          ENDIF
          IF (IMON>0) CALL STARTIME(10,1)
          IF (IMONM > 0) CALL STARTIME(22,1)
C
          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
          IF(IPARIT==0)THEN

            CALL SPMD_I7FCOM_POFF(
     1         IPARI     ,A    ,STIFN   ,VISCN   ,
     2         INTLIST   ,NBINTC  ,ICODT   ,SECFCUM ,NSTRF   ,
     3         ICONTACT  ,FANI    ,ISLEN7  ,IRLEN7  ,ISLEN11 ,
     4         IRLEN11   ,ISLEN17 ,IRLEN17 ,IGRBRIC ,
     5         IXS       ,IXS(L3) ,FTHE    ,IRLEN7T ,
     6         ISLEN7T   ,IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T,
     7         IRLEN20E  ,ISLEN20E,CONDN   ,2, INTBUF_TAB    ,
     8         H3D_DATA, MULTI_FVM,TAGNCONT       ,
     9         KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
          ELSE
            CALL SPMD_I7FCOM_PON(
     1         IPARI     ,INTLIST,
     2         NBINTC    ,NISKYFI,ICODT   ,SECFCUM ,NSTRF  ,
     3         ICONTACT  ,FANI   ,IGRBRIC ,IXS     ,
     4         IXS(L3)   ,NISKYFIE,NBINT20,
     5         2         ,INTBUF_TAB      ,SFSKYI   ,
     6         SISKY     ,H3D_DATA,MULTI_FVM,TAGNCONT,
     7         KLOADPINTER,LOADPINTER,LOADP_HYD_INTER)
          END IF
C
          IF (IMON>0) THEN
            CALL STOPTIME(10,1)
            IF (IMONM > 0) CALL STOPTIME(22,1)
          ENDIF
         ENDIF
       ENDIF

 22   CONTINUE

!   Mpi communication for Nlocal option : parith/off
       IF(IPARIT /= 0.AND.NSPMD > 1.AND. NLOC_DMG%IMOD > 0)THEN
            CALL SPMD_EXCH_SUB_PON(NLOC_DMG)
       ENDIF
       IF(NSPMD > 1.AND. NINTLOADP > 0)THEN
         LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
         CALL SPMD_EXCH_TAGNCONT(TAGNCONT,IAD_ELEM ,FR_ELEM,LENR     )
       ENDIF
!   ----------------------------------

       IF(NLOADP_HYD/=0.AND.IMPL_S/=1) THEN
          FEXT = ZERO
          CALL TRACE_IN(10,0,ZERO)
          IF (IMON>0) CALL STARTIME(4,1)
          IF (IMONM > 0) CALL STARTIME(41,1)
            CALL LOAD_PRESSURE (ILOADP    ,LOADP     ,LLOADP  ,NPC    ,TF       ,
     2                          A         ,V         ,X       ,SKEW   ,SENSOR_TAB,
     3                          IADS(I87M),FSKY    ,FANI(1,1+NFEA),TAGNCONT ,
     4                          KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,H3D_DATA ,
     5                          NPRESLOAD ,LOADP_TAGDEL )
          IF (IMONM > 0) CALL STOPTIME(41,1)
           IF (IMON>0) CALL STOPTIME(4,1)
          CALL TRACE_OUT(10)
       ENDIF
C--- //------------------------------
C       FORCE ASSEMBLY                          
C-------------------------------------

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF(IPARIT == 0 .AND. NTHREAD > 1)THEN
        IF (IMON>0) CALL STARTIME(9,1)

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,PARTFTSK,PARTLTSK,GREFTSK,GRELTSK,NODFT_NL,NODLT_NL)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        PARTFTSK  = 1+ITSK*NPSAV*NPART/NTHREAD
        PARTLTSK  = (ITSK+1)*NPSAV*NPART/NTHREAD
        GREFTSK  = 1+ITSK*NPSAV*NGPE/NTHREAD
        GRELTSK  = (ITSK+1)*NPSAV*NGPE/NTHREAD

C assemblage Parith/OFF necessaire avant communication frontiere si multi-thread

        CALL ASSPAR(
     1    NTHREAD    ,NUMNOD,NODFTSK,NODLTSK,IRODDL,
     2    NPART    ,PARTFTSK,PARTLTSK ,A      ,AR    ,
     3    PARTSAV  ,STIFN ,STIFR  ,VISCN  , FTHE ,
     4    ITHERM_FE,STCND ,GREFTSK,GRELTSK ,
     5    GRESAV   ,NGPE  ,NTHPART ,IALELAG, AFLOW,
     6    DMSPH    ,CONDN ,
     7    PINCH_DATA%APINCH,PINCH_DATA%STIFPINCH)


!   ----------------------------------
!   Accumulation of acceleration for Nlocal option : parith/off
        IF (NLOC_DMG%IMOD > 0) THEN
            NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
            NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
            CALL ASSPAR_SUB_POFF(NLOC_DMG%FNL ,NODFT_NL,NODLT_NL,
     .                           NLOC_DMG%POSI,NLOC_DMG%L_NLOC,NTHREAD )
            ! Non-local nodal stiffness
            IF (NODADT > 0) THEN
              CALL ASSPAR_SUB_POFF(NLOC_DMG%STIFNL,NODFT_NL,NODLT_NL,
     .                             NLOC_DMG%POSI,NLOC_DMG%L_NLOC,NTHREAD )
              CALL NLOCAL_DTNODA(NODFT_NL,NODLT_NL,NLOC_DMG,DTNOD_NLOCAL,DT2T)
            ENDIF
        ENDIF
!$OMP END PARALLEL
        IF (IMON>0) CALL STOPTIME(9,1)
      ENDIF
C Transfer of contact force from sleeping particles to solid nodes

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
      IF(NUMSPH /= 0 .AND. NSPHSOL /= 0 )THEN
        IF (IMONM > 0) CALL STARTIME(48,1)
        IF (IMONM > 0) CALL STARTIME(89,1)

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

        CALL SOLTOSPHF(
     1   A      ,SPBUF   ,IXS     ,KXSP    ,IPART(K10),
     2   NOD2SP ,IRST   ,NGROUNC  ,IGROUNC ,IPARG    ,
     3   STIFN  ,SOL2SPH,SPH2SOL  ,ELBUF_TAB,ITSK    ,
     4   NODFTSK,NODLTSK,ISKY     ,FSKYI    ,IGEO    ,
     5   SOL2SPH_TYP)

!$OMP END PARALLEL

        IF (IMONM > 0) CALL STOPTIME(89,1)
        IF (IMONM > 0) CALL STOPTIME(48,1)
      ENDIF

C===== Nitsche Method Reinit FORNEQS every cycle

        IF(NITSCHE/=0) THEN
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK)

C Init var parallel SMP
            CALL SMP_INIT(
     1        ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2        IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

            CALL ZEROR(FORNEQS(1,NODFTSK),NUMNTSK)

!$OMP END PARALLEL

C====== Nitsche equivalent nodal force computation FORNEQS ===========

        IF (INT24USE == 1)THEN
         CALL  I24NITSCHFOR3 (IPARI   ,INTBUF_TAB ,IPARIT  ,STRESSMEAN ,
     2                        INTLIST ,NBINTC     ,X       ,IADS       ,
     3                        FORNEQS ,FORNEQSKY  ,ITAB    ,IXS        ,
     4                        IADS(LL1),IADS(LL2) ,IADS(LL3),NFACNIT   )
        ENDIF
       ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

      IF(INT18KINE== 1)THEN
        ALLOCATE(MTF(14,NUMNOD))
        ALLOCATE(CAND_SAV(8,INT18ADD(NINTER+1)-1))
        IF (NSPMD > 1)THEN
          ALLOCATE(TAGPENE(NUMNOD))
        ELSE
          ALLOCATE(TAGPENE(1))
        ENDIF
      ENDIF
        INT18ADD(NINTER+1) = -IABS(INT18ADD(NINTER+1))
        INT18KINE=-IABS(INT18KINE)

      IF(NSPMD>1)THEN
        IF (IMON>0) CALL STARTIME(10,1)
        IF (IPARIT==0) THEN
          SIZE =  4 + IRODDL*4
          IF (N2D/=0) THEN
            SIZE = SIZE + 1
            IF(IFSUBM == 1) SIZE = SIZE + 1
          ELSEIF(IFSUBM==1)THEN
            SIZE = SIZE + 2
          ENDIF
C
          IF(ITHERM_FE > 0  )THEN
            SIZE = SIZE + 3
            IF(NODADT_THERM == 1 ) SIZE = SIZE + 1
          ENDIF
C
          IF(IALELAG > 0 )THEN
           SIZE = SIZE + 4
          ENDIF
C
          IF(SOL2SPH_FLAG/=0) SIZE = SIZE + 1
C
          IF(NITSCHE > 0 )THEN
           NFACNIT = 3
          ENDIF
C
          LENC = 0
          IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) LENC = 3
          IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) LENC = LENC+6
C
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          IF(IDTMINS /= 0)THEN

            CALL SPMD_EXCH_A_AMS_POFF(
     1       A       ,AR     ,STIFN,STIFR ,MS   ,
     2       IAD_ELEM,FR_ELEM,MSNF ,IFSUBM,SIZE ,
     3       LENR    ,FTHE , MCP,FR_LOC,NB_FR   ,
     4       MS_2D ,MCP_OFF,FORNEQS ,NFACNIT    ,
     5       LENC ,FANI ,H3D_DATA,FANI(1,NFNCA+1),
     6       FANI(1,NFTCA+1))
          ELSE 
            CALL SPMD_EXCH_A(
     1       A       ,AR     ,STIFN,STIFR ,MS   ,
     2       IAD_ELEM,FR_ELEM,MSNF ,IFSUBM,SIZE ,
     3       LENR    ,FTHE , MCP, DMSPH,CONDN, 
     4       MS_2D,MCP_OFF,
     5       FORNEQS ,NFACNIT,LENC ,FANI ,H3D_DATA,
     6       FANI(1,NFNCA+1) ,FANI(1,NFTCA+1) )

          ENDIF
C
        ELSE
          SIZE =  4 + IRODDL*4
          IF(IFSUBM==1)THEN
            SIZE = SIZE + 1
          ENDIF
          IF(N2D /= 0.AND.IFSUBM == 1) SIZE = SIZE + 1
          SIZI = NFSKYI+1
C
          IF(ITHERM_FE > 0 )THEN
           SIZE = SIZE + 1
           SIZI = SIZI + 1
            IF(NODADT_THERM == 1 ) THEN
               SIZE = SIZE + 1
               SIZI = SIZI + 1
            ENDIF
          ENDIF
          IF(INTPLYXFEM >  0) SIZI = SIZI + 5
C
          IF(IALELAG > 0 )THEN
           SIZE = SIZE + 4
          ENDIF
C
          LENS = FR_NBCC(1,NSPMD+1)
          LENR = FR_NBCC(2,NSPMD+1)
          LENI = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C
          LENS1 = 0
          LENR1 = 0
C
          IF(IPLYXFEM > 0) THEN
           LENS1 = FR_NBCC1(1,NSPMD+1)
           LENR1 = FR_NBCC1(2,NSPMD+1)
          ENDIF
C
          IF(ICRACK3D > 0) THEN
           LENS1 = FR_NBCC1(1,NSPMD+1)
           LENR1 = FR_NBCC1(2,NSPMD+1)
          ENDIF
C
          IF(SOL2SPH_FLAG/=0)THEN
            SIZE = SIZE + 1
          ENDIF
C
          LENC = 0
          IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) LENC = 3*LENI
          IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) LENC = LENC+6*LENI

          CALL SPMD_EXCH2_A_PON(
     1     IAD_ELEM    ,FR_ELEM   ,ADSKY   ,PROCNE    ,FR_NBCC     ,
     2     SIZE        ,LENR      ,LENS    ,FSKY      ,FSKY        ,
     3     FSKYM       ,IFSUBM    ,SIZI    ,LENI      ,IADSDP      ,
     4     IADRCP      ,ISENDP    ,IRECVP  ,FFSKY     ,PROCNE_PXFEM,
     5     FR_NBCC1    ,IADSDP_PXFEM,IADRCP_PXFEM     ,ISENDP_PXFEM,
     6     IRECVP_PXFEM,LENR1     ,LENS1   ,IADSDP_CRK,IADRCP_CRK  ,
     7     ISENDP_CRK  ,IRECVP_CRK,FSKYD   ,CRKNODIAD ,CRKSKY      ,
     8     FORNEQSKY   ,NFACNIT  ,LENC     , FANI     ,H3D_DATA    ,
     9     FANI(1,NFNCA+1),FANI(1,NFTCA+1) )
C
        ENDIF
        IF (IMON>0) CALL STOPTIME(10,1)
      ENDIF

C--- //------------------------------
C       PARITH/ON ASSEMBLY OF FORCES AFTER COMMUNICATION
C-------------------------------------
      CALL TRACE_IN(15,0,ZERO)

      IF (IMON>0) CALL STARTIME(9,1)
c     Parallel update of crack                                                  
      IF(ICRACK3D > 0 .AND. NSPMD > 1) CALL SPMD_MAX_XFE_I(NLEVSET)
      
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      DTNOD_NLOCAL = EP20
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,PARTFTSK,PARTLTSK,GREFTSK,GRELTSK,NODFTSK_2,NODLTSK_2,NODFT_NL,NODLT_NL)
      ITSK = OMP_GET_THREAD_NUM()

      NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
      NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
      PARTFTSK  = 1+ITSK*NPSAV*NPART/NTHREAD
      PARTLTSK  = (ITSK+1)*NPSAV*NPART/NTHREAD
      GREFTSK  = 1+ITSK*NPSAV*NGPE/NTHREAD
      GRELTSK  = (ITSK+1)*NPSAV*NGPE/NTHREAD
      NODFTSK_2   = NODFT_ASSPAR(ITSK+1)
      NODLTSK_2   = NODLT_ASSPAR(ITSK+1)

      IF(IPARIT==1)THEN

C------------------------
C Assembly Parith/ON spmd+multi-thread
C------------------------
        CALL ASSPAR4(
     1       A      ,AR      ,STIFN  ,STIFR       ,MS    ,
     2       FSKY   ,FSKY    ,ADSKY  ,ADSKY(IAD1B),FSKYM ,
     3       MSNF   ,ISKY    ,FSKYI  ,VISCN       ,FTHE  ,
     4       FTHESKY,FTHESKYI,NODFTSK,NODLTSK     ,ADSKYI,
     5       PARTSAV,PARTFTSK ,PARTLTSK ,ITSK     ,GREFTSK ,
     6       GRELTSK  ,GRESAV   ,AFLOW    ,FFSKY    ,MSF   ,
     7       ADSKY_PXFEM, INOD_PXFEM    ,ITAB     ,FSKYD ,
     8       DMSPH  ,CONDN   ,CONDNSKY  ,CONDNSKYI,
     9       MS_2D,ICNDS10  ,
     A       STIFND ,FORNEQS ,FORNEQSKY ,NFACNIT,NODFTSK_2,
     B       NODLTSK_2)   

!   ----------------------------------
!   Accumulation of acceleration for Nlocal option : parith/on
        IF (NLOC_DMG%IMOD>0) THEN
          NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
          NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
          CALL ASSPAR_SUB(NLOC_DMG%FNL(:,1) ,NLOC_DMG%FSKY,NLOC_DMG%ADDCNE,NODFT_NL     ,
     .                    NODLT_NL    ,NLOC_DMG%POSI  ,NLOC_DMG%NNOD, NLOC_DMG%L_NLOC)
          ! Non-local nodal stiffness
          IF (NODADT > 0) THEN
            CALL ASSPAR_SUB(NLOC_DMG%STIFNL(:,1),NLOC_DMG%STSKY,NLOC_DMG%ADDCNE,NODFT_NL,
     .                      NODLT_NL,NLOC_DMG%POSI,NLOC_DMG%NNOD,NLOC_DMG%L_NLOC)
            CALL NLOCAL_DTNODA(NODFT_NL,NODLT_NL,NLOC_DMG,DTNOD_NLOCAL,DT2T)
          ENDIF
        ENDIF
C------------------------
C       Assembly of Parith/ON forces on Phantom Xfem
C------------------------
        IF(ICRACK3D > 0)THEN
C         /---------------/
          CALL MY_BARRIER
C         /---------------/ 
          CALL ASSPAR_CRK(
     .         ADSKY_CRK,INOD_CRK ,CRKSKY  ,NODFTSK  ,NODLTSK  ,
     .         NODENR   ,NODLEVXF ,ITAB    )
        ENDIF
      ELSEIF(IPARIT==2)THEN
        
        IF(KDTINT/=0)THEN

!$OMP SINGLE
         CALL ANCMSG(MSGID=165,ANMODE=ANINFO)
!$OMP END SINGLE

         CALL ARRET(1)
        ENDIF
C
        CALL ASSPAR3(
     2       A          ,AR         ,ITSK       ,NODFTSK   ,
     3       NODLTSK    ,STIFN      ,STIFR      ,ITAB      ,FSKY       ,
     4   FSKY       ,ISKY       ,ADSKY      ,FSKYI     ,
     5       WA         ,PARTFTSK   ,PARTLTSK   ,PARTSAV   ,MS         ,
     6   FTHE       ,FTHESKY    ,FTHESKYI   ,GREFTSK     ,GRELTSK      ,
     7   GRESAV     )

      ELSEIF(IPARIT==3)THEN

C Assemblage Parith/ON 
        N1 =  1 + NUMNOD
        CALL ASSPAR5(
     1       NTHREAD   ,NUMNOD    ,NODFTSK  ,NODLTSK  ,IRODDL   ,
     2       NPART     ,PARTFTSK  ,PARTLTSK ,A        ,AR       ,
     3      PARTSAV    ,STIFN      ,STIFR    ,A(1,N1)  ,AR(1,N1) ,
     4      STIFN(N1)  ,STIFR(N1)  ,VISCN    ,VISCN(N1),GREFTSK    ,
     5       GRELTSK     ,GRESAV     ,NGPE     ,NTHPART)
C
      ENDIF

      IF(KDTINT/=0) CALL MODSTI(NODFTSK,NODLTSK,STIFN,VISCN,MS)

!$OMP END PARALLEL
C
      IF (IMON>0) CALL STOPTIME(9,1)
      CALL TRACE_OUT(15)

C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================

!       -------------------------------------------
!       check if a NaN appears in acc vectors (only available with /DEBUG/NAN option)
      IF( DEBUG(MACRO_DEBUG_NAN)/=0 )CALL CHECK_NAN_ACC(NCYCLE,A,AR)
!       write *.adb files for NON-LOCAL option
      IF (DEBUG(MACRO_DEBUG_ACC)==1.AND.(NLOC_DMG%IMOD>0)) THEN
        IF (ISPMD==0) THEN
            SIZ = NUMNODG
        ELSE
            SIZ = 0
        END IF
        IF ( NCYCLE>=DEBSTART .AND.
     .       MOD(NCYCLE-DEBSTART,RSTFREQ)==0 ) THEN
            CALL SPMD_COLLECT_NLOCAL(NLOC_DMG%FNL(:,1),NLOC_DMG%L_NLOC ,NLOC_DMG%NNOD,
     .                               NLOC_DMG%POSI    ,NLOC_DMG,SIZ,NODGLOB,ITAB     )
        ENDIF
      ENDIF

C----------------------------------
C       ITET2 of S10 Forces condensation; pass 1
C----------------------------------

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (NS10E > 0) THEN
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        GREFTSK   = 1+ITSK*NS10E/ NTHREAD
        GRELTSK   = (ITSK+1)*NS10E/NTHREAD
       CALL S10CNDF1(ICNDS10,WEIGHT ,IAD_CNDM1,FR_CNDM1,FR_NBCCCND1,
     1              ADDCNCND,PROCNCND,A   ,IADCND,FSKYCND,
     2              ITAGND  ,NODFTSK,NODLTSK,GREFTSK,GRELTSK,
     3              ITSK  ,ITAB   ,STIFN, STIFND)
!$OMP END PARALLEL
      END IF
c--------------------------------------
      IF(SOL2SPH_FLAG/=0)THEN
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        DO I=NODFTSK,NODLTSK
          IF(MS(I)/=ZERO)THEN
            IF(MS(I)-DMSPH(I) < EM03*MS(I))THEN
              MS(I)=ZERO
            ELSE
              MS(I)=MAX(ZERO,MS(I)-DMSPH(I))
            END IF
          END IF
          DMSPH(I)=ZERO
        ENDDO
!$OMP END PARALLEL
      ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

          IF (INT24USE == 1)THEN
            IF (IMON>0) CALL STARTIME(8,1)        
              CALL SPMD_EXCH_I24(IPARI    ,INTBUF_TAB       ,ITAB  ,
     *                           IAD_ELEM ,FR_ELEM ,INTLIST ,NBINTC,
     *                           IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,2,
     *                           INT24E2EUSE )
            IF (IMON>0) CALL STOPTIME(8,1)
          ENDIF

C
C Communication Interface type20 DAANC6
C
      IF(NBINT20>0.AND.NSPMD>1) THEN
        SIZE = 21
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_DA20(
     1   INTBUF_TAB,IPARI,IAD_ELEM,FR_ELEM,
     2   SIZE ,NBINT20,LENR ,INTLIST ,NBINTC )
      ENDIF
C
C Communication ICONTACT AIRBAG
C
      IF(KCONTACT/=0.AND.NSPMD>1) THEN
        SIZE = 1
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_ICONT(ICONTACT
     +    ,IAD_ELEM ,FR_ELEM,SIZE,LENR)
      ENDIF
C
C Communication IFOAM 
C
      IF(IALELAG > 0.AND.NSPMD>1) THEN 
        SIZE = 1
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_ICONT(IFOAM
     +    ,IAD_ELEM ,FR_ELEM,SIZE,LENR)
      ENDIF      
C
      IF(ISHSUB==1.AND.KSHSUB==-1) THEN
C     Shell computed at this cycle => go to internal force computations
        GOTO 20
      ELSEIF(ISHSUB==1.AND.KSHSUB==2) THEN
C     Internal forces assembled = go to timestep
        GOTO 23
      ENDIF
C
#ifdef DNC
      IF (IMADCPL>0)THEN
C   at every cycle except cycle 0, send the Nodes coordinates
         CALL DATA_RECV_MADCPL(X,A,V,MS,FANI,MADCLNOD,MADCLFRECV,H3D_DATA)
      ENDIF
#endif

CFP SKIP KINEMATIC FORCES
      IF(IALESUB==2 .AND. IFSUB==2) GOTO 23
      
C
C POROUS MEDIA (not parallel) 
C---------------------------------------------------------------------
      IF(NUMPOR>0) THEN
          CALL PORO(
     1       GEO    ,NODPOR ,MS,X   ,V     ,
     2       W      ,A      ,AR,SKEW,WEIGHT,
     3       NPORGEO)
      ENDIF
C---------------------------------------------------------------------
C DEBUG OUTPUT ACCELERATION
C---------------------------------------------------------------------
      IF (DEBUG(MACRO_DEBUG_ACC)==1) THEN
         IF (NCYCLE>=DEBSTART .AND.
     .                  MOD(NCYCLE-DEBSTART,RSTFREQ)==0) THEN

           IF(NSPMD > 1) THEN
             IF (ISPMD==0) THEN
               SIZ = NUMNODG
             ELSE
               SIZ = 0
             END IF
              CALL SPMD_COLLECT(A,ITAB,WEIGHT,NODGLOB,SIZ)
           ELSE
             CALL COLLECT(A,ITAB,WEIGHT,NODGLOB)
           END IF
        END IF
      END IF

C-----------------------------------------------
C       COMMUNICATION BETWEEN BOUNDARY ELEMENTS AFTER ASSEMBLY
C-----------------------------------------------
      IF (IFRWV > 0) THEN
        IF (NSPMD > 1) THEN
          SIZE = 1
          LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
C FR_WAVE boundary exchange
          CALL SPMD_EXCH_WAVE(FR_WAVE,IAD_ELEM ,FR_ELEM,SIZE,LENR)
        END IF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        DO I=NODFTSK,NODLTSK
          FR_WAVE(I)=ABS(FR_WAVE(I))
        ENDDO

!$OMP END PARALLEL

      ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C     Failure front wave
C-----------------------------------------------
       IF (FAILWAVE%WAVE_MOD > 0) THEN
         CALL UPDATE_FAILWAVE(FAILWAVE)
       ENDIF
C----------------------------------
C     TRAITEMENT SHOOTING NODES
C----------------------------------
      IF (IMON>0) CALL STARTIME(8,1)
      IF (N2D/=0.AND.IDEL7==2) THEN
          IF (IMON>0) CALL STARTIME(6,1)
          CALL CHKSTIFN(IPARI,MS,INTBUF_TAB)
          IF (IMON>0) CALL STOPTIME(6,1)
C
C IDEL7NG : global flag deleted segments/nodes int. type7, type2
C IDEL7NG = 0 => nothing
C IDEL7NG = 1 | 2 => deleted segments/nodes (pour au moins 1 interface)
C IDEL7NOK = 1 : need all interface such as IPARI(17)= 1 | 2
      ELSEIF(IDEL7NG>=1.OR.PDEL>0) THEN
C        IF (NCYCLE==97) IDEL7NOK = 1
C        IF (NCYCLE==98) IDEL7NOK = 1
        IF (NSPMD>1.AND.(IDEL7NG>=1.OR.PDEL>0)) THEN
C recuperation IDEL7NOK global
          IF (IMONM > 0 ) CALL STARTIME(76,1)
          CALL SPMD_ALLGLOB_ISUM9(IDEL7NOK,1)
          IF (IMONM > 0 ) CALL STOPTIME(76,1)
          IDEL7NOK = MIN(1,IDEL7NOK)
        ENDIF
        IDEL7NOK_SAV = IDEL7NOK
        IF (R2R_SIU==1.AND.IDEL7NG>=1) THEN
           CALL GET_SHMBUF_C(IDEL7NOK_R2R,2)
           IDEL7NOK = IDEL7NOK+IDEL7NOK_R2R
           IDEL7NOK = MIN(1,IDEL7NOK)
        ENDIF
C Attention WA utilise sur 2*NUMNOD (NUMNOD + NUMNOD SPECIFIQUE SPMD)
        IF ((IDEL7NG>=1.AND.IDEL7NOK==1).OR.(PDEL>0.AND.IDEL7NOK==1)) THEN
          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
          IF((INT24USE==1.OR.NINTER25/=0).AND.(IDEL7NG>=1.AND.IDEL7NOK==1))THEN
              INDSEGLO(2:NINTER+1)=0
              INDSEGLO(1)=1
          ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
          IF (IMONM > 0 ) CALL STARTIME(29,1)
          NINDEXP = 0

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)
C Init var parallel SMP
          CALL SMP_INIT(
     1     ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2     IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3     GREFTSK,GRELTSK)

C WA global : utilise de NUMNOD+1 a 3*NUMNOD
C WA local  : utilise de 1 a NUMNOD

        IF ((IDEL7NG>=1.AND.IDEL7NOK==1).OR.(PDEL>0.AND.IDEL7NOK==1)) THEN
          CALL TAGOFF3N(
     1       GEO        ,IXS       ,IXS(L1)   ,IXS(L1)   ,IXS(L3)  ,IXQ    ,
     2       IXC         ,IXT      ,IXP       ,IXR       ,IXTG     ,IXUR   ,
     3       WA(NUMNOD+1),NODFTSK  ,NODLTSK   ,IPARG     ,ELBUF    ,ITSK   ,
     4       WA(NWAFTSK) ,IXTG1    ,IAD_ELEM  ,FR_ELEM   ,ITAB     ,ITABM1 ,
     5       ADDCNEL     ,CNEL     ,KXSP      ,ELBUF_TAB ,TAGEL    ,IEXLNK , 
     6       IGRNOD      ,DD_R2R   ,DD_R2R_ELEM,SDD_R2R_ELEM,IDEL7NOK_SAV  ,
     7       IDEL7NOK_R2R,TAGTRIMC,TAGTRIMTG,S_ELEM_STATE,ELEM_STATE,
     8       SHOOT_STRUCT,SHOOT_STRUCT%GLOBAL_NB_ELEM_OFF)
          ! ---------------------
          ! check if a node is deactivated and deactivate all the corresponding secondary nodes
          CALL CHECK_NODAL_STATE( ITSK,WA(NUMNOD+1),NEWFRONT,INTBUF_TAB,SHOOT_STRUCT%SIZE_SEC_NODE,
     .           SHOOT_STRUCT%SHIFT_S_NODE,SHOOT_STRUCT%INTER_SEC_NODE,SHOOT_STRUCT%SEC_NODE_ID)
          ! ---------------------

          ! ---------------------
          ! check if a surface/edge must be deactivated and save the surface/edge id 
          IF(ITSK==0) THEN 
                CALL FIND_SURFACE_INTER( ITSK,ITAB,SHOOT_STRUCT,IXS,IXS(L1),IXC,IXTG )
                CALL FIND_EDGE_INTER( ITSK,ITAB,SHOOT_STRUCT,IXS,IXS(L1),
     1                                IXC,IXTG,IXQ,IXT,IXP,
     2                                IXR,GEO )
          ENDIF
          CALL MY_BARRIER( )
          ! ---------------------

          ! ---------------------
          ! exchange of surfaces (ie. 4 nodes) to deactivate and deactivation
          ! ONLY FOR LOCAL SURFACE / REMOTE ELEMENT
          IF(NSPMD>1) THEN 
            IF(ITSK==0) CALL SPMD_EXCH_DELETED_SURF_EDGE( IAD_ELEM,ITABM1,SHOOT_STRUCT,INTBUF_TAB,NEWFRONT,
     .                           IPARI,GEO,
     .                           IXS,IXC,IXT,IXP,IXR,IXTG,
     .                           ADDCNEL,CNEL,WA(NWAFTSK),TAGEL )
            CALL MY_BARRIER()
          ENDIF
          ! ---------------------

          ! ---------------------
          ! loop over the surface id and deactivate the surface
          ! ONLY FOR LOCAL SURFACE / LOCAL ELEMENT
          CALL CHECK_SURFACE_STATE( ITSK,SHOOT_STRUCT%SAVE_SURFACE_NB,SHOOT_STRUCT%SAVE_SURFACE,
     .                              SHOOT_STRUCT%SHIFT_INTERFACE,INTBUF_TAB )

          ! loop over the edge id and deactivate the edge
          ! ONLY FOR LOCAL EDGE / LOCAL ELEMENT
          CALL CHECK_EDGE_STATE( ITSK,SHOOT_STRUCT%SAVE_M_EDGE_NB,SHOOT_STRUCT%SAVE_S_EDGE_NB,
     .                           SHOOT_STRUCT%SAVE_M_EDGE,SHOOT_STRUCT%SAVE_S_EDGE,
     .                           SHOOT_STRUCT%SHIFT_INTERFACE,INTBUF_TAB,NEWFRONT,IPARI,GEO,
     .                           IXS,IXC,IXT,IXP,IXR,IXTG,
     .                           ADDCNEL,ITABM1,CNEL,WA(NWAFTSK),TAGEL,SHOOT_STRUCT )
          ! ---------------------
        ENDIF

        IF (IDEL7NG>=1.AND.IDEL7NOK==1) THEN
          CALL CHKSTFN3N(
     1       IPARI   ,GEO         ,IXS      ,IXQ         ,IXC     ,IXT       ,
     2       IXP     ,IXR         ,IXTG     ,WA(NUMNOD+1),IPARG   ,ITSK      ,
     3       NEWFRONT,WA(NWAFTSK) ,MS       ,IN          ,ANIN(NDMA+1),ITAB  ,
     4       ITABM1  ,ADDCNEL     , CNEL    ,INDIDEL     ,NINDEX1 ,NINDEX2   ,
     5       NINDEX3 ,NINDEX4     ,TAGEL    ,INT24USE    ,IBUFSEGLO ,INDSEGLO,
     6       IBUFIDEL ,INTBUF_TAB ,IAD_ELEM)

        ENDIF
   
        IF (PDEL>0.AND.IDEL7NOK==1) THEN
          CALL CHKLOAD(
     1       IBCL    ,IXS     ,IXQ         ,IXC    ,IXT        ,IXP     ,
     2       IXR     ,IXTG    ,WA(NUMNOD+1),ITSK   ,WA(NWAFTSK),ITAB    ,
     3       ITABM1  ,ADDCNEL ,CNEL        ,TAGEL  ,IPARG      ,GEO     ,
     4       IBUFPDEL,NINDEXPDEL,NINDEXP   ,NPRESLOAD,LOADP_TAGDEL      ,
     5       ILOADP  ,LLOADP  ,IAD_ELEM)

        ENDIF

!$OMP END PARALLEL
          IF (IMONM > 0 ) CALL STOPTIME(29,1)
          CALL DEALLOC_SHOOT_INTER( SHOOT_STRUCT )
        ENDIF
      ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C elt deletion => need to rebuild ams mass matrix ...
      IF(IDTMINS>=1.OR.IDTMINS_INT/=0)THEN
        IF(MCHECK==0)THEN
          ISMSNOK=0
          IF(IDEL7NG==0)THEN
            IF (NSPMD>1) THEN
C Get IDEL7NOK global
              IF (IMONM > 0 ) CALL STARTIME(76,1)
              CALL SPMD_ALLGLOB_ISUM9(IDEL7NOK,1)
              IF (IMONM > 0 ) CALL STOPTIME(76,1)
              IDEL7NOK = MIN(1,IDEL7NOK)
            ENDIF
          ENDIF
          ISMSNOK=IDEL7NOK
        ELSE
          ISMSNOK=1
        END IF
      END IF

#ifdef DNC
      IF(IMADCPL /=0)THEN
C  ---------------------------------------------------------
C   Radioss Madymo Coupling 
C  ---------------------------------------------------------
C   IF IDEL7NOK is > Deleted elements were found
C   MAD_FAIL_ELEMENTS is upgraded
C
C   If one domain has new deleted elements, MADYMO_DEL is set to 1
C   MADYMO_DEL is globalized with SPMD_ALLGLOB_ISUM9 
C   if Positive, all domains send the info during Time Step exchange
C   
C   --------------------------------------------------------
C   Don't communicate IDEL7NOK if already done. (IDEL7NG >0)
C  ---------------------------------------------------------

        MADYMO_DEL_GLOBAL=0
        MADYMO_DEL=0

        IF (IDEL7NOK > 0 ) THEN
          CALL MAD_ELFAIL( ELBUF_TAB,IPARG,
     *                     MADSOL,MADSH3,MADSH4,
     *                     MAD_TAG_SOL, MAD_TAG_SH,MAD_TAG_TG,
     *                     MADYMO_DEL,
     *                     MAD_FAIL_ELEMENTS)

        ENDIF
        
        MADYMO_DEL_GLOBAL = MADYMO_DEL

        CALL SPMD_ALLGLOB_ISUM9(MADYMO_DEL_GLOBAL,1)
      ENDIF
#endif
C  ---------------------------------------------------------
      IDEL7NOK=0
      IF (IMON>0) CALL STOPTIME(8,1)
C
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------------
C       FORCES EXTERNES de SECTIONS
C----------------------------------
      IF(ISECUT/=0)CALL SECT_FIO (
     1        NSTRF ,V,VR,FSAV(1,1+NINTER+NRWALL+NRBODY),
     2        A     ,AR     ,SECBUF,MS,IN,
     3        WEIGHT,IAD_CUT,FR_CUT)
C-----------------------------------------------------
C     SPOTWELD ELEMENT CLUSTERS
C-----------------------------------------------------
      IF (NCLUSTER > 0) THEN
        CALL CLUSTERF(CLUSTER ,ELBUF_TAB,X  ,A  ,AR  ,
     .                SKEW    ,IXS      ,IPARG ,FCLUSTER,MCLUSTER,
     .                H3D_DATA,GEO      )   
      ENDIF

C-----------------------------------------------------
C         KINEMATIC CONDITIONS FOR SEATBELTS
C-----------------------------------------------------

       IF (NSLIPRING + NRETRACTOR + N_ANCHOR_REMOTE > 0) THEN
         CALL KINE_SEATBELT_FORCE(A,STIFN,FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE)
       ENDIF

C-----------------------------------------------------
C         INTERFACES 18 KINE
C-----------------------------------------------------
        IF(NINTER /= 0.and.IALE+IEULER /= 0.and.
     .     INT18KINE == -1)THEN
     
C========================================================================================
C                    PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()

C        /---------------/
          CALL MY_BARRIER
C        /---------------/
          CALL I18MAIN_KINE_1(IPARI,INTBUF_TAB,X       ,V       ,
     2             A          ,ISKEW     ,SKEW     ,ICODT   ,WA      ,
     3             MS         ,ITAB      ,ITSK+1   ,KINET   ,STIFN   ,
     4             MTF        ,CAND_SAV  ,INT18ADD ,IAD_ELEM,FR_ELEM ,
     5             TAGPENE    ,H3D_DATA  ,MULTI_FVM,ALE_CONNECTIVITY%NE_CONNECT,XCELL)
!$OMP END PARALLEL
        ENDIF
C------------------------
C         INTERFACES TIED
C--- //0 ----------------
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
        IF(NINTER/=0)THEN
          IF (IMON>0) THEN
              CALL STARTIME(6,1)
              CALL STARTIME(2,1)
          ENDIF
          IF (IMONM > 0)CALL STARTIME(28,1)
!0.
          DO K=0,NHIN2
            CALL INTTI1(
     1         IPARI  ,X      ,V        ,A      ,
     2         VR     ,AR     ,WA       ,MS     ,IN     ,WEIGHT      ,
     3         STIFN  ,STIFR  ,K        ,ITAB   ,FR_I2M ,IAD_I2M     ,
     4         ADDCNI2,PROCNI2,IADI2    ,I2MSCH ,DMAS   ,ANIN(NDMA+1),
     5         SKEW   ,I2SIZE ,FR_NBCCI2,ANIN(NDIN+1)   ,IGEO,BUFGEO ,
     6         FSAV   ,NPC    ,TF       ,FANI(1,NFT2+1) ,IAD_ELEM,FR_ELEM,
     7         NATIV_SMS,DMINT2,ANIN(NDAMA2+1),NB_FRI2M ,FR_LOCI2M,
     8         DT2T  ,NELTST ,ITYPTST   ,INTBUF_TAB     ,TEMP     ,MCP   ,
     9         FTHE  ,CONDN  ,
     A         H3D_DATA,T2FAC_SMS,FANI(1,NFNCA2+1),FANI(1,NFTCA2+1)) 
          ENDDO
          IF (IMONM > 0) CALL STOPTIME(28,1)
          IF (IMON>0) THEN
              CALL STOPTIME(2,1)
              CALL STOPTIME(6,1)
          ENDIF
C
          IF((IDTMINS/=0.OR.IDTMINS_INT/=0).AND.NCYCLE==0)ISMSCH=1

        ENDIF
C
C----------------------------------
C       ITET2 of S10 Forces condensation; pass 2
C----------------------------------

C========================================================================================
C                    PARALLEL SECTION (SMP)
C========================================================================================

      IF (NS10E > 0) THEN
!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        GREFTSK   = 1+ITSK*NS10E/ NTHREAD
        GRELTSK   = (ITSK+1)*NS10E/NTHREAD
       CALL S10CNDF2(ICNDS10,WEIGHT ,IAD_CNDM,FR_CNDM,FR_NBCCCND,
     1              ADDCNCND,PROCNCND,A   ,IADCND,FSKYCND,
     2              ITAGND  ,NODFTSK,NODLTSK,GREFTSK,GRELTSK,
     3              ITSK  ,ITAB   ,STIFN  , STIFND)
!$OMP END PARALLEL

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

       IF (NCYCLE==0.OR.MCHECK/=0) 
     1    CALL CND_DMASI2(ICNDS10,NKEND,IMAP2ND,MASI2ND0,MS  ,WEIGHT)
      END IF
C
      IF(INTPLYXFEM > 0) THEN
            CALL I24PXFEM(
     1         IPARI  ,INTBUF_TAB ,WAGAP,IAD_ELEM,FR_ELEM)
        ENDIF      
C  
C----------------------------------------------------------
C         RBE2 - FORCES and MOMENTS (Torque)
C----------------------------------------------------------
      IF (NRBE2>0.OR.R2SIZE>0) THEN
          CALL MY_BARRIER
        IF (IMON>0) CALL STARTIME(4,ITASK+1)
        IF(ITASK==0)THEN
         CALL RBE2T1(IRBE2 ,LRBE2 ,X      ,A      ,AR     ,
     1               MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2               FR_RBE2M,NMRBE2,STIFN ,STIFR ,R2SIZE )
        END IF
        IF (IMON>0) CALL STOPTIME(4,ITASK+1)
      ENDIF
C----------------------------------------------------------
C         RBE3 - FORCES ET MOMENTS
C----------------------------------------------------------
      IF (NRBE3>0) THEN
        CALL MY_BARRIER
        IF (IMON>0) CALL STARTIME(4,1)
        IF (IMONM > 0) CALL STARTIME(45,1)
         CALL RBE3T1(IRBE3 ,LRBE3 ,X     ,A      ,AR     ,
     1               MS    ,IN    ,FRBE3 ,SKEW   ,WEIGHT ,
     2               STIFN ,STIFR ,IAD_RBE3M,FR_RBE3M,R3SIZE,
     3               FR_RBE3MP,DMAS ,ANIN(NDMA+1) ,DINER  ,
     4               ANIN(NDIN+1),RRBE3 ,RRBE3_PON,H3D_DATA)
        IF (IMONM > 0) CALL STOPTIME(45,1)
        IF (IMON>0) CALL STOPTIME(4,1)
      ENDIF

C------------------------
C     test of mass and inertia on main nodes of interf. type 2
C------------------------
      IF(TT==ZERO.AND.IALE+IEULER+ITHERM==0)THEN

        NEGMAS=0
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        CALL CHKMSIN(NODFTSK,NODLTSK,ITAB,MS,IN,NEGMAS)
!$OMP END PARALLEL
C Implicit barrier on NEGMAS
        IF(NEGMAS/=0) CALL ARRET(2)
      ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C
C Assemblage TYPE21
C
      IF(NINTSTAMP/=0)THEN
        CALL INTSTAMP_ASS(INTSTAMP,MS ,IN ,A ,AR ,
     .                    STIFN   ,STIFR  ,WEIGHT)
      END IF
C-----------------------------------------------------
C         RIGID BODY: SUM forces, stiff.
C-----------------------------------------------------

      IF(NRBYKIN>0)THEN
          IF (IMON>0) CALL STARTIME(4,1)
          IF (IMONM > 0) CALL STARTIME(40,1)

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

          CALL RBYSENS(
     1     IPARG,IPARI       ,MS    ,IN   ,
     2     IXS  ,IXQ  ,IXC   ,IXT   ,IXP  ,
     3     IXR  ,SKEW ,ITAB  ,ITABM1,ISKWN,
     4     NPBY ,WA   ,LPBY  ,FSKY  ,
     5     RBY  ,X    ,V     ,VR    ,IXTG ,
     6     IGRV ,LGRAV,SENSOR_TAB,A     ,AR   ,
     7     FSAV  ,STIFN ,STIFR,FANI(1,1+NFOA),WEIGHT,
     8     DMAS  ,DINER ,BUFSF,FR_RBY2,PARTSAV ,
     9     IPART ,ELBUF_TAB,ICFIELD,LCFIELD,TAGSLV_RBY)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
!$OMP PARALLEL

          CALL RBYFOR(
     1      RBY      ,A      ,AR     ,X    ,VR            ,
     2      FSAV     ,IN     ,STIFN  ,STIFR,FANI(1,1+NFOA),
     3      LPBY     ,NPBY   ,WEIGHT ,MS   ,V             ,
     4      IGRSURF  ,BUFSF  ,ICODR  ,ISKEW,SKEW          ,
     5      KINDRBY  ,IAD_RBY,FR_RBY6,RBY6 ,IRBKIN_L      ,
     6      NRBYKIN_L,NATIV_SMS,SFSAV6,FBSAV6,STABSEN     ,
     7      TABSENSOR,NODREAC,FTHREAC,CPTREAC)

!$OMP END PARALLEL
          IF (IMON>0) CALL STOPTIME(4,1)
          IF (IMONM > 0) CALL STOPTIME(40,1)
C-----------------------------------------------------
      ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C-----------------------------------------------------
C         FORCES  FLEXIBLE BODIES
C-----------------------------------------------------
      IF (NFXBODY>0)  THEN
         CALL FXBYFOR(FXBIPM, FXBRPM,  FXBNOD,  FXBMOD,  FXBGLM,
     .                FXBCPM, FXBCPS,  FXBLM ,  FXBFLS,  FXBDLS,
     .                FXBDEP, FXBVIT,  FXBACC,  A,       AR,
     .                X,      FXBMVN,  FXBMCD,  FXBSE,   FXBSV,
     .                FXBELM, FXBSIG,  ELBUF,   PARTSAV, ELBUF_TAB,
     .                FSAV,   FXBFP,   FXBEFW,  FXBFC,   D,
     .                DT2T,   ITYPTST, NELTST,  FXBGRVI, FXBGRVR,
     .                IGRV,   NPC,     TF    ,  FXBGRP,  FXBGRW ,
     .                IPARG , SENSOR_TAB,IAD_ELEM, FR_ELEM,
     .                AGRV  )
      END IF

C----------------------------------------------------------
C         ADAPTIVE MESHING : FORCES AND STIFNESS FROM SECND TO MAIN
C-----------------------------------------------------
      IF(NADMESH/=0)THEN
        IF (IMON>0) CALL STARTIME(37,1)
          CALL ADMFOR0(IXC, IPART(K3), IXTG, IPART(K8), IPART,
     1                 A  , STIFN    ,    AR, STIFR   ,X    ,
     2                 SH4TREE,SH3TREE,STCND,     FTHE,CONDN)
        IF (IMON>0) CALL STOPTIME(37,1)
      END IF
C----------------------------------------------------------
C         RIGID MATERIAL   
C----------------------------------------------------------
      IF (IRIGID_MAT > 0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
        CALL RMATFORP(
     1  A        ,AR        ,X           ,VR          ,IN      ,
     2  STIFN    ,STIFR     ,IRBYM       ,LNRBYM      ,RBYM    ,
     3  ICODRBYM ,WEIGHT ,MS     ,V      ,FR_RBYM ,
     4  IAD_RBYM ,ARBYM  ,VRBYM  ,ARRBYM ,VRRBYM  ,
     5  KINDRBYM ,RBYM6  )

!$OMP END PARALLEL

      ENDIF

C----------------------------------------------------------
C     Rigid wall - Force node (moving RWALL)
C----------------------------------------------------------
      IF(NRWALL>0)CALL RGWALF(A ,RWBUF ,NPRW ,MS )

C-----------------------------------------------
C     SELECTIVE MASS SCALING
C-----------------------------------------------
      IF( IDTMINS == 1 .AND.
     .   (ISMSCH/=0.OR.NCYCLE==0.OR.ISMSNOK/=0.OR.IADMESH/=0))THEN
C
C       Obsolete
      ELSEIF(IDTMINS == 2.OR.IDTMINS_INT/=0)THEN
C
        NSGDONE=1
        NRBDONE=1

        IF (IMON>0) CALL STARTIME(39,1)
        IF (IMON>0) CALL STARTIME(75,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
        L1 = 1+NIXS*NUMELS + NSVOIS*NIXS

C       Sorting of ISKYI_SMS and additional connections for TYPE2 + contact
        CALL SPMD_SORT_SMS(ISKYI_SMS,MSKYI_SMS,FR_SMS)
C
        IF(NSPMD > 1)THEN
          CALL SPMD_NLIST_SMS(FR_SMS,FR_RMS)
        END IF

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C
        CALL SMS_BUILD_MAT_2(
     1             ITSK     ,NODFTSK ,NODLTSK   ,
     2             IXC      ,IPARG   ,IXS       ,IXT       ,IXP       ,
     3             IXR      ,IXTG    ,TAGNOD_SMS,MS        ,MS0       ,
     4             INDX1_SMS,INDX2_SMS,JAD_SMS  ,JDI_SMS   ,LT_SMS    ,
     .             KAD_SMS ,KDI_SMS  ,LTK_SMS   ,PK_SMS    ,NODII_SMS ,
     5             JADC_SMS ,JADS_SMS ,JADT_SMS ,JADP_SMS ,JADR_SMS  ,
     6             JADTG_SMS,DIAG_SMS ,TAGPRT_SMS,TAGREL_SMS,
     7     IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),IPART(I15E),
     8     IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),IAD_ELEM   ,
     9             FR_ELEM  ,NPBY    ,LPBY  ,TAGSLV_RBY_SMS ,LAD_SMS  ,
     A             JSM_SMS ,DMELTG    ,DMELC      ,MSKYI_SMS,
     B             ISKYI_SMS,JADI_SMS,JDII_SMS  ,LTI_SMS    ,NODXI_SMS,
     C             DMELS    ,DMELTR  ,DMELP     ,DMELRT     ,IGEO     ,
     D             FR_SMS   ,FR_RMS  ,ELBUF     ,IPARI      ,INTBUF_TAB,
     E             KINET    ,TAGSLV_I21_SMS,JADI21_SMS,INTSTAMP,
     F             IXS(L1),JADS10_SMS,ILINK     ,LLINK      ,NNLINK   ,
     G             LNLINK   ,TAG_LNK_SMS,LJOINT ,IADCJ      ,FR_CJ    ,
     H             ITAB  ,WEIGHT  ,DMINT2    ,ELBUF_TAB,TAGMSR_RBY_SMS,
     I             NPRW  ,LPRW    ,FR_WALL   ,NRWL_SMS      ,RBY      ,
     J             X     ,A       ,AR        ,IN            ,V        ,
     K             VR    ,IRBE2   ,LRBE2     ,IRBE3         ,LRBE3    ,
     L             IAD_RBE3M ,FR_RBE3M ,NATIV_SMS,T2MAIN_SMS,T2FAC_SMS)
c
!$OMP END PARALLEL

        PTR_SMS => NODXI_SMS

        ISMSCH=0
        IF (IMON>0) CALL STOPTIME(39,1)
        IF (IMON>0) CALL STOPTIME(75,1)

      ENDIF
C----------------------------------------------------------
C     SCALE TIME STEP
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
C--- COUPLAGE RADIOSS 2 RADIOSS
        IF (IRAD2R /= 0) THEN      
          IF (NSPMD>1) CALL SPMD_BARRIER()
            CALL R2R_EXCHANGE(
     1        IEXLNK   ,IGRNOD  ,D       ,V       ,VR     ,
     2        A        ,AR      ,MS      ,IN     ,STIFN   ,STIFR  ,
     3        R2R_ON   ,DD_R2R  ,WEIGHT ,IAD_ELEM,FR_ELEM ,RBY    ,
     4        XDP      ,X       ,DD_R2R_ELEM, SDD_R2R_ELEM,OFF_SPH_R2R,
     5        NUMSPH_GLO_R2R,NLOC_DMG)
C
          IF (FLG_SPHINOUT_R2R/=0) THEN
            DO I=1,NUMNOD
              IF (OFF_SPH_R2R(I)==2) THEN
                OFF_SPH_R2R(I) = 1
              ELSE
                OFF_SPH_R2R(I) = 0
              ENDIF
            END DO
          ENDIF
C
        ENDIF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------
  23  CONTINUE
C----------------------------
C SHELLS SUBCYCLING
C----------------------------
C 23  CONTINUE
      IF(ISHSUB==1 .AND.KSHSUB==2)THEN
C       internal shell forces just computed 
C       Nodal time step for shells (SHNOD)
        IF(NODSUBDT/=0)THEN
C         temporary save of DTFAC1(11), DTMIN1(11), IDTMIN(11)
            DTFNOD    =DTFAC1(11)
            DTMNOD    =DTMIN1(11)
            IDTNOD    =IDTMIN(11)
C
            DTFAC1(11)=DTFSUB
            DTMIN1(11)=DTMSUB
            IF(NODSUBDT==2)THEN
              IDTMIN(11)=8
            END IF
C         /---------------/
        ELSE IF(NODADT==0)THEN
          GOTO 24
        END IF
      END IF

C----------------------------------------------------------
C       INTER/TYPE21 TIME STEP
C----------------------------------------------------------
      IF(NINTSTAMP/=0)THEN
        CALL INTSTAMP_DT(INTSTAMP,IPARI,NELTST,ITYPTST,DT2T,
     .                   PTR_SMS ,DIAG_SMS,MS ,V      ,STIFN,
     .                   STIFR   )
      END IF

      IMSCH=0

      IF (FLG_DTNODAMP==1) THEN
C---------------NODAL TIME STEP FOR DAMPING-----------------
        IF (IDAMP_RDOF==NDAMP)
     +  CALL DTNODAMP(ITAB  ,MS  ,IN  ,STIFN  ,STIFR  ,DT2T ,
     1                WEIGHT        ,IGRNOD   ,DAMPR ,ISTOP ,
     2              IDAMP_RDOF_TAB,ICONTACT,IXC,X)
        IF (NDAMP>0 .OR. ISTAT==3)
     +  CALL DTNODARAYL(MS       ,IN    ,STIFN ,STIFR ,DT2T     ,
     1                  IGRNOD   ,DAMPR )
      ENDIF
C -------------------------- 

      IF (I_EXCH_FLG_RAZ==0) THEN 
C--- Flag for reset of stifn and stifr - must be set only at first cycle - ncycle=0 not enough because of chkpt
        FLG_KJ2_RAZ = FLG_KJ2
        IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(FLG_KJ2_RAZ,1)
        I_EXCH_FLG_RAZ = 1
      ENDIF   
C 
      IF (FLG_KJ2==1) THEN
C
        IF (NCYCLE==0) THEN      
          ALLOCATE(STK_SN(NUMNOD),STK_SR(NUMNOD))
          STK_SN(1:NUMNOD)=STIFN(1:NUMNOD)
          STK_SR(1:NUMNOD)=STIFR(1:NUMNOD)
        ENDIF
C
        CALL JOINT_ELEM_TIMESTEP(MS,IN,STIFN,STIFR,IXR,IPART,
     1                   IPART(K6),IGEO,GEO,NPBY,IPARG,ELBUF_TAB,
     2                   DT2T,NELTST,ITYPTST,NRBODY,ITAB)
      ENDIF

C----------------------------------------------------------
C  FIND TARGET_DT FOR DEFINED % OF ADDED MASS
C----------------------------------------------------------
      IF ((NCYCLE==0).AND.(IDT_PERCENT_ADDMASS > 0).AND.(IDTMIN(11)==3.OR.IDTMIN(11)==8)) THEN
        CALL FIND_DT_FOR_TARGETED_ADDED_MASS(MS,STIFN,DTFAC1(11),IDTGR(11),TARGET_DT,
     .                                       PERCENT_ADDMASS,PERCENT_ADDMASS_OLD,MASS0_START,WEIGHT_MD,IGRNOD,
     .                                       ICNDS10)
        DTMIN1(11) = MAX(DTMIN1(11),TARGET_DT)
      ELSEIF ((IDT_PERCENT_ADDMASS == 2).AND.(IDTMIN(11) == 8)) THEN
C--     For /DT/NODA/STOP + % added mass - IDTMIN switch back to 1 after first cycle
        IF (IDT_PERCENT_ADDMASS == 2) THEN 
          IDTMIN(11) = 1
          DTMIN1(11) = DT_STOP_PERCENT_ADDMASS
        ENDIF
      ENDIF

C--------------------------------------------------
C  UPDATE MINIMUM NODAL DT IF DT IS GIVEN BY FVMBAG
C--------------------------------------------------
      IF(NSPMD > 1 .AND. NVOLU > 0 .AND. NFVBAG0 > 0) THEN      
      !
      ! End asynchronous communication
      ! This is an implicit Barrier
      ! 
        CALL MPI_MIN_REAL_END(DT2R,MIN_TAB,4,MPI_BUF) 
      ! This call changes DT2R and MIN_TAB the to minimum value of 
      ! DT2R and the corresponding MIN_TAB.
     
      ! NELTS = MIN_TAB(1) 
      ! ITYPTS= MIN_TAB(2)
      ! ISPMD = MIN_TAB(4) ! ID of the proc. that has the mini. value of DT2R

      ! DT2  = DT2R ! Min over the proc. of the value of DT2  

        DTMIN1_SAVE = DTMIN1(11)
        IF(MIN_TAB(2) == 52) DTMIN1(11) = MIN(DTMIN1_SAVE,DT2R,1.1*DT2OLD)

       ELSE

        DTMIN1_SAVE = DTMIN1(11)
        IF(ITYPTS == 52) DTMIN1(11) = MIN(DTMIN1_SAVE,DT2,1.1*DT2OLD)

       ENDIF

C   

C------------------------------------
C  THERMAL TIME STEP COMPUTATION
C-----------------------------
      IF(IDT_THERM == 1)THEN
         DT2 = DT_THERM
         DT2T = DT2
      ENDIF
C

      IF((ANIM_N(18) /= 0 .OR. H3D_DATA%N_SCAL_STIFR /= 0) .AND. IRODDL /= 0)
     .   STIFR_TMP(1:NUMNOD)=STIFR(1:NUMNOD)
      IF(ANIM_N(19) /= 0 .OR. H3D_DATA%N_SCAL_STIFN /= 0) 
     .   STIFN_TMP(1:NUMNOD)=STIFN(1:NUMNOD)
     
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,DT2TT,NELTSTT,ITYPTSTT,NODFTSK,NODLTSK)
!$OMP+PRIVATE(DMAST,DINERT)

      DT2TT = DT2T
      NELTSTT = NELTST
      ITYPTSTT= ITYPTST
      DMAST = ZERO
      DINERT = ZERO
      ITSK = OMP_GET_THREAD_NUM()
      NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
      NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C----------------------------------------------------------
C       NODAL TIME STEP
C----------------------------------------------------------
      IF(ISTATCND/=0)THEN
C       additional storage due to reset of stifn stifr
        STCND (NODFTSK:NODLTSK)=STIFN(NODFTSK:NODLTSK)
        STRCND(NODFTSK:NODLTSK)=STIFR(NODFTSK:NODLTSK)
      ENDIF

      IF(IDTMINS==0)THEN
C     IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
        CALL DTNODA(
     1              NODFTSK,NODLTSK ,NELTSTT,ITYPTSTT   ,ITAB   ,
     2              MS   ,IN    ,STIFN ,STIFR     ,DT2TT   ,
     3              DMAST,DINERT,ANIN,ANIN(NDMA+1),IMSCH  ,
     4              WEIGHT,A    ,AR    ,IGRNOD    ,
     5              ANIN(NDIN+1),RBYM  ,ARBYM ,ARRBYM,WEIGHT_MD,
     6              MCP         ,MCP_OFF,CONDN ,ALE_CONNECTIVITY%NALE  ,H3D_DATA  )
      ELSEIF(IDTMINS/=0)THEN
        CALL DTNODAMS(
     1              NODFTSK,NODLTSK ,NELTSTT,ITYPTSTT   ,ITAB   ,
     2              MS   ,IN    ,STIFN ,STIFR     ,DT2TT   ,
     3              DMAST,DINERT,ANIN,ANIN(NDMA+1),IMSCH  ,
     4              WEIGHT,A    ,AR    ,IGRNOD    ,
     5              ANIN(NDIN+1),RBYM  ,ARBYM ,ARRBYM ,ISMSCH ,
     6              NATIV_SMS   ,DIAG_SMS ,NPBY,TAGMSR_RBY_SMS,
     7              H3D_DATA    )
      END IF

#include "lockon.inc"
      DMAS  = DMAS  + DMAST
      DINER = DINER + DINERT
      IF(IDT_THERM == 1)THEN    
        IF(DT2TT<=DT2T)THEN
C---------Check remaining time for end simulation and correct time step----
C------------Need to stop computation at Tstop for /DT/THERM----------

          TREST=MAX(TSTOP-TT,ZERO)
  
          DTREST = TREST*(ONE+EM10)

          IF(DTREST<DT2T) THEN
             IF (DTANIM>ZERO) ILASTANIM = 1 
             IF (H3D_DATA%DTH3D>ZERO) ILASTH3D = 1  
          ENDIF     
          DT2T = MIN(DT2TT,DTREST) 
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        ENDIF
      ELSE
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        END IF
      ENDIF
#include "lockoff.inc"

!$OMP END PARALLEL

C ----RAZ of STIFN AND STIFR for kjoints with element time step----------------------  
      IF ((FLG_KJ2_RAZ==1).AND.(I7KGLO==0).AND.(IDTMINS==0).AND.(NODADT==0)) THEN
        STIFN(1:NUMNOD) = EM20
        IF (IRODDL > 0) STIFR(1:NUMNOD) = EM20
      ENDIF
C
      DTMIN1(11) = DTMIN1_SAVE

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================  
C
       IF(IDT_THERM == 1)THEN
         IF(DT2T<DT2)THEN
          NELTS  = NELTST
          ITYPTS = ITYPTST
          DT2    = DT2T
          ENDIF
       ELSE
         IF(DT2T<DT2)THEN
            NELTS  = NELTST
            ITYPTS = ITYPTST
            DT2    = DT2T
          ENDIF
       ENDIF

C----------------------------
C    IMPLICIT SYNCHRONISATION On DT2 FBIG
C----------------------------
C----------------------------
C           Sub-cycling for shell
C----------------------------
 24   CONTINUE
      IF(ISHSUB==1 .AND.KSHSUB==2)THEN
        IF(NODSUBDT/=0)THEN
            DTFAC1(11)=DTFNOD
            DTMIN1(11)=DTMNOD
            IDTMIN(11)=IDTNOD
          END IF

        IF(DT2T<DT2S)THEN
          NELTS  = NELTST
          ITYPTS = ITYPTST
          DT2S   = DT2T
        ENDIF

C----------------------------
C       write L01 (subcycling)
C----------------------------
        IF(NSPMD>1) THEN
          IWIOUT = 0
          IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
        CALL SPMD_GLOB_MIN5(DT2S ,ITYPTS,NELTS ,ICODT ,IMSCH,
     .                     TSTOP,IWIOUT,MSTOP, ISMSCH,
     .                     INT24USE,NBINTC,INTLIST,IPARI,INTBUF_TAB)
        IF(IEXICODT>0) THEN
            SIZE = 1
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            CALL SPMD_EXCH_ICODT(ICODT,IAD_ELEM,FR_ELEM,SIZE,LENR)
            IEXICODT = 0
          END IF
        ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)

C Init var parallel SMP
        CALL SMP_INIT(
     1    ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2    IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST

        CALL SHSUB3(
     1    LSHSUB ,V  ,DSAVE  ,
     2    ASAVE   ,A     ,D      ,NELTSTT,
     3    ITYPTSTT,ITSK  ,NODFTSK,NODLTSK,DT2SAVE,
     4    DT2TT   ,NELTSA,ITYPTSA,FSKY   ,
     5    FSKY   ,VR   ,DR    ,AR     ,DRSAVE ,
     6    ARSAVE ,STIFN,STSAVE,STIFR  ,STRSAVE,
     7    IPART  ,PARTSAV,GEO ,WEIGHT )
C
#include "lockon.inc"
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        END IF
#include "lockoff.inc"

!$OMP END PARALLEL

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

        KSHSUB=0

C-------------------------------------------------------
C       MAJ SPMD ecriture L01
C-------------------------------------------------------
        IF(NSPMD>1.AND.IWIOUT>0) THEN
         CALL SPMD_WIOUT(IOUT,IWIOUT)
         IWIOUT = 0
        ENDIF

C      Internal shell force computed = go back to other elements
        GOTO 211

      ENDIF

C-----------------------------
      IF(IALESUB==2 .AND.IFSUB==2)THEN
        IF(NSPMD>1) THEN
         IWIOUT = 0
         IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_MIN5(DT2  ,ITYPTS,NELTS ,ICODT ,IMSCH,
     .                       TSTOP,IWIOUT,MSTOP, ISMSCH,
     .                       INT24USE,NBINTC,INTLIST,IPARI,INTBUF_TAB)
         IF(IEXICODT>0) THEN
           SIZE = 1
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_ICODT(ICODT,IAD_ELEM,FR_ELEM,SIZE,LENR)
           IEXICODT = 0
         ENDIF
        ENDIF


C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)

C Init var parallel SMP
      CALL SMP_INIT(
     1  ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2  IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)


        DT2TT = DT2T
        NELTSTT = NELTST
        ITYPTSTT= ITYPTST

        CALL ALESUB2(
     1     ALE_CONNECTIVITY%NALE   ,V      ,DSAVE  ,ICODT  ,ISKEW,
     2     SKEW    ,ASAVE ,A      ,D      ,NELTSTT,
     3     ITYPTSTT,ITSK  ,NODFTSK,NODLTSK,DT2SAVE,
     4     STIFN   ,DT2TT ,NELTSA ,ITYPTSA,NELTS  ,
     5     WEIGHT ,FSKY   ,FSKY   )

#include "lockon.inc"
        IF(DT2TT<DT2T)THEN
          DT2T = DT2TT
          NELTST = NELTSTT
          ITYPTST= ITYPTSTT
        END IF
#include "lockoff.inc"

!$OMP END PARALLEL

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
        IFSUB=0
        IFSUBM=0

C-----------------------------
C       MAJ SPMD ecriture L01
C-----------------------------
        IF(NSPMD>1.AND.IWIOUT>0) THEN
         CALL SPMD_WIOUT(IOUT,IWIOUT)
         IWIOUT = 0
        ENDIF
        GOTO 21

      ENDIF

      IF (IMON>0) CALL STARTIME(6,1)

C----------------------------------
C       SCALE TIME STEP FOR OLD AIRBAGS
C----------------------------------
        IF(NRBAG>0)THEN
        IF (IMONM > 0) CALL STARTIME(50,1)
            CALL RBAGDT(GEO,IGEO)
        IF (IMONM > 0) CALL STOPTIME(50,1)
        ENDIF

C----------------------------------
C       SCALE TIME STEP FOR
C       - MONITORED VOLUMES
C       - a M + b K DAMPING
C----------------------------------

        IF(NVOLU>0)THEN
        IF (IMONM > 0) CALL STARTIME(50,1)
        IF(NSPMD>1)
     .    CALL SPMD_GLOB_MINV(DT2,ITYPTS,NELTS,VOLMON, FR_MV)

C========================================================================================
C                                   DOMAIN 0 
C========================================================================================

        IF(ISPMD==0) CALL MVOLUDT(MONVOL,VOLMON)
        IF (IMONM > 0) CALL STOPTIME(50,1)
        ENDIF
C----------------------------------
      IF (IMONM > 0) CALL STARTIME(52,1)
       IF (NODADT==0) THEN
        IF (ISTAT==3) THEN
         DAMPA3 = TWO*BETATE/(ONE + BETATE * DT12)
         IF (DT2>=EP06) DAMPA3=ZERO
        ELSE
         DAMPA3 = ZERO
        END IF 
       
        IF(IDAMP>0)THEN
         IF(DAMPB>=ZERO)THEN
          BB  = (MIN(DAMPB,DT1,DT2) + HALF*(DAMPA+DAMPA3)*DT2*DT2)
C--       IF no node/elemt on proc - dt2=10E6 -> dt2 can be equal to zero
          IF (DT2>=EP06) BB = ZERO
          DT2 = SQRT(BB*BB + DT2*DT2) - BB
         ELSE
          BB  = ONE - DAMPB - DAMPB
          DT2 = DT2/SQRT(BB)
         ENDIF
        ELSEIF(NDAMP>0) THEN
          IF(NRDAMP==4)THEN
            BB = ZERO
            DO I=1,NDAMP
              DAMPA = DAMPR(3,I)
              DAMPB = DAMPR(4,I)
              D_TSTART = DAMPR(17,I)
              D_TSTOP  = DAMPR(18,I)
C--           IF no node/elemt on proc - dt2=10E6 -> dt2 can be equal to zero
              IF ((TT>=D_TSTART).AND.(TT<=D_TSTOP).AND.(DT2 < EP06))
     .          BB=MAX(BB,(MIN(DAMPB,DT1,DT2)+HALF*(DAMPA+DAMPA3)*DT2*DT2))
            ENDDO
          ELSE
            BB = ZERO
            IF (FLG_DTNODAMP==1) GOTO 600
            DO I=1,NDAMP
              DAMPA = MAX(DAMPR(3,I),DAMPR(5,I),DAMPR(7,I))
              DAMPA = MAX(DAMPA,DAMPR(9,I),DAMPR(11,I),DAMPR(13,I))
              DAMPB = MAX(DAMPR(4,I),DAMPR(6,I),DAMPR(8,I))
              DAMPB = MAX(DAMPB,DAMPR(10,I),DAMPR(12,I),DAMPR(14,I))
              FACTB = DAMPR(16,I)
              D_TSTART = DAMPR(17,I)
              D_TSTOP  = DAMPR(18,I)
C--           IF no node/elemt on proc - dt2=10E6 -> dt2 can be equal to zero
              IF ((TT>=D_TSTART).AND.(TT<=D_TSTOP).AND.(DT2 < EP06)) THEN
                DAMPT = MIN(DT1,DT2)*FACTB
                BB=MAX(BB,(MIN(DAMPB,DAMPT)+HALF*(DAMPA+DAMPA3)*DT2*DT2))
              ENDIF
            ENDDO
600         CONTINUE
           END IF
           DT2 = SQRT(BB*BB + DT2*DT2) - BB
        ELSEIF(ISTAT==3) THEN
          BB  = HALF*DAMPA3*DT2*DT2
          DT2 = SQRT(BB*BB + DT2*DT2) - BB
        ENDIF
       END IF !(NODADT==0) THEN

      IF (IMONM > 0) CALL STOPTIME(52,1)

C-----------------------------
C       Write L01
C-----------------------------

      IF (IMONM > 0) CALL STARTIME(53,1)
      IF(NSPMD>1) THEN
         IWIOUT = 0
         IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
       ENDIF

      ! ----------------------
      ! user library : check out
      IF(DLIB_STRUCT(ID_ENGINE_USER_CHECK)%DLIB_BOOL) THEN
        TSTOP_USER = TSTOP
        TT_USER = TT
        MSTOP_USER = 0
        NCYCLE_USER = NCYCLE
        CALL ENGINE_USER_CHECK(ISPMD_USER,TSTOP_USER,NCYCLE_USER,TT_USER,MSTOP_USER)
        IF(MSTOP_USER > 0) THEN 
            MSTOP=1
            MREST=1
         ENDIF
      ENDIF
      ! ----------------------
#ifdef DNC
      IF(MDS_AVAIL==1) THEN
        TSTOP_USER = TSTOP
        TT_USER = TT
        MSTOP_USER = 0
        NCYCLE_USER = NCYCLE
        CALL MDS_ENGINE_USER_CHECK(ISPMD_USER,TSTOP_USER,NCYCLE_USER,TT_USER,MSTOP_USER)
        IF(MSTOP_USER > 0) THEN 
            MSTOP=1
            MREST=1
         ENDIF
      ENDIF
#endif

      ! ----------------------

      IF(NSPMD>1)THEN
        CALL SPMD_GLOB_MIN5(DT2  ,ITYPTS,NELTS ,ICODT ,IMSCH,
     .                      TSTOP,IWIOUT,MSTOP ,ISMSCH,
     .                      INT24USE,NBINTC,INTLIST,IPARI,INTBUF_TAB)

C If FVMBAGS switch to UP using NPOLH criterion, then
C an SPMD communication must be made to warn all processors (only
C processes in charge of the FVMBAGS know NPOLH)
        IF(NFVBAG0 >0 .AND. CHECK_NPOLH) CALL SPMD_FVB_SWITCH(MONVOL)

        IF(IEXICODT>0) THEN
           SIZE = 1
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_ICODT(ICODT,IAD_ELEM,FR_ELEM,SIZE,LENR)
           IEXICODT = 0
        ENDIF
      ENDIF

        IF(IDT_THERM == 0) DT2= MIN(DT2,1.1*DT2OLD,DTMX)
        IF (IMPL_S==1)  CALL IMP_DT2(DT2)

      IF (IMONM > 0) CALL STOPTIME(53,1)

C --------------------------

        IF ((NCYCLE==0).AND.(FLG_KJ2==1)) THEN  
          CALL JOINT_BLOCK_STIFFNESS(ITAB,MS,IN,STK_SN,STK_SR,
     1              WEIGHT,IXR,IPART,X,IPART(K6),
     2              IGEO,GEO,NPBY,IPARG,ELBUF_TAB,DMAST,DINERT)
          DEALLOCATE(STK_SN,STK_SR)      
        ENDIF 

C--------------------------------------------------------------C 
C      RADIOSS 2 RADIOSS COUPLING
C--------------------------------------------------------------C 
        IF (IRAD2R /= 0 .AND. R2R_ACTIV == 1) THEN 
        CALL R2R_SENDKINE(IEXLNK,IGRNOD,MS,IN)                      
        IF (NSPMD>1) CALL SPMD_BARRIER()
C--------------------------------------------------------------C                 
!$OMP PARALLEL PRIVATE(ITSK)
!$OMP MASTER
C--------------------------------------------------------------C 
C--------------------------------------------------------------C 
        IF (IMONM > 0) CALL STARTIME(54,1)
        IF (NCYCLE == ZERO) TT_DP = TT
C--------------------------------------------------------------C        
        IF(ISPMD==0)THEN
          CALL R2R_SEM_C()
          IF (IRESP==1) CALL SEND_FBUFDP_C(TT_DP,1)
          IF (IRESP/=1) CALL SEND_FBUFDP_C(TT,1)
          IF ((R2R_SIU==1).AND.(IDDOM/=0)) THEN
            CALL GET_IBUF_C(R2R_TH_MAIN,10)
          ENDIF
          CALL SEND_FBUF_C(DT2,1)
          CALL SEND_IBUF_C(R2R_MFILR,1)
          CALL SEND_IBUF_C(R2R_MSTOP,1)
          IF (R2R_MFILR==1) THEN
            CALL SEND_FBUF_C(TMAN_R2R,1)
            CALL SEND_IBUF_C(R2R_CTR,3)
          ENDIF
          IF (IRESP==1) CALL GET_FBUFDP_C(TT_DP,1)
          IF (IRESP/=1) CALL GET_FBUFDP_C(TT,1)
          CALL GET_IBUF_C(MREST,1)
          CALL GET_IBUF_C(R2R_MFILR,1)
          CALL GET_IBUF_C(R2R_MSTOP,1)
          IF (R2R_MFILR==2) THEN
             CALL GET_FBUF_C(TMAN_R2R,1)
             CALL GET_IBUF_C(R2R_CTR,3)
          ENDIF
          CALL GET_FBUF_C(DT2,1)
        END IF
C--------------------------------------------------------------C         
        IF(NSPMD>1)THEN
           IF(ISPMD==0) THEN
             RBUF(1) = TT
             RBUF(2) = DT2
           END IF
           CALL SPMD_RBCAST(RBUF,RBUF,2,1,0,2)
           CALL SPMD_IBCAST(MREST,MREST,1,1,0,2)
           CALL SPMD_IBCAST(R2R_TH_MAIN,R2R_TH_MAIN,10,1,0,2)
           IF(ISPMD/=0) THEN
             TT = RBUF(1)
             DT2 = RBUF(2)
           END IF
         END IF
C--------------------------------------------------------------C
         IF ((R2R_SIU==1).OR.(NSPMD==1)) THEN          
           CALL R2R_SEM_C()
           CALL GET_IBUF_C(BID,1)                               
           CALL R2R_UNLOCK_THREADS_C(NTHREAD)
         ELSEIF (ISPMD==0) THEN
           CALL R2R_SEM_C()
           CALL GET_IBUF_C(BID,1)                               
           CALL R2R_UNLOCK_THREADS_C(NTHREAD*NSPMD)           
         ENDIF         
!$OMP END MASTER
         CALL R2R_BLOCK_C()        
C--------------------------------------------------------------C                       
!$OMP END PARALLEL
C--------------------------------------------------------------C 
C--------------------------------------------------------------C
                  
         CALL R2R_GETDATA(IEXLNK  ,IGRNOD  ,X       ,V       ,
     .                    VR      ,A       ,AR      ,MS      ,IN      ,
     .                    XDP     ,D       ,R2R_ON  ,DD_R2R  ,WEIGHT  ,
     .                    IAD_ELEM,FR_ELEM ,STIFN   ,STIFR   ,DD_R2R_ELEM  ,
     .                    SDD_R2R_ELEM,NLOC_DMG)

         IF (IMONM > 0) CALL STOPTIME(54,1)  
        
        ENDIF

#ifdef DNC
C----------------------------------------------
C Radioss Madymo coupling - Exchange Timesteps
C----------------------------------------------
      IF(IMADCPL==1)THEN
        MADENDREQUEST = 0
        IF (IMONM > 0) CALL STARTIME(55,1)
C Exchange Time Step
         CALL TSTP_EXCH_MADCPL(MADENDREQUEST,MADCLNOD,MADCLFRECV,
     *                         V,A,MS ,MADYMO_DEL_GLOBAL )
         IF (MADENDREQUEST == -1)THEN
           MSTOP = 2
           CALL TRACE_OUT(3)
           RETURN
         ENDIF
        IF (IMONM > 0) CALL STOPTIME(55,1)
      ENDIF
#endif
C-----------------------------
      IF (IMONM > 0) CALL STARTIME(53,1)
        DT2OLD=DT2
        IF (INCONV==1) THEN
          DT12=HALF*(DT1+DT2)
          DT3=DT1
        ENDIF
        IF(IALESUB==0.AND.ISHSUB==0)DT2S=DT2
C-----------------------------
C       L01
C-----------------------------
      IF(NSPMD>1.AND.IWIOUT>0) THEN
         CALL SPMD_WIOUT(IOUT,IWIOUT)
         IWIOUT = 0
       ENDIF
C-----------------------------------------------------
C         RAD2RAD : EXTERNAL WORK OF COUPLED PROCESSES
C-----------------------------------------------------
      IF (IRAD2R /= 0) THEN
        IF(ISPMD==0)THEN
          R2RFX1 = R2RFX1*DT2 + R2RFX2*DT12*DT2
         ENDIF
        ENDIF         
C       EXTERNAL WORK OF CONCENTRATED LOADS
      IF (NCONLD/=0.AND.IMPL_S/=1) THEN
          TFEXT = TFEXT + TFEXC*DT2
        ENDIF
    
      IF (IMONM > 0) CALL STOPTIME(53,1)
      IF (IMON>0) CALL STOPTIME(6,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
      IF (IMON>0) CALL STARTIME(7,1)

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

      ITSK = OMP_GET_THREAD_NUM()
      NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
      NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     SAVING TOTAL FORCES (Storing Fint+Fext+Fcont in Freac) - after condensing forces to main nodes !
C-----------------------------------------------
      IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN
        CALL STORE_FORCES(NODFTSK,NODLTSK,A,AR,FREAC)
      END IF

C--- // ----------------------------------------
C     INTERNAL FORCES (ANIM)
C-----------------------------------------------
      CALL FORANI2(FANI,A,NFIA,NFEA,NODFTSK,NODLTSK,H3D_DATA)

!$OMP END PARALLEL

      IF (IMON>0) CALL STOPTIME(7,1)
C-----------------------------------------------
C     SELECTIVE MASS SCALING
C-----------------------------------------------
      IF(IDTMINS == 1)THEN
C
C       Obsolete
      ELSEIF(IDTMINS == 2.OR.IDTMINS_INT /= 0)THEN

        IF (IMON>0) CALL STARTIME(39,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        CALL SMS_MASS_SCALE_2(
     1        ITSK     ,NODFTSK   ,NODLTSK  ,NODII_SMS ,INDX2_SMS ,
     2        NODXI_SMS,MS        ,MS0      ,A         ,ICODT     ,
     3        ICODR    ,ISKEW     ,SKEW     ,JAD_SMS   ,JDI_SMS   ,
     4        LT_SMS   ,X_SMS     ,P_SMS    ,Z_SMS     ,Y_SMS     ,
     5        PREC_SMS ,INDX1_SMS ,DIAG_SMS ,IAD_ELEM  ,FR_ELEM   ,
     6        WEIGHT   ,NPBY      ,LPBY      ,
     7        TAGSLV_RBY_SMS,LAD_SMS ,KAD_SMS ,JSM_SMS ,IBFV      ,
     8        VEL      ,NPC      ,TF       ,V         ,X          ,
     9        D        ,SENSOR_TAB,IFRAME    ,XFRAME     ,
     A        JADI_SMS ,JDII_SMS ,LTI_SMS  ,FR_SMS    ,FR_RMS     ,
     B        ISKYI_SMS,MSKYI_SMS,RES_SMS  ,IGRV      ,AGRV       ,
     C        LGRAV    ,ILINK    ,LLINK    ,FR_RL     ,FRL6       ,
     D        NNLINK   ,LNLINK   ,FR_LL    ,FNL6      ,TAG_LNK_SMS,
     E        ITAB     ,FSAV     ,LJOINT   ,IADCJ     ,FR_CJ      ,
     F        AR       ,VR       ,IN       ,FRL       ,FNL        ,
     G        NPRW     ,LPRW     ,RWBUF    ,RWSAV     ,
     H        FANI(1,1+NFOA+2*(NSECT+NRBODY)),FR_WALL ,NRWL_SMS   ,
     I        INTSTAMP ,KINET    ,IXC      ,IXTG      ,SH4TREE    ,
     J        SH3TREE  ,CPTREAC  ,NODREAC  ,FTHREAC   ,
     K        FRWL6    ,3+IRODDL*3,TAGSLV_RBY,DAMPR   , DAMP      ,
     L        IGRNOD   ,DR       ,RBY      ,TAGMSR_RBY_SMS,
     M        JSM_SMS  ,IRBE2     ,LRBE2   ,IAD_RBE2  ,FR_RBE2M   ,
     N        NMRBE2   ,R2SIZE    ,IRBE3    ,LRBE3      ,FRBE3    ,
     O        IAD_RBE3M ,FR_RBE3M,FR_RBE3MP ,RRBE3      ,RRBE3_PON,
     P        PREC_SMS3 ,DIAG_SMS3,IAD_RBY  ,FR_RBY6    ,RBY6     ,
     Q        R3SIZE    ,BETATE   ,IBCSCYC    ,LBCSCYC)

!$OMP END PARALLEL

        IF (IMON>0) CALL STOPTIME(39,1)

      ENDIF
      

      IDUM1=0
      RDUM1=ZERO
C
      K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
      K2=K1+NUMELS
      K3=K2+NUMELQ
      K4=K3+NUMELC
      K5=K4+NUMELT
      K6=K5+NUMELP
      K7=K6+NUMELR
      K8=K7+NUMELUR
      K9=K8+NUMELTG
      K10=K9+NUMELX
      K11=K10+NUMSPH

C-----------------------------------------------------------------
C     IMPLICIT  SYNCHRONISATION (FORCES), FV(FUNC) ET SKEW(MOVING)
C-----------------------------------------------------------------

      IF (IMPL_S == 1) THEN
        ENCIN = ZERO
        ENROT = ZERO
        ENCIN2 = ZERO
        ENROT2 = ZERO       
        IF (IMON>0) CALL STARTIME(5,1)
C-----------------------
        IF (IMPL_S==1 .AND. INCONV==1) THEN
          CALL THBCS_IMP(NODFT,NODLT ,A,AR,
     &                   FTHREAC,NODREAC,CPTREAC,FTHDTM,DT3)
        ENDIF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP   PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

        IF (ILAG+IALE+IEULER/=0) THEN
C-----------------------
C         MULTIMATERIAL
C-----------------------
          IF (NMULT>0) THEN
            CALL BMULTN(FILL,DFILL,IMS,IFILL,NODFTSK,NODLTSK)
          ENDIF
          CALL IMP_FANII(FANI ,A     ,NFIA      ,NODFT    ,NODLT    ,
     .                 H3D_DATA )
          IF (IMPDEB==1.AND.IMCONV==0) THEN
            IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) THEN
              CALL IMP_FOUT(
     1        FANI     ,A        ,AR       ,NFIA      ,NFEA       ,
     2        NODFTSK  ,NODLTSK  ,H3D_DATA ,IMPBUF_TAB)
            ENDIF
          ENDIF
C---      // N/3 ---------------------------------------
C         INTERNAL FORCES (ANIM)
C-----------------------------------------------
          IF (ISECUT/=0) THEN
            IF (IMON>0) CALL STARTIME(7,1)
              CALL SECT_IO (
     1        NSTRF,D,DR,V,VR,FSAV(1,1+NINTER+NRWALL+NRBODY),
     2        SECFCUM,A             ,AR    ,SECBUF,MS      ,IN     ,
     3        X      ,FANI(1,NFOA+1),WEIGHT,XSEC  ,IAD_ELEM,FR_ELEM,
     4        RG_CUT ,IAD_CUT       ,FR_CUT,WEIGHT_MD,IOLDSECT,
     5        STABSEN,SFSAV6        ,TABSENSOR   ,FBSAV6  )
            IF(IMON>0) CALL STOPTIME(7,1)
          ENDIF
        ENDIF ! ILAG+IALE+IEULER/=0

!$OMP   END PARALLEL

        IF (IMON>0) CALL STOPTIME(5,1)
C-----------------------------------------------
        IF (ILAG+IALE+IEULER/=0)THEN
          IF(IMON>0) CALL STARTIME(4,1)
          IF(IMONM > 0) CALL STARTIME(40,1)
         
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL

          CALL RBYCOR(
     1         RBY ,X   ,V    ,VR     ,SKEW    ,FSAV     ,
     2         LPBY,NPBY,ISKEW,ITAB   ,WEIGHT  ,A        ,
     3         AR  ,MS  ,IN   ,KINDRBY,IRBKIN_L,NRBYKIN_L ,
     4         WEIGHT_MD,MS_2D)         
!$OMP     END PARALLEL

          IF(IMON>0) CALL STOPTIME(4,1)
          IF(IMONM > 0) CALL STOPTIME(40,1)
        ENDIF  ! ILAG+IALE+IEULER/=0
C========================================================================================
C-----  RBE2 Bilan crrection        
C========================================================================================
        IF (NRBE2>0)THEN
          IF(IMON>0) CALL STARTIME(4,1)
          IF(IMONM > 0) CALL STARTIME(40,1)
C                                     

!$OMP PARALLEL
            CALL RBE2COR(IRBE2 ,LRBE2 ,X    ,V     ,VR    ,
     2                   SKEW  ,ISKEW ,ITAB ,WEIGHT,A     ,
     3                   AR    ,MS0   ,IN   ,WEIGHT_MD)
!$OMP     END PARALLEL

          IF(IMON>0) CALL STOPTIME(4,1)
          IF(IMONM > 0) CALL STOPTIME(40,1)
        ENDIF
C--------------------
C       SENSOR //  IN IMPLICIT
C-----------------------------------------------
C       Exchange FBSAV6 if sensor 6/12 activated 
C-----------------------------------------------
        IF (NSENSOR > 0) THEN
          IPARSENS=0
          IF(STABSEN/=0) THEN
             IPARSENS=1
          ENDIF
          IF((NSPMD>1).AND.(IPARSENS/=0)) THEN
            DIM6=12
            DIM_EXCH=SFSAV6
            CALL SPMD_EXSUM_FB6(DIM6,DIM_EXCH,FBSAV6)
          ENDIF
        ENDIF
c
        IF (NSENSOR > 0 .AND. INCONV == 1) THEN
          CALL SENSOR_ENER_SAV(SENSOR_TAB ,PARTSAV ,PARTSAV2)
          ! -------------------------------
          ! pre-computation and mpi communication for type 16 sensor
          IF (COMM_SENS16%BOOL) THEN
             CALL SENSOR_DIST_SURF0(SENSOR_TAB,X,IGRSURF)
          ENDIF
          ! -------------------------------
          ! pre-computation and mpi communication for type 17 sensor
          IF(COMM_SENS17%BOOL) THEN
             CALL SENSOR_TEMP0(SENSOR_TAB,IGRNOD,TEMP,WEIGHT)
          ENDIF
          ! -------------------------------
          IF (NSPMD > 1) THEN
            CALL SENSOR_SPMD(SENSOR_TAB,IPARI   ,NPRW     ,ISENSP  ,NSENSP  ,
     .                   XSENS     ,X       ,ACCELM   ,IACCP   ,NACCP   ,
     .                   GAUGE     ,IGAUP   ,NGAUP    ,PARTSAV2)
          ENDIF      
c
          ! check activation condition of base sensors
          CALL SENSOR_BASE(SENSOR_TAB,
     .         XSENS     ,IPARSENS  ,TABSENSOR ,STABSEN   ,IPARI     ,
     .         SFSAV6    ,FSAV      ,FBSAV6    ,PARTSAV2  ,GAUGE     ,
     .         X         ,V         ,A         ,ACCELM    ,NPRW      ,
     .         SUBSETS   ,IGRSURF   ,IGRNOD    )
c
          ! check activation condition of logical sensors hierarchy
          CALL SENSOR_LOGICAL(SENSOR_TAB)

        ENDIF

        GOTO 111

      ENDIF
C-----------------------------
C       Non pure thermal case 
C-----------------------------
        IF (ILAG + IALE + IEULER /= 0) THEN

          IF (IMON>0) CALL STARTIME(5,1)
        
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP     PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- //   ----------------
C         MULTIMATERIAL
C-------  ----------------
          IF(NMULT>0) THEN
            CALL BMULTN(FILL,DFILL,IMS,IFILL,NODFTSK,NODLTSK)
          ENDIF

!$OMP     END PARALLEL

C-----------------------------------------------
C         SPH SMOOTHING OF VELOCITIES
C-----------------------------------------------
          IF ((NUMSPH/=0).OR.(SOL2SPH_FLAG==1)) THEN
C------------------------
C           Conservative smoothing of velocities
C------------------------
            IF (IMONM > 0) CALL STARTIME(48,1)
            IF (IMONM > 0) CALL STARTIME(89,1)

            ALLOCATE(WASPSYM(3*NSPHSYM+1),STAT=IERROR)
            IF(IERROR/=0)THEN
               CALL ANCMSG(MSGID=19,ANMODE=ANINFO,C1="WASPSYM")
               CALL ARRET(2)
            ENDIF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP      PARALLEL PRIVATE(ITSK,IPMTSK)
           ITSK = OMP_GET_THREAD_NUM()
           IPMTSK = 1 + ITSK*NPSAV*NPART

            CALL SPLISSV(
     1      X         ,V        ,MS       ,A         ,SPBUF     ,
     2      WA        ,ITAB     ,KXSP     ,IXSP      ,NOD2SP    ,
     3      D         ,ISPSYM   ,XSPSYM%BUF   ,VSPSYM%BUF    ,BUFMAT    ,
     4      BUFGEO    ,NPC      ,TF       ,PM        ,GEO       ,
     5      ISPCOND   ,XFRAME   ,WASPSYM,IPART(K10),PARTSAV(IPMTSK),
     6      WASPH(KSPH21) ,WSMCOMP%BUF ,WASPH(KSPACTIV) ,IPART,ITSK,
     7      SPH2SOL   ,SOL2SPH  ,IRST    ,IXS        ,IPARG     ,
     8      NGROUNC   ,IGROUNC  ,ELBUF_TAB,IAD_ELEM  ,FR_ELEM   ,
     9      IGEO      ,SOL2SPH_TYP)

!$OMP       END PARALLEL
            DEALLOCATE(WASPSYM)
          
C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================

            IF (IMONM > 0) CALL STOPTIME(48,1)
            IF (IMONM > 0) CALL STOPTIME(89,1)
C
C------------------------
C           Second Part of artificial forces work.
C--- //0 ----------------
            IF (IMON>0) CALL STARTIME(6,1)
            IF (IMONM > 0) CALL STARTIME(48,1)
            CALL SPWFVIS(SPBUF,IPART(K10),PARTSAV,IPARG,ELBUF_TAB,
     .                   KXSP ,WASPH(KSPACTIV))
            IF (IMONM > 0) CALL STOPTIME(48,1)
            IF (IMON>0) CALL STOPTIME(6,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
          CALL SMP_INIT(
     1      ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK   ,
     2      IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

          CALL ASSPART(
     2       PARTFTSK,PARTLTSK,PARTSAV,GREFTSK,GRELTSK,GRESAV)
     
!$OMP     END PARALLEL

        ENDIF  ! (NUMSPH/=0).OR.(SOL2SPH_FLAG==1)
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
C       BALANCING FORCES COMPUTED AT TT=0 IN GLOBAL REFERENCE SYSTEM 
C----------------------------------------------------------------------------------------
        IF (TT==ZERO.AND.IABS(ISIGI)==5) THEN
C--- //0 ----------------
          IF (IMON>0) CALL STARTIME(6,1)
          IF (IMONM > 0) CALL STARTIME(49,1)
           CALL FEQUILIBRE(A,FZERO,IXC,IXTG)
          IF (IMONM > 0) CALL STOPTIME(49,1)
          IF (IMON>0) CALL STOPTIME(6,1)
        ENDIF
C========================================================================================
C MPI communication for NLOC_DMG : parith/off
C========================================================================================
        IF(IPARIT == 0.AND.NSPMD > 1.AND. NLOC_DMG%IMOD > 0)THEN
            CALL SPMD_EXCH_SUB_POFF(NLOC_DMG)
        ENDIF
       
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK,NODFT_NL,NODLT_NL)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C       ACCELERATIONS (TRANSLATIONS)
C-----------------------------------------------

      !-----------------------------
      !FINITE VOLUME METHOD FOR ALE
      !-----------------------------
        IF(IALEFVM>0)THEN
          CALL ALEFVM_ACCELE(
     1                       A      , AR     , V   , VR   , FZERO,
     2                       NODFTSK, NODLTSK, ITAB, MSNF , MS   ,
     3                       ALE_CONNECTIVITY%NALE     )     
        ENDIF

        CALL ACCELE(A       ,AR      ,V    ,MS     ,IN     ,
     2              NODFTSK ,NODLTSK ,ITAB ,ALE_CONNECTIVITY%NALE   ,MS_2D  ,
     3              NPBY   )
C
        IF(IPLYXFEM > 0 )
     .    CALL PLY_ACCELE(INOD_PXFEM,MS_PLY,ZI_PLY,MS,
     .              NODFTSK,NODLTSK,NPLYMAX,NPLYXFE,NUMNOD,MSZ2 )
     
C
       IF(IALELAG > 0) THEN
          CALL FLOW_ACCELE(ALE_CONNECTIVITY%NALE, MSF    ,AFLOW        ,VFLOW    ,
     2                     NODFTSK,NODLTSK)
        ENDIF
c
        IF (NLOC_DMG%IMOD > 0) THEN
          NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
          NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
          CALL NLOCAL_ACC(NLOC_DMG, NODFT_NL ,NODLT_NL) 
        ENDIF
c
       IF (ICRACK3D > 0) THEN                                     
C        /---------------/
         CALL MY_BARRIER
C        /---------------/
         CALL CRK_ACCELE(ADSKY_CRK,INOD_CRK,NODLEVXF ,NODFTSK ,NODLTSK ,
     .                   NODENR   ,CRKSKY  ,MS       ,IN      ,ITAB    )
C        /---------------/
         CALL MY_BARRIER
C        /---------------/
         CALL CRK_ZERO_FSKY(CRKSKY,ADSKY_CRK,INOD_CRK,NODFTSK,NODLTSK,
     .                      NODLEVXF)
C        /---------------/
         CALL MY_BARRIER
C        /---------------/
       ENDIF
C 
       IF(NPINCH > 0) THEN
         CALL ACCELEPINCH(
     1                    PINCH_DATA%APINCH,      MS, PINCH_DATA%MSPINCH,
     2                 PINCH_DATA%STIFPINCH, NODFTSK,            NODLTSK,
     3                                 DT2T,   DTFAC)
       ENDIF
C
C---
C--- //  ---------------------------------------
C       TEMPERATURES COMPUTATION 
C-----------------------------------------------
       IF (ITHERM_FE > 0 )
     .    CALL TEMPUR(TEMP ,MCP,FTHE,NODFTSK,NODLTSK,WEIGHT,MCP_OFF)
!$OMP END PARALLEL

C------------------------------------------
C       DT_DC for thick-shell 
C-----------------------------------------------
       IF(NTSHEGG>0.AND.NSPMD > 1) 
     .     CALL SPMD_EXCH_VMAX(IAD_STSH ,FR_STSH ,IAD_RTSH ,FR_RTSH ,ALPHA_DC )
       IF(NTSHEG > 0) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+PRIVATE(ITSK,NODFTSK,NODLTSK,GREFTSK,GRELTSK)
          ITSK = OMP_GET_THREAD_NUM()
          GREFTSK   = 1+ITSK*NTSHEG/ NTHREAD
          GRELTSK   = (ITSK+1)*NTSHEG/NTHREAD
          CALL ACCDTDC(GREFTSK,GRELTSK,IENUNL ,ALPHA_DC,A       ,MS   ,ITAB )
!$OMP     END PARALLEL
       ENDIF


       IF(NTSHEGG>0.AND.NSPMD > 1) 
     .     CALL SPMD_EXCH_FA(IAD_STSH ,FR_STSH ,IAD_RTSH ,FR_RTSH ,A   ) 
C---------------------------------------------------------------------
C DEBUG TEMPERATURES OUTPUT
C---------------------------------------------------------------------
      IF (DEBUG(MACRO_DEBUG_TEMP)==1) THEN
         IF (NCYCLE>=TDEBSTART .AND.
     .                  MOD(NCYCLE-TDEBSTART,TRSTFREQ)==0) THEN

           IF(NSPMD > 1) THEN
             IF (ISPMD==0) THEN
               SIZ = NUMNODG
             ELSE
               SIZ = 0
             END IF
             CALL SPMD_COLLECTT(TEMP,ITAB,WEIGHT,NODGLOB,SIZ)
          ELSE
             CALL COLLECTT(TEMP,ITAB,WEIGHT,NODGLOB)
          END IF
        END IF
      END IF

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

        IF (IMON>0) CALL STOPTIME(5,1)
C-----------------------------------------------
C  INLETS,OUTLETS.
C-----------------------------------------------
        IF(NSPHIO/=0)THEN

C globalize NSPHACT in NSPHACTG
       NSPHACTG = NSPHACT

       IF(NSPMD>1)THEN
         CALL SPMD_GLOB_ISUM9(NSPHACTG,1)
         CALL SPMD_IBCAST(NSPHACTG,NSPHACTG,1,1,0,2)
       END IF

          IF(NSPHACTG/=0)THEN
C--- //0 ----------------
          IF (IMON>0) CALL STARTIME(6,1)
          IF (IMONM > 0) CALL STARTIME(48,1)
          IF (IMONM > 0) CALL STARTIME(89,1)

           CALL SPONFV (X   ,V     ,A       ,D       ,MS      ,
     2            SPBUF   ,ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3            NPC     ,TF      ,ISPHIO  ,VSPHIO  ,IPART   ,
     4           IPART(K10),WASPH(KSPACTIV) ,WA,WASPH(KSPH22) )

          IF (IMONM > 0) CALL STOPTIME(89,1)
          IF (IMONM > 0) CALL STOPTIME(48,1)
          IF (IMON>0) CALL STOPTIME(6,1)
          ENDIF
        END IF
        
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C-----------------------------------------------
C INLETS,OUTLETS.
C-----------------------------------------------
       IF(NSPHIO/=0.AND.NSPHACTG/=0)THEN
C--- //0 ----------------
           IF (IMON>0) CALL STARTIME(6,1)
           IF (IMONM > 0) CALL STARTIME(48,1)
           IF (IMONM > 0) CALL STARTIME(89,1)

           CALL SPONFV (X   ,V     ,A       ,D       ,MS      ,
     2            SPBUF   ,ITAB    ,KXSP    ,IXSP    ,NOD2SP  ,
     3            NPC     ,TF      ,ISPHIO  ,VSPHIO  ,IPART   ,
     4            IPART(K10),WASPH(KSPACTIV),WA,WASPH(KSPH22) )

           IF (IMONM > 0) CALL STOPTIME(89,1)
           IF (IMONM > 0) CALL STOPTIME(48,1)
           IF (IMON>0) CALL STOPTIME(6,1)
        ENDIF

C-----------------------------------------------
C Multidomains : resynchro des accelerations en SPH
C----------------------------------------------
        IF (NSPMD>1) THEN
          IF ((SDD_R2R_ELEM>0).AND.(NUMSPH_GLO_R2R>0)) THEN
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            CALL SPMD_EXCH_R2R_SPH(A,IAD_ELEM,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
          ENDIF
        ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
      IF (IMON>0) CALL STARTIME(7,1)

      IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     Gravity & other loadings <=> Needs to add external forces to Freac (Storing Fint+Fext+Fcont in Freac)
C-----------------------------------------------
        IFLAG = -1
        CALL STORE_ACCEL_FIELD(NODFTSK,NODLTSK,A,AR,MS,IN,FREAC,IFLAG)

!$OMP END PARALLEL

      END IF

      IF (IMON>0) CALL STOPTIME(7,1)
C------------------------------
C         GRAVITE / ACCEL. BASE
C------------------------------
        IF(NGRAV/=0.AND.IDTMINS==0.AND.IDTMINS_INT==0) THEN
        IF (IMON>0) CALL STARTIME(4,1)
        IF (IMONM > 0) CALL STARTIME(46,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()
          IF(MULTI_FVM%IS_USED)THEN
            CALL GRAVIT_FVM_FEM(
     1                  IGRV    ,AGRV   ,NPC   ,TF       ,A     ,
     2                  V       ,X      ,SKEW  ,MS       ,SENSOR_TAB,
     3                  WEIGHT ,LGRAV ,ITSK     ,ALE_CONNECTIVITY%NALE)
         ELSE
            CALL GRAVIT(
     1                  IGRV    ,AGRV   ,NPC   ,TF       ,A     ,
     2                  V       ,X      ,SKEW  ,MS       ,SENSOR_TAB,
     3                  WEIGHT ,LGRAV ,ITSK     ,ALE_CONNECTIVITY%NALE  ,
     4                  ITAB)
         ENDIF
!$OMP END PARALLEL

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------
C         GRAVITY FLEXIBLE BODIES
C----------------------
          IF (NFXBODY>0) THEN
             CALL FXGRVCOR(FXBIPM, FXBGRVI, A,  IGRV, AGRV,
     .                     NPC,    TF,      MS, V   , SKEW,
     .                     FXBGRW, IAD_ELEM, FR_ELEM)
            END IF
          IF (IMON>0) CALL STOPTIME(46,1)
          IF (IMONM > 0) CALL STOPTIME(4,1)
        ENDIF
C----------------------
C         /BCS/CYCLIC
C----------------------
        IF(NBCSCYC > 0)THEN
           CALL BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,V,A,ITAB)
        ENDIF
C----------------------
C         CENTRIFUGAL FORCES
C----------------------
         IF (NLOADC>0) THEN
          IF (IMON>0) CALL STARTIME(4,1)
          
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()
          CALL CFIELD_1(ICFIELD  ,CFIELD,NPC   ,TF    ,A,
     2                    V     ,X     ,XFRAME  ,MS,SENSOR_TAB,
     3                    WEIGHT,LCFIELD,ITSK ,IFRAME)
!$OMP END PARALLEL

          IF (IMON>0) CALL STOPTIME(4,1)
         ENDIF
         
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (IMON>0) CALL STARTIME(7,1)

      IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     Gravity & other loadings <=> Needs to add external forces to Freac (Storing Fint+Fext+Fcont in Freac)
C-----------------------------------------------
        IFLAG =  1
        CALL STORE_ACCEL_FIELD(NODFTSK,NODLTSK,A,AR,MS,IN,FREAC,IFLAG)

!$OMP END PARALLEL

      END IF

      IF (IMON>0) CALL STOPTIME(7,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C-----------------------------------------------
C       EXTERNAL FORCES COMPUTATION (ANIM) before applying other kinematic conditions (BCS, ...)
C-----------------------------------------------
      IF (IMON>0) CALL STARTIME(7,1)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

      ITSK = OMP_GET_THREAD_NUM()
      NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
      NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
      CALL FORANI3(FANI,A,MS,NFEA,NODFTSK,NODLTSK,H3D_DATA)

!$OMP END PARALLEL
      IF (IMON>0) CALL STOPTIME(7,1)
      
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C----------------------
C         INTERFACE ALE
C----------------------
        IERR=0
        IF(NINTER/=0.AND.IALE/=0)THEN
          IF(ISPMD==0)THEN
            IF (IMON>0) THEN
              CALL STARTIME(6,1)
              CALL STARTIME(2,1)
            ENDIF
            CALL INTAL1(IPARI,X    ,V ,
     2                  A    ,ISKEW           ,SKEW            ,ICODT,WA,
     3                  MS   ,ITAB            ,FSAV            ,INTBUF_TAB ,
     4                  FANI ,FANI(1,NFNCA+1) ,FANI(1,NFTCA+1) ,H3D_DATA   )
            IF (IMON>0) THEN
              CALL STOPTIME(2,1)
              CALL STOPTIME(6,1)
            ENDIF
          ENDIF
C        /---------------/
          CALL MY_BARRIER
C        /---------------/
        ENDIF
        IF(IERR==1)CALL ARRET(2)

        IF(NINTER /= 0.and.IALE+IEULER /= 0.and.
     .     INT18KINE == -1)THEN
     
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()


C        /---------------/
          CALL MY_BARRIER
C        /---------------/
          CALL I18MAIN_KINE_2(IPARI,INTBUF_TAB,X  ,V  ,
     2                  A    ,ISKEW,SKEW ,ICODT,WA,
     3                  MS   ,ITAB ,FSAV ,ITSK+1,KINET,
     4                  STIFN,MTF  ,CAND_SAV,FANI,INT18ADD,
     5                  IAD_ELEM,FR_ELEM,H3D_DATA )
!$OMP END PARALLEL
        ENDIF
        
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================
        
C----------------------
C  Activating valve (in/out or imposed velocity)
C----------------------

        IF(EBCS_TAB%nebcs_loc>0)THEN
          IF (IMON>0) THEN
            CALL STARTIME(6,1)
            CALL STARTIME(2,1)
          END IF
          IF(ISPMD==0)THEN
            CALL EBCCLAP(V,A,FV,EBCS_TAB)
          ENDIF
          IF (IMON>0) THEN
            CALL STOPTIME(2,1)
            CALL STOPTIME(6,1)
          END IF
        ENDIF

C----- // ----------------------
C         CONDITIONS AUX LIMITES
C-------------------------------
        IF (IMON>0) CALL STARTIME(4,1)
        IF (IMONM > 0) CALL STARTIME(42,1)

        CALL THBCS(NODFT  ,NODLT  ,ICODT  ,ICODR,ISKEW,
     2             SKEW   ,A      ,AR     ,MS   ,IN   ,
     3             FTHREAC,NODREAC,CPTREAC)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
        CALL BCS10(NODFTSK,NODLTSK  ,ICODT  ,ICODR,ISKEW,
     2             SKEW   ,A        ,AR     ,MS   ,V    ,
     3             VR     )
C---

        IF(IPLYBCS > 0 .AND. IPLYXFEM > 0 )
     .    CALL PLY_BCS(NODFTSK, NODLTSK,ICODT_PLY,ISKEW_PLY,SKEW,
     .                 INOD_PXFEM,MS_PLY,IBC_PLY)
!$OMP END PARALLEL

        IF (IMONM > 0) THEN
          CALL STOPTIME(42,1)
          CALL STARTIME(43,1)
        END IF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
C-------------------
C      [RLINK1][RLINK2]...   (SUR TASK LIBRE)
C      [RIVETS]                     -
C      [JOINT1][JOINT2]...          -

!$OMP PARALLEL PRIVATE(ITSK)
        ITSK = OMP_GET_THREAD_NUM()

C-----------------------------------
C         LIENS RIGIDES ENTRE NOEUDS
C---- // ----------------------------
        IF(NRLINK>0)CALL RLINK10(
     1     MS   ,IN    ,A    ,AR  ,V    ,
     2     VR   ,ILINK ,LLINK,SKEW,FR_RL,
     3     WEIGHT,FRL6)
        IF(NLINK>0) CALL RLINK11(
     1     MS   ,IN    ,A     ,AR    ,V    ,
     2     VR   ,NNLINK,LNLINK,SKEW  ,FR_LL,
     3     WEIGHT,FNL6  ,X     ,XFRAME)
C---------------
C         RIVETS
C---------------
          IF(NRIVET>0) THEN
            CALL RIVET1(
     +         MS    ,IN   ,A  ,AR,X ,
     +         LRIVET,RIVET,GEO,V ,VR,
     +         ITSK  )
          ENDIF

C-------------
C       JOINTS
C-- // -----------
        IF(NJOINT>0) THEN
          CALL CJOINT(A    ,AR    ,V ,VR,X    ,
     2                FSAV ,LJOINT,MS,IN,IADCJ,
     3                FR_CJ,TAG_LNK_SMS(NRLINK+NLINK+1),ITSK)
        ENDIF

!$OMP END PARALLEL

        IF (IMON>0) CALL STOPTIME(4,1)
        IF (IMONM > 0) CALL STOPTIME(43,1)
        ENDIF
C--------------------------- FIN CAS NON THERMIQUE PURE

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (IMON>0) CALL STARTIME(7,1)

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
      ITSK = OMP_GET_THREAD_NUM()
      NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
      NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C--- //-----------------------------------------
C         SAVE FORCES FROM INITIAL STATE
C---------------------------------------
      IF(TT==ZERO.AND.(ISIGI==2.OR.ISIGI==4)) THEN
#include      "vectorize.inc"
        DO I=NODFTSK,NODLTSK
            FZERO(1,I)=-A(1,I)
            A(1,I)=ZERO
            FZERO(2,I)=-A(2,I)
            A(2,I)=ZERO
            FZERO(3,I)=-A(3,I)
            A(3,I)=ZERO
         ENDDO
      ENDIF
!$OMP END PARALLEL

      IF (IMON>0) CALL STOPTIME(7,1)

C-----------------------------------------------
C          DAMPING alpha M + beta K
C-----------------------------------------------

          IF (NS10E>0.AND.(IDAMP/=0.OR.NDAMP>0.OR.ISTAT/=0)) THEN
            CALL S10GETVDM(ICNDS10,V,VND,VMD)
          END IF
          
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (IMON>0) CALL STARTIME(7,1)

      IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     Damping <=> Needs to add damping forces to Freac (Storing Fint+Fext+Fcont+Fdamp in Freac)
C-----------------------------------------------
        IFLAG = -1
        CALL STORE_ACCEL_FIELD(NODFTSK,NODLTSK,A,AR,MS,IN,FREAC,IFLAG)

!$OMP END PARALLEL

      END IF

      IF (IMON>0) CALL STOPTIME(7,1)
C-----------------------------------------------
      IF (IMONM > 0) CALL STARTIME(52,1)

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

      ITSK = OMP_GET_THREAD_NUM()
      NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
      NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

      IF(IDAMP/=0)THEN
        IF (NS10E>0) THEN
          CALL DAMPING(NODFTSK,NODLTSK,VMD,VR,A ,AR ,DAMP,MS,IN,
     .               IGRNOD,3+IRODDL*3,ITSK,WEIGHT,TAGSLV_RBY)
        ELSE
         CALL DAMPING(NODFTSK,NODLTSK,V ,VR,A ,AR ,DAMP,MS,IN,
     .               IGRNOD,3+IRODDL*3,ITSK,WEIGHT,TAGSLV_RBY)
        END IF !(NS10E>0) THEN
      ELSEIF(NDAMP>0)THEN
        IF(NRDAMP==4)THEN

!$OMP SINGLE
          CALL DAMPING44(
     .                 NODFT   ,NODLT            ,3+IRODDL*3,V       ,
     .                 VR      ,A       ,AR      ,MS        ,IN      ,
     .                 DAMPR   ,DAMP    ,IGRNOD  ,WEIGHT    ,TAGSLV_RBY)
!$OMP END SINGLE

        ELSE

!$OMP SINGLE
         IF (NS10E>0) THEN
           CALL DAMPING51(
     .                 NODFT   ,NODLT            ,3+IRODDL*3,VMD     ,
     .                 VR      ,A       ,AR      ,MS        ,IN      ,
     .                 DAMPR   ,DAMP    ,IGRNOD  ,WEIGHT    ,TAGSLV_RBY,
     .                 SKEW    ,ICONTACT,IDAMP_RDOF_TAB ,NODXI_SMS)
         ELSE
           CALL DAMPING51(
     .                 NODFT   ,NODLT            ,3+IRODDL*3,V       ,
     .                 VR      ,A       ,AR      ,MS        ,IN      ,
     .                 DAMPR   ,DAMP    ,IGRNOD  ,WEIGHT    ,TAGSLV_RBY,
     .                 SKEW    ,ICONTACT,IDAMP_RDOF_TAB ,NODXI_SMS)
         END IF
!$OMP END SINGLE

        END IF
      END IF

!$OMP END PARALLEL

      IF(IMON>0) CALL STOPTIME(52,1)
C-----------------------------------------------
      IF (IMON>0) CALL STARTIME(7,1)

      IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     Damping & other loadings <=> Needs to add damping forces to Freac (Storing Fint+Fext+Fcont+Fdamp in Freac)
C-----------------------------------------------
        IFLAG =  1
        CALL STORE_ACCEL_FIELD(NODFTSK,NODLTSK,A,AR,MS,IN,FREAC,IFLAG)

!$OMP END PARALLEL

      END IF

      IF (IMON>0) CALL STOPTIME(7,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (IMON>0) CALL STARTIME(7,1)

      IF(CPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     imposed loadings <=> Needs to add  to FTHREAC
C-----------------------------------------------
        IFLAG = -1
        CALL STORE_ACCEL_FIELD_TH(NODFTSK,NODLTSK ,A    ,AR     ,MS    ,
     .                            IN     ,FTHREAC ,IFLAG,NODREAC)

!$OMP END PARALLEL

      END IF

      IF (IMON>0) CALL STOPTIME(7,1)

C========================================================================================
C                                          NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------
C          STATIC 
C--------------------------
           IF(ISTAT/=0) THEN
             IF (IMON>0) CALL STARTIME(6,1)
             IF (IMONM > 0) CALL STARTIME(49,1)
             IF (NS10E>0) THEN
               CALL STATIC(VMD,VR,A,AR,MS,IN,IGRNOD,WEIGHT_MD)
             ELSE
               CALL STATIC(V,VR,A,AR,MS,IN,IGRNOD,WEIGHT_MD)
             END IF
             IF (IMONM > 0) CALL STOPTIME(49,1)
             IF (IMON>0) CALL STOPTIME(6,1)
           ENDIF
           ENCIN = ZERO
           ENROT = ZERO
           ENCIN2 = ZERO
           ENROT2 = ZERO           
C--------------------------
C          IMPOSED VELOCITY 
C--------------------------
           IF(NFXVEL/=0) THEN
             IF(IMON>0) THEN
               CALL STARTIME(6,1)
               CALL STARTIME(4,1)
               IF(IMONM > 0) CALL STARTIME(44,1)
             ENDIF
             CALL FIXVEL(IBFV  ,A     ,V      ,NPC    ,TF  ,
     2                   VEL   ,MS    ,X      ,SKEW   ,AR  ,
     3                   VR    ,IN    ,SENSOR_TAB,
     4                   WEIGHT,D     ,RBY    ,IFRAME ,
     5                   XFRAME,DR    ,PTR_SMS,
     6                   TT_DOUBLE,DDP)

             IF (FXVEL_FGEO ==1) THEN
               CALL FIXFINGEO(IBFV   ,A     ,V      ,NPC    ,TF     ,
     2                      VEL    ,MS    ,X      ,D      ,SENSOR_TAB ,
     3                      WEIGHT,CPTREAC,NODREAC,PTR_SMS,
     4                      FTHREAC,ITABM1,ITAB   )
             ENDIF
C
             IF(IMON>0) THEN
               IF(IMONM > 0) CALL STOPTIME(44,1)
               CALL STOPTIME(4,1)
               CALL STOPTIME(6,1)
             ENDIF
           ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (IMON>0) CALL STARTIME(7,1)

      IF(CPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

C--- // ---------------------------------------
C     imposed loadings <=> Needs to add  to FTHREAC
C-----------------------------------------------
        IFLAG = 1
        CALL STORE_ACCEL_FIELD_TH(NODFTSK,NODLTSK ,A    ,AR     ,MS    ,
     .                            IN     ,FTHREAC ,IFLAG,NODREAC)

!$OMP END PARALLEL

      END IF

      IF (IMON>0) CALL STOPTIME(7,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

        IF (IMON>0) CALL STARTIME(4,1)
        IF (IMONM > 0) CALL STARTIME(43,1)

!$OMP PARALLEL

C-------------------
C         RIGID WALL                               
C---- // -----------
        IF(NRWALL>0.AND.IDTMINS==1)THEN
C
C         Obsolete
        ELSEIF(NRWALL>0.AND.(IDTMINS==2.OR.IDTMINS_INT/=0))THEN
            CALL RGWAL0(
     1     X           ,A        ,V      ,RWBUF   ,LPRW,
     2     NPRW        ,MS       ,FSAV(1,NINTER+1),FR_WALL ,
     3     FANI(1,1+NFOA+2*(NSECT+NRBODY)),
     4     RWSAV       ,WEIGHT   ,FRWL6   ,NODXI_SMS, WEIGHT_MD,
     5     SFSAV6      ,FBSAV6   ,STABSEN ,TABSENSOR)
        ELSE
            CALL RGWAL0(
     1     X           ,A        ,V      ,RWBUF   ,LPRW,
     2     NPRW        ,MS       ,FSAV(1,NINTER+1),FR_WALL ,
     3     FANI(1,1+NFOA+2*(NSECT+NRBODY)),
     4     RWSAV       ,WEIGHT   ,FRWL6   ,NATIV_SMS, WEIGHT_MD,
     5     SFSAV6      ,FBSAV6   ,STABSEN ,TABSENSOR)
        ENDIF

!$OMP END PARALLEL

        IF (IMON>0) CALL STOPTIME(4,1)
        IF (IMONM > 0) CALL STOPTIME(43,1)

C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------
C         IMPOSED TEMPERATURES 
C--------------------------
      IF (NFXTEMP  > 0 .AND. ITHERM_FE > 0) THEN
        IF (IMON>0) THEN
          CALL STARTIME(6,1)
          CALL STARTIME(4,1)
          IF(IMONM > 0) CALL STARTIME(44,1)
        ENDIF
        CALL FIXTEMP(IBFTEMP  ,FBFTEMP    ,TEMP    ,NPC    ,TF  ,
     1               SENSOR_TAB)
        IF (IMON>0) THEN
          IF(IMONM > 0) CALL STOPTIME(44,1)
          CALL STOPTIME(4,1)
          CALL STOPTIME(6,1)
        ENDIF
      ENDIF
C---------------
        IF(ISECUT/=0)THEN
          IF (IMON>0) CALL STARTIME(7,1)
            CALL SECT_IO (
     1        NSTRF,D,DR,V,VR,FSAV(1,1+NINTER+NRWALL+NRBODY),
     2        SECFCUM,A             ,AR    ,SECBUF,MS      ,IN     ,
     3        X      ,FANI(1,NFOA+1),WEIGHT,XSEC  ,IAD_ELEM,FR_ELEM,
     4        RG_CUT ,IAD_CUT       ,FR_CUT,WEIGHT_MD,IOLDSECT,
     5        STABSEN,SFSAV6         ,TABSENSOR ,FBSAV6  )
          IF(IMON>0) CALL STOPTIME(7,1)
        ENDIF

C-----------------------------------------------------
c     adaptive meshing + static condensation : velocities on static nodes.
C-----------------------------------------------------

C========================================================================================
C                                    PARALLEL SECTION (SMP)
C========================================================================================

      IF(ISTATCND/=0)THEN
        IF (IMON>0) CALL STARTIME(38,1)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
          CALL CNDINT(IXC, IPART(K3), IXTG, IPART(K8), IPART,
     2              ITSK   ,A        ,V      ,AR       ,VR    ,
     3              MS     ,IN       ,NODFTSK,NODLTSK  ,X     ,
     4              SH4TREE,SH3TREE ,ITAB  ,STIFN ,STIFR ,
     5              MSCND  ,INCND   )
!$OMP END PARALLEL

        IF (IMON>0) CALL STOPTIME(38,1)
C
C         reimpose bcs.
        IF (IMON>0) CALL STARTIME(4,1)
        IF (IMONM > 0) CALL STARTIME(42,1)

C========================================================================================
C                                          PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

        CALL BCS10(NODFTSK,NODLTSK ,ICODT  ,ICODR,ISKEW,
     2               SKEW ,A      ,AR     ,MS   ,V    ,
     3               VR  )

!$OMP END PARALLEL

        IF (IMONM > 0) CALL STOPTIME(42,1)
        IF (IMON>0) CALL STOPTIME(4,1)
      END IF


C-----------------------------------------------------
C     adaptive meshing : velocities on secnd nodes
C-----------------------------------------------------

      IF(NADMESH/=0)THEN
        IF (IMON>0) CALL STARTIME(38,1)

C========================================================================================
C                                          PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()

          CALL ADMVIT(IXC, IPART(K3), IXTG, IPART(K8), IPART,
     1                ITSK   ,A        , V   , AR       , VR   ,
     2              SH4TREE,SH3TREE ,TEMP )

!$OMP END PARALLEL

          IF (IMON>0) CALL STOPTIME(38,1)
      END IF

C--------------------------------------------------
c     rigid bodies velocities on secondary nodes
C--------------------------------------------------
      IF(NRBYKIN>0)THEN

        IF(IMON>0) CALL STARTIME(4,1)
        IF(IMONM > 0) CALL STARTIME(40,1)

C========================================================================================
C                                          PARALLEL SECTION (SMP)
C========================================================================================

C----------------------------
C  CALCUL //
C      [NRBODY1][NRBODY2]...   (SUR TASK LIBRE)
C // --------------------------

!$OMP PARALLEL

        CALL RBYVIT(
     1    RBY    ,X       ,V        ,VR   ,SKEW ,
     2    FSAV   ,LPBY    ,NPBY     ,ISKEW,ITAB ,
     3    WEIGHT ,A       ,AR       ,MS   ,IN   ,
     4    KINDRBY,IRBKIN_L,NRBYKIN_L,NODREAC,FTHREAC,
     5    FREAC  )

!$OMP END PARALLEL

C--------------------------------------------------
        IF(IMON>0) CALL STOPTIME(4,1)
        IF(IMONM > 0) CALL STOPTIME(40,1)

      ENDIF
C---------------------------------------------------
C  rigid material
C--------------------------------------------------
C
      IF(IRIGID_MAT > 0 ) THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL

        CALL RMATACCE(RBYM , ARBYM, ARRBYM, VRBYM, VRRBYM ,
     1                IRBYM,LNRBYM,X       ,A    ,AR    ,
     2                V    ,VR    ,KINDRBYM)

!$OMP END PARALLEL

      ENDIF

C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================

C--------------------------------------------------
C         Flexible bodies solution and velocities on secondary nodes
C--------------------------------------------------
      IF (NFXBODY>0) THEN
          CALL FXBYVIT(FXBIPM, FXBNOD, FXBMOD, FXBGLM, FXBLM ,
     .                 FXBMVN, FXBMCD, FXBSE , FXBSV , FXBVIT,
     .                 FXBACC, FXBRPM, V     , VR    , A     ,
     .                 AR    , MS    , IN    , WEIGHT, FSAV  ,
     .                 FXBFC , FXBEDP, IAD_ELEM, FR_ELEM)
      END IF
C--------------------
C      RBE3
C--------------------
      IF (NRBE3>0) THEN
        CALL MY_BARRIER
        IF (ITASK==0) THEN
          CALL RBE3V(IRBE3 ,LRBE3 ,X    ,A     ,AR    ,
     1                 V   ,VR    ,FRBE3,SKEW  )
        ENDIF
        CALL MY_BARRIER
      ENDIF
C--------------------
C      RBE2
C--------------------
      IF (NRBE2>0) THEN
        CALL MY_BARRIER
        IF (ITASK==0) THEN
        CALL RBE2V(IRBE2 ,LRBE2 ,X    ,A     ,AR    ,
     1             V     ,VR    ,SKEW )
        ENDIF
        CALL MY_BARRIER
      ENDIF
C--------------------------------------------------
C  DAA  normal accelerations
C--------------------------------------------------
        IF (NFLOW>0) CALL FLOW1(IFLOW, RFLOW, NBGAUGE, A)

C========================================================================================
C                                   DOMAIN 0 
C========================================================================================

      IF (ISPMD==0) THEN
C------------------------
C       INTERFACES 14 & 15
C--- //0 ----------------
        IF(NINTER/=0)THEN
          IF (IMON>0) THEN
              CALL STARTIME(2,1)
          ENDIF
          CALL I14WFS(IPARI,INTBUF_TAB,IGRSURF,FSAV)
          IF (IMON>0) THEN
              CALL STOPTIME(2,1)
          ENDIF
        ENDIF
      ENDIF
C----      
      IF (NS10E>0) CALL S10CNDI2A(ICNDS10 ,ITAGND ,A      )
C------------------------
C         INTERFACES TIED
C--- //0 ----------------

        IF(NINTER/=0)THEN
          IF (IMON>0) THEN
          CALL STARTIME(6,1)
          CALL STARTIME(2,1)
        ENDIF
        IF (IMONM > 0) CALL STARTIME(28,1)
        DO K=NHIN2,0,-1
          CALL INTTI2(IPARI,X    ,V    ,A    ,
     2                VR   ,AR   ,K    ,MS   ,IN   ,WEIGHT,WA,SKEW,
     3                INTBUF_TAB)
        ENDDO
        IF (IMONM > 0) CALL STOPTIME(28,1)
          IF (IMON>0) THEN
            CALL STOPTIME(2,1)
            CALL STOPTIME(6,1)
          ENDIF
        ENDIF
C
C-----------------------------------------------------
C         KINEMATIC CONDITIONS FOR SEATBELTS
C-----------------------------------------------------

        IF (NSLIPRING + NRETRACTOR > 0) CALL KINE_SEATBELT_VEL(A,V,X,XDP)
C
C-----------------------------------------------------
     
        IF (NS10E>0) CALL S10CNDI2A1(ICNDS10 ,ITAGND ,A      )

        IF(NUMFRAM /= 0 .AND. N2D == 0)THEN
C----------------------------
C       MOVING FRAME - RETRIEVE ACCELERATION.
C--- //0 ----------------
         IF (IMON>0)CALL STARTIME(6,1)
         IF (IMONM > 0) CALL STARTIME(49,1)
         CALL MOVFRA1(XFRAME,IFRAME ,X, V ,A ,VR ,AR)
         IF (IMONM > 0) CALL STOPTIME(49,1)
         IF (IMON>0)CALL STOPTIME(6,1)
        ENDIF

       IF (IMON>0) CALL STARTIME(4,1)
       IF (IMONM > 0) CALL STARTIME(40,1)

       IF(NUMSPH/=0.AND.NSPHSOL/=0)THEN
C------------------------
C         Solids impose velocity to SPH
C--- // ----------------
          IF (IMONM > 0) CALL STARTIME(48,1)
          IF (IMONM > 0) CALL STARTIME(89,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()

          CALL SOLTOSPHA(
     1   ITSK    ,V       ,A         ,MS             ,PM         ,
     2   IPART   ,IXS     ,IPART(K1) ,KXSP           ,IPART(K10) ,
     3   IRST    ,SPBUF   ,PARTSAV   ,SOL2SPH ,IPARG       ,
     4   NGROUNC ,IGROUNC ,ELBUF_TAB ,IGEO)

!$OMP END PARALLEL

          IF (IMONM > 0) CALL STOPTIME(89,1)
          IF (IMONM > 0) CALL STOPTIME(48,1)
       ENDIF


C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C--------------------
C       ACCELEROMETRE
C-- // --------------
      IF (NACCELM > 0) THEN
!$OMP PARALLEL DO PRIVATE(K,N,ISK)
      DO K=1,NACCELM
C check that proc is concerned. Otherwise necessary exchange for sensor, th
        IF(IACCP(K)==ISPMD+1)THEN
          N  = LACCELM(1,K)
          IF(N > 0 .AND. N/=2*NUMNODG )THEN
            ISK= LACCELM(3,K)
            CALL ACCEL1(
     .        A(1,N),ACCELM(1,K),ACCELM(2,K),ACCELM(8,K),ACCELM(14,K),
     .        ACCELM(20,K),ACCELM(23,K),SKEW(1,ISK))
          END IF
        END IF
      ENDDO
C implicit barrier on end do
!$OMP END PARALLEL DO

      END IF ! Fin NACCELM > 0

C--------------------
C       SPH Gauges
C-- // --------------
      IF (NBGAUGE > 0) THEN
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(N)
      DO K=1,NBGAUGE
        IF(IGAUP(K)==ISPMD+1)THEN
          N  = LGAUGE(1,K)
C
C SPH Gauges
          IF(N < -NUMELS)THEN
            CALL SPGAUGE_F(
     .        GAUGE(10,K),GAUGE(9,K),GAUGE(14,K),GAUGE(22,K),
     .        GAUGE(30,K),4)
          END IF
        END IF
      ENDDO
C implicit barrier on end do
!$OMP END PARALLEL DO

      END IF ! Fin NBGAUGE > 0

C--------------------
C       SPH Flow
C-- // --------------
      IF (NSPHIO > 0) THEN
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(IVAD)
      DO K=1,NSPHIO
        IF (ISPHIO(1,K)>1) THEN
            IVAD = ISPHIO(4,K)
            CALL SPGAUGE_F(
     .        VSPHIO(IVAD+13),VSPHIO(IVAD+15),VSPHIO(IVAD+18),VSPHIO(IVAD+20),
     .        VSPHIO(IVAD+16),1)
        END IF
      ENDDO
C implicit barrier on end do
!$OMP END PARALLEL DO
      END IF

!$OMP PARALLEL
      IF(NRBYKIN>0)THEN
            CALL RBYCOR(
     1        RBY ,X   ,V    ,VR     ,SKEW    ,FSAV     ,
     2        LPBY,NPBY,ISKEW,ITAB   ,WEIGHT  ,A        ,
     3        AR  ,MS  ,IN   ,KINDRBY,IRBKIN_L,NRBYKIN_L,
     4        WEIGHT_MD,MS_2D)
      ENDIF
C---------RBE2      
      IF(NRBE2>0)THEN
C                                     
            CALL RBE2COR(IRBE2 ,LRBE2 ,X    ,V     ,VR    ,
     2                   SKEW  ,ISKEW ,ITAB ,WEIGHT,A     ,
     3                   AR    ,MS0   ,IN   ,WEIGHT_MD)
      ENDIF
!$OMP END PARALLEL
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

C------------------------------------------
C       SYNCHRONISATION:  A  V  (POUR SORTIES)
C------------------------------------------
C       SENSORS 
C-- // --------------
C-----------------------------------------------
C   Exchange FBSAV6 if sensor 6/12 activated 
C-----------------------------------------------
      IF (NSENSOR> 0 ) THEN
         IPARSENS=0
         IF(STABSEN/=0) THEN
            IPARSENS=1
         ENDIF
         IF (NSPMD > 1 .AND. IPARSENS /= 0) THEN
           DIM6=12
           DIM_EXCH=SFSAV6
           CALL SPMD_EXSUM_FB6(DIM6,DIM_EXCH,FBSAV6)
         ENDIF
      ENDIF
c
      IF (NSENSOR > 0) THEN

        CALL SENSOR_ENER_SAV(SENSOR_TAB,PARTSAV ,PARTSAV2)

        ! pre-computation and mpi communication for type 16 sensor
        IF (COMM_SENS16%BOOL) THEN
          CALL SENSOR_DIST_SURF0(SENSOR_TAB,X,IGRSURF)
        ENDIF

        ! pre-computation and mpi communication for type 17 sensor
        IF (COMM_SENS17%BOOL) THEN
          CALL SENSOR_TEMP0(SENSOR_TAB,IGRNOD,TEMP,WEIGHT)
        ENDIF

        IF (NSPMD > 1) THEN
          CALL SENSOR_SPMD(SENSOR_TAB,IPARI   ,NPRW     ,ISENSP    ,NSENSP    ,
     .                 XSENS     ,X       ,ACCELM   ,IACCP     ,NACCP     ,
     .                 GAUGE     ,IGAUP   ,NGAUP    ,PARTSAV2  ) 
        ENDIF
c       
        ! check activation condition of base sensors
        CALL SENSOR_BASE(SENSOR_TAB,
     .       XSENS     ,IPARSENS  ,TABSENSOR ,STABSEN   ,IPARI     ,
     .       SFSAV6    ,FSAV      ,FBSAV6    ,PARTSAV2  ,GAUGE     ,
     .       X         ,V         ,A         ,ACCELM    ,NPRW      ,
     .       SUBSETS   ,IGRSURF   ,IGRNOD    )
c       
        ! check activation condition of logical sensor hierarchy
        CALL SENSOR_LOGICAL(SENSOR_TAB)

      ENDIF  !  NSENSOR > 0
C========================================================================================

      IF (IMONM > 0) CALL STOPTIME(40,1)
      IF (IMON>0) CALL STOPTIME(4,1)
C-------------------------------------------------------
C     Conditions cinematiques par Multiplicateurs de Lagrange
C-------------------------------------------------------

      IF(LAG_NCF+LAG_NCL>0)THEN
        L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
        L2 = L1+6*NUMELS10
        L3 = L2+12*NUMELS20
C cas NSPMD == 1 et options supportees uniquement en SMP
        IF(LAG_SEC == 1 .AND. NSPMD == 1)THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,GREFTSK,GRELTSK)

C Init var parallel SMP
          CALL SMP_INIT(
     1      ITSK  ,NODFTSK  ,NODLTSK  ,NUMNTSK,NDTSK   ,
     2      IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK  ,
     3      GREFTSK,GRELTSK)

          CALL LAG_MULT(
     1       IPARI    ,X        ,A        ,
     2       WA(NWAFTSK),V        ,MS       ,IN       ,VR       ,
     3       ITSK       ,WA       ,ITAB     ,IXS      ,IXS(L2)  ,
     4       IXS(L3)  ,IGRNOD     ,FANI     ,FSAV     ,
     5       SKEW     ,AR       ,LAMBDA   ,LAGBUF   ,IBCSLAG  ,
     6       IXS(L1)  ,GJBUFI   ,GJBUFR   ,IBMPC    ,RBMPC    ,
     7       NPBYL    ,LPBYL    ,IBFV     ,VEL      ,NPC      ,
     8       TF         ,NEWFRONT ,ICONTACT ,RWBUF    ,LPRW     ,
     9       NPRW     ,RBYL     ,D        ,DR       ,KINET    ,
     A       SENSOR_TAB,INTBUF_TAB, H3D_DATA ,IGRBRIC)

!$OMP END PARALLEL

C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

        ELSE     
         IF(ISPMD==0) THEN
            NBNCL  = FR_LAGF(1,NSPMD+1)
            NBIKL  = FR_LAGF(2,NSPMD+1)
            NBNODL = FR_LAGF(3,NSPMD+1)
            NBNODLR= NBNODL*MAX(1,IRODDL)
          ELSE
            NBNCL = FR_LAGF(1,ISPMD+1)
            NBIKL = FR_LAGF(2,ISPMD+1)
            NBNODL=0
            NBNODLR=0
          END IF
          CALL LAG_MULTP(
     1       IPARI    ,X        ,A        ,
     2       WA       ,V        ,MS       ,IN       ,VR       ,
     3                 WA       ,ITAB     ,IXS      ,IXS(L2)  ,
     4       IXS(L3)  ,FANI     ,FSAV     ,
     5       SKEW     ,AR       ,LAMBDA   ,LAGBUF   ,IBCSLAG  ,
     6       IXS(L1)  ,GJBUFI   ,GJBUFR   ,IBMPC    ,RBMPC    ,
     7       NPBYL    ,LPBYL    ,IBFV     ,VEL      ,NPC      ,
     8       TF       ,NEWFRONT ,ICONTACT ,RWBUF    ,LPRW     ,
     9       NPRW     ,RBYL     ,D        ,DR       ,KINET    ,
     A       NODGLOB  ,WEIGHT   ,NBNCL    ,NBIKL    ,NBNODL   ,
     B       NBNODLR  ,FR_LAGF  ,LLAGF    ,IAD_ELEM ,FR_ELEM  ,
     C       INTBUF_TAB ,H3D_DATA)
        END IF
      ENDIF
c--------------
 111  CONTINUE
c--------------

      NTMP = IMCONV
      IF (IMPDEB==1.AND.IMCONV==0) THEN
       IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) THEN
        TTMP=TTMP+DTANIM
        IF ((IRAD2R==1).AND.(IRESP==1)) THEN
          TT_DP=TT_DP+TTMP
          TT = TT_DP
        ELSE
          ! double precision accumlation and then cast in simple precision
          TT_DOUBLE = TT_DOUBLE + TTMP
          IF (IMPL_S==1.OR.NEIG>0) THEN
            TT = TT + TTMP
          ELSE
            IF(IRESP == 1)THEN 
                TT = SNGL(TT_DOUBLE)
            ELSE
                TT = TT_DOUBLE
            ENDIF
          ENDIF
          !TT=TT+TTMP
        ENDIF
        NTMP = 1
       ENDIF
      ENDIF
C--------------------------------------------------
      IF( ANIM_CE(2156)/=0
     .     .AND.((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.
     .          (MANIM>=4.AND.MANIM<=15))) THEN

C========================================================================================
C                                         PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

        CALL ERR_THK(IXC     ,IXTG    ,IPARG   ,IAD_ELEM,FR_ELEM    ,
     .               WEIGHT  ,X       ,ELBUF_TAB,IPART   ,IPART(K3)  ,
     .               IPART(K8)  ,ITSK   ,NODFTSK ,NODLTSK ,ERR_THK_SH4,
     .               ERR_THK_SH3,SH4TREE,SH3TREE)

!$OMP END PARALLEL

      END IF

C
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
      IF(NTMP==1)THEN
       IF(IDTMINS==1)THEN
C
C        Obsolete
       ELSEIF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
C--------------------------------------------------
        IF (IMON>0) CALL STARTIME(39,1)

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

        CALL SMS_ENCIN_2(
     1        ITSK     ,NODFTSK  ,NODLTSK  ,NODXI_SMS ,
     2        MS       ,JAD_SMS  ,JDI_SMS  ,LT_SMS    ,INDX1_SMS,
     3        DIAG_SMS ,IAD_ELEM ,FR_ELEM  ,WEIGHT    ,V        ,
     4        A        ,X_SMS    ,Y_SMS    ,Z_SMS     ,XMOM_SMS ,
     5        ICODT    ,ICODR    ,ISKEW    ,SKEW      ,IBFV      ,
     6        VEL      ,NPC      ,TF       ,X         ,D         ,
     7        SENSOR_TAB,IFRAME   ,XFRAME    ,JADI_SMS ,
     8        JDII_SMS ,LTI_SMS  ,ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,
     9        FR_RMS   ,NPBY     ,TAGSLV_RBY_SMS,INTSTAMP,CPTREAC,
     A        NODREAC  ,FTHREAC  ,AR        ,VR        ,
     B        DR       ,IN       ,RBY      ,IRBE2     ,LRBE2     ,
     C        IAD_RBE2 ,FR_RBE2M ,NMRBE2   ,R2SIZE    ,IRBE3     ,
     D        LRBE3    ,FRBE3    ,IAD_RBE3M,FR_RBE3M  ,FR_RBE3MP ,
     E        RRBE3    ,RRBE3_PON,IAD_RBY  ,FR_RBY6   ,RBY6      ,
     F        LPBY     ,TAGMSR_RBY_SMS,R3SIZE,NODII_SMS,INDX2_SMS,
     G        IBCSCYC  ,LBCSCYC )

!$OMP END PARALLEL

        IF (IMON>0) CALL STOPTIME(39,1)

       END IF
      END IF
C---
      NISKY_SMS=0     
      
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

       IF (ICRACK3D > 0 .AND. NLEVSET > 0) THEN
         CALL XFEOFF(XFEM_TAB ,
     .               IPARG  ,IXC      ,NGROUC      ,IGROUC  ,IEL_CRK    ,
     .               ELCUTC ,IXTG     ,IADC_CRK    ,IAD_ELEM,IAD_EDGE   ,
     .               FR_EDGE,FR_NBEDGE,FR_ELEM     ,NXLAYMAX,INOD_CRK   ,
     .               CRKEDGE,XEDGE4N  ,XEDGE3N    )
       ENDIF
       
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

C-----------------------------
C FINITE VOLUME METHOD FOR ALE
C-----------------------------

      IF(IALEFVM>0)THEN
!$OMP PARALLEL
!$OMP+ PRIVATE(ITSK,NODFTSK,NODLTSK,NUMNTSK,NDTSK,IPMTSK,IGMTSK)
!$OMP+ PRIVATE(PARTFTSK,PARTLTSK,NWAFTSK,DT2TT,NELTSTT,ITYPTSTT)
!$OMP+ PRIVATE(GREFTSK,GRELTSK)

C Init var parallel SMP
        CALL SMP_INIT(
     1   ITSK   , NODFTSK , NODLTSK , NUMNTSK, NDTSK   ,
     2   IPMTSK , PARTFTSK, PARTLTSK, NWAFTSK, IGMTSK  ,
     3   GREFTSK, GRELTSK)
        DT2TT    = DT2T
        NELTSTT  = NELTST
        ITYPTSTT = ITYPTST
C
        CALL ALEFVM_MAIN(
     1   PM         ,GEO      ,X          ,A(1,NDTSK)   ,AR(1,NDTSK)    ,V         ,
     2   MS         ,WA       ,ELBUF_TAB  ,BUFMAT       ,PARTSAV(IPMTSK),TF        ,
     3   VEUL       ,FV       ,FSKY       ,VR           ,
     4   SKEW       ,W        ,
     5   D          ,DT2TT    ,DT2SAVE    ,ALE_CONNECTIVITY        ,
     6   IPARG      ,NPC      ,IXS        ,IXQ          ,IADS           ,IADS(I87B),
     7   ALE_CONNECTIVITY%NALE       ,ISKEW    ,
     8   NELTSTT    ,ITYPTSTT ,IPART      ,IPART(K1)    ,IPART(K2)      ,ITSK      ,
     A   NODFTSK    ,NODLTSK  ,NBRCVOIS   ,
     B   NBSDVOIS   ,LNRCVOIS ,LNSDVOIS   ,NERCVOIS     ,NESDVOIS       ,LERCVOIS  ,
     C   LESDVOIS   ,ISIZXV   ,IAD_ELEM   ,FR_ELEM      ,FSKYM          ,MSNF      ,
     D   IPARI      ,SEGVAR   ,ITAB       ,ISKWN        ,
     E   FSAV     ,
     F   WEIGHT     ,NPSEGCOM ,LSEGCOM    ,IPM          ,IGEO           ,
     G   ITABM1   ,NV46       )
  
!$OMP END PARALLEL
      ENDIF
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (IMON>0) CALL STARTIME(7,1)

      IF(COMPTREAC/=0.AND.(IMPL_S==0 .OR. INCONV==1)) THEN

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C--------------------------------------------------
C       FINALIZE REACTION FORCES Freac = MS*A - Freac     
C--------------------------------------------------
        CALL FINALIZE_REACTIONS(NODFTSK,NODLTSK,A,AR,MS,IN,FREAC)

!$OMP END PARALLEL
      END IF

      IF (IMON>0) CALL STOPTIME(7,1)
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================
      IF(NCYCLE > 0) THEN

        IF (IMON>0) CALL STARTIME(MACRO_TIMER_GENH3D1,1)
        L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
        L2 = L1+6*NUMELS10
        L3 = L2+12*NUMELS20
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
c
        ITSK = OMP_GET_THREAD_NUM()
C--------------------------------------------------
C       /H3D/TMAX    
C--------------------------------------------------
        CALL UPD_TMAX(ELBUF_TAB,IPARG   ,GEO     ,PM   ,
     .  	       IXS	,IXS(L1) ,IXS(L3) ,IXS(L2) ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,D       ,V       ,IAD_ELEM,FR_ELEM ,
     .             WEIGHT ,IPM   ,IGEO    ,STACK   ,ITSK    )  

!$OMP END PARALLEL
        IF (IMON>0) CALL STOPTIME(MACRO_TIMER_GENH3D1,1)
      END IF
      
C========================================================================================
C                                    NON PARALLEL SECTION (SMP)
C========================================================================================
C--------------------------------------------------
      IF(NTMP==1)THEN
C--------------------------------------------------
        IF (IMON>0) THEN
          CALL STARTIME(7,1)
        ENDIF
        CALL TRACE_IN(5,0,ZERO)
        L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
        L2 = L1+6*NUMELS10
        L3 = L2+12*NUMELS20
C Do not stop yet 
        IF (TT<=TSTOP.AND.ILASTANIM==3) THEN
          ILASTANIM=0
        END IF
        IF (TT<=TSTOP.AND.ILASTDYNAIN==3) THEN
          ILASTDYNAIN=0
        END IF
        IF (TT<=TSTOP.AND.ILASTH3D==3) THEN
          ILASTH3D=0
        END IF
C Regular animation                                               
        IF ((TT>TANIM .AND. TT<=TANIM_STOP).AND.ILASTANIM==0) THEN
          ILASTANIM=3
          LASTANIMCYCLE=NCYCLE
        END IF

        IF (TT>TSTAT.AND.ILASTANIM==0) THEN
          ILASTANIM=3
          LASTSTATCYCLE=NCYCLE
        END IF
        IF (TT>TDYNAIN.AND.ILASTDYNAIN==0) THEN
          ILASTDYNAIN=3
          LASTDYNCYCLE=NCYCLE
        END IF
        IF (TT>H3D_DATA%TH3D.AND.ILASTH3D==0) THEN
          ILASTH3D=3
          LASTH3DCYCLE=NCYCLE
        END IF
        
        IF(DEBUG(MACRO_DEBUG_CHKSM) >0) THEN
          IF(MOD(NCYCLE,DEBUG(MACRO_DEBUG_CHKSM)) == 0  ) THEN
        ! Print checksum
            CALL SPMD_FLUSH_ACCEL(A,ITAB,WEIGHT,NODGLOB,NUMNODG)
          ENDIF
        ENDIF

        CALL SORTIE_MAIN(
     1   PM            ,D              ,V              ,ALE_CONNECTIVITY          ,W             ,
     2   ELBUF         ,IPARG          ,IXS            ,IXQ            ,IXC           ,
     3   IXT           ,IXP            ,IXR            ,IXTG           ,WA            ,
     4   ITAB          ,X_A            ,GEO            ,MS             ,A             ,
     5   FANI          ,PARTSAV        ,ICUT           ,XCUT           ,
     6   FANI(1,1+NFIA),FANI(1,1+NFEA) ,FANI(1,1+NFOA) ,ANIN           ,LPBY          ,
     7   NPBY          ,NSTRF          ,RWBUF          ,NPRW           ,IXUR          ,
     8   TANI          ,INOISE         ,BUFNOIS        ,RBY            ,NEFLSW        ,
     9   NNFLSW        ,CRFLSW         ,FLSW           ,LOUT           ,
     B   FSAV          ,SKEW           ,ELBUF_TAB      ,CLUSTER        ,
     C   VR            ,IN             ,WEIGHT         ,FCLUSTER       ,MCLUSTER      ,
     D   DD_IAD        ,DMAS           ,DINER          ,ACCELM         ,GAUGE         ,
     E   IPARI         ,EANI           ,IPART          ,
     F   IGRNOD        ,SUBSETS        ,ITHGRP         ,ITHBUF         ,
     G   NOM_OPT       ,AR             ,IGRSURF        ,BUFSF          ,IDATA         ,
     H   RDATA         ,KXX            ,IXX            ,BUFMAT         ,BUFGEO        ,
     I   KXSP          ,IXSP           ,NOD2SP         ,SPBUF          ,DR,
     J   FSAVD          ,
     L   LRIVET        ,RIVET          ,ISKWN          ,IFRAME         ,XFRAME        ,
     M   IXS(L1)       ,IXS(L2)        ,IXS(L3),
     N   ITHGRPA       ,ITHBUFA,
     O   ITHGRPB       ,ITHBUFB,
     P   ITHGRPC       ,ITHBUFC,
     .   ITHGRPD       ,ITHBUFD,
     .   ITHGRPE       ,ITHBUFE,
     .   ITHGRPF       ,ITHBUFF,
     .   ITHGRPG       ,ITHBUFG,
     .   ITHGRPH       ,ITHBUFH,
     .   ITHGRPI       ,ITHBUFI        ,NDMA           ,MONVOL         ,VOLMON        ,
     .   IPM           ,IGEO           ,NODGLOB        ,IAD_ELEM       ,FR_ELEM       ,
     U   FR_RBY2       ,IAD_RBY2       ,FR_WALL        ,FR_SEC         ,FXBIPM        ,FXBRPM   ,
     V   NDIN          ,FXBDEP         ,FXBVIT         ,FXBACC         ,IFLOW         ,
     W   RFLOW         ,IPARTL         ,NPARTL         ,IACCP          ,NACCP         ,
     X   FASOLFR       ,FANI(1,NFNCA+1),FANI(1,NFTCA+1),IPART(I15ATH)  ,
     Y   FR_MV         ,IPART_STATE    ,SH4TREE        ,SH3TREE,TEMP   ,
     Z   THKE          ,ERR_THK_SH4    ,ERR_THK_SH3,
     a   INOD_PXFEM    ,FTHREAC        ,NODREAC        ,CPTREAC        ,GRESAV,
     b   DIAG_SMS      ,SH4TRIM        ,SH3TRIM        ,FANI(1,1+NFT2) ,XMOM_SMS      ,
     c   IRBE2         ,IRBE3          ,LRBE2          ,LRBE3          ,FR_RBE2       ,
     d   FR_RBE3M      ,IAD_RBE2       ,DXANCG         ,IEL_PXFEM      ,ZI_PLY        ,
     e   VFLOW         ,FCONTG         ,FNCONTG        ,FTCONTG        ,FREAC         ,
     f   INOD_CRK      ,IEL_CRK        ,ELCUTC         ,IADC_CRK       ,ANIN(NDAMA2+1),
     g   RES_SMS       ,SENSOR_TAB     ,
     h   QFRICINT      ,IGAUP          ,NGAUP          ,WEIGHT_MD      ,NCONT         ,
     i   INDEXCONT     ,NODGLOBXFE     ,NODEDGE        ,XFEM_TAB       ,
     j   NV46          ,RTHBUF         ,KXIG3D         ,IXIG3D         ,KNOT          ,
     k   WIGE          ,NERCVOIS       ,NESDVOIS       ,LERCVOIS       ,LESDVOIS      , 
     l   CRKEDGE       ,STACK          ,ISPHIO         ,VSPHIO         ,ICODE         ,
     m   INDX_CRK      ,XEDGE4N        ,XEDGE3N        ,SPH2SOL        ,STIFN_TMP     ,
     n   STIFR_TMP     ,DRAPE_Q4       ,DRAPE_T3       ,MS_2D          ,MULTI_FVM     ,
     o   SEGQUADFR     ,H3D_DATA       ,ISKEW          ,PSKIDS         ,ISKWP         ,
     p   KNOTLOCPC     ,KNOTLOCEL      ,PINCH_DATA     ,TAG_SKINS6     ,IRUNN_BIS     ,
     q   TF            ,NPC            ,IPART_DYNAIN   ,FCONT_MAX      ,MDS_MATID     ,
     r   FANI(1,NFNCA2+1),FANI(1,NFTCA2+1),IBCL        ,ILOADP         ,LLOADP        ,
     s   LOADP         ,TAGNCONT       ,LOADP_HYD_INTER,FORC           ) 
C
        IF((MSTOP == 1 .AND. ICTLSTOP == 0) .OR. MSTOP == 2 .OR. DT2<=ZERO)THEN
          CALL SORTIE_ERROR(
     1     V        ,NODGLOB    ,WEIGHT     ,ITAB       ,MS          ,
     2     MS0      ,10         ,PARTSAV    ,IPART      ,PM          ,
     3     IGEO     )
        END IF
C
        CALL TRACE_OUT(5)
C-------ADYREL----
       IF (ISTAT==3) CALL ENER_W0
C------------
C       TEMPS
C------------
C     ILASTANIM=0 No additional animation
C     ILASTANIM=1 One more cycle needed                           
C     ILASTANIM=2 Additional cycle done                                
C     ILASTANIM=3 regular animation                           
        IF (ILASTANIM==1) THEN
          ILASTANIM=2
        END IF
        IF (ILASTH3D==1) THEN
          ILASTH3D=2
        END IF
        IF(T1S==TT)NCYCLE=NCYCLE+1
C-------> Multidomain single precision:  double precision time scale----
        IF ((IRAD2R==1).AND.(IRESP==1)) THEN
          TT_DP=TT_DP+DT2
          TT=TT_DP
        ELSE
          ! double precision accumlation and then cast in simple precision
          TT_DOUBLE = TT_DOUBLE + DT2
          IF (IMPL_S==1.OR.NEIG>0) THEN
            TT = TT + DT2
          ELSE
            IF(IRESP == 1)THEN 
                TT = SNGL(TT_DOUBLE)
            ELSE
                TT = TT_DOUBLE
            ENDIF
          ENDIF
        ENDIF
C May do 1 more cycle to write animation if TT = TTANIM - 10%
        IF (IMADCPL /= 1) THEN
         IF (DTANIM>ZERO) THEN
          IF (TT>TSTOP.AND.ILASTANIM==0
     .        .AND.ABS((TT-TANIM)/DTANIM)<EM03) THEN
            ILASTANIM=1
            TANIM=TT-EM10
          END IF
          IF (TT>TSTOP.AND.ILASTANIM==0.AND.
     .        (NCYCLE-LASTANIMCYCLE)>=100) THEN
            ILASTANIM=1
            TANIM=TT-EM10
          END IF
         END IF
c
         IF (DTSTAT>ZERO) THEN
          IF (TT>TSTOP.AND.ILASTANIM==0
     .        .AND.ABS((TT-TSTAT)/DTSTAT)<EM03) THEN
            ILASTANIM=1
            TSTAT=TT-EM10
          END IF
          IF (TT>TSTOP.AND.ILASTANIM==0.AND.
     .        (NCYCLE-LASTSTATCYCLE)>=100) THEN
            ILASTANIM=1
            TSTAT=TT-EM10
          END IF
         END IF
c
         IF (DTDYNAIN>ZERO) THEN
          IF (TT>TSTOP.AND.ILASTDYNAIN==0
     .        .AND.ABS((TT-TDYNAIN)/DTDYNAIN)<EM03) THEN
            ILASTDYNAIN=1
            TDYNAIN=TT-EM10
          END IF
          IF (TT>TSTOP.AND.ILASTDYNAIN==0.AND.
     .        (NCYCLE-LASTDYNCYCLE)>=100) THEN
            ILASTDYNAIN=1
            TDYNAIN=TT-EM10
          END IF
         END IF
c
         IF (H3D_DATA%DTH3D>ZERO) THEN
          IF (TT>TSTOP.AND.ILASTH3D==0
     .        .AND.ABS((TT-H3D_DATA%TH3D)/H3D_DATA%DTH3D)<EM03) THEN
            ILASTH3D=1
            H3D_DATA%TH3D=TT-EM10
          END IF
          IF (TT>TSTOP.AND.ILASTH3D==0.AND.
     .        (NCYCLE-LASTH3DCYCLE)>=100) THEN
            ILASTH3D=1
            H3D_DATA%TH3D=TT-EM10
          END IF
         END IF
        ENDIF
C------------
        IF (IMON>0) THEN
          CALL STOPTIME(7,1)
        ENDIF
      ENDIF
      IF (IMPDEB==1.AND.IMCONV==0) THEN
       IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) THEN
        IF ((IRAD2R==1).AND.(IRESP==1)) THEN
          TT_DP=TT_DP-TTMP-DT2
          TT=TT_DP
         ELSE
          ! double precision accumlation and then cast in simple precision
          TT_DOUBLE = TT_DOUBLE - TTMP - DT2
          IF (IMPL_S==1.OR.NEIG>0) THEN
            TT = TT - TTMP - DT2
          ELSE
            IF(IRESP == 1)THEN 
                TT = SNGL(TT_DOUBLE)
            ELSE
                TT = TT_DOUBLE
            ENDIF
          ENDIF
          !TT=TT-TTMP-DT2
        ENDIF
       ENDIF
      ENDIF
C----------------------------------
C       IMPLICITE SOLVER
C----------------------------------
      IF (IMPL_S==1) THEN
       CALL TRACE_IN(3,NCYCLE,ZERO)
       IF (IMON>0) CALL STARTIME(34,1)
       
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

      IF (IMP_CHK > 0) THEN
#if defined(MUMPS5) 
         CALL IMP_CHKM(
     1  ICODE  ,ISKEW  ,ISKWN  ,IPART  ,IXUR   ,IXTG   ,IXS    ,IXQ    ,
     2  IXC    ,IXT    ,IXP    ,IXR    ,IXTG1          ,ITAB   ,ITABM1 ,
     3  NPC    ,IBCL   ,IBFV   ,SENSOR_TAB,NNLINK ,LNLINK ,IPARG  ,IGRV   ,
     4  IPARI  ,INTBUF_TAB,NPRW   ,ICONX  ,NPBY,LPBY   ,LRIVET ,
     5  NSTRF  ,LJOINT ,ICODT  ,ICODR  ,ISKY  ,ADSKY,IADS(I87J),
     6  ILINK  ,LLINK  ,WEIGHT         ,ITASK ,IBVEL  ,LBVEL  ,FBVEL  ,
     7  X      ,D      ,V      ,VR     ,DR     ,THKE   ,DAMP   ,MS     ,
     8  IN     ,PM     ,SKEW   ,GEO    ,EANI   ,BUFMAT ,BUFGEO ,BUFSF  ,
     9  TF     ,FORC   ,VEL    ,FSAV   ,AGRV   ,FR_WAVE,PARTS0 ,
     A  USER   ,ELBUF  ,RBY   ,RIVET,FR_ELEM,IAD_ELEM,
     B  WA     ,     A      ,AR     ,STIFN  ,STIFR  ,PARTSAV,FSKY   ,
     C  FSKYI  ,IFRAME ,XFRAME ,W16    ,IACTIV ,FSKYM  ,IGEO   ,IPM    ,
     D  TFEXT  ,NODFT  ,NODLT  ,NT_IMP ,NUM_IMP,NS_IMP ,NE_IMP ,IND_IMP,
     L  IT     ,RWBUF  ,LPRW    ,FR_WALL,NBINTC ,INTLIST,
     M  FANI(1,1+NFOA+2*(NSECT+NRBODY)),RWSAV  ,FSAVD ,
     N  DIRUL  ,LGRAV  ,IRBE3  ,LRBE3  ,FRBE3  ,
     O  FRWL6  ,IRBE2  ,LRBE2 ,ICFIELD,LCFIELD,CFIELD,ELBUF_TAB,
     P  WEIGHT_MD, STACK,SFSAV6  ,FBSAV6 ,STABSEN,TABSENSOR,DRAPE_Q4  , 
     Q  DRAPE_T3,H3D_DATA ,NDDL0,NNZK0,IMPBUF_TAB,CPTREAC,FTHREAC,NODREAC)
         MSTOP=2
#else
        WRITE(6,*) __LINE__,"Fatal error: MUMPS required"
        CALL FLUSH(6)
        CALL ARRET(5)
#endif
       ELSEIF ((TT<=TSTOP.OR.(TT-TSTOP)<EM10).AND.IBUCK==0) THEN
C-----integer : 1:IKC,2:IKUD,3:W_DDL,4:IADM,5:JDIM,6:NDOFI,7:IDDLI
C-----reel : 1,2,3,4:DIAG_K,LT_K,DIAG_M,LT_M,5,6:LB,DB,7:BKUD,8,9:D_IMP,DR_IMP
C----       10,11,12:ELBUF_C,BUFMAT_C,X_C,13,14:DD,DDR,15,16:X_ac,V_zero,23,24:AC,ACR
#if defined(MUMPS5) 
        CALL IMP_SOLV(
     1  ICODE  ,ISKEW  ,ISKWN  ,IPART  ,IXUR   ,IXTG   ,IXS    ,IXQ    ,
     2  IXC    ,IXT    ,IXP    ,IXR    ,IXTG1          ,ITAB   ,ITABM1 ,
     3  NPC    ,IBCL   ,IBFV   ,SENSOR_TAB,NNLINK ,LNLINK ,IPARG  ,IGRV   ,
     4  IPARI  ,INTBUF_TAB,NPRW   ,ICONX  ,NPBY,LPBY   ,LRIVET ,
     5  NSTRF  ,LJOINT ,ICODT  ,ICODR  ,ISKY  ,ADSKY,IADS(I87J),
     6  ILINK  ,LLINK  ,WEIGHT         ,ITASK ,IBVEL  ,LBVEL  ,FBVEL  ,
     7  X      ,D      ,V      ,VR     ,DR     ,THKE   ,DAMP   ,MS     ,
     8  IN     ,PM     ,SKEW   ,GEO    ,EANI   ,BUFMAT ,BUFGEO ,BUFSF  ,
     9  TF     ,FORC   ,VEL    ,FSAV   ,AGRV   ,FR_WAVE,PARTS0 ,
     A  USER   ,ELBUF  ,RBY   ,RIVET,FR_ELEM,IAD_ELEM,
     B  WA     ,A      ,AR     ,STIFN  ,STIFR  ,PARTSAV,FSKY   ,
     C  FSKYI  ,IFRAME ,XFRAME ,W16    ,IACTIV ,FSKYM  ,IGEO   ,IPM    ,
     D  TFEXT  ,NODFT  ,NODLT  ,NT_IMP ,NUM_IMP,NS_IMP ,NE_IMP ,IND_IMP,
     L  IT     ,RWBUF  ,LPRW   ,FR_WALL,NBINTC ,INTLIST,
     M  FANI(1,1+NFOA+2*(NSECT+NRBODY)),RWSAV  ,FSAVD ,
     N  GRAPHE , FAC_K  ,IPIV_K ,NKCOND,
     O  MONVOL ,IGRSURF,FR_MV  ,VOLMON ,DIRUL,
     P  NODGLOB,MUMPS_PAR,CDDLP ,ISENDTO,IRCVFROM,NEWFRONT,IMSCH  ,
     Q  I2MSCH ,ISIZXV,ILENXV ,ISLEN7  ,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17,
     R  IRLEN17,IRLEN7T,ISLEN7T,KINET  ,NUM_IMP1,TEMP  ,DT2PREV,WA     ,
     S  LGRAV  ,SH4TREE,SH3TREE,IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,
     T  IRLEN20E,ISLEN20E,IRBE3,LRBE3  ,FRBE3  ,FR_I2M,IAD_I2M,FR_RBE3M,
     U  IAD_RBE3M,FRWL6,IRBE2  ,LRBE2,INTBUF_TAB_CP,
     W  IKINE  ,DIAG_SMS,ICFIELD,LCFIELD,CFIELD,COUNT_REMSLV,
     X  COUNT_REMSLVE,ELBUF_TAB,ELBUF_IMP,XDP,WEIGHT_MD , STACK  ,        
     Y  SFSAV6  ,FBSAV6 ,STABSEN,TABSENSOR,DRAPE_Q4  , DRAPE_T3,
     Z  H3D_DATA,MULTI_FVM,IGRBRIC,IGRSH4N,IGRSH3N,IGRBEAM,FORNEQS,MAXDGAP,
     A  NDDL0   ,NNZK0 ,IT_T   ,IMPBUF_TAB,CPTREAC,FTHREAC,NODREAC)
#else 
        WRITE(6,*) __LINE__,"Fatal error: MUMPS required"
        CALL FLUSH(6)
        CALL ARRET(5)
#endif
C Modes buckling 
        IF (NBUCK>0) IBUCK=1
      ELSEIF (IBUCK>0) THEN
C
#if defined(MUMPS5) && defined(DNC)
         CALL IMP_BUCK(
     2 PM,             GEO,             IPM,       IGEO,           ELBUF,
     3 IXS,            IXQ,             IXC,       IXT,            IXP,
     4 IXR,            IXTG,            IXTG1,     IXUR,           IPARG,
     5 TF,             NPC,             FR_WAVE,   W16,            BUFMAT,
     6 THKE,           BUFGEO,          SENSOR_TAB,RBY,
     7 SKEW,           WA,                ICODT,   ICODR,         ISKEW,          
     9 IBFV,           VEL,             LPBY,      NPBY,           ITAB,
     A WEIGHT,         MS,              IN,        IPARI,     INTBUF_TAB,
     B X_A ,           ITSK ,    
     E FANI,           ICUT,            XCUT,      FANI(1,1+NFIA), FANI(1,1+NFEA),
     F FANI(1,1+NFOA), ANIN,            NSTRF,     RWBUF,          NPRW,
     G TANI,           DD_IAD,          EANI,      IPART,
     H NOM_OPT,        IGRSURF,         BUFSF,     IDATA,
     I RDATA,          KXX,             IXX,       KXSP,           IXSP,
     J NOD2SP,         SPBUF,           IXS(L1),   IXS(L2),        IXS(L3),
     K VR,             MONVOL,          VOLMON,    NODGLOB,        IAD_ELEM,
     L FR_ELEM,        FR_SEC,          FR_RBY2,   IAD_RBY2,       FR_WALL,
     M V,              A,               GRAPHE,    PARTSAV ,       XFRAME  ,
     N DIRUL,           
     O FSAV(1,NFNCA+1), FSAV(1,NFTCA+1),TEMP      ,SH4TREE,        SH3TREE,
     P ERR_THK_SH4,    ERR_THK_SH3    , IFRAME    ,LPRW      ,     ELBUF_TAB,
     Q FSAV           ,FSAVD          , RWSAV     ,AR        ,     IRBE3   ,
     R LRBE3          ,FRBE3          , FR_I2M    ,IAD_I2M   ,     FR_RBE3M,
     S IAD_RBE3M      ,FRWL6          , IBCL      ,FORC      ,     IRBE2   ,
     T LRBE2          ,IAD_RBE2       , FR_RBE2   ,WEIGHT_MD,
     U CLUSTER        ,FCLUSTER       , MCLUSTER  ,XFEM_TAB  ,
     V ALE_CONNECTIVITY    ,W              , NV46      ,NERCVOIS  ,     NESDVOIS ,
     W LERCVOIS       ,LESDVOIS       ,CRKEDGE    ,STACK     ,     SFSAV6   ,
     X FBSAV6         ,STABSEN        ,TABSENSOR  ,INDX_CRK  ,XEDGE4N       ,
     Y XEDGE3N        ,SPH2SOL        ,STIFN_TMP  ,STIFR_TMP ,     DRAPE_Q4 , 
     Z DRAPE_T3       ,H3D_DATA       ,SUBSETS    ,IGRNOD    ,     FCONT_MAX,
     A FANI(1,NFNCA2+1),FANI(1,NFTCA2+1),NDDL0    ,NNZK0     ,IMPBUF_TAB   )
#else 
        WRITE(6,*) __LINE__,"Fatal error: MUMPS required"
        CALL FLUSH(6)
        CALL ARRET(5)
#endif

         IF (IDYNA==0.AND.ITSK==0) CALL CP_DM(NUMGEO,GEO,IGEO,DMCP,2)
         MSTOP=2
       ELSE
        IF (ILASTANIM/=1) THEN
         IF (ILINE/=1.AND.ISPMD==0) THEN
          WRITE(IOUT,*)
          WRITE(ISTDO,*)
          WRITE(IOUT,1002)IT_T
          WRITE(ISTDO,1002)IT_T
          WRITE(IOUT,1003)IT_BCS,IT_PCG
          WRITE(ISTDO,1003)IT_BCS,IT_PCG
         ENDIF
        END IF !(ILASTANIM==2) THEN
        TT = MIN(TT,TSTOP+EM10)
        DT2 = DT2T
       ENDIF !IF (IMP_CHK>0)
        IF (TT>TSTOP.AND.INCONV==1) THEN
              CALL IMP_RESTARCP(X,V,VR,GEO,IGEO,DMCP,IMPBUF_TAB)
        ENDIF !IF (TT>TSTOP.AND.INCONV==1)

       CALL TRACE_OUT(3)
       IF (IMON>0) CALL STOPTIME(34,1)
C---------------------------
C     Not pure thermal case                      
C---------------------------
      ELSEIF(ILAG+IALE+IEULER/=0)THEN

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL

C----------------------------
C    RBODIES TO RIGIDE SURFACE.
C--- //  ----------------
          IF (NSURF/=0) THEN

C========================================================================================
C                                   DOMAIN 0 
C========================================================================================
          IF(ISPMD==0) THEN    ! traitement int14 sur p0
              CALL SRFVIT(X,V,VR,A,AR,
     .            NPBY  ,RBY  ,MS  ,IN   ,
     .            IGRSURF ,BUFSF)
            END IF
          ENDIF

!$OMP END PARALLEL

C----------------------------------------------------------------
C         USER WINDOWS - Save Accelerations before reset to zero
C----------------------------------------------------------------
        IF(IUSERWI /= 0)THEN 
          DO I=1,NUMNOD
            A_SAV(1,I)=A(1,I)*MS(I)
            A_SAV(2,I)=A(2,I)*MS(I)
            A_SAV(3,I)=A(3,I)*MS(I)            
          ENDDO
          IF(IRODDL/=0)THEN
          DO I=1,NUMNOD
            AR_SAV(1,I)=AR(1,I)*IN(I)
            AR_SAV(2,I)=AR(2,I)*IN(I)
            AR_SAV(3,I)=AR(3,I)*IN(I)            
          ENDDO
          ENDIF
        ENDIF
C------------------------------------------
C         SYNCHRONISATION SUR V TT ET NCYCLE (implicite)
C----------------------------

        IF(IMON>0) CALL STARTIME(5,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK,NODFT_NL,NODLT_NL)
        ITSK = OMP_GET_THREAD_NUM()
        NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
        NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
C-----------------
C         VITESSES
C-----------------
          CALL VITESSE(
     1                 A      , AR     , V   , VR   , FZERO,
     2                 NODFTSK, NODLTSK, ITAB,ALE_CONNECTIVITY%NALE             )
c        
        IF (NLOC_DMG%IMOD > 0) THEN
c
          NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
          NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
          CALL NLOCAL_VEL(NLOC_DMG, NODFT_NL ,NODLT_NL)        
        ENDIF
c
         IF(IALELAG > 0) THEN
             CALL FLOW_VITESSE(ALE_CONNECTIVITY%NALE,AFLOW   ,VFLOW   , FZERO  ,
     2                         NODFTSK,NODLTSK  ,WFLOW, V,IFOAM)
        ENDIF
C
        IF(NPINCH > 0) THEN
          CALL VITESSEPINCH(
     1                      PINCH_DATA%APINCH, PINCH_DATA%VPINCH,
     2                      NODFTSK          , NODLTSK          )
        ENDIF
C
CC
!$OMP END PARALLEL

C----------------------------------
C       ITET2 of S10 Kinematic
C----------------------------------
       IF (NS10E > 0) CALL S10CNDV(ICNDS10,VND   ,V   )
       
C========================================================================================
C                                     PARALLEL SECTION (SMP)
C==========================================================================================

       IF(IPLYXFEM > 0) THEN
          NTHOLD= NTHREAD
          NTHREAD=1
          call omp_set_num_threads(NTHREAD)
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
         ITSK = OMP_GET_THREAD_NUM()
         NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
         NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
c for smp parallel is not parit on
        CALL PLY_VITESSE(NODFTSK,NODLTSK,NPLYMAX,INOD_PXFEM,NUMNOD)
!$OMP END PARALLEL
         NTHREAD= NTHOLD
         call omp_set_num_threads(NTHREAD)
      ENDIF
C
        IF(IMON>0) CALL STOPTIME(5,1)

        IF(IALE+IEULER==0) THEN
C----------------------
C          LAGRANGE PUR
C----------------------

         IF(IMON>0) CALL STARTIME(5,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK,NODFT_NL,NODLT_NL)

          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

          CALL DEPLA(V      ,D      ,X  ,VR   ,DR    ,
     2               NODFTSK,NODLTSK,XDP,DDP)
C
          CALL DEPLAFAKEIGE(X ,V ,INTBUF_TAB, KXIG3D,
     2                      IXIG3D,IGEO, KNOT, WIGE,
     3                      KNOTLOCPC,KNOTLOCEL)

c        
         IF (NLOC_DMG%IMOD > 0) THEN
c
           NODFT_NL   = 1+ ITSK*NLOC_DMG%NNOD/NTHREAD
           NODLT_NL   = (ITSK+1)*NLOC_DMG%NNOD/NTHREAD
           CALL NLOCAL_INCR(NLOC_DMG, NODFT_NL,NODLT_NL)       
         ENDIF
c
         IF(IALELAG > 0) THEN
             CALL FLOW_DEPLA(ALE_CONNECTIVITY%NALE, VFLOW      ,DFLOW    ,
     2                     NODFTSK,NODLTSK)
         ENDIF
C
        IF(NPINCH > 0) THEN
          CALL DEPLAPINCH(PINCH_DATA%VPINCH,  PINCH_DATA%DPINCH,
     2                    PINCH_DATA%XPINCH,  NODFTSK          , NODLTSK)
        ENDIF
C
!$OMP END PARALLEL

          IF(IMON>0) CALL STOPTIME(5,1)
C
        ELSEIF(IALE/=0)THEN
C-------------------------------------
C          -A.L.E.-(+LAGRANGE)(+EULER)
C-------------------------------------

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
             CALL ALEWDX(
     1         GEO           ,X             ,D           ,V           ,VR         ,   
     2         W             ,WA            ,WB          ,RBY         ,SKEW       ,   
     3         PM            ,XLAS          ,MS          ,FSAV        ,               
     4         A             ,AR            ,TF          ,RWBUF       ,   
     5         IN            ,DT2SAVE        ,                                        
     6         IPARG         ,IXS           ,IXQ         ,NODPOR       ,              
     7         ISKEW         ,ICODT       ,ELBUF_TAB  ,   
     8         NPC           ,LINALE        ,NPRW        ,LAS        ,   
     9         IPARI         ,NODFTSK       ,NODLTSK     ,ITSK        ,               
     A         IAD_ELEM      ,FR_ELEM       ,NBRCVOIS    ,NBSDVOIS    ,LNRCVOIS   ,   
     B         LNSDVOIS      ,WEIGHT        ,ADSKY       ,FSKY        ,IADS       ,   
     C         FR_WALL     ,NPORGEO     ,PROCNE     ,   
     D         FR_NBCC       ,IADS(I87B)  ,XDP         ,IGRNOD     ,   
     E         DR            ,INTBUF_TAB    ,ITAB        ,MULTI_FVM   ,            
     F         ALE_CONNECTIVITY,DDP         ,NE_NERCVOIS ,NE_NESDVOIS, 
     G         NE_LERCVOIS   ,NE_LESDVOIS   ,XCELL       ,XFACE)

!$OMP END PARALLEL

C-------------------------
C       -EULER+LAGRANGE-
C No displacements computed if pure Euler
C-------------------------
        ELSEIF(ILAG==1)THEN
C // -----------------------------------
          IF(IMON>0) CALL STARTIME(5,1)

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)

          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD

           CALL EULDX(V,D,X,DDP,ALE_CONNECTIVITY%NALE,NODFTSK,NODLTSK)

!$OMP END PARALLEL

          IF(IMON>0) CALL STOPTIME(5,1)
        ENDIF
      ENDIF


C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================

C-------------------------------------
C     UPDATE XFEM CONFIGURATION
C-------------------------------------
      IF (ICRACK3D > 0) THEN
        IF (NLEVSET > 0)THEN
c
          CALL UPXFEM1(XFEM_TAB,
     .         IPARG        ,IXC        ,NGROUC      ,IGROUC  ,IXTG    ,
     .         IADC_CRK     ,IEL_CRK    ,INOD_CRK    ,ELCUTC  ,NODEDGE ,
     .         ENRTAG       ,CRKEDGE    ,XEDGE4N     ,XEDGE3N )
C
          IF (NSPMD > 1)       ! exchange ENRTAG
     .      CALL SPMD_CRK_ADV(IAD_ELEM ,FR_ELEM ,INOD_CRK ,ENRTAG)
c         /---------------/
          CALL MY_BARRIER
c         /---------------/

C========================================================================================
C                                   PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
c         USE ENRTAG => set positive enrichments
          CALL UPENR_CRK(ADSKY_CRK,INOD_CRK ,NODFTSK    ,NODLTSK       ,
     .                   NODENR   ,ENRTAG   ,NODLEVXF   ,PROCNE_CRK    )
!$OMP END PARALLEL

c         /---------------/
          CALL MY_BARRIER
c         /---------------/
c         set TAGXP after updating enrichments
          CALL UPXFEM_TAGXP(XFEM_TAB,
     .         IPARG        ,IXC        ,NGROUC      ,IGROUC  ,IXTG    ,
     .         IADC_CRK     ,IEL_CRK    ,INOD_CRK    ,ELCUTC  ,NODEDGE ,
     .         ENRTAG       ,CRKEDGE    ,XEDGE4N     ,XEDGE3N ,ITAB    )
C         /---------------/
          CALL MY_BARRIER
C         /---------------/
!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
c         update velocities of phantom elements
          CALL CRK_VITESSE(ADSKY_CRK,INOD_CRK ,NODLEVXF ,NODFTSK  ,NODLTSK  ,
     .                     X        ,V        ,VR       ,A        ,AR       ,
     .                     ITAB     )
!$OMP END PARALLEL
c         /---------------/
          CALL MY_BARRIER
c         /---------------/

C========================================================================================
C                                   NON PARALLEL SECTION (SMP)
C========================================================================================

          CALL CRK_VITESSE2(IPARG    ,NGROUC   ,IGROUC   ,ELCUTC   ,CRKEDGE  ,
     .                      NODEDGE  ,IXC      ,IXTG     ,XEDGE4N  ,XEDGE3N  ,
     .                      IADC_CRK ,IEL_CRK  ,INOD_CRK ,ITAB     )
c----------------------------------------------------------------------
c        spmd xfem velocity exchange
c----------------------------------------------------------------------
         IF (NSPMD > 1) THEN
           CALL SPMD_EXCH_CRKVEL(IAD_ELEM ,FR_ELEM ,INOD_CRK ,ITAB   ,
     .                           X        ,V       ,VR       )
         ENDIF
C         /---------------/
          CALL MY_BARRIER
C         /---------------/

C========================================================================================
C                                   PARALLEL SECTION (SMP)
C========================================================================================

!$OMP PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
          CALL CRK_ZERO_ACCEL(ADSKY_CRK,INOD_CRK ,NODFTSK  ,NODLTSK  ,NODLEVXF )
!$OMP END PARALLEL
c----------------------------------------------------------------------

C========================================================================================
C                                NON PARALLEL SECTION (SMP)
C========================================================================================

         CALL UPXFEM2(IPARG      ,IXC    ,NGROUC  ,IGROUC  ,IADC_CRK    ,
     .                IEL_CRK    ,ELCUTC ,IXTG    ,ENRTAG  ,INOD_CRK    ,
     .                IAD_ELEM   ,FR_ELEM,IAD_EDGE,FR_EDGE ,FR_NBEDGE   ,
     .                CRKEDGE    )


        ELSE  ! NLEVSET = 0
        
C========================================================================================
C                                   PARALLEL SECTION (SMP)
C========================================================================================

!$OMP   PARALLEL PRIVATE(ITSK,NODFTSK,NODLTSK)
          ITSK = OMP_GET_THREAD_NUM()
          NODFTSK   = 1+ITSK*NUMNOD/ NTHREAD
          NODLTSK   = (ITSK+1)*NUMNOD/NTHREAD
          CALL CRK_COORD_INI(ADSKY_CRK,INOD_CRK ,NODFTSK,NODLTSK,X        ,
     .                       NODLEVXF )
!$OMP   END PARALLEL
        END IF
C---
      END IF  ! XFEM
      
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

      IF(NINTSTAMP/=0)THEN
        CALL INTSTAMP_MOVE(INTSTAMP ,NPC   ,TF   ,SKEW ,PTR_SMS,
     .                        V     ,VR    ,MS   ,X    ,D      ,
     .                     NPBY     ,RBY   )
      END IF
      ! --------------------------------
      ! velocity update for /INT18 + LAW151
      IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
        IF(NSPMD>1.AND. IPARIT/=0) THEN
            CALL SPMD_INT18_LAW151_PON( IPARI,ISLEN7,IRLEN7,2,INTBUF_TAB,
     1                                 MULTI_FVM )
        ENDIF

C========================================================================================
C                                     PARALLEL SECTION (SMP)
C========================================================================================

!$OMP   PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()
          CALL INT18_LAW151_UPDATE(ITSK   ,MULTI_FVM,IGRBRIC  ,IPARI,IXS,
     1                             IGROUPS,IPARG    ,ELBUF_TAB,MULTI_FVM%FORCE_INT         ,
     2             X       ,          V       ,          MS         ,          KINET       ,
     3   MULTI_FVM%X_APPEND,MULTI_FVM%V_APPEND,MULTI_FVM%MASS_APPEND,MULTI_FVM%KINET_APPEND)
!$OMP   END PARALLEL

       ENDIF
       
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

      IF( MULTI_FVM%IS_USED ) THEN
!       write *.adb files for FVM solver option
        IF (DEBUG(MACRO_DEBUG_ACC)==1) THEN
            IF (ISPMD==0) THEN
                SIZ = NUMELSG
            ELSE
                SIZ = 0
            END IF
            IF ( NCYCLE>=DEBSTART .AND.
     .           MOD(NCYCLE-DEBSTART,RSTFREQ)==0 ) THEN
                CALL SPMD_COLLECT_MULTI_FVM(IXS,MULTI_FVM)
            ENDIF
        ENDIF
      ENDIF
C========================================================================================
C                                     NON PARALLEL SECTION (SMP)
C========================================================================================

       IF (INT24USE == 1)THEN
C E2E Fictive Node Position, Velocity, Mass
C Useful to do it before send back to Remote nodes, E2E Fictive node position,
C mass & velocity
         CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTBUF_TAB,
     *                                     X,V,MS,ITAB)
       ENDIF

      IF (NSPMD>1) THEN
        IF(IMONM == 2)THEN
          CALL STARTIME(60,1)
          CALL SPMD_BARRIER()
          CALL STOPTIME(60,1)
      END IF
        IF(IMON>0) CALL STARTIME(13,1)
        IF(ISIZXV>0) CALL SPMD_SD_XV(
     1     X     ,D       ,V      ,VR    ,MS   ,
     2     IN    ,IAD_ELEM,FR_ELEM,WEIGHT,IMSCH,
     3     W     ,ISIZXV ,ILENXV  ,XDP)
        IF (IMONM > 0) CALL STARTIME(23,1)
        L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
        L2 = L1+6*NUMELS10
        L3 = L2+12*NUMELS20
          CALL SPMD_I7XVCOM2(
     1     IPARI   ,X_A    ,V       ,MS      ,
     2     IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3     ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4     IRLEN17 ,IXS    ,IXS(L3) ,
     5     IGRBRIC ,TEMP   ,1       ,IRLEN7T ,ISLEN7T ,
     6     IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7     ISLEN20E,IKINE  ,DIAG_SMS,SENSOR_TAB,INTBUF_TAB ,INT24E2EUSE,
     8     FORNEQS ,MULTI_FVM)
        IF (IMONM > 0) CALL STOPTIME(23,1)
        IF (IMON>0) CALL STOPTIME(13,1)
        ENDIF

       IF (INT24USE == 1)THEN
        IF (IMON>0) CALL STARTIME(8,1)        
        CALL SPMD_EXCH_I24(IPARI,    INTBUF_TAB       ,ITAB  ,
     *                     IAD_ELEM, FR_ELEM ,INTLIST ,NBINTC,
     *                     IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,3,
     *                     INT24E2EUSE)
        IF (IMON>0) CALL STOPTIME(8,1)
       ENDIF

       IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1.AND.NSPMD>1) THEN
         CALL SPMD_I21TEMPCOM(IPARI,TEMP,INTBUF_TAB,SENSOR_TAB) 
       ENDIF

       IF(NUMFRAM/=0.AND.N2D==0)THEN
C----------------------------
C      MOVING FRAME: RETRIEVE POSITION & VELOCITY.
C--- //0 ----------------
        IF (IMON>0)CALL STARTIME(6,1)
        IF (IMONM > 0) CALL STARTIME(49,1)
        IF (IMPL_S >0) THEN
         CALL MOVFRA_IMP(XFRAME ,IFRAME ,X      ,V      ,A      ,
     .                   VR     ,AR     ,D      )
        ELSE
        CALL MOVFRA2(XFRAME ,IFRAME ,X      ,V      ,VR     ,
     .               D      )
        END IF !(IMPL_S >0)
        IF (IMONM > 0) CALL STOPTIME(49,1)
        IF (IMON>0) CALL STOPTIME(6,1)
       ENDIF

      IF(IDT_THERM == 1.AND.(TSTOP-TT)<=EM20)THEN    
         MSTOP_DT_THERM = 1
      ENDIF
       
 300  CONTINUE
 
C---------------
C     END OF RUN
C---------------
      IF (((TT>TSTOP).OR.(MSTOP_DT_THERM==1)).AND.IMCONV==1) THEN

          BOOL_RESTART = (ILASTANIM==0.OR.ILASTANIM==1.OR.ILASTANIM==3)
          IF (BOOL_RESTART.AND.(IRAD2R==0)) THEN
C
         IF(IDT_THERM == 1) THEN
            CALL BCSDTTH_COPY(ICODT, ICODR, ICODT0, ICODR0, 2)
         ENDIF
C
         CALL BCSN(ICODE,ICODT,ICODR,PARTS0,PARTSAV)
          
         IF (INT24USE == 1)THEN
C E2E Fictive Node Position, Velocity, Mass
C Useful to do it before send back to Remote nodes, E2E Fictive node position,
C mass & velocity
           CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTBUF_TAB,
     *                                       X,V,MS,ITAB)
         ENDIF

          IF(NSPMD>1)THEN
            L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
            L2 = L1+6*NUMELS10
            L3 = L2+12*NUMELS20
            CALL SPMD_I7XVCOM2(
     1         IPARI   ,X       ,V       ,MS      ,
     2         IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3         ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4         IRLEN17 ,IXS    ,IXS(L3) ,
     5         IGRBRIC ,TEMP   ,2       ,IRLEN7T ,ISLEN7T ,
     6         IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7         ISLEN20E,IKINE  ,DIAG_SMS,SENSOR_TAB,INTBUF_TAB ,INT24E2EUSE,
     8         FORNEQS ,MULTI_FVM)
          END IF

          IF (INT24USE == 1)THEN
            IF (IMON>0) CALL STARTIME(8,1)        
              CALL SPMD_EXCH_I24(IPARI,    INTBUF_TAB ,ITAB  ,
     *                     IAD_ELEM, FR_ELEM ,INTLIST ,NBINTC,
     *                     IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,3,
     *                     INT24E2EUSE)
     
            IF (IMON>0) CALL STARTIME(8,1)        
              CALL SPMD_EXCH_I24(IPARI,    INTBUF_TAB ,ITAB  ,
     *                     IAD_ELEM, FR_ELEM ,INTLIST ,NBINTC,
     *                     IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,4,
     *                     INT24E2EUSE)
            IF (IMON>0) CALL STOPTIME(8,1)
          ENDIF
          
C--------------------------------------------------------
C INTERFACE 21 - Communication of nodal temperature
C--------------------------------------------------------

          IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1.AND.NSPMD>1) THEN
             CALL SPMD_I21TEMPCOM(IPARI,TEMP,INTBUF_TAB,SENSOR_TAB) 
          ENDIF

          ICH = 0
          IF (RESTART_FILE==1) THEN
          CALL WRRESTP(AF    ,IAF    ,XDP    ,ICH   ,ADSKY  ,
     .                 ELBUF_TAB,XFEM_TAB,INTBUF_TAB, MULTI_FVM,
     .                 H3D_DATA ,INTBUF_FRIC_TAB,SUBSETS,MATPARAM_TAB,GROUP_PARAM_TAB,
     .                 PINCH_DATA, ALE_CONNECTIVITY, T_MONVOL,
     .                 SENSOR_TAB, EBCS_TAB)
          IF (NFLOW>0) CALL NFWREST(IFLOW, RFLOW)
          CALL FVWREST()
          IF (IMPL_S>0) THEN
            CALL IMP_TRANS(R_IMP)
            CALL IMPWREST(IRFORM,IMPL_S0)
          ENDIF
C
            IF(ITHERM_FE > 0 )CALL THCWREST(MCP, TEMP)
C
            IF(NITSCHE > 0 )CALL NITSCHEWREST(FORNEQS)
C
            CALL WRITE_UNITS(UNITAB)
            IF(IRIGID_MAT > 0 )
     *          CALL RIGMATWREST(RBYM  ,IRBYM ,LNRBYM,WEIGHT_RM)
C thermal boundary
            IF(NUMCONV > 0 )  CALL CONVWREST(IBCV, FCONV)
            IF(NUMRADIA > 0 ) CALL RADIAWREST(IBCR, FRADIA)
            IF(NFXFLUX > 0 )  CALL FXFLUXWREST(IBFFLUX, FBFFLUX)
            IF(NFXTEMP > 0 )  CALL FXTEMPWREST(IBFTEMP, FBFTEMP)
            IF (IPLYXFEM > 0)  THEN
              CALL PLYXFEM_WREST(MS_PLY,ZI_PLY,INOD_PXFEM,IEL_PXFEM,
     .                       ICODE_PLY,ISKEW_PLY,MSZ2)
              CALL PLYXFEM_WRESTANIM()
              CALL PLYXFEM_WRAVUPLY()
            ENDIF
            IF (NLOADC > 0) CALL WRITE_DB(CFIELD,LFACLOAD*NLOADC)
            IF (NLOADP > 0) CALL WRITE_DB(LOADP,LFACLOAD*NLOADP)
            IF (NINTLOADP > 0) CALL WRITE_DB(DGAPLOADINT,NINTER*NLOADP_HYD)

c-------------------------------
C xfem for layerd shell (cracks)
            IF (ICRACK3D > 0) THEN
              CALL CRKXFEM_WREST(INOD_CRK,IEL_CRK,NODLEVXF)
              CALL CRKXFEM_WRESTANIM(CRKEDGE,CRKSKY,INDX_CRK,XEDGE4N,XEDGE3N)
            ENDIF
c-------------------------------
            IF(IALEFVM > 0) CALL WRITE_DB(FCELL ,6*NUMELS)
c-------------------------------
C foam + air
            IF(IALELAG > 0 )CALL ALELAG_WREST() 
C THoutput
            IF(SRTHBUF > 0) CALL RTHBUFWREST() 
C
            IF (SKNOT > 0) CALL WRITE_DB(KNOT,SKNOT)
            IF (SKNOTLOCPC > 0) CALL WRITE_DB(KNOTLOCPC,SKNOTLOCPC)
            IF (SKNOTLOCEL > 0) CALL WRITE_DB(KNOTLOCEL,SKNOTLOCEL)
            IF (NUMELIG3D > 0) CALL WRITE_DB(WIGE,NUMNOD)

C
            IF(IPART_STACK >0) 
     .          CALL STACK_WREST(STACK%IGEO,STACK%GEO,STACK%PM )
C
            IF (NDRAPE > 0) CALL DRAPE_WREST(DRAPE_Q4  , DRAPE_T3) 
            IF (NUMPLY > 0) CALL PLY_INFO_WREST(PLY_INFO) 
!
            CALL ELAPSTIME(SECS)
            GLOBAL_COMP_TIME%ENGINE_TIME(GLOBAL_COMP_TIME%RUN_NBR) = SECS
!       ---------------------------
!         write the starter + engine elapsed time
            CALL WRITE_DPDB(GLOBAL_COMP_TIME%STARTER_TIME,1)
            CALL WRITE_DPDB(GLOBAL_COMP_TIME%ENGINE_TIME,GLOBAL_COMP_TIME%RUN_NBR)
!       ---------------------------
        
C Restart file size
            IF (MULTIREST >0)THEN
             CALL  file_size(RESTSIZE)
             IF (RESTSIZE > MULTIRESTS(IRPREV))MULTIRESTS(IRPREV)=RESTSIZE
            ELSE
             CALL file_size(RESTSIZE)
             IF (RESTSIZE > RESTARTFILESIZE) RESTARTFILESIZE=RESTSIZE
            ENDIF
C
            CALL CLOSE_C
          ENDIF 
          STATE_ANIM = 0
          IF (ILASTANIM==1) THEN
            STATE_ANIM = 1
          ELSEIF (ILASTANIM==0.OR.ILASTANIM==3) THEN
            STATE_ANIM = 2
          END IF

          STATE_H3D = 0
          IF (ILASTH3D==1) THEN
            STATE_H3D = 1
          ELSEIF (ILASTH3D==0.OR.ILASTH3D==3) THEN
            STATE_H3D = 2
          END IF

          IF( STATE_ANIM == 1 .OR. STATE_H3D == 1 ) THEN
            CALL TRACE_OUT(3)
            GOTO 100
          ELSEIF( STATE_ANIM == 2 .OR. STATE_H3D == 2 ) THEN
            MSTOP=2
            GOTO 500
          ENDIF

          ELSE
            MSTOP=2
            GOTO 500
          END IF
      END IF
C-----------------------
      IF(DT2<=ZERO) THEN
         WRITE(IOUT,*)' **ERROR : TIME STEP LESS OR EQUAL ZERO'
         WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL ZERO'
         IF ( ISTAMPING == 1) THEN
           WRITE(ISTDO,'(A)')' The run has gone to divergence.'
           WRITE(ISTDO,'(A)')' It could be due to a wrong definition of
     . the interfaces between the tools and the blank.'
           WRITE(ISTDO,'(A)')' You may need to check if there is enough
     . clearance between the tools,'
           WRITE(ISTDO,'(A)')' and that they do not penetrate each othe
     .r during their travel.'
           WRITE(IOUT, '(A)')' The run has gone to divergence.'
           WRITE(IOUT, '(A)')' It could be due to a wrong definition of
     . the interfaces between the tools and the blank.'
           WRITE(IOUT, '(A)')' You may need to check if there is enough
     . clearance between the tools,'
           WRITE(IOUT, '(A)')' and that they do not penetrate each othe
     .r during their travel.'
         ENDIF
C termination wait if  dt < 0
       IF (INT24USE == 1)THEN
C E2E Fictive Node Position, Velocity, Mass
C Useful to do it before send back to Remote nodes, E2E Fictive node position,
C mass & velocity
         CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTBUF_TAB,
     *                                     X,V,MS,ITAB)
       ENDIF
         IF(NSPMD>1)THEN                                       
           L1 = 1+NIXS*NUMELS + NSVOIS*NIXS                                     
           L2 = L1+6*NUMELS10                                     
           L3 = L2+12*NUMELS20                                    
           CALL SPMD_I7XVCOM2(                                    
     1          IPARI   ,X       ,V       ,MS      ,              
     2          IMSCH   ,I2MSCH  ,DT2PREV ,INTLIST ,NBINTC  ,     
     3          ISLEN7  ,IRLEN7  ,ISLEN11 ,IRLEN11 ,ISLEN17 ,     
     4          IRLEN17 ,IXS     ,IXS(L3) ,              
     5          IGRBRIC ,TEMP    ,2       ,IRLEN7T ,ISLEN7T ,     
     6          IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T,IRLEN20E,     
     7          ISLEN20E,IKINE   ,DIAG_SMS,SENSOR_TAB,INTBUF_TAB ,INT24E2EUSE ,
     8          FORNEQS ,MULTI_FVM) 
         END IF                                                   
         DEALLOCATE(ISENDTO, IRCVFROM)
         DEALLOCATE(INTLIST)
         DEALLOCATE(INTLIST25)
         DEALLOCATE(NISKYFI)
         DEALLOCATE(NISKYFIE)
         DEALLOCATE(FR_NBCC, FR_NBCCI2)
         DEALLOCATE(DRETRI)
         DEALLOCATE(XSEC)
         DEALLOCATE(IRBKIN_L)
         DEALLOCATE (ICODT0,ICODR0)
         DEALLOCATE(ISENDP)
         DEALLOCATE(IRECVP)
         DEALLOCATE(IRECVP_PXFEM)
         DEALLOCATE(ISENDP_CRK)
         DEALLOCATE(IRECVP_CRK)      
         DEALLOCATE(CNEL)
         DEALLOCATE(ADDCNEL)
         DEALLOCATE(ADDTMPL)
         DEALLOCATE(TAGEL)
         DEALLOCATE(IBUFIDEL)
         DEALLOCATE(INDIDEL)
         DEALLOCATE(IPARTL)
         DEALLOCATE(EMINX)
         IF(NADMESH/=0)THEN
           DEALLOCATE(LSH4ACT,LSH4KIN,PSH4ACT,PSH4KIN,
     .                LSH3ACT,LSH3KIN,PSH3ACT,PSH3KIN,
     .                MSH4SKY,MSH3SKY)
          IF(IDEL7NG>=1) DEALLOCATE(TAGTRIMC,TAGTRIMTG)
         END IF
         DEALLOCATE(STIFN_TMP,STIFR_TMP)
         DEALLOCATE(ACND,ARCND,STCND,STRCND)            
         DEALLOCATE(LSH4UPL,LSH3UPL,PSH4UPL,PSH3UPL)    
         DEALLOCATE(FTHREAC)                            
         DEALLOCATE(FTHDTM)                             
         DEALLOCATE(FREAC)                            
         DEALLOCATE(NODREAC)                            
         DEALLOCATE(GRTH)                               
         DEALLOCATE(IGRTH)                              
         DEALLOCATE(IGROUPC,IGROUPTG,IGROUPS)                   
         DEALLOCATE(GRESAV)                             
         DEALLOCATE(VARNOD)
         DEALLOCATE(PARTSAV2)                             
         IF(IMON>0) THEN     
           IF(NVOLU > 0)  CALL FVSTATS(MONVOL)
           CALL STOPTIME(1,1)   
            IF(IMONM>0) CALL PRINTIME_INTERF(INTBUF_TAB,IPARI,INTLIST,NBINTC)
            CALL PRINTIME(0)
         ELSE
            CALL ADD_ELAPSED_TIME_MON_OFF( )
         ENDIF                  
         CALL TRACE_OUT(3)      
         RETURN                 
      ENDIF
      IF (IFSUB==0.AND.IMCONV==1) THEN
      IF(IMON>0) CALL STARTIME(7,1)
      IF(  (RESTART_FILE==1).AND.( 
     1     (((NCYCLE/NCRST)*NCRST==NCYCLE.AND.(IRAD2R==0))
     2       .OR.MREST==1.OR.WMCHECK==1) ) ) THEN
C
         IF(IDT_THERM == 1) THEN
            CALL BCSDTTH_COPY(ICODT, ICODR, ICODT0, ICODR0, 2)
         ENDIF
C
      CALL BCSN(ICODE,ICODT,ICODR,PARTS0,PARTSAV)
      IF((MREST==1.OR.(NCYCLE/NCRST)*NCRST==NCYCLE)
     .       .AND.WMCHECK==0)THEN
       IF (INT24USE == 1)THEN
C E2E Fictive Node Position, Velocity, Mass
C Useful to do it before send back to Remote nodes, E2E Fictive node position,
C mass & velocity
         CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTBUF_TAB,
     *                                     X,V,MS,ITAB)
       ENDIF
      IF(NSPMD>1)THEN
        L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
        L2 = L1+6*NUMELS10
        L3 = L2+12*NUMELS20
        CALL SPMD_I7XVCOM2(
     1       IPARI   ,X       ,V       ,MS      ,
     2       IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3       ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4       IRLEN17 ,IXS    ,IXS(L3) ,
     5       IGRBRIC ,TEMP   ,3       ,IRLEN7T ,ISLEN7T ,
     6       IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7       ISLEN20E,IKINE  ,DIAG_SMS,SENSOR_TAB,INTBUF_TAB,INT24E2EUSE,
     8       FORNEQS ,MULTI_FVM)
        END IF

        IF (INT24USE == 1)THEN
          IF (IMON>0) CALL STARTIME(8,1)        
            CALL SPMD_EXCH_I24(IPARI,    INTBUF_TAB,ITAB,
     *                     IAD_ELEM, FR_ELEM ,INTLIST ,NBINTC,
     *                     IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,3,
     *                     INT24E2EUSE)
     
            CALL SPMD_EXCH_I24(IPARI,    INTBUF_TAB,ITAB,
     *                     IAD_ELEM, FR_ELEM ,INTLIST ,NBINTC,
     *                     IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE,4,
     *                     INT24E2EUSE)
          IF (IMON>0) CALL STOPTIME(8,1)
        ENDIF

C
        ICH = 0
C--------------------------------------------------------
C INTERFACE 21 - Communication of nodal temperature
C--------------------------------------------------------
          IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1.AND.NSPMD>1) THEN
             CALL SPMD_I21TEMPCOM(IPARI,TEMP,INTBUF_TAB,SENSOR_TAB) 
          ENDIF

          CALL WRRESTP(AF    ,IAF    ,XDP    ,ICH   ,ADSKY  ,
     .                 ELBUF_TAB,XFEM_TAB,INTBUF_TAB, MULTI_FVM,
     .                 H3D_DATA ,INTBUF_FRIC_TAB,SUBSETS,MATPARAM_TAB,GROUP_PARAM_TAB,
     .                 PINCH_DATA, ALE_CONNECTIVITY, T_MONVOL,
     .                 SENSOR_TAB, EBCS_TAB)
        IF (NFLOW>0) CALL NFWREST(IFLOW, RFLOW)
        CALL FVWREST()
        IF (IMPL_S>0) THEN
          CALL IMP_TRANS(R_IMP)
          CALL IMPWREST(IRFORM,IMPL_S0)
        ENDIF
C
        IF(ITHERM_FE > 0  ) CALL THCWREST(MCP, TEMP)
C
        IF(NITSCHE > 0 )CALL NITSCHEWREST(FORNEQS)
C
        CALL WRITE_UNITS(UNITAB)
C
        IF(IRIGID_MAT > 0 )
     *    CALL RIGMATWREST(RBYM ,IRBYM ,LNRBYM,WEIGHT_RM)
C thermal boundary
          IF(NUMCONV > 0 )  CALL CONVWREST(IBCV, FCONV)
          IF(NUMRADIA > 0 ) CALL RADIAWREST(IBCR, FRADIA)
          IF(NFXFLUX > 0 )  CALL FXFLUXWREST(IBFFLUX, FBFFLUX)
          IF(NFXTEMP > 0 )  CALL FXTEMPWREST(IBFTEMP, FBFTEMP)
C
          IF (IPLYXFEM > 0)THEN
            CALL PLYXFEM_WREST(MS_PLY,ZI_PLY,INOD_PXFEM,IEL_PXFEM,
     .                       ICODE_PLY,ISKEW_PLY,MSZ2)
            CALL PLYXFEM_WRESTANIM()
            CALL PLYXFEM_WRAVUPLY()
          ENDIF  
          IF (NLOADC > 0) CALL WRITE_DB(CFIELD,LFACLOAD*NLOADC)
          IF (NLOADP > 0) CALL WRITE_DB(LOADP,LFACLOAD*NLOADP)
          IF (NINTLOADP > 0) CALL WRITE_DB(DGAPLOADINT,NINTER*NLOADP_HYD)
c-------------------------------
C xfem for layerd shell (cracks)
c-------------------------------
          IF (ICRACK3D > 0) THEN
            CALL CRKXFEM_WREST(INOD_CRK,IEL_CRK,NODLEVXF)
            CALL CRKXFEM_WRESTANIM(CRKEDGE,CRKSKY,INDX_CRK,XEDGE4N,XEDGE3N)
          ENDIF
c-------------------------------
          IF(IALEFVM > 0) CALL WRITE_DB(FCELL ,6*NUMELS)
c-------------------------------
C foam + air
          IF(IALELAG > 0 )CALL ALELAG_WREST() 
C THoutput
         IF(SRTHBUF > 0) CALL RTHBUFWREST() 
C
          IF (SKNOT > 0) CALL WRITE_DB(KNOT,SKNOT)
          IF (SKNOTLOCPC > 0) CALL WRITE_DB(KNOTLOCPC,SKNOTLOCPC)
          IF (SKNOTLOCEL > 0) CALL WRITE_DB(KNOTLOCEL,SKNOTLOCEL)
          IF (NUMELIG3D > 0) CALL WRITE_DB(WIGE,NUMNOD)
C           
          IF(IPART_STACK >0) 
     .        CALL STACK_WREST(STACK%IGEO,STACK%GEO,STACK%PM)
C
          IF (NDRAPE > 0) CALL DRAPE_WREST(DRAPE_Q4  , DRAPE_T3)
          IF (NUMPLY > 0) CALL PLY_INFO_WREST(PLY_INFO)
!
          CALL ELAPSTIME(SECS)
          GLOBAL_COMP_TIME%ENGINE_TIME(GLOBAL_COMP_TIME%RUN_NBR) = SECS
!       ---------------------------
!         write the starter + engine elapsed time
          CALL WRITE_DPDB(GLOBAL_COMP_TIME%STARTER_TIME,1)
          CALL WRITE_DPDB(GLOBAL_COMP_TIME%ENGINE_TIME,GLOBAL_COMP_TIME%RUN_NBR)
!       ---------------------------

C Restart file size
          IF (MULTIREST >0)THEN
           CALL  file_size(RESTSIZE)
           IF (RESTSIZE > MULTIRESTS(IRPREV))MULTIRESTS(IRPREV)=RESTSIZE
          ELSE
           CALL file_size(RESTSIZE)
           IF (RESTSIZE > RESTARTFILESIZE) RESTARTFILESIZE=RESTSIZE
          ENDIF
C
          CALL CLOSE_C
        END IF
C
        IF(WMCHECK==1)THEN
          IF(ISPMD==0)THEN
            FILNAM = 'CHECK_DATA'
            OPEN(UNIT=ICHECKD,FILE=FILNAM,
     .           ACCESS='SEQUENTIAL',
     .           FORM='FORMATTED',STATUS='UNKNOWN')
            WRITE(ICHECKD, FMT='(3A, I1)')
     .           '/RERUN/',ROOTNAM(1:ROOTLEN),'/',IRUN
            CLOSE(ICHECKD)
          ENDIF
          IF(WMCHECK==1.AND.NCYCLE/=1)THEN
       IF (INT24USE == 1)THEN
C E2E Fictive Node Position, Velocity, Mass
C Useful to do it before send back to Remote nodes, E2E Fictive node position,
C mass & velocity
         CALL  I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTBUF_TAB,
     *                                     X,V,MS,ITAB)
       ENDIF

            IF(NSPMD>1)THEN
              L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
              L2 = L1+6*NUMELS10
              L3 = L2+12*NUMELS20  
              CALL SPMD_I7XVCOM2(
     1             IPARI   ,X       ,V       ,MS      ,
     2             IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3             ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4             IRLEN17 ,IXS    ,IXS(L3) ,
     5             IGRBRIC ,TEMP   ,3       ,IRLEN7T ,ISLEN7T ,
     6             IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7             ISLEN20E,IKINE  ,DIAG_SMS,SENSOR_TAB,INTBUF_TAB ,INT24E2EUSE  ,
     8             FORNEQS ,MULTI_FVM)
              END IF
C
              IF (INT24USE == 1)THEN
                IF (IMON>0) CALL STARTIME(8,1)        
                  CALL SPMD_EXCH_I24(IPARI,    INTBUF_TAB,ITAB,
     *                     IAD_ELEM, FR_ELEM,INTLIST ,NBINTC,
     *                     IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE ,3,
     *                     INT24E2EUSE)

                  CALL SPMD_EXCH_I24(IPARI,    INTBUF_TAB,ITAB,
     *                     IAD_ELEM, FR_ELEM,INTLIST ,NBINTC,
     *                     IAD_I24  ,FR_I24  ,SFR_I24,I24MAXNSNE ,4,
     *                     INT24E2EUSE)
                IF (IMON>0) CALL STOPTIME(8,1)
              ENDIF
C--------------------------------------------------------
C INTERFACE 21 - Communication of nodal temperature
C--------------------------------------------------------
             IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1.AND.NSPMD>1) THEN
                 CALL SPMD_I21TEMPCOM(IPARI,TEMP,INTBUF_TAB,SENSOR_TAB) 
             ENDIF

              ICH = 1
              CALL WRRESTP(AF    ,IAF    ,XDP    ,ICH   ,ADSKY     ,
     .                    ELBUF_TAB,XFEM_TAB,INTBUF_TAB, MULTI_FVM,
     .                    H3D_DATA ,INTBUF_FRIC_TAB,SUBSETS,MATPARAM_TAB,GROUP_PARAM_TAB,
     .                    PINCH_DATA, ALE_CONNECTIVITY, T_MONVOL,
     .                    SENSOR_TAB, EBCS_TAB)
              IF (NFLOW>0) CALL NFWREST(IFLOW, RFLOW)
              CALL FVWREST()
              IF (IMPL_S>0) THEN
                CALL IMP_TRANS(R_IMP)
                CALL IMPWREST(IRFORM,IMPL_S0)
              ENDIF
C
              IF(ITHERM_FE > 0  )CALL THCWREST(MCP, TEMP)
C
              IF(NITSCHE > 0 )CALL NITSCHEWREST(FORNEQS)
C
              CALL WRITE_UNITS(UNITAB)
              IF(IRIGID_MAT > 0 )
     *             CALL RIGMATWREST(RBYM ,IRBYM ,LNRBYM,WEIGHT_RM)
C thermal boundary
              IF(NUMCONV > 0 )  CALL CONVWREST(IBCV, FCONV)
              IF(NUMRADIA > 0 ) CALL RADIAWREST(IBCR, FRADIA)
              IF(NFXFLUX > 0 )  CALL FXFLUXWREST(IBFFLUX, FBFFLUX)
              IF(NFXTEMP > 0 )  CALL FXTEMPWREST(IBFTEMP, FBFTEMP)
C ply xfem (composites)
             IF (IPLYXFEM > 0)THEN     
              CALL PLYXFEM_WREST(MS_PLY,ZI_PLY,INOD_PXFEM,IEL_PXFEM,
     .                         ICODE_PLY,ISKEW_PLY,MSZ2)
              CALL PLYXFEM_WRESTANIM()
              CALL PLYXFEM_WRAVUPLY()
             ENDIF 
          IF (NLOADC > 0) CALL WRITE_DB(CFIELD,LFACLOAD*NLOADC)
          IF (NLOADP > 0) CALL WRITE_DB(LOADP,LFACLOAD*NLOADP)
          IF (NINTLOADP > 0) CALL WRITE_DB(DGAPLOADINT,NINTER*NLOADP_HYD)
c-------------------------------
C xfem for layerd shell (cracks)
          IF (ICRACK3D > 0) THEN
            CALL CRKXFEM_WREST(INOD_CRK,IEL_CRK,NODLEVXF)
            CALL CRKXFEM_WRESTANIM(CRKEDGE,CRKSKY,INDX_CRK,XEDGE4N,XEDGE3N)
          ENDIF
c-------------------------------
          IF(IALEFVM > 0) CALL WRITE_DB(FCELL ,6*NUMELS)
c-------------------------------
C foam + air
          IF(IALELAG > 0 )CALL ALELAG_WREST()
C THoutput
         IF(SRTHBUF > 0) CALL RTHBUFWREST()  
C
          IF (SKNOT > 0) CALL WRITE_DB(KNOT,SKNOT)
          IF (SKNOTLOCPC > 0) CALL WRITE_DB(KNOTLOCPC,SKNOTLOCPC)
          IF (SKNOTLOCEL > 0) CALL WRITE_DB(KNOTLOCEL,SKNOTLOCEL)
          IF (NUMELIG3D > 0) CALL WRITE_DB(WIGE,NUMNOD)
C
C
          IF(IPART_STACK >0) 
     .        CALL STACK_WREST(STACK%IGEO,STACK%GEO,STACK%PM )
C
          IF (NDRAPE > 0) CALL DRAPE_WREST(DRAPE_Q4  , DRAPE_T3)
          IF (NUMPLY > 0) CALL PLY_INFO_WREST(PLY_INFO)
!
          CALL ELAPSTIME(SECS)
          GLOBAL_COMP_TIME%ENGINE_TIME(GLOBAL_COMP_TIME%RUN_NBR) = SECS
!       ---------------------------
!         write the starter + engine elapsed time
          CALL WRITE_DPDB(GLOBAL_COMP_TIME%STARTER_TIME,1)
          CALL WRITE_DPDB(GLOBAL_COMP_TIME%ENGINE_TIME,GLOBAL_COMP_TIME%RUN_NBR)
!       ---------------------------

C Restart file size
              IF (MULTIREST >0)THEN
           CALL  file_size(RESTSIZE)
           IF (RESTSIZE > MULTIRESTS(IRPREV))MULTIRESTS(IRPREV)=RESTSIZE
              ELSE
                CALL file_size(RESTSIZE)
                IF (RESTSIZE > RESTARTFILESIZE) RESTARTFILESIZE=RESTSIZE
              ENDIF
C
              CALL CLOSE_C
            ENDIF
          ENDIF
        ENDIF
C
      IF((IDDW/=0).AND.(MSTOP/=0.OR.TT+DT2>=TSTOP)) THEN
        CALL CUMULTIME_MP(
     1          TAILLE,IPARG,
     2          IXC,IXQ,IXT,IXP,IXTG,
     3          IXR,IXUR,IXS,KXIG3D,IPM,
     4          IGEO,GEO,POIN_UMP,CPUTIME_MP,
     5          NBR_GPMP,CPUTIME_MP_GLOB,TAB_UMP,PM,
     6          BUFMAT,TABMP_L ,TAB_MAT )
        IF(IDDWSTAT/=0) THEN
         CALL PRINTIMEG(IPARG,PM,IPM,IXC,IXTG,IXS)
        ENDIF
      ENDIF  
C
C-------------------------------
        IF(IMON>0) CALL STOPTIME(7,1)
      ENDIF
C-------------------------------
 500  CONTINUE
C-------------------------------     
      
      IF(MSTOP/=0) THEN
        PARALLEL_SECTION = 0
C Multidomain -> close sockets
        IF (IRAD2R==1) THEN
          DO ITSK=1,NTHREAD
            CALL CLOSE_SOCK_C(SOCKET(ITSK))
          END DO
        ENDIF
C /KILL
        IF((NCYCLE/NCRST)*NCRST/=NCYCLE.AND.
     2     MREST/=1.AND.WMCHECK/=1.AND.NSPMD>1)THEN
          L1 = 1+NIXS*NUMELS + NSVOIS*NIXS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
          CALL SPMD_I7XVCOM2(
     1       IPARI   ,X       ,V       ,MS      ,
     2       IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3       ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4       IRLEN17 ,IXS    ,IXS(L3) ,
     5       IGRBRIC ,TEMP   ,2       ,IRLEN7T ,ISLEN7T ,
     6       IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7       ISLEN20E,IKINE  ,DIAG_SMS,SENSOR_TAB,INTBUF_TAB,INT24E2EUSE,
     8       FORNEQS ,MULTI_FVM)
        ENDIF
        
        DEALLOCATE(ISENDTO, IRCVFROM)
        DEALLOCATE(INTLIST25)
        DEALLOCATE(NISKYFI)
        DEALLOCATE(NISKYFIE)
        DEALLOCATE(FR_NBCC, FR_NBCCI2)
        DEALLOCATE(DRETRI)
        DEALLOCATE(XSEC)
        DEALLOCATE(IRBKIN_L)
        DEALLOCATE(ISENDP)
        DEALLOCATE(IRECVP)
        DEALLOCATE(IRECVP_PXFEM)
        DEALLOCATE(ISENDP_CRK)
        DEALLOCATE(IRECVP_CRK)         
        !IF(IDEL7NG>0)THEN
          DEALLOCATE(CNEL)
          DEALLOCATE(ADDCNEL)
          DEALLOCATE(ADDTMPL)
          DEALLOCATE(TAGEL)
          DEALLOCATE(IBUFIDEL)
          DEALLOCATE(INDIDEL)
        !ENDIF
        DEALLOCATE(IPARTL)
        DEALLOCATE(EMINX)
        IF(NADMESH/=0)THEN
          DEALLOCATE(LSH4ACT,LSH4KIN,PSH4ACT,PSH4KIN,
     .               LSH3ACT,LSH3KIN,PSH3ACT,PSH3KIN,
     .               MSH4SKY,MSH3SKY)
          IF(IDEL7NG>=1) DEALLOCATE(TAGTRIMC,TAGTRIMTG)
        END IF
C     
        !interface22
        IF(ALLOCATED(BRICK_LIST))       DEALLOCATE (BRICK_LIST)
        IF(ALLOCATED(LIST_B_OLD))       DEALLOCATE (LIST_B_OLD)        
        IF(ALLOCATED(NBOLD))            DEALLOCATE (NBOLD)         
        IF(ALLOCATED(EDGE_LIST))        DEALLOCATE (EDGE_LIST)
        IF(ALLOCATED(UVARL))            DEALLOCATE (UVARL)
        IF(ALLOCATED(SuperCellVOL_L))   DEALLOCATE (SuperCellVOL_L)
        IF(ALLOCATED(EINT_L))           DEALLOCATE (EINT_L)
        IF(ALLOCATED(RHO_L))            DEALLOCATE (RHO_L) 
        IF(ALLOCATED(MOM_L))            DEALLOCATE (MOM_L)         
        IF(ALLOCATED(SIG_L))            DEALLOCATE (SIG_L) 
        IF(ALLOCATED(VOLD_L))           DEALLOCATE (VOLD_L)         
        IF(ALLOCATED(IMERGEL))          DEALLOCATE (IMERGEL) 
        IF(ALLOCATED(OLD_SecndList))    DEALLOCATE (OLD_SecndList)
        IF(ALLOCATED(UNLINKED_CELLS_L)) DEALLOCATE (UNLINKED_CELLS_L)
        IF(ALLOCATED(N_UNLINKED_L))     DEALLOCATE (N_UNLINKED_L)
        IF(ALLOCATED(V22MAX_L))         DEALLOCATE (V22MAX_L)         
        IF(ALLOCATED(DX22MIN_L))        DEALLOCATE (DX22MIN_L)
        IF(ALLOCATED(INT22_FCELL_ANIM)) DEALLOCATE (INT22_FCELL_ANIM)        
        
        !ALEFVM
        IF(ALLOCATED(WFEXT_CELL))       DEALLOCATE (WFEXT_CELL)
        IF(ALLOCATED(FEXT_CELL))        DEALLOCATE (FEXT_CELL)        
        IF(ALLOCATED(FCELL))            DEALLOCATE (FCELL)
        IF(ALLOCATED(FINT_CELL))        DEALLOCATE (FINT_CELL)
        IF(ALLOCATED(VERTEX))           DEALLOCATE (VERTEX)
        IF(ALLOCATED(F_FACE))           DEALLOCATE (F_FACE)

        !/LOAD/PBLAST
        CALL PBLAST_DEALLOCATE(PBLAST_TAB)

C--     Rad2rad deallocation
        IF(ALLOCATED(IADD_NL))          DEALLOCATE (IADD_NL)
        IF(ALLOCATED(NBDOF_NL))         DEALLOCATE (NBDOF_NL)
        IF(ALLOCATED(NLLNK))            DEALLOCATE (NLLNK)    

C--     Seatblet stuctures deallocation
        IF (NSLIPRING > 0) THEN
          DO I=1,NSLIPRING
            DEALLOCATE(SLIPRING(I)%FRAM)
          ENDDO
          DEALLOCATE(SLIPRING)
        ENDIF
        IF (NRETRACTOR > 0) THEN
          DO I=1,NRETRACTOR
            DEALLOCATE(RETRACTOR(I)%INACTI_NODE)
          ENDDO
          DEALLOCATE(RETRACTOR)
        ENDIF

        !ALEMUSCL
        CALL ALEMUSCL_DEALLOCATE()
        !Multifluid law
        CALL MULTI_DEALLOCATE(MULTI_FVM)
        IF (MULTI_FVM%NS_DIFF) THEN
           CALL DIFFUSION%TERMINATE_DIFFUSION()
        ENDIF
        
        !EBCS
        IF(NEBCS > 0)CALL SEGVAR%destroy()
C
        DEALLOCATE(STIFN_TMP,STIFR_TMP)
        DEALLOCATE(ACND,ARCND,STCND,STRCND)
        DEALLOCATE(LSH4UPL,LSH3UPL,PSH4UPL,PSH3UPL)
        DEALLOCATE(ERR_THK_SH3,ERR_THK_SH4)
        DEALLOCATE(TAGNOD_SMS,NATIV_SMS,NODXI_SMS,TAGPRT_SMS,TAGREL_SMS,
     .             INDX1_SMS, TAGSLV_RBY_SMS, TAG_LNK_SMS, NRWL_SMS,
     .             JAD_SMS, JDI_SMS, JSM_SMS,
     .             JADC_SMS,JADS_SMS,JADS10_SMS,JADT_SMS,
     .             JADP_SMS,JADR_SMS,JADTG_SMS,
     .             X_SMS, P_SMS,Y_SMS, Z_SMS, PREC_SMS, PREC_SMS3,
     .             DIAG_SMS3, LT_SMS, 
     .             KAD_SMS, KDI_SMS , PK_SMS, LTK_SMS, 
     .             JADI_SMS, JDII_SMS, LTI_SMS, MSKYI_SMS, ISKYI_SMS,
     .             XMOM_SMS, TAGMSR_RBY_SMS, T2MAIN_SMS)
C
        DEALLOCATE(FTHREAC)
        DEALLOCATE(FTHDTM)
        DEALLOCATE(FREAC)
        DEALLOCATE(NODREAC)
        DEALLOCATE(GRTH)
        DEALLOCATE(IGRTH)
        DEALLOCATE(GRESAV)
        IF(NRBE3>0)THEN
          DEALLOCATE(RRBE3)
          IF (IPARIT>0) DEALLOCATE(RRBE3_PON)
        END IF
        IF(IMPL_S>0.OR.NEIG>0)THEN
                   CALL DEALLOC_IMPBUF(IMPBUF_TAB)
#if defined(MUMPS5) 
          CALL DEALLOCM_IMP(MUMPS_PAR)
#endif
        END IF
        IF(IPARIT==0.AND.IRESP==1) THEN
        DEALLOCATE( ACC_DP )
        ENDIF

        IF(IMON>0) THEN
          IF(NVOLU > 0)  CALL FVSTATS(MONVOL)
          CALL STOPTIME(1,1)
            IF(IMONM>0) CALL PRINTIME_INTERF(INTBUF_TAB,IPARI,INTLIST,NBINTC)
          CALL PRINTIME(0)
        ELSE
            CALL ADD_ELAPSED_TIME_MON_OFF( )
        ENDIF
        IF(ITHERM_FE > 0 ) CALL THERMBILAN()
        CALL TRACE_OUT(3)

        DEALLOCATE(WIBEM, WRBEM)
        IF(ITHERM_FE > 0  ) DEALLOCATE(FTHE,FTHESKY)
        IF(INTHEAT > 0  )   DEALLOCATE(FTHESKYI)
        IF(NODADT_THERM > 0  ) DEALLOCATE(CONDN,CONDNSKY)
        IF(NODADT_THERM > 0.AND.INTHEAT > 0) DEALLOCATE(CONDNSKYI)
        DEALLOCATE(VRBYM,VRRBYM,ARBYM,ARRBYM)
        IF(IPLYXFEM > 0) THEN
          DO I=1,NPLYMAX
            DEALLOCATE(PLY(I)%A)
            DEALLOCATE(PLY(I)%V)
            DEALLOCATE(PLY(I)%U)
            DEALLOCATE(PLYSKY(I)%FSKY)
          ENDDO
        ENDIF
        IF(INTPLYXFEM > 0)DEALLOCATE(PLYSKYI%FSKYI)
        DEALLOCATE(PLY, PLYSKY)
        DEALLOCATE(FEXT)
C
C Deallocte AMS / POFF
        DEALLOCATE (FR_LOC)
        DEALLOCATE (FR_LOCI2M)
        DEALLOCATE (ICODT0,ICODR0)
        DEALLOCATE(ISENSINT)
C
        CALL FREE_PINCH(PINCH_DATA)

        CALL DEALLOCATE_JOINT()
 
        RETURN
      ENDIF    ! MSTOP/=0     
      
      CALL TRACE_OUT(3)
      
C=============================
C END OF EXPLICIT ITERATIVE LOOP
C=============================

      GO TO 100
C
 1002 FORMAT(3X,'* IMPLICIT COMPUTATION TERMINATED WITH ',3X,
     .          'TOTAL NONLINEAR ITERATIONS:',I8)
 1003 FORMAT(3X,'* TOTAL NUM.OF MATRIX FACTORIZATION AND PCG ITERATION:'
     .          ,2X,I5,2x,I8)

      DEALLOCATE(IKINE)
c-----------

      RETURN
      END

