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
#include "hardware.inc"
C=======================================================================
C     // MEMOIRE PARTAGEE
C=======================================================================
Chd|====================================================================
Chd|  MY_BARRIER                    source/system/machine.F       
Chd|-- called by -----------
Chd|        A22CONV3                      source/ale/ale22/a22conv3.F   
Chd|        ACONVE                        source/ale/aconve.F           
Chd|        ADMDIV                        source/model/remesh/admdiv.F  
Chd|        ADMREGUL                      source/model/remesh/admregul.F
Chd|        ADMVIT                        source/model/remesh/admvit.F  
Chd|        AETURB                        source/ale/aeturb.F           
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|        AFLUX3_INT22_FVM              source/ale/ale22/aflux3_int22_fvm.F
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|        AGRAD0                        source/ale/agrad0.F           
Chd|        AKTURB                        source/ale/akturb.F           
Chd|        ALE51_FINISH                  source/ale/ale51/ale51_finish.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|        ALE51_UPWIND3_INT22           source/ale/ale22/ale51_upwind3_int22.F
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ALESUB2                       source/ale/alesub2.F          
Chd|        ALETHE                        source/ale/alethe.F           
Chd|        ALEW                          source/ale/alew.F             
Chd|        ALEW5                         source/ale/alew5.F            
Chd|        ALEW6                         source/ale/alew6.F            
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|        AL_CONSTRAINTH1               source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINTH2               source/implicit/nl_solv.F     
Chd|        AREZON                        source/ale/arezon.F           
Chd|        ASSPAR2                       source/assembly/asspar2.F     
Chd|        ASSPAR3                       source/assembly/asspar3.F     
Chd|        ASSPAR4                       source/assembly/asspar4.F     
Chd|        ASSPART                       source/assembly/asspart.F     
Chd|        BFGS_H1                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H1P                      source/implicit/imp_bfgs.F    
Chd|        BFGS_H2                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H2P                      source/implicit/imp_bfgs.F    
Chd|        CFIELD_1                      source/loads/general/load_centri/cfield.F
Chd|        CHK11MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|        CHK11MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|        CHK20EMSR3N                   source/interfaces/interf/chkstfn3.F
Chd|        CHK20EMSR3NB                  source/interfaces/interf/chkstfn3.F
Chd|        CHK20MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|        CHK20MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|        CHK23MSR3N                    source/interfaces/interf/chkstfn3.F
Chd|        CHK23MSR3NB                   source/interfaces/interf/chkstfn3.F
Chd|        CHK2MSR3N                     source/interfaces/interf/chkstfn3.F
Chd|        CHK2MSR3NB                    source/interfaces/interf/chkstfn3.F
Chd|        CHK2MSR3NP                    source/interfaces/interf/chkstfn3.F
Chd|        CHKLOAD                       source/interfaces/chkload.F   
Chd|        CHKMSR3N                      source/interfaces/interf/chkstfn3.F
Chd|        CHKMSR3NB                     source/interfaces/interf/chkstfn3.F
Chd|        CNDINT                        source/model/remesh/cndint.F  
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        DYNA_IVFAC                    source/implicit/imp_dyna.F    
Chd|        EFLUX3_INT22_FVM              source/ale/ale22/eflux3_int22_fvm.F
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        GRAVIT                        source/loads/general/grav/gravit.F
Chd|        GRAVIT_FVM_FEM                source/loads/general/grav/gravit_fvm_fem.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I11BUCE_VOX                   source/interfaces/intsort/i11buce.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|        I11TRIVOX                     source/interfaces/intsort/i11trivox.F
Chd|        I16MAIN                       source/interfaces/int16/i16main.F
Chd|        I17BUCE                       source/interfaces/int17/i17buce.F
Chd|        I17BUCE_PENA                  source/interfaces/int17/i17buce.F
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|        I17LAGM                       source/interfaces/int17/i17lagm.F
Chd|        I17MAIN                       source/interfaces/int17/i17main.F
Chd|        I17MAIN_PENA                  source/interfaces/int17/i17main_pena.F
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_S                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_V                source/interfaces/int18/i18main_kine.F
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|        I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|        I22IDENT                      source/interfaces/int22/i22ident.F
Chd|        I22INTERSECT                  source/interfaces/int22/i22intersect.F
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I22SUBVOL                     source/interfaces/int22/i22subvol.F
Chd|        I22TRIVOX                     source/interfaces/intsort/i22trivox.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I23TRIVOX                     source/interfaces/intsort/i23trivox.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I24TRIVOX                     source/interfaces/intsort/i24trivox.F
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|        I25EDGE_SWITCH                source/interfaces/int25/i25edge_switch.F
Chd|        I25GAP3                       source/interfaces/int25/i25gap3.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
Chd|        I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I25NORMP                      source/interfaces/int25/i25norm.F
Chd|        I25OPTCD                      source/interfaces/intsort/i25optcd.F
Chd|        I25OPTCD_E2S                  source/interfaces/intsort/i25optcd_e2s.F
Chd|        I25TAGN                       source/interfaces/int25/i25norm.F
Chd|        I25TRIVOX                     source/interfaces/intsort/i25trivox.F
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_FSA_INV2HP                source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH                  source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVH2                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_FSA_INVHP                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_INISI                     source/implicit/imp_pcg.F     
Chd|        IMP_INIST                     source/implicit/imp_pcg.F     
Chd|        IMP_INIX                      source/implicit/imp_pcg.F     
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        IMP_PRO_P                     source/implicit/imp_pcg.F     
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_UPDST                     source/implicit/imp_pcg.F     
Chd|        INT18_LAW151_UPDATE           source/interfaces/int18/int18_law151_update.F
Chd|        INTER_COLOR_COARSE_VOXEL      source/interfaces/generic/inter_color_coarse_voxel.F
Chd|        INTER_COLOR_VOXEL             source/interfaces/generic/inter_color_voxel.F
Chd|        INTER_COUNT_NODE_CURV         source/interfaces/generic/inter_count_node_curv.F
Chd|        INTER_DEALLOCATE_WAIT         source/interfaces/generic/inter_deallocate_wait.F
Chd|        INTER_MINMAX_NODE             source/interfaces/generic/inter_minmax_node.F
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        MAV_LTGH                      source/implicit/produt_v.F    
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MAV_LTH0                      source/implicit/produt_v.F    
Chd|        MAV_LT_H                      source/implicit/produt_v.F    
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|        MMV_LH                        source/implicit/produt_v.F    
Chd|        MMV_LTH                       source/implicit/produt_v.F    
Chd|        MORTHO_GS                     source/implicit/produt_v.F    
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|        NLOCAL_DTNODA                 source/time_step/nlocal_dtnoda.F
Chd|        PREC5HC_SOLV                  source/implicit/prec_solv.F   
Chd|        PREC5H_SOLV                   source/implicit/prec_solv.F   
Chd|        PREC_SOLVGH                   source/implicit/prec_solv.F   
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        PRODUT_UH                     source/implicit/produt_v.F    
Chd|        PRODUT_UH2                    source/implicit/produt_v.F    
Chd|        PRODUT_VMH                    source/implicit/produt_v.F    
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        RGWAL0                        source/constraints/general/rwall/rgwal0.F
Chd|        RGWALC                        source/constraints/general/rwall/rgwalc.F
Chd|        RGWALL                        source/constraints/general/rwall/rgwall.F
Chd|        RGWALP                        source/constraints/general/rwall/rgwalp.F
Chd|        RGWALS                        source/constraints/general/rwall/rgwals.F
Chd|        S10CNDF1                      source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDF2                      source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDFND                     source/elements/solid/solide10/s10cndf.F
Chd|        S10STFE_POFF                  source/elements/solid/solide10/s10cndf.F
Chd|        SEGGETV                       source/interfaces/interf/seggetv.F
Chd|        SHSUB3                        source/elements/shell/subcycling/shsub3.F
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|        SMS_ADMESH_2                  source/ams/sms_admesh.F       
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F  
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|        SMS_CJOINT_0                  source/ams/sms_cjoint.F       
Chd|        SMS_CJOINT_1                  source/ams/sms_cjoint.F       
Chd|        SMS_CJOINT_2                  source/ams/sms_cjoint.F       
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_INISI                     source/ams/sms_proj.F         
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|        SMS_INIX                      source/ams/sms_proj.F         
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|        SMS_MAV_LT1                   source/ams/sms_pcg.F          
Chd|        SMS_MAV_LT2                   source/ams/sms_pcg.F          
Chd|        SMS_MORTHO_GS                 source/ams/sms_proj.F         
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_PRODUT3                   source/ams/sms_proj.F         
Chd|        SMS_PRODUT_H                  source/ams/sms_proj.F         
Chd|        SMS_PRO_P                     source/ams/sms_proj.F         
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|        SMS_UPDST                     source/ams/sms_proj.F         
Chd|        SOLTOSPHA                     source/elements/sph/soltospha.F
Chd|        SOLTOSPHF                     source/elements/sph/soltosph.F
Chd|        SOLTOSPH_ON1                  source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON2                  source/elements/sph/soltosph_on2.F
Chd|        SPBUC3                        source/elements/sph/spbuc3.F  
Chd|        SPCLASV                       source/elements/sph/spclasv.F 
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|        SPHTRI0                       source/elements/sph/sphtri0.F 
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPSTABS                       source/elements/sph/spstab.F  
Chd|        SPSYMP                        source/elements/sph/spsym.F   
Chd|        SPSYM_ALLOC                   source/elements/sph/spsym_alloc.F
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|        TELESC                        source/constraints/general/cyl_joint/telesc.F
Chd|        UPDATE_STRUCT_INT21           source/interfaces/int21/update_struct_int21.F
Chd|        WRITE_CUT_CELL_BUFFER         source/interfaces/int22/write_cut_cell_buffer.F
Chd|        SET_U_SENS_MAXVALUE           source/user_interface/usensor.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE MY_BARRIER
#include     "comlock.inc"
#include     "my_real.inc"
#include     "task_c.inc"
#include     "units_c.inc"
#if defined(_OPENMP)
!$OMP BARRIER
#endif
        IF(PARALLEL_END.EQ.1) THEN
#if defined(_OPENMP)
          CALL OMP_SET_LOCK(LLOCK(1,1))
          IF(IOCLOSE.EQ.0) THEN
           CLOSE(UNIT=IOUT)
           CLOSE(UNIT=IUHIS)
           IOCLOSE = 1
          ENDIF  
          CALL OMP_UNSET_LOCK(LLOCK(1,1))
#endif
          CALL ARRET(7)
        END IF
      RETURN
      END
C
Chd|====================================================================
Chd|  IDP_LOCK                      source/system/machine.F       
Chd|-- called by -----------
Chd|        ASSPAR2                       source/assembly/asspar2.F     
Chd|        BAFIL2                        source/ale/bimat/bafil2.F     
Chd|        BALPH2                        source/ale/bimat/balph2.F     
Chd|        BEFIL2                        source/ale/bimat/befil2.F     
Chd|        QMASS2                        source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2                    source/elements/solid_2d/quad/qmass2.F
Chd|        USER_LOCK                     source/system/machine.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDP_LOCK(ID)
#include "comlock.inc"
      INTEGER ID
#if defined(_OPENMP)
        CALL OMP_SET_LOCK(LLOCK(1,ID))
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  USER_LOCK                     source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IDP_LOCK                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE USER_LOCK()
      CALL IDP_LOCK(1)
      RETURN
      END
C
Chd|====================================================================
Chd|  IDP_FREE                      source/system/machine.F       
Chd|-- called by -----------
Chd|        ASSPAR2                       source/assembly/asspar2.F     
Chd|        BAFIL2                        source/ale/bimat/bafil2.F     
Chd|        BALPH2                        source/ale/bimat/balph2.F     
Chd|        BEFIL2                        source/ale/bimat/befil2.F     
Chd|        QMASS2                        source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2                    source/elements/solid_2d/quad/qmass2.F
Chd|        USER_FREE                     source/system/machine.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDP_FREE(ID)
#include "comlock.inc"
      INTEGER ID
#if defined(_OPENMP)
        CALL OMP_UNSET_LOCK(LLOCK(1,ID))
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  USER_FREE                     source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IDP_FREE                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE USER_FREE()
      CALL IDP_FREE(1)
      RETURN
      END
C
Chd|====================================================================
Chd|  WAIT_TASK                     source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WAIT_TASK(ITID)
#include      "my_real.inc"
#include      "task_c.inc"
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
Chd|====================================================================
Chd|  CVMGN                         source/system/machine.F       
Chd|-- called by -----------
Chd|        I3FOR3                        source/interfaces/inter3d/i3for3.F
Chd|-- calls ---------------
Chd|====================================================================
     .   FUNCTION CVMGN(X1,X2,X3)
#include      "implicit_f.inc"
      my_real
     .   X1, X2, X3
      IF(X3.EQ.0.) THEN
       CVMGN=X2
      ELSE
       CVMGN=X1
      ENDIF
      RETURN
      END
      my_real
Chd|====================================================================
Chd|  CVMGT                         source/system/machine.F       
Chd|-- called by -----------
Chd|        M2LAWT                        source/materials/mat/mat002/m2lawt.F
Chd|        SIGEPS44T                     source/materials/mat/mat044/sigeps44t.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  SYNC_DATA                     source/system/machine.F       
Chd|-- called by -----------
Chd|        ADMREGUL                      source/model/remesh/admregul.F
Chd|        I10OPTCD                      source/interfaces/intsort/i10optcd.F
Chd|        I11OPTCD                      source/interfaces/intsort/i11optcd.F
Chd|        I20OPTCD                      source/interfaces/intsort/i20optcd.F
Chd|        I20OPTCDE                     source/interfaces/intsort/i20optcd.F
Chd|        I23OPTCD                      source/interfaces/int23/i23optcd.F
Chd|        I24OPTCD                      source/interfaces/intsort/i24optcd.F
Chd|        I25OPTCD                      source/interfaces/intsort/i25optcd.F
Chd|        I25OPTCD_E2S                  source/interfaces/intsort/i25optcd_e2s.F
Chd|        I25OPTCD_EDG                  source/interfaces/intsort/i25optcd_edg.F
Chd|        I7OPTCD                       source/interfaces/intsort/i7optcd.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SYNC_DATA(II)
      INTEGER II(*)
      RETURN
      END
C=======================================================================
C  ROUTINE ETIME
C=======================================================================
Chd|====================================================================
Chd|  MY_ETIME                      source/system/machine.F       
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|        DSKCOND                       source/implicit/dsolve/dscond.F
Chd|        DSMCOND                       source/implicit/dsolve/dscond.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  MY_FLUSH                      source/system/machine.F       
Chd|-- called by -----------
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MY_FLUSH(IUNIT)
C appel Fortran flush
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IUNIT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C call a rendre specifique machine en cas de pb de compilation
#if CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd
Cow pas de routine FLUSH sur AIX
#elif 1
      CALL FLUSH(IUNIT)
#endif
      RETURN
      END

Chd|====================================================================
Chd|  MKL_INIT                      source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MKL_INIT(MKL_DYN,OMP_NEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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

Chd|====================================================================
Chd|  MKL_RESET                     source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MKL_RESET(MKL_DYN,OMP_NEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
Chd|====================================================================
Chd|  CHECK_KMP_STACK               source/system/machine.F       
Chd|-- called by -----------
Chd|        SET_STACK_VALUES              source/system/machine.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHECK_KMP_STACK()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
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

#endif
      RETURN
      END
Chd|====================================================================
Chd|  GET_KMP_STACK                 source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_KMP_STACK( )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
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
Chd|====================================================================
Chd|  CHECK_KMP_STACK               source/system/machine.F       
Chd|-- called by -----------
Chd|        SET_STACK_VALUES              source/system/machine.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHECK_KMP_STACK( )
      RETURN
      END
C
Chd|====================================================================
Chd|  GET_KMP_STACK                 source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_KMP_STACK( )
      RETURN
      END
#endif

Chd|====================================================================
Chd|  SET_STACK_VALUES              source/system/machine.F       
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        CHECK_KMP_STACK               source/system/machine.F       
Chd|        SET_STACK_C                   source/system/stacksize_c.c   
Chd|====================================================================
      SUBROUTINE SET_STACK_VALUES( )
#include      "task_c.inc"
C Check & Set Stacksize value
       INTEGER Stack

       CALL SET_STACK_C(Stack)
       STACKSIZ = Stack

C Check KMP_STACKSIZE Value
       CALL  CHECK_KMP_STACK( )
      END

Chd|====================================================================
Chd|  WRITE_IOUT                    source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITE_IOUT(LINE,LEN1)
#include "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
#ifndef ncharline
#define ncharline 500
#endif
      CHARACTER(len=ncharline)  LINE
      INTEGER LEN1

#include "lockon.inc"
      WRITE(IOUT,'(A)') LINE(1:LEN1)
#include "lockoff.inc"

      END
C-----------------------------------------------

