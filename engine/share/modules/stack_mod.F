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
Chd|  STACK_MOD                     share/modules/stack_mod.F     
Chd|-- called by -----------
Chd|        ANIM_BUILD_INDEX_ALL          source/output/anim/reader/anim_build_index_all.F
Chd|        ANIM_DCOD_KEY_0               source/output/anim/reader/anim_dcod_key_0.F
Chd|        ANIM_RESET_INDEX_ALL          source/output/anim/reader/anim_reset_index_all.F
Chd|        ANIOFFC_PLY                   source/output/anim/generate/anioffc_ply.F
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CMAIN3PINCH                   source/elements/shell/coqueba/cmain3pinch.F
Chd|        CMATC3                        source/elements/shell/coqueba/cmatc3.F
Chd|        CNCOEFORT                     source/elements/sh3n/coquedk/cncoef3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        C_TF_NE                       source/output/sty/c_tf_ne.F   
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|        DMASANIC                      source/output/anim/generate/dmasanic.F
Chd|        DYNAIN_C_STRAG                source/output/dynain/dynain_c_strag.F
Chd|        DYNAIN_C_STRSG                source/output/dynain/dynain_c_strsg.F
Chd|        EIG                           stub/eig.F                    
Chd|        EIG1                          stub/eig1.F                   
Chd|        EIGCOND                       stub/eigcond.F                
Chd|        EIGP                          stub/eigp.F                   
Chd|        FAIL_SETOFF_C                 source/materials/fail/fail_setoff_c.F
Chd|        FAIL_SETOFF_NPG_C             source/materials/fail/fail_setoff_npg_c.F
Chd|        FAIL_SETOFF_WIND_FRWAVE       source/materials/fail/fail_setoff_wind_frwave.F
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|        H3D_LIST_QUAD_SCALAR          source/output/h3d/input_list/h3d_list_quad_scalar.F
Chd|        H3D_LIST_QUAD_TENSOR          source/output/h3d/input_list/h3d_list_quad_tensor.F
Chd|        H3D_LIST_SHELL_SCALAR         source/output/h3d/input_list/h3d_list_shell_scalar.F
Chd|        H3D_LIST_SHELL_TENSOR         source/output/h3d/input_list/h3d_list_shell_tensor.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_QUAD_TENSOR               source/output/h3d/h3d_results/h3d_quad_tensor.F
Chd|        H3D_QUAD_VECTOR               source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|        H3D_SHELL_SCALAR              source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|        H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        H3D_SHELL_VECTOR              source/output/h3d/h3d_results/h3d_shell_vector.F
Chd|        H3D_SHELL_VECTOR_1            source/output/h3d/h3d_results/h3d_shell_vector_1.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SOLID_TENSOR              source/output/h3d/h3d_results/h3d_solid_tensor.F
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        H3D_SPH_SCALAR                source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|        H3D_SPH_TENSOR                source/output/h3d/h3d_results/h3d_sph_tensor.F
Chd|        HIST2                         source/output/th/hist2.F      
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|        INIT_TH0                      source/output/th/init_th0.F   
Chd|        INI_TMAX                      source/output/ini_outmax.F    
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        LECTUR                        source/input/lectur.F         
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        OUTP_C_TF                     source/output/sty/outp_c_t.F  
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|        TENSORC                       source/output/anim/generate/tensorc.F
Chd|        TENSORC_PLY                   source/output/anim/generate/tensorc_ply.F
Chd|        THCOQ                         source/output/th/thcoq.F      
Chd|        UPD_TMAX                      source/output/upd_outmax.F    
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE STACK_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C---------------------------------------------
C Structures needed for stack (PID17) properties 
C---------------------------------------------

C PLY  Nodes structures for animations
      TYPE STACK_PLY
         INTEGER, DIMENSION (:,:), ALLOCATABLE :: IGEO        ! IGEO TABLE
         my_real ,
     .   DIMENSION (:,:), ALLOCATABLE :: GEO   , PM         ! GEO, PM
      END TYPE STACK_PLY
      INTEGER , DIMENSION(:,:), ALLOCATABLE :: 
     .      PLY_INFO
      
c
      END MODULE STACK_MOD

