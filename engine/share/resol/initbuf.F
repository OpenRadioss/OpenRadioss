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
Chd|  INITBUF_MOD                   share/resol/initbuf.F         
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|        AETURB                        source/ale/aeturb.F           
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|        AKTURB                        source/ale/akturb.F           
Chd|        ALE51_FINISH                  source/ale/ale51/ale51_finish.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|        ALEFLUX                       source/ale/aleflux.F          
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        AREZON                        source/ale/arezon.F           
Chd|        ATHERM                        source/ale/atherm.F           
Chd|        CUTFUNCE                      source/tools/sect/cutfunc.F   
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCC_CRK                    source/output/anim/generate/dfuncc_crk.F
Chd|        DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        DFUNGPS1                      source/output/anim/generate/dfuncf.F
Chd|        DFUNGPS2                      source/output/anim/generate/dfuncf.F
Chd|        DMASANI0                      source/output/anim/generate/dmasani0.F
Chd|        DMASANIS                      source/output/anim/generate/dmasani6.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        FORINTS                       source/elements/forints.F     
Chd|        GET_NODAL_IPART               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        GPSSTRAIN_SKIN                source/output/anim/generate/tensgpstrain.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_QUAD_VECTOR               source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|        H3D_SHELL_SCALAR              source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|        H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SHELL_VECTOR              source/output/h3d/h3d_results/h3d_shell_vector.F
Chd|        H3D_SHELL_VECTOR_1            source/output/h3d/h3d_results/h3d_shell_vector_1.F
Chd|        H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_SOL_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        H3D_SKIN_TSH_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SOLID_TENSOR              source/output/h3d/h3d_results/h3d_solid_tensor.F
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        H3D_SOL_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
Chd|        H3D_SOL_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|        H3D_SOL_SKIN_SCALAR1          source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
Chd|        H3D_SOL_SKIN_TENSOR           source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|        H3D_SPH_SCALAR                source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|        H3D_SPH_TENSOR                source/output/h3d/h3d_results/h3d_sph_tensor.F
Chd|        H3D_VELVECC22                 source/output/h3d/h3d_results/h3d_velvecc22.F
Chd|        H3D_VELVECZ22                 source/output/h3d/h3d_results/h3d_velvecz22.F
Chd|        I9GRD2                        source/interfaces/int09/i9grd2.F
Chd|        I9GRD3                        source/interfaces/int09/i9grd3.F
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        LASER2                        source/loads/laser/laser2.F   
Chd|        LASER3                        source/loads/laser/laser2.F   
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        MULTI_COMPUTE_DT              source/multifluid/multi_compute_dt.F
Chd|        MULTI_EVOLVE_GLOBAL           source/multifluid/multi_evolve_global.F
Chd|        MULTI_EVOLVE_PARTIAL          source/multifluid/multi_evolve_partial.F
Chd|        MULTI_FLUXES_COMPUTATION      source/multifluid/multi_fluxes_computation.F
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        MULTI_GLOBALIZE               source/multifluid/multi_globalize.F
Chd|        MULTI_MUSCL_FLUXES_COMPUTATIONsource/multifluid/multi_muscl_fluxes_computation.F
Chd|        MULTI_MUSCL_GRADIENTS         source/multifluid/multi_muscl_gradients.F
Chd|        MULTI_PRESSURE_EQUILIBRIUM    source/multifluid/multi_pressure_equilibrium.F
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|        MULTI_UPDATE_GLOBAL           source/multifluid/multi_update_global.F
Chd|        MULTI_UPDATE_PARTIAL          source/multifluid/multi_update_partial.F
Chd|        NODALD                        source/output/anim/generate/nodald.F
Chd|        NODALP                        source/output/anim/generate/nodalp.F
Chd|        NODALT                        source/output/anim/generate/nodalt.F
Chd|        NODALVFRAC                    source/output/anim/generate/nodalvfrac.F
Chd|        NODALZVOL                     source/output/anim/generate/nodalzvol.F
Chd|        NODAL_SCHLIEREN               source/output/anim/generate/nodal_schlieren.F
Chd|        OUTP_SP_S                     source/output/sty/outp_sp_s.F 
Chd|        OUTP_SP_T                     source/output/sty/outp_sp_t.F 
Chd|        OUTP_SP_TT                    source/output/sty/outp_sp_t.F 
Chd|        OUTP_S_S                      source/output/sty/outp_s_s.F  
Chd|        OUTP_S_T                      source/output/sty/outp_s_t.F  
Chd|        OUTP_S_TT                     source/output/sty/outp_s_t.F  
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|        RGWAT2                        source/interfaces/int09/rgwat2.F
Chd|        RGWAT3                        source/interfaces/int09/rgwat3.F
Chd|        S4ALESFEM                     source/elements/solid/solide4_sfem/s4alesfem.F
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|        SCHLIEREN                     source/output/anim/generate/schlieren.F
Chd|        SEGGETV                       source/interfaces/interf/seggetv.F
Chd|        SIGEPS37_SINGLE_CELL          source/interfaces/int22/sigeps37_single_cell.F
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|        SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|        SPECHAN                       source/elements/sph/spechan.F 
Chd|        SPGAUGE                       source/elements/sph/spgauge.F 
Chd|        SPONFPRS                      source/elements/sph/sponfprs.F
Chd|        SPONOF1                       source/elements/sph/sponof1.F 
Chd|        SPONOF2                       source/elements/sph/sponof2.F 
Chd|        SPWFVIS                       source/elements/sph/spwfvis.F 
Chd|        STAT_S_AUXF                   source/output/sta/stat_s_auxf.F
Chd|        STAT_S_EREF                   source/output/sta/stat_s_eref.F
Chd|        STAT_S_FAIL                   source/output/sta/stat_s_fail.F
Chd|        STAT_S_ORTHO                  source/output/sta/stat_s_ortho.F
Chd|        STAT_S_STRAF                  source/output/sta/stat_s_straf.F
Chd|        STAT_S_STRSF                  source/output/sta/stat_s_strsf.F
Chd|        STRN_TENSCOR3                 source/output/h3d/h3d_results/h3d_strn_tenscor3.F
Chd|        TENSGPS1                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS2                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|        TENSGPS_SKIN                  source/output/anim/generate/tensor6.F
Chd|        TENSOR0                       source/output/anim/generate/tensor0.F
Chd|        TENSORS                       source/output/anim/generate/tensor6.F
Chd|        THQUAD                        source/output/th/thquad.F     
Chd|        THSOL                         source/output/th/thsol.F      
Chd|        THSPH                         source/output/th/thsph.F      
Chd|        VELVECC22                     source/output/anim/generate/velvec.F
Chd|        VELVECZ22                     source/output/anim/generate/velvecz22.F
Chd|        WRITE_CUT_CELL_BUFFER         source/interfaces/int22/write_cut_cell_buffer.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INITBUF_MOD
      CONTAINS
Chd|====================================================================
Chd|  INITBUF                       share/resol/initbuf.F         
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|        AETURB                        source/ale/aeturb.F           
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|        AKTURB                        source/ale/akturb.F           
Chd|        ALE51_FINISH                  source/ale/ale51/ale51_finish.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        AREZON                        source/ale/arezon.F           
Chd|        ATHERM                        source/ale/atherm.F           
Chd|        CUTFUNCE                      source/tools/sect/cutfunc.F   
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCC_CRK                    source/output/anim/generate/dfuncc_crk.F
Chd|        DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        DFUNGPS1                      source/output/anim/generate/dfuncf.F
Chd|        DFUNGPS2                      source/output/anim/generate/dfuncf.F
Chd|        DMASANI0                      source/output/anim/generate/dmasani0.F
Chd|        DMASANIS                      source/output/anim/generate/dmasani6.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        FORINTS                       source/elements/forints.F     
Chd|        GET_NODAL_IPART               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        GPSSTRAIN_SKIN                source/output/anim/generate/tensgpstrain.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_QUAD_VECTOR               source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|        H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SHELL_VECTOR_1            source/output/h3d/h3d_results/h3d_shell_vector_1.F
Chd|        H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_SOL_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        H3D_SKIN_TSH_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        H3D_SOL_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
Chd|        H3D_SOL_SKIN_SCALAR1          source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
Chd|        H3D_SPH_SCALAR                source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|        H3D_SPH_TENSOR                source/output/h3d/h3d_results/h3d_sph_tensor.F
Chd|        I9GRD2                        source/interfaces/int09/i9grd2.F
Chd|        I9GRD3                        source/interfaces/int09/i9grd3.F
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|        LASER2                        source/loads/laser/laser2.F   
Chd|        LASER3                        source/loads/laser/laser2.F   
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        OUTP_SP_S                     source/output/sty/outp_sp_s.F 
Chd|        OUTP_SP_T                     source/output/sty/outp_sp_t.F 
Chd|        OUTP_SP_TT                    source/output/sty/outp_sp_t.F 
Chd|        OUTP_S_S                      source/output/sty/outp_s_s.F  
Chd|        OUTP_S_T                      source/output/sty/outp_s_t.F  
Chd|        OUTP_S_TT                     source/output/sty/outp_s_t.F  
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|        RGWAT2                        source/interfaces/int09/rgwat2.F
Chd|        RGWAT3                        source/interfaces/int09/rgwat3.F
Chd|        S4ALESFEM                     source/elements/solid/solide4_sfem/s4alesfem.F
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|        SEGGETV                       source/interfaces/interf/seggetv.F
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|        SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|        SPECHAN                       source/elements/sph/spechan.F 
Chd|        SPONFPRS                      source/elements/sph/sponfprs.F
Chd|        SPONOF1                       source/elements/sph/sponof1.F 
Chd|        SPONOF2                       source/elements/sph/sponof2.F 
Chd|        SPWFVIS                       source/elements/sph/spwfvis.F 
Chd|        STAT_S_AUXF                   source/output/sta/stat_s_auxf.F
Chd|        STAT_S_EREF                   source/output/sta/stat_s_eref.F
Chd|        STAT_S_FAIL                   source/output/sta/stat_s_fail.F
Chd|        STAT_S_ORTHO                  source/output/sta/stat_s_ortho.F
Chd|        STAT_S_STRAF                  source/output/sta/stat_s_straf.F
Chd|        STAT_S_STRSF                  source/output/sta/stat_s_strsf.F
Chd|        TENSGPS1                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS2                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|        TENSGPS_SKIN                  source/output/anim/generate/tensor6.F
Chd|        TENSOR0                       source/output/anim/generate/tensor0.F
Chd|        TENSORS                       source/output/anim/generate/tensor6.F
Chd|        THQUAD                        source/output/th/thquad.F     
Chd|        THSOL                         source/output/th/thsol.F      
Chd|        THSPH                         source/output/th/thsph.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INITBUF (IPARG    ,NG      ,
     2   MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3   NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4   JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5   NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLA    ,
     6   IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7   ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT (IN)  :: IPARG(NPARG,*),NG
      INTEGER, INTENT (OUT) :: MTN,LLT,NFT,IAD,ITY,NPT,JALE,ISMSTR,
     .   JEUL,JTUR,JTHE,JLAG,NVAUX,JMULT,JHBE,JIVF,JPOR,JPLA,JCLOSE,
     .   IREP,IINT,IGTYP,JCVT,ISROT,ISRAT,ISORTH,ISORTHG,ICSEN,IFAILURE,
     .   JSMS
C-----------------------------------------------
C    Comment : Initialisation des variables de vect01_c.inc
C              dans les boucles sur les groupes.
C              Necessaire pour routines telles que sbufad qui dimensionnent
C              le buffeur elementaire.
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      MTN     = IPARG(1,NG)    
      LLT     = IPARG(2,NG)    
      NFT     = IPARG(3,NG)    
      IAD     = IPARG(4,NG)    
      ITY     = IPARG(5,NG)    
      NPT     = IPARG(6,NG)     
      JALE    = IPARG(7,NG)     

      ISMSTR  = IPARG(9,NG)     

      IF(ITY.EQ.1.OR.ITY.EQ.2)THEN
         JEUL    = IPARG(11,NG) 
      ELSEIF (MTN .EQ. 151 .AND. ITY .EQ. 7) THEN
         JEUL    = IPARG(11,NG) 
      ELSE             ! attention dkt9_s3 utilise ce flag
         JEUL    = 0
      END IF 

      JTUR    = IPARG(12,NG)    
      JTHE    = IPARG(13,NG)    
      JLAG    = IPARG(14,NG)
      NVAUX   = IPARG(18,NG)    

      JMULT   = IPARG(20,NG)    

      JHBE    = IPARG(23,NG)    
      JIVF    = IPARG(24,NG) 
         
      JPOR    = IPARG(27,NG) 
         
      JPLA    = IPARG(29,NG)
      
      JCLOSE  = IPARG(33,NG)
      
      IREP    = IPARG(35,NG)      
      IINT    = IPARG(36,NG)    
      JCVT    = IPARG(37,NG)
      IGTYP   = IPARG(38,NG)
      ICSEN   = IPARG(39,NG)
      ISRAT   = IPARG(40,NG)      
      ISROT   = IPARG(41,NG)       
      IF(IPARG(28,NG) == 10) ISROT = IPARG(74,NG)     
      ISORTH  = IPARG(42,NG)      
      ISORTHG = ISORTH

      IFAILURE = IPARG(43,NG)                  

      JSMS = IPARG(52,NG)
C----
      RETURN
      END SUBROUTINE INITBUF
      END MODULE INITBUF_MOD
