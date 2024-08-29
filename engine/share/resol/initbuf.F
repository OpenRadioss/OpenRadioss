Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||====================================================================
      !||    initbuf_mod                      ../engine/share/resol/initbuf.F
      !||--- called by ------------------------------------------------------
      !||    aconve                           ../engine/source/ale/aconve.F
      !||    aeturb                           ../engine/source/ale/turbulence/aeturb.F
      !||    aflux0                           ../engine/source/ale/aflux0.F
      !||    afluxt                           ../engine/source/ale/ale51/afluxt.F
      !||    agauge                           ../engine/source/ale/agauge.F
      !||    akturb                           ../engine/source/ale/turbulence/akturb.F
      !||    ale51_finish                     ../engine/source/ale/ale51/ale51_finish.F
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    ale51_init                       ../engine/source/ale/ale51/ale51_init.F
      !||    aleflux                          ../engine/source/ale/porous/aleflux.F
      !||    alefvm_main                      ../engine/source/ale/alefvm/alefvm_main.F
      !||    alemain                          ../engine/source/ale/alemain.F
      !||    anim_nodal_p_elems               ../engine/source/output/anim/generate/anim_nodal_p_elems.F
      !||    anim_nodal_ssp_elems             ../engine/source/output/anim/generate/anim_nodal_ssp_elems.F90
      !||    arezon                           ../engine/source/ale/arezon.F
      !||    atherm                           ../engine/source/ale/atherm.F
      !||    cutfunce                         ../engine/source/tools/sect/cutfunce.F
      !||    dfunc0                           ../engine/source/output/anim/generate/dfunc0.F
      !||    dfuncc                           ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncc_crk                       ../engine/source/output/anim/generate/dfuncc_crk.F
      !||    dfuncc_ply                       ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    dfuncs                           ../engine/source/output/anim/generate/dfunc6.F
      !||    dfungps1                         ../engine/source/output/anim/generate/dfuncf.F
      !||    dfungps2                         ../engine/source/output/anim/generate/dfuncf.F
      !||    dmasani0                         ../engine/source/output/anim/generate/dmasani0.F
      !||    dmasanis                         ../engine/source/output/anim/generate/dmasani6.F
      !||    forint                           ../engine/source/elements/forint.F
      !||    forintp                          ../engine/source/elements/forintp.F
      !||    forints                          ../engine/source/elements/forints.F
      !||    get_nodal_ipart                  ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    get_unique_main_cell             ../engine/source/interfaces/int22/get_unique_master_cell.F
      !||    gpsstrain_skin                   ../engine/source/output/anim/generate/tensgpstrain.F
      !||    h3d_quad_scalar                  ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_quad_scalar_1                ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    h3d_quad_vector                  ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
      !||    h3d_shell_scalar                 ../engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
      !||    h3d_shell_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_vector                 ../engine/source/output/h3d/h3d_results/h3d_shell_vector.F
      !||    h3d_shell_vector_1               ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
      !||    h3d_skin_ixskin                  ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_off                     ../engine/source/output/h3d/h3d_results/h3d_skin_off.F
      !||    h3d_skin_scalar                  ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_sol_dim                 ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_tensor                  ../engine/source/output/h3d/h3d_results/h3d_skin_tensor.F
      !||    h3d_skin_tsh_dim                 ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_sol_skin_ixskin              ../engine/source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
      !||    h3d_sol_skin_scalar              ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
      !||    h3d_sol_skin_scalar1             ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
      !||    h3d_sol_skin_tensor              ../engine/source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
      !||    h3d_solid_scalar                 ../engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
      !||    h3d_solid_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    h3d_solid_tensor                 ../engine/source/output/h3d/h3d_results/h3d_solid_tensor.F
      !||    h3d_solid_tensor_1               ../engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
      !||    h3d_solid_vector                 ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
      !||    h3d_sph_scalar                   ../engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
      !||    h3d_sph_tensor                   ../engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
      !||    h3d_velvecc22                    ../engine/source/output/h3d/h3d_results/h3d_velvecc22.F
      !||    h3d_velvecz22                    ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
      !||    i9grd2                           ../engine/source/interfaces/int09/i9grd2.F
      !||    i9grd3                           ../engine/source/interfaces/int09/i9grd3.F
      !||    imp_glob_k                       ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_k0                      ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_khp                     ../engine/source/implicit/imp_glob_k.F
      !||    laser2                           ../engine/source/loads/laser/laser2.F
      !||    laser3                           ../engine/source/loads/laser/laser2.F
      !||    lech3d                           ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    link_with_unique_main_cell       ../engine/source/interfaces/int22/link_with_unique_master_cell.F
      !||    multi_compute_dt                 ../engine/source/multifluid/multi_compute_dt.F
      !||    multi_evolve_global              ../engine/source/multifluid/multi_evolve_global.F
      !||    multi_evolve_partial             ../engine/source/multifluid/multi_evolve_partial.F
      !||    multi_fluxes_computation         ../engine/source/multifluid/multi_fluxes_computation.F
      !||    multi_fvm2fem                    ../engine/source/multifluid/multi_fvm2fem.F
      !||    multi_globalize                  ../engine/source/multifluid/multi_globalize.F
      !||    multi_muscl_fluxes_computation   ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||    multi_muscl_gradients            ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_pressure_equilibrium       ../engine/source/multifluid/multi_pressure_equilibrium.F
      !||    multi_timeevolution              ../engine/source/multifluid/multi_timeevolution.F
      !||    multi_update_global              ../engine/source/multifluid/multi_update_global.F
      !||    multi_update_partial             ../engine/source/multifluid/multi_update_partial.F
      !||    nodal_schlieren                  ../engine/source/output/anim/generate/nodal_schlieren.F
      !||    nodald                           ../engine/source/output/anim/generate/nodald.F
      !||    nodaldt                          ../engine/source/output/anim/generate/nodaldt.F
      !||    nodalp                           ../engine/source/output/anim/generate/nodalp.F
      !||    nodalssp                         ../engine/source/output/anim/generate/nodalssp.F
      !||    nodalt                           ../engine/source/output/anim/generate/nodalt.F
      !||    nodalvfrac                       ../engine/source/output/anim/generate/nodalvfrac.F
      !||    nodalvol                         ../engine/source/output/anim/generate/nodalvol.F
      !||    nodalzvol                        ../engine/source/output/anim/generate/nodalzvol.F
      !||    outp_s_s                         ../engine/source/output/sty/outp_s_s.F
      !||    outp_s_t                         ../engine/source/output/sty/outp_s_t.F
      !||    outp_s_tt                        ../engine/source/output/sty/outp_s_t.F
      !||    outp_sp_s                        ../engine/source/output/sty/outp_sp_s.F
      !||    outp_sp_t                        ../engine/source/output/sty/outp_sp_t.F
      !||    outp_sp_tt                       ../engine/source/output/sty/outp_sp_t.F
      !||    output_schlieren                 ../engine/source/output/anim/generate/output_schlieren.F
      !||    prelecflow                       ../engine/source/elements/solid/solide/prelecflow.F
      !||    rgwat2                           ../engine/source/interfaces/int09/rgwat2.F
      !||    rgwat3                           ../engine/source/interfaces/int09/rgwat3.F
      !||    s4alesfem                        ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
      !||    s4lagsfem                        ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
      !||    schlieren_buffer_gathering       ../engine/source/output/anim/generate/schlieren_buffer_gathering.F
      !||    seggetv                          ../engine/source/interfaces/interf/seggetv.F
      !||    sigeps37_single_cell             ../engine/source/interfaces/int22/sigeps37_single_cell.F
      !||    sinit22_fvm                      ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    soltosphp                        ../engine/source/elements/sph/soltosph.F
      !||    spechan                          ../engine/source/elements/sph/spechan.F
      !||    spgauge                          ../engine/source/elements/sph/spgauge.F
      !||    sponfprs                         ../engine/source/elements/sph/sponfprs.F
      !||    sponof1                          ../engine/source/elements/sph/sponof1.F
      !||    sponof2                          ../engine/source/elements/sph/sponof2.F
      !||    spwfvis                          ../engine/source/elements/sph/spwfvis.F
      !||    stat_s_auxf                      ../engine/source/output/sta/stat_s_auxf.F
      !||    stat_s_eref                      ../engine/source/output/sta/stat_s_eref.F
      !||    stat_s_fail                      ../engine/source/output/sta/stat_s_fail.F
      !||    stat_s_ortho                     ../engine/source/output/sta/stat_s_ortho.F
      !||    stat_s_straf                     ../engine/source/output/sta/stat_s_straf.F
      !||    stat_s_strsf                     ../engine/source/output/sta/stat_s_strsf.F
      !||    strn_tenscor3                    ../engine/source/output/h3d/h3d_results/h3d_strn_tenscor3.F
      !||    tensgps1                         ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps2                         ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps3                         ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps_skin                     ../engine/source/output/anim/generate/tensor6.F
      !||    tensgpstrain                     ../engine/source/output/anim/generate/tensgpstrain.F
      !||    tensor0                          ../engine/source/output/anim/generate/tensor0.F
      !||    tensors                          ../engine/source/output/anim/generate/tensor6.F
      !||    thquad                           ../engine/source/output/th/thquad.F
      !||    thsol                            ../engine/source/output/th/thsol.F
      !||    thsph                            ../engine/source/output/th/thsph.F
      !||    velvecc22                        ../engine/source/output/anim/generate/velvec.F
      !||    velvecz22                        ../engine/source/output/anim/generate/velvecz22.F
      !||    write_cut_cell_buffer            ../engine/source/interfaces/int22/write_cut_cell_buffer.F
      !||====================================================================
      MODULE INITBUF_MOD
      CONTAINS
      !||====================================================================
      !||    initbuf                          ../engine/share/resol/initbuf.F
      !||--- called by ------------------------------------------------------
      !||    aconve                           ../engine/source/ale/aconve.F
      !||    aeturb                           ../engine/source/ale/turbulence/aeturb.F
      !||    aflux0                           ../engine/source/ale/aflux0.F
      !||    afluxt                           ../engine/source/ale/ale51/afluxt.F
      !||    agauge                           ../engine/source/ale/agauge.F
      !||    akturb                           ../engine/source/ale/turbulence/akturb.F
      !||    ale51_finish                     ../engine/source/ale/ale51/ale51_finish.F
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    ale51_init                       ../engine/source/ale/ale51/ale51_init.F
      !||    alefvm_main                      ../engine/source/ale/alefvm/alefvm_main.F
      !||    alemain                          ../engine/source/ale/alemain.F
      !||    arezon                           ../engine/source/ale/arezon.F
      !||    atherm                           ../engine/source/ale/atherm.F
      !||    cutfunce                         ../engine/source/tools/sect/cutfunce.F
      !||    dfuncc                           ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncc_crk                       ../engine/source/output/anim/generate/dfuncc_crk.F
      !||    dfuncc_ply                       ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    dfuncs                           ../engine/source/output/anim/generate/dfunc6.F
      !||    dfungps1                         ../engine/source/output/anim/generate/dfuncf.F
      !||    dfungps2                         ../engine/source/output/anim/generate/dfuncf.F
      !||    dmasani0                         ../engine/source/output/anim/generate/dmasani0.F
      !||    dmasanis                         ../engine/source/output/anim/generate/dmasani6.F
      !||    forint                           ../engine/source/elements/forint.F
      !||    forintp                          ../engine/source/elements/forintp.F
      !||    forints                          ../engine/source/elements/forints.F
      !||    get_nodal_ipart                  ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    gpsstrain_skin                   ../engine/source/output/anim/generate/tensgpstrain.F
      !||    h3d_quad_scalar_1                ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    h3d_quad_vector                  ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
      !||    h3d_shell_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_vector_1               ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
      !||    h3d_skin_ixskin                  ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_off                     ../engine/source/output/h3d/h3d_results/h3d_skin_off.F
      !||    h3d_skin_scalar                  ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_sol_dim                 ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_tensor                  ../engine/source/output/h3d/h3d_results/h3d_skin_tensor.F
      !||    h3d_skin_tsh_dim                 ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_sol_skin_ixskin              ../engine/source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
      !||    h3d_sol_skin_scalar1             ../engine/source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
      !||    h3d_solid_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    h3d_solid_tensor_1               ../engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
      !||    h3d_solid_vector                 ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
      !||    h3d_sph_scalar                   ../engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
      !||    h3d_sph_tensor                   ../engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
      !||    i9grd2                           ../engine/source/interfaces/int09/i9grd2.F
      !||    i9grd3                           ../engine/source/interfaces/int09/i9grd3.F
      !||    imp_glob_k                       ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_k0                      ../engine/source/implicit/imp_glob_k.F
      !||    laser2                           ../engine/source/loads/laser/laser2.F
      !||    laser3                           ../engine/source/loads/laser/laser2.F
      !||    lech3d                           ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    multi_fvm2fem                    ../engine/source/multifluid/multi_fvm2fem.F
      !||    outp_s_s                         ../engine/source/output/sty/outp_s_s.F
      !||    outp_s_t                         ../engine/source/output/sty/outp_s_t.F
      !||    outp_s_tt                        ../engine/source/output/sty/outp_s_t.F
      !||    outp_sp_s                        ../engine/source/output/sty/outp_sp_s.F
      !||    outp_sp_t                        ../engine/source/output/sty/outp_sp_t.F
      !||    outp_sp_tt                       ../engine/source/output/sty/outp_sp_t.F
      !||    prelecflow                       ../engine/source/elements/solid/solide/prelecflow.F
      !||    rgwat2                           ../engine/source/interfaces/int09/rgwat2.F
      !||    rgwat3                           ../engine/source/interfaces/int09/rgwat3.F
      !||    s4alesfem                        ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
      !||    s4lagsfem                        ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
      !||    seggetv                          ../engine/source/interfaces/interf/seggetv.F
      !||    sinit22_fvm                      ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    soltosphp                        ../engine/source/elements/sph/soltosph.F
      !||    spechan                          ../engine/source/elements/sph/spechan.F
      !||    sponfprs                         ../engine/source/elements/sph/sponfprs.F
      !||    sponof1                          ../engine/source/elements/sph/sponof1.F
      !||    sponof2                          ../engine/source/elements/sph/sponof2.F
      !||    spwfvis                          ../engine/source/elements/sph/spwfvis.F
      !||    stat_s_auxf                      ../engine/source/output/sta/stat_s_auxf.F
      !||    stat_s_eref                      ../engine/source/output/sta/stat_s_eref.F
      !||    stat_s_fail                      ../engine/source/output/sta/stat_s_fail.F
      !||    stat_s_ortho                     ../engine/source/output/sta/stat_s_ortho.F
      !||    stat_s_straf                     ../engine/source/output/sta/stat_s_straf.F
      !||    stat_s_strsf                     ../engine/source/output/sta/stat_s_strsf.F
      !||    tensgps1                         ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps2                         ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps3                         ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps_skin                     ../engine/source/output/anim/generate/tensor6.F
      !||    tensgpstrain                     ../engine/source/output/anim/generate/tensgpstrain.F
      !||    tensor0                          ../engine/source/output/anim/generate/tensor0.F
      !||    tensors                          ../engine/source/output/anim/generate/tensor6.F
      !||    thquad                           ../engine/source/output/th/thquad.F
      !||    thsol                            ../engine/source/output/th/thsol.F
      !||    thsph                            ../engine/source/output/th/thsph.F
      !||====================================================================
      SUBROUTINE INITBUF (IPARG    ,NG      ,
     2   MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3   NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4   JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5   MID     ,JPOR    ,JCVT    ,JCLOSE  ,JPLA    ,
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
     .   JEUL,JTUR,JTHE,JLAG,MID,JMULT,JHBE,JIVF,JPOR,JPLA,JCLOSE,
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

      IF(ITY==1.OR.ITY==2)THEN
         JEUL    = IPARG(11,NG) 
      ELSEIF (MTN == 151 .AND. ITY == 7) THEN
         JEUL    = IPARG(11,NG) 
      ELSE             ! attention dkt9_s3 utilise ce flag
         JEUL    = 0
      END IF 

      JTUR    = IPARG(12,NG)    
      JTHE    = IPARG(13,NG)    
      JLAG    = IPARG(14,NG)
      MID     = IPARG(18,NG)    
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
