Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    stack_mod                      ../engine/share/modules/stack_mod.F
      !||--- called by ------------------------------------------------------
      !||    anim_build_index_all           ../engine/source/output/anim/reader/anim_build_index_all.F
      !||    anim_dcod_key_0                ../engine/source/output/anim/reader/anim_dcod_key_0.F
      !||    anim_reset_index_all           ../engine/source/output/anim/reader/anim_reset_index_all.F
      !||    anioffc_ply                    ../engine/source/output/anim/generate/anioffc_ply.F
      !||    c3forc3                        ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk                    ../engine/source/elements/xfem/c3forc3_crk.F
      !||    c3ke3                          ../engine/source/elements/sh3n/coque3n/c3ke3.F
      !||    c_tf_ne                        ../engine/source/output/sty/c_tf_ne.F
      !||    cbaforc3                       ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cbake3                         ../engine/source/elements/shell/coqueba/cbake3.F
      !||    cdk6forc3                      ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3                       ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3                         ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk                     ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3                         ../engine/source/materials/mat_share/cmain3.F
      !||    cmain3pinch                    ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    cmatc3                         ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    cncoefort                      ../engine/source/elements/sh3n/coquedk/cncoef3.F
      !||    czforc3                        ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk                    ../engine/source/elements/xfem/czforc3_crk.F
      !||    czke3                          ../engine/source/elements/shell/coquez/czke3.F
      !||    dfuncc                         ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncc_ply                     ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    dmasanic                       ../engine/source/output/anim/generate/dmasanic.F
      !||    dynain_c_strag                 ../engine/source/output/dynain/dynain_c_strag.F
      !||    dynain_c_strsg                 ../engine/source/output/dynain/dynain_c_strsg.F
      !||    eig                            ../engine/stub/eig.F
      !||    eig1                           ../engine/stub/eig1.F
      !||    eigcond                        ../engine/stub/eigcond.F
      !||    eigp                           ../engine/stub/eigp.F
      !||    fail_setoff_c                  ../engine/source/materials/fail/fail_setoff_c.F
      !||    fail_setoff_npg_c              ../engine/source/materials/fail/fail_setoff_npg_c.F
      !||    fail_setoff_wind_frwave        ../engine/source/materials/fail/fail_setoff_wind_frwave.F
      !||    forintc                        ../engine/source/elements/forintc.F
      !||    freanim                        ../engine/source/output/anim/reader/freanim.F
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    genani                         ../engine/source/output/anim/generate/genani.F
      !||    gendynain                      ../engine/source/output/dynain/gendynain.F
      !||    genh3d                         ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genoutp                        ../engine/source/output/sty/genoutp.F
      !||    genstat                        ../engine/source/output/sta/genstat.F
      !||    h3d_list_quad_scalar           ../engine/source/output/h3d/input_list/h3d_list_quad_scalar.F
      !||    h3d_list_quad_tensor           ../engine/source/output/h3d/input_list/h3d_list_quad_tensor.F
      !||    h3d_list_shell_scalar          ../engine/source/output/h3d/input_list/h3d_list_shell_scalar.F
      !||    h3d_list_shell_tensor          ../engine/source/output/h3d/input_list/h3d_list_shell_tensor.F
      !||    h3d_quad_scalar                ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_quad_tensor                ../engine/source/output/h3d/h3d_results/h3d_quad_tensor.F
      !||    h3d_quad_vector                ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
      !||    h3d_read                       ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    h3d_shell_scalar               ../engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
      !||    h3d_shell_scalar_1             ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_tensor               ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
      !||    h3d_shell_vector               ../engine/source/output/h3d/h3d_results/h3d_shell_vector.F
      !||    h3d_shell_vector_1             ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
      !||    h3d_solid_tensor               ../engine/source/output/h3d/h3d_results/h3d_solid_tensor.F
      !||    h3d_solid_tensor_1             ../engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
      !||    h3d_solid_vector               ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
      !||    h3d_sph_scalar                 ../engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
      !||    h3d_sph_tensor                 ../engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
      !||    hist2                          ../engine/source/output/th/hist2.F
      !||    imp_buck                       ../engine/source/implicit/imp_buck.F
      !||    imp_chkm                       ../engine/source/implicit/imp_solv.F
      !||    imp_glob_k                     ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_k0                    ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_khp                   ../engine/source/implicit/imp_glob_k.F
      !||    imp_k_eig                      ../engine/stub/imp_k_eig.F
      !||    imp_solv                       ../engine/source/implicit/imp_solv.F
      !||    ini_tmax                       ../engine/source/output/ini_outmax.F
      !||    init_th                        ../engine/source/output/th/init_th.F
      !||    init_th0                       ../engine/source/output/th/init_th0.F
      !||    layini                         ../engine/source/elements/shell/coque/layini.F
      !||    lech3d                         ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lectur                         ../engine/source/input/lectur.F
      !||    mulawc                         ../engine/source/materials/mat_share/mulawc.F90
      !||    outp_c_tf                      ../engine/source/output/sty/outp_c_t.F
      !||    radioss2                       ../engine/source/engine/radioss2.F
      !||    rdresb                         ../engine/source/output/restart/rdresb.F
      !||    resol                          ../engine/source/engine/resol.F
      !||    resol_head                     ../engine/source/engine/resol_head.F
      !||    resol_init                     ../engine/source/engine/resol_init.F
      !||    restalloc                      ../engine/source/output/restart/arralloc.F
      !||    sortie_main                    ../engine/source/output/sortie_main.F
      !||    stat_c_strafg                  ../engine/source/output/sta/stat_c_strafg.F
      !||    stat_c_strsfg                  ../engine/source/output/sta/stat_c_strsfg.F
      !||    tensorc                        ../engine/source/output/anim/generate/tensorc.F
      !||    tensorc_ply                    ../engine/source/output/anim/generate/tensorc_ply.F
      !||    thcoq                          ../engine/source/output/th/thcoq.F
      !||    upd_tmax                       ../engine/source/output/upd_outmax.F
      !||    usermat_shell                  ../engine/source/materials/mat_share/usermat_shell.F
      !||    wrrestp                        ../engine/source/output/restart/wrrestp.F
      !||====================================================================
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
      CONTAINS

      !||====================================================================
      !||    stack_ply_alloc   ../engine/share/modules/stack_mod.F
      !||--- called by ------------------------------------------------------
      !||    radioss2          ../engine/source/engine/radioss2.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg            ../engine/source/output/message/message.F
      !||    arret             ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod       ../engine/share/message_module/message_mod.F
      !||====================================================================
        SUBROUTINE STACK_PLY_ALLOC(STACK,IPART_STACK,NPT_STACK,NS_STACK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
          USE MESSAGE_MOD
C-----------------------------------------------------------------------
          IMPLICIT NONE
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          TYPE(STACK_PLY),INTENT(INOUT) :: STACK
          INTEGER,INTENT(IN) :: IPART_STACK
          INTEGER,INTENT(IN) :: NPT_STACK
          INTEGER,INTENT(IN) :: NS_STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          INTEGER IERR
C-----------------------------------------------
          IF(IPART_STACK >0) THEN

               
               ALLOCATE(STACK%IGEO(4*NPT_STACK+2,NS_STACK),STAT=IERR)
               IF(IERR/=0) THEN
                  CALL ANCMSG(MSGID=19,ANMODE=ANINFO,C1='(/PROP/STACK/...)')
                  CALL ARRET(2)
               ENDIF
               STACK%IGEO = 0

               ALLOCATE(STACK%GEO(6*NPT_STACK+1,NS_STACK),STAT=IERR)
               IF(IERR/=0) THEN
                   CALL ANCMSG(MSGID=19,ANMODE=ANINFO,C1='(/PROP/STACK/...)')
                   CALL ARRET(2)
               ENDIF
               STACK%GEO = 0.0

               ALLOCATE(STACK%PM(20,NS_STACK),STAT=IERR)
               IF(IERR/=0) THEN
                   CALL ANCMSG(MSGID=19,ANMODE=ANINFO,C1='(/PROP/STACK/...)')
                   CALL ARRET(2)
               ENDIF
               STACK%PM = 0.0
          ELSE
               ALLOCATE(STACK%IGEO(0,0),STACK%GEO(0,0),STACK%PM(0,0))
          ENDIF 

        END SUBROUTINE STACK_PLY_ALLOC


      END MODULE STACK_MOD

