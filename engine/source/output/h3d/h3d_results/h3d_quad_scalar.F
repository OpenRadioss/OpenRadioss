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
      !||    h3d_quad_scalar              ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||--- called by ------------------------------------------------------
      !||    genh3d                       ../engine/source/output/h3d/h3d_results/genh3d.F
      !||--- calls      -----------------------------------------------------
      !||    h3d_quad_scalar_1            ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    schlieren_buffer_gathering   ../engine/source/output/anim/generate/schlieren_buffer_gathering.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod         ../common_source/modules/ale/ale_connectivity_mod.F
      !||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    h3d_quad_scalar_1_mod        ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    initbuf_mod                  ../engine/share/resol/initbuf.F
      !||    matparam_def_mod             ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    multi_fvm_mod                ../common_source/modules/ale/multi_fvm_mod.F90
      !||    multimat_param_mod           ../common_source/modules/multimat_param_mod.F90
      !||    names_and_titles_mod         ../common_source/modules/names_and_titles_mod.F
      !||    schlieren_mod                ../engine/share/modules/schlieren_mod.F
      !||    stack_mod                    ../engine/share/modules/stack_mod.F
      !||====================================================================
      SUBROUTINE H3D_QUAD_SCALAR(
     .                  ELBUF_TAB      ,QUAD_SCALAR,IPARG        ,
     .                  IXQ            ,PM          ,
     .                  EHOUR          ,
     .                  IPM            ,
     .                  X              ,V           ,W           ,ALE_CONNECT      ,
     .                  NERCVOIS       ,NESDVOIS    ,LERCVOIS    ,LESDVOIS,
     .                  ID_ELEM        ,
     .                  IS_WRITTEN_QUAD,IPARTQ      ,LAYER_INPUT ,NPART1,
     .                  IUVAR_INPUT    ,H3D_PART    ,KEYWORD     ,ITHERM ,
     .                  BUFMAT         ,MULTI_FVM   ,IR_INPUT    ,IS_INPUT    ,IT_INPUT  ,
     .                  ID             ,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD   
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
      USE h3d_quad_scalar_1_mod, ONLY: H3D_QUAD_SCALAR_1
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
      USE MATPARAM_DEF_MOD , ONLY : MATPARAM_STRUCT_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
!NGROUP, N2D
#include      "com01_c.inc"
!NUMELQ, NUMMAT, NUMNOD
#include      "com04_c.inc"
!NPARG, NPROPM, NPROPMI
#include      "param_c.inc"
! ISPMD
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) :: 
     .   QUAD_SCALAR(*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),EHOUR(*),                   
     .   PM(NPROPM,NUMMAT)
      INTEGER IPARG(NPARG,NGROUP),
     .   IXQ(NIXQ,NUMELQ),
     .   IPM(NPROPMI,NUMMAT),
     .   ID_ELEM(*),
     .   IS_WRITTEN_QUAD(*),IPARTQ(*),H3D_PART(*),
     .   LAYER_INPUT ,IUVAR_INPUT,
     .   IR_INPUT,IS_INPUT,IT_INPUT,NPART1
      INTEGER, INTENT(IN) :: ID
      INTEGER, INTENT(IN) :: ITHERM
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER(LEN=NCHARLINE100):: KEYWORD
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      my_real, TARGET :: BUFMAT(*)
      INTEGER, INTENT(in) :: NERCVOIS(*),NESDVOIS(*), LERCVOIS(*),LESDVOIS(*)
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   VALUE(MVSIZ)
      INTEGER NG,ILAY
      INTEGER 
     .        I,IUVAR
C-----------------------------------------------
      ILAY = LAYER_INPUT
      IUVAR = IUVAR_INPUT

      DO I=1,NUMELQ
         IS_WRITTEN_QUAD(I) = 0
      ENDDO
C
      !-------------------------------------------------------!
      !     INITIALIZATION IF SCHLIEREN DEFINED               !
      !-------------------------------------------------------!      
      IF(KEYWORD == 'SCHLIEREN')THEN
        CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM,ITHERM)  
      ENDIF
C-----------
      DO NG=1,NGROUP
        CALL H3D_QUAD_SCALAR_1(.FALSE., NG, 
     .                  M51_N0PHAS     , M51_NVPHAS,NGROUP, N2D, NUMELQ, NUMMAT, NUMNOD, NPARG, NPROPM, NPROPMI, ISPMD,
     .                  ELBUF_TAB      ,QUAD_SCALAR, NUMELQ, IPARG        ,
     .                  IXQ            , NIXQ         ,PM          ,
     .                  EHOUR          ,
     .                  IPM            ,
     .                  X              ,V          ,W           ,ALE_CONNECT      ,
     .                  ID_ELEM        ,
     .                  IS_WRITTEN_QUAD,IPARTQ     ,LAYER_INPUT , NPART1,
     .                  IUVAR_INPUT    ,H3D_PART   ,KEYWORD   ,
     .                  BUFMAT         ,MULTI_FVM  ,
     .                  ID             ,MAT_PARAM)

      ENDDO                 
C-----------------------------------------------
      RETURN
      END

