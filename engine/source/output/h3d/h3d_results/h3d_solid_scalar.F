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
      !||    h3d_solid_scalar             ../engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
      !||--- called by ------------------------------------------------------
      !||    genh3d                       ../engine/source/output/h3d/h3d_results/genh3d.F
      !||--- calls      -----------------------------------------------------
      !||    h3d_solid_scalar_1           ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    schlieren_buffer_gathering   ../engine/source/output/anim/generate/schlieren_buffer_gathering.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod         ../common_source/modules/ale/ale_connectivity_mod.F
      !||    aleanim_mod                  ../engine/share/modules/aleanim_mod.F
      !||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    h3d_mod                      ../engine/share/modules/h3d_mod.F
      !||    initbuf_mod                  ../engine/share/resol/initbuf.F
      !||    matparam_def_mod             ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    names_and_titles_mod         ../common_source/modules/names_and_titles_mod.F
      !||    schlieren_mod                ../engine/share/modules/schlieren_mod.F
      !||====================================================================
      SUBROUTINE H3D_SOLID_SCALAR(
     .                  ELBUF_TAB       ,SOLID_SCALAR ,IPARG       ,
     .                  IXS             ,PM          ,BUFMAT      ,
     .                  EHOUR        ,
     .                  IPM             ,
     .                  X            ,V         ,W           ,ALE_CONNECT,
     .                  NERCVOIS,NESDVOIS, LERCVOIS,LESDVOIS,
     .                  ID_ELEM      ,ITY_ELEM  ,IPARTS      ,LAYER_INPUT ,
     .                  IR_INPUT        ,IS_INPUT     ,IT_INPUT  ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SOLID,INFO1        ,KEYWORD   ,ITHERM      ,FANI_CELL   ,
     .                  MULTI_FVM       , IDMDS     ,IMDSVAR     ,
     .                  ID              ,MAT_PARAM    ,MODE         )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INITBUF_MOD
        USE ELBUFDEF_MOD
        USE SCHLIEREN_MOD
        USE H3D_MOD
        USE MULTI_FVM_MOD
        USE ALE_CONNECTIVITY_MOD
        USE ALEANIM_MOD , ONLY : FANI_CELL_
        USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
        USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real
     .     SOLID_SCALAR(*),X(3,*),V(3,*),W(3,*),EHOUR(*),
     .     PM(NPROPM,*)
        my_real, INTENT(IN),TARGET :: BUFMAT(*)
        INTEGER, INTENT(IN) :: ITHERM
        INTEGER IPARG(NPARG,*),IXS(NIXS,*),
     .     IPM(NPROPMI,*),
     .     ID_ELEM(*),ITY_ELEM(*),IPARTS(*),ID,
     .     H3D_PART(*),IS_WRITTEN_SOLID(*),INFO1,LAYER_INPUT,IR_INPUT,IS_INPUT,IT_INPUT,
     .     IUVAR_INPUT,IDMDS,IMDSVAR,NERCVOIS(*),NESDVOIS(*),
     .          LERCVOIS(*),LESDVOIS(*)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        CHARACTER(LEN=NCHARLINE100) :: KEYWORD
        TYPE(FANI_CELL_),INTENT(IN) :: FANI_CELL
        TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
        TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
        TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
        INTEGER ,INTENT(IN) :: MODE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,NG
C-----------------------------------------------
        DO I=1,NUMELS
          IS_WRITTEN_SOLID(I) = 0
        ENDDO
C
        !-------------------------------------------------------!
        !     INITIALIZATION IF SCHLIEREN DEFINED               !
        !-------------------------------------------------------!
        IF(KEYWORD == 'SCHLIEREN')THEN
          CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM,ITHERM)
        ENDIF
c
        DO 900 NG=1,NGROUP

          CALL H3D_SOLID_SCALAR_1(.FALSE.,
     .                    ELBUF_TAB       ,SOLID_SCALAR ,IPARG       ,
     .                    IXS          ,PM          ,BUFMAT      ,
     .                    EHOUR        ,
     .                    IPM             ,
     .                    X            ,V         ,W           ,ALE_CONNECT,
     .                    ID_ELEM      ,ITY_ELEM  ,IPARTS      ,LAYER_INPUT ,
     .                    IR_INPUT        ,IS_INPUT     ,IT_INPUT  ,IUVAR_INPUT ,H3D_PART    ,
     .                    IS_WRITTEN_SOLID,INFO1        ,KEYWORD   ,FANI_CELL   ,
     .                    MULTI_FVM       , NG        ,IDMDS       ,IMDSVAR     ,
     .                    ID           ,MAT_PARAM ,MODE     )


 900    CONTINUE   ! NG
C-----------------------------------------------
        RETURN
      END
