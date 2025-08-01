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
!||    h3d_shell_scalar             ../engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
!||--- called by ------------------------------------------------------
!||    genh3d                       ../engine/source/output/h3d/h3d_results/genh3d.F
!||--- calls      -----------------------------------------------------
!||    h3d_shell_scalar_1           ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
!||    schlieren_buffer_gathering   ../engine/source/output/anim/generate/schlieren_buffer_gathering.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod         ../common_source/modules/ale/ale_connectivity_mod.F
!||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod                  ../engine/share/resol/initbuf.F
!||    matparam_def_mod             ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    multi_fvm_mod                ../common_source/modules/ale/multi_fvm_mod.F90
!||    names_and_titles_mod         ../common_source/modules/names_and_titles_mod.F
!||    schlieren_mod                ../engine/share/modules/schlieren_mod.F
!||    stack_mod                    ../engine/share/modules/stack_mod.F
!||====================================================================
      SUBROUTINE H3D_SHELL_SCALAR(
     .                  ELBUF_TAB   ,SHELL_STACK   ,IPARG       ,GEO   ,
     .                  IXC         ,IXTG          ,PM          ,BUFMAT, 
     .                  EHOUR       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  X         ,V         ,W           ,ALE_CONNECT,
     .                  NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,ITY_ELEM  , 
     .                  IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,ITHERM     ,
     .                  D           , MULTI_FVM ,IDMDS    ,IMDSVAR     ,MDS_MATID  ,
     .                  ID          ,MODE      ,MAT_PARAM ,H3D_LIGHT   ,MAX_SHELL_STACKSIZE,
     .                  SHELL_STACKSIZE)
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
        USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C N2D,  NGROUP,
#include      "com01_c.inc"
C NUMGEO, NUMELC, NUMELTG, NUMMAT, NUMNOD,
#include      "com04_c.inc"
C NPARG, NPROPG, NNPROPM, NPROPMI, NPROPGI,
#include      "param_c.inc"
C SNERCVOIS, SNESDVOIS, SLERCVOIS, SLESDVOIS
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: MAX_SHELL_STACKSIZE
        REAL(KIND=4),DIMENSION(MAX_SHELL_STACKSIZE) :: SHELL_STACK
        INTEGER , INTENT(INOUT) :: SHELL_STACKSIZE
        my_real
     .     X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),D(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .     PM(NPROPM,NUMMAT),ERR_THK_SH4(NUMELC), ERR_THK_SH3(NUMELTG)
        INTEGER, INTENT(IN) :: ITHERM
        INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),
     .     IPM(NPROPMI,NUMMAT),
     .     IGEO(NPROPGI,NUMGEO), ID_ELEM(*),ITY_ELEM(*),
     .     IS_WRITTEN_SHELL(*),IPARTC(NUMELC),IPARTTG(NUMELTG),H3D_PART(*),
     .     LAYER_INPUT ,IPT_INPUT,PLY_INPUT,IUVAR_INPUT,IDMDS,ID,
     .     MDS_MATID(*),IMDSVAR,NERCVOIS(SNERCVOIS),NESDVOIS(SNESDVOIS),LERCVOIS(SLERCVOIS),LESDVOIS(SLESDVOIS)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        TYPE (STACK_PLY) :: STACK
        CHARACTER(LEN=NCHARLINE100):: KEYWORD
        TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
        my_real,INTENT(IN) :: BUFMAT(*)
        TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
        INTEGER ,INTENT(IN) :: MODE
        TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
        INTEGER ,INTENT(IN) :: H3D_LIGHT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real, DIMENSION(1) :: SCALAR_RESULTS
        INTEGER II,NG
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
c        DO II=1,NUMELC+NUMELTG
c          IS_WRITTEN_SHELL(II) = 0
c        ENDDO
C-----------------------------------------------

        !-------------------------------------------------------!
        !     SCHLIEREN INITIALIZATION (IF DEFINED)             !
        !       DENSITY FOR ALL TRIA ARE STORED IN WA_L         !
        !-------------------------------------------------------!
        ! /TRIA are 2d solid elements (new entity type derived from SH3N buffer, it is currently managed from h3d_shell_* subroutines. It will change in the future.
        IF(KEYWORD == 'SCHLIEREN' .AND. N2D > 0)THEN
          CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM,ITHERM)
        ENDIF
C-----------------------------------------------

        !-------------------------------------------------------!
        !     LOOP OVER ELEM GROUPS & OUTPUT DATA               !
        !-------------------------------------------------------!
        DO NG=1,NGROUP
          CALL H3D_SHELL_SCALAR_1(.FALSE.,
     .                     ELBUF_TAB   ,SCALAR_RESULTS,IPARG       ,GEO        ,
     .                     IXC       ,IXTG      ,PM          ,BUFMAT     ,
     .                     EHOUR       ,
     .                     IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                     X         ,V         ,W           ,ALE_CONNECT      ,
     .                     STACK       ,ID_ELEM   ,ITY_ELEM  ,
     .                     IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                     PLY_INPUT   ,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,
     .                     D           ,NG         ,MULTI_FVM,IDMDS       ,IMDSVAR    ,
     .                     MDS_MATID   ,ID         ,MODE     ,MAT_PARAM   ,
     .                     H3D_LIGHT   ,SHELL_STACK ,MAX_SHELL_STACKSIZE,SHELL_STACKSIZE)

        ENDDO
C-----------------------------------------------


        RETURN
      END
