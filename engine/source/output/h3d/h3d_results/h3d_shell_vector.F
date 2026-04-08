Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    h3d_shell_vector       ../engine/source/output/h3d/h3d_results/h3d_shell_vector.F
!||--- called by ------------------------------------------------------
!||    genh3d                 ../engine/source/output/h3d/h3d_results/genh3d.F
!||--- calls      -----------------------------------------------------
!||    h3d_shell_vector_1     ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    initbuf_mod            ../engine/share/resol/initbuf.F
!||    multi_fvm_mod          ../common_source/modules/ale/multi_fvm_mod.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||    schlieren_mod          ../engine/share/modules/schlieren_mod.F
!||    stack_mod              ../engine/share/modules/stack_mod.F
!||====================================================================
      SUBROUTINE H3D_SHELL_VECTOR(
     .                  ELBUF_TAB   ,SHELL_VECTOR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM         ,
     .                  EL2FA       ,NBF       ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,ITY_ELEM  ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,
     .                  D           ,MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE SCHLIEREN_MOD
      USE STACK_MOD
      USE MULTI_FVM_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
      use element_mod , only : nixq,nixc,nixtg
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
     .   SHELL_VECTOR(3,*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),D(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,NUMMAT),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*), NV46,ID_ELEM(*),ITY_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_SHELL(*),IPARTC(*),IPARTTG(*),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER(LEN=NCHARLINE100)::KEYWORD
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   FAIL(MVSIZ)
      INTEGER I, NG,
     .        NN3,
     .        NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*)
 
C-----------------------------------------------
c      print *,'LAYER_INPUT',LAYER_INPUT
c      print *,'IPT_INPUT',IPT_INPUT
c      print *,'GAUSS_INPUT',GAUSS_INPUT
c      print *,'PLY_INPUT',PLY_INPUT
c      print *,'IUVAR_INPUT',IUVAR_INPUT
c      print *,'KEYWORD',KEYWORD,'KEYWORD'
C
      DO I=1,NUMELC+NUMELTG
         IS_WRITTEN_SHELL(I) = 0
      ENDDO
c a corriger
      NN3 = 0
C
      DO NG=1,NGROUP
       CALL H3D_SHELL_VECTOR_1(
     .                  ELBUF_TAB   ,SHELL_VECTOR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM         ,
     .                  EL2FA       ,NBF       ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,ITY_ELEM  ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,
     .                  D           ,NG         ,MULTI_FVM)
      ENDDO
C-----------------------------------------------
      RETURN
      END

