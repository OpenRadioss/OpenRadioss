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
#ifndef DNC
Chd|====================================================================
Chd|  EIGP                          stub/eigp.F                   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GENANI_MOD                    source/output/anim/generate/genani.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE EIGP(
     1                   EIGIPM      , EIGRPM      , IKC0     , NDDL0     ,
     2                   DIAG_K      , LT_K        , IADK     , JDIK      , MS      ,
     3                   IN          , NDOF        , NDDL     , EIGIBUF   , X       ,
     4                   V           , A           , BUFEL    , IXS       , IXQ     ,
     5                   IXC         , IXT         , IXP      , IXR       , IXTG    ,
     6                   PM          , GEO         , CONT     , ICUT      , SKEW    ,
     7                   XCUT        , FINT        , ITAB     , FEXT      , FOPT    ,
     8                   ANIN        , LPBY        , NPBY     , NSTRF     , RWBUF   ,
     9                   NPRW        , TANI        , ELBUF_TAB, MATPARAM_TAB,DD_IAD    ,
     A                   FR_IAD      , DD_FRONT    , WEIGHT   , EANI      , IPART   ,
     B                   RBY         , NOM_OPT     , IGRSURF  ,
     C                   BUFSF       , IDATA       , RDATA    , BUFMAT    , BUFGEO  ,
     D                   KXX         , IXX         , KXSP     , IXSP      , NOD2SP  ,
     E                   SPBUF       , IXS10       , IXS20    , IXS16     , VR      ,
     F                   MONVOL      , VOLMON      , IPM      , IGEO      , IPARG   ,
     G                   NODGLOB     , IAD_ELEM    , FR_ELEM  , FR_SEC    , FR_RBY2 ,
     H                   IAD_RBY2    , FR_WALL     , NINT2    , IINT2     , IPARI   ,
     I                   INTBUF_TAB  , D           , LDDL     , PARTSAV   ,
     J                   INLOC       , FNCONT      , FTCONT   , TEMP      , THKE    ,
     K                   ERR_THK_SH4 , ERR_THK_SH3 , IRBE2    , IRBE3     ,
     L                   LRBE2       , LRBE3       , FR_RBE2  , FR_RBE3M  , IAD_RBE2,
     M                   WEIGHT_MD   , CLUSTER     , FCLUSTER , MCLUSTER  , XFEM_TAB,
     O                   W           , NV46     , NERCVOIS  , NESDVOIS,
     P                   LERCVOIS    , LESDVOIS    ,CRKEDGE   , INDX_CRK  , XEDGE4N ,
     Q                   XEDGE3N     ,STACK        ,SPH2SOL   ,STIFN      ,STIFR    ,
     R                   DRAPE_Q4    , DRAPE_T3    ,H3D_DATA  ,SUBSET     ,IGRNOD   ,
     S                   FCONT_MAX   ,FNCONTP2     ,FTCONTP2, ALE_CONNECTIVITY)   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GENANI_MOD
      USE ELBUFDEF_MOD         
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD
      USE CRACKXFEM_MOD
      USE STACK_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
      USE DRAPE_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "com_xfem1.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EIGIPM(*), IKC0(*), NDDL0, IADK(*), JDIK(*), NDOF(*),
     .    NDDL, EIGIBUF(*), IXS(*), IXQ(*), IXC(*), IXT(*), IXP(*),     
     .    IXR(*), IXTG(*), ICUT(*), ITAB(*), LPBY(*), NPBY(NNPBY,*),    
     .    NSTRF(*), NPRW(*),INDX_CRK(*),XEDGE4N(4,*),XEDGE3N(3,*),                                   
     .    DD_IAD(NSPMD+1,*), FR_IAD(*), DD_FRONT(4,*), WEIGHT(*),       
     .    IPART(*), NOM_OPT(LNOPT1,*),     
     .    IDATA(*), KXX(NIXX,*), IXX(*),              
     .    KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*), IXS10(6,*),         
     .    IXS20(12,*), IXS16(8,*), MONVOL(*), IPM(*), IGEO(*),          
     .    IPARG(*), NODGLOB(*), IAD_ELEM(2,*), FR_ELEM(*),              
     .    FR_SEC(NSPMD+1,*), FR_RBY2(3,*), IAD_RBY2(4,*),               
     .    FR_WALL(*), NINT2, IINT2(*), IPARI(NPARI,*),         
     .    LDDL(*), INLOC(*), WEIGHT_MD(*),NV46,NERCVOIS(*),
     .    NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),SPH2SOL(*)
      INTEGER IRBE2(NRBE2L,*),IRBE3(NRBE3L,*),LRBE2(*),LRBE3(*),
     .    FR_RBE2(3,*),FR_RBE3M(3,*),IAD_RBE2(4,*)
      my_real
     .        EIGRPM(*), DIAG_K(*), LT_K(*), MS(*), IN(*), X(3,*),
     .        V(3,*), A(3,*), BUFEL(*), PM(NPROPM,*), GEO(*), CONT(3,*),
     .        SKEW(LSKEW,*), XCUT(*), FINT(*), FEXT(*), FOPT(6,*),
     .        ANIN(*),RWBUF(*), TANI(*), EANI(*), RBY(NRBY,*), BUFSF(*),
     .        RDATA(*), BUFMAT(*), BUFGEO(*), SPBUF(*), VOLMON(*),
     .        D(3,*), VR(3,*), PARTSAV(NPSAV,*), FNCONT(*),
     .        FTCONT(*),TEMP(*),THKE(*), ERR_THK_SH4(*), ERR_THK_SH3(*),
     .        FCLUSTER(*),MCLUSTER(*),W(*),STIFN(*),STIFR(*),FCONT_MAX(*),
     .        FNCONTP2(3,*),FTCONTP2(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)    :: CLUSTER
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (STACK_PLY) :: STACK
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)  :: IGRSURF
      TYPE (DRAPE_)   :: DRAPE_Q4, DRAPE_T3
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

      RETURN
      END

#endif
