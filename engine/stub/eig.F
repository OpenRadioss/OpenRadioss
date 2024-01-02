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
#ifndef DNC
Chd|====================================================================
Chd|  EIG                           stub/eig.F                    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GENANI_MOD                    source/output/anim/generate/genani.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE EIG(K_DIAG    , K_LT     , IADK        , JDIK        , MS        ,
     2               IN        , NDDL     , NDOF        , NNZL        , X         ,
     3               D         , V        , A           , BUFEL       , IXS       ,
     4               IXQ       , IXC      , IXT         , IXP         , IXR       ,
     5               IXTG      , PM       , GEO         , CONT        , ICUT      ,
     6               SKEW      , XCUT     , FINT        , ITAB        , FEXT      ,
     7               FOPT      , ANIN     , LPBY        , NPBY        , NSTRF     ,
     8               RWBUF     , NPRW     , TANI        , ELBUF_TAB   ,MATPARAM_TAB,
     9               DD_IAD    , FR_IAD   , DD_FRONT    , CLUSTER     ,
     A               WEIGHT    , EANI     , IPART       , 
     B               RBY       , NOM_OPT  , IGRSURF     , BUFSF       , IDATA     ,
     C               RDATA     , BUFMAT   , BUFGEO      , KXX         , IXX       ,
     D               KXSP      , IXSP     , NOD2SP      , SPBUF       , IXS10     ,
     E               IXS20     , IXS16    , VR          , MONVOL      , VOLMON    ,
     F               IPM       , IGEO     , IPARG       ,               EIGIPM    ,
     G               EIGIBUF   , EIGRPM   , LDIAG       , LJDIK       , LJDIK2    ,
     H               IKC       , MAXNCV   , THKE        , NMS         , NINT2     ,
     I               IINT2     , IPARI    , INTBUF_TAB  , NODGLOB     ,
     J               IAD_ELEM  , FR_ELEM  , FR_SEC      , FR_RBY2     , IAD_RBY2  ,
     K               FR_WALL   , INLOC    , IDDL        , PARTSAV     , FNCONT    ,
     L               FTCONT    , TEMP     , ERR_THK_SH4 , ERR_THK_SH3 ,
     M               IRBE2     , IRBE3    , LRBE2       , LRBE3       , FR_RBE2   ,
     N               FR_RBE3M  , IAD_RBE2 , WEIGHT_MD   , FCLUSTER    , MCLUSTER  ,
     O               XFEM_TAB  , W           , NV46        , NERCVOIS  , 
     P               NESDVOIS  , LERCVOIS , LESDVOIS    ,CRKEDGE      ,INDX_CRK   ,
     Q               XEDGE4N   ,XEDGE3N   ,STACK        ,SPH2SOL      ,STIFN      ,
     R               STIFR     ,DRAPE_Q4  ,DRAPE_T3     ,H3D_DATA     ,SUBSET     ,
     S               IGRNOD    ,FCONT_MAX ,FNCONTP2     ,FTCONTP2, ALE_CONNECTIVITY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GENANI_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD         
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD
      USE CRACKXFEM_MOD
      USE STACK_MOD
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
      USE INOUTFILE_MOD
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
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "com_xfem1.inc"
#include      "sphcom.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "chara_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), NDDL, NDOF(*),
     .   IPARG(NPARG,*), IXS(*), IXQ(*), IXC(*), IXT(*), IXP(*),
     .   IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*) ,NPRW(*),
     .   IPART(*), IXR(*), IXTG(*), ITAB(*),ICUT(*),NSTRF(*),LPBY(*),
     .   NPBY(NNPBY,*),INDX_CRK(*),
     .   DD_IAD(NSPMD+1,*),XEDGE4N(4,*),XEDGE3N(3,*),
     .   FR_IAD(*),DD_FRONT(4,*),WEIGHT(*),
     .   IDATA(*),KXX(NIXX,*),IXX(*),KXSP(NISP,*),IXSP(KVOISPH,*),
     .   NOD2SP(*),IPM(*), IGEO(*),MONVOL(*), EIGIPM(*), EIGIBUF(*),
     .   LDIAG, LJDIK, LJDIK2, NNZL, IKC(*), MAXNCV, NMS,
     .   NINT2, IINT2(*), IPARI(NPARI,*), INLOC(*), IDDL(*),
     .   NODGLOB(*), IAD_ELEM(2,*), FR_ELEM(*), FR_SEC(NSPMD+1,*),
     .   FR_RBY2(3,*), IAD_RBY2(4,*), FR_WALL(*),NOM_OPT(LNOPT1,*),
     .   WEIGHT_MD(*),NV46,SPH2SOL(*)
      INTEGER  IRBE2(NRBE2L,*),IRBE3(NRBE3L,*),LRBE2(*),LRBE3(*),
     .         FR_RBE2(3,*),FR_RBE3M(3,*),IAD_RBE2(4,*),
     .         NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*)

      my_real
     .   K_DIAG(*) ,K_LT(*), MS(*), IN(*),FCLUSTER(*),MCLUSTER(*),
     .   PM(NPROPM,*), GEO(*),SKEW(LSKEW,*),VR(3,*),
     .   X(3,*), D(3,*), V(3,*), A(3,*), BUFEL(*),
     .   CONT(3,*),XCUT(*),FINT(*),EANI(*),RBY(NRBY,*),
     .   FEXT(3,*) ,FOPT(6,*),ANIN(*),RWBUF(*),TANI(*),
     .   BUFSF(*),RDATA(*),BUFMAT(*),BUFGEO(*),SPBUF(*),VOLMON(*),
     .   EIGRPM(*), THKE(*), PARTSAV(NPSAV,*), FNCONT(3,*),
     .   FTCONT(3,*),TEMP(*), ERR_THK_SH4(*),ERR_THK_SH3(*),W(*),
     .   STIFN(*),STIFR(*),FCONT_MAX(*),FNCONTP2(3,*),FTCONTP2(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)    :: CLUSTER
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (STACK_PLY) :: STACK
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(DRAPE_) :: DRAPE_Q4  ,DRAPE_T3
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MATPARAM_TAB

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER COLPTR(LDIAG+1), NNO, I,NG,ITY,
     .        IDIAG(LDIAG), CORRESP(2,LDIAG), CORRESP_IB(2,NMS),
     .        NEV, NCV, N, MAXN, MAXNEV, LDV, ISHFTS,
     .        MAXITR, MODE, PRSP, INFO, J, II, IM, IAD,
     .        NEL3D, NEL2D, NEL1D, NEL, N1, N2, N3, N4, N5, N6, N7, N8,
     .        KUIX, KXUSR ,KFACPTX,KXEDGE,KXFACET,KXSOLID,KNUMX1,KNUMX2,
     .        KNUMX3,KOFFX1,KOFFX2,KOFFX3,KMASS1,KMASS2,KMASS3,
     .        KFUNC1,KFUNC2,KFUNC3,KFIN,
     .        N9, N10, K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, L1,
     .        LENWA_TMP, IADK2(LDIAG+1), IADK_IB(LDIAG+1),K12,
     .        ITYP, NVAL, NBF, NN1, NBLO,
     .        NF, ID, NBLO0, IDF, IDBL, IPRI, NFC, NM, NNI, NELI,NIB,
     .        NFC0, IMA, LDIAG0, III, CORRESP0(2,LDIAG), NFR,
     .        REDIR(LDIAG), REDIR_IB(NMS), TSKD, TSKS, TSKR,
     .        ITMOD, NMOD, FILEN, NME, ISHELL, LMR, LRLF, LC,
     .        TAGRBY(2,EIGIPM(10)), IFACT, IBLO, IDMAST,
     .        IMAST, TAGIT2(5,EIGIPM(10)), IEIG, ISAVE, NFC2, NIR,
     .        NSL_RB, ISL_RB(NUMNOD), NSL_I2, ISL_I2(NUMNOD), K, KK,
     .        IPRIB, INFO_FAC, INOD, IDOF, NVAL2, IBID, LENG,INORM,
     .        RIBID(1),IBID_(1)
      INTEGER
     .        SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,SN9,NELG,
     .        SKUIX, SKXUSR ,SKFACPTX,SKXEDGE,SKXFACET,SKXSOLID,SKNUMX1,
     .        SKNUMX2,SKNUMX3,SKOFFX1,SKOFFX2,SKOFFX3,SKMASS1,SKMASS2,
     .        SKMASS3,SKFUNC1,SKFUNC2,SKFUNC3,SKFIN
      INTEGER(KIND=8) FACTORS(8)
      my_real
     .        TOL, SHIFT, PULS, FR1, SHIFTR,
     .        FR2,X0(3,NUMNOD), K_DIAG2(LDIAG), TSHIFT,
     .        FREQ(MAXNCV,2), FREQ2(EIGIPM(5)),
     .        SCALE(EIGIPM(5)+NMS+6), CMAX, DMAX, KSI, ORIG(3),
     .        RX, RY, RZ, VV(6), FACIT2(4,EIGIPM(10)), CUTFREQ, RBID,
     .        RRBID(1)
C ON PASSE LES GROS TABLEAUX EN ALLOCATABLE (A CAUSE DES RISQUES DE LIMITE
C DE STACK)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ROWIND, JDIK2, JDIK_IB
      my_real, DIMENSION(:), ALLOCATABLE ::
     .   VALUE, K_LT2, K_IB, BUFEL0
      my_real, DIMENSION(:,:), ALLOCATABLE ::
     .                                      VECT, VECT2, VECTS, VECTR
      CHARACTER WHICH*2, CHEIG*4, FILNAM*109
      my_real
     .   OFF_SAV(MVSIZ,NGROUP)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME

        CALL ARRET(5)
      END
#endif
