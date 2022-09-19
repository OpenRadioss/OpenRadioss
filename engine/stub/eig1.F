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
#ifndef DNC
Chd|====================================================================
Chd|  EIG1                          stub/eig1.F                   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GENANI_MOD                    source/output/anim/generate/genani.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE EIG1(K_DIAG   , K_LT      , IADK        ,JDIK        , MS        ,
     2                IN       , NDDL      , NDOF        ,NNZL        , X         ,
     3                D        , V         , A           ,BUFEL       , IXS       ,
     4                IXQ      , IXC       , IXT         ,IXP         , IXR       ,
     5                IXTG     , PM        , GEO         ,CONT        , ICUT      ,
     6                SKEW     , XCUT      , FINT        ,ITAB        , FEXT      ,
     7                FOPT     , ANIN      , LPBY        ,NPBY        , NSTRF     ,
     8                RWBUF    , NPRW      , IXUR        ,TANI        , ELBUF_TAB ,
     9                DD_IAD   , FR_IAD    , DD_FRONT    ,CLUSTER     ,
     A                WEIGHT   , EANI      , IPART       ,
     B                RBY      , NOM_OPT   , IGRSURF     ,BUFSF       , IDATA     ,
     C                RDATA    , BUFMAT    , BUFGEO      ,KXX         , IXX       ,
     D                KXSP     , IXSP      , NOD2SP      ,SPBUF       , IXS10     ,
     E                IXS20    , IXS16     , VR          ,MONVOL      , VOLMON    ,
     F                IPM      , IGEO      , IPARG       ,              EIGIPM    ,
     G                EIGIBUF  , EIGRPM    , LDIAG       ,LJDIK       , LJDIK2    ,
     H                IKC      , MAXNCV    , THKE        ,NMS         , NINT2     ,
     I                IINT2    , IPARI     , INTBUF_TAB  ,NODGLOB     ,
     J                IAD_ELEM , FR_ELEM   , FR_SEC      ,FR_RBY2     , IAD_RBY2  ,
     K                FR_WALL  , INLOC     , IDDL        ,PARTSAV     , FNCONT    ,
     L                FTCONT   , TEMP      , ERR_THK_SH4 ,ERR_THK_SH3 ,
     M                IRBE2    , IRBE3     , LRBE2       ,LRBE3       , FR_RBE2   ,
     N                FR_RBE3M , IAD_RBE2  , WEIGHT_MD   ,FCLUSTER    , MCLUSTER  ,
     O                XFEM_TAB , W           ,NV46        , NERCVOIS  ,
     P                NESDVOIS , LERCVOIS  ,LESDVOIS     ,CRKEDGE     ,INDX_CRK   ,
     Q                XEDGE4N  ,XEDGE3N    ,STACK        ,SPH2SOL     ,STIFN      ,
     R                STIFR    ,DRAPE_Q4, DRAPE_T3      ,H3D_DATA    ,SUBSET     ,
     S                IGRNOD   ,FCONT_MAX ,FNCONTP2     ,FTCONTP2, ALE_CONNECTIVITY)
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
      USE DRAPE_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "rnur_c.inc"
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
     .   IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*) ,NPRW(*),IXUR(*),
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
     .   WEIGHT_MD(*),NV46,NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),
     .   LESDVOIS(*),SPH2SOL(*)
      INTEGER  IRBE2(NRBE2L,*),IRBE3(NRBE3L,*),LRBE2(*),LRBE3(*),
     .         FR_RBE2(3,*),FR_RBE3M(3,*),IAD_RBE2(4,*)
      my_real
     .   K_DIAG(*) ,K_LT(*), MS(*), IN(*),
     .   PM(NPROPM,*), GEO(*),SKEW(LSKEW,*),VR(3,*),
     .   X(3,*), D(3,*), V(3,*), A(3,*), BUFEL(*),FCLUSTER(*),
     .   CONT(3,*),XCUT(*),FINT(*),EANI(*),RBY(NRBY,*),MCLUSTER(*),
     .   FEXT(3,*) ,FOPT(6,*),ANIN(*),RWBUF(*),TANI(*),
     .   BUFSF(*),RDATA(*),BUFMAT(*),BUFGEO(*),SPBUF(*),VOLMON(*),
     .   EIGRPM(*), THKE(*), PARTSAV(NPSAV,*), FNCONT(*),
     .   FTCONT(*),TEMP(*), ERR_THK_SH4(*), ERR_THK_SH3(*), W(*),
     .   STIFN(*),STIFR(*),FCONT_MAX(*),FNCONTP2(3,*) ,FTCONTP2(3,*)
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, IBC, IADK2(LDIAG+1), CORRESP(2,LDIAG),
     .        CORRESP_IB(2,NMS), IADK_IB(LDIAG+1), ITAG(NUMNOD), IRCM,
     .        NBM, INO, IDL, I, J, K, LEN, IPRI, NM, MAXITR, NBF, NF,
     .        NFC, N, NEV, NCV, MAXN, MAXNEV, LDV, ISHFTS, MODE,
     .        INFO, IDBL, IDF, NFC2, NFC0, NBLO0, NIB, NVAL,ITY,
     .        NBLO, II, IBLO, REDIR(LDIAG), REDIR_IB(NMS),
     .        COLPTR(LDIAG+1), IDIAG(LDIAG), NME, LMR, LRLF, LC, NFR,
     .        TAGRBY(2,EIGIPM(10)), TAGIT2(5,EIGIPM(10)), IM,
     .        NEL3D, NEL2D, NEL1D, NEL, N1, N2, N3, N4, N5, N6, N7, N8,
     .        KUIX, KXUSR ,KFACPTX,KXEDGE,KXFACET,KXSOLID,KNUMX1,KNUMX2,
     .        KNUMX3,KOFFX1,KOFFX2,KOFFX3,KMASS1,KMASS2,KMASS3,
     .        KFUNC1,KFUNC2,KFUNC3,KFIN,
     .        N9, N10, K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, L1,
     .        SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,SN9,NELG,K12,
     .        SKUIX, SKXUSR ,SKFACPTX,SKXEDGE,SKXFACET,SKXSOLID,SKNUMX1,
     .        SKNUMX2,SKNUMX3,SKOFFX1,SKOFFX2,SKOFFX3,SKMASS1,SKMASS2,
     .        SKMASS3,SKFUNC1,SKFUNC2,SKFUNC3,SKFIN,NG,
     .        ID, ISHELL, ITYP, IEIG, IDMAST, IMAST, NUV, NIR,
     .        NSL_RB, ISL_RB, NSL_I2, ISL_I2, IPRIB, IBID, LENG,RIBID(1),IBID_(1)
      my_real
     .        K_DIAG2(LDIAG), VV(6,EIGIPM(10)), SHIFT, TOL, CUTFREQ,
     .        TSHIFT, PULS, FR1, FREQ(MAXNCV,2), FREQ2(EIGIPM(5)),
     .        VECT(EIGIPM(14),MAXNCV), VECTL(EIGIPM(14),EIGIPM(5)),
     .        FR2, KSI, RBID, CMAX, DMAX, SCALE(EIGIPM(5)+NMS+6),
     .        X0(3,NUMNOD), VT(LDIAG), FACIT2(4,EIGIPM(10)),
     .        RX, RY, RZ, VVV(6),RRBID(1)
      CHARACTER WHICH*2
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: JDIK2, JDIK_IB, ROWIND
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: K_LT2, K_IB, KL, ML, KLS,
     .                                      VALUE, BUFEL0
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VECTA, VECT2, VECTR
      my_real
     .   OFF_SAV(MVSIZ,NGROUP)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C=======================================================================
C

      RETURN
      END
#endif

