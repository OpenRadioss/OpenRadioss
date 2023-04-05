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
#if defined(MUMPS5) && defined(DNC)
Chd|====================================================================
Chd|  IMP_BUCK                      source/implicit/imp_buck.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RW                         source/constraints/general/impvel/fv_imp0.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|        INI_KIC                       source/implicit/imp_solv.F    
Chd|        MUMPS_SET2                    source/implicit/imp_mumps.F   
Chd|        RECUDIS                       source/implicit/recudis.F     
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|        SPMD_CDDL                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_DS_RRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_RSEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_INF_G                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_INI                source/mpi/implicit/imp_spmd.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        ZEROR                         source/system/zero.F          
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GENANI_MOD                    source/output/anim/generate/genani.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|        IMP_WORKG                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE IMP_BUCK(
     2                    PM          , GEO         , IPM      , IGEO     , ELBUF     ,
     3                    IXS         , IXQ         , IXC      , IXT      , IXP       ,
     4                    IXR         , IXTG        , IXTG1    , IPARG     ,
     5                    TF          , NPC         , FR_WAVE  , W16      , BUFMAT    ,
     6                    THKE        , BUFGEO      ,SENSOR_TAB, RBY       ,
     7                    SKEW        , WA          ,ICODT     , ICODR    , ISKEW     ,
     9                    IBFV        , VEL         , LPBY     , NPBY     , ITAB      ,
     A                    WEIGHT      , MS          , IN       ,IPARI    , INTBUF_TAB ,  
     B                    X           ,ITASK        ,
     E                    CONT        , ICUT        , XCUT     , FINT     , FEXT      ,
     F                    FOPT        , ANIN        , NSTRF    , RWBUF    , NPRW      ,
     G                    TANI        , DD_IAD      , EANI     , IPART    ,
     H                    NOM_OPT     , IGRSURF     , BUFSF    , IDATA    ,
     I                    RDATA       , KXX         , IXX      , KXSP     , IXSP      ,
     J                    NOD2SP      , SPBUF       , IXS10    , IXS20    , IXS16     ,
     K                    VR          , MONVOL      , VOLMON   , NODGLOB  , IAD_ELEM  ,
     L                    FR_ELEM     , FR_SEC      , FR_RBY2  , IAD_RBY2 , FR_WALL   ,
     M                    V           , A           , GRAPHE   , PARTSAV  , XFRAME    ,
     N                    DIRUL        , 
     O                    FNCONT      ,FTCONT       , TEMP     , SH4TREE  , SH3TREE   ,
     P                    ERR_THK_SH4 ,ERR_THK_SH3  , IFRAME   , LPRW     ,
     P                    ELBUF_TAB   ,FSAV         , FSAVD    , RWSAV    , AR        ,
     R                    IRBE3       ,LRBE3        , FRBE3    , FR_I2M   , IAD_I2M   ,
     S                    FR_RBE3M    ,IAD_RBE3M    , FRWL6    , IBCL     , FORC      ,
     T                    IRBE2       ,LRBE2        , IAD_RBE2 , FR_RBE2  , WEIGHT_MD ,
     U                    CLUSTER     ,FCLUSTER     , MCLUSTER , XFEM_TAB ,
     V                    ALE_CONNECT ,W            , NV46     , NERCVOIS , NESDVOIS  ,
     W                    LERCVOIS    ,LESDVOIS     ,CRKEDGE   , STACK    ,DIMFB      ,
     X                    FBSAV6      ,STABSEN      ,TABSENSOR ,INDX_CRK  ,XEDGE4N    ,
     Y                    XEDGE3N     ,SPH2SOL      ,STIFN     ,STIFR     ,DRAPE_SH4N   ,
     Z                    DRAPE_SH3N    ,H3D_DATA     ,SUBSET    ,IGRNOD    ,FCONT_MAX  ,
     a                    FNCONTP2    ,FTCONTP2     ,NDDL0     ,NNZK0     ,IMPBUF_TAB ,
     b                    DRAPEG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE GENANI_MOD
      USE IMP_WORKG
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
      USE IMPBUFDEF_MOD
      USE SENSOR_MOD
      USE ANIM_MOD
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
#include      "sphcom.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "impl1_c.inc"
#include      "buckcom.inc"
#include      "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL0, NNZK0,  IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IXS(*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),INDX_CRK(*),   
     .        IXTG1(4,*), IPARG(NPARG,*),
     .        NPC(*), ICODT(*), ICODR(*), ISKEW(*), IBFV(NIFV,*),
     .        LPBY(*), NPBY(NNPBY,*), ITAB(*),
     .        WEIGHT(*),IPARI(NPARI,*),ITASK, ICUT(*), NSTRF(*), NPRW(*),
     .        DD_IAD(NSPMD+1,*), IPART(*),
     .        NOM_OPT(LNOPT1,*), IDATA(*),KXX(NIXX,*),
     .        IXX(*), KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*),
     .        IXS10(6,*), IXS20(12,*), IXS16(8,*), MONVOL(*),
     .        NODGLOB(*), IAD_ELEM(2,*), FR_ELEM(*),
     .        FR_SEC(NSPMD+1,*), FR_RBY2(3,*), IAD_RBY2(4,*),
     .        FR_WALL(*),DIRUL(*),SH4TREE(*),SH3TREE(*),
     .        WEIGHT_MD(*),NV46,NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),
     .        LESDVOIS(*),XEDGE4N(4,*),XEDGE3N(3,*),SPH2SOL(*)
      INTEGER IFRAME(LISKN,*),LPRW(*), IRBE3(*),LRBE3(*),
     .        FR_I2M(*),IAD_I2M(*),FR_RBE3M(*),IAD_RBE3M(*),IBCL(*),
     .        IRBE2(*),LRBE2(*),IAD_RBE2(*),FR_RBE2(*),
     .        DIMFB,STABSEN,TABSENSOR(*)
      my_real
     .        PM(NPROPM,*), GEO(NPROPG,*),
     .        ELBUF(*), TF(*), W16(*), BUFMAT(*),
     .        THKE(*), BUFGEO(*),RBY(*),
     .        SKEW(LSKEW,*),  WA(*), VEL(LFXVELR,*), MS(*),
     .        IN(*),FR_WAVE(*), CONT(3,*),
     .        XCUT(*), FINT(*), FEXT(3,*), FOPT(6,*), ANIN(*), RWBUF(*),
     .        TANI(*), EANI(*), BUFSF(*), RDATA(*), SPBUF(*), VR(3,*),
     .        VOLMON(*), X(3,*), V(3,*), A(3,*), PARTSAV(NPSAV,*),
     .        XFRAME(NXFRAME,*),
     .        FNCONT(3,*),FTCONT(3,*),TEMP(*), ERR_THK_SH4(*),
     .        ERR_THK_SH3(*),FRBE3(*),FORC(*),FCLUSTER(*),MCLUSTER(*),
     .        FNCONTP2(3,*) ,FTCONTP2(3,*)
      my_real
     .        FSAV(NTHVKI,*) ,FSAVD(NTHVKI,*), RWSAV(*),AR(3,*),W(*),
     .        STIFN(*),STIFR(*),FCONT_MAX(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
      DOUBLE PRECISION
     .        FRWL6(*)
      DOUBLE PRECISION
     .        FBSAV6(12,6,DIMFB)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)    :: CLUSTER
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (STACK_PLY) :: STACK
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SUBSET_) , DIMENSION(NSUBS)  :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)  :: IGRSURF
      TYPE(DRAPE_)    :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (IMPBUF_STRUCT_) ,TARGET :: IMPBUF_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE(DRAPEG_)    :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NM, NMMAX, MAXITR, N, MAXNCV, NEV, NCV, MAXN,
     .        MAXNEV, LDV, ISHFTS, MODE, INFO, PRSP,
     .        NEL3D, NEL2D, NEL1D, NEL, N1, N2, N3, N4, N5, N6, N7, N8,
     .        N9, N10, K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, L1,
     .        L2, L3, LI1, LI2, LI3, LI4, LI5, LI6, LI7, LI8, LI9, LI10,
     .        LI11, LI12,LI13,LI14,LI15, NT_RW, IPRI, NDDL_INI0,K12,
     .        SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,SN9,NELG,
     .        SKUIX, SKXUSR ,SKFACPTX,SKXEDGE,SKXFACET,SKXSOLID,SKNUMX1,
     .        SKNUMX2,SKNUMX3,SKOFFX1,SKOFFX2,SKOFFX3,SKMASS1,SKMASS2,
     .        SKMASS3,SKFUNC1,SKFUNC2,SKFUNC3,SKFIN,
     .        IBID, IBID1, IBID2,  INFO_FAC, J, NNZL, NTMP,
     .        NNMAX, NKMAX, IWKLI, IPMESS, IOPT, IRQTAG, MSGOFF, NDDLC,
     .        INO, II, NBLF, LTITR1, LENG, NDDLI7, MULTN(NUMNOD),
     .        MULTD(NDDL0), IACTI(NDDL0), CDDLP(NDDL0), JJ, ND, ID, NKC,
     .        NDDLG0, NNZKG0, NDDLG, NNZKG, NNMAXG, NDDL0P(NSPMD),
     .        NNZK0P(NSPMD), NDDLP(NSPMD), NNZKP(NSPMD), NKLOC,NNDL,
     .        NKFRONT, NKFLOC, NZLOC, NNZ, NNMAXP(NSPMD), NN,RIBID(1),IBID_(1)
      my_real
     .        SHIFT, TOL, CMAX, X0(3,NUMNOD), D(3,NUMNOD), DR(3,NUMNOD),
     .        DMAX, SCALE, RBID, BBID(NDDL0), CMAXP, DMAXP, MASS(NDDL0),
     .        TOL0, RRBID(1)
      CHARACTER*2 WHICH, TITRE*80
      INTEGER, DIMENSION(:), ALLOCATABLE :: ROWIND, COLPTR
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITK
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: VALUE_OP,
     .                                      VALUE_K, VALUE_KG,
     .                                      DIAG_OP, LT_OP, RTK
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VECT, EIG, VECTD
      INTEGER, POINTER     :: NDDL,NNZK,NRBYAC,NINT2,NMC,NMC2,NMONV
      INTEGER, DIMENSION(:) ,POINTER     :: IADK,JDIK
      INTEGER, DIMENSION(:) ,POINTER     :: IDDL,NDOF,INLOC,LSIZE,IKC,
     .                                      IRBYAC,NSC,IINT2,NKUD,IMONV,
     .                                      IKINW,IKUD
      my_real, DIMENSION(:) ,POINTER     :: DIAG_K,LT_K,BKUD,ELBUF_C,BUFMAT_C
      my_real, DIMENSION(:) ,POINTER     :: D_IMP,DR_IMP, LB
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      DATA MSGOFF /100000/
C-----------------------------------------------
          NDDL => IMPBUF_TAB%NDDL
          NNZK => IMPBUF_TAB%NNZK
          NRBYAC => IMPBUF_TAB%NRBYAC
          NINT2 => IMPBUF_TAB%NINT2
          NMC => IMPBUF_TAB%NMC
          NMC2 => IMPBUF_TAB%NMC2
          NMONV => IMPBUF_TAB%NMONV
          IADK => IMPBUF_TAB%IADK
          JDIK => IMPBUF_TAB%JDIK
          IDDL => IMPBUF_TAB%IDDL
          NDOF => IMPBUF_TAB%NDOF
          INLOC => IMPBUF_TAB%INLOC
          LSIZE => IMPBUF_TAB%LSIZE
          IRBYAC => IMPBUF_TAB%IRBYAC
          NSC => IMPBUF_TAB%NSC
          IINT2 => IMPBUF_TAB%IINT2
          NKUD => IMPBUF_TAB%NKUD
          IMONV => IMPBUF_TAB%IMONV
          IKINW => IMPBUF_TAB%IKINW
          IKC => IMPBUF_TAB%IKC
          IKUD => IMPBUF_TAB%IKUD
C         
          DIAG_K  =>IMPBUF_TAB%DIAG_K  
          LT_K    =>IMPBUF_TAB%LT_K    
          BKUD    =>IMPBUF_TAB%BKUD    
          D_IMP   =>IMPBUF_TAB%D_IMP   
          DR_IMP  =>IMPBUF_TAB%DR_IMP   
          ELBUF_C =>IMPBUF_TAB%ELBUF_C 
          BUFMAT_C=>IMPBUF_TAB%BUFMAT_C
          LB      =>IMPBUF_TAB%LB
C------------------------------------------------------------
      L1 = 1+NIXS*NUMELS
      L2 = L1+6*NUMELS10
      L3 = L2+12*NUMELS20
citask0      IF (ITASK == 0) THEN

       IF (ISPMD==0) THEN
         WRITE(IOUT,*)
         WRITE(IOUT,*)' ** BUCKLING MODES COMPUTATION **'
         WRITE(ISTDO,*)
         WRITE(ISTDO,*)' ** BUCKLING MODES COMPUTATION **'
         WRITE(IOUT,*)
         WRITE(ISTDO,*)
       ENDIF
      NDDLI7=0
      IBID=0
      RIBID(1) = 0
      RRBID(1) = ZERO
C
      IF (IBUCKL==0) THEN
       IF (NRWALL>0) THEN
         CALL ANCMSG(MSGID=75,ANMODE=ANINFO,
     .            C1='RIGID WALLS')
         CALL ARRET(2)
       ENDIF
       NT_RW=0
       DO I=1,NUMNOD
        N1 = 3*(I-1)+1
        N2 = 3*(I-1)+2
        N3 = 3*(I-1)+3
        X(1,I)=X(1,I)-D_IMP(N1)
        X(2,I)=X(2,I)-D_IMP(N2)
        X(3,I)=X(3,I)-D_IMP(N3)
       ENDDO
       CALL ZEROR(V,NUMNOD)
       CALL ZEROR(A,NUMNOD)
      ENDIF
C Calcul de la matrice de rigidite geometrique
      ALLOCATE(DIAG_KG(NDDL0), LT_KG(NNZK0))
       DIAG_KG=ZERO
       LT_KG  =ZERO
      NDDL=NDDL0
      NNZK=NNZK0
      NDDL_L = NDDL
      NNMAX=LSIZE(9)
      NKMAX=LSIZE(10)
      NMC2=LSIZE(11)
C
      LI1 =1
      LI2 = LI1+LSIZE(4)
      LI3 = LI2+LSIZE(5)
      LI4 = LI3+LSIZE(1)
      LI5 = LI4+LSIZE(3)
      LI6 = LI5+LSIZE(7)
      LI7 = LI6+LSIZE(2)
      LI8 = LI7+LSIZE(6)
      LI9 = LI8+NINT2
      LI10 = LI9+LSIZE(8)
      LI11 = LI10+(LSIZE(8)-LCOKM)*LSIZE(9)
      LI12 = LI11+LCOKM*LSIZE(10)
      LI13 = LI12+4*LSIZE(11)
      LI14 = LI13+LSIZE(14)
      LI15 = LI14+LSIZE(15)
C
      NTMP=0

citask0citask0      END IF !(ITASK == 0) THEN

      IF (IBUCKL>0) THEN

citask0       IF (ITASK == 0) THEN

        IF(NFXVEL/=0) THEN
          CALL FV_IMP(IBFV  ,NPC    ,TF     ,VEL   ,SENSOR_TAB,
     1                D_IMP  ,DR_IMP ,IKC   ,IDDL  ,
     2                SKEW  ,IFRAME ,XFRAME ,V     ,VR    ,
     3                X     ,DIRUL  ,NDOF   ,A     ,AR    )
        ENDIF
C-------------
        NT_RW=0
        NRWDONE = 0
           IMCONV = 1
        IF (NRWALL>0) THEN
         CALL RGWAL0_IMP(
     1     X           ,D_IMP    ,V      ,RWBUF   ,LPRW    ,
     2     NPRW        ,MS       ,FSAV(1,NINTER+1),FR_WALL ,
     3     FOPT        ,RWSAV    ,WEIGHT          ,
     4     FSAVD(1,NINTER+1),
     5     NT_RW       ,IDDL     ,IKC    ,IMCONV,NDOF,FRWL6,
     6     WEIGHT_MD   ,DIMFB    , FBSAV6,STABSEN,TABSENSOR)
         IF(NT_RW>0) THEN
          CALL FV_RW(IDDL   ,IKC   ,NDOF  ,D_IMP  ,V )
         ENDIF
        ENDIF

citask0       END IF !(ITASK == 0) THEN
C
       NGDONE = 1
       IKG=0
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
       CALL IMP_GLOB_KHP(
     1   PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2   IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3   IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,
     4   IXS(L2)   ,IXS(L3)   ,IPARG     ,TF        ,NPC       ,
     5   FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6   RBY       ,SKEW      ,X         ,
     7   WA        ,IDDL      ,NDOF      ,DIAG_K    ,LT_K      ,
     8   IADK      ,JDIK      ,IKG       ,IBID      ,ITASK     ,
     9   ELBUF_TAB ,STACK     ,DRAPE_SH4N, DRAPE_SH3N   ,DRAPEG )
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/

citask0       IF (ITASK == 0) THEN

       CALL UPD_GLOB_K(
     1   ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2   TF        ,VEL       ,XFRAME    ,
     3   RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4   ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5   IRBYAC    ,NSC       ,IKINW(LI1),NMC       ,IKINW(LI2),
     6   IKINW(LI3),IKINW(LI4),NINT2     ,IINT2     ,IKINW(LI8),
     7   IKINW(LI5),IKINW(LI6),IKINW(LI7),IPARI     ,INTBUF_TAB,
     8   NDDL      ,NNZK      ,IADK      ,JDIK      ,
     9   DIAG_K    ,LT_K      ,NDOF      ,IDDL      ,IKC       ,
     A   D_IMP     ,LB        ,NKUD      ,IKUD      ,BKUD      ,
     B   NMC2      ,IKINW(LI12),NT_RW    ,DR_IMP    ,DIRUL     ,
     C   IRBE3     ,LRBE3     ,FRBE3     ,IKINW(LI13),IRBE2    ,
     D   LRBE2     ,IKINW(LI14),IKINW(LI15))
C
       IF (NSPMD>1) THEN
         CALL UPD_FR_K(
     1    IADK     ,JDIK     ,NDOF      ,IKC      ,IDDL     ,
     2    INLOC    ,FR_ELEM  ,IAD_ELEM  ,NDDL     )
       ENDIF
       NDDL=NDDL0
       NNZK=NNZK0

citask0       END IF !(ITASK == 0) THEN

      ENDIF !IF (IBUCKL>0) THEN
citask0      IF (ITASK == 0) THEN
      CALL IND_GLOB_K(NPBY  ,LPBY      ,
     1   ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,IKINW(LI1),
     2   NMC       ,IKINW(LI2),IKINW(LI3),IKINW(LI4),NINT2     ,
     3   IINT2     ,IPARI     ,INTBUF_TAB ,IKINW(LI8),IKINW(LI5),
     4   IKINW(LI6),IKINW(LI7),IPARG     ,ELBUF     ,ELBUF_TAB ,
     5   IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     6   IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,IXS(L2)   ,
     7   IXS(L3)   ,IDDL      ,NDOF      ,IADK      ,
     8   JDIK      ,NDDL      ,NNZK      ,NNMAX     ,LSIZE(8)  ,
     9   INLOC     ,NKMAX     ,IKINW(LI9),IKINW(LI10),IKINW(LI11),
     A   NMC2      ,IKINW(LI12),NTMP     ,LSIZE(12) ,LSIZE(13) ,
     B   FR_ELEM   ,IAD_ELEM  ,IPM       ,IGEO      ,IRBE3     ,
     C   LRBE3     ,IKINW(LI13),FR_I2M   ,IAD_I2M   ,FR_RBE3M  ,
     D   IAD_RBE3M ,IRBE2     ,LRBE2     ,IKINW(LI14),IKINW(LI15))
citask0      END IF !(ITASK == 0) THEN
C
      NGDONE = 1
      IKG=1
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
      CALL IMP_GLOB_KHP(
     1   PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2   IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3   IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,
     4   IXS(L2)   ,IXS(L3)   ,IPARG     ,TF        ,NPC       ,
     5   FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6   RBY       ,SKEW      ,X         ,
     7   WA        ,IDDL      ,NDOF      ,DIAG_KG   ,LT_KG     ,
     8   IADK      ,JDIK      ,IKG       ,IBID      ,ITASK     ,
     9   ELBUF_TAB ,STACK     ,DRAPE_SH4N, DRAPE_SH3N   ,DRAPEG   )
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/

citask0      IF (ITASK == 0) THEN

C--------include load-stiffness matrix-in Kg---------
      IF (IKPRES>0) THEN
         CALL IMP_KPRES(IBCL  ,FORC   ,NPC   ,TF    ,X     ,
     2                  SKEW  ,SENSOR_TAB,WEIGHT,IBID  ,
     3                  IDDL  ,NDOF   ,IADK  ,JDIK  ,DIAG_KG,
     4                  LT_KG )
      END IF
      CALL UPD_GLOB_K(
     1   ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2   TF        ,VEL       ,XFRAME    ,
     3   RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4   ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5   IRBYAC    ,NSC       ,IKINW(LI1),NMC       ,IKINW(LI2),
     6   IKINW(LI3),IKINW(LI4),NINT2     ,IINT2     ,IKINW(LI8),
     7   IKINW(LI5),IKINW(LI6),IKINW(LI7),IPARI     ,INTBUF_TAB,
     8   NDDL      ,NNZK      ,IADK      ,JDIK      ,
     9   DIAG_KG   ,LT_KG     ,NDOF      ,IDDL      ,IKC       ,
     A   D_IMP     ,LB        ,NKUD      ,IKUD      ,BKUD      ,
     B   NMC2      ,IKINW(LI12),NT_RW    ,DR_IMP    ,DIRUL     ,
     C   IRBE3     ,LRBE3     ,FRBE3     ,IKINW(LI13),IRBE2    ,
     D   LRBE2     ,IKINW(LI14),IKINW(LI15))
C
       IF (NSPMD>1) THEN
         CALL UPD_FR_K(
     1    IADK     ,JDIK     ,NDOF      ,IKC      ,IDDL     ,
     2    INLOC    ,FR_ELEM  ,IAD_ELEM  ,NDDL     )
       ENDIF
C
       DO I=1,NDDL
         DIAG_KG(I)=-(DIAG_KG(I)-DIAG_K(I))
       ENDDO
       DO I=1,NNZK
         LT_KG(I)=-(LT_KG(I)-LT_K(I))
       ENDDO
C
      IF (BISOLV>=2) THEN
         WRITE(6,*) "/IMPL/GRAPH is deprecated"
         CALL ARRET(5)
      ENDIF
C Calcul des charges critiques et des modes de flambement
      NM=BINCV
      NMMAX=BMAXNCV
      MAXITR=BNITER
      N=NDDL
      MAXNCV=NMMAX*NBUCK
      SHIFT=SHFTBUCK
      NEV=NBUCK
      NCV=NM*NEV
C
      IF (NSPMD==1) NCV=MIN(NCV,N)
      MAXN=N
      MAXNEV=NEV
      LDV=MAXN
      WHICH='LM'
      ISHFTS=1
      MODE=4
      INFO=0
      TOL=ZERO
      IPRI=BIPRI
C
       ALLOCATE(VECT(LDV,MAXNCV), EIG(MAXNCV,2))
C
c       IF (BISOLV==1.AND.NSPMD==1) THEN
cC Resolution BCS (mono)
c         WRITE(6,*) "BCS Solver not available" 
c         CALL FLUSH(6)
c         CALL ARRET(5)
C
       IF (BISOLV==1) THEN
C Resolution MUMPS (SPMD)
         ALLOCATE(DIAG_OP(NDDL), LT_OP(NNZK))
         DO I=1,NDDL
            DIAG_OP(I)=DIAG_K(I)-SHIFT*DIAG_KG(I)
         ENDDO
         DO I=1,NNZK
            LT_OP(I)=LT_K(I)-SHIFT*LT_KG(I)
         ENDDO
C
         DO I=1,NUMNOD
            MULTN(I)=1
         ENDDO
         DO I=1,NSPMD
            IF (I==ISPMD+1) CYCLE
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               JJ=FR_ELEM(J)
               MULTN(JJ)=MULTN(JJ)+1
            ENDDO
         ENDDO
         NKC=0
         DO NN=1,NUMNOD
            I=INLOC(NN)
            DO J=1,NDOF(I)
               ND=IDDL(I)+J
               ID=ND-NKC
               IF (IKC(ND)<1) THEN
                  MULTD(ID)=MULTN(I)
               ELSE
                  NKC=NKC+1
               ENDIF
            ENDDO
         ENDDO
C
         CALL SPMD_MUMPS_INI(MUMPS_PAR, 2)
C
         MUMPS_PAR%ICNTL(3)=IOUT
         MUMPS_PAR%ICNTL(4)=1
         IF (NSPMD>1) THEN
            MUMPS_PAR%ICNTL(18)=3
C
            NDDLG0 = NDDL0
            NNZKG0 = 0
            NDDLG = NDDL
            NNZKG = NNZK
            NNMAXG = 0
            CALL SPMD_INF_G(
     1       NDDLG0   ,NNZKG0  ,NDDLG    ,NNZKG    ,NNMAXG    ,
     2       NDDL0P   ,NNZK0P  ,NDDLP    ,NNZKP    ,NNMAXP    )
C
            CALL SPMD_CDDL(NDDL,  NODGLOB, IDDL,  NDOF,  CDDLP,
     .                     INLOC, IKC,     NDDLG, NDDLP)
C
            ALLOCATE(ITK(2,NDDL+NNZK), RTK(NDDL+NNZK))
C
            DO I=1,NDDL
               IACTI(I)=I
            ENDDO
C
            CALL INI_KIC
            CALL MUMPS_SET2(
     .            IADK,  JDIK,     DIAG_OP, LT_OP, CDDLP,
     .            NKLOC, NKFRONT,  ITK,     RTK,   IDDL,
     .            INLOC, IAD_ELEM, FR_ELEM, NDOF,  IKC,
     .            NDDL,  NNZK,     IACTI  , NDDLI7,NDDLI7,
     .            IBID , IBID    , IBID   , RBID, RBID )
C
c           CALL SPMD_MUMPS_FRONT(
c    .            ITK,   RTK, NKFRONT, NKFLOC, NKLOC,
c    .            NDDLG, 1  )
C
            NKFLOC = 0
            NZLOC=NKLOC+NKFLOC
            ALLOCATE(MUMPS_PAR%A_LOC(NZLOC),
     .               MUMPS_PAR%IRN_LOC(NZLOC),
     .               MUMPS_PAR%JCN_LOC(NZLOC))
            IF (ISPMD==0) THEN
             ALLOCATE(MUMPS_PAR%RHS(NDDLG))
            ELSE
             ALLOCATE(MUMPS_PAR%RHS(0))
            ENDIF
            MUMPS_PAR%N=NDDLG
            MUMPS_PAR%NZ_LOC=NZLOC
C
            DO I=1,NZLOC
               MUMPS_PAR%IRN_LOC(I)=ITK(1,I)
               MUMPS_PAR%JCN_LOC(I)=ITK(2,I)
               MUMPS_PAR%A_LOC(I)=RTK(I)
            ENDDO
            DEALLOCATE(ITK, RTK)
         ELSE
            MUMPS_PAR%ICNTL(18)=0
C
            DO I=1,NDDL
               CDDLP(I)=I
            ENDDO
C
            ALLOCATE(MUMPS_PAR%A(NDDL+NNZK),
     .               MUMPS_PAR%IRN(NDDL+NNZK),
     .               MUMPS_PAR%JCN(NDDL+NNZK),
     .               MUMPS_PAR%RHS(NDDL))
C
            NNZ=0
            DO I=1,NDDL
               NNZ=NNZ+1
               MUMPS_PAR%IRN(NNZ)=I
               MUMPS_PAR%JCN(NNZ)=I
               MUMPS_PAR%A(NNZ)=DIAG_OP(I)
               DO J=IADK(I),IADK(I+1)-1
                  JJ=JDIK(J)
                  NNZ=NNZ+1
                  MUMPS_PAR%IRN(NNZ)=I
                  MUMPS_PAR%JCN(NNZ)=JJ
                  MUMPS_PAR%A(NNZ)=LT_OP(J)
               ENDDO
            ENDDO
C
            IF (ISPMD==0) THEN
               WRITE(ISTDO,*)
               WRITE(ISTDO,'(A21,I8,A8,I8)')
     .   ' MUMPS    DIM : NNZ =',NNZ,' NNZFR =',0
            ENDIF
C
            MUMPS_PAR%N=NDDL
            MUMPS_PAR%NZ=NNZ
            NDDLG=NDDL
         ENDIF
C
#ifdef DNC
         CALL EIGBUCKP(N,       NEV,    NCV,      WHICH,     INFO,
     .                 MAXN,    MAXNEV, MAXNCV,   LDV,       ISHFTS,
     .                 MAXITR,  MODE,   TOL,      IADK,      JDIK,
     .                 DIAG_K,  LT_K,   DIAG_KG,  LT_KG,     EIG,
     .                 VECT,    IPRI,   SHIFT,    MUMPS_PAR, CDDLP,
     .                 NDDL,    MULTD )
#endif
C
         DEALLOCATE(DIAG_OP, LT_OP)
      ELSEIF (BISOLV==2) THEN
         WRITE(6,*) "/IMPL/GRAPH is deprecated"
         CALL ARRET(5)
      ENDIF
C Sortie des charges critiques dans le listing pour les solveur 1 et 2
      IF ((NSPMD==1.OR.ISPMD==0).AND.BISOLV==1) THEN
         WRITE(IOUT,'(A6,1PG11.4,A35,I10)')
     .    'SHIFT ',SHIFT,' NUMBER OF BUCKLING CRITICAL LOADS ',NBUCK
         WRITE(IOUT,'(A)') ' CRITICAL LOADS:'
         WRITE(IOUT,'(A)') '         NUMBER  CRITICAL LOAD'
         DO I=1,NBUCK
            WRITE(IOUT,'(5X,I10,4X,1PG11.4)') I,EIG(I,1)
         ENDDO
         WRITE(IOUT,*)
      ENDIF
C Sortie des modes de flambement sur l'ANIM et/ou sur OUTP
      CMAX=ZERO
      DO I=1,NUMNOD
         X0(1,I)=X(1,I)
         X0(2,I)=X(2,I)
         X0(3,I)=X(3,I)
         CMAX=MAX(CMAX,ABS(X(1,I)))
         CMAX=MAX(CMAX,ABS(X(2,I)))
         CMAX=MAX(CMAX,ABS(X(3,I)))
      ENDDO
C
      IF (NSPMD>1) THEN
         IF (ISPMD==0) THEN
            DO I=1,NSPMD-1
               IRQTAG=MSGOFF + I
               CALL SPMD_DS_RRECV(CMAXP, 1, IRQTAG, I+1)
               CMAX=MAX(CMAX,CMAXP)
            ENDDO
         ELSE
            IRQTAG=MSGOFF + ISPMD
            CALL SPMD_DS_RSEND(CMAX, 1, IRQTAG, 1)
         ENDIF
         CALL SPMD_RBCAST(CMAX, CMAX, 1, 1, 0, 2)
      ENDIF
C
      NEL3D = NUMELS + NUMSPH + 3*NUMELS16
      NEL2D = NUMELC + NUMELTG + NUMELQ 
      NEL1D = NUMELT + NUMELP  + 2*NUMELR + NANIM1D
      NEL = MAX(NEL1D,NEL2D,NEL3D)
      NELG = MAX (  NUMELSG+3*NUMELS16G+NUMSPHG,
     .              NUMELCG+NUMELTGG+NUMELQG,
     .              NUMELTG + NUMELPG  + 2*NUMELRG)
C
      SN1 = MAX(3*NUMNOD,6*NEL3D,3*NEL2D,9*NEL1D,NUMSPH)
      SN2 = NEL+3*NUMELS16+NUMSPH
      SN3 = 3 * NUMNOD + 2*NUMELS16
      SN4 = NPART + 1
      SN5 = NEL2D
      SN6 = NPART
      SN7 = NELG+1
C
      N1 = 1
      N2 = N1 + MAX(3*NUMNOD,6*NEL3D,3*NEL2D,9*NEL1D)
      N3 = N2 + NEL
      N4 = N3 + 3 * NUMNOD
      N5 = N4 + NPART + 1
      N6 = N5 + NEL2D
      N7 = N6 + NPART
      N8 = N7 + NEL + 1
      IF (NUMELX>0) THEN
         SKUIX = 2*MAXNX
         SKXUSR = 3*MAXNX
         SKFACPTX = NPART
         SKXEDGE = 2*NANIM1D
         SKXFACET = 4*NANIM2D
         SKXSOLID = 8*NANIM3D
         SKNUMX1 = NANIM1D
         SKNUMX2 = NANIM2D
         SKNUMX3 = NANIM3D
         SKOFFX1 = NANIM1D
         SKOFFX2 = NANIM2D
         SKOFFX3 = NANIM3D
         SKMASS1 = NANIM1D
         SKMASS2 = NANIM2D
         SKMASS3 = NANIM3D
         SKFUNC1 = 10*NANIM1D
         SKFUNC2 = 10*NANIM2D
         SKFUNC3 = 10*NANIM3D
      ELSE
         SKUIX = 1
         SKXUSR = 1
         SKFACPTX = 1
         SKXEDGE = 1
         SKXFACET = 1
         SKXSOLID = 1
         SKNUMX1 = 1
         SKNUMX2 = 1
         SKNUMX3 = 1
         SKOFFX1 = 1
         SKOFFX2 = 1
         SKOFFX3 = 1
         SKMASS1 = 1
         SKMASS2 = 1
         SKMASS3 = 1
         SKFUNC1 = 1
         SKFUNC2 = 1
         SKFUNC3 = 1
      ENDIF
      SN9 =  NPART
C
      K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
      K2=K1+NUMELS
      K3=K2+NUMELQ
      K4=K3+NUMELC
      K5=K4+NUMELT
      K6=K5+NUMELP
      K7=K6+NUMELR
      K8=K7
      K9=K8+NUMELTG
      K10=K9+NUMELX
      K11=K10+NUMSPH
      K12=K11+NUMELIG3D
      L1=1+6*(NUMELC+NUMELTG)*IEPSDOT
C------------D,DR peut utiliser D_IMP,DR_IMP---
      DO I=1,NUMNOD
         D(1,I)=ZERO
         D(2,I)=ZERO
         D(3,I)=ZERO
         DR(1,I)=ZERO
         DR(2,I)=ZERO
         DR(3,I)=ZERO
      ENDDO
C
      DO I=1,NBUCK
         CALL RECUDIS(NDDL, IDDL, NDOF,  IKC, VECT(1,I),
     .                D,    DR,   INLOC)
C
         CALL RECUKIN(RBY,    LPBY,   NPBY,    SKEW,  ISKEW,
     .                ITAB,   WEIGHT, MS,      IN,
     .                IBFV,   VEL,    ICODT  , ICODR,
     .                NRBYAC, IRBYAC, NINT2,   IINT2, IPARI,
     .                INTBUF_TAB    , NDOF,    D,     DR,
     .                X    ,  XFRAME , DIRUL,  IXR    ,IXC  ,
     .                IXTG ,SH4TREE ,SH3TREE,   IRBE3 ,LRBE3,
     7                FRBE3 , IRBE2 ,LRBE2  )
C
         DMAX=ZERO
         DO J=1,NUMNOD
            DMAX=MAX(DMAX,ABS(D(1,J)))
            DMAX=MAX(DMAX,ABS(D(2,J)))
            DMAX=MAX(DMAX,ABS(D(3,J)))
         ENDDO
C
         IF (NSPMD>1) THEN
            IF (ISPMD==0) THEN
               DO J=1,NSPMD-1
                  IRQTAG=MSGOFF + NSPMD + J
                  CALL SPMD_DS_RRECV(DMAXP, 1, IRQTAG, J+1)
                  DMAX=MAX(DMAX,DMAXP)
               ENDDO
               SCALE=ZERO
               IF (DMAX>ZERO) SCALE=ONE/DMAX
            ELSE
               IRQTAG=MSGOFF + NSPMD + ISPMD
               CALL SPMD_DS_RSEND(DMAX, 1, IRQTAG, 1)
            ENDIF
              CALL SPMD_RBCAST(SCALE, SCALE, 1, 1, 0, 2)
         ELSE
            SCALE=ZERO
            IF (DMAX>ZERO) SCALE=ONE/DMAX
         ENDIF
C
         DO J=1,NUMNOD
            X(1,J)=X0(1,J)+SCALE*D(1,J)
            X(2,J)=X0(2,J)+SCALE*D(2,J)
            X(3,J)=X0(3,J)+SCALE*D(3,J)
         ENDDO
C
         IF (DTANIM>ZERO) THEN
            IANIM=IANIM+1
            TT=EIG(I,1)
            CALL GENANI(
     1             X          ,D         ,V          ,A           ,ELBUF     ,
     2             IXS        ,IXQ       ,IXC        ,IXT         ,IXP       ,
     3             IXR        ,IXTG      ,SN1        ,SN2         ,SN3       ,
     4             SN4        ,IPARG     ,PM         ,GEO         ,MS        ,
     5             SN5        ,CONT      ,SN6        ,ICUT        ,SKEW      ,
     6             XCUT       ,FINT      ,ITAB       ,SN7         ,FEXT      ,
     7             FOPT       ,ANIN      ,LPBY       ,NPBY        ,NSTRF     ,
     8             RWBUF      ,NPRW      ,TANI        ,ELBUF_TAB ,
     A             DD_IAD     ,WEIGHT    ,EANI       ,IPART       ,CLUSTER   ,
     B             IPART(K1)  ,IPART(K2) ,IPART(K3)  ,IPART(K4)   ,IPART(K5) ,
     C             IPART(K6)  ,IPART(K7) ,IPART(K8)  ,
     D             RBY        ,SN3       ,TANI(L1)   ,NOM_OPT     ,IGRSURF   ,
     E             BUFSF      ,IDATA     ,RDATA      ,SN9         ,BUFMAT    ,
     F             BUFGEO     ,KXX       ,IXX        ,IPART(K9)   ,SKUIX     ,
     G             SKXUSR     ,SKFACPTX  ,SKXEDGE    ,SKXFACET    ,SKXSOLID  ,
     H             SKNUMX1    ,SKNUMX2   ,SKNUMX3    ,SKOFFX1     ,SKOFFX2   ,
     I             SKOFFX3    ,SKMASS1   ,SKMASS2    ,SKMASS3     ,SKFUNC1   ,
     J             SKFUNC2    ,SKFUNC3   ,KXSP       ,IXSP        ,NOD2SP    ,
     K             IPART(K10) ,SPBUF     ,IXS10      ,IXS20       ,IXS16     ,
     L             VR         ,MONVOL    ,VOLMON     ,IPM         ,IGEO      ,NODGLOB,
     M             IAD_ELEM   ,FR_ELEM   ,FR_SEC     ,FR_RBY2     ,IAD_RBY2  ,
     N             FR_WALL    ,RIBID     ,RRBID      ,FNCONT      ,FTCONT    ,
     O             TEMP       ,THKE      ,ERR_THK_SH4,ERR_THK_SH3 ,RRBID     ,
     P             IPARI      ,RRBID     ,RRBID      ,ALE_CONNECT ,
     Q             IRBE2      ,IRBE3     ,LRBE2      ,LRBE3       ,FR_RBE2,
     R             FR_RBE3M   ,IAD_RBE2  ,RRBID      ,RIBID       ,RIBID     ,
     S             RRBID      ,RRBID     ,RRBID      ,RRBID       ,RRBID     ,
     S             RRBID      ,RIBID     ,RIBID      ,RIBID       ,RIBID     ,
     U             RRBID      ,RRBID     ,WEIGHT_MD  ,RIBID       ,RIBID     ,
     V             FCLUSTER   ,MCLUSTER  ,XFEM_TAB   ,W           ,
     W             NV46       ,IPART(K11),RIBID      ,RIBID       ,IBID      ,
     X             RRBID      ,RRBID     ,NERCVOIS   ,NESDVOIS    ,LERCVOIS  ,
     Y             LESDVOIS   ,CRKEDGE   ,INDX_CRK   ,XEDGE4N     ,XEDGE3N   ,
     Z             STACK      ,SPH2SOL   ,STIFN      ,STIFR       ,IGRNOD    ,
     1             H3D_DATA   ,SUBSET    ,MULTI_FVM  ,RRBID       ,RRBID     ,
     2             FCONT_MAX  ,FNCONTP2 ,FTCONTP2    )
         ENDIF
C
         IF (DTOUTP>ZERO) THEN
            IF (ISPMD==0) THEN
                LENG = NUMNODG
            ELSE
                LENG = 0
            ENDIF
            IOUTP=IOUTP+1
            TT=EIG(I,1)
            CALL GENOUTP(
     1             X     ,D         ,V      ,A     ,
     2             IXS   ,IXQ       ,IXC    ,IXT   ,IXP   ,
     3             IXR   ,IXTG      ,IPARG  ,PM    ,IGEO  ,
     4             MS    ,CONT      ,ITAB   ,PARTSAV,FINT ,
     5             FEXT  ,TANI      ,EANI   ,ANIN  ,IPART ,
     6             VR    ,ELBUF_TAB ,DD_IAD,WEIGHT,
     7             IPM       ,KXSP   ,SPBUF ,NODGLOB,LENG ,
     8             FOPT  ,NOM_OPT   ,NPBY   ,FNCONT   ,FTCONT    ,
     9             GEO   ,THKE      ,STACK  ,DRAPE_SH4N, DRAPE_SH3N ,DRAPEG )
         ENDIF
      ENDDO
c
      DEALLOCATE(DIAG_KG, LT_KG, VECT, EIG)

      DO I=1,NUMNOD
         X(1,I)=X0(1,I)
         X(2,I)=X0(2,I)
         X(3,I)=X0(3,I)
      ENDDO
C
citask0      END IF !(ITASK == 0) THEN

C
      RETURN
      END
#endif 

Chd|====================================================================
Chd|  STOBUCK                       source/implicit/imp_buck.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STOBUCK(DIAG_K  , LT_K  , DIAG_KG, LT_KG   , IADK   ,
     .                   JDIK    , ROWIND, COLPTR , VALUE_OP, VALUE_K,
     .                   VALUE_KG, N     , SIGMA  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), ROWIND(*), COLPTR(*), N
      my_real
     .        DIAG_K(*), LT_K(*), DIAG_KG(*), LT_KG(*), VALUE_OP(*),
     .        VALUE_K(*), VALUE_KG(*), SIGMA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, INDCOL(N), J, JJ, IAD
C 1ER PASSAGE : CALCUL DES INDICES DANS COLPTR
      DO I=1,N
         INDCOL(I)=0
      ENDDO
      DO I=1,N
         INDCOL(I)=INDCOL(I)+1
         DO J=IADK(I),IADK(I+1)-1
            JJ=JDIK(J)
            INDCOL(I)=INDCOL(I)+1
            INDCOL(JJ)=INDCOL(JJ)+1
         ENDDO
      ENDDO
      COLPTR(1)=1
      DO I=1,N
         COLPTR(I+1)=COLPTR(I)+INDCOL(I)
         INDCOL(I)=COLPTR(I)-1
      ENDDO
C 2EME PASSAGE : REMPLISSAGE DE ROWIND, VALUE_OP ET VALUE_K
      DO I=1,N
         INDCOL(I)=INDCOL(I)+1
         IAD=INDCOL(I)
         VALUE_OP(IAD)=DIAG_K(I)-SIGMA*DIAG_KG(I)
         VALUE_K(IAD)=DIAG_K(I)
         VALUE_KG(IAD)=DIAG_KG(I)
         ROWIND(IAD)=I
         DO J=IADK(I),IADK(I+1)-1
            JJ=JDIK(J)
            INDCOL(I)=INDCOL(I)+1
            INDCOL(JJ)=INDCOL(JJ)+1
            IAD=INDCOL(I)
            VALUE_OP(IAD)=LT_K(J)-SIGMA*LT_KG(J)
            VALUE_K(IAD)=LT_K(J)
            VALUE_KG(IAD)=LT_KG(J)
            ROWIND(IAD)=JJ
            IAD=INDCOL(JJ)
            VALUE_OP(IAD)=LT_K(J)-SIGMA*LT_KG(J)
            VALUE_K(IAD)=LT_K(J)
            VALUE_KG(IAD)=LT_KG(J)
            ROWIND(IAD)=I
         ENDDO
      ENDDO
C
      RETURN
      END
C------------------------
