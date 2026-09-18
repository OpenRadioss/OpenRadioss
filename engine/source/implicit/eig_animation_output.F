Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you:
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    eig_animation_output
!||
!|| Write the public /EIG solution through the established ANIM
!|| pipeline.  The Engine's /EIG control flow exits before SORTIE_MAIN,
!|| so modal output must be emitted immediately after the eigensolve.
!||====================================================================
      SUBROUTINE EIG_ANIMATION_OUTPUT(
     2                    PM          , GEO         , IPM      , IGEO     , ELBUF     ,
     3                    IXS         , IXQ         , IXC      , IXT      , IXP       ,
     4                    IXR         , IXTG        , IPARG    , IPARI    , SKEW     ,
     5                    LPBY        , NPBY        , ITAB      ,
     E                    CONT        , ICUT        , XCUT     , FINT     , FEXT      ,
     F                    FOPT        , NSTRF       , RWBUF    , NPRW     ,
     G                    TANI        , DD_IAD      , EANI     , IPART    ,
     H                    NOM_OPT     , IGRSURF     , BUFSF    , IDATA    ,
     I                    RDATA       , BUFMAT      , BUFGEO   , KXX      , IXX       ,
     I                    KXSP        , IXSP        ,
     J                    NOD2SP      , SPBUF       , IXS10    , IXS20    , IXS16     ,
     K                    VR          , MONVOL      , VOLMON   , NODGLOB  , IAD_ELEM  ,
     L                    FR_ELEM     , FR_SEC      , FR_RBY2  , IAD_RBY2 , FR_WALL   ,
     M                    X           , V           , A        ,
     N                    RBY         , MS          , WEIGHT   ,
     O                    FNCONT      , FTCONT      , TEMP     , THKE     ,
     P                    ERR_THK_SH4 , ERR_THK_SH3 , ELBUF_TAB,
     R                    IRBE3       , LRBE3       , FRBE3    , IRBE2    , LRBE2     ,
     S                    IAD_RBE2    , FR_RBE2     , WEIGHT_MD,
     U                    CLUSTER     , FCLUSTER    , MCLUSTER , XFEM_TAB ,
     V                    ALE_CONNECT , W           , NV46     , NERCVOIS , NESDVOIS  ,
     W                    LERCVOIS    , LESDVOIS    , CRKEDGE  , STACK    ,
     X                    INDX_CRK    , XEDGE4N     , XEDGE3N  , SPH2SOL  ,
     Y                    STIFN       , STIFR       , DRAPE_SH4N, DRAPE_SH3N,
     Z                    H3D_DATA    , SUBSET      , MULTI_FVM, IGRNOD   , FCONT_MAX,
     A                    IFLOW       , RFLOW       , DIAG_SMS , FNCONT2  , DXANCG,
     A                    NOD_PXFEM   , IEL_PXFEM   , ZI_PLY   , VGAZ     , FCONTG,
     A                    FNCONTG     , FTCONTG     , FANREAC  , INOD_CRK , IEL_CRK,
     A                    ELCUTC      , IADC_CRK    , PDAMA2   , RES_SMS  , NODGLOBXFE,
     A                    NODEDGE     , KXIG3D      , IXIG3D   , KNOT     , WIGE,
     A                    KNOTLOCPC   , KNOTLOCEL   , FNCONTP2 , FTCONTP2 , DRAPEG,
     A                    MATPARAM_TAB,
     B                    GLOB_THERM  , OUTPUT      , MODE_INDEX, D, DR,
     B                    BUFEL0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EIG_MOD, ONLY : PUBLIC_EIG_NNODE,
     .     PUBLIC_EIG_NODE,PUBLIC_EIG_FREQ,
     .     PUBLIC_EIG_READY
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
      USE GLOB_THERM_MOD
      USE SPMD_MOD
      USE OUTPUT_MOD, ONLY : OUTPUT_
      USE ELEMENT_MOD, ONLY : NIXS,NIXQ,NIXC,NIXT,NIXTG,NIXP,NIXR
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
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*),IGEO(NPROPGI,*),IXS(*),IXQ(NIXQ,*),
     .        IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXTG(NIXTG,*),IPARG(NPARG,*),IPARI(NPARI,*),
     .        LPBY(*),NPBY(NNPBY,*),
     .        ITAB(*),ICUT(*),NSTRF(*),NPRW(*),
     .        DD_IAD(NSPMD+1,*),IPART(*),NOM_OPT(LNOPT1,*),IDATA(*),
     .        KXX(NIXX,*),IXX(*),KXSP(NISP,*),IXSP(KVOISPH,*),
     .        NOD2SP(*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .        NODGLOB(*),IAD_ELEM(2,*),FR_ELEM(*),FR_SEC(NSPMD+1,*),
     .        IAD_RBY2(4,*),IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*),
     .        IAD_RBE2(*),FR_RBE2(*),NV46,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),INDX_CRK(*),XEDGE4N(4,*),
     .        XEDGE3N(3,*),SPH2SOL(*),IFLOW(*),NOD_PXFEM(*),
     .        IEL_PXFEM(*),INOD_CRK(*),IEL_CRK(*),ELCUTC(2,*),
     .        IADC_CRK(*),NODGLOBXFE(*),NODEDGE(2,*),KXIG3D(*),
     .        IXIG3D(*),KNOTLOCPC(*),KNOTLOCEL(*),MODE_INDEX
      my_real PM(NPROPM,*),GEO(NPROPG,*),ELBUF(*),SKEW(LSKEW,*),
     .        CONT(3,*),XCUT(*),
     .        FINT(*),FEXT(3,*),FOPT(6,*),RWBUF(*),TANI(*),EANI(*),
     .        BUFSF(*),RDATA(*),BUFMAT(*),BUFGEO(*),SPBUF(*),VR(3,*),
     .        VOLMON(*),X(3,*),D(3,*),DR(3,*),
     .        V(3,*),A(3,*),RBY(*),MS(*),WEIGHT(*),
     .        FNCONT(3,*),FTCONT(3,*),TEMP(*),THKE(*),ERR_THK_SH4(*),
     .        ERR_THK_SH3(*),FRBE3(*),WEIGHT_MD(*),RFLOW(*),DIAG_SMS(*),
     .        FNCONT2(3,*),DXANCG(3,*),ZI_PLY(*),VGAZ(*),FCONTG(*),
     .        FNCONTG(*),FTCONTG(*),FANREAC(6,*),PDAMA2(2,*),RES_SMS(*),
     .        KNOT(*),WIGE(*),
     .        FCLUSTER(*),MCLUSTER(*),W(*),STIFN(*),STIFR(*),
     .        FCONT_MAX(*),FNCONTP2(3,*),FTCONTP2(3,*),
     .        FR_RBY2(3,*),FR_WALL(*),MONVOL(*),BUFEL0(*)
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP),TARGET :: ELBUF_TAB
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE(CLUSTER_),DIMENSION(NCLUSTER) :: CLUSTER
      TYPE(XFEM_EDGE_),DIMENSION(*) :: CRKEDGE
      TYPE(STACK_PLY) :: STACK
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(SUBSET_),DIMENSION(NSUBS) :: SUBSET
      TYPE(MULTI_FVM_STRUCT),INTENT(INOUT) :: MULTI_FVM
      TYPE(GROUP_),DIMENSION(NGRNOD) :: IGRNOD
      TYPE(SURF_),DIMENSION(NSURF) :: IGRSURF
      TYPE(DRAPE_) :: DRAPE_SH4N(NUMELC_DRAPE),
     .                DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE(DRAPEG_) :: DRAPEG
      TYPE(T_ALE_CONNECTIVITY),INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_),DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM_TAB
      TYPE(GLOB_THERM_),INTENT(INOUT) :: GLOB_THERM
      TYPE(OUTPUT_),INTENT(INOUT) :: OUTPUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NEL3D,NEL2D,NEL1D,NEL,NELG,K1,K2,K3,K4,K5,
     .        K6,K7,K8,K9,K10,K11,L1,SN1,SN2,SN3,SN4,SN5,SN6,SN7,
     .        SN9,SKUIX,SKXUSR,SKFACPTX,SKXEDGE,SKXFACET,SKXSOLID,
     .        SKNUMX1,SKNUMX2,SKNUMX3,SKOFFX1,SKOFFX2,SKOFFX3,
     .        SKMASS1,SKMASS2,SKMASS3,SKFUNC1,SKFUNC2,SKFUNC3,
     .        EIG_OUTPUT_KIND,SIG3DSOLID
      my_real DMAX,DMAX_GLOBAL,SCALE,TT_SAVE
C=======================================================================
      IF (.NOT.PUBLIC_EIG_READY) RETURN
      IF (OUTPUT%DTANIM.LE.ZERO) RETURN
C
      TT_SAVE=TT
      SIG3DSOLID=0
C
C Match the established buckling output dimensions and scratch offsets.
      NEL3D=NUMELS+NUMSPH+3*NUMELS16
      NEL2D=NUMELC+NUMELTG+NUMELQ
      NEL1D=NUMELT+NUMELP+2*NUMELR+NANIM1D
      NEL=MAX(NEL1D,NEL2D,NEL3D)
      NELG=MAX(NUMELSG+3*NUMELS16G+NUMSPHG,
     .         NUMELCG+NUMELTGG+NUMELQG,
     .         NUMELTG+NUMELPG+2*NUMELRG)
      SN1=MAX(3*NUMNOD,6*NEL3D,3*NEL2D,9*NEL1D,NUMSPH)
      SN2=NEL+3*NUMELS16+NUMSPH
      SN3=3*NUMNOD+2*NUMELS16
      SN4=NPART+1
      SN5=NEL2D
      SN6=NPART
      SN7=NELG+1
      IF (NUMELX.GT.0) THEN
        SKUIX=2*MAXNX
        SKXUSR=3*MAXNX
        SKFACPTX=NPART
        SKXEDGE=2*NANIM1D
        SKXFACET=4*NANIM2D
        SKXSOLID=8*NANIM3D
        SKNUMX1=NANIM1D
        SKNUMX2=NANIM2D
        SKNUMX3=NANIM3D
        SKOFFX1=NANIM1D
        SKOFFX2=NANIM2D
        SKOFFX3=NANIM3D
        SKMASS1=NANIM1D
        SKMASS2=NANIM2D
        SKMASS3=NANIM3D
        SKFUNC1=10*NANIM1D
        SKFUNC2=10*NANIM2D
        SKFUNC3=10*NANIM3D
      ELSE
        SKUIX=1
        SKXUSR=1
        SKFACPTX=1
        SKXEDGE=1
        SKXFACET=1
        SKXSOLID=1
        SKNUMX1=1
        SKNUMX2=1
        SKNUMX3=1
        SKOFFX1=1
        SKOFFX2=1
        SKOFFX3=1
        SKMASS1=1
        SKMASS2=1
        SKMASS3=1
        SKFUNC1=1
        SKFUNC2=1
        SKFUNC3=1
      ENDIF
      SN9=NPART
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
      L1=1+6*(NUMELC+NUMELTG)*IEPSDOT
C
      I=MODE_INDEX
      EIG_OUTPUT_KIND=1
      IF (PUBLIC_EIG_FREQ(I).EQ.ZERO) EIG_OUTPUT_KIND=3
C Dynamic vectors have no value in the static eigenvector representation.
C Keep them deterministic until a documented modal derivation is implemented.
      IF (ANIM_V(1).EQ.1) V(1:3,1:NUMNOD)=ZERO
      IF (ANIM_V(3).EQ.1) A(1:3,1:NUMNOD)=ZERO
      IF (ANIM_V(5).EQ.1) FINT(1:3*NUMNOD)=ZERO
      IF (ANIM_V(9).EQ.1.AND.IRODDL.NE.0)
     .  VR(1:3,1:NUMNOD)=ZERO
      IF (ANIM_V(17).EQ.1.OR.ANIM_V(18).EQ.1)
     .  FANREAC(1:6,1:NUMNOD)=ZERO
C Contact forces are dynamic quantities and have no modal value.  GENANI
C nevertheless emits every requested contact vector, so clear each allocated
C payload before entering the ordinary animation writer.
      IF (ANIM_V(4).EQ.1) THEN
        CONT(1:3,1:NUMNOD)=ZERO
        IF (NINTSTAMP.NE.0)
     .    FCONTG(1:3*NUMNODG)=ZERO
      ENDIF
      IF (ANIM_V(12).EQ.1) THEN
        FNCONT(1:3,1:NUMNOD)=ZERO
        FTCONT(1:3,1:NUMNOD)=ZERO
        IF (NINTSTAMP.NE.0) THEN
          FNCONTG(1:3*NUMNODG)=ZERO
          FTCONTG(1:3*NUMNODG)=ZERO
        ENDIF
      ENDIF
      IF (ANIM_V(13).EQ.1)
     .  FNCONT2(1:3,1:NUMNOD)=ZERO
      IF (ANIM_V(26).EQ.1) THEN
        IF (NINTSTAMP.EQ.0) THEN
          FCONT_MAX(1:3*NUMNOD)=ZERO
        ELSEIF (ISPMD.EQ.0) THEN
          FCONT_MAX(1:3*NUMNODG)=ZERO
        ENDIF
      ENDIF
      IF (ANIM_V(27).EQ.1) THEN
        FNCONTP2(1:3,1:NUMNOD)=ZERO
        FTCONTP2(1:3,1:NUMNOD)=ZERO
      ENDIF
C The ordinary element routines have now populated their standard GBUF
C result fields (solid, shell, beam, truss, spring, and other supported
C families).  GBUF energy has family-specific storage conventions (for
C example, shell and beam banks and solid energy density), so GENANI's
C established DFUNC routines perform the authoritative output conversion.
C Limit modal output to elements wholly contained in the /EIG node set.  The
C The enclosing rollback pass restores each group's activity state before
C the next mode.
      CALL EIGOFF(IXS,IXQ,IXC,IXT,IXP,IXR,IXTG,
     .            PUBLIC_EIG_NODE,PUBLIC_EIG_NNODE,ELBUF,
     .            IPARG,BUFEL0,ELBUF_TAB)
      DMAX=MAXVAL(ABS(D(1:3,1:NUMNOD)))
      DMAX_GLOBAL=DMAX
      IF (NSPMD.GT.1)
     .  CALL SPMD_ALLREDUCE(DMAX,DMAX_GLOBAL,1,SPMD_MAX)
      SCALE=ZERO
      IF (DMAX_GLOBAL.GT.ZERO) SCALE=ONE/DMAX_GLOBAL
      X(1:3,1:NUMNOD)=X(1:3,1:NUMNOD)+SCALE*D(1:3,1:NUMNOD)
      TT=PUBLIC_EIG_FREQ(I)
      IANIM=IANIM+1
        CALL GENANI(
     1       X          ,D         ,V          ,A           ,BUFEL0    ,
     2       IXS        ,IXQ       ,IXC        ,IXT         ,IXP       ,
     3       IXR        ,IXTG      ,SN1        ,SN2         ,SN3       ,
     4       SN4        ,IPARG     ,PM         ,GEO         ,MS        ,
     5       SN5        ,CONT      ,SN6        ,ICUT        ,SKEW      ,
     6       XCUT       ,FINT      ,ITAB       ,SN7         ,FEXT      ,
     7       FOPT       ,LPBY      ,NPBY       ,NSTRF       ,
     8       RWBUF      ,NPRW      ,TANI       ,ELBUF_TAB   ,MATPARAM_TAB,
     A       DD_IAD     ,WEIGHT    ,EANI       ,IPART       ,CLUSTER   ,
     B       IPART(K1)  ,IPART(K2) ,IPART(K3)  ,IPART(K4)   ,IPART(K5) ,
     C       IPART(K6)  ,IPART(K7) ,IPART(K8)  ,
     D       RBY        ,SN3       ,TANI(L1)   ,NOM_OPT     ,IGRSURF   ,
     E       BUFSF      ,IDATA     ,RDATA      ,SN9         ,BUFMAT    ,
     F       BUFGEO     ,KXX       ,IXX        ,IPART(K9)   ,SKUIX     ,
     G       SKXUSR     ,SKFACPTX  ,SKXEDGE    ,SKXFACET    ,SKXSOLID  ,
     H       SKNUMX1    ,SKNUMX2   ,SKNUMX3    ,SKOFFX1     ,SKOFFX2   ,
     I       SKOFFX3    ,SKMASS1   ,SKMASS2    ,SKMASS3     ,SKFUNC1   ,
     J       SKFUNC2    ,SKFUNC3   ,KXSP       ,IXSP        ,NOD2SP    ,
     K       IPART(K10) ,SPBUF     ,IXS10      ,IXS20       ,IXS16     ,
     L       VR         ,MONVOL    ,VOLMON     ,IPM         ,IGEO      ,NODGLOB,
     M       IAD_ELEM   ,FR_ELEM   ,FR_SEC     ,FR_RBY2     ,IAD_RBY2  ,
     N       FR_WALL    ,IFLOW     ,RFLOW      ,FNCONT      ,FTCONT    ,
     O       TEMP       ,THKE      ,ERR_THK_SH4,ERR_THK_SH3 ,DIAG_SMS  ,
     P       IPARI      ,FNCONT2   ,DR         ,ALE_CONNECT ,
     Q       IRBE2      ,IRBE3     ,LRBE2      ,LRBE3       ,FR_RBE2   ,
     R       FRBE3      ,IAD_RBE2  ,DXANCG     ,NOD_PXFEM   ,IEL_PXFEM ,
     S       ZI_PLY     ,VGAZ      ,FCONTG     ,FNCONTG     ,FTCONTG   ,
     T       FANREAC    ,INOD_CRK  ,IEL_CRK    ,ELCUTC      ,IADC_CRK ,
     U       PDAMA2     ,RES_SMS   ,WEIGHT_MD  ,NODGLOBXFE  ,NODEDGE  ,
     V       FCLUSTER   ,MCLUSTER  ,XFEM_TAB   ,W           ,
     W       NV46       ,IPART(K11),KXIG3D     ,IXIG3D      ,SIG3DSOLID,
     X       KNOT       ,WIGE      ,NERCVOIS   ,NESDVOIS    ,LERCVOIS  ,
     Y       LESDVOIS   ,CRKEDGE   ,INDX_CRK   ,XEDGE4N     ,XEDGE3N   ,
     Z       STACK      ,SPH2SOL   ,STIFN      ,STIFR       ,IGRNOD    ,
     1       H3D_DATA   ,SUBSET    ,MULTI_FVM  ,KNOTLOCPC   ,KNOTLOCEL ,
     2       FCONT_MAX  ,FNCONTP2  ,FTCONTP2   ,GLOB_THERM  ,
     .       DRAPE_SH4N ,DRAPE_SH3N,DRAPEG     ,OUTPUT,
     .       EIG_OUTPUT_KIND,SCALE)
      X(1:3,1:NUMNOD)=X(1:3,1:NUMNOD)-SCALE*D(1:3,1:NUMNOD)
      TT=TT_SAVE
      RETURN
      END
