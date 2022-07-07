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
Chd|====================================================================
Chd|  GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        DRBE2CNT                      source/output/anim/generate/drbe2cnt.F
Chd|        DRBE3CNT                      source/output/anim/generate/drbe3cnt.F
Chd|        DRBYCNT                       source/output/anim/generate/drbycnt.F
Chd|        FRETITL                       source/input/freform.F        
Chd|        GET_NODAL_IPART               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3DLIB_LOAD                   source/output/h3d/h3d_build_cpp/h3d_dl.c
Chd|        H3D_CONSTIT                   source/output/h3d/h3d_build_fortran/h3d_constit.F
Chd|        H3D_CREATE_DATATYPE           source/output/h3d/h3d_build_fortran/h3d_create_datatype.F
Chd|        H3D_CREATE_RBE2_IMPI          source/output/h3d/h3d_build_fortran/h3d_create_rbe2_impi.F
Chd|        H3D_CREATE_RBE3_IMPI          source/output/h3d/h3d_build_fortran/h3d_create_rbe3_impi.F
Chd|        H3D_CREATE_RBODIES_IMPI       source/output/h3d/h3d_build_fortran/h3d_create_rbodies_impi.F
Chd|        H3D_DXYZ_RWALL                source/output/h3d/h3d_build_fortran/h3d_dxyz_rwall.F
Chd|        H3D_DXYZ_RWALL_UPDATE         source/output/h3d/h3d_build_fortran/h3d_dxyz_rwall_update.F
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|        H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|        H3D_NODAL_VECTOR              source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|        H3D_ONED_OFF                  source/output/h3d/spmd/spmd_h3d_oned_off.F
Chd|        H3D_ONED_SCALAR               source/output/h3d/h3d_results/h3d_oned_scalar.F
Chd|        H3D_ONED_TENSOR               source/output/h3d/h3d_results/h3d_oned_tensor.F
Chd|        H3D_ONED_TORSOR               source/output/h3d/h3d_results/h3d_oned_torsor.F
Chd|        H3D_ONED_VECTOR               source/output/h3d/h3d_results/h3d_oned_vector.F
Chd|        H3D_QUAD_OFF                  source/output/h3d/spmd/spmd_h3d_quad_off.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_QUAD_TENSOR               source/output/h3d/h3d_results/h3d_quad_tensor.F
Chd|        H3D_QUAD_VECTOR               source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|        H3D_SHELL_OFF                 source/output/h3d/spmd/spmd_h3d_shell_off.F
Chd|        H3D_SHELL_SCALAR              source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        H3D_SHELL_VECTOR              source/output/h3d/h3d_results/h3d_shell_vector.F
Chd|        H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_OFF                  source/output/h3d/h3d_results/h3d_skin_off.F
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        H3D_SOLID_OFF                 source/output/h3d/spmd/spmd_h3d_solid_off.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_TENSOR              source/output/h3d/h3d_results/h3d_solid_tensor.F
Chd|        H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        H3D_SPH_OFF                   source/output/h3d/spmd/spmd_h3d_sph_off.F
Chd|        H3D_SPH_SCALAR                source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|        H3D_SPH_TENSOR                source/output/h3d/h3d_results/h3d_sph_tensor.F
Chd|        SCANOR                        source/output/anim/generate/scanor.F
Chd|        SPMD_H3D_GATHER_I             source/output/h3d/spmd/spmd_h3d_gather_i.F
Chd|        SPMD_H3D_GATHER_I_NODE        source/output/h3d/spmd/spmd_h3d_gather_i_node.F
Chd|        SPMD_H3D_GATHER_I_NODE_PART   source/output/h3d/spmd/spmd_h3d_gather_i_node_part.F
Chd|        SPMD_H3D_GATHER_R             source/output/h3d/spmd/spmd_h3d_gather_r.F
Chd|        SPMD_H3D_GATHER_R_NODAL_VALUE source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
Chd|        SPMD_H3D_GATHER_R_NODE        source/output/h3d/spmd/spmd_h3d_gather_r_node.F
Chd|        SPMD_H3D_GATHER_T_NODE        source/output/h3d/spmd/spmd_h3d_gather_t_node.F
Chd|        SPMD_H3D_MAX_R_NODAL_VALUE    source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
Chd|        SPMD_H3D_MAX_R_NODAL_VALUE_21 source/output/h3d/spmd/spmd_h3d_gather_r_nodal_value.F
Chd|        SPMD_H3D_SUM_R_NODAL          source/output/h3d/spmd/spmd_h3d_gather_r_node.F
Chd|        SPMD_H3D_SUM_R_NODAL_21       source/output/h3d/spmd/spmd_h3d_gather_r_node.F
Chd|        SPMD_OUTPITAB                 source/mpi/interfaces/spmd_outp.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        ALEANIM_MOD                   share/modules/aleanim_mod.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE GENH3D(X        ,D        ,V           ,A           ,BUFEL    ,
     2                  IXS      ,IXQ      ,IXC         ,IXT         ,IXP      ,
     3                  IXR      ,IXTG     ,SWAFT       ,SMAS        ,SXNORM   ,
     4                  SIAD     ,IPARG    ,PM          ,GEO         ,MS       ,
     5                  SINVERT  ,CONT     ,SMATER      ,ICUT        ,SKEW     ,
     6                  XCUT     ,FINT     ,ITAB        ,SEL2FA      ,FEXT     ,
     7                  FOPT     ,ANIN     ,LPBY        ,NPBY        ,NSTRF    ,
     8                  RWBUF    ,NPRW     ,IXUR        ,TANI        ,ELBUF_TAB,
     A                  DD_IAD   ,WEIGHT   ,EANI        ,IPART       ,CLUSTER  ,
     B                  IPARTS   ,IPARTQ   ,IPARTC      ,IPARTT      ,IPARTP   ,
     C                  IPARTR   ,IPARTUR  ,IPARTTG     ,
     D                  RBY      ,SWA4     ,TORS        ,NOM_OPT     ,
     E                  BUFSF    ,IDATA    ,RDATA       ,SIADG       ,BUFMAT   ,
     F                  BUFGEO   ,KXX      ,IXX         ,IPARTX      ,SUIX     ,
     G                  SXUSR    ,SNFACPTX ,SIXEDGE     ,SIXFACET    ,SIXSOLID ,
     H                  SNUMX1   ,SNUMX2   ,SNUMX3      ,SOFFX1      ,SOFFX2   ,
     I                  SOFFX3   ,SMASS1   ,SMASS2      ,SMASS3      ,SFUNC1   ,
     J                  SFUNC2   ,SFUNC3   ,KXSP        ,IXSP        ,NOD2SP   ,
     K                  IPARTSP  ,SPBUF    ,IXS10       ,IXS20       ,IXS16    ,
     L                  VR       ,MONVOL   ,VOLMON      ,IPM         ,IGEO     ,NODGLOB,
     M                  IAD_ELEM ,FR_ELEM  ,FR_SEC      ,FR_RBY2     ,IAD_RBY2 ,
     N                  FR_WALL  ,IFLOW    ,RFLOW       ,FNCONT      ,FTCONT   ,
     O                  TEMP     ,THKE     ,ERR_THK_SH4 ,ERR_THK_SH3 ,DIAG_SMS ,
     P                  IPARI    ,FNCONT2  ,DR          ,ALE_CONNECT       ,
     Q                  IRBE2    ,IRBE3    ,LRBE2       ,LRBE3       ,FR_RBE2,
     R                  FR_RBE3M ,IAD_RBE2 ,DXANCG      ,NOD_PXFEM   ,IEL_PXFEM,
     S                  ZI_PLY   ,VGAZ     ,FCONTG      ,FNCONTG     ,FTCONTG  ,
     T                  FANREAC  ,INOD_CRK ,IEL_CRK     ,ELCUTC      ,IADC_CRK ,
     U                  PDAMA2   ,RES_SMS  ,WEIGHT_MD   ,NODGLOBXFE  ,NODEDGE  ,
     V                  FCLUSTER ,MCLUSTER ,XFEM_TAB    ,W           ,
     W                  NV46     ,IPARTIG3D,KXIG3D      ,IXIG3D      ,SIG3DSOLID,
     X                  KNOT     ,WIGE     ,NERCVOIS    ,NESDVOIS    ,LERCVOIS  ,
     Y                  LESDVOIS ,CRKEDGE  ,INDX_CRK    ,XEDGE4N     ,XEDGE3N   ,
     Z                  STACK    ,SPH2SOL  ,STIFN       ,STIFR       ,IGRNOD    ,
     1                  SH4TREE  ,SH3TREE  ,H3D_DATA    ,MULTI_FVM   ,SUBSET    ,
     2                  PSKIDS   ,TAG_SKINS6,TF         ,NPF         ,FCONT_MAX ,
     3                  MDS_MATID,FNCONTP2 ,FTCONTP2    ,IBCL        ,ILOADP    ,
     4                  LLOADP   ,FAC      ,SENSOR      ,LSENSOR     ,TAGNCONT  ,
     5                  LOADP_HYD_INTER    ,XFRAME      ,FORC        )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
      USE PLYXFEM_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD         
      USE CRACKXFEM_MOD
      USE CLUSTER_MOD
      USE STACK_MOD
      USE H3D_MOD
      USE ALEANIM_MOD  !, ONLY : FANI_CELL
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "build_info.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "com_xfem1.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr13_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "scr25_c.inc"
#include      "submodel.inc"
#include      "chara_c.inc"
#include      "scrcut_c.inc"
#include      "rnur_c.inc"
#include      "r4r8_p.inc"
#include      "task_c.inc"
#include      "couple_c.inc"
#include      "spmd_c.inc"
#include      "flowcom.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "filescount_c.inc"
#include      "tabsiz_c.inc"
#include      "parit_c.inc"
#include      "intstamp_c.inc"
#include      "nchara_c.inc"
#include      "timeri_c.inc"
#include      "macro.inc"
#include      "my_allocate.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SWAFT,SMAS,SXNORM,SIAD,SINVERT,SMATER,SEL2FA,SWA4,
     .        SIADG,NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),
     .        LESDVOIS(*),SPH2SOL(*),SH4TREE(*),SH3TREE(*),TAG_SKINS6(*)
      INTEGER
     .  SUIX, SXUSR ,SFACPTX,SIXEDGE,SIXFACET,SIXSOLID,SNUMX1,
     .  SNUMX2,SNUMX3,SOFFX1,SOFFX2,SOFFX3,SMASS1,SMASS2,
     .  SMASS3,SFUNC1,SFUNC2,SFUNC3,SFIN,SNFACPTX,NPF(*)

      INTEGER  IGEO(NPROPGI,*),IPM(NPROPMI,*),INDX_CRK(*),
     .  LRBE2(*),LRBE3(*),FR_RBE2(3,*),FR_RBE3M(3,*),
     .  NOD_PXFEM(*), IEL_PXFEM(*),NODEDGE(2,*),XEDGE4N(4,*),XEDGE3N(3,*),
     .  INOD_CRK(*),IEL_CRK(*),ELCUTC(2,*),IADC_CRK(*)
      my_real
     .   X(*), D(*), V(*), A(3,*), BUFEL(*),
     .   PM(NPROPM,*), GEO(NPROPG,*),CONT(*),
     .   XCUT(*) , FINT(3,*),MS(*),RWBUF(NRWLP,*),SKEW(LSKEW,*),
     .   RBY(NRBY,*),FEXT(3,*) ,FOPT(6,*),ANIN(*),TANI(6,*),EANI(*),
     .   TORS(15,*),BUFSF(*), RDATA(*),
     .   BUFMAT(*),BUFGEO(*),
     .   SPBUF(*), VR(*),VOLMON(*), RFLOW(*), FNCONT(3,*), FTCONT(3,*),
     .   TEMP(*), THKE(*), ERR_THK_SH4(*), ERR_THK_SH3(*), DIAG_SMS(*),
     .   FNCONT2(3,*), DR(3,*),DXANCG(3,*),ZI_PLY(*),VGAZ(*),
     .   FCONTG(*), FNCONTG(*), FTCONTG(*),FANREAC(6,*),PDAMA2(2,*),
     .   RES_SMS(*),FCLUSTER(3,*),MCLUSTER(3,*),W(*),
     .   WIGE(*),KNOT(*),STIFN(*),STIFR(*),PSKIDS(*),TF(*),FCONT_MAX(*),
     .   FNCONTP2(3,*) ,FTCONTP2(3,*) 
      INTEGER IPARG(NPARG,*),NSTRF(*),LPBY(*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),MONVOL(*) ,
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*) ,
     .        ICUT(*), ITAB(*),NPBY(NNPBY,*),NPRW(*),IXUR(NIXUR,*),
     .        WEIGHT(*),IPART(LIPART1,*),IPARTS(*),IPARTQ(*),IPARTC(*),
     .        IPARTT(*),IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),
     .        NOM_OPT(*),
     .        IDATA(*),KXX(NIXX,*), IXX(*), IPARTX(*),
     .        KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*), IPARTSP(*),
     .        NODGLOB(*),IAD_ELEM(2,*),FR_ELEM(*),FR_WALL(*), IFLOW(*),
     .        IPARI(NPARI,*),IRBE2(NRBE2L,*),IRBE3(NRBE3L,*),
     .        WEIGHT_MD(*),NODGLOBXFE(*),IPARTIG3D(*)
      INTEGER CTEXT(111), IB 
      INTEGER DD_IAD(NSPMD+1,*),
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        N1,N2,N3
      INTEGER FR_SEC(NSPMD+1,*),FR_RBY2(3,*),IAD_RBY2(4,*),
     .        NERBT(NRBODY),LOC_PROC,PROC,NERBE2T(NRBE2G),
     .        NERBE3T(NRBE3G),IAD_RBE2(4,*),NV46,KXIG3D(*),
     .        IXIG3D(*),SIG3DSOLID,MDS_MATID(*)
      INTEGER LLOADP(SLLOADP),LSENSOR(NSENSI,*)
      INTEGER ILOADP(SIZLOADP,*),IBCL(NIBCLD,*)
      INTEGER TAGNCONT(NLOADP_HYD_INTER,NUMNOD),LOADP_HYD_INTER(NLOADP_HYD)
      my_real
     .       FAC(LFACCLD,*), SENSOR(NSENSR,*), XFRAME(NXFRAME,*),FORC(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)    :: CLUSTER
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (STACK_PLY) :: STACK
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (SUBSET_) , TARGET, DIMENSION(NSUBS) :: SUBSET
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: WAFT  , MAS   , XNORM, 
     .                                      XMASS1, XMASS2, XMASS3,
     .                                      XFUNC1, XFUNC2, XFUNC3,
     .                                      XUSR, WAFT_P
      INTEGER IAD(SIAD),INVERT(SINVERT),MATER(SMATER),EL2FA(SEL2FA),
     .        IADG(NSPMD,SIADG),
     .        UIX(SUIX)      ,NFACPTX(3,SNFACPTX),
     .        IXEDGE(SIXEDGE), IXFACET(SIXFACET), IXSOLID(SIXSOLID),
     .        INUMX1(SNUMX1) , INUMX2(SNUMX2)   , INUMX3(SNUMX3),
     .        IOFFX1(SOFFX1) , IOFFX2(SOFFX2)   , IOFFX3(SOFFX3),
     .        SZ16,SHFT16,IADGPS,NSN,IADISO     , IG3DSOLID(SIG3DSOLID),
     .        FIRST_NODE_IG3D
      REAL
     .       , DIMENSION(:), ALLOCATABLE :: WA4, MAS4
      my_real
     .   X_TEMP(3,64*NUMELIG3D),D_TEMP(3,64*NUMELIG3D),
     .   V_TEMP(3,64*NUMELIG3D),A_TEMP(3,64*NUMELIG3D),TABSTRESL(6,64*NUMELIG3D),
     .   BID_TEMP(3,64*NUMELIG3D)
       

      my_real ,DIMENSION (:), ALLOCATABLE :: CBUF
      INTEGER ,DIMENSION (:), ALLOCATABLE :: ICBUF
      SAVE CBUF,ICBUF

      CHARACTER*80 STR, MES*30, CAUX,TITL*100, H3DTITLE
      CHARACTER*ncharline KEYWORD
      CHARACTER CH_H3D*4,FILNAM*100, CHANIM1*4
      INTEGER I, NBF, NBPART, MAGIC, J, IFUNC, FILEN, NPSOL,
     .        NODCUT,NELCUT,LENR,LENI,LENCUT,IXEL,
     .        MIC1,MIC2,MIC3,MIC4,MIC5,MAC1,MAC2,MAC3,NPSPR,N,K,
     .        I3000, NESCT,NERBY,NERWL,NNWL,TMPNBF,ISK(6),
     .        NESBW2,NEL,NFT,ITY,NG,OUI,LAYER,IPT,GAUSS,ID_PLY,IUVAR,
     .        ALL_SUB_CHILDS,IDMDS,IMDSVAR,ID
      INTEGER II,II_L,INC,P,NSLARB_L,NDMA2,NUMELS_T,NSKEWA,NB1D,
     .        M1,M2,M3,M,M01,NBONED_T, NBF_L, LEN, NUMELT_T,
     .        NUMELR_T, NUMELP_T
      INTEGER ISECT,NESCT1,IRBY,NERBY1,IRWL,NERWL1,NERBE2,NERBE3,
     .        NERBE2_1,NERBE3_1
      INTEGER NSURG,NESRG, NNSRG, NESRG1, NNSRG1,ISRG,ISRF,ISRK
      INTEGER NSMAD,NESMD, NNSMD, NESMD1, NNSMD1,ISMD
      INTEGER NENT,OFFSI,OFFSRF,OFFSRV
      INTEGER NESPH,NNSPH,INSPH,NESPHG,NNSPHG,SNNSPHG,SZNNSPH,SHFTSPH
      INTEGER I161,I16A,I16B,I16C,I16D,I16E,I16F,I16G,I16H,I16I,I16J,
     .        I16K,I16L,I16M,I16N
      INTEGER MXSUBS,NSECTSA
      INTEGER IPRT, IAUX
      INTEGER IFLAG1D,NNNSRG,NNN,BUF
      INTEGER NANIMONED_L,IUS,NANIM3D_L
      INTEGER ISPH3D,M4,N0
      INTEGER SBUFSPM,SBUFRECVM,SBUFSPO,SPORBY,N_OUTP_DATA,LEN_H3DTITLE
      my_real
     .   CDG(3),XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX, SCALE,
     .   RVAL
      INTEGER K1, KK1, K2, KIBJET, KIBHOL, IADHOL, KK2, KRBJET, KRBHOL,
     .        RADHOL, ITYP, KI1, KR1, NCA, NTG, NJET, NVENT, NTGI
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VFLU    , VVAR1   , AFLU, 
     .                         VFLU_ALE, FANREACT, FANREACR

      INTEGER IADI, IADR, NINOUT, NNO, II1, II2, IR1, NNO_L, NNN_L,
     .        II3, II4
      INTEGER NNS, NNI, NNT, NNA, NBA, KI2, KR2
      INTEGER FVOFF(2,NFVBAG), INOD(4), INORM(3), NFVTR, NFVNOD,
     .        NFVPART, NFVSUBS, IDMAX, KK, NN, FVIAD, JJ, OFFPART,
     .        ELOFF, IDCMAX, NND, NBID1, NBID2, NBID3, NFVNODT, IDP,
     .        NBPART2D, ISUBG,EMPSIZPL,IRUNN,IR,IS,IT,INTER_INPUT,INTERSKID,
     .        NI, ITYSKID

      INTEGER, DIMENSION(:), ALLOCATABLE :: SHELL_ID,SHELL_ID_P,
     .                                      SHELL_ITY,SHELL_ITY_P,ONED_ID,ONED_ID_P,
     .                                      ONED_ITY,ONED_ITY_P,SOLID_ID,SOLID_ID_P,
     .                                      SOLID_ITY,SOLID_ITY_P,IS_WRITEN_SHELL,
     .                                      IS_WRITEN_SHELL_P,IS_WRITEN_ONED,
     .                                      IS_WRITEN_ONED_P,IS_WRITEN_NODE,
     .                                      IS_WRITEN_NODE_P,NODE_ID,NODE_ID_P,
     .                                      IS_WRITEN_SOLID,IS_WRITEN_SOLID_P,
     .                                      SPH_ID,SPH_ID_P,IS_WRITEN_SPH,IS_WRITEN_SPH_P,
     .                                      QUAD_ID,QUAD_ID_P,IS_WRITEN_QUAD,IS_WRITEN_QUAD_P,
     .                                      SKIN_ID_P,IS_WRITEN_SKIN,IS_WRITEN_SKIN_P,
     .                                      ISOLNOD,ISOLNOD_P
      INTEGER  INFO1,INFO2,IS_CORNER_DATA, COMPID_RBODIES, COMPID_RBE2S, COMPID_RBE3S, MAX_PART_ID
      INTEGER  CPT,ERROR_LOAD,NUMNOD_H3DPART,WORK(70000)
c a mettre en allocatable
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: 
     .         IXC_P,
     .         IXC_TMP,
     .         IXTG_TMP,
     .         IXTG_P,
     .         IXS_TMP,
     .         IXS_P,
     .         IXR_TMP,
     .         IXR_P,
     .         IXP_TMP,
     .         IXP_P,
     .         KXSP_TMP,
     .         KXSP_P,
     .         IXT_TMP,
     .         IXT_P,
     .         IXS10_TMP,
     .         IXS10_P,
     .         IXS16_TMP,
     .         IXS16_P,
     .         IXS20_TMP,
     .         IXS20_P,
     .         IXQ_TMP,
     .         IXQ_P,
     .         IXSKIN_TMP,
     .         IXSKIN_P
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .         IPARTS_P,
     .         IPARTR_P,
     .         IPARTP_P,
     .         IPARTT_P,
     .         IPARTSP_P,
     .         IPARTS10_P,
     .         IPARTS16_P,
     .         IPARTS20_P,
     .         IPARTC_P,
     .         IPARTTG_P,
     .         IPARTQ_P,  
     .         IPARTSKIN_P,
     .         NODAL_IPART
      my_real, DIMENSION(:), ALLOCATABLE :: X_P,D_P
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAB_P

      INTEGER :: IAD_H3DPART,IAD_P,NUMNODG0,IP0 ,NIXSKIN
      INTEGER :: NUMELC_P(NSPMD),DISP(NSPMD),NUMNOD_P(NSPMD) 



C
      INTEGER MAX_NOD_ID,MAX_NCORN
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: NODAL_SCALAR,NODAL_SCALAR_P,
     .                                      NODAL_VECTOR,NODAL_VECTOR_P,
     .                                      NODAL_TENSOR,NODAL_TENSOR_P,
     .                                      ONED_SCALAR,ONED_SCALAR_P,
     .                                      ONED_VECTOR,ONED_VECTOR_P,
     .                                      ONED_TENSOR,ONED_TENSOR_P,
     .                                      ONED_TORSOR,ONED_TORSOR_P,
     .                                      SHELL_SCALAR,SHELL_SCALAR_P,
     .                                      SHELL_VECTOR,SHELL_VECTOR_P,
     .                                      SHELL_TENSOR,SHELL_TENSOR_P,
     .                                      SOLID_SCALAR,SOLID_SCALAR_P,
     .                                      SOLID_VECTOR,SOLID_VECTOR_P,
     .                                      SOLID_TENSOR,SOLID_TENSOR_P,
     .                                      SOLID_TENSOR_CORNER,SOLID_TENSOR_CORNER_P,
     .                                      SKIN_SCALAR,SKIN_SCALAR_P,
     .                                      SKIN_VECTOR,SKIN_VECTOR_P,
     .                                      SKIN_TENSOR,SKIN_TENSOR_P,
     .                                      SPH_SCALAR,SPH_SCALAR_P,
     .                                      SPH_TENSOR,SPH_TENSOR_P,
     .                                      QUAD_SCALAR,QUAD_SCALAR_P,
     .                                      QUAD_VECTOR,QUAD_VECTOR_P,
     .                                      QUAD_TENSOR,QUAD_TENSOR_P
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNOD,TAGNOD_P,TAGNOD_P_PART,
     .                                      ITAB_P_PART,ITABM1_P
      my_real
     .   XWL(NRWALL) ,YWL(NRWALL) , ZWL(NRWALL), V1(NRWALL), V2(NRWALL), V3(NRWALL),
     .   VV1(NRWALL), VV2(NRWALL), VV3(NRWALL), XL(NRWALL), XN(NRWALL), YN(NRWALL),  
     .   ZN(NRWALL)

      LOGICAL IS_FILE_EXISTS
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_CHILD,SUB_IAD,SUB_TITLE
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_NCHILD
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_LEVEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUB_ID
      INTEGER (KIND=8) :: H3DTOTALSIZE8

      INTEGER :: LEN_TMP_NAME,LEN_RADVERS,OBJECT_ID
      CHARACTER(len=2148) :: TMP_NAME
      CHARACTER RADVERS*68
C=======================================================================
      CALL STARTIME(MACRO_TIMER_GENH3D,1)
      NDMA2 = NUMNOD*(MIN(1,ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT)
     .              +MIN(1,ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS)
     .              +MIN(1,ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER))
      ALLOCATE(MAS(SMAS))
      ALLOCATE(SUB_NCHILD(NSUBS))
      ALLOCATE(SUB_LEVEL(NSUBS))
      ALLOCATE(SUB_ID(NSUBS))

c
      I161=1
      I16A=I161+LNOPT1*NRBODY0
      I16B=I16A+LNOPT1*NACCELM
      I16C=I16B+LNOPT1*NVOLU
      I16D=I16C+LNOPT1*(NINTER+NINTSUB)
      I16E=I16D+LNOPT1*NRWALL
      I16F=I16E !obsolete option removed
      I16G=I16F+LNOPT1*NJOINT
      I16H=I16G+LNOPT1*NSECT
      I16I=I16H+LNOPT1*NLINK
      I16J=I16I+LNOPT1*(NUMSKW+1+NUMFRAM+1)
      I16K=I16J+LNOPT1*NFXBODY
      I16L=I16K+LNOPT1*NFLOW
      I16M=I16L+LNOPT1*NRBE2
      I16N=I16M+LNOPT1*NRBE3



c
      ERROR_LOAD = 0
c
      H3D_DATA%IH3D_RUN = H3D_DATA%IH3D_RUN + 1
      IF(ISPMD == 0) THEN
        NUMNODG0 = NUMNODG  
        IP0 = 1
      ELSE
        NUMNODG0 = 1
        IP0 = 0
      ENDIF
C----- same size of skin than quad      
      NIXSKIN = NIXQ

      MY_ALLOCATE(X_P,8*NUMNODG0)
      MY_ALLOCATE(ITAB_P,NUMNODG0)

      MY_ALLOCATE2D( IXTG_P       ,NIXTG,NUMELTGG*IP0)
      MY_ALLOCATE2D( IXS_P        ,NIXS,(NUMELSG-NUMELS10G-NUMELS16G-NUMELS20G)*IP0)
      MY_ALLOCATE2D( IXP_P        ,NIXP,NUMELPG*IP0)
      MY_ALLOCATE2D( IXR_P        ,NIXR,NUMELRG*IP0)
      MY_ALLOCATE2D( KXSP_P       ,NISP,NUMSPHG*IP0)
      MY_ALLOCATE2D( IXT_P        ,NIXT,NUMELTRG*IP0) !!
      MY_ALLOCATE2D( IXC_P        ,NIXC,NUMELCG*IP0)
      MY_ALLOCATE2D( IXS10_P      ,11,NUMELS10G*IP0)
      MY_ALLOCATE2D( IXS16_P      ,17,NUMELS16G*IP0)
      MY_ALLOCATE2D( IXS20_P      ,21,NUMELS20G*IP0)
      MY_ALLOCATE  ( IPARTC_P     ,NUMELCG*IP0)
      MY_ALLOCATE  ( IPARTTG_P    ,NUMELTGG*IP0)
      MY_ALLOCATE  ( IPARTQ_P     ,NUMELQG*IP0) 
      MY_ALLOCATE  ( IPARTS_P     ,(NUMELSG-NUMELS10G-NUMELS16G-NUMELS20G)*IP0)
      MY_ALLOCATE  ( IPARTR_P     ,NUMELRG*IP0)
      MY_ALLOCATE  ( IPARTP_P     ,NUMELPG*IP0)
      MY_ALLOCATE  ( IPARTT_P     ,NUMELTRG*IP0)
      MY_ALLOCATE  ( IPARTSP_P    ,NUMSPHG*IP0)
      MY_ALLOCATE  ( IPARTS10_P   ,NUMELS10G*IP0)
      MY_ALLOCATE  ( IPARTS16_P   ,NUMELS16G*IP0)
      MY_ALLOCATE  ( IPARTS20_P   ,NUMELS20G*IP0)
      MY_ALLOCATE2D( IXQ_P        ,NIXQ,NUMELQG*IP0)
      MY_ALLOCATE2D( IXC_TMP      ,NIXC,NUMELC)
      MY_ALLOCATE2D( IXTG_TMP     ,NIXTG,NUMELTG)
      MY_ALLOCATE2D( IXR_TMP      ,NIXR,NUMELR)
      MY_ALLOCATE2D( IXP_TMP      ,NIXP,NUMELP)
      MY_ALLOCATE2D( KXSP_TMP     ,NISP,NUMSPH)
      MY_ALLOCATE2D( IXT_TMP      ,NIXT,NUMELT)
      MY_ALLOCATE2D( IXS10_TMP    ,11,NUMELS10)
      MY_ALLOCATE2D( IXS16_TMP    ,17,NUMELS16)
      MY_ALLOCATE2D( IXS20_TMP    ,21,NUMELS20)
      MY_ALLOCATE2D( IXQ_TMP      ,NIXQ,NUMELQ)
      MY_ALLOCATE2D( IXSKIN_TMP   ,NIXSKIN,NUMSKIN)
      MY_ALLOCATE2D( IXSKIN_P     ,NIXSKIN,NUMSKING*IP0)
      MY_ALLOCATE  ( IPARTSKIN_P  ,NUMSKING*IP0) 
c
      MY_ALLOCATE(D_P,NUMNODG0*8)

      IF(NSPMD > 1) THEN 
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_R_NODE(WEIGHT,D,3*NUMNOD,D_P,3*NUMNODG) 
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ENDIF
c
C**********************************************************************************************
C COMPUTE COMPONENT ID FOR RBODIES
C**********************************************************************************************
      MAX_PART_ID = 0
      COMPID_RBODIES = 0
      COMPID_RBE2S = 0
      COMPID_RBE3S = 0
      DO I=1,NPART
        MAX_PART_ID = MAX(MAX_PART_ID,IPART(4,I))
      ENDDO
      IF(H3D_DATA%RBODY_SINGLE == 1)COMPID_RBODIES = MAX_PART_ID + 1
      IF(H3D_DATA%RBE2_SINGLE == 1)COMPID_RBE2S = MAX_PART_ID + 2
      IF(H3D_DATA%RBE3_SINGLE == 1)COMPID_RBE3S = MAX_PART_ID + 3
C**********************************************************************************************
C BUILD H3D FILE
C**********************************************************************************************
      IF (ISPMD.EQ.0) THEN
        FILNAM=ROOTNAM(1:ROOTLEN)//'.h3d'
        FILEN = ROOTLEN + 4
        LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 4
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))   
C
c**********************************************************************************************
C CREATE PARTS + HIERARCHY
C**********************************************************************************************
        IF ((H3D_DATA%IH3D == 1 .AND. IRUN == 1) 
     . .OR.( H3D_DATA%IH3D_RUN == 1 .AND. IRUN > 1)) THEN

          IF (NSUBS > 0) THEN
            ALL_SUB_CHILDS = 0
            DO J=1,NSUBS
              ALL_SUB_CHILDS = ALL_SUB_CHILDS + SUBSET(J)%NCHILD
              SUB_ID    (J) = SUBSET(J)%ID
              SUB_NCHILD(J) = SUBSET(J)%NCHILD
              SUB_LEVEL (J) = SUBSET(J)%LEVEL
            ENDDO
!
            ALLOCATE(SUB_CHILD(ALL_SUB_CHILDS))
            SUB_CHILD(1:ALL_SUB_CHILDS) = 0
            ALLOCATE(SUB_IAD(NSUBS))
            SUB_IAD(1:NSUBS) = 0
            ALLOCATE(SUB_TITLE(NSUBS*LTITR))
            SUB_TITLE(1:NSUBS*LTITR) = 0
            K = 0
            DO J=1,NSUBS
              DO I=1,SUBSET(J)%NCHILD
                K = K + 1
                SUB_CHILD(K) = SUBSET(J)%CHILD(I)
              ENDDO
              SUB_IAD(J) = K
            ENDDO
!  title
            DO J=1,NSUBS
               CALL FRETITL(SUBSET(J)%TITLE,SUB_TITLE(LTITR * (J-1)+1),LTITR)
            ENDDO
          ELSE
            ALLOCATE(SUB_CHILD(0))
            ALLOCATE(SUB_IAD(0))
            ALLOCATE(SUB_TITLE(0))
          ENDIF !IF (NSUBS > 0) THEN
        ENDIF ! IH3D,IRUN

        IF (H3D_DATA%IH3D == 1 .AND. IRUN == 1) THEN 
C
c**********************************************************************************************
C OPEN H3D LIB
C********************************************************************************************** 
          ERROR_LOAD = 0
          CALL H3DLIB_LOAD(ERROR_LOAD)
          IF(ERROR_LOAD == 1) THEN
            CALL ANCMSG(MSGID=274,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
C
c**********************************************************************************************
C OPEN H3D FILE
C********************************************************************************************** 
          IS_H3D_USED = .TRUE.
          WRITE(RADVERS,'(A,A)') 'Radioss ',VERS
          LEN_RADVERS = LEN_TRIM(RADVERS)
C
          CALL STARTIME(MACRO_TIMER_LIBH3D,1)
          CALL C_H3D_OPEN_FILE(TMP_NAME,LEN_TMP_NAME,H3D_DATA%PERCENTAGE_ERROR,H3D_DATA%COMP_LEVEL,
     .                         RADVERS,LEN_RADVERS,FAC_MASS,FAC_LENGTH,FAC_TIME)
          CALL C_H3D_CREATE_COMPONENTS(IPART,LIPART1,NPART,LTITR,IGEO,NPROPGI,
     .                                H3D_DATA%PARTS(1)%PART,
     .                                 NRBODY, NRWALL, NOM_OPT, LNOPT1, I16D, NPBY, NNPBY,
     .                                 SUB_NCHILD, NSUBS, NRBE2, NRBE3, I16L, I16M, N2D ,IRBE2,
     .                                 NRBE2L,SUB_ID,SUB_CHILD,SUB_LEVEL,SUB_IAD,SUB_TITLE,IRBE3,
     .                                 NRBE3L,COMPID_RBODIES,COMPID_RBE2S,COMPID_RBE3S)
          CALL STOPTIME(MACRO_TIMER_LIBH3D,1)


C
        ELSEIF( H3D_DATA%IH3D_RUN == 1 .AND. IRUN > 1) THEN
c**********************************************************************************************
C OPEN H3D LIB
C********************************************************************************************** 
          ERROR_LOAD = 0
          CALL H3DLIB_LOAD(ERROR_LOAD)
          IF(ERROR_LOAD == 1) THEN
            CALL ANCMSG(MSGID=274,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
C
c**********************************************************************************************
C REOPEN H3D FILE
C********************************************************************************************** 
          INQUIRE( FILE=TMP_NAME(1:LEN_TMP_NAME), EXIST=IS_FILE_EXISTS ) 
          IF (IS_FILE_EXISTS) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            IS_H3D_USED = .TRUE.
            CALL C_H3D_REOPEN_FILE(TMP_NAME,LEN_TMP_NAME,H3D_DATA%PERCENTAGE_ERROR,H3D_DATA%COMP_LEVEL)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
          ELSE
c**********************************************************************************************
C OPEN H3D FILE
C********************************************************************************************** 
            WRITE(CH_H3D,'(I4.4)')IRUN
            FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CH_H3D//'.h3d'
            LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM)) 
            H3D_DATA%IH3D = 1
            FILEN = ROOTLEN + 9
            IS_H3D_USED = .TRUE.
C
            WRITE(RADVERS,'(A,A)') 'Radioss ',VERS
            LEN_RADVERS = LEN_TRIM(RADVERS)
C
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_OPEN_FILE(TMP_NAME,LEN_TMP_NAME,H3D_DATA%PERCENTAGE_ERROR,H3D_DATA%COMP_LEVEL,
     .                           RADVERS,LEN_RADVERS,FAC_MASS,FAC_LENGTH,FAC_TIME)
            CALL C_H3D_CREATE_COMPONENTS(IPART,LIPART1,NPART,LTITR,IGEO,NPROPGI,H3D_DATA%PARTS(1)%PART,
     .                                 NRBODY, NRWALL, NOM_OPT, LNOPT1, I16D, NPBY, NNPBY,
     .                                 SUB_NCHILD, NSUBS, NRBE2G, NRBE3G, I16E, I16F, N2D  ,IRBE2,
     .                                 NRBE2L,SUB_ID,SUB_CHILD,SUB_LEVEL,SUB_IAD,SUB_TITLE,IRBE3,
     .                                 NRBE3L,COMPID_RBODIES,COMPID_RBE2S,COMPID_RBE3S)
             CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
        ENDIF
      ENDIF
c**********************************************************************************************
C CREATE ITAB_P
C********************************************************************************************** 
      IF(NSPMD > 1) THEN 
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,ITAB,NUMNOD,ITAB_P,NUMNODG) 
        CALL SPMD_H3D_GATHER_R_NODE(WEIGHT,X,3*NUMNOD,X_P,3*NUMNODG) 
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ELSE
        DO I=1,NUMNOD
          ITAB_P(I) = ITAB(I)
        ENDDO
      ENDIF
c**********************************************************************************************
C H3D/PART
C********************************************************************************************** 
      IF (ISPMD == 0)THEN
        ALLOCATE(TAGNOD_P(NUMNODG))
      ELSE
        ALLOCATE(TAGNOD_P(1))
      ENDIF

      IF(H3D_DATA%IPART_SELECT == 1) THEN
        ALLOCATE(TAGNOD(NUMNOD))
        TAGNOD(1:NUMNOD) = 0
c
c
        DO I=1,NUMSPH
          IF (H3D_DATA%PARTS(1)%PART(IPARTSP(I)) == 1) THEN
            IF(KXSP(2,I) > 0 )TAGNOD(KXSP(2,I)) = 1
          ENDIF
        ENDDO
c
        DO I=1,NUMELR
          IF (H3D_DATA%PARTS(1)%PART(IPARTR(I)) == 1) THEN
            DO J=2,4
              IF(IXR(J,I) > 0 )TAGNOD(IXR(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c
        DO I=1,NUMELP
          IF (H3D_DATA%PARTS(1)%PART(IPARTP(I)) == 1) THEN
            DO J=2,4
              IF(IXP(J,I) > 0 )TAGNOD(IXP(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c
        DO I=1,NUMELT
          IF (H3D_DATA%PARTS(1)%PART(IPARTT(I)) == 1) THEN
            DO J=2,4
              IF(IXT(J,I) > 0 )TAGNOD(IXT(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c
        DO I=1,NRBODY
          IF(NPBY(1,I) > 0 ) TAGNOD(NPBY(1,I)) = 1
          DO J=1,NPBY(2,I)
           IF(LPBY(NPBY(11,I)+J) > 0)  TAGNOD(LPBY(NPBY(11,I)+J)) = 1
          ENDDO
        ENDDO
c
        DO I=1,NUMELC
          IF (H3D_DATA%PARTS(1)%PART(IPARTC(I)) == 1) THEN
            DO J=2,5
              IF(IXC(J,I) > 0 )TAGNOD(IXC(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c
        DO I=1,NUMELTG
          IF (H3D_DATA%PARTS(1)%PART(IPARTTG(I)) == 1) THEN
            DO J=2,4
              TAGNOD(IXTG(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c
        DO I=1,NUMELS
          IF (H3D_DATA%PARTS(1)%PART(IPARTS(I)) == 1) THEN
            DO J=2,9
              IF(IXS(J,I) > 0 )TAGNOD(IXS(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c
        DO I=1,NUMELQ
          IF (H3D_DATA%PARTS(1)%PART(IPARTQ(I)) == 1) THEN
            DO J=2,5
              IF(IXQ(J,I) > 0 )TAGNOD(IXQ(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO

        NUMNOD_H3DPART = 0

        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(NUMNOD,1,NUMNOD_P,NSPMD) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          IF(ISPMD == 0) THEN
C bug fix
          DO I=1,NSPMD
            NUMNOD_H3DPART = NUMNOD_H3DPART + NUMNOD_P(I)
          ENDDO
        ENDIF
        ENDIF
C
        ALLOCATE(TAGNOD_P_PART(NUMNOD_H3DPART))
        ALLOCATE(ITAB_P_PART(NUMNOD_H3DPART))
        ALLOCATE(ITABM1_P(2*NUMNODG))
C
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,TAGNOD,NUMNOD,TAGNOD_P,NUMNODG) 
          CALL SPMD_H3D_GATHER_I_NODE_PART(WEIGHT,TAGNOD,ITAB,NUMNOD,ITAB_P_PART,NUMNOD_H3DPART) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          IF(ISPMD == 0) THEN 
            CALL H3D_CONSTIT(ITAB_P,ITABM1_P,NUMNODG)
            DO I=1,NUMNOD_H3DPART
              IF(ITAB_P_PART(I) .NE. 0)THEN
                IF(SYSFUS2(ITAB_P_PART(I),ITABM1_P,NUMNODG) .NE. 0) THEN
                  TAGNOD_P(SYSFUS2(ITAB_P_PART(I),ITABM1_P,NUMNODG)) = 1
                ENDIF 
              ENDIF 
            ENDDO 
          ENDIF
        ELSE
          DO I=1,NUMNOD
            TAGNOD_P(I) = TAGNOD(I)
          ENDDO
        ENDIF
        DEALLOCATE(TAGNOD_P_PART)
        DEALLOCATE(ITAB_P_PART)
        DEALLOCATE(ITABM1_P)
      ELSE IF(ISPMD == 0) THEN
        DO I=1,NUMNODG
          TAGNOD_P(I) = 1
        ENDDO
      ENDIF
c**********************************************************************************************
C CREATE NODES
C********************************************************************************************** 
c
      IF(NSPMD > 1 .AND. ISPMD.EQ.0)THEN
        MAX_NOD_ID = 0
        DO I=1,NUMNODG
          MAX_NOD_ID = MAX(MAX_NOD_ID,ITAB_P(I))
        ENDDO
      ELSEIF(NSPMD == 1)THEN
        MAX_NOD_ID = 0
        DO I=1,NUMNOD
          MAX_NOD_ID = MAX(MAX_NOD_ID,ITAB(I))
        ENDDO
      ENDIF
      IF(NSPMD > 1 .AND. ISPMD.EQ.0 .AND. H3D_DATA%IH3D == 1 )THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_CREATE_NODES(ITAB_P,NUMNODG,X_P,TAGNOD_P,D_P)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

      ELSEIF(ISPMD.EQ.0  .AND. H3D_DATA%IH3D == 1)THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_CREATE_NODES(ITAB,NUMNOD,X,TAGNOD_P,D)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

      ENDIF
      DEALLOCATE(X_P)
c**********************************************************************************************
C CREATE SPH
C********************************************************************************************** 
c
      IF(H3D_DATA%IH3D == 1) THEN
        IF(NSPMD > 1) THEN 
          DO I=1,NUMSPH
            DO J=1,2
              KXSP_TMP(J,I) = KXSP(J,I)
            ENDDO
            KXSP_TMP(3,I) = ITAB(KXSP(3,I))
            DO J=4,NISP
              KXSP_TMP(J,I) = KXSP(J,I)
            ENDDO
          ENDDO
c
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(KXSP_TMP,NISP*NUMSPH,KXSP_P,NISP*NUMSPHG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
c
        ELSE
          DO I=1,NUMSPH
            DO J=1,2
              KXSP_P(J,I) = KXSP(J,I)
            ENDDO
            KXSP_P(3,I) = ITAB(KXSP(3,I))
            DO J=4,NISP
              KXSP_P(J,I) = KXSP(J,I)
            ENDDO
          ENDDO
        ENDIF
c
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTSP,NUMSPH,IPARTSP_P,NUMSPHG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMSPH
            IPARTSP_P(I) = IPARTSP(I)
          ENDDO
        ENDIF
      ENDIF

      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN 
                 CALL STARTIME(MACRO_TIMER_LIBH3D,1)
                 CALL C_H3D_CREATE_SPH(ITAB_P,NUMNODG,KXSP_P,NISP,NUMSPHG,IPARTSP_P,IPART,LIPART1,X,H3D_DATA%PARTS(1)%PART)
                 CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
c**********************************************************************************************
C CREATE 1D CONNECTIVITY
C**********************************************************************************************
C
C         CREATE SPRINGS
C
      IF(H3D_DATA%IH3D == 1) THEN
        IF(NSPMD > 1) THEN 
          DO I=1,NUMELR
            IXR_TMP(1,I) = IXR(1,I)
            DO J=2,4
              IF (IXR(J,I) .NE. 0 )IXR_TMP(J,I) = ITAB(IXR(J,I))
            ENDDO
            IXR_TMP(4:NIXR,I) = IXR(4:NIXR,I)
          ENDDO
c
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXR_TMP,NIXR*NUMELR,IXR_P,NIXR*NUMELRG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
c
        ELSE
          DO I=1,NUMELR
            IXR_P(1,I) = IXR(1,I)
            DO J=2,4
               IF (IXR(J,I) .NE. 0 ) IXR_P(J,I) = ITAB(IXR(J,I))
            ENDDO
            IXR_P(4:NIXR,I) = IXR(4:NIXR,I)
          ENDDO
        ENDIF
c
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTR,NUMELR,IPARTR_P,NUMELRG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMELR
            IPARTR_P(I) = IPARTR(I)
          ENDDO
        ENDIF
      ENDIF
c
      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN 
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_CREATE_SPRINGS(ITAB,NUMNOD,IXR_P,NIXR,NUMELRG,IPARTR_P,IPART,LIPART1,H3D_DATA%PARTS(1)%PART)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
C
C         CREATE BEAMS
C
C
      IF(H3D_DATA%IH3D == 1) THEN
        IF(NSPMD > 1) THEN 
          DO I=1,NUMELP
            IXP_TMP(1,I) = IXP(1,I)
            DO J=2,4
              IXP_TMP(J,I) = ITAB(IXP(J,I))
            ENDDO
            IXP_TMP(4:NIXP,I) = IXP(4:NIXP,I)
          ENDDO
c
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXP_TMP,NIXP*NUMELP,IXP_P,NIXP*NUMELPG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
c
        ELSE
          DO I=1,NUMELP
            IXP_P(1,I) = IXP(1,I)
            DO J=2,4
              IXP_P(J,I) = ITAB(IXP(J,I))
            ENDDO
            IXP_P(4:NIXP,I) = IXP(4:NIXP,I)
          ENDDO
        ENDIF
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTP,NUMELP,IPARTP_P,NUMELPG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMELP
            IPARTP_P(I) = IPARTP(I)
          ENDDO
        ENDIF
      ENDIF
c
c
      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN 
         CALL STARTIME(MACRO_TIMER_LIBH3D,1)
         CALL C_H3D_CREATE_BEAMS(ITAB,NUMNOD,IXP_P,NIXP,NUMELPG,IPARTP_P,IPART,LIPART1,H3D_DATA%PARTS(1)%PART)
         CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
C
C         CREATE TRUSS
C
C
      IF(H3D_DATA%IH3D == 1) THEN
        IF(NSPMD > 1) THEN 
          DO I=1,NUMELT
            IXT_TMP(1,I) = IXT(1,I)
            DO J=2,4
              IXT_TMP(J,I) = ITAB(IXT(J,I))
            ENDDO
            IXT_TMP(4:NIXT,I) = IXT(4:NIXT,I)
          ENDDO
c
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXT_TMP,NIXT*NUMELT,IXT_P,NIXP*NUMELTRG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
c
        ELSE
          DO I=1,NUMELT
            IXT_P(1,I) = IXT(1,I)
            DO J=2,4
              IXT_P(J,I) = ITAB(IXT(J,I))
            ENDDO
            IXT_P(4:NIXT,I) = IXT(4:NIXT,I)
          ENDDO
        ENDIF
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTT,NUMELT,IPARTT_P,NUMELTRG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMELT
            IPARTT_P(I) = IPARTT(I)
          ENDDO
        ENDIF
      ENDIF
c
c
      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_CREATE_TRUSS(ITAB,NUMNOD,IXT_P,NIXT,NUMELTRG,IPARTT_P,IPART,LIPART1,H3D_DATA%PARTS(1)%PART)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
C
c**********************************************************************************************
C CREATE RBODY CONNECTIVITY
C********************************************************************************************** 

      IF(NSPMD > 1 .AND. H3D_DATA%IH3D == 1)THEN
             NERBY = 0
             IF (NRBODY.GT.0)
     .           CALL DRBYCNT(NERBY,NPBY,FR_RBY2)
             NERBE2 = 0
             IF (NRBE2G.GT.0)
     .         CALL DRBE2CNT(NERBE2,IRBE2,LRBE2,WEIGHT)
             NERBE3 = 0
             IF (NRBE3G.GT.0)
     .         CALL DRBE3CNT(NERBE3,IRBE3,LRBE3,WEIGHT)
c
             SBUFSPM=0
             SBUFRECVM=0
             SBUFSPO=0
             SPORBY=0

             DO I=1,NSPMD
               SBUFSPM = SBUFSPM + IAD_RBY2(1,I)
               SBUFRECVM = SBUFRECVM + IAD_RBY2(2,I)+1
             ENDDO
               SBUFSPM = SBUFSPM + 2*NRBYKIN
               SBUFRECVM = SBUFRECVM + 2*NRBYKIN*NSPMD
             DO I=1,NRBYKIN

               IF ((ISPMD+1).EQ.ABS(FR_RBY2(3,I)))
     .           SBUFSPO = SBUFSPO + FR_RBY2(2,I)
             ENDDO
             SBUFSPO = SBUFSPO + NRBYKIN*2
             IF (ISPMD.EQ.0) THEN
               SPORBY = NERBY+NRBYKIN*2
             ELSE
               SPORBY=1
             ENDIF

        CALL H3D_CREATE_RBODIES_IMPI(NPBY,LPBY,FR_RBY2,IAD_RBY2,
     .                        SBUFSPM,SBUFRECVM,SBUFSPO,SPORBY,
     .                        NODGLOB,WEIGHT,ITAB,COMPID_RBODIES)

        CALL H3D_CREATE_RBE2_IMPI(LRBE2, IRBE2,NODGLOB,WEIGHT,NERBE2,
     .                         NERBE2T,ITAB,COMPID_RBE2S)

        CALL H3D_CREATE_RBE3_IMPI(LRBE3, IRBE3,NODGLOB,WEIGHT,NERBE3,
     .                         NERBE3T,ITAB,COMPID_RBE3S)

      ELSE
        IF(NSPMD == 1 .AND. ISPMD.EQ.0 .AND. H3D_DATA%IH3D == 1) THEN 
          CALL STARTIME(MACRO_TIMER_LIBH3D,1)
          CALL C_H3D_CREATE_RBODIES(ITAB,NUMNOD,NPBY,NNPBY,LPBY,NRBODY,COMPID_RBODIES)
          CALL C_H3D_CREATE_RBE2(ITAB,NUMNOD,IRBE2,NRBE2L,LRBE2,NRBE2,COMPID_RBE2S,COMPID_RBE2S)
          CALL C_H3D_CREATE_RBE3(ITAB,NUMNOD,IRBE3,NRBE3L,LRBE3,NRBE3,COMPID_RBE3S,COMPID_RBE3S)
          CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
        ENDIF
      ENDIF
C
c**********************************************************************************************
C CREATE SH4N & SH3N CONNECTIVITY
C********************************************************************************************** 
c
      IF(H3D_DATA%IH3D  == 1) THEN
        IF(NSPMD > 1 .AND. H3D_DATA%IH3D == 1) THEN 
          DO I=1,NUMELC
            IXC_TMP(1,I) = IXC(1,I)
            DO J=2,5
              IXC_TMP(J,I) = ITAB(IXC(J,I))
            ENDDO
            IXC_TMP(6:NIXC,I) = IXC(6:NIXC,I)
          ENDDO
c
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXC_TMP,NIXC*NUMELC,IXC_P,NIXC*NUMELCG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
c
          DO I=1,NUMELTG
            IXTG_TMP(1,I) = IXTG(1,I)
            DO J=2,4
              IXTG_TMP(J,I) = ITAB(IXTG(J,I))
            ENDDO
            IXTG_TMP(5:NIXTG,I) = IXTG(5:NIXTG,I)
          ENDDO
c
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXTG_TMP,NIXTG*NUMELTG,IXTG_P,NIXTG*NUMELTGG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
c
        ELSE
          DO I=1,NUMELC
            IXC_P(1,I) = IXC(1,I)
            DO J=2,5
              IXC_P(J,I) = ITAB(IXC(J,I))
            ENDDO
            IXC_P(6:NIXC,I) = IXC(6:NIXC,I)
          ENDDO

          DO I=1,NUMELTG
            IXTG_P(1,I) = IXTG(1,I)
            DO J=2,4
              IXTG_P(J,I) = ITAB(IXTG(J,I))
            ENDDO
            IXTG_P(5:NIXTG,I) = IXTG(5:NIXTG,I)
          ENDDO
          
        ENDIF
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTC,NUMELC,IPARTC_P,NUMELCG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTTG,NUMELTG,IPARTTG_P,NUMELTGG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMELC
            IPARTC_P(I) = IPARTC(I)
          ENDDO
          DO I=1,NUMELTG
            IPARTTG_P(I) = IPARTTG(I)
          ENDDO
        ENDIF
      ENDIF
c


      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN 
          CALL STARTIME(MACRO_TIMER_LIBH3D,1)
          CALL C_H3D_CREATE_SHELLS(ITAB_P,NUMNODG,IXC_P,NIXC,NUMELCG,IPARTC_P,IPART,LIPART1,
     .                             H3D_DATA%PARTS(1)%PART)
          CALL C_H3D_CREATE_SH3NS(ITAB_P,NUMNODG,IXTG_P,NIXTG,NUMELTGG,IPARTTG_P,IPART,LIPART1,
     .                                    H3D_DATA%PARTS(1)%PART)
          CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
C
c**********************************************************************************************
C CREATE RWALLS
C********************************************************************************************** 
c
      IF(H3D_DATA%IH3D  == 1 .AND. NRWALL.GT.0) THEN

         CALL SCANOR(X,D,CDG,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,SCALE,
     .            WEIGHT)
C

         CALL H3D_DXYZ_RWALL(
     2                  NSTRF,RWBUF,NPRW  ,X,XMIN,
     3                  YMIN,ZMIN,XMAX,YMAX,ZMAX,
     4                  FR_SEC,FR_WALL,WEIGHT,ITAB,
     5                  XWL ,YWL , ZWL, V1, V2, V3, VV1, VV2, VV3, XL, XN, YN, ZN )


        IF(ISPMD.EQ.0) THEN 
          CALL STARTIME(MACRO_TIMER_LIBH3D,1)
          CALL C_H3D_CREATE_RWALLS(NOM_OPT, LNOPT1, I16D, NPRW, NRWALL, MAX_NOD_ID,
     .                           XWL ,YWL , ZWL, V1, V2, V3, VV1, VV2, VV3, XL, XN, YN, ZN)
          CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
        ENDIF

      ENDIF
C
c**********************************************************************************************
C CREATE SOLIDS CONNECTIVITY
C********************************************************************************************** 
c
      IF(H3D_DATA%IH3D == 1) THEN

        IF(NSPMD > 1) THEN 
          ALLOCATE(IXS_TMP(NIXS,NUMELS8))
c
          DO I=1,NUMELS8
            IXS_TMP(1,I) = IXS(1,I)
            DO J=2,9
              IXS_TMP(J,I) = ITAB(IXS(J,I))
            ENDDO
            IXS_TMP(10:NIXS,I) = IXS(10:NIXS,I)
          ENDDO
c
          DO I=1,NUMELS10
            IXS10_TMP(1,I) = ITAB(IXS(2,NUMELS8+I))
            IXS10_TMP(2,I) = ITAB(IXS(4,NUMELS8+I))
            IXS10_TMP(3,I) = ITAB(IXS(7,NUMELS8+I))
            IXS10_TMP(4,I) = ITAB(IXS(6,NUMELS8+I))
            DO J=1,6
			  IF (IXS10(J,I)>0) THEN
                IXS10_TMP(4+J,I) = ITAB(IXS10(J,I))
		      ELSE
                IXS10_TMP(4+J,I) = 0
			  END IF
            ENDDO
            IXS10_TMP(11,I) = IXS(NIXS,NUMELS8+I)
          ENDDO
c
c
          DO I=1,NUMELS16
            IXS16_TMP(1,I) = ITAB(IXS(2,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_TMP(2,I) = ITAB(IXS(3,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_TMP(3,I) = ITAB(IXS(4,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_TMP(4,I) = ITAB(IXS(5,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_TMP(5,I) = ITAB(IXS(6,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_TMP(6,I) = ITAB(IXS(7,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_TMP(7,I) = ITAB(IXS(8,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_TMP(8,I) = ITAB(IXS(9,NUMELS8+NUMELS10+NUMELS20+I))
            DO J=1,8
              IXS16_TMP(8+J,I) = ITAB(IXS16(J,I))
            ENDDO
            IXS16_TMP(17,I) = IXS(NIXS,NUMELS8+NUMELS10+NUMELS20+I)
          ENDDO
c
c
          DO I=1,NUMELS20
            IXS20_TMP(1,I) = ITAB(IXS(2,NUMELS8+NUMELS10+I))
            IXS20_TMP(2,I) = ITAB(IXS(3,NUMELS8+NUMELS10+I))
            IXS20_TMP(3,I) = ITAB(IXS(4,NUMELS8+NUMELS10+I))
            IXS20_TMP(4,I) = ITAB(IXS(5,NUMELS8+NUMELS10+I))
            IXS20_TMP(5,I) = ITAB(IXS(6,NUMELS8+NUMELS10+I))
            IXS20_TMP(6,I) = ITAB(IXS(7,NUMELS8+NUMELS10+I))
            IXS20_TMP(7,I) = ITAB(IXS(8,NUMELS8+NUMELS10+I))
            IXS20_TMP(8,I) = ITAB(IXS(9,NUMELS8+NUMELS10+I))
            DO J=1,12
              IXS20_TMP(8+J,I) = ITAB(IXS20(J,I))
            ENDDO
            IXS20_TMP(21,I) = IXS(NIXS,NUMELS8+NUMELS10+I)
          ENDDO
c
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXS_TMP,NIXS*NUMELS8,IXS_P,NIXS*(NUMELSG-NUMELS10G-NUMELS16G-NUMELS20G)) 
          CALL SPMD_H3D_GATHER_I(IXS10_TMP,11*NUMELS10,IXS10_P,11*NUMELS10G) 
          CALL SPMD_H3D_GATHER_I(IXS16_TMP,17*NUMELS16,IXS16_P,17*NUMELS16G) 
          CALL SPMD_H3D_GATHER_I(IXS20_TMP,21*NUMELS20,IXS20_P,21*NUMELS20G) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          DEALLOCATE(IXS_TMP)
        ELSE
c
          DO I=1,NUMELS8
            IXS_P(1,I) = IXS(1,I)
            DO J=2,9
              IXS_P(J,I) = ITAB(IXS(J,I))
            ENDDO
            IXS_P(10:NIXS,I) = IXS(10:NIXS,I)
          ENDDO
c
          DO I=1,NUMELS10
            IXS10_P(1,I) = ITAB(IXS(2,NUMELS8+I))
            IXS10_P(2,I) = ITAB(IXS(4,NUMELS8+I))
            IXS10_P(3,I) = ITAB(IXS(7,NUMELS8+I))
            IXS10_P(4,I) = ITAB(IXS(6,NUMELS8+I))
            DO J=1,6
              IXS10_P(4+J,I) = ITAB(IXS10(J,I))
            ENDDO
            IXS10_P(11,I) = IXS(NIXS,NUMELS8+I)
          ENDDO
c
          DO I=1,NUMELS16
            IXS16_P(1,I) = ITAB(IXS(2,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_P(2,I) = ITAB(IXS(3,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_P(3,I) = ITAB(IXS(4,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_P(4,I) = ITAB(IXS(5,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_P(5,I) = ITAB(IXS(6,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_P(6,I) = ITAB(IXS(7,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_P(7,I) = ITAB(IXS(8,NUMELS8+NUMELS10+NUMELS20+I))
            IXS16_P(8,I) = ITAB(IXS(9,NUMELS8+NUMELS10+NUMELS20+I))
            DO J=1,8
              IXS16_P(8+J,I) = IXS16(J,I)
            ENDDO
            IXS16_P(17,I) = IXS(NIXS,NUMELS8+NUMELS10+NUMELS20+I)
          ENDDO
c
          DO I=1,NUMELS20
            IXS20_P(1,I) = ITAB(IXS(2,NUMELS8+NUMELS10+I))
            IXS20_P(2,I) = ITAB(IXS(3,NUMELS8+NUMELS10+I))
            IXS20_P(3,I) = ITAB(IXS(4,NUMELS8+NUMELS10+I))
            IXS20_P(4,I) = ITAB(IXS(5,NUMELS8+NUMELS10+I))
            IXS20_P(5,I) = ITAB(IXS(6,NUMELS8+NUMELS10+I))
            IXS20_P(6,I) = ITAB(IXS(7,NUMELS8+NUMELS10+I))
            IXS20_P(7,I) = ITAB(IXS(8,NUMELS8+NUMELS10+I))
            IXS20_P(8,I) = ITAB(IXS(9,NUMELS8+NUMELS10+I))
            DO J=1,12
              IXS20_P(8+J,I) = ITAB(IXS20(J,I))
            ENDDO
            IXS20_P(21,I) = IXS(NIXS,NUMELS8+NUMELS10+I)
          ENDDO
        ENDIF
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTS,NUMELS8,IPARTS_P,NUMELSG-NUMELS10G-NUMELS16G-NUMELS20G) 
          CALL SPMD_H3D_GATHER_I(IPARTS(NUMELS8+1),NUMELS10,IPARTS10_P,NUMELS10G) 
          CALL SPMD_H3D_GATHER_I(IPARTS(NUMELS8+NUMELS10+1),NUMELS20,IPARTS20_P,NUMELS20G) 
          CALL SPMD_H3D_GATHER_I(IPARTS(NUMELS8+NUMELS10+NUMELS20+1),NUMELS16,IPARTS16_P,NUMELS16G) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMELS8
            IPARTS_P(I) = IPARTS(I)
          ENDDO
          DO I=1,NUMELS10
            IPARTS10_P(I) = IPARTS(I+NUMELS8)
          ENDDO
          DO I=1,NUMELS20
            IPARTS20_P(I) = IPARTS(I+NUMELS8+NUMELS10)
          ENDDO
          DO I=1,NUMELS16
            IPARTS16_P(I) = IPARTS(I+NUMELS8+NUMELS10+NUMELS20)
          ENDDO
        ENDIF
      ENDIF
c
c
      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN 
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_CREATE_SOLID8N(ITAB_P,NUMNODG,IXS_P,NIXS,NUMELSG,IPARTS_P,IPART,LIPART1,
     .                  H3D_DATA%PARTS(1)%PART,NUMELS10G,IXS10_P,IPARTS10_P,NUMELS16G,IXS16_P,
     .                  IPARTS16_P,NUMELS20G,IXS20_P,IPARTS20_P)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
C
c**********************************************************************************************
C CREATE QUADS CONNECTIVITY
C********************************************************************************************** 
c
      IF(H3D_DATA%IH3D == 1) THEN
        IF(NSPMD > 1) THEN 
          DO I=1,NUMELQ
            IXQ_TMP(1,I) = IXQ(1,I)
            DO J=2,5
              IXQ_TMP(J,I) = ITAB(IXQ(J,I))
            ENDDO
            IXQ_TMP(6:NIXQ,I) = IXQ(6:NIXQ,I)
          ENDDO
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXQ_TMP,NIXQ*NUMELQ,IXQ_P,NIXQ*NUMELQG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMELQ
            IXQ_P(1,I) = IXQ(1,I)
            DO J=2,5
              IXQ_P(J,I) = ITAB(IXQ(J,I))
            ENDDO
            IXQ_P(6:NIXQ,I) = IXQ(6:NIXQ,I)
          ENDDO
        ENDIF
        IF(NSPMD > 1) THEN 
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IPARTQ,NUMELQ,IPARTQ_P,NUMELQG) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          DO I=1,NUMELQ
            IPARTQ_P(I) = IPARTQ(I)
          ENDDO
        ENDIF
      ENDIF
c
c
      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN 
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_CREATE_QUADS(ITAB_P,NUMNODG,IPART,LIPART1,H3D_DATA%PARTS(1)%PART,
     .                  IXQ_P,NIXQ,NUMELQG,IPARTQ_P)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
c**********************************************************************************************
C CREATE SKIN (4-N) CONNECTIVITY ( THICK SHELLS +solid +PLOAD...)
C**********************************************************************************************
C-------------nodal_pid used for PLOAD---        
       IF (NUMSKINP>0) THEN
          ALLOCATE(NODAL_IPART(NUMNOD))
          CALL GET_NODAL_IPART(ELBUF_TAB,IPARG,IPARTC,IPARTTG,IPARTS,
     .                          IXC, IXTG, IXS,IXS10,IXS16,IXS20,
     .                          NODAL_IPART)
       END IF
      IF(H3D_DATA%IH3D == 1 .AND. NUMSKING>0 ) THEN
C----create connectivity IXSKIN_P(NIXQ,NUMSKING),IPARTSKIN_P(NUMSKING) 
        IXSKIN_TMP = 0 
        CALL H3D_SKIN_IXSKIN(ELBUF_TAB,IPARG,IPARTS,IXS,IXS10,
     .                       ITAB ,IXSKIN_TMP  ,TAG_SKINS6,
     .                       IBCL,ILOADP,LLOADP,NODAL_IPART) 
c--------IXSKIN_TMP(1,i) :ipart (instead of mid)--IXSKIN_TMP(2-5,i) : Nj(user_id)
        IF(NSPMD > 1) THEN 
C         
          CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
          CALL SPMD_H3D_GATHER_I(IXSKIN_TMP,NIXSKIN*NUMSKIN,IXSKIN_P,NIXSKIN*NUMSKING) 
          CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
        ELSE
          IXSKIN_P(1:NIXSKIN,1:NUMSKIN) = IXSKIN_TMP(1:NIXSKIN,1:NUMSKIN)
        END IF

c
        IF(ISPMD == 0) THEN 
C-----usr_id not important but unique ---------        
          DO I=1,NUMSKING
            IPARTSKIN_P(I) = IXSKIN_P(1,I)
            IXSKIN_P(NIXSKIN,I) = I 
          ENDDO
          CALL STARTIME(MACRO_TIMER_LIBH3D,1)
C 
          CALL C_H3D_CREATE_SKINS(ITAB_P,NUMNODG,IPART,LIPART1,H3D_DATA%PARTS(1)%PART,
     .                            IXSKIN_P,NIXSKIN,NUMSKING,IPARTSKIN_P)
          CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
        ENDIF
      ENDIF
C
c**********************************************************************************************
C CREATE NODAL DISPLACEMENT DATAS
C********************************************************************************************** 
      IF(ISPMD.EQ.0.AND. H3D_DATA%IH3D == 1) THEN 
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_CREATE_DISPLACEMENT_DATATYPE()
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
C
c**********************************************************************************************
C CREATE RESULTS DATAS
C********************************************************************************************** 
      IF(ISPMD.EQ.0.AND. ( H3D_DATA%IH3D_RUN == 1)) THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL H3D_CREATE_DATATYPE(H3D_DATA,IPARI) 
C
c**********************************************************************************************
C FINALIZE FORMAT
C********************************************************************************************** 
       CALL C_H3D_CREATE_RESULTS_END()
       CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

      ENDIF
C
c**********************************************************************************************
C UPDATE NODES DISPLACEMENTS
c**********************************************************************************************
c
      IF(NRWALL.GT.0) THEN

         CALL SCANOR(X,D,CDG,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,SCALE,
     .            WEIGHT)
C

         CALL H3D_DXYZ_RWALL_UPDATE(
     2                  NSTRF,RWBUF,NPRW  ,D,XMIN,
     3                  YMIN,ZMIN,XMAX,YMAX,ZMAX,
     4                  FR_SEC,FR_WALL,WEIGHT,ITAB,
     5                  XWL ,YWL , ZWL, V1, V2, V3, VV1, VV2, VV3)
      ENDIF
c
      H3DTITLE = ' '
      LEN_H3DTITLE = 0
      IF (H3D_DATA%N_TITLE .NE. 0) THEN
        DO I=1,H3D_DATA%N_TITLE
          IF(H3D_DATA%ITITLE(I) == H3D_DATA%IH3D) THEN
            H3DTITLE = H3D_DATA%TITLE(I)
            LEN_H3DTITLE = LEN_TRIM(H3D_DATA%TITLE(I))
          ENDIF
        ENDDO
      ENDIF
c
      IF(NSPMD > 1 .AND. ISPMD.EQ.0)THEN
         CALL STARTIME(MACRO_TIMER_LIBH3D,1)
         CALL C_H3D_UPDATE_NODES(H3DTITLE,LEN_H3DTITLE,TT,H3D_DATA%IH3D,
     .                           ITAB_P,NUMNODG,D_P, NRWALL, MAX_NOD_ID,
     .                           XWL ,YWL , ZWL, V1, V2, 
     .                           V3, VV1, VV2, VV3,KXSP_P,
     .                           NISP,NUMSPHG,TAGNOD_P,NPRW)
         CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ELSEIF(ISPMD.EQ.0 )THEN
         CALL STARTIME(MACRO_TIMER_LIBH3D,1)
         CALL C_H3D_UPDATE_NODES(H3DTITLE,LEN_H3DTITLE,TT,H3D_DATA%IH3D,
     .                           ITAB,NUMNOD,D, NRWALL, MAX_NOD_ID,
     .                           XWL ,YWL , ZWL, V1, V2,
     .                           V3, VV1, VV2, VV3,KXSP_P,
     .                           NISP,NUMSPHG,TAGNOD_P,NPRW)
         CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
c 
c**********************************************************************************************
C OUTPUT RESULTS
c**********************************************************************************************
      DEALLOCATE(D_P)
      ALLOCATE(NODAL_SCALAR(NUMNOD))
      ALLOCATE(NODAL_VECTOR(3*NUMNOD))
      ALLOCATE(NODAL_TENSOR(6*NUMNOD))
      ALLOCATE(NODE_ID(NUMNOD))
      ALLOCATE(IS_WRITEN_NODE(NUMNOD))
      IS_WRITEN_NODE(1:NUMNOD) = 0
c
      IF (ISPMD == 0 )THEN
        ALLOCATE(IS_WRITEN_NODE_P(NUMNODG))
        ALLOCATE(NODAL_SCALAR_P(NUMNODG))
        ALLOCATE(NODAL_VECTOR_P(3*NUMNODG))
        ALLOCATE(NODAL_TENSOR_P(6*NUMNODG))
        ALLOCATE(NODE_ID_P(NUMNODG))
        IS_WRITEN_NODE_P(1:NUMNODG) = 0
      ELSE
        ALLOCATE(IS_WRITEN_NODE_P(1))
        ALLOCATE(NODAL_SCALAR_P(1))
        ALLOCATE(NODAL_VECTOR_P(1))
        ALLOCATE(NODAL_TENSOR_P(1))
        ALLOCATE(NODE_ID_P(1))
        IS_WRITEN_NODE_P(1) = 0
      ENDIF
c
c
      ALLOCATE(ONED_SCALAR(NUMELR+NUMELP+NUMELT))
      ALLOCATE(ONED_VECTOR(3*(NUMELR+NUMELP+NUMELT)))
      ALLOCATE(ONED_TENSOR(6*(NUMELR+NUMELP+NUMELT)))
      ALLOCATE(ONED_TORSOR(9*(NUMELR+NUMELP+NUMELT)))
      ALLOCATE(ONED_ID(NUMELRG+NUMELPG+NUMELTRG))
      ALLOCATE(ONED_ITY(NUMELRG+NUMELPG+NUMELTRG))
      ALLOCATE(IS_WRITEN_ONED(NUMELRG+NUMELPG+NUMELTRG))
      ONED_ID(1:NUMELR+NUMELP+NUMELT) = 0
      ONED_ITY(1:NUMELR+NUMELP+NUMELT) = 0
      IS_WRITEN_ONED(1:NUMELR+NUMELP+NUMELT) = 0
c
      IF (ISPMD == 0 )THEN
        ALLOCATE(ONED_SCALAR_P(NUMELRG+NUMELPG+NUMELTRG))
        ALLOCATE(ONED_VECTOR_P(3*(NUMELRG+NUMELPG+NUMELTRG)))
        ALLOCATE(ONED_TENSOR_P(6*(NUMELRG+NUMELPG+NUMELTRG)))
        ALLOCATE(ONED_TORSOR_P(9*(NUMELRG+NUMELPG+NUMELTRG)))
        ALLOCATE(ONED_ID_P(NUMELRG+NUMELPG+NUMELTRG))
        ALLOCATE(ONED_ITY_P(NUMELRG+NUMELPG+NUMELTRG))
        ALLOCATE(IS_WRITEN_ONED_P(NUMELRG+NUMELPG+NUMELTRG))
        IS_WRITEN_ONED_P(1:NUMELRG+NUMELPG+NUMELTRG) = 0
      ELSE
        ALLOCATE(ONED_SCALAR_P(1))
        ALLOCATE(ONED_VECTOR_P(1))
        ALLOCATE(ONED_TENSOR_P(1))
        ALLOCATE(ONED_TORSOR_P(1))
        ALLOCATE(ONED_ID_P(1))
        ALLOCATE(ONED_ITY_P(1))
        ALLOCATE(IS_WRITEN_ONED_P(1))
        IS_WRITEN_ONED_P(1) = 0
      ENDIF
c
c
      ALLOCATE(SHELL_SCALAR(NUMELC+NUMELTG))
      ALLOCATE(SHELL_VECTOR(3*(NUMELC+NUMELTG)))
      ALLOCATE(SHELL_TENSOR(3*(NUMELC+NUMELTG)))
      ALLOCATE(SHELL_ID(NUMELC+NUMELTG))
      ALLOCATE(SHELL_ITY(NUMELC+NUMELTG))
      ALLOCATE(IS_WRITEN_SHELL(NUMELC+NUMELTG))
      SHELL_ID(1:NUMELC+NUMELTG) = 0
      SHELL_ITY(1:NUMELC+NUMELTG) = 0
      IS_WRITEN_SHELL(1:NUMELC+NUMELTG) = 0
c
      IF (ISPMD == 0 )THEN
        ALLOCATE(SHELL_SCALAR_P(NUMELCG+NUMELTGG))
        ALLOCATE(SHELL_VECTOR_P(3*(NUMELCG+NUMELTGG)))
        ALLOCATE(SHELL_TENSOR_P(3*(NUMELCG+NUMELTGG)))
        ALLOCATE(SHELL_ID_P(NUMELCG+NUMELTGG))
        ALLOCATE(SHELL_ITY_P(NUMELCG+NUMELTGG))
        ALLOCATE(IS_WRITEN_SHELL_P(NUMELCG+NUMELTGG))
        IS_WRITEN_SHELL_P(1:NUMELCG+NUMELTGG) = 0
      ELSE
        ALLOCATE(SHELL_SCALAR_P(1))
        ALLOCATE(SHELL_VECTOR_P(1))
        ALLOCATE(SHELL_TENSOR_P(1))
        ALLOCATE(SHELL_ID_P(1))
        ALLOCATE(SHELL_ITY_P(1))
        ALLOCATE(IS_WRITEN_SHELL_P(1))
        IS_WRITEN_SHELL_P(1) = 0
      ENDIF
c
c-------not available now with S16,S20
      MAX_NCORN = 10
      ALLOCATE(SOLID_SCALAR(NUMELS))
      ALLOCATE(SOLID_VECTOR(3*NUMELS))
      ALLOCATE(SOLID_TENSOR(6*NUMELS))
      ALLOCATE(SOLID_TENSOR_CORNER(6*NUMELS*MAX_NCORN))
      ALLOCATE(SOLID_ID(NUMELS))
      ALLOCATE(ISOLNOD(NUMELS))
      ALLOCATE(SOLID_ITY(NUMELS))
      ALLOCATE(IS_WRITEN_SOLID(NUMELS))
      SOLID_ID(1:NUMELS) = 0
      ISOLNOD(1:NUMELS) = 0
      SOLID_ITY(1:NUMELS) = 0
      IS_WRITEN_SOLID(1:NUMELS) = 0
c
      IF (ISPMD == 0 )THEN
        ALLOCATE(SOLID_SCALAR_P(NUMELSG))
        ALLOCATE(SOLID_VECTOR_P(3*NUMELSG))
        ALLOCATE(SOLID_TENSOR_P(6*NUMELSG))
        ALLOCATE(SOLID_TENSOR_CORNER_P(6*NUMELSG*MAX_NCORN))
        ALLOCATE(SOLID_ID_P(NUMELSG))
        ALLOCATE(SOLID_ITY_P(NUMELSG))
        ALLOCATE(ISOLNOD_P(NUMELSG))
        ALLOCATE(IS_WRITEN_SOLID_P(NUMELSG))
        IS_WRITEN_SOLID_P(1:NUMELSG) = 0
      ELSE
        ALLOCATE(SOLID_SCALAR_P(1))
        ALLOCATE(SOLID_VECTOR_P(1))
        ALLOCATE(SOLID_TENSOR_P(1))
        ALLOCATE(SOLID_TENSOR_CORNER_P(1))
        ALLOCATE(SOLID_ID_P(1))
        ALLOCATE(SOLID_ITY_P(1))
        ALLOCATE(ISOLNOD_P(1))
        ALLOCATE(IS_WRITEN_SOLID_P(1))
        IS_WRITEN_SOLID_P(1) = 0
      ENDIF
c
c
      ALLOCATE(SPH_SCALAR(NUMSPH))
      ALLOCATE(SPH_TENSOR(6*NUMSPH))
      ALLOCATE(SPH_ID(NUMSPH))
      ALLOCATE(IS_WRITEN_SPH(NUMSPH))
      SPH_ID(1:NUMSPH) = 0
      IS_WRITEN_SPH(1:NUMSPH) = 0
c
      IF (ISPMD == 0 )THEN
        ALLOCATE(SPH_SCALAR_P(NUMSPHG))
        ALLOCATE(SPH_TENSOR_P(6*NUMSPHG))
        ALLOCATE(SPH_ID_P(NUMSPHG))
        ALLOCATE(IS_WRITEN_SPH_P(NUMSPHG))
        IS_WRITEN_SPH_P(1:NUMSPHG) = 0
      ELSE
        ALLOCATE(SPH_SCALAR_P(1))
        ALLOCATE(SPH_TENSOR_P(1))
        ALLOCATE(SPH_ID_P(1))
        ALLOCATE(IS_WRITEN_SPH_P(1))
        IS_WRITEN_SPH_P(1) = 0
      ENDIF
c
c
      ALLOCATE(QUAD_SCALAR(NUMELQ))
      ALLOCATE(QUAD_VECTOR(3*NUMELQ))
      ALLOCATE(QUAD_TENSOR(6*NUMELQ))
      ALLOCATE(QUAD_ID(NUMELQ))
      ALLOCATE(IS_WRITEN_QUAD(NUMELQ))
      QUAD_ID(1:NUMELQ) = 0
      IS_WRITEN_QUAD(1:NUMELQ) = 0
c
      IF (ISPMD == 0 )THEN
        ALLOCATE(QUAD_SCALAR_P(NUMELQG))
        ALLOCATE(QUAD_VECTOR_P(3*NUMELQG))
        ALLOCATE(QUAD_TENSOR_P(6*NUMELQG))
        ALLOCATE(QUAD_ID_P(NUMELQG))
        ALLOCATE(IS_WRITEN_QUAD_P(NUMELQG))
        IS_WRITEN_QUAD_P(1:NUMELQG) = 0
      ELSE
        ALLOCATE(QUAD_SCALAR_P(1))
        ALLOCATE(QUAD_VECTOR_P(1))
        ALLOCATE(QUAD_TENSOR_P(1))
        ALLOCATE(QUAD_ID_P(1))
        ALLOCATE(IS_WRITEN_QUAD_P(1))
        IS_WRITEN_QUAD_P(1) = 0
      ENDIF
c-------- for skin : only SKIN_ID_P (user id): unique
      ALLOCATE(SKIN_TENSOR(3*NUMSKIN))
      ALLOCATE(SKIN_VECTOR(3*NUMSKIN))
      ALLOCATE(SKIN_SCALAR(NUMSKIN))
      ALLOCATE(IS_WRITEN_SKIN(NUMSKIN))
      IS_WRITEN_SKIN(1:NUMSKIN) = 0
c
      IF (ISPMD == 0 )THEN
        ALLOCATE(SKIN_TENSOR_P(3*NUMSKING))
        ALLOCATE(SKIN_VECTOR_P(3*NUMSKING))
        ALLOCATE(SKIN_SCALAR_P(NUMSKING))
        ALLOCATE(SKIN_ID_P(NUMSKING))
        ALLOCATE(IS_WRITEN_SKIN_P(NUMSKING))
        IS_WRITEN_SKIN_P(1:NUMSKING) = 0
      ELSE
        ALLOCATE(SKIN_TENSOR_P(1))
        ALLOCATE(SKIN_VECTOR_P(1))
        ALLOCATE(SKIN_SCALAR_P(1))
        ALLOCATE(SKIN_ID_P(1))
        ALLOCATE(IS_WRITEN_SKIN_P(1))
        IS_WRITEN_SKIN_P(1) = 0
      ENDIF

      N_OUTP_DATA = 0
C-----------------------------------------------
C UPDATE ELEMENTS OFF
C-----------------------------------------------
c
      CALL H3D_SHELL_OFF(
     .                 ELBUF_TAB ,IPARG ,IXC, IXTG,NUMELC,SHELL_SCALAR, SHELL_ID, SHELL_ITY,
     .                 IPART, IPARTC ,IPARTTG)
c
      IF (NSPMD > 1 ) THEN
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_I(SHELL_ID,NUMELC+NUMELTG,SHELL_ID_P,NUMELCG+NUMELTGG) 
        CALL SPMD_H3D_GATHER_I(SHELL_ITY,NUMELC+NUMELTG,SHELL_ITY_P,NUMELCG+NUMELTGG) 
        CALL SPMD_H3D_GATHER_R(SHELL_SCALAR,NUMELC+NUMELTG,SHELL_SCALAR_P,NUMELCG+NUMELTGG) 
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ELSE
        SHELL_ID_P(1:NUMELC+NUMELTG) =  SHELL_ID(1:NUMELC+NUMELTG)
        SHELL_ITY_P(1:NUMELC+NUMELTG) =  SHELL_ITY(1:NUMELC+NUMELTG)
        SHELL_SCALAR_P(1:NUMELC+NUMELTG) =  SHELL_SCALAR(1:NUMELC+NUMELTG)
      ENDIF
c
      IF(ISPMD == 0) THEN
         CALL STARTIME(MACRO_TIMER_LIBH3D,1)
         CALL C_H3D_ERODED_SHELL(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXC_P,
     .       NIXC,NUMELCG,IPARTC,IXTG,NIXTG,
     .       NUMELTGG,IPARTTG,SHELL_SCALAR_P,SHELL_ID_P,
     .       H3D_DATA%N_OUTP_H3D+3,SHELL_ITY_P,NUMELS,
     .       NUMELQ,NUMELT,NUMELP,NUMELR) 
         CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF 
c
      CALL H3D_SOLID_OFF(
     .                 ELBUF_TAB ,IPARG ,IXS ,SOLID_SCALAR, SOLID_ID, SOLID_ITY, ISOLNOD)
c
      IF (NSPMD > 1 ) THEN
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_I(SOLID_ID,NUMELS,SOLID_ID_P,NUMELSG) 
        CALL SPMD_H3D_GATHER_I(SOLID_ITY,NUMELS,SOLID_ITY_P,NUMELSG) 
        CALL SPMD_H3D_GATHER_R(SOLID_SCALAR,NUMELS,SOLID_SCALAR_P,NUMELSG) 
        CALL SPMD_H3D_GATHER_I(ISOLNOD,NUMELS,ISOLNOD_P,NUMELSG) 
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ELSE
        SOLID_ID_P(1:NUMELS) =  SOLID_ID(1:NUMELS)
        SOLID_ITY_P(1:NUMELS) =  SOLID_ITY(1:NUMELS)
        SOLID_SCALAR_P(1:NUMELS) =  SOLID_SCALAR(1:NUMELS)
        ISOLNOD_P(1:NUMELS) =  ISOLNOD(1:NUMELS)
      ENDIF
c
      IF(ISPMD == 0) THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_ERODED_SOLID(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXS_P,
     .                     NIXS,NUMELSG,IPARTS,SOLID_SCALAR_P,SOLID_ID_P,
     .                     H3D_DATA%N_OUTP_H3D+4,SOLID_ITY_P,
     .                     NUMELQ,NUMELT,NUMELP,NUMELR)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

      ENDIF
c
      CALL H3D_ONED_OFF(
     .                 ELBUF_TAB ,IPARG ,IXT, IXP, IXR ,ONED_SCALAR, ONED_ID, ONED_ITY,
     .                 IPART , IPARTT  ,IPARTP  ,IPARTR)
c
      IF (NSPMD > 1 ) THEN
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_I(ONED_ID,NUMELT+NUMELP+NUMELR,ONED_ID_P,NUMELTRG+NUMELPG+NUMELRG) 
        CALL SPMD_H3D_GATHER_I(ONED_ITY,NUMELT+NUMELP+NUMELR,ONED_ITY_P,NUMELTRG+NUMELPG+NUMELRG) 
        CALL SPMD_H3D_GATHER_R(ONED_SCALAR,NUMELT+NUMELP+NUMELR,ONED_SCALAR_P,NUMELTRG+NUMELPG+NUMELRG) 
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ELSE
        ONED_ID_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ID(1:NUMELT+NUMELP+NUMELR)
        ONED_ITY_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ITY(1:NUMELT+NUMELP+NUMELR)
        ONED_SCALAR_P(1:NUMELT+NUMELP+NUMELR) =  ONED_SCALAR(1:NUMELT+NUMELP+NUMELR)
      ENDIF
c
      IF(ISPMD == 0) THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_ERODED_ONED(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXT_P,
     .                     NIXT,NUMELTRG,IPARTT,IXP_P,NIXP,
     .                     NUMELPG,IPARTP,IXR_P,NIXR,NUMELRG,
     .                     IPARTR,ONED_SCALAR_P,ONED_ID_P,H3D_DATA%N_OUTP_H3D+5,ONED_ITY_P)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

      ENDIF 
c
      CALL H3D_SPH_OFF(
     .                 ELBUF_TAB ,IPARG ,KXSP ,SPH_SCALAR, SPH_ID)
c
      IF (NSPMD > 1 ) THEN
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_I(SPH_ID,NUMSPH,SPH_ID_P,NUMSPHG)  
        CALL SPMD_H3D_GATHER_R(SPH_SCALAR,NUMSPH,SPH_SCALAR_P,NUMSPHG)
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ELSE
        SPH_ID_P(1:NUMSPH) =  SPH_ID(1:NUMSPH)
        SPH_SCALAR_P(1:NUMSPH) =  SPH_SCALAR(1:NUMSPH)
      ENDIF
c
      IF(ISPMD == 0) THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_ERODED_SPH(TT,H3D_DATA%IH3D,NUMSPHG,SPH_SCALAR_P,SPH_ID_P,
     .                     H3D_DATA%N_OUTP_H3D+6)
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

      ENDIF
c
      CALL H3D_QUAD_OFF(
     .                 ELBUF_TAB ,IPARG ,IXQ,QUAD_SCALAR, QUAD_ID,
     .                 IPART     ,IPARTQ)
c
      IF (NSPMD > 1 ) THEN
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_I(QUAD_ID,NUMELQ,QUAD_ID_P,NUMELQ) 
        CALL SPMD_H3D_GATHER_R(QUAD_SCALAR,NUMELQ,QUAD_SCALAR_P,NUMELQ) 
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ELSE
        QUAD_ID_P(1:NUMELQ) =  QUAD_ID(1:NUMELQ)
        QUAD_SCALAR_P(1:NUMELQ) =  QUAD_SCALAR(1:NUMELQ)
      ENDIF
c
      IF(ISPMD == 0) THEN
         CALL STARTIME(MACRO_TIMER_LIBH3D,1)
         CALL C_H3D_ERODED_QUAD(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXQ_P,
     .       NIXQ,NUMELQG,IPARTQ,QUAD_SCALAR_P,QUAD_ID_P,
     .       H3D_DATA%N_OUTP_H3D+7) 
         CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF 
C
      CALL H3D_SKIN_OFF(ELBUF_TAB,IPARG,IXS,IXS10,TAG_SKINS6,SKIN_SCALAR)
      IF (NSPMD > 1 ) THEN
        CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
        CALL SPMD_H3D_GATHER_R(SKIN_SCALAR,NUMSKIN,SKIN_SCALAR_P,NUMSKING) 
        CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
      ELSE
        SKIN_SCALAR_P(1:NUMSKIN) =  SKIN_SCALAR(1:NUMSKIN)
      ENDIF
c
      IF(ISPMD == 0) THEN
        DO II=1,NUMSKING
          SKIN_ID_P(II) = II 
        ENDDO
         CALL STARTIME(MACRO_TIMER_LIBH3D,1)
         CALL C_H3D_ERODED_SKIN(TT,H3D_DATA%IH3D,ITAB,NUMNOD,SKIN_SCALAR_P,SKIN_ID_P,
     .                          H3D_DATA%N_OUTP_H3D+8,NUMSKING) 
         CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF 
C-----------------------------------------------
C  UPDATE RESULTS VALUES
C-----------------------------------------------
c     
      DO I = 1,H3D_DATA%N_OUTP_H3D
c-----   
c nodal scalar 
c-----
        IF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 1 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INTER_INPUT = H3D_DATA%OUTPUT_LIST(I)%INTER
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          INTERSKID = 0
          IF(KEYWORD == 'SKID_LINE')THEN
            DO NI=1,NINTER
               IF(NI == INTER_INPUT ) THEN
                  INTERSKID = H3D_DATA%N_SKID_INTER (NI) 
                  ITYSKID = IPARI(7,NI)
                  EXIT
                ENDIF
             ENDDO
          ENDIF

c          IF(ISPMD == 0) print *,'nodal scalar',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c          IF(ISPMD == 0) print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_NODAL_SCALAR(
     .                     ELBUF_TAB, NODAL_SCALAR ,IFUNC         ,IPARG       ,GEO        ,
     .                     MAS      ,PM            ,ANIN          , ITAB       , NODE_ID   ,
     .                     INFO1    ,INFO2         ,IS_WRITEN_NODE,H3D_DATA%OUTPUT_LIST(I)%PART ,
     .                                                                         IPARTC      ,
     .                     IPARTTG  ,IXC           , IXTG         ,TEMP         ,IFLOW     ,
     .                     RFLOW    ,IXS           ,IXQ           ,NV46         ,MONVOL    ,
     .                     VOLMON   , ALE_CONNECT  ,DIAG_SMS      ,MS           ,PDAMA2    ,
     .                     X        , STIFR        , STIFN        ,KEYWORD      ,H3D_DATA  ,
     .                     NPBY     ,RBY           ,INTERSKID     ,H3D_DATA%N_SCAL_SKID,PSKIDS,
     .                     NODGLOB  ,ITYSKID       )

c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 

            IF(KEYWORD == 'SKID_LINE')THEN
               CALL SPMD_OUTPITAB(NODE_ID,WEIGHT,NODGLOB,NODE_ID_P)
               CALL SPMD_OUTPITAB(IS_WRITEN_NODE,WEIGHT,NODGLOB,IS_WRITEN_NODE_P)
               IF(ITYSKID== 21) THEN
                  CALL SPMD_H3D_MAX_R_NODAL_VALUE_21(NODGLOB,NODAL_SCALAR_P,NUMNODG,PSKIDS,
     .                 INTERSKID,H3D_DATA%N_SCAL_SKID)
               ELSE
                  CALL SPMD_H3D_MAX_R_NODAL_VALUE(NODGLOB,NODAL_SCALAR,NUMNOD,NODAL_SCALAR_P,NUMNODG)
               ENDIF
            ELSE
               CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,NODE_ID,NUMNOD,NODE_ID_P,NUMNODG) 
               CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,IS_WRITEN_NODE,NUMNOD,IS_WRITEN_NODE_P,NUMNODG) 
               CALL SPMD_H3D_GATHER_R_NODAL_VALUE(WEIGHT,NODAL_SCALAR,NUMNOD,NODAL_SCALAR_P,NUMNODG) 
            ENDIF
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            NODE_ID_P(1:NUMNOD) =  NODE_ID(1:NUMNOD)
            IS_WRITEN_NODE_P(1:NUMNOD) =  IS_WRITEN_NODE(1:NUMNOD)
            NODAL_SCALAR_P(1:NUMNOD) =  NODAL_SCALAR(1:NUMNOD)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_NODAL_SCALAR(TT,H3D_DATA%IH3D,ITAB,NUMNODG,NODAL_SCALAR_P,NODE_ID_P,
     .                           N_OUTP_DATA,IS_WRITEN_NODE_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----    
c nodal vector 
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 1 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 2)THEN
c
          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0) print *,'nodal vector',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c          IF(ISPMD == 0) print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_NODAL_VECTOR(
     .                     ELBUF_TAB, NODAL_VECTOR ,IFUNC      ,IPARG,GEO ,MAS       ,PM         ,
     .                      ANIN , ITAB, NODE_ID   ,INFO1        ,INFO2      ,
     .                     IS_WRITEN_NODE,H3D_DATA%OUTPUT_LIST(I)%PART ,IPARTC,IPARTTG,
     .                  IXC, IXTG,TEMP,IFLOW,RFLOW,IXS,IXQ,NV46,MONVOL   ,VOLMON, DIAG_SMS,MS,
     .                   PDAMA2,X, STIFR, STIFN, A, D, V, CONT, FCONTG, FINT, FEXT,KEYWORD,
     .                   FNCONT  , FNCONTG , FTCONT , FTCONTG   , FNCONT2,
     .                   DR      , DXANCG  , FANREAC, FCLUSTER  , MCLUSTER ,
     .                   VR      , FOPT    , NPBY   , IXUR      , VGAZ     ,
     .                   IPARI   , IGRNOD  , WEIGHT , NODGLOB   ,FCONT_MAX ,
     .                   FNCONTP2, FTCONTP2)

c
          IF (NSPMD > 1 ) THEN
            IF(KEYWORD == 'CONT'.OR.KEYWORD == 'PCONT/NORMAL'.OR.KEYWORD == 'PCONT/TANGENT'.OR.KEYWORD == 'FEXT'
     .         .OR.(KEYWORD == 'CONT/MAX'.AND.NINTSTAMP==0)
     .         .OR.(KEYWORD == 'CONT2'.OR.KEYWORD == 'PCONT2/NORMAL'.OR.KEYWORD == 'PCONT2/TANGENT')) THEN

               CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
               CALL SPMD_OUTPITAB(NODE_ID,WEIGHT,NODGLOB,NODE_ID_P)
               CALL SPMD_OUTPITAB(IS_WRITEN_NODE,WEIGHT,NODGLOB,IS_WRITEN_NODE_P)
               IF(NINTSTAMP==0) THEN
                  CALL SPMD_H3D_SUM_R_NODAL(NODGLOB,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG)
               ELSEIF(KEYWORD == 'CONT')THEN
                  CALL SPMD_H3D_SUM_R_NODAL_21(NODGLOB,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG,FCONTG)
               ELSEIF(KEYWORD == 'PCONT/NORMAL')THEN    
                  CALL SPMD_H3D_SUM_R_NODAL_21(NODGLOB,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG,FNCONTG)
               ELSEIF(KEYWORD == 'PCONT/TANGENT')THEN  
                  CALL SPMD_H3D_SUM_R_NODAL_21(NODGLOB,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG,FTCONTG)
               ELSEIF(KEYWORD == 'CONT2')THEN
                  CALL SPMD_H3D_SUM_R_NODAL_21(NODGLOB,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG,FNCONT2)
               ELSEIF(KEYWORD == 'PCONT2/NORMAL')THEN    
                  CALL SPMD_H3D_SUM_R_NODAL_21(NODGLOB,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG,FNCONTP2)
               ELSEIF(KEYWORD == 'PCONT2/TANGENT')THEN  
                  CALL SPMD_H3D_SUM_R_NODAL_21(NODGLOB,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG,FTCONTP2)
               ENDIF                     
               CALL STOPTIME(MACRO_TIMER_SPMDH3D,1) 

            ELSEIF(KEYWORD == 'CONT/MAX'.AND.NINTSTAMP/=0) THEN

               CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
               CALL SPMD_OUTPITAB(NODE_ID,WEIGHT,NODGLOB,NODE_ID_P)
               CALL SPMD_OUTPITAB(IS_WRITEN_NODE,WEIGHT,NODGLOB,IS_WRITEN_NODE_P)
               IF(ISPMD == 0) THEN
                  NODAL_VECTOR_P(1:3*NUMNODG) =FCONT_MAX(1:3*NUMNODG)
               ENDIF 
               CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)

            ELSE
               CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
               CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,NODE_ID,NUMNOD,NODE_ID_P,NUMNODG) 
               CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,IS_WRITEN_NODE,NUMNOD,IS_WRITEN_NODE_P,NUMNODG) 
               CALL SPMD_H3D_GATHER_R_NODE(WEIGHT,NODAL_VECTOR,3*NUMNOD,NODAL_VECTOR_P,3*NUMNODG) 
               CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
            ENDIF

          ELSE

            NODE_ID_P(1:NUMNOD) =  NODE_ID(1:NUMNOD)
            IS_WRITEN_NODE_P(1:NUMNOD) =  IS_WRITEN_NODE(1:NUMNOD)
            NODAL_VECTOR_P(1:3*NUMNOD) =  NODAL_VECTOR(1:3*NUMNOD)

          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_NODAL_VECTOR(TT,H3D_DATA%IH3D,ITAB,NUMNODG,NODAL_VECTOR_P,NODE_ID_P,
     .                           N_OUTP_DATA,IS_WRITEN_NODE_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c-----    
c nodal tensor 
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 1 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3)THEN
c
          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0) print *,'nodal tensor',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c          IF(ISPMD == 0) print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_NODAL_TENSOR(
     .                     ELBUF_TAB, NODAL_TENSOR ,IFUNC      ,IPARG,GEO ,MAS       ,PM         ,
     .                      ANIN , ITAB, NODE_ID   ,INFO1        ,INFO2      ,
     .                     IS_WRITEN_NODE,H3D_DATA%OUTPUT_LIST(I)%PART ,IPARTC,IPARTTG,
     .                  IXC, IXTG,TEMP,IFLOW,RFLOW,IXS,IXQ,NV46,MONVOL   ,VOLMON, DIAG_SMS,MS,
     .                   PDAMA2,X, STIFR, STIFN, A, D, V, CONT, FCONTG, FINT, FEXT,KEYWORD,
     .                   BUFMAT   , IXS10       , IXS16          , IXS20 , IXT      ,
     .                   IXP      , IXR        ,IAD_ELEM        ,FR_ELEM     , WEIGHT)

c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,NODE_ID,NUMNOD,NODE_ID_P,NUMNODG) 
            CALL SPMD_H3D_GATHER_I_NODE(WEIGHT,IS_WRITEN_NODE,NUMNOD,IS_WRITEN_NODE_P,NUMNODG) 
            CALL SPMD_H3D_GATHER_T_NODE(WEIGHT,NODAL_TENSOR,6*NUMNOD,NODAL_TENSOR_P,6*NUMNODG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            NODE_ID_P(1:NUMNOD) =  NODE_ID(1:NUMNOD)
            IS_WRITEN_NODE_P(1:NUMNOD) =  IS_WRITEN_NODE(1:NUMNOD)
            NODAL_TENSOR_P(1:6*NUMNOD) =  NODAL_TENSOR(1:6*NUMNOD)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_NODAL_TENSOR(TT,H3D_DATA%IH3D,ITAB,NUMNODG,NODAL_TENSOR_P,NODE_ID_P,
     .                           N_OUTP_DATA,IS_WRITEN_NODE_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c-----    
c 1d scalar 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 4 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1 .AND. (NUMELTRG+NUMELPG+NUMELRG) > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
          IPT = H3D_DATA%OUTPUT_LIST(I)%IPT
c          IF(ISPMD == 0) print *,'nodal scalar',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c          IF(ISPMD == 0) print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_ONED_SCALAR(ELBUF_TAB   ,IFUNC  ,IPARG    ,GEO  ,
     .                IXT          ,IXP    ,IXR    ,PM   ,
     .                ANIN(NDMA2+1),ONED_SCALAR, 
     .                ONED_ID      ,ONED_ITY,INFO1 ,INFO2    , IS_WRITEN_ONED  ,
     .                IPARTT ,IPARTP,IPARTR,H3D_DATA%OUTPUT_LIST(I)%PART,
     .                KEYWORD, X , D   ,IPT)

c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(ONED_ID,NUMELT+NUMELP+NUMELR,ONED_ID_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(ONED_ITY,NUMELT+NUMELP+NUMELR,ONED_ITY_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_ONED,NUMELT+NUMELP+NUMELR,IS_WRITEN_ONED_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_R(ONED_SCALAR,NUMELT+NUMELP+NUMELR,ONED_SCALAR_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            ONED_ID_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ID(1:NUMELT+NUMELP+NUMELR)
            ONED_ITY_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ITY(1:NUMELT+NUMELP+NUMELR)
            IS_WRITEN_ONED_P(1:NUMELT+NUMELP+NUMELR) =  IS_WRITEN_ONED(1:NUMELT+NUMELP+NUMELR)
            ONED_SCALAR_P(1:NUMELT+NUMELP+NUMELR) =  ONED_SCALAR(1:NUMELT+NUMELP+NUMELR)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_ONED_SCALAR(TT,H3D_DATA%IH3D,ITAB,NUMELTRG,NUMELPG,NUMELRG,ONED_SCALAR_P,ONED_ID_P,
     .                           N_OUTP_DATA,ONED_ITY_P,IS_WRITEN_ONED_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----    
c 1d vector 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 4 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 2 .AND. (NUMELTRG+NUMELPG+NUMELRG) > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0) print *,'1d vector',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c          IF(ISPMD == 0) print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_ONED_VECTOR(ELBUF_TAB   ,IFUNC  ,IPARG    ,GEO  ,
     .                IXT          ,IXP    ,IXR    ,PM   ,
     .                ANIN(NDMA2+1),ONED_VECTOR, 
     .                ONED_ID      ,ONED_ITY,INFO1 ,INFO2    , IS_WRITEN_ONED  ,
     .                IPARTT ,IPARTP,IPARTR,H3D_DATA%OUTPUT_LIST(I)%PART,
     .                KEYWORD      , X     , D     ,TORS   )

c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(ONED_ID,NUMELT+NUMELP+NUMELR,ONED_ID_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(ONED_ITY,NUMELT+NUMELP+NUMELR,ONED_ITY_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_ONED,NUMELT+NUMELP+NUMELR,IS_WRITEN_ONED_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_R(ONED_VECTOR,3*(NUMELT+NUMELP+NUMELR),ONED_VECTOR_P,3*(NUMELTRG+NUMELPG+NUMELRG)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            ONED_ID_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ID(1:NUMELT+NUMELP+NUMELR)
            ONED_ITY_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ITY(1:NUMELT+NUMELP+NUMELR)
            IS_WRITEN_ONED_P(1:NUMELT+NUMELP+NUMELR) =  IS_WRITEN_ONED(1:NUMELT+NUMELP+NUMELR)
            ONED_VECTOR_P(1:3*(NUMELT+NUMELP+NUMELR)) =  ONED_VECTOR(1:3*(NUMELT+NUMELP+NUMELR))
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_ONED_VECTOR(TT,H3D_DATA%IH3D,ITAB,NUMELTRG,NUMELPG,NUMELRG,ONED_VECTOR_P,ONED_ID_P,
     .                           N_OUTP_DATA,ONED_ITY_P,IS_WRITEN_ONED_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----    
c 1d tensor 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 4 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3 .AND. (NUMELTRG+NUMELPG+NUMELRG) > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          IPT   = H3D_DATA%OUTPUT_LIST(I)%IPT
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0) print *,'1d tensor',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c          IF(ISPMD == 0) print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_ONED_TENSOR(ELBUF_TAB   ,IFUNC  ,IPARG    ,GEO  ,
     .                IXT          ,IXP    ,IXR    ,PM   ,
     .                ANIN(NDMA2+1),ONED_TENSOR, 
     .                ONED_ID      ,ONED_ITY,INFO1 ,INFO2    , IS_WRITEN_ONED  ,
     .                IPARTT ,IPARTP,IPARTR,H3D_DATA%OUTPUT_LIST(I)%PART,
     .                KEYWORD      , X     , D     ,IPT)
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(ONED_ID,NUMELT+NUMELP+NUMELR,ONED_ID_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(ONED_ITY,NUMELT+NUMELP+NUMELR,ONED_ITY_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_ONED,NUMELT+NUMELP+NUMELR,IS_WRITEN_ONED_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_R(ONED_TENSOR,6*(NUMELT+NUMELP+NUMELR),ONED_TENSOR_P,6*(NUMELTRG+NUMELPG+NUMELRG)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            ONED_ID_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ID(1:NUMELT+NUMELP+NUMELR)
            ONED_ITY_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ITY(1:NUMELT+NUMELP+NUMELR)
            IS_WRITEN_ONED_P(1:NUMELT+NUMELP+NUMELR) =  IS_WRITEN_ONED(1:NUMELT+NUMELP+NUMELR)
            ONED_TENSOR_P(1:6*(NUMELT+NUMELP+NUMELR)) =  ONED_TENSOR(1:6*(NUMELT+NUMELP+NUMELR))
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_ONED_TENSOR(TT,H3D_DATA%IH3D,ITAB,NUMELTRG,NUMELPG,NUMELRG,ONED_TENSOR_P,ONED_ID_P,
     .                           N_OUTP_DATA,ONED_ITY_P,IS_WRITEN_ONED_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----    
c 1d torsor 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 4 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 4 .AND. (NUMELTRG+NUMELPG+NUMELRG) > 0)THEN
c
          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0) print *,'nodal vector',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c          IF(ISPMD == 0) print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_ONED_TORSOR(IPARG,IFUNC ,IXT  ,IXP  ,
     .                  IXR   ,TORS ,ONED_TORSOR,ONED_ID      ,
     .                  ONED_ITY,INFO1 ,INFO2    , IS_WRITEN_ONED,
     .                  IPARTT ,IPARTP,IPARTR,H3D_DATA%OUTPUT_LIST(I)%PART,KEYWORD)

c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(ONED_ID,NUMELT+NUMELP+NUMELR,ONED_ID_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(ONED_ITY,NUMELT+NUMELP+NUMELR,ONED_ITY_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_ONED,NUMELT+NUMELP+NUMELR,IS_WRITEN_ONED_P,NUMELTRG+NUMELPG+NUMELRG) 
            CALL SPMD_H3D_GATHER_R(ONED_TORSOR,9*(NUMELT+NUMELP+NUMELR),ONED_TORSOR_P,9*(NUMELTRG+NUMELPG+NUMELRG)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            ONED_ID_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ID(1:NUMELT+NUMELP+NUMELR)
            ONED_ITY_P(1:NUMELT+NUMELP+NUMELR) =  ONED_ITY(1:NUMELT+NUMELP+NUMELR)
            IS_WRITEN_ONED_P(1:NUMELT+NUMELP+NUMELR) =  IS_WRITEN_ONED(1:NUMELT+NUMELP+NUMELR)
            ONED_TORSOR_P(1:9*(NUMELT+NUMELP+NUMELR)) =  ONED_TORSOR(1:9*(NUMELT+NUMELP+NUMELR))
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_ONED_TORSOR(TT,H3D_DATA%IH3D,ITAB,NUMELTRG,NUMELPG,NUMELRG,ONED_TORSOR_P,ONED_ID_P,
     .         N_OUTP_DATA,ONED_ITY_P,IS_WRITEN_ONED_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
          ENDIF 
c-----
c shell scalar 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 2 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1 .AND. (NUMELCG+NUMELTGG) > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IPT = H3D_DATA%OUTPUT_LIST(I)%IPT
          ID_PLY = H3D_DATA%OUTPUT_LIST(I)%PLY
          GAUSS = H3D_DATA%OUTPUT_LIST(I)%GAUSS
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          IDMDS = H3D_DATA%OUTPUT_LIST(I)%IDMDS
          IMDSVAR = H3D_DATA%OUTPUT_LIST(I)%IMDSVAR
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c         IF(ISPMD == 0)  print *,'shell scalar',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c         IF(ISPMD == 0)  print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_SHELL_SCALAR(
     .                     ELBUF_TAB ,SHELL_SCALAR ,IFUNC      ,IPARG        ,GEO        ,
     .                     IXQ       ,IXC       ,IXTG          ,PM           ,BUFMAT     ,
     .                     EL2FA     ,NBF       ,IXUR          ,IAD          ,
     .                     NBF_L     ,EANI      ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                     IPM       ,IGEO      ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                     INVERT    ,X         ,V             ,W            ,ALE_CONNECT,
     .                     NV46      ,NERCVOIS  ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                     STACK     ,SHELL_ID  ,SHELL_ITY     ,INFO1        ,INFO2      ,
     .                     IS_WRITEN_SHELL,IPARTC ,IPARTTG     ,LAYER        ,IPT        ,
     .                     ID_PLY     ,GAUSS     ,IUVAR ,H3D_DATA%OUTPUT_LIST(I)%PART,KEYWORD,
     .                     D          ,MULTI_FVM ,IDMDS        ,IMDSVAR      ,MDS_MATID  )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SHELL,NUMELC+NUMELTG,IS_WRITEN_SHELL_P,NUMELCG+NUMELTGG) 
            CALL SPMD_H3D_GATHER_R(SHELL_SCALAR,NUMELC+NUMELTG,SHELL_SCALAR_P,NUMELCG+NUMELTGG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SHELL_P(1:NUMELC+NUMELTG) =  IS_WRITEN_SHELL(1:NUMELC+NUMELTG)
            SHELL_SCALAR_P(1:NUMELC+NUMELTG) =  SHELL_SCALAR(1:NUMELC+NUMELTG)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SHELL_SCALAR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXC_P,
     .                           NIXC,NUMELCG,IPARTC,IXTG,NIXTG,
     .                           NUMELTGG,IPARTTG,SHELL_SCALAR_P,SHELL_ID_P,
     .                           N_OUTP_DATA,SHELL_ITY_P,NUMELS,
     .                           NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITEN_SHELL_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c-----
c shell vector 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 2 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 2 .AND. (NUMELCG+NUMELTGG) > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IPT = H3D_DATA%OUTPUT_LIST(I)%IPT
          ID_PLY = H3D_DATA%OUTPUT_LIST(I)%PLY
          GAUSS = H3D_DATA%OUTPUT_LIST(I)%GAUSS
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c         IF(ISPMD == 0)  print *,'shell vector',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c         IF(ISPMD == 0)  print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_SHELL_VECTOR(
     .                     ELBUF_TAB ,SHELL_VECTOR ,IFUNC      ,IPARG,GEO    ,
     .                     IXQ       ,IXC       ,IXTG          ,PM       ,
     .                     EL2FA     ,NBF       ,IXUR          ,IAD          ,
     .                     NBF_L     ,EANI      ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                     IPM       ,IGEO      ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                     INVERT    ,X         ,V             ,W            ,
     .                     NV46      ,NERCVOIS  ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                     STACK     ,SHELL_ID  ,SHELL_ITY     ,INFO1        ,INFO2      ,
     .                     IS_WRITEN_SHELL,IPARTC ,IPARTTG     ,LAYER        ,IPT        ,
     .                     ID_PLY     ,GAUSS     ,IUVAR ,H3D_DATA%OUTPUT_LIST(I)%PART,KEYWORD,
     .                     D         ,MULTI_FVM)
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SHELL,NUMELC+NUMELTG,IS_WRITEN_SHELL_P,NUMELCG+NUMELTGG) 
            CALL SPMD_H3D_GATHER_R(SHELL_VECTOR,3*(NUMELC+NUMELTG),SHELL_VECTOR_P,3*(NUMELCG+NUMELTGG)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SHELL_P(1:NUMELC+NUMELTG) =  IS_WRITEN_SHELL(1:NUMELC+NUMELTG)
            SHELL_VECTOR_P(1:3*(NUMELC+NUMELTG)) =  SHELL_VECTOR(1:3*(NUMELC+NUMELTG))
          ENDIF
c
          IF(ISPMD == 0) THEN
c            print *,'C_H3D_UPDATE_SHELL_VECTOR'
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SHELL_VECTOR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXC_P,
     .                           NIXC,NUMELCG,IPARTC,IXTG,NIXTG,
     .                           NUMELTGG,IPARTTG,SHELL_VECTOR_P,SHELL_ID_P,
     .                           N_OUTP_DATA,SHELL_ITY_P,NUMELS,
     .                           NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITEN_SHELL_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c-----
c shell tensor   
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 2 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3 .AND. (NUMELCG+NUMELTGG) > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IPT = H3D_DATA%OUTPUT_LIST(I)%IPT
          ID_PLY = H3D_DATA%OUTPUT_LIST(I)%PLY
          GAUSS = H3D_DATA%OUTPUT_LIST(I)%GAUSS
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          OBJECT_ID = H3D_DATA%OUTPUT_LIST(I)%OBJECT_ID
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1 
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
c         IF(ISPMD == 0)   print *,'shell tensor',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c         IF(ISPMD == 0)   print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

           CALL H3D_SHELL_TENSOR(ELBUF_TAB,SHELL_TENSOR ,IPARG ,IFUNC ,INVERT,NELCUT,
     .                  EL2FA    ,NBF   ,WAFT  ,TANI  ,IAD   ,
     .                  NBF_L    ,NBPART,IADG  ,X     ,IXC   ,
     .                  IGEO     ,IXTG  ,IPM   ,STACK ,SHELL_ID   ,SHELL_ITY    ,INFO1,
     .                  INFO2    ,IS_WRITEN_SHELL ,IPARTC ,IPARTTG     ,LAYER        ,IPT        ,
     .                  ID_PLY     ,GAUSS     ,IUVAR ,H3D_DATA%OUTPUT_LIST(I)%PART ,KEYWORD,D    ,
     .                  OBJECT_ID  ,BUFMAT    )

c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SHELL,NUMELC+NUMELTG,IS_WRITEN_SHELL_P,NUMELCG+NUMELTGG) 
            CALL SPMD_H3D_GATHER_R(SHELL_TENSOR,3*(NUMELC+NUMELTG),SHELL_TENSOR_P,3*(NUMELCG+NUMELTGG)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SHELL_P(1:NUMELC+NUMELTG) =  IS_WRITEN_SHELL(1:NUMELC+NUMELTG)
            SHELL_TENSOR_P(1:3*(NUMELC+NUMELTG)) =  SHELL_TENSOR(1:3*(NUMELC+NUMELTG))
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SHELL_TENSOR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXC_P,
     .                           NIXC,NUMELCG,IPARTC,IXTG,NIXTG,
     .                           NUMELTGG,IPARTTG,SHELL_TENSOR_P,SHELL_ID_P,
     .                           N_OUTP_DATA,SHELL_ITY_P,NUMELS,
     .                           NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITEN_SHELL_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 

c-----
c solid scalar   
c-----    
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 3.AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1 .AND. NUMELSG > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IR = H3D_DATA%OUTPUT_LIST(I)%IR
          IS = H3D_DATA%OUTPUT_LIST(I)%IS
          IT = H3D_DATA%OUTPUT_LIST(I)%IT
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          IDMDS = H3D_DATA%OUTPUT_LIST(I)%IDMDS
          IMDSVAR = H3D_DATA%OUTPUT_LIST(I)%IMDSVAR
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0)  print *,'solid scalar',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

          CALL H3D_SOLID_SCALAR(
     .                    ELBUF_TAB ,SOLID_SCALAR ,IFUNC      ,IPARG        ,GEO        ,
     .                    IXQ       ,IXS       ,IXTG          ,PM           ,BUFMAT     ,
     .                    EL2FA     ,NBF       ,IXUR          ,IAD          ,
     .                    NBF_L     ,EANI      ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                    IPM       ,IGEO      ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                    INVERT    ,X         ,V             ,W            ,ALE_CONNECT,
     .                    NV46      ,NERCVOIS  ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                    STACK     ,SOLID_ID  ,SOLID_ITY     ,IPARTS       ,LAYER      ,  
     .                    IR        ,IS        ,IT            ,IUVAR        ,H3D_DATA%OUTPUT_LIST(I)%PART,
     .                    IS_WRITEN_SOLID,INFO1,KEYWORD       ,FANI_CELL    ,SFANI_CELL ,
     .                    MULTI_FVM, H3D_DATA  ,IDMDS         ,IMDSVAR      ,MDS_MATID  )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SOLID,NUMELS,IS_WRITEN_SOLID_P,NUMELSG) 
            CALL SPMD_H3D_GATHER_R(SOLID_SCALAR,NUMELS,SOLID_SCALAR_P,NUMELSG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SOLID_P(1:NUMELS) =  IS_WRITEN_SOLID(1:NUMELS)
            SOLID_SCALAR_P(1:NUMELS) =  SOLID_SCALAR(1:NUMELS)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SOLID_SCALAR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXS_P,
     .                          NIXS,NUMELSG,IPARTS,SOLID_SCALAR_P,SOLID_ID_P,
     .                          N_OUTP_DATA,SOLID_ITY_P,
     .                          NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITEN_SOLID_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----
c solid vector   
c-----    
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 3.AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 2 .AND. NUMELSG > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IR = H3D_DATA%OUTPUT_LIST(I)%IR
          IS = H3D_DATA%OUTPUT_LIST(I)%IS
          IT = H3D_DATA%OUTPUT_LIST(I)%IT
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0)  print *,'solid scalar',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

          CALL H3D_SOLID_VECTOR(
     .                    ELBUF_TAB ,SOLID_VECTOR ,IFUNC      ,IPARG,GEO    ,
     .                    IXQ       ,IXS       ,IXTG          ,PM           ,
     .                    EL2FA     ,NBF       ,IXUR          ,IAD          ,
     .                    NBF_L     ,EANI      ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                    IPM       ,IGEO      ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                    INVERT    ,X         ,V             ,W            ,
     .                    NV46      ,NERCVOIS  ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                    STACK     ,SOLID_ID  ,SOLID_ITY     ,IPARTS       ,LAYER      ,  
     .                    IR        ,IS        ,IT            ,IUVAR        ,H3D_DATA%OUTPUT_LIST(I)%PART,
     .                    IS_WRITEN_SOLID,INFO1,KEYWORD       ,FANI_CELL    ,SFANI_CELL ,
     .                    H3D_DATA, MULTI_FVM)
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SOLID,NUMELS,IS_WRITEN_SOLID_P,NUMELSG) 
            CALL SPMD_H3D_GATHER_R(SOLID_VECTOR,3*NUMELS,SOLID_VECTOR_P,3*NUMELSG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SOLID_P(1:NUMELS) =  IS_WRITEN_SOLID(1:NUMELS)
            SOLID_VECTOR_P(1:3*NUMELS) =  SOLID_VECTOR(1:3*NUMELS)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SOLID_VECTOR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXS_P,
     .                          NIXS,NUMELSG,IPARTS,SOLID_VECTOR_P,SOLID_ID_P,
     .                          N_OUTP_DATA,SOLID_ITY_P,
     .                          NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITEN_SOLID_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----
c solid tensor   
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 3.AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3 .AND. NUMELSG > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          INFO2 = H3D_DATA%OUTPUT_LIST(I)%INFO2
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IR = H3D_DATA%OUTPUT_LIST(I)%IR
          IS = H3D_DATA%OUTPUT_LIST(I)%IS
          IT = H3D_DATA%OUTPUT_LIST(I)%IT
          OBJECT_ID = H3D_DATA%OUTPUT_LIST(I)%OBJECT_ID
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
          IS_CORNER_DATA = H3D_DATA%OUTPUT_LIST(I)%IS_CORNER_DATA
c         IF(ISPMD == 0)  print *,'solid tensor',N_OUTP_DATA,'KEYWORD',LAYER,IR,IS,IT
c
          CALL H3D_SOLID_TENSOR(
     .                    ELBUF_TAB,SOLID_TENSOR, IPARG   ,IFUNC   ,IXS     ,PM      ,
     2                  EL2FA    ,NNN     ,WAFT    ,TANI    ,
     3                  NBPART   ,X       ,IADG    ,IPART   ,
     4                  IPARTSP ,IPARTS  ,ISPH3D  ,IPM     ,IGEO     ,  SOLID_ID   ,SOLID_ITY  , IS_WRITEN_SOLID,
     5                  LAYER   ,   IR   ,IS      ,IT      ,H3D_DATA%OUTPUT_LIST(I)%PART,INFO1  ,KEYWORD ,D    ,
     6                  SOLID_TENSOR_CORNER,IS_CORNER_DATA , IXS10 ,MAX_NCORN,OBJECT_ID)
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SOLID,NUMELS,IS_WRITEN_SOLID_P,NUMELSG) 
            CALL SPMD_H3D_GATHER_R(SOLID_TENSOR,6*NUMELS,SOLID_TENSOR_P,6*NUMELSG) 
            IF (IS_CORNER_DATA == 1) 
     .        CALL SPMD_H3D_GATHER_R(SOLID_TENSOR_CORNER,6*NUMELS*MAX_NCORN,SOLID_TENSOR_CORNER_P,6*NUMELSG*MAX_NCORN) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SOLID_P(1:NUMELS) =  IS_WRITEN_SOLID(1:NUMELS)
            SOLID_TENSOR_P(1:6*NUMELS) =  SOLID_TENSOR(1:6*NUMELS)
            IF (IS_CORNER_DATA == 1) 
     .        SOLID_TENSOR_CORNER_P(1:6*NUMELS*MAX_NCORN) =  SOLID_TENSOR_CORNER(1:6*NUMELS*MAX_NCORN)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SOLID_TENSOR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXS_P,
     .                          NIXS,NUMELSG,IPARTS,SOLID_TENSOR_P,SOLID_ID_P,
     .                          N_OUTP_DATA,SOLID_ITY_P,
     .                          NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITEN_SOLID_P,
     3                          SOLID_TENSOR_CORNER_P,IS_CORNER_DATA,ISOLNOD_P,MAX_NCORN)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----
c sph scalar   
c-----    
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 5.AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1 .AND. NUMSPHG > 0)THEN

          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0)  print *,'SPH scalar',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

          CALL H3D_SPH_SCALAR(
     .                    ELBUF_TAB ,SPH_SCALAR,IFUNC         ,IPARG        ,GEO        ,
     .                    KXSP      ,PM        ,IPART         ,
     .                    EL2FA     ,NBF       ,IXUR          ,IAD          ,
     .                    NBF_L     ,EANI      ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                    IPM       ,IGEO      ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                    INVERT    ,X         ,V             ,W            ,
     .                    NV46      ,NERCVOIS  ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                    STACK     ,SPH_ID  ,IPARTSP      ,IUVAR      ,  
     .                    H3D_DATA%OUTPUT_LIST(I)%PART,IS_WRITEN_SPH,INFO1,KEYWORD,SPBUF  )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(SPH_ID,NUMSPH,SPH_ID_P,NUMSPHG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SPH,NUMSPH,IS_WRITEN_SPH_P,NUMSPHG) 
            CALL SPMD_H3D_GATHER_R(SPH_SCALAR,NUMSPH,SPH_SCALAR_P,NUMSPHG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            SPH_ID_P(1:NUMSPH) =  SPH_ID(1:NUMSPH)
            IS_WRITEN_SPH_P(1:NUMSPH) =  IS_WRITEN_SPH(1:NUMSPH)
            SPH_SCALAR_P(1:NUMSPH) =  SPH_SCALAR(1:NUMSPH)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SPH_SCALAR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,KXSP_P,
     .                          NISP,NUMSPHG,IPARTS,SPH_SCALAR_P,SPH_ID_P,
     .                          N_OUTP_DATA,
     .                          NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITEN_SPH_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----
c sph tensor   
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 5.AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3 .AND. NUMSPHG > 0)THEN

          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
c          IF(ISPMD == 0)  print *,'SPH tensor',N_OUTP_DATA,'KEYWORD',KEYWORD
c
          CALL H3D_SPH_TENSOR(
     .                    ELBUF_TAB,SPH_TENSOR, IPARG   ,IFUNC   ,KXSP    ,PM      ,
     2                  EL2FA    ,NNN     ,WAFT    ,TANI    ,
     3                  NBPART   ,X       ,IADG    ,IPART   ,
     4                  IPARTSP ,ISPH3D  ,IPM     ,IGEO     ,  SPH_ID   , IS_WRITEN_SPH,
     5                  H3D_DATA%OUTPUT_LIST(I)%PART,KEYWORD  )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(SPH_ID,NUMSPH,SPH_ID_P,NUMSPHG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SPH,NUMSPH,IS_WRITEN_SPH_P,NUMSPHG) 
            CALL SPMD_H3D_GATHER_R(SPH_TENSOR,6*NUMSPH,SPH_TENSOR_P,6*NUMSPHG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            SPH_ID_P(1:NUMSPH) =  SPH_ID(1:NUMSPH)
            IS_WRITEN_SPH_P(1:NUMSPH) =  IS_WRITEN_SPH(1:NUMSPH)
            SPH_TENSOR_P(1:6*NUMSPH) =  SPH_TENSOR(1:6*NUMSPH)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SPH_TENSOR(TT,H3D_DATA%IH3D,ITAB,NUMSPHG,IPARTS,SPH_TENSOR_P,SPH_ID_P,
     .                          N_OUTP_DATA,IS_WRITEN_SPH_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----
c quad scalar 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 6 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1 .AND. NUMELQG > 0)THEN

          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IR = H3D_DATA%OUTPUT_LIST(I)%IR
          IS = H3D_DATA%OUTPUT_LIST(I)%IS
          IT = H3D_DATA%OUTPUT_LIST(I)%IT
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD

c         IF(ISPMD == 0)  print *,'QUAD scalar',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c         IF(ISPMD == 0)  print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_QUAD_SCALAR(
     .                     ELBUF_TAB ,QUAD_SCALAR ,IFUNC      ,IPARG,GEO    ,
     .                     IXQ       ,IXC       ,IXTG          ,PM       ,
     .                     EL2FA     ,NBF       ,IXUR          ,IAD          ,
     .                     NBF_L     ,EANI      ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                     IPM       ,IGEO      ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                     INVERT    ,X         ,V             ,W            ,ALE_CONNECT,
     .                     NV46      ,NERCVOIS  ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                     STACK     ,QUAD_ID  ,INFO1        ,INFO2      ,
     .                     IS_WRITEN_QUAD,IPARTQ ,IPARTTG     ,LAYER        ,IPT        ,
     .                     ID_PLY     ,GAUSS     ,IUVAR ,H3D_DATA%OUTPUT_LIST(I)%PART,KEYWORD,
     .                     BUFMAT     ,MULTI_FVM ,IR           ,IS           ,IT         )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(QUAD_ID,NUMELQ,QUAD_ID_P,NUMELQG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_QUAD,NUMELQ,IS_WRITEN_QUAD_P,NUMELQG) 
            CALL SPMD_H3D_GATHER_R(QUAD_SCALAR,NUMELQ,QUAD_SCALAR_P,NUMELQG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            QUAD_ID_P(1:NUMELQ) =  QUAD_ID(1:NUMELQ)
            IS_WRITEN_QUAD_P(1:NUMELQ) =  IS_WRITEN_QUAD(1:NUMELQ)
            QUAD_SCALAR_P(1:NUMELQ) =  QUAD_SCALAR(1:NUMELQ)
          ENDIF
c
          IF(ISPMD == 0) THEN
c            print *,'C_H3D_UPDATE_QUAD_SCALAR'
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_QUAD_SCALAR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXC_P,
     .                           NIXC,NUMELC,IPARTC,IXTG,NIXTG,
     .                           NUMELTGG,IPARTTG,QUAD_SCALAR_P,QUAD_ID_P,
     .                           N_OUTP_DATA,NUMELS,
     .                           NUMELQG,NUMELT,NUMELP,NUMELR,IS_WRITEN_QUAD_P) 
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c-----
c quad vector 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 6 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 2 .AND. NUMELQG > 0)THEN

          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IR = H3D_DATA%OUTPUT_LIST(I)%IR
          IS = H3D_DATA%OUTPUT_LIST(I)%IS
          IT = H3D_DATA%OUTPUT_LIST(I)%IT
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD

c         IF(ISPMD == 0)  print *,'QUAD vector',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c         IF(ISPMD == 0)  print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

          CALL H3D_QUAD_VECTOR(
     .                     ELBUF_TAB ,QUAD_VECTOR ,IFUNC      ,IPARG,GEO    ,
     .                     IXQ       ,IXC       ,IXTG          ,PM       ,
     .                     EL2FA     ,NBF       ,IXUR          ,IAD          ,
     .                     NBF_L     ,EANI      ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                     IPM       ,IGEO      ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                     INVERT    ,X         ,V             ,W            ,
     .                     NV46      ,NERCVOIS  ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                     STACK     ,QUAD_ID  ,INFO1        ,INFO2      ,
     .                     IS_WRITEN_QUAD,IPARTQ ,IPARTTG     ,LAYER        ,IPT        ,
     .                     ID_PLY     ,GAUSS     ,IUVAR ,H3D_DATA%OUTPUT_LIST(I)%PART,KEYWORD,
     .                     BUFMAT    ,MULTI_FVM  ,IR          ,IS           ,IT           )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(QUAD_ID,NUMELQ,QUAD_ID_P,NUMELQG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_QUAD,NUMELQ,IS_WRITEN_QUAD_P,NUMELQG) 
            CALL SPMD_H3D_GATHER_R(QUAD_VECTOR,3*NUMELQ,QUAD_VECTOR_P,3*NUMELQG) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            QUAD_ID_P(1:NUMELQ) =  QUAD_ID(1:NUMELQ)
            IS_WRITEN_QUAD_P(1:NUMELQ) =  IS_WRITEN_QUAD(1:NUMELQ)
            QUAD_VECTOR_P(1:3*NUMELQ) =  QUAD_VECTOR(1:3*NUMELQ)
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
c            print *,'C_H3D_UPDATE_QUAD_VECTOR'
            CALL C_H3D_UPDATE_QUAD_VECTOR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXC_P,
     .                           NIXC,NUMELC,IPARTC,IXTG,NIXTG,
     .                           NUMELTGG,IPARTTG,QUAD_VECTOR_P,QUAD_ID_P,
     .                           N_OUTP_DATA,NUMELS,
     .                           NUMELQG,NUMELT,NUMELP,NUMELR,IS_WRITEN_QUAD_P) 
             CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c-----
c quad tensor   
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 6 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3 .AND. NUMELQG > 0)THEN

          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IR = H3D_DATA%OUTPUT_LIST(I)%IR
          IS = H3D_DATA%OUTPUT_LIST(I)%IS
          IT = H3D_DATA%OUTPUT_LIST(I)%IT
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD

c         IF(ISPMD == 0)   print *,'QUAD tensor',N_OUTP_DATA,'IFUNC',IFUNC,'INFO1',INFO1,'INFO2',INFO2

c         IF(ISPMD == 0)   print *,'H3D_DATA%OUTPUT_LIST(I)%PART',H3D_DATA%OUTPUT_LIST(I)%PART(1:NPART)

           CALL H3D_QUAD_TENSOR(ELBUF_TAB,QUAD_TENSOR ,IPARG ,IFUNC ,INVERT,NELCUT,
     .                  EL2FA    ,WAFT  ,TANI  ,IAD   ,
     .                  NBPART,IADG  ,X     ,IXQ   ,
     .                  IGEO     ,IXTG  ,IPM   ,STACK ,QUAD_ID    ,INFO1,
     .                  INFO2    ,IS_WRITEN_QUAD ,IPARTQ ,IPARTTG     ,LAYER        ,IPT        ,
     .                  ID_PLY     ,GAUSS     ,IUVAR ,H3D_DATA%OUTPUT_LIST(I)%PART ,KEYWORD ,
     .                  IR        ,IS        ,IT      )

c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(QUAD_ID,NUMELQ,QUAD_ID_P,NUMELQG) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_QUAD,NUMELQ,IS_WRITEN_QUAD_P,NUMELQG) 
            CALL SPMD_H3D_GATHER_R(QUAD_TENSOR,6*(NUMELQ),QUAD_TENSOR_P,6*(NUMELQG)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            QUAD_ID_P(1:NUMELQ) =  QUAD_ID(1:NUMELQ)
            IS_WRITEN_QUAD_P(1:NUMELQ) =  IS_WRITEN_QUAD(1:NUMELQ)
            QUAD_TENSOR_P(1:6*(NUMELQ)) =  QUAD_TENSOR(1:6*(NUMELQ))
          ENDIF
c
          IF(ISPMD == 0) THEN
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_QUAD_TENSOR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,IXC_P,
     .                           NIXC,NUMELCG,IPARTC,IXTG,NIXTG,
     .                           NUMELTGG,IPARTTG,QUAD_TENSOR_P,QUAD_ID_P,
     .                           N_OUTP_DATA,NUMELS,
     .                           NUMELQG,NUMELT,NUMELP,NUMELR,IS_WRITEN_QUAD_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c
c-----
c skin scalar   
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 7 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1 .AND. NUMSKING > 0)THEN

          IFUNC = H3D_DATA%OUTPUT_LIST(I)%ID
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD

          CALL H3D_SKIN_SCALAR(
     .                    ELBUF_TAB ,SKIN_SCALAR ,IFUNC      ,IPARG        ,GEO        ,
     .                    IXS       ,IXS10 ,IXS16 , IXS20    ,PM          ,
     .                    IPM       ,IGEO      ,X             ,V            ,W          ,
     .                    IPARTS    ,H3D_DATA%OUTPUT_LIST(I)%PART,
     .                    IS_WRITEN_SKIN,INFO1,KEYWORD       , H3D_DATA  ,
     6                    IAD_ELEM        ,FR_ELEM     , WEIGHT    ,TAG_SKINS6,
     7                    NPF  ,TF   ,BUFMAT,IBCL    ,ILOADP     ,LLOADP    ,FAC    , 
     8                    SENSOR,LSENSOR ,TAGNCONT    ,LOADP_HYD_INTER,XFRAME,FORC  ,
     9                    NODAL_IPART )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SKIN,NUMSKIN,IS_WRITEN_SKIN_P,NUMSKING) 
            CALL SPMD_H3D_GATHER_R(SKIN_SCALAR,NUMSKIN,SKIN_SCALAR_P,NUMSKING) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SKIN_P(1:NUMSKIN) =  IS_WRITEN_SKIN(1:NUMSKIN)
            SKIN_SCALAR_P(1:NUMSKIN) =  SKIN_SCALAR(1:NUMSKIN)
          ENDIF
c
          IF(ISPMD == 0) THEN
            DO II=1,NUMSKING
              SKIN_ID_P(II) = II 
            ENDDO
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SKIN_SCALAR(TT,H3D_DATA%IH3D,ITAB,NUMNOD,
     .                          SKIN_SCALAR_P,SKIN_ID_P,N_OUTP_DATA,
     .                          NUMSKING,IS_WRITEN_SKIN_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----
c skin vector 
c-----
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 7 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 2 .AND. NUMSKING > 0)THEN

          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          INFO1 = H3D_DATA%OUTPUT_LIST(I)%INFO1
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD

          CALL H3D_SKIN_VECTOR(SKIN_VECTOR,NODAL_IPART,
     .                   IS_WRITEN_SKIN  ,H3D_DATA%OUTPUT_LIST(I)%PART,INFO1   ,KEYWORD ,
     .                   IBCL,ILOADP,LLOADP,FAC ,NPF,TF ,SENSOR,LSENSOR ,
     .                   TAGNCONT,LOADP_HYD_INTER,FORC,XFRAME,X ,V  )
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SKIN,NUMSKIN,IS_WRITEN_SKIN_P,NUMSKING) 
            CALL SPMD_H3D_GATHER_R(SKIN_VECTOR,NUMSKIN,SKIN_VECTOR_P,3*(NUMSKING)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SKIN_P(1:NUMSKIN) =  IS_WRITEN_SKIN(1:NUMSKIN)
            SKIN_VECTOR_P(1:3*(NUMSKIN)) =  SKIN_VECTOR(1:3*(NUMSKIN))
          ENDIF
c
          IF(ISPMD == 0) THEN
            DO II=1,NUMSKING
              SKIN_ID_P(II) = II 
            ENDDO
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SKIN_VECTOR(TT,H3D_DATA%IH3D,
     .                           NUMSKING,SKIN_VECTOR_P,SKIN_ID_P,
     .                           N_OUTP_DATA,IS_WRITEN_SKIN_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF
c-----
c skin tensor   
c-----   
        ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OK .NE. 0 .AND. H3D_DATA%OUTPUT_LIST(I)%ETYPE == 7 .AND.
     .     H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3 .AND. NUMSKING > 0)THEN

          N_OUTP_DATA = H3D_DATA%OUTPUT_LIST(I)%N_OUTP
          LAYER = H3D_DATA%OUTPUT_LIST(I)%LAYER
          IUVAR = H3D_DATA%OUTPUT_LIST(I)%IUVAR
          KEYWORD = H3D_DATA%OUTPUT_LIST(I)%KEYWORD
           CALL H3D_SKIN_TENSOR(
     .                   ELBUF_TAB,SKIN_TENSOR, IPARG   ,IXS     ,X     ,PM  ,
     4                   IPARTS  ,IPM     ,IGEO    ,IXS10   ,IXS16   ,IXS20   ,
     5                   IS_WRITEN_SKIN  ,H3D_DATA%OUTPUT_LIST(I)%PART,INFO1   ,
     6                   KEYWORD ,IAD_ELEM        ,FR_ELEM     , WEIGHT   ,TAG_SKINS6)
c
          IF (NSPMD > 1 ) THEN
            CALL STARTIME(MACRO_TIMER_SPMDH3D,1) 
            CALL SPMD_H3D_GATHER_I(IS_WRITEN_SKIN,NUMSKIN,IS_WRITEN_SKIN_P,NUMSKING) 
            CALL SPMD_H3D_GATHER_R(SKIN_TENSOR,3*(NUMSKIN),SKIN_TENSOR_P,3*(NUMSKING)) 
            CALL STOPTIME(MACRO_TIMER_SPMDH3D,1)
          ELSE
            IS_WRITEN_SKIN_P(1:NUMSKIN) =  IS_WRITEN_SKIN(1:NUMSKIN)
            SKIN_TENSOR_P(1:3*(NUMSKIN)) = SKIN_TENSOR(1:3*(NUMSKIN))
          ENDIF
c
          IF(ISPMD == 0) THEN
C-----usr_id not important but should be unique ---------        
            DO II=1,NUMSKING
              SKIN_ID_P(II) = II 
            ENDDO
            CALL STARTIME(MACRO_TIMER_LIBH3D,1)
            CALL C_H3D_UPDATE_SKIN_TENSOR(TT,H3D_DATA%IH3D,
     .                           NUMSKING,SKIN_TENSOR_P,SKIN_ID_P,
     .                           N_OUTP_DATA,IS_WRITEN_SKIN_P)
            CALL STOPTIME(MACRO_TIMER_LIBH3D,1)

          ENDIF 
c
        ENDIF
      ENDDO

      DEALLOCATE(NODAL_SCALAR)
      DEALLOCATE(NODAL_VECTOR)
      DEALLOCATE(NODE_ID)
      DEALLOCATE(IS_WRITEN_NODE)
      DEALLOCATE(IS_WRITEN_NODE_P)
      DEALLOCATE(NODAL_VECTOR_P)
      DEALLOCATE(NODAL_SCALAR_P)
      DEALLOCATE(NODE_ID_P)
c
      DEALLOCATE(ONED_SCALAR)
      DEALLOCATE(ONED_VECTOR)
      DEALLOCATE(ONED_TENSOR)
      DEALLOCATE(ONED_TORSOR)
      DEALLOCATE(ONED_ID)
      DEALLOCATE(ONED_ITY)
      DEALLOCATE(IS_WRITEN_ONED)
      DEALLOCATE(IS_WRITEN_ONED_P)
      DEALLOCATE(ONED_ITY_P)
      DEALLOCATE(ONED_ID_P)
      DEALLOCATE(ONED_SCALAR_P)
      DEALLOCATE(ONED_VECTOR_P)
      DEALLOCATE(ONED_TENSOR_P)
      DEALLOCATE(ONED_TORSOR_P)
c
      DEALLOCATE(SHELL_SCALAR)
      DEALLOCATE(SHELL_VECTOR)
      DEALLOCATE(SHELL_TENSOR)
      DEALLOCATE(SHELL_ID)
      DEALLOCATE(SHELL_ITY)
      DEALLOCATE(IS_WRITEN_SHELL)
      DEALLOCATE(SHELL_SCALAR_P)
      DEALLOCATE(IS_WRITEN_SHELL_P)
      DEALLOCATE(SHELL_TENSOR_P)
      DEALLOCATE(SHELL_ID_P)
      DEALLOCATE(SHELL_ITY_P)
c
      DEALLOCATE(SOLID_SCALAR)
      DEALLOCATE(SOLID_VECTOR)
      DEALLOCATE(SOLID_TENSOR)
      DEALLOCATE(SOLID_TENSOR_CORNER)
      DEALLOCATE(SOLID_ID)
      DEALLOCATE(ISOLNOD)
      DEALLOCATE(SOLID_ITY)
      DEALLOCATE(IS_WRITEN_SOLID)
      DEALLOCATE(SOLID_SCALAR_P)
      DEALLOCATE(SOLID_VECTOR_P)
      DEALLOCATE(SOLID_TENSOR_P)
      DEALLOCATE(SOLID_TENSOR_CORNER_P)
      DEALLOCATE(IS_WRITEN_SOLID_P)
      DEALLOCATE(SOLID_ID_P)
      DEALLOCATE(ISOLNOD_P)
      DEALLOCATE(SOLID_ITY_P)
c
      DEALLOCATE(SPH_SCALAR)
      DEALLOCATE(SPH_TENSOR)
      DEALLOCATE(SPH_ID)
      DEALLOCATE(IS_WRITEN_SPH)
      DEALLOCATE(SPH_SCALAR_P)
      DEALLOCATE(SPH_TENSOR_P)
      DEALLOCATE(IS_WRITEN_SPH_P)
      DEALLOCATE(SPH_ID_P)
c
      DEALLOCATE(QUAD_SCALAR)
      DEALLOCATE(QUAD_VECTOR)
      DEALLOCATE(QUAD_TENSOR)
      DEALLOCATE(QUAD_ID)
      DEALLOCATE(IS_WRITEN_QUAD)
      DEALLOCATE(QUAD_SCALAR_P)
      DEALLOCATE(QUAD_TENSOR_P)
      DEALLOCATE(IS_WRITEN_QUAD_P)
      DEALLOCATE(QUAD_ID_P)
c
      DEALLOCATE(SKIN_TENSOR,SKIN_VECTOR,SKIN_SCALAR)
      DEALLOCATE(IS_WRITEN_SKIN)
      DEALLOCATE(SKIN_TENSOR_P,SKIN_VECTOR_P,SKIN_SCALAR_P)
      DEALLOCATE(IS_WRITEN_SKIN_P)
      DEALLOCATE(SKIN_ID_P)
      DEALLOCATE(IXSKIN_TMP)
      DEALLOCATE(IXSKIN_P)
c
      DEALLOCATE(ITAB_P)


      DEALLOCATE(  IXTG_P)
      DEALLOCATE(  IXS_P)
      DEALLOCATE(  IXP_P)
      DEALLOCATE(  IXR_P)
      DEALLOCATE(  KXSP_P)
      DEALLOCATE(  IXT_P)
      DEALLOCATE(  IXC_P)
      DEALLOCATE(  IXS10_P)
      DEALLOCATE(  IXS16_P)
      DEALLOCATE(  IXS20_P)
      DEALLOCATE(  IPARTC_P)
      DEALLOCATE(  IPARTTG_P)
      DEALLOCATE(  IPARTQ_P) 
      DEALLOCATE(  IPARTS_P)
      DEALLOCATE(  IPARTR_P)
      DEALLOCATE(  IPARTP_P)
      DEALLOCATE(  IPARTT_P)
      DEALLOCATE(  IPARTSP_P)
      DEALLOCATE(  IPARTS10_P)
      DEALLOCATE(  IPARTS16_P)
      DEALLOCATE(  IPARTS20_P)
      DEALLOCATE(  IXQ_P)
      DEALLOCATE(  IXC_TMP)
      DEALLOCATE(  IXTG_TMP)
c     DEALLOCATE(  IXS_TMP)
      DEALLOCATE(  IXR_TMP)
      DEALLOCATE(  IXP_TMP)
      DEALLOCATE(  KXSP_TMP)
      DEALLOCATE(  IXT_TMP)
      DEALLOCATE(  IXS10_TMP)
      DEALLOCATE(  IXS16_TMP)
      DEALLOCATE(  IXS20_TMP)
      DEALLOCATE(  IXQ_TMP)
      DEALLOCATE( SUB_ID,SUB_LEVEL,SUB_NCHILD)
      IF(ALLOCATED(SUB_CHILD)) DEALLOCATE( SUB_CHILD,SUB_IAD,SUB_TITLE)
      
      
      IF (ALLOCATED(NODAL_IPART))  DEALLOCATE(NODAL_IPART)

c
      IF (ALLOCATED(TAGNOD))  DEALLOCATE(TAGNOD)
c
      DEALLOCATE(TAGNOD_P)
c
      DEALLOCATE(MAS)
C
      IF(ISPMD == 0)  THEN
        CALL STARTIME(MACRO_TIMER_LIBH3D,1)
        CALL C_H3D_WRITE_TOC()
        CALL STOPTIME(MACRO_TIMER_LIBH3D,1)
      ENDIF
C
      IF (ISPMD.EQ.0) THEN
        INQUIRE(FILE=TMP_NAME(1:LEN_TMP_NAME), SIZE=H3DTOTALSIZE8)
        H3DTOTALSIZE8 = H3DTOTALSIZE8/(1024*1024)
        H3DTOTALSIZE = H3DTOTALSIZE8 
        WRITE (IOUT,1000)  FILNAM(1:FILEN),H3D_DATA%IH3D,NCYCLE,TT
        WRITE (ISTDO,1000) FILNAM(1:FILEN),H3D_DATA%IH3D,NCYCLE,TT
      ENDIF
c-----------       
       CALL STOPTIME(MACRO_TIMER_GENH3D,1)

       RETURN
 1000  FORMAT (4X,' H3D FILE:',1X,A,' UPDATED:  FRAME=',1X,I5,' , NC=',1X,I7,' , TIME=',1X,G11.4)
      END SUBROUTINE GENH3D
