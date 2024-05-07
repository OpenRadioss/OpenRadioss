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
Chd|====================================================================
Chd|  GENANI_MOD                    source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        EIG                           stub/eig.F                    
Chd|        EIG1                          stub/eig1.F                   
Chd|        EIGP                          stub/eigp.F                   
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|====================================================================
      MODULE GENANI_MOD
      CONTAINS
c 
Chd|====================================================================
Chd|  GENANI                        source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        ALEVEC                        source/output/anim/generate/genani.F
Chd|        ALEVFLU                       source/output/anim/generate/genani.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANIMBALE                      source/output/anim/generate/genani.F
Chd|        ANIMCALE                      source/output/anim/generate/genani.F
Chd|        ANIMIG3D                      source/output/anim/generate/animig3d.F
Chd|        ANIMX                         source/output/anim/generate/animx.F
Chd|        ANIOFF0                       source/output/anim/generate/anioff0.F
Chd|        ANIOFFC                       source/output/anim/generate/anioffc.F
Chd|        ANIOFFC_CRK                   source/output/anim/generate/anioffc_crk.F
Chd|        ANIOFFC_PLY                   source/output/anim/generate/anioffc_ply.F
Chd|        ANIOFFF                       source/output/anim/generate/aniofff.F
Chd|        ANIOFFS                       source/output/anim/generate/anioff6.F
Chd|        ANISKEW                       source/output/anim/generate/aniskew.F
Chd|        ANISKEWF                      source/output/anim/generate/aniskewf.F
Chd|        ANIVFLOWP                     source/output/anim/generate/genani.F
Chd|        ANI_TXT                       source/output/anim/generate/ani_txt.F
Chd|        ANI_TXT50                     source/output/anim/generate/ani_txt.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CLOSE_C                       ../common_source/tools/input_output/write_routtines.c
Chd|        CNTSKEW                       source/output/anim/generate/aniskewf.F
Chd|        CUR_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        CUTCNT                        source/tools/sect/cutcnt.F    
Chd|        CUTFUNC                       source/tools/sect/cutfunc.F   
Chd|        CUTFUNCE                      source/tools/sect/cutfunce.F  
Chd|        CUTMAIN                       source/tools/sect/cutmain.F   
Chd|        CUTMASS                       source/tools/sect/cutmass.F   
Chd|        DELNUMB0                      source/output/anim/generate/delnumb0.F
Chd|        DELNUMBC                      source/output/anim/generate/delnumbc.F
Chd|        DELNUMBC_CRK                  source/output/anim/generate/delnumbc_crk.F
Chd|        DELNUMBC_PLY                  source/output/anim/generate/delnumbc_ply.F
Chd|        DELNUMBF                      source/output/anim/generate/delnumbf.F
Chd|        DELNUMBS                      source/output/anim/generate/delnumb6.F
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCC_CRK                    source/output/anim/generate/dfuncc_crk.F
Chd|        DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|        DFUNCF                        source/output/anim/generate/dfuncf.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        DFUNGPS1                      source/output/anim/generate/dfuncf.F
Chd|        DFUNGPS2                      source/output/anim/generate/dfuncf.F
Chd|        DMASANI0                      source/output/anim/generate/dmasani0.F
Chd|        DMASANIC                      source/output/anim/generate/dmasanic.F
Chd|        DMASANIF                      source/output/anim/generate/dmasanif.F
Chd|        DMASANIS                      source/output/anim/generate/dmasani6.F
Chd|        DONERBE2                      source/output/anim/generate/donerbe2.F
Chd|        DONERBE3                      source/output/anim/generate/donerbe3.F
Chd|        DONERBY                       source/output/anim/generate/donerby.F
Chd|        DONERWL                       source/output/anim/generate/donerwl.F
Chd|        DONESEC                       source/output/anim/generate/donesec.F
Chd|        DONESRG                       source/output/anim/generate/donesrg.F
Chd|        DPARRBE2                      source/output/anim/generate/dparrbe2.F
Chd|        DPARRBE3                      source/output/anim/generate/dparrbe3.F
Chd|        DPARRBY                       source/output/anim/generate/dparrby.F
Chd|        DPARRWS                       source/output/anim/generate/dparrws.F
Chd|        DPARSRG                       source/output/anim/generate/dparsrg.F
Chd|        DRBE2CNT                      source/output/anim/generate/drbe2cnt.F
Chd|        DRBE3CNT                      source/output/anim/generate/drbe3cnt.F
Chd|        DRBYCNT                       source/output/anim/generate/drbycnt.F
Chd|        DSECCNT                       source/output/anim/generate/dseccnt.F
Chd|        DSECNOR                       source/output/anim/generate/dsecnor.F
Chd|        DSPHCNT                       source/output/anim/generate/dsphcnt.F
Chd|        DSPHNOR                       source/output/anim/generate/dsphnor.F
Chd|        DSRGCNT                       source/output/anim/generate/dsrgcnt.F
Chd|        DSRGNOR                       source/output/anim/generate/dsrgnor.F
Chd|        DXYZSECT                      source/output/anim/generate/dxyzsect.F
Chd|        DXYZSPH                       source/output/anim/generate/dxyzsph.F
Chd|        DXYZSRG                       source/output/anim/generate/dxyzsrg.F
Chd|        FILE_SIZE                     ../common_source/tools/input_output/write_routtines.c
Chd|        FRETITL2                      source/input/freform.F        
Chd|        NODALD                        source/output/anim/generate/nodald.F
Chd|        NODALP                        source/output/anim/generate/nodalp.F
Chd|        NODALSSP                      source/output/anim/generate/nodalssp.F
Chd|        NODALT                        source/output/anim/generate/nodalt.F
Chd|        NODALVFRAC                    source/output/anim/generate/nodalvfrac.F
Chd|        NODALZVOL                     source/output/anim/generate/nodalzvol.F
Chd|        NODAL_SCHLIEREN               source/output/anim/generate/nodal_schlieren.F
Chd|        NORCUT                        source/tools/sect/norcut.F    
Chd|        OPEN_C                        ../common_source/tools/input_output/write_routtines.c
Chd|        PARCUT                        source/tools/sect/parcut.F    
Chd|        PARSOR0                       source/output/anim/generate/parsor0.F
Chd|        PARSORC                       source/output/anim/generate/parsorc.F
Chd|        PARSORF                       source/output/anim/generate/parsorf.F
Chd|        PARSORS                       source/output/anim/generate/parsors.F
Chd|        PARSOR_CRK                    source/output/anim/generate/parsor_crk.F
Chd|        PARSOR_PLY                    source/output/anim/generate/parsor_ply.F
Chd|        SCANOR                        source/output/anim/generate/scanor.F
Chd|        SPMD_CRK_IDMAX                source/mpi/anim/spmd_crk_idmax.F
Chd|        SPMD_DPARRBE2                 source/mpi/anim/spmd_dparrbe2.F
Chd|        SPMD_DPARRBE3                 source/mpi/anim/spmd_dparrbe3.F
Chd|        SPMD_DPARRBY                  source/mpi/anim/spmd_dparrby.F
Chd|        SPMD_EXCH_N                   source/mpi/generic/spmd_exch_n.F
Chd|        SPMD_EXCH_NODAREA             source/mpi/anim/spmd_exch_nodarea.F
Chd|        SPMD_EXCH_NODAREA2            source/mpi/anim/spmd_exch_nodarea2.F
Chd|        SPMD_EXCH_NODAREAI            source/mpi/anim/spmd_exch_nodareai.F
Chd|        SPMD_FVB_ADIM                 source/mpi/anim/spmd_fvb_adim.F
Chd|        SPMD_FVB_AELF                 source/mpi/anim/spmd_fvb_aelf.F
Chd|        SPMD_FVB_AMON                 source/mpi/anim/spmd_fvb_amon.F
Chd|        SPMD_FVB_ANOD                 source/mpi/anim/spmd_fvb_anod.F
Chd|        SPMD_FVB_ANUM                 source/mpi/anim/spmd_fvb_anum.F
Chd|        SPMD_FVB_AOFF                 source/mpi/anim/spmd_fvb_aoff.F
Chd|        SPMD_FVB_APAR                 source/mpi/anim/spmd_fvb_apar.F
Chd|        SPMD_FVB_ASUB1                source/mpi/anim/spmd_fvb_asub1.F
Chd|        SPMD_FVB_ASUB2                source/mpi/anim/spmd_fvb_asub2.F
Chd|        SPMD_FVB_ATIT                 source/mpi/anim/spmd_fvb_atit.F
Chd|        SPMD_FVB_ATR                  source/mpi/anim/spmd_fvb_atr.F
Chd|        SPMD_FVB_AVEC                 source/mpi/anim/spmd_fvb_avec.F
Chd|        SPMD_GATHERF                  source/mpi/anim/spmd_gatherf.F
Chd|        SPMD_GATHERITAB               source/mpi/anim/spmd_gatheritab.F
Chd|        SPMD_GATHERITAB_CRK           source/mpi/anim/spmd_gatheritab_crk.F
Chd|        SPMD_GLOB_IMAX9               source/mpi/generic/spmd_glob_imax9.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_MAX_XFE_I                source/mpi/elements/spmd_xfem.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        TENCGPS1                      source/output/anim/generate/tensorc.F
Chd|        TENCGPS2                      source/output/anim/generate/tensorc.F
Chd|        TENSGPS1                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS2                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|        TENSOR0                       source/output/anim/generate/tensor0.F
Chd|        TENSORC                       source/output/anim/generate/tensorc.F
Chd|        TENSORC_CRK                   source/output/anim/generate/tensorc_crk.F
Chd|        TENSORC_PLY                   source/output/anim/generate/tensorc_ply.F
Chd|        TENSORS                       source/output/anim/generate/tensor6.F
Chd|        TORSEUR                       source/output/anim/generate/torseur.F
Chd|        VELVEC                        source/output/anim/generate/velvec.F
Chd|        VELVEC2                       source/output/anim/generate/velvec.F
Chd|        VELVEC3                       source/output/anim/generate/velvec.F
Chd|        VELVECC                       source/output/anim/generate/velvec.F
Chd|        VELVECC21                     source/output/anim/generate/velvec.F
Chd|        VELVECC22                     source/output/anim/generate/velvec.F
Chd|        VELVECC_MAX                   source/output/anim/generate/velvec.F
Chd|        VELVECZ22                     source/output/anim/generate/velvecz22.F
Chd|        WRITE_C_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        WRITE_S_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        XFECUT                        source/output/anim/generate/xfecut.F
Chd|        XYZ16                         source/output/anim/generate/genani.F
Chd|        XYZCUT                        source/tools/sect/xyzcut.F    
Chd|        XYZNOD                        source/output/anim/generate/xyznod.F
Chd|        XYZNOD_CRK                    source/output/anim/generate/xyznod_crk.F
Chd|        XYZNOD_CRK0                   source/output/anim/generate/xyznod_crk.F
Chd|        XYZNOD_PLY                    source/output/anim/generate/xyznod_ply.F
Chd|        XYZNOR                        source/output/anim/generate/xyznor.F
Chd|        XYZNOR16                      source/output/anim/generate/genani.F
Chd|        XYZNOR_CRK                    source/output/anim/generate/xyznor_crk.F
Chd|        XYZNOR_PLY                    source/output/anim/generate/xyznor_ply.F
Chd|        ALEANIM_MOD                   share/modules/aleanim_mod.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE GENANI(X        ,D        ,V           ,A           ,BUFEL    ,
     2                  IXS      ,IXQ      ,IXC         ,IXT         ,IXP      ,
     3                  IXR      ,IXTG     ,SWAFT       ,SMAS        ,SXNORM   ,
     4                  SIAD     ,IPARG    ,PM          ,GEO         ,MS       ,
     5                  SINVERT  ,CONT     ,SMATER      ,ICUT        ,SKEW     ,
     6                  XCUT     ,FINT     ,ITAB        ,SEL2FA      ,FEXT     ,
     7                  FOPT     ,ANIN     ,LPBY        ,NPBY        ,NSTRF    ,
     8                  RWBUF    ,NPRW     ,TANI        ,ELBUF_TAB   ,MAT_PARAM,
     A                  DD_IAD   ,WEIGHT   ,EANI        ,IPART       ,CLUSTER  ,
     B                  IPARTS   ,IPARTQ   ,IPARTC      ,IPARTT      ,IPARTP   ,
     C                  IPARTR   ,IPARTUR  ,IPARTTG     ,
     D                  RBY      ,SWA4     ,TORS        ,NOM_OPT     ,IGRSURF  ,
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
     P                  IPARI    ,FNCONT2  ,DR          ,ALE_CONNECTIVITY       ,
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
     1                  H3D_DATA ,SUBSET   ,MULTI_FVM   ,KNOTLOCPC   ,KNOTLOCEL ,
     2                  FCONT_MAX,FNCONTP2 ,FTCONTP2    )
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
      USE ALEANIM_MOD                 
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD 
      USE INOUTFILE_MOD
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
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "com_xfem1.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "scr25_c.inc"
#include      "chara_c.inc"
#include      "scrcut_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "flowcom.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "filescount_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SWAFT,SMAS,SXNORM,SIAD,SINVERT,SMATER,SEL2FA,SWA4,
     .        SIADG,NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),
     .        LESDVOIS(*),SPH2SOL(*)
      INTEGER
     .  SUIX, SXUSR ,SFACPTX,SIXEDGE,SIXFACET,SIXSOLID,SNUMX1,
     .  SNUMX2,SNUMX3,SOFFX1,SOFFX2,SOFFX3,SMASS1,SMASS2,
     .  SMASS3,SFUNC1,SFUNC2,SFUNC3,SFIN,SNFACPTX

      INTEGER  IGEO(NPROPGI,*),IPM(NPROPMI,*),INDX_CRK(*),
     .  LRBE2(*),LRBE3(*),FR_RBE2(3,*),FR_RBE3M(3,*),
     .  NOD_PXFEM(*), IEL_PXFEM(*),NODEDGE(2,*),XEDGE4N(4,*),XEDGE3N(3,*),
     .  INOD_CRK(*),IEL_CRK(*),ELCUTC(2,*),IADC_CRK(*)
      my_real
     .   X(3*NUMNOD), D(3*NUMNOD), V(3*NUMNOD), A(3,NUMNOD), BUFEL(*),
     .   PM(NPROPM,NUMMAT), GEO(NPROPG,*),CONT(*),
     .   XCUT(*) , FINT(3,*),MS(*),RWBUF(NRWLP,*),SKEW(LSKEW,*),
     .   RBY(NRBY,*),FEXT(3,*) ,FOPT(6,*),ANIN(*),TANI(6,*),EANI(*),
     .   TORS(15,*),BUFSF(*), RDATA(*),
     .   BUFMAT(*),BUFGEO(*),
     .   SPBUF(*), VR(*),VOLMON(*), RFLOW(*), FNCONT(3,*), FTCONT(3,*),
     .   TEMP(*), THKE(*), ERR_THK_SH4(*), ERR_THK_SH3(*), DIAG_SMS(*),
     .   FNCONT2(3,*), DR(3,*),DXANCG(3,*),ZI_PLY(*),VGAZ(*),
     .   FCONTG(*), FNCONTG(*), FTCONTG(*),FANREAC(6,*),PDAMA2(2,*),
     .   RES_SMS(*),FCLUSTER(3,*),MCLUSTER(3,*),W(*),
     .   WIGE(*),KNOT(*),STIFN(*),STIFR(*),KNOTLOCPC(*),KNOTLOCEL(*),
     .   FCONT_MAX(*),FNCONTP2(3,*)  ,FTCONTP2(3,*)
      INTEGER IPARG(NPARG,*),NSTRF(*),LPBY(*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),MONVOL(*) ,
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*) ,
     .        ICUT(*), ITAB(*),NPBY(NNPBY,*),NPRW(*),
     .        WEIGHT(*),IPART(LIPART1,*),IPARTS(*),IPARTQ(*),IPARTC(*),
     .        IPARTT(*),IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),
     .        NOM_OPT(*),
     .        IDATA(*),KXX(NIXX,*), IXX(*), IPARTX(*),
     .        KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*), IPARTSP(*),
     .        NODGLOB(*),IAD_ELEM(2,*),FR_ELEM(*),FR_WALL(*), IFLOW(*),
     .        IPARI(NPARI,*),IRBE2(NRBE2L,*),IRBE3(NRBE3L,*),
     .        WEIGHT_MD(*),NODGLOBXFE(*),IPARTIG3D(*)
      INTEGER CTEXT(2159), IB
      INTEGER DD_IAD(NSPMD+1,*),
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        N1,N2,N3
      INTEGER FR_SEC(NSPMD+1,*),FR_RBY2(3,*),IAD_RBY2(4,*),
     .        NERBT(NRBODY),LOC_PROC,PROC,NERBE2T(NRBE2G),
     .        NERBE3T(NRBE3G),IAD_RBE2(4,*),NV46,KXIG3D(*),
     .        IXIG3D(*),SIG3DSOLID
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)    :: CLUSTER
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (STACK_PLY) :: STACK
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
!
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)  :: IGRSURF
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ANIMSIZE
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: WAFT  , MAS   , XNORM, 
     .                                      XMASS1, XMASS2, XMASS3,
     .                                      XFUNC1, XFUNC2, XFUNC3,
     .                                      XUSR
      INTEGER IAD(SIAD),INVERT(SINVERT),MATER(SMATER),EL2FA(SEL2FA),
     .        IADG(NSPMD,SIADG),IADG_TPR(NSPMD,SIADG),
     .        UIX(SUIX)      ,NFACPTX(3,SNFACPTX),
     .        IXEDGE(SIXEDGE), IXFACET(SIXFACET), IXSOLID(SIXSOLID),
     .        INUMX1(SNUMX1) , INUMX2(SNUMX2)   , INUMX3(SNUMX3),
     .        IOFFX1(SOFFX1) , IOFFX2(SOFFX2)   , IOFFX3(SOFFX3),
     .        SZ16,SHFT16,IADGPS,NSN,IADISO     , IG3DSOLID(SIG3DSOLID),
     .        FIRST_NODE_IG3D
      REAL
     .       , DIMENSION(:), ALLOCATABLE :: WA4, MAS4, WA4_FVM
      my_real
     .   X_TEMP(3,64*NUMELIG3D),D_TEMP(3,64*NUMELIG3D),
     .   V_TEMP(3,64*NUMELIG3D),A_TEMP(3,64*NUMELIG3D),TABSTRESL(6,64*NUMELIG3D),
     .   BID_TEMP(3,64*NUMELIG3D),CONT_TEMP(3,64*NUMELIG3D),FINT_TEMP(3,64*NUMELIG3D),
     .   FEXT_TEMP(3,64*NUMELIG3D),FANREACT_TEMP(3,64*NUMELIG3D)
       

      my_real ,DIMENSION (:), ALLOCATABLE :: CBUF
      INTEGER ,DIMENSION (:), ALLOCATABLE :: ICBUF
      SAVE CBUF,ICBUF

      CHARACTER*80 STR, MES*30, CAUX,TITL*100
      CHARACTER CHANIM*9,FILNAM*100, CHANIM1*4
      INTEGER I, IDX,NBF, NBPART, MAGIC, J, IFUNC, FILEN, NPSOL,
     .        NODCUT,NELCUT,LENR,LENI,LENCUT,LENCUTO,IXEL,
     .        MIC1,MIC2,MIC3,MIC4,MIC5,MAC1,MAC2,MAC3,NPSPR,N,K,
     .        I3000, NESCT,NERBY,NERWL,NNWL,TMPNBF,ISK(6),
     .        NESBW2,NEL,NFT,ITY,NG,OUI,IPT
      INTEGER II,II_L,INC,P,NSLARB_L,NDMA2,NUMELS_T,NSKEWA,NB1D,
     .        M1,M2,M3,M,M01,NB1D_T, NBF_L, LEN, NUMELT_T,
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
      INTEGER NANIM1D_L,IUS,NANIM3D_L
      INTEGER ISPH3D,M4,N0
      INTEGER LTITL
      INTEGER LRBUF,BUFL,BUFFERP(NPART),SBUFSPM,SBUFRECVM,SBUFSPO,
     .        SPORBY,NUMSPH_T,NUMELS16_T,LRBUFG,NNNG
      my_real
     .   CDG(3), S3000,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX, SCALE,
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
     .        NBPART2D,NRBE2T,NRBE3T,EMPSIZPL
      my_real
     .        GAMA, SSP, FAC
      INTEGER, DIMENSION(:), ALLOCATABLE :: OFFTR, ITAGT, FVEL2FA,
     .                                      FVINUM, FVPBUF,EL2FA_PLY,
     .                                      IAD_PLY,ITAB_PLY

      my_real
     .       , DIMENSION(:), ALLOCATABLE :: FVMASS, FVPRES, FVQX, FVQY,
     .                                      FVQZ, FVRHO, FVENER, FVCSON,
     .                                      FVGAMA, FVVISU,WAFT_PLY,
     .                                      WAFT_CRK

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_PLYG
      INTEGER IUL,IAD_GP2,IFIRST,
     .        NEL_PLY, NFNOD_PXFEM,IDPLY,NBF_PXFEM_L,
     .        IPLY,NPLYSUBS,ID_PART,NBF_PXFEM,SWAFT_PXFEM,MAXPART,
     .        NBF_PXFEMG,NFNOD_PXFEMG,NFNODSZ(NPLYMAX),PLYNUMC,
     .        NFSHSZ(NPLYMAX),SEL2FA_PLY,IADPC,IFV,IAD_GP3,IAD_ISO,IAD_GP4
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGPS      
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: WGPS  , VGPS

      REAL R4
      SAVE LENCUTO
      DATA LENCUTO/0/
      INTEGER IERROR
      INTEGER ITG,NPLYPARTW,ILAY,ILEV,IANIM_TMP,CPT,KKT
C
      INTEGER NFNOD_CRKXFEM,IDCRK,ICRK,NCRKSUBS,
     .        NBF_CRKXFEM,LEN_CRKX,NBF_CRKXFEMG,NFNOD_CRKXFEMG,
     .      NFSHSZCRK(NLEVMAX),SEL2FA_CRK,NCRKPARTW,NXFENODG2(NLEVMAX),
     .      IDMAXNOD,SWAFT_CRK
      INTEGER, DIMENSION(:), ALLOCATABLE :: EL2FA_CRK,IAD_CRK,ITAB_CRK,
     .        IAD_LAY
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_CRKG

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME
      LOGICAL :: CONDITION
      INTEGER :: IS_WRITTEN_NODE_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
      INTEGER :: IS_WRITTEN_NODE(NUMNOD)
C=======================================================================
      ALLOCATE(WAFT(SWAFT)   , MAS(SMAS)     , XNORM(SXNORM) ,
     .         XMASS1(SMASS1), XMASS2(SMASS2), XMASS3(SMASS3),
     .         XFUNC1(SFUNC1), XFUNC2(SFUNC2), XFUNC3(SFUNC3),
     .         XUSR(SXUSR) )
      WAFT(1:SWAFT) = ZERO
 
      ALLOCATE (WA4(SWA4), MAS4(SMAS))
      ALLOCATE (WA4_FVM(AIRBAGS_TOTAL_FVM_IN_H3D))

      ALLOCATE(VFLU(3,NUMNOD), VVAR1(3,NUMNOD), AFLU(3,NUMNOD),
     .        VFLU_ALE(3,NUMNOD),FANREACT(3,NUMNOD),FANREACR(3,NUMNOD))

      ALLOCATE(WGPS(NUMNOD), VGPS(NUMNOD), ITAGPS(NUMNOD))
       
       IF (ANIM_PLY > 0)THEN
         NPLYPARTW=NPLYPART
       ELSE
         NPLYPARTW=0
       ENDIF
       IF (ANIM_CRK > 0 .and. ICRACK3D > 0 .and. NXEL > 0) THEN
         NCRKPARTW = INT(NCRKPART/NXEL)
       ELSE
         NCRKPARTW = 0
       ENDIF
      NRBE2T = NRBE2G
      NRBE3T = NRBE3G

      LOC_PROC = ISPMD+1
      IF(ANIM_VERS>=47)THEN
        LTITL = 80
      ELSE
        LTITL = 40
      ENDIF
      IF(ANIM_VERS<44)THEN
       ISPH3D=1
      ELSE
       ISPH3D=0
      ENDIF
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
      I16M=I16L+LNOPT1*NRBE2T
      I16N=I16M+LNOPT1*NRBE3T
C-----------------------------------------------
      S3000 = THREE1000
      I3000 = S3000
C-----------------------------------------------
C   OPEN FILE
C-----------------------------------------------
      IF(ISPMD==0) THEN
       IF(ANIM_VERS<50)THEN
        IF(IANIM>=1000)THEN
          IANIM_TMP = IANIM 
          CPT = 1
          DO WHILE(IANIM_TMP /= 0)
            IANIM_TMP = IANIM_TMP / 10
            CPT = CPT + 1
          ENDDO
          IF (CPT == 5)THEN
            WRITE(CHANIM,'(I4.4)')IANIM
            FILNAM=ROOTNAM(1:ROOTLEN)//'A'//CHANIM
            FILEN = ROOTLEN + 5
          ELSEIF (CPT == 6)THEN
            WRITE(CHANIM,'(I5.5)')IANIM
            FILNAM=ROOTNAM(1:ROOTLEN)//'A'//CHANIM
            FILEN = ROOTLEN + 6
          ELSEIF (CPT == 7)THEN
            WRITE(CHANIM,'(I6.6)')IANIM
            FILNAM=ROOTNAM(1:ROOTLEN)//'A'//CHANIM
            FILEN = ROOTLEN + 7
          ELSEIF (CPT == 8)THEN
            WRITE(CHANIM,'(I7.7)')IANIM
            FILNAM=ROOTNAM(1:ROOTLEN)//'A'//CHANIM
            FILEN = ROOTLEN + 8
          ELSEIF (CPT == 9)THEN
            WRITE(CHANIM,'(I8.8)')IANIM
            FILNAM=ROOTNAM(1:ROOTLEN)//'A'//CHANIM
            FILEN = ROOTLEN + 9
          ELSE
            IANIM = 1
            WRITE(CHANIM,'(I3.3)')IANIM
            FILNAM=ROOTNAM(1:ROOTLEN)//'A'//CHANIM
            FILEN = ROOTLEN + 4
          ENDIF
        ELSE
          WRITE(CHANIM,'(I3.3)')IANIM
          FILNAM=ROOTNAM(1:ROOTLEN)//'A'//CHANIM
          FILEN = ROOTLEN + 4
        ENDIF
       ENDIF
       IF(ANIM_VERS>=50)THEN
        IF(IANIM>=10000)IANIM=1
        WRITE(CHANIM1,'(I4.4)')IANIM
        FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHANIM1//'.ani'
        FILEN = ROOTLEN + 9
       ENDIF
C
        LEN_TMP_NAME = OUTFILE_NAME_LEN + FILEN      
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:FILEN)
        DO I=1,LEN_TMP_NAME
         CTEXT(I)=ICHAR(TMP_NAME(I:I))
        ENDDO
        CALL CUR_FIL_C(0)
        IF(IZIP==0)THEN
          CALL OPEN_C(CTEXT,LEN_TMP_NAME,0)
        ELSEIF(IZIP==1)THEN
          CALL OPEN_C(CTEXT,LEN_TMP_NAME,3)
        ELSEIF(IZIP==2)THEN
          CALL OPEN_C(CTEXT,LEN_TMP_NAME,6)
        ENDIF
      ENDIF
C-----------------------------------------------
C  USER ROUTINE RETURNS ALL NEEDED INFORMATION FOR ALL X-ELEMENTS.
C-----------------------------------------------
C    NFACPTX must not be accessed to if NUMELX==0.
      NANIM1D_L = 0
      IF (NUMELXG>0) THEN

       DO I=1,NPART
        NFACPTX(1,I)=0
        NFACPTX(2,I)=0
        NFACPTX(3,I)=0
       ENDDO
       CALL ANIMX(ELBUF_TAB,
     .            IPARG    ,ITAB     ,X        ,KXX      ,IXX    ,
     .            IPARTX   ,PM       ,GEO      ,BUFMAT   ,BUFGEO ,
     .            UIX      ,XUSR     ,NFACPTX  ,IXEDGE   ,IXFACET,
     .            IXSOLID  ,INUMX1   ,INUMX2   ,INUMX3   ,IOFFX1 ,
     .            IOFFX2   ,IOFFX3   ,XMASS1   ,XMASS2   ,XMASS3 ,
     .            XFUNC1   ,XFUNC2   ,XFUNC3   ,NANIM1D_L)
      ENDIF
C-----------------------------------------------
C ISO GEO ELEMS __1.
C-----------------------------------------------
      NANIM3D_L = 0
      IF (NUMELIG3D>0) THEN
       BID_TEMP(:,:) = ZERO  ! MIS A ZERO POUR LES VALEURS VECTORIELLES QUI NE SONT PAS CALCULEES
       FIRST_NODE_IG3D = 1000000000  ! ATTENTION SI ON CREE DES POINTS AUTRE PART, ON RISQUE DE DEBORDER DES 1000000000
       CALL ANIMIG3D(ELBUF_TAB,IPARG    ,X     ,D     ,V      ,A ,
     .               WIGE     ,KXIG3D   ,IXIG3D,IG3DSOLID,NANIM3D_L,
     .               X_TEMP   ,D_TEMP   ,V_TEMP,A_TEMP, TABSTRESL,IGEO  ,
     .               KNOT     ,ITAB     ,IPARTIG3D,IPART ,CONT, CONT_TEMP,
     .               FINT     ,FINT_TEMP,FEXT  ,FEXT_TEMP, FANREAC, FANREACT_TEMP,
     .               KNOTLOCPC,KNOTLOCEL)
      ENDIF
C-----------------------------------------------
C      SKEW + NB1D
C-----------------------------------------------
C     dans le starter on calcul NB1DG et NSKEWAG
        NSKEWA=NUMELP + NUMELT + NUMSKW
        NB1D  =NUMELP + NUMELT + NUMELR
C
        DO I=1,NUMELR
          IF(IGEO(11,IXR(1,I))==12)THEN
            NB1D = NB1D+1
          ENDIF
        ENDDO
           NSKEWA = NSKEWAG
      IF(NSPMD>1.AND.NFVBAG>0)
     .   CALL SPMD_FVB_AMON(MONVOL, VOLMON)
C
           IF (ANIM_V(10)>0) THEN
              DO I=1,3
                 DO J=1,NUMNOD
                    VFLU_ALE(I,J)=ZERO
                 ENDDO
              ENDDO
              K1=1
              KK1=1
              K2=1+NIMV*NVOLU
              KIBJET=K2+LICBAG
              KIBHOL=KIBJET+LIBAGJET
              IADHOL=KIBHOL+LIBAGHOL
              KK2=1+NRVOLU*NVOLU
              KRBJET=KK2+LRCBAG
              KRBHOL=KRBJET+LRBAGJET
              RADHOL=KRBHOL+LRBAGHOL
              IFV=0
              DO I=1,NVOLU
                 ITYP=MONVOL(K1+1)
                 NCA=MONVOL(K1+2)
                 NJET=MONVOL(K1+7)
                 NVENT=MONVOL(K1+10)
                 IF (ITYP==6.OR.ITYP==8) THEN
                    IFV=MONVOL(K1-1+45)             
                    NNS=MONVOL(K1-1+32)
                    NTG=MONVOL(K1-1+33)
                    NBA=MONVOL(K1-1+62)
                    NNA=MONVOL(K1-1+64)
                    NNI =MONVOL(K1-1+68)
                    NTGI=MONVOL(K1-1+69)
                    NNT =NNS+NNI
                    KI1=IADHOL+MONVOL(K1-1+31)
                    KI2=KI1+MONVOL(K1-1+20)-1
                    KR1=RADHOL+MONVOL(K1-1+34)+3*NNT
                    KR2=KR1+3*NNT+4*(NTG+NTGI)+3*NNA
                    CALL ALEVFLU(
     .      VFLU_ALE,    NNT,         MONVOL(KI1), VOLMON(KR1), NNA,
     .      MONVOL(KI2), VOLMON(KR2), IFV)
                 ENDIF
                 K1=K1+NIMV
                 KK1=KK1+NRVOLU
                 K2=K2+NICBAG*NCA
                 KK2=KK2+NRCBAG*NCA
              ENDDO
           ENDIF
           DO I=1,3
              DO J=1,NUMNOD
                 VFLU(I,J)=ZERO
              ENDDO
           ENDDO
           IADI=0
           IADR=0
           DO I=1,NFLOW
              ITYP=IFLOW(IADI+2)
              IF (ITYP==1) THEN
                 NINOUT=IFLOW(IADI+4)
                 NNO=IFLOW(IADI+5)
                 NEL=IFLOW(IADI+6)
                 NNN=IFLOW(IADI+7)
                 NNO_L=IFLOW(IADI+16)
                 NNN_L=IFLOW(IADI+22)
                 II1=1+NIFLOW
                 II2=II1+NNO+3*NEL+NINOUT*NIIOFLOW
                 IF(NSPMD == 1) THEN
                   II3=II2+NNN+NEL
                   II4=II3+NNO
                 ELSE
                   II3=II2+NNN+NEL+2*NNO
                   II4=II3+2*NNO
                 ENDIF
                 IR1=1+NRFLOW+2*(NNO+NNN)
                 CALL ANIVFLOWP(
     .               VFLU,       NNO,        NNN,        NNO_L,      NNN_L,
     .               IFLOW(II1), IFLOW(II2), IFLOW(II3), IFLOW(II4), RFLOW(IR1))
              ENDIF
C
              IADR=IADR+IFLOW(IADI+15)
              IADI=IADI+IFLOW(IADI+14)
           ENDDO
C=======================================================================
C
C      COQUE 3N 4N
C
C=======================================================================
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
      DO I=1,NPART
        MATER(I)=0
      ENDDO
        DO NG = 1, NGROUP
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          ITY   =IPARG(5,NG)
          IF(ITY==2)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTQ(N)) = 1
            ENDDO
          ELSEIF(ITY==3)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTC(N)) = 1
            ENDDO
          ELSEIF(ITY==7)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTTG(N)) = 1
            ENDDO
          ELSEIF(ITY==50)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTUR(N)) = 1
            ENDDO
          ENDIF
C     2D geometry is not yet treated.
        ENDDO
C
        IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(MATER,NPART)
        DO I=1,NPART
          IF(MATER(I)>1)MATER(I) = 1
        ENDDO
        IF(NSPMD > 1)CALL SPMD_IBCAST(MATER,MATER,NPART,1,0,2)
C
      NBPART = 0
      DO I=1,NPART
        NBPART = NBPART + MATER(I)
      ENDDO
C
      NBF =  NUMELQ + NUMELC + NUMELTG
      NBF_L = NBF
      NBF = NUMELQG+NUMELCG+NUMELTGG
      DO I=1,NUMELQ + NUMELC + NUMELTG +  1
        EL2FA(I)=0
      ENDDO
C
C ---  ply xfem
C
       NBF_PXFEM  = 0
       NFNOD_PXFEM = 0

       NBF_PXFEMG  = 0
       NFNOD_PXFEMG = 0

       IF (ANIM_PLY > 0 ) THEN

         IF (NSPMD > 1) THEN
C Xfem init for NSPMD > 1

C Ply Elements Elements
           DO I=1,NPLYMAX
             NFSHSZ(I) = PLYSHELL(I)%PLYNUMSHELL
           ENDDO
           CALL SPMD_GLOB_ISUM9(NFSHSZ,NPLYMAX)
           IF (ISPMD == 0)THEN
              DO I=1,NPLYMAX
                 NBF_PXFEMG = NBF_PXFEMG + NFSHSZ(I)
              ENDDO
              NFNOD_PXFEMG = NPLYNODG
              SWAFT_PXFEM = MAX(3*NFNOD_PXFEMG,3*NBF_PXFEMG)
              SEL2FA_PLY = NBF_PXFEMG

           ELSE
              DO I=1,NPLYMAX
                 NFNOD_PXFEM = NFNOD_PXFEM + PLYNOD(I)%PLYNUMNODS
                 NBF_PXFEM   = NBF_PXFEM   + PLYSHELL(I)%PLYNUMSHELL
              ENDDO
              SWAFT_PXFEM = MAX(3*NFNOD_PXFEM,3*NBF_PXFEM)
              SEL2FA_PLY = NBF_PXFEM
           ENDIF
           ALLOCATE(EL2FA_PLY(SEL2FA_PLY), WAFT_PLY(SWAFT_PXFEM),
     .          IAD_PLY(NPLYMAX),IAD_PLYG(NSPMD,NPLYMAX))
           EL2FA_PLY = 0
           WAFT_PLY = ZERO
           IAD_PLY = 0
           IAD_PLYG = 0
         ELSE
C Xfem init for NSPMD = 1

cc         NBF_PXFEM =  EPLYXFE*NPLYPART
           DO I=1,NPLYMAX
             NFNOD_PXFEM = NFNOD_PXFEM + PLYNOD(I)%PLYNUMNODS
             NBF_PXFEM   = NBF_PXFEM + PLYSHELL(I)%PLYNUMSHELL
           ENDDO

           SWAFT_PXFEM = MAX(3*NFNOD_PXFEM,3*NBF_PXFEM)
           ALLOCATE(EL2FA_PLY(NBF_PXFEM), WAFT_PLY(SWAFT_PXFEM),
     .          IAD_PLY(NPLYMAX),IAD_PLYG(NSPMD,NPLYMAX))
           EL2FA_PLY = 0
           WAFT_PLY = ZERO
           IAD_PLY = 0
           IAD_PLYG = 0

C
           NFNOD_PXFEMG = NFNOD_PXFEM
           NBF_PXFEMG = NBF_PXFEM
         ENDIF
      ENDIF
c --- plyxfem
c--------------------------------------------------------------------      
c     xfem crack for layered shells
c--------------------------------------------------------------------      
      NBF_CRKXFEM    = 0                                                                  
      NFNOD_CRKXFEM  = 0                                                                 
      NBF_CRKXFEMG   = 0                                                                 
      NFNOD_CRKXFEMG = 0                                                                
      NXFENODG       = 0                                                                      
C
      IF (ANIM_CRK > 0) THEN                                                            
        IF (NSPMD > 1) THEN                                                             
          DO I=1,NLEVMAX                                                                
            NFSHSZCRK(I) = CRKSHELL(I)%CRKNUMSHELL                                      
          ENDDO                                                                         
          CALL SPMD_GLOB_ISUM9(NFSHSZCRK,NLEVMAX)                                       
          CALL SPMD_IBCAST(NFSHSZCRK,NFSHSZCRK,NLEVMAX,1,0,2)                           
C
          DO I=1,NLEVMAX                                                                
            NXFENODG2(I) = CRKNOD(I)%CRKNUMNODS                                         
          ENDDO                                                                         
          CALL SPMD_GLOB_ISUM9(NXFENODG2,NLEVMAX)                                       
C
          IF (ISPMD == 0) THEN                                                           
             DO I=1,NLEVMAX                                                             
                NBF_CRKXFEMG = NBF_CRKXFEMG + NFSHSZCRK(I)                              
             ENDDO                                                                      
C
             DO I=1,NLEVMAX                                                             
               NXFENODG    = NXFENODG + NXFENODG2(I)                                       
               NBF_CRKXFEM = NBF_CRKXFEM + CRKSHELL(I)%CRKNUMSHELL                   
             ENDDO                                                                      
C
             NFNOD_CRKXFEMG = NXFENODG                                                  
             LEN_CRKX   = MAX(NFNOD_CRKXFEMG,NBF_CRKXFEMG) ! max de noeuds et elems phant 
             SEL2FA_CRK = NBF_CRKXFEMG + 1                                              
             SWAFT_CRK  = MAX(3*NFNOD_CRKXFEMG,3*NBF_CRKXFEMG)                           
          ELSE                                                                          
            DO I=1,NLEVMAX                                                             
              NFNOD_CRKXFEM = NFNOD_CRKXFEM + CRKNOD(I)%CRKNUMNODS                       
              NBF_CRKXFEM   = NBF_CRKXFEM   + CRKSHELL(I)%CRKNUMSHELL                    
            ENDDO                                                                      
            LEN_CRKX   = MAX(NFNOD_CRKXFEM,NBF_CRKXFEM)                                  
            SEL2FA_CRK = NBF_CRKXFEM + 1                                               
            SWAFT_CRK  = MAX(3*NFNOD_CRKXFEM,3*NBF_CRKXFEM)                             
          ENDIF                                                                         
          ALLOCATE(EL2FA_CRK(SEL2FA_CRK),WAFT_CRK(SWAFT_CRK),                           
     .             IAD_CRK(NLEVMAX),IAD_CRKG(NSPMD,NLEVMAX)) 
          IF (NXEL > 0) THEN 
            ALLOCATE(IAD_LAY(INT(NLEVMAX/NXEL)))
          ELSE
            ALLOCATE(IAD_LAY(0))
          ENDIF                                          
          EL2FA_CRK = 0                                                                 
          IAD_CRK   = 0                                                                 
          IAD_CRKG  = 0                                                                 
          IAD_LAY   = 0                                                                 
          WAFT_CRK  = ZERO                                                               
        ELSE  ! NSPMD = 1                                                               
          DO I=1,NLEVMAX                                                                
            NFNOD_CRKXFEM = NFNOD_CRKXFEM + CRKNOD(I)%CRKNUMNODS                        
            NBF_CRKXFEM   = NBF_CRKXFEM + CRKSHELL(I)%CRKNUMSHELL                       
          ENDDO                                                                         
C
          LEN_CRKX = MAX(NFNOD_CRKXFEM,NBF_CRKXFEM)                                     
          SWAFT_CRK = MAX(3*NFNOD_CRKXFEM,3*NBF_CRKXFEM)                                
          ALLOCATE(EL2FA_CRK(NBF_CRKXFEM))
          ALLOCATE(WAFT_CRK(SWAFT_CRK))                         
          ALLOCATE(IAD_CRK(NLEVMAX))
          ALLOCATE(IAD_CRKG(NSPMD,NLEVMAX))                            
          ALLOCATE(IAD_LAY(INT(NLEVMAX/NXEL)))                                          
          EL2FA_CRK = 0                                                                 
          IAD_CRK   = 0                                                                 
          IAD_CRKG  = 0                                                                 
          IAD_LAY   = 0                                                                 
          WAFT_CRK  = ZERO                                                               
C
          NFNOD_CRKXFEMG = NFNOD_CRKXFEM                                                
          NBF_CRKXFEMG   = NBF_CRKXFEM                                                  
        ENDIF                                                                           
      ELSE
          ALLOCATE(EL2FA_CRK(0),IAD_CRK(0),IAD_CRKG(0,0),IAD_LAY(0),
     .             WAFT_CRK(0))
      ENDIF
C-----------------------------------------------
C   COUPES DANS LES SOLIDES
C-----------------------------------------------
      NODCUT=0
      NELCUT=0
      MIC1=1
      MIC2=1
      MIC3=1
      MIC4=1
      MIC5=1
      MAC1=1
      MAC2=1
      MAC3=1
      IF(NCUTS>0)THEN
          CALL CUTCNT (ICUT,XCUT,IXS,X,D,LENCUT)
          LENCUT = MAX(LENCUT,NCUTS)
          IF(LENCUT>LENCUTO)THEN
            IF(ALLOCATED(CBUF))THEN
              DEALLOCATE(CBUF)
              DEALLOCATE(ICBUF)
            ENDIF
            LENR=42*LENCUT
            LENI=28*LENCUT+2*NCUTS
            ALLOCATE(CBUF(LENR),STAT=oui)
            ALLOCATE(ICBUF(LENI),STAT=oui)
            IF(oui/=0) THEN
               CALL ANCMSG(MSGID=29,ANMODE=ANINFO)
               CALL ARRET(2)
            ENDIF
            LENCUTO=LENCUT
          ENDIF
          CALL CUTMAIN(ICUT  ,XCUT  ,IXS  ,X   ,D      ,
     .                 NODCUT,NELCUT,ICBUF,CBUF,LENCUTO,NBF)
          MIC1=1
          MIC2=MIC1+10*LENCUTO
          MIC3=MIC2+12*LENCUTO
          MIC4=MIC3+6*LENCUTO
          MIC5=MIC4+NCUTS
          MAC1=1
          MAC2=MAC1+18*LENCUTO
          MAC3=MAC2+6*LENCUTO
      ENDIF
C     IF(.NOT.(ALLOCATED(CBUF)))  ALLOCATE(CBUF(1),STAT=oui)
C     IF(.NOT.(ALLOCATED(ICBUF)))  ALLOCATE(ICBUF(1),STAT=oui)

C-----------------------------------------------
C  pour avoir une unification des tests en SPH
      NUMSPH_T = NUMSPHG

      NESCT = 0
      NERWL = 0
      NNWL  = 0
      NESBW2= 0
      IF(NSECT+NRWALL>0) THEN
         CALL DSECCNT(NESCT,NERWL,NESBW2,NSTRF,
     1                RWBUF ,NPRW,NNWL,IXS)
      END IF

      NESRG=0
      NNSRG=0
      NSURG=0
      IF (NSURF>0)
     . CALL DSRGCNT(IGRSURF, NSURG,NESRG,NNSRG,NESBW2)
      NESMD=0
      NNSMD=0
      NSMAD=0
      NESPH=0
      NNSPH=0
      NNSPHG = 0
      IF (ISPH3D==1.AND.NUMSPH_T+MAXPJET>0)
     .  CALL DSPHCNT(NESPH,NNSPH,NESPHG,NNSPHG)
C-----------------------------------------------
C   MAILLAGE VOLUMES FINIS POUR FVMBAG
C-----------------------------------------------
      IDMAX=0
      NFVNOD=0
      NFVTR=0
      NFVPART=0
      NFVSUBS=0
C traitement equivalent en mono
      IF (ANIM_PLY > 0) THEN
            IDMAX=0
            DO I=1,NUMNOD
               IDMAX=MAX(IDMAX,ITAB(I))
            ENDDO
        IF (NSPMD > 1) CALL SPMD_GLOB_IMAX9(IDMAX,1)
      ENDIF
C
      IF (NSPMD == 1) THEN
         IF (NFVBAG>0.OR. ANIM_PLY > 0) THEN
            IDMAX=0
            DO I=1,NUMNOD
               IDMAX=MAX(IDMAX,ITAB(I))
            ENDDO
         ENDIF
C
         IF (IFVANI==1) THEN
            DO I=1,NFVBAG
               NFVTR=NFVTR+FVDATA(I)%NNTR
               FVOFF(1,I)=NUMNOD+NODCUT+NSECT+NRWALL+NNWL
     .                 +NNSRG+NNSMD+NNSPH+2*NUMELS16+NFVNOD
               FVOFF(2,I)=IDMAX+NFVNOD
               NFVNOD=NFVNOD+FVDATA(I)%NNS_ANIM
               NFVPART=NFVPART+FVDATA(I)%NPOLH_ANIM
               NFVSUBS=NFVSUBS+1
            ENDDO
         ENDIF
      ELSE
         IF (IFVANI==1)
     .      CALL SPMD_FVB_ADIM(NFVTR, FVOFF,  NFVNOD, NFVPART, NFVSUBS,
     .                         IDMAX, ITAB,   NODCUT, NNWL,    NNSRG,
     .                         NNSMD, NNSPHG)
      ENDIF
      IF (ISPMD==0.AND.NFVTR>0)
     .   ALLOCATE(FVEL2FA(NFVTR), FVINUM(NFVTR))
C
c--------------------------------------------------------------------      
c     xfem crack for layered shells
      IF (ANIM_CRK > 0) THEN
        IF (NSPMD == 1) THEN
          DO I=1,NUMNOD
            IDMAX = MAX(IDMAX,ITAB(I)) ! Id max de tous les noeuds std
          ENDDO
        ELSE
          CALL SPMD_CRK_IDMAX(IDMAX,ITAB)
          CALL SPMD_MAX_XFE_I(IDMAX)
        ENDIF
      ENDIF
      IDMAXNOD = IDMAX
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
C
      NUMELS_T = NUMELSG
      NUMELS16_T = NUMELS16G
      NUMELT_T = NUMELTRG
      NUMELR_T = NUMELRG
      NUMELP_T = NUMELPG

      IF (ISPMD==0) THEN
C             0x542b
        MAGIC = 21548
        CALL WRITE_I_C(MAGIC,1)
C
        R4 = TT
        IF (NEIG==0) THEN
           CALL WRITE_R_C(R4,1)
           CALL ANI_TXT('Time=',5)
           KKT=0
           DO K=1,NLTITLE
             IF(NTITLETAB(K)==IANIM) THEN
               KKT=K
               TITL = TITLETAB(K)
             ENDIF
           ENDDO
           IF(KKT/= 0) THEN 
             CALL ANI_TXT(TITL,81)
           ELSE
             CALL ANI_TXT('ModAnim',7)
           ENDIF
        ELSE
           IF (R4>=ZERO) THEN
              CALL WRITE_R_C(R4,1)
              CALL ANI_TXT('Frequency=',10)
              CALL ANI_TXT('Eigenmode',9)
           ELSE
              CALL WRITE_R_C(ZERO,1)
              CALL ANI_TXT('Frequency=',10)
              CALL ANI_TXT('Static mode',11)
           ENDIF
        ENDIF
        CALL ANI_TXT('Radioss Run=',12)
C
        CALL WRITE_I_C(ANIM_M,1)
        CALL WRITE_I_C(1,1)

        IF(NUMELS_T+ISPH3D*(NUMSPH_T+MAXPJET)+NUMELIG3D==0) THEN
          CALL WRITE_I_C(0,1)
        ELSE
          CALL WRITE_I_C(1,1)
        ENDIF

        IFLAG1D = NUMELT_T+NUMELP_T+NUMELR_T+NANIM1D+NRBODY+
     .            NRBE2T+NRBE3T
        IF (IFLAG1D/=0) IFLAG1D = 1
        CALL WRITE_I_C(IFLAG1D,1)

C HIERARCHY
        CALL WRITE_I_C(1,1)
C TH
        CALL WRITE_I_C(0,1)
C REP. SHELL

        IF(ISHFRAM==1)THEN
          CALL WRITE_I_C(0,1)
        ELSE
          CALL WRITE_I_C(1,1)
        ENDIF

        IF(ISPH3D==0.AND.
     .     (NUMSPH_T+MAXPJET/=0))THEN
         CALL WRITE_I_C(1,1)
        ELSE
         CALL WRITE_I_C(0,1)
        ENDIF

        IF(ANIM_VERS>=47)THEN
          CALL WRITE_I_C(1,1)
        ELSE
          CALL WRITE_I_C(0,1)
        ENDIF
        CALL WRITE_I_C(0,1)
        IF (NFVNOD>0) THEN
           NFVNODT=NFVNOD+3
        ELSE
           NFVNODT=0
        ENDIF
        CALL WRITE_I_C(NUMNODG+NODCUT+NSECT+NRWALL+NNWL
     .           +NNSRG+NNSMD+NNSPHG+2*NUMELS16G+NFVNODT+NFNOD_PXFEMG
     .           +NFNOD_CRKXFEMG+64*NUMELIG3D,1)
c
        CALL WRITE_I_C(NBF+NELCUT+NESBW2+NFVTR+NBF_PXFEMG
     .                +NBF_CRKXFEMG,1)
        NBPART2D=NBPART+NCUTS+NSECT+NRWALL+NSURG+NSMAD
Cplyxfem
        NBPART2D = NBPART2D + NPLYPARTW
C crack xfem
        NBPART2D = NBPART2D + NCRKPARTW
        CALL WRITE_I_C(NBPART+NCUTS
     .       +NSECT+NRWALL+NSURG+NSMAD+NFVPART+NPLYPARTW
     .       +NCRKPARTW,1)
        CALL WRITE_I_C(NN_ANI,1)
        IF(NBF+NELCUT+NESBW2+NFVTR+NBF_PXFEMG
     .        +NBF_CRKXFEMG==0)THEN
          CALL WRITE_I_C(0,1)
        ELSE
          CALL WRITE_I_C(NCE_ANI,1)
        ENDIF
        CALL WRITE_I_C(NV_ANI,1)
        IF(NBF+NELCUT+NESBW2+NFVTR+NBF_PXFEMG
     .        +NBF_CRKXFEMG==0)THEN
          CALL WRITE_I_C(0,1)
        ELSE
          CALL WRITE_I_C(NCT_ANI,1)
        ENDIF
        CALL WRITE_I_C(NSKEWA,1)
      ENDIF

C-----------------------------------------------
C      SKEW
C-----------------------------------------------
       IF (ISPMD==0) THEN
          BUFL = NB1DG*6
       ELSE
          BUFL = NB1D*6
       ENDIF
       CALL ANISKEW(ELBUF_TAB,SKEW ,IPARG,X     ,IXT ,
     2                 IXP      ,IXR  ,GEO  ,DD_IAD,BUFL)
C-----------------------------------------------
C   NODE X Y Z
C-----------------------------------------------
      CALL SCANOR(X,D,CDG,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,SCALE,
     .            WEIGHT)
C
      CALL XYZNOD(X,X_TEMP,NODGLOB,WEIGHT)

      IF(NODCUT>0)CALL XYZCUT(CBUF,NODCUT)
C
      IF(NSECT+NRWALL>0) CALL DXYZSECT(
     2                  NSTRF,RWBUF,NPRW  ,X,XMIN,
     3                  YMIN,ZMIN,XMAX,YMAX,ZMAX,
     4                  FR_SEC,FR_WALL,WEIGHT,ITAB)

      IF (NSURG>0) CALL DXYZSRG(NESRG,IGRSURF,BUFSF)

      SNNSPHG = NNSPHG
      IF (ISPH3D*(NUMSPH_T+MAXPJET)>0)
     .   CALL DXYZSPH(NESPH,KXSP,X,SPBUF,SNNSPHG,NNSPH)
         SZ16 = NUMELS16G
      IF (SZ16>0)
     2   CALL XYZ16(IXS,IXS10,IXS20,IXS16,X)
C ply xfem
      IF(ANIM_PLY > 0)THEN
            IDPLY = NUMNODG+NODCUT+NSECT+NRWALL+NNWL
     .         +NNSRG+NNSMD+NNSPHG+2*SZ16
c
              EMPSIZPL=0
              DO I = 1,NPLYPART
                IPLY = INDX_PLY(I)
                CALL XYZNOD_PLY(IPLY,IDPLY,NOD_PXFEM,X,ZI_PLY,NODGLOB,
     *                          EMPSIZPL )
              ENDDO
      ENDIF
c--------------------------------------------------------------------      
c     xfem crack for layered shells
      IF (ANIM_CRK > 0) THEN
         IDCRK = NUMNODG+NODCUT+NSECT+NRWALL+NNWL    
     .                  +NNSRG+NNSMD+NNSPHG+2*SZ16   
         IF (NSPMD > 1) CALL SPMD_MAX_XFE_I(IDCRK)    
           DO I = 1,NCRKPART                         
             ICRK = INDX_CRK(I)        ! = ILEV        
             CALL XYZNOD_CRK0(ICRK)
             CALL XFECUT(IPARG   ,IXC      ,IXTG   ,ICRK   ,ELCUTC ,      
     .                   IEL_CRK ,IADC_CRK ,NODEDGE,CRKEDGE,XEDGE4N,
     .                   XEDGE3N )              
             CALL XYZNOD_CRK(ICRK,NFNOD_CRKXFEMG,NODGLOBXFE)              
           ENDDO
      ENDIF
c--------------------------------------------------------------------      
c
      IF (NFVNOD>0) THEN
C traitement equivalent mono
         IF (NSPMD == 1) THEN
            DO I=1,NFVBAG
               DO J=1,FVDATA(I)%NNS_ANIM
                  R4=FVDATA(I)%NOD_ANIM(1,J)
                  CALL WRITE_R_C(R4,1)
                  R4=FVDATA(I)%NOD_ANIM(2,J)
                  CALL WRITE_R_C(R4,1)
                  R4=FVDATA(I)%NOD_ANIM(3,J)
                  CALL WRITE_R_C(R4,1)
               ENDDO
            ENDDO
         ELSE
            CALL SPMD_FVB_ANOD()
         ENDIF
         IF (ISPMD==0) THEN
            R4=EM10
            CALL WRITE_R_C(R4,1)
            R4=ZERO
            CALL WRITE_R_C(R4,1)
            R4=ZERO
            CALL WRITE_R_C(R4,1)
            R4=ZERO
            CALL WRITE_R_C(R4,1)
            R4=EM10
            CALL WRITE_R_C(R4,1)
            R4=ZERO
            CALL WRITE_R_C(R4,1)
            R4=ZERO
            CALL WRITE_R_C(R4,1)
            R4=ZERO
            CALL WRITE_R_C(R4,1)
            R4=EM10
            CALL WRITE_R_C(R4,1)
            NBID1=NUMNODG+NODCUT+NSECT+NRWALL+NNWL
     .            +NNSRG+NNSMD+NNSPHG+2*NUMELS16G+NFVNOD+NFNOD_PXFEMG
     .            +NFNOD_CRKXFEMG+1
            NBID2=NBID1+1
            NBID3=NBID2+1
         ENDIF
      ENDIF
C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      CALL PARSORC(X  ,D    ,XNORM,IAD  ,CDG   ,
     .            BUFEL,IPARG,IXQ  ,IXC   ,IXTG ,
     .            ELBUF_TAB,INVERT,EL2FA,IADG ,
     .            MATER,IPARTQ,IPARTC,IPARTUR,IPARTTG,
     .            NODGLOB)
C

C pour la version spmd il faut mettre a jour XNORM

      IF (NSPMD>1) THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_N(XNORM,IAD_ELEM,FR_ELEM,LENR)
      ENDIF

         BUF = SECTIOM*4

      IF(NCUTS>0)CALL PARCUT(ICBUF,NELCUT)

      IF(NSECT+NRWALL>0) CALL DPARRWS(
     1            NESBW2,NSTRF, IXC  ,
     2            IXTG ,X    ,NODCUT,RWBUF,NPRW,
     3            NODGLOB,BUF,IXS)

      IF (NSURG>0) CALL DPARSRG(NSURG,NNWL,NODCUT)

C ply xfem
      IF (ANIM_PLY > 0) THEN
         PLYNUMC = 0
         DO I=1,NPLYMAX
            PLYNUMC=PLYNUMC+PLYSHELL(I)%PLYNUMSHELL
         ENDDO
         CALL PARSOR_PLY(NEL_PLY,  X,       D,       XNORM,  CDG,
     .                   IPARG,    IXC,     IXTG,    INVERT, EL2FA_PLY,
     .                   MATER,    IPARTC,  NODGLOB, IDPLY,  IAD_PLY,
     .                   IAD_PLYG, PLYNUMC, NBF_PXFEMG )
      ENDIF
c--------------------------------------------------------------------      
c     xfem crack for layered shells
      IF (ANIM_CRK > 0) THEN    ! part xfem
        CALL PARSOR_CRK(
     .       IPARG  ,IXC     ,IXTG       ,EL2FA_CRK   ,IDCRK   ,
     .       IAD_CRK,IAD_CRKG,NBF_CRKXFEM,NBF_CRKXFEMG,IEL_CRK ,
     .       NODGLOBXFE,INDX_CRK,ITAB    )
      ENDIF
c--------------------------------------------------------------------      
C traitement mono equivalent
      IF (NSPMD == 1) THEN
         II=0
         IF (IFVANI==1) THEN
            ELOFF=0
            DO I=1,NFVBAG
               ALLOCATE(ITAGT(FVDATA(I)%NNTR))
               DO J=1,FVDATA(I)%NNTR
                  ITAGT(J)=0
               ENDDO
C
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                      FVDATA(I)%IFVPADR_ANIM(J+1)-1
                     KK=FVDATA(I)%IFVPOLH_ANIM(K)
                     DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                         FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY_ANIM(N)
                        IF (ITAGT(NN)==1) CYCLE
                        INOD(1)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(1,NN)-1
                        INOD(2)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(2,NN)-1
                        INOD(3)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(3,NN)-1
                        INOD(4)=INOD(3)
                        II=II+1
C Nombre de noeuds distincts de la coque (apres fusion dans FVMESH)
                        NND=1
                        IF (INOD(2)/=INOD(1)) NND=NND+1
                        IF (INOD(3)/=INOD(1).AND.
     .                      INOD(3)/=INOD(2)) NND=NND+1
                        IF (NND/=3) THEN
                           INOD(1)=NBID1-1
                           INOD(2)=NBID2-1
                           INOD(3)=NBID3-1
                           INOD(4)=INOD(3)
                        ENDIF
C
                        CALL WRITE_I_C(INOD,4)
                        ITAGT(NN)=1
                        FVEL2FA(ELOFF+NN)=II
                        FVINUM(II)=ELOFF+NN
                     ENDDO
                  ENDDO
               ENDDO
               ELOFF=ELOFF+FVDATA(I)%NNTR
               DEALLOCATE(ITAGT)
            ENDDO
         ENDIF
      ELSE
         IF (IFVANI==1)
     .      CALL SPMD_FVB_ATR(NBID1, NBID2, NBID3, FVEL2FA, FVINUM,
     .                        FVOFF)
      ENDIF
C-----------------------------------------------
C   OFF
C-----------------------------------------------
      CALL ANIOFFC(ELBUF_TAB,IPARG,WAFT ,EL2FA,NBF  ,
     .             IAD  ,NBF_L,NBPART,IADG,NODGLOB ,
     .             IPART,IPARTC,IPARTTG)

      IF (ISPMD==0) THEN
            DO J=1,NESBW2+NELCUT
              CALL WRITE_C_C(1,1)
            ENDDO
      ENDIF
Cplyxfem
      NEL_PLY = 0
      IF(ANIM_PLY > 0) THEN
           CALL ANIOFFC_PLY( IPLY,     NEL_PLY,   ELBUF_TAB,  IPARG,
     .                       WAFT_PLY, EL2FA_PLY, NBF_PXFEM, IAD_PLY,
     .                       PLYNUMC,  NBPART,    IAD_PLYG,  NODGLOB,
     .                       IPART,    IPARTC,    IPARTTG,   NBF_PXFEMG,
     .                       IPM,      IGEO,      IXC     , STACK )
      ENDIF
cplyxfem
C
      IF (ANIM_CRK > 0) THEN    
        CALL ANIOFFC_CRK(
     .     XFEM_TAB ,IPARG     ,IPART       ,IPARTC     ,IPARTTG ,
     .     WAFT_CRK ,EL2FA_CRK ,NBF_CRKXFEMG,NBF_CRKXFEM,IAD_CRKG,
     .     IEL_CRK  ,INDX_CRK)
      ENDIF 
C
      IF (NSPMD == 1) THEN
         IF (IFVANI==1) THEN
            ALLOCATE(OFFTR(NFVTR))
            DO I=1,NFVTR
               OFFTR(I)=0
            ENDDO
            ELOFF=0
            DO I=1,NFVBAG
               DO J=1,FVDATA(I)%NPOLH
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO N=FVDATA(I)%IFVTADR(KK),
     .                         FVDATA(I)%IFVTADR(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY(N)
                        IF (NN>0) THEN
                           N1=FVDATA(I)%IFVTRI_ANIM(1,NN)
                           N2=FVDATA(I)%IFVTRI_ANIM(2,NN)
                           N3=FVDATA(I)%IFVTRI_ANIM(3,NN)
                           NND=1
                           IF (N2/=N1) NND=NND+1
                           IF (N3/=N2.AND.N3/=N1) NND=NND+1
C
                           NN=FVEL2FA(ELOFF+NN)
                           IF (NND==3) OFFTR(NN)=1
                        ENDIF
                     ENDDO
                  ENDDO
               ENDDO
               ELOFF=ELOFF+FVDATA(I)%NNTR
            ENDDO
C
            CALL WRITE_C_C(OFFTR,NFVTR)
            DEALLOCATE(OFFTR)
         ENDIF
      ELSE
         IF (IFVANI==1) CALL SPMD_FVB_AOFF(FVEL2FA)
      ENDIF
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
        IF (ISPMD==0) THEN
          DO I = 1, NBPART
            BUFFERP(I) = 0
            DO K = 1, NSPMD
              BUFFERP(I) = BUFFERP(I) + IADG(K,I)
            ENDDO
          ENDDO
          CALL WRITE_I_C(BUFFERP,NBPART)
        ENDIF
      IF (ISPMD==0) THEN
       IF(NCUTS>0)THEN
        CALL WRITE_I_C(ICBUF(MIC4),NCUTS)
       ENDIF
      ENDIF

C  a faire sur tous les procs en spmd

       NESCT1=0
       DO ISECT=1,NSECT
        CALL DONESEC(ISECT,NESCT1,NSTRF,IXS)

        IF (ISPMD==0) THEN
         CALL WRITE_I_C(NELCUT+NBF+NESCT1,1)
        endif
       END DO
       IF (NFVPART>0) THEN
          IF (ISPMD==0) ALLOCATE(FVPBUF(NFVPART))
          NESMD1=0
          IF(NSPMD > 1)
     .     CALL SPMD_FVB_APAR(NELCUT, NBF,    NESCT, NERWL, NESRG,
     .                       NESMD1, FVPBUF)
       ENDIF
C
      IF (ISPMD==0) THEN
       NERWL1=0
       DO IRWL=1,NRWALL
        CALL DONERWL(IRWL,NERWL1,NPRW)
        CALL WRITE_I_C(NELCUT+NBF+NESCT+NERWL1,1)
       END DO
       NESRG1=0

       DO ISRG=1,NSURG
        CALL DONESRG(ISRG,NESRG1)
        CALL WRITE_I_C(NELCUT+NBF+NESCT+NERWL+NESRG1,1)
       END DO
       NESMD1=0
C
       IF(ANIM_PLY > 0 )THEN
         IF (NSPMD==1)THEN
           DO I = 1,NPLYPART
             IPLY = INDX_PLY(I) 
             IAD_PLY(IPLY) = IAD_PLY(IPLY)
     .                   +  NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
             CALL WRITE_I_C(IAD_PLY(IPLY)  ,1)
           ENDDO
cc         CALL WRITE_I_C(IAD_PLY  ,NPLYPART)
         ELSE
            DO I = 1,NPLYPART
                IPLY = INDX_PLY(I)
                IADPC=0
                DO P=1,NSPMD
                   IADPC = IADPC + IAD_PLYG(P,I)
                ENDDO
                IADPC = IADPC
     *              +  NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
                CALL WRITE_I_C(IADPC ,1)
            ENDDO
         ENDIF
         DEALLOCATE(IAD_PLY)
       ENDIF
c------------------------------------------------------------------
C     xfem crack for layered shells
c------------------------------------------------------------------
      IF (ANIM_CRK > 0) THEN    ! adresses de parts xfem par layer
        IF(NSPMD==1)THEN
          DO ILAY = 1,NXLAYMAX
            ILEV = ILAY*NXEL
            ICRK = INDX_CRK(ILEV)
            IAD_LAY(ILAY) = IAD_LAY(ILAY) + IAD_CRK(ICRK)
          END DO
          DO ILAY=1,NCRKPARTW
            IAD_LAY(ILAY) = IAD_LAY(ILAY)
     .                    + NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
            CALL WRITE_I_C(IAD_LAY(ILAY),1)
          ENDDO
c---
        ELSE  ! NSPMD > 1
          DO ILAY = 1,NXLAYMAX
            ILEV = ILAY*NXEL
            ICRK = INDX_CRK(ILEV)
            DO P=1,NSPMD
              IAD_LAY(ILAY) = IAD_LAY(ILAY) + IAD_CRKG(P,ICRK) 
            ENDDO 
          END DO
          DO ILAY=1,NCRKPARTW
            IAD_LAY(ILAY) = IAD_LAY(ILAY)
     .                    + NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
            CALL WRITE_I_C(IAD_LAY(ILAY),1)
          ENDDO
        ENDIF
c---
      ENDIF
c------------------------------------------------------------------
c          DO I = 1,NCRKPART
c            ICRK = INDX_CRK(I)
c            IXEL = MOD(ICRK-1, NXEL) + 1
c            ILAY = (ICRK-IXEL)/NXEL  + 1
c            IAD_LAY(ILAY) = IAD_CRK(ICRK)
c                IAD_LAY(ILAY) = IAD_LAY(ILAY) + IAD_CRK(ICRK)
c          DO ILAY=1,NCRKPARTW
c            IAD_LAY(ILAY) = IAD_LAY(ILAY)
c     .                    + NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
c            CALL WRITE_I_C(IAD_LAY(ILAY),1)
c
c          DO I = 1,NCRKPART
c            ICRK = INDX_CRK(I)
c            IXEL = MOD(ICRK-1, NXEL) + 1
c            ILAY = (ICRK-IXEL)/NXEL  + 1
c              DO P=1,NSPMD
c                IAD_LAY(ILAY) = IAD_LAY(ILAY) + IAD_CRKG(P,ICRK) 
c          DO ILAY=1,NCRKPARTW
c            IAD_LAY(ILAY) = IAD_LAY(ILAY)
c     .                    + NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
c            CALL WRITE_I_C(IAD_LAY(ILAY),1)
c------------------------------------------------------------------
C
        IF (NSPMD == 1 ) THEN
           IF (IFVANI==1) THEN
              FVIAD=NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
              DO I=1,NFVBAG
                 ALLOCATE(ITAGT(FVDATA(I)%NNTR))
                 DO J=1,FVDATA(I)%NNTR
                    ITAGT(J)=0
                 ENDDO
ccC
                 DO J=1,FVDATA(I)%NPOLH_ANIM
                    DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                          FVDATA(I)%IFVPADR_ANIM(J+1)-1
                       KK=FVDATA(I)%IFVPOLH_ANIM(K)
                       DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                            FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                          NN=FVDATA(I)%IFVPOLY_ANIM(N)
                          IF (ITAGT(NN)==0) THEN
                             FVIAD=FVIAD+1
                             ITAGT(NN)=1
                          ENDIF
                       ENDDO
                    ENDDO
                    CALL WRITE_I_C(FVIAD,1)
                 ENDDO
                  DEALLOCATE(ITAGT)
              ENDDO
             DEALLOCATE(FVPBUF)
           ENDIF
        ELSE
           IF (IFVANI==1.AND.NFVPART>0) THEN
              DO I=1,NFVPART
                 FVIAD=FVPBUF(I)
                 CALL WRITE_I_C(FVIAD,1)
              ENDDO
              DEALLOCATE(FVPBUF)
           ENDIF
        ENDIF
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
       MAXPART = 0
       DO I=1,NPART
        IF(MATER(I)/=0)THEN
          WRITE(STR,'(I9,A1)')IPART(4,I),':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10
          CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),40)
          DO J=1,LTITL
            IF(TITL(J:J)/=' ') IB = J+10
            CTEXT(J+10)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        ENDIF
          MAXPART = MAX(MAXPART,IPART(4,I))
       ENDDO
C-----------------------------------------------
C     CUTS PART
C-----------------------------------------------
       IF(NCUTS>0)THEN
        DO I=1,NCUTS
          WRITE(STR,'(9H     CUT:)')
          DO J=1,9
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB=9
          DO J=1,40
            IAUX = ICUT(44*(I-1)+J)
            CAUX(1:1) = CHAR(IAUX)
            IF(CAUX(1:1)/=' ') IB = J+9
            CTEXT(J+9)=ICUT(44*(I-1)+4+J)
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        ENDDO
       ENDIF
       IF (INVSTR<40) THEN
        DO ISECT=1,NSECT
         WRITE(STR,'(I9,A2,A7)') ISECT,': ','Section'
          DO J=1,18
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 18
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
       ELSE
        DO ISECT=1,NSECT
          WRITE(STR,'(I9,A2)') NOM_OPT(I16G+LNOPT1*(ISECT-1)),': '
          DO J=1,11
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          CALL FRETITL2(TITL,NOM_OPT(I16G+LNOPT1*(ISECT-1)
     &                                   +LNOPT1-LTITR),40)
          IB = LTITL+10
          DO J=1,LTITL
            CTEXT(J+11)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
       END IF
       IF (INVSTR<40) THEN
        DO IRWL=1,NRWALL
          WRITE(STR,'(I9,A2,A10)') IRWL,': ','Rigid Wall'
          DO J=1,21
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 21
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
       ELSE
        DO IRWL=1,NRWALL
          WRITE(STR,'(I9,A2)') NOM_OPT(I16D+LNOPT1*(IRWL-1)),': '
          DO J=1,11
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = LTITL+10
          CALL FRETITL2(TITL,NOM_OPT(I16D+LNOPT1*(IRWL-1)
     &                                   +LNOPT1-LTITR),40)
          DO J=1,LTITL
            CTEXT(J+11)=ICHAR(TITL(J:J))
          END DO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
       ENDIF
C
       ISRG=1
       DO ISRF=1,NSURF
         IF (IGRSURF(ISRF)%TYPE==101) THEN
C         RADIOSS'S ellipsoid.
          WRITE(STR,'(I9,A1)') ISRG,':'
          DO J=1,10
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB=10
          TITL = IGRSURF(ISRF)%TITLE
          DO J=1,LTITL
            IF(TITL(J:J)/=' ') IB = J+10
            CTEXT(J+10)=ICHAR(TITL(J:J))
          END DO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
          ISRG=ISRG+1
         END IF
      END DO

      ENDIF
C
      MAXPART = MAXPART  + NSECT + NRWALL + NSURF + NCUTS
      IF(ANIM_PLY > 0 ) THEN
        IF (ISPMD==0)THEN
         DO I=1,NPLYPART
           WRITE(STR,'(I8,A15)') IDPID_PLY(I),': PLY COMPOSITE'
          DO J=1,24
              CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB=24
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
         END DO
         MAXPART = MAXPART + NPLYPART
        ENDIF
      ENDIF
C xfem crack for layered shells +++
      IF(ANIM_CRK > 0) THEN
        IF (ISPMD==0)THEN
         DO I=1,NCRKPARTW
           ILAY = I
           WRITE(STR,'(I9,A1)') MAXPART + I,':'
           DO K=1,10
             CTEXT(K)=ICHAR(STR(K:K))
           ENDDO
           TITL=' '
           WRITE(TITL,'(A20,I9)') 'CRACKED SHELL LAYER ',ILAY
           DO K=1,LTITL
             CTEXT(K+10)=ICHAR(TITL(K:K))
           ENDDO
           CTEXT(40)=0
           CALL WRITE_C_C(CTEXT,10+LTITL)
         END DO
         MAXPART = MAXPART + NCRKPARTW
        ENDIF
      ENDIF
C xfem crack for layered shells ---
      IF (NSPMD == 1) THEN
         IF (IFVANI==1) THEN
            DO I=1,NFVBAG
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  WRITE(STR,'(I9,A1)') MAXPART + J,':'
                  DO K=1,10
                     CTEXT(K)=ICHAR(STR(K:K))
                  ENDDO
                  TITL=' '
                  WRITE(TITL,'(A11,I9)') 'POLYHEDRON ',J
                  DO K=1,LTITL
                     CTEXT(K+10)=ICHAR(TITL(K:K))
                  ENDDO
                  CTEXT(31)=0
                  CALL WRITE_C_C(CTEXT,10+LTITL)
               ENDDO
            ENDDO
         ENDIF
      ELSE
         IF (IFVANI==1) CALL SPMD_FVB_ATIT(CTEXT, STR, TITL, LTITL,
     *                                       MAXPART    )
      ENDIF
C
C-----------------------------------------------
C   NORMAL
C-----------------------------------------------
      CALL XYZNOR(XNORM,NODGLOB,WEIGHT)

      IF(NODCUT>0)THEN
        CALL NORCUT(XCUT,ICBUF(MIC5))
      ENDIF
      CALL DSECNOR(X    ,RWBUF,NPRW)
      IF (NSURG>0) CALL DSRGNOR(IGRSURF,BUFSF)

         IF (ISPMD==0) THEN
              SNNSPHG= NNSPHG
         ELSE
              SNNSPHG= NNSPH
         ENDIF

      IF (ISPH3D*(NUMSPH_T+MAXPJET)>0)
     .   CALL DSPHNOR(KXSP,X,SPBUF,NNSPHG)
        IF (ISPMD==0.AND.NUMELS16G>0)
     .   CALL XYZNOR16(IXS,IXS10,IXS20,IXS16,X)

      IF(ANIM_PLY > 0) THEN
        EMPSIZPL=0
        DO I = 1,NPLYPART
           IPLY = INDX_PLY(I)
           CALL XYZNOR_PLY(IPLY,XNORM,NODGLOB,WEIGHT,EMPSIZPL)
        ENDDO
      ENDIF
C
      IF (ANIM_CRK > 0) THEN
        DO I = 1,NCRKPART
          ICRK = INDX_CRK(I)
          CALL XYZNOR_CRK(ICRK,XNORM,NFNOD_CRKXFEMG)
        ENDDO
      END IF
C
      IF (ISPMD==0) THEN
         IF (IFVANI==1) THEN
            DO I=1,NFVNOD
               INORM(1) = 0
               INORM(2) = 0
               INORM(3) = 0
               CALL WRITE_S_C(INORM,3)
            ENDDO
            IF (NFVNOD>0) THEN
               DO I=1,3
                  INORM(1) = 0
                  INORM(2) = 0
                  INORM(3) = 0
                  CALL WRITE_S_C(INORM,3)
               ENDDO
            ENDIF
         ENDIF
      ENDIF
C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M==1.OR.ANIM_CE(3)==1.OR.
     .   ANIM_CE(25)==1)THEN
         CALL DMASANIC(ELBUF_TAB,X    ,D    ,GEO  ,IPARG,
     .               IXQ  ,IXC  ,IXTG ,MAS  ,PM   ,
     .               EL2FA,NBF  ,IGEO , STACK )
      ENDIF
C-----------------------------------------------
C   N FUNC TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
       CTEXT(81)=0
       IF(ANIM_N(01)==1) CALL ANI_TXT('Time Step',9)
       IF(ANIM_N(02)==1) CALL ANI_TXT('Mass Change',11)
       IF(ANIM_N(03)==1) CALL ANI_TXT('Nodal Pressure',14)
       IF(ANIM_N(04)==1) CALL ANI_TXT('Nodal Density',13)
       IF(ANIM_N(05)==1) CALL ANI_TXT('Nodal Specific Energy',21)
       IF(ANIM_N(06)==1) CALL ANI_TXT('Nodal Temperature',17)
       IF(ANIM_N(07)==1) CALL ANI_TXT('Nodal Variable 1',16)
       IF(ANIM_N(08)==1) CALL ANI_TXT('Nodal Variable 2',16)
       IF(ANIM_N(09)==1) CALL ANI_TXT('Nodal Variable 3',16)
       IF(ANIM_N(10)==1) CALL ANI_TXT('Nodal Variable 4',16)
       IF(ANIM_N(11)==1) CALL ANI_TXT('Nodal Variable 5',16)
       IF(ANIM_N(12)==1) CALL ANI_TXT('Inertia Change',14)
       IF(ANIM_N(13)==1) CALL ANI_TXT('Nodal Potential',15)
       IF(ANIM_N(14)==1) CALL ANI_TXT('Non Diagonal Mass Change',24)
       IF(ANIM_N(15)==1) CALL ANI_TXT('%damage(type2 interface) / Normal',33)          
       IF(ANIM_N(16)==1) CALL ANI_TXT('%damage(type2 interface) / Tangent',34)
       IF(ANIM_N(17)==1) CALL ANI_TXT('Nodal Schlieren',15)
       IF(ANIM_N(18)==1) CALL ANI_TXT('Nodal Rotational Stiffness',26)
       IF(ANIM_N(19)==1) CALL ANI_TXT('Nodal Stiffness',15)
       IF(ANIM_N(20)==1) CALL ANI_TXT('Nodal Volumetric Fraction - 1',29)       
       IF(ANIM_N(21)==1) CALL ANI_TXT('Nodal Volumetric Fraction - 2',29)       
       IF(ANIM_N(22)==1) CALL ANI_TXT('Nodal Volumetric Fraction - 3',29)       
       IF(ANIM_N(23)==1) CALL ANI_TXT('Nodal Volumetric Fraction - 4',29)    
       IF(ANIM_N(24)==1) CALL ANI_TXT('Centroid Volumetric Fraction - 1',32)   !inter22     
       IF(ANIM_N(25)==1) CALL ANI_TXT('Centroid Volumetric Fraction - 2',32)   !inter22        
       IF(ANIM_N(26)==1) CALL ANI_TXT('Centroid Volumetric Fraction - 3',32)   !inter22        
       IF(ANIM_N(27)==1) CALL ANI_TXT('Centroid Volumetric Fraction - 4',32)   !inter22                                       
       IF(ANIM_N(28)==1) CALL ANI_TXT('Centroid New Volume',19)                !inter22                                             
       IF(ANIM_N(29)==1) CALL ANI_TXT('Centroid Old Volume',19)                !inter22          
       IF(ANIM_N(30)==1) CALL ANI_TXT('Nodal Sound Speed',17)                  !FVMBAG
C----------------------------
C    GPS TEXT
C----------------------------
       IADGPS = IAD_GPS
       IF(ANIM_N(IADGPS+1)==1) CALL ANI_TXT('GPS1 Pressure',13)
       IF(ANIM_N(IADGPS+2)==1) CALL ANI_TXT('GPS1 Von Mises',14)
       IF(ANIM_N(IADGPS+3)==1) CALL ANI_TXT('GPS1 SIGXX',10)
       IF(ANIM_N(IADGPS+4)==1) CALL ANI_TXT('GPS1 SIGYY',10)
       IF(ANIM_N(IADGPS+5)==1) CALL ANI_TXT('GPS1 SIGZZ',10)
       IF(ANIM_N(IADGPS+6)==1) CALL ANI_TXT('GPS1 SIGXY',10)
       IF(ANIM_N(IADGPS+7)==1) CALL ANI_TXT('GPS1 SIGZY',10)
       IF(ANIM_N(IADGPS+8)==1) CALL ANI_TXT('GPS1 SIGXZ',10)
       IF(ANIM_N(IADGPS+9)==1) CALL ANI_TXT('GPS1 SIGXX_U',12)
       IF(ANIM_N(IADGPS+10)==1) CALL ANI_TXT('GPS1 SIGYY_U',12)
       IF(ANIM_N(IADGPS+11)==1) CALL ANI_TXT('GPS1 SIGZZ_U',12)
       IF(ANIM_N(IADGPS+12)==1) CALL ANI_TXT('GPS1 SIGXY_U',12)
       IF(ANIM_N(IADGPS+13)==1) CALL ANI_TXT('GPS1 SIGZY_U',12)
       IF(ANIM_N(IADGPS+14)==1) CALL ANI_TXT('GPS1 SIGXZ_U',12)
       IF(ANIM_N(IADGPS+15)==1) CALL ANI_TXT('GPS1 SIGXX_L',12)
       IF(ANIM_N(IADGPS+16)==1) CALL ANI_TXT('GPS1 SIGYY_L',12)
       IF(ANIM_N(IADGPS+17)==1) CALL ANI_TXT('GPS1 SIGZZ_L',12)
       IF(ANIM_N(IADGPS+18)==1) CALL ANI_TXT('GPS1 SIGXY_L',12)
       IF(ANIM_N(IADGPS+19)==1) CALL ANI_TXT('GPS1 SIGZY_L',12)
       IF(ANIM_N(IADGPS+20)==1) CALL ANI_TXT('GPS1 SIGXZ_L',12)
       IADGPS = IAD_GPS+100
       IF(ANIM_N(IADGPS+1)==1) CALL ANI_TXT('GPS2 Pressure',13)
       IF(ANIM_N(IADGPS+2)==1) CALL ANI_TXT('GPS2 Von Mises',14)
       IF(ANIM_N(IADGPS+3)==1) CALL ANI_TXT('GPS2 SIGXX',10)
       IF(ANIM_N(IADGPS+4)==1) CALL ANI_TXT('GPS2 SIGYY',10)
       IF(ANIM_N(IADGPS+5)==1) CALL ANI_TXT('GPS2 SIGZZ',10)
       IF(ANIM_N(IADGPS+6)==1) CALL ANI_TXT('GPS2 SIGXY',10)
       IF(ANIM_N(IADGPS+7)==1) CALL ANI_TXT('GPS2 SIGZY',10)
       IF(ANIM_N(IADGPS+8)==1) CALL ANI_TXT('GPS2 SIGXZ',10)
       IF(ANIM_N(IADGPS+9)==1) CALL ANI_TXT('GPS2 SIGXX_U',12)
       IF(ANIM_N(IADGPS+10)==1) CALL ANI_TXT('GPS2 SIGYY_U',12)
       IF(ANIM_N(IADGPS+11)==1) CALL ANI_TXT('GPS2 SIGZZ_U',12)
       IF(ANIM_N(IADGPS+12)==1) CALL ANI_TXT('GPS2 SIGXY_U',12)
       IF(ANIM_N(IADGPS+13)==1) CALL ANI_TXT('GPS2 SIGZY_U',12)
       IF(ANIM_N(IADGPS+14)==1) CALL ANI_TXT('GPS2 SIGXZ_U',12)
       IF(ANIM_N(IADGPS+15)==1) CALL ANI_TXT('GPS2 SIGXX_L',12)
       IF(ANIM_N(IADGPS+16)==1) CALL ANI_TXT('GPS2 SIGYY_L',12)
       IF(ANIM_N(IADGPS+17)==1) CALL ANI_TXT('GPS2 SIGZZ_L',12)
       IF(ANIM_N(IADGPS+18)==1) CALL ANI_TXT('GPS2 SIGXY_L',12)
       IF(ANIM_N(IADGPS+19)==1) CALL ANI_TXT('GPS2 SIGZY_L',12)
       IF(ANIM_N(IADGPS+20)==1) CALL ANI_TXT('GPS2 SIGXZ_L',12)
c stress gps
       IADGPS = IAD_GPS+200
       IF(ANIM_N(IADGPS+1)==1) CALL ANI_TXT('GPS SIGXX',9)
       IF(ANIM_N(IADGPS+2)==1) CALL ANI_TXT('GPS SIGYY',9)
       IF(ANIM_N(IADGPS+3)==1) CALL ANI_TXT('GPS SIGZZ',9)
       IF(ANIM_N(IADGPS+4)==1) CALL ANI_TXT('GPS SIGXY',9)
       IF(ANIM_N(IADGPS+5)==1) CALL ANI_TXT('GPS SIGZY',9)
       IF(ANIM_N(IADGPS+6)==1) CALL ANI_TXT('GPS SIGXZ',9)
c stress isogeo
       IADISO = IAD_GPS+300
       IF(ANIM_N(IADISO+1)==1) CALL ANI_TXT('STRESS ISOGEO SIGXX',19)
       IF(ANIM_N(IADISO+2)==1) CALL ANI_TXT('STRESS ISOGEO SIGYY',19)
       IF(ANIM_N(IADISO+3)==1) CALL ANI_TXT('STRESS ISOGEO SIGZZ',19)
       IF(ANIM_N(IADISO+4)==1) CALL ANI_TXT('STRESS ISOGEO SIGXY',19)
       IF(ANIM_N(IADISO+5)==1) CALL ANI_TXT('STRESS ISOGEO SIGZY',19)
       IF(ANIM_N(IADISO+6)==1) CALL ANI_TXT('STRESS ISOGEO SIGXZ',19)
c strain gps
       IADGPS = IAD_GPS+400
       IF(ANIM_N(IADGPS+1)==1) CALL ANI_TXT('GPSTRAIN EPSXX',14)
       IF(ANIM_N(IADGPS+2)==1) CALL ANI_TXT('GPSTRAIN EPSYY',14)
       IF(ANIM_N(IADGPS+3)==1) CALL ANI_TXT('GPSTRAIN EPSZZ',14)
       IF(ANIM_N(IADGPS+4)==1) CALL ANI_TXT('GPSTRAIN EPSXY',14)
       IF(ANIM_N(IADGPS+5)==1) CALL ANI_TXT('GPSTRAIN EPSZY',14)
       IF(ANIM_N(IADGPS+6)==1) CALL ANI_TXT('GPSTRAIN EPSXZ',14)
C-----------------------------------------------
C   E(quad+shell+truss+..) FUNC TEXT
C-----------------------------------------------
       IF(NBF+NELCUT+NESBW2/=0)THEN
        IF(ANIM_CE(1)==1) CALL ANI_TXT('Plastic Strain',14)
        IF(ANIM_CE(2)==1) CALL ANI_TXT('Density',7)
        IF(ANIM_CE(3)==1) CALL ANI_TXT('Specific Energy',15)
        IF(ANIM_CE(4)==1) CALL ANI_TXT('Temperature',11)
        IF(ANIM_CE(5)==1) CALL ANI_TXT('Thickness',9)
        IF(ANIM_CE(6)==1) CALL ANI_TXT('Pressure',8)
        IF(ANIM_CE(7)==1) CALL ANI_TXT('Von Mises',9)
        IF(ANIM_CE(8)==1) CALL ANI_TXT('Turbulent Energy',16)
        IF(ANIM_CE(9)==1) CALL ANI_TXT('Turbulent Viscosity',19)
        IF(ANIM_CE(10)==1) CALL ANI_TXT('Vorticity-X',11)
        IF(ANIM_CE(11)==1) CALL ANI_TXT('Damage 1',8)
        IF(ANIM_CE(12)==1) CALL ANI_TXT('Damage 2',8)
        IF(ANIM_CE(13)==1) CALL ANI_TXT('Damage 3',8)
        IF(ANIM_CE(14)==1) CALL ANI_TXT('Stress X ',9)
        IF(ANIM_CE(15)==1) CALL ANI_TXT('Stress Y ',9)
        IF(ANIM_CE(16)==1) CALL ANI_TXT('Stress Z ',9)
        IF(ANIM_CE(17)==1) CALL ANI_TXT('Stress XY',9)
        IF(ANIM_CE(18)==1) CALL ANI_TXT('Stress YZ',9)
        IF(ANIM_CE(19)==1) CALL ANI_TXT('Stress ZX',9)
        IF(ANIM_CE(20)==1) CALL ANI_TXT('User Var 1',10)
        IF(ANIM_CE(21)==1) CALL ANI_TXT('User Var 2',10)
        IF(ANIM_CE(22)==1) CALL ANI_TXT('User Var 3',10)
        IF(ANIM_CE(23)==1) CALL ANI_TXT('User Var 4',10)
        IF(ANIM_CE(24)==1) CALL ANI_TXT('User Var 5',10)
        IF(ANIM_CE(25)==1) CALL ANI_TXT('Hourglass Energy per unit mass',30)
        IF(ANIM_CE(26)==1) CALL ANI_TXT('Strain Rate',11)
        IF(ANIM_CE(27)==1) CALL ANI_TXT('User Var 6',10)
        IF(ANIM_CE(28)==1) CALL ANI_TXT('User Var 7',10)
        IF(ANIM_CE(29)==1) CALL ANI_TXT('User Var 8',10)
        IF(ANIM_CE(30)==1) CALL ANI_TXT('User Var 9',10)
        IF(ANIM_CE(31)==1) CALL ANI_TXT('User Var 10',11)
        IF(ANIM_CE(32)==1) CALL ANI_TXT('User Var 11',11)
        IF(ANIM_CE(33)==1) CALL ANI_TXT('User Var 12',11)
        IF(ANIM_CE(34)==1) CALL ANI_TXT('User Var 13',11)
        IF(ANIM_CE(35)==1) CALL ANI_TXT('User Var 14',11)
        IF(ANIM_CE(36)==1) CALL ANI_TXT('User Var 15',11)
        IF(ANIM_CE(37)==1) CALL ANI_TXT('User Var 16',11)
        IF(ANIM_CE(38)==1) CALL ANI_TXT('User Var 17',11)
        IF(ANIM_CE(39)==1) CALL ANI_TXT('User Var 18',11)
         DO I=40,2039
           IF(ANIM_CE(I)==1)THEN
            II = (I - 39)/100 + 1
            IUS = MOD ((I - 39), 100)
            IF(IUS==0)THEN
              IUS = 100
              II = II -1
            ENDIF
            WRITE(MES,'(A,I2,A,I3,A)')
     .       'User Var',II,'(Layer',IUS,')'
           CALL ANI_TXT(MES,20)
           ENDIF
         ENDDO
        IF(ANIM_CE(2040)==1) CALL ANI_TXT('Plastic Strain Upper',20)
        IF(ANIM_CE(2041)==1) CALL ANI_TXT('Plastic Strain Lower',20)
        DO I=2042,2141
           IF(ANIM_CE(I)==1)THEN
             IUS = MOD ((I - 2041), 100)
             IF(IUS==0)IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Plast Strn Layer ',IUS, ' '
               CALL ANI_TXT(MES,21)
            END IF
        END DO
        IF(ANIM_CE(2142)==1) CALL ANI_TXT('Nb of Failed layers',19)
        IF(ANIM_CE(2143)==1) CALL ANI_TXT('Airbag crossing mass',20)
        IF(ANIM_CE(2144)==1)
     .     CALL ANI_TXT('Airbag crossing velocity',24)
        IF(ANIM_CE(2145)==1) CALL ANI_TXT('FVMBAG - Mass',13)
        IF(ANIM_CE(2146)==1) CALL ANI_TXT('FVMBAG - Pressure',17)
        IF(ANIM_CE(2147)==1)
     .     CALL ANI_TXT('FVMBAG - Fluid velocity X',25)
        IF(ANIM_CE(2148)==1)
     .     CALL ANI_TXT('FVMBAG - Fluid velocity Y',25)
        IF(ANIM_CE(2149)==1)
     .     CALL ANI_TXT('FVMBAG - Fluid velocity Z',25)
        IF(ANIM_CE(2150)==1) CALL ANI_TXT('FVMBAG - Density',16)
        IF(ANIM_CE(2151)==1)
     .     CALL ANI_TXT('FVMBAG - Specific Energy',24)
        IF(ANIM_CE(2152)==1) CALL ANI_TXT('FVMBAG - Sound Speed',20)
        IF(ANIM_CE(2153)==1) CALL ANI_TXT('FVMBAG - Gama',13)
        IF(ANIM_CE(2154)==1)
     .     CALL ANI_TXT('FVMBAG - Visu Polyhedra',23)
        IF(ANIM_CE(2155)==1) CALL ANI_TXT('Thinning Percentage',19)
        IF(ANIM_CE(2156)==1)
     .    CALL ANI_TXT('Estimated Error on Thickness',28)
         DO I=2240,10139
           IF(ANIM_CE(I)==1)THEN
            II = (I - 2239)/100 + 21
            IUS = MOD ((I - 2239), 100)
            IF(IUS==0)THEN
              IUS = 100
              II = II -1
            ENDIF
            WRITE(MES,'(A,I2,A,I3,A)')
     .       'User Var',II,'(Layer',IUS,')'
           CALL ANI_TXT(MES,20)
           ENDIF
         ENDDO
C
        DO I=10140,10239
           IF(ANIM_CE(I)==1)THEN
            II = I - 10139
            WRITE(MES,'(A,I3,A)')
     .       'PHI,(Layer ',II,')'
            CALL ANI_TXT(MES,16)
           ENDIF
        ENDDO
        IF(ANIM_CE(10240)==1) CALL  ANI_TXT('INTER PLY - MIN-DAMAGE',22)     
        IF(ANIM_CE(10241)==1) CALL  ANI_TXT('INTER PLY -SIGZZ',16) 
        IF(ANIM_CE(10242)==1) CALL  ANI_TXT('INTER PLY -SIGYZ',16) 
        IF(ANIM_CE(10243)==1) CALL  ANI_TXT('INTER PLY -SIGXZ',16)    
        IF(ANIM_CE(10244)==1) CALL  ANI_TXT('INTER PLY -EPSZZ',16) 
        IF(ANIM_CE(10245)==1) CALL  ANI_TXT('INTER PLY -EPSYZ',16) 
        IF(ANIM_CE(10246)==1) CALL  ANI_TXT('INTER PLY -EPSXZ',16) 
        IF(ANIM_CE(10247)==1) CALL  ANI_TXT('INTER PLY -EINT',15)
        IF(ANIM_CE(10248)==1) CALL ANI_TXT('Volumetric Fraction 1',21)
        IF(ANIM_CE(10249)==1) CALL ANI_TXT('Volumetric Fraction 2',21)
        IF(ANIM_CE(10250)==1) CALL ANI_TXT('Volumetric Fraction 3',21)
        IF(ANIM_CE(10251)==1) CALL ANI_TXT('Volumetric Fraction 4',21)
        IF(ANIM_CE(10252)==1) CALL ANI_TXT('Burn Fraction',13)
C-----------------------------------------------
C   ELEMENT FAILURE COEFFECIENT : NXT CRITRIA
C-----------------------------------------------
        IF(ANIM_CE(10253)==1) CALL  ANI_TXT('NXT FAILURE FACTOR',18)
        IF(ANIM_CE(10254)==1) CALL  ANI_TXT('SIGMA1/h',8)
        IF(ANIM_CE(10255)==1) CALL  ANI_TXT('SIGMA2/h',8)
C-----------------------------------------------
C   DAMAGE FACTOR : MAX of ALL CRITRIA BY LAYER
C-----------------------------------------------
        IF(ANIM_CE(10256)==1) CALL ANI_TXT('MAX DAMAGE ELEMENT',18)
        IF(ANIM_CE(10257)==1) CALL ANI_TXT('MAX DAMAGE UPPER',16)
        IF(ANIM_CE(10258)==1) CALL ANI_TXT('MAX DAMAGE LOWER',16)
        IF(ANIM_CE(10259)==1) CALL ANI_TXT('MAX DAMAGE MEMBRANE',19)
        DO I=10260,10359
           IF(ANIM_CE(I)==1)THEN
            II = I - 10259
            WRITE(MES,'(A,I3,A)')
     .       'DAMAGE,(Layer ',II,')'
            CALL ANI_TXT(MES,20)
           ENDIF
        ENDDO   
        IF(ANIM_CE(10360)==1) CALL ANI_TXT('NXT FAILURE FACTOR UPPER',24)
        IF(ANIM_CE(10361)==1) CALL ANI_TXT('NXT FAILURE FACTOR LOWER',24)
        IF(ANIM_CE(10362)==1) CALL ANI_TXT('NXT FAILURE FACTOR MEMBRANE',27)
        DO I=10363,10462
           IF(ANIM_CE(I)==1)THEN
            II = I - 10362
            WRITE(MES,'(A,I3,A)')
     .       'NXT FAILURE FACTOR,(Layer ',II,')'
            CALL ANI_TXT(MES,32)
           ENDIF
        ENDDO  
        IF(ANIM_CE(10463)==1) CALL ANI_TXT('SIGMA1/h UPPER',14)
        IF(ANIM_CE(10464)==1) CALL ANI_TXT('SIGMA1/h LOWER',14)
        IF(ANIM_CE(10465)==1) CALL ANI_TXT('SIGMA1/h MEMBRANE',17)
        DO I=10466,10565
           IF(ANIM_CE(I)==1)THEN
            II = I - 10465
            WRITE(MES,'(A,I3,A)')
     .       'SIGMA1/h,(Layer ',II,')'
            CALL ANI_TXT(MES,22)
           ENDIF
        ENDDO 
        IF(ANIM_CE(10566)==1) CALL ANI_TXT('SIGMA2/h UPPER',14)
        IF(ANIM_CE(10567)==1) CALL ANI_TXT('SIGMA2/h LOWER',14)
        IF(ANIM_CE(10568)==1) CALL ANI_TXT('SIGMA2/h MEMBRANE',17)
        DO I=10569,10668
           IF(ANIM_CE(I)==1)THEN
            II = I - 10568
            WRITE(MES,'(A,I3,A)')
     .       'SIGMA2/h,(Layer ',II,')'
            CALL ANI_TXT(MES,22)
           ENDIF
        ENDDO 
        IF(ANIM_CE(10669)==1)CALL ANI_TXT('INTER PLY - MAX-DAMAGE',22)
        IF(ANIM_CE(10670)==1)CALL ANI_TXT('TIME DELETION ELEMENT',21)
        IF(ANIM_CE(10671)==1)CALL ANI_TXT('Sound Speed',11)        
        IF(ANIM_CE(10672)==1)CALL ANI_TXT('Schlieren',9)    
        IF(ANIM_CE(10673)==1)CALL ANI_TXT('phi MEMBRANE',12)         
        IF(ANIM_CE(10674)==1)CALL ANI_TXT('phi UPPER',9)       
        IF(ANIM_CE(10675)==1)CALL ANI_TXT('phi LOWER',9)        
        IF(ANIM_CE(10676)==1)CALL ANI_TXT('Domain',6)                  
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
        IF(ANIM_CE(10677)==1)CALL ANI_TXT('Equiv stress',12)
C-----------------------------------------------
C   Plastic Strain output (all NPTT) for new PID51 composite shell
C-----------------------------------------------
C  upper for PID51
        DO I=10678,10777
           IF (ANIM_CE(I) == 1) THEN
             IUS = MOD ((I - 10677), 100)
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Plastic Strain Upper Layer',IUS, ' '
               CALL ANI_TXT(MES,30)
            END IF
        END DO
C  lower for PID51
        DO I=10778,10877
           IF (ANIM_CE(I) == 1) THEN
             IUS = MOD ((I - 10777), 100)
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Plastic Strain Lower Layer',IUS, ' '
               CALL ANI_TXT(MES,30)
            END IF
        END DO
C all NPTT for PID51
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            IF (ANIM_CE(IUS + 10877) == 1) THEN
              ILAY = I
              IPT  = J
              WRITE(MES,'(A,I4,I3,A)')
     .        'Plast Strn Layer/IPT ',ILAY,IPT, ' '
              CALL ANI_TXT(MES,29)
            END IF
          ENDDO
        ENDDO
C-----------------------------------------------
C   Artificial Viscosity
C-----------------------------------------------
        IF(ANIM_CE(11888)==1)CALL ANI_TXT('Artificial Viscosity',20)  !previous PID uses ANIM_CE(10678:11887)
        IF(ANIM_CE(11889)==1)CALL ANI_TXT('Detonation Time',15) 
        
        !multumaterial law 20 ouptus
        IF(ANIM_CE(11890)==1)CALL ANI_TXT('Density-1',9) 
        IF(ANIM_CE(11891)==1)CALL ANI_TXT('Density-2',9) 
        IF(ANIM_CE(11892)==1)CALL ANI_TXT('Density-3',9) 
        IF(ANIM_CE(11893)==1)CALL ANI_TXT('Density-4',9)                         

        IF(ANIM_CE(11894)==1)CALL ANI_TXT('Specific Energy-1',17)                         
        IF(ANIM_CE(11895)==1)CALL ANI_TXT('Specific Energy-2',17)                         
        IF(ANIM_CE(11896)==1)CALL ANI_TXT('Specific Energy-3',17)                         
        IF(ANIM_CE(11897)==1)CALL ANI_TXT('Specific Energy-4',17)                         

        IF(ANIM_CE(11898)==1)CALL ANI_TXT('Temperature-1',13)     
        IF(ANIM_CE(11899)==1)CALL ANI_TXT('Temperature-2',13)     
        IF(ANIM_CE(11900)==1)CALL ANI_TXT('Temperature-3',13)     
        IF(ANIM_CE(11901)==1)CALL ANI_TXT('Temperature-4',13)                             
        
        IF(ANIM_CE(11902)==1)CALL ANI_TXT('Pressure-1',10)                             
        IF(ANIM_CE(11903)==1)CALL ANI_TXT('Pressure-2',10)                             
        IF(ANIM_CE(11904)==1)CALL ANI_TXT('Pressure-3',10)                             
        IF(ANIM_CE(11905)==1)CALL ANI_TXT('Pressure-4',10)                                                     

        IF(ANIM_CE(11906)==1)CALL ANI_TXT('Plastic Strain-1',16) 
        IF(ANIM_CE(11907)==1)CALL ANI_TXT('Plastic Strain-2',16) 
        IF(ANIM_CE(11908)==1)CALL ANI_TXT('Plastic Strain-3',16) 
        IF(ANIM_CE(11909)==1)CALL ANI_TXT('Plastic Strain-4',16) 

        IF(ANIM_CE(11910)==1)CALL ANI_TXT('Sound Speed-1',13) 
        IF(ANIM_CE(11911)==1)CALL ANI_TXT('Sound Speed-2',13) 
        IF(ANIM_CE(11912)==1)CALL ANI_TXT('Sound Speed-3',13) 
        IF(ANIM_CE(11913)==1)CALL ANI_TXT('Sound Speed-4',13)                         
        
        IF(ANIM_CE(11914)==1)CALL ANI_TXT('Volume-1',8)                                 
        IF(ANIM_CE(11915)==1)CALL ANI_TXT('Volume-2',8)                                 
        IF(ANIM_CE(11916)==1)CALL ANI_TXT('Volume-3',8)                                 
        IF(ANIM_CE(11917)==1)CALL ANI_TXT('Volume-4',8)                                                                 

        IF(ANIM_CE(11918)==1)CALL ANI_TXT('Mass-1',6)                                 
        IF(ANIM_CE(11919)==1)CALL ANI_TXT('Mass-2',6)                                 
        IF(ANIM_CE(11920)==1)CALL ANI_TXT('Mass-3',6)                                 
        IF(ANIM_CE(11921)==1)CALL ANI_TXT('Mass-4',6)                   
        
        IF(ANIM_CE(11922)==1)CALL ANI_TXT('Artificial Viscosity-1',22) 
        IF(ANIM_CE(11923)==1)CALL ANI_TXT('Artificial Viscosity-2',22) 
        IF(ANIM_CE(11924)==1)CALL ANI_TXT('Artificial Viscosity-3',22)                         
        IF(ANIM_CE(11925)==1)CALL ANI_TXT('Artificial Viscosity-4',22)  


C
C /ANIM/SHELL/IDPLY/
C
        DO I=1,MX_PLY_ANIM 
          IF(ANIM_CE(11925+I) == 1) THEN
             WRITE(MES,'(A,I10)')
     .        'PLY_ID ',PLY_ANIM( 3 * (I - 1) + 1)
              CALL ANI_TXT(MES,17)
          ENDIF
        ENDDO
C
C /ANIM/SHELL/IDPLY/PHI
C
        DO I=1,MX_PLY_ANIM 
          IF(ANIM_CE( (11925+MX_PLY_ANIM) +I) == 1) THEN
            WRITE(MES,'(A,I10,A)')
     .       'PHI,(Ply_id ',PLY_ANIM_PHI( 3 * (I - 1) + 1),')'
              CALL ANI_TXT(MES,23)
          ENDIF
        ENDDO
C
C /ANIM/SHELL/IDPLY/EPSP
C
        DO I=1,MX_PLY_ANIM 
          IF(ANIM_CE( (11925+2*MX_PLY_ANIM) +I) == 1) THEN
            WRITE(MES,'(A,I10,A,I3)')
     .       'Plas Str PLY/IPT',PLY_ANIM_EPSP( 3 * (I - 1) + 1),
     .        ' ',PLY_ANIM_EPSP( 3 * (I - 1) + 3)
              CALL ANI_TXT(MES,30)
          ENDIF
        ENDDO
C
C /ANIM/SHELL/IDPLY/DAMA
C
        DO I=1,MX_PLY_ANIM 
          IF(ANIM_CE( (11925+3*MX_PLY_ANIM) +I) == 1) THEN
            WRITE(MES,'(A,I10,A,I3)')
     .       'DAMAGE PLY/IPT',PLY_ANIM_DAMA( 3 * (I - 1) + 1),
     .        ' ',PLY_ANIM_DAMA( 3 * (I - 1) + 3)
              CALL ANI_TXT(MES,28)
          ENDIF
        ENDDO
C
C /ANIM/SHELL/FLDF/
C
        IDX = 11925+4*MX_PLY_ANIM
        IF(ANIM_CE(IDX+1) == 1) CALL ANI_TXT('FLD FAILURE FACTOR UPPER',24)
        IF(ANIM_CE(IDX+2) == 1) CALL ANI_TXT('FLD FAILURE FACTOR LOWER',24)
        IF(ANIM_CE(IDX+3) == 1) CALL ANI_TXT('FLD FAILURE FACTOR MEMBRANE',27)
C
C /ANIM/SHELL/FLDZON/
C
        IDX = 11925+4*MX_PLY_ANIM+3
        IF(ANIM_CE(IDX+1) == 1) CALL ANI_TXT('FLD ZONE INDEX UPPER',20)
        IF(ANIM_CE(IDX+2) == 1) CALL ANI_TXT('FLD ZONE INDEX LOWER',20)
        IF(ANIM_CE(IDX+3) == 1) CALL ANI_TXT('FLD ZONE INDEX MEMBRANE',23)
        
C-----------------------------------------------
C   DAMAGE FACTOR : MAX of ALL CRITRIA BY LAYER AND BY IPT
C-----------------------------------------------
C  upper for PID_51, 52
        IDX = 11931+4*MX_PLY_ANIM
        DO I=IDX+1,IDX+100
          IF (ANIM_CE(I) == 1) THEN
            IUS = MOD ((I - IDX), 100)
            IF (IUS == 0) IUS = 100
            WRITE(MES,'(A,I3,A)')
     .      'MAX DAMAGE UPPER Layer',IUS, ' '
              CALL ANI_TXT(MES,26)
          ENDIF
        ENDDO
C  lower for PID_51, 52
        IDX = 12031+4*MX_PLY_ANIM
        DO I=IDX+1,IDX+100
          IF (ANIM_CE(I) == 1) THEN
            IUS = MOD ((I - IDX), 100)
            IF (IUS == 0) IUS = 100
            WRITE(MES,'(A,I3,A)')
     .      'MAX DAMAGE LOWER Layer',IUS, ' '
              CALL ANI_TXT(MES,26)
          ENDIF
        ENDDO
C  membrane for PID_51, 52
        IDX = 12131+4*MX_PLY_ANIM
        DO I=IDX+1,IDX+100
          IF (ANIM_CE(I) == 1) THEN
            IUS = MOD ((I - IDX), 100)
            IF (IUS == 0) IUS = 100
            WRITE(MES,'(A,I3,A)')
     .      'MAX DAMAGE MEMBRANE Layer',IUS, ' '
              CALL ANI_TXT(MES,29)
          ENDIF
        ENDDO
C all NPTT through all layers for PID_51, 52
        IDX = 12231+4*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            IF (ANIM_CE(IUS + IDX) == 1) THEN
              ILAY = I
              IPT  = J
              WRITE(MES,'(A,I4,I3,A)')
     .        'MAX DAMAGE Layer/IPT ',ILAY,IPT, ' '
              CALL ANI_TXT(MES,29)
            END IF
          ENDDO
        ENDDO
C
        !/ANIM/ELEM/DT
        IDX = 4*MX_PLY_ANIM 
        IF(ANIM_CE(IDX+13242)==1)CALL ANI_TXT('Element Time Step',17)
C
        !/ANIM/ELEM/AMS
        IDX = 4*MX_PLY_ANIM 
        IF(ANIM_CE(IDX+13242+1)==1)CALL ANI_TXT('AMS selection',13) 
C
        !/ANIM/ELEM/EINT
        IDX = 4*MX_PLY_ANIM 
        IF(ANIM_CE(IDX+13242+2)==1)CALL ANI_TXT('Internal Energy',15)
        !/ANIM/ELEM/WPLA
        IDX = 4*MX_PLY_ANIM 
        IF(ANIM_CE(IDX+13242+3)==1)CALL ANI_TXT('Plastic Work',12)
!!!
        IDX = 13245 + 4*MX_PLY_ANIM
        IF(ANIM_CE(IDX + 1)==1) CALL ANI_TXT('Plastic Work Upper',18)
        IF(ANIM_CE(IDX + 2)==1) CALL ANI_TXT('Plastic Work Lower',18)
        IDX = 13247 + 4*MX_PLY_ANIM
        DO I=1,100
           IF(ANIM_CE(IDX + I)==1)THEN
             IUS = I
             IF(IUS==0)IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Plast Work Layer ',IUS, ' '
               CALL ANI_TXT(MES,21)
            END IF
        END DO
C
C-----------------------------------------------
C   Plastic WORK output (all NPTT) for new PID51 composite shell
C-----------------------------------------------
C  upper for PID51
        IDX = 13347 + 4*MX_PLY_ANIM 
        DO I=1,100
           IF (ANIM_CE(IDX + I) == 1) THEN
             IUS = I
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Plastic Work Upper Layer',IUS, ' '
               CALL ANI_TXT(MES,28)
            END IF
        END DO
C  lower for PID51
        IDX = 13447 + 4*MX_PLY_ANIM 
        DO I=1,100
           IF (ANIM_CE(IDX + I) == 1) THEN
             IUS = I
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Plastic Work Lower Layer',IUS, ' '
               CALL ANI_TXT(MES,28)
            END IF
        END DO
C all NPTT for PID51
        IDX =   13547 + 4*MX_PLY_ANIM 
        DO I=1,100
          DO J=1,10
            IUS = 10*(I-1)+J
            IF (ANIM_CE(IDX + IUS) == 1) THEN
              ILAY = I
              IPT  = J
              WRITE(MES,'(A,I4,I3,A)')
     .        'Plast Work Layer/IPT ',ILAY,IPT, ' '
              CALL ANI_TXT(MES,29)
            END IF
          ENDDO
        ENDDO 
        
        !-----------------------------------------------
        !   Element status ( OFF value in buffer : 
        !   0.0:deleted  1.0:activated  0.to1.0 under failure <0:Standby
        !-----------------------------------------------
        IDX = 13547 + 4*MX_PLY_ANIM +1000 +1
        IF(ANIM_CE(IDX) == 1) CALL ANI_TXT('Element Status',14)
        
        IDX = 13547 + 4*MX_PLY_ANIM +1000 +2
        IF(ANIM_CE(IDX) == 1) CALL ANI_TXT('Mach Number',11)  
        
        IDX = 13547 + 4*MX_PLY_ANIM +1000 +3
        IF(ANIM_CE(IDX) == 1) CALL ANI_TXT('Color Function',14)  
        
        IDX = 13547 + 4*MX_PLY_ANIM +1000 +4
        IF(ANIM_CE(IDX)==1) CALL ANI_TXT('Damage (Mean value)',19)
        IF(ANIM_CE(IDX+1)==1) CALL ANI_TXT('Damage (Upper value)',20)
        IF(ANIM_CE(IDX+2)==1) CALL ANI_TXT('Damage (Lower value)',20)
        IF(ANIM_CE(IDX+3)==1) CALL ANI_TXT('Damage (Membrane value)',23)
        DO I=IDX+3+1,IDX+3+11
           IF(ANIM_CE(I)==1)THEN
            II = I - (IDX+3)
            WRITE(MES,'(A,I3,A)')
     .       'Damage (Thck. point ',II,' value)'
            CALL ANI_TXT(MES,30)
           ENDIF
        ENDDO 

        !---QUAD VOLUME
        IDX =  IDX+3+11
        IDX = IDX+1   ! => IDX = 4*MX_PLY_ANIM + 14566
        IF(ANIM_CE(IDX) == 1) THEN
          CALL ANI_TXT('Volume',6)  
        ENDIF

        !---NON LOCAL PLASTIC STRAIN 
        IDX = 14567 + 4*MX_PLY_ANIM 
        IF(ANIM_CE(IDX)   == 1) CALL ANI_TXT('Non-local plastic strain',24)
        IF(ANIM_CE(IDX+1) == 1) CALL ANI_TXT('Non-local plastic strain (Upper)',32)
        IF(ANIM_CE(IDX+2) == 1) CALL ANI_TXT('Non-local plastic strain (Lower)',32)
        DO I=IDX+2+1,IDX+2+11
          IF (ANIM_CE(I) == 1) THEN
            II = I - (IDX+2)
            WRITE(MES,'(A,I3,A)')
     .       'Nloc plast at point ',II,' '
            CALL ANI_TXT(MES,30)
          ENDIF
        ENDDO 

        !---NON LOCAL PLASTIC STRAIN RATE
        IDX = 14581 + 4*MX_PLY_ANIM 
        IF(ANIM_CE(IDX)   == 1) CALL ANI_TXT('Non-local plastic strain rate',29)
        IF(ANIM_CE(IDX+1) == 1) CALL ANI_TXT('Non-local plastic strain rate (Upper)',37)
        IF(ANIM_CE(IDX+2) == 1) CALL ANI_TXT('Non-local plastic strain rate (Lower)',37)
        DO I=IDX+2+1,IDX+2+11
          IF (ANIM_CE(I) == 1) THEN
            II = I - (IDX+2)
            WRITE(MES,'(A,I3,A)')
     .       'Nloc rate at point ',II,' '
            CALL ANI_TXT(MES,30)
          ENDIF
        ENDDO 
        
        !---NTILLOTSON EoS
        IDX = 14596 + 4*MX_PLY_ANIM
        IF(ANIM_CE(IDX) == 1) CALL ANI_TXT('Region identifier in p,v diagram',32)  
        

C----------------------------------------------------------------------
C   Tsai-Wu criterion output for composites
C----------------------------------------------------------------------
!/ANIM/ELEM/TSAIWU
        IDX = 14595 + 4*MX_PLY_ANIM 
        IF(ANIM_CE(IDX    )==1) CALL ANI_TXT('Tsai-Wu Criterion'  ,17)
        IF(ANIM_CE(IDX + 1)==1) CALL ANI_TXT('Tsai-Wu Crit. Upper',19)
        IF(ANIM_CE(IDX + 2)==1) CALL ANI_TXT('Tsai-Wu Crit. Lower',19)
        DO I=1,100
          IF(ANIM_CE(IDX + 2 + I)==1)THEN
            IUS = I
            IF(IUS==0) IUS = 100
            WRITE(MES,'(A,I3,A)')
     .       'Tsai-Wu Crit. Layer ',IUS, ' '
            CALL ANI_TXT(MES,24)
          END IF
        END DO
c
C  upper for PID51
        IDX = 14697 + 4*MX_PLY_ANIM
        DO I=1,100
           IF (ANIM_CE(IDX + I) == 1) THEN
             IUS = I
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Tsai-Wu Crit. Upper Layer',IUS, ' '
               CALL ANI_TXT(MES,29)
            END IF
        END DO
C  lower for PID51
        IDX = 14797 + 4*MX_PLY_ANIM
        DO I=1,100
           IF (ANIM_CE(IDX + I) == 1) THEN
             IUS = I
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')
     .       'Tsai-Wu Crit. Lower Layer',IUS, ' '
               CALL ANI_TXT(MES,29)
            END IF
        END DO
C all NPTT for PID51
        IDX = 14897 + 4*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*(I-1)+J
            IF (ANIM_CE(IDX + IUS) == 1) THEN
              ILAY = I
              IPT  = J
              WRITE(MES,'(A,I4,I3,A)')
     .        'Tsai-Wu Crit. Lay/IPT ',ILAY,IPT, ' '
              CALL ANI_TXT(MES,30)
            END IF
          ENDDO
        ENDDO 

        !---NEXT ANIM
        !IDX=IDX+1   ! => IDX = 4*MX_PLY_ANIM + 15898
  
       ENDIF !  (NBF+NELCUT+NESBW2/=0)
!
      ENDIF !  IF (ISPMD==0)
C-----------------------------------------------
C   NODAL FUNC
C-----------------------------------------------
      K = 0
      IAD_GP2=IAD_GPS+100
      IAD_GP3=IAD_GP2+100
      IAD_ISO=IAD_GP3+100
      IAD_GP4=IAD_ISO+100
      DO I = 1,MX_ANI
        IFUNC = I
        IF (I==(IAD_GPS+3).OR.I==(IAD_GPS+9).OR.I==(IAD_GPS+15)
     .      .OR.I==(IAD_GP2+3).OR.I==(IAD_GP2+9)
     .      .OR.I==(IAD_GP2+15) .OR.I==(IAD_GP3+1).OR.I==(IAD_ISO+1) )  
     .             IFIRST=0
        IF(ANIM_N(I)/=1) CYCLE
          DO N=1,NUMNOD
            WA4(N) = ZERO
          ENDDO
          IF(I<3.OR.I==12)THEN
            DO N=1,NUMNOD
             WA4(N)=ANIN(N+K)
            ENDDO
            K = K + NUMNOD
          ELSEIF(I>=3.AND.I<=11 .OR. I==30) THEN
           IF(I == 6 .AND. (ITHERM_FE  > 0  )) THEN
             DO N=1,NUMNOD
              WA4(N)=TEMP(N)
             ENDDO
           ELSE
             ! Nodal Pressure (from elems ; ANIM not compatible with fvmbag centroids)
             IF (I==3) THEN
               IF(N2D==0)CALL NODALP(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,NUMELS,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0)
               IF(N2D/=0)CALL NODALP(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXQ,NIXQ,NUMELQ,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0)
             ENDIF
             ! Nodal Density (from elems ; ANIM not compatible with fvmbag centroids)
             IF (I==4) THEN
               IF(N2D==0)CALL NODALD(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,NUMELS,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0) 
               IF(N2D/=0)CALL NODALD(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXQ,NIXQ,NUMELQ,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0) 
             ENDIF
             ! Nodal Temperature (from elems ; ANIM not compatible with fvmbag centroids)
             IF (I==6) THEN
               IF(N2D==0)CALL NODALT(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,NUMELS,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0)
               IF(N2D/=0)CALL NODALT(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXQ,NIXQ,NUMELQ,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0)
             ENDIF
             ! Nodal sound speed (from elems ; ANIM not compatible with fvmbag centroids)
             IF (I==30) THEN
               IF(N2D==0)CALL NODALSSP(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,NUMELS,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0, MULTI_FVM)
               IF(N2D/=0)CALL NODALSSP(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXQ,NIXQ,NUMELQ,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0, MULTI_FVM)
             ENDIF
             

             !Then Monitored Volumes (Pressure,Density,Temperature,Sound Speed)
             CALL ANIMBALE(I, WA4, MONVOL, VOLMON ,2)
           ENDIF

          !Nodal Potential (/BEM/FLOW)
          ELSEIF (I==13) THEN
            CALL NODALP(I, WA4, WA4_FVM, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,NUMELS,ITAB,NV46,MONVOL,VOLMON,
     .                               0, IS_WRITTEN_NODE, IS_WRITTEN_NODE_FVM, ISPMD, FVDATA, SWA4, 0)
     
C------------ams : diagonale mass change
          ELSEIF(I==14)THEN
            IF(IDTMINS==0)THEN
              DO N=1,NUMNOD
                WA4(N)=ZERO
              ENDDO
            ELSE
              DO N=1,NUMNOD
                WA4(N)=MAX(ZERO,DIAG_SMS(N)/MAX(EM20,MS(N))-ONE)
              ENDDO
            END IF
C------------dama type2 : % damage inter type2 with pena
          ELSEIF(I==15)THEN
            DO N=1,NUMNOD
             WA4(N)=PDAMA2(1,N)
            ENDDO
          ELSEIF(I==16)THEN
            DO N=1,NUMNOD
             WA4(N)=PDAMA2(2,N)
            ENDDO
          ELSEIF(I==17)THEN
             CALL NODAL_SCHLIEREN(WA4,X,IXS,IXQ,ITAB,IPARG,0,ELBUF_TAB,ALE_CONNECTIVITY)  
          ELSEIF(I==18)THEN
            IF(IRODDL/=0)THEN
              DO N=1,NUMNOD
                WA4(N)=STIFR(N)
              ENDDO
            ELSE
              DO N=1,NUMNOD
                WA4(N)=ZERO
              ENDDO
            ENDIF     
          ELSEIF(I==19)THEN
            DO N=1,NUMNOD
              WA4(N)=STIFN(N)
            ENDDO
          ELSEIF(I>=20 .AND. I<=23)THEN            
             !ALE Nodal Volumetric Fraction (multimaterial only)
             IF(N2D==0)CALL NODALVFRAC(I, WA4, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,ITAB,NV46)
             IF(N2D/=0)CALL NODALVFRAC(I, WA4, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXQ,NIXQ,ITAB,NV46)
          ELSEIF(I>=24 .AND. I<=27)THEN            
             !ALE Nodal Volumetric Fraction (multimaterial only)
             !IF(N2D==0)CALL NODALZVFRAC(I, WA4, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,ITAB,NV46) 
          ELSEIF(I==28.OR.I==29)THEN            
             !inter22 polyhedra volumes (new & old)
             IF(N2D==0)CALL NODALZVOL(I, WA4, IFLOW, RFLOW,IPARG,ELBUF_TAB,IXS,NIXS,ITAB,NV46)                                         
C------------gps1 part
          ELSEIF (I>IAD_GPS.AND.I<(IAD_GPS+3)) THEN
            DO N=1,NUMNOD
             ITAGPS(N) = 0
             WGPS(N) = ZERO
            ENDDO
                   J = I - IAD_GPS
            
            CALL DFUNGPS1(ELBUF_TAB ,WGPS    ,J       ,IPARG   ,GEO     ,
     .                    IXS       ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC       ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    ITAGPS    )

            ! --------------------------
            IF(NSPMD > 1)THEN
             LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
             CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
             CALL SPMD_EXCH_NODAREA(WGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
            ENDIF
            ! --------------------------

             DO N=1,NUMNOD
              IF (ITAGPS(N)>0) WA4(N)=WGPS(N)/ITAGPS(N)
             ENDDO
          ELSEIF (I>(IAD_GPS+2).AND.I<(IAD_GPS+9)) THEN
                  IFIRST=IFIRST+1
           IF (IFIRST==1) THEN
            DO N=1,NUMNOD
             ITAGPS(N) = 0
            ENDDO
            DO J=1,3
             DO N=1,NUMNOD
              VFLU(J,N) = ZERO
              AFLU(J,N) = ZERO
             ENDDO
            ENDDO
            CALL TENSGPS1(VFLU    ,AFLU    ,IPARG   ,GEO     ,
     .                    IXS   ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC   ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    X     ,ITAGPS  ,ELBUF_TAB)
           ENDIF
                  J = I-(IAD_GPS+2)
           ! --------------------------
           IF(NSPMD > 1)THEN
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF (IFIRST==1) CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
            IF (J<=3) THEN
             CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            ELSE
             CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
            ENDIF
           ENDIF     
           ! --------------------------     

           IF (J<=3) THEN
            DO N=1,NUMNOD
             IF (ITAGPS(N)>0) WA4(N)=VFLU(J,N)/ITAGPS(N)
            ENDDO
           ELSE
            DO N=1,NUMNOD
             IF (ITAGPS(N)>0) WA4(N)=AFLU(J-3,N)/ITAGPS(N)
            ENDDO
           ENDIF
C
          ELSEIF (I>(IAD_GPS+8).AND.I<(IAD_GPS+15)) THEN
                  IFIRST=IFIRST+1
           IF (IFIRST==1) THEN
                   IUL = 1
            DO N=1,NUMNOD
             ITAGPS(N) = 0
            ENDDO
            DO J=1,3
             DO N=1,NUMNOD
              VFLU(J,N) = ZERO
              AFLU(J,N) = ZERO
             ENDDO
            ENDDO
            CALL TENCGPS1(ELBUF_TAB ,IPARG,IUL  ,VFLU  ,AFLU   ,
     .                    X     ,IXC  ,IGEO ,IXTG  ,ITAGPS )
           ENDIF
                  J = I-(IAD_GPS+8)
           ! --------------------------
           IF(NSPMD > 1)THEN
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF (IFIRST==1) CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
            IF (J<=3) THEN
             CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            ELSE
             CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
            ENDIF
           ENDIF
           ! --------------------------

           IF (J<=3) THEN
            DO N=1,NUMNOD
             IF (ITAGPS(N)>0) WA4(N)=VFLU(J,N)/ITAGPS(N)
            ENDDO
           ELSE
            DO N=1,NUMNOD
             IF (ITAGPS(N)>0) WA4(N)=AFLU(J-3,N)/ITAGPS(N)
            ENDDO
           ENDIF
          ELSEIF (I>(IAD_GPS+14).AND.I<(IAD_GPS+21)) THEN
                  IFIRST=IFIRST+1
           IF (IFIRST==1) THEN
                   IUL = 2
            DO N=1,NUMNOD
             ITAGPS(N) = 0
            ENDDO
            DO J=1,3
             DO N=1,NUMNOD
              VFLU(J,N) = ZERO
              AFLU(J,N) = ZERO
             ENDDO
            ENDDO
            CALL TENCGPS1(ELBUF_TAB ,IPARG,IUL  ,VFLU  ,AFLU   ,
     .                    X     ,IXC  ,IGEO ,IXTG  ,ITAGPS )
           ENDIF
                  J = I-(IAD_GPS+14)
           ! --------------------------
           IF(NSPMD > 1)THEN
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF (IFIRST==1) CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
            IF (J<=3) THEN
             CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            ELSE
             CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
            ENDIF
           ENDIF
           ! --------------------------

           IF (J<=3) THEN
            DO N=1,NUMNOD
             IF (ITAGPS(N)>0) WA4(N)=VFLU(J,N)/ITAGPS(N)
            ENDDO
           ELSE
            DO N=1,NUMNOD
             IF (ITAGPS(N)>0) WA4(N)=AFLU(J-3,N)/ITAGPS(N)
            ENDDO
           ENDIF
C------------gps2 part
          ELSEIF (I>IAD_GP2.AND.I<(IAD_GP2+3)) THEN
            DO N=1,NUMNOD
             VGPS(N) = ZERO
             WGPS(N) = ZERO
            ENDDO
            J = I - IAD_GP2
            CALL DFUNGPS2(ELBUF_TAB ,WGPS    ,J       ,IPARG   ,GEO     ,
     .                    IXS       ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC       ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    X         ,VGPS    )

            ! --------------------------
            IF(NSPMD > 1)THEN
             LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
             CALL SPMD_EXCH_NODAREA(VGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT) 
             CALL SPMD_EXCH_NODAREA(WGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
            ENDIF
            ! --------------------------

             DO N=1,NUMNOD
              IF (VGPS(N)>ZERO) WA4(N)=WGPS(N)/VGPS(N)
             ENDDO
          ELSEIF (I>(IAD_GP2+2).AND.I<(IAD_GP2+9)) THEN
                  IFIRST=IFIRST+1
           IF (IFIRST==1) THEN
            DO N=1,NUMNOD
             VGPS(N) = ZERO
            ENDDO
            DO J=1,3
             DO N=1,NUMNOD
              VFLU(J,N) = ZERO
              AFLU(J,N) = ZERO
             ENDDO
            ENDDO
            CALL TENSGPS2(VFLU    ,AFLU    ,IPARG   ,GEO     ,
     .                    IXS   ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                    IXC   ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                    X     ,VGPS   ,ELBUF_TAB )
           ENDIF
                  J = I-(IAD_GP2+2)

           ! --------------------------
           IF(NSPMD > 1)THEN
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF (IFIRST==1) CALL SPMD_EXCH_NODAREA(VGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT) 
            IF (J<=3) THEN
             CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            ELSE
             CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
            ENDIF
           ENDIF
           ! --------------------------

           IF (J<=3) THEN
            DO N=1,NUMNOD
             IF (VGPS(N)>ZERO) WA4(N)=VFLU(J,N)/VGPS(N)
            ENDDO
           ELSE
            DO N=1,NUMNOD
             IF (VGPS(N)>ZERO) WA4(N)=AFLU(J-3,N)/VGPS(N)
            ENDDO
           ENDIF
C
          ELSEIF (I>(IAD_GP2+8).AND.I<(IAD_GP2+15)) THEN
                  IFIRST=IFIRST+1
           IF (IFIRST==1) THEN
                   IUL = 1
            DO N=1,NUMNOD
             VGPS(N) = ZERO
            ENDDO
            DO J=1,3
             DO N=1,NUMNOD
              VFLU(J,N) = ZERO
              AFLU(J,N) = ZERO
             ENDDO
            ENDDO
            CALL TENCGPS2(ELBUF_TAB ,IPARG,IUL  ,VFLU  ,AFLU   ,
     .                    X     ,IXC  ,IGEO ,IXTG  ,GEO    ,
     .                    VGPS   )
           ENDIF
                  J = I-(IAD_GP2+8)
           ! --------------------------
           IF(NSPMD > 1)THEN
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF (IFIRST==1) CALL SPMD_EXCH_NODAREAI(VGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
            IF (J<=3) THEN
             CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            ELSE
             CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
            ENDIF
           ENDIF
           ! --------------------------
           IF (J<=3) THEN
            DO N=1,NUMNOD
             IF (VGPS(N)>ZERO) WA4(N)=VFLU(J,N)/VGPS(N)
            ENDDO
           ELSE
            DO N=1,NUMNOD
             IF (VGPS(N)>ZERO) WA4(N)=AFLU(J-3,N)/VGPS(N)
            ENDDO
           ENDIF
          ELSEIF (I>(IAD_GP2+14).AND.I<(IAD_GP2+21)) THEN
                  IFIRST=IFIRST+1
           IF (IFIRST==1) THEN
                   IUL = 2
            DO N=1,NUMNOD
             VGPS(N) = ZERO
            ENDDO
            DO J=1,3
             DO N=1,NUMNOD
              VFLU(J,N) = ZERO
              AFLU(J,N) = ZERO
             ENDDO
            ENDDO
            CALL TENCGPS2(ELBUF_TAB ,IPARG,IUL  ,VFLU  ,AFLU   ,
     .                    X     ,IXC  ,IGEO ,IXTG  ,GEO    ,
     .                    VGPS   )
           ENDIF
                  J = I-(IAD_GP2+14)
           ! --------------------------
           IF(NSPMD > 1)THEN
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF (IFIRST==1) CALL SPMD_EXCH_NODAREA(VGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
            IF (J<=3) THEN
             CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            ELSE
             CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
            ENDIF
           ENDIF
           ! --------------------------

           IF (J<=3) THEN
            DO N=1,NUMNOD
             IF (VGPS(N)>ZERO) WA4(N)=VFLU(J,N)/VGPS(N)
            ENDDO
           ELSE
            DO N=1,NUMNOD
             IF (VGPS(N)>ZERO) WA4(N)=AFLU(J-3,N)/VGPS(N)
            ENDDO
           ENDIF
          ELSEIF (I>(IAD_GP3).AND.I<(IAD_GP3+7)) THEN
                    
C------------gps part
            IFIRST=IFIRST+1
            IF (IFIRST==1) THEN
             DO N=1,NUMNOD
              ITAGPS(N) = 0
             ENDDO
             DO J=1,3
              DO N=1,NUMNOD
               VFLU(J,N) = ZERO
               AFLU(J,N) = ZERO
              ENDDO
             ENDDO
             CALL TENSGPS3(ELBUF_TAB,VFLU    ,AFLU    ,IPARG   ,GEO     ,
     .                     IXS      ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                     IXC      ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                     X        ,ITAGPS  ,PM)
            ENDIF
            J = I-IAD_GP3
          ! --------------------------
            IF(NSPMD > 1)THEN
              LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
              IF (IFIRST==1) CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
              IF (J<=3) THEN
               CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
              ELSE
               CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
              ENDIF
            ENDIF
          ! --------------------------

            IF (J<=3) THEN
             DO N=1,NUMNOD
              IF (ITAGPS(N)>0) WA4(N)=VFLU(J,N)/ITAGPS(N)
             ENDDO
            ELSE
             DO N=1,NUMNOD
              IF (ITAGPS(N)>0) WA4(N)=AFLU(J-3,N)/ITAGPS(N)
             ENDDO
            ENDIF
          ELSEIF (I>(IAD_ISO).AND.I<(IAD_ISO+7)) THEN
C------------iso geo stress
           DO N=1,64*NUMELIG3D
            WA4(NUMNOD + N)=TABSTRESL(I-IAD_ISO,N)
           ENDDO
          ELSEIF (I>(IAD_GP4).AND.I<(IAD_GP4+7)) THEN
                    
C------------gps part
           IFIRST=IFIRST+1
           IF (IFIRST==1) THEN
             DO N=1,NUMNOD
              ITAGPS(N) = 0
             ENDDO
             DO J=1,3
              DO N=1,NUMNOD
               VFLU(J,N) = ZERO
               AFLU(J,N) = ZERO
              ENDDO
             ENDDO
             CALL TENSGPSTRAIN(ELBUF_TAB,VFLU    ,AFLU    ,IPARG   ,GEO     ,
     .                     IXS      ,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .                     IXC      ,IXTG    ,IXT     ,IXP     ,IXR     ,
     .                     X        ,ITAGPS  ,PM      )
           ENDIF
           J = I-IAD_GP4
           ! --------------------------
           IF(NSPMD > 1)THEN
             LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
             IF (IFIRST==1) CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
             IF (J<=3) THEN
              CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
             ELSE
              CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J-3)
             ENDIF
           ENDIF
           ! --------------------------

           IF (J<=3) THEN
             DO N=1,NUMNOD
               IF (ITAGPS(N)>0) WA4(N)=VFLU(J,N)/ITAGPS(N)
             ENDDO
           ELSE
             DO N=1,NUMNOD
               IF (ITAGPS(N)>0) WA4(N)=AFLU(J-3,N)/ITAGPS(N)
             ENDDO
           ENDIF
c----------------------------------------
           
          ENDIF

c----------------------------------------
          IF (NSPMD == 1) THEN
            IF (NUMELIG3D /= 0) THEN
              DO J=1,NUMNOD + 64*NUMELIG3D
                CALL WRITE_R_C(WA4(J),1)
              ENDDO
            ELSE
              DO J=1,NUMNOD
                CALL WRITE_R_C(WA4(J),1)
              ENDDO
            ENDIF
          ELSE
               IF (ISPMD==0) THEN
                  CALL SPMD_GATHERF(WA4,WEIGHT,NODGLOB,NUMNODG)
               ELSE
                  CALL SPMD_GATHERF(WA4,WEIGHT,NODGLOB,1)
               END IF
          END IF
          R4 = ZERO
          IF(NCUTS>0) THEN
             IF (I<3.OR.I==12) THEN
                CALL CUTFUNC(ANIN(K+1),ICBUF(MIC2),CBUF(MAC2),NODCUT)
             ELSE
                DO N=1,NODCUT
                   CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF
          ENDIF
            IF (ISPMD==0) THEN
              DO N=1,NSECT+NRWALL+NNWL+NNSRG+NNSMD+NNSPHG+2*NUMELS16G
                 CALL WRITE_R_C(R4,1)
              ENDDO
            ENDIF
C nodal ply dmass = zero (is not computed for anim)
          IF(ISPMD == 0. AND. ANIM_PLY >0 ) THEN 
             R4 = ZERO
             DO N=1,NFNOD_PXFEMG          
              CALL WRITE_R_C(R4,1)
            ENDDO
          ENDIF
C xfem crack for layered shells +++
          IF(ISPMD == 0 .AND. ANIM_CRK > 0)THEN       
             R4 = ZERO
             DO N=1,NFNOD_CRKXFEMG
              CALL WRITE_R_C(R4,1)
            ENDDO
          ENDIF
C xfem crack for layered shells ---
C
          IF (ISPMD==0.AND.NFVNOD>0) THEN
             R4=ZERO
             DO N=1,NFVNOD+3
                CALL WRITE_R_C(R4,1)
             ENDDO
          ENDIF
      ENDDO
C-----------------------------------------------
C   ELEMENT FUNC (quad+coque)
C-----------------------------------------------
      NDMA2= NUMNOD*(MIN(1,ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT)
     .              +MIN(1,ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS)
     .              +MIN(1,ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER))
C traitement equivalent monoproc
      IF (NSPMD == 1 .AND. NFVTR>0) THEN
         ALLOCATE(FVMASS(NFVTR),FVPRES(NFVTR), FVQX(NFVTR),
     .            FVQY(NFVTR), FVQZ(NFVTR), FVRHO(NFVTR),
     .            FVENER(NFVTR), FVCSON(NFVTR), FVGAMA(NFVTR),
     .            FVVISU(NFVTR))
         DO I=1,NFVTR
            FVMASS(I)=ZERO
            FVPRES(I)=ZERO
            FVQX(I)=ZERO
            FVQY(I)=ZERO
            FVQZ(I)=ZERO
            FVRHO(I)=ZERO
            FVENER(I)=ZERO
            FVCSON(I)=ZERO
            FVGAMA(I)=ZERO
            FVVISU(I)=ZERO
         ENDDO
C
         ELOFF=0
         DO I=1,NFVBAG
            IF (FVDATA(I)%NPOLH_ANIM>0) THEN
C
               DO J=1,FVDATA(I)%NPOLH
                  GAMA=FVDATA(I)%GPOLH(J)
                  SSP=SQRT((GAMA-ONE)*GAMA*FVDATA(I)%EPOLH(J)/
     .                                   FVDATA(I)%MPOLH(J))
                  DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                     KK=FVDATA(I)%IFVPOLH(K)
                     DO N=FVDATA(I)%IFVTADR(KK),
     .                         FVDATA(I)%IFVTADR(KK+1)-1
                        NN=FVDATA(I)%IFVPOLY(N)
                        FAC=ONE
                        IF (FVDATA(I)%IFVTRI(4,NN)>0) THEN
                           IDP=FVDATA(I)%IDPOLH(J)
                           FVVISU(FVEL2FA(ELOFF+NN))=IDP-(IDP/8)*8+1
                        ELSE
                           FVVISU(FVEL2FA(ELOFF+NN))=-1
                           FAC=HALF
                        ENDIF
                        NN=FVEL2FA(ELOFF+NN)
                        FVMASS(NN)=FVMASS(NN)+FAC*FVDATA(I)%MPOLH(J)
                        FVPRES(NN)=FVPRES(NN)+FAC*FVDATA(I)%PPOLH(J)
                        IF (FVDATA(I)%MPOLH(J)>ZERO) THEN
                           FVQX(NN)=FVQX(NN)+FAC*FVDATA(I)%QPOLH(1,J)/
     .                                           FVDATA(I)%MPOLH(J)
                           FVQY(NN)=FVQY(NN)+FAC*FVDATA(I)%QPOLH(2,J)/
     .                                           FVDATA(I)%MPOLH(J)
                           FVQZ(NN)=FVQZ(NN)+FAC*FVDATA(I)%QPOLH(3,J)/
     .                                           FVDATA(I)%MPOLH(J)
                           FVENER(NN)=FVENER(NN)+FAC*FVDATA(I)%EPOLH(J)/
     .                                               FVDATA(I)%MPOLH(J)
                        ENDIF
                        FVRHO(NN)=FVRHO(NN)+FAC*FVDATA(I)%RPOLH(J)
                        FVCSON(NN)=FVCSON(NN)+FAC*SSP
                        FVGAMA(NN)=FVGAMA(NN)+FAC*GAMA
                     ENDDO
                  ENDDO
               ENDDO
               ELOFF=ELOFF+FVDATA(I)%NNTR
            ENDIF
         ENDDO
      ELSEIF(NFVTR>0) THEN
         IF (ISPMD==0) THEN
            ALLOCATE(FVMASS(NFVTR),FVPRES(NFVTR), FVQX(NFVTR),
     .               FVQY(NFVTR), FVQZ(NFVTR), FVRHO(NFVTR),
     .               FVENER(NFVTR), FVCSON(NFVTR), FVGAMA(NFVTR),
     .               FVVISU(NFVTR))
            DO I=1,NFVTR
               FVMASS(I)=ZERO
               FVPRES(I)=ZERO
               FVQX(I)=ZERO
               FVQY(I)=ZERO
               FVQZ(I)=ZERO
               FVRHO(I)=ZERO
               FVENER(I)=ZERO
               FVCSON(I)=ZERO
               FVGAMA(I)=ZERO
               FVVISU(I)=ZERO
            ENDDO
         ENDIF
C
         CALL SPMD_FVB_AELF(FVMASS,  FVPRES, FVQX,   FVQY,   FVQZ,
     .                      FVRHO,   FVENER, FVCSON, FVGAMA, FVVISU,
     .                      FVEL2FA)
      ENDIF
C  en spmd localement NBF = 0 mais pas la somme sur pi !
       DO I = 1,MX_ANI
        IFUNC = I
        IF(ANIM_CE(I)==1)THEN
          IF(I<=2142 .OR. I==2155 .OR. I==2156 .OR. (I>=2239.AND.I<=10251) .OR. 
     .      (I>=10253.AND.I<=10675) .OR. (I >= 10676 .AND. I <= 1000000)) THEN
            CALL DFUNCC(ELBUF_TAB ,WAFT        ,IFUNC         ,IPARG,GEO    ,
     .                  IXQ       ,IXC         ,IXTG          ,MAS          ,PM         ,
     .                  EL2FA     ,NBF         ,IAD          ,
     .                  NBF_L     ,EANI        ,ANIN(NDMA2+1) ,NBPART       ,IADG       ,
     .                  IPM       ,IGEO        ,THKE          ,ERR_THK_SH4  ,ERR_THK_SH3,
     .                  INVERT    ,X           ,V             ,W            ,ALE_CONNECTIVITY,
     .                  NV46      ,NERCVOIS    ,NESDVOIS      ,LERCVOIS     ,LESDVOIS   ,
     .                  STACK     ,BUFMAT      ,MULTI_FVM)
            IF(NCUTS>0)
     .      CALL CUTFUNCE(ICBUF,NELCUT,ELBUF_TAB,IFUNC,IPARG,PM,IXS)
            IF (ISPMD==0) THEN
              R4 = ZERO
              DO J=1,NESBW2
                CALL WRITE_R_C(R4,1)
              ENDDO
            ENDIF
C ply xfem
C ply information
            IF (ANIM_PLY > 0)THEN
             CALL DFUNCC_PLY(ELBUF_TAB, WAFT_PLY, IFUNC, IPARG, GEO,
     .                        IXC ,  IXTG    , MAS,   PM,    EL2FA_PLY,
     .                        NBF_PXFEM,IAD  , PLYNUMC,EANI,ANIN(NDMA2+1),
     .                        NPLYPART,IAD_PLYG,IPM  ,IGEO , THKE,
     .                        ERR_THK_SH4, ERR_THK_SH3,
     .                        NBF_PXFEMG ,X , STACK )
            ENDIF
            IF (ANIM_CRK > 0)THEN
             CALL DFUNCC_CRK(
     .           ELBUF_TAB   ,LEN_CRKX   ,IFUNC      ,IPARG,GEO          ,
     .           IXC         ,IXTG       ,MAS        ,PM   ,EL2FA_CRK    ,
     .           NBF_CRKXFEM ,IAD        ,NBF_CRKXFEM,EANI ,ANIN(NDMA2+1),
     .           NCRKPART    ,IAD_CRKG   ,IPM        ,IGEO ,THKE         ,
     .           ERR_THK_SH4 ,ERR_THK_SH3,XFEM_TAB   ,IEL_CRK   ,INDX_CRK,
     .           NBF_CRKXFEMG,EL2FA      ,CRKEDGE    )
            ENDIF
C

            IF (ISPMD==0.AND.NFVTR>0) THEN
               R4=ZERO
               DO J=1,NFVTR
                  CALL WRITE_R_C(R4,1)
               ENDDO
            ENDIF
          ELSEIF (I==2143.OR.I==2144) THEN
             CALL ANIMCALE(I,      MONVOL, VOLMON, NBF, EL2FA,
     .                     NBPART, IADG,   NBF_L )
             IF (ISPMD==0) THEN
                R4 = ZERO
                DO J=1,NESBW2
                   CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF
C
             IF(ISPMD == 0 .AND. ANIM_PLY > 0 ) THEN
               R4=ZERO
                DO J=1,NBF_PXFEMG
                   CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF
C xfem crack for layered shells +++
             IF(ISPMD==0 .AND. ANIM_CRK > 0)THEN
               R4=ZERO
                DO J=1,NBF_CRKXFEMG
                  CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF
C xfem crack for layered shells ---
             IF (ISPMD==0.AND.NFVTR>0) THEN
                R4=ZERO
                DO J=1,NFVTR
                   CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF

          ELSEIF (I>=2145.AND.I<=2154) THEN
             IF (ISPMD==0) THEN
                R4=ZERO
                DO J=1,NBF+NELCUT+NESBW2+NBF_PXFEMG+NBF_CRKXFEMG
                   CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF
             IF (ISPMD==0.AND.NFVTR>0) THEN
                IF (I==2145) THEN
                   DO J=1,NFVTR
                      R4=FVMASS(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2146) THEN
                   DO J=1,NFVTR
                      R4=FVPRES(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2147) THEN
                   DO J=1,NFVTR
                      R4=FVQX(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2148) THEN
                   DO J=1,NFVTR
                      R4=FVQY(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2149) THEN
                   DO J=1,NFVTR
                      R4=FVQZ(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2150) THEN
                   DO J=1,NFVTR
                      R4=FVRHO(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2151) THEN
                   DO J=1,NFVTR
                      R4=FVENER(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2152) THEN
                   DO J=1,NFVTR
                      R4=FVCSON(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2153) THEN
                   DO J=1,NFVTR
                      R4=FVGAMA(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ELSEIF (I==2154) THEN
                   DO J=1,NFVTR
                      R4=FVVISU(J)
                      CALL WRITE_R_C(R4,1)
                   ENDDO
                ENDIF
             ENDIF 
           ENDIF!I values
         ENDIF !IF(ANIM_CE(I)==1)THEN
       ENDDO !next I
      IF (ISPMD==0.AND.NFVTR>0)
     .   DEALLOCATE(FVMASS, FVPRES, FVQX, FVQY, FVQZ, FVRHO, FVENER,
     .              FVCSON, FVGAMA)
C-----------------------------------------------
C   VECT TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
        IF(ANIM_V(1)==1) CALL ANI_TXT('Velocity',8)
        IF(ANIM_V(2)==1) CALL ANI_TXT('Displacement',12)
        IF(ANIM_V(3)==1) CALL ANI_TXT('Acceleration',12)
        IF(ANIM_V(4)==1.AND.ANIMCONT==1) CALL ANI_TXT('Contact Forces',14)
        IF(ANIM_V(5)==1) CALL ANI_TXT('Internal Forces',15)
        IF(ANIM_V(6)==1) CALL ANI_TXT('External Forces',15)
        IF(ANIM_V(7)==1) CALL ANI_TXT('Sect.RBY,Wall F.',16)
        IF(ANIM_V(8)==1) CALL ANI_TXT('Sect.RBY Moments',16)
        IF(ANIM_V(9)==1) CALL ANI_TXT('Rotational Velocity',19)
        IF(ANIM_V(10)==1) CALL ANI_TXT('Fluid velocity',14)
        IF(ANIM_V(11)==1) CALL ANI_TXT('Residual Forces',15)
        IF(ANIM_V(12)==1) THEN
          CALL ANI_TXT('Contact Pressure / Normal',25)
          CALL ANI_TXT('Contact Pressure / Tangent',26)
        END IF
        IF(ANIM_V(13)==1) CALL ANI_TXT('Tied Contact Forces',19)
        IF(ANIM_V(14)==1) CALL ANI_TXT('Rotational DOF',14)
        IF(ANIM_V(16)==1) CALL ANI_TXT('Gaz Velocity',12)
        IF(ANIM_V(17)==1) CALL ANI_TXT('Reaction Forces',15)
        IF(ANIM_V(18)==1) CALL ANI_TXT('Reaction Moments',16)
        IF(ANIM_V(19)==1) CALL ANI_TXT('Cluster Forces',14)
        IF(ANIM_V(20)==1) CALL ANI_TXT('Cluster Moments',15)
        IF(ANIM_V(21)==1) CALL ANI_TXT('inter22 - Centroid Velocity',27)          !inter22
        IF(ANIM_V(22)==1) CALL ANI_TXT('inter22 - Faces Velocity',24)             !inter22
        IF(ANIM_V(23)==1) 
     .  CALL ANI_TXT('inter22 - Centroid Momentum Density',35)  !inter22
        IF(ANIM_V(24)==1) CALL ANI_TXT('inter22 - Faces Pressure Forces',31)      !inter22
        IF(ANIM_V(25)==1) 
     . CALL ANI_TXT('inter22 - Centroid Internal Force',33)    !inter22
        IF(ANIM_V(26)==1)
     . CALL ANI_TXT('Maximum Contact Forces Over Time',32)
        IF(ANIM_V(27)==1) THEN
          CALL ANI_TXT('Tied Contact Pressure / Normal',30)
          CALL ANI_TXT('Tied Contact Pressure / Tangent',31)
        END IF    
      ENDIF
C-----------------------------------------------
C    VECT
C-----------------------------------------------
      NNNSRG=NNSRG+NNSMD+NNSPHG+2*NUMELS16G
C     IF(NCUTS == 0) THEN
        IF(.NOT. ALLOCATED(ICBUF)) ALLOCATE(ICBUF(1)) 
        IF(.NOT. ALLOCATED(CBUF)) ALLOCATE(CBUF(1))
C     ENDIF
      IF(ANIM_V(1)==1) THEN
       CALL VELVEC(V,V_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,1,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
      ENDIF
      IF(ANIM_V(2)==1)
     . CALL VELVEC(D,D_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,2,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
      IF(ANIM_V(3)==1)
     . CALL VELVEC(A,A_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,3,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
      IF(ANIM_V(4)==1.AND.ANIMCONT==1)THEN
       IF(NINTSTAMP==0)THEN
         CALL VELVECC(CONT,CONT_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       NFNOD_CRKXFEMG)
       ELSE
         CALL VELVECC21(CONT,CONT_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       FCONTG,NFNOD_CRKXFEMG)
       END IF
      END IF
      IF(ANIM_V(5)==1)
     . CALL VELVEC(FINT,FINT_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,5,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
      IF(ANIM_V(6)==1)
     . CALL VELVECC(FEXT,FEXT_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       NFNOD_CRKXFEMG)

      IF(ANIM_V(7)==1) CALL VELVEC2(ICBUF(MIC2),BID_TEMP,CBUF(MAC2),
     .    NODCUT,FOPT(1,1),NPBY,NNWL,NNNSRG,
     .    NODGLOB,WEIGHT,FR_SEC,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .    NFNOD_CRKXFEMG)
      IF(ANIM_V(8)==1) CALL VELVEC2(ICBUF(MIC2),BID_TEMP,CBUF(MAC2),
     .    NODCUT,FOPT(4,1),NPBY,NNWL,NNNSRG,
     .    NODGLOB,WEIGHT,FR_SEC,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .    NFNOD_CRKXFEMG)
      IF(ANIM_V(9)==1 .AND. IRODDL/=0) THEN
          CALL VELVEC(VR,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,
     .                NNWL,NNNSRG,NODGLOB,WEIGHT,NFVNOD,9,
     .                NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)   
      ELSEIF(ANIM_V(9)==1) THEN
        IF (NSPMD == 1) THEN
          DO I=1,NUMNOD
            R4 = ZERO
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          ENDDO
          IF (NUMELIG3D /= 0)THEN
           DO I=1,64*NUMELIG3D
            R4 = BID_TEMP(1,I)
            CALL WRITE_R_C(R4,1)
            R4 = BID_TEMP(2,I)
            CALL WRITE_R_C(R4,1)
            R4 = BID_TEMP(3,I)
            CALL WRITE_R_C(R4,1)
           ENDDO
          ENDIF
        ELSEIF (ISPMD == 0) THEN
          DO I=1,NUMNODG
            R4 = ZERO
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          ENDDO
        ENDIF
      ENDIF
      IF (ANIM_V(10)==1) THEN
cc            CALL VELVEC(VFLU,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),
c     .                  NODCUT,NNWL,NNNSRG,NODGLOB,WEIGHT,0,ANIM_V(10),
c     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,VFLU_ALE)
C avoir pour anim ply en spmd
            CALL VELVEC3(VFLU,BID_TEMP,VFLU_ALE,ICBUF(MIC2),CBUF(MAC2),
     .                 NODCUT,NNWL,NNNSRG,NODGLOB,WEIGHT,0,NFNOD_PXFEM,
     .                 NFNOD_PXFEMG,NFNOD_CRKXFEMG)
         IF (NFVNOD>0) THEN
            IF (NSPMD == 1) THEN
               CALL ALEVEC()
            ELSE
               CALL SPMD_FVB_AVEC()
            ENDIF
         ENDIF
      ENDIF
      IF (ANIM_V(11)==1) THEN
       IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
         FAC=ONE
         IF (IMPL_S>0.AND.IDYNA==0)FAC=ZERO 
          DO J=1,3
          DO N=1,NUMNOD
           VFLU(J,N)=FEXT(J,N)+FINT(J,N)-FAC*MS(N)*A(J,N)
          ENDDO
          ENDDO
          CALL VELVEC(VFLU,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .         NNNSRG,NODGLOB,WEIGHT,NFVNOD,11,
     .         NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
        ELSE
          CALL VELVEC(RES_SMS,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .          NNNSRG,NODGLOB,WEIGHT,NFVNOD,11,
     .          NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
        END IF
      ENDIF
      IF(ANIM_V(12)==1) THEN
       IF(NINTSTAMP==0)THEN
        CALL VELVECC(FNCONT,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       NFNOD_CRKXFEMG)
        CALL VELVECC(FTCONT,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       NFNOD_CRKXFEMG)
       ELSE
        CALL VELVECC21(FNCONT,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       FNCONTG,NFNOD_CRKXFEMG)
        CALL VELVECC21(FTCONT,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       FTCONTG,NFNOD_CRKXFEMG)
       END IF
      END IF
C
      IF(ANIM_V(13)==1) THEN
        CALL VELVECC(FNCONT2,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       NFNOD_CRKXFEMG)
      ENDIF
C
      IF(ANIM_V(14)==1 .AND.(IDROT == 1 .OR. ISECUT>0 .OR. IISROT>0 .OR. IMPOSE_DR>0 ) .AND. IRODDL/=0) THEN
        CALL VELVEC(DR,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,14,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
      ENDIF
C
      IF (ANIM_V(15) == 1) THEN
        CALL VELVECC(DXANCG,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .               NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,
     .               NFNOD_PXFEMG,NFNOD_CRKXFEMG)
      ENDIF
      IF(ANIM_V(16)==1 .AND. IALELAG  > 0 ) THEN
        CALL VELVEC(VGAZ,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,16,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
      ELSEIF(ANIM_V(16)==1 .AND. IALELAG  == 0 ) THEN
        IF (ISPMD == 0) THEN
          DO I=1,NUMNODG
            R4 = ZERO
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          ENDDO
        ENDIF
      ENDIF
      IF(ANIM_V(17)==1) THEN
        DO N=1,NUMNOD
         FANREACT(1,N)=FANREAC(1,N)
         FANREACT(2,N)=FANREAC(2,N)
         FANREACT(3,N)=FANREAC(3,N)
        ENDDO
        CALL VELVEC(FANREACT,FANREACT_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,17,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)
      ENDIF
      IF(ANIM_V(18)==1) THEN
        DO N=1,NUMNOD
         FANREACR(1,N)=FANREAC(4,N)
         FANREACR(2,N)=FANREAC(5,N)
         FANREACR(3,N)=FANREAC(6,N)
        ENDDO
        CALL VELVEC(FANREACR,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,18,
     .       NFNOD_PXFEM,NOD_PXFEM,INDX_PLY,NFNOD_CRKXFEMG,ITAB)

      ENDIF
c
      IF (ANIM_V(19) == 1) THEN
c       Cluster forces
        CALL VELVECC(FCLUSTER,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .               NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,
     .               NFNOD_PXFEMG,NFNOD_CRKXFEMG)
      ENDIF
c
      IF (ANIM_V(20) == 1) THEN
c       Cluster moments
        CALL VELVECC(MCLUSTER,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .               NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,
     .               NFNOD_PXFEMG,NFNOD_CRKXFEMG)
      ENDIF
      
      IF (ANIM_V(21) == 1) THEN
c       interface 22, centroid vel
        CALL VELVECC22(ELBUF_TAB,IPARG,1,IXS,IXQ,ITAB)
      ENDIF         

      IF (ANIM_V(22) == 1) THEN
c       interface 22
        CALL VELVECZ22(ELBUF_TAB,IPARG ,IPARI ,IGRNOD ,   X,
     .                 IXS      ,IXQ   ,ITAB  ,1      )
      ENDIF
      
      IF (ANIM_V(23) == 1) THEN
c       interface 22, centroid rho.vel
        CALL VELVECC22(ELBUF_TAB,IPARG,2,IXS,IXQ,ITAB)
      ENDIF 
      
      IF (ANIM_V(24) == 1) THEN
c       interface 22 / internal forces
        CALL VELVECZ22(ELBUF_TAB,IPARG ,IPARI ,IGRNOD ,   X,
     .                 IXS      ,IXQ   ,ITAB  ,2      )
      ENDIF    
      
      IF (ANIM_V(25) == 1) THEN
c       interface 22, centroid rho.vel
        CALL VELVECC22(ELBUF_TAB,IPARG,3,IXS,IXQ,ITAB)
      ENDIF 

      IF(ANIM_V(26)==1)THEN

        IF(NINTSTAMP==0)THEN
           CALL VELVECC(FCONT_MAX,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .          NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .          NFNOD_CRKXFEMG)
        ELSE
           CALL VELVECC_MAX(FCONT_MAX,NODCUT,NNWL,NNSRG,NFVNOD,
     .                   NFNOD_PXFEMG,NFNOD_CRKXFEMG)
        ENDIF
      END IF    

      IF(ANIM_V(27)==1) THEN
        CALL VELVECC(FNCONTP2,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       NFNOD_CRKXFEMG)
        CALL VELVECC(FTCONTP2,BID_TEMP,ICBUF(MIC2),CBUF(MAC2),NODCUT,NNWL,
     .       NNNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .       NFNOD_CRKXFEMG)
      ENDIF                    
C-----------------------------------------------
C   2D TENSOR TEXT
C-----------------------------------------------
C  en spmd localement NBF = 0 mais pas la somme sur p0 !
C  sur p0 nbf global
      IF (ISPMD==0.AND.NBF+NELCUT+NESBW2/=0)THEN
        IF(ANIM_CT(1)==1) CALL ANI_TXT('Stress (membrane)',17)
        IF(ANIM_CT(2)==1) CALL ANI_TXT('Stress (moment/t^2)',19)
        IF(ANIM_CT(3)==1) CALL ANI_TXT('Stress (upper)',14)
        IF(ANIM_CT(4)==1) CALL ANI_TXT('Stress (lower)',14)
        IF(ANIM_CT(5)==1) CALL ANI_TXT('Strain (membrane)',17)
        IF(ANIM_CT(6)==1) CALL ANI_TXT('Strain (Curvature)',18)
        IF(ANIM_CT(7)==1) CALL ANI_TXT('Strain (upper)',14)
        IF(ANIM_CT(8)==1) CALL ANI_TXT('Strain (lower)',14)
        IF(ANIM_CT(91)==1)CALL ANI_TXT('Strn rate (membrane)',20)
        IF(ANIM_CT(92)==1)CALL ANI_TXT('Strn rate (Curvature)',21)
        IF(ANIM_CT(93)==1)CALL ANI_TXT('Strn rate (upper)',17)
        IF(ANIM_CT(94)==1)CALL ANI_TXT('Strn rate (lower)',17)
        DO I=1,100
         IF(ANIM_CT(100+I)==1)THEN
           WRITE(MES,'(A,I3,A)')'Stress (layer',I,')'
           CALL ANI_TXT(MES,18)
         ENDIF
        ENDDO
        DO I=1,100
         IF(ANIM_CT(200+I)==1)THEN
            WRITE(MES,'(A,I3,A)')'Strain (layer',I,')'
            CALL ANI_TXT(MES,18)
         ENDIF
        ENDDO
        DO I=1,100
         IF(ANIM_CT(300+I)==1)THEN
            WRITE(MES,'(A,I3,A)')'Strn rate (layer',I,')'
            CALL ANI_TXT(MES,21)
         ENDIF
        ENDDO
C-----------------------------------------------
C   stress TENS output (all NPTT) for new PID51 composite shell
C-----------------------------------------------
C
C Stress = UPPER =
C
        DO I=401,500
           IF (ANIM_CT(I) == 1) THEN
             IUS = MOD ((I - 400), 100)
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')'Stress (UPPER/LAYER',IUS,')'
             CALL ANI_TXT(MES,24)
            END IF
        END DO
C
C Stress = LOWER =
C
        DO I=501,600
           IF (ANIM_CT(I) == 1) THEN
             IUS = MOD ((I - 500), 100)
             IF(IUS==0) IUS = 100
             WRITE(MES,'(A,I3,A)')'Stress (LOWER/LAYER',IUS,')'
             CALL ANI_TXT(MES,24)
            END IF
        END DO
C
C Stress =  all NPTT =
C
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            IF (ANIM_CT(IUS + 600) == 1) THEN
              ILAY = I
              IPT  = J
              WRITE(MES,'(A,I3,I3,A)')
     .        'Stress (LAYER/IPT',ILAY,IPT,')'
              CALL ANI_TXT(MES,25)
            END IF
          ENDDO
        ENDDO
C
C Stress /ANIM/SHELL/IDPLY/STRESS/KEY5/KEY6
C
        DO I=1,MX_PLY_ANIM
          IF (ANIM_CT(1610 + I) == 1) THEN
            WRITE(MES,'(A,I10,X,I3,A)')
     .      'Stress (PLY/IPT',PLY_ANIM_STRESS( 3 * (I - 1) + 1),
     .      PLY_ANIM_STRESS( 3 * (I - 1) + 3),')'
            CALL ANI_TXT(MES,30)
          END IF
        ENDDO
C
C Strain /ANIM/SHELL/IDPLY/STRAIN/KEY5/KEY6
C
        DO I=1,MX_PLY_ANIM
          IF (ANIM_CT( (1610+ MX_PLY_ANIM) + I) == 1) THEN
            WRITE(MES,'(A,I10,X,I3,A)')
     .      'Strain (PLY/IPT',PLY_ANIM_STRAIN( 3 * (I - 1) + 1),
     .      PLY_ANIM_STRAIN( 3 * (I - 1) + 3),')'
            CALL ANI_TXT(MES,30)
          END IF
        ENDDO
C
C Strain /ANIM/SHELL/IDPLY/EPSDOT/KEY5/KEY6
C
        DO I=1,MX_PLY_ANIM
          IF (ANIM_CT( (1610+ 2*MX_PLY_ANIM) + I) == 1) THEN
            WRITE(MES,'(A,I10,X,I3,A)')
     .      'Epsdot (PLY/IPT',PLY_ANIM_EPSDOT( 3 * (I - 1) + 1),
     .      PLY_ANIM_EPSDOT( 3 * (I - 1) + 3),')'
            CALL ANI_TXT(MES,30)
          END IF
        ENDDO
C-----------------------------------------------
C   STRAIN TENSOR (PID51 + PID 52) /ANIM/SHELL/TENS/STRAIN/KEY5/KEY6
C-----------------------------------------------
!  upper for PID_51, 52
        IDX = 1610 + 3*MX_PLY_ANIM
        DO I=IDX+1,IDX+100
          IF (ANIM_CT(I) == 1) THEN
            IUS = MOD ((I - IDX), 100)
            IF (IUS == 0) IUS = 100
            WRITE(MES,'(A,I3,A)')'Strain (UPPER/LAYER',IUS,')'
            CALL ANI_TXT(MES,24)
          ENDIF
        ENDDO
!  lower for PID_51, 52
        IDX = 1710 + 3*MX_PLY_ANIM
        DO I=IDX+1,IDX+100
          IF (ANIM_CT(I) == 1) THEN
            IUS = MOD ((I - IDX), 100)
            IF (IUS == 0) IUS = 100
            WRITE(MES,'(A,I3,A)')'Strain (LOWER/LAYER',IUS,')'
            CALL ANI_TXT(MES,24)
          ENDIF
        ENDDO
! all NPTT through all layers for PID_51, 52
        IDX = 1810 + 3*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            IF (ANIM_CT(IUS + IDX) == 1) THEN
              ILAY = I
              IPT  = J
              WRITE(MES,'(A,I3,I3,A)')
     .        'Strain (LAYER/IPT',ILAY,IPT,')'
              CALL ANI_TXT(MES,25)
            END IF
          ENDDO
        ENDDO
C-----------------------------------------------
C   EPSDOT TENSOR (PID51 + PID 52) /ANIM/SHELL/TENS/EPSDOT/KEY5/KEY6
C-----------------------------------------------
!  upper for PID_51, 52
        IDX = 2820 + 3*MX_PLY_ANIM
        DO I=IDX+1,IDX+100
          IF (ANIM_CT(I) == 1) THEN
            IUS = MOD ((I - IDX), 100)
            IF (IUS == 0) IUS = 100
            WRITE(MES,'(A,I3,A)')'Epsdot (UPPER/LAYER',IUS,')'
            CALL ANI_TXT(MES,24)
          ENDIF
        ENDDO
!  lower for PID_51, 52
        IDX = 2920 + 3*MX_PLY_ANIM
        DO I=IDX+1,IDX+100
          IF (ANIM_CT(I) == 1) THEN
            IUS = MOD ((I - IDX), 100)
            IF (IUS == 0) IUS = 100
            WRITE(MES,'(A,I3,A)')'Epsdot (LOWER/LAYER',IUS,')'
            CALL ANI_TXT(MES,24)
          ENDIF
        ENDDO
! all NPTT through all layers for PID_51, 52
        IDX = 3020 + 3*MX_PLY_ANIM
        DO I=1,100
          DO J=1,10
            IUS = 10*I+J
            IF (ANIM_CT(IUS + IDX) == 1) THEN
              ILAY = I
              IPT  = J
              WRITE(MES,'(A,I3,I3,A)')
     .        'Epsdot (LAYER/IPT',ILAY,IPT,')'
              CALL ANI_TXT(MES,25)
            END IF
          ENDDO
        ENDDO

c-----------------------------------------------------------
c       Stress output in material (orthotropic) coordinates
c-----------------------------------------------------------
        IDX = 3120 + 3*MX_PLY_ANIM

        IF (ANIM_CT(IDX+1)==1) CALL ANI_TXT('Mstress (membrane)',18)
        IF (ANIM_CT(IDX+2)==1) CALL ANI_TXT('Mstress (upper)',15)
        IF (ANIM_CT(IDX+3)==1) CALL ANI_TXT('Mstress (lower)',15)
c
        IDX = 3120 + 3*MX_PLY_ANIM + 3
        DO I = IDX+1,IDX+100
         IF (ANIM_CT(I)==1) THEN
           WRITE(MES,'(A,I3,A)')'Mstress (layer',I,')'
           CALL ANI_TXT(MES,18)
         ENDIF
        ENDDO
c
c       /ANIM/SHELL/IDPLY/MSTRESS/KEY5/KEY6  (prop type 17, 51 and 52 only)
c
        IDX = 3120 + 3*MX_PLY_ANIM + 103
        DO I = IDX+1,IDX + MX_PLY_ANIM
          IF (ANIM_CT(I) == 1) THEN
            WRITE(MES,'(A,I10,X,I3,A)')
     .      'Mstress (PLY/IPT',PLY_ANIM_STRESS( 3 * (I - 1) + 1),
     .      PLY_ANIM_STRESS(3 * (I - 1) + 3),')'
            CALL ANI_TXT(MES,30)
          END IF
        ENDDO
!
      ENDIF  ! IF(ISPMD==0.AND.NBF+NELCUT+NESBW2/=0)
C-----------------------------------------------
C   2D TENSOR
C-----------------------------------------------
      DO I = 1,MX_ANI
        IFUNC = I
        IF (ANIM_CT(I) == 1) THEN
          CALL TENSORC(ELBUF_TAB,IPARG ,IFUNC ,INVERT,NELCUT,
     .                 EL2FA    ,NBF   ,WAFT  ,TANI  ,IAD   ,
     .                 NBF_L    ,NBPART,IADG  ,X     ,IXC   ,
     .                 IGEO     ,IXTG  ,IPM   ,STACK ,MAT_PARAM)
          IF (ISPMD==0) THEN
            R4 = ZERO
            DO J=1,NESBW2
             CALL WRITE_R_C(R4,1)
             CALL WRITE_R_C(R4,1)
             CALL WRITE_R_C(R4,1)
            ENDDO
          ENDIF
C
          IF (ANIM_PLY > 0) THEN    ! prop type 17 only
             CALL TENSORC_PLY(IPLY,    NEL_PLY,  ELBUF_TAB ,IPARG,
     .                        IFUNC,   INVERT,   EL2FA_PLY ,NBF_PXFEM,
     .                        WAFT_PLY,TANI,     IAD,       PLYNUMC,
     .                        NBPART,  IAD_PLYG, X,         IXC,
     .                        IGEO,    IXTG,     NBF_PXFEMG,IPM, STACK )
          ENDIF
C
          IF (ANIM_CRK > 0) THEN
             CALL TENSORC_CRK(
     .               ELBUF_TAB,XFEM_TAB ,IPARG    ,IPM         ,
     .               IFUNC    ,INVERT   ,EL2FA_CRK,NBF_CRKXFEMG,
     .               LEN_CRKX ,TANI     ,IAD      ,NBF_CRKXFEM ,
     .               NBPART   ,IAD_CRKG ,X        ,IXC         ,
     .               IGEO     ,IXTG     ,IEL_CRK  ,IADC_CRK    ,
     .               CRKEDGE  ,INDX_CRK )
          ENDIF
C
          IF (ISPMD==0.AND.NFVTR>0) THEN
            R4 = ZERO
            DO J=1,NFVTR
               CALL WRITE_R_C(R4,1)
               CALL WRITE_R_C(R4,1)
               CALL WRITE_R_C(R4,1)
            ENDDO
          ENDIF
        ENDIF
      ENDDO

C-----------------------------------------------
C   ELEMENT MASS
C-----------------------------------------------
      IF(ANIM_M==1)THEN
C traitement equivalent monoproc
       IF(NSPMD == 1) THEN
         DO I=1,NBF
            R4 = MAS(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
C xfem crack for layered shells +++
cc         IF(NBF_CRKXFEM > 0) THEN
cc           DO I=1,NBF_CRKXFEM     
cc            R4 = ZERO
cc            CALL WRITE_R_C(R4,1)
cc           ENDDO
cc         ENDIF
C xfem crack for layered shells ---
       ELSE
         DO I = 1, NBF_L
           MAS4(I) = MAS(I)
         ENDDO
         IF(ISPMD==0) THEN
           BUF = (NUMELQG+NUMELCG+NUMELTGG)
         ELSE
           BUF=1
         END IF
         CALL SPMD_R4GET_PARTN(1,NBF_L,NBPART,IADG,MAS4,BUF)
       ENDIF

       IF (ISPMD==0) THEN
         R4 = 0.
         DO J=1,NESBW2+NELCUT
          CALL WRITE_R_C(R4,1)
         ENDDO
       ENDIF
C rajouter la masse s'il le faut
       IF(ISPMD == 0 .AND. ANIM_PLY > 0 ) THEN
           R4 = ZERO
           DO I=1,NBF_PXFEMG
              CALL WRITE_R_C(R4,1)
           ENDDO
       ENDIF       
C xfem crack for layered shells +++
       IF(ISPMD == 0 .AND. ANIM_CRK > 0)THEN
           R4 = ZERO
           DO I=1,NBF_CRKXFEMG
             CALL WRITE_R_C(R4,1)
           ENDDO
       ENDIF
C xfem crack for layered shells ---
       IF (ISPMD==0.AND.NFVTR>0) THEN
         R4=ZERO
         DO J=1,NFVTR
            CALL WRITE_R_C(R4,1)
         ENDDO
       ENDIF

C-----------------------------------------------
C   NODAL MASS (FLUX FOR CUT)
C-----------------------------------------------
         DO I=1,NUMNOD
           IF (WEIGHT_MD(I)==1) THEN
              WA4(I)=MS(I)
           ELSE
              WA4(I)=ZERO
           END IF
         ENDDO
C
         DO N=1,NRBYKIN
           M=NPBY(1,N)
           IF (M>0) THEN
             WA4(M)=WA4(M)+(RBY(15,N)-MS(M))* WEIGHT_MD(M)
           ENDIF
         ENDDO
C traitement equivalent mono
         IF (NSPMD == 1) THEN
           DO K=1,NUMNOD
             R4 = WA4(K)
             CALL WRITE_R_C(R4,1)
           ENDDO
         ELSE
           IF (ISPMD==0) THEN
             BUF = NUMNODG
           ELSE
             BUF = 1
           ENDIF
           CALL SPMD_GATHERF(WA4,WEIGHT,NODGLOB,BUF)
         ENDIF

       R4 = ZERO
       IF(NODCUT>0)
     . CALL CUTMASS(ICBUF,CBUF,CBUF(MAC2),NODCUT,NELCUT,
     .   V,CBUF(MAC3),ICBUF(MIC2))
       IF (ISPMD==0) THEN
         R4 = 0.
c           SZ16 = NUMELS16
c           SZNNSPH = NNSPH
           SZ16 = NUMELS16G
           SZNNSPH = NNSPHG
         DO N=1,NSECT+NRWALL+NNWL+NNSRG+NNSMD+SZNNSPH+2*SZ16
           CALL WRITE_R_C(R4,1)
         ENDDO
       ENDIF
C nodal/ply mass is not computed for anim is always zero
       IF(ISPMD==0 .AND. ANIM_PLY > 0 ) THEN
          R4=ZERO
          DO I=1,NFNOD_PXFEMG
              CALL WRITE_R_C(R4,1)  
          ENDDO
       ENDIF
C xfem crack for layered shells +++
       IF(ISPMD==0 .AND. ANIM_CRK > 0)THEN
          R4=ZERO
          DO I=1,NFNOD_CRKXFEMG
            CALL WRITE_R_C(R4,1)  
          ENDDO
       ENDIF
C xfem crack for layered shells ---
       IF (ISPMD==0.AND.NFVNOD>0) THEN
          R4=ZERO
          DO N=1,NFVNOD+3
             CALL WRITE_R_C(R4,1)
          ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------
C   NODAL and ELEMENT NUMBERING
C-----------------------------------------------
C traitement equivalent monoproc
      IF (NSPMD == 1) THEN
        CALL WRITE_I_C(ITAB,NUMNOD)
        IF (NUMELIG3D>0) THEN
          DO I=1,64*NUMELIG3D
            CALL WRITE_I_C(FIRST_NODE_IG3D+I,1)
          ENDDO
        ENDIF
      ELSE
        IF (ISPMD==0) THEN
          BUF = NUMNODG
        ELSE
          BUF = 1
        ENDIF
          CALL SPMD_GATHERITAB(ITAB,WEIGHT,NODGLOB,BUF)
      ENDIF

c         SZ16 = NUMELS16
c         SZNNSPH = NNSPH
         SZ16 = NUMELS16G
         SZNNSPH = NNSPHG

      IF (ISPMD==0) THEN
         DO I=1,NODCUT+NSECT+NRWALL+NNWL+NNSRG+NNSMD+SZNNSPH+2*SZ16
          CALL WRITE_I_C(0,1)
         ENDDO
      ENDIF
C IDMAX a revoir pour VMBAG
C  ----
       IF(ANIM_PLY > 0) THEN
        IF (NSPMD == 1) THEN
            II = 0
            ALLOCATE(ITAB_PLY(NPLYXFE))
            ITAB_PLY =0
            DO J=1,NPLYPART
               IPLY = INDX_PLY(J)
               NN  = PLYNOD(IPLY)%PLYNUMNODS
               DO I=1,NN
                  ITAB_PLY(I) = IDMAX  + II + I
               ENDDO
              CALL WRITE_I_C(ITAB_PLY,NN)
              II = II + NN
            ENDDO
            DEALLOCATE(ITAB_PLY)
            IDMAX =  IDMAX + NFNOD_PXFEM
        ELSE
          IF (ISPMD==0)THEN
            ALLOCATE(ITAB_PLY(NFNOD_PXFEMG))
            II = 0
            ITAB_PLY =0
            DO J=1,NFNOD_PXFEMG
              ITAB_PLY(J) = IDMAX + J
            ENDDO
c            DO J=1,NPLYMAX
c               NN  = PLYNODG(J)%PLYNUMNODS
c               DO I=1,NN
c                  ITAB_PLY(I) = IDMAX  + II + I
              CALL WRITE_I_C(ITAB_PLY,NFNOD_PXFEMG)
c              II = II + NN
            DEALLOCATE(ITAB_PLY)
            IDMAX =  IDMAX + NFNOD_PXFEMG
          ENDIF
        ENDIF
       ENDIF
C xfem crack for layerd shells +++
       IF(ANIM_CRK > 0)THEN
        IF(NSPMD == 1)THEN
            II = 0
            ALLOCATE(ITAB_CRK(NFNOD_CRKXFEMG))
            ITAB_CRK =0
           DO ICRK=1,NCRKPART
             DO I=1,CRKNOD(ILEV)%CRKNUMNODS
               J = CRKNOD(ILEV)%CRKNUMNODS*(ICRK-1)
               K = NODGLOBXFE(I+J)
               ITAB_CRK(K) = CRKNOD(ICRK)%XFECRKNODID(I)+IDMAX
               CALL WRITE_I_C(ITAB_CRK(K),1)
             ENDDO
           ENDDO
           DEALLOCATE(ITAB_CRK)
        ELSE
          IF(ISPMD==0)THEN
            BUF = NFNOD_CRKXFEMG
          ELSE
            BUF = 1
          END IF
          DO ICRK=1,NCRKPART
            CALL SPMD_GATHERITAB_CRK(ICRK,BUF,IDMAXNOD,NODGLOBXFE)
          END DO
        ENDIF
       ELSE
         ALLOCATE(ITAB_CRK(0))
       ENDIF
C xfem crack for layerd shells ---
C  Traitment equivalent monoproc
      IF (NSPMD == 1 .AND. NFVNOD>0) THEN
         DO I=1,NFVBAG
            IF (FVDATA(I)%NPOLH_ANIM>0) THEN
               DO J=1,FVDATA(I)%NNS_ANIM
                  JJ=FVOFF(2,I)+J
                  CALL WRITE_I_C(JJ,1)
               ENDDO
            ENDIF
         ENDDO
         CALL WRITE_I_C(IDMAX+NFVNOD+1,1)
         CALL WRITE_I_C(IDMAX+NFVNOD+2,1)
         CALL WRITE_I_C(IDMAX+NFVNOD+3,1)
      ELSEIF(NFVNOD>0) THEN
         CALL SPMD_FVB_ANUM(FVOFF, IDMAX, NFVNOD)
      ENDIF
C
      CALL DELNUMBC(IPARG,IXQ  ,IXC   ,IXTG ,INVERT,
     .             EL2FA,NBF  ,WAFT ,NELCUT,
     .             DD_IAD,IAD ,NBF_L,
     .             NBPART,IADG,NODGLOB,IDCMAX)
      IF (ISPMD==0) THEN
         DO J=1,NESBW2
          CALL WRITE_I_C(0,1)
         ENDDO
      ENDIF
C

      IF(ANIM_PLY > 0 ) THEN
          NEL_PLY= 0
          CALL DELNUMBC_PLY(IPLY,NEL_PLY,IPARG,
     .             IXC   ,IXTG ,INVERT,
     .             EL2FA_PLY,NBF_PXFEM  ,WAFT_PLY ,NELCUT,
     .             DD_IAD,IAD ,PLYNUMC,
     .             NBPART,IAD_PLYG,NODGLOB,IDCMAX,NBF_PXFEMG)
       ENDIF
C
      IF (ANIM_CRK > 0) THEN
          CALL DELNUMBC_CRK(
     .         IPARG    ,IEL_CRK  ,WAFT_CRK   ,IDCMAX      ,
     .         EL2FA_CRK,IAD_CRKG ,NBF_CRKXFEM,NBF_CRKXFEMG,INDX_CRK)
       ENDIF
C
      IF (ISPMD==0.AND.NFVTR>0) THEN
         DO I=1,NFVTR
            CALL WRITE_I_C(IDCMAX+FVINUM(I),1)
         ENDDO
         DEALLOCATE(FVEL2FA, FVINUM)
      ENDIF
C-----------------------------------------------
C   HIERARCHY
C-----------------------------------------------
       IF (NFVPART>0) THEN
          IF (ISPMD==0) ALLOCATE(FVPBUF(NFVPART))
          II=NSUBS
     .        +MIN(1,NSECT)+MIN(1,NRBODY+ NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .        +MIN(1,NSURG+NSMAD) + MIN(1,NPLYPARTW)
     .        +MIN(1,NCRKPARTW)-1
          IF(NSPMD > 1) CALL SPMD_FVB_ASUB1(II, FVPBUF)
       ENDIF
C
       IF (ISPMD==0) THEN
C       Transmis a ANIM ::
clm      Subset Rbodies  == NSUBS
Cclm     Subset Sections == NSUBS+MIN(1,NRBODY+NRBE2+NRBE3)
clm      Subset Rwalls   == NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2+NRBE3)
C       Subset Surfaces == NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2+NRBE3)+MIN(1,NRWALL)
clm      Subset global   == NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2+NRBE3)+MIN(1,NRWALL)
C                                +MIN(1,NSURG+NSMAD) + MIN(1,NPLYPART)
         DO I=1,NPART
           IF(MATER(I)==1) THEN
            IF (IPART(3,I)<NSUBS) THEN
             CALL WRITE_I_C(IPART(3,I)-1,1)
            ELSE
             CALL WRITE_I_C(NSUBS
     .                      +MIN(1,NSECT)+MIN(1,NRBODY+ NRBE2T+NRBE3T)
     .                      +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)
     .                      +NFVSUBS+MIN(1,NPLYPARTW)
     .                      +MIN(1,NCRKPARTW)-1,1)
            END IF
           END IF
         ENDDO
         DO I=1,NCUTS
           CALL WRITE_I_C(NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .          +MIN(1,NSURG+NSMAD)+MIN(1,NPLYPARTW)
     .          +MIN(1,NCRKPARTW)-1,1)
         ENDDO
         DO I=1,NSECT
            CALL WRITE_I_C(NSUBS+MIN(1,NRBODY+NRBE2T+NRBE3T)-1,1)
         END DO
         DO I=1,NRWALL
           CALL WRITE_I_C(NSUBS+MIN(1,NSECT)
     .                    +MIN(1,NRBODY+NRBE2T+NRBE3T)-1,1)
         END DO
         DO I=1,NSURG
           CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+
     .                         NRBE3T)+MIN(1,NRWALL)-1,1)
         END DO
         DO I=1,NSMAD
           CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+
     .                         NRBE3T)+MIN(1,NRWALL)-1,1)
         END DO
c plyxfem
         IF(ANIM_PLY > 0) THEN
          IF (ISPMD==0)THEN
           II = NSUBS + MIN(1,NSECT) + MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                + MIN(1,NRWALL)+ MIN(1,NSURG+NSMAD) - 1
c
           DO I= 1,NPLYPART
             CALL WRITE_I_C(II  ,1)
           ENDDO
          ENDIF
         ENDIF
c xfem crack for layered shells +++
         IF(ANIM_CRK > 0)THEN
          IF (ISPMD==0)THEN
           II = NSUBS + MIN(1,NSECT) + MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                + MIN(1,NRWALL)+ MIN(1,NSURG+NSMAD) - 1      
           DO I= 1,NCRKPARTW
             CALL WRITE_I_C(II  ,1)
           ENDDO
          ENDIF
         ENDIF
C xfem crack for layered shells ---
C traitement equivalent monoproc
         IF (NSPMD == 1.AND.NFVTR>0) THEN
            II=NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .          +MIN(1,NSURG+NSMAD) +MIN(NPLYPARTW,1)
     .          +MIN(1,NCRKPARTW)-1
            DO I=1,NFVBAG
               IF (FVDATA(I)%NPOLH_ANIM>0) THEN
                  II=II+1
                  DO J=1,FVDATA(I)%NPOLH_ANIM
                     CALL WRITE_I_C(II-1,1)
                  ENDDO
               ENDIF
            ENDDO
         ELSEIF (NFVPART>0) THEN
            CALL WRITE_I_C(FVPBUF, NFVPART)
            DEALLOCATE(FVPBUF)
         ENDIF
         DO I=1,NPART
           IF(MATER(I)==1)CALL WRITE_I_C(IPART(1,I),1)
         ENDDO
         DO I=1,NCUTS+NRWALL+NSECT+NSURG+NSMAD+NPLYPARTW+NCRKPARTW
             CALL WRITE_I_C(0,1)
         ENDDO
C
         DO I=1,NFVPART
               CALL WRITE_I_C(0,1)
         ENDDO
C
         DO I=1,NPART
           IF(MATER(I)==1)CALL WRITE_I_C(IPART(2,I),1)
         ENDDO
         DO I=1,NCUTS+NRWALL+NSECT+NSURG+NSMAD+NPLYPARTW+NCRKPARTW
           CALL WRITE_I_C(0,1)
         ENDDO
         DO I=1,NFVPART
            CALL WRITE_I_C(0,1)
         ENDDO
       ENDIF
C=======================================================================
C
C      BRICKS
C
C=======================================================================
      IF(NUMELS_T+NUMELS16_T+ISPH3D*(NUMSPH_T+MAXPJET)+NUMELIG3D==0)
     .     GOTO 400
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
        DO I=1,NPART
          BUFFERP(I) = MATER(I)
          MATER(I) = 0
        ENDDO
C
C
        DO NG = 1, NGROUP
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          ITY   =IPARG(5,NG)
          IF(ITY==1)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTS(N))=2
            ENDDO
C     3D geometry is not yet treated.
          ELSEIF (ISPH3D==1.AND.ITY==51)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTSP(N))=2
            ENDDO
          ELSEIF (ITY==101)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTIG3D(N))=2
            ENDDO
          ENDIF
        ENDDO
C
        IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(MATER,NPART)
        DO I=1,NPART
          IF(MATER(I)>2) MATER(I) = 2
        ENDDO
        IF(NSPMD > 1) CALL SPMD_IBCAST(MATER,MATER,NPART,1,0,2)
        DO I=1,NPART
          MATER(I) = MATER(I)+BUFFERP(I)
        ENDDO

C
      NBPART = 0
      DO I=1,NPART
        NBPART = NBPART + MATER(I)/2
      ENDDO

C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      IF (ISPMD==0) THEN
        CALL WRITE_I_C(NUMELS_T+3*NUMELS16G+ISPH3D*NUMSPHG+27*NUMELIG3D,1)
        CALL WRITE_I_C(NBPART,1)
        CALL WRITE_I_C(NSE_ANI,1)
        CALL WRITE_I_C(NST_ANI,1)
      ENDIF

C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
       SHFTSPH = NUMNODG+NODCUT+NSECT+NRWALL+NNWL+NNSRG+NNSMD
       SHFTSPH = SHFTSPH + SPHSHIFT
       SHFT16 = NUMNODG+NODCUT+NSECT+NRWALL+NNWL+NNSRG+NNSMD+NNSPHG
       SHFT16 = SHFT16 + NUM16SHIFT
C  index debut de zone des noeuds sur tetras SPH.
       INSPH=NUMNOD+NODCUT+NSECT+NRWALL+NNWL+NNSRG+NNSMD

      CALL PARSORS(IAD  ,IPARG     ,IXS    ,MATER,IPARTS,
     2            EL2FA ,                   DD_IAD,
     3            IADG            ,INSPH  ,KXSP  ,IPARTSP,
     4            IXS10 ,IXS20    ,IXS16  ,NNSPH ,ISPH3D,
     5            NODGLOB,SHFT16  ,SHFTSPH,NNSPHG,IPARTIG3D,
     6            KXIG3D,IGEO,IG3DSOLID)
C-----------------------------------------------
C   OFF
C-----------------------------------------------
      NNN = NUMELS+ISPH3D*(NUMSPH+MAXPJET)+3*NUMELS16+27*NUMELIG3D
      CALL ANIOFFS(ELBUF_TAB,IPARG,WAFT  ,EL2FA  ,NNN      ,
     .             NBPART ,IADG ,ISPH3D )
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
        IF (ISPMD==0) THEN
          DO I = 1, NBPART
            BUFFERP(I) = 0
            DO K = 1, NSPMD
              BUFFERP(I) = BUFFERP(I) + IADG(K,I)
            ENDDO
          ENDDO
          CALL WRITE_I_C(BUFFERP,NBPART)
        ENDIF

C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
      IF (ISPMD==0) THEN
       DO I=1,NPART
        IF(MATER(I)==2)THEN
          WRITE(STR,'(I9,A1)')IPART(4,I),':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10
            CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),40)
            DO J=1,LTITL
              IF(TITL(J:J)/=' ') IB = J+10
              CTEXT(J+10)=ICHAR(TITL(J:J))
            END DO
            CTEXT(IB+1)=0
            CALL WRITE_C_C(CTEXT,10+LTITL)
        ENDIF
       ENDDO
      ENDIF

C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M==1.OR.ANIM_SE(3)==1.OR.ANIM_SE(25)==1)THEN
        CALL DMASANIS(ELBUF_TAB,X       ,D       ,GEO     ,IPARG   ,
     .                IXS      ,MAS     ,PM      ,EL2FA   ,NUMELS  ,
     .                IPART    ,IPARTSP ,ISPH3D )
      ENDIF
C-----------------------------------------------
C   BRICK FUNC TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
       CTEXT(81)=0
       IF(ANIM_SE(1)==1) CALL ANI_TXT('Plastic Strain',14)
       IF(ANIM_SE(2)==1) CALL ANI_TXT('Density',7)
       IF(ANIM_SE(3)==1) CALL ANI_TXT('Specific Energy',15)
       IF(ANIM_SE(4)==1) CALL ANI_TXT('Temperature',11)
       IF(ANIM_SE(6)==1) CALL ANI_TXT('Pressure',8)
       IF(ANIM_SE(7)==1) CALL ANI_TXT('Von Mises',9)
       IF(ANIM_SE(8)==1) CALL ANI_TXT('Turbulent Energy',16)
       IF(ANIM_SE(9)==1) CALL ANI_TXT('Turbulent Viscosity',19)
       IF(ANIM_SE(10)==1) CALL ANI_TXT('Vorticity-X',11)
       IF(ANIM_SE(11)==1) CALL ANI_TXT('Damage 1',8)
       IF(ANIM_SE(12)==1) CALL ANI_TXT('Damage 2',8)
       IF(ANIM_SE(13)==1) CALL ANI_TXT('Damage 3',8)
       IF(ANIM_SE(14)==1) CALL ANI_TXT('Stress X ',9)
       IF(ANIM_SE(15)==1) CALL ANI_TXT('Stress Y ',9)
       IF(ANIM_SE(16)==1) CALL ANI_TXT('Stress Z ',9)
       IF(ANIM_SE(17)==1) CALL ANI_TXT('Stress XY',9)
       IF(ANIM_SE(18)==1) CALL ANI_TXT('Stress YZ',9)
       IF(ANIM_SE(19)==1) CALL ANI_TXT('Stress ZX',9)
       IF(ANIM_SE(20)==1) CALL ANI_TXT('User Var 1',10)
       IF(ANIM_SE(21)==1) CALL ANI_TXT('User Var 2',10)
       IF(ANIM_SE(22)==1) CALL ANI_TXT('User Var 3',10)
       IF(ANIM_SE(23)==1) CALL ANI_TXT('User Var 4',10)
       IF(ANIM_SE(24)==1) CALL ANI_TXT('User Var 5',10)       
       IF(ANIM_SE(25)==1) CALL ANI_TXT('Hourglass Energy per unit mass',30)
       IF(ANIM_SE(26)==1) CALL ANI_TXT('Strain Rate',11)
       IF(ANIM_SE(27)==1) CALL ANI_TXT('User Var 6',10)
       IF(ANIM_SE(28)==1) CALL ANI_TXT('User Var 7',10)
       IF(ANIM_SE(29)==1) CALL ANI_TXT('User Var 8',10)
       IF(ANIM_SE(30)==1) CALL ANI_TXT('User Var 9',10)
       IF(ANIM_SE(31)==1) CALL ANI_TXT('User Var 10',11)
       IF(ANIM_SE(32)==1) CALL ANI_TXT('User Var 11',11)
       IF(ANIM_SE(33)==1) CALL ANI_TXT('User Var 12',11)
       IF(ANIM_SE(34)==1) CALL ANI_TXT('User Var 13',11)
       IF(ANIM_SE(35)==1) CALL ANI_TXT('User Var 14',11)
       IF(ANIM_SE(36)==1) CALL ANI_TXT('User Var 15',11)
       IF(ANIM_SE(37)==1) CALL ANI_TXT('User Var 16',11)
       IF(ANIM_SE(38)==1) CALL ANI_TXT('User Var 17',11)
       IF(ANIM_SE(39)==1) CALL ANI_TXT('User Var 18',11)
       IF(ANIM_SE(40)==1) CALL ANI_TXT('User Var 19',11)
       IF(ANIM_SE(41)==1) CALL ANI_TXT('User Var 20',11)
       IF(ANIM_SE(42)==1) CALL ANI_TXT('User Var 21',11)
       IF(ANIM_SE(43)==1) CALL ANI_TXT('User Var 22',11)
       IF(ANIM_SE(44)==1) CALL ANI_TXT('User Var 23',11)
       IF(ANIM_SE(45)==1) CALL ANI_TXT('User Var 24',11)
       IF(ANIM_SE(46)==1) CALL ANI_TXT('User Var 25',11)
       IF(ANIM_SE(47)==1) CALL ANI_TXT('User Var 26',11)
       IF(ANIM_SE(48)==1) CALL ANI_TXT('User Var 27',11)
       IF(ANIM_SE(49)==1) CALL ANI_TXT('User Var 28',11)
       IF(ANIM_SE(50)==1) CALL ANI_TXT('User Var 29',11)
       IF(ANIM_SE(51)==1) CALL ANI_TXT('User Var 30',11)
       IF(ANIM_SE(52)==1) CALL ANI_TXT('User Var 31',11)
       IF(ANIM_SE(53)==1) CALL ANI_TXT('User Var 32',11)
       IF(ANIM_SE(54)==1) CALL ANI_TXT('User Var 33',11)
       IF(ANIM_SE(55)==1) CALL ANI_TXT('User Var 34',11)
       IF(ANIM_SE(56)==1) CALL ANI_TXT('User Var 35',11)
       IF(ANIM_SE(57)==1) CALL ANI_TXT('User Var 36',11)
       IF(ANIM_SE(58)==1) CALL ANI_TXT('User Var 37',11)
       IF(ANIM_SE(59)==1) CALL ANI_TXT('User Var 38',11)
       IF(ANIM_SE(60)==1) CALL ANI_TXT('User Var 39',11)
       IF(ANIM_SE(61)==1) CALL ANI_TXT('User Var 40',11)
       IF(ANIM_SE(62)==1) CALL ANI_TXT('User Var 41',11)
       IF(ANIM_SE(63)==1) CALL ANI_TXT('User Var 42',11)
       IF(ANIM_SE(64)==1) CALL ANI_TXT('User Var 43',11)
       IF(ANIM_SE(65)==1) CALL ANI_TXT('User Var 44',11)
       IF(ANIM_SE(66)==1) CALL ANI_TXT('User Var 45',11)
       IF(ANIM_SE(67)==1) CALL ANI_TXT('User Var 46',11)
       IF(ANIM_SE(68)==1) CALL ANI_TXT('User Var 47',11)
       IF(ANIM_SE(69)==1) CALL ANI_TXT('User Var 48',11)
       IF(ANIM_SE(70)==1) CALL ANI_TXT('User Var 49',11)
       IF(ANIM_SE(71)==1) CALL ANI_TXT('User Var 50',11)
       IF(ANIM_SE(72)==1) CALL ANI_TXT('User Var 51',11)
       IF(ANIM_SE(73)==1) CALL ANI_TXT('User Var 52',11)
       IF(ANIM_SE(74)==1) CALL ANI_TXT('User Var 53',11)
       IF(ANIM_SE(75)==1) CALL ANI_TXT('User Var 54',11)
       IF(ANIM_SE(76)==1) CALL ANI_TXT('User Var 55',11)
       IF(ANIM_SE(77)==1) CALL ANI_TXT('User Var 56',11)
       IF(ANIM_SE(78)==1) CALL ANI_TXT('User Var 57',11)
       IF(ANIM_SE(79)==1) CALL ANI_TXT('User Var 58',11)
       IF(ANIM_SE(80)==1) CALL ANI_TXT('User Var 59',11)
       IF(ANIM_SE(81)==1) CALL ANI_TXT('User Var 60',11)
       !-------------------------------------------------------------!
       DO I=82,281
         IF(ANIM_SE(I)==1)THEN
           II = I - 81
           WRITE(MES,'(A,I3)')
     .         'WPLA layer',II
           CALL ANI_TXT(MES,20)
         ENDIF
       ENDDO
       !-------------------------------------------------------------!
       IF(ANIM_SE(282)==1) CALL ANI_TXT('Failed layers',13)
       !-------------------------------------------------------------!
       !283 to 286 : VOLUMETRIC FRACTION FOR ALE MULTIMATERIAL LAWS
       !  radioss2.F: check consistency with engine keyword and occurrence material law. ANIM_SE(.) flag is enabled only if consistent
       !  dfunc6.F : corresponding value (in MBUF%VAR) is stored in this function according material law number
       IF(ANIM_SE(283)==1) CALL ANI_TXT('Volumetric Fraction 1',21)        !law51 & law37 & law20
       IF(ANIM_SE(284)==1) CALL ANI_TXT('Volumetric Fraction 2',21)        !law37 & law37 & law20
       IF(ANIM_SE(285)==1) CALL ANI_TXT('Volumetric Fraction 3',21)        !law51       
       IF(ANIM_SE(286)==1) CALL ANI_TXT('Volumetric Fraction 4',21)        !law51      
       !-------------------------------------------------------------!     
         
       DO I=1,200
        IF(ANIM_SE(286+3*(I-1)+1)==1)THEN
          WRITE(MES,'(A,I3,A)')'Psi (layer',I,')'
          CALL ANI_TXT(MES,15)
        ENDIF
        IF(ANIM_SE(286+3*(I-1)+2)==1)THEN
          WRITE(MES,'(A,I3,A)')'Teta (layer',I,')'
          CALL ANI_TXT(MES,16)
        ENDIF
        IF(ANIM_SE(286+3*(I-1)+3)==1)THEN
          WRITE(MES,'(A,I3,A)')'Phi (layer',I,')'
          CALL ANI_TXT(MES,15)
        ENDIF
       ENDDO
 
       IF(ANIM_SE(887)==1) CALL ANI_TXT('Burn Fraction',13)        !law51                
       IF(ANIM_SE(888)==1) CALL ANI_TXT('Damage variable1',16)                       
       IF(ANIM_SE(889)==1) CALL ANI_TXT('Damage variable2',16)                       
       IF(ANIM_SE(890)==1) CALL ANI_TXT('Damage variable3',16)  
       DO I=1, 999
        IF(ANIM_SE(890+I)==1)THEN
          WRITE(MES,'(A,I3,A)')'Damage var1 Intg Point (',I,')'
          CALL ANI_TXT(MES,30)
        ENDIF
       ENDDO                     
       DO I=1, 999
        IF(ANIM_SE(1890+I)==1)THEN
          WRITE(MES,'(A,I3,A)')'Damage var2 Intg Point (',I,')'
          CALL ANI_TXT(MES,30)
        ENDIF
       ENDDO                     
       DO I=1, 999
        IF(ANIM_SE(2890+I)==1)THEN
          WRITE(MES,'(A,I3,A)')'Damage var3 Intg Point (',I,')'          
          CALL ANI_TXT(MES,30)
        ENDIF
       ENDDO   
       IF(ANIM_SE(3890)==1) CALL ANI_TXT('MAX DAMAGE ELEMENT',18) 

       DO I=1, 999
        IF(ANIM_SE(3890+I)==1)THEN
          WRITE(MES,'(A,I3,A)')'MAX DAMAGE Intg Point (',I,')'          
          CALL ANI_TXT(MES,30)
        ENDIF
       ENDDO  
       DO I=1, 4010
        IF(ANIM_SE(5910+I)==1)THEN
            II = I +5910-3890
            WRITE(MES,'(A,3I3)')
     .          'MAX DAMAGE Intg Pt ',ABS(II)/2010,
     .           MOD(ABS(II)/10,201),MOD(ABS(II),10)
          CALL ANI_TXT(MES,28)
        ENDIF
       ENDDO  
       IF(ANIM_SE(4890)==1) CALL ANI_TXT('TIME DELETION ELEMENT',21)                     
       IF(ANIM_SE(4891)==1) CALL ANI_TXT('Sound Speed',11)                            
       IF(ANIM_SE(4892)==1) CALL ANI_TXT('Schlieren',9)                               
       IF(ANIM_SE(4893)==1) CALL ANI_TXT('Domain',6)         
       IF(ANIM_SE(4894)==1) CALL ANI_TXT('Filling percentage',18)         
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
        IF(ANIM_SE(4895)==1)CALL ANI_TXT('Equiv stress',12)
        IF(ANIM_SE(4896)==1)CALL ANI_TXT('Artificial Viscosity',20) 

C-----------------------------------------------
C   LAW51 OUTPUTS
C-----------------------------------------------
        IF(ANIM_SE(4897)==1)CALL ANI_TXT('Density-1',9) 
        IF(ANIM_SE(4898)==1)CALL ANI_TXT('Density-2',9) 
        IF(ANIM_SE(4899)==1)CALL ANI_TXT('Density-3',9) 
        IF(ANIM_SE(4900)==1)CALL ANI_TXT('Density-4',9)                         

        IF(ANIM_SE(4901)==1)CALL ANI_TXT('Specific Energy-1',17)                         
        IF(ANIM_SE(4902)==1)CALL ANI_TXT('Specific Energy-2',17)                         
        IF(ANIM_SE(4903)==1)CALL ANI_TXT('Specific Energy-3',17)                         
        IF(ANIM_SE(4904)==1)CALL ANI_TXT('Specific Energy-4',17)                         

        IF(ANIM_SE(4905)==1)CALL ANI_TXT('Temperature-1',13)     
        IF(ANIM_SE(4906)==1)CALL ANI_TXT('Temperature-2',13)     
        IF(ANIM_SE(4907)==1)CALL ANI_TXT('Temperature-3',13)     
        IF(ANIM_SE(4908)==1)CALL ANI_TXT('Temperature-4',13)                             
        
        IF(ANIM_SE(4909)==1)CALL ANI_TXT('Pressure-1',10)                             
        IF(ANIM_SE(4910)==1)CALL ANI_TXT('Pressure-2',10)                             
        IF(ANIM_SE(4911)==1)CALL ANI_TXT('Pressure-3',10)                             
        IF(ANIM_SE(4912)==1)CALL ANI_TXT('Pressure-4',10)                                                     

        IF(ANIM_SE(4913)==1)CALL ANI_TXT('Plastic Strain-1',16) 
        IF(ANIM_SE(4914)==1)CALL ANI_TXT('Plastic Strain-2',16) 
        IF(ANIM_SE(4915)==1)CALL ANI_TXT('Plastic Strain-3',16) 
        IF(ANIM_SE(4916)==1)CALL ANI_TXT('Plastic Strain-4',16) 

        IF(ANIM_SE(4917)==1)CALL ANI_TXT('Sound Speed-1',13) 
        IF(ANIM_SE(4918)==1)CALL ANI_TXT('Sound Speed-2',13) 
        IF(ANIM_SE(4919)==1)CALL ANI_TXT('Sound Speed-3',13) 
        IF(ANIM_SE(4920)==1)CALL ANI_TXT('Sound Speed-4',13)                         
        
        IF(ANIM_SE(4921)==1)CALL ANI_TXT('Volume',6)  !THIS ONE IS A GLOBAL OUTPUT                               

        IF(ANIM_SE(4922)==1)CALL ANI_TXT('Volume-1',8)                                 
        IF(ANIM_SE(4923)==1)CALL ANI_TXT('Volume-2',8)                                 
        IF(ANIM_SE(4924)==1)CALL ANI_TXT('Volume-3',8)                                 
        IF(ANIM_SE(4925)==1)CALL ANI_TXT('Volume-4',8)                                                                 

        IF(ANIM_SE(4926)==1)CALL ANI_TXT('Mass-1',6)                                 
        IF(ANIM_SE(4927)==1)CALL ANI_TXT('Mass-2',6)                                 
        IF(ANIM_SE(4928)==1)CALL ANI_TXT('Mass-3',6)                                 
        IF(ANIM_SE(4929)==1)CALL ANI_TXT('Mass-4',6)                   
                                                      
        IF(ANIM_SE(4930)==1)CALL ANI_TXT('Detonation Time',15) 
        
        IF(ANIM_SE(4931)==1)CALL ANI_TXT('Artificial Viscosity-1',22) 
        IF(ANIM_SE(4932)==1)CALL ANI_TXT('Artificial Viscosity-2',22) 
        IF(ANIM_SE(4933)==1)CALL ANI_TXT('Artificial Viscosity-3',22)                         
        IF(ANIM_SE(4934)==1)CALL ANI_TXT('Artificial Viscosity-4',22)  

        IF(ANIM_SE(4935)==1)CALL ANI_TXT('Density of Liquid - (law37)',27)                         
        IF(ANIM_SE(4936)==1)CALL ANI_TXT('Density of Gas    - (law37)',27)  

        IF(ANIM_SE(4937)==1)CALL ANI_TXT('Element Time Step',17)
        
        IF(ANIM_SE(4938)==1)CALL ANI_TXT('Momentum Density X  ',20)
        IF(ANIM_SE(4939)==1)CALL ANI_TXT('Momentum Density Y  ',20)
        IF(ANIM_SE(4940)==1)CALL ANI_TXT('Momentum Density Z  ',20)
        IF(ANIM_SE(4941)==1)CALL ANI_TXT('Momentum Density XY ',20)
        IF(ANIM_SE(4942)==1)CALL ANI_TXT('Momentum Density YZ ',20)
        IF(ANIM_SE(4943)==1)CALL ANI_TXT('Momentum Density XZ ',20)  
        IF(ANIM_SE(4944)==1)CALL ANI_TXT('Momentum Density ABS',20) 
        
        IF(ANIM_SE(4945)==1)CALL ANI_TXT('Velocity X  ',12)
        IF(ANIM_SE(4946)==1)CALL ANI_TXT('Velocity Y  ',12)
        IF(ANIM_SE(4947)==1)CALL ANI_TXT('Velocity Z  ',12)
        IF(ANIM_SE(4948)==1)CALL ANI_TXT('Velocity XY ',12)
        IF(ANIM_SE(4949)==1)CALL ANI_TXT('Velocity YZ ',12)
        IF(ANIM_SE(4950)==1)CALL ANI_TXT('Velocity XZ ',12)  
        IF(ANIM_SE(4951)==1)CALL ANI_TXT('Velocity ABS',12) 
        
        IF(ANIM_SE(4952)==1)CALL ANI_TXT('Internal Force X  ',18)
        IF(ANIM_SE(4953)==1)CALL ANI_TXT('Internal Force Y  ',18)
        IF(ANIM_SE(4954)==1)CALL ANI_TXT('Internal Force Z  ',18)
        IF(ANIM_SE(4955)==1)CALL ANI_TXT('Internal Force XY ',18)
        IF(ANIM_SE(4956)==1)CALL ANI_TXT('Internal Force YZ ',18)
        IF(ANIM_SE(4957)==1)CALL ANI_TXT('Internal Force XY ',18)
        IF(ANIM_SE(4958)==1)CALL ANI_TXT('Internal Force ABS',18)
C
        IF(ANIM_SE(4959)==1)CALL ANI_TXT('AMS selection',13)         
        
        IF(ANIM_SE(4960)==1) CALL ANI_TXT('Vorticity-Y',11)
        IF(ANIM_SE(4961)==1) CALL ANI_TXT('Vorticity-Z',11)
        IF(ANIM_SE(4962)==1) CALL ANI_TXT('Vorticity',9) 
        IF(ANIM_SE(4963)==1) CALL ANI_TXT('Internal Energy',15)                                      
         !/ANIM/ELEM/WPLA
        IF(ANIM_SE(4964)==1)CALL ANI_TXT('Plastic Work',12) 
        IF(ANIM_SE(4965)==1)CALL ANI_TXT('Element status',14)
        IF(ANIM_SE(4966)==1)CALL ANI_TXT('Mach Number',11)
        IF(ANIM_SE(4967)==1)CALL ANI_TXT('Color Function',14)
        IF(ANIM_SE(4968)==1)CALL ANI_TXT('Damage',6)
        IF(ANIM_SE(4969)==1)CALL ANI_TXT('Non-local plastic strain',24)
        IF(ANIM_SE(4970)==1)CALL ANI_TXT('Non-local plastic strain rate',29)
        IF(ANIM_SE(4971)==1)CALL ANI_TXT('Tsai-Wu Criterion',17) 
        DO I=1,200
          IF(ANIM_SE(4971+I)==1)THEN
            WRITE(MES,'(A,I3)')
     .         'Tsai-Wu Crit. layer',I
            CALL ANI_TXT(MES,22)
          ENDIF
        ENDDO
        IF(ANIM_SE(5172)==1)CALL ANI_TXT('Region identifier in p,v diagram',32)
        
C-----------------------------------------------                                         
      ENDIF !(ISPMD==0)

C-----------------------------------------------
C   ELEMENT FUNC (brick)
C-----------------------------------------------
      NDMA2= NUMNOD*(MIN(1,ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT)
     .              +MIN(1,ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS)
     .              +MIN(1,ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER))
      NNN = NUMELS+ISPH3D*(NUMSPH+MAXPJET)+3*NUMELS16+27*NUMELIG3D
      DO I = 1,MX_ANI
        IFUNC = I
        IF(ANIM_SE(I)==1) THEN
          CALL DFUNCS(
     1              ELBUF_TAB ,WAFT      ,IFUNC       ,IPARG       ,GEO          ,
     2              IXS       ,MAS       ,PM          ,EL2FA       ,NNN          ,
     3              IPM       ,IGEO      ,NBPART      ,EANI        ,ANIN(NDMA2+1),
     4              IADG      ,SPBUF     ,IPART       ,IPARTSP     ,ISPH3D       ,
     5              X         ,V         ,W           ,ALE_CONNECTIVITY,
     6              NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS    ,BUFMAT       ,
     7              FANI_CELL ,MULTI_FVM ,MAT_PARAM   )
        ENDIF
      ENDDO
C-----------------------------------------------
C   3D TENSOR TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
        IF(ANIM_ST(1)==1) CALL ANI_TXT('Stress',6)
        IF(ANIM_ST(2)==1) CALL ANI_TXT('Strain',6)
        IF(ANIM_ST(3)==1) CALL ANI_TXT('Strn rate',9)
        IF(ANIM_ST(4)==1) CALL ANI_TXT('Damage',6)
        IF(ANIM_ST(5)==1) CALL ANI_TXT('Plastic strain tensor',21)
!
        DO I=10,1009
          IF(ANIM_ST(I)==1)THEN
            II = I - 10
            WRITE(MES,'(A,I3)')
     .          'Strs Intg Point',II
            CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
        DO I=1010,2009
          IF(ANIM_ST(I)==1)THEN
            II = I - 1010
               WRITE(MES,'(A,I3)')
     .          'Stra Intg Point',II
                 CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
        DO I=2010,22109
          IF(ANIM_ST(I)==1)THEN
            II = I - 2010
            WRITE(MES,'(A,3I3)')
     .          'Strs In Pt',ABS(II)/2010,
     .           MOD(ABS(II)/10,201),MOD(ABS(II),10)
            CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
        DO I=22110,42209
          IF(ANIM_ST(I)==1)THEN
            II = I - 22110
            WRITE(MES,'(A,3I3)')
     .          'Stra In Pt',ABS(II)/2010,
     .           MOD(ABS(II)/10,201),MOD(ABS(II),10)
            CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
!
        DO I=42210,43209
          IF(ANIM_ST(I)==1)THEN
            II = I - 42210
               WRITE(MES,'(A,I3)')
     .          'Plastic Strn Intg Point',II
                 CALL ANI_TXT(MES,30)
          ENDIF
        ENDDO
!
        DO I=43210,63309
          IF(ANIM_ST(I)==1)THEN
            II = I - 43210
            WRITE(MES,'(A,3I3)')
     .          'Plastic Strn In Pt',ABS(II)/2010,
     .           MOD(ABS(II)/10,201),MOD(ABS(II),10)
            CALL ANI_TXT(MES,30)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C   3D TENSOR
C-----------------------------------------------
      DO I = 1,MX_ANI
       IFUNC = I
       IF(ANIM_ST(I)==1)THEN
           CALL TENSORS(ELBUF_TAB,IPARG   ,IFUNC   ,IXS     ,PM      ,
     2                  EL2FA    ,NNN     ,WAFT    ,TANI    ,
     3                  NBPART   ,X       ,IADG    ,IPART   ,
     4                  IPARTSP ,ISPH3D  ,IPM     ,IGEO)
       ENDIF
      ENDDO
C-----------------------------------------------
C   ELEMENT MASS
C-----------------------------------------------
      IF(ANIM_M==1)THEN
       IF(NSPMD == 1) THEN
         DO I=1,NNN
            R4 = MAS(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
       ELSE
C---- Mass stored here only if sph particles are tetra - if not it's done later
         DO I = 1,NNN
           MAS4(I) = MAS(I)
         ENDDO
         IF(ISPMD==0) THEN
           BUF = NUMELSG+3*NUMELS16G+NUMSPHG+27*NUMELIG3D
         ELSE
           BUF=1
         END IF
         NNNG = NUMELS+3*NUMELS16+NUMSPH+27*NUMELIG3D
         CALL SPMD_R4GET_PARTN(1,NNNG,NBPART,IADG,MAS4,BUF)
       ENDIF
      ENDIF
C-----------------------------------------------
C   BRICK NUMBERING
C-----------------------------------------------
       CALL DELNUMBS(IPARG  ,IXS   ,EL2FA ,NNN   ,WAFT   ,
     .                   DD_IAD,IAD ,NBPART,IADG ,KXSP ,
     .             ISPH3D )
C-----------------------------------------------
C   HIERARCHY
C-----------------------------------------------
       IF (ISPMD==0) THEN
         DO I=1,NPART
          IF(MATER(I)==2)THEN
            IF (IPART(3,I)<NSUBS) THEN
             CALL WRITE_I_C(IPART(3,I)-1,1)
            ELSE
             CALL WRITE_I_C(NSUBS
     .                      +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                      +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)-1,1)
            END IF
          END IF
         ENDDO
         DO I=1,NPART
           IF(MATER(I)==2)CALL WRITE_I_C(IPART(1,I),1)
         ENDDO
         DO I=1,NPART
           IF(MATER(I)==2)CALL WRITE_I_C(IPART(2,I),1)
         ENDDO
       ENDIF
C=======================================================================
 400  CONTINUE
C=======================================================================
C
C     POUTRE TRUSS SPRING
C     + RBODIES
C
C=======================================================================
      NERBY = 0
      IF (NRBODY>0)
     .    CALL DRBYCNT(NERBY,NPBY,FR_RBY2)
      NERBE2 = 0
      IF (NRBE2T>0)
     .    CALL DRBE2CNT(NERBE2,IRBE2,LRBE2,WEIGHT)
      NERBE3 = 0

      IF (NRBE3T>0)
     .    CALL DRBE3CNT(NERBE3,IRBE3,LRBE3,WEIGHT)
          NB1D_T = NB1DG

      IF(NB1DG+NRBODY+NRBE2T+NRBE3T+NANIM1D==0) GOTO 600
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
        DO I=1,NPART
          BUFFERP(I) = MATER(I)
          MATER(I) = 0
        ENDDO
C
        DO NG = 1, NGROUP
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          ITY   =IPARG(5,NG)
          IF(ITY==4)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTT(N))=3
            ENDDO
          ELSEIF(ITY==5)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTP(N))=3
            ENDDO
          ELSEIF(ITY==6)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTR(N))=3
            ENDDO
          ELSEIF(ITY==100)THEN
            DO I=1,NEL
              N = I+NFT
              IPRT=IPARTX(N)
              IF (NFACPTX(1,IPRT)>0) THEN
               MATER(IPRT)=3
              ELSE
               MATER(IPRT)=0
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(MATER,NPART)
        DO I=1,NPART
          IF(MATER(I)>3) MATER(I) = 3
        ENDDO
        IF(NSPMD > 1)CALL SPMD_IBCAST(MATER,MATER,NPART,1,0,2)
        DO I=1,NPART
          MATER(I) = MATER(I) + BUFFERP(I)
        ENDDO
C
      NBPART = 0
      DO I=1,NPART
        NBPART = NBPART + MATER(I)/3
      ENDDO
C
      DO I=1,NB1D + 1
        EL2FA(I)=0
      ENDDO
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      IF(ISPMD==0) THEN
        CALL WRITE_I_C(NB1D_T+NANIM1D+NERBY+NERBE2+NERBE3,1)
        CALL WRITE_I_C(NBPART+NRBODY+NRBE2T+NRBE3T,1)
        CALL WRITE_I_C(NFE_ANI,1)
        CALL WRITE_I_C(NFT_ANI,1)
C FLAG POUR SKEW
        CALL WRITE_I_C(1,1)
      ENDIF

C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      CALL PARSORF(ELBUF_TAB,
     .             X      ,D     ,IAD    ,CDG   ,IPARG   ,
     .             IXT    ,IXP   ,IXR    ,MATER ,EL2FA   ,
     .             DD_IAD ,IADG  ,IPARTT ,IPARTP,IPARTR  ,
     .             NFACPTX,IXEDGE,NODGLOB,NB1D  ,NANIM1D_L,
     .             IPART  ,IGEO  ,IADG_TPR,SIADG)
      IF(NRBODY>0) THEN
C test equivalent monoproc
          IF (NSPMD > 1) THEN
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

               IF ((ISPMD+1)==ABS(FR_RBY2(3,I)))
     .           SBUFSPO = SBUFSPO + FR_RBY2(2,I)
             ENDDO
             SBUFSPO = SBUFSPO + NRBYKIN*2
             IF (ISPMD==0) THEN
               SPORBY = NERBY+NRBYKIN*2
             ELSE
               SPORBY=1
             ENDIF
             CALL SPMD_DPARRBY(NPBY,LPBY,FR_RBY2,IAD_RBY2,
     .                        SBUFSPM,SBUFRECVM,SBUFSPO,SPORBY,
     .                        NODGLOB,WEIGHT,ITAB)
          ELSE
            CALL DPARRBY(LPBY ,NPBY )
          ENDIF
      ENDIF
      IF(NRBE2T>0) THEN
          IF (NSPMD>1) THEN
            CALL SPMD_DPARRBE2(LRBE2, IRBE2,NODGLOB,WEIGHT,NERBE2,
     *                         NERBE2T)
          ELSE
            CALL DPARRBE2(LRBE2 ,IRBE2 )
          ENDIF
      ENDIF
      IF(NRBE3T>0) THEN
          IF (NSPMD>1) THEN
            CALL SPMD_DPARRBE3(LRBE3, IRBE3,NODGLOB,WEIGHT,NERBE3,
     *                         NERBE3T)
          ELSE
            CALL DPARRBE3(LRBE3 ,IRBE3 )
          ENDIF
      ENDIF
C-----------------------------------------------
C   OFF
C-----------------------------------------------
      CALL ANIOFFF(ELBUF_TAB,      IPARG ,WAFT,EL2FA ,
     .             NB1D     ,IAD  ,NBPART,IADG,IOFFX1,
     .             NANIM1D_L)
      IF (ISPMD==0) THEN
        DO J=1,NERBY+NERBE2+NERBE3
          CALL WRITE_C_C(1,1)
        ENDDO
      ENDIF
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
        IF (ISPMD==0) THEN
          DO I = 1, NBPART
            BUFFERP(I) = 0
            DO K = 1, NSPMD
              BUFFERP(I) = BUFFERP(I) + IADG(K,I)
            ENDDO
          ENDDO
          CALL WRITE_I_C(BUFFERP,NBPART)
        ENDIF
C  il faut un gather de toutes les tailles de RB sur le P0
      DO I=1,NRBODY
        NERBT(I)=0
      ENDDO
        DO I=1,NRBODY
          PROC=ABS(FR_RBY2(3,I))
          IF (PROC==LOC_PROC) THEN
             NERBT(I)=FR_RBY2(2,I)
          ENDIF
        ENDDO
        IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(NERBT,NRBODY)
      IF (ISPMD==0) THEN
       NERBY1=0
       NERBE2_1 = 0
       NERBE3_1 = 0
         DO IRBY=1,NRBODY
          CALL DONERBY(IRBY,NERBY1,NPBY,NERBT)
          CALL WRITE_I_C(NB1D+NANIM1D+NERBY1,1)
         END DO
         DO I=1,NRBE2T
          CALL DONERBE2(I,NERBE2_1,IRBE2,NERBE2T)
          CALL WRITE_I_C(NB1D+NANIM1D+NERBY1+NERBE2_1,1)
         END DO
         DO I=1,NRBE3T
          CALL DONERBE3(I,NERBE3_1,IRBE3,NERBE3T)
          CALL WRITE_I_C(NB1D+NANIM1D+NERBY1+NERBE2_1+NERBE3_1,1)
         END DO
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------


       DO I=1,NPART
        IF(MATER(I)==3)THEN
          WRITE(STR,'(I9,A1)')IPART(4,I),':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10

            CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),40)
            DO J=1,LTITL
              IF(TITL(J:J)/=' ') IB = J+10
              CTEXT(J+10)=ICHAR(TITL(J:J))
            END DO
            CTEXT(IB+1)=0
            CALL WRITE_C_C(CTEXT,10+LTITL)

        ENDIF
       ENDDO

       IF (INVSTR<40) THEN
        DO IRBY=1,NRBODY
          WRITE(STR,'(I9,A2,A10)') IRBY,': ','Rigid Body'
          DO J=1,21
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 21
          CTEXT(IB+1)=0

          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
        DO I=1,NRBE2T
          WRITE(STR,'(I9,A2,A4)') I,': ','Rbe2'
          DO J=1,15
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 15
          CTEXT(IB+1)=0
        END DO
        DO I=1,NRBE3T
          WRITE(STR,'(I9,A2,A4)') I,': ','Rbe3'
          DO J=1,15
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 15
          CTEXT(IB+1)=0

          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
       ELSE
        DO IRBY=1,NRBODY
          WRITE(STR,'(I9,A2)') NOM_OPT(I161+LNOPT1*(IRBY-1)),': '
          DO J=1,11
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO

          CALL FRETITL2(TITL,NOM_OPT(I161+LNOPT1*(IRBY-1)
     &                                   +LNOPT1-LTITR),40)
          IB = LTITL+10
          DO J=1,LTITL
            CTEXT(J+11)=ICHAR(TITL(J:J))
          END DO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
        DO I=1,NRBE2T
          WRITE(STR,'(I9,A2)') NOM_OPT(I16L+LNOPT1*(I-1)),': '
          DO J=1,11
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO

          CALL FRETITL2(TITL,NOM_OPT(I16L+LNOPT1*(I-1)
     &                                   +LNOPT1-LTITR),40)
          IB = LTITL+10
          DO J=1,LTITL
            CTEXT(J+11)=ICHAR(TITL(J:J))
          END DO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
        DO I=1,NRBE3T
          WRITE(STR,'(I9,A2)') NOM_OPT(I16M+LNOPT1*(I-1)),': '
          DO J=1,11
           CTEXT(J)=ICHAR(STR(J:J))
          ENDDO

          CALL FRETITL2(TITL,NOM_OPT(I16M+LNOPT1*(I-1)
     &                                   +LNOPT1-LTITR),40)
          IB = LTITL+10
          DO J=1,LTITL
            CTEXT(J+11)=ICHAR(TITL(J:J))
          END DO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        END DO
       END IF
      ENDIF
C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M==1.OR.ANIM_FE(3)==1)THEN
        CALL DMASANIF(ELBUF_TAB, X    ,D    ,GEO  ,IPARG,
     .                IXT      ,IXP   ,IXR  ,MAS  ,PM   ,
     .                EL2FA    ,NB1D  )
      ENDIF
C-----------------------------------------------
C   E(truss+..) FUNC TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
       IF(ANIM_FE(1)==1) CALL ANI_TXT('Plastic Strain',14)
       IF(ANIM_FE(3)==1) CALL ANI_TXT('Specific Energy',15)
       IF(ANIM_FE(7)==1) CALL ANI_TXT('Von Mises',9)
       IF(ANIM_FE(11)==1) CALL ANI_TXT('Damage 1',8)
       IF(ANIM_FE(12)==1) CALL ANI_TXT('Damage 2',8)
       IF(ANIM_FE(13)==1) CALL ANI_TXT('Damage 3',8)
       IF(ANIM_FE(14)==1) CALL ANI_TXT('Stress X ',9)
       IF(ANIM_FE(15)==1) CALL ANI_TXT('Stress Y ',9)
       IF(ANIM_FE(16)==1) CALL ANI_TXT('Stress Z ',9)
       IF(ANIM_FE(17)==1) CALL ANI_TXT('Stress XY',9)
       IF(ANIM_FE(18)==1) CALL ANI_TXT('Stress YZ',9)
       IF(ANIM_FE(19)==1) CALL ANI_TXT('Stress ZX',9)
       IF(ANIM_FE(20)==1) CALL ANI_TXT('Element Time Step',17)
       IF(ANIM_FE(21)==1) CALL ANI_TXT('AMS selection',13)
       IF(ANIM_FE(22)==1) CALL ANI_TXT('Element status',14)
!
       DO I=23,122
         IF (ANIM_FE(I) == 1) THEN
           IUS = MOD ((I - 22), 100)
           IF (IUS==0) IUS = 100
           WRITE(MES,'(A,I3,A)') 'Plast Strn IPT ',IUS, ' '
           CALL ANI_TXT(MES,21)
         END IF
       ENDDO
!       
       IF (ANIM_FE(123)==1) CALL ANI_TXT('Strain X ',9)! For TRUSS element only
       IF (ANIM_FE(124)==1) CALL ANI_TXT('Strain rate',11)
       IF (ANIM_FE(125)==1) CALL ANI_TXT('Damage ',7)
!
      ENDIF
C-----------------------------------------------
C   ELEMENT FUNC (truss+..)
C-----------------------------------------------
      NDMA2= NUMNOD*(MIN(1,ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT)
     .              +MIN(1,ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS)
     .              +MIN(1,ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER))
      DO I = 1,MX_ANI
        IFUNC = I
        IF(ANIM_FE(I)==1) THEN

          CALL DFUNCF(ELBUF_TAB    ,WAFT   ,IFUNC  ,IPARG    ,GEO  ,
     .                IXT          ,IXP    ,IXR    ,MAS      ,PM   ,
     .                EL2FA        ,NB1D   ,IAD    ,NBPART   ,EANI ,
     .                ANIN(NDMA2+1),IADG   ,XFUNC1 ,NANIM1D_L,IGEO )
          IF (ISPMD==0) THEN
            R4 = 0.
            DO J=1,NERBY+NERBE2+NERBE3
              CALL WRITE_R_C(R4,1)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C-----------------------------------------------
C   TORSEUR TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
        IF(ANIM_FT(1)==1) CALL ANI_TXT('Force & Moment ',15)
      ENDIF
C-----------------------------------------------
C   TORSEUR
C-----------------------------------------------
      DO I = 1,MX_ANI
         IFUNC = I
         IF(ANIM_FT(I)==1)THEN
           CALL TORSEUR(IADG_TPR  ,IPARG,IFUNC ,IXT  ,IXP  ,
     .                  IXR   ,EL2FA,NB1D  ,WAFT ,TORS ,
     .                  NBPART)
         IF (ISPMD==0) THEN
             R4 = 0.
             DO J=1,NANIM1D
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
             ENDDO
         ENDIF
C
         IF (ISPMD==0) THEN
             R4 = 0.
             DO J=1,NERBY+NERBE2+NERBE3
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
             ENDDO
         ENDIF
         ENDIF
      ENDDO
C-----------------------------------------------
C   SKEW
C-----------------------------------------------
        CALL CNTSKEW(IPARG,LRBUF,LRBUFG)
        IF (ISPMD==0) LRBUF=LRBUFG
c        LRBUF = 0

      CALL ANISKEWF(GEO,SKEW,IPARG,IXR,DD_IAD,LRBUF)
      IF (ISPMD==0) THEN
C       GLOBAL SKEW FOR X-ELEMENTS.
         DO I=1,NANIM1D
          CALL WRITE_I_C(0,1)
         ENDDO
      ENDIF
      IF (ISPMD==0) THEN
         DO J=1,NERBY+NERBE2+NERBE3
          CALL WRITE_I_C(0,1)
         ENDDO
      ENDIF
C-----------------------------------------------
C   ELEMENT MASS
C-----------------------------------------------
      IF(ANIM_M==1)THEN
       IF(NSPMD == 1) THEN
         DO I=1,NB1D
            R4 = MAS(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
         DO I=1,NANIM1D
            R4 = XMASS1(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
       ELSE
         IF (ISPMD==0) THEN
           BUF = NB1DG+NANIM1D
         ELSE
           BUF = 1
         ENDIF
         DO I = 1, NB1D
           MAS4(I) = MAS(I)
         ENDDO
         DO I=1,NANIM1D_L
            MAS4(NB1D+I) = XMASS1(I)
         ENDDO
         CALL SPMD_R4GET_PARTN(1,NB1D+NANIM1D_L,NBPART,IADG,MAS4,BUF)
       ENDIF
       IF (ISPMD==0) THEN
         R4 = 0.
         DO J=1,NERBY+NERBE2+NERBE3
          CALL WRITE_R_C(R4,1)
         ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------
C   ELEMENT NUMBERING
C-----------------------------------------------
      CALL DELNUMBF(IPARG  ,IXT   ,IXP      ,IXR  ,EL2FA ,
     .              NB1D   ,WAFT  ,DD_IAD   ,IAD  ,NBPART,
     .              IADG   ,INUMX1,NANIM1D_L)
      IF (ISPMD==0) THEN
         DO J=1,NERBY+NERBE2+NERBE3
          CALL WRITE_I_C(0,1)
         ENDDO
      ENDIF
C-----------------------------------------------
C   HIERARCHY
C-----------------------------------------------
      IF (ISPMD==0) THEN
       DO I=1,NPART
          IF(MATER(I)==3)THEN
            IF (IPART(3,I)<NSUBS) THEN
             CALL WRITE_I_C(IPART(3,I)-1,1)
            ELSE
             CALL WRITE_I_C(NSUBS
     .                      +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                      +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)-1,1)
            END IF
          END IF
       ENDDO
clm    Subset Rbodies  == NSUBS
       DO I=1,NRBODY+NRBE2T+NRBE3T
           CALL WRITE_I_C(NSUBS-1,1)
       END DO
       DO I=1,NPART
         IF(MATER(I)==3)CALL WRITE_I_C(IPART(1,I),1)
       ENDDO
       DO I=1,NRBODY+NRBE2T+NRBE3T
           CALL WRITE_I_C(0,1)
       ENDDO
       DO I=1,NPART
         IF(MATER(I)==3)CALL WRITE_I_C(IPART(2,I),1)
       ENDDO
       DO I=1,NRBODY+NRBE2T+NRBE3T
           CALL WRITE_I_C(0,1)
       ENDDO
      ENDIF
C=======================================================================
 600  CONTINUE
C=======================================================================
C
C   HIERARCHY
C
C=======================================================================
      IF (ISPMD==0) THEN
        J=0
        DO I=1,NPART
          IF(MATER(I)==1)THEN
            J=J+1
            MATER(I)=J
          ELSE
            MATER(I)=-MATER(I)
          ENDIF
        ENDDO
        M01=J
        J=J+NCUTS+NRWALL+NSECT+NSURG+NSMAD+NPLYPARTW+NCRKPARTW
        M1=J
        DO I=1,NPART
          IF(MATER(I)==-2)THEN
            J=J+1
            MATER(I)=J
          ENDIF
        ENDDO
        M2=J
        DO I=1,NPART
          IF(MATER(I)==-3)THEN
            J=J+1
            MATER(I)=J
          ENDIF
        ENDDO
        M3=J+NRBODY+NRBE2T+NRBE3T
      ENDIF
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
Cplyxfem
      IF (ANIM_PLY > 0)THEN
        NPLYSUBS = MIN(1,NPLYPART)
      ELSE
        NPLYSUBS= 0
      ENDIF
C xfem crack for layered shells +++
      IF(ANIM_CRK > 0)THEN
        NCRKSUBS = MIN(1,NCRKPART)
      ELSE
        NCRKSUBS= 0
      ENDIF
C  xfem crack for layered shells ---
      IF (ISPMD==0) THEN
        CALL WRITE_I_C(NSUBS
     .       +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .       +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS+ NPLYSUBS
     .       +NCRKSUBS,1)
      ENDIF
C-----------------------------------------------
C   SUBSET HEAD/PARENT/
C-----------------------------------------------
      IF (NFVSUBS > 0 .AND. NSPMD > 1) CALL SPMD_FVB_ASUB2()
      IF (ISPMD==0) THEN
       IF (NSUBS==1) THEN
C ONE SEUL SUBSET OU INPUT V31
          MXSUBS=1
C-----------------
C SUBSET RBODIES
C-----------------
          IF (NRBODY+NRBE2T+NRBE3T>0) THEN
           WRITE(STR,'(I8,A28)')MXSUBS+1,':RBODIES & RBE2 & RBE3 MODEL'
           DO J=1,36
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(37)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .                    +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=0
           N2=0
           N3=NRBODY+NRBE2T+NRBE3T
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
           DO J=NRBODY+NRBE2T+NRBE3T,1,-1
              CALL WRITE_I_C(M3-J-M2,1)
           ENDDO
          END IF
C-----------------
C SUBSET SECTIONS
C-----------------
          IF (NSECT>0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS
     .                 +MIN(1,NRBODY+NRBE2T+NRBE3T)+1,':SECTIONS MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .                    +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSECT
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSECT,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-NRWALL-NPLYPARTW
     .                       -NCRKPARTW-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET RWALLS
C-----------------
          IF (NRWALL>0) THEN
           WRITE(STR,'(I8,A13)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+1,
     .             ':RWALLS MODEL'
           DO J=1,21
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(22)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .                    +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NRWALL
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NRWALL,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-NPLYPARTW
     .                       -NCRKPARTW-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET SURFACES
C-----------------
          IF (NSURG+NSMAD>0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .              +MIN(1,NRWALL)+1,':SURFACES MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .                    +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSURG+NSMAD
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSURG+NSMAD,1,-1
            CALL WRITE_I_C(M1-NPLYPARTW-NCRKPARTW-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSETS ply
C-----------------
          IF (NPLYSUBS>0) THEN
             II= MXSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .         +MIN(1,NSURG+NSMAD) + 1
                WRITE(STR,'(I8,A8)')
     .               II,': PLIES '
cc                WRITE(STR,'(I8,A8,I8)')
cc     .               II,':PLY ID ',I
                   DO J=1,24
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(25)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NPLYSUBS
     .                    +NFVSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   N1=NPLYPARTW
                   N2=0
                   N3=0
                   CALL WRITE_I_C(N1,1)
cc
                   DO J=NPLYPARTW,1,-1
                     CALL WRITE_I_C(M1-J,1)
                   ENDDO
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
          ENDIF
C xfem crack sor layered shells +++
C-----------------
C SUBSETS crk
C-----------------
          IF (NCRKSUBS>0) THEN
             II= MXSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .         +MIN(1,NSURG+NSMAD) + 1
                WRITE(STR,'(I8,A9)') 
     .               II,': CRACKS '
C
                   DO J=1,17
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(18)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NPLYSUBS
     .                    +NFVSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   N1=NCRKPARTW
                   N2=0
                   N3=0
                   CALL WRITE_I_C(N1,1)
cc                   
                   DO J=NCRKPARTW,1,-1
                     CALL WRITE_I_C(M1-J,1)
                   ENDDO
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
          ENDIF
C xfem crack sor layered shells ---
C-----------------
C SUBSETS FVMBAG
C-----------------
          IF (NFVSUBS>0) THEN
             II=NSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .         +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+ NPLYSUBS
     .         +NCRKSUBS-1
             OFFPART=NBPART2D + NPLYSUBS + NCRKSUBS
             DO I=1,NFVBAG
                IF (FVDATA(I)%NPOLH_ANIM>0) THEN
                   II=II+1
                   WRITE(STR,'(I8,A11,I8)')
     .                 II,':FVMBAG ID ',FVDATA(I)%ID
                   DO J=1,27
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(28)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .                    +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   CALL WRITE_I_C(FVDATA(I)%NPOLH_ANIM,1)
                   DO J=1,FVDATA(I)%NPOLH_ANIM
                      CALL WRITE_I_C(OFFPART+J-1,1)
                   ENDDO
                   OFFPART=OFFPART+FVDATA(I)%NPOLH_ANIM
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
                ENDIF
             ENDDO
          ENDIF
C--------------
C GLOBAL MODEL
C--------------
          WRITE(STR,'(I8,A13)')1,':GLOBAL MODEL'
          DO J=1,21
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          CTEXT(22)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT
          CALL WRITE_I_C(-1,1)
C #SUBSETS FILS
          CALL WRITE_I_C(MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                  +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .                  +NPLYSUBS+NCRKSUBS,1)
C SUBSETS FILS
          IF (NRBODY+NRBE2T+NRBE3T>0)
     .      CALL WRITE_I_C(NSUBS-1,1)
          IF (NSECT>0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NRBODY+NRBE2T+NRBE3T)-1,1)
          IF (NRWALL>0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NSECT)
     .                +MIN(1,NRBODY+NRBE2T+NRBE3T)-1,1)
          IF (NSURG+NSMAD>0)
     .     CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+
     .                          NRBE3T)+MIN(1,NRWALL)-1,1)
          IF (ISPMD==0 .AND.NPLYSUBS > 0) THEN
             II=NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                  +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD) - 1
             CALL WRITE_I_C(II,1)
          ENDIF
C xfem crack for layered shells +++
          IF (ISPMD==0 .AND.NCRKSUBS > 0) THEN
             II=NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                  +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD) - 1 
             CALL WRITE_I_C(II,1)
          ENDIF
C xfem crack for layered shells ---
          IF (ISPMD==0.AND.NFVSUBS>0) THEN
             II=MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .                   +MIN(1,NSURG+NSMAD)+1
             DO I=1,NFVBAG
                II=II+1
                CALL WRITE_I_C(II-1,1)
             ENDDO
          ENDIF
C #PARTS FILLES
          N1=0
          N2=0
          N3=0
          DO K=1,NPART
            IF(MATER(K)>0.AND.MATER(K)<=M01)THEN
               N1=N1+1
            ELSEIF(MATER(K)>M1.AND.MATER(K)<=M2)THEN
               N2=N2+1
            ELSEIF(MATER(K)>M2)THEN
               N3=N3+1
            ENDIF
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          N1=N1+NCUTS
C PARTS FILLES 2D
          CALL WRITE_I_C(N1,1)
          DO K=1,NPART
            IF(MATER(K)>0.AND.MATER(K)<=M01)
     .         CALL WRITE_I_C(MATER(K)-1,1)
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          DO J=1,NCUTS
              CALL WRITE_I_C(M1-NRWALL-NSECT-NSURG-NSMAD-NPLYPARTW
     .                            -NCRKPARTW-J,1)
          ENDDO
C PARTS FILLES 3D
          CALL WRITE_I_C(N2,1)
          DO K=1,NPART
            IF(MATER(K)>M1.AND.MATER(K)<=M2)
     .          CALL WRITE_I_C(MATER(K)-M1-1,1)
          ENDDO
C PARTS FILLES 1D
          CALL WRITE_I_C(N3,1)
          DO K=1,NPART
            IF(MATER(K)>M2)CALL WRITE_I_C(MATER(K)-M2-1,1)
          ENDDO
       ELSE
C----------------
C +SIEURS SUBSET ET INPUT V4.X
C----------------
        MXSUBS=0
        DO I=1,NSUBS-1
          IF (SUBSET(I)%ID > MXSUBS) MXSUBS=SUBSET(I)%ID
          WRITE(STR,'(I9,A1)') SUBSET(I)%ID,':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10
          TITL(1:LTITL) = SUBSET(I)%TITLE(1:LTITL)
          DO J=1,LTITL
            IF(TITL(J:J)/=' ') IB = J+10
            CTEXT(J+10)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT
          IF (SUBSET(I)%PARENT < NSUBS) THEN
           CALL WRITE_I_C(SUBSET(I)%PARENT-1,1)
          ELSE
           CALL WRITE_I_C(SUBSET(I)%PARENT
     .          +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .          +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .          +NPLYSUBS+NCRKSUBS-1,1)
          END IF
C #SUBSETS FILS
          CALL WRITE_I_C(SUBSET(I)%NCHILD,1)
C SUBSETS FILS
          DO J=1,SUBSET(I)%NCHILD
            CALL WRITE_I_C(SUBSET(I)%CHILD(J)-1,1)
          ENDDO
C #PARTS FILLES
          N1=0
          N2=0
          N3=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>0.AND.MATER(K)<=M01)THEN
               N1=N1+1
            ELSEIF(MATER(K)>M1.AND.MATER(K)<=M2)THEN
               N2=N2+1
            ELSEIF(MATER(K)>M2)THEN
               N3=N3+1
            ENDIF
          ENDDO
C PARTS FILLES 2D
          CALL WRITE_I_C(N1,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>0.AND.MATER(K)<=M01)
     .         CALL WRITE_I_C(MATER(K)-1,1)
          ENDDO
C PARTS FILLES 3D
          CALL WRITE_I_C(N2,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M1.AND.MATER(K)<=M2)
     .          CALL WRITE_I_C(MATER(K)-M1-1,1)
          ENDDO
C PARTS FILLES 1D
          CALL WRITE_I_C(N3,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M2)CALL WRITE_I_C(MATER(K)-M2-1,1)
          ENDDO
        ENDDO
C-----------------
C SUBSET RBODIES
C-----------------
          IF (NRBODY+NRBE2T+NRBE3T>0) THEN
           WRITE(STR,'(I8,A14)')MXSUBS+1,':RBODIES MODEL'
           DO J=1,22
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(23)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .          +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .          +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=0
           N2=0
           N3=NRBODY+NRBE2T+NRBE3T
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
           DO J=NRBODY+NRBE2T+NRBE3T,1,-1
              CALL WRITE_I_C(M3-J-M2,1)
           ENDDO
          END IF
C-----------------
C SUBSET SECTIONS
C-----------------
          IF (NSECT>0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                 +1,':SECTIONS MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .          +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .          +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSECT
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSECT,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-NRWALL-NPLYPARTW-
     .                       NCRKPARTW-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET RWALLS
C-----------------
          IF (NRWALL>0) THEN
           WRITE(STR,'(I8,A13)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .              +1,':RWALLS MODEL'
           DO J=1,21
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(22)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .          +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NFVSUBS
     .          +NPLYSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NRWALL
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NRWALL,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-NPLYPARTW
     .                       -NCRKPARTW-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET SURFACES
C-----------------
          IF (NSURG+NSMAD>0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .              +MIN(1,NRWALL)+1,':SURFACES MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NPLYSUBS
     .                    +NFVSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSURG+NSMAD
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSURG+NSMAD,1,-1
              CALL WRITE_I_C(M1-J-NPLYPARTW-NCRKPARTW,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSETS PLY
C-----------------
          IF (NPLYSUBS>0) THEN
             II=MXSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .         +MIN(1,NSURG+NSMAD)+1
cc             OFFPART=NBPART2D
                WRITE(STR,'(I8,A8)')
     .               II,': PLIES '
                   DO J=1,24
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(25)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NPLYSUBS
     .                    +NFVSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   N1=NPLYPARTW
                   N2=0
                   N3=0
                   CALL WRITE_I_C(N1,1)
cc
                   DO J=NPLYPARTW,1,-1
                     CALL WRITE_I_C(M1-J,1)
                   ENDDO
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
          ENDIF
C xfem crack for layered shells +++
C-----------------
C SUBSETS CRK
C-----------------
          IF (NCRKSUBS>0) THEN
             II=MXSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)+MIN(1,NRWALL)
     .         +MIN(1,NSURG+NSMAD)+1
cc             OFFPART=NBPART2D
                WRITE(STR,'(I8,A9)') 
     .               II,': CRACKS '
                   DO J=1,17
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(18)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NPLYSUBS
     .                    +NFVSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   N1=NCRKPARTW
                   N2=0
                   N3=0
                   CALL WRITE_I_C(N1,1)
c                   
                   DO J=NCRKPARTW,1,-1
                     CALL WRITE_I_C(M1-J,1)
                   ENDDO
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
          ENDIF
C xfem crack for layered shells ---
C-----------------
C SUBSETS FVMBAG
C-----------------
          IF (NFVSUBS>0) THEN
             II=NSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .         +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+ NPLYSUBS
     .         +NCRKSUBS-1
             OFFPART=NBPART2D+ NPLYSUBS+NCRKSUBS
             DO I=1,NFVBAG
                IF (FVDATA(I)%NPOLH_ANIM>0) THEN
                   II=II+1
                   WRITE(STR,'(I8,A11,I8)')
     .                 II,':FVMBAG ID ',FVDATA(I)%ID
                   DO J=1,27
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(28)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                    +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NPLYSUBS
     .                    +NFVSUBS+NCRKSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   CALL WRITE_I_C(FVDATA(I)%NPOLH_ANIM,1)
                   DO J=1,FVDATA(I)%NPOLH_ANIM
                      CALL WRITE_I_C(OFFPART+J-1,1)
                   ENDDO
                   OFFPART=OFFPART+FVDATA(I)%NPOLH_ANIM
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
                ENDIF
             ENDDO
          ENDIF
C--------------
C GLOBAL MODEL
C--------------
          WRITE(STR,'(I9,A1)') SUBSET(NSUBS)%ID,':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10
          TITL(1:LTITL) = SUBSET(I)%TITLE(1:LTITL)
          DO J=1,LTITL
            IF(TITL(J:J)/=' ') IB = J+10
            CTEXT(J+10)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT
          CALL WRITE_I_C(SUBSET(NSUBS)%PARENT-1,1)
C #SUBSETS FILS
          CALL WRITE_I_C(SUBSET(NSUBS)%NCHILD
     .         +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .         +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)+NPLYSUBS+NFVSUBS
     .         +NCRKSUBS,1)
C SUBSETS FILS
          DO J=1,SUBSET(NSUBS)%NCHILD
            CALL WRITE_I_C(SUBSET(NSUBS)%CHILD(J)-1,1)
          ENDDO
          IF (NRBODY+NRBE2T+NRBE3T>0)
     .      CALL WRITE_I_C(NSUBS-1,1)
          IF (NSECT>0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NRBODY+NRBE2T+NRBE3T)-1,1)
          IF (NRWALL>0)
     .     CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+
     .      NRBE3T)-1,1)
          IF (NSURG+NSMAD>0)
     .     CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+
     .                          NRBE3T)+MIN(1,NRWALL)-1,1)
           IF (ISPMD==0.AND.NPLYSUBS>0) THEN
              II=NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD) - 1
              CALL WRITE_I_C(II,1)
          ENDIF
C xfem crack for layered shells +++
           IF (ISPMD==0.AND.NCRKSUBS>0) THEN
              II=NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD) - 1 
              CALL WRITE_I_C(II,1)
          ENDIF
C xfem crack for layered shells ---
          IF (ISPMD==0.AND.NFVSUBS>0) THEN
             II=NSUBS+MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                   +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)
             DO I=1,NFVBAG
                CALL WRITE_I_C(II-1,1)
                II=II+1
             ENDDO
          ENDIF
C #PARTS FILLES
          N1=0
          N2=0
          N3=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>0.AND.MATER(K)<=M01)THEN
               N1=N1+1
            ELSEIF(MATER(K)>M1.AND.MATER(K)<=M2)THEN
               N2=N2+1
            ELSEIF(MATER(K)>M2)THEN
               N3=N3+1
            ENDIF
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          N1=N1+NCUTS
C PARTS FILLES 2D
          CALL WRITE_I_C(N1,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>0.AND.MATER(K)<=M01)
     .         CALL WRITE_I_C(MATER(K)-1,1)
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          DO J=1,NCUTS
              CALL WRITE_I_C(M1-NRWALL-NSECT-NSURG-NSMAD-NPLYPARTW-
     .                       NCRKPARTW-J,1)
          ENDDO
C PARTS FILLES 3D
          CALL WRITE_I_C(N2,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M1.AND.MATER(K)<=M2)
     .          CALL WRITE_I_C(MATER(K)-M1-1,1)
          ENDDO
C PARTS FILLES 1D
          CALL WRITE_I_C(N3,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M2)CALL WRITE_I_C(MATER(K)-M2-1,1)
          ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      IF (ISPMD==0) THEN
        CALL WRITE_I_C(NUMMAT+1,1)
        CALL WRITE_I_C(NUMGEO+1,1)
      ENDIF
C-----------------------------------------------
C   MAT HEAD
C-----------------------------------------------
      IF (ISPMD==0) THEN
       CALL ANI_TXT50('Dummy Material',14)
       DO I=1,NUMMAT
          WRITE(STR,'(I9,A1)') IPM(1,I),':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10
          CALL FRETITL2(TITL,IPM(NPROPMI-LTITR+1,I),40)
          DO J=1,LTITL
            IF(TITL(J:J)/=' ') IB = J+10
            CTEXT(J+10)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
       ENDDO
      ENDIF
C-----------------------------------------------
C   MAT TYPE
C-----------------------------------------------
      IF (ISPMD==0) THEN
       CALL WRITE_I_C(0,1)
       DO I=1,NUMMAT
         CALL WRITE_I_C(NINT(PM(19,I)),1)
       ENDDO
      ENDIF
C-----------------------------------------------
C   PROP HEAD
C-----------------------------------------------
      IF (ISPMD==0) THEN
       CALL ANI_TXT50('Dummy Property',14)
       DO I=1,NUMGEO
          WRITE(STR,'(I9,A1)') IGEO(1,I),':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10
          CALL FRETITL2(TITL,IGEO(NPROPGI-LTITR+1,I),40)
          DO J=1,LTITL
            IF(TITL(J:J)/=' ') IB = J+10
            CTEXT(J+10)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
       ENDDO
      ENDIF
C-----------------------------------------------
C   PROP TYPE
C-----------------------------------------------
      IF (ISPMD==0) THEN
       CALL WRITE_I_C(0,1)
       DO I=1,NUMGEO
         CALL WRITE_I_C(NINT(GEO(12,I)),1)
       ENDDO
      ENDIF
C=======================================================================
C
C     Additional particles description, case of /ANIM/VERS/44 only.
C
C=======================================================================
      IF(ISPH3D==1.OR.NUMSPH_T+MAXPJET==0) GOTO 700
C-----------------------------------------------
C   prepare sorties SUBSET : PART FILLES meshless
C-----------------------------------------------
      IF (ISPMD==0) THEN
        DO I=1,NPART
          MATER(I)=-MATER(I)
        ENDDO
      ENDIF
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
        DO I=1,NPART
          BUFFERP(I) = MATER(I)
          MATER(I) = 0
        ENDDO

        DO NG = 1, NGROUP
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          ITY   =IPARG(5,NG)
          IF(ITY==51)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTSP(N))=4
            ENDDO
          ENDIF
        ENDDO
        IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(MATER,NPART)
        DO I=1,NPART
          IF(MATER(I)>4) MATER(I) = 4
        ENDDO
        IF(NSPMD > 1) CALL SPMD_IBCAST(MATER,MATER,NPART,1,0,2)
        DO I=1,NPART
          MATER(I) = MATER(I)+BUFFERP(I)
        ENDDO
C
      NBPART = 0
      DO I=1,NPART
        IF(MATER(I)==4)NBPART = NBPART + 1
      ENDDO
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      IF (ISPMD==0) THEN
        CALL WRITE_I_C(NUMSPHG+MAXPJET,1)
        CALL WRITE_I_C(NBPART,1)
        CALL WRITE_I_C(NSE_ANI+1,1)
        CALL WRITE_I_C(NST_ANI,1)
      ENDIF
C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      CALL PARSOR0(IAD ,IPARG   ,MATER   ,EL2FA   ,
     2         DD_IAD  ,IADG               ,
     3         KXSP    ,IPARTSP ,NODGLOB)
C-----------------------------------------------
C     OFF SPH
C-----------------------------------------------
      NNN = NUMSPH+MAXPJET
      CALL ANIOFF0(ELBUF_TAB ,IPARG   ,WAFT      ,EL2FA   ,
     .             NNN       ,NBPART  ,IADG      ,SWAFT,SPH2SOL)
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
        IF (ISPMD==0) THEN
          DO I = 1, NBPART
            BUFFERP(I) = 0
            DO K = 1, NSPMD
              BUFFERP(I) = BUFFERP(I) + IADG(K,I)
            ENDDO
          ENDDO
          CALL WRITE_I_C(BUFFERP,NBPART)
        ENDIF
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
      IF (ISPMD==0) THEN
       DO I=1,NPART
        IF(MATER(I)==4)THEN
          WRITE(STR,'(I9,A1)')IPART(4,I),':'
          DO J=1,10
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 10
          CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),40)
          DO J=1,LTITL
            IF(TITL(J:J)/=' ') IB = J+10
            CTEXT(J+10)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
        ENDIF
       ENDDO
      ENDIF
C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M==1.OR.ANIM_SE(3)==1.OR.
     .   ANIM_SE(25)==1)THEN
         CALL DMASANI0(X ,D       ,ELBUF_TAB,GEO     ,IPARG   ,
     2           MAS     ,PM      ,EL2FA   ,IPART   ,IPARTSP )
      ENDIF
C-----------------------------------------------
C   FUNC TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
       CTEXT(81)=0
C     diameter is compulsory.
       CALL ANI_TXT('Diameter',8)
C
       IF(ANIM_SE(1)==1) CALL ANI_TXT('Plastic Strain',14)
       IF(ANIM_SE(2)==1) CALL ANI_TXT('Density',7)
       IF(ANIM_SE(3)==1) CALL ANI_TXT('Specific Energy',15)
       IF(ANIM_SE(4)==1) CALL ANI_TXT('Temperature',11)
       IF(ANIM_SE(6)==1) CALL ANI_TXT('Pressure',8)
       IF(ANIM_SE(7)==1) CALL ANI_TXT('Von Mises',9)
       IF(ANIM_SE(8)==1) CALL ANI_TXT('Turbulent Energy',16)
       IF(ANIM_SE(9)==1) CALL ANI_TXT('Turbulent Viscosity',19)
       IF(ANIM_SE(10)==1) CALL ANI_TXT('Vorticity',9)
       IF(ANIM_SE(11)==1) CALL ANI_TXT('Damage 1',8)
       IF(ANIM_SE(12)==1) CALL ANI_TXT('Damage 2',8)
       IF(ANIM_SE(13)==1) CALL ANI_TXT('Damage 3',8)
       IF(ANIM_SE(14)==1) CALL ANI_TXT('Stress X ',9)
       IF(ANIM_SE(15)==1) CALL ANI_TXT('Stress Y ',9)
       IF(ANIM_SE(16)==1) CALL ANI_TXT('Stress Z ',9)
       IF(ANIM_SE(17)==1) CALL ANI_TXT('Stress XY',9)
       IF(ANIM_SE(18)==1) CALL ANI_TXT('Stress YZ',9)
       IF(ANIM_SE(19)==1) CALL ANI_TXT('Stress ZX',9)
       IF(ANIM_SE(20)==1) CALL ANI_TXT('User Var 1',10)
       IF(ANIM_SE(21)==1) CALL ANI_TXT('User Var 2',10)
       IF(ANIM_SE(22)==1) CALL ANI_TXT('User Var 3',10)
       IF(ANIM_SE(23)==1) CALL ANI_TXT('User Var 4',10)
       IF(ANIM_SE(24)==1) CALL ANI_TXT('User Var 5',10)
       IF(ANIM_SE(25)==1) CALL ANI_TXT('Hourglass Energy per unit mass',30)
       IF(ANIM_SE(26)==1) CALL ANI_TXT('Strain Rate',11)
       IF(ANIM_SE(27)==1) CALL ANI_TXT('User Var 6',10)
       IF(ANIM_SE(28)==1) CALL ANI_TXT('User Var 7',10)
       IF(ANIM_SE(29)==1) CALL ANI_TXT('User Var 8',10)
       IF(ANIM_SE(30)==1) CALL ANI_TXT('User Var 9',10)
       IF(ANIM_SE(31)==1) CALL ANI_TXT('User Var 10',11)
       IF(ANIM_SE(32)==1) CALL ANI_TXT('User Var 11',11)
       IF(ANIM_SE(33)==1) CALL ANI_TXT('User Var 12',11)
       IF(ANIM_SE(34)==1) CALL ANI_TXT('User Var 13',11)
       IF(ANIM_SE(35)==1) CALL ANI_TXT('User Var 14',11)
       IF(ANIM_SE(36)==1) CALL ANI_TXT('User Var 15',11)
       IF(ANIM_SE(37)==1) CALL ANI_TXT('User Var 16',11)
       IF(ANIM_SE(38)==1) CALL ANI_TXT('User Var 17',11)
       IF(ANIM_SE(39)==1) CALL ANI_TXT('User Var 18',11)
       IF(ANIM_SE(40)==1) CALL ANI_TXT('User Var 19',11)
       IF(ANIM_SE(41)==1) CALL ANI_TXT('User Var 20',11)
       IF(ANIM_SE(42)==1) CALL ANI_TXT('User Var 21',11)
       IF(ANIM_SE(43)==1) CALL ANI_TXT('User Var 22',11)
       IF(ANIM_SE(44)==1) CALL ANI_TXT('User Var 23',11)
       IF(ANIM_SE(45)==1) CALL ANI_TXT('User Var 24',11)
       IF(ANIM_SE(46)==1) CALL ANI_TXT('User Var 25',11)
       IF(ANIM_SE(47)==1) CALL ANI_TXT('User Var 26',11)
       IF(ANIM_SE(48)==1) CALL ANI_TXT('User Var 27',11)
       IF(ANIM_SE(49)==1) CALL ANI_TXT('User Var 28',11)
       IF(ANIM_SE(50)==1) CALL ANI_TXT('User Var 29',11)
       IF(ANIM_SE(51)==1) CALL ANI_TXT('User Var 30',11)
       IF(ANIM_SE(52)==1) CALL ANI_TXT('User Var 31',11)
       IF(ANIM_SE(53)==1) CALL ANI_TXT('User Var 32',11)
       IF(ANIM_SE(54)==1) CALL ANI_TXT('User Var 33',11)
       IF(ANIM_SE(55)==1) CALL ANI_TXT('User Var 34',11)
       IF(ANIM_SE(56)==1) CALL ANI_TXT('User Var 35',11)
       IF(ANIM_SE(57)==1) CALL ANI_TXT('User Var 36',11)
       IF(ANIM_SE(58)==1) CALL ANI_TXT('User Var 37',11)
       IF(ANIM_SE(59)==1) CALL ANI_TXT('User Var 38',11)
       IF(ANIM_SE(60)==1) CALL ANI_TXT('User Var 39',11)
       IF(ANIM_SE(61)==1) CALL ANI_TXT('User Var 40',11)
       IF(ANIM_SE(62)==1) CALL ANI_TXT('User Var 41',11)
       IF(ANIM_SE(63)==1) CALL ANI_TXT('User Var 42',11)
       IF(ANIM_SE(64)==1) CALL ANI_TXT('User Var 43',11)
       IF(ANIM_SE(65)==1) CALL ANI_TXT('User Var 44',11)
       IF(ANIM_SE(66)==1) CALL ANI_TXT('User Var 45',11)
       IF(ANIM_SE(67)==1) CALL ANI_TXT('User Var 46',11)
       IF(ANIM_SE(68)==1) CALL ANI_TXT('User Var 47',11)
       IF(ANIM_SE(69)==1) CALL ANI_TXT('User Var 48',11)
       IF(ANIM_SE(70)==1) CALL ANI_TXT('User Var 49',11)
       IF(ANIM_SE(71)==1) CALL ANI_TXT('User Var 50',11)
       IF(ANIM_SE(72)==1) CALL ANI_TXT('User Var 51',11)
       IF(ANIM_SE(73)==1) CALL ANI_TXT('User Var 52',11)
       IF(ANIM_SE(74)==1) CALL ANI_TXT('User Var 53',11)
       IF(ANIM_SE(75)==1) CALL ANI_TXT('User Var 54',11)
       IF(ANIM_SE(76)==1) CALL ANI_TXT('User Var 55',11)
       IF(ANIM_SE(77)==1) CALL ANI_TXT('User Var 56',11)
       IF(ANIM_SE(78)==1) CALL ANI_TXT('User Var 57',11)
       IF(ANIM_SE(79)==1) CALL ANI_TXT('User Var 58',11)
       IF(ANIM_SE(80)==1) CALL ANI_TXT('User Var 59',11)
       IF(ANIM_SE(81)==1) CALL ANI_TXT('User Var 60',11)
       DO I=82,281
         IF(ANIM_SE(I)==1)THEN
           II = I - 81
           WRITE(MES,'(A,I3)')
     .         'WPLA layer',II
           CALL ANI_TXT(MES,20)
         ENDIF
       ENDDO
       DO I=1,200
        IF(ANIM_SE(286+3*(I-1)+1)==1)THEN
          WRITE(MES,'(A,I3,A)')'Psi (layer',I,')'
          CALL ANI_TXT(MES,15)
        ENDIF
        IF(ANIM_SE(286+3*(I-1)+2)==1)THEN
          WRITE(MES,'(A,I3,A)')'Teta (layer',I,')'
          CALL ANI_TXT(MES,16)
        ENDIF
        IF(ANIM_SE(286+3*(I-1)+3)==1)THEN
          WRITE(MES,'(A,I3,A)')'Phi (layer',I,')'
          CALL ANI_TXT(MES,15)
        ENDIF
       ENDDO    
       IF(ANIM_SE(3890)==1) CALL ANI_TXT('MAX DAMAGE ELEMENT',18)                        
       IF(ANIM_SE(4893)==1) CALL ANI_TXT('Domain',6) 
       IF(ANIM_SE(4937)==1) CALL ANI_TXT('Element Time Step',17)
       IF(ANIM_SE(4959)==1) CALL ANI_TXT('AMS selection',13)    
       IF(ANIM_SE(4965)==1) CALL ANI_TXT('Element status',14) 
       IF(ANIM_SE(4895)==1)CALL ANI_TXT('Equiv stress',12)
       IF(ANIM_SE(5172)==1)CALL ANI_TXT('Region identifier in p,v diagram',32)
      ENDIF
C-----------------------------------------------
C   ELEMENT FUNC (SPH)
C-----------------------------------------------
      NNN = NUMSPH+MAXPJET
      DO I = 0,MX_ANI
        IFUNC = I
        CONDITION = (IFUNC == 0) 
        IF(IFUNC > 0) CONDITION = CONDITION .OR. (ANIM_SE(I) == 1)
        IF(CONDITION) THEN
          CALL DFUNC0(ELBUF_TAB ,WAFT            ,IFUNC    ,IPARG   ,
     2                MAS       ,PM              ,EL2FA    ,NNN     ,
     3                NBPART    ,IADG            ,SPBUF    ,IPART   ,
     4                IPARTSP   ,ALE_CONNECTIVITY,IXQ      ,IPM     ,
     5                X         ,V               ,W        ,
     6                NERCVOIS  ,NESDVOIS        ,LERCVOIS ,LESDVOIS,
     7                BUFMAT    ,MULTI_FVM)
        ENDIF
      ENDDO
C-----------------------------------------------
C   3D TENSOR TEXT
C-----------------------------------------------
      IF (ISPMD==0) THEN
        IF(ANIM_ST(1)==1) CALL ANI_TXT('Stress',6)
        IF(ANIM_ST(2)==1) CALL ANI_TXT('Strain',6)
        IF(ANIM_ST(3)==1) CALL ANI_TXT('Strn rate',9)
        IF(ANIM_ST(4)==1) CALL ANI_TXT('Damage',6)
        IF(ANIM_ST(5)==1) CALL ANI_TXT('Plastic Strain Tensor',21)
!
        DO I=10,1009
          IF(ANIM_ST(I)==1)THEN
            II = I - 10
            WRITE(MES,'(A,I3)')
     .          'Strs Intg Point',II
            CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
        DO I=1010,2009
          IF(ANIM_ST(I)==1)THEN
            II = I - 1010
               WRITE(MES,'(A,I3)')
     .          'Stra Intg Point',II
                 CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
        DO I=2010,22109
          IF(ANIM_ST(I)==1)THEN
            II = I - 2010
            WRITE(MES,'(A,3I3)')
     .          'Strs In Pt',ABS(II)/2010,
     .           MOD(ABS(II)/10,201),MOD(ABS(II),10)
            CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
        DO I=22110,42209
          IF(ANIM_ST(I)==1)THEN
            II = I - 22110
            WRITE(MES,'(A,3I3)')
     .          'Stra In Pt',ABS(II)/2010,
     .           MOD(ABS(II)/10,201),MOD(ABS(II),10)
            CALL ANI_TXT(MES,20)
          ENDIF
        ENDDO
!
        DO I=42210,43209
          IF(ANIM_ST(I)==1)THEN
            II = I - 42210
               WRITE(MES,'(A,I3)')
     .          'Plastic Strn Intg Point',II
                 CALL ANI_TXT(MES,30)
          ENDIF
        ENDDO
!
        DO I=43210,63309
          IF(ANIM_ST(I)==1)THEN
            II = I - 43210
            WRITE(MES,'(A,3I3)')
     .          'Plastic Strn In Pt',ABS(II)/2010,
     .           MOD(ABS(II)/10,201),MOD(ABS(II),10)
            CALL ANI_TXT(MES,30)
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C   3D TENSOR (SPH)
C-----------------------------------------------
      DO I = 1,MX_ANI
       IFUNC = I
       IF(ANIM_ST(I)==1)THEN
           CALL TENSOR0(ELBUF_TAB,IPARG   ,IFUNC   ,PM      ,EL2FA   ,
     2                NNN     ,WAFT    ,TANI          ,IAD     ,
     3                NBPART  ,X       ,IADG    ,IPART   ,IPARTSP ,
     4                IPM     )
       ENDIF
      ENDDO
C-----------------------------------------------
C   ELEMENT MASS (SPH)
C-----------------------------------------------
      IF(ANIM_M==1)THEN
       IF(NSPMD == 1) THEN
         DO I=1,NNN
            R4 = MAS(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
       ELSE
         DO I = 1,NNN
           MAS4(I) = MAS(I)
         ENDDO
         IF(ISPMD==0) THEN
           BUF = NUMSPHG
         ELSE
           BUF=1
         END IF
         CALL SPMD_R4GET_PARTN(1,NNN,NBPART,IADG,MAS4,BUF)
       ENDIF
      ENDIF
C-----------------------------------------------
C   NUMBERING (SPH)
C-----------------------------------------------
       CALL DELNUMB0(IPARG ,EL2FA ,NNN  ,WAFT ,DD_IAD,
     .               IAD   ,NBPART,IADG ,KXSP )
C-----------------------------------------------
C   HIERARCHY
C-----------------------------------------------
       IF (ISPMD==0) THEN
         DO I=1,NPART
          IF(MATER(I)==4)THEN
            IF (IPART(3,I)<NSUBS) THEN
             CALL WRITE_I_C(IPART(3,I)-1,1)
            ELSE
             CALL WRITE_I_C(NSUBS
     .                      +MIN(1,NSECT)+MIN(1,NRBODY+NRBE2T+NRBE3T)
     .                      +MIN(1,NRWALL)+MIN(1,NSURG+NSMAD)-1,1)
            END IF
          END IF
         ENDDO
         DO I=1,NPART
           IF(MATER(I)==4)CALL WRITE_I_C(IPART(1,I),1)
         ENDDO
         DO I=1,NPART
           IF(MATER(I)==4)CALL WRITE_I_C(IPART(2,I),1)
         ENDDO
       ENDIF
C-----------------------------------------------
C   SUBSET : PART FILLES MESHLESS
C-----------------------------------------------
      IF (ISPMD==0) THEN
        J=M3
        DO I=1,NPART
          IF(MATER(I)==4)THEN
            J=J+1
            MATER(I)=J
          ENDIF
        ENDDO
        M4=J
        IF (NSUBS==1) THEN
C #PARTS FILLES meshless
          N0=0
          DO K=1,NPART
            IF(MATER(K)>M3)THEN
               N0=N0+1
            ENDIF
          ENDDO
C PARTS FILLES meshless
          CALL WRITE_I_C(N0,1)
          DO K=1,NPART
            IF(MATER(K)>M3)
     .         CALL WRITE_I_C(MATER(K)-M3-1,1)
          ENDDO
        ELSE
C----------------
C +SIEURS SUBSET
C----------------
        DO I=1,NSUBS-1
C #PARTS FILLES meshless
          N0=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M3)THEN
               N0=N0+1
            ENDIF
          ENDDO
C PARTS FILLES meshless
          CALL WRITE_I_C(N0,1)
         DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M3)
     .         CALL WRITE_I_C(MATER(K)-M3-1,1)
          ENDDO
        ENDDO
C--------------
C GLOBAL MODEL
C--------------
C #PARTS FILLES meshless
          N0=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M3)THEN
               N0=N0+1
            ENDIF
          ENDDO
C PARTS FILLES meshless
          CALL WRITE_I_C(N0,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K)>M3)
     .         CALL WRITE_I_C(MATER(K)-M3-1,1)
          ENDDO
        ENDIF
C--------------
        DO I=1,NPART
          IF(MATER(I)<0)MATER(I)=-MATER(I)
        ENDDO
      ENDIF
C=======================================================================
 700  CONTINUE
C=======================================================================
      IF (ISPMD==0) THEN

        CALL FILE_SIZE(ANIMSIZE)
        CALL CLOSE_C()
        ANIMTOTALSIZE=ANIMTOTALSIZE+ANIMSIZE
C-----------------------------------------------
        WRITE (IOUT,1000)  FILNAM(1:FILEN)
        WRITE (ISTDO,1000) FILNAM(1:FILEN)
 1000   FORMAT (4X,' ANIMATION FILE:',1X,A,' WRITTEN')
      ENDIF
C
       IF(ANIM_PLY > 0) THEN
          DEALLOCATE(WAFT_PLY)
          DEALLOCATE(EL2FA_PLY)
          DEALLOCATE(IAD_PLYG)
       ENDIF
       IF(ANIM_CRK > 0) THEN
          DEALLOCATE(EL2FA_CRK)
          DEALLOCATE(IAD_CRKG)
          DEALLOCATE(IAD_CRK)
          DEALLOCATE(IAD_LAY)
          DEALLOCATE(WAFT_CRK)
       ENDIF

       DEALLOCATE(WAFT,MAS,XNORM,XMASS1,XMASS2,XMASS3,
     .            XFUNC1,XFUNC2,XFUNC3,XUSR)
 
       DEALLOCATE(WA4,MAS4)
       DEALLOCATE(WA4_FVM)

       DEALLOCATE(VFLU,VVAR1,AFLU,VFLU_ALE,FANREACT,FANREACR)

       DEALLOCATE(WGPS,VGPS,ITAGPS)
c-----------       
       RETURN
      END SUBROUTINE GENANI
      END MODULE GENANI_MOD
c
Chd|====================================================================
Chd|  XYZ16                         source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GATHER_XYZ16             source/mpi/anim/spmd_gather_xyz16.F
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE XYZ16(IXS,IXS10,IXS20,IXS16,X)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .  X(3,*)
      INTEGER IXS(NIXS,*),
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  XX,YY,ZZ
      REAL R4,R4NP(6*NUMELS16)
      INTEGER I, J, K,N1,N2,N3,N4,N5,N6,N7,N8,
     . JJ,BUF
C-----------------------------------------------
      JJ = 0
      DO J=1,NUMELS16
        I = J+NUMELS8+NUMELS10+NUMELS20
        N1 = IXS(2,I)
        N2 = IXS(3,I)
        N3 = IXS(4,I)
        N4 = IXS(5,I)
        N5 = IXS16(1,J)
        N6 = IXS16(2,J)
        N7 = IXS16(3,J)
        N8 = IXS16(4,J)
        IF(N5==0)N5=N1
        IF(N6==0)N6=N2
        IF(N7==0)N7=N3
        IF(N8==0)N8=N4
        XX = HALF *(X(1,N5)+X(1,N6)+X(1,N7)+X(1,N8))
     .      -FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
        YY = HALF *(X(2,N5)+X(2,N6)+X(2,N7)+X(2,N8))
     .      -FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
        ZZ = HALF *(X(3,N5)+X(3,N6)+X(3,N7)+X(3,N8))
     .      -FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))
        IF (NSPMD == 1) THEN
          R4 = XX
          CALL WRITE_R_C(R4,1)
          R4 = YY
          CALL WRITE_R_C(R4,1)
          R4 = ZZ
          CALL WRITE_R_C(R4,1)
        ELSE
          R4NP(JJ+1) = XX
          R4NP(JJ+2) = YY
          R4NP(JJ+3) = ZZ
        ENDIF
        N1 = IXS(6,I)
        N2 = IXS(7,I)
        N3 = IXS(8,I)
        N4 = IXS(9,I)
        N5 = IXS16(5,J)
        N6 = IXS16(6,J)
        N7 = IXS16(7,J)
        N8 = IXS16(8,J)
        IF(N5==0)N5=N1
        IF(N6==0)N6=N2
        IF(N7==0)N7=N3
        IF(N8==0)N8=N4
        XX = HALF *(X(1,N5)+X(1,N6)+X(1,N7)+X(1,N8))
     .      -FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
        YY = HALF *(X(2,N5)+X(2,N6)+X(2,N7)+X(2,N8))
     .      -FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
        ZZ = HALF *(X(3,N5)+X(3,N6)+X(3,N7)+X(3,N8))
     .      -FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))
        IF (NSPMD == 1) THEN
          R4 = XX
          CALL WRITE_R_C(R4,1)
          R4 = YY
          CALL WRITE_R_C(R4,1)
          R4 = ZZ
          CALL WRITE_R_C(R4,1)
        ELSE
          R4NP(JJ+4) = XX
          R4NP(JJ+5) = YY
          R4NP(JJ+6) = ZZ
          JJ = JJ + 6
        ENDIF
      ENDDO
      IF (NSPMD > 1) THEN
        IF (ISPMD==0) THEN
          BUF = 6*NUMELS16G
        ELSE
          BUF=1
        ENDIF
C
      CALL SPMD_GATHER_XYZ16(R4NP,BUF)
C
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  XYZNOR16                      source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_S_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE XYZNOR16(IXS,IXS10,IXS20,IXS16,X)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .  X(3,*)
      INTEGER IXS(NIXS,*),
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  XX,YY,ZZ
      REAL R4
      INTEGER I, J, K,N1,N2,N3,N4,N5,N6,N7,N8,I3000,SIZ
C-----------------------------------------------
      I3000 = 3000
      SIZ = NUMELS16G
      DO J=1,SIZ
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  ANIMBALE                      source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|-- calls ---------------
Chd|        SPMD_FVB_SCAT_NUM_NODA        source/mpi/anim/spmd_fvb_scat_num_noda.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE ANIMBALE(IAN ,WA4 ,MONVOL,VOLMON, IOPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*),IAN
      my_real
     .   VOLMON(*)
      REAL WA4(*)
      INTEGER, INTENT(IN) :: IOPT
      ! OPT=1 : write internal nodes (1:NUMNOD)            : calls for /H3D
      ! OPT=2 : write also additional nodes (1:NUMNOD+NNA) : calls for /ANIM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .     I,ITYP,K1,K2,K3,KK1,KK2,N,NG,N1,N2
      INTEGER
     . NNOD_ALE,NTETRA_MX,KR1,KR2,KR3,KR9,KRA1,KRA2,KRA3,KRA8
      INTEGER KIBJET,KRBJET,KIBHOL,KRBHOL,
     .        NJET,IADJET,RADJET,NVENT,IADHOL,RADHOL
      my_real
     .   CV
      INTEGER NNBEM, NTG, KIBALE, KRBALE
      my_real
     .        GAMAI, CPAI, CPBI, CPCI, PINI, TI, CPI, CVI, RHOEI, RHOI,
     .        COEFA, RHO, PRES
      INTEGER NNS, NBA, NNA, NNI, NTGI,NNT
      INTEGER IFV
C
      K1 = 1
      KK1 = 1
      KIBHOL =1 + NIMV*NVOLU  + LICBAG + LIBAGJET
      KRBHOL =1 + NRVOLU * NVOLU + LRCBAG + LRBAGJET
      KIBALE=KIBHOL + LIBAGHOL
      KRBALE=KRBHOL + LRBAGHOL
      IFV=0
      DO I=1,NVOLU
        ITYP=MONVOL(K1+1)
        IF(ITYP==6.OR.ITYP==8)THEN
          IFV=MONVOL(K1-1+45)
          NNS=MONVOL(K1-1+32)
          NTG=MONVOL(K1-1+33)
          NBA=MONVOL(K1-1+62)
          NNA=MONVOL(K1-1+64)
          NNI =MONVOL(K1-1+68)
          NTGI=MONVOL(K1-1+69)
          NNT = NNS+NNI
          KR1=KRBALE+MONVOL(K1-1+34)
          KR2=KR1+NNT
          KR3=KR2+NNT
          KR9=KR1+6*NNT+4*(NTG+NTGI)
          KRA1=KR9+NNT
          KRA2=KRA1+NNA
          KRA3=KRA2+NNA          
          KRA8=KRA1+12*NNA+NTGI
          IF (IAN==3) THEN
C pression
             IF (NSPMD > 1)THEN
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA1),NNA)
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR1),NNT)
             ENDIF
             IF(IOPT==2)THEN
               DO N=1,FVSPMD(IFV)%NNA_L
                  N1=FVSPMD(IFV)%IBUFA_L(1,N)
                  N2=FVSPMD(IFV)%IBUFA_L(2,N)
                  WA4(N2)=VOLMON(KRA1-1+N1)
               ENDDO
             ENDIF
C
             DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                N1=FVSPMD(IFV)%IBUF_L(1,N)
                N2=FVSPMD(IFV)%IBUF_L(2,N)
                WA4(N2)=VOLMON(KR1-1+N1)
             ENDDO
          ELSEIF (IAN==4) THEN
C densite
             IF (NSPMD > 1)THEN
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA2),NNA)
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR2),NNT)
             ENDIF
             IF(IOPT==2)THEN
               DO N=1,FVSPMD(IFV)%NNA_L
                  N1=FVSPMD(IFV)%IBUFA_L(1,N)
                  N2=FVSPMD(IFV)%IBUFA_L(2,N)
                  WA4(N2)=VOLMON(KRA2-1+N1)
               ENDDO
             ENDIF
C
             DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                N1=FVSPMD(IFV)%IBUF_L(1,N)
                N2=FVSPMD(IFV)%IBUF_L(2,N)
                WA4(N2)=VOLMON(KR2-1+N1)
             ENDDO
          ELSEIF (IAN==6) THEN
C temperature
             IF (NSPMD > 1)THEN
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA3),NNA)
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR3),NNT)
             ENDIF
             IF(IOPT==2)THEN
               DO N=1,FVSPMD(IFV)%NNA_L
                  N1=FVSPMD(IFV)%IBUFA_L(1,N)
                  N2=FVSPMD(IFV)%IBUFA_L(2,N)
                  WA4(N2)=VOLMON(KRA3-1+N1)
               ENDDO
             ENDIF
C
             DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                N1=FVSPMD(IFV)%IBUF_L(1,N)
                N2=FVSPMD(IFV)%IBUF_L(2,N)
                WA4(N2)=VOLMON(KR3-1+N1)
             ENDDO
          ELSEIF (IAN==30) THEN
C ssp:sound speed
             IF (NSPMD > 1)THEN
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KRA8),NNA)
               CALL SPMD_FVB_SCAT_NUM_NODA(IFV,VOLMON(KR9),NNT)
             ENDIF
             IF(IOPT==2)THEN
               DO N=1,FVSPMD(IFV)%NNA_L
                  N1=FVSPMD(IFV)%IBUFA_L(1,N)
                  N2=FVSPMD(IFV)%IBUFA_L(2,N)
                  WA4(N2)=VOLMON(KRA8-1+N1)
               ENDDO
             ENDIF
C  
             DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
                N1=FVSPMD(IFV)%IBUF_L(1,N)
                N2=FVSPMD(IFV)%IBUF_L(2,N)
                WA4(N2)=VOLMON(KR9-1+N1)
             ENDDO
          ENDIF
        ENDIF
        K1 = K1 + NIMV
        KK1 = KK1 + NRVOLU
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  ALEVFLU                       source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_FVB_SCAT_NUM_NODA        source/mpi/anim/spmd_fvb_scat_num_noda.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|====================================================================
      SUBROUTINE ALEVFLU(VFLU , NNT, IBUF, U, NNA,
     .                   IBUFA, UA,  IFV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNT, IBUF(*), NNA, IBUFA(*),IFV
      my_real
     .        VFLU(3,*), U(3,*), UA(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,N,N1,N2
C
      IF (NSPMD > 1)THEN
          CALL SPMD_FVB_SCAT_NUM_NODA(IFV,UA,NNA*3)
          CALL SPMD_FVB_SCAT_NUM_NODA(IFV,U, NNT*3)
      ENDIF


      IF (KMESH(IFV) < 2) THEN
         DO N=1,FVSPMD(IFV)%NNA_L
            N1=FVSPMD(IFV)%IBUFA_L(1,N)
            N2=FVSPMD(IFV)%IBUFA_L(2,N)
            VFLU(1,N2)=UA(1,N1)
            VFLU(2,N2)=UA(2,N1)
            VFLU(3,N2)=UA(3,N1)
         ENDDO
      ENDIF
C
      DO N=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
         N1=FVSPMD(IFV)%IBUF_L(1,N)
         N2=FVSPMD(IFV)%IBUF_L(2,N)
         VFLU(1,N2)=U(1,N1)
         VFLU(2,N2)=U(2,N1)
         VFLU(3,N2)=U(3,N1)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  ANIVFLOW                      source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ANIVFLOW(VFLU, NNO, NNI, IBUF, IBUFI,
     .                    U   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNO, NNI, IBUF(*), IBUFI(*)
      my_real
     .        VFLU(3,*), U(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
C
      DO I=1,NNO
         II=IBUF(I)
         VFLU(1,II)=U(1,I)
         VFLU(2,II)=U(2,I)
         VFLU(3,II)=U(3,I)
      ENDDO
      DO I=1,NNI
         II=IBUFI(I)
         VFLU(1,II)=U(1,NNO+I)
         VFLU(2,II)=U(2,NNO+I)
         VFLU(3,II)=U(3,NNO+I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  ANIVFLOWP                     source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ANIVFLOWP(VFLU, NNO  , NNI  , NNO_L , NNI_L,
     .                     IBUF, IBUFI, IBUFL, IBUFIL, U    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNO, NNI, NNO_L, NNI_L, IBUF(*), IBUFI(*),
     .        IBUFL(*), IBUFIL(*)
      my_real
     .        VFLU(3,*), U(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, III
C
      DO I=1,NNO_L
         II=IBUFL(I)
         III=IBUF(II)
         VFLU(1,III)=U(1,II)
         VFLU(2,III)=U(2,II)
         VFLU(3,III)=U(3,II)
      ENDDO
      DO I=1,NNI_L
         II=IBUFIL(I)
         III=IBUFI(II)
         VFLU(1,III)=U(1,NNO+II)
         VFLU(2,III)=U(2,NNO+II)
         VFLU(3,III)=U(3,NNO+II)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  ANIMCALE                      source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE ANIMCALE(IANIM , MONVOL, VOLMON, NBF, EL2FA,
     .                    NBPART, IADG  , NBF_L )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IANIM, MONVOL(*), NBF, EL2FA(*), NBPART, IADG(NSPMD,*),
     .        NBF_L
      my_real
     .        VOLMON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K1, KK1, KIBHOL, KRBHOL, KIBALE, KRBALE, ITYP, NNS,
     .        NTG, KI1, KR1, KR2, J, JJ, BUF, NNI, NTGI, NNT, NTGT
      my_real
     .        WA(NBF)
      REAL    R4
C
      DO I=1,NBF
         WA(I)=ZERO
      ENDDO
C
      K1 = 1
      KK1 = 1
      KIBHOL =1 + NIMV*NVOLU  + LICBAG + LIBAGJET
      KRBHOL =1 + NRVOLU * NVOLU + LRCBAG + LRBAGJET
      KIBALE=KIBHOL + LIBAGHOL
      KRBALE=KRBHOL + LRBAGHOL
      DO I=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP==8) THEN
            NNS=MONVOL(K1-1+32)
            NTG=MONVOL(K1-1+33)
            NNI =MONVOL(K1-1+68)
            NTGI=MONVOL(K1-1+69)
            NNT=NNS+NNI
            NTGT=NTG+NTGI
            KI1=KIBALE+MONVOL(K1-1+31)
            KI1=KI1+NNT+5*NTGT
            KR1=KRBALE+MONVOL(K1-1+34)
            KR1=KR1+6*NNT+NTGT
            KR2=KR1+NTGT
            IF (IANIM==2143) THEN
               DO J=1,NTGT
                  JJ=MONVOL(KI1-1+J)
                  IF (JJ==0) CYCLE
                  JJ=EL2FA(1+JJ)
                  WA(JJ)=VOLMON(KR1-1+J)
               ENDDO
            ELSEIF (IANIM==2144) THEN
               DO J=1,NTGT
                  JJ=MONVOL(KI1-1+J)
                  IF (JJ==0) CYCLE
                  JJ=EL2FA(1+JJ)
                  WA(JJ)=VOLMON(KR2-1+J)
               ENDDO
            ENDIF
         ENDIF
      ENDDO
      IF (NSPMD == 1) THEN
        DO I=1,NBF
           R4 = WA(I)
           CALL WRITE_R_C(R4,1)
        ENDDO
      ELSE
        IF (ISPMD==0) THEN
           BUF = (NUMELQG+NUMELCG+NUMELTGG)*4
        ELSE
           BUF=1
        ENDIF
        CALL SPMD_R4GET_PARTN(1,NBF_L,NBPART,IADG,WA,BUF)
      ENDIF

C
      RETURN
      END
Chd|====================================================================
Chd|  ALEVEC                        source/output/anim/generate/genani.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE ALEVEC()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NNS_ANIM, NNTR, J, K, KK, L, LL, N1, N2, N3
      my_real
     .        VVT(3)
      REAL R4
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPTR, NPN
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VTR, VV
C
      DO I=1,NFVBAG
         NNS_ANIM=FVDATA(I)%NNS_ANIM
         NNTR=FVDATA(I)%NNTR
         ALLOCATE(VTR(3,NNTR), VV(3,NNS_ANIM), NPTR(NNTR),
     .            NPN(NNS_ANIM))
C
         DO J=1,NNTR
            NPTR(J)=0
            VTR(1,J)=ZERO
            VTR(2,J)=ZERO
            VTR(3,J)=ZERO
         ENDDO
         DO J=1,NNS_ANIM
            NPN(J)=0
            VV(1,J)=ZERO
            VV(2,J)=ZERO
            VV(3,J)=ZERO
         ENDDO
         DO J=1,FVDATA(I)%NPOLH
            IF (FVDATA(I)%MPOLH(J)==ZERO) CYCLE
            DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
               KK=FVDATA(I)%IFVPOLH(K)
               DO L=FVDATA(I)%IFVTADR(KK),FVDATA(I)%IFVTADR(KK+1)-1
                  LL=FVDATA(I)%IFVPOLY(L)
                  NPTR(LL)=NPTR(LL)+1
                  VTR(1,LL)=VTR(1,LL)+FVDATA(I)%QPOLH(1,J)/
     .                                FVDATA(I)%MPOLH(J)
                  VTR(2,LL)=VTR(2,LL)+FVDATA(I)%QPOLH(2,J)/
     .                                FVDATA(I)%MPOLH(J)
                  VTR(3,LL)=VTR(3,LL)+FVDATA(I)%QPOLH(3,J)/
     .                                FVDATA(I)%MPOLH(J)
               ENDDO
            ENDDO
         ENDDO
         DO J=1,NNTR
            N1=FVDATA(I)%IFVTRI_ANIM(1,J)
            N2=FVDATA(I)%IFVTRI_ANIM(2,J)
            N3=FVDATA(I)%IFVTRI_ANIM(3,J)
            NPN(N1)=NPN(N1)+1
            NPN(N2)=NPN(N2)+1
            NPN(N3)=NPN(N3)+1
            IF (NPTR(J)/=0) THEN
               VVT(1)=VTR(1,J)/NPTR(J)
               VVT(2)=VTR(2,J)/NPTR(J)
               VVT(3)=VTR(3,J)/NPTR(J)
            ELSE
               VVT(1)=ZERO
               VVT(2)=ZERO
               VVT(3)=ZERO
            ENDIF
            VV(1,N1)=VV(1,N1)+VVT(1)
            VV(2,N1)=VV(2,N1)+VVT(2)
            VV(3,N1)=VV(3,N1)+VVT(3)
            VV(1,N2)=VV(1,N2)+VVT(1)
            VV(2,N2)=VV(2,N2)+VVT(2)
            VV(3,N2)=VV(3,N2)+VVT(3)
            VV(1,N3)=VV(1,N3)+VVT(1)
            VV(2,N3)=VV(2,N3)+VVT(2)
            VV(3,N3)=VV(3,N3)+VVT(3)
         ENDDO
C
         DO J=1,NNS_ANIM
            R4 = VV(1,J)/NPN(J)
            CALL WRITE_R_C(R4,1)
            R4 = VV(2,J)/NPN(J)
            CALL WRITE_R_C(R4,1)
            R4 = VV(3,J)/NPN(J)
            CALL WRITE_R_C(R4,1)
         ENDDO
C
         DEALLOCATE(VTR, VV, NPTR, NPN)
      ENDDO
C
      R4=ZERO
      DO I=1,3
         CALL WRITE_R_C(R4,1)
         CALL WRITE_R_C(R4,1)
         CALL WRITE_R_C(R4,1)
      ENDDO
C
      RETURN
      END
