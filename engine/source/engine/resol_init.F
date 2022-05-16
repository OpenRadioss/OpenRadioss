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
Chd|  RESOL_INIT                    source/engine/resol_init.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ADMGVID                       source/model/remesh/admgvid.F 
Chd|        ADMINI                        source/model/remesh/admini.F  
Chd|        ADMORDR                       source/model/remesh/admordr.F 
Chd|        ANIM_XFE_INIT                 source/output/anim/generate/anim_crk_init.F
Chd|        ASSADD2                       source/assembly/assadd2.F     
Chd|        CHKINIT                       source/interfaces/interf/chkstfn3.F
Chd|        CNDMASI2_DIM                  source/elements/solid/solide10/s10cndf.F
Chd|        CNDMASI2_INI                  source/elements/solid/solide10/s10cndf.F
Chd|        CNDORDR                       source/model/remesh/cndordr.F 
Chd|        DIM_TSHEDG                    source/elements/thickshell/solidec/scdtdc3.F
Chd|        FILLIPARTL                    source/engine/resol_init.F    
Chd|        FINDGROUPC                    source/elements/findgroup.F   
Chd|        FINDGROUPS                    source/elements/findgroup.F   
Chd|        GRPSPLIT                      source/engine/resol_init.F    
Chd|        IMP_INIT                      source/implicit/imp_init.F    
Chd|        IND_TSHEDG                    source/elements/thickshell/solidec/scdtdc3.F
Chd|        INITIMEG                      source/system/timer.F         
Chd|        INIT_KYNE                     source/engine/resol_init.F    
Chd|        INIT_REAC_NOD                 source/output/th/init_reac_nod.F
Chd|        INIT_TH_GROUP                 source/output/th/init_th_group.F
Chd|        KININI                        source/constraints/general/kinini.F
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|        RBE2_INIT                     source/constraints/general/rbe2/rbe2f.F
Chd|        S10CNDI2_INI                  source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDS_DIM                   source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDS_INI                   source/elements/solid/solide10/s10cndf.F
Chd|        S10CND_INI                    source/elements/solid/solide10/s10cndf.F
Chd|        SECT_INI                      source/tools/sect/sectio.F    
Chd|        SPMD_ANIM_PLY_INIT            source/mpi/anim/spmd_anim_ply_init.F
Chd|        SPMD_FAILWAVE_BOUNDARIES      source/mpi/output/spmd_exch_failwave.F
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUB_BOUNDARIES           source/mpi/spmd_exch_sub.F    
Chd|        TSHCDCOM_DIM                  source/elements/thickshell/solidec/scdtdc3.F
Chd|        TSHCDCOM_INI                  source/elements/thickshell/solidec/scdtdc3.F
Chd|        ZERO1                         source/system/zero.F          
Chd|        ZEROR                         source/system/zero.F          
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DTDC_MOD                      share/modules/dtdc_mod.F      
Chd|        ECND_MOD                      share/modules/ecdn_mod.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE RESOL_INIT(
     1  ITASK   ,IADS10   ,IADS20  ,IADS16    ,FR_NBCC   ,
     2  ISENDTO ,IRCVFROM ,IAD_ELEM,FR_ELEM   ,ITABM1    ,
     3  IPARI   ,IPARG    ,ITAB    ,IXS10     ,IXS20     ,
     4  I13A    ,I13B     ,I13C    ,I13D      ,I13E      ,
     5  I13F    ,I13G     ,I13H    ,I13I      ,I15A      ,
     6  I15B    ,I15C     ,I15D    ,I15E      ,I15F      ,
     7  I15G    ,I15H     ,I15I    ,I87A      ,I87B      ,
     8  I87C    ,I87D     ,I87E    ,I87F      ,I87G      ,
     9  NFIA    ,NFEA     ,NFOA    ,NDMA      ,NDMA2     ,
     A  NODFT   ,NODLT    ,NDTASK  ,NUMNTHREAD  ,IXS16     ,
     B  IXS     ,IXQ      ,IXC     ,IXT       ,IXP       ,
     C  IXR     ,IXTG     ,ADSKY   ,IADS      ,IKINE     ,
     D  FSKY    ,A        ,AR      ,V         ,VR        ,
     E  X       ,D        ,MS      ,IN        ,STIFN     ,
     F  STIFR   ,DMAS     ,DINER   ,FANI      ,ANIN      ,
     G           WA       ,UWA     ,PM        ,GEO       ,
     H  PARTSAV ,PARTS0    ,MONVOL    ,
     I  I87H    ,I87I     ,I87J    ,I87K      ,IXUR      ,
     J  I15J    ,KXX      ,
     K  SECBUF  ,SECFCUM  ,NSTRF   ,IGRNOD    ,IEXLNK    ,
     L  XFRAME  ,I8A      ,I8AR    ,I8STIFN   ,I8STIFR   ,
     M  FSKYM   ,IXTG1    ,IB      ,VISCN     ,DD_R2R    ,
     O  I8VISCN ,ELBUF    ,IPART   ,MADPRT    ,MADSH4    ,
     P  MADSH3  ,MADSOL   ,MADNOD  ,MADFAIL   ,IGEO      ,
     Q  INTLIST ,NBINTC   ,PROCNE  ,NISKYFI   ,WEIGHT    ,
     R  ISIZXV  ,ILENXV   ,ADDCNI2 ,PROCNI2   ,IAD_I2M   ,
     S  FR_I2M  ,FR_NBCCI2,I2SIZE  ,FR_MAD    ,LWIBEM    ,
     T  LWRBEM  ,FXBFP    ,FXBEFW  ,FXBEDP    ,FXBGRP    ,
     U  FXBGRW  ,NDIN     ,IADSDP  ,IADRCP    ,ISENDP    ,
     V  IRECVP  ,ISLEN7   ,IRLEN7  ,ISLEN11   ,IRLEN11   ,
     W  LWIFLOW ,LWRFLOW  ,IFLOW   ,ADDCNEL   ,CNEL      ,
     X  ADDTMPL ,IPARTL   ,NPARTL  ,NFNCA     ,NFTCA     ,
     Y  I15ATH  ,I35ATH   ,IPM     ,SH4TREE   ,IPADMESH  ,
     Z  MSC     ,INC      ,SH3TREE ,MSTG      ,INTG      ,
     a  PTG     ,FTHE     ,FTHESKY ,FTHESKYI  ,NME17     ,
     b  ISLEN17 ,IRLEN17  ,IRLEN7T ,ISLEN7T   ,LINDIDEL  ,
     c  LBUFIDEL,SH4TRIM  ,SH3TRIM ,MSCND     ,INCND     ,
     d  IRLEN20 ,ISLEN20  ,IRLEN20T,ISLEN20T  ,NBINT20   ,
     e  IRLEN20E,ISLEN20E ,NISKYFIE,IRBE3     ,R3SIZE    ,
     f  MCP     ,MS0      ,INOD_PXFEM,IEL_PXFEM,IADC_PXFEM,
     g  ADSKY_PXFEM,ICODT,ICODR    ,IBFV      ,ADMSMS    ,
     h  ITHGRP ,ITHBUF  ,NODREAC   ,IGROUC    ,NGROUC	 ,
     i  IGROUNC  ,NGROUNC ,FR_RBY  ,FR_RBY6   ,NPBY	 ,
     j  NOM_SECT ,MCPC    ,MCPTG   ,GRTH      ,IGRTH	 ,
     k  NELEM	 ,LAG_SEC ,NPRW    ,DIAG_SMS  ,DMELC	 ,
     l  DMELTG   ,NGRTH   ,NFT2    ,DMELS     ,DMELTR	 ,
     m  DMELP	 ,DMELRT  ,RES_SMS ,I87L      ,IRBE2	 ,
     n  LRBE2	 ,NMRBE2  ,IAD_RBE2 ,FR_RBE2  ,FR_RBE2M  ,
     o  R2SIZE   ,LPBY    ,PROCNE_PXFEM ,ISENDP_PXFEM,IRECVP_PXFEM,
     p  IADSDP_PXFEM,IADRCP_PXFEM,FR_NBCC1,RBY,INT18KINE ,
     q  XDP      ,I87M,INOD_CRKXFEM,IEL_CRKXFEM ,IADC_CRKXFEM,
     r  ADSKY_CRKXFEM,PROCNE_CRKXFEM,ISENDP_CRKXFEM,IRECVP_CRKXFEM,
     s  IADSDP_CRKXFEM,IADRCP_CRKXFEM ,INT24USE,NDAMA2  ,
     t  IGROUPC  ,IGROUPTG ,IGROUPS ,IGROUPFLG ,DMINT2  ,IRBKIN_L,
     u  NRBYKIN_L,KINDRBY  ,ELBUF_TAB ,SENSOR,DD_R2R_ELEM,
     v  SDD_R2R_ELEM ,KINET, WEIGHT_MD,DMSPH ,IOLDSECT,LBUFIDEL24,
     w  INTBUF_TAB   ,NUMSPH_GLO_R2R, FLG_SPHINOUT_R2R,I15K,
     y  CONDN    ,CONDNSKY ,KXFENOD2ELC  ,ELCUTC ,NODEDGE,
     z  IAD_EDGE ,CRKNODIAD,FR_EDGE     ,FR_NBEDGE ,NODLEVXF,
     x  CRKEDGE  ,XFEM_TAB   ,ISENSINT  , NISUBMAX,
     1  STABSEN  ,TABSENSOR,INTLIST25  ,INT24E2EUSE  ,TABMP_L   , 
     2  I87N ,TAB_MAT,H3D_DATA,TAGTRIMC,TAGTRIMTG,
     3  IGRBRIC ,IGRQUAD  ,IGRSH4N  ,IGRSH3N  ,IGRTRUSS   ,
     4  IGRBEAM ,IGRSPRING,IGRPART  ,FORNEQS  ,INT7ITIED ,
     5  FXVEL_FGEO,FAILWAVE,NLOC_DMG,PINCH_DATA,SLLOADP  ,
     6  TAGSLV_RBY,NFNCA2 ,NFTCA2  ,IN0  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
      USE ELBUFDEF_MOD    
      USE INTBUFDEF_MOD            
      USE CRACKXFEM_MOD
      USE ECND_MOD  
      USE H3D_MOD
      USE GROUPDEF_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE PINCHTYPE_MOD
      USE PBLAST_MOD
      USE DTDC_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr12_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "secur_c.inc" 
#include      "units_c.inc"
#include      "cong2_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "rnur_c.inc"
#include      "timerc_c.inc"
#include      "timeri_c.inc"
#include      "couple_c.inc"
#include      "rad2r_c.inc"
#include      "scr18_c.inc"
#include      "spmd_c.inc"
#include      "fxbcom.inc"
#include      "subc_c.inc"
#include      "flowcom.inc"
#include      "remesh_c.inc"
#include      "sms_c.inc"
#include      "lagmult.inc"
#include      "sphcom.inc"
#include      "inter22.inc"
#include      "thermal_c.inc"
#include      "alemuscl.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK, NBINTC, NODFT, NODLT, LINDIDEL, LBUFIDEL,
     .        NUMNTHREAD, NDTASK, NFIA, NFEA, NFOA ,NDMA, NFNCA, NFTCA,
     .        NDMA2,NDIN,N1,N2,N3,IGTYP,NPARTL,NGROUC,NGROUNC,
     .        I13A,I13B,I13C,I13D,I13E,I13F,I13G,I13H,I13I,
     .        I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,I15K,
     .        I87A,I87B,I87C,I87D,I87E,I87F,I87G,I87H,I87I,I87J,
     .        I87K,I87L,I87M,I87N,NFNCA2,NFTCA2,
     .        ISIZXV , ILENXV, I2SIZE, ISLEN7,IRLEN7 ,ISLEN11 ,IRLEN11,
     .        I15ATH, I35ATH, NME17,ISLEN17,IRLEN17,IRLEN7T,ISLEN7T,
     .        IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,NBINT20,IRLEN20E,
     .        ISLEN20E,R3SIZE,NELEM,LAG_SEC, NGRTH, NFT2,NMRBE2,
     .        INT18KINE,INT24USE,NDAMA2, NRBYKIN_L,IOLDSECT,LBUFIDEL24,
     .        TABMP_L,TAGTRIMC(*),TAGTRIMTG(*), SLLOADP
      INTEGER 
     .        IXS(NIXS,*),IXS10(6,*) ,IXS20(12,*),IADS10(6,*),
     .        IADS20(12,*),IXS16(6,*) ,IADS16(12,*), IGEO(NPROPGI,*),
     .        IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .        IXR(NIXR,*), IXTG(NIXTG,*), IXUR(NIXUR,*), IXTG1(4,*),
     .        ITAB(*), IPARG(NPARG,*), IPARI(NPARI,*), ADSKY(*),
     .        IADS(*), IEXLNK(NR2R,*),
     .        WEIGHT(*), NSTRF(*), IB(NIBCLD,*), ITABM1(*),
     .        MONVOL(*),KXX(NIXX,*),ISENDTO(NINTER+1,NSPMD+1),
     .        FR_NBCC(2,NSPMD+1), IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),
     .        IRCVFROM(NINTER+1,NSPMD+1), INTLIST(NINTER), PROCNE(*),
     .        NISKYFI(*),ADDCNI2(*),PROCNI2(*),IAD_I2M(*),FR_I2M(*),
     .        FR_NBCCI2(*), IPART(*), IADSDP(*), IADRCP(*),
     .        ISENDP(*),IRECVP(*),DD_R2R(NSPMD+1,*),IPARTL(*),
     .        MADPRT(*), MADSH4(*), MADSH3(*), MADSOL(*), MADNOD(*),
     .        MADFAIL(*), FR_MAD(5,*), LWIBEM, LWRBEM, LWIFLOW, LWRFLOW,
     .        IFLOW(*), ADDCNEL(0:*), CNEL(0:*), ADDTMPL(0:*),
     .        IPM(NPROPMI,*), SH4TREE(*), IPADMESH(*), SH3TREE(*),
     .        SH4TRIM(*), SH3TRIM(*), NISKYFIE(*),IRBE3(NRBE3L,*),
     .        ICODT(*), ICODR(*),IBFV(NIFV,*),  
     .        INOD_PXFEM(*),IEL_PXFEM(*) ,IADC_PXFEM(4,*),ELCUTC(2,*),
     .        ADSKY_PXFEM(*), KXFENOD2ELC(*),NODLEVXF(*),CRKNODIAD(*),
     .        NODEDGE(*),IAD_EDGE(*),FR_EDGE(*),FR_NBEDGE(*),
     .        ITHGRP(NITHGR,*) ,ITHBUF(*) ,NODREAC(*),
     .        IGROUC(*),IGROUNC(*),FR_RBY(*),FR_RBY6(*),NPBY(*),
     .        NOM_SECT(*), GRTH(*),IGRTH(*), NPRW(*),IAD_RBE2(*),
     .        FR_RBE2(*),FR_RBE2M(*),R2SIZE, IRBE2(NRBE2L,*),LRBE2(*),
     .        IKINE(NUMNOD),LPBY(*), PROCNE_PXFEM(*),
     .        ISENDP_PXFEM(*),IRECVP_PXFEM(*),IADSDP_PXFEM(*),
     .        IADRCP_PXFEM(*),FR_NBCC1(2,*),INOD_CRKXFEM(*),
     .        IEL_CRKXFEM(*),IADC_CRKXFEM(*),ADSKY_CRKXFEM(0:*),
     .        PROCNE_CRKXFEM(*),ISENDP_CRKXFEM(*),IRECVP_CRKXFEM(*),
     .        IADSDP_CRKXFEM(*),IADRCP_CRKXFEM(*),
     .        IGROUPC(*),IGROUPTG(*),IGROUPS(*),IGROUPFLG(2),
     .        IRBKIN_L(*), KINDRBY(*), DD_R2R_ELEM(*),SDD_R2R_ELEM,
     .        KINET(*),WEIGHT_MD(*),NUMSPH_GLO_R2R,FLG_SPHINOUT_R2R,
     .        ISENSINT(NISUBMAX+1,NINTER),NISUBMAX,STABSEN,
     .        TABSENSOR(STABSEN), INTLIST25(NINTER25) ,INT24E2EUSE ,FXVEL_FGEO,
     .        TAGSLV_RBY(NUMNOD)
!     INT7ITIED : check if an interface type 7 with ITIED /= 0 is used
!                 in order to force the communication of a list of candidate nodes 
!                 INT7ITIED = 0 type 7 + ITIED/=0 not used
!                 INT7ITIED = 1 type 7 + ITIED/=0 used
      INTEGER, INTENT(INOUT) :: INT7ITIED
      my_real
     .   X(3,*), D(3,*), V(3,*), VR(3,*),
     .   MS(*), IN(*),  WA(*),  A(3,*), AR(3,*),
     .   FANI(3,*), UWA(*), STIFN(*),  STIFR(*),
     .   ANIN(*), FSKY(*),PARTSAV(NPSAV,*),PARTS0(*),
     .   DMAS, DINER     , FSKYM(*),
     .   PM(NPROPM,*)    , GEO(NPROPG,*), 
     .   VISCN(*),
     .   SECBUF(*),SECFCUM(7,NUMNOD,NSECT),XFRAME(NXFRAME,*),
     .   ELBUF(*), MSC(*), INC(*), MSTG(*), INTG(*), PTG(*),
     .   MSCND(*), INCND(*), FTHE(*), FTHESKY(*),FTHESKYI(*), MCP(*),
     .   MS0(*), ADMSMS(*), MCPC(*), MCPTG(*), DIAG_SMS(*),
     .   DMELC(*), DMELTG(*), DMELS(*), DMELTR(*), DMELP(*), DMELRT(*),
     .   RES_SMS(3,*),RBY(NRBY,*), DMINT2(4,I2NSN25), SENSOR(NSENSR,*),
     .   DMSPH(*),CONDN(*),CONDNSKY ,TAB_MAT(NGROUP),FORNEQS(3,*)
      my_real
     .   FXBFP(*), FXBEFW(*), FXBEDP(*), FXBGRP(*), FXBGRW(*),IN0(*)
      INTEGER*8 
     .  I8A(3,3,*),I8AR(3,3,*),I8STIFN(3,*),I8STIFR(3,*),
     .  I8VISCN(3,*)
C
      DOUBLE PRECISION XDP(3,*)     
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*)           :: CRKEDGE
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (PINCH) :: PINCH_DATA
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (NLOCAL_STR_)   ,TARGET :: NLOC_DMG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMUEL, I, J, K, NG, NINT7,NNOD,K2S,K0,IAD1,IDUM,LLL,
     .        LRBUF, LIBUF, ITY, IAD, NNBEM, ITYP,IROTG,NS,LF,LT,LL,L,
     .        L1,L2,ISECTR,NFR,IC,ICR,NISUB, NI25,NBR
      INTEGER JD(50),KD(50),JFI,KFI,NMN,II,NINOUT,NNO,NEL,IFLGADM,
     .        NAD_SMS(NUMNOD),MAD_SMS(NUMNOD),N,JJ,KK, NFT, ISOLNOD,NBS
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISEND,IRECV
      INTEGER :: ITIED
      my_real
     .        RDUM
      CHARACTER ZONE*5
      INTEGER VALUES(8)
C=======================================================================
       IDUM = 0 
       RDUM = ZERO
       ISECTR = 0         
C-----------------------------------------------
C-----------------------------------------------
C //
C-----------------------------------------------
C
C Partie Sequentielle
C
      IF (ITASK == 0)THEN
C Initialisation ITYPTS a 0 pour DTIX
C
        ITYPTS=0
C
C initialisation tableau de conditions cinematique 
C (actuellement utilise uniquement pour RBY et INT20)
C
      CALL INIT_KYNE(IKINE,NPBY,LPBY,TAGSLV_RBY)
C
C initialisation tableau de noeuds pour forces de reaction
C
        CPTREAC = 0
        IF (IREAC == 1 ) 
     .      CALL INIT_REAC_NOD(CPTREAC,NODREAC,NTHGRP,ITHGRP,ITHBUF)
C
C initialisation tableau d elements pour TH de GROUPE D ELEMENTS
C
        NGRTH = 0
        IF (IGRELEM == 1 ) THEN
          CALL INIT_TH_GROUP(GRTH     ,IGRTH   ,NELEM    ,NGRTH   ,IPARG  ,
     .                       IPART    ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N,
     .                       IGRTRUSS ,IGRBEAM ,IGRSPRING)
        ENDIF
C----- reset initial mass        
        IF (IMASSI /= 0) THEN
          MS(1:NUMNOD)=MS0(1:NUMNOD)
          IF (IRODDL /=0) IN(1:NUMNOD)=IN0(1:NUMNOD)
        END IF
C
C initialisation des structures paralleles
C
        IROTG=0
        DO I=1,NRBE3
          IROTG=MAX(IROTG,IRBE3(6,I))
        ENDDO
        CALL SPMD_MAX_I(IROTG)
        IF(IROTG.EQ.0) THEN
          R3SIZE = 5
        ELSE
          R3SIZE = 10
        ENDIF
 
C---------RBE2----        
        IROTG=0
        DO I=1,NRBE2 
	        IC = IRBE2(4,I)     
         ICR=(IC-512*(IC/512))/64
         IROTG=MAX(IROTG,ICR)   
	       IF (IRBE2(11,I)==0) IROTG =1
        ENDDO
        CALL SPMD_MAX_I(IROTG)
        IF(IROTG.EQ.0) THEN
          R2SIZE = 4
        ELSE
          R2SIZE = 8
        ENDIF
	       NS = NRBE2
        CALL SPMD_MAX_I(NS)
	       IF (NS.EQ.0) R2SIZE = 0
        NFR = IAD_RBE2(NSPMD+1)-IAD_RBE2(1)
        IF (NSPMD.EQ.1) THEN
          R3SIZE = 0
          R2SIZE = 0
        ENDIF
C Initialisation IRBE2
        CALL RBE2_INIT(IRBE2   ,LRBE2   ,NMRBE2 ,FR_RBE2 ,FR_RBE2M,NFR)
C
        CALL MPP_INIT(
     1       IPARI   ,ISENDTO ,IRCVFROM,INTLIST ,NBINTC  ,
     2       ISIZXV  ,ILENXV  ,IAD_ELEM,I2SIZE  ,ITASK   ,
     3       ISLEN7  ,IRLEN7  ,ISLEN11 ,IRLEN11 ,IGRBRIC ,
     4       NME17   ,ISLEN17 ,IRLEN17 ,IRLEN7T ,ISLEN7T ,
     5       LINDIDEL,LBUFIDEL,IRLEN20 ,ISLEN20 ,IRLEN20T,
     6       ISLEN20T,NBINT20 ,IRLEN20E,ISLEN20E,FR_RBY  ,
     7       FR_RBY6 ,NPBY    ,IRBKIN_L,
     8       NRBYKIN_L,KINDRBY,SENSOR  ,LBUFIDEL24, INTBUF_TAB)
C
        IF(IDEL7NG.GT.0.OR.IRAD2R.GT.0.OR.IALEMUSCL.GT.0.OR.PDEL.GT.0) THEN
           CALL CHKINIT(
     2       IXS        ,IXQ       ,IXC       ,IXT         ,IXP       ,
     3       IXR        ,IXTG      ,IXUR      ,IXS10       ,IXS20     ,
     4       IXS16      ,IXTG1     ,GEO       ,ADDCNEL     ,CNEL      ,
     5       ADDTMPL    ,IPARG     )
        ENDIF
     
C			  	
        IF (IRAD2R /= 0) THEN
          CALL R2R_INIT(IEXLNK ,ITAB,IGRNOD,X       ,
     2                  MS     ,IN  ,DD_R2R,WEIGHT ,IAD_ELEM,
     3                  FR_ELEM,ADDCNEL,CNEL,IXC,IPARG,ICODT,ICODR,
     4                  IBFV,D,RBY,NPBY,XDP,STIFN,STIFR,DD_R2R_ELEM,
     5                  SDD_R2R_ELEM,WEIGHT_MD,ILENXV,NUMSPH_GLO_R2R,
     6                  FLG_SPHINOUT_R2R,IPARI)
        END IF
C
        NFIA = NUMNOD*MIN(1,ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT)
        NFEA = NFIA + NUMNOD*MIN(1,ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT)
        NFNCA= NFEA + NUMNOD*MIN(1,ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT)
        NFTCA= NFNCA+ NUMNOD*MIN(1,ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT)
        NFOA = NFTCA+ NUMNOD*MIN(1,ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT)
        NFT2 = NFOA+ 2*(NSECT+NRBODY+NRWALL+NUMELUR)
        NFNCA2= NFT2  + NUMNOD*MIN(1,ANIM_V(13)+H3D_DATA%N_VECT_CONT2)
        NFTCA2= NFNCA2+ NUMNOD*MIN(1,ANIM_V(27)+H3D_DATA%N_VECT_PCONT2)
        NDMA = NUMNOD*MIN(1,ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT)
        NDIN = NDMA +NUMNOD*MIN(1,ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS)
        NDMA2 = NDIN+NUMNOD*MIN(1,ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER)
        NDAMA2 = NDMA2+NUMELR*(ANIM_FE(11)+ANIM_FE(12)+ANIM_FE(13))
        IF(IRODDL.NE.0)THEN
          DO NG=1,NINTER
            ITY = IPARI(7,NG)
            IF(ITY.EQ.2) THEN
              NMN=IPARI(6,NG)
              DO II = 1, NMN
                I = INTBUF_TAB(NG)%MSR(II)
                INTBUF_TAB(NG)%NMAS(NMN+II) = IN(I)
C               For multidomains inertia of main nodes on multidomains interface msut be non zero
                IF (IRAD2R.EQ.1) IN(I)=MAX(EM20,IN(I))
              END DO
            END IF
          END DO
        END IF
        DMAS = ZERO
        DINER = ZERO
C
        IF(MCHECK.EQ.0)NCYCLE=  0
        IF(MCHECK.EQ.0)NCYCLSH=0
        I7KGLO = 0
        NABFWR = 0
C
        I13A=1+2*NSNOD
        I13B=I13A+NSELS
        I13C=I13B+NSELQ
        I13D=I13C+NSELC
        I13E=I13D+NSELT
        I13F=I13E+NSELP
        I13G=I13F+NSELR
C       I13H=I13G+NSELUR
        I13H=I13G+NSELU
        I13I=I13H+NSELTG
        I15ATH=1+LIPART1*(NPART+NTHPART)
        I15A=I15ATH+2*9*(NPART+NTHPART)
        I15B=I15A+NUMELS
        I15C=I15B+NUMELQ
        I15D=I15C+NUMELC
        I15E=I15D+NUMELT
        I15F=I15E+NUMELP
        I15G=I15F+NUMELR
        I15H=I15G+NUMELUR
        I15I=I15H+NUMELTG
        I15J=I15I+NUMELX
        I15K=I15J+NUMSPH
        I35ATH=1+LISUB1*NSUBS
C
        I87A = 1
        I87B = I87A +  8 * NUMELS   + 6 * NUMELS10
     .              + 12 * NUMELS20 + 8 * NUMELS16
        I87C = I87B + 4 * NUMELQ
        I87D = I87C + 4 * NUMELC
        I87E = I87D + 2 * NUMELT
        I87F = I87E + 2 * NUMELP
        I87G = I87F + 3 * NUMELR
        I87H = I87G + 3 * NUMELTG
        I87H = I87H + 3 * NUMELTG6
        I87I = I87H + 4 * NUMELUR
        I87J = I87I + 4 * NSKYMV0
        I87K = I87J + 4 * NCONLD
        I87L = I87K + 4 * NUMCONV
        I87M = I87L + 4 * NUMRADIA
        I87N = I87M + SLLOADP
C        I87O = I87N + 4 * NFXFLUX
C
C---------------------------- 
        MAXNX=0
        DO I=1,NUMELX
          IF (KXX(3,I).GT.MAXNX) MAXNX=KXX(3,I)
        ENDDO
C---------------------------- 
        DO I=1,NPART
          PARTSAV(8,I)=PARTS0(I)
        ENDDO
C---------------------------- 
        IF (ISPMD.EQ.0)THEN
          CALL DATE_AND_TIME(STARTDATE, STARTTIME, ZONE, VALUES)
          WRITE(ISTDO,'(A,I2.2,A,I2.2,A,I4.4)') 
     *     ' ',VALUES(3),'/',VALUES(2),'/',VALUES(1)
          WRITE(IOUT,'(A,I2.2,A,I2.2,A,I4.4)') 
     *     ' ',VALUES(3),'/',VALUES(2),'/',VALUES(1)
        END IF
C
        MANIM = 0
        MREST = 0
        MSTOP = 0
        ICTLSTOP = 0
        H3D_DATA%MH3D = 0
        IF(DTIN.NE.0. .AND. MCHECK.EQ.0)THEN   !go on with previous time step in case of checkpoint restart (/CHKPT)
          IF(DT2OLD.EQ.ZERO)THEN
            DT2OLD=DTIN/UNP1
          ELSE
            DT2OLD= MIN(DT2OLD,DTIN/UNP1)
          ENDIF
        ENDIF
C------------------------
C      PARAL. ARITH.
C------------------------
        IF(IPARIT.EQ.3) THEN
          DO I=1,NUMNOD
            I8A(1,1,I) = 0
            I8A(2,1,I) = 0
            I8A(3,1,I) = 0
            I8A(1,2,I) = 0
            I8A(2,2,I) = 0
            I8A(3,2,I) = 0
            I8A(1,3,I) = 0
            I8A(2,3,I) = 0
            I8A(3,3,I) = 0
            I8STIFN(1,I) = 0
            I8STIFN(2,I) = 0
            I8STIFN(3,I) = 0
          ENDDO
          IF(IRODDL.NE.0)THEN
           DO I=1,NUMNOD
            I8AR(1,1,I) = 0
            I8AR(2,1,I) = 0
            I8AR(3,1,I) = 0
            I8AR(1,2,I) = 0
            I8AR(2,2,I) = 0
            I8AR(3,2,I) = 0
            I8AR(1,3,I) = 0
            I8AR(2,3,I) = 0
            I8AR(3,3,I) = 0
            I8STIFR(1,I) = 0
            I8STIFR(2,I) = 0
            I8STIFR(3,I) = 0
           ENDDO
          ENDIF
          IF(KDTINT.NE.0)THEN
           DO I=1,NUMNOD
            I8VISCN(1,I) = 0
            I8VISCN(2,I) = 0
            I8VISCN(3,I) = 0
           ENDDO
          ENDIF
        ELSEIF(IPARIT.NE.0) THEN
C
C parith/on
C
          IF(IVECTOR.EQ.1)THEN
            IAD1 = NUMNOD+2
          ELSE
            IAD1 = 1
          ENDIF
          CALL ASSADD2(
     1       ADSKY   ,ADSKY(IAD1),FSKY  ,FSKYM    ,IAD_ELEM     ,
     2       FR_ELEM ,FR_NBCC    ,PROCNE,NISKYFI  ,ADDCNI2      ,
     3       PROCNI2 ,IAD_I2M    ,FR_I2M,FR_NBCCI2,ADDCNI2(IAD1),
     4       IADSDP  ,IADRCP     ,ISENDP,IRECVP   ,FTHESKY      ,
     5       NISKYFIE,INOD_PXFEM ,ADSKY_PXFEM,PROCNE_PXFEM,
     6       ISENDP_PXFEM,IRECVP_PXFEM ,IADSDP_PXFEM,IADRCP_PXFEM,
     7       FR_NBCC1,INOD_CRKXFEM,ADSKY_CRKXFEM,PROCNE_CRKXFEM,
     8     ISENDP_CRKXFEM,IRECVP_CRKXFEM,IADSDP_CRKXFEM,IADRCP_CRKXFEM,
     9     CONDNSKY)
        ENDIF
C
        CALL FILLIPARTL(
     1     IPARTL     ,IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     2     IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),IPART(I15I),
     3     IPART(I15J),IPART(I15K),NPARTL     )
C------------------------
C     SPLIT GROUP FOR OPTIMIZATION
C------------------------
        CALL GRPSPLIT(
     1     IPARG, IGROUC, NGROUC, IGROUNC, NGROUNC,
     2     IXC,IXS,IXTG,IPM,IGEO,PM,GEO,TABMP_L,TAB_MAT)
C--------------------------
C     FIND GROUP FOR SHELLS
C--------------------------
        IF(IGROUPFLG(1) == 1 )
     1          CALL FINDGROUPC(IPARG, IGROUC, NGROUC, IGROUPC, IGROUPTG)
C--------------------------
C     FIND GROUP FOR BRICKS
C--------------------------
        IF(IGROUPFLG(2) == 1 ) CALL FINDGROUPS(IPARG, IGROUPS)
C----------------------------------------------------------
C   ON TAGUE TOUS LES NOEUDS DE TOUTES LES SECTIONS
C----------------------------------------------------------
        IF(ISECUT.NE.0)THEN
          K0=NSTRF(25)
          DO I=1,NSECT
            NNOD=NSTRF(K0+6)
            K2S=K0+30+NSTRF(K0+14)
            DO J=1,NNOD
             SECFCUM(4,NSTRF(K2S),I)=1.
             K2S=K2S+1
            ENDDO
            IF (NSTRF(K0) .GE. 100 ) ISECTR = I
            K0=NSTRF(K0+24)
          ENDDO
          CALL SECT_INI(NSTRF,SECBUF,NOM_SECT,ISECTR,NSECT,IOLDSECT)
        ENDIF
C-----------------------------------------------------
C   Calcul SQRT H1, H2, H3 pour elements coques
C-----------------------------------------------------
        DO I = 1, NUMGEO      
          IGTYP = IGEO(11,I)  
          IF(IGTYP==1.OR.(IGTYP>=9 .AND. IGTYP<=11).OR.IGTYP==16) THEN  
            GEO(18,I) = SQRT(GEO(13,I))                        
            GEO(19,I) = SQRT(GEO(14,I))                        
            GEO(20,I) = SQRT(GEO(15,I))                        
          ENDIF                                                
        ENDDO                                                  
C-----------------------------------------------------
C   Calcul optionel SQRT(G), SQRT(A11) SQRT(A12), SQRT(NU), SQRT(SHF) pour ancien restart
C-----------------------------------------------------
        IF(PMINVER.LT.6)THEN
          DO I = 1, NUMGEO
            GEO(100,I) = SQRT(GEO(38,I))      ! SHFSR
          END DO
          DO I = 1, NUMMAT
            IF(IPM(2,I)==999)CYCLE !possible negative square root otherwise PM(25)=CPE(gas)
            PM(12,I) = SQRT(ABS(PM(22,I)))    ! GSR
            PM(13,I) = SQRT(ABS(PM(24,I)))    ! A11SR
            PM(14,I) = SQRT(ABS(PM(25,I)))    ! A12SR
            PM(190,I)= SQRT(ABS(PM(21,I)))    ! NUSR
          END DO
        END IF
C----------------------------------------------------------
C   INIT FLEX BODY
C----------------------------------------------------------
       IF (NFXBODY.GT.0) THEN
          DO I=1,LENVAR
             FXBFP(I)=ZERO
             FXBGRP(I)=ZERO
          ENDDO
          DO I=1,NFXBODY
             FXBEFW(I)=ZERO
             FXBGRW(I)=ZERO
             FXBEDP(I)=ZERO
          ENDDO
       ENDIF          
C----------------------------------------------------------
C   LONGUEUR DES TABLEAUX DE TRAVAIL POUR AIRBAG BEM
C----------------------------------------------------------
       IAD=0
       LWIBEM=0
       LWRBEM=0
       DO I=1,NVOLU
          ITYP=MONVOL(IAD+2)
          IF (ITYP.EQ.7) THEN
             NNBEM=MONVOL(IAD+32)
             LWIBEM=LWIBEM+1+NNBEM
             LWRBEM=LWRBEM+NNBEM**2
          ENDIF
          IAD=IAD+NIMV
       ENDDO
C----------------------------------------------------------
C   LONGUEUR DES TABLEAUX DE TRAVAIL POUR FLOW BEM
C----------------------------------------------------------
       IAD=0
       LWIFLOW=0
       LWRFLOW=0
       DO I=1,NFLOW
          ITYP=IFLOW(IAD+2)
          IF (ITYP == 1 .OR.ITYP == 3) THEN
             LWIFLOW=LWIFLOW+IFLOW(IAD+8)
             LWRFLOW=LWRFLOW+IFLOW(IAD+9)
          ENDIF
          IAD=IAD+LIFLOW
       ENDDO
C----------------------------------------------------------
C   Domain Decomposition Weight computation
C----------------------------------------------------------
       IF(IDDW.GT.0) CALL INITIMEG(NGROUP)
C----------------------------------------------------------
C   INIT ADAPTIVE MESHING (sequentielle)
C----------------------------------------------------------
       IF(NADMESH.NE.0)THEN
         CALL ADMINI(IXC ,IPART(I15C),IXTG   ,IPART(I15H),IPART,
     .               IGEO,IPM        ,IPARG  ,X       ,MS      ,
     .               IN  ,ELBUF_TAB  ,SH4TREE,IPADMESH,MSC     ,
     .               INC ,SH3TREE    ,MSTG   ,INTG    ,PTG     ,
     .               SH4TRIM ,SH3TRIM,MSCND  ,INCND   ,PM      ,
     .               MCP ,MCPC       ,MCPTG  ,TAGTRIMC ,TAGTRIMTG)
         CALL ADMORDR(SH4TREE,SH3TREE,IXC,IXTG)
         IADMESH=0
         NGDONE=1
       END IF
       IF(ISTATCND.NE.0)THEN
C        ADAPTIVE MESHING + STATIC CONDENSATION
         CALL CNDORDR(IPART,IPART(I15C),IPART(I15H),SH4TREE,SH3TREE)
       END IF
C----------------------------------------------------------
C   INIT MULTIPLICATEURS LAGRANGES (sequentielle)
C----------------------------------------------------------
       IF(LAG_NCF+LAG_NCL > 0)THEN
         LAG_SEC=0
C compatibilisation des options non supportees si NSPMD > 1
         DO I = 1, NINTER
           IF(IPARI(33,I)/=0)LAG_SEC=1
         END DO
         DO I = 1, NRWALL
           IF(NPRW(I+5*NRWALL).EQ.1)LAG_SEC=1
         END DO
         IF(NBCSLAG+NGJOINT+NRBYLAG > 0)LAG_SEC=1
C NUMMPC + NFVLAG : ok (parallele SPMD)
       END IF

C-----------------------
C INTERFACE TYPE 18 KINE
C-----------------------
       INT18KINE=0
       DO I=1, NINTER
         IF(IPARI(7,I)==7.and.
     *          IPARI(34,I)==-2.and.IPARI(22,I)==7)THEN
           INT18KINE=1
         ENDIF
       ENDDO
C-----------------------
C INTERFACE TYPE 7 FLAG + ITIED /= 0
C-----------------------
      INT7ITIED = 0
      DO I=1, NINTER
        ITYP = IPARI(7,I)
        ITIED = IPARI(85,I)
        IF(ITYP==7.AND.ITIED/=0)THEN
           INT7ITIED = 1
        ENDIF
        IF(ITYP==10) INT7ITIED = 1
      ENDDO
C-----------------------
C INTERFACE TYPE 22 CEL
C-----------------------       
!      !Interface22: counting the number of int22 and buffer size for allocation
!      II = 0
!      J  = 0
!      DO I=1,NINTER
!         !NBRIC_G = IPARI(32,I)
!         !NBRIC_L = IGRN(2, IPARI(45,I) )
!         IF(IPARI(7,I).EQ.22)THEN
!           II=II+1
!           J = MAX( J , IGRN(2,IPARI(45,I)) )     
!         END IF
!      ENDDO
!      INT22  = II  !number of int22 interfaces.
!      I22LEN = J   !maximum nb of 3D elem.
C-----------------------
C INTERFACE TYPE 24 FLAG
C-----------------------
      INT24USE = 0
      DO I=1, NINTER
        IF(IPARI(7,I)==24)THEN
           INT24USE = 1
C Check if type 24 has E2E , set INT24E2EUSE
           IF(IPARI(59,I) >0) INT24E2EUSE=1
        ENDIF
      ENDDO
C-----------------------
C INTERFACE TYPE 25 LIST
C-----------------------
      NI25 = 0
      DO I=1, NINTER
        IF(IPARI(7,I)==25)THEN
           NI25 = NI25 + 1
           INTLIST25(NI25)=I
        ENDIF
      ENDDO
C-----------------------
C SENSOR INTERFACE
C-----------------------
      IF (STABSEN /= 0) THEN 
        DO N=1,NINTER
          NISUB =IPARI(36,N)
          ISENSINT(1,N) = TABSENSOR(N+1 + NSECT) - TABSENSOR(N + NSECT)
C
          IF (IPARI(71,N).GT.0) THEN
C--         sensor associated to all interfaces of type19
            ISENSINT(1,N) = ISENSINT(1,IPARI(71,N))
          ENDIF
C
          DO I=1,NISUB
            ISENSINT(I+1,N) = TABSENSOR(I +1 + NSECT + NINTER) -
     .                        TABSENSOR(I + NSECT + NINTER)
          ENDDO
        ENDDO
      ENDIF
C-----------------------
C INTERFACE TYPE 2 PENALITE
C-----------------------
       INT2PEN=0
       DO I=1, NINTER
         IF (IPARI(7,I) == 2 .AND. IPARI(20,I) == 25) THEN
           INT2PEN=1
           EXIT
         ENDIF
       ENDDO

C-----------------------
C /IMPDISP/FGEO
C-----------------------
       FXVEL_FGEO=0
       DO N=1,NFXVEL
         IF (IBFV(13,N) > 0  ) THEN
           FXVEL_FGEO = 1
           EXIT
         ENDIF
       ENDDO


      ENDIF   ! ITASK==0
C-----------------------------------------------------
C Fin partie sequentielle
C-----------------------------------------------------
C
C Partie Parallelle
C
      CALL MY_BARRIER()
C--- // --------------------------------------
C         INITIALISATION DES FORCES ET MOMENTS
C---------------------------------------------
      IF(NINTER.NE.0.AND.ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT .GT.0) 
     .               CALL ZEROR(FANI(1,NODFT),NUMNTHREAD)
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0) THEN
        CALL ZEROR(FANI(1,NFNCA+NODFT),NUMNTHREAD)
        CALL ZEROR(FANI(1,NFTCA+NODFT),NUMNTHREAD)
      END IF
      IF(ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS .GT.0)THEN
#include      "vectorize.inc"
        DO I=NODFT,NODLT
          ANIN(I+NDMA) = ZERO
        ENDDO
      ENDIF
      IF(ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER .GT.0)THEN
#include      "vectorize.inc"
        DO I=NODFT,NODLT
          ANIN(I+NDIN) = ZERO
        ENDDO
      END IF
      IF(ANIM_N(15) == 1  .OR. ANIM_N(16) == 1 .OR. H3D_DATA%N_SCAL_DAMA2 == 1)THEN
#include      "vectorize.inc"
        DO I=NODFT,NODLT
          ANIN(NDAMA2+2*(I-1)+1) = ZERO
          ANIN(NDAMA2+2*(I-1)+2) = ZERO
        ENDDO
      ENDIF
C-----------------------------------------------
      XMIN_GLOB = EP30
      YMIN_GLOB = EP30
      ZMIN_GLOB = EP30
      XMAX_GLOB = -EP30
      YMAX_GLOB = -EP30
      ZMAX_GLOB = -EP30
c
c      DO I=1,NUMNOD
c        XMIN_GLOB = MIN(XMIN_GLOB,X(1,I))
c        YMIN_GLOB = MIN(YMIN_GLOB,X(2,I))
c        ZMIN_GLOB = MIN(ZMIN_GLOB,X(3,I))
c        XMAX_GLOB = MAX(XMAX_GLOB,X(1,I))
c        YMAX_GLOB = MAX(YMAX_GLOB,X(2,I))
c        ZMAX_GLOB = MAX(ZMAX_GLOB,X(3,I))
c      END DO
      
c
c      CALL SPMD_GLOB_MIN(XMIN_GLOB,1)
c      CALL SPMD_GLOB_MIN(YMIN_GLOB,1)
c      CALL SPMD_GLOB_MIN(ZMIN_GLOB,1)
c      CALL SPMD_GLOB_MAX(XMAX_GLOB,1)
c      CALL SPMD_GLOB_MAX(YMAX_GLOB,1)
c      CALL SPMD_GLOB_MAX(ZMAX_GLOB,1)
C-----------------------------------------------
C  RESTARTING RADIOSS ENGINE.
        IF (IPARIT.EQ.0) THEN
          CALL ZEROR(A(1,NDTASK),NUMNOD)
          IF(IRODDL.NE.0)CALL ZEROR(AR(1,NDTASK),NUMNOD)
          DO I=NDTASK,NDTASK+NUMNOD-1
            STIFN(I)=EM20
          ENDDO
          IF(IRODDL.NE.0)THEN
            DO I=NDTASK,NDTASK+NUMNOD-1
              STIFR(I)=EM20
            ENDDO
          ENDIF
C
          IF(KDTINT.NE.0)THEN
            CALL ZERO1(VISCN(NDTASK),NUMNOD)
          ENDIF
C
           IF(ITHERM_FE > 0) THEN
              CALL ZERO1(FTHE(NDTASK),NUMNOD)
           ENDIF  
C
          IF(SOL2SPH_FLAG.NE.0)THEN
            CALL ZERO1(DMSPH(NDTASK),NUMNOD)
          ENDIF

          IF(NODADT_THERM > 0) THEN
             CALL ZERO1(CONDN(NDTASK),NUMNOD)
          ENDIF  
C  
          IF(NPINCH > 0) THEN
            CALL ZEROR(PINCH_DATA%APINCH(1,NDTASK),NPINCH)
            DO I=NDTASK,NDTASK+NUMNOD-1
              PINCH_DATA%STIFPINCH(I)=EM20
            ENDDO
          ENDIF   
C  
c          IF(IPLYXFEM > 0) THEN
c           DO J=1,NPLYMAX
c             DO I=1,NPLYXFE
c              PLY(J)%A(1,I)=ZERO
c              PLY(J)%A(2,I)=ZERO
c              PLY(J)%A(3,I)=ZERO
c              PLY(J)%V(1,I)=ZERO
c              PLY(J)%V(2,I)=ZERO
c              PLY(J)%V(3,I)=ZERO
c              PLY(J)%U(1,I)=ZERO
c              PLY(J)%U(2,I)=ZERO
c              PLY(J)%U(3,I)=ZERO              
c             ENDDO
c            ENDDO 
c           ENDIF   
          
        ELSE    ! IPARIT.GT.0
          CALL ZEROR(A(1,NODFT),NUMNTHREAD)
          IF(IRODDL.NE.0)CALL ZEROR(AR(1,NODFT),NUMNTHREAD)
          DO I=NODFT,NODLT
            STIFN(I)=EM20
          ENDDO
          IF(IRODDL.NE.0)THEN 
            DO I=NODFT,NODLT
              STIFR(I)=EM20
            ENDDO
          ENDIF
          IF(KDTINT.NE.0)THEN
            CALL ZERO1(VISCN(NODFT),NUMNTHREAD)
          ENDIF
C
          IF(ITHERM_FE > 0  ) THEN
            CALL ZERO1(FTHE(NODFT),NUMNTHREAD)
          ENDIF 
C
          IF(NSPHSOL.NE.0)THEN
            CALL ZERO1(DMSPH(NODFT),NUMNTHREAD)
          ENDIF

          IF(NODADT_THERM > 0) THEN
             CALL ZERO1(CONDN(NODFT),NUMNTHREAD)
          ENDIF  
C
          IF(NPINCH > 0) THEN
            CALL ZEROR(PINCH_DATA%APINCH(1,NODFT),NUMNTHREAD)
            DO I=NODFT,NODLT
              PINCH_DATA%STIFPINCH(I)=EM20
            ENDDO
          ENDIF
C         
c          IF(IPLYXFEM > 0) THEN
c           DO J=1,NPLYMAX
c             DO I=1,NPLYXFE
c              PLY(J)%A(1,I)=ZERO
c              PLY(J)%A(2,I)=ZERO
c              PLY(J)%A(3,I)=ZERO
cc              PLY(J)%V(1,I)=ZERO
c              PLY(J)%V(2,I)=ZERO
c              PLY(J)%V(3,I)=ZERO
c              PLY(J)%U(1,I)=ZERO
cc              PLY(J)%U(2,I)=ZERO
c              PLY(J)%U(3,I)=ZERO              
c             ENDDO
c           ENDDO 
c          ENDIF  
          ENDIF

C
      IF(IPARIT.EQ.0) THEN
        IF(IRODDL.EQ.0) THEN
          DO I = NODFT, NODLT
            STIFN(I) = STIFN(I)*WEIGHT(I)
C            PINCH_DATA%STIFPINCH(I) = PINCH_DATA%STIFPINCH(I)*WEIGHT(I)
          ENDDO
        ELSE
          DO I = NODFT, NODLT
            STIFN(I) = STIFN(I)*WEIGHT(I)
            STIFR(I) = STIFR(I)*WEIGHT(I)
C            PINCH_DATA%STIFPINCH(I) = PINCH_DATA%STIFPINCH(I)*WEIGHT(I)
          ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------------
C   INIT IMPLICIT 
C----------------------------------------------------------
C --default values----
      IF (ITASK==0) CALL IMP_INIT(V,VR,IPARG,IPM,IGEO,ELBUF_TAB)
C----------------------------------------------------------
C   INIT ADAPTIVE MESHING //
C----------------------------------------------------------
      IF(NADMESH.NE.0)THEN
        IFLGADM=0
        CALL ADMGVID(
     1    IPARG     ,ELBUF_TAB ,FSKY   ,FSKY  ,FTHESKY,
     2    IADS(I87C),IADS(I87G),IFLGADM,IGROUC,NGROUC,
     3    CONDNSKY  )
      END IF
C
C----------------------------------------------------------
       IF( ITASK == 0) CALL KININI()
C----------------------------------------------------------
C   INIT SELECTIVE MASS SCALING
C----------------------------------------------------------
       IF(IDTMINS == 1 .AND. IDTMINS_OLD == 1)THEN
         IF(DTFACS /= DTFACS_OLD .OR. DTMINS /= DTMINS_OLD)THEN
C
C          Forget about previous mass scaling (reversibility)
           ADMSMS(NODFT:NODLT)=ZERO
           RES_SMS(1:3,NODFT:NODLT)=ZERO
         ELSEIF(IDTGRS_OLD/=0)THEN
           IF( IDTGRS < 0.AND.
     .        -IDTGRS /= IGRPART(IDTGRS_OLD)%ID) THEN
C
C            Forget about previous mass scaling (reversibility)
             ADMSMS(NODFT:NODLT)=ZERO
             RES_SMS(1:3,NODFT:NODLT)=ZERO
           ELSE
C          ..as if single run
           END IF
         ELSEIF(IDTGRS_OLD==0.AND.IDTGRS/=0)THEN
C
C          Forget about previous mass scaling (reversibility)
           ADMSMS(NODFT:NODLT)=ZERO
           RES_SMS(1:3,NODFT:NODLT)=ZERO
         ELSE
C          ..as if single run
         END IF
C
       ELSEIF(IDTMINS == 2 .AND. IDTMINS_OLD == 2)THEN
         IF(DTFACS /= DTFACS_OLD .OR. DTMINS /= DTMINS_OLD)THEN
C          ..keep non diagonal mass from previous run
         ELSEIF(IDTGRS_OLD/=0)THEN
           IF( IDTGRS < 0.AND.
     .        -IDTGRS/= IGRPART(IDTGRS_OLD)%ID) THEN
C
C            Forget about previous mass scaling (reversibility)
             IF(ITASK==0)THEN
               DMELC (1:NUMELC )=ZERO
               DMELTG(1:NUMELTG)=ZERO
               DMELS (1:NUMELS )=ZERO
               DMELTR(1:NUMELT )=ZERO
               DMELP (1:NUMELP )=ZERO
               DMELRT(1:NUMELR )=ZERO
               DMINT2(1:4,1:I2NSN25)=ZERO
             END IF
             RES_SMS(1:3,NODFT:NODLT)=ZERO
           ELSE
C          ..as if single run
           END IF
         ELSEIF(IDTGRS_OLD==0.AND.IDTGRS/=0)THEN
C
C          Forget about previous mass scaling (reversibility)
           IF(ITASK==0)THEN
             DMELC (1:NUMELC )=ZERO
             DMELTG(1:NUMELTG)=ZERO
             DMELS (1:NUMELS )=ZERO
             DMELTR(1:NUMELT )=ZERO
             DMELP (1:NUMELP )=ZERO
             DMELRT(1:NUMELR )=ZERO
             DMINT2(1:4,1:I2NSN25)=ZERO
           END IF
           RES_SMS(1:3,NODFT:NODLT)=ZERO
         ELSE
C          ..as if single run
         END IF
C
       ELSEIF(IDTMINS == 1 .AND. IDTMINS_OLD /= IDTMINS)THEN
C
         ADMSMS(NODFT:NODLT)=ZERO
         RES_SMS(1:3,NODFT:NODLT)=ZERO
C
       ELSEIF(IDTMINS == 2 .AND. IDTMINS_OLD /= IDTMINS)THEN
C
         IF(ITASK==0)THEN
           DMELC (1:NUMELC )=ZERO
           DMELTG(1:NUMELTG)=ZERO
           DMELS (1:NUMELS )=ZERO
           DMELTR(1:NUMELT )=ZERO
           DMELP (1:NUMELP )=ZERO
           DMELRT(1:NUMELR )=ZERO
           DMINT2(1:4,1:I2NSN25)=ZERO
         END IF
         RES_SMS(1:3,NODFT:NODLT)=ZERO
C
       ELSEIF(IDTMINS_INT /= 0 .AND. IDTMINS_INT_OLD /= IDTMINS_INT)THEN
C
         RES_SMS(1:3,NODFT:NODLT)=ZERO
C
       END IF
C
      IF(ITASK == 0) THEN 
       NISKY_SMS=0
C enforce sorting contacts
      KFORSMS=0
      IF((IDTMINS==2.AND.IDTMINS_OLD/=IDTMINS).OR.
     .   (IDTMINS_INT/=0.AND.IDTMINS_INT_OLD/=IDTMINS_INT))THEN
        KFORSMS=1
      END IF
      ENDIF
C
      IF(ANIM_PLY > 0.AND. ITASK == 0) THEN
        CALL SPMD_ANIM_PLY_INIT ()
      ENDIF
C
      IF (ICRACK3D > 0 .AND. ITASK == 0)THEN
        CALL ANIM_XFE_INIT(IXC,IXTG,INOD_CRKXFEM,IEL_CRKXFEM,
     .                     IADC_CRKXFEM,IADC_CRKXFEM(1+4*ECRKXFEC))
      ENDIF
C-----------------------
C ITET=2 OF S10
C-----------------------
      IF(NS10E > 0) THEN
        IF (ITASK == 0) THEN
          IF(NSPMD>1) THEN
            CALL S10CNDS_DIM(ICNDS10,ITAGND,FR_ELEM,IAD_ELEM,NBS  )
            ALLOCATE (IAD_CNDS(NSPMD+1),FR_CNDS(NBS))
            CALL S10CNDS_INI(ICNDS10,ITAGND,FR_ELEM,IAD_ELEM,IAD_CNDS,FR_CNDS )
          ELSE
            ALLOCATE (IAD_CNDS(0),FR_CNDS(0))
          END IF

          CALL CNDMASI2_DIM(IPARI,INTBUF_TAB,ICNDS10,ITAGND,WEIGHT,NKEND) 
            IF(NKEND>0) THEN
              ALLOCATE (IMAP2ND(NKEND),MASI2ND0(NKEND))
              CALL CNDMASI2_INI(IPARI,INTBUF_TAB,ICNDS10,ITAGND,
     .                    NKEND,IMAP2ND,MASI2ND0,MS0,WEIGHT, itab ) 
          IF(MCHECK>0) NKEND = -NKEND
        END IF
        CALL S10CNDI2_INI(IPARI,INTBUF_TAB,ICNDS10,ITAGND,WEIGHT,
     .                   FR_CNDS,IAD_CNDS,itab ) 
        CALL S10CND_INI(ICNDS10,ITAGND,IAD_CNDM,FR_CNDM,FR_NBCCCND,
     1                 ADDCNCND,PROCNCND,VND   ,V    ,ITAB ,
     2                 IAD_CNDM1,FR_CNDM1,FR_NBCCCND1)
        END IF
        CALL MY_BARRIER()
      ENDIF
!$OMP SINGLE
       IF (FAILWAVE%WAVE_MOD > 0) THEN
         CALL SPMD_FAILWAVE_BOUNDARIES(FAILWAVE,IAD_ELEM,FR_ELEM)
       ENDIF
       ! Non-local regularization is activated
       IF (NLOC_DMG%IMOD > 0) THEN
         CALL SPMD_SUB_BOUNDARIES(NLOC_DMG,IAD_ELEM,FR_ELEM)
       ENDIF
C       
       IF (NUMSKINP > 0) THEN
         DO I=1,NLOADP_B
           NBS = PBLAST_TAB(I)%SIZ
           ALLOCATE (PBLAST_TAB(I)%PRES(NBS))
         ENDDO
       END IF
C-----------------------
C DT_DC OF TSH
C-----------------------
       NTSHEG =0       
       NTSHEGG =0       
       IF (IDTTSH>0) CALL DIM_TSHEDG(NTSHEG, IXS ,IPARG )
       IF(NSPMD>1) THEN
         NTSHEGG = NTSHEG
         CALL SPMD_MAX_I(NTSHEGG)
       END IF
       IF (NTSHEG > 0) THEN
         ALLOCATE (IENUNL(2*NTSHEG),ALPHA_DC(NUMNOD))
         IENUNL=0
         ALPHA_DC=UN
         CALL IND_TSHEDG(IENUNL, IXS ,IPARG )
         IF(NSPMD>1) THEN
          NBS = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
          ALLOCATE (ISEND(NBS),IRECV(NBS))
          ISEND=0
          CALL TSHCDCOM_DIM(IENUNL,FR_ELEM,IAD_ELEM,NBS,NBR ,
     .                      ISEND ,IRECV )
          ALLOCATE (IAD_STSH(NSPMD+1),FR_STSH(NBS))
          CALL TSHCDCOM_INI(ISEND,IAD_ELEM,FR_ELEM,IAD_STSH,FR_STSH)
          ALLOCATE (IAD_RTSH(NSPMD+1),FR_RTSH(NBR))
          CALL TSHCDCOM_INI(IRECV,IAD_ELEM,FR_ELEM,IAD_RTSH,FR_RTSH)
          DEALLOCATE(ISEND,IRECV)
         END IF
       END IF
!$OMP END SINGLE
C-------------------------------------------
      RETURN
      END
C
C-----------------------------------------------
Chd|====================================================================
Chd|  GRPSPLIT                      source/engine/resol_init.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        SORT_MID_PID                  source/system/sort_mid_pid.F  
Chd|====================================================================
      SUBROUTINE  GRPSPLIT(
     1     IPARG, IGROUC, NGROUC, IGROUNC, NGROUNC,
     2     IXC,IXS,IXTG,IPM,IGEO,PM,GEO,TABMP_L,TAB_MAT)
C----6------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IGROUC(*),IGROUNC(*),
     .        NGROUC, NGROUNC,TABMP_L

      INTEGER IXC(NIXC,*),IXS(NIXS,*),IXTG(NIXTG,*),
     .        IPM(NPROPMI,*),IGEO(NPROPGI,*)
      my_real PM(NPROPM,*),GEO(NPROPG,*)
      my_real TAB_MAT(NGROUP)
!     tab_mat_prop 
!                 1 : shell
!                 2 : tri
!           3 --> 9 : solid
!                 3 : ISOL=8
!                 4 : ISOL=10
!                 5 : ISOL=16
!                 6 : ISOL=20
!                 7 : ISOL=6
!                 8 : ISOL=4
!                 9 : ISOL=others
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, ITY, N_SHELL, N_SOL(7),N_TRI,MARQUEUR,MARQUEUR_2,MARQUEUR_3
      INTEGER I,J,II,JJ,K,INDI
      INTEGER COMPTEUR_MAT_PROP_SHELL,COMPTEUR_MAT_PROP_SOL,COMPTEUR_MAT_PROP_TRI,
     .        MID,PID,MTN,NEL,NFT,FIRST,LAST,SHIFT,ISOL,GR_ID,GR_ID2
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_SHELL_LOC,TAB_TRI_LOC
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: TAB_SOL_LOC
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_LOC
      INTEGER, DIMENSION(:), ALLOCATABLE :: IGROUC_SHELL,IGROUC_TRI,MID_SHELL,MID_TRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: POIN_GROUP_MID_SHELL,POIN_GROUP_MID_TRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: POIN_GROUP_PID_SHELL,POIN_GROUP_PID_TRI
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: POIN_GROUP_MID_SOL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: POIN_GROUP_PID_SOL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IGROUC_SOL,MID_SOL
      my_real POIDS_J,POIDS_J1
C-----------------------------------------------
C
       N_SHELL = 0
       N_SOL(1:7) = 0
       N_TRI = 0
       NGROUC = 0
       NGROUNC = 0

       ALLOCATE(TAB_SHELL_LOC(NGROUP,5))
       ALLOCATE(TAB_TRI_LOC(NGROUP,5))
       ALLOCATE(TAB_SOL_LOC(NGROUP,5,7))
       ALLOCATE( IGROUC_SHELL(NGROUP),IGROUC_TRI(NGROUP) )
       ALLOCATE( IGROUC_SOL(NGROUP,7) )

       ALLOCATE( POIN_GROUP_MID_SHELL(NGROUP),POIN_GROUP_MID_TRI(NGROUP) )
       ALLOCATE( POIN_GROUP_PID_SHELL(NGROUP),POIN_GROUP_PID_TRI(NGROUP) )
       ALLOCATE( POIN_GROUP_MID_SOL(NGROUP,7),POIN_GROUP_PID_SOL(NGROUP,7) )

       ALLOCATE(MID_SHELL(NUMMAT))
       ALLOCATE(MID_TRI(NUMMAT))
       ALLOCATE(MID_SOL(NUMMAT,7))

       COMPTEUR_MAT_PROP_SHELL = 0
       MID_SHELL(1:NUMMAT) = 0
       MID_TRI(1:NUMMAT) = 0
       MID_SOL(1:NUMMAT,1:7) = 0

       DO NG = 1, NGROUP
         ITY   =IPARG(5,NG)
         IF(ITY.EQ.3.OR.ITY.EQ.7)THEN
           NGROUC = NGROUC + 1
           IGROUC(NGROUC)=NG
           IF(ITY==3) THEN  
            N_SHELL = N_SHELL + 1
            NFT   = IPARG(3,NG)+1
            MID = IXC(1,NFT)
            PID = IXC(6,NFT)
            MTN = IPARG(1,NG)
            MID_SHELL(MID) = MID_SHELL(MID) + 1
            POIN_GROUP_MID_SHELL(N_SHELL) = MID
            POIN_GROUP_PID_SHELL(N_SHELL) = PID
            IGROUC_SHELL(N_SHELL) = NG

            TAB_SHELL_LOC(N_SHELL,1) = IPARG(2,NG)
            TAB_SHELL_LOC(N_SHELL,2) = NG
            TAB_SHELL_LOC(N_SHELL,3) = MID
            TAB_SHELL_LOC(N_SHELL,4) = PID
            TAB_SHELL_LOC(N_SHELL,5) = NGROUC

           ELSEIF(ITY==7) THEN
            N_TRI = N_TRI + 1
            NFT   = IPARG(3,NG)+1
            MID = IXTG(1,NFT)
            PID = IXTG(5,NFT)
            MTN = IPARG(1,NG)
            MID_TRI(MID) = MID_TRI(MID) + 1
            POIN_GROUP_MID_TRI(N_TRI) = MID
            POIN_GROUP_PID_TRI(N_TRI) = PID
            IGROUC_TRI(N_TRI) = NG

            TAB_TRI_LOC(N_TRI,1) = IPARG(2,NG)
            TAB_TRI_LOC(N_TRI,2) = NG
            TAB_TRI_LOC(N_TRI,3) = MID
            TAB_TRI_LOC(N_TRI,4) = PID
            TAB_TRI_LOC(N_TRI,5) = NGROUC

           ENDIF
         ELSE
           NGROUNC = NGROUNC + 1
           IGROUNC(NGROUNC)=NG
           IF(ITY==1) THEN
            NFT   = IPARG(3,NG)+1
            MID = IXS(1,NFT)
            PID = IXS(10,NFT)
            MTN = IPARG(1,NG)
            ISOL = IPARG(28,NG)
            IF(ISOL==4) THEN
             INDI = 6
            ELSEIF(ISOL==6) THEN
             INDI = 5
            ELSEIF(ISOL==8) THEN
             INDI = 1
            ELSEIF(ISOL==10) THEN
             INDI = 2
            ELSEIF(ISOL==16) THEN
             INDI = 3
            ELSEIF(ISOL==20) THEN
             INDI = 4
            ELSE
             INDI = 7
            ENDIF

            N_SOL(INDI) = N_SOL(INDI) + 1
            IGROUC_SOL(N_SOL(INDI),INDI) = NG

            TAB_SOL_LOC(N_SOL(INDI),1,INDI) = IPARG(2,NG)
            TAB_SOL_LOC(N_SOL(INDI),2,INDI) = NG
            TAB_SOL_LOC(N_SOL(INDI),3,INDI) = MID
            TAB_SOL_LOC(N_SOL(INDI),4,INDI) = PID
            TAB_SOL_LOC(N_SOL(INDI),5,INDI) = NGROUNC

            MID_SOL(MID,INDI) = MID_SOL(MID,INDI) + 1 
            POIN_GROUP_MID_SOL(N_SOL(INDI),INDI) = MID
            POIN_GROUP_PID_SOL(N_SOL(INDI),INDI) = PID

           ENDIF
         END IF
       END DO
! -------------------------
       IF(N_SHELL>0) THEN

        ALLOCATE( TAB_LOC(N_SHELL,3) )
        TAB_LOC(1:N_SHELL,1:3) = -1


        CALL SORT_MID_PID(N_SHELL,IGROUC_SHELL,
     1                    POIN_GROUP_MID_SHELL,POIN_GROUP_PID_SHELL,
     2                    MID_SHELL,TAB_LOC,TAB_SHELL_LOC,TAB_MAT)


        DO I = 1,N_SHELL
                J = TAB_LOC(I,1)
                II = TAB_SHELL_LOC(I,5)
                JJ = TAB_SHELL_LOC(J,2)
                IGROUC(II) = JJ
        ENDDO        
 
        DEALLOCATE( TAB_LOC )
       ENDIF ! N_SHELL>0 
! -------------------------
       IF(N_TRI>0) THEN

        ALLOCATE( TAB_LOC(N_TRI,3) )
        TAB_LOC(1:N_TRI,1:3) = -1


        CALL SORT_MID_PID(N_TRI,IGROUC_TRI,
     1                    POIN_GROUP_MID_TRI,POIN_GROUP_PID_TRI,
     2                    MID_TRI,TAB_LOC,TAB_TRI_LOC,TAB_MAT)


        DO I = 1,N_TRI
                J = TAB_LOC(I,1)
                II = TAB_TRI_LOC(I,5)
                JJ = TAB_TRI_LOC(J,2)
                IGROUC(II) = JJ
        ENDDO
 
        DEALLOCATE( TAB_LOC )
       ENDIF ! N_TRI>0 
! -------------------------
       DO INDI=1,7
        IF(N_SOL(INDI)>0) THEN

                ALLOCATE( TAB_LOC(N_SOL(INDI),3) )
                TAB_LOC(1:N_SOL(INDI),1:3) = -1


                CALL SORT_MID_PID(N_SOL(INDI),IGROUC_SOL(1,INDI),
     1                            POIN_GROUP_MID_SOL(1,INDI),POIN_GROUP_PID_SOL(1,INDI),
     2                            MID_SOL(1,INDI),TAB_LOC,TAB_SOL_LOC(1,1,INDI),TAB_MAT)


                DO I = 1,N_SOL(INDI)
                        J = TAB_LOC(I,1)
                        II = TAB_SOL_LOC(I,5,INDI)
                        JJ = TAB_SOL_LOC(J,2,INDI)
                        IGROUNC(II) = JJ
                ENDDO
 
                DEALLOCATE( TAB_LOC )
        ENDIF ! N_SOL>0 
       ENDDO
! -------------------------

       DEALLOCATE(MID_SHELL)
       DEALLOCATE(MID_TRI)
       DEALLOCATE(MID_SOL)

       DEALLOCATE( POIN_GROUP_MID_SHELL,POIN_GROUP_MID_TRI )
       DEALLOCATE( POIN_GROUP_PID_SHELL,POIN_GROUP_PID_TRI )
       DEALLOCATE( POIN_GROUP_MID_SOL,POIN_GROUP_PID_SOL )


       DEALLOCATE(TAB_SHELL_LOC)
       DEALLOCATE(TAB_TRI_LOC)
       DEALLOCATE(TAB_SOL_LOC)
       DEALLOCATE( IGROUC_SHELL,IGROUC_TRI )
       DEALLOCATE( IGROUC_SOL )

       RETURN
       END
C
C-----------------------------------------------
Chd|====================================================================
Chd|  FILLIPARTL                    source/engine/resol_init.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  FILLIPARTL(
     1     IPARTL ,IPARTS,IPARTQ ,IPARTC ,IPARTT,
     2     IPARTP ,IPARTR,IPARTUR,IPARTTG,IPARTX,
     3     IPARTSP,IPARTIG3D,NPARTL)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "rnur_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------!$OMP+PRIVATE(
      INTEGER IPARTS(*),IPARTQ(*),IPARTC(*),IPARTT(*),IPARTSP(*),
     .        IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),IPARTX(*),
     .        IPARTL(*),IPARTIG3D(*),
     .        NPARTL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C //
C-----------------------------------------------
C
       DO I = 1, NPART
         IPARTL(I) = 0
       END DO
C
       DO I = 1, NUMELS
         IPARTL(IPARTS(I))=1
       END DO
C
       DO I = 1, NUMELQ
         IPARTL(IPARTQ(I))=1
       END DO
C
       DO I = 1, NUMELC
         IPARTL(IPARTC(I))=1
       END DO
C
       DO I = 1, NUMELT
         IPARTL(IPARTT(I))=1
       END DO
C
       DO I = 1, NUMELP
         IPARTL(IPARTP(I))=1
       END DO
C
       DO I = 1, NUMELR
         IPARTL(IPARTR(I))=1
       END DO
C
       DO I = 1, NUMELUR
         IPARTL(IPARTUR(I))=1
       END DO
C
       DO I = 1, NUMELTG
         IPARTL(IPARTTG(I))=1
       END DO
C
       DO I = 1, NUMELX
         IPARTL(IPARTX(I))=1
       END DO
C
       DO I = 1, NUMELS
         IPARTL(IPARTS(I))=1
       END DO
C
       DO I = 1, NUMSPH
         IPARTL(IPARTSP(I))=1
       END DO
C
       DO I = 1, NUMELIG3D
         IPARTL(IPARTIG3D(I))=1
       END DO
C
       NPARTL = 0
       DO I = 1, NPART
         IF(IPARTL(I).GT.0)THEN
           NPARTL = NPARTL + 1
           IPARTL(NPARTL) = I
         END IF
       END DO
C
       RETURN
       END
C
Chd|====================================================================
Chd|  SMP_INIT                      source/engine/resol_init.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMP_INIT(
     1   ITSK  ,NODFTSK ,NODLTSK ,NUMNTSK,NDTSK,
     2   IPMTSK,PARTFTSK,PARTLTSK,NWAFTSK,IGMTSK,
     3   GREFTSK,GRELTSK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITSK, NODFTSK, NODLTSK, NUMNTSK, NDTSK,
     1        IPMTSK, PARTFTSK, PARTLTSK, NWAFTSK, IGMTSK,
     2        GREFTSK,GRELTSK
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LENWA_T, OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C      
C     Initialisation des variables pour // SMP
C 
      ITSK = OMP_GET_THREAD_NUM()
      NODFTSK = 1+ITSK*NUMNOD/ NTHREAD
      NODLTSK = (ITSK+1)*NUMNOD/NTHREAD
      NUMNTSK = NODLTSK - NODFTSK + 1
      NDTSK = 1 + ITSK*NUMNOD
      IPMTSK = 1 + ITSK*NPSAV*NPART
      PARTFTSK = 1+ITSK*NPSAV*NPART/ NTHREAD
      PARTLTSK = (ITSK+1)*NPSAV*NPART/NTHREAD
      LENWA_T = LENWA / NTHREAD
      NWAFTSK = 1+ITSK*LENWA_T
      IGMTSK = 1 + ITSK*NPSAV*NGPE
      GREFTSK = 1+ITSK*NPSAV*NGPE/ NTHREAD
      GRELTSK = (ITSK+1)*NPSAV*NGPE/NTHREAD
c     NWALTSK = (ITSK+1)*LENWA_T     
c     LOUT = ISPMD.EQ.0.AND.ITSK.EQ.0
C
      RETURN
      END
Chd|====================================================================
Chd|  INIT_KYNE                     source/engine/resol_init.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_KYNE(IKINE,NPBY,LPBY,TAGSLV_RBY)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "lagmult.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKINE(NUMNOD),NPBY(NNPBY,*),LPBY(*),TAGSLV_RBY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N,I,J,K,NSN

      DO J=1,NUMNOD
        IKINE(J)=0
      ENDDO
C-------------------------------------
C Traitement Rigid Bodies 
C-------------------------------------
      K = 0
      DO N=1,NRBYKIN
        NSN = NPBY(2,N)
        DO I=1,NSN
          J=LPBY(K+I)
          IKINE(J) = (IKINE(J)/2)*2 + 1
        ENDDO
        K = K + NSN
      ENDDO
C-------------------------------------------
      TAGSLV_RBY(1:NUMNOD)=0
      K=0
      DO N=1,NRBYKIN
       NSN=NPBY(2,N)
       IF(NPBY(7,N).GE.1)THEN
         DO I=1,NSN
           TAGSLV_RBY(LPBY(I+K))=N
         ENDDO
       ENDIF
       K=K+NSN
      ENDDO
C-------------------------------------------
      DO N=1,NRBYLAG 
        NSN = NPBY(2,N)
        DO I=1,NSN
          J=LPBY(K+I)
          IKINE(J) = (IKINE(J)/2)*2 + 1
        ENDDO
        K = K + 3*NSN
      ENDDO
      RETURN
      END
