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
Chd|====================================================================
Chd|  SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANICON0                       source/output/anim/generate/anicon0.F
Chd|        ANICON2                       source/output/anim/generate/anicon2.F
Chd|        ANI_PCONT                     source/output/anim/generate/ani_pcont.F
Chd|        ANI_PCONT21                   source/output/anim/generate/ani_pcont.F
Chd|        CNDMASI2                      source/elements/solid/solide10/s10cndf.F
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        ENG_QAPRINT_DRIVER            source/output/qaprint/eng_qaprint_driver.F
Chd|        FCONT2_MAX_OUTPUT             source/output/h3d/h3d_results/fcont2_max_output.F
Chd|        FCONT2_MIN_OUTPUT             source/output/h3d/h3d_results/fcont2_min_output.F
Chd|        FCONT_MAX_OUTPUT              source/output/h3d/h3d_results/fcont_max_output.F
Chd|        FLUXSW                        source/fluid/fluxsw.F         
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|        HIST2                         source/output/th/hist2.F      
Chd|        NOISE                         source/general_controls/computation/noise.F
Chd|        S10MVND                       source/elements/solid/solide10/s10cndv.F
Chd|        SPMD_EXCH_ADM                 source/mpi/anim/spmd_exch_adm.F
Chd|        SPMD_SD_SKW_ANIM              source/mpi/output/spmd_sd_skw.F
Chd|        ALEANIM_MOD                   share/modules/aleanim_mod.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ECND_MOD                      share/modules/ecdn_mod.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        GENANI_MOD                    source/output/anim/generate/genani.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE SORTIE_MAIN(
     1            PM        ,D           ,V          ,ALE_CONNECT     ,W        ,         
     2            ELBUF     ,IPARG       ,IXS        ,IXQ       ,IXC      ,         
     3            IXT       ,IXP         ,IXR        ,IXTG      ,WA       ,         
     4            ITAB      ,X           ,GEO        ,MS        ,A        ,         
     5            CONT      ,PARTSAV                 ,ICUT      ,XCUT     ,         
     6            FINT      ,FEXT        ,FOPT       ,ANIN      ,LPBY     ,         
     7            NPBY      ,NSTRF       ,RWBUF      ,NPRW      ,
     8            TANI      ,INOISE      ,BUFNOIS    ,RBY       ,NEFLSW   ,         
     9            NNFLSW    ,CRFLSW      ,FLSW       ,LOUT      ,                
     B            FSAV      ,SKEW        ,ELBUF_TAB  ,CLUSTER   ,                   
     C            VR        ,IN          ,WEIGHT     ,FCLUSTER  ,MCLUSTER ,         
     D            DD_IAD    ,DMAS        ,DINER      ,ACCELM    ,GAUGE,             
     E            IPARI     ,EANI        ,IPART,                                    
     F            IGRNOD    ,SUBSET      ,      
     G            NOM_OPT   ,AR          ,IGRSURF    ,BUFSF     ,IDATA    ,         
     H            RDATA     ,KXX         ,IXX        ,BUFMAT    ,BUFGEO   ,         
     I            KXSP      ,IXSP        ,NOD2SP     ,SPBUF     ,DR        ,                    
     J            FSAVD     ,IXRI        ,RIVET      ,ISKWN     ,IFRAME    ,               
     M            XFRAME    ,IXS10       ,IXS20      ,IXS16     ,NDMA      ,         
     T            MONVOL    ,VOLMON      ,IPM        ,IGEO      ,NODGLOB   ,
     .            IAD_ELEM  ,FR_ELEM  ,         
     U            FR_RBY2   ,IAD_RBY2    ,FR_WALL    ,FR_SEC    ,FXBIPM   ,FXBRPM,  
     V            NDIN      ,FXBDEP      ,FXBVIT     ,FXBACC    ,IFLOW,             
     W            RFLOW     ,IPARTL      ,NPARTL     ,IACCP     ,NACCP,             
     X            FASOLFR   ,FNCONT      ,FTCONT     ,IPARTH    ,         
     Y            FR_MV     ,IPART_STATE ,SH4TREE    ,SH3TREE   ,TEMP,              
     Z            THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,                             
     a            INOD_PXFEM,FTHREAC     ,NODREAC    ,CPTREAC   ,GRESAV   ,         
     b            DIAG_SMS  ,SH4TRIM     ,SH3TRIM    ,FNCONT2   ,XMOM_SMS ,         
     c            IRBE2     ,IRBE3       ,LRBE2      ,LRBE3     ,FR_RBE2  ,         
     d            FR_RBE3M  ,IAD_RBE2    ,DXANCG     ,IEL_PXFEM ,ZI_PLY   ,         
     e            VGAZ      ,FCONTG      ,FNCONTG    ,FTCONTG   ,FANREAC  ,         
     f            INOD_CRK  ,IEL_CRK     ,ELCUTC     ,IADC_CRK  ,PDAMA2   ,         
     g            RES_SMS   ,SENSOR_TAB,                   
     h            QFRICINT  ,IGAUP       ,NGAUP      ,WEIGHT_MD ,NCONT    ,         
     i            INDEXCONT ,NODGLOBXFE  ,NODEDGE    ,XFEM_TAB  ,           
     j            NV46      ,RTHBUF      ,KXIG3D     ,IXIG3D    ,KNOT     ,
     k            WIGE      ,NERCVOIS    ,NESDVOIS   ,LERCVOIS  ,LESDVOIS ,
     l            CRKEDGE   ,STACK       ,ISPHIO     ,VSPHIO    ,ICODE    ,
     m            INDX_CRK  ,XEDGE4N     ,XEDGE3N    ,SPH2SOL   ,STIFN    ,
     n            STIFR     ,DRAPE_SH4N, DRAPE_SH3N       ,MS_2D     ,MULTI_FVM,
     o            SEGQUADFR ,H3D_DATA    ,ISKEW      ,PSKIDS    ,ISKWP    ,
     p            KNOTLOCPC ,KNOTLOCEL   ,PINCH_DATA ,TAG_SKINS6,IRUNN_BIS, 
     q            TF        ,NPC         ,DYNAIN_DATA,FCONT_MAX,MDS_MATID,
     r            FNCONTP2  ,FTCONTP2    ,IBCL        ,ILOADP   ,LLOADP   ,
     s            LOADP     ,TAGNCONT    ,LOADP_HYD_INTER,FORC  ,DRAPEG,USER_WINDOWS,
     t            OUTPUT ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GENANI_MOD
      USE ELBUFDEF_MOD         
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD  
      USE CRACKXFEM_MOD 
      USE STACK_MOD
      USE ECND_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
      USE ALEANIM_MOD !FANI_CELL 
      USE PINCHTYPE_MOD
      USE INOUTFILE_MOD
      USE DRAPE_MOD
      USE SENSOR_MOD
      USE ALE_CONNECTIVITY_MOD
      USE OUTMAX_MOD
      USE ANIM_MOD
      USE OUTPUTS_MOD
      USE MESSAGE_MOD
      USE STATE_MOD
      USE USER_WINDOWS_MOD
      USE OUTPUT_MOD
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
#include      "scr07_c.inc"
#include      "scrnoi_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr20_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "scrfs_c.inc"
#include      "task_c.inc"
#include      "scr13_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com10_c.inc"
#include      "spmd_c.inc"
#include      "fxbcom.inc"
#include      "filescount_c.inc"
#include      "intstamp_c.inc"
#include      "my_allocate.inc"
#include      "chara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      LOGICAL LOUT
      INTEGER IPARG(*), IXS(*), IXQ(*), IXC(*), IXT(*), IXP(*),
     .   IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*) ,INDX_CRK(*),
     .   IXR(*), IXTG(*), ITAB(*),ICUT(*),NSTRF(*),LPBY(*),
     .   NPRW(*),IPART(*),
     .   INOISE(*), NPBY(NNPBY,*),NEFLSW(*),NNFLSW(*),
     .   DD_IAD(NSPMD+1,*), IACCP(*), NACCP(*),
     .   IPARI(NPARI,*),WEIGHT(*),
     .   NOM_OPT(LNOPT1,*),
     .   IDATA(*),KXX(NIXX,*),IXX(*),
     .   KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .   NPARTSAV,IXRI(4,*),
     .   ISKWN(LISKN,*),IFRAME(LISKN,*),IPM(*), IGEO(*),FXBIPM(NBIPM,*),
     .   IPARTH(2,*),
     .   NDMA ,MONVOL(*),NODGLOB(*),
     .   FR_ELEM(*),IAD_ELEM(2,*), NDIN, IFLOW(*), FASOLFR(*), FR_MV(*),
     .   IPARTL(*), NPARTL, IPART_STATE(*), SH4TREE(*), SH3TREE(*),
     .   INOD_PXFEM(*), SH4TRIM(*), SH3TRIM(*),
     .   IRBE2(*),IRBE3(*),LRBE2(*),LRBE3(*),
     .   FR_RBE2(*),FR_RBE3M(*),IEL_PXFEM(*),
     .   INOD_CRK(*),IEL_CRK(*),ELCUTC(2,*),XEDGE4N(4,*),XEDGE3N(3,*),
     .   IADC_CRK(*),IGAUP(*), NGAUP(*),
     .   WEIGHT_MD(*),NODGLOBXFE(*),NODEDGE(2,*),NV46,KXIG3D(*),
     .   IXIG3D(*),NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .   ISPHIO(NISPHIO,*),ICODE(*),SPH2SOL(*),SEGQUADFR(*),
     .   ISKEW(*),TAG_SKINS6(*),IRUNN_BIS
       my_real
     .   BUFNOIS(*),PARTSAV(NPSAV,*),RBY(NRBY,*),CRFLSW(*),FLSW(*),
     .   FSAV(NTHVKI,*),SKEW(LSKEW,*), VR(3,*), IN(*), DMAS,DINER,
     .   ACCELM(LLACCELM,*),BUFSF(*),RDATA(*),BUFMAT(*),BUFGEO(*),
     .   SPBUF(*),RIVET(*),XFRAME(NXFRAME,*),ZI_PLY(*),VGAZ(*)
      my_real
     .   PM(NPROPM,*),D(*),V(*),W(*),ELBUF(*),WA(*),X(*),GEO(*),
     .   MS(*), A(3,*), CONT(3,*),XCUT(*),FINT(*),EANI(*),
     .   FEXT(3,*) ,FOPT(6,*),ANIN(*),RWBUF(*),TANI(*),
     .   AR(3,*),TF(*),FCONT_MAX(*)
      my_real
     .   DIFF,FSAVD(NTHVKI,*),
     .   DR(3,*),VOLMON(*),
     .   FXBRPM(*),FXBDEP(*),FXBVIT(*),FXBACC(*), RFLOW(*),
     .   FNCONT(3,*),FTCONT(3,*),TEMP(*),THKE(*),DXANCG(3,*),
     .   ERR_THK_SH4(*),ERR_THK_SH3(*),FTHREAC(*),
     .   GRESAV(*), DIAG_SMS(*), FNCONT2(3,*), XMOM_SMS(3,*),
     .   FCONTG(*), FNCONTG(*), FTCONTG(*),FANREAC(*),PDAMA2(*),
     .   RES_SMS(*),QFRICINT(*), GAUGE(*),
     .   FCLUSTER(*),MCLUSTER(*),RTHBUF(*),WIGE(*),KNOT(*),VSPHIO(*),
     .   STIFN(*),STIFR(*),MS_2D(*),PSKIDS(*),KNOTLOCPC(*),KNOTLOCEL(*),
     .   FNCONTP2(3,*)  ,FTCONTP2(3,*)  
      INTEGER FR_SEC(NSPMD+1,*),FR_RBY2(3,*),IAD_RBY2(4,*),FR_WALL(*),
     .   NODREAC(*),IAD_RBE2(4,*),INDEXCONT(*),NCONT,ITY
      INTEGER CPTREAC,NPC(*),MDS_MATID(*)
      INTEGER LLOADP(*),ILOADP(SIZLOADP,*),IBCL(NIBCLD,*)
      INTEGER TAGNCONT(NLOADP_HYD_INTER,NUMNOD),LOADP_HYD_INTER(NLOADP_HYD)
      my_real
     .       LOADP(LFACCLD,*),FORC(*)

      INTEGER, DIMENSION(*), INTENT(IN) :: ISKWP
C-----------------------------------------------      
      TYPE(OUTPUT_),INTENT(INOUT) :: OUTPUT
C-----------------------------------------------      
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)         :: CLUSTER
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (STACK_PLY) :: STACK
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)  :: IGRSURF
      TYPE (PINCH) :: PINCH_DATA
      TYPE (DRAPE_)  :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_)  :: DRAPEG
      TYPE (SENSOR_STR_), DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
      TYPE(USER_WINDOWS_),INTENT(IN)        :: USER_WINDOWS     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,I,M,JALE,NEL,NFACE,
     .        K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11,K12,
     .        NEL1D,NEL2D,NEL3D, L1, IFIL, IFILABF, CPTFILE,
     .        IAD, J, THUPDT,IFLAG, IFV,ITHFLAG
      INTEGER
     .    KUIX, KXUSR ,KFACPTX,KXEDGE,KXFACET,KXSOLID,KNUMX1,KNUMX2,
     .    KNUMX3,KOFFX1,KOFFX2,KOFFX3,KMASS1,KMASS2,KMASS3,
     .    KFUNC1,KFUNC2,KFUNC3,KFIN,IFLAG_ANIPCONT
      INTEGER
     .  SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,SN9,NELG,
     .  SKUIX, SKXUSR ,SKFACPTX,SKXEDGE,SKXFACET,SKXSOLID,SKNUMX1,
     .  SKNUMX2,SKNUMX3,SKOFFX1,SKOFFX2,SKOFFX3,SKMASS1,SKMASS2,
     .  SKMASS3,SKFUNC1,SKFUNC2,SKFUNC3,SKFIN,SIG3DSOLID,IERROR
      INTEGER
     .  LENR,LENG,LENGC,LENGTG,LENGS,LENGT,LENGR,LENGP,LENGQ
      INTEGER SIZE_MES,LONG,LONG_TMP,N
      INTEGER IFILNAM(2148),IFILNAM_TMP(2148)
      my_real
     .   TFEXTH,t1,t2
      my_real
     .   TABFWR0(10),BID
      my_real
     .      ,  DIMENSION(:),ALLOCATABLE :: VMCDN
      my_real
     .      ,  DIMENSION(:),ALLOCATABLE :: CSEFRIC,CSEFRIC_STAMP,CSEFRICG,CSEFRICG_STAMP
      CHARACTER FILNAMABF*100,FILNAMABF_TMP*100

      INTEGER :: LEN_TMP_NAME,LEN_TMP_NAME2
      CHARACTER(len=100) :: TMP_NAME,TMP_NAME2   
C=======================================================================
      IF(IPLYXFEM == 0 ) ANIM_PLY = 0
      IF(ICRACK3D > 0 ) ANIM_CRK = 1
      CPTFILE=0
C----------------------
C       FICHIERS NOISE
C----------------------
      IF(NNOISE>0)THEN
        CALL NOISE(
     1     BUFNOIS,INOISE,INOISE(2*NNOISE+1),BUFNOIS(2),V     ,
     2     A      ,IXS   ,ELBUF_TAB        ,IPARG     ,WEIGHT,
     3     IXQ    )
      ENDIF
C-----------------------
C       ECRITURE LISTING
C-----------------------
C      IF(T1S==TT) THEN
C-------should use relative velocity for dynamic condensed nodes (Nd of S10)
       IF (NS10E > 0) THEN
        LENG = 3*NUMNOD
        ALLOCATE(VMCDN(LENG))
        VMCDN(1:LENG)=V(1:LENG)
        CALL S10MVND(ICNDS10 ,VND   ,VMCDN    )
        CALL CNDMASI2(ICNDS10,NKEND,IMAP2ND,MASI2ND0,MS ,V  ,A   ,
     1                WEIGHT ,MAS_ND ,KEND  )
        CALL ECRIT(PARTSAV  ,MS      ,VMCDN  ,IN       ,VR     , 
     2             RBY      ,NPBY    ,DMAS   ,DINER    ,WEIGHT ,
     3             A        ,AR      ,FXBIPM ,FXBRPM   ,MONVOL ,
     4             XMOM_SMS ,SENSOR_TAB,QFRICINT ,IPARI  ,
     5             WEIGHT_MD,TFEXTH ,IFLAG   ,MS_2D, MULTI_FVM ,
     6             MAS_ND   ,KEND   ,H3D_DATA,DYNAIN_DATA,USER_WINDOWS%USREINT)
        DEALLOCATE(VMCDN)
       ELSE
        BID = ZERO
        CALL ECRIT(PARTSAV  ,MS      ,V      ,IN       ,VR     , 
     2             RBY      ,NPBY    ,DMAS   ,DINER    ,WEIGHT ,
     3             A        ,AR      ,FXBIPM ,FXBRPM   ,MONVOL ,
     4             XMOM_SMS ,SENSOR_TAB,QFRICINT ,IPARI  ,
     5             WEIGHT_MD,TFEXTH  ,IFLAG  ,MS_2D, MULTI_FVM ,
     6             BID      ,BID     ,H3D_DATA,DYNAIN_DATA,USER_WINDOWS%USREINT)
       ENDIF
C---------------------------------------
C       CALCUL DU FLUX ET DE LA ROTATION
C---------------------------------------
        IF(TT>=THIS .AND. NSFLSW/=0)
     .  CALL FLUXSW(NSFLSW,NEFLSW,  NNFLSW,  CRFLSW,  FLSW  ,
     .              X     ,V       ,ELBUF  )
C-----------------------------------------------
C     CUMUL MASS SPMD
C-----------------------------------------------
      IF(IMACH==3.AND.NSPMD>1.AND.(ANIM_N(2)+OUTP_N(2)>0).AND.
     .  ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.
     .   MANIM==4 .OR.MANIM==5 .OR.MANIM==6 .OR.MANIM==7.OR.
     .   MANIM==8 .OR.MANIM==9 .OR.MANIM==10.OR.MANIM==11.OR.
     .   MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15)) THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_ADM(ANIN(NDMA+1),IAD_ELEM,FR_ELEM,LENR,WEIGHT_MD)
      ENDIF
      IF(IMACH==3.AND.NSPMD>1.AND.
     .  (ANIM_N(12)+OUTP_N(3)>0).AND.
     .  ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.
     .   MANIM==4 .OR.MANIM==5 .OR.MANIM==6 .OR.MANIM==7.OR.
     .   MANIM==8 .OR.MANIM==9 .OR.MANIM==10.OR.MANIM==11.OR.
     .   MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15)) THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_ADM(ANIN(NDIN+1),IAD_ELEM,FR_ELEM,LENR,WEIGHT)
      ENDIF
C----------------------
C       Compute contact pressures from normal + tangential contact forces.
C       Tied Contact : Inter type2
C----------------------
      IFLAG_ANIPCONT = 0

      IF((H3D_DATA%N_SCAL_CSE_FRIC /=0).OR.(H3D_DATA%N_SCAL_CSE_FRICINT /=0)
     .    .AND.
     .   (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D .OR.
     .  MANIM==4 .OR.MANIM==5 .OR.MANIM==6 .OR.MANIM==7.OR.
     .  MANIM==8 .OR.MANIM==9 .OR.MANIM==10.OR.MANIM==11.OR.
     .  MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15.OR.
     .  H3D_DATA%MH3D/=0)) THEN

           IF(NINEFRIC > 0) THEN
             MY_ALLOCATE(CSEFRIC,NINEFRIC*NUMNOD) ! friction energy by surface unit
           ELSE
             MY_ALLOCATE(CSEFRIC,0)
           ENDIF   
           IF(NINEFRIC_STAMP > 0) THEN
             MY_ALLOCATE(CSEFRIC_STAMP,NINEFRIC_STAMP*NUMNODG) ! friction energy by surface unit

           ELSE
             MY_ALLOCATE(CSEFRIC_STAMP,0) 
           ENDIF 
           IF(H3D_DATA%N_SCAL_CSE_FRIC /=0) THEN
              MY_ALLOCATE(CSEFRICG,NUMNOD) ! Global friction energy by surface unit
              CSEFRICG = ZERO
              IF(NINTSTAMP /= 0) THEN
                 MY_ALLOCATE(CSEFRICG_STAMP,NUMNODG) ! Global friction energy by surface unit
              ELSE
                 MY_ALLOCATE(CSEFRICG_STAMP,0) 
              ENDIF
           ELSE
              MY_ALLOCATE(CSEFRICG,0)
              MY_ALLOCATE(CSEFRICG_STAMP,0) 
           ENDIF
      ELSE
         MY_ALLOCATE(CSEFRIC,0)
         MY_ALLOCATE(CSEFRIC_STAMP,0)
         MY_ALLOCATE(CSEFRICG,0)
         MY_ALLOCATE(CSEFRICG_STAMP,0) 
      ENDIF


      IF(((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT /=0.OR.
     .   ANIM_V(27)+H3D_DATA%N_VECT_PCONT2 /=0..OR.
     .   H3D_DATA%N_SCAL_CSE_FRIC /=0.OR.H3D_DATA%N_SCAL_CSE_FRICINT /=0)
     .    .AND.
     .   ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP) .OR.
     .  MANIM==4 .OR.MANIM==5 .OR.MANIM==6 .OR.MANIM==7.OR.
     .  MANIM==8 .OR.MANIM==9 .OR.MANIM==10.OR.MANIM==11.OR.
     .  MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15.OR.
     .  H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0
     .   .OR.H3D_DATA%N_VECT_PCONT2_MAX>0)THEN
        IF(NINTSTAMP==0)THEN
          CALL ANI_PCONT(IXS ,IXC    ,IXTG    ,FASOLFR  ,X      ,
     .               FNCONT  ,FTCONT,IAD_ELEM ,FR_ELEM  ,WEIGHT ,
     .               IXQ     ,SEGQUADFR,IXS10 ,FNCONTP2,FTCONTP2,
     .               H3D_DATA,CSEFRIC,CSEFRICG)
        ELSE
          IFLAG_ANIPCONT = 1
          CALL ANI_PCONT21(IXS ,IXC    ,IXTG    ,FASOLFR  ,X      ,
     .               FNCONT    ,FTCONT ,IAD_ELEM,FR_ELEM  ,WEIGHT ,
     .               NODGLOB   ,FNCONTG,FTCONTG,FNCONTP2,FTCONTP2,
     .               H3D_DATA  ,CSEFRIC_STAMP,CSEFRICG_STAMP)
        END IF
      END IF
C------------------------------------------------------
C       Compute maximum contact force and pressure
C----------------------------------------------------
      IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0) THEN
         CALL FCONT_MAX_OUTPUT(CONT,FCONTG,NODGLOB,FCONT_MAX,WEIGHT)
      ENDIF 
      IF(H3D_DATA%N_VECT_PCONT_MAX /=0) THEN
         CALL FCONT_MAX_OUTPUT(FNCONT,FNCONTG,NODGLOB,FNCONT_MAX,WEIGHT)
         CALL FCONT_MAX_OUTPUT(FTCONT,FTCONTG,NODGLOB,FTCONT_MAX,WEIGHT)
      ENDIF 
      IF(H3D_DATA%N_VECT_CONT2_MAX /=0) THEN
         CALL FCONT2_MAX_OUTPUT(FNCONT2,FCONT2_MAX,WEIGHT)
      ENDIF 
      IF(H3D_DATA%N_VECT_PCONT2_MAX /=0) THEN
         CALL FCONT2_MAX_OUTPUT(FNCONTP2,FNCONT2_MAX,WEIGHT)
         CALL FCONT2_MAX_OUTPUT(FTCONTP2,FTCONT2_MAX,WEIGHT)
      ENDIF 
      IF(H3D_DATA%N_VECT_CONT2_MIN /=0) THEN
         CALL FCONT2_MIN_OUTPUT(FNCONT2,FCONT2_MIN,WEIGHT)
      ENDIF 
      IF(H3D_DATA%N_VECT_PCONT2_MAX /=0) THEN
         CALL FCONT2_MIN_OUTPUT(FNCONTP2,FNCONT2_MIN,WEIGHT)
         CALL FCONT2_MIN_OUTPUT(FTCONTP2,FTCONT2_MIN,WEIGHT)
      ENDIF 
  
      IF ((TT>=TANIM .AND. TT<=TANIM_STOP).OR. ! NUMELCRK > NUMELCRK2 .or.   !  => ANIM par fissure avance
     .   MANIM==4 .OR.MANIM==5 .OR.MANIM==6 .OR.MANIM==7.OR.
     .   MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15 .OR.
     .   (MSTOP == 1  .AND. ICTLSTOP == 0) .OR. 
     .   MSTOP == 2 .OR. DT2<=ZERO )THEN
       IANIM=IANIM+1
C      N1 = 1
C      N2 = N1 + NUMNOD
C      N3 = 1
C      N4 = N3 + 3 * NUMNOD
C      N5 = N4 + NUMMAT + NUMGEO + 1
C      N6 = N5 + NUMELC + NUMELTG + NUMELQ + 6*NUMELS
C    .         + NUMELT + NUMELP  + NUMELR
C      N7 = N6 + NUMMAT

!       need to communicate the SKEW array on main proc 
         CALL SPMD_SD_SKW_ANIM(SKEW,ISKWP)
         NEL3D = NUMELS + NUMSPH + 3*NUMELS16
         NEL2D = NUMELC + NUMELTG + NUMELQ
         NEL1D = NUMELT + NUMELP  + 2*NUMELR + NANIM1D
         NEL = MAX(NEL1D,NEL2D,NEL3D)
C
         NELG = MAX (  NUMELSG+3*NUMELS16G+NUMSPHG+27*NUMELIG3D,
     .                 NUMELCG+NUMELTGG+NUMELQG,
     .                 NUMELTRG + NUMELPG  + 2*NUMELRG + NANIM1D)
         SN1 = MAX(3*NUMNOD,6*NEL3D+6*27*NUMELIG3D,3*NEL2D,9*NEL1D,NUMSPH)
         SN2 = NEL+3*NUMELS16+NUMSPH
         SN3 = 3 * NUMNOD + 2*NUMELS16 + 64*NUMELIG3D
         IF (NSPMD > 1) THEN
            DO IFV = 1, NFVBAG
               IF (KMESH(IFV) > 1) THEN
                  SN3 = SN3 + 3 * FVSPMD(IFV)%NNA_L
               ENDIF
            ENDDO
         ENDIF
         SN4 = NPART + 1
         SN5 = NEL2D
         SN6 = NPART
         IF (IMACH==3) THEN
           SN7 = NELG + 1
         ELSE
           SN7 = NEL+1
         ENDIF
         N1 = 1
         N2 = N1 + MAX(3*NUMNOD,6*NEL3D,3*NEL2D,9*NEL1D)
         N3 = N2 + NEL
         N4 = N3 + 3 * NUMNOD
         N5 = N4 + NPART + 1
         N6 = N5 + NEL2D
         N7 = N6 + NPART
         N8 = N7 + NEL + 1
          IF  (     (IMACH/=3.AND.NUMELX>0)
     .          .OR.(IMACH==3.AND.NUMELXG>0)) THEN

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
c a modifier
         SIG3DSOLID = 8*27*NUMELIG3D
c
         IF (IMACH==3) THEN
          SN9 =  NPART
         ELSE
          SN9 = 1
         ENDIF
C
         K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
         K2=K1+NUMELS
         K3=K2+NUMELQ
         K4=K3+NUMELC
         K5=K4+NUMELT
         K6=K5+NUMELP
         K7=K6+NUMELR
         K8=K7+0
         K9=K8+NUMELTG
         K10=K9+NUMELX
         K11=K10+NUMSPH
         K12=K11+NUMELIG3D
         L1=1+6*(NUMELC+NUMELTG)*IEPSDOT
c
	 CALL GENANI(
     1  	   X	      ,D	 ,V	      ,A	   ,ELBUF     ,
     2  	   IXS        ,IXQ	 ,IXC	      ,IXT	   ,IXP       ,
     3  	   IXR        ,IXTG	 ,SN1	      ,SN2	   ,SN3       ,
     4  	   SN4        ,IPARG	 ,PM	      ,GEO	   ,MS        ,
     5  	   SN5        ,CONT	 ,SN6	      ,ICUT	   ,SKEW      ,
     6  	   XCUT       ,FINT	 ,ITAB        ,SN7	   ,FEXT      ,
     7  	   FOPT       ,ANIN	 ,LPBY        ,NPBY	   ,NSTRF     ,
     8  	   RWBUF      ,NPRW	 ,TANI	   ,ELBUF_TAB ,
     A  	   DD_IAD     ,WEIGHT	 ,EANI        ,IPART	   ,CLUSTER   ,
     B  	   IPART(K1)  ,IPART(K2) ,IPART(K3)   ,IPART(K4)   ,IPART(K5) ,
     C  	   IPART(K6)  ,IPART(K7) ,IPART(K8)   ,
     D  	   RBY        ,SN3	 ,TANI(L1)    ,NOM_OPT     ,IGRSURF   ,
     E  	   BUFSF      ,IDATA	 ,RDATA       ,SN9	   ,BUFMAT    ,
     F  	   BUFGEO     ,KXX	 ,IXX	      ,IPART(K9)   ,SKUIX     ,
     G  	   SKXUSR     ,SKFACPTX  ,SKXEDGE     ,SKXFACET    ,SKXSOLID  ,
     H  	   SKNUMX1    ,SKNUMX2   ,SKNUMX3     ,SKOFFX1     ,SKOFFX2   ,
     I  	   SKOFFX3    ,SKMASS1   ,SKMASS2     ,SKMASS3     ,SKFUNC1   ,
     J  	   SKFUNC2    ,SKFUNC3   ,KXSP        ,IXSP	   ,NOD2SP    ,
     K  	   IPART(K10) ,SPBUF	 ,IXS10       ,IXS20	   ,IXS16     ,
     L  	   VR	      ,MONVOL	 ,VOLMON      ,IPM	   ,IGEO,NODGLOB,
     M  	   IAD_ELEM   ,FR_ELEM   ,FR_SEC      ,FR_RBY2     ,IAD_RBY2  ,
     N  	   FR_WALL    ,IFLOW	 ,RFLOW       ,FNCONT	   ,FTCONT    ,
     O  	   TEMP       ,THKE	 ,ERR_THK_SH4 ,ERR_THK_SH3 ,DIAG_SMS,
     P  	   IPARI      ,FNCONT2   ,DR	      ,ALE_CONNECT	   ,
     Q  	   IRBE2      ,IRBE3	 ,LRBE2       ,LRBE3	   ,FR_RBE2   ,
     R  	   FR_RBE3M   ,IAD_RBE2  ,DXANCG      ,INOD_PXFEM  ,IEL_PXFEM,
     S  	   ZI_PLY     ,VGAZ	 ,FCONTG      ,FNCONTG     ,FTCONTG  ,
     T  	   FANREAC    ,INOD_CRK  ,IEL_CRK     ,ELCUTC	   ,IADC_CRK ,
     U  	   PDAMA2     ,RES_SMS   ,WEIGHT_MD   ,NODGLOBXFE  ,NODEDGE  ,
     V  	   FCLUSTER   ,MCLUSTER  ,XFEM_TAB    ,W	   ,
     W  	   NV46       ,IPART(K11),KXIG3D      ,IXIG3D	   ,SIG3DSOLID,
     X  	   KNOT       ,WIGE	 ,NERCVOIS    ,NESDVOIS    ,LERCVOIS  ,
     Y  	   LESDVOIS   ,CRKEDGE   ,INDX_CRK    ,XEDGE4N     ,XEDGE3N   ,
     Z  	   STACK      ,SPH2SOL   ,STIFN       ,STIFR	   ,IGRNOD    ,
     1             H3D_DATA   ,SUBSET    ,MULTI_FVM   ,KNOTLOCPC   ,KNOTLOCEL ,
     2             FCONT_MAX  ,FNCONTP2  ,FTCONTP2    )
c
       ENDIF
      IF((TT>=TANIM .AND. TT<=TANIM_STOP))TANIM=TANIM+DTANIM
      IF ((TSTOP<TANIM).AND.(TANIM<TSTOP+EM06*TSTOP)
     .     .AND.(TSTOP+EM06*TSTOP<TANIM+DTANIM)) THEN
         TANIM=TSTOP
      END IF
C-----------------------------------------------
C     H3D FILE
C-----------------------------------------------
      IF (ILASTH3D==1.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP) .OR. H3D_DATA%MH3D ==1 .OR.
     .   (MSTOP == 1  .AND. ICTLSTOP == 0 .AND. H3D_DATA%IH3D/= 0) .OR. 
     .   (MSTOP == 2  .AND. H3D_DATA%IH3D/= 0) .OR. 
     .   (DT2<=ZERO .AND. H3D_DATA%IH3D/= 0) )THEN

         H3D_DATA%IH3D=H3D_DATA%IH3D+1


         NEL3D = NUMELS + NUMSPH + 3*NUMELS16
         NEL2D = NUMELC + NUMELTG + NUMELQ
         NEL1D = NUMELT + NUMELP  + 2*NUMELR + NANIM1D
         NEL = MAX(NEL1D,NEL2D,NEL3D)
C
         NELG = MAX (  NUMELSG+3*NUMELS16G+NUMSPHG+27*NUMELIG3D,
     .                 NUMELCG+NUMELTGG+NUMELQG,
     .                 NUMELTRG + NUMELPG  + 2*NUMELRG + NANIM1D)
         SN1 = MAX(3*NUMNOD,6*NEL3D+6*27*NUMELIG3D,3*NEL2D,9*NEL1D,NUMSPH)
         SN2 = NEL+3*NUMELS16+NUMSPH
         SN3 = 3 * NUMNOD + 2*NUMELS16 + 64*NUMELIG3D
         SN4 = NPART + 1
         SN5 = NEL2D
         SN6 = NPART
         IF (IMACH==3) THEN
           SN7 = NELG + 1
         ELSE
           SN7 = NEL+1
         ENDIF
         N1 = 1
         N2 = N1 + MAX(3*NUMNOD,6*NEL3D,3*NEL2D,9*NEL1D)
         N3 = N2 + NEL
         N4 = N3 + 3 * NUMNOD
         N5 = N4 + NPART + 1
         N6 = N5 + NEL2D
         N7 = N6 + NPART
         N8 = N7 + NEL + 1
          IF  (     (IMACH/=3.AND.NUMELX>0)
     .          .OR.(IMACH==3.AND.NUMELXG>0)) THEN

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
c a modifier
         SIG3DSOLID = 8*27*NUMELIG3D
c
         IF (IMACH==3) THEN
          SN9 =  NPART
         ELSE
          SN9 = 1
         ENDIF
C
         K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
         K2=K1+NUMELS
         K3=K2+NUMELQ
         K4=K3+NUMELC
         K5=K4+NUMELT
         K6=K5+NUMELP
         K7=K6+NUMELR
         K8=K7+0
         K9=K8+NUMELTG
         K10=K9+NUMELX
         K11=K10+NUMSPH
         K12=K11+NUMELIG3D
         L1=1+6*(NUMELC+NUMELTG)*IEPSDOT
c
         CALL GENH3D(
     1             X          ,D         ,V           ,A           ,ELBUF     ,
     2             IXS        ,IXQ       ,IXC         ,IXT         ,IXP       ,
     3             IXR        ,IXTG      ,SN1         ,SN2         ,SN3       ,
     4             SN4        ,IPARG     ,PM          ,GEO         ,MS        ,
     5             SN5        ,CONT      ,SN6         ,ICUT        ,SKEW      ,
     6             XCUT       ,FINT      ,ITAB        ,SN7         ,FEXT      ,
     7             FOPT       ,ANIN      ,LPBY        ,NPBY        ,NSTRF     ,
     8             RWBUF      ,NPRW      ,TANI        ,ELBUF_TAB ,
     A             DD_IAD     ,WEIGHT    ,EANI        ,IPART       ,CLUSTER   ,
     B             IPART(K1)  ,IPART(K2) ,IPART(K3)   ,IPART(K4)   ,IPART(K5) ,
     C             IPART(K6)  ,IPART(K7) ,IPART(K8)   ,
     D             RBY        ,SN3       ,TANI(L1)    ,NOM_OPT     ,
     E             BUFSF      ,IDATA     ,RDATA       ,SN9         ,BUFMAT    ,
     F             BUFGEO     ,KXX       ,IXX         ,IPART(K9)   ,SKUIX     ,
     G             SKXUSR     ,SKFACPTX  ,SKXEDGE     ,SKXFACET    ,SKXSOLID  ,
     H             SKNUMX1    ,SKNUMX2   ,SKNUMX3     ,SKOFFX1     ,SKOFFX2   ,
     I             SKOFFX3    ,SKMASS1   ,SKMASS2     ,SKMASS3     ,SKFUNC1   ,
     J             SKFUNC2    ,SKFUNC3   ,KXSP        ,IXSP        ,NOD2SP    ,
     K             IPART(K10) ,SPBUF     ,IXS10       ,IXS20       ,IXS16     ,
     L             VR         ,MONVOL    ,VOLMON      ,IPM         ,IGEO,NODGLOB,
     M             IAD_ELEM   ,FR_ELEM   ,FR_SEC      ,FR_RBY2     ,IAD_RBY2  ,
     N             FR_WALL    ,IFLOW     ,RFLOW       ,FNCONT      ,FTCONT    ,
     O             TEMP       ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3 ,DIAG_SMS,
     P             IPARI      ,FNCONT2   ,DR          ,ALE_CONNECT       ,
     Q             IRBE2      ,IRBE3     ,LRBE2       ,LRBE3       ,FR_RBE2   ,
     R             FR_RBE3M   ,IAD_RBE2  ,DXANCG      ,INOD_PXFEM  ,IEL_PXFEM,
     S             ZI_PLY     ,VGAZ      ,FCONTG      ,FNCONTG     ,FTCONTG  ,
     T             FANREAC    ,INOD_CRK  ,IEL_CRK     ,ELCUTC      ,IADC_CRK ,
     U             PDAMA2     ,RES_SMS   ,WEIGHT_MD   ,NODGLOBXFE  ,NODEDGE  ,
     V             FCLUSTER   ,MCLUSTER  ,XFEM_TAB    ,W           ,
     W             NV46       ,IPART(K11),KXIG3D      ,IXIG3D      ,SIG3DSOLID,
     X             KNOT       ,WIGE      ,NERCVOIS    ,NESDVOIS    ,LERCVOIS  ,
     Y             LESDVOIS   ,CRKEDGE   ,INDX_CRK    ,XEDGE4N     ,XEDGE3N   ,
     Z             STACK      ,SPH2SOL   ,STIFN       ,STIFR       ,IGRNOD    ,
     1             SH4TREE    ,SH3TREE   ,H3D_DATA    ,MULTI_FVM   ,SUBSET    ,
     2             PSKIDS     ,TAG_SKINS6,TF          ,NPC         ,FCONT_MAX ,
     3             MDS_MATID  ,FNCONTP2  ,FTCONTP2    ,IBCL        ,ILOADP    ,
     4             LLOADP     ,LOADP     ,SENSOR_TAB  ,TAGNCONT    ,LOADP_HYD_INTER,
     5             XFRAME     ,FORC      ,AR          ,CSEFRIC     ,CSEFRICG  ,
     6             CSEFRIC_STAMP,CSEFRICG_STAMP)
      ENDIF
      IF(ALLOCATED(CSEFRIC)) DEALLOCATE(CSEFRIC)
      IF(ALLOCATED(CSEFRICG)) DEALLOCATE(CSEFRICG)
      IF(ALLOCATED(CSEFRIC_STAMP)) DEALLOCATE(CSEFRIC_STAMP)
      IF(ALLOCATED(CSEFRICG_STAMP)) DEALLOCATE(CSEFRICG_STAMP)

      IF(TT>=H3D_DATA%TH3D) H3D_DATA%TH3D = H3D_DATA%TH3D + H3D_DATA%DTH3D
      IF((TSTOP< H3D_DATA%TH3D).AND.(H3D_DATA%TH3D < TSTOP+EM06*TSTOP) .AND.(TSTOP+EM06*TSTOP< H3D_DATA%TH3D + H3D_DATA%DTH3D)) THEN
         H3D_DATA%TH3D = TSTOP
      END IF

      IF (IMACH==3.AND.ISPMD==0) THEN
          LENG = NUMNODG
      ELSE
          LENG = 0
      ENDIF
C
      IF(TT>=TOUTP.OR.
     .   MANIM==8 .OR.MANIM==9 .OR.MANIM==10.OR.MANIM==11.OR.
     .   MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15.OR.
     .   MOUTPT==1) THEN
       IOUTP=IOUTP+1
       CALL GENOUTP(X     ,D        ,V      ,A      ,
     2             IXS   ,IXQ       ,IXC    ,IXT    ,IXP   ,
     3             IXR   ,IXTG      ,IPARG  ,PM     ,IGEO  ,
     4             MS    ,CONT      ,ITAB   ,PARTSAV,FINT  ,
     5             FEXT  ,TANI      ,EANI   ,ANIN   ,IPART ,
     6             VR    ,ELBUF_TAB ,DD_IAD ,WEIGHT,
     7             IPM   ,KXSP      ,SPBUF  ,NODGLOB,LENG  ,
     8             FOPT  ,NOM_OPT   ,NPBY   ,FNCONT ,FTCONT ,
     9             GEO   ,THKE      ,STACK  ,DRAPE_SH4N, DRAPE_SH3N,
     A             DRAPEG)
       IF(TT>=TOUTP)TOUTP=TOUTP+DTOUTP
       IF ((TSTOP<TOUTP).AND.(TOUTP<TSTOP+EM06*TSTOP)
     .     .AND.(TSTOP+EM06*TSTOP<TOUTP+DTOUTP)) THEN
         TOUTP=TSTOP
       END IF
      END IF
C
C .sta files
      IF(TT>=TSTAT.OR.
     .   MANIM==8 .OR.MANIM==9 .OR.MANIM==10.OR.MANIM==11.OR.
     .   MANIM==12.OR.MANIM==13.OR.MANIM==14.OR.MANIM==15.OR.
     .   MSTATT==1) THEN
       ISTATF=ISTATF+1

       K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
       K2=K1+NUMELS
       K3=K2+NUMELQ
       K4=K3+NUMELC
       K5=K4+NUMELT
       K6=K5+NUMELP
       K7=K6+NUMELR
       K8=K7+0
       K9=K8+NUMELTG
       K10=K9+NUMELX
       K11=K10+NUMSPH
C
       IF(IMACH/=3)THEN
         LENGC = NUMELC
         LENGTG= NUMELTG
         LENGS = NUMELS
         LENGR = NUMELR
         LENGP = NUMELP
         LENGT = NUMELT
         LENGQ = NUMELQ
       ELSEIF (ISPMD==0) THEN
         LENGC = NUMELCG
         LENGTG= NUMELTGG
         LENGS = NUMELSG
         LENGR = NUMELRG
         LENGP = NUMELPG
         LENGT = NUMELTRG
         LENGQ = NUMELQG
       ELSE
         LENGC = 0
         LENGTG= 0
         LENGS = 0
         LENGR = 0
         LENGP = 0
         LENGT = 0
         LENGQ = 0
       ENDIF
C
       CALL GENSTAT(X        ,MS    ,ELBUF_TAB, ELBUF  ,SPBUF ,
     2             IXS      ,IXQ    ,IXC    ,IXT   ,IXP    ,
     3             IXR      ,IXTG   ,KXSP   ,IPARG ,IPM    ,
     4             IGEO     ,ITAB   ,IPART  ,PM    ,GEO    ,
     5             IPART(K1),IPART(K2),IPART(K3),IPART(K4),IPART(K5) ,
     6             IPART(K6),IPART(K7),IPART(K8),IPART(K9),IPART(K10),
     7             DD_IAD   ,WEIGHT,NODGLOB ,LENG  ,IPART_STATE,
     8             LENGC    ,LENGTG,SH4TREE ,SH3TREE, LENGS,
     9             SH4TRIM  ,SH3TRIM,TEMP  ,IXS10  ,THKE  ,
     A             IXS16    ,IXS20  ,ICODE ,LENGR ,LENGP  ,
     B             LENGT    ,ISKEW  ,V     ,VR    ,LENGQ  ,
     C             MULTI_FVM,BUFMAT ,NPBY  ,LPBY  ,STACK  ,
     D             DRAPE_SH4N, DRAPE_SH3N,DR,DRAPEG   )
       IF(TT>=TSTAT)TSTAT=TSTAT+DTSTAT
       IF ((TSTOP<TSTAT).AND.(TSTAT<TSTOP+EM06*TSTOP)
     .     .AND.(TSTOP+EM06*TSTOP<TSTAT+DTSTAT)) THEN
         TSTAT=TSTOP
       END IF
      END IF
C----------------------
C     DYNAIN FILE
C----------------------
      IF(TT>=DYNAIN_DATA%TDYNAIN) THEN
       DYNAIN_DATA%IDYNAINF=DYNAIN_DATA%IDYNAINF+1

       K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
       K2=K1+NUMELS
       K3=K2+NUMELQ
       K4=K3+NUMELC
       K5=K4+NUMELT
       K6=K5+NUMELP
       K7=K6+NUMELR
       K8=K7+0
       K9=K8+NUMELTG
       K10=K9+NUMELX
       K11=K10+NUMSPH
C
       IF(IMACH/=3)THEN
         LENGC = NUMELC
         LENGTG= NUMELTG
       ELSEIF (ISPMD==0) THEN
         LENGC = NUMELCG
         LENGTG= NUMELTGG
       ELSE
         LENGC = 0
         LENGTG= 0
       ENDIF
C
       CALL GENDYNAIN(X        ,ELBUF_TAB, ELBUF  ,IXC      ,IXTG     ,
     2                IPARG    ,IPM      , IGEO   ,ITAB     ,IPART    ,
     3                PM       ,GEO      , IPART(K3),IPART(K8),LENG   ,
     4                LENGC    ,LENGTG   , WEIGHT ,NODGLOB  ,THKE     ,               
     5                NPBY     ,LPBY     , STACK  ,DRAPE_SH4N ,DRAPE_SH3N , 
     6                DYNAIN_DATA,DRAPEG) 
             
       IF(TT>=DYNAIN_DATA%TDYNAIN)DYNAIN_DATA%TDYNAIN=DYNAIN_DATA%TDYNAIN+DYNAIN_DATA%DTDYNAIN
       IF ((TSTOP<DYNAIN_DATA%TDYNAIN).AND.(DYNAIN_DATA%TDYNAIN<TSTOP+EM06*TSTOP)
     .     .AND.(TSTOP+EM06*TSTOP<DYNAIN_DATA%TDYNAIN+DYNAIN_DATA%DTDYNAIN)) THEN
         DYNAIN_DATA%TDYNAIN=TSTOP
       END IF
      END IF

C----------------------
C       FICHIERS G ET T
C----------------------
        IF(NABFILE /= 0) THEN
          TABFWR0(1) = TABFWR(1)
          TABFWR0(2) = TABFWR(2)
          TABFWR0(3) = TABFWR(3)
          TABFWR0(4) = TABFWR(4)
          TABFWR0(5) = TABFWR(5)
          TABFWR0(6) = TABFWR(6)
          TABFWR0(7) = TABFWR(7)
          TABFWR0(8) = TABFWR(8)
          TABFWR0(9) = TABFWR(9)
          TABFWR0(10) = TABFWR(10)
        ENDIF
        IF(NINTER+NEXMAD/=0.AND.ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT > 0)THEN
            CALL ANICON0(CONT,NUMNOD)
            !law151 reaction forces can be output only with H3D (not ANIM files since vectores are at element centroids)
            IF (MULTI_FVM%IS_USED .AND. H3D_DATA%N_VECT_CONT > 0)CALL ANICON0(FANI_CELL,NUMELS+NUMELQ) !/H3D/ELEM/VECT/CONT
        ENDIF
        IF(NINTSTAMP/=0.AND.ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0)THEN
            CALL ANICON2(FCONTG,NCONT,INDEXCONT)     
        ENDIF
        IF(NINTER/=0.AND.ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0)THEN
            CALL ANICON0(FNCONT,NUMNOD)
            CALL ANICON0(FTCONT,NUMNOD)
        ENDIF
        IF(NINTSTAMP/=0.AND.ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0)THEN
          IF(IFLAG_ANIPCONT > 0) THEN
            CALL ANICON0(FNCONTG,NUMNODG)
            CALL ANICON0(FTCONTG,NUMNODG)
          ELSE
            CALL ANICON2(FNCONTG,NCONT,INDEXCONT)
            CALL ANICON2(FTCONTG,NCONT,INDEXCONT)  
          ENDIF
        ENDIF

        IF(NINTER/=0.AND.ANIM_V(27)+H3D_DATA%N_VECT_PCONT2>0)THEN
            CALL ANICON0(FNCONTP2,NUMNOD)
            CALL ANICON0(FTCONTP2,NUMNOD)
        ENDIF

        N1 = 1
C second tableau temporaire de taille ngroup pour le spmd
        N2 = 1
C
      IF (TH_VERS < 40)THEN

            WRITE(ISTDO,*)' ** ERROR : TH VERSION OLDER THAN 41 IS ',
     *                    'NO MORE SUPPORTED'
      ELSE
c check if an update of any output (TH,tha,thb,abf,..) is done  
c in this case set flag to 1    
        THUPDT = 0
        IF(TT>=THIS) THEN
	  THUPDT = 1
	ELSEIF(NABFILE/=0)THEN
	  IF(TT>=TABFIS(1)) THUPDT = 1
	ENDIF
	IF(THUPDT==0)THEN	
     	  DO I=1,9
	    IF(TT>=THIS1(I)) THEN
	      THUPDT = 1
	    ELSEIF(NABFILE/=0)THEN
	      IF(TT>=TABFIS(I+1)) THUPDT = 1
	    ENDIF
	  ENDDO
	ENDIF

        IF (ILASTANIM/=1.AND.ILASTANIM/=2) THEN
C         to avoid double points
          IF (NTHGRP01(1)/=0)THEN
            IUNIT = IUHI(1)
            IFIL = IUNIT
            IF (TT>=THIS1(1))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 1
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                 IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                 FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,                                 
     5                 PARTSAV,ACCELM,LOUT ,
     6                 IPARI,WEIGHT,IPART,IGRSURF,
     7                 OUTPUT%TH%ITHGRPA, OUTPUT%TH%ITHBUFA, SUBSET,GEO  ,
     8                 KXX   ,IXR,
     9                 KXSP  ,NOD2SP ,SPBUF ,
     B                 NPARTSAV,AR   ,VR    ,DR,
     D                 FSAVD ,IXRI   ,RIVET  ,IXP      ,
     E                 ISKWN ,IFRAME ,XFRAME ,IXC      ,IXQ ,
     F                 DTHIS1(1), THIS1(1), IFIL, NTHGRP1(1),IXTG ,
     G                 IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                 FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                 IPARTH ,2     ,1     ,
     J                 1      ,MONVOL,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                 FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                 IGAUP  ,NGAUP ,AFORM(1),WEIGHT_MD,SIZE_MES,
     M                 RTHBUF ,THKE ,STACK ,ISPHIO  ,VSPHIO,
     N                 ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUFA)

            IF (SIZE_MES==1 .AND.AFORM(1)==3 .AND.ISPMD==0 ) 
     *          CALL file_size(MULTITHFILESIZE(1))



            IF(ABFILE(2) /= 0) THEN
#ifdef DNC
            IFILABF = 12
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB,                               
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPA,OUTPUT%TH%ITHBUFA,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(2), TABFIS(2),IFILABF,NTHGRP1(1),IXTG,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPARTH ,2     ,1      ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(2),
     L                  2     ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF ,THKE ,
     M                  STACK ,ISPHIO  ,VSPHIO,ITHFLAG,1,PINCH_DATA,MULTI_FVM,W) 
            CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(2) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_a.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_a.abf'
              LONG = 11

              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO
!               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(2),IFILNAM,
!     .                      ROOTLEN+LONG,IFILNAM_TMP,ROOTLEN+LONG_TMP,
!     .                      CPTFILE)
               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(2),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(2) = 0
             ENDIF
#endif
            ELSE
              TABFIS(2)= EP30
            ENDIF

          ELSE
            THIS1(1)= EP30
            TABFIS(2)= EP30
          ENDIF
C
          IF(NTHGRP01(2)/=0)THEN
            IUNIT = IUHI(2)
            IFIL = IUNIT
            IF (TT>=THIS1(2))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 2
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                 IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                 FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                 PARTSAV,ACCELM,LOUT ,
     6                 IPARI,WEIGHT,IPART,IGRSURF,
     7                 OUTPUT%TH%ITHGRPB,OUTPUT%TH%ITHBUFB,SUBSET,GEO  ,
     8                 KXX   ,IXR,
     9                 KXSP  ,NOD2SP ,SPBUF ,
     B                 NPARTSAV,AR   ,VR    ,DR,
     D                 FSAVD ,IXRI   ,RIVET  ,IXP ,
     E                 ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                 DTHIS1(2), THIS1(2), IFIL, NTHGRP1(2),IXTG ,
     G                 IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                 FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                 IPARTH(1,(NPART+NTHPART)+1),2,1,
     J                 1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                 FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                 IGAUP  ,NGAUP, AFORM(2),WEIGHT_MD,SIZE_MES,
     M                 RTHBUF ,THKE ,STACK,ISPHIO  ,VSPHIO,
     N                 ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUFB)
            IF (SIZE_MES==1 .AND.AFORM(2)==3 .AND.ISPMD==0 )
     *        CALL file_size(MULTITHFILESIZE(2))
     
            IF(ABFILE(3) /= 0) THEN
#ifdef DNC
            IFILABF = 13
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPB,OUTPUT%TH%ITHBUFB,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(3), TABFIS(3),IFILABF,NTHGRP1(2),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPARTH(1,(NPART+NTHPART)+1),2   ,1  ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(3),
     L                  3     ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF ,THKE,
     M                  STACK ,ISPHIO  ,VSPHIO,ITHFLAG,2,PINCH_DATA,MULTI_FVM,W)
            CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(3) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_b.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_b.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)

              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO

               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(3),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(3) = 0
             ENDIF
#endif
            ELSE
              TABFIS(3)= EP30
            ENDIF



          ELSE
            THIS1(2)= EP30
            TABFIS(3)= EP30
          ENDIF
C
          IF(NTHGRP01(3)/=0)THEN
            IUNIT = IUHI(3)
            IFIL = IUNIT
!
            IF (TT>=THIS1(3))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 3
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                 IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                 FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                 PARTSAV,ACCELM,LOUT ,
     6                 IPARI,WEIGHT,IPART,IGRSURF,
     7                 OUTPUT%TH%ITHGRPC,OUTPUT%TH%ITHBUFC,SUBSET,GEO  ,
     8                 KXX   ,IXR,
     9                 KXSP  ,NOD2SP ,SPBUF ,
     B                 NPARTSAV,AR   ,VR    ,DR,
     D                 FSAVD ,IXRI   ,RIVET  , IXP , 
     E                 ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                 DTHIS1(3), THIS1(3), IFIL, NTHGRP1(3),IXTG ,
     G                 IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                 FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                 IPARTH(1,2*(NPART+NTHPART)+1),2,1,
     J                 1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                 FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                 IGAUP  ,NGAUP ,AFORM(3),WEIGHT_MD,SIZE_MES,
     M                 RTHBUF ,THKE ,STACK ,ISPHIO  ,VSPHIO,
     n                 ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUFC)

        IF(SIZE_MES==1 .AND.
     *     AFORM(3)==3   .AND.
     *     ISPMD==0 ) CALL file_size(MULTITHFILESIZE(3))
            IF(ABFILE(4) /= 0) THEN
#ifdef DNC
            IFILABF = 14
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPC,OUTPUT%TH%ITHBUFC,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(4), TABFIS(4),IFILABF,NTHGRP1(1),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPART  ,2     ,1      ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(4),
     L                  4      ,GAUGE  ,IGAUP,NGAUP  ,RTHBUF  ,THKE  ,
     M                  STACK  ,ISPHIO  ,VSPHIO,ITHFLAG,1,PINCH_DATA,MULTI_FVM,W)
              CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(4) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_c.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_c.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)

              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO
               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(4),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(4) = 0
             ENDIF
#endif
            ELSE
              TABFIS(4)= EP30
            ENDIF
          ELSE
            THIS1(3)= EP30
            TABFIS(4)= EP30
          ENDIF
C
          IF(NTHGRP01(4)/=0)THEN
            IUNIT = IUHI(4)
            IFIL = IUNIT
!
            IF (TT>=THIS1(4))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 4
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                 IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                 FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                 PARTSAV,ACCELM,LOUT ,
     6                 IPARI,WEIGHT,IPART,IGRSURF,
     7                 OUTPUT%TH%ITHGRPD,OUTPUT%TH%ITHBUFD,SUBSET,GEO  ,
     8                 KXX   ,IXR,
     9                 KXSP  ,NOD2SP ,SPBUF ,
     B                 NPARTSAV,AR   ,VR    ,DR,
     D                 FSAVD ,IXRI   ,RIVET  , IXP,
     E                 ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                 DTHIS1(4), THIS1(4), IFIL, NTHGRP1(4),IXTG ,
     G                 IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                 FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                 IPARTH(1,3*(NPART+NTHPART)+1),2,1,
     J                 1      ,MONVOL ,VOLMON,FR_MV ,TEMP,INOD_PXFEM,
     K                 FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                 IGAUP  ,NGAUP ,AFORM(4),WEIGHT_MD,SIZE_MES,
     M                 RTHBUF ,THKE ,STACK ,ISPHIO  ,VSPHIO,
     N                 ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUFD)
         IF(SIZE_MES==1 .AND.
     *     AFORM(4)==3   .AND.
     *     ISPMD==0 )CALL file_size(MULTITHFILESIZE(4))


         IF(ABFILE(5) /= 0) THEN
#ifdef DNC
            IFILABF = 15
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPD,OUTPUT%TH%ITHBUFD,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(5), TABFIS(5),IFILABF,NTHGRP1(1),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPART  ,2     ,1      ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(5),
     L                  5     ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF   ,THKE ,
     M                  STACK ,ISPHIO  ,VSPHIO,ITHFLAG,1,PINCH_DATA,MULTI_FVM,W)
              CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(5) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_d.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_d.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO
               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(5),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(5) = 0
             ENDIF
#endif
            ELSE
              TABFIS(5)= EP30
            ENDIF
          ELSE
            THIS1(4)= EP30
            TABFIS(5)= EP30
          ENDIF
C
          IF(NTHGRP01(5)/=0)THEN
            IUNIT = IUHI(5)
            IFIL = IUNIT
!
            IF (TT>=THIS1(5))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 5
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                PARTSAV,ACCELM,LOUT ,
     6                IPARI,WEIGHT,IPART,IGRSURF,
     7                OUTPUT%TH%ITHGRPE,OUTPUT%TH%ITHBUFE,SUBSET,GEO  ,
     8                KXX   ,IXR,
     9                KXSP  ,NOD2SP ,SPBUF ,
     B                NPARTSAV,AR   ,VR    ,DR,
     D                FSAVD ,IXRI   ,RIVET  , IXP , 
     E                ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                DTHIS1(5), THIS1(5), IFIL, NTHGRP1(5),IXTG ,
     G                IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                IPARTH(1,4*(NPART+NTHPART)+1),2,1,
     J                1      ,MONVOL ,VOLMON,FR_MV ,TEMP,INOD_PXFEM,
     K                FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                IGAUP  ,NGAUP ,AFORM(5),WEIGHT_MD,SIZE_MES,
     M                RTHBUF  ,THKE ,STACK  ,ISPHIO  ,VSPHIO,
     N                ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUFE)
        IF(SIZE_MES==1 .AND.
     *     AFORM(5)==3   .AND.
     *     ISPMD==0 ) CALL file_size(MULTITHFILESIZE(5))
     
        IF(ABFILE(6) /= 0) THEN
#ifdef DNC
            IFILABF = 16
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPE,OUTPUT%TH%ITHBUFE,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(6), TABFIS(6),IFILABF,NTHGRP1(1),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPART  ,2     ,1      ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(6),
     L                  6     ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF,THKE ,
     M                  STACK ,ISPHIO  ,VSPHIO,ITHFLAG,1,PINCH_DATA,MULTI_FVM,W)
              CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(6) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_e.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_e.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)

              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO
               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(6),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(6) = 0
             ENDIF
#endif
            ELSE
              TABFIS(6)= EP30
            ENDIF
          ELSE
            THIS1(5)= EP30
            TABFIS(6)= EP30
          ENDIF
C
          IF(NTHGRP01(6)/=0)THEN
            IUNIT = IUHI(6)
            IFIL = IUNIT
            IF (TT>=THIS1(6))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 6
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                PARTSAV,ACCELM,LOUT ,
     6                IPARI,WEIGHT,IPART,IGRSURF,
     7                OUTPUT%TH%ITHGRPF,OUTPUT%TH%ITHBUFF,SUBSET,GEO  ,
     8                KXX   ,IXR,
     9                KXSP  ,NOD2SP ,SPBUF ,
     B                NPARTSAV,AR   ,VR    ,DR,
     D                FSAVD ,IXRI   ,RIVET  , IXP , 
     E                ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                DTHIS1(6), THIS1(6), IFIL, NTHGRP1(6),IXTG ,
     G                IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                IPARTH(1,5*(NPART+NTHPART)+1),2,1,
     J                1      ,MONVOL,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                IGAUP,NGAUP ,AFORM(6),WEIGHT_MD,SIZE_MES,
     M                RTHBUF,THKE ,STACK  ,ISPHIO  ,VSPHIO,
     N                ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUFF)

        IF(SIZE_MES==1 .AND.
     *     AFORM(6)==3   .AND.
     *     ISPMD==0 ) CALL file_size(MULTITHFILESIZE(6))
     
            IF(ABFILE(7) /= 0) THEN
#ifdef DNC
            IFILABF = 17
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPF,OUTPUT%TH%ITHBUFF,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(7), TABFIS(7),IFILABF,NTHGRP1(1),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPART  ,2     ,1      ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(7),
     L                  7     ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF,THKE ,
     M                  STACK ,ISPHIO  ,VSPHIO,ITHFLAG,1,PINCH_DATA,MULTI_FVM,W)
              CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(7) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_f.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_f.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)

              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO
               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(7),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(7) = 0
             ENDIF
#endif
            ELSE
              TABFIS(7)= EP30
            ENDIF
          ELSE
            THIS1(6)= EP30
            TABFIS(7)= EP30
          ENDIF
C
          IF(NTHGRP01(7)/=0)THEN
            IUNIT = IUHI(7)
            IFIL = IUNIT
!
             IF (TT>=THIS1(7))THEN
               SIZE_MES=1
             ELSE
               SIZE_MES=0
             ENDIF
            ITHFLAG = 7
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                PARTSAV,ACCELM,LOUT ,
     6                IPARI,WEIGHT,IPART,IGRSURF,
     7                OUTPUT%TH%ITHGRPG,OUTPUT%TH%ITHBUFG,SUBSET,GEO  ,
     8                KXX   ,IXR,
     9                KXSP  ,NOD2SP ,SPBUF ,
     B                NPARTSAV,AR   ,VR    ,DR,
     D                FSAVD ,IXRI   ,RIVET  ,IXP , 
     E                ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                DTHIS1(7), THIS1(7), IFIL, NTHGRP1(7),IXTG ,
     G                IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                IPARTH(1,6*(NPART+NTHPART)+1),2,1,
     J                1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                FTHREAC,NODREAC,GRESAV ,GAUGE  ,
     L                IGAUP,NGAUP ,AFORM(7),WEIGHT_MD,SIZE_MES,
     M                RTHBUF,THKE ,STACK  ,ISPHIO  ,VSPHIO,
     N                ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUFG)
        IF(SIZE_MES==1 .AND.
     *     AFORM(7)==3   .AND.
     *     ISPMD==0 ) CALL file_size(MULTITHFILESIZE(7))
     
        IF(ABFILE(8) /= 0) THEN
#ifdef DNC
            IFILABF = 18
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPG,OUTPUT%TH%ITHBUFG,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(8), TABFIS(8),IFILABF,NTHGRP1(1),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPART  ,2     ,1      ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(8),
     L                  8      ,GAUGE  ,IGAUP,NGAUP  ,RTHBUF  ,THKE  ,
     M                  STACK  ,ISPHIO  ,VSPHIO,ITHFLAG,1,PINCH_DATA,MULTI_FVM,W)
              CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(8) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_g.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_g.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)

              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO
               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(8),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(8) = 0
             ENDIF
#endif
            ELSE
              TABFIS(8)= EP30
            ENDIF
          ELSE
            THIS1(7)= EP30
            TABFIS(8)= EP30
          ENDIF
C
          IF(NTHGRP01(8)/=0)THEN
            IUNIT = IUHI(8)
            IFIL = IUNIT
!
            IF (TT>=THIS1(8))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 8
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                PARTSAV,ACCELM,LOUT ,
     6                IPARI,WEIGHT,IPART,IGRSURF,
     7                OUTPUT%TH%ITHGRPH,OUTPUT%TH%ITHBUFH,SUBSET,GEO  ,
     8                KXX   ,IXR,
     9                KXSP  ,NOD2SP ,SPBUF ,
     B                NPARTSAV,AR   ,VR    ,DR,
     D                FSAVD ,IXRI   ,RIVET  , IXP , 
     E                ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                DTHIS1(8), THIS1(8), IFIL, NTHGRP1(8),IXTG ,
     G                IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                IPARTH(1,7*(NPART+NTHPART)+1),2,1,
     J                1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                IGAUP  ,NGAUP ,AFORM(8),WEIGHT_MD,SIZE_MES,
     M                RTHBUF ,THKE ,STACK ,ISPHIO  ,VSPHIO,
     N                ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%ITHBUFH)
        IF(SIZE_MES==1 .AND.
     *     AFORM(8)==3   .AND.
     *     ISPMD==0 ) CALL file_size(MULTITHFILESIZE(8))
     
       IF(ABFILE(9) /= 0) THEN
#ifdef DNC
            IFILABF = 19
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPH,OUTPUT%TH%ITHBUFH,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(9), TABFIS(9),IFILABF,NTHGRP1(1),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPART  ,2     ,1      ,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(9),
     L                  9     ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF,THKE ,
     M                  STACK ,ISPHIO  ,VSPHIO,ITHFLAG,1,PINCH_DATA,MULTI_FVM,W)
              CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(9) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_h.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_h.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)

              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO
               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(9),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(9) = 0
             ENDIF
#endif
            ELSE
              TABFIS(9)= EP30
            ENDIF
          ELSE
            THIS1(8)= EP30
            TABFIS(9)= EP30
          ENDIF
C
          IF(NTHGRP01(9)/=0)THEN
            IUNIT = IUHI(9)
            IFIL = IUNIT
            IF (TT>=THIS1(9))THEN
              SIZE_MES=1
            ELSE
              SIZE_MES=0
            ENDIF
            ITHFLAG = 9
            CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                PARTSAV,ACCELM,LOUT ,
     6                IPARI,WEIGHT,IPART,IGRSURF,
     7                OUTPUT%TH%ITHGRPI,OUTPUT%TH%ITHBUFI,SUBSET,GEO  ,
     8                KXX   ,IXR,
     9                KXSP  ,NOD2SP ,SPBUF ,
     B                NPARTSAV,AR   ,VR    ,DR,
     D                FSAVD ,IXRI   ,RIVET  ,IXP , 
     E                ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ ,
     F                DTHIS1(9), THIS1(9), IFIL, NTHGRP1(9),IXTG   ,
     G                IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                IPARTH(1,8*(NPART+NTHPART)+1),2,1,
     J                1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                IGAUP,NGAUP ,AFORM(9),WEIGHT_MD,SIZE_MES,
     M                RTHBUF ,THKE ,STACK ,ISPHIO  ,VSPHIO,
     N                ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%ITHBUFI)
        IF(SIZE_MES==1 .AND.
     *     AFORM(9)==3   .AND.
     *     ISPMD==0 ) CALL file_size(MULTITHFILESIZE(9))
            IF(ABFILE(10) /= 0) THEN
#ifdef DNC
            IFILABF = 20
            CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                  IXS  ,ELBUF,WA(N2),IPARG,
     4                  FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                  PARTSAV,ACCELM,LOUT ,
     6                  IPARI,WEIGHT,IPART,IGRSURF,
     7                  OUTPUT%TH%ITHGRPI,OUTPUT%TH%ITHBUFI,SUBSET,GEO  ,
     8                  KXX   ,IXR,
     9                  KXSP  ,NOD2SP ,SPBUF ,
     B                  NPARTSAV,AR   ,VR    ,DR,
     D                  FSAVD ,IXRI   ,RIVET  , IXP ,
     E                  ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                  DTABF(10),TABFIS(10),IFILABF,NTHGRP1(9),IXTG   ,
     G                  IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                  FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                  IPARTH(1,8*(NPART+NTHPART)+1),2,1,
     J                  1     ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                  FTHREAC,NODREAC,GRESAV  ,TABFWR(10),
     L                  10    ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF,THKE ,
     M                  STACK ,ISPHIO  ,VSPHIO,ITHFLAG,9,PINCH_DATA,MULTI_FVM,W)
              CPTFILE=CPTFILE+1

            IF(TT>=TABFWR0(10) .AND. ISPMD == 0) THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_i.tmp'
              LONG_TMP = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_i.abf'
              LONG = 11
              LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
              ENDDO
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
              ENDDO

               CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(10),IFILNAM,
     .                      LEN_TMP_NAME2,IFILNAM_TMP,LEN_TMP_NAME,
     .                      CPTFILE)
               NABFWR(10) = 0
             ENDIF
#endif
            ELSE
              TABFIS(10)= EP30
            ENDIF
          ELSE
            THIS1(9)= EP30
            TABFIS(10)= EP30
          ENDIF
C
        IUNIT = IUHIS
        IFIL = 1
        IF (TT>=THIS)THEN
          SIZE_MES=1
        ELSE
          SIZE_MES=0
        ENDIF
        ITHFLAG = 10
        CALL HIST2(PM   ,D    ,X     ,V   ,A    ,
     2                IXS  ,ELBUF,WA(N2),IPARG,SENSOR_TAB,
     4                FSAV ,FLSW ,SKEW,ELBUF_TAB,CLUSTER,
     5                PARTSAV,ACCELM,LOUT ,
     6                IPARI,WEIGHT,IPART,IGRSURF,
     7                OUTPUT%TH%ITHGRP,OUTPUT%TH%ITHBUF,SUBSET,GEO  ,
     8                KXX   ,IXR,
     9                KXSP  ,NOD2SP ,SPBUF ,
     B                NPARTSAV,AR   ,VR    ,DR,
     D                FSAVD ,IXRI   ,RIVET  , IXP , 
     E                ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                DTHIS ,   THIS,IFIL  , NTHGRP,IXTG   ,
     G                IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                IPART ,LIPART1    ,8      ,
     J                12    ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                FTHREAC,NODREAC,GRESAV  ,GAUGE  ,
     L                IGAUP,NGAUP,ITFORM,WEIGHT_MD,SIZE_MES,
     M                RTHBUF ,THKE ,STACK,ISPHIO  ,VSPHIO,
     N                ITHFLAG,PINCH_DATA,MULTI_FVM,W,OUTPUT%TH%SITHBUF)

        IF(SIZE_MES==1 .AND.
     *     ITFORM==3   .AND.
     *     ISPMD==0 )CALL file_size(THFILESIZE)

        IF(ABFILE(1) /= 0) THEN
#ifdef DNC
          IFILABF = 11
          CALL ABFHIST2(PM   ,D    ,X     ,V   ,A    ,
     2                IXS  ,ELBUF,WA(N2),IPARG,
     4                FSAV ,FLSW ,SKEW,ELBUF_TAB,SENSOR_TAB, 
     5                PARTSAV,ACCELM,LOUT ,
     6                IPARI,WEIGHT,IPART,IGRSURF,
     7                OUTPUT%TH%ITHGRP,OUTPUT%TH%ITHBUF,SUBSET,GEO  ,
     8                KXX   ,IXR,
     9                KXSP  ,NOD2SP ,SPBUF ,
     B                NPARTSAV,AR   ,VR    ,DR,
     D                FSAVD ,IXRI   ,RIVET  , IXP ,
     E                ISKWN ,IFRAME ,XFRAME,IXC    ,IXQ,
     F                DTABF(1), TABFIS(1),IFILABF,NTHGRP,IXTG   ,
     G                IGEO   ,IPM   ,FXBIPM, FXBDEP, FXBVIT,
     H                FXBACC ,IPARTL,NPARTL, IACCP , NACCP ,
     I                IPART ,LIPART1    ,8      ,
     J                12    ,MONVOL ,VOLMON,FR_MV  ,TEMP,INOD_PXFEM,
     K                FTHREAC,NODREAC,GRESAV  ,TABFWR(1),
     L                1     ,GAUGE  ,IGAUP,NGAUP   ,RTHBUF,THKE ,
     M                STACK ,ISPHIO  ,VSPHIO,ITHFLAG,10,PINCH_DATA,MULTI_FVM,W)

         IF((TT>=TABFWR0(1) .OR. TT+DT2>TSTOP).AND. ISPMD == 0)THEN
          FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.tmp'
          LONG_TMP = 9
          LEN_TMP_NAME = OUTFILE_NAME_LEN+LONG_TMP+ROOTLEN
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:LONG_TMP+ROOTLEN)

          FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.abf'
          LONG = 9
          LEN_TMP_NAME2 = OUTFILE_NAME_LEN+LONG+ROOTLEN
          TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:LONG+ROOTLEN)

          DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG
            IFILNAM(I)=ICHAR(TMP_NAME2(I:I))!FILNAMABF(I:I))
          ENDDO
          DO I=1,LEN_TMP_NAME!ROOTLEN+LONG_TMP
            IFILNAM_TMP(I)=ICHAR(TMP_NAME(I:I))!(FILNAMABF_TMP(I:I))
          ENDDO

         CALL ABFFILE_UPDATE(ABINP,ABOUT,NABFWR(1),IFILNAM,LEN_TMP_NAME2,
     .                      IFILNAM_TMP,LEN_TMP_NAME,0)

           NABFWR(1) = 0
         ENDIF
#endif
       ELSE
         TABFIS(1)= EP30
       ENDIF

C     to avoid double points (end of ilastanim/=2)
      END IF
      ENDIF 
C----------------------------------------------
C     Driver to QAPRINT 
C----------------------------------------------
      CALL ENG_QAPRINT_DRIVER(IRUNN_BIS)
C-------------------------------------------------------
C     TRAITEMENT SUR FSAV NON CUMULE
C-------------------------------------------------------
      IF(ISPMD==0.AND.THUPDT==1) THEN
C en SPMD il faut remettre FSAV a 0 sur P0, la ou FSAV n est pas cumule
        IF(NVOLU>0)THEN
         IAD = NINTER+NRWALL+NRBODY+NSECT+NJOINT
          DO I=1,NVOLU
            DO J=1,NTHVKI
              FSAV(J,IAD+I) = ZERO
            ENDDO
          ENDDO
        ENDIF
      ENDIF

C  FSAV(26) : contact elastic energy is not cumulated : only for int10 and int18 for the moment
      IF(NINTER>0)THEN
          DO I=1,NINTER
            ITY = IPARI(7,I)
            IF(ITY /= 10.AND.(ITY /= 7.OR.IPARI(22,I) /= 7)) THEN 
               FSAV(26,I) = ZERO
            ENDIF
          ENDDO
      ENDIF
      IF (ISPMD==0.AND.IFLAG==1) TFEXT  = TFEXT- TFEXTH
C-----------
      RETURN
      END
