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
Chd|  RESOL_HEAD                    source/engine/resol_head.F    
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        RESOL                         source/engine/resol.F         
Chd|        TRACE_IN                      source/system/trace_back.F    
Chd|        TRACE_OUT                     source/system/trace_back.F    
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        DIFFUSION_MOD                 share/modules/diffusion_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        EIG_MOD                       share/modules/eig_mod.F       
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FXB_MOD                       share/modules/fxb_mod.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        RBODY_MOD                     share/modules/rbody_mod.F     
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        SOLTOSPH_MOD                  share/modules/soltosph_mod.F  
Chd|        SPH_MOD                       share/modules/sph_mod.F       
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        TABLE_GLOB_MOD                share/modules/table_glob_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE RESOL_HEAD(
     .           ITASK       ,AF          ,IAF            ,IDATA      ,RDATA,XDP       ,
     .           GRAPHE      ,IFLOW       ,RFLOW          ,MCP        ,TEMP            ,
     .           ELBUF_TAB   ,XFEM_TAB    ,INTBUF_TAB     ,STACK      ,IRUNN_BIS       ,
     .           MULTI_FVM   ,H3D_DATA    ,INTBUF_FRIC_TAB,SUBSETS    ,IGRNOD          ,
     .           IGRBRIC     ,IGRQUAD     ,IGRSH4N        ,IGRSH3N    ,IGRTRUSS        ,
     .           IGRBEAM     ,IGRSPRING   ,IGRPART        ,IGRSURF    ,IGRSLIN         , 
     .           FORNEQS     ,MATPARAM_TAB,GROUP_PARAM_TAB,PINCH_DATA ,ALE_CONNECTIVITY,
     .           DRAPE_SH4N  ,DRAPE_SH3N  ,T_MONVOL       ,SENSORS    ,EBCS_TAB        ,
     .           DIFFUSION   ,SEGVAR      ,DYNAIN_DATA    ,DRAPEG     ,USER_WINDOWS    ,
     .           OUTPUT      ,INTERFACES  ,DT   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFMOD
      USE DSGRAPH_MOD
      USE TABLE_GLOB_MOD
      USE ELBUFDEF_MOD
      USE SPH_MOD
      USE SOLTOSPH_MOD
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD
      USE STACK_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE MATPARAM_DEF_MOD
      USE PINCHTYPE_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MONVOL_STRUCT_MOD
      USE DRAPE_MOD
      USE FXB_MOD
      USE EIG_MOD
      USE RBODY_MOD
      USE SENSOR_MOD
      USE EBCS_MOD
      USE DIFFUSION_MOD
      USE SEGVAR_MOD
      USE STATE_MOD
      USE USER_WINDOWS_MOD
      USE OUTPUT_MOD
      USE INTERFACES_MOD
      USE DT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C------------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr_fac_c.inc"
#include      "scr01_c.inc"
#include      "task_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK,IAF(*),IDATA(*),IFLOW(*),
     .   IRUNN_BIS
C     REAL
      my_real
     .   AF(*),RDATA(*),RFLOW(*),MCP(*),TEMP(*) ,
     .   FORNEQS(*)
      DOUBLE PRECISION XDP(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE(INTBUF_STRUCT_), DIMENSION(NINTER) :: INTBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(INTBUF_FRIC_STRUCT_), DIMENSION(NINTERFRIC) :: INTBUF_FRIC_TAB
!
      TYPE(SUBSET_) ,DIMENSION(NSUBS)   :: SUBSETS
      TYPE(GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(GROUP_)  ,DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE(GROUP_)  ,DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE(GROUP_)  ,DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE(GROUP_)  ,DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE(GROUP_)  ,DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE(GROUP_)  ,DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE(GROUP_)  ,DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE(GROUP_)  ,DIMENSION(NGRPART) :: IGRPART
      TYPE(SURF_)   ,DIMENSION(NSURF)   :: IGRSURF
      TYPE(SURF_)   ,DIMENSION(NSURF)   :: IGRSLIN
      TYPE(MATPARAM_STRUCT_),DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE(GRP_PARAM_STRUCT_) , DIMENSION(NGROUP) :: GROUP_PARAM_TAB
      TYPE(PINCH) :: PINCH_DATA
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      TYPE(DRAPE_)                            :: DRAPE_SH3N(NUMELTG_DRAPE),DRAPE_SH4N(NUMELC_DRAPE)
      TYPE(DRAPEG_)                           :: DRAPEG
      TYPE(SENSORS_)  ,INTENT(INOUT)          :: SENSORS
      TYPE(t_ebcs_tab), INTENT(INOUT)         :: EBCS_TAB
      TYPE(T_DIFFUSION), INTENT(INOUT)        :: DIFFUSION
      TYPE(t_segvar)                          :: SEGVAR
      TYPE(DYNAIN_DATABASE), INTENT(INOUT)    :: DYNAIN_DATA
      TYPE(USER_WINDOWS_),INTENT(INOUT)       :: USER_WINDOWS
      TYPE(OUTPUT_),INTENT(INOUT)             :: OUTPUT
      TYPE(INTERFACES_),INTENT(INOUT)         :: INTERFACES
      TYPE(DT_),INTENT(INOUT)         :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IUN,IFUN,IUNUSED,IPV,MFUN,I
      INTEGER ITRACE(10)
      DATA IUN/1/,IUNUSED/1/
      my_real
     .  FSAVD(1)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IOCLOSE = 0
C-----------------------------------------------
C     TRACE BACK  
C-----------------------------------------------
      ITASKP1 = 1
      ITASKP1_DEF = 1
      ITRACE(1)=0          
      ITRACE(2)=NTHREAD
      CALL TRACE_IN(2,ITRACE,ZERO)
C
      CALL RESOL(
     .  AF              ,IAF            ,
     1  ICODE           ,ISKEW          ,ISKWN          ,NETH           ,IPART          ,NOM_OPT     ,KXX              ,IXX       ,
     2  IXTG            ,IXS            ,IXQ            ,IXC            ,IXT            ,IXP         ,IXR       ,
     3                   ITAB           ,ITABM1         ,IFILL          ,
     4  IMS             ,NPC            ,IBCL           ,MATPARAM_TAB   ,GROUP_PARAM_TAB,
     5  IBFV            ,IECRAN         ,ILAS           ,LACCELM        ,NNLINK         ,LNLINK      ,
     6  IPARG           ,DD_IAD         ,IGRV           ,IEXLNK         ,KINET       ,
     7  IPARI           ,NPRW           ,ICONX          ,NPBY           ,
     8  LPBY            ,LRIVET         ,NSTRF          ,LJOINT         ,NODPOR         ,MONVOL      ,IAD_ELEM  ,
     9  FR_ELEM         ,WEIGHT         ,MAIN_PROC      ,ICODT          ,ICODR          ,ADSKY       ,IADS             ,ILINK     ,
     A  LLINK           ,LINALE         ,NEFLSW         ,NNFLSW         ,ICUT           ,CLUSTER     ,
     B  ITASK           ,IAF(IF01)      ,
     C  X               ,D              ,V              ,VR             ,DR             ,THKE        ,DAMP             ,MS        ,
     D  IN              ,PM             ,SKEW           ,GEO            ,EANI           ,BUFMAT      ,BUFGEO           ,BUFSF     ,
     E  W               ,VEUL           ,FILL           ,DFILL          ,ALPH           ,WB          ,DSAVE            ,ASAVE     ,
     .  MSNF            , 
     F  TF              ,FORC           ,VEL            ,FSAV           ,FZERO          ,XLAS        ,ACCELM           ,
     G  GRAV            ,FR_WAVE        ,FAILWAVE       ,PARTS0         ,ELBUF          ,RWBUF       ,SENSORS,
     H  RWSAV           ,RBY            ,RIVET          ,SECBUF         ,VOLMON         ,LAMBDA      ,
     I  WA              ,FV             ,A              ,AR             ,STIFN          ,STIFR       ,PARTSAV          ,FSKY      ,
     J  UWA             ,VAL2           ,PHI            ,SEGVAR         ,R              ,CRFLSW      ,
     K  FLSW            ,FANI           ,XCUT           ,ANIN           ,TANI           ,SECFCUM     ,AF(MF01),
     L  IDATA           ,RDATA          ,
     M  IFRAME          ,KXSP           ,IXSP           ,NOD2SP         ,ISPSYM         ,ISPCOND     ,
     N  XFRAME          ,SPBUF          ,XSPSYM         ,VSPSYM         ,PV             ,
     O  FSAVD           ,IBVEL          ,LBVEL          ,WASPH          ,XDP            ,W16         ,
     P  ISPHIO          ,LPRTSPH        ,LONFSPH        ,VSPHIO         ,FBVEL          ,LAGBUF      ,IBCSLAG,
     Q  IACTIV          ,DAMPR          ,GJBUFI         ,GJBUFR         ,RBMPC          ,IBMPC       ,SPHVELN,
     T  NBRCVOIS        ,NBSDVOIS       ,LNRCVOIS       ,LNSDVOIS       ,NERCVOIS       ,NESDVOIS    ,LERCVOIS         ,
     U  LESDVOIS        ,NPSEGCOM       ,LSEGCOM        ,NPORGEO        ,FSKYM          ,
     V  IXTG1           ,NPBYL          ,LPBYL          ,RBYL           ,IGEO           ,IPM         ,
     W  VISCN           ,MADPRT         ,MADSH4         ,MADSH3         ,MADSOL         ,MADNOD      ,MADFAIL          ,
     X  IAD_RBY         ,FR_RBY         ,FR_WALL        ,PROCNE         ,IAD_RBY2       ,
     Y  FR_RBY2         ,IAD_I2M        ,FR_I2M         ,ADDCNI2        ,PROCNI2        ,IADI2       ,FR_MV            ,
     Z  IADMV2          ,FR_LL          ,FR_RL          ,IADCJ          ,
     a  FR_CJ           ,FR_SEC         ,
     b  IAD_SEC         ,IAD_CUT        ,FR_CUT         ,RG_CUT         ,NEWFRONT       ,FR_MAD      ,NODGLOB          ,
     c  FXBIPM          ,FXBRPM         ,FXBNOD         ,FXBMOD         ,FXBGLM         ,FXBCPM      ,FXBCPS           ,
     d  FXBLM           ,FXBFLS         ,FXBDLS         ,FXBDEP         ,FXBVIT         ,FXBACC      ,FXBELM           ,
     e  FXBSIG          ,FXBGRVI        ,FXBGRVR        ,EIGIPM         ,EIGIBUF        ,EIGRPM      ,LSHSUB           ,
     f  DRSAVE          ,ARSAVE         ,STSAVE         ,STRSAVE        ,FSAVSAV        ,LNODPOR     ,
     g  FR_I18          ,IADSDP         ,IADRCP         ,GRAPHE         ,IFLOW          ,RFLOW,
     h  LGRAV           ,DD_R2R         ,FASOLFR        ,FR_LAGF        ,
     j  LLAGF           ,LPRW           ,ICONTACT       ,RCONTACT       ,SH4TREE        ,SH3TREE     ,
     k  IPADMESH        ,PADMESH        ,MSC            ,MSTG           ,INC            ,INTG        ,PTG              ,
     l  ISKWP           ,NSKWP          ,ISENSP         ,NSENSP         ,IACCP          ,NACCP       ,IPART_STATE      ,
     m  ACONTACT        ,PCONTACT       ,FACTIV         ,MCP            ,TEMP,
     p  SH4TRIM         ,SH3TRIM        ,MSCND          ,INCND          ,
     q  IBFFLUX         ,FBFFLUX        ,RBYM           ,IRBYM          ,LNRBYM         ,ICODRBYM,
     r  IBCV            ,FCONV          ,IBFTEMP        ,FBFTEMP        ,IRBE3          ,LRBE3       ,FRBE3            ,
     s  IAD_RBE3M       ,FR_RBE3M       ,FR_RBE3MP      ,IAD_RBYM       ,FR_RBYM,
     t  WEIGHT_RM       ,MS_PLY         ,ZI_PLY         ,INOD_PXFEM     ,IEL_PXFEM      ,IADC_PXFEM  ,
     u  ADSKY_PXFEM     ,ICODE_PLY      ,ICODT_PLY      ,ISKEW_PLY      ,MS0            ,ADMSMS      ,
     v  MADCLNOD        ,NOM_SECT       ,MCPC           ,MCPTG          ,DMELC          ,DMELTG      ,MSSA             ,
     w  DMELS           ,MSTR           ,DMELTR         ,MSP            ,DMELP          ,MSRT        ,DMELRT           ,
     x  IBCR            ,FRADIA         ,RES_SMS        ,TABLE          ,IRBE2          ,LRBE2       ,IAD_RBE2         ,
     y  FR_RBE2         ,PHIE           ,MSF            ,
     z  PROCNE_PXFEM    ,IADSDP_PXFEM   ,IADRCP_PXFEM   ,ICFIELD        ,LCFIELD        ,CFIELD      ,
     1  MSZ2            ,DIAG_SMS       ,ILOADP         ,LLOADP      ,LOADP            ,
     2  INOD_CRKXFEM    ,IEL_CRKXFEM    ,IADC_CRKXFEM   ,ADSKY_CRKXFEM  ,
     3  CNE_CRKXFEM     ,PROCNE_CRKXFEM ,IADSDP_CRKXFEM ,IADRCP_CRKXFEM ,IBUFSSG_IO     ,
     4  IBC_PLY         ,DMINT2         ,IBORDNODE      ,
     5  ELBUF_TAB       ,POR ,NODEDGE   ,IAD_EDGE       ,
     6  FR_EDGE         ,FR_NBEDGE      ,CRKNODIAD      ,LGAUGE         ,GAUGE          ,
     7  IGAUP           ,NGAUP          ,NODLEVXF       ,DD_R2R_ELEM    ,WEIGHT_MD      ,
     8  NODGLOBXFE      ,SPH2SOL        ,SOL2SPH        ,IRST           ,FSKYD          ,
     9  DMSPH           ,WAGAP          ,XFEM_TAB       ,ELCUTC         ,NODENR         ,
     A  KXFENOD2ELC     ,ENRTAG         ,INTBUF_TAB     ,
     B  RTHBUF          ,KXIG3D         ,IXIG3D         ,KNOT           ,WIGE           ,
     C  TABSENSOR       ,FBSAV6         ,WSMCOMP        ,STACK          ,
     D  CPUTIME_MP_GLOB ,CPUTIME_MP     ,TAB_UMP        ,POIN_UMP       ,SOL2SPH_TYP    ,
     E  IRUNN_BIS       ,ADDCSRECT      ,IAD_FRNOR      ,FR_NOR         ,PROCNOR        ,
     F  IAD_FREDG       ,FR_EDG         ,DRAPE_SH4N     ,DRAPE_SH3N       ,TAB_MAT        , 
     G  NATIV0_SMS      ,MULTI_FVM      ,DDP            ,SEGQUADFR      ,MS_2D          ,
     H  H3D_DATA        ,INTBUF_FRIC_TAB,SUBSETS        ,IGRNOD         ,IGRBRIC        ,
     I  IGRQUAD         ,IGRSH4N        ,IGRSH3N        ,IGRTRUSS       ,IGRBEAM        ,
     J  IGRSPRING       ,IGRPART        ,IGRSURF        ,FORNEQS        ,
     K  NLOC_DMG        ,ISKWP_L        ,KNOTLOCPC      ,KNOTLOCEL      ,PINCH_DATA     ,TAG_SKINS6  ,ALE_CONNECTIVITY ,
     L  XCELL           , XFACE         ,NE_NERCVOIS    ,NE_NESDVOIS    ,NE_LERCVOIS    ,NE_LESDVOIS ,IBCSCYC          ,LBCSCYC   ,
     M  T_MONVOL        ,ID_GLOBAL_VOIS ,FACE_VOIS      ,TAGSLV_RBY     ,DYNAIN_DATA    ,FCONT_MAX   ,EBCS_TAB         ,DIFFUSION ,
     N  KLOADPINTER     ,LOADPINTER     ,IN0            ,DGAPLOADINT    ,DRAPEG         ,USER_WINDOWS ,OUTPUT          ,INTERFACES,
     O  DT              )
C
      CALL TRACE_OUT(2)
C
      RETURN
      END
