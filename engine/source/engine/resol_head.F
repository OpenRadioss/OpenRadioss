Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    resol_head             ../engine/source/engine/resol_head.F
      !||--- called by ------------------------------------------------------
      !||    radioss2               ../engine/source/engine/radioss2.F
      !||--- calls      -----------------------------------------------------
      !||    resol                  ../engine/source/engine/resol.F
      !||    trace_in               ../engine/source/system/trace_back.F
      !||    trace_out              ../engine/source/system/trace_back.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
      !||    cluster_mod            ../engine/share/modules/cluster_mod.F
      !||    connectivity_mod       ../common_source/modules/connectivity.F90
      !||    damp_mod               ../engine/share/modules/damp_mod.F
      !||    diffusion_mod          ../engine/share/modules/diffusion_mod.F
      !||    drape_mod              ../engine/share/modules/drape_mod.F
      !||    dsgraph_mod            ../engine/share/modules/dsgraph_mod.F
      !||    dt_mod                 ../engine/source/modules/dt_mod.F
      !||    ebcs_mod               ../common_source/modules/boundary_conditions/ebcs_mod.F90
      !||    eig_mod                ../engine/share/modules/eig_mod.F
      !||    fxb_mod                ../engine/share/modules/fxb_mod.F
      !||    glob_therm_mod         ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||    groupdef_mod           ../common_source/modules/groupdef_mod.F
      !||    h3d_mod                ../engine/share/modules/h3d_mod.F
      !||    intbuf_fric_mod        ../common_source/modules/interfaces/intbuf_fric_mod.F90
      !||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||    monvol_struct_mod      ../engine/share/modules/monvol_struct_mod.F
      !||    multi_fvm_mod          ../common_source/modules/ale/multi_fvm_mod.F90
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||    nodal_arrays_mod       ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    output_mod             ../common_source/modules/output/output_mod.F90
      !||    pinchtype_mod          ../common_source/modules/pinchtype_mod.F
      !||    python_funct_mod       ../common_source/modules/python_mod.F90
      !||    rbe3_mod               ../common_source/modules/constraints/rbe3_mod.F90
      !||    restmod                ../engine/share/modules/restart_mod.F
      !||    segvar_mod             ../engine/share/modules/segvar_mod.F
      !||    sensor_mod             ../common_source/modules/sensor_mod.F90
      !||    skew_mod               ../common_source/modules/skew_mod.F90
      !||    soltosph_mod           ../engine/share/modules/soltosph_mod.F
      !||    sph_mod                ../engine/share/modules/sph_mod.F
      !||    stack_mod              ../engine/share/modules/stack_mod.F
      !||    state_mod              ../common_source/modules/state_mod.F
      !||    table_glob_mod         ../engine/share/modules/table_glob_mod.F
      !||    timer_mod              ../engine/source/system/timer_mod.F90
      !||    unitab_mod             ../common_source/modules/unitab_mod.F
      !||    user_windows_mod       ../common_source/modules/user_windows_mod.F
      !||====================================================================
      SUBROUTINE RESOL_HEAD( ELEMENT, NODES,
     .           ITASK       ,AF          ,IAF            ,IDATA      ,RDATA,
     .           GRAPHE      ,IFLOW       ,RFLOW          ,
     .           STACK       ,IRUNN_BIS   , TIMERS,
     .           MULTI_FVM   ,H3D_DATA    ,SUBSETS    ,IGRNOD          ,
     .           IGRBRIC     ,IGRQUAD     ,IGRSH4N        ,IGRSH3N    ,IGRTRUSS        ,
     .           IGRBEAM     ,IGRSPRING   ,IGRPART        ,IGRSURF    ,IGRSLIN         , 
     .           FORNEQS     ,PINCH_DATA  ,ALE_CONNECTIVITY,
     .           DRAPE_SH4N  ,DRAPE_SH3N  ,T_MONVOL       ,SENSORS    ,EBCS_TAB        ,
     .           DIFFUSION   ,SEGVAR      ,DYNAIN_DATA    ,DRAPEG     ,USER_WINDOWS    ,
     .           OUTPUT      ,INTERFACES  ,DT             ,LOADS      ,MAT_ELEM        ,
     .           PYTHON      ,NAMES_AND_TITLES, UNITAB    ,SKEWS      ,LIFLOW          ,
     .           LRFLOW      ,glob_therm ,PBLAST          ,RBE3 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE connectivity_mod
      use nodal_arrays_mod
      USE RESTMOD
      USE DSGRAPH_MOD
      USE TABLE_GLOB_MOD
      USE SPH_MOD
      USE SOLTOSPH_MOD
      USE CLUSTER_MOD
      USE STACK_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE MAT_ELEM_MOD
      USE PINCHTYPE_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MONVOL_STRUCT_MOD
      USE DRAPE_MOD
      USE FXB_MOD
      USE EIG_MOD
      USE SENSOR_MOD
      USE EBCS_MOD
      USE DIFFUSION_MOD
      USE SEGVAR_MOD
      USE LOADS_MOD
      USE STATE_MOD
      USE USER_WINDOWS_MOD
      USE OUTPUT_MOD
      USE INTERFACES_MOD
      USE DT_MOD
      USE PYTHON_FUNCT_MOD
      USE DAMP_MOD
      USE NAMES_AND_TITLES_MOD
      USE UNITAB_MOD
      USE SKEW_MOD
      use glob_therm_mod
      use PBLAST_MOD
      use TIMER_MOD
      use rbe3_mod
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
      type(connectivity_), intent(inout) :: ELEMENT 
      type(nodal_arrays_), intent(inout) :: NODES
      type(timer_), intent(inout) :: TIMERS
      INTEGER, INTENT(IN) :: LIFLOW  !< Size of IFLOW
      INTEGER, INTENT(IN) :: LRFLOW  !< Size of RFLOW

      INTEGER,INTENT(INOUT) :: IFLOW(LIFLOW)
      INTEGER,INTENT(INOUT) :: RFLOW(LRFLOW)
C
      INTEGER ITASK,IAF(*),IDATA(*),IRUNN_BIS
C     REAL
      my_real
     .   AF(*),RDATA(*),                          
     .   FORNEQS(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE (STACK_PLY) :: STACK
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(UNIT_TYPE_) :: UNITAB
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
      TYPE(PINCH) :: PINCH_DATA
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
      TYPE(DRAPE_)                            :: DRAPE_SH3N(NUMELTG_DRAPE),DRAPE_SH4N(NUMELC_DRAPE)
      TYPE(DRAPEG_)                           :: DRAPEG
      TYPE(SENSORS_)  ,INTENT(INOUT)          :: SENSORS
      TYPE(t_ebcs_tab), INTENT(INOUT)         :: EBCS_TAB
      TYPE(T_DIFFUSION)    ,INTENT(INOUT)     :: DIFFUSION
      TYPE(t_segvar)                          :: SEGVAR
      TYPE(DYNAIN_DATABASE), INTENT(INOUT)    :: DYNAIN_DATA
      TYPE(USER_WINDOWS_),INTENT(INOUT)       :: USER_WINDOWS
      TYPE(OUTPUT_),INTENT(INOUT)             :: OUTPUT
      TYPE(INTERFACES_),INTENT(INOUT)         :: INTERFACES
      TYPE(DT_),INTENT(INOUT)                 :: DT
      TYPE(LOADS_)    ,INTENT(INOUT)          :: LOADS
      TYPE(MAT_ELEM_) ,INTENT(INOUT)          :: MAT_ELEM
      TYPE(PYTHON_)   , INTENT(INOUT)         :: PYTHON
      TYPE(NAMES_AND_TITLES_),INTENT(INOUT)   :: NAMES_AND_TITLES !< NAMES_AND_TITLES host the input deck names and titles for outputs
      TYPE(SKEW_), INTENT(INOUT)              :: SKEWS
      type (glob_therm_) ,intent(inout)       :: glob_therm
      type (pblast_) ,intent(inout)           :: PBLAST
      type (rbe3_) ,intent(inout)             :: RBE3
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
C-----------------------------------------------
C     TRACE BACK  
C-----------------------------------------------
      ITASKP1 = 1
      ITASKP1_DEF = 1
      ITRACE(1)=0          
      ITRACE(2)=NTHREAD
      CALL TRACE_IN(2,ITRACE,ZERO)
C
      CALL RESOL(TIMERS, ELEMENT,NODES,
     .  AF              ,IAF            ,
     1  ISKWN          ,NETH           ,IPART          ,NOM_OPT     ,KXX              ,IXX       ,
     2  IXTG            ,IXS            ,IXQ            ,IXT            ,IXP         ,IXR       ,
     3  IFILL          ,MAT_ELEM       ,
     4  IMS             ,NPC            ,IBCL           ,
     5  IBFV            ,IECRAN         ,ILAS           ,LACCELM        ,NNLINK         ,LNLINK      ,
     6  IPARG           ,DD_IAD         ,IGRV           ,IEXLNK         ,KINET       ,
     7  IPARI           ,NPRW           ,ICONX          ,NPBY           ,
     8  LPBY            ,LRIVET         ,NSTRF          ,LJOINT         ,NODPOR         ,MONVOL      ,IAD_ELEM  ,
     9  FR_ELEM         ,ILINK     ,
     A  LLINK           ,LINALE         ,NEFLSW         ,NNFLSW         ,ICUT           ,CLUSTER     ,
     B  ITASK           ,IAF(IF01)      ,
     C  THKE        ,DAMP               ,
     D  PM             ,SKEWS          ,GEO            ,EANI           ,BUFMAT      ,BUFGEO           ,BUFSF     ,
     E  W               ,VEUL           ,FILL           ,DFILL          ,ALPH           ,WB          ,DSAVE            ,ASAVE     ,
     .  MSNF            , 
     F  TF              ,FORC           ,VEL            ,FSAV           ,FZERO          ,XLAS        ,ACCELM           ,
     G  GRAV            ,FR_WAVE        ,FAILWAVE       ,PARTS0         ,ELBUF          ,RWBUF       ,SENSORS,
     H  RWSAV           ,RBY            ,RIVET          ,SECBUF         ,VOLMON         ,LAMBDA      ,
     I  WA              ,FV             ,PARTSAV          ,
     J  UWA             ,VAL2           ,PHI            ,SEGVAR         ,R              ,CRFLSW      ,
     K  FLSW            ,FANI           ,XCUT           ,ANIN           ,TANI           ,SECFCUM     ,AF(MF01),
     L  IDATA           ,RDATA          ,
     M  IFRAME          ,KXSP           ,IXSP           ,NOD2SP         ,ISPSYM         ,ISPCOND     ,
     N  XFRAME          ,SPBUF          ,XSPSYM         ,VSPSYM         ,PV             ,
     O  FSAVD           ,IBVEL          ,LBVEL          ,WASPH          ,W16         ,
     P  ISPHIO          ,LPRTSPH        ,LONFSPH        ,VSPHIO         ,FBVEL          ,LAGBUF      ,IBCSLAG,
     Q  IACTIV          ,DAMPR          ,GJBUFI         ,GJBUFR         ,RBMPC          ,IBMPC       ,SPHVELN,
     T  NBRCVOIS        ,NBSDVOIS       ,LNRCVOIS       ,LNSDVOIS       ,NERCVOIS       ,NESDVOIS    ,LERCVOIS         ,
     U  LESDVOIS        ,NPSEGCOM       ,LSEGCOM        ,NPORGEO        ,
     V  IXTG1           ,NPBYL          ,LPBYL          ,RBYL           ,IGEO           ,IPM         ,
     W  MADPRT         ,MADSH4         ,MADSH3         ,MADSOL         ,MADNOD      ,MADFAIL          ,
     X  IAD_RBY         ,FR_RBY         ,FR_WALL        ,PROCNE         ,IAD_RBY2       ,
     Y  FR_RBY2         ,IAD_I2M        ,FR_I2M         ,ADDCNI2        ,PROCNI2        ,IADI2       ,FR_MV            ,
     Z  IADMV2          ,FR_LL          ,FR_RL          ,IADCJ          ,
     a  FR_CJ           ,FR_SEC         ,
     b  IAD_SEC         ,IAD_CUT        ,FR_CUT         ,RG_CUT         ,NEWFRONT       ,FR_MAD      ,NODGLOB          ,
     c  FXBIPM          ,FXBRPM         ,FXBNOD         ,FXBMOD         ,FXBGLM         ,FXBCPM      ,FXBCPS           ,
     d  FXBLM           ,FXBFLS         ,FXBDLS         ,FXBDEP         ,FXBVIT         ,FXBACC      ,FXBELM           ,
     e  FXBSIG          ,FXBGRVI        ,FXBGRVR        ,EIGIPM         ,EIGIBUF        ,EIGRPM      ,
     f  LNODPOR         ,
     g  FR_I18          ,GRAPHE         ,IFLOW          ,RFLOW,
     h  LGRAV           ,DD_R2R         ,FASOLFR        ,FR_LAGF        ,
     j  LLAGF           ,LPRW           ,ICONTACT       ,RCONTACT       ,SH4TREE        ,SH3TREE     ,
     k  IPADMESH        ,PADMESH        ,MSC            ,MSTG           ,INC            ,INTG        ,PTG              ,
     l  ISKWP           ,NSKWP          ,ISENSP         ,NSENSP         ,IACCP          ,NACCP       ,IPART_STATE      ,
     m  ACONTACT        ,PCONTACT       ,FACTIV         ,
     p  SH4TRIM         ,SH3TRIM        ,MSCND          ,INCND          ,
     q  IBFFLUX         ,FBFFLUX        ,RBYM           ,IRBYM          ,LNRBYM         ,ICODRBYM,
     r  IBCV            ,FCONV          ,IBFTEMP        ,FBFTEMP        ,IAD_RBYM       ,FR_RBYM,
     t  WEIGHT_RM       ,MS_PLY         ,ZI_PLY         ,INOD_PXFEM     ,IEL_PXFEM      ,IADC_PXFEM  ,
     u  ADSKY_PXFEM     ,ICODE_PLY      ,ICODT_PLY      ,ISKEW_PLY      ,ADMSMS      ,
     v  MADCLNOD        ,NOM_SECT       ,MCPC           ,MCPTG          ,DMELC          ,DMELTG      ,MSSA             ,
     w  DMELS           ,MSTR           ,DMELTR         ,MSP            ,DMELP          ,MSRT        ,DMELRT           ,
     x  IBCR            ,FRADIA         ,RES_SMS        ,TABLE          ,IRBE2          ,LRBE2       ,IAD_RBE2         ,
     y  FR_RBE2         ,PHIE           ,MSF            ,
     z  PROCNE_PXFEM    ,IADSDP_PXFEM   ,IADRCP_PXFEM   ,ICFIELD        ,LCFIELD        ,CFIELD      ,
     1  MSZ2            ,DIAG_SMS       ,ILOADP         ,LLOADP      ,LOADP            ,
     2  INOD_CRKXFEM    ,IEL_CRKXFEM    ,IADC_CRKXFEM   ,ADSKY_CRKXFEM  ,
     3  CNE_CRKXFEM     ,PROCNE_CRKXFEM ,IADSDP_CRKXFEM ,IADRCP_CRKXFEM ,IBUFSSG_IO     ,
     4  IBC_PLY         ,DMINT2         ,IBORDNODE      ,
     5  MAT_ELEM%ELBUF  ,POR            ,NODEDGE        ,IAD_EDGE       ,
     6  FR_EDGE         ,FR_NBEDGE      ,CRKNODIAD      ,LGAUGE         ,GAUGE          ,
     7  IGAUP           ,NGAUP          ,NODLEVXF       ,DD_R2R_ELEM    ,
     8  NODGLOBXFE      ,SPH2SOL        ,SOL2SPH        ,IRST           ,
     9  DMSPH           ,WAGAP          ,MAT_ELEM%XFEM_TAB,ELCUTC       ,NODENR         ,
     A  KXFENOD2ELC     ,ENRTAG         ,
     B  RTHBUF          ,KXIG3D         ,IXIG3D         ,KNOT           ,WIGE           ,
     C  WSMCOMP        ,STACK          ,
     D  CPUTIME_MP_GLOB ,CPUTIME_MP     ,TAB_UMP        ,POIN_UMP       ,SOL2SPH_TYP    ,
     E  IRUNN_BIS       ,ADDCSRECT      ,IAD_FRNOR      ,FR_NOR         ,PROCNOR        ,
     F  INTERFACES%SPMD_ARRAYS%IAD_FREDG,INTERFACES%SPMD_ARRAYS%FR_EDG  ,DRAPE_SH4N     ,DRAPE_SH3N     ,TAB_MAT        , 
     G  NATIV0_SMS      ,MULTI_FVM      ,SEGQUADFR      ,MS_2D          ,
     H  H3D_DATA        ,SUBSETS        ,IGRNOD         ,IGRBRIC        ,
     I  IGRQUAD         ,IGRSH4N        ,IGRSH3N        ,IGRTRUSS       ,IGRBEAM        ,
     J  IGRSPRING       ,IGRPART        ,IGRSURF        ,FORNEQS        ,
     K  NLOC_DMG        ,ISKWP_L        ,KNOTLOCPC      ,KNOTLOCEL      ,PINCH_DATA     ,TAG_SKINS6  ,ALE_CONNECTIVITY ,
     L  XCELL           , XFACE         ,NE_NERCVOIS    ,NE_NESDVOIS    ,NE_LERCVOIS    ,NE_LESDVOIS ,IBCSCYC          ,LBCSCYC   ,
     M  T_MONVOL        ,ID_GLOBAL_VOIS ,FACE_VOIS      ,DYNAIN_DATA    ,FCONT_MAX   ,EBCS_TAB         ,DIFFUSION ,
     N  KLOADPINTER     ,LOADPINTER     ,DGAPLOADINT    ,DRAPEG         ,USER_WINDOWS ,OUTPUT          ,INTERFACES,
     O  DT              ,LOADS          ,PYTHON         ,DPL0CLD        ,VEL0CLD        ,
     P  NDAMP_VREL      ,ID_DAMP_VREL   ,FR_DAMP_VREL   ,NDAMP_VREL_RBYG,NAMES_AND_TITLES,UNITAB,LIFLOW,LRFLOW,
     R  GLOB_THERM      ,PBLAST         ,RBE3)
C
      CALL TRACE_OUT(2)
C
      RETURN
      END
