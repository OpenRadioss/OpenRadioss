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
Chd|  INITIA                        source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ADDMASPART                    source/tools/admas/addmaspart.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ASSTIFI                       source/interfaces/inter3d1/asstifi.F
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CHECKMP                       source/elements/initia/initia.F
Chd|        CHEKMP2                       source/elements/initia/initia.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CONNESURF                     source/initial_conditions/inivol/connesurf.F
Chd|        EPORIN3                       source/ale/ale3d/eporin3.F    
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        FXBSINI                       source/constraints/fxbody/fxbsini.F
Chd|        FXBVINI                       source/constraints/fxbody/fxbvini.F
Chd|        GETPHASE                      source/initial_conditions/inivol/getphase.F
Chd|        IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|        INIBOLTPREL                   source/loads/bolt/iniboltprel.F
Chd|        INIFILL                       source/initial_conditions/inivol/inifill.F
Chd|        INIGRAV_LOAD                  source/initial_conditions/inigrav/inigrav_load.F
Chd|        ININODE_RM                    source/materials/mat/mat019/ininode_rm.F
Chd|        INIRBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        INIRBY                        source/constraints/general/rbody/inirby.F
Chd|        INIRBYS                       source/constraints/general/rbody/inirby.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INISOLDIST                    source/initial_conditions/inivol/inisoldist.F
Chd|        INISRF                        source/constraints/general/rbody/inisrf.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        INIVOL_SET                    source/initial_conditions/inivol/inivol_set.F
Chd|        INI_FVMINIVEL                 source/elements/initia/ini_fvminivel.F
Chd|        INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|        INI_INIMAP1D                  source/initial_conditions/inimap/ini_inimap1d.F
Chd|        INI_INIMAP2D                  stub/ini_inimap2d.F           
Chd|        INI_SEATBELT                  source/tools/seatbelts/ini_seatbelt.F
Chd|        INSPCND                       source/elements/sph/inspcnd.F 
Chd|        LASER10                       source/loads/laser/laser10.F  
Chd|        LGMINI_RBY                    source/tools/lagmul/lgmini_rby.F
Chd|        MODBUFEL                      source/constraints/fxbody/modbufel.F
Chd|        MODDEPL                       source/constraints/fxbody/moddepl.F
Chd|        MULTIFLUID_GLOBAL_TDET        source/multifluid/multifluid_global_tdet.F
Chd|        MULTIFLUID_INIT2              source/multifluid/multifluid_init2.F
Chd|        MULTIFLUID_INIT2T             source/multifluid/multifluid_init2t.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|        NLOC_DMG_INIT                 source/materials/fail/nloc_dmg_init.F
Chd|        OUTPART                       source/elements/initia/initia.F
Chd|        OUTPART5                      source/elements/initia/initia.F
Chd|        PINIT3                        source/elements/beam/pinit3.F 
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|        RCHECKMASS                    source/elements/spring/rcheckmass.F
Chd|        RETRIRBY                      source/constraints/general/merge/hm_read_merge.F
Chd|        RINI33_RB                     source/elements/joint/rjoint/rini33_rb.F
Chd|        RINI45_RB                     source/elements/joint/rjoint/rini45_rb.F
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCALEINI                      source/elements/initia/scaleini.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SGSAVINI                      source/elements/solid/solide/scoor3.F
Chd|        SGSAVINIEREF                  source/elements/initia/initia.F
Chd|        SGSAVINIEREFQ                 source/elements/initia/initia.F
Chd|        SGSAVREF                      source/elements/initia/initia.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SMS_AUTO_DT                   source/ams/sms_auto_dt.F      
Chd|        SPINIT3                       source/elements/sph/spinit3.F 
Chd|        SPMD_MSIN                     source/elements/initia/spmd_msin.F
Chd|        SPMD_MSIN_ADDMASS             source/elements/initia/spmd_msin_addmass.F
Chd|        SPMD_PARTSAV_PON              source/elements/initia/spmd_msin_addmass.F
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|        TINIT3                        source/elements/truss/tinit3.F
Chd|        XINIT3                        source/elements/xelem/xinit3.F
Chd|        STRR                          source/tools/univ/strr.F      
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        BPRELOAD_MOD                  share/modules1/bpreload_mod.F 
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FUNC2D_MOD                    share/modules1/func2d_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIMAP1D_MOD                  share/modules1/inimap1d_mod.F 
Chd|        INIMAP2D_MOD                  share/modules1/inimap2d_mod.F 
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE INITIA(IPARG      ,ELBUF        ,MS           ,IN           ,V       , 
     1                  X          ,IXS          ,IXQ          ,IXC          ,IXT     , 
     2                  IXP        ,IXR          ,DETONATORS   ,GEO          ,PM      , 
     3                  RBY        ,NPBY         ,LPBY         ,NPC          ,NPTS    ,	        
     4                  PLD        ,VEUL         ,ALE_CONNECTIVITY ,SKEW     ,FILL    , 
     5                  IPART      ,ITAB         ,IXUR         , 
     6                  IXTG       ,THK          ,NLOC_DMG     ,GROUP_PARAM_TAB,
     7                  IGRNOD     ,IGRSURF      ,BUFSF        ,VR           , 
     8                  BUFMAT     ,XLAS         ,LAS          ,DTELEM       ,MSS     ,
     9                  MSQ        ,MSC          ,MST          ,MSP          ,MSR     , 
     A                  MSTG       ,PTG          ,MSUR         ,EVUR         ,INC     , 
     B                  INP        ,INR          ,INTG         ,INUR         ,INDEX   , 
     C                  ITRI       ,KXX          ,IXX          ,XELEMWA      ,	        
     E                  IWA        ,
     F                  KXSP       ,IXSP         ,NOD2SP       ,ISPCOND      ,ICODE   , 
     G                  ISKEW      ,ISKN         ,ISPSYM       ,XFRAME       ,ISPTAG  , 
     H                  SPBUF      ,MSSX         ,NSIGI        ,
     I                  NPBYL      ,LPBYL        ,RBYL         ,MSNF         ,MSSF    , 
     J                  NSIGSH     ,IGEO         ,IPM          ,NSIGS        ,	        
     K                  NSIGSPH    ,VNS          ,VNSX         ,STC          ,STT     , 
     L                  STP        ,STR          ,STTG         ,STUR         ,BNS     , 
     M                  BNSX       ,VOLNOD       ,BVOLNOD      ,ETNOD        ,NSHNOD  , 
     N                  STIFINT    ,FXBDEP       ,FXBVIT       ,FXBACC       ,FXBIPM  , 
     O                  FXBRPM     ,FXBELM       ,FXBSIG       ,FXBMOD       ,INS     , 
     P                  PTSHEL     ,PTSH3N       ,PTSOL        ,PTQUAD  ,
     Q                  WMA        ,PTSPH        ,FXBNOD       ,MBUFEL       ,MDEPL   , 
     R                  FXANI      ,NUMEL        ,NSIGRS       ,
     T                  SH4TREE    ,SH3TREE      ,MCP          ,TEMP         ,	        
     U                  IMERGE2    ,IADMERGE2    ,
     V                  SLNRBM     ,NSLNRBM      ,RMSTIFN      ,RMSTIFR,
     U                  MS_LAYER   ,ZI_LAYER     ,ITAG         ,ITAGEL       ,MCPC    ,
     W                  MCPTG      ,XREFC        ,XREFTG       ,XREFS        ,MSSA    ,
     X                  MSRT       ,IRBE2        ,LRBE2        ,INIVOL       ,KVOL    , NBSUBMAT,
     Y                  IXS10      ,IXS16        ,IXS20        ,TOTADDMAS    ,
     Z                  IPMAS      ,STIFN        ,MSZ2         ,ITAGN        ,SITAGE,
     1                  ITAGE      ,IXR_KJ       ,ELBUF_TAB,
     2                  NOM_OPT    ,PTR_NOPT_RBE2,PTR_NOPT_ADM ,PTR_NOPT_FUN ,
     3                  SOL2SPH    ,IRST         ,SH3TRIM      ,XFEM_TAB     ,
     4                  KXIG3D     ,IXIG3D       ,MSIG3D       ,KNOT         ,NCTRLMAX,
     5                  WIGE       ,STACK        ,
     7                  RNOISE     ,DRAPE       ,SH4ANG       ,SH3ANG ,
     8                  GEO_STACK  ,IGEO_STACK   ,STIFINTR     ,STRC         ,STRP   ,
     8                  STRR       ,STRTG        ,PERTURB      ,ITAGND       ,NATIV_SMS,
     9                  ILOADP     ,FACLOAD      ,PTSPRI       ,NSIGBEAM ,
     A                  PTBEAM     ,NSIGTRUSS    ,PTTRUSS      ,
     B                  MULTI_FVM    ,SIGI         ,SIGSH        ,SIGSP    ,
     C                  SIGSPH     ,SIGRS        ,SIGBEAM      ,SIGTRUSS     ,STRSGLOB ,
     D                  STRAGLOB   ,ORTHOGLOB    ,ISIGSH       ,IYLDINI      ,KSIGSH3  ,
     E                  FAIL_INI   ,IUSOLYLD     ,IUSER        ,IDDLEVEL     ,INIMAP1D ,
     F                  INIMAP2D   ,FUNC2D,      FVM_INIVEL    ,TAGPRT_SMS   ,IGRBRIC  ,
     G                  IGRQUAD    ,IGRSH4N      ,IGRSH3N      ,IGRPART      ,TOTMAS   ,
     H                  KNOTLOCPC  ,KNOTLOCEL    ,VNIGE        ,BNIGE        ,FXBGLM, 
     I                  FXBCPM     ,FXBCPS       ,FXBLM        ,FXBFLS       ,FXBDLS,
     J                  FXB_MATRIX ,FXB_MATRIX_ADD,FXB_LAST_ADRESS,PTR_NOPT_FXB,R_SKEW,
     K                  KNOD2EL1D  ,NOD2EL1D     ,EBCS_TAB     ,RBY_INIAXIS  ,ALEA    ,
     K                  KNOD2ELC   ,NOD2ELC      ,DR           ,SLRBODY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------                     
      USE SUBMODEL_MOD
      USE ELBUFDEF_MOD      
      USE MESSAGE_MOD      
      USE STACK_MOD
      USE MULTI_FVM_MOD
      USE BPRELOAD_MOD
      USE INIMAP1D_MOD
      USE INIMAP2D_MOD
      USE FUNC2D_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
      USE NLOCAL_REG_MOD
      USE MATPARAM_DEF_MOD  
      USE DETONATORS_MOD 
      USE DRAPE_MOD    
      USE INIVOL_DEF_MOD
      USE INIVOL_ARRAY_MOD
      USE EBCS_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "sphcom.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "warn_c.inc"
#include      "tablen_c.inc"
#include      "lagmult.inc"
#include      "scry_c.inc"
#include      "scr12_c.inc"
#include      "fxbcom.inc"
#include      "sysunit.inc"
#include      "elbuf_c.inc"
#include      "userlib.inc"
#include      "drape_c.inc"
#include      "sms_c.inc"
#include      "boltpr_c.inc"
#include      "mmale51_c.inc"
#include      "titr_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP), IXS(NIXS,*), IXQ(NIXQ,*),IXC(NIXC,*),
     .   IGEO(NPROPGI,*), IXT(NIXT,*),IXP(NIXP,*), IXR(NIXR,*),
     .   NPBY(NNPBY,*),LPBY(*),NPBYL(NNPBY,*),LPBYL(*),NPC(*),
     .   ITAB(*), IPART(*),
     .   LAS(*),IXUR(6,*),
     .   IXTG(NIXTG,*),INDEX(*),ITRI(*),IWA(*),KXX(NIXX,*),IXX(*),
     .   KXSP(*)   ,IXSP(*)  ,NOD2SP(*),ISPCOND(*),ISPSYM(*),ISPTAG(*),
     .   ICODE(*),ISKEW(*),ISKN(LISKN,*), IPM(NPROPMI,*), NSHNOD(*),
     .   PTSHEL(*),PTSH3N(*),PTSOL(*),PTQUAD(*),PTSPH(*),
     .   IXS10(*)   ,IXS20(*)   ,IXS16(*), SH4TREE(*), SH3TREE(*),
     .   IMERGE2(NUMNOD+1),IADMERGE2(NUMNOD+1),
     .   SLNRBM(*) ,NSLNRBM(*),ITAG(*),ITAGEL(*),IRBE2(*) ,LRBE2(*),
     .   ITAGN(*),
     .   IXR_KJ(5,*), SOL2SPH(*), IRST(*),SH3TRIM(*),KXIG3D(NIXIG3D,*),
     .   IXIG3D(*),IGEO_STACK(*),PERTURB(NPERTURB),
     .   NATIV_SMS(*),PTSPRI(*),PTBEAM(*),PTTRUSS(*),STRSGLOB(*),
     .   STRAGLOB(*),ORTHOGLOB(*),ISIGSH,IYLDINI,KSIGSH3,FAIL_INI(5),
     .   IUSOLYLD,IUSER,IDDLEVEL,NBSUBMAT, TAGPRT_SMS(*),SITAGE,FXB_MATRIX_ADD(4,*),
     .   FXB_LAST_ADRESS(*),PTR_NOPT_FXB,R_SKEW(*), NPTS,KNOD2EL1D(*)  ,NOD2EL1D(*),
     .   KNOD2ELC(*),NOD2ELC(*)
      TYPE(T_EBCS_TAB), INTENT(INOUT) :: EBCS_TAB     
      INTEGER,TARGET  :: ITAGE(*)
      INTEGER,POINTER :: ptr_ITAGE
      INTEGER NSIGI,NSIGSH,
     .        NSIGS, NSIGSPH, FXBIPM(NBIPM,*), FXBELM(*),NSIGRS,
     .        NUMEL,STAT,
     .        NCTRLMAX,NSIGBEAM,NSIGTRUSS
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*),SLRBODY
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      my_real
     .   ELBUF(*), MS(*), IN(*), V(*), X(*), GEO(*),PM(NPROPM,*),
     .   RBY(NRBY,*),PLD(*),VEUL(*),SKEW(LSKEW,*),FILL(*),
     .   THK(*),BUFSF(*), VR(3,*),BUFMAT(*),PTG(3,*),XLAS(*),
     .   DTELEM(*),MSS(*), MSQ(*),MSC(*),MST(*),MSP(*),MSR(*),
     .   MSTG(*),MSUR(*),INC(*),RBYL(NRBY,*),
     .   INP(*),INR(*),INTG(*),INUR(*),EVUR(*),
     .   XELEMWA(*),
     .   XFRAME(NXFRAME,*),SPBUF(*),MSSX(*),MSNF(*),
     .   MSSF(*), WMA(*),
     .   VNS(*) ,VNSX(*) ,STC(*) ,STT(*) ,STP(*) ,STR(*) ,
     .   STTG(*) ,STUR(*) ,BNS(*) ,BNSX(*) ,
     .   VOLNOD(*)  ,BVOLNOD(*) , ETNOD(*), STIFINT(*), FXBDEP(*),
     .   FXBVIT(*), FXBACC(*), FXBRPM(*), FXBSIG(*), FXBMOD(*),
     .   INS(*), MCP(*),TEMP(*),RMSTIFN(*), RMSTIFR(*),
     .   MS_LAYER(*),ZI_LAYER(*), MCPC(*), MCPTG(*),
     .   MBUFEL(LBUFEL,*), MDEPL(3*NUMNOD,*),
     .   XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*), MSSA(*), MSRT(*),
     .   KVOL(NBSUBMAT,*),TOTADDMAS,MSZ2(*),
     .   MSIG3D(*),KNOT(*),WIGE(*),RNOISE(*),
     .   SH4ANG(*),SH3ANG(*),GEO_STACK(*),STIFINTR(*),
     .   STRC(*),STRR(*),STRP(*),STRTG(*),SIGI(NSIGS,*),SIGSH(MAX(1,NSIGSH),*),
     .   SIGSP(NSIGI,*),SIGSPH(NSIGSPH,*),SIGRS(NSIGRS,*),SIGBEAM(NSIGBEAM,*),
     .   SIGTRUSS(NSIGTRUSS,*),TOTMAS, KNOTLOCPC(*),KNOTLOCEL(*),VNIGE(*),BNIGE(*),
     .   FXBGLM(*),FXBCPM(*),FXBCPS(*),FXBLM(*),FXBFLS(*),FXBDLS(*),FXB_MATRIX(*),
     .   RBY_INIAXIS(7,*),ALEA(*),DR(SDR)

      my_real, DIMENSION(NUMNOD*2), TARGET ::
     .   STIFN
      my_real , DIMENSION(:), POINTER ::
     .    STIFR
C
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_RBE2,PTR_NOPT_ADM,
     .        PTR_NOPT_FUN,IOPT
      INTEGER FXBNOD(*), FXANI(2,*),ITAGND(*)
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE (INIMAP1D_STRUCT), DIMENSION(NINIMAP1D), INTENT(INOUT) :: INIMAP1D
      TYPE (INIMAP2D_STRUCT), DIMENSION(NINIMAP2D), INTENT(INOUT) :: INIMAP2D
      TYPE (FUNC2D_STRUCT), DIMENSION(NFUNC2D), INTENT(IN) :: FUNC2D
      TYPE (FVM_INIVEL_STRUCT), INTENT(IN) :: FVM_INIVEL(*)
      TYPE (NLOCAL_STR_)   :: NLOC_DMG 
      TYPE (GRP_PARAM_STRUCT_), DIMENSION(NGROUP)  :: GROUP_PARAM_TAB
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (ADMAS_)  , DIMENSION(NODMAS)  :: IPMAS
      TYPE (INIVOL_) , DIMENSION(NINIVOL) :: INIVOL
      TYPE (DETONATOR_STRUCT_) :: DETONATORS
      TYPE (DRAPE_) :: DRAPE(*)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C remove automatic allocation to reduce stack consumption
C      INTEGER *8 I8MI(6,NUMNOD)
      INTEGER (KIND=8), DIMENSION(:,:), ALLOCATABLE :: I8MI
      INTEGER NG, NEL, NVC, K, N, M, NSL, NN1, NN2, NN3, I, K0,NV46,
     .   ISPH, J, IG, OFFSET,ISOLNOD,IPROP,IGTYP,
     .   I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,I15K,I15ATH,
     .   I15L,NC1_OLD, NC2_OLD, NC3_OLD, NC4_OLD,
     .   NC5_OLD, NC6_OLD, NC7_OLD, NC8_OLD,
     .   NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8,
     .   IINT, ISENS,ITHK, IHBE, JHBE, ILEV,ISH3N,
     .   KK1, KK2, KK3,IADUIX,IADUX ,IADUV ,IADUVR,IADUMS,
     .   IADUIN,IADUSM,IADUSR,IADUMV,IADURV, NUVAR,ICNOD, RBYID,
     .   ADRRPM,ALM,ASIG,NELS,NELC,NELTG,AMOD,NBNO,NME,NML,ARPM,LVSIG,
     .   IFILE,IRCS,NELT,NELP,FXBID, ANOD, IRCM, NSNI, NSN, NMANI, IMIN, IMAX,
     .   NELEMR,CPT_ELTENS,IXFEM,ITG,ISUBSTACK,NCTRL, ITETRA10, KK,PX,PY,PZ,IPID
      INTEGER IFRAC,IDP,IDC,IDSURF,IFILL,JMID,JADCONTY, NBCONTY,NSEG_SWIFT_SURF,NSEG_USED,
     .   NSURF_INVOL,IMAT,ICPRE
      INTEGER SOLMAT(0:MAXLAW), COQMAT(0:MAXLAW), TRUMAT(0:MAXLAW),
     .   POUMAT(0:MAXLAW),SPHMAT(0:MAXLAW),
     .   RESMAT(0:MAXLAW),RESPID(0:50), SPHPID(0:50),
     .   SOLPID(0:50), COQPID(0:52), TRUPID(0:50), POUPID(0:50)
      INTEGER II,NINDX
C remove automatic allocation to reduce stack consumption
      INTEGER, DIMENSION(:), ALLOCATABLE :: KNOD2SURF
      INTEGER NTRACE0,NTRACE,NNOD2SURF,JAD0,
     .   NSEGSURF,FLAG_KJ
      INTEGER IBOLTP !Bolt preloading
      my_real
     .   DT2S, B1, B2, B3, B6, B5, B9, 
     .   XG, YG, ZG, XX, YY, ZZ, XY, XZ, YZ,DTNODA,FILL_RATIO
      my_real, DIMENSION(:), ALLOCATABLE ::
     .    MBUFEL_TMP, MDEPL_TMP,PARTSAV,MCPS,MCPSX,
     .    MS_LAYERC,ZI_LAYERC, MSZ2C,ZPLY,PARTSAV1_PON,MCPP

      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .                                   IRIG_NODE, CONNEC
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGN,
     . IPHASE,ITAGNSOL,INOD2SURF,PART_FILL,IVOLSURF,ITAGSURF,SEGTOSURF,SWIFTSURF
      my_real, DIMENSION(:), ALLOCATABLE ::
     .    PART_AREA,NOD_NORM,ELE_AREA
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NSOLTOSF,NBIP,INPHASE
      my_real, DIMENSION(:,:), ALLOCATABLE :: DIS
      my_real
     .    ADDEDMS(NPART), MST3(MVSIZ),PTTRIA(MVSIZ)
      INTEGER ID,ISTOT, NF1,ICUMU,I15_,SIPHASE, NUMEL_TOT,NNOD,NSROT
      CHARACTER*nchartitle,
     .   TITR
      LOGICAL :: ERROR_THROWN
            
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE(G_BUFEL_) ,POINTER :: GBUF   
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C-----------------------------------------------
      EXTERNAL UEL2SYS
      INTEGER  UEL2SYS
c___________________________________________________
      my_real
     .    r8_deuxm43
      integer*8  i8_deuxp43
      data i8_deuxp43 /'80000000000'x/
      r8_deuxm43 = 1.d00 / i8_deuxp43
c___________________________________________________
C                   1 2 3 4 5 6 7 8 9 10
      DATA SOLPID/1,0,0,0,0,0,1,0,0,0,0,
     1              0,0,0,1,1,0,0,0,0,1,
     2              1,1,0,0,0,0,0,0,1,0,
     3              0,0,0,0,0,0,0,0,0,0,
     4              0,0,1,0,0,0,0,0,0,0/
      DATA COQPID/1,1,0,0,0,0,0,1,0,1,1,
     1              1,0,0,0,0,1,1,0,1,0,
     2              0,0,0,0,0,0,0,0,0,0,
     3              0,0,0,0,0,0,0,0,0,0,
     4              0,0,0,0,0,0,0,0,0,0,
     5              1,1/
      DATA TRUPID/0,0,1,0,0,0,0,0,0,0,0,
     1              0,0,0,0,0,0,0,0,0,0,
     2              0,0,0,0,0,0,0,0,0,0,
     3              0,0,0,0,0,0,0,0,0,0,
     4              0,0,0,0,0,0,0,0,0,0/
      DATA POUPID/0,0,0,1,0,0,0,0,0,0,0,
     1              0,0,0,0,0,0,0,1,0,0,
     2              0,0,0,0,0,0,0,0,0,0,
     3              0,0,0,0,0,0,0,0,0,0,
     4              0,0,0,0,0,0,0,0,0,0/
      DATA RESPID/0,0,0,0,1,0,0,0,1,0,0,
     1              0,1,1,0,0,0,0,0,0,0,
     2              0,0,1,0,1,1,1,0,1,1,
     3              1,1,1,0,1,1,0,0,0,0,
     4              0,0,0,1,1,1,0,0,0,0/
      DATA SPHPID/0,0,0,0,0,0,0,0,0,0,0,
     1              0,0,0,0,0,0,0,0,0,0,
     2              0,0,0,0,0,0,0,0,0,0,
     3              0,0,0,1,0,0,0,0,0,0,
     4              0,0,0,0,0,0,0,0,0,0/
C=======================================================================

      ILOOP_NRF51 = 0
      NVC = 0
      STAT = 0

      IF(IPARI0==3)THEN
        ALLOCATE (I8MI(6,NUMNOD)                   ,STAT=stat)
      ELSE
        ALLOCATE (I8MI(6,1)                        ,STAT=stat)
      ENDIF

      ALLOCATE (PARTSAV(20*NPART)                  ,STAT=stat)

      STIFR => STIFN(NUMNOD+1:NUMNOD*2)
      ALLOCATE (PARTSAV1_PON(NPART)                ,STAT=stat)
C
      IF(NPRELOAD > 0) THEN
        ALLOCATE (VPRELOAD(7*NUMELS)               ,STAT=stat)
      ENDIF       
C
      IF (NPART   > 0) PARTSAV= ZERO
      IF (NPART   > 0) PARTSAV1_PON=ZERO
      IF (NPRELOAD > 0 .AND. NUMELS > 0) VPRELOAD = ZERO
C xfem
      ITG = 0
      IF(ICRACK3D > 0)ITG = 1 + NUMELC
C---
C initial volume fraction
C---
      NTRACE0 = 3
      NTRACE0 = 2*NTRACE0+1
      NTRACE  = NTRACE0**3
C
C     To avoid thwrowing ngroup times the same error
      ERROR_THROWN = .FALSE. 
C
      ANIM_M=0
      DO I=1,MX_ANI
         ANIM_N(I)=0
         ANIM_V(I)=0
         ANIM_CE(I)=0
         ANIM_CT(I)=0
         ANIM_SE(I)=0
         ANIM_ST(I)=0
         ANIM_FE(I)=0
         ANIM_FT(I)=0
      ENDDO
      NN_ANI=0
      NV_ANI=0
      NCE_ANI=0
      NCT_ANI=0
      NSE_ANI=0
      NST_ANI=0
      NFE_ANI=0
      NINDX = 0
C ---
      IF(IRIGID_MAT > 0 ) THEN
        NELEMR = NUMELC + NUMELS10 + NUMELS8 + NUMELTG
        ALLOCATE(IRIG_NODE(NUMNOD))
        ALLOCATE(CONNEC(NELEMR*10))
          IRIG_NODE = 0
          CONNEC = 0
      ELSE
          ALLOCATE(CONNEC(0),IRIG_NODE(0))
      ENDIF
C
C  ply xfem
C
      IF(IPLYXFEM> 0 ) THEN
        ALLOCATE(MS_LAYERC(NPLYMAX*NUMELC))
        ALLOCATE(ZI_LAYERC(NPLYMAX*NUMELC))
        ALLOCATE(MSZ2C(NUMELC))
        ALLOCATE(ZPLY(NPLYMAX))
        MS_LAYERC = ZERO
        ZI_LAYERC = ZERO
        MSZ2C = ZERO
        ZPLY = ZERO
       ELSE
        ALLOCATE(MS_LAYERC(0))
        ALLOCATE(ZI_LAYERC(0))
        ALLOCATE(MSZ2C(0))
        ALLOCATE(ZPLY(0))
      ENDIF
C-------------------------------------
C     MASSE + INERTIE IPARITH = 4
C-------------------------------------
      IF(IPARI0 == 3)THEN
        DO N=1,NUMNOD
            I8MI(1,N) = 0
            I8MI(2,N) = 0
            I8MI(3,N) = 0
            I8MI(4,N) = 0
            I8MI(5,N) = 0
            I8MI(6,N) = 0
        ENDDO
      ENDIF
      DO N=0,MAXLAW
        SOLMAT(N) = 1
        COQMAT(N) = 0
        TRUMAT(N) = 0
        POUMAT(N) = 0
        SPHMAT(N) = 0
        RESMAT(N) = 0
      ENDDO
      DO N=51,MAXLAW
        SOLMAT(N) = 0
      ENDDO
      SOLMAT(15) = 0
      SOLMAT(19) = 0
      SOLMAT(25) = 1
      SOLMAT(27) = 0
      SOLMAT(32) = 0
      SOLMAT(43) = 0
C
C     solids
      SOLMAT(53) = 1
      SOLMAT(51) = 1
      SOLMAT(52) = 1
      SOLMAT(56) = 1
      SOLMAT(59) = 1
      SOLMAT(60) = 1
      SOLMAT(61) = 0
      SOLMAT(62) = 1
      SOLMAT(65) = 1
      SOLMAT(66) = 1
      SOLMAT(67) = 1
      SOLMAT(68) = 1
      SOLMAT(69) = 1
      SOLMAT(70) = 1
      SOLMAT(71) = 1
      SOLMAT(72) = 1
      SOLMAT(74) = 1
      SOLMAT(75) = 1
      SOLMAT(76) = 1
      SOLMAT(77) = 1
      SOLMAT(78) = 1
      SOLMAT(79) = 1
      SOLMAT(80) = 1
      SOLMAT(81) = 1
      SOLMAT(82) = 1
      SOLMAT(83) = 1
      SOLMAT(84) = 1
      SOLMAT(88) = 1
      SOLMAT(92) = 1
      SOLMAT(90) = 1
      SOLMAT(93) = 1
      SOLMAT(94) = 1
      SOLMAT(95) = 1
      SOLMAT(96) = 1
      SOLMAT(97) = 1
      SOLMAT(99) = 1
      SOLMAT(100)= 1
      SOLMAT(101)= 1
      SOLMAT(102)= 1
      SOLMAT(103)= 1
      SOLMAT(104)= 1
      SOLMAT(105)= 1
      SOLMAT(106)= 1
      SOLMAT(107)= 1
      SOLMAT(109)= 1
      SOLMAT(111)= 1
      SOLMAT(112)= 1
      SOLMAT(115)= 1
      SOLMAT(116)= 1
      SOLMAT(117)= 1
      SOLMAT(120)= 1
      SOLMAT(121)= 1
      SOLMAT(151)= 1
      SOLMAT(187)= 1
      SOLMAT(200)= 1
C     shells
      COQMAT(0)  = 1
      COQMAT(1)  = 1
      COQMAT(2)  = 1
      COQMAT(7 ) = 1
      COQMAT(13) = 1
      COQMAT(15) = 1
      COQMAT(19) = 1
      COQMAT(22) = 1
      COQMAT(25) = 1
      COQMAT(27) = 1
      COQMAT(29) = 1
      COQMAT(30) = 1
      COQMAT(31) = 1
      COQMAT(32) = 1
      COQMAT(34) = 1
      COQMAT(35) = 1
      COQMAT(36) = 1
      COQMAT(42) = 1
      COQMAT(43) = 1
      COQMAT(44) = 1
      COQMAT(45) = 1
      COQMAT(48) = 1
      COQMAT(52) = 1
      COQMAT(55) = 1
      COQMAT(56) = 1
      COQMAT(57) = 1
      COQMAT(58) = 1
      COQMAT(60) = 1
      COQMAT(62) = 1
      COQMAT(63) = 1
      COQMAT(64) = 1
      COQMAT(65) = 1
      COQMAT(66) = 1
      COQMAT(69) = 1
      COQMAT(71) = 1
      COQMAT(72) = 1
      COQMAT(73) = 1
      COQMAT(76) = 1
      COQMAT(78) = 1
      COQMAT(80) = 1
      COQMAT(82) = 1
      COQMAT(85) = 1
      COQMAT(86) = 1
      COQMAT(87) = 1
      COQMAT(88) = 1
      COQMAT(91) = 1
      COQMAT(92) = 0  ! is not available
      COQMAT(93) = 1
      COQMAT(94) = 0  ! is not available
      COQMAT(96) = 1  
      COQMAT(98) = 1  
      COQMAT(99) = 1
      COQMAT(104) = 1
      COQMAT(107) = 1
      COQMAT(109) = 1
      COQMAT(110) = 1
      COQMAT(112) = 1
      COQMAT(119) = 1
      COQMAT(121) = 1
      COQMAT(151) = 1
      COQMAT(200) = 1
C     truss
      TRUMAT(0)  = 1
      TRUMAT(1)  = 1
      TRUMAT(2)  = 1
      TRUMAT(34) = 1
      TRUMAT(44) = 1
C     beam
      POUMAT(0)  = 1
      POUMAT(1)  = 1
      POUMAT(2)  = 1
      POUMAT(34) = 1
      POUMAT(36) = 1
      POUMAT(44) = 1
      POUMAT(71) = 1
C     sph
      SPHMAT(1)  = 1
      SPHMAT(2)  = 1
      SPHMAT(3)  = 1
      SPHMAT(4)  = 1
      SPHMAT(5)  = 1
      SPHMAT(6)  = 1
      SPHMAT(10) = 1
      SPHMAT(12) = 1
      SPHMAT(18) = 1
      SPHMAT(21) = 1
      SPHMAT(22) = 1
      SPHMAT(23) = 1
      SPHMAT(24) = 1
      SPHMAT(28) = 1
      SPHMAT(29) = 1
      SPHMAT(30) = 1
      SPHMAT(31) = 1
      SPHMAT(32) = 1
      SPHMAT(33) = 1
      SPHMAT(34) = 1
      SPHMAT(35) = 1
      SPHMAT(36) = 1
      SPHMAT(38) = 1
      SPHMAT(40) = 1
      SPHMAT(41) = 1
      SPHMAT(42) = 1
      SPHMAT(49) = 1
      SPHMAT(50) = 1
      SPHMAT(53) = 1

      SPHMAT(66) = 1
      SPHMAT(70) = 1
      SPHMAT(72) = 1
      SPHMAT(75) = 1
      SPHMAT(76) = 1
      SPHMAT(79) = 1
      SPHMAT(81) = 1
      SPHMAT(88) = 1
      SPHMAT(90) = 1 ! not tested
      SPHMAT(92) = 1 ! not tested
      SPHMAT(93) = 1 ! not tested
      SPHMAT(94) = 1 ! not tested
      SPHMAT(97) = 1
      SPHMAT(102)= 1
      SPHMAT(103)= 1 
      SPHMAT(111)= 1 ! is not tested
      SPHMAT(105)= 1
      RESMAT(54) = 1

      I15ATH = 1+LIPART1*NPART+LIPART1*NTHPART
      I15A   = I15ATH+2*9*NPART+2*9*NTHPART
      I15B   = I15A+NUMELS
      I15C   = I15B+NUMELQ
      I15D   = I15C+NUMELC
      I15E   = I15D+NUMELT
      I15F   = I15E+NUMELP
      I15G   = I15F+NUMELR
      I15H   = I15G
      I15I   = I15H+NUMELTG
      I15J   = I15I+NUMELX
      I15K   = I15J+NUMSPH
      I15L   = I15K+NUMELIG3D

C-----------------------------------------------------
C     VERIFICATION DES MATERIAUX ET PID
C-----------------------------------------------------
      CALL CHECKMP(NUMELS,IXS,NIXS,NIXS-1,NIXS,
     .             SOLMAT,SOLPID,IPM,IGEO,'BRICK'  ,IPART(I15A))
      CALL CHECKMP(NUMELQ,IXQ,NIXQ,NIXQ-1,NIXQ,
     .             SOLMAT,SOLPID,IPM,IGEO,'QUAD'   ,IPART(I15B))
      CALL CHECKMP(NUMELC,IXC,NIXC,NIXC-1,NIXC,
     .             COQMAT,COQPID,IPM,IGEO,'SHELL'  ,IPART(I15C))
      CALL CHECKMP(NUMELTG,IXTG,NIXTG,NIXTG-1,NIXTG,
     .             COQMAT,COQPID,IPM,IGEO,'SHELL3N',IPART(I15H))
      CALL CHECKMP(NUMELT,IXT,NIXT,NIXT-1,NIXT,
     .             TRUMAT,TRUPID,IPM,IGEO,'TRUSS'  ,IPART(I15D))
      CALL CHECKMP(NUMELP,IXP,NIXP,NIXP-1,NIXP,
     .             POUMAT,POUPID,IPM,IGEO,'BEAM'   ,IPART(I15E))
      CALL CHECKMP(NUMELR,IXR,NIXR,     1,NIXR,
     .             -1    ,RESPID,IPM,IGEO,'SPRING' ,IPART(I15F))
      CALL CHEKMP2(NUMSPH,IPART ,IPART(I15J),KXSP,NISP,
     .             NISP,SPHMAT,SPHPID,IPM,IGEO,
     .             'SPHCEL')

C---------------------------------------------------------
C     Tri du vecteur de bolt Preloading
C---------------------------------------------------------
      IF (NPRELOAD > 0) THEN
        CALL INIBOLTPREL(IXS,IPRELOAD ,PRELOAD  ,VPRELOAD, IFLAG_BPRELOAD)
      ENDIF
C-----------------------------------------------------
C     PREPARATION DU CALCUL DES MASSES PAR PARTICULE SI CONDITION(S) DE
C     SYMETRIE
C-----------------------------------------------------
      IF (NUMSPH/=0.AND.NSPCOND/=0)
     .  CALL INSPCND(ISPCOND ,IGRNOD  ,KXSP    ,IXSP    ,
     .               NOD2SP  ,ITAB    ,ICODE   ,ISKEW   ,ISKN    ,
     .               SKEW    ,XFRAME  ,X       ,ISPSYM  ,ISPTAG  ,
     .               PM      ,GEO     ,IPART   ,IPART(I15J))
C--------------------------------------------
C Seat belts initialization :
C--------------------------------------------
      IF (N_SEATBELT > 0) CALL INI_SEATBELT(IPARG,ELBUF_TAB,KNOD2EL1D,NOD2EL1D,IXR,
     .                                      X,ITAB,IPM,ALEA,KNOD2ELC,
     .                                      NOD2ELC,IXC,NPBY,LPBY,SLRBODY,
     .                                      ICODE)
C-----------------------------------------------------
C     INITIALISATION DES BUFFERS DES ELEMENTS
C     INITIALISATION DES MASSES ET DES INERTIES
C-----------------------------------------------------
C
C  for heat transfer
C
      IF(ITHERM_FE > 0 ) THEN
        ALLOCATE(MCPS(8*NUMELS))
        MCPS = ZERO
        IF(NUMELS10 > 0.OR.NUMELS16 > 0 .OR.NUMELS20 > 0)THEN
         ALLOCATE(MCPSX(12*NUMELS))
         MCPSX = ZERO
        ENDIF
         ALLOCATE(MCPP(NUMELP))
        MCPP = ZERO
      ELSE
        ALLOCATE(MCPSX(0), MCPS(0), MCPP(0))  
      ENDIF
C---
      IF ((IMASADD > 0).OR.(NLOC_DMG%IMOD > 0)) THEN
        ALLOCATE(PART_AREA(NPART)      ,STAT=stat)
        PART_AREA(1:NPART)  = ZERO
        ALLOCATE(ELE_AREA(NUMELC+NUMELTG)      ,STAT=stat)
        ELE_AREA(1:NUMELC+NUMELTG)  = ZERO
      ELSE
        ALLOCATE(PART_AREA(0),ELE_AREA(0))
      END IF
C---
      WRITE(IOUT,'(//)')
      DT2S=1.E6
      CPT_ELTENS = 0
C
      DO  NG=1,NGROUP
        MTN=IPARG(1,NG)
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ITY=IPARG(5,NG)
        NPT=IPARG(6,NG)
        JALE=IPARG(7,NG)
        ISMSTR=IPARG(9,NG)
        JEUL  =IPARG(11,NG)
        JTUR  =IPARG(12,NG)
        JTHE  =IPARG(13,NG)
        JLAG  =IPARG(14,NG)
        ISH3N =IPARG(23,NG)
        JMULT =IPARG(20,NG)
        JPOR  =IPARG(27,NG)
        ISOLNOD = IPARG(28,NG)
        USER_GRP_DOMAIN = IPARG(32,NG)+1
        IGTYP  = IPARG(38,NG)
        ISRAT  = IPARG(40,NG)
        ISORTH = IPARG(42,NG)
        ISROT  = IPARG(41,NG)
        IF(ISOLNOD == 10) ISROT  = IPARG(74,NG)
        IEXPAN = IPARG(49,NG)
        ISHXFEM_PLY  = IPARG(50,NG)
        IF (ICRACK3D == 0) THEN
          IPARG(54,NG) = 0
        END IF
        IXFEM = IPARG(54,NG)
        ISUBSTACK  = IPARG(71,NG)
        IBOLTP  = IPARG(72,NG)
        IFORMDT = IPARG(73,NG)
        JCLOS=0
        ISTOT = 0
        IF (ITY==1.AND.(ISMSTR>=10.AND.ISMSTR<=12)) ISTOT = 1 
        IF (ITY == 3.OR.ITY == 7) THEN
C Initialize vectorization flags to zero for 3 and 4 nodes shell
          NC1_OLD = 0
          NC2_OLD = 0
          NC3_OLD = 0
          NC4_OLD = 0
        ELSEIF (ITY == 1) THEN
C Initialize vectorization flags to zero for solid elements
          NC1_OLD = 0
          NC2_OLD = 0
          NC3_OLD = 0
          NC4_OLD = 0
          NC5_OLD = 0
          NC6_OLD = 0
          NC7_OLD = 0
          NC8_OLD = 0
          IF((ISOLNOD == 4 .AND.ISROT==2).OR.
     .       (ISOLNOD == 10.AND.ISROT==1).OR.
     .       (ISOLNOD == 10.AND.ISROT==3))THEN
            ISROT = 0
            IPARG(41,NG) = 0
          ENDIF
        ENDIF
        IF((NUMELS/=0).AND.(N2D/=0))THEN
         CALL ANCMSG(MSGID=603,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2)
        END IF
C------------------------------------------------------------------------------
C Warning : for a new element type perform the computation of mass and inertia 
C           in parallel arithmetic in subroutine SPMD_MSIN
C------------------------------------------------------------------------------
C
       IF ((MTN == 0 .AND. IGTYP /= 52 .AND. IGTYP /= 51)  .or. 
     .     (IGTYP == 0 .and. (ITY == 1 .or. ITY == 3 .or. ITY == 7)) ) THEN
          LFT=1
          LLT=NEL
          NFT = IPARG(3,NG)
          IHBE=IPARG(23,NG)
          ISOLNOD = IPARG(28,NG)
          ILEV=IPARG(45,NG)
C
          CALL INIVOID(ELBUF_TAB(NG),
     1        IXC        ,IXS        ,IXTG       ,X         ,V        ,
     2        PM         ,GEO        ,MS         ,IN        ,PTG      ,
     3        MSC        ,MSS        ,MSTG       ,INC       ,INTG     ,
     4        THK(1+NFT) ,THK(1+NFT+NUMELC),PARTSAV,IPART(I15A),
     5        IPART(I15C),IPART(I15H),VEUL       ,DTELEM    ,IHBE     ,
     6        ISOLNOD    ,NVC        ,I8MI       ,MSNF      ,MSSF     ,
     7        IGEO       ,ETNOD      ,NSHNOD     ,STC       ,STTG     ,
     8        WMA        ,SH4TREE    ,SH3TREE    ,MCP       ,MCPC     ,
     9        TEMP       ,MCPS       ,XREFC      ,XREFTG    ,XREFS    ,
     A        MSSA       ,VOLNOD     ,BVOLNOD    ,VNS       ,BNS      ,
     B        SH3TRIM    ,ISUBSTACK  ,STACK      ,RNOISE    ,PERTURB  ,
     C        ELE_AREA   ,PART_AREA  ,IPART(I15D),IXT       ,IPART(I15E),
     D        IXP        ,MST        ,MSP        ,STT       ,STP       ,
     E        STRP       ,INP        ,STIFINT    ,MCPP      ,INR       ,
     F        MSR        ,MSRT       ,STR        ,IPART(I15F),ITAB     ,
     G        IXR       , IMERGE2    ,IADMERGE2  ,NEL)
C
       ELSEIF( MTN == 13) THEN
C Rigid material
           LFT=1
           LLT=NEL
           NFT = IPARG(3,NG)
           IHBE=IPARG(23,NG)
           ISOLNOD = IPARG(28,NG)
           ILEV=IPARG(45,NG)
C
          CALL INIRIG_MAT(
     1        IXC    ,IXS        ,IXTG       ,IXS10     ,X   ,
     2        V      ,PM         ,GEO        ,MS        ,IN  ,
     3        PTG    ,MSC        ,MSS        ,MSTG      ,INC ,
     4        INTG   ,THK(1+NFT) ,THK(1+NFT+NUMELC),PARTSAV,IPART(I15A),
     5        IPART(I15C),IPART(I15H),VEUL       ,DTELEM    ,IHBE     ,
     6        ISOLNOD    ,NVC        ,I8MI       ,MSNF      ,MSSF     ,
     7        IGEO       ,ETNOD      ,NSHNOD     ,STC       ,STTG     ,
     8        WMA        ,SH4TREE    ,SH3TREE    ,MCP       ,MCPC     ,
     9        TEMP       ,MCPS       ,MSSX       ,MCPSX      ,INS     ,
     A        STIFN      ,STIFR      ,CONNEC     ,IRIG_NODE ,NELEMR   ,
     B        NINDX      ,XREFC      ,XREFTG     ,XREFS     ,MSSA     ,
     C        SH3TRIM    ,ISUBSTACK  ,BUFMAT     ,IPM       ,STACK    ,
     D        RNOISE     ,STRC       ,STRTG      ,PERTURB   ,NEL      ,
     E        GROUP_PARAM_TAB(NG))
C
       ELSE
C Element types
        LFT=1
        LLT=NEL
        OFFSET=0
        NFT = IPARG(3,NG)
        JSPH=0
        JCVT=0
        NF1 = NFT + 1
        !----------------------------------------!
        !   ITY == 1                   3D-SOLIDS !
        !----------------------------------------!
        IF (ITY == 1) THEN
         GBUF => ELBUF_TAB(NG)%GBUF
         IF (IUSOLYLD == 1 ) THEN
          CALL SCALEINI(
     .           ELBUF_TAB(NG), IXS      , SIGSP    ,SIGI  ,   NSIGI,
     .           NEL          ,LFT       , LLT      ,NFT   ,   NSIGS,
     .           PTSOL ,IGEO )
         ENDIF                  
         IF (ISOLNOD == 4.AND.(ISROT==0.OR.ISROT==3))THEN
            IF (MULTI_FVM%IS_USED) THEN
               CALL MULTIFLUID_INIT3T(ELBUF_TAB(NG), 
     .              NEL, NSIGS, NSIGI, IXS, IGEO, IPM, IPARG, ALE_CONNECTIVITY, IPART(I15A), PTSOL, 
     .              NPC, IPART, ILOADP,
     .              XREFS, GEO, PM, FACLOAD, PLD, SKEW, SIGI, BUFMAT, X, 
     .              WMA, PARTSAV, MS, V, MSS, MSSF, MSSA, MSNF, MCPS, ERROR_THROWN, DETONATORS)
            ELSE
               CALL S4INIT3(
     1              ELBUF_TAB(NG),MS          ,IXS      ,PM         ,X          ,  
     2              DETONATORS    ,GEO          ,VEUL   ,ALE_CONNECTIVITY  ,IPARG(1,NG),
     3              DTELEM  ,SIGI         ,NEL    ,SKEW       ,IGEO       ,
     4              STIFN   ,PARTSAV      ,V      ,IPART(I15A),MSS        ,
     5              IPART        ,MSNF        ,IPARG    ,
     6              MSSF         ,IPM         ,NSIGS    ,VOLNOD     ,BVOLNOD    ,    
     7              VNS          ,BNS         ,WMA      ,PTSOL      ,BUFMAT     ,  
     8              MCP          ,MCPS        ,TEMP     ,NPC        ,PLD        ,  
     9              IUSER        ,SIGSP       ,NSIGI    ,MSSA       ,XREFS      ,  
     A              STRSGLOB(NF1),STRAGLOB(NF1),FAIL_INI ,SPBUF      ,SOL2SPH    ,
     B              ILOADP       ,FACLOAD      ,RNOISE   ,PERTURB    )                            
               IF (ISTOT == 1) THEN
                  CALL SGSAVINI(ISOLNOD,X,IXS(1,NFT+1),GBUF%SMSTR,NEL)
               ENDIF   
               IF (NXREF > 0 .AND. JLAG/=0 .AND. JSPH==0)THEN
                  CALL S4REFSTA3(
     1                 ELBUF_TAB(NG),IXS        ,PM         ,GEO      ,IPARG(1,NG),
     2                 IPM        ,IGEO       ,SKEW       ,X        ,XREFS      ,
     3                 NEL          ,IPART(I15A),IPART    ,BUFMAT     ,
     4                 NPC          ,PLD        )
C Case total strain
                  IF (ISTOT == 1) THEN
                     CALL SGSAVREF(ISOLNOD,XREFS(1,1,NFT+1),GBUF%SMSTR,NEL)
                  END IF
               ENDIF
               IF (NSIGI > 0 ) THEN
                 CALL SGSAVINIEREF(ISOLNOD,STRAGLOB(NF1),SIGSP,NSIGI,PTSOL(NF1),
     .                             GBUF%SMSTR,GBUF%OFF,NEL)
               END IF
            ENDIF
         ELSEIF(ISOLNOD == 10 .OR.(ISOLNOD == 4 .AND.ISROT == 1))THEN
          KK1=1+NUMELS*NIXS
          CALL S10INIT3(ELBUF_TAB(NG),
     1         MS        ,IXS      ,PM         ,X          ,
     2         DETONATORS  ,GEO       ,VEUL    ,ALE_CONNECTIVITY  ,IPARG(1,NG),
     3         DTELEM      ,SIGI      ,NEL     ,SKEW       ,IGEO       ,
     4         STIFN       ,PARTSAV   ,V       ,IPART(I15A),MSS        ,
     5         IXS10       ,IPART     ,
     7         MSSX      ,SIGSP   ,NSIGI      ,IPM        ,
     8         IUSER       ,NSIGS     ,VOLNOD  ,BVOLNOD    ,VNS        ,
     9         BNS         ,VNSX      ,BNSX    ,PTSOL      ,BUFMAT     ,
     A         MCP         ,MCPS      ,MCPSX   ,TEMP       ,NPC        ,
     B         PLD         ,IN        ,STIFR   ,INS        ,MSSA       ,
     C         STRSGLOB(NF1),STRAGLOB(NF1),FAIL_INI,ILOADP ,FACLOAD    ,
     D         PERTURB     ,RNOISE    )
              IF (NSIGI > 0 ) THEN
                 NNOD = 10
                 NSROT = 0
                 IF(ISOLNOD == 4 .AND.ISROT == 1) NSROT = 4
                 CALL SGSAVINIEREFQ(NNOD,STRAGLOB(NF1),SIGSP,NSIGI,PTSOL(NF1),
     .                            GBUF%SMSTR,GBUF%OFF,IXS(1,NF1),DR,NSROT,NEL)
              END IF
         ELSEIF(ITY==1.AND.ISOLNOD==20)THEN
          KK1=1+NUMELS*NIXS+NUMELS10*6
          CALL S20INIT3(
     1         ELBUF_TAB(NG),MS        ,IXS    ,PM         ,X          ,
     2         DETONATORS  ,GEO       ,VEUL   ,ALE_CONNECTIVITY  ,IPARG(1,NG),
     3         DTELEM      ,SIGI      ,NEL    ,SKEW       ,IGEO       ,
     4         STIFN       ,PARTSAV   ,V      ,IPART(I15A),MSS        ,
     5         IXS20        ,IPART    ,MSSX   ,SIGSP      ,NSIGI      ,
     7         IPM         , IUSER    ,NSIGS  ,VOLNOD     ,BVOLNOD    ,
     8         VNS         ,BNS       ,VNSX   ,BNSX       ,PTSOL      ,
     9         BUFMAT      ,MCP       ,MCPS   ,MCPSX      ,TEMP       ,
     A         NPC         ,PLD       ,STRSGLOB(NF1),STRAGLOB(NF1),FAIL_INI   ,
     B         ILOADP      ,FACLOAD   ,PERTURB,RNOISE     )
         ELSEIF(ITY==1.AND.ISOLNOD==16)THEN
          KK1=1+NUMELS*NIXS+NUMELS10*6+NUMELS20*12
          CALL S16INIT3(
     1       ELBUF_TAB(NG),MS        ,IXS     ,PM         ,X          ,  
     2         DETONATORS  ,GEO       ,VEUL   ,ALE_CONNECTIVITY  ,IPARG(1,NG),
     3         DTELEM      ,SIGI      ,NEL    ,SKEW       ,IGEO       ,
     4         STIFN       ,PARTSAV   ,V      ,IPART(I15A),MSS        ,
     5       IXS16        ,IPART     ,MSSX    ,SIGSP      ,NSIGI      ,           
     6       IPM          ,IUSER     ,NSIGS   ,VOLNOD     ,BVOLNOD    , 
     7       VNS          ,BNS       ,VNSX    ,BNSX       ,PTSOL      ,    
     8       BUFMAT       ,MCP       ,MCPS    ,MCPSX      ,TEMP       ,    
     9       NPC          ,PLD       ,STRSGLOB(NF1),STRAGLOB(NF1),FAIL_INI   ,
     A       ILOADP       ,FACLOAD   ,PERTURB ,RNOISE     )                
         ELSEIF(ITY==1)THEN
          JHBE=IPARG(23,NG)
          JCLOS=IPARG(33,NG)
          IINT =IPARG(36,NG)
          IF (JHBE==1.OR.JHBE==2.OR.JHBE==12.OR.JHBE==16) THEN
            JCVT=0
          ELSE
            JCVT=1
          ENDIF
          IPROP =  IXS(10,NFT+1)
          IGTYP =  NINT(GEO(NPROPG*(IPROP-1)+12))
          NUVAR =  NINT(GEO(NPROPG*(IPROP-1)+25))
          ISTRAIN = IPARG(44,NG)
          IF (JHBE == 15) THEN
           !Thick shells PA6 / HQEPH
            IF (ISOLNOD == 6)THEN
              CALL S6CINIT3(
     .             ELBUF_TAB(NG),MS       ,IXS      ,PM         ,X    ,
     .             DETONATORS   ,GEO      ,VEUL     ,ALE_CONNECTIVITY      ,IPARG(1,NG),
     .             DTELEM       ,SIGI     ,NEL      ,SKEW       ,IGEO       ,
     .             STIFN        ,PARTSAV  ,V        ,IPART(I15A),MSS,
     .             IPART        ,
     .             SIGSP        ,NSIGI    ,IPM      ,IUSER      ,NSIGS   ,
     .             VOLNOD       ,BVOLNOD  ,VNS      ,BNS        ,PTSOL   ,
     .             BUFMAT       ,MCP      ,MCPS     ,MCPSX      ,TEMP    ,
     .             NPC          ,PLD      ,STRSGLOB(NF1),STRAGLOB(NF1),MSSA    ,
     .             ORTHOGLOB    ,FAIL_INI ,ILOADP   ,FACLOAD   ,PERTURB  ,
     .             RNOISE       )
            ELSE
              CALL SCINIT3(ELBUF_TAB(NG),
     .             MS         ,IXS       ,PM        ,X           ,MSS        ,
     .             DETONATORS ,GEO       ,VEUL      ,ALE_CONNECTIVITY       ,IPARG(1,NG),
     .             DTELEM     ,SIGI      ,NEL       ,SKEW        ,IGEO       ,
     .             STIFN      ,PARTSAV   ,V         ,IPART(I15A) ,IPART      ,
     .             SIGSP      ,NSIGI     ,MSNF      ,MSSF        ,IPM        ,
     .             IUSER      ,NSIGS     ,VOLNOD    ,BVOLNOD     ,VNS        ,
     .             BNS        ,WMA       ,PTSOL     ,BUFMAT      ,MCP        ,
     .             MCPS       ,TEMP      ,NPC       ,PLD         ,MSSA       ,
     .             STRSGLOB(NF1),STRAGLOB(NF1),ORTHOGLOB ,FAIL_INI    ,ILOADP     ,
     .             FACLOAD   ,RNOISE     ,PERTURB   )
            ENDIF
          ELSEIF (JHBE == 14 .AND. 
     .           (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22)) THEN
           !HA8 thick shell
            GBUF => ELBUF_TAB(NG)%GBUF
            CALL S8CINIT3(
     .             ELBUF_TAB(NG),MS ,IXS   ,PM   ,X          ,
     .             DETONATORS,GEO       ,VEUL  ,ALE_CONNECTIVITY,IPARG(1,NG),
     .             DTELEM  ,SIGI      ,NEL   ,SKEW ,IGEO       ,
     .             STIFN   ,PARTSAV   ,V     ,IPART(I15A),MSS,
     .             IPART   ,SIGSP ,NSIGI ,MSNF  ,MSSF  ,IPM   ,
     .             IUSER   ,NSIGS   ,VOLNOD  ,BVOLNOD ,VNS     ,
     .             BNS     ,WMA     ,PTSOL   ,BUFMAT  ,MCP     ,
     .             MCPS    ,TEMP    ,NPC     ,PLD     ,XREFS   ,
     .             MSSA    ,STRSGLOB,STRSGLOB(NF1),STRAGLOB(NF1),FAIL_INI,
     .             ILOADP  ,FACLOAD ,PERTURB ,RNOISE  )
            IF (ISTOT == 1) THEN
              CALL SGSAVINI(ISOLNOD,X,IXS(1,NFT+1),GBUF%SMSTR,NEL)
            ENDIF
            IF (NSIGI > 0 ) THEN
               CALL SGSAVINIEREF(ISOLNOD,STRAGLOB(NF1),SIGSP,NSIGI,PTSOL(NF1),
     .                           GBUF%SMSTR,GBUF%OFF,NEL)
            END IF
          ELSEIF (JHBE == 14 .OR. JHBE == 222 .OR. JHBE == 17) THEN
           !HA8 and H8E solid
            GBUF => ELBUF_TAB(NG)%GBUF
            CALL S8ZINIT3(
     .           ELBUF_TAB(NG),MS        ,IXS     ,PM   ,X          ,
     .             DETONATORS,GEO       ,VEUL  ,ALE_CONNECTIVITY,IPARG(1,NG),
     .             DTELEM,SIGI      ,NEL   ,SKEW ,IGEO       ,
     .             STIFN ,PARTSAV   ,V     ,IPART(I15A),MSS,
     .             IPART ,
     .             SIGSP ,NSIGI ,MSNF  ,MSSF  ,IPM   ,
     .             IUSER   ,NSIGS   ,VOLNOD  ,BVOLNOD ,VNS     ,
     .             BNS     ,WMA     ,PTSOL   ,BUFMAT  ,MCP     ,
     .             MCPS    ,TEMP    ,NPC     ,PLD     ,XREFS   ,
     .             MSSA    ,STRSGLOB(NF1),STRAGLOB(NF1),FAIL_INI,SPBUF   ,
     .             KXSP    ,IPART(I15J) ,NOD2SP  ,SOL2SPH ,IRST,
     .             ILOADP  ,FACLOAD     ,PERTURB ,RNOISE  )
            IF (ISTOT == 1) THEN
              CALL SGSAVINI(ISOLNOD,X,IXS(1,NFT+1),GBUF%SMSTR,NEL)
            END IF
             IF (NSIGI > 0 ) THEN
                 CALL SGSAVINIEREF(ISOLNOD,STRAGLOB(NF1),SIGSP,NSIGI,PTSOL(NF1),
     .                             GBUF%SMSTR,GBUF%OFF,NEL)
             END IF
          ELSEIF (IGTYP>=29) THEN
            CALL SUINIT3(ELBUF_TAB(NG),MS    ,IXS      ,PM   ,X          ,
     .             DETONATORS  ,GEO       ,VEUL  ,ALE_CONNECTIVITY ,IPARG(1,NG),
     .             DTELEM,SIGI      ,NEL   ,SKEW       ,IGEO       ,
     .             STIFN ,PARTSAV   ,V     ,IPART(I15A),MSS,
     .             IPART ,SIGSP     , 
     .             NSIGI ,IN        ,VR    ,IPM        ,NSIGS   ,
     .             VOLNOD ,BVOLNOD  ,VNS   ,BNS        ,PTSOL   ,
     .             BUFMAT ,NPC      ,PLD   ,FAIL_INI   ,INS     ,
     .             ILOADP ,FACLOAD  ,PERTURB,RNOISE    )
          ELSE
            GBUF => ELBUF_TAB(NG)%GBUF
            IF (JMULT == 0) THEN
               CALL SINIT3(
     1              ELBUF_TAB(NG),MS         ,IXS      ,PM         ,X          ,  
     2              DETONATORS   ,GEO          ,VEUL   ,ALE_CONNECTIVITY ,IPARG(1,NG),
     3              DTELEM       ,SIGI         ,NEL    ,SKEW       ,IGEO       ,
     4              STIFN        ,PARTSAV      ,V      ,IPART(I15A),MSS        ,
     5              IPART        ,SIGSP        ,NG     ,IPARG      ,                               
     7              NSIGI        ,MSNF         ,NVC    ,MSSF       ,IPM        ,
     8              IUSER        ,NSIGS        ,VOLNOD ,BVOLNOD    ,VNS        ,
     9              BNS          ,IN           ,VR     ,INS        ,WMA        ,
     A              PTSOL        ,BUFMAT       ,MCP    ,MCPS       ,TEMP       ,
     B              XREFS        ,NPC          ,PLD    ,MSSA       ,STRSGLOB(NF1),
     C              STRAGLOB(NF1),FAIL_INI     ,SPBUF  ,KXSP       ,IPART(I15J),
     D              NOD2SP       ,SOL2SPH      ,IRST   ,ILOADP     ,FACLOAD    ,
     E              RNOISE       ,PERTURB      )        
            ELSE IF (JMULT > 0 .AND. MTN == 151) THEN
      !Multifluid law
               CALL MULTIFLUID_INIT3 (
     1              ELBUF_TAB(NG),MS         ,IXS      ,PM         ,X          ,  
     2              GEO          ,ALE_CONNECTIVITY     ,IPARG(1,NG),
     3              DTELEM       ,SIGI         ,NEL    ,SKEW       ,IGEO       ,
     4              STIFN        ,PARTSAV      ,V      ,IPART(I15A),MSS        ,
     5              IPART        ,SIGSP        ,NG     ,IPARG      ,                               
     7              NSIGI        ,MSNF         ,NVC    ,MSSF       ,IPM        ,
     8              IUSER        ,NSIGS        ,VOLNOD ,BVOLNOD    ,VNS        ,
     9              BNS          ,IN           ,VR     ,INS        ,WMA        ,
     A              PTSOL        ,BUFMAT       ,MCP    ,MCPS       ,TEMP       ,
     B              XREFS        ,NPC          ,PLD    ,MSSA       ,STRSGLOB(NF1),
     C              STRAGLOB(NF1),FAIL_INI     ,SPBUF  ,KXSP       ,IPART(I15J),
     D              NOD2SP       ,SOL2SPH      ,IRST   ,ILOADP     ,FACLOAD    , 
     E              MULTI_FVM, ERROR_THROWN,DETONATORS)    
            ENDIF
           
            IF (NPT == 1 .AND. ISTOT == 1) THEN
                CALL SGSAVINI(ISOLNOD,X,IXS(1,NFT+1),GBUF%SMSTR,NEL)
            ENDIF    
            CALL SREFSTA3(
     1        ELBUF_TAB(NG),IXS        ,PM         ,GEO      ,IPARG(1,NG),  
     2        IPM        ,IGEO       ,SKEW       ,X        ,XREFS     ,
     3        NEL          ,IPART(I15A),IPART      ,BUFMAT   ,              
     6        NPC          ,PLD        )
C
C Case total strain: conf_ref <- XREF
            IF (NXREF > 0 .AND. (NPT == 1 .AND. ISTOT == 1) ) THEN
             CALL SGSAVREF(ISOLNOD,XREFS(1,1,NFT+1),GBUF%SMSTR,NEL)
            END IF
            IF (NSIGI > 0 ) THEN
               CALL SGSAVINIEREF(ISOLNOD,STRAGLOB(NF1),SIGSP,NSIGI,PTSOL(NF1),
     .                           GBUF%SMSTR,GBUF%OFF,NEL)
            END IF
            NC1 = NVC / 128
            NC2 = (NVC-NC1*128) / 64
            NC3 = (NVC-NC1*128-NC2*64) / 32
            NC4 = (NVC-NC1*128-NC2*64-NC3*32)/16
            NC5 = (NVC-NC1*128-NC2*64-NC3*32-NC4*16)/8
            NC6 = (NVC-NC1*128-NC2*64-NC3*32-NC4*16-NC5*8)/4
            NC7 = (NVC-NC1*128-NC2*64-NC3*32-NC4*16-NC5*8-NC6*4)/2
            NC8 = (NVC-NC1*128-NC2*64-NC3*32-NC4*16-NC5*8-NC6*4-NC7*2)
            IF (NC1 == 1) NC1_OLD = 1
            IF (NC2 == 1) NC2_OLD = 1
            IF (NC3 == 1) NC3_OLD = 1
            IF (NC4 == 1) NC4_OLD = 1
            IF (NC5 == 1) NC5_OLD = 1
            IF (NC6 == 1) NC6_OLD = 1
            IF (NC7 == 1) NC7_OLD = 1
            IF (NC8 == 1) NC8_OLD = 1
            IPARG(19,NG) = NC1_OLD*128+NC2_OLD*64+NC3_OLD*32+NC4_OLD*16+NC5_OLD*8+NC6_OLD*4+NC7_OLD*2+NC8
          ENDIF
         ENDIF
        !----------------------------------------!
        !   ITY == 2                        QUAD !
        !----------------------------------------!
        ELSEIF(ITY == 2)THEN
           IHBE    =IPARG(23,NG)
           IF (JMULT == 0) THEN
              IF (IHBE == 17 .OR. (N2D == 1.AND.IHBE == 22)) THEN
                 CALL Q4INIT2(ELBUF_TAB(NG),MS    ,IXQ,PM,X,
     2                DETONATORS,GEO,VEUL,ALE_CONNECTIVITY,IPARG(1,NG),
     3                DTELEM,SIGI,IGEO   ,
     4                NEL   ,SKEW   , MSQ  ,IPART  ,IPART(I15B),
     5                IPM   ,NSIGS  ,WMA   ,PTQUAD ,BUFMAT     ,
     6                NPC   ,PLD    ,IPARG ,ILOADP ,FACLOAD    ,
     7                PARTSAV,V     )
              ELSE
                 CALL QINIT2(
     .                ELBUF_TAB(NG),MS,IXQ   ,PM   ,X       ,
     .                DETONATORS,GEO,VEUL,ALE_CONNECTIVITY,IPARG(1,NG),
     .                DTELEM,SIGI,IGEO   ,
     .                NEL   ,SKEW, MSQ, IPART, IPART(I15B),
     .                IPM     ,NSIGS  ,
     .                WMA     ,PTQUAD ,BUFMAT  ,NPC  ,PLD,
     .                IPARG   ,ILOADP ,FACLOAD ,PARTSAV,V)
              ENDIF
           ELSE  ! JMULT > 0
              IF (MTN == 20) THEN
                 CALL BINIT2(
     .                ELBUF_TAB(NG),MS        ,IXQ        ,PM          ,X  ,   
     .                DETONATORS   ,VEUL      ,ALE_CONNECTIVITY      ,IPARG(1,NG)  ,FILL,
     .                SIGI         ,BUFMAT    ,NEL        ,
     .                SKEW         ,MSQ       ,IPART      ,IPART(I15B),             
     .                GEO          ,IGEO      ,IPM        ,                         
     .                NSIGS        ,WMA       ,PTQUAD     ,NPC         ,PLD ,
     .                IPARG        ,ILOADP    ,FACLOAD    ,PARTSAV     ,V   )
              ELSE IF (MTN == 151) THEN
C 2D multifluid law
                 CALL MULTIFLUID_INIT2(NEL, NSIGS, 
     .                IPARG, IXQ, IPM, ALE_CONNECTIVITY, IGEO, IPART, IPART(I15B), NPC, 
     .                PTQUAD, ILOADP, X, PM, 
     .                GEO, SIGI, SKEW, PLD, BUFMAT, FACLOAD, ELBUF_TAB(NG), ERROR_THROWN,DETONATORS) 
              ELSE
                 CALL ARRET(2)
              ENDIF    
           ENDIF
        !----------------------------------------!
        !   ITY == 3                       SHELL !
        !----------------------------------------!
        ELSEIF (ITY == 3)THEN
          ISTRAIN =IPARG(44,NG)
          IHBE    =IPARG(23,NG)
          ITHK    =IPARG(28,NG)
          ILEV    =IPARG(45,NG)
          IXFEM   =IPARG(54,NG)
          DT2=DT2S
          IF (IHBE>10.AND.IHBE<29) THEN
            NULLIFY(ptr_ITAGE)
            IF (SITAGE>0) ptr_ITAGE=>ITAGE(1)
            CALL CBAINIT3(ELBUF_TAB(NG),
     1             IXC,PM    ,X     ,GEO        ,
     2             MS        ,IN    ,NVC   ,DTELEM,IGRSH4N    ,
     3             XREFC     ,NEL   ,ITHK  ,IHBE  ,IGRSH3N    ,
     4             THK(1+NFT),ISIGSH,SIGSH ,STIFN ,STIFR      ,
     5             PARTSAV   ,V     ,IPART(I15C)  ,MSC,INC    ,
     6             SKEW      ,I8MI  ,NSIGSH  ,IGEO   ,
     7             IPM      ,IUSER  ,ETNOD   ,NSHNOD  ,STC    ,
     8             PTSHEL   ,BUFMAT ,SH4TREE ,MCP     ,MCPC   ,
     9             TEMP     ,MS_LAYER, ZI_LAYER ,ITAG ,ITAGEL ,
     A             IPARG(1,NG),MS_LAYERC,ZI_LAYERC,PART_AREA,CPT_ELTENS,
     B             MSZ2C    ,ZPLY   ,ITAGN   ,ptr_ITAGE   ,IXFEM  ,
     C             NPC        ,PLD    ,XFEM_TAB,ISUBSTACK ,STACK    ,
     D             RNOISE     ,DRAPE  ,SH4ANG    ,GEO_STACK,
     E             IGEO_STACK ,STRC   ,PERTURB            ,IYLDINI ,ELE_AREA,
     F             NLOC_DMG   ,NG        ,GROUP_PARAM_TAB(NG))
          ELSE
            NULLIFY(ptr_ITAGE)
            IF (SITAGE>0) ptr_ITAGE => ITAGE(1)       
            CALL CINIT3(ELBUF_TAB(NG),
     1      IXC       ,PM         ,X          ,GEO        ,
     2      MS        ,IN         ,NVC        ,DTELEM     ,IGRSH4N    ,
     3      XREFC     ,NEL        ,ITHK       ,IHBE       ,IGRSH3N    ,
     4      THK(1+NFT),ISIGSH     ,SIGSH      ,STIFN      ,STIFR      ,
     5      PARTSAV   ,V          ,IPART(I15C),MSC        ,INC        ,
     8      SKEW      ,IPARG(1,NG),I8MI       ,NSIGSH     ,IGEO       ,
     9      IUSER     ,ETNOD      ,NSHNOD     ,STC        ,PTSHEL     ,
     A      IPM       ,BUFMAT     ,SH4TREE    ,MCP        ,MCPC       ,
     B      TEMP      ,CPT_ELTENS ,PART_AREA  ,ITAGN      ,ptr_ITAGE  ,
     C      IXFEM     ,NPC        ,PLD  ,XFEM_TAB,ISUBSTACK,
     D      STACK     ,RNOISE     ,DRAPE      ,SH4ANG     ,
     E      GEO_STACK,IGEO_STACK  ,STRC       ,PERTURB    ,IYLDINI    ,
     F      ELE_AREA  ,NG         ,GROUP_PARAM_TAB(NG)    ,NLOC_DMG   )
          ENDIF
          NC1 = NVC / 8
          NC2 = (NVC-NC1*8) / 4
          NC3 = (NVC-NC1*8-NC2*4) / 2
          NC4 = NVC-NC1*8-NC2*4-NC3*2
          IF (NC1 == 1) NC1_OLD = 1
          IF (NC2 == 1) NC2_OLD = 1
          IF (NC3 == 1) NC3_OLD = 1
          IF (NC4 == 1) NC4_OLD = 1
          IPARG(19,NG)=NC1_OLD*8+NC2_OLD*4+NC3_OLD*2+NC4_OLD
          DT2S=DT2
          DT2=0.     
        !----------------------------------------!
        !   ITY == 4                       TRUSS !
        !----------------------------------------!
        ELSEIF (ITY == 4) THEN
          CALL TINIT3(ELBUF_TAB(NG),
     1                IXT    ,PM         ,X        ,GEO    ,MS     ,
     2                DTELEM ,NFT        ,NEL      ,STIFN  ,PARTSAV,
     3                V      ,IPART(I15D),MST      ,STIFINT,STT    ,
     4                IGEO   ,NSIGTRUSS  ,SIGTRUSS ,PTTRUSS)
        !----------------------------------------!
        !   ITY == 5                        BEAM !
        !----------------------------------------!
        ELSEIF (ITY == 5) THEN
          CALL PINIT3(ELBUF_TAB(NG),
     1                STP        ,IXP      ,PM      ,X       ,GEO	 ,
     2                MS         ,IN       ,DTELEM  ,NFT     ,NEL  ,
     3                STIFN      ,STIFR    ,PARTSAV ,V       ,IPART(I15E),
     4                MSP        ,INP      ,IGEO    ,STIFINT ,STRP       ,
     5                NSIGBEAM   ,SIGBEAM  ,PTBEAM  ,IUSER   ,MCP        ,
     6                MCPP       ,TEMP     )
        !----------------------------------------!
        !   ITY == 6                      SPRING !
        !----------------------------------------!
        ELSEIF (ITY == 6) THEN
          IOPT = PTR_NOPT_FUN + 1
          CALL RINIT3(ELBUF_TAB(NG),
     1                IXR       ,X           ,GEO       ,MS       ,NPC        ,
     2                PLD       ,IN          ,SKEW      ,DTELEM   ,NEL        ,
     3                STIFN     ,STIFR       ,PARTSAV   ,V        ,IPART(I15F),
     4                ITAB      ,MSR         ,
     5                INR       ,STIFINT     ,STR(NFT+1),IGEO     ,SIGRS      ,
     6                NSIGRS    ,IMERGE2     ,IADMERGE2 ,MSRT(NFT+1),IXR_KJ   ,
     7                NOM_OPT(1,IOPT),STRR   ,PTSPRI    ,IPM       , PM     ,
     8                BUFMAT    ,R_SKEW)
        !----------------------------------------!
        !   ITY == 7                SH3N or TRIA !
        !----------------------------------------!
        ELSEIF(ITY == 7 .OR. ITY == 8)THEN
          ISTRAIN =IPARG(44,NG)
          ITHK    =IPARG(28,NG)
          ISH3N   =IPARG(23,NG)
          ICNOD   =IPARG(11,NG)
          IF (ISH3N == 30 .AND. ICNOD == 6) ISH3N = 0
          ILEV    =IPARG(45,NG)
          DT2=DT2S
          IF (ISH3N == 30) THEN
           CALL CDKINIT3(ELBUF_TAB(NG),GROUP_PARAM_TAB(NG),
     1          IXTG      ,PM    ,X        ,GEO        ,
     2          MS        ,IN    ,NVC      ,DTELEM,
     3          XREFTG    ,OFFSET,NEL      ,ITHK       ,THK(1+NFT+NUMELC),
     4          ISIGSH    ,SIGSH(1,KSIGSH3),STIFN,STIFR,         PARTSAV ,
     5          V         ,IPART(I15H)     ,MSTG ,INTG ,          PTG    ,
     8          SKEW     ,ISH3N  ,NSIGSH ,IGEO   ,IPM      ,
     9          IUSER    ,ETNOD  ,NSHNOD ,STTG   ,PTSH3N   ,
     A          BUFMAT   ,SH3TREE,MCP    ,MCPTG ,  TEMP ,
     B          IPARG(1,NG),CPT_ELTENS,PART_AREA   ,NPC ,PLD      , 
     C          SH3TRIM ,ISUBSTACK,STACK ,RNOISE,
     D          DRAPE(1+NUMELC),SH3ANG ,GEO_STACK,IGEO_STACK,STRTG,
     E          PERTURB,IYLDINI ,ELE_AREA(1+NUMELC),NLOC_DMG)
          ELSEIF (MTN == 151 .OR. N2D > 0) THEN
             CALL MULTIFLUID_INIT2T(ELBUF_TAB(NG), NEL, NSIGS, NVC, IPARG, IXTG, ALE_CONNECTIVITY,
     .            IGEO, IPART, IPART(I15H), IPM, PTSH3N, NPC, ILOADP, 
     .            X, PM, GEO, SIGI, SKEW, PLD, BUFMAT, FACLOAD, MULTI_FVM, ERROR_THROWN, DETONATORS)
          ELSE
           NULLIFY(ptr_ITAGE)
           IF (SITAGE > 0) ptr_ITAGE => ITAGE(NUMELC+1)  
           CALL C3INIT3(ELBUF_TAB(NG),
     1          IXTG  ,PM    ,X     ,GEO        ,IGRSH4N,
     2          MS        ,IN    ,NVC   ,DTELEM,IGRSH3N ,
     3          XREFTG    ,OFFSET,NEL   ,ITHK  ,THK(1+NFT+NUMELC),
     4          ISIGSH    ,SIGSH(1,KSIGSH3),STIFN,STIFR,PARTSAV ,
     5          V         ,IPART(I15H),MSTG,INTG  ,PTG    ,
     8          SKEW,IPARG(1,NG) , NSIGSH ,IGEO,IUSER  ,
     9          ETNOD   ,NSHNOD  ,STTG    ,PTSH3N  ,IPM      ,
     A          BUFMAT  ,SH3TREE ,MCP     ,MCPTG    , TEMP   ,
     B          CPT_ELTENS,PART_AREA,ptr_ITAGE,ITAGN,IXFEM  ,
     C          NPC     ,PLD     ,SH3TRIM ,XFEM_TAB,
     D          ISUBSTACK , STACK,RNOISE  ,
     E          DRAPE(1+NUMELC) ,SH3ANG,GEO_STACK,IGEO_STACK,STRTG,
     F          PERTURB ,ISH3N,IYLDINI ,ELE_AREA(1+NUMELC),
     G          NLOC_DMG,NG,GROUP_PARAM_TAB(NG))
          ENDIF
          NC1 = NVC / 8
          NC2 = (NVC-NC1*8) / 4
          NC3 = (NVC-NC1*8-NC2*4) / 2
          IF (NC1 == 1) NC1_OLD = 1
          IF (NC2 == 1) NC2_OLD = 1
          IF (NC3 == 1) NC3_OLD = 1
          IPARG(19,NG)=NC1_OLD*8+NC2_OLD*4+NC3_OLD*2
          DT2S=DT2
          DT2=0.                                                        
        !----------------------------------------!
        !   ITY == 51                        SPH !
        !----------------------------------------!
        ELSEIF(ITY == 51)THEN
C         SPH cells
          JSPH=1
          ISPH2SOL=IPARG(69,NG)
          CALL SPINIT3(ITY   ,SPBUF    ,KXSP     ,X     ,GEO    ,
     2             MS        ,NPC      ,PLD      ,IN    ,SKEW   ,
     3             DTELEM    ,NEL      ,STIFN    ,STIFR ,IGEO     ,
     4             PARTSAV ,V          ,IPART(I15J),BUFMAT,
     5             PM      ,ITAB       ,MSR        ,INR   ,IXSP   ,
     6                 NOD2SP  ,IPARG(1,NG),ALE_CONNECTIVITY      ,DETONATORS  ,
     7                 SIGSPH  ,ISPTAG     ,IPART      ,
     8                 IPM     ,NSIGSPH    ,PTSPH      ,NPC   ,
     9                 PLD     ,ELBUF_TAB(NG),MCP,TEMP ,ILOADP,
     A                 FACLOAD)
        !----------------------------------------------------------!
        !   ITY == 100   Pulley PID28 + User elements PID 29-30-31 !
        !----------------------------------------------------------!
        ELSEIF(ITY == 100)THEN
          IADUIX=1
          IADUX =IADUIX+MAXNX
          IADUV =IADUX +3*MAXNX
          IADUVR=IADUV +3*MAXNX
          IADUMS=IADUVR+3*MAXNX
          IADUIN=IADUMS+MAXNX
          IADUSM=IADUIN+MAXNX
          IADUSR=IADUSM+MAXNX
          IADUMV=IADUSR+MAXNX
          IADURV=IADUMV+MAXNX
          CALL XINIT3(ELBUF_TAB(NG),KXX,IXX    ,X      ,V     ,
     2             VR      ,MS         ,IN     ,
     3             SKEW    ,DTELEM     ,NEL    ,STIFN  ,STIFR ,
     4             PARTSAV ,IPART(I15I),GEO    ,
     5             ITAB ,XELEMWA(IADUIX) ,XELEMWA(IADUX) ,XELEMWA(IADUV) ,
     6             XELEMWA(IADUVR) ,XELEMWA(IADUMS) ,XELEMWA(IADUIN) ,
     7             XELEMWA(IADUSM) ,XELEMWA(IADUSR) ,XELEMWA(IADUMV) ,
     8             XELEMWA(IADURV) ,IGEO, NFT)
c
        !----------------------------------------!
        !   ITY == 101                     IGE3D !
        !----------------------------------------!
        ELSEIF (ITY == 101) THEN
            NCTRL = IPARG(75,NG)
            PX = IGEO(41,IPARG(62,NG))
            PY = IGEO(42,IPARG(62,NG))
            PZ = IGEO(43,IPARG(62,NG))
            CALL IG3DINIT3(ELBUF_TAB(NG),MS ,KXIG3D ,IXIG3D   ,PM   ,X,
     .             DETONATORS  ,GEO       ,VEUL  ,ALE_CONNECTIVITY,IPARG(1,NG),
     .             DTELEM,SIGI      ,NEL   ,SKEW ,IGEO       ,
     .             STIFN ,PARTSAV   ,V     ,IPART(I15K),MSS,
     .             IPART ,SIGSP     , 
     .             NSIGI ,IN       ,VR      ,IPM     ,NSIGS   ,
     .             VNIGE  ,BNIGE   ,PTSOL   ,
     .             BUFMAT ,NPC     ,PLD    ,FAIL_INI,NCTRL,
     .             MSIG3D ,KNOT    ,NCTRLMAX,WIGE ,PX,PY,PZ,
     .             KNOTLOCPC,KNOTLOCEL)
        ENDIF          
C
        IF (ITY == 3) THEN
          WRITE(IOUT,'(A,I10,A,I5)')' SHELL GROUP',NG, ' VECTORIZATION CODE =',IPARG(19,NG)
        ELSEIF (ITY == 7) THEN
          WRITE(IOUT,'(A,I10,A,I5)')' TRIANGULAR SHELL GROUP',NG, ' VECTORIZATION CODE =',IPARG(19,NG)
        ELSEIF (ITY == 1) THEN
          WRITE(IOUT,'(A,I10,A,I5)')' BRICK GROUP',NG,' VECTORIZATION CODE =',IPARG(19,NG)
        ENDIF
C
       ENDIF
      END DO ! End loop on element group NG
C-----------
      !loop over material initialisation done.
      !IF NRF outlet, print its automatic characteristic
       IF(M51_IFLG6==1 .AND. lSET_51_IFLG6==1)THEN
         !first initialization of group whose MAT=51 + iform=6
         WRITE (IOUT,1001)LC0MAX,SSP0MAX,TCP_REF
       ENDIF

 1001 FORMAT(
     .//
     .'     NON REFLECTING FRONTIERS (/MAT/LAW51)    '/
     .'     -------------------------------------    '/
     & 5X,'INITIALIZATION OF GLOBAL PARAMETERS      ',/    
     & 5X,'CHARACTERISTIC LENGTH. . . . . . . . . .=',E12.4/
     & 5X,'REFERENCE SOUND SPEED. . . . . . . . .  =',E12.4/
     & 5X,'CHARACTERISTIC TIME. . . . . . . . . . .=',E12.4//)

C Add error message when y < zero and N2D=1
      CALL ANCMSG(MSGID=1228,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                PRMOD=MSG_PRINT )

      USER_GRP_DOMAIN = 0
C-------------------------------------------------
C Tetrahedron : Smooth finite element formulations
C Option ITETRA==3 - set general Flag
C-------------------------------------------------
      ISFEM=0
      DO NG = 1, NGROUP
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        ISROT  = IPARG(41,NG)
        ICPRE  = IPARG(10,NG)
        IF(ITY /= 1)CYCLE
        IF(IPARG(8, NG) == 1) CYCLE
        IF(ISOLNOD /= 4 .AND. ISOLNOD /= 10) CYCLE
        IF(ISOLNOD==4.AND.ISROT == 3) ISFEM=1
        IF(ICPRE>0.AND.(ISOLNOD==10.OR.(ISOLNOD==4.AND.ISROT == 1)))
     .     ISFEM=1
      ENDDO
C--------------------------------------------
C Warning : Elements initially in tension
C--------------------------------------------
      IF (CPT_ELTENS /= 0) THEN
         CALL ANCMSG(MSGID=863,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=CPT_ELTENS)
      ENDIF
C---------------------------------------------------------------
C Additional nodal mass from added part mass : /ADMAS option
C---------------------------------------------------------------
      ADDEDMS(1:NPART) = ZERO
      IF(IMASADD > 0)THEN
        CALL SPMD_PARTSAV_PON(
     1    IXS        ,IXS10      ,IXS20      ,IXS16        ,IXQ         ,
     2    IXC        ,IXT        ,IXP        ,IXR          ,IXTG        ,
     3    IXUR       ,MSS        ,MSSX       ,MSQ          ,MSC         ,
     4    MST        ,MSP        ,MSR        ,MSTG         ,MSUR        ,
     5    INDEX      ,ITRI       ,GEO        ,PARTSAV1_PON ,IPART(I15A) ,
     6    IPART(I15B),IPART(I15C),IPART(I15D),IPART(I15E)  ,IPART(I15F) ,
     7    IPART(I15G),IPART(I15H),IPART      )
        CALL ADDMASPART(IPART,IPMAS,PARTSAV,
     .                  PART_AREA,PM,ADDEDMS,NOM_OPT(1,PTR_NOPT_ADM+1),
     .                  PARTSAV1_PON)
        CALL SPMD_MSIN_ADDMASS(
     1    IXS        ,IXS10      ,IXS20      ,IXS16      ,IXQ        ,
     2    IXC        ,IXT        ,IXP        ,IXR        ,IXTG       ,
     3    IXUR       ,MSS        ,MSSX       ,MSQ        ,MSC        ,
     4    MST        ,MSP        ,MSR        ,MSTG       ,MSUR       ,
     5    PTG        ,EVUR       ,MS         ,INDEX      ,ITRI       ,
     6    GEO        ,SH4TREE    ,SH3TREE    ,PARTSAV    ,IPMAS      ,
     7    IPART(I15A),IPART(I15B),IPART(I15C),IPART(I15D),
     8    IPART(I15E),IPART(I15F),IPART(I15G),IPART(I15H),TOTADDMAS  ,
     9    IPART      ,THK        ,PM         ,PART_AREA  ,
     A    ADDEDMS    ,ITAB       ,PARTSAV1_PON,ELE_AREA  )
      END IF
C---------------------------------------------------------------
C Parallel arithmetic : initialisation of nodal mass and inertia
C---------------------------------------------------------------
        KK1=1+NUMELS*NIXS
        KK2=KK1+NUMELS10*6
        KK3=KK2+NUMELS20*12
        CALL SPMD_MSIN(
     1               IXS     ,IXQ  ,IXC  ,IXT     ,IXP     ,
     2               IXR     ,IXTG ,IXUR ,MSS     ,MSQ     ,
     3               MSC     ,MST  ,MSP  ,MSR     ,MSTG    ,
     4               MSUR    ,INC  ,INP  ,INR     ,INTG    ,
     5               INUR    ,INDEX,ITRI ,MS      ,IN      ,
     6               PTG     ,EVUR ,GEO  ,IXS10   ,IXS20   ,
     7               IXS16   ,MSSX ,MSNF ,MSSF    ,VNS     ,
     8               VNSX    ,STC     ,STT     ,STP  ,STR  ,
     9               STTG    ,STUR    ,BNS     ,BNSX ,VOLNOD ,
     A               BVOLNOD ,ETNOD   ,STIFINT ,INS  ,MCPC  ,
     B               MCP     ,MCPS ,MCPSX   ,MCPTG,SH4TREE,
     C               SH3TREE ,MS_LAYERC, ZI_LAYERC , MS_LAYER,
     D               ZI_LAYER,MSZ2C, MSZ2,ZPLY               ,
     E               KXIG3D   ,IXIG3D  ,MSIG3D,NCTRLMAX,STRC ,
     F               STRP,STRR,STRTG,STIFINTR,NSHNOD,VNIGE,BNIGE,
     G               MCPP )
      IF(I7STIFS/=0)
     .  CALL ASSTIFI(VOLNOD,BVOLNOD,ETNOD,NSHNOD,STIFINT)
C
C--------------------------------------------
C Laser impact
C--------------------------------------------
      IF(NLASER/=0)THEN
         CALL LASER10(LAS ,XLAS ,X   ,IXQ,IPARG)
      ENDIF
C-----------------------------------------------------
C Porous elements
C Modification of volumes and normals
C-----------------------------------------------------
      DO NG=1,NGROUP
        MTN=IPARG(1,NG)
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        ITY=IPARG(5,NG)
        NPT=IPARG(6,NG)
        JALE=IPARG(7,NG)
        ISMSTR=IPARG(9,NG)
        JEUL  =IPARG(11,NG)
        JTUR  =IPARG(12,NG)
        JTHE  =IPARG(13,NG)
        JLAG  =IPARG(14,NG)
        JMULT =IPARG(20,NG)
        JPOR  =IPARG(27,NG)
        IF(ITY==1 .AND. JEUL*(1-IMULTI_FVM)/=0 .AND. N2D==0)THEN
            LFT=1
            LLT=NEL
            NFT = IPARG(3,NG)
            CALL EPORIN3(IXS,VEUL,ALE_CONNECTIVITY,GEO,NFT,NEL)
        ENDIF
      ENDDO ! next element group NG
C-----------------------------------------------------
C Option /INIVOL
C-----------------------------------------------------
      IF (NINIVOL > 0) THEN
        DO II=1,NINIVOL
          IFRAC   = INIVOL(II)%ID
          NBCONTY = INIVOL(II)%NBCONTY
          IDP     = INIVOL(II)%IPARTFILL
!
          NUMEL_TOT= MAX(NUMELTG,MAX(NUMELS,NUMELQ))
          SIPHASE = (NBSUBMAT+1)*NUMEL_TOT
          INIVOL(II)%SIPHASE = SIPHASE
          ALLOCATE(IPHASE(INIVOL(II)%SIPHASE)  ,STAT=stat)
          IPHASE = 0
          ALLOCATE(NBIP(NBSUBMAT,NUMEL_TOT)    ,STAT=stat)
          NBIP = 0
          ALLOCATE(ITAGNSOL(NUMNOD)         ,STAT=stat)
          ITAGNSOL = 0
          ALLOCATE (KNOD2SURF(NUMNOD+1)     ,STAT=stat)
          KNOD2SURF = 0
          ALLOCATE(PART_FILL(NPART)         ,STAT=stat)
          PART_FILL = 0
          ALLOCATE(IVOLSURF(NSURF) ,STAT=stat)
          IVOLSURF = 0
          ALLOCATE(ITAGSURF(NSURF) ,STAT=stat)
          ITAGSURF = 0
          ALLOCATE(SWIFTSURF(NSURF) ,STAT=stat)
          SWIFTSURF = 0
          !C------------------------- 
          !C  FILL background ale mesh with phase
          !C-------------------------
          DO NG=1,NGROUP
            MTN     = IPARG(1,NG)
            NEL     = IPARG(2,NG)
            NFT     = IPARG(3,NG)
            IAD     = IPARG(4,NG)
            ITY     = IPARG(5,NG)
            ISOLNOD = IPARG(28,NG)
            IF(ITY == 1)THEN
              IF (ISOLNOD /= 4 .AND. ISOLNOD /= 8) CYCLE
              IMAT = IXS(1,1+NFT)
              I15_=I15A
            ELSEIF(N2D>0)THEN
              IF(ITY == 7)THEN
                IMAT = IXTG(1,1+NFT)
                I15_=I15H
              ELSEIF(ITY == 2)THEN
                IMAT = IXQ(1,1+NFT)
                I15_=I15B
              ELSE
                CYCLE
              ENDIF
            ENDIF
            INVOL   = IPARG(53,NG)
            IF (MTN /= 51 .AND. MTN /= 151) CYCLE
                LFT    = 1
                LLT    = NEL
                NFT    = IPARG(3,NG)
                CALL INIFILL(IXS       ,IPM    ,NEL       ,IPART(I15_),IPHASE  ,
     .                       IDP       ,KVOL   ,BUFMAT    ,ITAGNSOL   ,ISOLNOD ,
     .                       NBIP      ,NTRACE ,PART_FILL ,NBSUBMAT   ,MTN     ,
     .                       ELBUF_TAB ,NG     ,MULTI_FVM ,IXQ        ,IXTG    ,
     .                       ITY       ,IMAT   ,SIPHASE   ,NUMEL_TOT)
C
          ENDDO ! next element group NG                                                                                        
C------------------------------------
C surface containers :                                                                                
C------------------------------------
!---
          NNOD2SURF = 0
          KNOD2SURF(1:NUMNOD+1) = 0
          NSEG_USED = 0
!---
          DO IDC=1,NBCONTY
            IDSURF   = INIVOL(II)%SURFCONTY(IDC)
            NSEGSURF = IGRSURF(IDSURF)%NSEG
            IF (IGRSURF(IDSURF)%TYPE /= 101. and. IGRSURF(IDSURF)%TYPE /= 200. and. IVOLSURF(IDSURF) == 0) THEN
              IVOLSURF(IDSURF) = 1
              CALL CONNESURF(NSEGSURF,IGRSURF(IDSURF)%NODES,KNOD2SURF,NNOD2SURF)
              NSEG_USED = NSEG_USED + NSEGSURF
            ENDIF
          ENDDO ! DO IDC=1,NBCONTY
          KNOD2SURF(1:NUMNOD+1) = 0
!---
          NSURF_INVOL = 0
          IVOLSURF(1:NSURF) = 0
          DO IDC=1,NBCONTY
            IDSURF  = INIVOL(II)%SURFCONTY(IDC)
            IF (IVOLSURF(IDSURF) == 0) THEN
               NSURF_INVOL = NSURF_INVOL + 1
               IVOLSURF(IDSURF) = NSURF_INVOL
            ENDIF
          ENDDO
!---
          ALLOCATE(NSOLTOSF(NBCONTY,NUMNOD)    ,STAT=stat)
          NSOLTOSF = 0
          ALLOCATE(INOD2SURF(NNOD2SURF*NUMNOD) ,STAT=stat)
          INOD2SURF = 0
          ALLOCATE(DIS(NSURF_INVOL,NUMNOD)      ,STAT=stat)
          !ALLOCATE(DIS(NBCONTY,NUMNOD)         ,STAT=stat)
          DIS = ZERO
                   
          ALLOCATE(NOD_NORM(3*NUMNOD)          ,STAT=stat)
          NOD_NORM(1:3*NUMNOD) = ZERO
          ALLOCATE(SEGTOSURF(NSEG_USED)        ,STAT=stat)
          SEGTOSURF(1:NSEG_USED) = 0
!---
          ITAGSURF(1:NSURF) = 0
          NSEG_SWIFT_SURF = 0
          DO IDC=1,NBCONTY
            IDSURF     = INIVOL(II)%SURFCONTY(IDC)
            JMID       = INIVOL(II)%CONTY(IDC)%IPHASE
            IFILL      = INIVOL(II)%CONTY(IDC)%IFILL
            FILL_RATIO = INIVOL(II)%CONTY(IDC)%FILL_RATIO
            FILL_RATIO = FILL_RATIO/EP9
            ICUMU      = INIVOL(II)%CONTY(IDC)%ICUMU
            NSEGSURF   = IGRSURF(IDSURF)%NSEG
            ALLOCATE(TAGN(NUMNOD)        ,STAT=stat)
            TAGN(1:NUMNOD) = 0
            IF (ITAGSURF(IDSURF) == 0) THEN
              ITAGSURF(IDSURF) = 1  !distances,node to surf, are now already calculated with IDSURF
              CALL GETPHASE(
     .        X         ,IGRSURF(IDSURF)%TYPE   ,ITAGNSOL  ,DIS      ,NSOLTOSF   ,
     .        IGRSURF(IDSURF)%ELTYP  ,KNOD2SURF ,NNOD2SURF ,INOD2SURF,TAGN       ,
     .        IDSURF    ,NSEGSURF    ,BUFSF     ,NOD_NORM  ,IGRSURF(IDSURF)%NODES,
     .        IGRSURF(IDSURF)%IAD_BUFR,IDC      ,NBCONTY   ,NSEG_SWIFT_SURF,SWIFTSURF,
     .        SEGTOSURF ,IVOLSURF,NSURF_INVOL,ITAB)
            ENDIF
            DEALLOCATE(TAGN)
          ENDDO ! DO IDC=1,NBCONTY
!-------------
          DO NG=1,NGROUP                                                                                   
!---
            MTN     = IPARG(1,NG)
            NEL     = IPARG(2,NG)
            NFT     = IPARG(3,NG)
            ITY     = IPARG(5,NG)
            ISOLNOD = IPARG(28,NG)
            INVOL   = IPARG(53,NG)
            IF (INVOL <=  0) CYCLE
            IF (MTN /= 51 .AND. MTN /= 151) CYCLE
            IF(N2D ==0 .AND. ITY /= 1)THEN
              CYCLE
            ELSEIF(N2D>0 .AND. ITY /= 7 .AND. ITY/= 2)THEN
              CYCLE
            ENDIF
!---
! loop over containers
!---
              ALLOCATE(INPHASE(NTRACE,NEL) ,STAT=stat)
              INPHASE(1:NTRACE,1:NEL) = 1
              NUMEL_TOT= MAX(NUMELTG,MAX(NUMELS,NUMELQ))
              DO IDC=1,NBCONTY
                IDSURF     = INIVOL(II)%SURFCONTY(IDC)
                JMID       = INIVOL(II)%CONTY(IDC)%IPHASE
                IFILL      = INIVOL(II)%CONTY(IDC)%IFILL
                FILL_RATIO = INIVOL(II)%CONTY(IDC)%FILL_RATIO
                FILL_RATIO = FILL_RATIO/EP9
                ICUMU      = INIVOL(II)%CONTY(IDC)%ICUMU
                NSEGSURF   = IGRSURF(IDSURF)%NSEG
C                                                                          
                  LFT      = 1 
                  LLT      = NEL
                  NFT      = IPARG(3,NG)
                  IF(ITY == 1)THEN
                    I15_=I15A
                  ELSEIF(N2D>0)THEN
                    IF(ITY == 7)THEN
                      I15_=I15H
                    ELSEIF(ITY == 2)THEN
                      I15_=I15B
                    ELSE
                      I15_=0
                   ENDIF
                  ENDIF                                                                                
                  CALL INISOLDIST(
     1              IFILL      ,IXS       ,PM         ,X        ,GEO     ,
     2              IPARG(1,NG),IDP       ,IPART(I15_),IPM      ,XREFS   ,
     4              NTRACE     ,NTRACE0   ,DIS        ,NSOLTOSF ,NBIP    ,
     5              NNOD2SURF  ,INOD2SURF ,KNOD2SURF,IGRSURF(IDSURF)%ELTYP,IGRSURF(IDSURF)%NODES,
     6              JMID       ,IPHASE    ,INPHASE    ,KVOL     ,IGRSURF(IDSURF)%TYPE,
     7              IGRSURF(IDSURF)%IAD_BUFR,BUFSF    ,NOD_NORM ,ISOLNOD ,NBSUBMAT,
     8              FILL_RATIO ,ICUMU      ,IDC       ,NBCONTY  ,NSEGSURF,
     9              IDSURF     ,SWIFTSURF  ,SEGTOSURF ,IGRSURF  ,IVOLSURF,
     A              NSURF_INVOL,IXQ,IXTG,ITY,NEL,NUMEL_TOT,ITAB) 
!
                    MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
                    NUVAR =  ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
                    NF1   =  NFT+1 
                  IF (IDC==NBCONTY) CALL INIVOL_SET(
     1                              MBUF%VAR, NUVAR,  NEL, KVOL(1,NF1),MTN ,
     2                              ELBUF_TAB,NG,NBSUBMAT,MULTI_FVM, 
     3                              IXS,IXQ,IXTG,IDP,IPART(I15_)
     .                           )
!
              ENDDO ! DO IDC=1,NBCONTY
              DEALLOCATE(INPHASE)
          ENDDO ! next element group NG                                                                                       
!---
          IF(ALLOCATED(IPHASE))   DEALLOCATE(IPHASE)
          IF(ALLOCATED(NBIP))     DEALLOCATE(NBIP)
          IF(ALLOCATED(ITAGNSOL)) DEALLOCATE(ITAGNSOL)
          IF(ALLOCATED(KNOD2SURF))DEALLOCATE(KNOD2SURF)
          IF(ALLOCATED(PART_FILL))DEALLOCATE(PART_FILL)
          IF(ALLOCATED(IVOLSURF)) DEALLOCATE(IVOLSURF)
          IF(ALLOCATED(SWIFTSURF))DEALLOCATE(SWIFTSURF)
          IF(ALLOCATED(NSOLTOSF)) DEALLOCATE(NSOLTOSF)
          IF(ALLOCATED(INOD2SURF))DEALLOCATE(INOD2SURF)
          IF(ALLOCATED(DIS))      DEALLOCATE(DIS)
          IF(ALLOCATED(NOD_NORM)) DEALLOCATE(NOD_NORM)
          IF(ALLOCATED(SEGTOSURF))DEALLOCATE(SEGTOSURF)
!---
        ENDDO ! next II=1,NINIVOL
      ELSE
        ALLOCATE(IPHASE(0),NBIP(0,0),ITAGNSOL(0),KNOD2SURF(0),PART_FILL(0),IVOLSURF(0),
     .           SWIFTSURF(0),NSOLTOSF(0,0),INOD2SURF(0),DIS(0,0),NOD_NORM(0),SEGTOSURF(0))
      ENDIF ! IF (NINIVOL > 0) 
C---------------------------------
C  Gravity (after INIVOL)
C---------------------------------
      IF (NINIGRAV>0)THEN
        NV46=4
        IF(N2D==0)NV46 = 6
        CALL INIGRAV_LOAD(
     1                    ELBUF_TAB  , IPART      ,   IGRPART,   IPARG  ,   IPART(I15H),
     2                    IPART(I15A), IPART(I15B),   X      ,   IXS    ,   IXQ,
     3                    IXTG,        PM         ,   IPM    ,   BUFMAT ,   MULTI_FVM,
     4                    ALE_CONNECTIVITY, NV46       ,   IGRSURF,   ITAB   ,   EBCS_TAB,
     5                    NPC        , PLD
     .                    )
      ENDIF

C---------------------------------------------------
C Initialization of global detonation times (Law151)
C---------------------------------------------------
      CALL MULTIFLUID_GLOBAL_TDET(IPARG,ELBUF_TAB,MULTI_FVM,IPM)

C---------------------------------
C Initialization on 1D curves
C---------------------------------
      IF (NINIMAP1D > 0) THEN
         WRITE(ISTDO, '(A)') TITRE(53)
         CALL INI_INIMAP1D(INIMAP1D    ,ELBUF_TAB ,IPART   ,IPARG   ,IPART(I15A),
     .                     IPART(I15B) ,X         ,V       ,IXS     ,IXQ        , 
     .                     IXTG        ,PM        ,IPM     ,BUFMAT  ,MULTI_FVM  ,
     .                     PLD         ,NPC       ,IGRBRIC ,IGRQUAD ,IGRSH3N    ,
     .                     NPTS)
      ENDIF
C---------------------------------
C Initialization on 2D functions
C---------------------------------
      IF (NINIMAP2D > 0) THEN
         WRITE(ISTDO, '(A)') TITRE(53)
         CALL INI_INIMAP2D(INIMAP2D    ,ELBUF_TAB ,IPART   ,IPARG   ,IPART(I15A),
     .                     IPART(I15B) ,X         ,V       ,IXS     ,IXQ        , 
     .                     IXTG        ,PM        ,IPM     ,BUFMAT  ,MULTI_FVM  ,
     .                     FUNC2D      ,IGRBRIC   ,IGRQUAD ,IGRSH3N )
      ENDIF
C---------------------------------
C Initialization of FVM velocities
C---------------------------------
      IF (MULTI_FVM%IS_USED .AND. NINVEL > 0) THEN
         CALL INI_FVMINIVEL(FVM_INIVEL ,MULTI_FVM ,IGRBRIC ,IGRQUAD ,IGRSH3N)
      ENDIF
C------------------------------------------------------------------
C SMS : Initialization of GBUF%ISMS and automatic element selection
C------------------------------------------------------------------
      IF (ISMS_SELEC >= 1) THEN
        CALL SMS_AUTO_DT(DTELEM,NATIV_SMS,
     .       IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     .       IXR     ,IXTG     ,IXS10   ,IXS16   ,IXS20     ,
     .       IPART(I15A)  ,IPART(I15B)   ,IPART(I15C)  ,IPART(I15D)  ,IPART(I15E)    ,
     .       IPART(I15F)  ,IPART(I15G)   ,IPART(I15H)  ,IPART(I15I)  ,IPART          ,
     .       IPARG        ,ELBUF_TAB     ,IGEO         ,IDDLEVEL     ,TAGPRT_SMS     )
      ENDIF
C
      IF(ILAG+IALE+IEULER == 0)THEN
        DEALLOCATE(I8MI)
        RETURN
      ENDIF
C-------------------------------------
C Initialization of rigid bodies
C-------------------------------------
      B1=ZERO
      B2=ZERO
      B3=ZERO
      B6=ZERO
      B5=ZERO
      B9=ZERO
      TOTMAS=ZERO
      XG=ZERO
      YG=ZERO
      ZG=ZERO
C
      IF(NRBYKIN>0)THEN
       RBYID=0
       DO I=1,NUMNOD
        IWA(I)=0
       ENDDO
       DO N=1,NRBYKIN
         M=NPBY(1,N)
         NSL=NPBY(2,N)
         ISPH=NPBY(5,N)
         RBYID= NPBY(6,N)
         ISENS=NPBY(4,N)
         ID=NOM_OPT(1,N)
         CALL FRETITL2(TITR,
     .                 NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
         IF(ISENS == 0)THEN
           CALL INIRBY(N     ,RBY    ,M      ,LPBY ,
     .             MS,IN     ,X      ,ITAB   ,SKEW,
     .             B1,B2     ,B3     ,B5     ,B6  ,
     .             B9,ISPH   ,TOTMAS ,XG     ,YG  ,
     .             ZG,STIFN  ,STIFR  ,NPBY   ,RBYID ,
     .             V ,VR     ,ID     ,TITR   ,ITAGND,
     .             RBY_INIAXIS)
           IWA(M)=N
         ENDIF
       ENDDO
C       
C-------------------------------------------
C Initialization of rigid bodies with sensor
C-------------------------------------------
       DO N=1,NRBYKIN
         M=NPBY(1,N)
         NSL=NPBY(2,N)
         ISPH=NPBY(5,N)
         ISENS=NPBY(4,N)
         RBYID= NPBY(6,N)
         ID=NOM_OPT(1,N)
         CALL FRETITL2(TITR,
     .                 NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
         IF(ISENS/=0)THEN
           CALL INIRBYS(N    ,RBY    ,M   ,LPBY ,
     .             MS,IN     ,X      ,ITAB   ,SKEW,
     .             B1,B2     ,B3     ,B5     ,B6  ,
     .             B9,ISPH   ,TOTMAS ,XG     ,YG  ,
     .             ZG,NPBY   ,IWA    ,V      ,VR  ,
     .             RBYID,ID  ,TITR   ,ITAGND,RBY_INIAXIS)
         ENDIF
       ENDDO
      ENDIF
C----------------------------------------------------------
C Initialization of rigid bodies using Lagrange multipliers
C----------------------------------------------------------
      IF(NRBYLAG/=0)
     .  CALL LGMINI_RBY(NPBYL  ,LPBYL  ,RBYL   ,MS     ,IN     ,
     .                  X      ,V      ,VR     ,ITAB   ,NOM_OPT)
C-------------------------------------
C Sorting of rigid bodies structures
C-------------------------------------
      IF (NRBMERGE > 0) THEN
        CALL RETRIRBY(NPBY  ,LPBY   ,
     .                RBY   ,NOM_OPT)
      ENDIF
C-------------------------------------
C Initialization of rigid materials
C-------------------------------------
      IF(IRIGID_MAT  > 0)THEN
        CALL ININODE_RM(CONNEC  ,IRIG_NODE, SLNRBM , NSLNRBM ,NRBYM ,
     .                  NGSLNRBYM,STIFN ,STIFR,RMSTIFN, RMSTIFR ,
     .                  NELEMR,NINDX )
       ENDIF
C-----------------------------------------------------------
C Verification of imposed motion to surfaces by rigid bodies
C-----------------------------------------------------------
      CALL INISRF(X,V,VR,NPBY,RBY,IGRSURF,BUFSF)
C-----------------------------------------------------
C Check for springs with stiffness but no nodal mass
C-----------------------------------------------------
      CALL RCHECKMASS(IXR      ,GEO      ,PM       ,MSR      ,INR    ,
     .                MS       ,IN       ,ITAB     ,IGEO     ,IPM    ,
     .                BUFMAT   ,IPART    ,IPART(I15F),NPBY   ,LPBY    )
C-------------------------------------
C Initialization of flexible bodies
C-------------------------------------
      IF (NFXBODY>0) THEN
C
C--      Automatic setting of fxbody from pch files
C
         CALL INI_FXBODY(FXBIPM,  FXBRPM, FXBNOD, FXBGLM,FXBCPM,   
     .                   FXBCPS,  FXBLM,  FXBFLS, FXBDLS,FXBMOD, 
     .                   ITAB, X ,MS, IN, FXB_MATRIX,
     .                   FXB_MATRIX_ADD,FXB_LAST_ADRESS,ICODE,NOM_OPT(1,PTR_NOPT_FXB+1))
C
         ALLOCATE(MBUFEL_TMP(LBUFEL), MDEPL_TMP(3*NUMNOD))
C
         NMANI=0
         DO I=1,LENVAR
            FXBDEP(I)=ZERO
            FXBVIT(I)=ZERO
            FXBACC(I)=ZERO
         ENDDO
         CALL FXBVINI(FXBIPM, FXBVIT, FXBRPM, V, VR)
         IRCS=0
         DO I=1,NFXBODY
            ALM=FXBIPM(19,I)
            ASIG=FXBIPM(20,I)
            AMOD=FXBIPM(7,I)
            ARPM=FXBIPM(14,I)
            NBNO=FXBIPM(3,I)
            NME=FXBIPM(17,I)
            NML=FXBIPM(4,I)
            NELS=FXBIPM(21,I)
            NELC=FXBIPM(22,I)
            NELT=FXBIPM(34,I)
            NELP=FXBIPM(35,I)
            NELTG=FXBIPM(23,I)
            LVSIG=NELS*7+NELC*10+NELT*2+NELP*8+NELTG*10
            IFILE=FXBIPM(29,I)
            IF (IFILE == 0) THEN
               AMOD=AMOD+NME*NBNO*6
            ELSEIF (IFILE == 1) THEN
               AMOD=AMOD+NME*FXBIPM(18,I)*6
            ENDIF
            FXBIPM(31,I)=IRCS
            CALL FXBSINI(
     .         FXBELM(ALM) , FXBSIG(ASIG), NELS, NELC,         NELTG,
     .         X           , IPARG       , PM  , FXBMOD(AMOD), NML  ,
     .         NBNO        , IXS         , IXC , IXTG        , GEO  ,
     .         FXBRPM(ARPM), I   , FXBIPM(29,I), LVSIG ,FXBIPM(18,I),
     .         NME         , IRCS, FXBIPM(30,I), NELT,         NELP ,
     .         IXT         , IXP )
C
            FXBIPM(33,I)=IRCS
            ADRRPM=FXBIPM(14,I)
            FXBRPM(ADRRPM+10)=ZERO
            FXBRPM(ADRRPM+11)=ZERO
C
C Animation output of flexible body local modes
            IF (FXBIPM(36,I) == 1) THEN
               FXBID=FXBIPM(1,I)
               ANOD=FXBIPM(6,I)
               IFILE=FXBIPM(29,I)
               IRCM=FXBIPM(30,I)
               IRCS=FXBIPM(31,I)
               NSNI=FXBIPM(18,I)
               NSN=FXBIPM(3,I)
               IRCM=IRCM+(NSN-NSNI)*FXBIPM(17,I)
               IMIN=FXBIPM(37,I)
               IMAX=FXBIPM(38,I)
C
               DO J=1,FXBIPM(4,I)
                  DO K=1,3*NUMNOD
                     MDEPL_TMP(K)=ZERO
                  ENDDO
                  DO K=1,LBUFEL
                     MBUFEL_TMP(K)=ELBUF(K)
                  ENDDO
C
                  CALL MODDEPL(
     .   FXBNOD(ANOD), FXBMOD(AMOD), MDEPL_TMP , IFILE, IRCM,
     .   NSNI,         NSN,          AMOD      )
C
                  CALL MODBUFEL(
     . FXBELM(ALM), FXBSIG(ASIG), MBUFEL_TMP, NELS,         NELC,
     . NELT,        NELP,         NELTG,      FXBRPM(ARPM), LBUFEL,
     . ASIG       , IFILE,        IRCS ,      LVSIG       )
C
                  IF (J>=IMIN.AND.J<=IMAX) THEN
                     NMANI=NMANI+1
                     FXANI(1,NMANI)=FXBID
                     FXANI(2,NMANI)=J
                     DO K=1,3*NUMNOD
                        MDEPL(K,NMANI)=MDEPL_TMP(K)
                     ENDDO
                     DO K=1,LBUFEL
                        MBUFEL(K,NMANI)=MBUFEL_TMP(K)
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
C
         DEALLOCATE(MBUFEL_TMP, MDEPL_TMP)
      ENDIF ! end flexible bodies
C-----------------------------------------------------
C Initialization and check of rigid bodies type /RBE2 :
C-----------------------------------------------------
      CALL INIRBE2(IRBE2  ,LRBE2  ,ITAB  ,X    ,MS   ,
     .             IN     ,STIFN  ,STIFR ,TOTMAS,XG  ,
     .             YG     ,ZG     ,B1    ,B2   ,B3    ,
     .             B5     ,B6     ,B9    ,     
     .             NOM_OPT(1,PTR_NOPT_RBE2+1),ITAGND)
C------------------------------------------------------
C Initialization of joint type spring (PID33 and PID45)
C------------------------------------------------------
      FLAG_KJ = 0
      DO NG=1,NGROUP
        NEL = IPARG(2,NG)
        ITY = IPARG(5,NG)
        NFT = IPARG(3,NG)
        IAD = IPARG(4,NG)
        LFT = 1
        LLT = NEL
        IF (ITY == 6) THEN
          IPROP=IXR(1,1+NFT)
          IGTYP =  NINT(GEO(NPROPG*(IPROP-1)+12))	 	  
          GBUF => ELBUF_TAB(NG)%GBUF
          IF (IGTYP==33) THEN 
            NUVAR =  NINT(GEO(NPROPG*(IPROP-1)+25))
            CALL RINI33_RB(NEL,NUVAR,IPROP,IXR,NPBY,
     .                     LPBY,RBY,STIFR,GBUF%VAR,ITAB,
     .                     IGEO(1,IPROP),IXR_KJ,GBUF%MASS)
          ELSEIF (IGTYP==45) THEN
            IF (FLAG_KJ==0) WRITE(IOUT,1500)
            FLAG_KJ = 1
            NUVAR =  NINT(GEO(NPROPG*(IPROP-1)+25))
            CALL RINI45_RB(NEL,NUVAR,IPROP,IXR,NPBY,
     .                     LPBY,RBY,STIFR,GBUF%VAR,ITAB,
     .                     IGEO(1,IPROP),IXR_KJ,GBUF%MASS,MS,IN)
          ENDIF
        ENDIF
      ENDDO
C
C----------------------------------------------------------------
      IF(IPRI>=2) THEN
         WRITE(IOUT,1000)
         WRITE(IOUT,'(5(I10,1X,1PG20.13))') (ITAB(I),MS(I),I=1,NUMNOD)
        IF(ITHERM_FE > 0) THEN
         WRITE(IOUT,1600)
         WRITE(IOUT,'(5(I10,1X,1PG20.13))') (ITAB(I),TEMP(I),I=1,NUMNOD)
         WRITE(IOUT,1700)
         WRITE(IOUT,'(5(I10,1X,1PG20.13))') (ITAB(I),MCP(I),I=1,NUMNOD)
        ENDIF
      ENDIF
C-------------------------------------
C Mass and Inertia by parts
C-------------------------------------
      CALL OUTPART(PARTSAV,IPART,NPART)
C-------------------------------------
C     INFO supp in PROP&MAT / PARTS
C-------------------------------------
      CALL OUTPART5(GROUP_PARAM_TAB,IPART,IPARG,IGEO,GEO ,PM )
C-------------------------------------
C Mass and inertia parallel arithmetic
C-------------------------------------
      IF(IPARI0 == 3)THEN
        DO N=1,NUMNOD
            MS(N) = MS(N) +
     .              I8MI(1,N) + r8_deuxm43 * (
     .              I8MI(2,N) + r8_deuxm43 * I8MI(3,N))
        ENDDO
        IF(IRODDL/=0)THEN
          DO N=1,NUMNOD
            IN(N) = IN(N) +
     .              I8MI(4,N) + r8_deuxm43 * (
     .              I8MI(5,N) + r8_deuxm43 * I8MI(6,N))
          ENDDO
        ENDIF
      ENDIF
C-------------------------------------
C Total mass and total inertia
C-------------------------------------
      IF (NS10E >0) THEN
      DO N=1,NUMNOD
       IF (ITAGND(N)/=0) CYCLE
       NN3=3*N
       NN2=NN3-1
       NN1=NN2-1
       TOTMAS=TOTMAS+MS(N)
       XG=XG+MS(N)*X(NN1)
       YG=YG+MS(N)*X(NN2)
       ZG=ZG+MS(N)*X(NN3)
c
       XX=(X(NN1))**2
       YY=(X(NN2))**2
       ZZ=(X(NN3))**2
       XY=(X(NN1))*(X(NN2))
       XZ=(X(NN1))*(X(NN3))
       YZ=(X(NN2))*(X(NN3))
C
       B1=B1+(YY+ZZ)*MS(N)
       B5=B5+(XX+ZZ)*MS(N)
       B9=B9+(XX+YY)*MS(N)
       B2=B2-XY*MS(N)
       B6=B6-YZ*MS(N)
       B3=B3-XZ*MS(N)
      ENDDO
      ELSE
      DO N=1,NUMNOD
       NN3=3*N
       NN2=NN3-1
       NN1=NN2-1
       TOTMAS=TOTMAS+MS(N)
       XG=XG+MS(N)*X(NN1)
       YG=YG+MS(N)*X(NN2)
       ZG=ZG+MS(N)*X(NN3)
c
       XX=(X(NN1))**2
       YY=(X(NN2))**2
       ZZ=(X(NN3))**2
       XY=(X(NN1))*(X(NN2))
       XZ=(X(NN1))*(X(NN3))
       YZ=(X(NN2))*(X(NN3))
C
       B1=B1+(YY+ZZ)*MS(N)
       B5=B5+(XX+ZZ)*MS(N)
       B9=B9+(XX+YY)*MS(N)
       B2=B2-XY*MS(N)
       B6=B6-YZ*MS(N)
       B3=B3-XZ*MS(N)
      ENDDO
      END IF
C
      IF(IRODDL/=0)THEN
       DO N=1,NUMNOD
        B1=B1+IN(N)
        B5=B5+IN(N)
        B9=B9+IN(N)
       ENDDO
      ENDIF
C-----  substraction of middle node S10+Itet=2     
      XG=XG/MAX(TOTMAS,EM20)
      YG=YG/MAX(TOTMAS,EM20)
      ZG=ZG/MAX(TOTMAS,EM20)
      WRITE(IOUT,1100)
      WRITE(IOUT,'(5X,1PG20.13,3(1X,G20.13))')
     . TOTMAS,XG,YG,ZG
C
      XX=XG**2
      YY=YG**2
      ZZ=ZG**2
      XY=XG*YG
      XZ=XG*ZG
      YZ=YG*ZG
C
      B1=B1-(YY+ZZ)*TOTMAS
      B5=B5-(XX+ZZ)*TOTMAS
      B9=B9-(XX+YY)*TOTMAS
      B2=B2+XY*TOTMAS
      B6=B6+YZ*TOTMAS
      B3=B3+XZ*TOTMAS
      WRITE(IOUT,1200)
      WRITE(IOUT,'(4X,3(1X,1PG20.13),3(1X,G20.13))')
     . B1,B5,B9,B2,B6,B3
C
C Print out the total additional nonstructural nodal mass
C
      WRITE(IOUT,'(//)')
      WRITE(IOUT,1300)
      WRITE(IOUT,1400) TOTADDMAS
C
C-----------------------------------------------------------------------------------
C     Initialization of non-local variable regularization structure for damage models
C-----------------------------------------------------------------------------------
      CALL NLOC_DMG_INIT(ELBUF_TAB,NLOC_DMG ,IPARG    ,IXC      ,
     .                   IXS      ,IXTG     ,ELE_AREA ,DTELEM   ,
     .                   NUMEL    ,IPM      ,X        ,XREFS    ,
     .                   XREFC    ,XREFTG   ,BUFMAT   ,PM       )
c
c-----------------------------------------------------------------------------------

      IF(ITHERM_FE > 0 ) THEN
        DEALLOCATE(MCPS,MCPP)
        IF(NUMELS10 > 0.OR.NUMELS16 > 0 .OR.NUMELS20 > 0)
     .    DEALLOCATE(MCPSX)
      ENDIF
C
      DEALLOCATE (PARTSAV)

      DEALLOCATE(MS_LAYERC,ZI_LAYERC,MSZ2C,ZPLY)
      DEALLOCATE (PARTSAV1_PON)
C
      DEALLOCATE(CONNEC,IRIG_NODE)
      IF(ALLOCATED(PART_AREA))DEALLOCATE(PART_AREA)
      DEALLOCATE(I8MI)
      IF(ALLOCATED(VPRELOAD)) DEALLOCATE (VPRELOAD)
      IF(ALLOCATED(ELE_AREA))DEALLOCATE(ELE_AREA)
c-----------      
      RETURN
c-----------      
 1000 FORMAT(//
     . 5X,'NODAL MASSES',/
     . 5X,'------------',/
     . 5X,' NODE MASS',22X,'NODE MASS',22X,'NODE MASS',22X,'NODE MASS',
     .22X,'NODE MASS'/)
 1100 FORMAT(//
     . 5X,'TOTAL MASS AND MASS CENTER',/
     . 5X,'--------------------------',/
     . 5X,'                MASS',20X,'X',20X,'Y',20X,'Z'/)
 1200 FORMAT(//
     . 5X,'TOTAL INERTIA',/
     . 5X,'-------------',/
     .22X,'IXX',18X,'IYY',18X,'IZZ',18X,'IXY',18X,'IYZ',18X,'IZX')
 1300 FORMAT(
     .       5X,' ADDED NODAL NON-STRUCTURAL MASSES '   /
     .       5X,'-----------------------------------'   //)
 1400 FORMAT(//1X,' TOTAL ADDED MASS = ',1PG20.13//)
 1500 FORMAT(//
     . 5X,'KJOINT2 SPRING DEFINITION',/
     . 5X,'------------------------'/)
 1600 FORMAT(//
     . 5X,'INITIAL NODAL TEMPERATURES',/
     . 5X,'--------------------------',/
     . 6X,5('NODE TEMPERATURE',15X),'NODE TEMPERATURE'/)
 1700 FORMAT(//
     . 5X,'INITIAL NODAL MCP         ',/
     . 5X,'--------------------------',/
     . 6X,5('NODE MCP        ',15X),'NODE MCP        '/)
      RETURN
      END
C
Chd|====================================================================
Chd|  CHEKMP2                       source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHEKMP2(NUMEL ,IPART   ,IPARTEL ,IX      ,NIX     ,
     1                  NE     ,EMAT    ,EPID    ,IPM      ,IGEO    ,
     2                  ELEM   )
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEL,NIX,NE
      INTEGER IPART(LIPART1,*),IPARTEL(*),IX(NIX,*),EMAT(0:*),EPID(0:*),
     . IGEO(NPROPGI,*), IPM(NPROPMI,*)
C     REAL
c      my_real
c     .   PM(NPROPM,*)
      CHARACTER *(*) ELEM
      CHARACTER*nchartitle,
     .   TITR2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MT, IG, IPRT
C
      IF(ELEM == 'SPRING')THEN
       DO I=1,NUMEL
        IPRT=IPARTEL(I)
        IG  =IPART(2,IPRT)
        IF(IG<=0)THEN
C
C         WRITE(IOUT,*)' **ERROR INVALID PROPERTY NUMBER',IG
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
           CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(EPID(IGEO(11,IG)) == 0)THEN
C         WRITE(IOUT,*)' **ERROR INVALID PROPERTY TYPE',IGEO(11,IG)
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
           CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
           CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ENDIF
       ENDDO
       CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)

      ELSEIF(ELEM == 'BRICK'.OR.ELEM == 'QUAD')THEN
       DO I=1,NUMEL
        IPRT=IPARTEL(I)
        MT  =IPART(1,IPRT)
        IG  =IPART(2,IPRT)
        IF(MT<=0)THEN
C         WRITE(IOUT,*)' **ERROR INVALID MATERIAL NUMBER',MT
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
           CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=MT,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(EMAT(IPM(2,MT)) == 0)THEN
C         WRITE(IOUT,*)' **ERROR INVALID MATERIAL LAW',NINT(PM(19,MT))
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
         CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MT),LTITR)
         CALL ANCMSG(MSGID=62,
     .  	       MSGTYPE=MSGERROR,
     .  	       ANMODE=ANINFO_BLIND_2,
     .  	       I1=IPM(1,MT),
     .  	       C1=TITR2,
     .  	       I2=IPM(2,MT),
     .  	       C2=ELEM,
     .  	       I3=IX(NE,I))
        ENDIF
        IF (IG<=0) THEN
           CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(IG/=0.AND.EPID(IGEO(11,IG)) == 0)THEN
           CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
           CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ENDIF
       ENDDO
       CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
C
      ELSEIF(ELEM == 'SPHCEL')THEN
       DO I=1,NUMEL
        IPRT=IPARTEL(I)
        MT  =IPART(1,IPRT)
        IG  =IPART(2,IPRT)
        IF(MT<=0)THEN
C         IERR = IERR + 1
C         WRITE(IOUT,*)' **ERROR INVALID MATERIAL NUMBER',MT
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
           CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=MT,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(EMAT(IPM(2,MT)) == 0)THEN
C         IERR = IERR + 1
C         WRITE(IOUT,*)' **ERROR INVALID MATERIAL LAW',NINT(PM(19,MT))
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
           CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MT),LTITR)
           CALL ANCMSG(MSGID=62,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IPM(1,MT),
     .                 C1=TITR2,
     .                 I2=IPM(2,MT),
     .                 C2=ELEM,
     .                 I3=IX(NE,I))
        ENDIF
        IF(IG/=0.AND.EPID(IGEO(11,IG)) == 0)THEN
C         IERR = IERR + 1
C         WRITE(IOUT,*)' **ERROR INVALID PROPERTY TYPE',IGEO(11,IG)
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
           CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
           CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ENDIF
       ENDDO
       CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
      ELSE
         DO I=1,NUMEL
            IPRT=IPARTEL(I)
            MT  =IPART(1,IPRT)
            IG  =IPART(2,IPRT)
            IF(MT<=0)THEN
C
C         WRITE(IOUT,*)' **ERROR INVALID MATERIAL NUMBER',MT
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
               CALL ANCMSG(MSGID=61,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=MT,
     .                     C1=ELEM,
     .                     I2=IX(NE,I),
     .                     PRMOD=MSG_CUMU)
            ELSEIF(EMAT(IPM(2,MT)) == 0)THEN
C
C         WRITE(IOUT,*)' **ERROR INVALID MATERIAL LAW',NINT(PM(19,MT))
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
               CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MT),LTITR)
               CALL ANCMSG(MSGID=62,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=IPM(1,MT),
     .                     C1=TITR2,
     .                     I2=IPM(2,MT),
     .                     C2=ELEM,
     .                     I3=IX(NE,I))
            ENDIF
            IF(IG<=0)THEN
C
C         WRITE(IOUT,*)' **ERROR INVALID PROPERTY NUMBER',IG
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
             CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
            ELSEIF(EPID(IGEO(11,IG)) == 0)THEN
C
C         WRITE(IOUT,*)' **ERROR INVALID PROPERTY TYPE',IGEO(11,IG)
C         WRITE(IOUT,*)'         FOR ',ELEM,IX(NE,I)
               CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
               CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
            ENDIF
         ENDDO
         CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
         CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
         CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHECKMP                       source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHECKMP(NUMEL,IX,NIX,NG,NE,EMAT,EPID,IPM,IGEO,ELEM,IPARTEL)
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEL,NIX,NG,NE,CPT
      INTEGER IX(NIX,*),EMAT(0:*),EPID(0:*), IGEO(NPROPGI,*),IPM(NPROPMI,*),IPARTEL(*)
      CHARACTER *(*) ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MT, IG
      CHARACTER*nchartitle,
     .   TITR,TITR2
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
      IF(ELEM=='SHELL3N' .AND. N2D>0)RETURN
      IF(ELEM=='TRIA'    .AND. N2D==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(ELEM == 'SPRING')THEN
       DO I=1,NUMEL
        IG=IX(NG,I)
        IF(IG<=0)THEN
           CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(EPID(IGEO(11,IG)) == 0)THEN
           CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
           CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ENDIF
       ENDDO
       CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
      ELSEIF(ELEM == 'BRICK'.OR.ELEM == 'QUAD'.OR.ELEM == 'TRIA')THEN
       DO I=1,NUMEL
        MT=IX(1,I)
        IG=IX(NG,I)
        IF(MT<=0)THEN
           CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=MT,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(EMAT(IPM(2,MT)) == 0)THEN
           CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MT),LTITR)
           CALL ANCMSG(MSGID=62,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=IPM(1,MT),
     .                 C1=TITR2,
     .                 I2=IPM(2,MT),
     .                 C2=ELEM,
     .                 I3=IX(NE,I))
        ENDIF
        IF (IG<=0) THEN
           CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(IG/=0) THEN
          IF (EPID(IGEO(11,IG)) == 0)THEN
           CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
           CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
          ENDIF
        ENDIF
       ENDDO
       CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)

      ELSEIF(ELEM == 'SPHCEL')THEN
       DO I=1,NUMEL
        MT=IX(1,I)
        IG=IX(NG,I)
        IF(MT<=0)THEN
           CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=MT,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ELSEIF(EMAT(IPM(2,MT)) == 0)THEN
           CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MT),LTITR)
           CALL ANCMSG(MSGID=62,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=IPM(1,MT),
     .                 C1=TITR2,
     .                 I2=IX(NE,I),
     .                 C2=ELEM,
     .                 I3=IX(NE,I))
        ENDIF
        IF(IG/=0.AND.EPID(IGEO(11,IG)) == 0)THEN
           CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
           CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
        ENDIF
       ENDDO
       CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
       ELSEIF(ELEM == 'BEAM')THEN
         DO I=1,NUMEL
            MT=IX(1,I)
            IG=IX(NG,I)
c            IGTYP=IGEO(11,IG)
            IF(MT<=0)THEN
               CALL ANCMSG(MSGID=61,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=MT,
     .                     C1=ELEM,
     .                     I2=IX(NE,I),
     .                     PRMOD=MSG_CUMU)
            ELSEIF(EMAT(IPM(2,MT)) == 0)THEN
               CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MT),LTITR)
               CALL ANCMSG(MSGID=62,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=IPM(1,MT),
     .                     C1=TITR2,
     .                     I2=IPM(2,MT),
     .                     C2=ELEM,
     .                     I3=IX(NE,I))
            ENDIF
            IF(IG<=0)THEN
               CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
            ELSEIF(EPID(IGEO(11,IG)) == 0)THEN
               CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
               CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
            ENDIF

            IF((IGEO(11,IG) == 3.AND.IPM(2,MT) == 36).OR.
     .         (IGEO(11,IG) == 18.AND.IPM(2,MT) == 1)) THEN
               CALL FRETITL2(TITR,
     .                       IGEO(NPROPGI-LTITR+1,IG),LTITR)
               CALL ANCMSG(MSGID=745,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=IX(NE,I),
     .                     C1=TITR,
     .                     I2=IGEO(1,IG),
     .                     I3=IPM(2,MT))
            ENDIF
         ENDDO
         CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
         CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
         CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
      ELSE
         DO I=1,NUMEL
            MT=IX(1,I)
            IG=IX(NG,I)
            
            IF(IPARTEL(I) == 0)THEN
               CALL ANCMSG(MSGID=1125,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND,
     .                 C1=ELEM,
     .                 I1=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
            ELSEIF(MT<=0)THEN
               CALL ANCMSG(MSGID=61,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=MT,
     .                     C1=ELEM,
     .                     I2=IX(NE,I),
     .                     PRMOD=MSG_CUMU)
            ELSEIF(EMAT(IPM(2,MT)) == 0)THEN
               CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,MT),LTITR)
               CALL ANCMSG(MSGID=62,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=IPM(1,MT),
     .                     C1=TITR2,
     .                     I2=IPM(2,MT),
     .                     C2=ELEM,
     .                     I3=IX(NE,I))
            ENDIF
            IF(IPARTEL(I) == 0)THEN
              CONTINUE
            ELSEIF(IG<=0)THEN
               CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=ELEM,
     .                 I2=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
            ELSEIF(EPID(IGEO(11,IG)) == 0)THEN
               CALL FRETITL2(TITR2,IGEO(NPROPGI-LTITR+1,IG),LTITR)
               CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IGEO(1,IG),
     .                 C1=ELEM,
     .                 I2=IGEO(11,IG),
     .                 I3=IX(NE,I),
     .                 PRMOD=MSG_CUMU)
            ENDIF
            IF((IGEO(11,IG) == 9).AND.(IPM(2,MT) == 25).AND.
     .       (IPM(10,MT) == 1)) THEN
               CALL ANCMSG(MSGID=561,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     C1=ELEM,
     .                     I1=IX(NE,I))
            ENDIF
         ENDDO
         CALL ANCMSG(MSGID=59,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
         CALL ANCMSG(MSGID=60,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
         CALL ANCMSG(MSGID=61,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
         CALL ANCMSG(MSGID=1125,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND,
     .                 C1=ELEM,
     .                 PRMOD=MSG_PRINT)
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  OUTPART                       source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|====================================================================
      SUBROUTINE OUTPART(PARTSAV,IPART,NPART)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPART,IPART(LIPART1,*)
      my_real
     .   PARTSAV(20,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   MAS,SM,XX,YY,ZZ,XY,YZ,ZX,XG,YG,ZG,
     .   IXX,IXY,IYY,IYZ,IZZ,IZX,EK,VX,VY,VZ
      CHARACTER TEXT*nchartitle
C======================================================================|
C
      WRITE(IOUT,'(//,A)')'PART MASS & INERTIA'
      WRITE(IOUT,'(A,/)') '-------------------'
C
      DO I=1,NPART
        MAS = PARTSAV(1,I)
        SM = 1./MAX(MAS,EM20)
        XG = PARTSAV(2,I) * SM
        YG = PARTSAV(3,I) * SM
        ZG = PARTSAV(4,I) * SM
        XX = XG*XG
        XY = XG*YG
        YY = YG*YG
        YZ = YG*ZG
        ZZ = ZG*ZG
        ZX = ZG*XG
        IXX = PARTSAV(5,I) - (YY+ZZ)*MAS
        IYY = PARTSAV(6,I) - (ZZ+XX)*MAS
        IZZ = PARTSAV(7,I) - (XX+YY)*MAS
        IXY = PARTSAV(8,I) + XY*MAS
        IYZ = PARTSAV(9,I) + YZ*MAS
        IZX = PARTSAV(10,I)+ ZX*MAS
        VX = PARTSAV(11,I) * SM
        VY = PARTSAV(12,I) * SM
        VZ = PARTSAV(13,I) * SM
        EK = PARTSAV(14,I)
        CALL FRETITL2(TEXT,IPART(LIPART1-LTITR+1,I),LTITR)
        WRITE(IOUT,'(/,A,I10,A)')'PART : ',IPART(4,I),TRIM(TEXT)
        WRITE(IOUT,'(A)')       '----'
        WRITE(IOUT,'(2A)')
     .  ' Mass        Ixx         Iyy         Izz         ',
     .  'Ixy        Iyz        Izx'
        WRITE(IOUT,'(1P4G20.13,3G20.13)')MAS,IXX,IYY,IZZ,IXY,IYZ,IZX
        WRITE(IOUT,'(2A)')
     .  ' X           Y           Z           Kin. Energy ',
     .  'Vx         Vy         Vz'
        WRITE(IOUT,'(1P4G20.13,3G20.13)')XG,YG,ZG,EK,VX,VY,VZ
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SGSAVREF                      source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGSAVREF(NPE,XREF,SAV,NEL)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE,NEL
C     REAL
      my_real
     .  XREF(8,3,*)
      DOUBLE PRECISION
     .  SAV(NEL,3*(NPE-1))
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N
C     REAL
      my_real
     .   XL(MVSIZ),YL(MVSIZ),ZL(MVSIZ)
C-----------------------------------------------
      NPE1=NPE-1
C
       DO I=LFT,LLT
        XL(I)=XREF(NPE,1,I)
        YL(I)=XREF(NPE,2,I)
        ZL(I)=XREF(NPE,3,I)
       ENDDO
       DO N=1,NPE1
        DO I=LFT,LLT
         SAV(I,N)        = XREF(N,1,I)-XL(I)
         SAV(I,N+NPE1)   = XREF(N,2,I)-YL(I)
         SAV(I,N+2*NPE1) = XREF(N,3,I)-ZL(I)
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SGSAVINIEREF                  source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGSAVINIEREF(NPE,STRAGLOB,SIGSP,NSIGI,PTSOL,SAV,OFFG,NEL)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE,NEL,STRAGLOB(*),PTSOL(*),NSIGI
C
C-------!!!!! uniforme SAV between Ismstr>=10 and Ismstr=1
      DOUBLE PRECISION
     .  SAV(NEL,3*(NPE-1))
C
      my_real
     .   SIGSP(NSIGI,*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N,JJ,IIS,IIS0,N2
C
      DOUBLE PRECISION
     .   XL(NPE),YL(NPE),ZL(NPE)
C-----------------------------------------------
      NPE1=NPE-1
C
       IIS0= NUSOLID+4+NVSOLID1 + NVSOLID2 + NVSOLID3 + NVSOLID4 + NVSOLID5
       DO I=LFT,LLT
        JJ=PTSOL(I)
        IF(STRAGLOB(I) == 10.AND.JJ>0) THEN  		       
          DO N=1,NPE
            IIS= IIS0 + (N-1)*3
            XL(N)=SIGSP(IIS+1,JJ)			       
            YL(N)=SIGSP(IIS+2,JJ)			       
            ZL(N)=SIGSP(IIS+3,JJ)
          END DO
          IF (ISMSTR==1) THEN
            DO N=1,NPE1
              N2 = 3*(N -1) +1
              SAV(I,N2)   = XL(N)-XL(NPE)
              SAV(I,N2+1) = YL(N)-YL(NPE)
              SAV(I,N2+2) = ZL(N)-ZL(NPE)
            END DO
            OFFG(I) =TWO
          ELSE            
            DO N=1,NPE1
              SAV(I,N)        = XL(N)-XL(NPE)
              SAV(I,N+NPE1)   = YL(N)-YL(NPE)
              SAV(I,N+2*NPE1) = ZL(N)-ZL(NPE)
            END DO
          END IF
        ENDIF          
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SGSAVINIEREFQ                 source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGSAVINIEREFQ(NPE,STRAGLOB,SIGSP,NSIGI,PTSOL,SAV,OFFG,
     .                         IXS,DR,NDR,NEL)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE,NEL,STRAGLOB(*),PTSOL(*),NSIGI,NDR
      INTEGER IXS(NIXS,NEL)
C     REAL
C-------dim different for quadratic element(historic)
      DOUBLE PRECISION
     .  SAV(NEL,3*NPE)
C     REAL
      my_real
     .   SIGSP(NSIGI,*),OFFG(*),DR(SDR)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N,JJ,IIS,IIS0,NN,NC(NEL,4)
C     REAL
C-----------------------------------------------
       IIS0= NUSOLID+4+NVSOLID1 + NVSOLID2 + NVSOLID3 + NVSOLID4 + NVSOLID5
       IF (NDR>0) THEN
         DO I=1,NEL
           NC(I,1) =IXS(2,I)
           NC(I,2) =IXS(4,I)
           NC(I,3) =IXS(7,I)
           NC(I,4) =IXS(6,I)
         END DO
       END IF
       DO I=LFT,LLT
        JJ=PTSOL(I)
        IF(STRAGLOB(I) == 10.AND.JJ>0) THEN  		       
          DO N=1,NPE
            IIS= IIS0 + (N-1)*3
            SAV(I,N)       = SIGSP(IIS+1,JJ)			       
            SAV(I,N+NPE)   = SIGSP(IIS+2,JJ)			       
            SAV(I,N+2*NPE) = SIGSP(IIS+3,JJ)
          END DO
          DO N=1,NDR
            IIS= IIS0 +(NPE+N-1)*3
            NN = 3*(NC(I,N)-1)
            DR(NN+1) = SIGSP(IIS+1,JJ)			       
            DR(NN+2) = SIGSP(IIS+2,JJ)			       
            DR(NN+3) = SIGSP(IIS+3,JJ)
          END DO
         IF (ISMSTR==1) OFFG(I) =TWO 
        ENDIF          
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  OUTPART5                      source/elements/initia/initia.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE OUTPART5(GROUP_PARAM_TAB,IPART,IPARG,IGEO, GEO ,PM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*),IPARG(NPARG,*),IGEO(NPROPGI,*)
      my_real
     .   GEO(NPROPG,*),PM(NPROPM,*)
      TYPE(GRP_PARAM_STRUCT_) , DIMENSION(NGROUP) :: GROUP_PARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG,IPID,IMID,MID,PID,ITY,IGTYP,ETY,N_NOD,ISOLNOD,
     .        IHBE,ISMSTR,ICPRE,JCVT,IINT,IHKT,ITET4,ITET10,IMATVIS,NPT,NLY,
     .        ICSTR,IDRIL,ITHK,IPLAS,I2GEO(NUMGEO),NG2,NG1,NG0,JG,ETYE,
     .        IH4,IH3,IGMAT,I2GEO1(NUMGEO),I2GEO2(NUMGEO),IHBE0
      my_real
     .   MAS,SM,XX,YY,ZZ,XY,YZ,ZX,XG,YG,ZG,
     .   IXX,IXY,IYY,IYZ,IZZ,IZX,EK,VX,VY,VZ,HM,HR,HF,DN,QA,QB,QH,
     .   NS_A,NS_B,DM,QF,QM,QR,DF
      CHARACTER(len=nchartitle) TEXT
      CHARACTER(len=23), DIMENSION(27) :: EL_TYP
      DATA EL_TYP / 'SOLID-HEXA ', 
     .    'TETRA4 ',
     .    'TETRA10 ',
     .    'S20 ',      
     .    'ELEM-USER ', 
     .    'SOLID-IGE ', 
     .    'THICK-SHELL HEXA ' ,
     .    'THICK-SHELL PENTA '  ,
     .    'THICK-SHELL S16 '    ,
     .    'THICK-SHELL S20 '    , 
     .    'SHELL-4nodes '         , 
     .    'SHELL-3nodes '         , 
     .    'QUAD-2D '            ,
     .    'TRUSS '              ,
     .    'BEAM '               ,
     .    'SPRING '             , 
     .    'SPH '                ,
     .    'SHELL-(3nodes+4nodes)'    , 
     .    'THICK-SHELL HEXA+PENTA ', 
     .    'THICK-SHELL S16+S20 ' , 
     .    'SOLID-(HEXA+TETRA4)'   , 
     .    'SOLID-(HEXA+TETRA10)'  , 
     .    'TETRA4+TETRA10 '      , 
     .    'HEXA+TETRA4+TETRA10 ' , 
     .    'MULTI-STRAND '        ,
     .    'KJOINT '              ,
     .    'N/A '  /
C======================================================================|
c        IPART(1,I)=IMID id sys
c        IPART(2,I)=IPID id sys
c        IPART(3,I)=ISID
c        IPART(4,I)=ID
c        IPART(5,I)=MID id user
c        IPART(6,I)=PID id user
c        IPART(7,I)=SID
c        IPART(8,I)=ITH
C
       ETYE =27
       I2GEO(1:NUMGEO) = 0
       I2GEO1(1:NUMGEO) = 0
       I2GEO2(1:NUMGEO) = 0
C Up to 3 different elements using same PID (tetra4,tetra10,hexa) 
C Only solid and shell have initialized surely IPARG(62,NG)  
      DO NG=1,NGROUP
       IF (IPARG(8,NG)==1.OR.IPARG(62,NG)==0) CYCLE
       IPID=IPARG(62,NG)
       IF (I2GEO(IPID)==0) THEN
        I2GEO(IPID) = NG
       ELSE
C---- check if ITY, ISOLNOD are the same  
        NG0 = I2GEO(IPID)
        IF (NG0>2*NGROUP) CYCLE
        IF (NG0>NGROUP) NG0 =-I2GEO1(IPID)
        ITY = IPARG(5,NG0)
        IGTYP= IPARG(38,NG0)
        ISOLNOD = IPARG(28,NG0)
C-------ITY : shell        
        IF (IPARG(5,NG)/= ITY) THEN     
         I2GEO(IPID) = I2GEO(IPID) + NGROUP
         IF (I2GEO1(IPID)==0.AND.I2GEO(IPID)>NGROUP) I2GEO1(IPID) = -NG
C-------ITY=1 : solid        
        ELSEIF (ITY==1.AND.IPARG(5,NG)==1) THEN     
         IF (IPARG(28,NG)/=ISOLNOD) I2GEO(IPID) = I2GEO(IPID) + NGROUP
         IF (I2GEO1(IPID)==0.AND.I2GEO(IPID)>NGROUP) I2GEO1(IPID) = -NG
         IF (I2GEO2(IPID)==0.AND.I2GEO(IPID)>2*NGROUP) I2GEO2(IPID) = -NG
C-------ITY=1 : thick-shell        
        ELSEIF (IGTYP>=20.AND.IGTYP<=22.AND.IPARG(38,NG)==IGTYP) THEN     
         IF (IPARG(28,NG)/=ISOLNOD) I2GEO(IPID) = I2GEO(IPID) + NGROUP     
         IF (I2GEO1(IPID)==0.AND.I2GEO(IPID)>NGROUP) I2GEO1(IPID) = -NG
        END IF
       END IF
      END DO
C      
      WRITE(IOUT,'(//,A)')'PART ELEMENT/MATERIAL PARAMETER REVIEW:'
      WRITE(IOUT,'(A,/)') '-----------------------'
C-------We suppose the orders of IPART/IPARG are the same
      DO I=1,NPART
        CALL FRETITL2(TEXT,IPART(LIPART1-LTITR+1,I),LTITR)
        IMID=IPART(1,I)
        IPID=IPART(2,I)
        IF(IPID == 0) CYCLE
        IGTYP= IGEO(11,IPID)
        IF(IMID == 0) CYCLE
        MID = NINT(PM(19,IMID)) 
        NPT =IGEO(4,IPID)
        IHBE0 =IGEO(10,IPID)
c        MID = IPM(2,IMID)
        NG0 = I2GEO(IPID)
        NG2=0
        NG1=0
        NG = NG0
C---- case 2 elem use same pid 1): shell 3n,4n  2): thick-shell hexa-penda 3): S16-S20      
C----                          4): solid hexa-tetra4, 5): solid hexa-tetra10,6): solid tetra4-tetra10,       
C---- case 3 elem use same pid 1): solid hexa-tetra4-tetra10 
C-----case not-considered :  thick-shell hexa-S16 hexa-S20, hexa-S16-S20    
        ETY=ETYE
        IF (NG0>2*NGROUP) THEN
         IF (I2GEO2(IPID)>0) THEN
          NG = I2GEO2(IPID)
         ELSEIF (I2GEO1(IPID)>0) THEN
          NG = I2GEO1(IPID)
          I2GEO2(IPID)= -I2GEO2(IPID)
         ELSE
          NG = NG-2*NGROUP
          I2GEO1(IPID)= -I2GEO1(IPID)
         END IF
        ELSEIF (NG0>NGROUP) THEN
         IF (I2GEO1(IPID)>0) THEN
          NG = I2GEO1(IPID)
         ELSE
          NG = NG-NGROUP
          I2GEO1(IPID)= -I2GEO1(IPID)
         END IF
        END IF
C----- Limited to 3 parts used same pid     
        IF (NG >0) THEN
         ITY = IPARG(5,NG)
         ISOLNOD = IPARG(28,NG)
c         MID = IPARG(28,NG)
         IHBE=IPARG(23,NG)
         NPT =MAX(NPT,IPARG(6,NG))
        ELSE
         ITY =0
        END IF
C------- set ele_type(ETY)
        SELECT CASE (IGTYP)
C-------thick-shell        
         CASE(20,21,22)                                             
          IF (NG0>NGROUP) THEN
           NG1 = IABS(I2GEO1(IPID))
           IF((ISOLNOD+IPARG(28,NG1))==14) THEN
            ETY = 17+2
           ELSEIF((ISOLNOD+IPARG(28,NG1))==36) THEN
            ETY = 17+3
           END IF
          ELSEIF (ISOLNOD==8) THEN
           ETY=7
          ELSEIF (ISOLNOD==6) THEN
           ETY=8
          ELSEIF (ISOLNOD==16) THEN
           ETY=9
          ELSEIF (ISOLNOD==20) THEN
           ETY=10
          ELSE
           ETY=ETYE
          END IF
         CASE(2)                                          
          ETY=14
         CASE(3,18)                                          
          ETY=15
         CASE(4,8,12,13,23,25,26,32,35,36,44,45,46)                                          
          ETY=16
          MID = -1
         CASE(28)                                          
          ETY=25
          MID = -1
         CASE(29,30,31)                                          
          ETY=5
         CASE(33)                                          
          ETY=26
          MID = -1
         CASE(34)                                          
          ETY=17
         CASE DEFAULT 
C---- solid         
          IF(ITY==1)THEN
           IF (NG0>2*NGROUP) THEN
            ETY = 17+7
           ELSEIF (NG0>NGROUP) THEN
            NG1 = IABS(I2GEO1(IPID))
            IF((ISOLNOD+IPARG(28,NG1))==12) THEN
             ETY = 17+4
            ELSEIF((ISOLNOD+IPARG(28,NG1))==18) THEN
             ETY = 17+5
            ELSEIF((ISOLNOD+IPARG(28,NG1))==14) THEN
             ETY = 17+6
            END IF
           ELSEIF (ISOLNOD==8) THEN
            ETY=1
           ELSEIF (ISOLNOD==4) THEN
            ETY=2
           ELSEIF (ISOLNOD==10) THEN
            ETY=3
           ELSEIF (ISOLNOD==20) THEN
            ETY=4
           ELSE
            ETY=ETYE
           END IF
          ELSEIF(ITY==2.AND.N2D>0)THEN
           ETY=13
C------ shell           
          ELSEIF(ITY==3.OR.ITY==7)THEN
           IF (NG0>NGROUP) THEN
            ETY = 17+1
           ELSEIF(ITY==3)THEN
            ETY=11
           ELSE
            ETY=12
           END IF
         ELSEIF(ITY==4)THEN
           ETY=14
         ELSEIF(ITY==5)THEN
          ETY=15
         ELSEIF(ITY==6)THEN
          ETY=16
         ELSEIF(ITY==51)THEN
          ETY=17
         ELSEIF(ITY==101)THEN
          ETY=6
         ELSE
          ETY=ETYE
         END IF
        END SELECT
        IF (MID>0) THEN        
         WRITE(IOUT,'(A,I10,1X,A,3X,A,I4,2A)')'Part id,name:',IPART(4,I),TEXT(1:20),'Mat type:',MID,' Elm type: ',EL_TYP(ETY)
        ELSEIF (MID==0) THEN
         WRITE(IOUT,'(A,I10,1X,A,3X,A,2A)')'Part id,name:',IPART(4,I),TEXT(1:20),'Mat type: VOID',' Elm type: ',EL_TYP(ETY)
C----- spring, KJOINT -----        
        ELSE
         WRITE(IOUT,'(A,I10,1X,A,16X,2A)')'Part id,name:',IPART(4,I),TEXT(1:20),' Elm type: ',EL_TYP(ETY)
        END IF        
        WRITE(IOUT,'(A)') '----'
C-print in fonction of elem types ITY
        SELECT CASE (ITY)
         CASE(1)
          QH =ZERO
          IF (ETY==21.OR.ETY==22.OR.ETY==24) THEN
		   IHBE=IHBE0
           NG0 = I2GEO(IPID)
           IF (NG0>2*NGROUP) THEN
            IF (I2GEO2(IPID)>0) THEN
             NG0 = I2GEO2(IPID)
		     IF (IPARG(28,NG0)==8) NG = NG0
            ELSEIF (I2GEO1(IPID)>0) THEN
             NG0 = I2GEO1(IPID)
		     IF (IPARG(28,NG0)==8) NG = NG0
            END IF
           ELSEIF (NG0>NGROUP) THEN
             NG0 = I2GEO1(IPID)
		     IF (IPARG(28,NG0)==8) NG = NG0
           ELSE
		     IF (IPARG(28,NG0)==8) NG = NG0
           END IF
          END IF		   
          IF (IHBE<=2.AND.ISOLNOD==8) QH = GEO(13,IPID)
          QA = GEO(14,IPID)
          QB = GEO(15,IPID)
C-------this is done in ENGINE, should be done in Starter          
          IF (MID==70 .AND.IGEO(31,IPID) == 1) THEN
           QA = ZERO
           QB = ZERO
          END IF
          DN = ZERO
          IF (ISOLNOD==8.AND.(IHBE==24.OR.IHBE==15)) DN = GEO(13,IPID)
          NS_A = GEO(16,IPID)
          NS_B = GEO(17,IPID)
          NPT =IPARG(6,NG)
          IINT = IPARG(36,NG)
          IF (IHBE==17.AND.IINT==2) IHBE=18 
          ISMSTR=IPARG(9,NG)
          ICPRE = IPARG(10,NG)
          IF (ICPRE==0.AND.ISOLNOD==8) ICPRE=3 
          JCVT = IPARG(37,NG)+1
          IHKT = 0
          IF (IHBE==24.AND.ISOLNOD==8) IHKT = IINT 
          IF(MID == 68)THEN
             ITET4 = 0
          ELSE
             ITET4 = IPARG(41,NG)
          ENDIF
          ITET10 = IPARG(74,NG)
          IMATVIS = IPARG(45,NG)
          IF (IGTYP>=20.AND.IGTYP<=22) THEN
           IF (IHBE==14 .OR. IHBE==16) THEN
             NLY = MOD(ABS(NPT)/10,10)
           ELSE
             NLY = NPT
           ENDIF  
           IF (ICPRE==0) ICPRE=3 
c-----          
           IF (IGTYP==22.AND.IHBE==14 ) THEN
C------  IPARG(6,NG)= NPG after elbuf_ini          
             NPT =MAX(NPT,IGEO(4,IPID))
             ICSTR = IPARG(17,NG)
             SELECT CASE (ICSTR)
              CASE(100)                                             
               NLY = ABS(NPT)/100
               IF (NLY ==0) NLY =IINT 
              CASE(10)                                          
               NLY = MOD(ABS(NPT)/10,10)
               IF (NLY ==0) NLY =IINT 
              CASE(1)                                          
               NLY = MOD(ABS(NPT),10)
               IF (NLY ==0) NLY =IINT 
             END SELECT 
            ENDIF  
           WRITE(IOUT,'(A)') '  Isolid  Ismstr   Icpre     NPT'
           WRITE(IOUT,'(4I8)')IHBE,Ismstr,ICPRE, NLY
          ELSE
c2345678+-------+-------+-------+-------+-------+-------+-------+-------+-------+  
c---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|
           WRITE(IOUT,'(A)')'  Isolid  Ismstr   Icpre  Iframe    IHKT Itetra4 Itetra10 IMATVIS'
           WRITE(IOUT,'(8I8)')IHBE,Ismstr, Icpre, JCVT,  IHKT ,Itet4, Itet10,IMATVIS
          END IF
          WRITE(IOUT,'(A)') '--      qa        qb   lamda_v      mu_v         h        dn'
          WRITE(IOUT,'(6F10.4,/)')QA,QB,NS_A,NS_B,QH,DN   
C          
         CASE(2)
C----------        
          JCVT = IPARG(37,NG)+1
          WRITE(IOUT,'(A)') '   Isolid  Iframe N2D(1:ASY;2:STR-PLANE)'
          WRITE(IOUT,'(3I8,/)')IHBE,JCVT,  N2D
c--------
         CASE(3,7)
C-------- shell        
          ISMSTR=IPARG(9,NG)
          IDRIL =IPARG(41,NG) 
          ITHK =IPARG(28,NG) 
          IPLAS =IPARG(29,NG) 
          IGMAT =IGEO(98,IPID)
          QF = ZERO
          QM = ZERO
          QR = ZERO
          DN = ZERO
C         Ishell Ismstr Ish3n Idril
C------ just consisting with manuel
          IF (IDRIL==0) IDRIL=2
          IH3= 0
          IH4= 0
          IF (NG0>NGROUP) THEN
           NG1 = IABS(I2GEO1(IPID))
C           IF(ITY==3)THEN
CC------   Ishel=2 -> 0 Ishel=3 -> 2        
C            IH4 = IPARG(23,NG) 
C            IH3 = IPARG(23,NG1) 
C           ELSE
C            IH3 = IPARG(23,NG) 
C            IH4 = IPARG(23,NG1) 
C           END IF
C           IF (IH4>4.OR.IH4==2) IH4 =IH4 +1
C           IF (IH4==0) IH4 =2
C           MIXED CASE take directly defining in pid
            IH3 = IGEO(18,IPID) 
            IH4 = IGEO(10,IPID) 
           WRITE(IOUT,'(A)') '  Ishell   Ish3n  Ismstr   Idril     NPT    ITHK   IPLAS'
           WRITE(IOUT,'(7I8)')IH4,IH3,ISMSTR,IDRIL,NPT,ITHK,IPLAS
          ELSEIF(ITY==3)THEN
            IH4 = IPARG(23,NG) 
           IF (IH4>4.OR.IH4==2) IH4 =IH4 +1
           IF (IH4==0) IH4 =2
           WRITE(IOUT,'(A)') '  Ishell  Ismstr   Idril     NPT    ITHK   IPLAS'
           WRITE(IOUT,'(6I8)')IH4,ISMSTR,IDRIL,NPT,ITHK,IPLAS           
          ELSE
            IH3 = IPARG(23,NG) 
           WRITE(IOUT,'(A)') '   Ish3n  Ismstr   Idril     NPT    ITHK   IPLAS'
           WRITE(IOUT,'(6I8)')IH3,ISMSTR,IDRIL,NPT,ITHK,IPLAS
          END IF
          IF (IH4>0.AND.IH4<=4) THEN
           QF = GEO(13,IPID)
           QM = GEO(14,IPID)
           QR = GEO(15,IPID)
          END IF
C--------verify dm in Engine         
          DM = GROUP_PARAM_TAB(NG)%VISC_DM
c          DM = GEO(16,IPID)
C--------verify dn  12, dkt...    
          IF (IH4==24) DN = GEO(13,IPID)
          IF (DN==ZERO.AND.IH4==12) THEN
            DN = EM03
          END IF
          IF (DN==ZERO.AND.IH3==30) DN = EM4
c          DN = GROUP_PARAM_TAB(NG)%VISC_DN
          WRITE(IOUT,'(A)') '--      hm        hf        hr        dm        dn'
          WRITE(IOUT,'(5F10.4,/)')QM,QF,QR,DM,DN       
c
         CASE(6)
           WRITE(IOUT,'(A,I5/)') 'Spring type:',IGTYP
c
         CASE DEFAULT 
c
          IF(IGTYP==2)THEN
C---------- truss, nothing       
            WRITE(IOUT,*) 
          ELSEIF(IGTYP==3.OR.IGTYP==18)THEN
            ISMSTR=IGEO(5,IPID)
            DM = GEO(16,IPID)
            DF = GEO(17,IPID)
            WRITE(IOUT,'(A)') '  Ismstr        dm        df'
            WRITE(IOUT,'(I8,2F10.4/)')ISMSTR,DM,DF
C----------spring w/ NG=0       
          ELSEIF(IGTYP==4.OR.IGTYP==8.OR.IGTYP==12.OR.IGTYP==13.OR.
     .           IGTYP==32.OR.IGTYP==35.OR.IGTYP==36 .OR. IGTYP == 23)THEN
           WRITE(IOUT,'(A,I5/)') 'Spring type:',IGTYP
          END IF
        END SELECT 

      ENDDO
C
      RETURN
      END

