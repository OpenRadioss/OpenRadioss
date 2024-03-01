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
Chd|  ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ST_QAPRINT_ADMESH             source/output/qaprint/st_qaprint_admesh.F
Chd|        ST_QAPRINT_ALE_OPTIONS_DRIVER source/output/qaprint/st_qaprint_ale_options_driver.F
Chd|        ST_QAPRINT_CLUSTERS           source/output/qaprint/st_qaprint_clusters.F
Chd|        ST_QAPRINT_COMPOSITE_OPTIONS  source/output/qaprint/st_qaprint_composite_options.F
Chd|        ST_QAPRINT_CONSTRAINTS        source/output/qaprint/st_qaprint_constraints.F
Chd|        ST_QAPRINT_DFS_DETONATORS     source/output/qaprint/st_qaprint_dfs_detonators.F
Chd|        ST_QAPRINT_DFS_LASERS         source/output/qaprint/st_qaprint_dfs_lasers.F
Chd|        ST_QAPRINT_EBCS               source/output/qaprint/st_qaprint_ebcs.F
Chd|        ST_QAPRINT_ELEMENT            source/output/qaprint/st_qaprint_element.F
Chd|        ST_QAPRINT_FRICTION           source/output/qaprint/st_qaprint_friction.F
Chd|        ST_QAPRINT_GENERAL_CONTROLS   source/output/qaprint/st_qaprint_general_controls.F
Chd|        ST_QAPRINT_GROUPS             source/output/qaprint/st_qaprint_groups.F
Chd|        ST_QAPRINT_INITIAL_CONDITIONS source/output/qaprint/st_qaprint_initial_conditions.F
Chd|        ST_QAPRINT_INITIAL_STATE      source/output/qaprint/st_qaprint_initial_state.F
Chd|        ST_QAPRINT_INIVOL             source/output/qaprint/st_qaprint_inivol.F
Chd|        ST_QAPRINT_INTERFACES         source/output/qaprint/st_qaprint_interfaces.F
Chd|        ST_QAPRINT_INTERNAL_GROUPS    source/output/qaprint/st_qaprint_internal_groups.F
Chd|        ST_QAPRINT_LOADS              source/output/qaprint/st_qaprint_loads.F
Chd|        ST_QAPRINT_MADYMO             source/output/qaprint/st_qaprint_madymo.F
Chd|        ST_QAPRINT_MATERIALS          source/output/qaprint/st_qaprint_materials.F
Chd|        ST_QAPRINT_MODEL_TOOLS        source/output/qaprint/st_qaprint_model_tools.F
Chd|        ST_QAPRINT_MONVOL             source/output/qaprint/st_qaprint_monvol.F
Chd|        ST_QAPRINT_MULTIDOMAINS       source/output/qaprint/st_qaprint_multidomains.F
Chd|        ST_QAPRINT_NODES              source/output/qaprint/st_qaprint_nodes.F
Chd|        ST_QAPRINT_OUTPUT_DATABASES   source/output/qaprint/st_qaprint_output_databases.F
Chd|        ST_QAPRINT_PROPERTIES         source/output/qaprint/st_qaprint_properties.F
Chd|        ST_QAPRINT_REFERENCE_STATE    source/output/qaprint/st_qaprint_reference_state.F
Chd|        ST_QAPRINT_REFSTA             source/output/qaprint/st_qaprint_refsta.F
Chd|        ST_QAPRINT_SEATBELTS          source/output/qaprint/st_qaprint_seatbelts.F
Chd|        ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|        ST_QAPRINT_SURF               source/output/qaprint/st_qaprint_surf.F
Chd|        ST_QAPRINT_TIME_HISTORIES     source/output/qaprint/st_qaprint_time_histories.F
Chd|        ST_QAPRINT_TRANSFORMATIONS    source/output/qaprint/st_qaprint_transformations.F
Chd|        ST_QAPRINT_USERWI             source/output/qaprint/st_qaprint_userwi.F
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale/ale_ebcs_mod.F
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        FUNC2D_MOD                    share/modules1/func2d_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIMAP1D_MOD                  share/modules1/inimap1d_mod.F 
Chd|        INIMAP2D_MOD                  share/modules1/inimap2d_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_DRIVER(IGEO        ,GEO        ,BUFGEO          ,IPM            ,PM               ,
     2                             BUFMAT      ,NOM_OPT    ,INOM_OPT        ,NUMLOADP       ,ILOADP           ,
     3                             LLOADP      ,LOADP      ,IBCL            ,FORC           ,IPRES            ,
     4                             PRES        ,NPBY       ,LPBY            ,RBY            ,IBCR             ,
     5                             FRADIA      ,IBCV       ,FCONV           ,IBFTEMP        ,FBFTEMP          ,
     6                             IGRV        ,LGRV       ,AGRV            ,IBFFLUX        ,FBFFLUX          ,
     7                             ITAB        ,V          ,VR              ,W              ,ICODE            ,
     8                             ISKEW       ,ICFIELD    ,LCFIELD         ,CFIELD         ,DAMPR            ,
     9                             TEMP        ,IBCSLAG    ,IPARI           ,INTBUF_TAB     ,CLUSTERS         ,
     A                             IBOX        ,IPMAS      ,IBFVEL          ,FBFVEL         ,NIMPACC          ,
     B                             LACCELM      ,ACCELM    ,NOM_SECT        ,NSTRF          ,SECBUF           ,
     C                             SKEW        ,ISKWN      ,XFRAME          ,T_MONVOL       ,T_MONVOL_METADATA,
     D                             I2RUPT      ,AREASL     ,INTBUF_FRIC_TAB ,NPFRICORTH     ,MAT_ELEM         ,
     E                             PFRICORTH   ,IREPFORTH  ,PHIFORTH        ,VFORTH         ,XREFC            ,
     F                             XREFTG      ,XREFS      ,TAGXREF         ,IXS            ,IXC              ,
     G                             IXTG        ,RWBUF      ,NPRW            ,LPRW           ,ITHVAR           ,
     H                             IPART       ,SUBSETS    ,IPARTTH         ,NTHGRPMX       ,NIMPDISP         ,
     M                             NIMPVEL     ,DETONATORS ,IBCSCYC         ,NPC            ,PLD              ,
     N                             TABLE       ,NPTS       ,IRBE3           ,LRBE3          ,FRBE3            ,
     O                             MGRBY       ,IXS10      ,ISOLNOD         ,IXR            ,R_SKEW           ,
     P                             IXP         ,IXT        ,X               ,THKE           ,SH4ANG           ,
     Q                             THKEC       ,SH3ANG     ,SET             ,LSUBMODEL      ,IGRNOD           ,
     R                             IGRPART     ,IGRBRIC    ,IGRSH4N         ,IGRSH3N        ,IGRQUAD          ,
     S                             IGRBEAM     ,IGRTRUSS   ,IGRSPRING       ,IGRSURF        ,IGRSLIN          ,
     T                             IXQ         ,ISPCOND    ,RTRANS          ,IRAND          ,ALEA             ,
     U                             XSEED       ,XLAS       ,LAS             ,IRBE2          ,LRBE2            ,
     V                             KXSP        ,IPARTSP    ,DRAPE           ,IXR_KJ         ,IACTIV           ,
     W                             FACTIV      ,UNITAB     ,NPBYL           ,LPBYL          ,RBYL             ,
     X                             XYZREF      ,SENSORS    ,FUNC2D          ,
     Y                             INICRACK    ,IPRELOAD   ,PRELOAD         ,IFLAG_BPRELOAD ,IBMPC,
     Z                             IBMPC2      ,IBMPC3     ,IBMPC4          ,RBMPC          ,LJOINT           ,
     A                             NNLINK      ,LNLINK     ,BUFSF           ,SBUFSF_        ,PM_STACK         ,
     B                             GEO_STACK   ,IGEO_STACK ,IPARG           ,IPADMESH       ,PADMESH          ,
     C                             LIFLOW      , LRFLOW    ,IFLOW           ,RFLOW          ,
     D                             SH4TREE     ,SH3TREE    ,SH4TRIM         ,SH3TRIM        ,QP_IPERTURB      ,
     E                             QP_RPERTURB ,LLINAL     ,LINALE          ,FVM_INIVEL     ,GJBUFI           ,
     F                             GJBUFR      ,MS         ,IN              ,LGAUGE         ,GAUGE            ,
     G                             KXX         ,IXX        ,IPARTX          ,IXRI           ,IXS16            ,
     H                             IEXMAD      ,FXBIPM     ,FXBFILE_TAB     ,EIGIPM         ,EIGRPM           ,
     I                             ISPHIO      ,VSPHIO     ,EBCS_TAB        ,INIMAP1D       ,INIMAP2D         ,
     J                             NSIGSH      ,SIGSH      ,NSIGI           ,SIGSP          ,NSIGS            ,
     K                             SIGI        ,NSIGBEAM   ,SIGBEAM         ,NSIGTRUSS      ,SIGTRUSS         ,
     L                             NSIGRS      ,SIGRS      ,MERGE_NODE_TAB  ,MERGE_NODE_TOL ,
     M                             IMERGE      ,NMERGE_TOT ,IEXLNK          ,DRAPEG         ,USER_WINDOWS,OUTPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE INTBUFDEF_MOD
      USE CLUSTER_MOD
      USE OPTIONDEF_MOD
      USE MONVOL_STRUCT_MOD
      USE INTBUF_FRIC_MOD                     
      USE GROUPDEF_MOD      
      USE DETONATORS_MOD            
      USE TABLE_MOD
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE DRAPE_MOD
      USE UNITAB_MOD
      USE ALE_EBCS_MOD
      USE SENSOR_MOD
      USE FUNC2D_MOD
      USE MULTI_FVM_MOD
      USE EBCS_MOD
      USE INIMAP1D_MOD
      USE INIMAP2D_MOD
      USE USER_WINDOWS_MOD
      USE OUTPUT_MOD
      USE MAT_ELEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "tabsiz_c.inc"
#include      "thermal_c.inc"
#include      "sphcom.inc"
#include      "com_xfem1.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD),IXR_KJ(5,*)
      INTEGER, INTENT(IN) :: NNLINK(10,SNNLINK), LNLINK(SLNLINK)
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
      INTEGER,INTENT(IN) :: NOM_SECT(SNOM_SECT),NSTRF(SNSTRF),IGEO_STACK(4* NPT_STACK+2,NS_STACK)
      my_real,INTENT(IN) :: SECBUF(SSECBUF)    
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      TYPE(MONVOL_METADATA_), INTENT(IN) :: T_MONVOL_METADATA
      TYPE(SUBSET_), DIMENSION(NSUBS), INTENT(IN) :: SUBSETS
      TYPE(DETONATORS_STRUCT_) :: DETONATORS
      my_real, INTENT(IN) :: XLAS(*)
      INTEGER, INTENT(IN) :: LAS(*)
      TYPE (DRAPE_)       :: DRAPE(NUMELC_DRAPE +  NUMELTG_DRAPE)
      TYPE (DRAPEG_)                         :: DRAPEG
      INTEGER, INTENT(IN) :: IACTIV(LACTIV,*)
      my_real, INTENT(IN) :: FACTIV(LRACTIV,*),GEO_STACK(6*NPT_STACK+1,NS_STACK),PM_STACK(20,NS_STACK)
      TYPE (UNIT_TYPE_) ::UNITAB
      my_real, INTENT(IN) :: XYZREF(SX)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE(FUNC2D_STRUCT), DIMENSION(NFUNC2D), INTENT(IN) :: FUNC2D
      TYPE (INICRACK_) , DIMENSION(NINICRACK) :: INICRACK
      INTEGER, INTENT(IN) :: LLINAL
      INTEGER, DIMENSION(LLINAL), INTENT(IN) :: LINALE
      my_real, INTENT(IN) :: QP_RPERTURB(NPERTURB,4)
      INTEGER, INTENT(IN) :: QP_IPERTURB(NPERTURB,6)
      TYPE (FVM_INIVEL_STRUCT), DIMENSION(NINVEL), INTENT(IN) :: FVM_INIVEL
      INTEGER, INTENT(IN) :: LGAUGE(3,NBGAUGE)
      my_real, INTENT(IN) :: GAUGE(LLGAUGE,NBGAUGE)
      INTEGER, INTENT(IN) :: KXX(NIXX,*)
      INTEGER, INTENT(IN) :: IXX(*)
      INTEGER, INTENT(IN) :: IXRI(4,*)
      INTEGER, INTENT(IN) :: FXBIPM(NBIPM,NFXBODY),EIGIPM(*)
      my_real, INTENT(IN) :: EIGRPM(*)
      CHARACTER, DIMENSION(NFXBODY) :: FXBFILE_TAB*2148
      INTEGER ISPHIO(NISPHIO,NSPHIO)
      my_real
     .        VSPHIO(SVSPHIO)
      TYPE(t_ebcs_tab), INTENT(IN) :: EBCS_TAB
      TYPE(INIMAP1D_STRUCT), DIMENSION(NINIMAP1D), INTENT(IN) :: INIMAP1D
      TYPE(INIMAP2D_STRUCT), DIMENSION(NINIMAP2D), INTENT(IN) :: INIMAP2D
      INTEGER, INTENT(IN) :: NSIGSH,NSIGI,NSIGS,NSIGBEAM,NSIGTRUSS,NSIGRS
      my_real, INTENT(IN) :: SIGSH(MAX(1,NSIGSH),*),SIGSP(NSIGI,*),SIGI(NSIGS,*),
     .                       SIGBEAM(NSIGBEAM,*),SIGTRUSS(NSIGTRUSS,*),
     .                       SIGRS(NSIGRS,*)
      INTEGER, INTENT(IN) :: IMERGE(*),MERGE_NODE_TAB(*),NMERGE_TOT
      my_real, INTENT(IN) :: MERGE_NODE_TOL(*)
      INTEGER,INTENT(IN) :: IPARG(NPARG,NGROUP)
      TYPE(USER_WINDOWS_),INTENT(IN)      :: USER_WINDOWS
      TYPE(OUTPUT_)      ,INTENT(IN)      :: OUTPUT
      TYPE(MAT_ELEM_)    ,INTENT(IN)      :: MAT_ELEM
C-----------------------------------------------
C      NOM_OPT(LNOPT1,SNOM_OPT1) 
C         * Possibly, NOM_OPT(1) = ID
C        NOM_OPT(LNOPT1-LTITL+1:LTITL) <=> TITLES of the OPTIONS
C--------------------------------------------------
C      SNOM_OPT1= NRBODY+NACCELM+NVOLU+NINTER+NINTSUB+
C     +           NRWALL+NJOINT+NSECT+NLINK+
C     +           NUMSKW+1+NUMFRAM+1+NFXBODY+NFLOW+NRBE2+
C     +           NRBE3+NSUBMOD+NFXVEL+NUMBCS+NUMMPC+
C     +           NGJOINT+NUNIT0+NFUNCT+NADMESH+
C     +           NSPHIO+NSPCOND+NRBYKIN+NEBCS+
C     +           NINICRACK+NODMAS+NBGAUGE+NCLUSTER+NINTERFRIC+
C     +           NRBMERGE
C-----------------------------------------------
C      INOM_OPT(SINOM_OPT)
C--------------------------------------------------
C      INOM_OPT(1) = NRBODY
C      INOM_OPT(2) = INOM_OPT(1) + NACCELM
C      INOM_OPT(3) = INOM_OPT(2) + NVOLU
C      INOM_OPT(4) = INOM_OPT(3) + NINTER
C      INOM_OPT(5) = INOM_OPT(4) + NINTSUB
C      INOM_OPT(6) = INOM_OPT(5) + NRWALL
C      INOM_OPT(7) = INOM_OPT(6) 
C      INOM_OPT(8) = INOM_OPT(7) + NJOINT
C      INOM_OPT(9) = INOM_OPT(8) + NSECT
C      INOM_OPT(10)= INOM_OPT(9) + NLINK
C      INOM_OPT(11)= INOM_OPT(10)+ NUMSKW+1+NUMFRAM+1+NSUBMOD
C      INOM_OPT(12)= INOM_OPT(11)+ NFXBODY
C      INOM_OPT(13)= INOM_OPT(12)+ NFLOW
C      INOM_OPT(14)= INOM_OPT(13)+ NRBE2
C      INOM_OPT(15)= INOM_OPT(14)+ NRBE3
C      INOM_OPT(16)= INOM_OPT(15)+ NFXVEL
C      INOM_OPT(17)= INOM_OPT(16)+ NUMBCS
C      INOM_OPT(18)= INOM_OPT(17)+ NUMMPC
C      INOM_OPT(19)= INOM_OPT(18)+ NGJOINT
C      INOM_OPT(20)= INOM_OPT(19)+ NUNIT0
C      INOM_OPT(21)= INOM_OPT(20)+ NFUNCT
C      INOM_OPT(22)= INOM_OPT(21)+ NADMESH
C      INOM_OPT(23)= INOM_OPT(22)+ NSPHIO
C      INOM_OPT(24)= INOM_OPT(23)+ NSPCOND
C      INOM_OPT(25)= INOM_OPT(24)+ NEBCS
C      INOM_OPT(26)= INOM_OPT(25)+ NINICRACK
C      INOM_OPT(27)= INOM_OPT(26)+ NODMAS
C      INOM_OPT(28)= INOM_OPT(27)+ NBGAUGE
C      INOM_OPT(29)= INOM_OPT(28)+ NCLUSTER
C      INOM_OPT(30)= INOM_OPT(29)+ NINTERFRIC
C      INOM_OPT(31)= INOM_OPT(30)+ NRBMERGE
C      .. TO BE MAINTAINED (cf doc/inom_opt.txt) ..
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NIMPDISP,NIMPVEL,NIMPACC
      INTEGER, INTENT(IN) :: IGEO(NPROPGI,NUMGEO),IPM(NPROPMI,NUMMAT)
      INTEGER, INTENT(IN) :: NUMLOADP, ILOADP(SIZLOADP,NLOADP), LLOADP(NUMLOADP)
      INTEGER, INTENT(IN) :: ICFIELD(SIZFIELD,NLOADC), LCFIELD(SLCFIELD)
      INTEGER, INTENT(IN) :: IBCL(NIBCLD,NCONLD-NPRELD), IPRES(NIBCLD,NPRELD)
      INTEGER, INTENT(IN) :: IGRV(NIGRV,NGRAV), LGRV(*)
      INTEGER, INTENT(IN) :: NPBY(NNPBY,NRBYKIN), NPBYL(NNPBY,NRBYLAG), LPBY(*), LPBYL(*)
      INTEGER, INTENT(IN) :: IBCR(NIRADIA,NUMRADIA),IBCV(NICONV,NUMCONV),IBFTEMP(NIFT,NFXTEMP),
     .                       IBFFLUX(NITFLUX,NFXFLUX),IBFVEL(NIFV,NFXVEL)
      INTEGER, INTENT(IN) :: ICODE(NUMNOD), ISKEW(NUMNOD)
      INTEGER, INTENT(IN) :: IBCSLAG(5,NBCSLAG)
      INTEGER, INTENT(IN) :: IPARI(NPARI,NINTER)
      INTEGER, INTENT(IN) :: LACCELM(3, NACCELM)
      INTEGER, INTENT(IN) :: ISKWN(LISKN,*)
      INTEGER, INTENT(IN) :: NPFRICORTH , PFRICORTH(*) , IREPFORTH(*)
      INTEGER, INTENT(IN) :: TAGXREF(NUMNOD),IXC(NIXC,*),IXTG(NIXTG,*),IXS(NIXS,*), IXS10(6,*), 
     .                       IXS16(8,*)
      INTEGER, INTENT(IN) :: ISOLNOD(NUMELS),IXR(NIXR,*), R_SKEW(*),IXP(NIXP,*),IXT(NIXT,*),
     .                       IXQ(NIXQ,*),KXSP(NISP,*),IPARTSP(*),IPARTX(*)
      INTEGER, INTENT(IN) :: NPRW(NRWALL,NNPRW),LPRW(SLPRW)
      INTEGER, INTENT(IN) :: ITHVAR(SITHVAR),
     .                       IPART(LIPART1*(NPART+NTHPART)),
     .                       IPARTTH(18*(NPART+NTHPART)),NTHGRPMX,IBCSCYC(*)
      INTEGER, INTENT(IN) :: NPTS,NPC(*),IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)  
      INTEGER, INTENT(IN) :: MGRBY(NMGRBY,SMGRBY)
      INTEGER, INTENT(IN) :: ISPCOND(NISPCOND,*),LJOINT(*),GJBUFI(LKJNI,*)
      INTEGER, INTENT(IN) :: IRAND(*)
      INTEGER, INTENT(IN) :: IPRELOAD(3,*), IFLAG_BPRELOAD(*)
      INTEGER, INTENT(IN) :: IBMPC(NUMMPC),IBMPC2(LMPC),IBMPC3(LMPC),IBMPC4(LMPC)
      INTEGER, INTENT(IN) :: IPADMESH(KIPADMESH,*)
      INTEGER, INTENT(IN) :: SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .                       SH4TRIM(*),SH3TRIM(*) 
      INTEGER, INTENT(IN) :: IEXMAD(*),IEXLNK(NR2R,*)

      my_real, INTENT(IN) ::
     .                        GEO(NPROPG,NUMGEO), BUFGEO(*), PM(NPROPM,NUMMAT), BUFMAT(*)
      my_real, INTENT(IN) ::
     .                        LOADP(LFACLOAD,NLOADP), CFIELD(LFACLOAD,NLOADC),
     .                        FORC(LFACCLD,NCONLD-NPRELD), PRES(LFACCLD,NPRELD),
     .                        AGRV(LFACGRV,NGRAV),PRELOAD(6,*)  
      my_real, INTENT(IN) ::
     .                        RBY(NRBY,NRBYKIN),RBYL(NRBY,NRBYLAG),FRBE3(SFRBE3),GJBUFR(LKJNR,*),MS(*),IN(*)
      my_real, INTENT(IN) ::  
     .                        FRADIA(LFACTHER,NUMRADIA), FCONV(LFACTHER,NUMCONV),
     .                        FBFTEMP(LFACTHER,NFXTEMP), FBFFLUX(LFACTHER,NFXFLUX),
     .                        FBFVEL(LFXVELR,NFXVEL)
      my_real, INTENT(IN) ::
     .                        V(3,NUMNOD), VR(SVR) , ! SVR=3*NUMNOD*IRODDL
     .                        W(SW)                  ! SW=3*NUMNOD*IALE
      my_real, INTENT(IN) ::
     .                        DAMPR(NRDAMP,*)
      my_real, INTENT(IN) ::
     .                        TEMP(NUMNOD)
      my_real, INTENT(IN) ::
     .                        ACCELM(LLACCELM, NACCELM)
      my_real, INTENT(IN) ::
     .                        SKEW(LSKEW,*)
      my_real, INTENT(IN) ::
     .                        XFRAME(NXFRAME,*)
      my_real, INTENT(IN) ::
     .                        I2RUPT(6,*),AREASL(*)
      my_real, INTENT(IN) ::   PHIFORTH(*), VFORTH(3,*)
      my_real, INTENT(IN) ::
     .                        XREFC(4,3,NUMELC),XREFTG(3,3,NUMELTG),XREFS(8,3,NUMELS8)
      my_real, INTENT(IN) ::
     .                        RWBUF(NRWLP,NRWALL)
      my_real, INTENT(IN) ::
     .                        PLD(*)
      my_real, INTENT(IN) ::
     .                        X(3,NUMNOD),
     .                        THKE(*),THKEC(*),SH4ANG(*),SH3ANG(*)
      my_real, INTENT(IN) ::
     .                        RTRANS(NTRANSF,*)
      my_real, INTENT(IN) ::
     .                        ALEA(*),XSEED(*)
      my_real, INTENT(IN) ::
     .                        RBMPC(SRBMPC)

      my_real, INTENT(IN)  ::  BUFSF(SBUFSF)
      INTEGER,INTENT(IN) :: SBUFSF_

      my_real, INTENT(IN) :: PADMESH(KPADMESH,*)
     
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (BOX_)    ,DIMENSION(NBBOX)  ,INTENT(IN) :: IBOX
      TYPE (ADMAS_)  ,DIMENSION(NODMAS) ,INTENT(IN) :: IPMAS
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE(TTABLE) TABLE(*)
!
      TYPE (SET_)    , DIMENSION(NSETS) :: SET
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRPART) :: IGRPART
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , INTENT(IN), DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (SURF_)   , INTENT(IN), DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , INTENT(IN), DIMENSION(NSLIN)   :: IGRSLIN
      INTEGER, INTENT(IN) :: LIFLOW, LRFLOW
      INTEGER, DIMENSION(LIFLOW), INTENT(IN) :: IFLOW
      my_real, DIMENSION(LRFLOW), INTENT(IN) :: RFLOW
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MY_ID, MY_PID, MY_MAT, MY_RBODY, MY_LOAD
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------

      ! doqa is set in QA_OUT_MOD depending on DO_QA env variable setting
      ! we skip the specific QAPRINT treatment if doqa is != 1
      
      IF ( doqa /= 1  ) RETURN

      CALL ST_QAPRINT_MATERIALS(MAT_ELEM   ,IPM       ,PM        ,BUFMAT    )

      CALL ST_QAPRINT_PROPERTIES(IGEO      ,GEO       ,BUFGEO    ,
     .                           PM_STACK  ,GEO_STACK ,IGEO_STACK)

      CALL ST_QAPRINT_INITIAL_CONDITIONS(
     1     NOM_OPT   ,INOM_OPT  ,ITAB      ,V         ,VR        ,
     2     W         ,TEMP      ,INICRACK  ,FVM_INIVEL, 
     3     INIMAP1D, INIMAP2D)

      CALL ST_QAPRINT_CONSTRAINTS(NOM_OPT   ,INOM_OPT  ,NPBY      ,LPBY      ,RBY       ,
     2                            IBFTEMP   ,FBFTEMP   ,IBFFLUX   ,FBFFLUX   ,ITAB      ,
     3                            ICODE     ,ISKEW     ,IBCSLAG   ,IBFVEL    ,FBFVEL    ,
     4                            NIMPDISP  ,NIMPVEL   ,NIMPACC   ,RWBUF     ,NPRW      ,
     5                            LPRW      ,IBCSCYC   ,IRBE3     ,LRBE3     ,FRBE3     ,
     6                            MGRBY     ,ISPCOND   ,IRBE2     ,LRBE2     ,NPBYL     ,
     7                            LPBYL     ,RBYL      ,IBMPC     ,IBMPC2    ,IBMPC3    ,
     8                            IBMPC4    ,RBMPC     ,LJOINT    ,NNLINK    ,LNLINK,
     9                            LLINAL    ,LINALE    ,GJBUFI    ,GJBUFR    ,MS        ,
     9                            IN        ,FXBIPM    ,FXBFILE_TAB)

      CALL ST_QAPRINT_LOADS(NOM_OPT   ,INOM_OPT  ,NUMLOADP  ,ILOADP    ,LLOADP    ,
     2                      LOADP     ,IBCL      ,FORC      ,IPRES     ,PRES      ,
     3                      IBCR      ,FRADIA    ,IBCV      ,FCONV     ,IGRV      ,
     4                      LGRV      ,AGRV      ,ICFIELD   ,LCFIELD   ,CFIELD    ,
     5                      IPRELOAD  ,PRELOAD   ,IFLAG_BPRELOAD,
     6                      LIFLOW, LRFLOW, IFLOW,RFLOW     ,ISPHIO    ,VSPHIO    )

      CALL ST_QAPRINT_GENERAL_CONTROLS(NOM_OPT   ,INOM_OPT  ,DAMPR, IRAND, ALEA, 
     1                                 XSEED     ,UNITAB    ,QP_IPERTURB       ,
     2                                 QP_RPERTURB,EIGIPM   , EIGRPM)
      CALL ST_QAPRINT_INTERFACES(NOM_OPT   ,INOM_OPT  ,IPARI  ,INTBUF_TAB,I2RUPT ,  
     1                           AREASL)      
      CALL ST_QAPRINT_CLUSTERS(NOM_OPT   ,INOM_OPT  ,CLUSTERS)
      CALL ST_QAPRINT_OUTPUT_DATABASES(NOM_OPT, INOM_OPT, LACCELM ,ACCELM,LGAUGE,GAUGE)
      CALL ST_QAPRINT_MODEL_TOOLS(NOM_OPT, INOM_OPT, IBOX, IPMAS, NOM_SECT, NSTRF, SECBUF ,
     .                            SKEW   , ISKWN   , XFRAME, NPC,PLD,TABLE,NPTS, 
     .                            IACTIV , FACTIV  ,SENSORS,FUNC2D)
      CALL ST_QAPRINT_MONVOL(T_MONVOL, T_MONVOL_METADATA)
      CALL ST_QAPRINT_FRICTION(NOM_OPT,INOM_OPT,INTBUF_FRIC_TAB,NPFRICORTH,PFRICORTH ,IREPFORTH,PHIFORTH,VFORTH) 
      CALL ST_QAPRINT_REFERENCE_STATE(XREFC     ,XREFTG    ,XREFS     ,TAGXREF   ,
     1                                IXS       ,IXC       ,IXTG      )

      CALL ST_QAPRINT_TIME_HISTORIES(OUTPUT%TH   ,ITHVAR   ,IPART    ,SUBSETS  ,
     1                               IPARTTH  ,NTHGRPMX )

      CALL ST_QAPRINT_DFS_DETONATORS(DETONATORS)    

      CALL ST_QAPRINT_DFS_LASERS(XLAS,LAS)   

      CALL ST_QAPRINT_NODES(ITAB ,X,IMERGE,MERGE_NODE_TOL, MERGE_NODE_TAB,NMERGE_TOT,MS)

      CALL ST_QAPRINT_ELEMENT(IXS ,IXS10 ,IPM ,IGEO ,ITAB ,
     1                        ISOLNOD,IXR,R_SKEW,ISKWN,IXP,
     2                        IXT    ,X  ,IXC   ,IXTG ,THKE,
     3                        SH4ANG ,THKEC,SH3ANG,KXSP,IPARTSP,
     4                        IPART  ,IXR_KJ,KXX, IXX ,IPARTX,
     5                        IXRI   ,IXS16 ,IXQ )       
!
      CALL ST_QAPRINT_SET(SET    ,LSUBMODEL,ITAB     ,IGRNOD ,IGRPART,
     .                    IPART  ,IGRBRIC  ,IGRSH4N  ,IGRSH3N,IGRQUAD,
     .                    IGRBEAM,IGRTRUSS ,IGRSPRING,IGRSURF,IGRSLIN,
     .                    IXC    ,IXTG     ,IXQ      ,IXP    ,IXT    ,
     .                    IXR    ,IXS      )
      CALL ST_QAPRINT_TRANSFORMATIONS(RTRANS,LSUBMODEL,IGRNOD)
      CALL ST_QAPRINT_COMPOSITE_OPTIONS(DRAPE,DRAPEG)
      CALL ST_QAPRINT_ALE_OPTIONS_DRIVER
      CALL ST_QAPRINT_INIVOL()  
      CALL ST_QAPRINT_REFSTA(XREFC     ,XREFTG  ,XREFS    ,TAGXREF   ,IXS   ,IXC   ,IXTG   ,ITAB, XYZREF)   
      CALL ST_QAPRINT_EBCS(EBCS_TAB)  
      CALL ST_QAPRINT_GROUPS(IGRNOD ,IGRPART ,IGRBRIC  ,IGRSH4N   ,IGRSH3N   ,
     .                       IGRQUAD,IGRBEAM ,IGRTRUSS ,IGRSPRING)  
      CALL ST_QAPRINT_SURF(IGRSURF, IGRSLIN, BUFSF, SBUFSF_)  
      CALL ST_QAPRINT_ADMESH(IPART ,IPADMESH,PADMESH,SH4TREE,SH3TREE,
     .                        IXC  ,IXTG    ,SH4TRIM,SH3TRIM    )
      CALL ST_QAPRINT_MADYMO(IEXMAD ,ITAB   ,IPART  ,IXS    ,IXC    ,IXTG   )  
      CALL ST_QAPRINT_SEATBELTS(ITAB)
      CALL ST_QAPRINT_INITIAL_STATE(
     .                    NSIGSH   ,SIGSH    ,NSIGI   ,SIGSP    ,NSIGS   ,
     .                    SIGI     ,NSIGBEAM ,SIGBEAM ,NSIGTRUSS,SIGTRUSS,
     .                    NSIGRS   ,SIGRS    )
      CALL ST_QAPRINT_MULTIDOMAINS(IPART,IEXLNK,IGRNOD)
      CALL ST_QAPRINT_USERWI(USER_WINDOWS)
      CALL ST_QAPRINT_INTERNAL_GROUPS(IPARG)
C-----------------------------------------------------------------------

      RETURN
      END
