Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    forintc                     ../engine/source/elements/forintc.F
!||--- called by ------------------------------------------------------
!||    resol                       ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    c3forc3                     ../engine/source/elements/sh3n/coque3n/c3forc3.F
!||    c3forc3_crk                 ../engine/source/elements/xfem/c3forc3_crk.F
!||    cbaforc3                    ../engine/source/elements/shell/coqueba/cbaforc3.F
!||    cdk6forc3                   ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
!||    cdkforc3                    ../engine/source/elements/sh3n/coquedk/cdkforc3.F
!||    cforc3                      ../engine/source/elements/shell/coque/cforc3.F
!||    cforc3_crk                  ../engine/source/elements/xfem/cforc3_crk.F
!||    czforc3                     ../engine/source/elements/shell/coquez/czforc3.F
!||    czforc3_crk                 ../engine/source/elements/xfem/czforc3_crk.F
!||    seatbelt_reduction_factor   ../engine/source/tools/seatbelts/seatbelt_reduction_factor.F
!||    section_3n                  ../engine/source/tools/sect/section_3n.F
!||    section_c                   ../engine/source/tools/sect/section_c.F
!||    sensor_energy_part          ../engine/source/tools/sensor/sensor_energy_part.F
!||    startimeg                   ../engine/source/system/timer.F
!||    stoptimeg                   ../engine/source/system/timer.F
!||    ush_force3                  ../engine/source/user_interface/ushforce3.F90
!||--- uses       -----------------------------------------------------
!||    crackxfem_mod               ../engine/share/modules/crackxfem_mod.F
!||    drape_mod                   ../engine/share/modules/drape_mod.F
!||    dt_mod                      ../engine/source/modules/dt_mod.F
!||    elbufdef_mod                ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                 ../common_source/modules/elements/element_mod.F90
!||    failwave_mod                ../common_source/modules/failwave_mod.F
!||    glob_therm_mod              ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    groupdef_mod                ../common_source/modules/groupdef_mod.F
!||    mat_elem_mod                ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    nlocal_reg_mod              ../common_source/modules/nlocal_reg_mod.F
!||    output_mod                  ../common_source/modules/output/output_mod.F90
!||    pinchtype_mod               ../common_source/modules/pinchtype_mod.F
!||    rupture_mod                 ../engine/share/modules/rupture_mod.F
!||    sensor_mod                  ../common_source/modules/sensor_mod.F90
!||    stack_mod                   ../engine/share/modules/stack_mod.F
!||    table_mod                   ../engine/share/modules/table_mod.F
!||    timer_mod                   ../engine/source/system/timer_mod.F90
!||    ush_force3_mod              ../engine/source/user_interface/ushforce3.F90
!||====================================================================
      SUBROUTINE FORINTC(TIMERS, GPU, IPRI,
     1    PM        ,GEO       ,X         ,A         ,AR        ,
     2    V         ,VR        ,MS        ,IN        ,NLOC_DMG  ,
     3    WA        ,STIFN     ,STIFR     ,FSKY      ,CRKSKY    ,
     4    TF        ,BUFMAT    ,PARTSAV   ,D         ,MAT_ELEM  ,
     5    DR        ,EANI      ,TANI      ,FANI      ,
     6    FSAV      ,SENSORS   ,SKEW      ,FAILWAVE  ,
     7    DT2T      ,THKE      ,BUFGEO    ,IADC      ,IADTG      ,
     8    IPARG     ,NPC       ,IXC       ,IXTG      ,NELTST    ,
     A    IPARI     ,ITYPTST   ,NSTRF     ,
     B    IPART     ,IPARTC    ,IPARTTG   ,SECFCUM   ,
     D    FSAVD     ,GROUP_PARAM_TAB,
     F    FZERO     ,IXTG1     ,IADTG1    ,IGEO      ,IPM       ,
     G    MADFAIL   ,XSEC      ,ITASK     ,MCP       ,
     H    TEMP      ,FTHE      ,FTHESKY   ,
     I    MS_PLY    ,ZI_PLY    ,INOD_PXFEM,XEDGE4N   ,XEDGE3N   ,
     J    IEL_PXFEM ,IADC_PXFEM,IGROUC    ,NGROUC    ,GRESAV    ,
     K    GRTH      ,IGRTH     ,MSTG      ,DMELTG    ,MSC       ,
     L    DMELC     ,TABLE     ,KNOD2ELC  ,PTG       ,MSZ2      ,
     M    INOD_CRK  ,IEL_CRK   ,IADC_CRK  ,ELCUTC    ,NODENR    ,
     N    IBORDNODE ,NODEDGE   ,CRKNODIAD ,ELBUF_TAB ,
     O    XFEM_TAB  ,CONDN     ,CONDNSKY  ,CRKEDGE   ,
     P    STACK     ,ITAB      ,GLOB_THERM,
     S    DRAPE_SH4N  ,DRAPE_SH3N  ,SUBSET    ,XDP       ,VPINCH,
     T    APINCH    ,STIFPINCH ,DRAPEG,OUTPUT ,DT        ,
     Y    SNPC      ,  STF, USERL_AVAIL,MAXFUNC,
     U    SBUFMAT   ,IPARIT    ,LSKY      ,NODADT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD
      USE CRACKXFEM_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE GROUPDEF_MOD
      USE PINCHTYPE_MOD
      USE SENSOR_MOD
      USE RUPTURE_MOD
      USE DRAPE_MOD
      USE OUTPUT_MOD
      USE ELBUFDEF_MOD
      USE DT_MOD
      use glob_therm_mod
      use element_mod , only : nixc,nixtg
      use ush_force3_mod , only : ush_force3
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com_xfem1.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "couple_c.inc"
#include      "impl1_c.inc"
#include      "stati_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: GPU
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER,INTENT(IN)    :: USERL_AVAIL ! Flag for User libraries availability
      INTEGER,INTENT(IN)    :: MAXFUNC     ! Maximum number of functions
      INTEGER,INTENT(IN)    :: SBUFMAT     ! Size of Bufmat
      INTEGER,INTENT(IN)    :: STF         ! Size of TF
      INTEGER,INTENT(IN)    :: SNPC        ! Size of NPC
      INTEGER,INTENT(IN)    :: IPARIT, LSKY, NODADT
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*), IGEO(NPROPGI,*), IPM(NPROPMI,*),
     .   NPC(*), IPARG(NPARG,*), IPARI(NPARI,*), 
     .   NSTRF(*), IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   IADC(4,*), IADTG(3,*),NELTST,
     .   ITYPTST,IXTG1(4,*),XEDGE4N(4,*),XEDGE3N(3,*),
     .   IADTG1(3,*),MADFAIL(*),ITASK,
     .   INOD_PXFEM(*),IEL_PXFEM(*) ,IADC_PXFEM(4,*), IGROUC(*),
     .   NGROUC,GRTH(*),IGRTH(*),KNOD2ELC(*),
     .   INOD_CRK(*),IEL_CRK(*),IADC_CRK(*),ELCUTC(2,*),
     .   NODENR(*),IBORDNODE(*),NODEDGE(2,*),CRKNODIAD(*),
     .   ITAB(*)
C     REAL
      my_real
     .   X(3,*)    ,D(3,*)  ,V(3,*)   ,VR(3,*),
     .   MS(*)     ,IN(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),
     .   GEO(NPROPG,*),BUFMAT(*) ,TF(STF) ,FSAV(NTHVKI,*) ,
     .   WA(*), THKE(*),
     .   A(3,*)    ,AR(3,*) ,FANI(3,*) ,PARTSAV(NPSAV,*)    ,
     .   STIFN(*) ,STIFR(*),FSKY(*) ,
     .   DR(3,*) ,TANI(*),EANI(*),
     .   BUFGEO(*) ,DT2T, SECFCUM(7,NUMNOD,NSECT),
     .   FSAVD(NTHVKI,*),
     .   FZERO(3,4,(NUMELC+NUMELTG)),XSEC(4,3,NSECT),
     .   MCP(*),TEMP(*),FTHE(*),FTHESKY(*),
     .   MS_PLY(*), ZI_PLY(*),GRESAV(*),
     .   MSTG(*), DMELTG(*), MSC(*), DMELC(*),CONDN(*),CONDNSKY(*),
     .   PTG(3,*),MSZ2(*),APINCH(3,*),STIFPINCH(*),VPINCH(3,*)
      integer, intent(inout) :: ipri
      DOUBLE PRECISION :: XDP(3,*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*)           :: CRKEDGE
      TYPE (XFEM_SKY_)    , DIMENSION(*) :: CRKSKY
      TYPE (STACK_PLY) :: STACK 
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (NLOCAL_STR_)   :: NLOC_DMG 
      TYPE (GROUP_PARAM_) , DIMENSION(NGROUP) :: GROUP_PARAM_TAB
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (DRAPE_)    :: DRAPE_SH4N(NUMELC_DRAPE),DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_)               :: DRAPEG
      TYPE (SENSORS_) ,INTENT(INOUT) ,TARGET :: SENSORS
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (DT_) ,INTENT(IN) :: DT
      type (glob_therm_) ,intent(inout) :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      DOUBLE PRECISION, POINTER :: pFBSAV6
      INTEGER INDXOF(MVSIZ)
      INTEGER I,II,J,N, NG, NVC, MLW, JFT, JLT,ISOLNOD,ITHK,IPLA,
     .    K2, NF1, OFFSET,TYP,
     .   K0, K5, K9, NSG, NEL, KFTS,IOFC, ISTRA,
     .   ICNOD,NFT1,ISENS_ENERGY,
     .   IEXPAN, IG,ITG1,ITG2,ITG3,NLEVXF,ISEATBELT
      INTEGER IPARSENS,ISECT
      INTEGER ISH3N,ISHPLYXFEM,IXFEM
      INTEGER IXEL,ACTIFXFEM,ISUBSTACK
      my_real
     .   FX(MVSIZ,20),FY(MVSIZ,20),FZ(MVSIZ,20),
     .   MX(MVSIZ,4),MY(MVSIZ,4),MZ(MVSIZ,4)
C
      my_real
     .   UXINT_MEAN(NXEL*NXLAYMAX,MVSIZ),UYINT_MEAN(NXEL*NXLAYMAX,MVSIZ),
     .   UZINT_MEAN(NXEL*NXLAYMAX,MVSIZ)
C======================================================================|
      ITG1 = 1+4*ECRKXFEC
      ITG2 = 2*NUMELC
      ITG3 = 1+NUMELC
      IPRI = 0
      IF(MOD(NCYCLE,IABS(NCPRI))==0.OR.TT >= OUTPUT%TH%THIS.OR.
     +       MDESS /= 0.OR.TT >= OUTPUT%TH%THIS1(1).OR.TT >= OUTPUT%TH%THIS1(2)
     +       .OR.TT >= OUTPUT%TH%THIS1(3) .OR.TT >= OUTPUT%TH%THIS1(4).OR.TT >= OUTPUT%TH%THIS1(5)
     +       .OR.TT >= OUTPUT%TH%THIS1(6) .OR.TT >= OUTPUT%TH%THIS1(7).OR.TT >= OUTPUT%TH%THIS1(8)
     +       .OR.TT >= OUTPUT%TH%THIS1(9) .OR.NTH /= 0.OR.NANIM/=0
     +       .OR.TT >= TABFIS(1).OR.TT >= TABFIS(2)
     +       .OR. TT >= TABFIS(3).OR.TT >= TABFIS(4).OR.TT >= TABFIS(5)
     +       .OR. TT >= TABFIS(6).OR.TT >= TABFIS(7).OR.TT >= TABFIS(8)
     +       .OR. TT >= TABFIS(9).OR. TT >= TABFIS(10).OR. ISTAT==3) IPRI=1
!
      ISENS_ENERGY = 0
      DO I=1,SENSORS%NSENSOR
        TYP = SENSORS%SENSOR_TAB(I)%TYPE
        IF (TYP == 14) ISENS_ENERGY = 1 ! save internal/kinetic energy (PARTSAV)
      ENDDO ! DO I=1,NSENSOR
C
C Dynamic parallel loop SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUC
C
            NG = IGROUC(IG)
            SENSORS%NGR_SENSOR(ITASK+1) = NG
            NGR_FAIL_WIND = NG
            IF (IPARG(1, NG) == 151) THEN !  Bypass law 151
               CYCLE
            ENDIF
C---------temporarily used to avoid pass KTBUF_STR everywhere
            NG_IMP = NG
            IF(IPARG(8,NG)==1)GOTO 250
            ITY   =IPARG(5,NG)
C            IF(ITY/=3.AND.ITY/=7)GOTO 250
            IF (IDDW>0) CALL STARTIMEG(NG)
            OFFSET  = 0
            MLW     = IPARG(1,NG)
C MLW= 0 ----> void
C MLW = 13 ----> rigid material
            IF (MLW == 0 .OR. MLW == 13) GOTO 250
C---
            NEL     = IPARG(2,NG)
            NFT     = IPARG(3,NG)
            NPT     = IPARG(6,NG)
            JALE    = IPARG(7,NG)
            ISMSTR  = IPARG(9,NG)
            NSG     = IPARG(10,NG)
            JEUL    = IPARG(11,NG)
            JTUR    = IPARG(12,NG)
            JTHE    = IPARG(13,NG)
            JLAG    = IPARG(14,NG)
            ISTRA   = IPARG(44,NG)
            NVC     = IPARG(19,NG)
            JMULT   = IPARG(20,NG)
            JHBE    = IPARG(23,NG)
            ISH3N   = IPARG(23,NG)
            JIVF    = IPARG(24,NG)
            JPOR    = IPARG(27,NG)
            ITHK    = IPARG(28,NG)
            ISOLNOD = IPARG(28,NG)
            IPLA    = IPARG(29,NG)
            ICNOD   = IPARG(11,NG)
            IREP    = IPARG(35,NG)
            IINT    = IPARG(36,NG)
            JCVT    = IPARG(37,NG)
            IGTYP   = IPARG(38,NG)
            ISORTH  = IPARG(42,NG)
            ISORTHG = ISORTH
            ISRAT   = IPARG(40,NG)
            ISROT   = IPARG(41,NG)
            IFAILURE= IPARG(43,NG)
            KFTS    = IPARG(30,NG)
            JCLOSE  = IPARG(33,NG)
            ICSEN   = IPARG(39,NG)
            IEXPAN  = IPARG(49,NG)
            ISHPLYXFEM  = IPARG(50,NG)
            IGRE    = IPARG(51,NG)
            JSMS    = IPARG(52,NG)
            IXFEM   = IPARG(54,NG)
            NLEVXF  = IPARG(65,NG)
            ACTIFXFEM=IPARG(70,NG)
            ISUBSTACK=IPARG(71,NG)
            ISEATBELT=IPARG(91,NG)
            IF(GPU == 1 ) THEN
              IF (ITY == 3  ! 4-node shell
     .             .AND. MLW == 2  ! material law 2 (Johnson-Cook)
     .             .AND. IPARG(8,NG) /= 1  ! group active
     .             .AND. NPT > 0  ! through-thickness integration (not global)
     .             .AND. JHBE < 11  ! standard formulation (not BATOZ, not QEPH)
     .             .AND. IGTYP < 29  ! not user property
     .             .AND. IXFEM == 0  ! no XFEM
     .             .AND. IFAILURE == 0  ! no failure model (simplify for mockup)
     .             .AND. NSECT == 0  ! no section (simplify for mockup)
     .             .AND. NEXMAD == 0  ! no explicit mass scaling (simplify for mockup)
     .             .AND. ICRACK3D == 0 .AND. IXFEM == 0 .AND. ACTIFXFEM == 0 
     .             .AND. NSLIPRING == 0 .AND. ISEATBELT == 0  ! no slipring/seatbelt (simplify for mockup)
     .             .AND. GPU == 1  ! GPU enabled
     .             .AND. ISUBSTACK == 0 .AND. ISENS_ENERGY == 0) THEN
                CYCLE
              ENDIF
            ENDIF
            LFT   = 1
            LLT   = MIN(NVSIZ,NEL)
            MTN   = MLW
            JFT=LFT
            JLT=LLT
            NF1 = NFT+1
            JSPH=0
C----6---------------------------------------------------------------7---------8
        IF(ITY==3)THEN
          IOFC = 0
C
C--      Reduction factor computation for seatbelts in sliprings
         IF ((NSLIPRING > 0).AND.(ISEATBELT==1)) THEN
           CALL SEATBELT_REDUCTION_FACTOR(ELBUF_TAB(NG),JFT,JLT,IXC,NEL,X,NFT)
         ENDIF
C
         IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .    CALL SENSOR_ENERGY_PART(IPARTC(NF1) ,SUBSET ,IPRI,SENSORS,ITASK)
          IF (IGTYP >= 29.AND.IGTYP<=31) THEN  !user prop
            CALL USH_FORCE3(IGTYP,ELBUF_TAB(NG),                          
     1             NUMNOD,  NPARG,   NPART,   NUMMAT,    NPSAV, 
     2        IPARG(1,NG),   NIXC,IXC(1,NF1),IPARTC(NF1),    4, 
     3              ITASK,   IPRI,USERL_AVAIL,   PM ,      NEL, 
     4                  X,      V,      VR,        A,       AR, 
     5                  D,  STIFN,   STIFR,THKE(NF1),IADC(1,NF1), 
     6               FSKY,PARTSAV,     DT1,     DT2T,      TT , 
     7           FX(1:MVSIZ,1:4),FY(1:MVSIZ,1:4),FZ(1:MVSIZ,1:4),MX(1:MVSIZ,1:4), MY(1:MVSIZ,1:4), 
     8           MZ(1:MVSIZ,1:4),SENSORS,IPARIT,LSKY,  NODADT)
          ELSE IF (JHBE >= 11.AND.JHBE <= 19) THEN
              
            CALL CBAFORC3(TIMERS,
     1        ELBUF_TAB(NG),      JFT,                JLT,                NFT,          
     2        NPT,                IPARI,              MLW,                IPRI,         
     3        ITHK,               NELTST,             ITYPTST,            ITAB,         
     4        MAT_ELEM,           ISTRA,              IPLA,               TT,           
     5        DT1,                DT2T,               PM,                 GEO,          
     6        PARTSAV,            IXC(1,NF1),         FAILWAVE,           BUFMAT,       
     7        TF,                 NPC,                IADC(1,NF1),        X,            
     8        D,                  DR,                 V,                  VR,           
     9        A,                  AR,                 STIFN,              STIFR,        
     A        FSKY,               TANI,               OFFSET,             EANI,         
     B        INDXOF,             IPARTC(NF1),        THKE(NF1),          NVC,          
     C        IOFC,               JHBE,               FX(1,1),            FX(1,2),      
     D        FX(1,3),            FX(1,4),            FY(1,1),            FY(1,2),      
     E        FY(1,3),            FY(1,4),            FZ(1,1),            FZ(1,2),      
     F        FZ(1,3),            FZ(1,4),            MX(1,1),            MX(1,2),      
     G        MX(1,3),            MX(1,4),            MY(1,1),            MY(1,2),      
     H        MY(1,3),            MY(1,4),            MZ(1,1),            MZ(1,2),      
     I        MZ(1,3),            MZ(1,4),            KFTS,               ISMSTR,       
     J        IGEO,               GROUP_PARAM_TAB(NG),IPM,                IFAILURE,     
     K        ITASK,              JTHE,               TEMP,               FTHE,         
     L        FTHESKY,            IEXPAN,             ISHPLYXFEM,         MS,           
     M        IN,                 MS_PLY,             ZI_PLY,             INOD_PXFEM,   
     N        IEL_PXFEM,          IADC_PXFEM,         GRESAV,             GRTH,         
     O        IGRTH(NF1),         MSC(NF1),           DMELC(NF1),         JSMS,         
     P        TABLE,              IPARG(1,NG),        SENSORS,            MSZ2,         
     Q        CONDN,              CONDNSKY,           ISUBSTACK,          STACK,        
     R        DRAPE_SH4N,         NEL,                NLOC_DMG,           VPINCH,       
     S        APINCH,             STIFPINCH,          DRAPEG%INDX_SH4N,   IGRE,         
     T        JTUR,               DT,                 NCYCLE,             SNPC,         
     Y        STF,                GLOB_THERM,         NXLAYMAX,           IDEL7NOK,
     U        USERL_AVAIL,        MAXFUNC,            SBUFMAT    ,
     X        IPART      ,        LIPART1)                                                       

          ELSEIF (JHBE >= 21 .AND. JHBE <= 29) THEN

            CALL CZFORC3(TIMERS,
     1        ELBUF_TAB(NG),      JFT,                JLT,                NFT,
     2        NPT,                ITAB,               MLW,
     3        IPRI,               ITHK,               NELTST,             ISTRA,
     4        IPLA,               DT1,                DT2T,
     5        PM,                 GEO,                PARTSAV,            IXC(1,NF1),
     6        ITYPTST,            BUFMAT,             TF,                 NPC,
     7        IADC(1,NF1),        FAILWAVE,           X,
     8        DR,                 V,                  VR,                 A,
     9        AR,                 STIFN,              STIFR,              FSKY,
     A        TANI,               INDXOF,             ISMSTR,
     B        GROUP_PARAM_TAB(NG),IPARTC(NF1),        THKE(NF1),          NVC,
     C        IOFC,               JHBE,               FX(1,1),            FX(1,2),
     D        FX(1,3),            FX(1,4),            FY(1,1),            FY(1,2),
     E        FY(1,3),            FY(1,4),            FZ(1,1),            FZ(1,2),
     F        FZ(1,3),            FZ(1,4),            MX(1,1),            MX(1,2),
     G        MX(1,3),            MX(1,4),            MY(1,1),            MY(1,2),
     H        MY(1,3),            MY(1,4),            MZ(1,1),            MZ(1,2),
     I        MZ(1,3),            MZ(1,4),            KFTS,               FZERO(1,1,NF1),
     J        IGEO,               IPM,                IFAILURE,           ITASK,
     K        JTHE,               TEMP,               FTHE,               FTHESKY,
     L        IEXPAN,             GRESAV,             GRTH,               IGRTH(NF1),
     M        XEDGE4N,            MSC(NF1),           DMELC(NF1),         JSMS,
     N        TABLE,              IPARG(1,NG),        MAT_ELEM    ,       IXFEM,
     O        KNOD2ELC,           SENSORS,            ELCUTC(1,NF1),      INOD_CRK,
     P        IEL_CRK,            NODENR,             IADC_CRK,
     Q        NODEDGE,            CRKNODIAD,          CONDN,              CONDNSKY,
     R        STACK,              ISUBSTACK,          XFEM_TAB(NG,1:NXEL),CRKEDGE,
     S        DRAPE_SH4N,         NEL,                NLOC_DMG,           DRAPEG%INDX_SH4N,
     T        IGRE,               JTUR,               DT      ,           NCYCLE,
     Y        SNPC,               STF,                GLOB_THERM,         IDEL7NOK,   
     U        USERL_AVAIL,        MAXFUNC,            SBUFMAT    ,
     X        IPART      ,        LIPART1)

        IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
          DO IXEL=1,NXEL
            CALL CZFORC3_CRK(TIMERS, XFEM_TAB(NG,IXEL),
     1        JFT        ,JLT       ,NFT       ,ITYPTST    ,
     2        IPARI      ,MLW       ,IPRI      ,ITHK      ,NELTST     ,
     3        ISTRA      ,IPLA      ,TT        ,DT1       ,DT2T       ,
     4        PM         ,GEO       ,PARTSAV   ,IXC(1,NF1),GROUP_PARAM_TAB(NG),
     5        BUFMAT     ,TF        ,NPC       ,IADC(1,NF1),FAILWAVE  ,
     6        X          ,D         ,DR        ,V         ,VR         ,
     7        A          ,AR        ,STIFN     ,STIFR     ,FSKY       ,
     8        TANI      ,OFFSET    ,EANI      ,INDXOF     ,   
     9        IPARTC(NF1),THKE(NF1) ,NVC       ,IOFC      ,JHBE       ,
     A        FX(1,1)    ,FX(1,2)   ,FX(1,3)   ,FX(1,4)   ,FY(1,1)    ,
     B        FY(1,2)    ,FY(1,3)   ,FY(1,4)   ,FZ(1,1)   ,FZ(1,2)    ,
     C        FZ(1,3)    ,FZ(1,4)   ,MX(1,1)   ,MX(1,2)   ,MX(1,3)    ,
     D        MX(1,4)    ,MY(1,1)   ,MY(1,2)   ,MY(1,3)   ,MY(1,4)    ,
     E        MZ(1,1)    ,MZ(1,2)   ,MZ(1,3)   ,MZ(1,4),
     F        KFTS       ,FZERO(1,1,NF1),ISMSTR,MAT_ELEM  ,
     I        IGEO       ,IPM        ,IFAILURE ,ITASK     ,JTHE        ,
     J        TEMP       ,FTHE       ,FTHESKY  ,IEXPAN    ,GRESAV      ,
     K        GRTH       ,IGRTH(NF1) ,MSC(NF1) ,DMELC(NF1),JSMS        ,
     L        TABLE      ,IPARG(1,NG),IXFEM    ,INOD_CRK  ,IEL_CRK     ,
     M        IADC_CRK   ,ELCUTC(1,NF1),CRKSKY ,
     N        SENSORS    ,IXEL     ,
     O        ISUBSTACK  ,UXINT_MEAN ,UYINT_MEAN,UZINT_MEAN,NLEVXF      ,
     P        NODEDGE    ,CRKEDGE    ,STACK    ,DRAPE_SH4N  ,NLOC_DMG,DRAPEG%INDX_SH4N,IGRE,
     *        DT         ,NCYCLE     ,SNPC      ,  STF      ,GLOB_THERM ,
     *        IDEL7NOK   ,USERL_AVAIL,MAXFUNC  ,SBUFMAT   ,
     X        IPART      ,        LIPART1)
          ENDDO
        ENDIF
c
      ELSE
         CALL CFORC3(TIMERS,
     1   ELBUF_TAB(NG),      JFT,                JLT,                PM,
     2   IXC(1,NF1),         X,                  A,                  AR,
     3   V,                  VR,                 FAILWAVE,           NVC,
     4   MLW,                GEO,                TF,                 NPC,
     5   BUFMAT,             PARTSAV,            DT2T,               NELTST,
     6   ITYPTST,            STIFN,              STIFR,              FSKY,
     7   IADC(1,NF1),        ITAB,               D,                  DR,
     8   TANI,               OFFSET,             EANI,               FX(1,1),
     9   FX(1,2),            FX(1,3),            FX(1,4),            FY(1,1),
     A   FY(1,2),            FY(1,3),            FY(1,4),            FZ(1,1),
     B   FZ(1,2),            FZ(1,3),            FZ(1,4),            MX(1,1),
     C   MX(1,2),            MX(1,3),            MX(1,4),            MY(1,1),
     D   MY(1,2),            MY(1,3),            MY(1,4),            MZ(1,1),
     E   MZ(1,2),            MZ(1,3),            MZ(1,4),            INDXOF,
     F   IPARTC(NF1),        THKE(NF1),          GROUP_PARAM_TAB(NG),MAT_ELEM,
     G   NEL,                ISTRA,              JHBE,               ITHK,
     H   IOFC,               IPLA,               NFT,                ISMSTR,
     I   NPT,                KFTS,               FZERO(1,1,NF1),     IGEO,
     J   IPM,                IFAILURE,           ITASK,              JTHE,
     K   TEMP,               FTHE,               FTHESKY,            IEXPAN,
     L   GRESAV,             GRTH,               XEDGE4N,            IGRTH(NF1),
     M   MSC(NF1),           DMELC(NF1),         JSMS,               TABLE,
     N   IPARG(1,NG),        IXFEM,              KNOD2ELC,           SENSORS  ,
     O   ELCUTC(1,NF1),      INOD_CRK,           IEL_CRK,            IBORDNODE,
     P   NODENR,             IADC_CRK,           NODEDGE,            CRKNODIAD,
     Q   CONDN,              CONDNSKY,           STACK,              ISUBSTACK,
     R   XFEM_TAB, NG  ,CRKEDGE,      DRAPE_SH4N,         IPRI,
     S   NLOC_DMG,           DRAPEG%INDX_SH4N,   IGRE,               JTUR,
     T   OUTPUT,             DT,                 SNPC,               STF  , 
     U   GLOB_THERM,         USERL_AVAIL,        MAXFUNC,            SBUFMAT  ,
     X        IPART)
c
        IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
          DO IXEL=1,NXEL
             CALL CFORC3_CRK(TIMERS, XFEM_TAB(NG,IXEL),
     1       JFT      ,JLT      ,PM       ,IXC(1,NF1),X         ,
     2       A        ,AR       ,V        ,VR       ,FAILWAVE   ,   
     3       NVC      ,MLW      ,GEO      ,TF       ,NPC        ,
     4       BUFMAT   ,PARTSAV  ,DT2T     ,NELTST   ,ITYPTST    ,
     5       STIFN    ,STIFR    ,FSKY     ,CRKSKY   ,IADC(1,NF1),
     6       D        ,DR       ,TANI     ,OFFSET   ,EANI       ,
     7       FX(1,1)  ,FX(1,2)  ,FX(1,3)  ,FX(1,4)  ,FY(1,1)    ,
     8       FY(1,2)  ,FY(1,3)  ,FY(1,4)  ,FZ(1,1)  ,FZ(1,2)    ,
     9       FZ(1,3)  ,FZ(1,4)  ,MX(1,1)  ,MX(1,2)  ,MX(1,3)    ,
     A       MX(1,4)  ,MY(1,1)  ,MY(1,2)  ,MY(1,3)  ,MY(1,4)    ,
     B       MZ(1,1)  ,MZ(1,2)  ,MZ(1,3)  ,MZ(1,4)  ,INDXOF     ,
     C       IPARTC(NF1),THKE(NF1),GROUP_PARAM_TAB(NG),MAT_ELEM , 
     F       NEL      ,ISTRA     ,JHBE    ,KFTS     ,
     G       ITHK     ,IOFC      ,IPLA    ,NFT      ,ISMSTR     ,
     H       FZERO(1,1,NF1),IGEO      ,IPM         ,IFAILURE  ,ITASK       ,   
     I       JTHE          ,TEMP      , FTHE       ,FTHESKY   ,IEXPAN      ,
     J       GRESAV      ,GRTH        ,
     K       IGRTH(NF1)    ,MSC(NF1)  ,DMELC(NF1)  ,JSMS      ,TABLE       ,
     L       IPARG(1,NG) ,IXFEM       ,INOD_CRK    ,IEL_CRK   ,IADC_CRK    ,
     M       ELCUTC(1,NF1),
     N       SENSORS,IXEL      ,STACK       ,
     O       ISUBSTACK   ,UXINT_MEAN  ,UYINT_MEAN  ,UZINT_MEAN,NLEVXF      ,
     P       NODEDGE     ,CRKEDGE     ,DRAPE_SH4N   ,IPRI       ,NLOC_DMG  ,
     Q       DRAPEG%INDX_SH4N,IGRE    ,DT           ,SNPC      ,  STF      ,
     R       GLOB_THERM  ,USERL_AVAIL, MAXFUNC,SBUFMAT ,
     X       IPART  )
          ENDDO
        END IF

      ENDIF
!----------------------------------------------------------------------------
          IF(NSECT>0)THEN
               K0=NSTRF(25)
               N=NINTER+NRWALL+NRBODY
               DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K5=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     .            +2*NSTRF(K0+7)+2*NSTRF(K0+8)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTION_C(JFT,JLT,NFT,NSTRF(K0+9),NSTRF(K0+3),
     2           NSTRF(K0+4),NSTRF(K0+5),NSTRF(K5),X,V,VR,FSAV(1,N),
     3           IXC        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I) ,
     4           FX       ,FY ,FZ ,MX ,MY ,MZ ,
     5           NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     6           NSTRF(K2),MS,
     8           XSEC(1,1,I) ,pFBSAV6,IPARSENS)
                K0 = NSTRF(K0+24)
               ENDDO
          ENDIF
             IF(NEXMAD/=0.AND.IOFC/=0)THEN
               IMADFSH4=1
               DO J=1,IOFC
                II=INDXOF(J)+JFT-1+NFT
                MADFAIL(II)=1
               ENDDO
             ENDIF
C----6---------------------------------------------------------------7--
        ELSEIF(ITY==7)THEN
          IOFC = 0
          IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .       CALL SENSOR_ENERGY_PART(IPARTTG(NF1) ,SUBSET ,IPRI,SENSORS,ITASK)
          IF (IGTYP >= 29.AND.IGTYP<=31) THEN  !user prop
            CALL USH_FORCE3(IGTYP,ELBUF_TAB(NG),                          
     1             NUMNOD,  NPARG,   NPART,   NUMMAT,    NPSAV, 
     2        IPARG(1,NG),  NIXTG,IXTG(1,NF1),IPARTTG(NF1),  3, 
     3              ITASK,    IPRI,USERL_AVAIL,   PM,      NEL, 
     4                  X,      V,      VR,        A,       AR, 
     5                  D,  STIFN,   STIFR,THKE(NUMELC+NF1),IADTG(1,NF1), 
     6               FSKY,PARTSAV,     DT1,     DT2T,      TT , 
     7           FX(1:MVSIZ,1:3),FY(1:MVSIZ,1:3),FZ(1:MVSIZ,1:3),MX(1:MVSIZ,1:3), MY(1:MVSIZ,1:3), 
     8           MZ(1:MVSIZ,1:3),SENSORS,IPARIT,LSKY,  NODADT)
          ELSEIF (ICNOD == 6) THEN
            NFT1 = NF1-NUMELTG+NUMELTG6
            CALL CDK6FORC3(TIMERS,
     1        ELBUF_TAB(NG),   JFT,                  JLT,             PM,
     2        IXTG(1,NF1),     X,                    A,               AR,
     3        V,               VR,                   FAILWAVE,        NVC,
     4        MLW,             GEO,                  TF,              NPC,
     5        BUFMAT,          PARTSAV,              DT2T,            NELTST,
     6        ITYPTST,         STIFN,                STIFR,           FSKY,
     7        IADTG(1,NF1),    GROUP_PARAM_TAB(NG),  TANI(1+6*NUMELC),OFFSET,
     8        IPARTTG(NF1),    THKE(NUMELC+NF1),     FX(1,1),         FX(1,2),
     9        FX(1,3),         FY(1,1),              FY(1,2),         FY(1,3),
     A        FZ(1,1),         FZ(1,2),              FZ(1,3),         FX(1,4),
     B        FX(1,5),         FX(1,6),              FY(1,4),         FY(1,5),
     C        FY(1,6),         FZ(1,4),              FZ(1,5),         FZ(1,6),
     D        MAT_ELEM,        NEL,                  ISTRA,           ISH3N,
     E        ITHK,            IOFC,                 IPLA,            NFT,
     F        ISMSTR,          NPT,                  KFTS,            IXTG1(1,NFT1),
     G        IADTG1(1,NFT1),  IGEO,                 IPM,             IFAILURE,
     H        IEXPAN,          GRESAV,               GRTH,            IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1),
     I        MSTG(NF1),       DMELTG(NF1),          JSMS,            TABLE,
     J        IPARG(1,NG),     SENSORS,              PTG(1,NF1),      JTHE,
     K        CONDN,           CONDNSKY,             ISUBSTACK,       STACK,
     L        ITASK,           DRAPE_SH3N,           IPRI,            NLOC_DMG,
     M        DRAPEG%INDX_SH3N,IGRE,                 JTUR,            DT  ,
     N        NCYCLE,          SNPC,                 STF ,           GLOB_THERM ,
     O        NXLAYMAX,        IDEL7NOK,             USERL_AVAIL,    MAXFUNC,       
     P        SBUFMAT     ,
     X        IPART       ,        LIPART1)
          ELSE

            IF (ISH3N == 30) THEN
              CALL CDKFORC3(TIMERS,
     1          ELBUF_TAB(NG),                         JFT,                  JLT,             PM,
     2          IXTG(1,NF1),                           X,                    A,               AR,
     3          V,                                     VR,                   FAILWAVE,        NVC,
     4          MLW,                                   GEO,                  TF,              NPC,
     5          BUFMAT,                                PARTSAV,              DT2T,            NELTST,
     6          ITYPTST,                               STIFN,                STIFR,           FSKY,
     7          IADTG(1,NF1),                          ITAB,                 TANI(1+6*NUMELC),IPARTTG(NF1),
     8          THKE(NUMELC+NF1),                      GROUP_PARAM_TAB(NG),  FX(1,1),         FX(1,2),
     9          FX(1,3),                               FY(1,1),              FY(1,2),         FY(1,3),
     A          FZ(1,1),                               FZ(1,2),              FZ(1,3),         MX(1,1),
     B          MX(1,2),                               MX(1,3),              MY(1,1),         MY(1,2),
     C          MY(1,3),                               MZ(1,1),              MZ(1,2),         MZ(1,3),
     D          MAT_ELEM,                              NEL,                  ISTRA,           ISH3N,
     E          ITHK,                                  IOFC,                 IPLA,            NFT,
     F          ISMSTR,                                NPT,                  KFTS,            IGEO,
     G          IPM,                                   IFAILURE,             GRESAV,          GRTH,
     H          IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1),MSTG(NF1),            DMELTG(NF1),     JSMS,
     I          TABLE,                                 IPARG(1,NG),          SENSORS,         PTG(1,NF1),
     J          JTHE,                                  CONDN,                CONDNSKY,        ISUBSTACK,
     K          STACK,                                 ITASK,                DRAPE_SH3N,      IPRI,
     L          NLOC_DMG,                              DRAPEG%INDX_SH3N,     IGRE,            JTUR,
     M          DT,                                    NCYCLE,               SNPC,             STF,       
     M          GLOB_THERM  ,                          NXLAYMAX,             IDEL7NOK,        USERL_AVAIL,
     M          MAXFUNC,                               SBUFMAT  ,
     X          IPART      ,        LIPART1)
     
            ELSE
              CALL C3FORC3(TIMERS,
     1          ELBUF_TAB(NG),         JFT,                                   JLT,               PM,
     2          IXTG(1,NF1),           X,                                     A,                 AR,
     3          V,                     VR,                                    FAILWAVE,          NVC,
     4          MLW,                   GEO,                                   TF,                NPC,
     5          BUFMAT,                PARTSAV,                               DT2T,              NELTST,
     6          ITYPTST,               STIFN,                                 STIFR,             FSKY,
     7          IADTG(1,NF1),          ITAB,                                  TANI(1+6*NUMELC),  OFFSET,
     8          IPARTTG(NF1),          THKE(NUMELC+NF1),                      FX(1,1),           FX(1,2),
     9          FX(1,3),               FY(1,1),                               FY(1,2),           FY(1,3),
     A          FZ(1,1),               FZ(1,2),                               FZ(1,3),           MX(1,1),
     B          MX(1,2),               MX(1,3),                               MY(1,1),           MY(1,2),
     C          MY(1,3),               MZ(1,1),                               MZ(1,2),           MZ(1,3),
     D          GROUP_PARAM_TAB(NG),   MAT_ELEM,                              NEL,               ISTRA,
     E          ISH3N,                 XEDGE3N,                               ITHK,              IOFC,
     F          IPLA,                  NFT,                                   ISMSTR,            NPT,
     G          KFTS,                  FZERO(1,1,NF1+NUMELC),                 IGEO,              IPM,
     H          IFAILURE,              ITASK,                                 JTHE,              TEMP,
     I          FTHE,                  FTHESKY,                               IEXPAN,            GRESAV,
     J          GRTH,                  IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1),MSTG(NF1),         DMELTG(NF1),
     K          JSMS,                  TABLE,                                 IPARG(1,NG),       IXFEM,
     L          SENSORS,               PTG(1,NF1),                            IBORDNODE,         ELCUTC(1,NF1+ITG3-1),
     M          INOD_CRK,              IEL_CRK(ITG3),                         NODENR,            IADC_CRK(ITG1),
     N          NODEDGE,               CRKNODIAD,                             KNOD2ELC,          CONDN,
     O          CONDNSKY,              STACK,                                 ISUBSTACK,         XFEM_TAB(NG,1:NXEL),
     P          CRKEDGE,               DRAPE_SH3N,                            IPRI,              NLOC_DMG,
     Q          XDP,                   DRAPEG%INDX_SH3N,                      IGRE,              JTUR,
     R          DT,                    SNPC,                                  STF,               GLOB_THERM,
     S          IDEL7NOK,              USERL_AVAIL,                           MAXFUNC,           SBUFMAT ,
     X          IPART      ,        LIPART1  )

                IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
                  DO IXEL=1,NXEL
                    CALL C3FORC3_CRK(TIMERS,
     1                XFEM_TAB(NG,IXEL), JFT,                  JLT,              PM,
     2                IXTG(1,NF1),       X,                    A,                AR,
     3                V,                 VR,                   FAILWAVE,         NVC,
     4                MLW,               GEO,                  TF,               NPC,
     5                BUFMAT,            PARTSAV,              DT2T,             NELTST,
     6                ITYPTST,           STIFN,                STIFR,            FSKY,
     7                CRKSKY,            IADTG(1,NF1),         TANI(1+6*NUMELC), OFFSET,
     8                IPARTTG(NF1),      THKE(NUMELC+NF1),     FX(1,1),          FX(1,2),
     9                FX(1,3),           FY(1,1),              FY(1,2),          FY(1,3),
     A                FZ(1,1),           FZ(1,2),              FZ(1,3),          MX(1,1),
     B                MX(1,2),           MX(1,3),              MY(1,1),          MY(1,2),
     C                MY(1,3),           MZ(1,1),              MZ(1,2),          MZ(1,3),
     D                KFTS,              GROUP_PARAM_TAB(NG),  MAT_ELEM,         NEL,
     E                ISTRA,             ISH3N,                ITHK,             IOFC,
     F                IPLA,              NFT,                  ISMSTR,           FZERO(1,1,NF1+NUMELC),
     G                IGEO,              IPM,                  IFAILURE,         ITASK,
     H                JTHE,              TEMP,                 FTHE,             FTHESKY,
     I                IEXPAN,            GRESAV,               GRTH,             IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1),
     J                MSTG(NF1),         DMELTG(NF1),          JSMS,             TABLE,
     K                IPARG(1,NG),       SENSORS,              PTG(1,NF1),       IXFEM,
     L                INOD_CRK,          IEL_CRK(ITG3),        IADC_CRK(ITG1),   ELCUTC(1,NF1+ITG3-1),
     M                IXEL,              STACK,                ISUBSTACK,        UXINT_MEAN,
     N                UYINT_MEAN,        UZINT_MEAN,           NLEVXF,           NODEDGE,
     O                CRKEDGE,           DRAPE_SH3N,           IPRI,             NLOC_DMG,
     P                DRAPEG%INDX_SH3N,  IGRE,                 DT  ,             NCYCLE  ,
     Q                SNPC,              STF,                 GLOB_THERM   ,     IDEL7NOK,
     A                USERL_AVAIL,       MAXFUNC,              SBUFMAT ,
     X                IPART      ,        LIPART1   )
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
!----------------------------------------------------------------------------------------
           IF(NSECT>0)THEN
             K0=NSTRF(25)
             N=NINTER+NRWALL+NRBODY
             DO I=1,NSECT
               N=N+1
               K2=K0+30+NSTRF(K0+14)
               K9=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1          +2*NSTRF(K0+7)+2*NSTRF(K0+8)+2*NSTRF(K0+9)
     2          +2*NSTRF(K0+10)+2*NSTRF(K0+11)+2*NSTRF(K0+12)
               IPARSENS=0
               ISECT=0
               IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
               CALL SECTION_3N(JFT,JLT,NFT,NSTRF(K0+13),NSTRF(K0+3),
     2         NSTRF(K0+4),NSTRF(K0+5),NSTRF(K9),X,V,VR,FSAV(1,N),
     3         IXTG        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I) ,
     4         FX ,FY ,FZ  ,MX  ,MY,  MZ,
     5         NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     6         NSTRF(K2),MS,
     8         XSEC(1,1,I) , pFBSAV6,IPARSENS)
                  K0=NSTRF(K0+24)
             ENDDO
           ENDIF
        ENDIF
        IF (IDDW>0) CALL STOPTIMEG(NG)

  250   CONTINUE
      END DO
!$OMP END DO
C----6---------------------------------------------------------------7---------8
      RETURN
      END


!      SUBROUTINE FORINTC_PREPARE_GPU(TIMERS, GPU,  SHELLS,
!     1    PM        ,GEO       ,X         ,A         ,AR        ,
!     2    V         ,VR        ,MS        ,IN        ,NLOC_DMG  ,
!     3    WA        ,STIFN     ,STIFR     ,FSKY      ,CRKSKY    ,
!     4    TF        ,BUFMAT    ,PARTSAV   ,D         ,MAT_ELEM  ,
!     5    DR        ,EANI      ,TANI      ,FANI      ,
!     6    FSAV      ,SENSORS   ,SKEW      ,FAILWAVE  ,
!     7    DT2T      ,THKE      ,BUFGEO    ,IADC      ,IADTG      ,
!     8    IPARG     ,NPC       ,IXC       ,IXTG      ,NELTST    ,
!     A    IPARI     ,ITYPTST   ,NSTRF     ,
!     B    IPART     ,IPARTC    ,IPARTTG   ,SECFCUM   ,
!     D    FSAVD     ,GROUP_PARAM_TAB,
!     F    FZERO     ,IXTG1     ,IADTG1    ,IGEO      ,IPM       ,
!     G    MADFAIL   ,XSEC      ,ITASK     ,MCP       ,
!     H    TEMP      ,FTHE      ,FTHESKY   ,
!     I    MS_PLY    ,ZI_PLY    ,INOD_PXFEM,XEDGE4N   ,XEDGE3N   ,
!     J    IEL_PXFEM ,IADC_PXFEM,IGROUC    ,NGROUC    ,GRESAV    ,
!     K    GRTH      ,IGRTH     ,MSTG      ,DMELTG    ,MSC       ,
!     L    DMELC     ,TABLE     ,KNOD2ELC  ,PTG       ,MSZ2      ,
!     M    INOD_CRK  ,IEL_CRK   ,IADC_CRK  ,ELCUTC    ,NODENR    ,
!     N    IBORDNODE ,NODEDGE   ,CRKNODIAD ,ELBUF_TAB ,
!     O    XFEM_TAB  ,CONDN     ,CONDNSKY  ,CRKEDGE   ,
!     P    STACK     ,ITAB      ,GLOB_THERM,
!     S    DRAPE_SH4N  ,DRAPE_SH3N  ,SUBSET    ,XDP       ,VPINCH,
!     T    APINCH    ,STIFPINCH ,DRAPEG,OUTPUT ,DT        ,
!     Y    SNPC      ,  STF, USERL_AVAIL,MAXFUNC,
!     U    SBUFMAT   ,IPARIT    ,LSKY      ,NODADT )
!C-----------------------------------------------
!C   M o d u l e s
!C-----------------------------------------------
!      USE TIMER_MOD
!      USE TABLE_MOD
!      USE MAT_ELEM_MOD
!      USE CRACKXFEM_MOD
!      USE STACK_MOD
!      USE FAILWAVE_MOD
!      USE NLOCAL_REG_MOD
!      USE GROUPDEF_MOD
!      USE PINCHTYPE_MOD
!      USE SENSOR_MOD
!      USE RUPTURE_MOD
!      USE DRAPE_MOD
!      USE OUTPUT_MOD
!      USE ELBUFDEF_MOD
!      USE DT_MOD
!      use glob_therm_mod
!      use element_mod , only : nixc,nixtg
!      use ush_force3_mod , only : ush_force3
!      use shell_gpu_mod, only : SHELLS_,
!     .  shell_gpu_data_create, shell_gpu_allocate,
!     .  shell_gpu_set_mat_params
!C-----------------------------------------------
!C   I m p l i c i t   T y p e s
!C-----------------------------------------------
!#include      "implicit_f.inc"
!#include      "comlock.inc"
!C-----------------------------------------------
!C   G l o b a l   P a r a m e t e r s
!C-----------------------------------------------
!#include      "mvsiz_p.inc"
!#include      "param_c.inc"
!C-----------------------------------------------
!C   C o m m o n   B l o c k s
!C-----------------------------------------------
!#include      "com01_c.inc"
!#include      "com04_c.inc"
!#include      "com06_c.inc"
!#include      "com08_c.inc"
!#include      "com_xfem1.inc"
!#include      "vect01_c.inc"
!#include      "scr06_c.inc"
!#include      "scr07_c.inc"
!#include      "scr17_c.inc"
!#include      "task_c.inc"
!#include      "couple_c.inc"
!#include      "impl1_c.inc"
!#include      "stati_c.inc"
!C-----------------------------------------------
!C   D u m m y   A r g u m e n t s
!C-----------------------------------------------
!      TYPE(SHELLS_), INTENT(INOUT) :: SHELLS
!      INTEGER , INTENT(IN) :: GPU
!      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
!      INTEGER,INTENT(IN)    :: USERL_AVAIL ! Flag for User libraries availability
!      INTEGER,INTENT(IN)    :: MAXFUNC     ! Maximum number of functions
!      INTEGER,INTENT(IN)    :: SBUFMAT     ! Size of Bufmat
!      INTEGER,INTENT(IN)    :: STF         ! Size of TF
!      INTEGER,INTENT(IN)    :: SNPC        ! Size of NPC
!      INTEGER,INTENT(IN)    :: IPARIT, LSKY, NODADT
!      INTEGER IXC(NIXC,*), IXTG(NIXTG,*), IGEO(NPROPGI,*), IPM(NPROPMI,*),
!     .   NPC(*), IPARG(NPARG,*), IPARI(NPARI,*), 
!     .   NSTRF(*), IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
!     .   IADC(4,*), IADTG(3,*),NELTST,
!     .   ITYPTST,IXTG1(4,*),XEDGE4N(4,*),XEDGE3N(3,*),
!     .   IADTG1(3,*),MADFAIL(*),ITASK,
!     .   INOD_PXFEM(*),IEL_PXFEM(*) ,IADC_PXFEM(4,*), IGROUC(*),
!     .   NGROUC,GRTH(*),IGRTH(*),KNOD2ELC(*),
!     .   INOD_CRK(*),IEL_CRK(*),IADC_CRK(*),ELCUTC(2,*),
!     .   NODENR(*),IBORDNODE(*),NODEDGE(2,*),CRKNODIAD(*),
!     .   ITAB(*)
!C     REAL
!      my_real
!     .   X(3,*)    ,D(3,*)  ,V(3,*)   ,VR(3,*),
!     .   MS(*)     ,IN(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),
!     .   GEO(NPROPG,*),BUFMAT(*) ,TF(STF) ,FSAV(NTHVKI,*) ,
!     .   WA(*), THKE(*),
!     .   A(3,*)    ,AR(3,*) ,FANI(3,*) ,PARTSAV(NPSAV,*)    ,
!     .   STIFN(*) ,STIFR(*),FSKY(*) ,
!     .   DR(3,*) ,TANI(*),EANI(*),
!     .   BUFGEO(*) ,DT2T, SECFCUM(7,NUMNOD,NSECT),
!     .   FSAVD(NTHVKI,*),
!     .   FZERO(3,4,(NUMELC+NUMELTG)),XSEC(4,3,NSECT),
!     .   MCP(*),TEMP(*),FTHE(*),FTHESKY(*),
!     .   MS_PLY(*), ZI_PLY(*),GRESAV(*),
!     .   MSTG(*), DMELTG(*), MSC(*), DMELC(*),CONDN(*),CONDNSKY(*),
!     .   PTG(3,*),MSZ2(*),APINCH(3,*),STIFPINCH(*),VPINCH(3,*)
!      DOUBLE PRECISION :: XDP(3,*)
!      TYPE (TTABLE) TABLE(*)
!      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
!      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
!      TYPE (XFEM_EDGE_)   , DIMENSION(*)           :: CRKEDGE
!      TYPE (XFEM_SKY_)    , DIMENSION(*) :: CRKSKY
!      TYPE (STACK_PLY) :: STACK 
!      TYPE (FAILWAVE_STR_) :: FAILWAVE 
!      TYPE (NLOCAL_STR_)   :: NLOC_DMG 
!      TYPE (GROUP_PARAM_) , DIMENSION(NGROUP) :: GROUP_PARAM_TAB
!      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
!      TYPE (DRAPE_)    :: DRAPE_SH4N(NUMELC_DRAPE),DRAPE_SH3N(NUMELTG_DRAPE)
!      TYPE (DRAPEG_)               :: DRAPEG
!      TYPE (SENSORS_) ,INTENT(INOUT) ,TARGET :: SENSORS
!      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
!      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
!      TYPE (DT_) ,INTENT(IN) :: DT
!      type (glob_therm_) ,intent(inout) :: glob_therm
!C-----------------------------------------------
!C   L o c a l   V a r i a b l e s
!C-----------------------------------------------
!      DOUBLE PRECISION, POINTER :: pFBSAV6
!      INTEGER INDXOF(MVSIZ)
!      INTEGER I,II,J,N, NG, NVC, MLW, JFT, JLT,ISOLNOD,ITHK,IPLA,
!     .    K2, NF1,IPRI, OFFSET,TYP,
!     .   K0, K5, K9, NSG, NEL, KFTS,IOFC, ISTRA,
!     .   ICNOD,NFT1,ISENS_ENERGY,
!     .   IEXPAN, IG,ITG1,ITG2,ITG3,NLEVXF,ISEATBELT
!      INTEGER IPARSENS,ISECT
!      INTEGER ISH3N,ISHPLYXFEM,IXFEM
!      INTEGER IXEL,ACTIFXFEM,ISUBSTACK
!      my_real
!     .   FX(MVSIZ,20),FY(MVSIZ,20),FZ(MVSIZ,20),
!     .   MX(MVSIZ,4),MY(MVSIZ,4),MZ(MVSIZ,4)
!
!
!      integer :: law2_counter(NGROUP)
!      integer :: nod_counter(NGROUP)
!      logical, dimension(:), allocatable :: node_mask
!      integer, dimension(:), allocatable :: glob_2_gpu, gpu_2_glob, node_local_id
!      integer :: SUPER_GROUP_ID(NGROUP)
!      integer :: PREVIOUS_SUPER_GROUP_ID
!      integer :: IPARG_PREVIOUS(NPARG)
!      integer :: SU,nod,imat,ipid
!      integer :: elem_counter(NGROUP)
!      integer :: IT, gpu_offset, IJ1, IJ2, IJ3, IJ4, IJ5
!      integer :: glob_id
!C======================================================================|
!
!      allocate(node_mask(NUMNOD))
!      allocate(glob_2_gpu(NUMNOD))
!      allocate(gpu_2_glob(NUMNOD))
!      allocate(node_local_id(NUMNOD))
!      IPARG_PREVIOUS = 0
!      glob_2_gpu = 0
!      gpu_2_glob = 0
!      node_mask = .false.
!      ITG1 = 1+4*ECRKXFEC
!      ITG2 = 2*NUMELC
!      ITG3 = 1+NUMELC
!      IPRI = 0
!      ISENS_ENERGY = 0
!      law2_counter = 0
!      nod_counter = 0
!      elem_counter = 0
!      DO I=1,SENSORS%NSENSOR
!        TYP = SENSORS%SENSOR_TAB(I)%TYPE
!        IF (TYP == 14) ISENS_ENERGY = 1 ! save internal/kinetic energy (PARTSAV)
!      ENDDO ! DO I=1,NSENSOR
!      PREVIOUS_SUPER_GROUP_ID = 0
!      DO IG = 1, NGROUC
!            NG = IGROUC(IG)
!            IF (IPARG(1, NG) == 151) THEN !  Bypass law 151
!               CYCLE
!            ENDIF
!C---------temporarily used to avoid pass KTBUF_STR everywhere
!            NG_IMP = NG
!            IF(IPARG(8,NG)==1) CYCLE
!            ITY   =IPARG(5,NG)
!C            IF(ITY/=3.AND.ITY/=7)GOTO 250
!            OFFSET  = 0
!            MLW     = IPARG(1,NG)
!C MLW= 0 ----> void
!C MLW = 13 ----> rigid material
!            IF (MLW == 0 .OR. MLW == 13) CYCLE
!            NEL     = IPARG(2,NG)
!            NFT     = IPARG(3,NG)
!            NPT     = IPARG(6,NG)
!            JALE    = IPARG(7,NG)
!            ISMSTR  = IPARG(9,NG)
!            NSG     = IPARG(10,NG)
!            JEUL    = IPARG(11,NG)
!            JTUR    = IPARG(12,NG)
!            JTHE    = IPARG(13,NG)
!            JLAG    = IPARG(14,NG)
!            ISTRA   = IPARG(44,NG)
!            NVC     = IPARG(19,NG)
!            JMULT   = IPARG(20,NG)
!            JHBE    = IPARG(23,NG)
!            ISH3N   = IPARG(23,NG)
!            JIVF    = IPARG(24,NG)
!            JPOR    = IPARG(27,NG)
!            ITHK    = IPARG(28,NG)
!            ISOLNOD = IPARG(28,NG)
!            IPLA    = IPARG(29,NG)
!            ICNOD   = IPARG(11,NG)
!            IREP    = IPARG(35,NG)
!            IINT    = IPARG(36,NG)
!            JCVT    = IPARG(37,NG)
!            IGTYP   = IPARG(38,NG)
!            ISORTH  = IPARG(42,NG)
!            ISORTHG = ISORTH
!            ISRAT   = IPARG(40,NG)
!            ISROT   = IPARG(41,NG)
!            IFAILURE= IPARG(43,NG)
!            KFTS    = IPARG(30,NG)
!            JCLOSE  = IPARG(33,NG)
!            ICSEN   = IPARG(39,NG)
!            IEXPAN  = IPARG(49,NG)
!            ISHPLYXFEM  = IPARG(50,NG)
!            IGRE    = IPARG(51,NG)
!            JSMS    = IPARG(52,NG)
!            IXFEM   = IPARG(54,NG)
!            NLEVXF  = IPARG(65,NG)
!            ACTIFXFEM=IPARG(70,NG)
!            ISUBSTACK=IPARG(71,NG)
!            ISEATBELT=IPARG(91,NG)
!            IF (ITY == 3  ! 4-node shell
!     .           .AND. MLW == 2  ! material law 2 (Johnson-Cook)
!     .           .AND. IPARG(8,NG) /= 1  ! group active
!     .           .AND. NPT > 0  ! through-thickness integration (not global)
!     .           .AND. JHBE < 11  ! standard formulation (not BATOZ, not QEPH)
!     .           .AND. IGTYP < 29  ! not user property
!     .           .AND. IXFEM == 0  ! no XFEM
!     .           .AND. IFAILURE == 0  ! no failure model (simplify for mockup)
!     .           .AND. NSECT == 0  ! no section (simplify for mockup)
!     .           .AND. NEXMAD == 0  ! no explicit mass scaling (simplify for mockup)
!     .           .AND. ICRACK3D == 0 .AND. IXFEM == 0 .AND. ACTIFXFEM == 0 
!     .           .AND. NSLIPRING == 0 .AND. ISEATBELT == 0  ! no slipring/seatbelt (simplify for mockup)
!     .           .AND. GPU == 1  ! GPU enabled
!     .           .AND. ISUBSTACK == 0 .AND. ISENS_ENERGY == 0) THEN
!            IF(ALL(IPARG(6:71,NG) == IPARG_PREVIOUS(6:71))) THEN
!             SUPER_GROUP_ID(NG) = PREVIOUS_SUPER_GROUP_ID
!            ELSE
!             PREVIOUS_SUPER_GROUP_ID = PREVIOUS_SUPER_GROUP_ID + 1
!             SUPER_GROUP_ID(NG) = PREVIOUS_SUPER_GROUP_ID
!             node_mask = .false.
!            ENDIF
!            IPARG_PREVIOUS = IPARG(:,NG) 
!            IF(.TRUE.) THEN
!              LFT   = 1
!              LLT   = MIN(NVSIZ,NEL)
!              MTN   = MLW
!              JFT=LFT
!              JLT=LLT
!              NF1 = NFT+1
!              SU = SUPER_GROUP_ID(NG)
!              law2_counter(SU) = law2_counter(SU) + NEL
!              do i=1,NEL
!                do j=1,4
!                  if (node_mask(IXC(1+j,NF1+i)) .eqv. .false.) then
!                    nod_counter(SU) = nod_counter(SU) + 1
!!                   gpu_2_glob(node_counter) = IXC(1+j,NF1+i)
!!                   glob_2_gpu(IXC(1+j,NF1+i)) = node_counter
!                    node_mask(IXC(1+j,NF1+i)) = .true.
!                  end if
!                end do
!              end do
!            ENDIF
!C----6---------------------------------------------------------------7---------8
!        ENDIF
!      END DO
!
!
!
!
!      ALLOCATE(SHELLS%LAW2(PREVIOUS_SUPER_GROUP_ID))
!      PREVIOUS_SUPER_GROUP_ID = 0
!      DO IG = 1, NGROUC
!            NG = IGROUC(IG)
!            NEL     = IPARG(2,NG)
!            NFT     = IPARG(3,NG)
!            NPT     = IPARG(6,NG)
! 
!            IF(SUPER_GROUP_ID(NG) == 0) CYCLE ! skip groups not selected for GPU
!            IF(SUPER_GROUP_ID(NG) /= PREVIOUS_SUPER_GROUP_ID) THEN
!              PREVIOUS_SUPER_GROUP_ID = SUPER_GROUP_ID(NG)
!              SU = SUPER_GROUP_ID(NG)
!              nod = nod_counter(SU)
!              nod_counter(SU) = 0 ! reset for next super group
!              SHELLS%LAW2(SU)%numelc = law2_counter(SU)
!              SHELLS%LAW2(SU)%numnod = nod 
!              allocate(SHELLS%LAW2(SU)%gpu_2_glob(nod))
!              SHELLS%LAW2(SU)%npt = IPARG(6,NG)
!              SHELLS%LAW2(SU)%ismstr = IPARG(9,NG)
!              SHELLS%LAW2(SU)%ithk = IPARG(28,NG)
!              SHELLS%LAW2(SU)%handle = shell_gpu_data_create()
!              call shell_gpu_allocate(
!     .          shells%LAW2(SU)%handle,
!     .          SHELLS%LAW2(SU)%numelc,
!     .          SHELLS%LAW2(SU)%numnod,
!     .          SHELLS%LAW2(SU)%npt,
!     .          SHELLS%LAW2(SU)%ismstr,
!     .          SHELLS%LAW2(SU)%ithk)
!C
!C             Retrieve material index and property index
!C
!              imat = ELBUF_TAB(NG)%bufly(1)%imat
!              NF1  = IPARG(3,NG) + 1
!              ipid = IXC(6,NF1)
!C
!C             Elastic properties  (cf. sigeps02c / mat_param)
!C
!              SHELLS%LAW2(SU)%e_young = MAT_ELEM%mat_param(imat)%young
!              SHELLS%LAW2(SU)%nu      = MAT_ELEM%mat_param(imat)%nu
!              SHELLS%LAW2(SU)%g_shear = MAT_ELEM%mat_param(imat)%shear
!              SHELLS%LAW2(SU)%a11     = MAT_ELEM%mat_param(imat)%young
!     .                                / (ONE - MAT_ELEM%mat_param(imat)%nu**2)
!              SHELLS%LAW2(SU)%a12     = SHELLS%LAW2(SU)%a11
!     .                                * MAT_ELEM%mat_param(imat)%nu
!C
!C             Johnson-Cook / Zerilli-Armstrong hardening (uparam)
!C
!              SHELLS%LAW2(SU)%ca      = MAT_ELEM%mat_param(imat)%uparam(1)
!              SHELLS%LAW2(SU)%cb      = MAT_ELEM%mat_param(imat)%uparam(2)
!              SHELLS%LAW2(SU)%cn      = MAT_ELEM%mat_param(imat)%uparam(3)
!              SHELLS%LAW2(SU)%epmx    = MAT_ELEM%mat_param(imat)%uparam(4)
!              SHELLS%LAW2(SU)%ymax    = MAT_ELEM%mat_param(imat)%uparam(5)
!              SHELLS%LAW2(SU)%cc      = MAT_ELEM%mat_param(imat)%uparam(6)
!              SHELLS%LAW2(SU)%epdr    = MAT_ELEM%mat_param(imat)%uparam(7)
!              SHELLS%LAW2(SU)%fisokin = MAT_ELEM%mat_param(imat)%uparam(8)
!C
!C             Thermal parameters
!C
!              SHELLS%LAW2(SU)%rhocp   = MAT_ELEM%mat_param(imat)%therm%rhocp
!              SHELLS%LAW2(SU)%tref    = MAT_ELEM%mat_param(imat)%therm%tref
!              SHELLS%LAW2(SU)%tmelt   = MAT_ELEM%mat_param(imat)%therm%tmelt
!C
!C             Formulation-dependent: m_exp / z3 / z4  (cf. sigeps02c)
!C
!              SHELLS%LAW2(SU)%iform   = MAT_ELEM%mat_param(imat)%iparam(1)
!              IF (SHELLS%LAW2(SU)%iform == 1) THEN
!C               Zerilli-Armstrong
!                SHELLS%LAW2(SU)%z3    = MAT_ELEM%mat_param(imat)%uparam(10)
!                SHELLS%LAW2(SU)%z4    = MAT_ELEM%mat_param(imat)%uparam(11)
!                SHELLS%LAW2(SU)%m_exp = ONE
!              ELSE
!C               Johnson-Cook
!                SHELLS%LAW2(SU)%z3    = ZERO
!                SHELLS%LAW2(SU)%z4    = ZERO
!                SHELLS%LAW2(SU)%m_exp = MAT_ELEM%mat_param(imat)%uparam(10)
!              ENDIF
!C
!C             Integer flags (iparam)
!C
!              SHELLS%LAW2(SU)%icc     = MAT_ELEM%mat_param(imat)%iparam(2)
!              SHELLS%LAW2(SU)%vp      = MAT_ELEM%mat_param(imat)%iparam(3)
!C
!C             Strain-rate filter coefficient (PM(9,imat) = 2*pi*fcut)
!C
!              SHELLS%LAW2(SU)%asrate  = PM(9,imat)
!C
!C             Density, sound speed, shear correction factor
!C
!              SHELLS%LAW2(SU)%rho      = MAT_ELEM%mat_param(imat)%rho
!              SHELLS%LAW2(SU)%ssp      = PM(27,imat)
!              SHELLS%LAW2(SU)%shf_coef = GEO(38,ipid)
!C
!C             Plasticity algorithm
!C
!              SHELLS%LAW2(SU)%ipla    = IPARG(29,NG)
!
!C             hourglass parameters (from GEO property array)
!              SHELLS%LAW2(SU)%h1   = GEO(13,ipid)
!              SHELLS%LAW2(SU)%h2   = GEO(14,ipid)
!              SHELLS%LAW2(SU)%h3   = GEO(15,ipid)
!              SHELLS%LAW2(SU)%srh1 = GEO(18,ipid)
!              SHELLS%LAW2(SU)%srh2 = GEO(19,ipid)
!              SHELLS%LAW2(SU)%srh3 = GEO(20,ipid)
!
!              call shell_gpu_set_mat_params(
!     .          SHELLS%LAW2(SU)%handle,
!     .          SHELLS%LAW2(SU)%e_young,
!     .          SHELLS%LAW2(SU)%nu,
!     .          SHELLS%LAW2(SU)%g_shear,
!     .          SHELLS%LAW2(SU)%a11,
!     .          SHELLS%LAW2(SU)%a12,
!     .          SHELLS%LAW2(SU)%ca,
!     .          SHELLS%LAW2(SU)%cb,
!     .          SHELLS%LAW2(SU)%cn,
!     .          SHELLS%LAW2(SU)%cc,
!     .          SHELLS%LAW2(SU)%epdr,
!     .          SHELLS%LAW2(SU)%epmx,
!     .          SHELLS%LAW2(SU)%ymax,
!     .          SHELLS%LAW2(SU)%m_exp,
!     .          SHELLS%LAW2(SU)%fisokin,
!     .          SHELLS%LAW2(SU)%rhocp,
!     .          SHELLS%LAW2(SU)%tref,
!     .          SHELLS%LAW2(SU)%tmelt,
!     .          SHELLS%LAW2(SU)%asrate,
!     .          SHELLS%LAW2(SU)%rho,
!     .          SHELLS%LAW2(SU)%ssp,
!     .          SHELLS%LAW2(SU)%shf_coef,
!     .          SHELLS%LAW2(SU)%ipla,
!     .          SHELLS%LAW2(SU)%vp,
!     .          SHELLS%LAW2(SU)%iform,
!     .          SHELLS%LAW2(SU)%icc,
!     .          SHELLS%LAW2(SU)%z3,
!     .          SHELLS%LAW2(SU)%z4)
!
!              allocate(SHELLS%LAW2(SU)%n1(nod))
!              allocate(SHELLS%LAW2(SU)%n2(nod))
!              allocate(SHELLS%LAW2(SU)%n3(nod))
!              allocate(SHELLS%LAW2(SU)%n4(nod))
!              allocate(SHELLS%LAW2(SU)%xx(nod))
!              allocate(SHELLS%LAW2(SU)%xy(nod))
!              allocate(SHELLS%LAW2(SU)%xz(nod))
!              allocate(SHELLS%LAW2(SU)%vx(nod))
!              allocate(SHELLS%LAW2(SU)%vy(nod))
!              allocate(SHELLS%LAW2(SU)%vz(nod))
!              allocate(SHELLS%LAW2(SU)%vrx(nod))
!              allocate(SHELLS%LAW2(SU)%vry(nod))
!              allocate(SHELLS%LAW2(SU)%vrz(nod))
!              allocate(SHELLS%LAW2(SU)%fx(nod))
!              allocate(SHELLS%LAW2(SU)%fy(nod))
!              allocate(SHELLS%LAW2(SU)%fz(nod))
!              allocate(SHELLS%LAW2(SU)%mx(nod))
!              allocate(SHELLS%LAW2(SU)%my(nod))
!              allocate(SHELLS%LAW2(SU)%mz(nod))
!              allocate(SHELLS%LAW2(SU)%stifn(nod))
!              allocate(SHELLS%LAW2(SU)%stifr(nod))
!              !IP state arrays [NPT*NUMELC]
!              allocate(SHELLS%LAW2(SU)%sigxx(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%sigyy(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%sigxy(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%sigyz(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%sigzx(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%pla(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%epsd_ip(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%sigbakxx(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%sigbakyy(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%sigbakxy(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%tempel(NPT*SHELLS%LAW2(SU)%numelc))
!              allocate(SHELLS%LAW2(SU)%eint(2*SHELLS%LAW2(SU)%numelc))
!              SHELLS%LAW2(SU)%eint = 0.0d0
!              SHELLS%LAW2(SU)%sigxx = 0.0d0
!              SHELLS%LAW2(SU)%sigyy = 0.0d0
!              SHELLS%LAW2(SU)%sigxy = 0.0d0
!              SHELLS%LAW2(SU)%sigyz = 0.0d0
!              SHELLS%LAW2(SU)%sigzx = 0.0d0
!              SHELLS%LAW2(SU)%pla = 0.0d0
!              SHELLS%LAW2(SU)%epsd_ip = 0.0d0
!              SHELLS%LAW2(SU)%sigbakxx = 0.0d0
!              SHELLS%LAW2(SU)%sigbakyy = 0.0d0
!              SHELLS%LAW2(SU)%sigbakxy = 0.0d0
!              SHELLS%LAW2(SU)%tempel = 0.0d0
!              elem_counter(SU) = 0
!              node_mask = .false.
!              node_local_id = 0
!            endif
!C
!C           Copy per-IP state from ELBUF_TAB into flat GPU arrays
!C           Layout in lbuf%sig: [sigxx(1:NEL), sigyy(1:NEL), sigxy(1:NEL), sigyz(1:NEL), sigzx(1:NEL)]
!C           Layout in GPU arrays: (IT-1)*total_numelc + elem_counter + i
!C
!            NF1 = NFT + 1
!            do IT = 1, NPT
!              do i = 1, NEL
!                gpu_offset = (IT-1)*SHELLS%LAW2(SU)%numelc
!     .                     + elem_counter(SU) + i
!C               Stress components from lbuf%sig (5*NEL flat array)
!                IJ1 = 0
!                IJ2 = NEL
!                IJ3 = 2*NEL
!                IJ4 = 3*NEL
!                IJ5 = 4*NEL
!                if (ELBUF_TAB(NG)%bufly(1)%l_sig > 0) then
!                  SHELLS%LAW2(SU)%sigxx(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ1+i)
!                  SHELLS%LAW2(SU)%sigyy(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ2+i)
!                  SHELLS%LAW2(SU)%sigxy(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ3+i)
!                  SHELLS%LAW2(SU)%sigyz(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ4+i)
!                  SHELLS%LAW2(SU)%sigzx(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sig(IJ5+i)
!                endif
!C               Plastic strain
!                if (ELBUF_TAB(NG)%bufly(1)%l_pla > 0) then
!                  SHELLS%LAW2(SU)%pla(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%pla(i)
!                endif
!C               Strain rate
!                if (ELBUF_TAB(NG)%bufly(1)%l_epsd > 0) then
!                  SHELLS%LAW2(SU)%epsd_ip(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%epsd(i)
!                endif
!C               Back-stress components from lbuf%sigb (same layout as sig)
!                if (ELBUF_TAB(NG)%bufly(1)%l_sigb > 0) then
!                  SHELLS%LAW2(SU)%sigbakxx(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sigb(IJ1+i)
!                  SHELLS%LAW2(SU)%sigbakyy(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sigb(IJ2+i)
!                  SHELLS%LAW2(SU)%sigbakxy(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%sigb(IJ3+i)
!                endif
!C               Temperature
!                if (ELBUF_TAB(NG)%bufly(1)%l_temp > 0) then
!                  SHELLS%LAW2(SU)%tempel(gpu_offset) =
!     .              ELBUF_TAB(NG)%bufly(1)%lbuf(1,1,IT)%temp(i)
!                endif
!              end do
!            end do
!C           Internal energy from global buffer: GBUF%EINT(1:2*NEL)
!            do i = 1, NEL
!              SHELLS%LAW2(SU)%eint(elem_counter(SU)+i) =
!     .          ELBUF_TAB(NG)%gbuf%eint(i)
!              SHELLS%LAW2(SU)%eint(
!     .          SHELLS%LAW2(SU)%numelc+elem_counter(SU)+i) =
!     .          ELBUF_TAB(NG)%gbuf%eint(NEL+i)
!            end do
!C           Advance element counter for this super-group
!            elem_counter(SU) = elem_counter(SU) + NEL
!C           Build node mapping
!            do i=1,NEL
!              do j=1,4
!                if (node_mask(IXC(1+j,NF1+i)) .eqv. .false.) then
!                  nod_counter(SU) = nod_counter(SU) + 1
!                  SHELLS%LAW2(SU)%gpu_2_glob(nod_counter(SU)) = IXC(1+j,NF1+i)
!                  node_mask(IXC(1+j,NF1+i)) = .true.
!                  node_local_id(IXC(1+j,NF1+i)) = nod_counter(SU)
!                end if
!              end do
!            end do
!            do i=1,NEL
!                SHELLS%LAW2(SU)%n1(elem_counter(SU)+i) = node_local_id(IXC(2,NF1+i))
!                SHELLS%LAW2(SU)%n2(elem_counter(SU)+i) = node_local_id(IXC(3,NF1+i))
!                SHELLS%LAW2(SU)%n3(elem_counter(SU)+i) = node_local_id(IXC(4,NF1+i))
!                SHELLS%LAW2(SU)%n4(elem_counter(SU)+i) = node_local_id(IXC(5,NF1+i))
!            enddo
!            SHELLS%LAW2(SU)%numnod = nod_counter(SU)
!            ! gather X and V for nodes in this super-group
!            do i=1,nod_counter(SU)
!              glob_id = SHELLS%LAW2(SU)%gpu_2_glob(i)
!              SHELLS%LAW2(SU)%xx(i) = X(1,glob_id)
!              SHELLS%LAW2(SU)%xy(i) = X(2,glob_id)
!              SHELLS%LAW2(SU)%xz(i) = X(3,glob_id)
!              SHELLS%LAW2(SU)%vx(i) = V(1,glob_id)
!              SHELLS%LAW2(SU)%vy(i) = V(2,glob_id)
!              SHELLS%LAW2(SU)%vz(i) = V(3,glob_id)
!
!              if(iroddl == 1) then
!              SHELLS%LAW2(SU)%vrx(i) = VR(1,glob_id)
!              SHELLS%LAW2(SU)%vry(i) = VR(2,glob_id)
!              SHELLS%LAW2(SU)%vrz(i) = VR(3,glob_id)
!              endif
!            end do
! 
!
!      ENDDO
!
!
!      DEALLOCATE(node_mask)
!      DEALLOCATE(node_local_id)
!      DEALLOCATE(glob_2_gpu)
!      DEALLOCATE(gpu_2_glob)
!
!C
!C----6---------------------------------------------------------------7---------8
!      RETURN
!      END


