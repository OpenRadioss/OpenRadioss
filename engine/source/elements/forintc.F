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
!||    omp_get_thread_num          ../engine/source/engine/openmp_stub.F90
!||    seatbelt_reduction_factor   ../engine/source/tools/seatbelts/seatbelt_reduction_factor.F
!||    section_3n                  ../engine/source/tools/sect/section_3n.F
!||    section_c                   ../engine/source/tools/sect/section_c.F
!||    sensor_energy_part          ../engine/source/tools/sensor/sensor_energy_part.F
!||    startimeg                   ../engine/source/system/timer.F
!||    stoptimeg                   ../engine/source/system/timer.F
!||--- uses       -----------------------------------------------------
!||    crackxfem_mod               ../engine/share/modules/crackxfem_mod.F
!||    drape_mod                   ../engine/share/modules/drape_mod.F
!||    dt_mod                      ../engine/source/modules/dt_mod.F
!||    elbufdef_mod                ../common_source/modules/mat_elem/elbufdef_mod.F90
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
!||====================================================================
      SUBROUTINE FORINTC(TIMERS,
     1    PM        ,GEO       ,X         ,A         ,AR        ,
     2    V         ,VR        ,MS        ,IN        ,NLOC_DMG  ,
     3    WA        ,STIFN     ,STIFR     ,FSKY      ,CRKSKY    ,
     4    TF        ,BUFMAT    ,PARTSAV   ,D         ,MAT_ELEM  ,
     5    DR        ,EANI      ,TANI      ,FANI      ,
     6    FSAV      ,SENSORS   ,SKEW      ,ANIN      ,FAILWAVE  ,
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
     U    SBUFMAT)
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
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER,INTENT(IN)    :: USERL_AVAIL ! Flag for User libraries availability
      INTEGER,INTENT(IN)    :: MAXFUNC     ! Maximum number of functions
      INTEGER,INTENT(IN)    :: SBUFMAT     ! Size of Bufmat
      INTEGER,INTENT(IN)    :: STF         ! Size of TF
      INTEGER,INTENT(IN)    :: SNPC        ! Size of NPC
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
     .   STIFN(*) ,STIFR(*),ANIN(*) ,FSKY(*) ,
     .   DR(3,*) ,TANI(*),EANI(*),
     .   BUFGEO(*) ,DT2T, SECFCUM(7,NUMNOD,NSECT),
     .   FSAVD(NTHVKI,*),
     .   FZERO(3,4,(NUMELC+NUMELTG)),XSEC(4,3,NSECT),
     .   MCP(*),TEMP(*),FTHE(*),FTHESKY(*),
     .   MS_PLY(*), ZI_PLY(*),GRESAV(*),
     .   MSTG(*), DMELTG(*), MSC(*), DMELC(*),CONDN(*),CONDNSKY(*),
     .   PTG(3,*),MSZ2(*),APINCH(3,*),STIFPINCH(*),VPINCH(3,*)
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
     .   K1, K2, NF1,IPRI, NELEM, OFFSET, NSGRP, K,JJ,NP,TYP,IPRT,
     .   K0, K3, K5, K6, K7, K8, K9, NSG, NEL, KFTS,IOFC, ISTRA,
     .   JJ19,NPE,ICNOD,NFT1,ISUBS,ISENS_ENERGY,
     .   L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,
     .   L17,L18,L19,L20,L21,L22,L23,L24,L25,L26,L27,L28,L29,L30,
     .   ICP,ICS,IEXPAN, IG,ITG1,ITG2,ITG3,NLEVXF,NRBDIM,ISEATBELT
      INTEGER MTN1,MTN2,IPARSENS,ISECT
      INTEGER ISH3N,ISHPLYXFEM,IXFEM,ITSK,OMP_GET_THREAD_NUM
      INTEGER IXEL,ACTIFXFEM,ISUBSTACK,DIM6,DIM_EXCH
      my_real
     .   FX(MVSIZ,20),FY(MVSIZ,20),FZ(MVSIZ,20),
     .   MX(MVSIZ,4),MY(MVSIZ,4),MZ(MVSIZ,4),BID
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
          IF (JHBE >= 11.AND.JHBE <= 19) THEN
              
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
     U        USERL_AVAIL,        MAXFUNC,            SBUFMAT  )                                                         

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
     U        USERL_AVAIL,        MAXFUNC,            SBUFMAT )

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
     *        IDEL7NOK   ,USERL_AVAIL,MAXFUNC  ,SBUFMAT)
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
     R   XFEM_TAB(NG,1:NXEL),CRKEDGE,            DRAPE_SH4N,         IPRI,
     S   NLOC_DMG,           DRAPEG%INDX_SH4N,   IGRE,               JTUR,
     T   OUTPUT,             DT,                 SNPC,               STF  , 
     U   GLOB_THERM,         USERL_AVAIL,        MAXFUNC,            SBUFMAT )
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
     R       GLOB_THERM  ,USERL_AVAIL, MAXFUNC,SBUFMAT)
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
          IF (ICNOD == 6) THEN
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
     P        SBUFMAT     )
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
     M          MAXFUNC,                               SBUFMAT )
     
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
     S          IDEL7NOK,              USERL_AVAIL,                           MAXFUNC,           SBUFMAT)

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
     A                USERL_AVAIL,       MAXFUNC,              SBUFMAT )
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


