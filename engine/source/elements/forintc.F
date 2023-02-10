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
Chd|  FORINTC                       source/elements/forintc.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        SEATBELT_REDUCTION_FACTOR     source/tools/seatbelts/seatbelt_reduction_factor.F
Chd|        SECTIO3N                      source/tools/sect/sectio.F    
Chd|        SECTIOC                       source/tools/sect/sectio.F    
Chd|        SENSOR_ENERGY_PART            source/tools/sensor/sensor_energy_part.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        RUPTURE_MOD                   share/modules/rupture_mod.F   
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FORINTC(
     1    PM        ,GEO       ,X         ,A         ,AR        ,
     2    V         ,VR        ,MS        ,IN        ,NLOC_DMG  ,
     3    WA        ,STIFN     ,STIFR     ,FSKY      ,CRKSKY    ,
     4    TF        ,BUFMAT    ,PARTSAV   ,D         ,MATPARAM_TAB,
     5    DR        ,EANI      ,TANI      ,FANI      ,
     6    FSAV      ,SENSOR_TAB,SKEW      ,ANIN      ,FAILWAVE  ,
     7    DT2T      ,THKE      ,BUFGEO    ,IADC      ,IADTG	,
     8    IPARG     ,NPC       ,IXC       ,IXTG      ,NELTST    ,
     A    IPARI     ,ITYPTST   ,NSTRF     ,
     B    IPART     ,IPARTC    ,IPARTTG   ,SECFCUM   ,
     D    FSAVD     ,GROUP_PARAM_TAB,
     F    FZERO     ,IXTG1     ,IADTG1    ,IGEO      ,IPM       ,
     G    MADFAIL   ,XSEC      ,FSAVSAV   ,ITASK     ,MCP       ,
     H    TEMP      ,FTHE      ,FTHESKY   ,
     I    MS_PLY    ,ZI_PLY    ,INOD_PXFEM,XEDGE4N   ,XEDGE3N   ,
     J    IEL_PXFEM ,IADC_PXFEM,IGROUC    ,NGROUC    ,GRESAV    ,
     K    GRTH      ,IGRTH     ,MSTG      ,DMELTG    ,MSC       ,
     L    DMELC     ,TABLE     ,KNOD2ELC  ,PTG       ,MSZ2      ,
     M    INOD_CRK  ,IEL_CRK   ,IADC_CRK  ,ELCUTC    ,NODENR    ,
     N    IBORDNODE ,NODEDGE   ,CRKNODIAD ,ELBUF_TAB ,
     O    XFEM_TAB  ,CONDN     ,CONDNSKY  ,CRKEDGE   ,
     P    DIMFB     , FBSAV6   ,STABSEN   ,TABSENSOR ,STACK     ,ITAB ,
     S    DRAPE_Q4  ,DRAPE_T3  ,SUBSET    ,XDP       ,VPINCH,
     T    APINCH    ,STIFPINCH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE CRACKXFEM_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE MATPARAM_DEF_MOD
      USE GROUPDEF_MOD
      USE PINCHTYPE_MOD
      USE SENSOR_MOD
      USE RUPTURE_MOD
      USE DRAPE_MOD
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
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "vect01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "couple_c.inc"
#include      "impl1_c.inc"
#include      "stati_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   IXC(NIXC,*), IXTG(NIXTG,*), IGEO(NPROPGI,*), IPM(NPROPMI,*),
     .   NPC(*), IPARG(NPARG,*), IPARI(NPARI,*), 
     .   NSTRF(*), IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   IADC(4,*), IADTG(3,*),NELTST,
     .   ITYPTST,IXTG1(4,*),XEDGE4N(4,*),XEDGE3N(3,*),
     .   IADTG1(3,*),MADFAIL(*),ITASK,
     .   INOD_PXFEM(*),IEL_PXFEM(*) ,IADC_PXFEM(4,*), IGROUC(*),
     .   NGROUC,GRTH(*),IGRTH(*),KNOD2ELC(*),
     .   INOD_CRK(*),IEL_CRK(*),IADC_CRK(*),ELCUTC(2,*),
     .   NODENR(*),IBORDNODE(*),NODEDGE(2,*),CRKNODIAD(*),
     .   DIMFB, STABSEN,TABSENSOR(STABSEN),ITAB(*)
C     REAL
      my_real
     .   X(3,*)    ,D(3,*)  ,V(3,*)   ,VR(3,*),
     .   MS(*)     ,IN(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),
     .   GEO(NPROPG,*),BUFMAT(*) ,TF(*) ,FSAV(NTHVKI,*) ,
     .   WA(*), THKE(*),
     .   A(3,*)    ,AR(3,*) ,FANI(3,*) ,PARTSAV(NPSAV,*)    ,
     .   STIFN(*) ,STIFR(*),ANIN(*) ,FSKY(*) ,
     .   DR(3,*) ,TANI(*),EANI(*),
     .   BUFGEO(*) ,DT2T, SECFCUM(7,NUMNOD,NSECT),
     .   FSAVD(NTHVKI,*),
     .   FZERO(3,4,(NUMELC+NUMELTG)),XSEC(4,3,NSECT),
     .   FSAVSAV(NTHVKI,*),MCP(*),TEMP(*),FTHE(*),FTHESKY(*),
     .   MS_PLY(*), ZI_PLY(*),GRESAV(*),
     .   MSTG(*), DMELTG(*), MSC(*), DMELC(*),CONDN(*),CONDNSKY(*),
     .   PTG(3,*),MSZ2(*),APINCH(3,*),STIFPINCH(*),VPINCH(3,*)
      DOUBLE PRECISION,TARGET :: FBSAV6(12,6,DIMFB)
      DOUBLE PRECISION :: XDP(3,*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*)           :: CRKEDGE
      TYPE (XFEM_SKY_)    , DIMENSION(*) :: CRKSKY
      TYPE (STACK_PLY) :: STACK 
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (NLOCAL_STR_)   :: NLOC_DMG 
      TYPE (GRP_PARAM_STRUCT_) , DIMENSION(NGROUP) :: GROUP_PARAM_TAB
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (DRAPE_)  :: DRAPE_Q4,DRAPE_T3
      TYPE (MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
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
      IF(MOD(NCYCLE,IABS(NCPRI))==0.OR.TT >= THIS.OR.TT >= TDES.OR.TT >= TPAT.OR.
     +       MDESS /= 0.OR.TT >= THIS1(1).OR.TT >= THIS1(2).
     +       OR.TT >= THIS1(3) .OR.TT >= THIS1(4).OR.TT >= THIS1(5).
     +       OR.TT >= THIS1(6) .OR.TT >= THIS1(7).OR.TT >= THIS1(8).
     +       OR.TT >= THIS1(9) .OR.NTH /= 0.OR.NANIM/=0.
     +       OR.TT >= TABFIS(1).OR.TT >= TABFIS(2).
     +       OR. TT >= TABFIS(3).OR.TT >= TABFIS(4).OR.TT >= TABFIS(5).
     +       OR. TT >= TABFIS(6).OR.TT >= TABFIS(7).OR.TT >= TABFIS(8).
     +       OR. TT >= TABFIS(9).OR. TT >= TABFIS(10).OR.
     +       ISHSUB /= 0 .OR. ISTAT==3) IPRI=1
!
      ISENS_ENERGY = 0
      DO I=1,NSENSOR
        TYP = SENSOR_TAB(I)%TYPE
        IF (TYP == 14) ISENS_ENERGY = 1 ! save internal/kinetic energy (PARTSAV)
      ENDDO ! DO I=1,NSENSOR
C
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1, NGROUC
C
            NG = IGROUC(IG)
            NGR_SENSOR = NG
            NGR_FAIL_WIND = NG
            IF (IPARG(1, NG) == 151) THEN
C     Debranchement loi 151
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
     .    CALL SENSOR_ENERGY_PART(IPARTC(NF1) ,SUBSET ,IPRI)
          IF (JHBE >= 11.AND.JHBE <= 19) THEN
              CALL CBAFORC3(ELBUF_TAB(NG),
     1        JFT        ,JLT       ,NFT       ,NPT       ,
     2        IPARI      ,MLW       ,IPRI      ,ITHK      ,NELTST     ,
     3        ITYPTST    ,ITAB      ,MATPARAM_TAB,
     4        ISTRA      ,IPLA      ,TT        ,DT1       ,DT2T       ,
     5        PM         ,GEO       ,PARTSAV   ,IXC(1,NF1),FAILWAVE   ,
     6        BUFMAT     ,TF        ,NPC       ,IADC(1,NF1),
     7        X          ,D         ,DR        ,V         ,VR         ,
     8        A          ,AR        ,STIFN     ,STIFR     ,FSKY       ,
     9        TANI      ,OFFSET    ,EANI      ,INDXOF     ,
     A        IPARTC(NF1),THKE(NF1) ,NVC       ,IOFC      ,JHBE       ,
     B        FX(1,1)    ,FX(1,2)   ,FX(1,3)   ,FX(1,4)   ,FY(1,1)    ,
     C        FY(1,2)    ,FY(1,3)   ,FY(1,4)   ,FZ(1,1)   ,FZ(1,2)    ,
     D        FZ(1,3)    ,FZ(1,4)   ,MX(1,1)   ,MX(1,2)   ,MX(1,3)    ,
     E        MX(1,4)    ,MY(1,1)   ,MY(1,2)   ,MY(1,3)   ,MY(1,4)    ,
     F        MZ(1,1)    ,MZ(1,2)   ,MZ(1,3)   ,MZ(1,4),
     I        KFTS       ,ISMSTR    ,IGEO      ,GROUP_PARAM_TAB(NG)   ,
     K        IPM        ,IFAILURE  ,ITASK     ,JTHE      ,TEMP       ,
     L        FTHE       ,FTHESKY   ,IEXPAN    ,ISHPLYXFEM ,MS        ,
     M        IN         ,MS_PLY    ,ZI_PLY    ,INOD_PXFEM,IEL_PXFEM  ,
     N        IADC_PXFEM ,GRESAV    ,GRTH      ,IGRTH(NF1) ,MSC(NF1)  ,
     O        DMELC(NF1) ,JSMS      ,TABLE     ,IPARG(1,NG),SENSOR_TAB,
     P        MSZ2       ,CONDN     ,CONDNSKY  ,ISUBSTACK  ,STACK     ,
     R        DRAPE_Q4   ,NEL       ,NLOC_DMG  ,VPINCH     ,
     R        APINCH     ,STIFPINCH )
          ELSEIF (JHBE >= 21 .AND. JHBE <= 29) THEN

              CALL CZFORC3(ELBUF_TAB(NG),
     1      JFT        ,JLT       ,NFT       ,NPT       ,ITAB       ,              
     2        IPARI      ,MLW       ,IPRI      ,ITHK      ,NELTST     ,
     3        ISTRA      ,IPLA      ,TT        ,DT1       ,DT2T       ,
     4        PM         ,GEO       ,PARTSAV   ,IXC(1,NF1),ITYPTST    ,
     5      BUFMAT     ,TF        ,NPC       ,IADC(1,NF1),FAILWAVE  ,             
     6        X          ,D         ,DR        ,V         ,VR         ,
     7        A          ,AR        ,STIFN     ,STIFR     ,FSKY       ,
     8      TANI      ,EANI      ,INDXOF    ,ISMSTR     ,GROUP_PARAM_TAB(NG),              
     9        IPARTC(NF1),THKE(NF1) ,NVC       ,IOFC      ,JHBE       ,
     A        FX(1,1)    ,FX(1,2)   ,FX(1,3)   ,FX(1,4)   ,FY(1,1)    ,
     B        FY(1,2)    ,FY(1,3)   ,FY(1,4)   ,FZ(1,1)   ,FZ(1,2)    ,
     C        FZ(1,3)    ,FZ(1,4)   ,MX(1,1)   ,MX(1,2)   ,MX(1,3)    ,
     D        MX(1,4)    ,MY(1,1)   ,MY(1,2)   ,MY(1,3)   ,MY(1,4)    ,
     E        MZ(1,1)    ,MZ(1,2)   ,MZ(1,3)   ,MZ(1,4)   ,KFTS       ,
     F        FZERO(1,1,NF1),IGEO   ,IPM       ,IFAILURE  ,ITASK      ,
     G        JTHE        ,TEMP     ,FTHE      ,FTHESKY   ,IEXPAN     ,
     H      GRESAV      ,GRTH     ,IGRTH(NF1),XEDGE4N   ,MSC(NF1)   ,  
     I        DMELC(NF1)  ,JSMS     ,TABLE     ,IPARG(1,NG),MATPARAM_TAB,
     J        IXFEM       ,KNOD2ELC  ,SENSOR_TAB,
     K        ELCUTC(1,NF1),INOD_CRK,IEL_CRK   ,IBORDNODE  ,NODENR    ,
     L        IADC_CRK    ,NODEDGE  ,CRKNODIAD ,CONDN    ,
     M      CONDNSKY    ,STACK,ISUBSTACK,XFEM_TAB(NG,1:NXEL),CRKEDGE,       
     O      DRAPE_Q4      ,NEL,NLOC_DMG)                         

      IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
          DO IXEL=1,NXEL
           CALL CZFORC3_CRK(XFEM_TAB(NG,IXEL),
     1          JFT        ,JLT       ,NFT       ,ITYPTST    ,
     2        IPARI      ,MLW       ,IPRI      ,ITHK      ,NELTST     ,
     3          ISTRA      ,IPLA      ,TT        ,DT1       ,DT2T       ,
     4          PM         ,GEO       ,PARTSAV   ,IXC(1,NF1),GROUP_PARAM_TAB(NG),
     5          BUFMAT     ,TF        ,NPC       ,IADC(1,NF1),FAILWAVE  ,
     6          X          ,D         ,DR        ,V         ,VR         ,
     7          A          ,AR        ,STIFN     ,STIFR     ,FSKY       ,
     8          TANI      ,OFFSET    ,EANI      ,INDXOF     ,   
     9          IPARTC(NF1),THKE(NF1) ,NVC       ,IOFC      ,JHBE       ,
     A          FX(1,1)    ,FX(1,2)   ,FX(1,3)   ,FX(1,4)   ,FY(1,1)    ,
     B          FY(1,2)    ,FY(1,3)   ,FY(1,4)   ,FZ(1,1)   ,FZ(1,2)    ,
     C          FZ(1,3)    ,FZ(1,4)   ,MX(1,1)   ,MX(1,2)   ,MX(1,3)    ,
     D          MX(1,4)    ,MY(1,1)   ,MY(1,2)   ,MY(1,3)   ,MY(1,4)    ,
     E          MZ(1,1)    ,MZ(1,2)   ,MZ(1,3)   ,MZ(1,4),
     F          KFTS       ,FZERO(1,1,NF1),ISMSTR ,MATPARAM_TAB,
     I          IGEO       ,IPM        ,IFAILURE ,ITASK     ,JTHE        ,
     J          TEMP       ,FTHE       ,FTHESKY  ,IEXPAN    ,GRESAV      ,
     K          GRTH       ,IGRTH(NF1) ,MSC(NF1) ,DMELC(NF1),JSMS        ,
     L          TABLE      ,IPARG(1,NG),IXFEM    ,INOD_CRK  ,IEL_CRK     ,
     M          IADC_CRK   ,ELCUTC(1,NF1),CRKSKY ,
     N          SENSOR_TAB,IXEL     ,
     O          ISUBSTACK  ,UXINT_MEAN ,UYINT_MEAN,UZINT_MEAN,NLEVXF      ,
     P          NODEDGE    ,CRKEDGE    ,STACK    ,DRAPE_Q4  ,NLOC_DMG    )
          ENDDO
            ENDIF
c
          ELSE
            CALL CFORC3(ELBUF_TAB(NG),
     1        JFT      ,JLT      ,PM       ,IXC(1,NF1),X         ,       
     2        A        ,AR       ,V        ,VR       ,FAILWAVE   ,                   
     3        NVC      ,MLW      ,GEO      ,TF       ,NPC        ,       
     4        BUFMAT   ,PARTSAV  ,DT2T     ,NELTST   ,ITYPTST    ,       
     5        STIFN    ,STIFR    ,FSKY     ,IADC(1,NF1),ITAB     ,       
     6        D        ,DR       ,TANI     ,OFFSET   ,EANI       ,       
     7        FX(1,1)  ,FX(1,2)  ,FX(1,3)  ,FX(1,4)  ,FY(1,1)    ,       
     8        FY(1,2)  ,FY(1,3)  ,FY(1,4)  ,FZ(1,1)  ,FZ(1,2)    ,       
     9        FZ(1,3)  ,FZ(1,4)  ,MX(1,1)  ,MX(1,2)  ,MX(1,3)    ,       
     A        MX(1,4)  ,MY(1,1)  ,MY(1,2)  ,MY(1,3)  ,MY(1,4)    ,       
     B        MZ(1,1)  ,MZ(1,2)  ,MZ(1,3)  ,MZ(1,4)  ,INDXOF     ,       
     C        IPARTC(NF1),THKE(NF1),GROUP_PARAM_TAB(NG),MATPARAM_TAB,                                     
     F        NEL      ,ISTRA    ,JHBE     ,                   
     G        ITHK     ,IOFC     ,IPLA     ,NFT      ,ISMSTR     ,       
     H        NPT      ,KFTS     ,                                       
     I        FZERO(1,1,NF1),IGEO ,IPM         ,IFAILURE  ,ITASK   ,     
     J        JTHE          ,TEMP , FTHE       ,FTHESKY   ,IEXPAN ,      
     K        GRESAV      ,GRTH   ,XEDGE4N  ,                                 
     L        IGRTH(NF1),MSC(NF1)  ,DMELC(NF1)  ,JSMS,TABLE    ,         
     M        IPARG(1,NG),IXFEM,KNOD2ELC,SENSOR_TAB,           
     N        ELCUTC(1,NF1),INOD_CRK,IEL_CRK ,IBORDNODE ,NODENR   ,      
     O        IADC_CRK ,NODEDGE ,CRKNODIAD ,CONDN   ,
     P        CONDNSKY ,STACK ,ISUBSTACK ,XFEM_TAB(NG,1:NXEL),CRKEDGE,
     R        DRAPE_Q4     ,IPRI        ,NLOC_DMG)
c
         IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
          DO IXEL=1,NXEL
           CALL CFORC3_CRK(XFEM_TAB(NG,IXEL),
     1            JFT      ,JLT      ,PM       ,IXC(1,NF1),X         ,
     2            A        ,AR       ,V        ,VR       ,FAILWAVE   ,   
     3            NVC      ,MLW      ,GEO      ,TF       ,NPC        ,
     4            BUFMAT   ,PARTSAV  ,DT2T     ,NELTST   ,ITYPTST    ,
     5            STIFN    ,STIFR    ,FSKY     ,CRKSKY   ,IADC(1,NF1),
     6            D        ,DR       ,TANI     ,OFFSET   ,EANI       ,
     7            FX(1,1)  ,FX(1,2)  ,FX(1,3)  ,FX(1,4)  ,FY(1,1)    ,
     8            FY(1,2)  ,FY(1,3)  ,FY(1,4)  ,FZ(1,1)  ,FZ(1,2)    ,
     9            FZ(1,3)  ,FZ(1,4)  ,MX(1,1)  ,MX(1,2)  ,MX(1,3)    ,
     A            MX(1,4)  ,MY(1,1)  ,MY(1,2)  ,MY(1,3)  ,MY(1,4)    ,
     B            MZ(1,1)  ,MZ(1,2)  ,MZ(1,3)  ,MZ(1,4)  ,INDXOF     ,
     C            IPARTC(NF1),THKE(NF1),GROUP_PARAM_TAB(NG),MATPARAM_TAB, 
     F            NEL      ,ISTRA     ,JHBE    ,KFTS     ,
     G            ITHK     ,IOFC      ,IPLA    ,NFT      ,ISMSTR     ,
     H FZERO(1,1,NF1),IGEO      ,IPM         ,IFAILURE  ,ITASK       ,           
     I JTHE          ,TEMP      , FTHE       ,FTHESKY   ,IEXPAN      ,
     J GRESAV      ,GRTH        ,
     K IGRTH(NF1)    ,MSC(NF1)  ,DMELC(NF1)  ,JSMS      ,TABLE       ,
     L IPARG(1,NG) ,IXFEM       ,INOD_CRK    ,IEL_CRK   ,IADC_CRK    ,
     M ELCUTC(1,NF1),
     N SENSOR_TAB,IXEL      ,STACK       ,
     O ISUBSTACK   ,UXINT_MEAN  ,UYINT_MEAN  ,UZINT_MEAN,NLEVXF      ,
     P NODEDGE     ,CRKEDGE     ,DRAPE_Q4   ,IPRI       ,NLOC_DMG    )
          ENDDO
            END IF
          ENDIF
C
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
                IF(STABSEN/=0) ISECT=TABSENSOR(I+1)-TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6=>FBSAV6(1,1,ISECT)
                ENDIF
C
                CALL SECTIOC(JFT,JLT,NFT,NSTRF(K0+9),NSTRF(K0+3),
     2           NSTRF(K0+4),NSTRF(K0+5),NSTRF(K5),X,V,VR,FSAV(1,N),
     3           IXC        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I) ,
     4           FX       ,FY ,FZ ,MX ,MY ,MZ ,
     5           NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     6           NSTRF(K2),MS,
     8           XSEC(1,1,I) ,FSAVSAV(1,I),pFBSAV6,IPARSENS)
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
     .        CALL SENSOR_ENERGY_PART(IPARTTG(NF1) ,SUBSET ,IPRI)
           IF (ICNOD == 6) THEN
              NFT1 = NF1-NUMELTG+NUMELTG6
              CALL CDK6FORC3(ELBUF_TAB(NG),
     1 JFT         ,JLT         ,PM          ,IXTG(1,NF1) ,X           ,            
     2 A           ,AR          ,V           ,VR          ,FAILWAVE    ,           
     3 NVC         ,MLW         ,GEO         ,TF          ,NPC         ,            
     4 BUFMAT      ,PARTSAV     ,DT2T        ,NELTST      ,ITYPTST     ,            
     5 STIFN       ,STIFR       ,FSKY        ,IADTG(1,NF1),GROUP_PARAM_TAB(NG),       
     6 TANI(1+6*NUMELC),OFFSET  ,IPARTTG(NF1),THKE(NUMELC+NF1),FX(1,1) ,            
     7 FX(1,2)     ,FX(1,3)     ,FY(1,1)     ,FY(1,2)     ,FY(1,3)     ,            
     8 FZ(1,1)     ,FZ(1,2)     ,FZ(1,3)     ,FX(1,4)     ,FX(1,5)     ,            
     9 FX(1,6)     ,FY(1,4)     ,FY(1,5)     ,FY(1,6)     ,FZ(1,4)     ,            
     A FZ(1,5)     ,FZ(1,6)     ,MATPARAM_TAB,          
     B NEL       ,ISTRA       ,ISH3N       ,                          
     C ITHK        ,IOFC        ,IPLA        ,NFT         ,ISMSTR      ,           
     D NPT         ,KFTS        ,            
     E IXTG1(1,NFT1),IADTG1(1,NFT1),IGEO     ,IPM         ,IFAILURE   ,             
     F IEXPAN      ,GRESAV      ,GRTH        ,                          
     .      IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1) ,MSTG(NF1) ,             
     G DMELTG(NF1) ,JSMS        ,TABLE       ,IPARG(1,NG) ,SENSOR_TAB,             
     H PTG(1,NF1)  ,JTHE        ,CONDN       ,CONDNSKY   ,
     I ISUBSTACK   ,STACK       ,ITASK,DRAPE_T3,
     . IPRI        ,NLOC_DMG    )
           ELSE
             IF (ISH3N == 30) THEN
              CALL CDKFORC3(ELBUF_TAB(NG),
     1 JFT         ,JLT         ,PM          ,IXTG(1,NF1) ,X           ,
     2 A           ,AR          ,V           ,VR          ,FAILWAVE    ,
     3 NVC         ,MLW         ,GEO         ,TF          ,NPC         ,
     4 BUFMAT      ,PARTSAV     ,DT2T        ,NELTST      ,ITYPTST     ,
     5 STIFN       ,STIFR       ,FSKY        ,IADTG(1,NF1),ITAB        ,
     6 TANI(1+6*NUMELC),IPARTTG(NF1),THKE(NUMELC+NF1),GROUP_PARAM_TAB(NG),FX(1,1) ,
     7 FX(1,2)     ,FX(1,3)     ,FY(1,1)     ,FY(1,2)     ,FY(1,3)     ,
     8 FZ(1,1)     ,FZ(1,2)     ,FZ(1,3)     ,MX(1,1)     ,MX(1,2)     ,
     9 MX(1,3)     ,MY(1,1)     ,MY(1,2)     ,MY(1,3)     ,MZ(1,1)     ,
     A MZ(1,2)     ,MZ(1,3)     ,MATPARAM_TAB,
     B NEL       ,ISTRA       ,ISH3N       ,
     C ITHK        ,IOFC        ,IPLA        ,NFT         ,ISMSTR      ,
     D NPT         ,KFTS        ,
     E IGEO        ,IPM         ,IFAILURE    ,GRESAV      ,GRTH        ,
     F IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1),
     .              MSTG(NF1)   ,DMELTG(NF1) ,JSMS        ,TABLE       ,
     G IPARG(1,NG) ,SENSOR_TAB  ,PTG(1,NF1)  ,JTHE        ,
     J CONDN       ,CONDNSKY    ,ISUBSTACK , STACK        ,ITASK       ,
     K DRAPE_T3,IPRI   ,NLOC_DMG    )
             ELSE
              CALL C3FORC3(ELBUF_TAB(NG),
     1 JFT         ,JLT         ,PM          ,IXTG(1,NF1) ,X           ,
     2 A           ,AR          ,V           ,VR          ,FAILWAVE    ,
     3 NVC         ,MLW         ,GEO         ,TF          ,NPC         ,
     4 BUFMAT      ,PARTSAV     ,DT2T        ,NELTST      ,ITYPTST     ,
     5 STIFN       ,STIFR       ,FSKY        ,IADTG(1,NF1),ITAB        ,
     6 TANI(1+6*NUMELC),OFFSET  ,IPARTTG(NF1),THKE(NUMELC+NF1),FX(1,1) ,
     7 FX(1,2)     ,FX(1,3)     ,FY(1,1)     ,FY(1,2)     ,FY(1,3)     ,
     8 FZ(1,1)     ,FZ(1,2)     ,FZ(1,3)     ,MX(1,1)     ,MX(1,2)     ,
     9 MX(1,3)     ,MY(1,1)     ,MY(1,2)     ,MY(1,3)     ,MZ(1,1)     ,
     A MZ(1,2)     ,MZ(1,3)     ,GROUP_PARAM_TAB(NG),MATPARAM_TAB,
     B NEL         ,ISTRA       ,ISH3N       ,XEDGE3N     ,
     C ITHK        ,IOFC        ,IPLA        ,NFT         ,ISMSTR      ,
     D NPT         ,KFTS        ,
     E FZERO(1,1,NF1+NUMELC),IGEO,IPM        ,IFAILURE    ,ITASK       ,
     F JTHE        ,TEMP        , FTHE       ,FTHESKY     ,IEXPAN      ,
     G GRESAV      ,GRTH,IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1),
     H MSTG(NF1)   ,DMELTG(NF1) ,JSMS        ,TABLE       ,IPARG(1,NG) ,
     I IXFEM       ,SENSOR_TAB  ,PTG(1,NF1)  ,IBORDNODE   ,
     J ELCUTC(1,NF1+ITG3-1),INOD_CRK,IEL_CRK(ITG3),NODENR,IADC_CRK(ITG1),
     K NODEDGE     ,CRKNODIAD   ,KNOD2ELC    ,CONDN,
     L CONDNSKY    ,STACK       ,ISUBSTACK   ,XFEM_TAB(NG,1:NXEL),CRKEDGE,
     N DRAPE_T3,IPRI   ,NLOC_DMG    ,XDP)

       IF (ICRACK3D > 0 .AND. IXFEM > 0 .AND. ACTIFXFEM > 0) THEN
         DO IXEL=1,NXEL
           CALL C3FORC3_CRK(XFEM_TAB(NG,IXEL),
     1     JFT         ,JLT         ,PM          ,IXTG(1,NF1) ,X           ,
     2     A           ,AR          ,V           ,VR          ,FAILWAVE    ,
     3     NVC         ,MLW         ,GEO         ,TF          ,NPC         ,
     4     BUFMAT      ,PARTSAV     ,DT2T        ,NELTST      ,ITYPTST     ,
     5     STIFN       ,STIFR       ,FSKY        ,CRKSKY      ,IADTG(1,NF1),
     6     TANI(1+6*NUMELC),OFFSET  ,IPARTTG(NF1),THKE(NUMELC+NF1),FX(1,1) ,
     7     FX(1,2)     ,FX(1,3)     ,FY(1,1)     ,FY(1,2)     ,FY(1,3)     ,
     8     FZ(1,1)     ,FZ(1,2)     ,FZ(1,3)     ,MX(1,1)     ,MX(1,2)     ,
     9     MX(1,3)     ,MY(1,1)     ,MY(1,2)     ,MY(1,3)     ,MZ(1,1)     ,
     A     MZ(1,2)     ,MZ(1,3)     ,KFTS        ,GROUP_PARAM_TAB(NG),MATPARAM_TAB,
     B     NEL         ,ISTRA       ,ISH3N       ,
     C     ITHK        ,IOFC        ,IPLA        ,NFT         ,ISMSTR      ,
     D     FZERO(1,1,NF1+NUMELC),IGEO,IPM        ,IFAILURE    ,ITASK       ,
     E     JTHE        ,TEMP        , FTHE       ,FTHESKY     ,IEXPAN      ,
     F     GRESAV      ,GRTH,IGRTH(NUMELC+NUMELT+NUMELP+NUMELR+NF1),
     G     MSTG(NF1)   ,DMELTG(NF1) ,JSMS        ,TABLE       ,IPARG(1,NG) ,
     H     SENSOR_TAB  ,PTG(1,NF1)  ,IXFEM       ,INOD_CRK    ,
     I     IEL_CRK(ITG3),IADC_CRK(ITG1),ELCUTC(1,NF1+ITG3-1)  ,IXEL        ,
     J     STACK       ,ISUBSTACK   ,UXINT_MEAN  ,UYINT_MEAN  ,UZINT_MEAN  ,
     K     NLEVXF      ,NODEDGE     ,CRKEDGE     ,DRAPE_T3    ,IPRI        ,
     L     NLOC_DMG    )
        ENDDO
        ENDIF
       ENDIF
      ENDIF
c
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
               IF(STABSEN/=0) ISECT=TABSENSOR(I+1)-TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6=>FBSAV6(1,1,ISECT)
                ENDIF
C
               CALL SECTIO3N(JFT,JLT,NFT,NSTRF(K0+13),NSTRF(K0+3),
     2         NSTRF(K0+4),NSTRF(K0+5),NSTRF(K9),X,V,VR,FSAV(1,N),
     3         IXTG        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I) ,
     4         FX ,FY ,FZ  ,MX  ,MY,  MZ,
     5         NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     6         NSTRF(K2),MS,
     8         XSEC(1,1,I) ,FSAVSAV(1,I), pFBSAV6,IPARSENS)
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


