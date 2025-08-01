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
!||    spstres                ../engine/source/elements/sph/spstres.F
!||--- called by ------------------------------------------------------
!||    forintp                ../engine/source/elements/forintp.F
!||--- calls      -----------------------------------------------------
!||    mmain                  ../engine/source/materials/mat_share/mmain.F90
!||    spback3                ../engine/source/elements/sph/spback3.F
!||    spbilan                ../engine/source/elements/sph/spbilan.F
!||    spdefo3                ../engine/source/elements/sph/spdefo3.F
!||    spload3                ../engine/source/elements/sph/spload3.F
!||    spmallb3               ../engine/source/elements/sph/spmall3.F
!||    spoff3                 ../engine/source/elements/sph/spoff3.F
!||    spreploc               ../engine/source/elements/sph/spreploc.F
!||    sptempel               ../engine/source/elements/sph/sptemp.F
!||    spvol3                 ../engine/source/elements/sph/spvol3.F
!||    srota3                 ../engine/source/elements/solid/solide/srota3.F
!||    sstra3                 ../engine/source/elements/solid/solide/sstra3.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    dt_mod                 ../engine/source/modules/dt_mod.F
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    glob_therm_mod         ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    mmain_mod              ../engine/source/materials/mat_share/mmain.F90
!||    nlocal_reg_mod         ../common_source/modules/nlocal_reg_mod.F
!||    output_mod             ../common_source/modules/output/output_mod.F90
!||    sensor_mod             ../common_source/modules/sensor_mod.F90
!||    table_mod              ../engine/share/modules/table_mod.F
!||    timer_mod              ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE SPSTRES(TIMERS, ELBUF_TAB,NG     , 
     1    PM        ,GEO       ,X         ,V         ,MS        ,
     2    W         ,SPBUF     ,WA        ,NLOC_DMG  ,
     3    ITAB      ,PLD       ,BUFMAT    ,BUFGEO    ,PARTSAV   ,
     4    FSAV      ,DT2T      ,IPARG     ,NPC       ,KXSP      ,
     5    IXSP      ,NOD2SP    ,NELTST    ,ITYPTST   ,IPART     ,
     6    IPARTSP   ,FV        ,NEL       ,IPM       ,GRESAV    ,
     7    GRTH      ,IGRTH     ,TABLE     ,ISTRAIN   ,VOLN      ,
     8    IGEO      ,IEXPAN    ,TEMP      ,ITASK     ,SPH2SOL   ,
     9    MAT_ELEM  ,H3D_STRAIN,OUTPUT    ,SNPC      ,STF,
     A    SBUFMAT   ,SVIS,NSVOIS,IDTMINS, IRESP,      
     .    IDEL7NG,   IDEL7NOK,    IDTMIN,     MAXFUNC,LIPART1,
     .    IMON_MAT,  USERL_AVAIL, impl_s,
     V    idyna,     DT         , GLOB_THERM,sensors)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE OUTPUT_MOD
      USE ELBUFDEF_MOD
      USE DT_MOD
      use glob_therm_mod
      use sensor_mod
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr07_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_) ,INTENT(INOUT) :: TIMERS
      INTEGER, INTENT(IN) :: SNPC
      INTEGER, INTENT(IN) :: STF
      INTEGER, INTENT(IN) :: SBUFMAT
      INTEGER, INTENT(IN) :: NSVOIS
      INTEGER, INTENT(IN) :: IDTMINS
      INTEGER ,INTENT(IN) :: IRESP
      INTEGER ,INTENT(IN) :: IDEL7NG
      INTEGER ,INTENT(INOUT) :: IDEL7NOK
      integer,dimension(102) :: IDTMIN
      INTEGER ,INTENT(IN) :: MAXFUNC
      INTEGER ,INTENT(IN) :: LIPART1
      INTEGER, INTENT(IN) :: IMPL_S
      INTEGER, INTENT(IN) :: IDYNA
      INTEGER, INTENT(IN) :: USERL_AVAIL
      INTEGER, INTENT(IN) :: IMON_MAT
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),ITAB(*),
     .   IPART(LIPART1,*) ,IPARTSP(*), NPC(*), IPARG(NPARG,*),NG,
     .   NELTST,ITYPTST,NEL,GRTH(*),IGRTH(*) , ISTRAIN,
     .   IPM(NPROPMI,*),IGEO(*),IEXPAN,ITASK,SPH2SOL(*),H3D_STRAIN
      my_real
     .   X(3,*)    ,V(3,*)   ,MS(*)   ,
     .   PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*) ,BUFGEO(*) ,W(3,*)  ,
     .   PLD(*) ,FSAV(NTHVKI,*) ,SPBUF(NSPBUF,*) ,
     .   WA(*) , PARTSAV(*) ,DT2T, FV(*),
     .   GRESAV(*),VOLN(MVSIZ),TEMP(*)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (DT_) ,INTENT(IN) :: DT
      type (glob_therm_) ,intent(inout)   :: glob_therm
      type (sensors_)    ,intent(in)    :: sensors     !< sensor structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF1, IFLAG, NB3S, I, OFFSET, IPRT,ILAY
C-----
      INTEGER MXT(MVSIZ),NGL(MVSIZ), IBIDON(1),IBID
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  ,   
     . RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     . SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,GAMA(MVSIZ,6),
     . VDX(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ) ,SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . TEMPEL(MVSIZ)
C-----
C Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ), BID(MVSIZ)
C Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUMAX(MVSIZ)
C-----
C Variables utilisees (en arguments) dans les routines solides (uniquement).
      INTEGER NC1(MVSIZ),NGEO(MVSIZ)
      my_real
     .   OFF(MVSIZ) , RHOO(MVSIZ),
     .   VX1(MVSIZ), VY1(MVSIZ), VZ1(MVSIZ),
     .   DXY(MVSIZ),DYX(MVSIZ),
     .   DYZ(MVSIZ),DZY(MVSIZ),
     .   DZX(MVSIZ),DXZ(MVSIZ),DIE(MVSIZ)
C Variables void MMAIN 
      my_real
     .   SIGY(MVSIZ),ET(MVSIZ),
     .   R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      INTEGER SZ_R1_FREE
      my_real, 
     .  DIMENSION(:), POINTER :: EINT
      my_real VARNL(NEL)
      TYPE(TTABLE) TABLE(*)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF   
      TYPE(t_ale_connectivity) :: ALE_CONNECT
      INTEGER SZ_IX
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      SZ_IX=NUMELQ+NUMELS+NSVOIS
      ILAY=1
      SZ_R1_FREE = MVSIZ
      IBID = 0
      GBUF => ELBUF_TAB(NG)%GBUF
      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
      TEMPEL(:) = ZERO
C-----------
      NF1=NFT+1
C-----------
C GATHER PARTICLE VARIABLES
      CALL SPOFF3(
     1   KXSP(1,NF1), VD2,         VIS,         GBUF%OFF,
     2   OFF,         NC1,         NGL,         MXT,
     3   NGEO,        IPART,       IPARTSP(NF1),NEL)
C-----------
C DOWNLOAD RHOO, RHON, DELTAX and D FROM WA.
      CALL SPLOAD3(
     1   SPBUF,   WA,      RHOO,    GBUF%RHO,
     2   DELTAX,  GBUF%OFF,DXX,     DXY,
     3   DXZ,     DYX,     DYY,     DYZ,
     4   DZX,     DZY,     DZZ,     MUMAX,
     5   LFT,     LLT,     NFT)
C-----------
C     LAGRANGIAN ONLY.
C   do not enter spvol3 case thermal only material.
C  (actually only Lagrangian case is available , not Ale neither Euler)
      IF(JLAG+JALE+JEUL/=0)THEN
        CALL SPVOL3(
     1   OFF,      PM,       GBUF%VOL, GBUF%EINT,
     2   RHOO,     GBUF%RHO, VOLN,     DVOL,
     3   NGL,      MXT,      LFT,      LLT,
     4   JLAG)
      END IF
C-----------
      CALL SPDEFO3(
     1   DXX,     DXY,     DXZ,     DYX,
     2   DYY,     DYZ,     DZX,     DZY,
     3   DZZ,     D4,      D5,      D6,
     4   WXX,     WYY,     WZZ,     NEL)
C-----------
C     when particle is sleeping, stresses (global system) do not rotate.
      CALL SROTA3(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
      CALL SPREPLOC(
     1   GBUF%GAMA,WXX,      WYY,      WZZ,
     2   GAMA,     NEL,      LFT,      LLT,
     3   ISORTH)
C-----------
Cbm   compute temperature in element
      IF (JTHE < 0) CALL SPTEMPEL(
     1   KXSP,    TEMP,    TEMPEL,  LFT,
     2   LLT,     NFT)
      DIE(1:MVSIZ)=ZERO
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
C     LAW 11 uses IELVS=IBIDON and IXS=IBIDON should not be used.
      IBIDON(1)=0
      OFFSET=0
      CALL MMAIN(TIMERS,
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2                ALE_CONNECT, IBIDON,      IPARG,
     3   V,           PLD,         NPC,         BUFMAT,
     4   STI,         X,           DT2T,        NELTST,
     5   ITYPTST,     OFFSET,      NEL,         W,
     6   OFF,         NGEO,        MXT,         NGL,
     7   VOLN,        VD2,         DVOL,        DELTAX,
     8   VIS,         QVIS,        CXX,         S1,
     9   S2,          S3,          S4,          S5,
     A   S6,          DXX,         DYY,         DZZ,
     B   D4,          D5,          D6,          WXX,
     C   WYY,         WZZ,         RX,          RY,
     D   RZ,          SX,          SY,          SZ,
     E   VDX,         VDY,         VDZ,         MUMAX,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   R1_FREE,     LBUF%PLA,    R3_FREE,     R4_FREE,
     H   DXX,         DXY,         DXZ,         DYX,
     I   DYY,         DYZ,         DZX,         DZY,
     J   DZZ,         IPM,         GAMA,        BID,
     K   BID,         BID,         BID,         BID,
     L   BID,         BID,         ISTRAIN,     TEMPEL,
     M   DIE,         IEXPAN,      ILAY,           BID,
     N   BID,         1,           1,           1,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         IPARG(1,NG), IGEO,        BID,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAt_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH,        SZ_R1_FREE,
     S   SNPC,        STF,         SBUFMAT,     GLOB_THERM,
     *   SVIS,        SZ_IX,       IRESP,
     T   N2D,         th_strain,   NGROUP,      TT, 
     .   DT1,         NTABLE,      NUMELQ,      nummat,
     .   numgeo,      numnod,      numels,
     .   IDEL7NOK,    IDTMIN,      MAXFUNC,
     .   IMON_MAT,    USERL_AVAIL, impl_s,
     .   idyna,       DT         , BID   ,sensors)
C-----------------------------
      IF(ISTRAIN==1)THEN 
        CALL SSTRA3(DXX,DYY      ,DZZ ,D4  ,D5  ,
     .              D6 ,LBUF%STRA,WXX ,WYY ,WZZ ,
     .              OFF,NEL, IPARG(37,NG))
      ENDIF
C-----------------------------
C     SMALL STRAIN 
C-----------------------------
       CALL SPMALLB3(
     1   GBUF%OFF,    OFF,         KXSP(1,NF1), SPH2SOL(NF1),
     2   LFT,         LLT,         ISMSTR)
C--------------------------
C     SUBMATERIALS SYNTHESIS
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IFLAG==0.OR.TT>=OUTPUT%TH%THIS.OR.MDESS/=0 .OR.TT>=TABFIS(1).
     .   OR.TT>=TABFIS(2).OR.TT>=TABFIS(3).OR.TT>=TABFIS(4).
     .   OR.TT>=TABFIS(5).OR.TT>=TABFIS(6).OR.TT>=TABFIS(7).
     .   OR.TT>=TABFIS(8).OR.TT>=TABFIS(9).OR.TT>=TABFIS(10))THEN
         IF (MTN == 11) THEN                                     
           EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)                    
         ELSE                                                    
           EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)                    
         ENDIF                                                   
         CALL SPBILAN(
     1   V,           PARTSAV,     NC1,         EINT,
     2   GBUF%RHO,    GBUF%RK,     GBUF%VOL,    VOLN,
     3   IPARTSP(NF1),GBUF%OFF,    GRESAV,      GRTH,
     4   IGRTH(NF1),  GBUF%EINTTH, IEXPAN,      X,
     5   LFT,         LLT,         JTUR,        IGRE)
      ENDIF
C----------------------------
C     RETURN TENSORS BACK FOR INTEGRATION.
C----------------------------
      CALL SPBACK3(
     1   GBUF%SIG,QVIS,    STI,     CXX,
     2   WA,      SSP_EQ,  DIE,     NEL,
     3   LFT,     LLT,     NFT)
C----------------------------
      RETURN
      END
