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
!||    s10forc3               ../engine/source/elements/solid/solide10/s10forc3.F
!||--- called by ------------------------------------------------------
!||    forint                 ../engine/source/elements/forint.F
!||--- calls      -----------------------------------------------------
!||    boltst                 ../engine/source/elements/solid/solide/boltst.F
!||    mmain                  ../engine/source/materials/mat_share/mmain.F90
!||    nsvis_sm12             ../engine/source/elements/solid/solide10/nsvis_sm12.F
!||    s10_icp                ../engine/source/elements/solid/solide4_sfem/s10_icp.F
!||    s10bilan               ../engine/source/elements/solid/solide10/s10bilan.F
!||    s10coor3               ../engine/source/elements/solid/solide10/s10coor3.F
!||    s10cumu3               ../engine/source/elements/solid/solide10/s10cumu3.F
!||    s10cumu3p              ../engine/source/elements/solid/solide10/s10cumu3p.F
!||    s10defo3               ../engine/source/elements/solid/solide10/s10defo3.F
!||    s10defot3              ../engine/source/elements/solid/solide10/s10defot3.F
!||    s10deri3               ../engine/source/elements/solid/solide10/s10deri3.F
!||    s10derit3              ../engine/source/elements/solid/solide10/s10derit3.F
!||    s10divde12             ../engine/source/elements/solid/solide10/s10divde12.F
!||    s10dvm12               ../engine/source/elements/solid/solide10/s10dvm12.F
!||    s10fint3               ../engine/source/elements/solid/solide10/s10fint3.F
!||    s10for_distor          ../engine/source/elements/solid/solide10/s10for_distor.F
!||    s10get_x0              ../engine/source/elements/solid/solide10/s10get_x0.F90
!||    s10get_x3              ../engine/source/elements/solid/solide10/s10get_x3.F
!||    s10len3                ../engine/source/elements/solid/solide10/s10len3.F
!||    s10malla3              ../engine/source/elements/solid/solide10/s10malla3.F
!||    s10mallb3              ../engine/source/elements/solid/solide10/s10mallb3.F
!||    s10mallgeo3            ../engine/source/elements/solid/solide10/s10mallgeo3.F
!||    s10nx3                 ../engine/source/elements/solid/solide10/s10nx3.F
!||    s10nxt4                ../engine/source/elements/solid/solide10/s10nxt4.F
!||    s10pijto3              ../engine/source/elements/solid/solide10/s10pijto3.F
!||    s10rcoor12             ../engine/source/elements/solid/solide10/s10rcoor12.F
!||    s10sav12               ../engine/source/elements/solid/solide10/s10sav12.F
!||    s10sav3                ../engine/source/elements/solid/solide10/s10sav3.F
!||    s10sigp3               ../engine/source/elements/solid/solide4_sfem/s10sigp3.F
!||    s10therm               ../engine/source/elements/solid/solide10/s10therm.F
!||    s10upd11t12            ../engine/source/elements/solid/solide10/s10upd11t12.F
!||    s4therm_itet1          ../engine/source/elements/solid/solide4/s4therm-itet1.F
!||    s8e_sigp               ../engine/source/elements/solid/solide8e/s8e_sig.F
!||    sdistor_ini            ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    sgcoor10               ../engine/source/elements/solid/solide10/sgcoor10.F
!||    sgeodel3               ../engine/source/elements/solid/solide/sgeodel3.F
!||    smallb3                ../engine/source/elements/solid/solide/smallb3.F
!||    smallgeo3              ../engine/source/elements/solid/solide/smallgeo3.F
!||    sordef12               ../engine/source/elements/solid/solidez/sordef12.F
!||    sordeft12              ../engine/source/elements/solid/solidez/sordeft12.F
!||    sorthdir3              ../engine/source/elements/solid/solide/sorthdir3.F
!||    sreploc3               ../engine/source/elements/solid/solide/sreploc3.F
!||    srho3                  ../engine/source/elements/solid/solide/srho3.F
!||    srota3                 ../engine/source/elements/solid/solide/srota3.F
!||    sroto12_sig            ../engine/source/elements/solid/solidez/sroto12_sig.F
!||    sstra3                 ../engine/source/elements/solid/solide/sstra3.F
!||    startime               ../engine/source/system/timer_mod.F90
!||    stoptime               ../engine/source/system/timer_mod.F90
!||    sxfillopt              ../engine/source/elements/solid/solide10/sxfillopt.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    dt_mod                 ../engine/source/modules/dt_mod.F
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    glob_therm_mod         ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    mmain_mod              ../engine/source/materials/mat_share/mmain.F90
!||    nlocal_reg_mod         ../common_source/modules/nlocal_reg_mod.F
!||    s10get_x0_mod          ../engine/source/elements/solid/solide10/s10get_x0.F90
!||    sdistor_ini_mod        ../engine/source/elements/solid/solide/sdistror_ini.F90
!||    sensor_mod             ../common_source/modules/sensor_mod.F90
!||    table_mod              ../engine/share/modules/table_mod.F
!||    timer_mod              ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE S10FORC3(TIMERS,
     1   ELBUF_TAB,       NG,         PM,         GEO,
     2   IXS,             X,          A,          V,
     3   MS,              W,          FLUX,       FLU1,
     4   VEUL,            FV,         ALE_CONNECT,IPARG,
     5   TF,              NPF,        BUFMAT,     PARTSAV,
     6   NLOC_DMG,        DT2T,       NELTST,     ITYPTST,
     7   STIFN,           FSKY,       IADS,       OFFSET,
     8   EANI,            IPARTS,     IXS10,      IADS10,
     9   NEL,             FX,         FY,         FZ,
     A   AR,              VR,         DR,         IPM,
     B   ISTRAIN,         ISOLNOD,    ITASK,      TEMP,
     C   FTHE,            FTHESKY,    IEXPAN,     STIFR,
     D   D,               GRESAV,     GRTH,       IGRTH,
     E   TABLE,           MSSA,       DMELS,      IGEO,
     F   XDP,             VOLN,       CONDN,      CONDNSKY,
     G   SFEM_NODVAR,     ITAGDN,     SENSORS,    IOUTPRT,
     H   MAT_ELEM,        H3D_STRAIN, DT,         IDEL7NOK,
     I   SNPC,            STF,        SBUFMAT,    SVIS,
     J   NSVOIS,          IDTMINS,    IDEL7NG,    MAXFUNC,
     V   USERL_AVAIL,     GLOB_THERM, impl_s,     idyna  ,
     W   S_SFEM_NODVAR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE SENSOR_MOD
      USE DT_MOD
      USE ELBUFDEF_MOD
      USE SDISTOR_INI_MOD, ONLY : SDISTOR_INI
      use glob_therm_mod
      USE S10GET_X0_MOD, ONLY : S10GET_X0
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
#include      "scr03_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   l o c a l   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NPE
      PARAMETER (NPE=10)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER, INTENT(IN) :: S_SFEM_NODVAR
      INTEGER, INTENT(INOUT) :: IDEL7NOK
      INTEGER, INTENT(IN) :: SNPC
      INTEGER, INTENT(IN) :: STF
      INTEGER, INTENT(IN) :: SBUFMAT
      INTEGER, INTENT(IN) :: NSVOIS
      INTEGER, INTENT(IN) :: IDTMINS
      INTEGER ,INTENT(IN) :: IDEL7NG
      INTEGER ,INTENT(IN) :: MAXFUNC
      INTEGER, INTENT(IN) :: IMPL_S
      INTEGER, INTENT(IN) :: IDYNA
      INTEGER, INTENT(IN) :: USERL_AVAIL
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP),NPF(*),IADS(8,*),
     .        IPARTS(*),IXS10(6,*),IADS10(6,*),IPM(*),ITASK,GRTH(*),
     .        IGRTH(*),IGEO(NPROPGI,*),IOUTPRT,H3D_STRAIN
      INTEGER NELTST,ITYPTST,OFFSET,NEL,NG,ISTRAIN,ISOLNOD,IEXPAN,ITAGDN(*)      
      DOUBLE PRECISION  XDP(3,*)
      
      my_real DT2T
      my_real PM(NPROPM,*), GEO(NPROPG,*), X(*), A(*), V(3,*), MS(*), W(*),
     .        FLUX(6,*),FLU1(*), VEUL(*), FV(*), TF(*),
     .        BUFMAT(*),PARTSAV(*),STIFN(*), FSKY(*),EANI(*),
     .        AR(*),VR(*) ,DR(*) ,STIFR(*),D(*), MSSA(*)  ,DMELS(*)
      my_real FX(MVSIZ,10),FY(MVSIZ,10),FZ(MVSIZ,10),
     .        TEMP(*), FTHE(*), FTHESKY(*),GRESAV(*),VOLN(MVSIZ),CONDN(*),
     .        CONDNSKY(*),SFEM_NODVAR(S_SFEM_NODVAR)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (SENSORS_)  ,INTENT(INOUT) :: SENSORS
      TYPE(DT_)        ,INTENT(INOUT) :: DT
      type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IP,LCO,NF1,NF2,IFLAG,IOFFS,IPTR,IPTS,IPTT,ILAY,IMAT
      INTEGER IBID,IBIDON(1),NNEGA,INDEX(MVSIZ),ITET,iel,ISM12_11
C-----
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ)
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,DELTAX2(MVSIZ),
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     . SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     . VDX(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . CONDE(MVSIZ),CONDEG(MVSIZ), VOLG(MVSIZ), JACGM(MVSIZ)
C-----
C Arrays for material laws
      my_real
     .   STI(MVSIZ),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ),
     .   WXXG(MVSIZ) , WYYG(MVSIZ) , WZZG(MVSIZ)
C Arrays for material laws (sph only)
      my_real
     .   MUVOID(MVSIZ)
C-----
C     void MMAIN
      my_real
     . SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6),
     . R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
C For solid elements
      INTEGER NC(MVSIZ,10),ICP,MX,IPLAW1

      DOUBLE PRECISION
     .  XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),
     .  WXX0(MVSIZ) , WYY0(MVSIZ) , WZZ0(MVSIZ),
     .  XX0(MVSIZ,10), YY0(MVSIZ,10), ZZ0(MVSIZ,10),VOLDP(MVSIZ,5)

      my_real
     .  TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),OFF(MVSIZ),VOLP(MVSIZ,5),
     .  RHOO(MVSIZ),OFFS(MVSIZ),THEM(MVSIZ,10),TEMPEL(MVSIZ),
     .  VX(MVSIZ,10),VY(MVSIZ,10),VZ(MVSIZ,10),
     .  PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .  NX(MVSIZ,10,5),VDXX(MVSIZ,10),VDYY(MVSIZ,10),VDZZ(MVSIZ,10),
     .  DXY(MVSIZ),DYX(MVSIZ),DYZ(MVSIZ),DZY(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),
     .  STIG(MVSIZ),  WIP(5,5), ALPH(5,5), BETA(5,5),BID(MVSIZ),
     .  DIE(MVSIZ), MBID(1),OFFG0(MVSIZ),AMU(MVSIZ),SUM,RHO0_1,CNS2
      my_real NXT4(MVSIZ,4,4)
      my_real, DIMENSION(MVSIZ) ::
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C
      my_real
     .    VX0(MVSIZ,10),VY0(MVSIZ,10),VZ0(MVSIZ,10),
     .    MFXX(MVSIZ,5),MFXY(MVSIZ,5),MFYX(MVSIZ,5),
     .    MFYY(MVSIZ,5),MFYZ(MVSIZ,5),MFZY(MVSIZ,5),
     .    MFZZ(MVSIZ,5),MFZX(MVSIZ,5),MFXZ(MVSIZ,5),DIVDE(MVSIZ),
     .    NU(MVSIZ),FAC(MVSIZ),FACP(MVSIZ),E0(MVSIZ),C1,DVM(MVSIZ),
     .    VISP(MVSIZ),FACDB,RBID(MVSIZ),SIGP(NEL,6),
     .    FLD(MVSIZ),STI_C(MVSIZ),LL(MVSIZ),OFFG(MVSIZ),FQMAX
c
      my_real VARNL(NEL),DELTAX4(MVSIZ)
c     Flag Bolt Preloading
      INTEGER IBOLTP,NBPRELD,II(6),ISCTL,ISTAB(MVSIZ)
      INTEGER SZ_IX
      my_real, 
     .  DIMENSION(:), POINTER :: BPRELD
      my_real, dimension(mvsiz) :: fheat
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C--------------------------------------------------------
      DATA WIP / 1.  ,0.  ,0.  ,0.  ,0.  ,
     2           0.  ,0.  ,0.  ,0.  ,0.  ,
     3           0.  ,0.  ,0.  ,0.  ,0.  ,
     4           0.25,0.25,0.25,0.25,0.  ,
     5           0.45,0.45,0.45,0.45,-0.8/
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF  => ELBUF_TAB(NG)%GBUF
      IBOLTP = IPARG(72,NG)
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
      ISM12_11 = ELBUF_TAB(NG)%BUFLY(1)%L_SIGL
C-----
      SZ_IX=NUMELQ+NUMELS+NSVOIS
      NF1=NFT+1
      NF2=NF1-NUMELS8
      IBID = 0
      IBIDON  = 0
      IOFFS=0
       IPTS = 1
       IPTT = 1
       ILAY = 1
      IF(ISROT == 1) THEN
        IISROT=1
        NF2=1
      END IF
      ICP = IPARG(10,NG)
      DO I=LFT,LLT
        OFFS(I)=EP20
      END DO
C      
      DO IP=1,3
        DO J=1,5
          ALPH(J,IP)=ZERO
          BETA(J,IP)=ZERO
        END DO
      END DO
c
      ALPH(1,4)=ZEP5854102
      ALPH(2,4)=ZEP5854102
      ALPH(3,4)=ZEP5854102
      ALPH(4,4)=ZEP5854102
      ALPH(5,4)=ZERO
      ALPH(1,5)=HALF
      ALPH(2,5)=HALF
      ALPH(3,5)=HALF
      ALPH(4,5)=HALF
      ALPH(5,5)=FOURTH
      BETA(1,4)=ZEP1381966
      BETA(2,4)=ZEP1381966
      BETA(3,4)=ZEP1381966
      BETA(4,4)=ZEP1381966
      BETA(5,4)=ZERO
      BETA(1,5)=ONE_OVER_6
      BETA(2,5)=ONE_OVER_6
      BETA(3,5)=ONE_OVER_6
      BETA(4,5)=ONE_OVER_6
      BETA(5,5)=FOURTH
C   
      TEMPEL(:) = ZERO
      FHEAT(:)  = ZERO
      IF (JTHE < 0) THEM(LFT:LLT,1:10) = ZERO
C      
      IF (ICP==1) THEN
        MX = IXS(1,NF1)
        NU(LFT:LLT)=MIN(HALF,PM(21,MX))
        FACP(LFT:LLT)=ONE
      ELSEIF (ICP==2) THEN
        MX = IXS(1,NF1)
        NU(LFT:LLT)=MIN(HALF,PM(21,MX))
        C1 =PM(32,MX)
        E0(LFT:LLT) =THREE*(ONE-TWO*NU(LFT:LLT))*C1
        SIGP=ZERO   
        IF (GBUF%G_PLA>0) THEN
          CALL S8E_SIGP(ELBUF_TAB(NG),SIGP,         NEL)
        END IF 
        CALL S10SIGP3(SIGP,E0 ,GBUF%PLA,FACP ,GBUF%G_PLA,NEL )
      END IF
C-----------
      CALL S10COOR3(
     1   X,           IXS(1,NF1),  IXS10(1,NF2),V,
     2   W,           XX,          YY,          ZZ,
     3   VX,          VY,          VZ,          VDXX,
     4   VDYY,        VDZZ,        VDX,         VDY,
     5   VDZ,         VD2,         VIS,         GBUF%OFF,
     6   OFF,         GBUF%SMSTR,  NC,          NGL,
     7   MXT,         NGEO,        FX,          FY,
     8   FZ,          STIG,        GBUF%SIG,    GBUF%EINT,
     9   GBUF%RHO,    GBUF%QVIS,   GBUF%PLA,    GBUF%EPSD,
     A   VR,          DR,          D,           WXXG,
     B   WYYG,        WZZG,        GBUF%G_PLA,  XDP,
     C   NEL,         CONDEG,      GBUF%G_EPSD, JALE,
     D   ISMSTR,      JEUL,        JLAG,        ISRAT,
     E   ISROT)
C     
      IPLAW1 = 0
      CNS2 = ZERO
      IF (ISM12_11>0 .AND.IDTMIN(1)==3) THEN
         MX = IXS(1,NF1)
         RHO0_1 =PM( 1,MX)
         IF (PM(21,MX)>0.49) IPLAW1=1
         IF (IPLAW1==1) THEN
           FACDB = ONE- ZEP02
           FACDB = MIN(FACDB,TWO*PM(21,MX))
           FACP(LFT:LLT)=FACDB
           VISP(LFT:LLT)=TWO
           CNS2 = ZEP02
           IF (IGEO(35,NGEO(1))>0) CNS2=CNS2-ABS(GEO(17,NGEO(1)))
         END IF 
      ELSEIF (ISMSTR==10.AND.MTN==1) THEN
         MX = IXS(1,NF1)
         RHO0_1 =PM( 1,MX)
         IF (PM(21,MX)>0.49) THEN
           VISP(LFT:LLT)=TWO
           CNS2 = ZEP02
           IF (IGEO(35,NGEO(1))>0) CNS2=CNS2-ABS(GEO(17,NGEO(1)))
         END IF 
      END IF 
      ISCTL = IGEO(97,NGEO(1))
      IF (ISROT == 1) ISCTL = 0 ! uncompatible with itetra4=1
C     
      CALL S10NX3(
     1   NX,      NEL,     NPT)
      IF(JTHE < 0 .AND. ISOLNOD == 4) CALL S10NXT4(NXT4,NEL)
C-----------
C GATHER NODAL VARIABLES FOR TOTAL STRAIN CASE.
C-----------
      IF (ISMSTR >= 10.AND.ISMSTR <= 12) THEN
       CALL SGCOOR10(
     1   XX,        YY,        ZZ,        X,
     2   XDP,       XX0,       YY0,       ZZ0,
     3   VX0,       VY0,       VZ0,       GBUF%SMSTR,
     4   NC,        D,         GBUF%OFF,  OFFG0,
     5   NEL,       MTN,       ISMSTR)
C----------------------
       IF (ISMSTR ==  11) THEN
        CALL S10DERIT3(
     1   VOLP,       DELTAX,     DELTAX2,    XX0,
     2   YY0,        ZZ0,        PX,         PY,
     3   PZ,         NX,         RX,         RY,
     4   RZ,         SX,         SY,         SZ,
     5   TX,         TY,         TZ,         WIP(1,NPT),
     6   ALPH(1,NPT),BETA(1,NPT),VOLN,       VOLG,
     7   VOLDP,      NEL,        GBUF%OFF,   NPT)
        CALL S10LEN3(
     1   VOLP,         NGL,          DELTAX,       DELTAX2,
     2   PX,           PY,           PZ,           VOLG,
     3   GBUF%VOL,     RX,           RY,           RZ,
     4   SX,           SY,           SZ,           TX,
     5   TY,           TZ,           NC,           NEL,
     6   MXT,          PM,           GBUF%ISMS,    GBUF%DT_PITER,
     7   NPT,          IINT,         ISROT,        IFORMDT)
       ELSE
C!!!!!!calcul local rep for ISMSTR 10 to 11 (offg>un) 
        IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
         CALL S10RCOOR12(
     1   GBUF%OFF,X,       XDP,     NC,
     2   E1X,     E2X,     E3X,     E1Y,
     3   E2Y,     E3Y,     E1Z,     E2Z,
     4   E3Z,     NEL)
        END IF !(ISMSTR == 10.AND.ISORTH/=0.AND.IDTMIN(1)==3)
        IBID = 1
        DO IP=1,NPT
         LBUF => ELBUF_TAB(NG)%BUFLY(IBID)%LBUF(IP,IBID,IBID)
         CALL S10PIJTO3(PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),LBUF%PIJ,LLT)
        ENDDO
       END IF !(ISMSTR ==  11) THEN
       DO IP=1,NPT
        CALL S10DEFOT3(
     1   PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),VX0,
     2   VY0,       VZ0,       MFXX(1,IP),MFXY(1,IP),
     3   MFXZ(1,IP),MFYX(1,IP),MFYY(1,IP),MFYZ(1,IP),
     4   MFZX(1,IP),MFZY(1,IP),MFZZ(1,IP),NEL)
       END DO 
       IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
         DO IP=1,NPT
          CALL SORDEFT12(LFT,LLT,MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP),
     .         MFYX(1,IP), MFYY(1,IP), MFYZ(1,IP),
     .         MFZX(1,IP), MFZY(1,IP), MFZZ(1,IP),
     .         E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,GBUF%OFF)
         END DO 
       ENDIF
      ENDIF     ! ISMSTR == 10
C-----------
      IF (ISMSTR /= 11) THEN
      CALL S10DERI3(
     1   OFF,        VOLP,       NGL,        DELTAX,
     2   DELTAX2,    XX,         YY,         ZZ,
     3   PX,         PY,         PZ,         NX,
     4   RX,         RY,         RZ,         SX,
     5   SY,         SZ,         TX,         TY,
     6   TZ,         WIP(1,NPT), ALPH(1,NPT),BETA(1,NPT),
     7   VOLN,       VOLG,       VOLDP,      NC,
     8   GBUF%SMSTR, GBUF%OFF,   NEL,        NPT,
     9   ISMSTR,     JLAG)
C
        CALL S10LEN3(
     1   VOLP,         NGL,          DELTAX,       DELTAX2,
     2   PX,           PY,           PZ,           VOLG,
     3   GBUF%VOL,     RX,           RY,           RZ,
     4   SX,           SY,           SZ,           TX,
     5   TY,           TZ,           NC,           NEL,
     6   MXT,          PM,           GBUF%ISMS,    GBUF%DT_PITER,
     7   NPT,          IINT,         ISROT,        IFORMDT)

        IF (IPLAW1>0) THEN
           CALL S10DVM12(
     .               PX, PY, PZ, VX, VY, VZ, 
     .               DVM ,GBUF%OFF, NPT ,NEL)
        END IF
C--- for /CST avoid multi-printout----
       IF (ISMSTR == 2 .OR.ISMSTR ==12) THEN
        DO IP=1,NPT
         IPTR = IP
         LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
         DO I=LFT,LLT
          IF (GBUF%OFF(I)==TWO) LBUF%OFF(I)=GBUF%OFF(I)
         ENDDO
        ENDDO
       END IF 
      END IF 
C --------------------------
C  --- UPDATE REF. CONFIGURATION 
C --------------------------
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
        CALL S10SAV3(
     1   GBUF%OFF,  GBUF%SMSTR,NC,        XX,
     2   YY,        ZZ,        NEL)
      END IF !(ISMSTR == 2) THEN
C-----------------------------
      IF (ISORTH == 0) THEN            
        DO I=LFT,LLT                                            
          GAMA(I,1) = ONE                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = ONE                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SREPLOC3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      E1X,     E2X,     E3X,
     4   E1Y,     E2Y,     E3Y,     E1Z,
     5   E2Z,     E3Z,     LLT)
        CALL SORTHDIR3(
     1   RX,       RY,       RZ,       SX,
     2   SY,       SZ,       TX,       TY,
     3   TZ,       E1X,      E2X,      E3X,
     4   E1Y,      E2Y,      E3Y,      E1Z,
     5   E2Z,      E3Z,      GBUF%GAMA,GAMA,
     6   NEL,      IREP)
      ENDIF
      IF(ICP >0 .AND. ISMSTR/=10) THEN
         DO I=LFT,LLT
          IF(GBUF%OFF(I) == ZERO) CYCLE
          SUM=SFEM_NODVAR(NC(I,1))+SFEM_NODVAR(NC(I,2))+SFEM_NODVAR(NC(I,3))+SFEM_NODVAR(NC(I,4))
          JACGM(I)=FOURTH*SUM
         ENDDO
      ENDIF
C-----------------------------
C     POINTS D' INTEGRATION 
C-----------------------------
      DO IP=1,NPT
        IPTR = IP
        LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
       IF (IOFFS == 1)THEN
        DO I=LFT,LLT
         IF (OFFS(I)<=TWO) LBUF%OFF(I)=OFFS(I)
        ENDDO
       END IF
C
        CALL S10DEFO3(
     1   PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),VX,
     2   VY,        VZ,        DXX,       DXY,
     3   DXZ,       DYX,       DYY,       DYZ,
     4   DZX,       DZY,       DZZ,       D4,
     5   D5,        D6,        WXX,       WYY,
     6   WZZ,       VOLP(1,IP),VOLN,      LBUF%RHO,
     7   RHOO,      NEL,       JHBE,      ISROT)

         IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
          CALL SORDEF12(LFT,LLT,DXX, DYY, DZZ,
     .         D4, D5, D6,
     .         E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
         ENDIF
         IF (ICP>0) THEN
           IF (ISMSTR==10) THEN
             DO I=LFT,LLT
               IF(GBUF%OFF(I) == ZERO) CYCLE
               JACGM(I)=SFEM_NODVAR(NC(I,IP))
             ENDDO
           END IF
           CALL S10_ICP(
     1   GBUF%OFF,   JACGM,      FACP,       NU,
     2   MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP), MFYX(1,IP),
     3   MFYY(1,IP), MFYZ(1,IP), MFZX(1,IP), MFZY(1,IP),
     4   MFZZ(1,IP), LBUF%VOL,   VOLN,       LBUF%VOL0DP,
     5   VOLDP(1,IP),NEL,        ISMSTR)
         ENDIF
C   
        DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))  
        IF (IPLAW1>0) CALL S10DIVDE12(DVM ,DIVDE ,FACP,GBUF%OFF,NEL)
        CALL SRHO3(
     1   PM,         LBUF%VOL,   LBUF%RHO,   LBUF%EINT,
     2   DIVDE,      FLUX(1,NF1),FLU1(NF1),  VOLN,
     3   DVOL,       NGL,        MXT,        OFF,
     4   0,          GBUF%TAG22, VOLDP(1,IP),LBUF%VOL0DP,
     5   AMU,        GBUF%OFF,   NEL,        MTN,
     6   JALE,       ISMSTR,     JEUL,       JLAG)
c
       IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
          CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .                     E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!          CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .                 E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
       ENDIF
        CALL SROTA3(
     1   LBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
C-----------------------------
C       SMALL STRAIN
C-----------------------------
        CALL S10MALLA3(
     1   GBUF%OFF,   OFF,        WXX,        WYY,
     2   WZZ,        WXXG,       WYYG,       WZZG,
     3   WIP(IP,NPT),NEL,        ISMSTR,     JLAG)
           
        IF(JTHE < 0 ) THEN                                        
          DO I=LFT,LLT                                            
            TEMPEL(I)= ZERO                                       
          ENDDO                                                   
          IF(ISOLNOD == 10) THEN      
          DO J = 1,10                                             
            DO I=LFT,LLT                                          
              TEMPEL(I)= TEMPEL(I) +  NX(I,J,IP)*TEMP(NC(I,J))       
            ENDDO                                                 
          ENDDO                                                   
          ELSEIF(ISOLNOD == 4) THEN
            DO J = 1,4                                             
              DO I=LFT,LLT                                          
                TEMPEL(I)= TEMPEL(I) +  NXT4(I,J,IP)*TEMP(NC(I,J))
              ENDDO 
            ENDDO 
        ENDIF                                                     
        ENDIF                                                     
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(TIMERS,35)
C
      IF(IBOLTP /= 0) CALL BOLTST(
     .                IP,        BPRELD,    LBUF%SIG,TT,        
     .                NEL   ,NPT   ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB)
C
        CALL MMAIN(TIMERS,
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2                ALE_CONNECT, IXS,         IPARG,
     3   V,           TF,          NPF,         BUFMAT,
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
     E   VDX,         VDY,         VDZ,         MUVOID,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   R1_FREE,     LBUF%PLA,    R3_FREE,     AMU,
     H   MFXX(1,IP),  MFXY(1,IP),  MFXZ(1,IP),  MFYX(1,IP),
     I   MFYY(1,IP),  MFYZ(1,IP),  MFZX(1,IP),  MFZY(1,IP),
     J   MFZZ(1,IP),  IPM,         GAMA,        BID,
     K   BID,         BID,         BID,         BID,
     L   BID,         BID,         ISTRAIN,     TEMPEL,
     M   DIE,         IEXPAN,      ILAY,        MSSA,
     N   DMELS,       IPTR,        IPTS,        IPTT,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         IPARG(1,NG), IGEO,        CONDE,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH,        MVSIZ,
     *   SNPC,        STF,         SBUFMAT,     GLOB_THERM, 
     S   SVIS,        SZ_IX,       IRESP,
     T   N2D,         th_strain,   NGROUP,      TT, 
     .   DT1,         NTABLE,      NUMELQ,      nummat,
     .   numgeo,      numnod,      numels,
     .   IDEL7NOK,    IDTMIN,      MAXFUNC,
     .   IMON_MAT,    USERL_AVAIL, impl_s,
     .   idyna,       DT,          fheat      ,sensors)
C
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(TIMERS,35)
C-----------  return to global system    
       IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
         CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .                 E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!         CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .                 E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
         IF (ISTRAIN == 1) THEN 
          CALL SORDEF12(LFT,LLT,DXX, DXY, DXZ,
     .         D4, D5, D6,
     .         E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
         ENDIF
       ENDIF
        IF (ISTRAIN == 1) CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
C--------------------------
        IFLAG=MOD(NCYCLE,NCPRI)
        IF(IOUTPRT>0)THEN
           CALL S10BILAN(PARTSAV,LBUF%EINT,LBUF%RHO,LBUF%RK,LBUF%VOL,
     .                   VX, VY, VZ,NX(1,1,IP),VOLN,IPARTS,
     .                   GRESAV,GRTH,IGRTH,IEXPAN,LBUF%EINTTH,
     .                   GBUF%FILL,XX,YY,ZZ,ITASK,IPARG(1,NG),GBUF%OFF,SENSORS,
     .                   NEL, ELBUF_TAB(NG)%BUFLY(ILAY)%L_WPLA, LBUF%WPLA)
        ENDIF
C-------------------------
        IF (CNS2>ZERO)
     .     CALL NSVIS_SM12(VISP  ,CNS2,CXX  ,VOLN ,DXX     ,
     .                     DYY     ,DZZ    ,D4    ,D5  ,D6   ,
     .                     LBUF%VOL,RHO0_1,STI   ,NEL  ,SVIS ) 
C----------------------------
C       INTERNAL FORCES
C----------------------------
        CALL S10FINT3(
     1   LBUF%SIG,   PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP),
     2   FX,         FY,         FZ,         VOLN,
     3   QVIS,       STI,        STIG,       LBUF%EINT,
     4   LBUF%RHO,   LBUF%QVIS,  LBUF%PLA,   LBUF%EPSD,
     5   GBUF%EPSD,  GBUF%SIG,   GBUF%EINT,  GBUF%RHO,
     6   GBUF%QVIS,  GBUF%PLA,   WIP(IP,NPT),GBUF%G_PLA,
     7   NEL,        CONDE,      CONDEG,     GBUF%G_EPSD,
     8   ISRAT,      SVIS ,GLOB_THERM%NODADT_THERM)
C
        DO I=LFT,LLT
         IF (LBUF%OFF(I) > ONE .AND. GBUF%OFF(I) == ONE) THEN
C          switched to small strain
           OFFS(I)=MIN(LBUF%OFF(I),OFFS(I))
           IOFFS  =1
         END IF
        ENDDO
C
        IF (JTHE < 0 .AND. ISOLNOD == 10) THEN
          IMAT = MXT(1)
          IF (MAT_ELEM%MAT_PARAM(IMAT)%HEAT_FLAG == 1) THEN
            CALL S10THERM(
     1           PM,        IMAT,       NC,        VOLN,
     2           PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),NX(1,1,IP),
     3           DT1,       TEMP,      TEMPEL,    FHEAT,
     4           THEM,      GBUF%OFF,  LBUF%OFF,  NEL,
     5           GLOB_THERM%THEACCFACT)
          ELSE
            CALL S10THERM(
     1           PM,        IMAT,       NC,        VOLN,
     2           PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),NX(1,1,IP),
     3           DT1,       TEMP,      TEMPEL,    DIE,
     4           THEM,      GBUF%OFF,  LBUF%OFF,  NEL,
     5           GLOB_THERM%THEACCFACT)
          END IF
        ENDIF
c----------
      ENDDO   ! end integration point loop

      IF (JTHE < 0 .AND. ISOLNOD == 4) THEN
          IMAT = MXT(1)
          IF (MAT_ELEM%MAT_PARAM(IMAT)%HEAT_FLAG == 1) THEN
            CALL S4THERM_ITET1(PM   ,IMAT   ,NC   ,NEL  ,
     .                         XX   ,YY     ,ZZ   ,DT1  ,FHEAT ,
     .                         TEMP ,THEM   ,GBUF%OFF   ,LBUF%OFF,
     .                         GLOB_THERM%THEACCFACT)
          ELSE
            CALL S4THERM_ITET1(PM   ,IMAT   ,NC   ,NEL  ,
     .                         XX   ,YY     ,ZZ   ,DT1  ,DIE ,
     .                         TEMP ,THEM   ,GBUF%OFF   ,LBUF%OFF,
     .                         GLOB_THERM%THEACCFACT)
          END IF
      ENDIF
c-----------------------------
      IF (JLAG+JALE+JEUL /= 0) THEN
c
C-----  small strain
C------ correction of GBUF%SMSTR will be done only from next cycle
        CALL S10MALLB3(
     1   GBUF%SMSTR,GBUF%OFF,  WXXG,      WYYG,
     2   WZZG,      NEL,       ISMSTR,    JLAG)
        IF (IOFFS == 1)THEN
          DO I=LFT,LLT
C           switch to small strain
            IF (OFFS(I)<=TWO) GBUF%OFF(I) = OFFS(I)
          END DO
C
        IPTS = 1
        IPTT = 1
        ILAY = 1
          DO IP=1,NPT
           IPTR = IP
           LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
            DO I=LFT,LLT
              IF (GBUF%OFF(I) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
            END DO
          END DO
        END IF
c
        ITET=1
        CALL S10MALLGEO3(NGL,GBUF%OFF ,VOLG ,DELTAX, GBUF%VOL ,
     .                       RX , RY , RZ , 
     .                       SX , SY , SZ ,
     .                       TX , TY , TZ ,DELTAX4,GEO(1,NGEO(1)),
     .                       NEL,NPT,ISMSTR,ISROT,DT)
        RBID(LFT:LLT)=ZERO
        CALL SGEODEL3(NGL,GBUF%OFF,VOLG,DELTAX4,GBUF%VOL,GEO(1,NGEO(1)),RBID,DT,NEL,IDEL7NOK)
        CALL SMALLB3(GBUF%OFF,OFF,NEL,ISMSTR)
        CALL SMALLGEO3(NGL, GBUF%OFF ,VOLG ,DELTAX4, GBUF%VOL ,ITET, NEL, ISMSTR,DT)
C-----------!!!-seperate LBUF%SIGL=LBUF%SIG,w/ npt>0 ISM12_11
       IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        IOFFS =0
         DO I=LFT,LLT
          IF(GBUF%OFF(I)/=OFFG0(I).AND.ABS(GBUF%OFF(I)) > ONE ) IOFFS=1
         ENDDO
         IF (IOFFS == 1) THEN
           CALL S10SAV12(
     1   GBUF%OFF,  OFFG0,     GBUF%SMSTR,NC,
     2   XX,        YY,        ZZ,        NEL)
           IF (ISM12_11>0 .AND. ISORTH == 0) THEN
             CALL S10UPD11T12(
     1   ELBUF_TAB(NG),GBUF%OFF,     OFFG0,        NC,
     2   XX,           YY,           ZZ,           NEL,
     3   NPT)
           END IF 
          IPTS = 1
          IPTT = 1
          ILAY = 1
          DO IP=1,NPT
           IPTR = IP
           LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
            DO I=LFT,LLT
              IF (ABS(GBUF%OFF(I)) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
            END DO
          END DO
         END IF !(IOFFS == 1) THEN
       END IF 
C----------------------------
C     distortion control  
C----------------------------
      IF (ISCTL > 0) THEN
         OFFG(1:NEL) = MIN(OFF(1:NEL),ABS(GBUF%OFF(1:NEL))) ! no offg for tetra10 
         CALL SDISTOR_INI(                                      
     1                NEL      ,STI_C    ,NPROPM     ,NUMMAT  ,       
     2                ISMSTR   ,MXT      ,ISTAB      ,PM      ,       
     3                GBUF%SIG ,GBUF%RHO ,CXX        ,OFFG    ,       
     4                GBUF%OFF ,LL       ,VOLN       ,FLD     ,       
     5                CNS2     ,FQMAX    )
         IF (ISMSTR>=11)  ! XX,YY,ZZ now current conf.
     *     CALL S10GET_X3(
     1          X,     XDP,      DR,    NUMNOD,
     2         XX,      YY,      ZZ,        NC,
     3      ISROT,   IRESP,    NEL )
         IF (ISMSTR<10)  ! remove limitation of Ismstr>=
     *     CALL S10GET_X0(
     1        NEL,  NUMNOD,       X,     XDP,         
     2          D,     XX0,     YY0,     ZZ0,
     3         NC)
         CALL S10FOR_DISTOR(
     .         STIG,   FLD  ,   STI_C,
     .          XX ,     YY ,     ZZ ,   
     .          VX ,     VY ,     VZ ,     
     .          FX ,     FY ,     FZ ,     
     .          XX0,     YY0,     ZZ0,
     .         CNS2,   ISTAB,    LL  ,
     .        FQMAX,    NEL ,GBUF%EINT_DISTOR,
     .          DT1)
      ENDIF
c-----------------------------
      IF(NFILSOL/=0) CALL SXFILLOPT(
     1   NPE,      GBUF%FILL,STIG,     FX,
     2   FY,       FZ,       NEL)
C-----
        IF (IPARIT == 0) THEN
          CALL S10CUMU3(
     1   GBUF%OFF,  A,         NC,        STIFN,
     2   STIG,      FX,        FY,        FZ,
     3   DELTAX2,   THEM,      FTHE,      AR,
     4   X,         STIFR,     GBUF%SMSTR,CONDN,
     5   CONDEG,    ITAGDN,    NEL,       ISMSTR,
     6   JTHE,      ISROT ,GLOB_THERM%NODADT_THERM)
          ELSE
          CALL S10CUMU3P(
     1   GBUF%OFF,  STIG,      FSKY,      FSKY,
     2   IADS,      FX,        FY,        FZ,
     3   DELTAX2,   IADS10,    NC,        THEM,
     4   FTHESKY,   AR,        X,         GBUF%SMSTR,
     5   CONDNSKY,  CONDEG,    ITAGDN,    NEL,
     6   NFT,       ISMSTR,    JTHE,      ISROT,GLOB_THERM%NODADT_THERM)
        ENDIF
c-----
      ENDIF
c-----------
      RETURN
      END
