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
Chd|  SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        AMASS3                        source/elements/solid/solide/amass3.F
Chd|        AMASS3P                       source/elements/solid/solide/amass3.F
Chd|        AMOMT3                        source/elements/solid/solide/amomt3.F
Chd|        BOLTST                        source/elements/solid/solide/boltst.F
Chd|        CHECK_OFF_ALE                 source/elements/solid/solide/check_off_ale.F
Chd|        EMOMT3B                       source/elements/solid/solide/emomt3b.F
Chd|        EPXLE3                        source/elements/solid/solide/epxle3.F
Chd|        FE_CLOSE                      source/elements/solid/solide/fe_close.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MOD_CLOSE                     source/elements/solid/solide/mod_close.F
Chd|        NSVIS_SM12                    source/elements/solid/solide10/nsvis_sm12.F
Chd|        S11DEFO3                      source/elements/solid/solide/s11defo3.F
Chd|        S8SAV12                       source/elements/solid/solide/srcoor3.F
Chd|        S8SAV3                        source/elements/solid/solide/srcoor3.F
Chd|        S8UPD11T12                    source/elements/solid/solide/sforc3.F
Chd|        SCOOR_CP2SP                   source/elements/solid/solidez/szforc3.F
Chd|        SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3.F
Chd|        SDEFO3                        source/elements/solid/solide/sdefo3.F
Chd|        SDEFOT3                       source/elements/solid/solide/sdefo3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SDLEN8                        source/elements/solid/solidez/szforc3.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/szforc3.F
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SFINT3                        source/elements/solid/solide/sfint3.F
Chd|        SFINT_REG                     source/elements/solid/solide/sfint_reg.F
Chd|        SGCOOR3                       source/elements/solid/solide/sgcoor3.F
Chd|        SMALLB3                       source/elements/solid/solide/small3.F
Chd|        SMALLGEO3                     source/elements/solid/solide/small3.F
Chd|        SORDEFT3                      source/elements/solid/solidez/szforc3.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SRMALLA11                     source/elements/solid/solide/small3.F
Chd|        SRMALLA3                      source/elements/solid/solide/srmall3.F
Chd|        SROTO3                        source/elements/solid/solidez/szforc3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        SRROTADP                      source/elements/solid/solide/srrota3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STHERM                        source/elements/solid/solide/stherm.F
Chd|        STOPTIME                      source/system/timer.F         
Chd|        STORTH3                       source/elements/solid/solidez/szforc3.F
Chd|        SZDERI3                       source/elements/solid/solidez/szderi3.F
Chd|        SZDERIT3                      source/elements/solid/solidez/szderi3.F
Chd|        SZDERITO3                     source/elements/solid/solidez/szderi3.F
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|        SZORDEF3                      source/elements/solid/solidez/szforc3.F
Chd|        SZTORTH3                      source/elements/solid/solidez/szforc3.F
Chd|        VISC_ET                       source/elements/solid/solidez/szforc3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SZFORC3(ELBUF_TAB,NG      ,
     1                   PM       ,GEO     ,IXS     ,X      ,
     2                   A        ,V       ,MS      ,W      ,FLUX   ,
     3                   FLU1     ,VEUL      ,FV      ,ALE_CONNECT    ,IPARG    ,
     4                   TF       ,NPF     ,BUFMAT  ,PARTSAV,
     5                   DT2T     ,NELTST  ,ITYPTST ,STIFN  ,FSKY   ,
     6                   IADS     ,OFFSET  ,EANI    ,IPARTS ,ICP    ,
     7                   F11      ,F21     ,F31     ,F12    ,F22    ,
     8                   F32      ,F13     ,F23     ,F33    ,F14    ,
     9                   F24      ,F34     ,F15     ,F25    ,F35    ,
     A                   F16      ,F26     ,F36     ,F17    ,F27    ,
     B                   F37      ,F18     ,F28     ,F38    ,NEL    ,
     C                   FSKYM    ,MSNF    ,NVC     ,IPM    ,ITASK  , 
     D                   QMV      ,ISTRAIN ,IMATVIS ,TEMP   ,FTHE   , 
     E                   FTHESKY  ,IEXPAN  ,GRESAV  ,GRTH   ,IGRTH  , 
     F                   MSSA     ,DMELS   ,TABLE   ,IGEO   ,XDP    ,
     G                   CONDN    ,CONDNSKY  ,
     H                   D        ,TAGPRT_SMS,SENSOR_TAB,IOUTPRT  ,
     I                   NALE     ,NLOC_DMG  ,MATPARAM_TAB,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
      USE SENSOR_MOD
      USE ALE_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "scr07_c.inc"
#include      "com08_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP), NPF(*),IADS(8,*),
     .        IPARTS(*),IPM(NPROPMI,NUMMAT),ITASK,IMATVIS,GRTH(*),IGRTH(*),
     .        IGEO(NPROPGI,*), TAGPRT_SMS(*)        
      INTEGER NELTST,ITYPTST,OFFSET,NEL ,ICP, NDDIM,
     .        NVC,ISTRAIN,IEXPAN,NG,NPTS,IOUTPRT,NALE(*),H3D_STRAIN

      DOUBLE PRECISION 
     .        XDP(3,*)
      my_real
     .   DT2T
      my_real
     .   PM(NPROPM,NUMMAT), GEO(NPROPG,*), X(*), A(*), V(3,*), MS(*), W(*), 
     .   FLUX(6,*), FSKYM(*),
     .   FLU1(*), VEUL(*), FV(*), TF(*), BUFMAT(*),
     .   PARTSAV(*),STIFN(*), FSKY(*),EANI(*),MSNF(*),
     .   F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),
     .   F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .   F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),
     .   F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),
     .   F15(MVSIZ),F25(MVSIZ),F35(MVSIZ),
     .   F16(MVSIZ),F26(MVSIZ),F36(MVSIZ),
     .   F17(MVSIZ),F27(MVSIZ),F37(MVSIZ),
     .   F18(MVSIZ),F28(MVSIZ),F38(MVSIZ),
     .   TEMP(*), FTHE(*), FTHESKY(*),
     .   QMV(12,*),GRESAV(*), MSSA(*), DMELS(*),
     .   CONDN(*),CONDNSKY(*),D(*)

 
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,LCO,NF1,IFLAG,ILAY,IPTR,IPTS,IPTT,II(6)
      INTEGER IBID,IBIDV(1),ITET,IADBUF,IVISC,IP,NNOD
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),INOD(8),
     .        IPOS(8), L_NLOC, INLOC
      my_real
     . VOLN(MVSIZ), VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ),
     . JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ), JAC9(MVSIZ),
     . VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . CONDE(MVSIZ),DIVDE(MVSIZ)
                 
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),     
     .   X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8),VOLDP(MVSIZ)
C-----
C
      my_real
     .   STI(MVSIZ),GAMA(MVSIZ,6),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ)
C
      my_real
     .   MUVOID(MVSIZ)
C-----
C 8-nodes solid connectivities
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      my_real
     .  OFF(MVSIZ) , RHOO(MVSIZ),HH(MVSIZ),BID(1),
     .  X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .  X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .  Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .  Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .  Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .  Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .  VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .  VX5(MVSIZ),VX6(MVSIZ),VX7(MVSIZ),VX8(MVSIZ),
     .  VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .  VY5(MVSIZ),VY6(MVSIZ),VY7(MVSIZ),VY8(MVSIZ),
     .  VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .  VZ5(MVSIZ),VZ6(MVSIZ),VZ7(MVSIZ),VZ8(MVSIZ),
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),
     .  PX1H1(MVSIZ),PX2H1(MVSIZ),PX3H1(MVSIZ),PX4H1(MVSIZ),
     .  PX1H2(MVSIZ),PX2H2(MVSIZ),PX3H2(MVSIZ),PX4H2(MVSIZ),
     .  PX1H3(MVSIZ),PX2H3(MVSIZ),PX3H3(MVSIZ),PX4H3(MVSIZ),
     .  PX1H4(MVSIZ),PX2H4(MVSIZ),PX3H4(MVSIZ),PX4H4(MVSIZ),
     .  VDX1(MVSIZ),VDX2(MVSIZ),VDX3(MVSIZ),VDX4(MVSIZ),
     .  VDX5(MVSIZ),VDX6(MVSIZ),VDX7(MVSIZ),VDX8(MVSIZ),
     .  VDY1(MVSIZ),VDY2(MVSIZ),VDY3(MVSIZ),VDY4(MVSIZ),
     .  VDY5(MVSIZ),VDY6(MVSIZ),VDY7(MVSIZ),VDY8(MVSIZ),
     .  VDZ1(MVSIZ),VDZ2(MVSIZ),VDZ3(MVSIZ),VDZ4(MVSIZ),
     .  VDZ5(MVSIZ),VDZ6(MVSIZ),VDZ7(MVSIZ),VDZ8(MVSIZ),
     .  VGXA(MVSIZ),VGYA(MVSIZ),VGZA(MVSIZ), VGA2(MVSIZ),
     .  DXY(MVSIZ),DYX(MVSIZ),DYZ(MVSIZ),DZY(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),
     .  R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .  R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .  R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .  G1X(MVSIZ),G2X(MVSIZ),G3X(MVSIZ),
     .  G1Y(MVSIZ),G2Y(MVSIZ),G3Y(MVSIZ),
     .  G1Z(MVSIZ),G2Z(MVSIZ),G3Z(MVSIZ),
     .  N1X(MVSIZ),N2X(MVSIZ),N3X(MVSIZ),
     .  N1Y(MVSIZ),N2Y(MVSIZ),N3Y(MVSIZ),
     .  N1Z(MVSIZ),N2Z(MVSIZ),N3Z(MVSIZ),
     .  N4X(MVSIZ),N5X(MVSIZ),N6X(MVSIZ),
     .  N4Y(MVSIZ),N5Y(MVSIZ),N6Y(MVSIZ),
     .  N4Z(MVSIZ),N5Z(MVSIZ),N6Z(MVSIZ),
     .  VX0(MVSIZ,8),VY0(MVSIZ,8),VZ0(MVSIZ,8),
     .  MFXX(MVSIZ),MFXY(MVSIZ),MFYX(MVSIZ),
     .  MFYY(MVSIZ),MFYZ(MVSIZ),MFZY(MVSIZ),
     .  MFZZ(MVSIZ),MFZX(MVSIZ),MFXZ(MVSIZ),
     .  TEMPEL(MVSIZ),THEM(MVSIZ,8),DIE(MVSIZ),
     .  SIGY(MVSIZ), SIGO(NEL,6),ET(MVSIZ), SIGN(NEL,6),
     .  R1_FREE(MVSIZ),R3_DAM(MVSIZ),R4_FREE(MVSIZ),OFFG0(MVSIZ),AMU(MVSIZ),
     .  XGXA(MVSIZ),XGYA(MVSIZ),XGZA(MVSIZ),
     .  XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),
     .  XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ),CNS2,RHO0_1
      my_real, 
     .  DIMENSION(:), POINTER :: EINT
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----
C----- 
C     IBOLTP : flag bolt preloading
      INTEGER IBOLTP,NBPRELD,IMAT,ISM12_11,MX
      my_real, DIMENSION(:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: BPRELD,DNL
      my_real :: P(MVSIZ)      
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_TAB(NG)%GBUF
      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
      ISM12_11 = ELBUF_TAB(NG)%BUFLY(1)%L_SIGL
      NPTS = IPARG(6,NG) 
      INLOC = IPARG(78,NG)
      ALLOCATE(VAR_REG(NEL))   
      IBID = 0
      IBIDV = 0
      IF (JCVT==1 .AND. ISORTH>0) JCVT=2
C
      IBOLTP = IPARG(72,NG)
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
C
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
C
      NF1=NFT+1
C
      DO I=1,NEL
        SIGY(I)   = EP20
        SIGO(I,1) = GBUF%SIG(II(1)+I)
        SIGO(I,2) = GBUF%SIG(II(2)+I)	  
        SIGO(I,3) = GBUF%SIG(II(3)+I)	  
        SIGO(I,4) = GBUF%SIG(II(4)+I)	  
        SIGO(I,5) = GBUF%SIG(II(5)+I)	  
        SIGO(I,6) = GBUF%SIG(II(6)+I)
      ENDDO
C
C Gather nodal variables and compute rotation
      CALL SRCOOR3(X,IXS(1,NF1),V ,W  ,GBUF%GAMA ,GAMA  ,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   VD2,VIS,GBUF%OFF,OFF,GBUF%SMSTR,GBUF%RHO,RHOO,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .   IOUTPRT, VGXA, VGYA, VGZA, VGA2,
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8, 
     .   XDP, X0 , Y0 , Z0 , NEL, XGXA, XGYA, XGZA,
     .   XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,IPARG(1,NG),
     .   GBUF%GAMA_R) 
C
      CNS2 = ZERO
      IF (ICP==1.AND.MTN==1.AND.ISMSTR==12) THEN
         MX = MXT(LFT)
         RHO0_1 =PM( 1,MX)
         CNS2 = ZEP02
         IF (IGEO(35,NGEO(1))>0) CNS2=CNS2-ABS(GEO(17,NGEO(1)))
      END IF
C Total strain Pij are in global system for ISMSTR=10,12 and in local system for ISMSTR=11
      IF ((ISMSTR >= 10.AND.ISMSTR <= 12).AND.JLAG > 0) THEN
        CALL SGCOOR3(TT,8,X,IXS(1,NF1),
     .    X0, Y0, Z0, VX0, VY0, VZ0, 
     .    GBUF%SMSTR,D ,GBUF%OFF,OFFG0,NEL ,XDP )
        IF (ISMSTR == 11) THEN
        CALL SRROTADP(
     .    R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .    X0(1,1), X0(1,2), X0(1,3), X0(1,4), 
     .    X0(1,5), X0(1,6), X0(1,7), X0(1,8),
     .    Y0(1,1), Y0(1,2), Y0(1,3), Y0(1,4), 
     .    Y0(1,5), Y0(1,6), Y0(1,7), Y0(1,8),
     .    Z0(1,1), Z0(1,2), Z0(1,3), Z0(1,4), 
     .    Z0(1,5), Z0(1,6), Z0(1,7), Z0(1,8) )
        CALL SRROTA3(
     .    R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .    VX0(1,1), VX0(1,2), VX0(1,3), VX0(1,4), 
     .    VX0(1,5), VX0(1,6), VX0(1,7), VX0(1,8),
     .    VY0(1,1), VY0(1,2), VY0(1,3), VY0(1,4), 
     .    VY0(1,5), VY0(1,6), VY0(1,7), VY0(1,8),
     .    VZ0(1,1), VZ0(1,2), VZ0(1,3), VZ0(1,4), 
     .    VZ0(1,5), VZ0(1,6), VZ0(1,7), VZ0(1,8) )
        CALL SZDERIT3(OFF,VOLN,NGL,NEL,
     .    X0(1,1), X0(1,2), X0(1,3), X0(1,4), 
     .    X0(1,5), X0(1,6), X0(1,7), X0(1,8),
     .    Y0(1,1), Y0(1,2), Y0(1,3), Y0(1,4), 
     .    Y0(1,5), Y0(1,6), Y0(1,7), Y0(1,8),
     .    Z0(1,1), Z0(1,2), Z0(1,3), Z0(1,4), 
     .    Z0(1,5), Z0(1,6), Z0(1,7), Z0(1,8),
     .    PX1, PX2, PX3, PX4,
     .    PY1, PY2, PY3, PY4,
     .    PZ1, PZ2, PZ3, PZ4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
     .    JAC1,JAC2,JAC3,
     .    JAC4,JAC5,JAC6,JAC9)
        ELSE
          CALL SZDERITO3(OFF,VOLN,
     .         PX1, PX2, PX3, PX4,
     .         PY1, PY2, PY3, PY4,
     .         PZ1, PZ2, PZ3, PZ4,
     .         GBUF%JAC_I,NEL)
        END IF
         CALL SDEFOT3(
     .    PX1, PX2, PX3, PX4,
     .    PY1, PY2, PY3, PY4,
     .    PZ1, PZ2, PZ3, PZ4,
     .    VX0(1,1), VX0(1,2), VX0(1,3), VX0(1,4), 
     .    VX0(1,5), VX0(1,6), VX0(1,7), VX0(1,8),
     .    VY0(1,1), VY0(1,2), VY0(1,3), VY0(1,4), 
     .    VY0(1,5), VY0(1,6), VY0(1,7), VY0(1,8),
     .    VZ0(1,1), VZ0(1,2), VZ0(1,3), VZ0(1,4), 
     .    VZ0(1,5), VZ0(1,6), VZ0(1,7), VZ0(1,8),
     .    MFXX, MFXY, MFXZ, MFYX, MFYY, MFYZ, MFZX, MFZY, MFZZ)
c
        IF (ISMSTR==10.OR.ISMSTR==12.OR.ISORTH/=0) THEN
          CALL SZTORTH3(LFT,LLT,ISORTH,NEL,
     .       R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .         G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z, GBUF%GAMA)
          CALL SORDEFT3(LFT,LLT,MFXX, MFXY, MFXZ, MFYX, MFYY, MFYZ,
     .         MFZX, MFZY, MFZZ,
     .         G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z)
        ENDIF
      ENDIF   !  ISMSTR==10.AND.JLAG>0
C-----------
C
      IF(JALE+JLAG /= 0)THEN
        IF(JCLOSE /= 0) CALL MOD_CLOSE(
     1   GEO,     NGEO,    X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      HH,      XD1,
     8   XD2,     XD3,     XD4,     XD5,
     9   XD6,     XD7,     XD8,     YD1,
     A   YD2,     YD3,     YD4,     YD5,
     B   YD6,     YD7,     YD8,     ZD1,
     C   ZD2,     ZD3,     ZD4,     ZD5,
     D   ZD6,     ZD7,     ZD8,     NEL)
C
C------
        IF (ISMSTR==11) THEN
         CALL SCOOR_CP2SP(X0,Y0,Z0,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
        ELSE
         CALL SZDERI3(OFF,VOLN,NGL,ISMSTR,
     .        XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .        YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .        ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .        PX1, PX2, PX3, PX4,
     .        PY1, PY2, PY3, PY4,
     .        PZ1, PZ2, PZ3, PZ4,
     .        PX1H1, PX1H2, PX1H3, PX1H4,
     .        PX2H1, PX2H2, PX2H3, PX2H4,
     .        PX3H1, PX3H2, PX3H3, PX3H4,
     .        PX4H1, PX4H2, PX4H3, PX4H4,
     .        JAC1,JAC2,JAC3,JAC4,JAC5,JAC6,JAC9,
     .        GBUF%SMSTR,GBUF%OFF,NEL,VOLDP)
        END IF
C        
        IF (IDT1SOL == 1) THEN
          CALL SDLEN8(DELTAX,VOLN,IXS(1,NF1),
     .      X1, X2, X3, X4, X5, X6, X7, X8,
     .      Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .      Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
        ELSEIF(IDTMINS==2)THEN
C
C         Possible mix of elts computing DELTAX like in sdlen3 & sdlen8
          CALL SDLEN_SMS(DELTAX,VOLN,IXS(1,NF1),
     .      X1, X2, X3, X4, X5, X6, X7, X8,
     .      Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .      Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .      IPARTS, TAGPRT_SMS, GBUF%ISMS)
        ELSE
          CALL SDLEN3(VOLN,DELTAX,
     .      X1, X2, X3, X4, X5, X6, X7, X8,
     .      Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .      Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .      N1X, N2X, N3X, N4X, N5X, N6X,
     .      N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .      N1Z, N2Z, N3Z, N4Z, N5Z, N6Z)
        END IF   !IF (IDTHEPH == 1) THEN
      ELSEIF (JEUL /= 0) THEN
        CALL EPXLE3(
     1   GBUF%VOL,VEUL,    X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      PX1,     PX2,
     8   PX3,     PX4,     PY1,     PY2,
     9   PY3,     PY4,     PZ1,     PZ2,
     A   PZ3,     PZ4,     PX5,     PX6,
     B   PX7,     PX8,     PY5,     PY6,
     C   PY7,     PY8,     PZ5,     PZ6,
     D   PZ7,     PZ8,     PX1H1,   PX1H2,
     E   PX1H3,   PX2H1,   PX2H2,   PX2H3,
     F   PX3H1,   PX3H2,   PX3H3,   PX4H1,
     G   PX4H2,   PX4H3,   VOLN,    DELTAX,
     H   N1X,     N2X,     N3X,     N4X,
     I   N5X,     N6X,     N1Y,     N2Y,
     J   N3Y,     N4Y,     N5Y,     N6Y,
     K   N1Z,     N2Z,     N3Z,     N4Z,
     L   N5Z,     N6Z,     NEL,     NFT,
     M   JHBE)
      ENDIF
c
      IF (JALE+JEUL > 0 .AND. MTN == 11) THEN
        CALL S11DEFO3(
     1   PM,         V,          VEUL,       X,
     2   IXS,        ALE_CONNECT,DXX,        DXY,
     3   DXZ,        DYX,        DYY,        DYZ,
     4   DZX,        DZY,        DZZ,        D4,
     5   D5,         D6,         WXX,        WYY,
     6   WZZ,        BUFMAT,     NEL,        NFT)
      ELSE
        CALL SDEFO3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .   WXX, WYY, WZZ, BID,BID,BID,IXS)
      ENDIF
C
      IF (JCVT == 2) THEN
       CALL STORTH3(LFT,LLT,NEL,
     .   G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z, GBUF%GAMA)
       CALL SZORDEF3(LFT,LLT,DXX,DYY,DZZ,D4,D5,D6,
     .              G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z)
      ENDIF
      DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))  
      CALL SRHO3(PM         ,GBUF%VOL  , GBUF%RHO , GBUF%EINT, DIVDE,
     .           FLUX(1,NF1),FLU1(NF1) , VOLN     , DVOL     , NGL, MXT , OFF,
     .           IBID       ,GBUF%TAG22, VOLDP    ,LBUF%VOL0DP,AMU  ,GBUF%OFF)
C
C Extract stresses + small strain
       CALL SRMALLA3(GBUF%SIG,S1,S2,S3,S4,S5,S6,
     .             GBUF%OFF,OFF,NEL)
       CALL SRMALLA11(GBUF%SMSTR,GBUF%OFF,WXX,WYY,WZZ,
     .     R11, R12, R13, R21, R22, R23, R31, R32, R33,NEL)
C
C  Update reference configuration (possible future change to small strain option)
C  Total strain option doesn't change the reference configuration
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
       CALL S8SAV3(GBUF%OFF,GBUF%SMSTR,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             NEL)
      END IF !(ISMSTR == 2) THEN
C
C Element temperature
      IF(JTHE < 0) THEN       
        DO I=1,NEL
          TEMPEL(I) = ONE_OVER_8 *(  TEMP(NC1(I)) + TEMP(NC2(I))  
     .                             + TEMP(NC3(I)) + TEMP(NC4(I)) 
     .                             + TEMP(NC5(I)) + TEMP(NC6(I)) 
     .                             + TEMP(NC7(I)) + TEMP(NC8(I))) 
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
      ILAY = 1 
      IPTR = 1 
      IPTS = 1 
      IPTT = 1 
      IP = 1
C
C Compute regularized non local variable in Gauss point
      IF (INLOC > 0) THEN
        L_NLOC = NLOC_DMG%L_NLOC
        DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        DO I=1,NEL
          INOD(1) = NLOC_DMG%IDXI(NC1(I))
          INOD(2) = NLOC_DMG%IDXI(NC2(I))
          INOD(3) = NLOC_DMG%IDXI(NC3(I))
          INOD(4) = NLOC_DMG%IDXI(NC4(I))
          INOD(5) = NLOC_DMG%IDXI(NC5(I))
          INOD(6) = NLOC_DMG%IDXI(NC6(I))
          INOD(7) = NLOC_DMG%IDXI(NC7(I))
          INOD(8) = NLOC_DMG%IDXI(NC8(I))
          DO J = 1, 8
            IPOS(J) = NLOC_DMG%POSI(INOD(J))
          ENDDO
          VAR_REG(I) = DNL(IPOS(1)) + DNL(IPOS(2)) + DNL(IPOS(3)) + DNL(IPOS(4)) 
     .               + DNL(IPOS(5)) + DNL(IPOS(6)) + DNL(IPOS(7)) + DNL(IPOS(8))
          VAR_REG(I) = VAR_REG(I)*ONE_OVER_8
        ENDDO
      ENDIF
C
      IF(IBOLTP /= 0) CALL BOLTST(
     1   IBOLTP,    IP,        BPRELD,    LBUF%SIG,
     2   TT,        NEL,       SENSOR_TAB,NPT)
C
C Compute stresses according to the material law
      CALL MMAIN(ELBUF_TAB,NG     ,PM     ,GEO     ,FV      ,ALE_CONNECT,
     2           IXS      ,IPARG  ,V      ,TF      ,NPF     ,BUFMAT  ,
     3           STI      ,X      ,DT2T   ,NELTST  ,ITYPTST ,OFFSET  ,
     4           NEL      ,W      ,OFF    ,NGEO    ,
     6           MXT      ,NGL    ,VOLN   ,VD2     ,DVOL    ,DELTAX  ,
     7           VIS      ,QVIS   ,CXX    ,S1      ,S2      ,S3      ,
     8           S4       ,S5     ,S6     ,DXX     ,DYY     ,DZZ     ,
     9           D4       ,D5     ,D6     ,WXX     ,WYY     ,WZZ     ,
     A           JAC1     ,JAC2   ,JAC3   ,JAC4    ,JAC5    ,JAC6    ,
     B           VDX      ,VDY    ,VDZ    ,MUVOID  ,SSP_EQ  ,AIRE    ,
     C           SIGY     ,ET     ,R1_FREE,LBUF%PLA,R3_DAM  , AMU    ,
     D           MFXX     ,MFXY   ,MFXZ   ,MFYX    ,MFYY    ,MFYZ    , 
     E           MFZX     ,MFZY   ,MFZZ   ,IPM     ,GAMA    ,BID     ,
     F           DXY      ,DYX    ,DYZ    ,DZY     ,DZX     ,DXZ     ,
     G           ISTRAIN  ,TEMPEL ,DIE    ,IEXPAN  ,ILAY    ,MSSA    ,
     H           DMELS    ,IPTR   ,IPTS   ,IPTT    ,TABLE   ,BID     ,
     I           BID      ,BID    ,BID    ,IPARG(1,NG),IGEO ,CONDE   ,
     J           ITASK    ,NLOC_DMG,VAR_REG,MATPARAM_TAB,H3D_STRAIN)
      IF (ISTRAIN == 1)THEN 
        CALL SSTRA3(DXX      ,DYY      ,DZZ   ,D4   ,D5   ,
     .              D6       ,LBUF%STRA,WXX   ,WYY  ,WZZ  ,
     .              OFF      ,NEL      )
      ENDIF
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C
      IF (JCVT == 2) THEN
       CALL SROTO3(LFT,LLT,GBUF%SIG,SIGN,
     .            G1X, G2X, G3X, G1Y, G2Y, G3Y, G1Z, G2Z, G3Z,NEL)
      ELSE
        DO I=1,NEL
          SIGN(I,1) = GBUF%SIG(II(1)+I)
          SIGN(I,2) = GBUF%SIG(II(2)+I)
          SIGN(I,3) = GBUF%SIG(II(3)+I)
          SIGN(I,4) = GBUF%SIG(II(4)+I)
          SIGN(I,5) = GBUF%SIG(II(5)+I)
          SIGN(I,6) = GBUF%SIG(II(6)+I)
        ENDDO
      ENDIF
C
      ITET = 0
      CALL SGEODEL3(NGL,GBUF%OFF,VOLN,DELTAX,GBUF%VOL,GEO(1,NGEO(1)),ITET,NEL )
C
C Small strain formulation is activated:
C           a) by minimum time step in MMAIN
C           b) by small volume or bad aspect ratio in SMALLGEO3
C If ISMSTR=2 GBUF%SMSTR was updated by S8SAV3 (before MMAIN, each cycle) fixed when off=2
C If ISMSTR=12 fixed GBUF%SMSTR before, updated only once by S8SAV12 
      CALL SMALLB3(GBUF%OFF,OFF)
      CALL SMALLGEO3(NGL, GBUF%OFF ,VOLN ,DELTAX, GBUF%VOL ,ITET )
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        CALL S8SAV12(GBUF%OFF,OFFG0,GBUF%SMSTR,X  ,XDP   ,
     .     NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8   ,NEL)
        IF (ISM12_11>0 .AND. ISORTH == 0) THEN
          CALL S8UPD11T12(GBUF%OFF,OFFG0  ,X  ,XDP  ,
     .     NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8   ,
     .     GBUF%JAC_I,GBUF%SIG,LBUF%SIGL  ,NEL   )
        END IF
      ENDIF
      IF (JLAG+JALE+JEUL == 0) THEN
C
C Balance thermal material
        IFLAG=MOD(NCYCLE,NCPRI)
        IF(IOUTPRT>0)THEN
          IF (MTN == 11) THEN                                    
            EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)                   
          ELSE                                                   
            EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)                   
          ENDIF                                                  
          CALL SRBILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .         VGXA, VGYA, VGZA, VGA2, VOLN,IPARTS,
     .         GRESAV,GRTH,IGRTH,GBUF%OFF,IEXPAN,GBUF%EINTTH,
     .         GBUF%FILL, XGXA, XGYA, XGZA,
     .         XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
        ENDIF
        RETURN
      ENDIF
C
C ALE or EULER : update masses 
      IF (JALE+JEUL > 0 .AND. ALE%GLOBAL%INCOMP == 0) THEN
        IF(IPARIT == 0)THEN
          CALL AMASS3(MS,GBUF%RHO,VEUL(LVEUL*NFT+44),GBUF%TAG22,
     .                VOLN,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     .                MSNF,NVC,OFF,IXS)
        ELSE
          CALL AMASS3P(FSKYM,GBUF%RHO,VEUL(LVEUL*NFT+44),GBUF%TAG22,VOLN,IADS,OFF,IXS)
        ENDIF
      ENDIF
C
C Update ET for /VISC/PRONY
      IF (IMATVIS>2) THEN
       IVISC = ELBUF_TAB(NG)%BUFLY(ILAY)%IVISC
       IF (IVISC>0) THEN
            CALL VISC_ET(
     .           NEL     ,MXT     ,IPM     ,BUFMAT  ,CXX     ,PM      ,
     .           ET      )

       END IF
      END IF
C
C Anti hourglass forces
      IF (ISORTH>0) THEN
       IF (MTN>=28) THEN
        IADBUF = IPM(7,MXT(1))
       ELSE
        IADBUF = 1 ! BUFMAT starts at 1 => BUFMAT(0) is wrong 
       ENDIF
       CALL SZHOUR3_OR(ELBUF_TAB(NG),PM,GBUF%RHO,OFF,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   PX1H1, PX1H2, PX1H3, PX1H4, 
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4, 
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   VOLN,MXT,CXX,VIS,VD2,DELTAX,NGEO,GEO,PARTSAV,IPARTS,
     .   DXX, DYY, DZZ, D4, D5, D6 ,
     .   GBUF%HOURG,JAC1,JAC5,JAC9,GBUF%EINT,GBUF%VOL,SIGY,SIGN,SIGO,ICP,
     .   LBUF%PLA,IMATVIS,ET ,R3_DAM,NEL,GAMA  ,BUFMAT(IADBUF),
     .   GBUF%STRHG,LBUF%STRA,ISTRAIN)
C
      ELSE
      CALL SZHOUR3(ELBUF_TAB(NG),PM,GBUF%RHO,OFF,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   PX1H1, PX1H2, PX1H3, PX1H4, 
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4, 
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   VOLN,MXT,CXX,VIS,VD2,DELTAX,NGEO,GEO,PARTSAV,IPARTS,
     .   DXX, DYY, DZZ, D4, D5, D6 ,
     .   GBUF%HOURG,JAC1,JAC5,JAC9,GBUF%EINT,GBUF%VOL,SIGY,SIGN,SIGO,ICP,
     .   LBUF%PLA,IMATVIS,ET ,R3_DAM,NEL,GBUF%STRHG,ISTRAIN)
      END IF !(ISORTH>0) THEN
      IF (CNS2 > ZERO) 
     .     CALL NSVIS_SM12(GBUF%OFF ,CNS2,CXX  ,VOLN ,DXX     ,
     .                     DYY     ,DZZ    ,D4    ,D5  ,D6   ,
     .                     LBUF%VOL,RHO0_1 ,STI   ,NEL   ) 
C 
      IF(JCLOSE /= 0) CALL FE_CLOSE(
     1   HH,      GBUF%RHO,VOLN,    VX1,
     2   VX2,     VX3,     VX4,     VX5,
     3   VX6,     VX7,     VX8,     VY1,
     4   VY2,     VY3,     VY4,     VY5,
     5   VY6,     VY7,     VY8,     VZ1,
     6   VZ2,     VZ3,     VZ4,     VZ5,
     7   VZ6,     VZ7,     VZ8,     F11,
     8   F21,     F31,     F12,     F22,
     9   F32,     F13,     F23,     F33,
     A   F14,     F24,     F34,     F15,
     B   F25,     F35,     F16,     F26,
     C   F36,     F17,     F27,     F37,
     D   F18,     F28,     F38,     NEL)
C
      IF (JALE == 1 .OR. (JEUL == 1.AND.INTEG8 == 0)) THEN
C
C ALE or EULER : update momentum transport forces
        CALL AMOMT3(
     1   PM,         GBUF%RHO,   VOLN,       X1,
     2   X2,         X3,         X4,         X5,
     3   X6,         X7,         X8,         Y1,
     4   Y2,         Y3,         Y4,         Y5,
     5   Y6,         Y7,         Y8,         Z1,
     6   Z2,         Z3,         Z4,         Z5,
     7   Z6,         Z7,         Z8,         VX1,
     8   VX2,        VX3,        VX4,        VX5,
     9   VX6,        VX7,        VX8,        VY1,
     A   VY2,        VY3,        VY4,        VY5,
     B   VY6,        VY7,        VY8,        VZ1,
     C   VZ2,        VZ3,        VZ4,        VZ5,
     D   VZ6,        VZ7,        VZ8,        F11,
     E   F21,        F31,        F12,        F22,
     F   F32,        F13,        F23,        F33,
     G   F14,        F24,        F34,        F15,
     H   F25,        F35,        F16,        F26,
     I   F36,        F17,        F27,        F37,
     J   F18,        F28,        F38,        PX1,
     K   PX2,        PX3,        PX4,        PY1,
     L   PY2,        PY3,        PY4,        PZ1,
     M   PZ2,        PZ3,        PZ4,        DXX,
     N   DXY,        DXZ,        DYX,        DYY,
     O   DYZ,        DZX,        DZY,        DZZ,
     P   VDX1,       VDX2,       VDX3,       VDX4,
     Q   VDX5,       VDX6,       VDX7,       VDX8,
     R   VDY1,       VDY2,       VDY3,       VDY4,
     S   VDY5,       VDY6,       VDY7,       VDY8,
     T   VDZ1,       VDZ2,       VDZ3,       VDZ4,
     U   VDZ5,       VDZ6,       VDZ7,       VDZ8,
     V   VDX,        VDY,        VDZ,        DELTAX,
     W   VIS,        MXT,        QMV,        BUFMAT,
     X   IPARG(1,NG),IXS,        GBUF%TAG22, NC1,
     Y   NC2,        NC3,        NC4,        NC5,
     Z   NC6,        NC7,        NC8,        NALE,
     1   NEL)
      ELSEIF(JEUL == 1 .AND. INTEG8 == 1)THEN
C
C Does not include upwind SUPG or TG
        CALL EMOMT3B(
     1   PM,                GBUF%RHO,          VEUL(LVEUL*NFT+44),F11,
     2   F21,               F31,               F12,               F22,
     3   F32,               F13,               F23,               F33,
     4   F14,               F24,               F34,               F15,
     5   F25,               F35,               F16,               F26,
     6   F36,               F17,               F27,               F37,
     7   F18,               F28,               F38,               PX1,
     8   PX2,               PX3,               PX4,               PY1,
     9   PY2,               PY3,               PY4,               PZ1,
     A   PZ2,               PZ3,               PZ4,               PX5,
     B   PX6,               PX7,               PX8,               PY5,
     C   PY6,               PY7,               PY8,               PZ5,
     D   PZ6,               PZ7,               PZ8,               DXX,
     E   DXY,               DXZ,               DYX,               DYY,
     F   DYZ,               DZX,               DZY,               DZZ,
     G   VDX,               VDY,               VDZ,               MXT,
     H   QMV,               BUFMAT,            VX1,               VX2,
     I   VX3,               VX4,               VX5,               VX6,
     J   VX7,               VX8,               VY1,               VY2,
     K   VY3,               VY4,               VY5,               VY6,
     L   VY7,               VY8,               VZ1,               VZ2,
     M   VZ3,               VZ4,               VZ5,               VZ6,
     N   VZ7,               VZ8,               IPARG(1,NG),       NEL,
     O   MTN)
      ENDIF

        IF(JEUL+JALE/=0) CALL CHECK_OFF_ALE(F11,F21,F31,F12,F22,
     1                           F32,F13,F23,F33,F14,
     2                           F24,F34,F15,F25,F35,
     3                           F16,F26,F36,F17,F27,
     4                           F37,F18,F28,F38,GBUF%OFF,
     5                           LFT,LLT,NEL)
C
C Energy, momentum, mass balance per part in case of output
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IFLAG == 0.OR.TT >= THIS.OR.MDESS /= 0.                             
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS).                  
     .  OR.TT >= THIS1(1).OR.TT >= THIS1(2).                     
     .  OR.TT >= THIS1(3).OR.TT >= THIS1(4).OR.TT >= THIS1(5).   
     .  OR.TT >= THIS1(6).OR.TT >= THIS1(7).OR.TT >= THIS1(8).   
     .  OR.TT >= THIS1(9).OR.NTH /= 0.OR.NANIM /= 0.             
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(1)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(2)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(3)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(4)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(5)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(6)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(7)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(8)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= THIS1(9)))THEN          
        IF (MTN == 11) THEN                                      
          EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)                     
        ELSE                                                     
          EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)                     
        ENDIF                                                    
        CALL SRBILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,  
     .       VGXA, VGYA, VGZA, VGA2, VOLN,IPARTS,                
     .       GRESAV,GRTH,IGRTH,GBUF%OFF,IEXPAN,GBUF%EINTTH,
     .       GBUF%FILL, XGXA, XGYA, XGZA,
     .       XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))                                  
      ENDIF
C
C Compute nodal internal forces
      CALL SFINT3(SIGN,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   VOLN,QVIS,
     .   N1X, N2X, N3X, N4X, N5X, N6X,
     .   N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .   N1Z, N2Z, N3Z, N4Z, N5Z, N6Z,
     .   BID,   P, IXS, NEL)
C
C Virtual internal forces of regularized non local dof 
      IF (INLOC > 0) THEN  
        IMAT = MXT(LFT)  
        CALL SFINT_REG(
     1   NLOC_DMG,VAR_REG, NEL,     OFF,
     2   VOLN,    NC1,     NC2,     NC3,
     3   NC4,     NC5,     NC6,     NC7,
     4   NC8,     PX1,     PX2,     PX3,
     5   PX4,     PY1,     PY2,     PY3,
     6   PY4,     PZ1,     PZ2,     PZ3,
     7   PZ4,     IMAT,    ITASK,   DT2T,
     8   GBUF%VOL,NFT)
      ENDIF      
C
C Finite element heat transfert
      IF(JTHE < 0) THEN
        CALL STHERM(
     1   PM,      MXT,     VOLN,    NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     PX1,
     4   PX2,     PX3,     PX4,     PY1,
     5   PY2,     PY3,     PY4,     PZ1,
     6   PZ2,     PZ3,     PZ4,     DT1,
     7   TEMP,    TEMPEL,  DIE,     THEM,
     8   GBUF%OFF,LBUF%OFF,PARTSAV, IPARTS,
     9   GBUF%VOL,NEL)
      ENDIF 
C
C Transform forces from convected frame to global frame
      CALL SRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F11, F12, F13, F14, F15, F16, F17, F18,
     .   F21, F22, F23, F24, F25, F26, F27, F28,
     .   F31, F32, F33, F34, F35, F36, F37, F38)
C
      IF(NFILSOL/=0) CALL SFILLOPT(
     1   GBUF%FILL,STI,      F11,      F21,
     2   F31,      F12,      F22,      F32,
     3   F13,      F23,      F33,      F14,
     4   F24,      F34,      F15,      F25,
     5   F35,      F16,      F26,      F36,
     6   F17,      F27,      F37,      F18,
     7   F28,      F38,      NEL)
C
C Assemble internal forces
      IF(IPARIT == 0)THEN
          CALL SCUMU3(GBUF%OFF,A,NC1, NC2, NC3, NC4,
     2             NC5, NC6, NC7, NC8,STIFN,STI,
     3   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     4   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     5   NVC,BID,BID,BID,
     6   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     7   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     8   THEM, FTHE,CONDN,CONDE)
      ELSE
          CALL SCUMU3P(GBUF%OFF,STI,FSKY,FSKY,IADS,
     2   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     3   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     4   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     5   BID,BID,BID,
     6   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     7   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     8   THEM , FTHESKY,CONDNSKY,CONDE)
      ENDIF
      IF (ALLOCATED(VAR_REG)) DEALLOCATE(VAR_REG)
      RETURN
      END SUBROUTINE SZFORC3
C                                                                      12
Chd|====================================================================
Chd|  STORTH3                       source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STORTH3(JFT,JLT,NEL,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,GAMA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER JFT, JLT ,NEL
C     REAL
      my_real
     .   G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),GAMA(NEL,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
      IF (ISORTH == 1) THEN 
      DO I=JFT,JLT
         G1X(I)=GAMA(I,1)
         G1Y(I)=GAMA(I,2)
         G1Z(I)=GAMA(I,3)
         G2X(I)=GAMA(I,4)
         G2Y(I)=GAMA(I,5)
         G2Z(I)=GAMA(I,6)
         G3X(I)=G1Y(I)*G2Z(I)-G1Z(I)*G2Y(I)
         G3Y(I)=G1Z(I)*G2X(I)-G1X(I)*G2Z(I)
         G3Z(I)=G1X(I)*G2Y(I)-G1Y(I)*G2X(I)
       ENDDO
        ELSE
       DO I=JFT,JLT
         G1X(I)=ONE
         G1Y(I)=ZERO
         G1Z(I)=ZERO
         G2X(I)=ZERO
         G2Y(I)=ONE
         G2Z(I)=ZERO
         G3X(I)=ZERO
         G3Y(I)=ZERO
         G3Z(I)=ONE       
       ENDDO
        ENDIF 
C-----------
      RETURN
      END SUBROUTINE STORTH3
C                                                                     12
Chd|====================================================================
Chd|  SZORDEF3                      source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SROTO3V                       source/elements/solid/solidez/szforc3.F
Chd|====================================================================
      SUBROUTINE SZORDEF3(JFT,JLT,DXX,DYY,DZZ,D4,D5,D6,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   DXX(*),DYY(*),DZZ(*),D4(*),D5(*),D6(*), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DEF(MVSIZ,6),DEFN(MVSIZ,6)
C=======================================================================
      DO I=JFT,JLT
        DEF(I,1) = DXX(I)
        DEF(I,2) = DYY(I)
        DEF(I,3) = DZZ(I)
        DEF(I,4) = HALF*D4(I)
        DEF(I,5) = HALF*D5(I)
        DEF(I,6) = HALF*D6(I)
      ENDDO
      CALL SROTO3V(JFT,JLT,DEF,DEFN,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
      DO I=JFT,JLT
        DXX(I) = DEFN(I,1)
        DYY(I) = DEFN(I,2)
        DZZ(I) = DEFN(I,3)
        D4(I) = TWO*DEFN(I,4)
        D5(I) = TWO*DEFN(I,5)
        D6(I) = TWO*DEFN(I,6)
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SZORDEF3
C                                                                     12
Chd|====================================================================
Chd|  SROTO3                        source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTO3(JFT,JLT,SIG,SIGN,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
C     REAL
      my_real
     .   SIG(NEL,6),SIGN(NEL,6), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ
C-----------------------------------------------
      DO I=JFT,JLT
         SX = SIG(I,1)*G1X(I)+SIG(I,4)*G1Y(I)+SIG(I,6)*G1Z(I)
         SY = SIG(I,4)*G1X(I)+SIG(I,2)*G1Y(I)+SIG(I,5)*G1Z(I)
         SZ = SIG(I,6)*G1X(I)+SIG(I,5)*G1Y(I)+SIG(I,3)*G1Z(I)
         SIGN(I,1) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIGN(I,4) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,6) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G2X(I)+SIG(I,4)*G2Y(I)+SIG(I,6)*G2Z(I)
         SY = SIG(I,4)*G2X(I)+SIG(I,2)*G2Y(I)+SIG(I,5)*G2Z(I)
         SZ = SIG(I,6)*G2X(I)+SIG(I,5)*G2Y(I)+SIG(I,3)*G2Z(I)
         SIGN(I,2) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,5) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G3X(I)+SIG(I,4)*G3Y(I)+SIG(I,6)*G3Z(I)
         SY = SIG(I,4)*G3X(I)+SIG(I,2)*G3Y(I)+SIG(I,5)*G3Z(I)
         SZ = SIG(I,6)*G3X(I)+SIG(I,5)*G3Y(I)+SIG(I,3)*G3Z(I)
         SIGN(I,3) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SROTO3
C                                                                     12
Chd|====================================================================
Chd|  SROTO3V                       source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        SZORDEF3                      source/elements/solid/solidez/szforc3.F
Chd|        SZORSTR3                      source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTO3V(JFT,JLT,SIG,SIGN,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
!-----------------------------------------------
!       Description
!       vectorized version of SROTO3 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   SIG(MVSIZ,6),SIGN(MVSIZ,6), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ
C-----------------------------------------------
      DO I=JFT,JLT
         SX = SIG(I,1)*G1X(I)+SIG(I,4)*G1Y(I)+SIG(I,6)*G1Z(I)
         SY = SIG(I,4)*G1X(I)+SIG(I,2)*G1Y(I)+SIG(I,5)*G1Z(I)
         SZ = SIG(I,6)*G1X(I)+SIG(I,5)*G1Y(I)+SIG(I,3)*G1Z(I)
         SIGN(I,1) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIGN(I,4) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,6) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G2X(I)+SIG(I,4)*G2Y(I)+SIG(I,6)*G2Z(I)
         SY = SIG(I,4)*G2X(I)+SIG(I,2)*G2Y(I)+SIG(I,5)*G2Z(I)
         SZ = SIG(I,6)*G2X(I)+SIG(I,5)*G2Y(I)+SIG(I,3)*G2Z(I)
         SIGN(I,2) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,5) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G3X(I)+SIG(I,4)*G3Y(I)+SIG(I,6)*G3Z(I)
         SY = SIG(I,4)*G3X(I)+SIG(I,2)*G3Y(I)+SIG(I,5)*G3Z(I)
         SZ = SIG(I,6)*G3X(I)+SIG(I,5)*G3Y(I)+SIG(I,3)*G3Z(I)
         SIGN(I,3) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SROTO3V
C                                                                     12
Chd|====================================================================
Chd|  SORDEFT3                      source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORDEFT3(JFT,JLT,MXX, MXY, MXZ,  
     .                     MYX, MYY, MYZ,MZX, MZY, MZZ,
     .                     G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   MXX(*), MXY(*), MXZ(*),MYX(*), MYY(*), MYZ(*),  
     .   MZX(*), MZY(*), MZZ(*),G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ,FXX,FXY,FXZ,FYX,FYY,FYZ,FZX,FZY,FZZ
C-----------------------------------------------
      DO I=JFT,JLT
         SX = MXX(I)*G1X(I)+MYX(I)*G1Y(I)+MZX(I)*G1Z(I)
         SY = MXY(I)*G1X(I)+MYY(I)*G1Y(I)+MZY(I)*G1Z(I)
         SZ = MXZ(I)*G1X(I)+MYZ(I)*G1Y(I)+MZZ(I)*G1Z(I)
         FXX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FXY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FXZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G2X(I)+MYX(I)*G2Y(I)+MZX(I)*G2Z(I)
         SY = MXY(I)*G2X(I)+MYY(I)*G2Y(I)+MZY(I)*G2Z(I)
         SZ = MXZ(I)*G2X(I)+MYZ(I)*G2Y(I)+MZZ(I)*G2Z(I)
         FYX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FYY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FYZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G3X(I)+MYX(I)*G3Y(I)+MZX(I)*G3Z(I)
         SY = MXY(I)*G3X(I)+MYY(I)*G3Y(I)+MZY(I)*G3Z(I)
         SZ = MXZ(I)*G3X(I)+MYZ(I)*G3Y(I)+MZZ(I)*G3Z(I)
         FZX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FZY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FZZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         MXX(I)=FXX
         MXY(I)=FXY
         MXZ(I)=FXZ
         MYX(I)=FYX
         MYY(I)=FYY
         MYZ(I)=FYZ
         MZX(I)=FZX
         MZY(I)=FZY
         MZZ(I)=FZZ
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SORDEFT3
Chd|====================================================================
Chd|  SDLEN8                        source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        BASISF                        source/elements/solid/solide8/basisf.F
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen3.F
Chd|====================================================================
      SUBROUTINE SDLEN8(DELTAX,VOLG,IXS,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IXS(NIXS,*)
C     REAL
      my_real
     .   DELTAX(*),VOLG(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, IPT, IDEGE(MVSIZ)
C     REAL
      my_real
     .   AJ11, AJ12, AJ13, AJ21,
     .   AJ22, AJ23, AJ31, AJ32,
     .   AJ33, AI11, AI12, AI13,
     .   AI21, AI22, AI23, AI31,
     .   AI32, AI33
C     REAL
      my_real
     .   X12(MVSIZ), X34(MVSIZ), X56(MVSIZ),
     .   X78(MVSIZ), Y12(MVSIZ), Y34(MVSIZ), Y56(MVSIZ), Y78(MVSIZ),
     .   Z12(MVSIZ), Z34(MVSIZ), Z56(MVSIZ), Z78(MVSIZ), X14(MVSIZ),
     .   X23(MVSIZ), X58(MVSIZ), X67(MVSIZ), Y14(MVSIZ), Y23(MVSIZ),
     .   Y58(MVSIZ), Y67(MVSIZ), Z14(MVSIZ), Z23(MVSIZ), Z58(MVSIZ),
     .   Z67(MVSIZ), X15(MVSIZ), X26(MVSIZ), X37(MVSIZ), X48(MVSIZ),
     .   Y15(MVSIZ), Y26(MVSIZ), Y37(MVSIZ), Y48(MVSIZ), Z15(MVSIZ),
     .   Z26(MVSIZ), Z37(MVSIZ), Z48(MVSIZ), H(8), VLINV,
     .   DELTA1,DELTA2,DELTA3,
     .   SPX1,SPX2,SPX3,SPX4,SPX5,SPX6,SPX7,SPX8,
     .   SPY1,SPY2,SPY3,SPY4,SPY5,SPY6,SPY7,SPY8,
     .   SPZ1,SPZ2,SPZ3,SPZ4,SPZ5,SPZ6,SPZ7,SPZ8,
     .   XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,SMAX(MVSIZ),VMIN,
     .   P1(8), P2(8), P3(8),VLINC(MVSIZ,8)
C-----------------------------------------------
C      
      IF (IDTS6>0) THEN
       CALL SDLEN_DEGE(VOLG,DELTAX,
     .  X1, X2, X3, X4, X5, X6, X7, X8,
     .  Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .  Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .  IXS,IDEGE)
      ELSE
       IDEGE(LFT:LLT)=0
      END IF
C
      DO I=LFT,LLT
       X12(I)=X1(I)-X2(I)
       Y12(I)=Y1(I)-Y2(I)
       Z12(I)=Z1(I)-Z2(I)
       X34(I)=X3(I)-X4(I)
       Y34(I)=Y3(I)-Y4(I)
       Z34(I)=Z3(I)-Z4(I)
       X56(I)=X5(I)-X6(I)
       Y56(I)=Y5(I)-Y6(I)
       Z56(I)=Z5(I)-Z6(I)
       X78(I)=X7(I)-X8(I)
       Y78(I)=Y7(I)-Y8(I)
       Z78(I)=Z7(I)-Z8(I)
       X14(I)=X1(I)-X4(I)
       Y14(I)=Y1(I)-Y4(I)
       Z14(I)=Z1(I)-Z4(I)
       X23(I)=X2(I)-X3(I)
       Y23(I)=Y2(I)-Y3(I)
       Z23(I)=Z2(I)-Z3(I)
       X58(I)=X5(I)-X8(I)
       Y58(I)=Y5(I)-Y8(I)
       Z58(I)=Z5(I)-Z8(I)
       X67(I)=X6(I)-X7(I)
       Y67(I)=Y6(I)-Y7(I)
       Z67(I)=Z6(I)-Z7(I)
       X15(I)=X1(I)-X5(I)
       Y15(I)=Y1(I)-Y5(I)
       Z15(I)=Z1(I)-Z5(I)
       X26(I)=X2(I)-X6(I)
       Y26(I)=Y2(I)-Y6(I)
       Z26(I)=Z2(I)-Z6(I)
       X37(I)=X3(I)-X7(I)
       Y37(I)=Y3(I)-Y7(I)
       Z37(I)=Z3(I)-Z7(I)
       X48(I)=X4(I)-X8(I)
       Y48(I)=Y4(I)-Y8(I)
       Z48(I)=Z4(I)-Z8(I)
      END DO 
C      
      DO IPT=1,8
       CALL BASISF (H,P1,P2,P3,IPT)
C
       DO I=LFT,LLT
          AJ11=P1(1)*X12(I)+P1(3)*X34(I)+P1(5)*X56(I)+P1(7)*X78(I)
          AJ12=P1(1)*Y12(I)+P1(3)*Y34(I)+P1(5)*Y56(I)+P1(7)*Y78(I)
          AJ13=P1(1)*Z12(I)+P1(3)*Z34(I)+P1(5)*Z56(I)+P1(7)*Z78(I)
          AJ21=P2(1)*X14(I)+P2(2)*X23(I)+P2(5)*X58(I)+P2(6)*X67(I)
          AJ22=P2(1)*Y14(I)+P2(2)*Y23(I)+P2(5)*Y58(I)+P2(6)*Y67(I)
          AJ23=P2(1)*Z14(I)+P2(2)*Z23(I)+P2(5)*Z58(I)+P2(6)*Z67(I)
          AJ31=P3(1)*X15(I)+P3(2)*X26(I)+P3(3)*X37(I)+P3(4)*X48(I)
          AJ32=P3(1)*Y15(I)+P3(2)*Y26(I)+P3(3)*Y37(I)+P3(4)*Y48(I)
          AJ33=P3(1)*Z15(I)+P3(2)*Z26(I)+P3(3)*Z37(I)+P3(4)*Z48(I)

          AI11= AJ22*AJ33-AJ23*AJ32
          AI21=-AJ21*AJ33+AJ23*AJ31
          AI31= AJ21*AJ32-AJ22*AJ31
          VLINC(I,IPT)=AJ11*AI11+AJ12*AI21+AJ13*AI31
       ENDDO
      END DO ! IPT=1,8
C      
      DO I=LFT,LLT
c mediane * 4
        XX1 = X1(I) + X2(I) + X3(I) + X4(I)
     .      - X5(I) - X6(I) - X7(I) - X8(I)
        YY1 = Y1(I) + Y2(I) + Y3(I) + Y4(I)
     .      - Y5(I) - Y6(I) - Y7(I) - Y8(I)
        ZZ1 = Z1(I) + Z2(I) + Z3(I) + Z4(I)
     .      - Z5(I) - Z6(I) - Z7(I) - Z8(I)
        XX2 = X1(I) + X2(I) + X5(I) + X6(I)
     .      - X3(I) - X4(I) - X7(I) - X8(I)
        YY2 = Y1(I) + Y2(I) + Y5(I) + Y6(I)
     .      - Y3(I) - Y4(I) - Y7(I) - Y8(I)
        ZZ2 = Z1(I) + Z2(I) + Z5(I) + Z6(I)
     .      - Z3(I) - Z4(I) - Z7(I) - Z8(I)
        XX3 = X1(I) + X4(I) + X5(I) + X8(I)
     .      - X3(I) - X2(I) - X7(I) - X6(I)
        YY3 = Y1(I) + Y4(I) + Y5(I) + Y8(I)
     .      - Y3(I) - Y2(I) - Y7(I) - Y6(I)
        ZZ3 = Z1(I) + Z4(I) + Z5(I) + Z8(I)
     .      - Z3(I) - Z2(I) - Z7(I) - Z6(I)
C surface * 16
        SMAX(I) =          (YY1 * ZZ2 - YY2 * ZZ1)**2
     .                + (ZZ1 * XX2 - ZZ2 * XX1)**2
     .                + (XX1 * YY2 - XX2 * YY1)**2
        SMAX(I) = MAX(SMAX(I),(YY1 * ZZ3 - YY3 * ZZ1)**2
     .                + (ZZ1 * XX3 - ZZ3 * XX1)**2
     .                + (XX1 * YY3 - XX3 * YY1)**2)
        SMAX(I) = MAX(SMAX(I),(YY3 * ZZ2 - YY2 * ZZ3)**2
     .                + (ZZ3 * XX2 - ZZ2 * XX3)**2
     .                + (XX3 * YY2 - XX2 * YY3)**2)
      ENDDO
C volume / 8
      IF (IDTS6>0) THEN
                DO I=LFT,LLT
                        IF(IDEGE(I)==0)THEN
                                VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .                                     VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
C
                                DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
                        ENDIF
                ENDDO
      ELSE
                DO I=LFT,LLT
                        VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .                             VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
C
                        DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
                ENDDO
      ENDIF
                        
        
C-----------
      RETURN
      END SUBROUTINE SDLEN8
Chd|====================================================================
Chd|  SDLEN_SMS                     source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        BASISF                        source/elements/solid/solide8/basisf.F
Chd|        DEGENES8                      source/elements/solid/solide/sdlen3.F
Chd|        DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|        IDEGE8                        source/elements/solid/solide/sdlen3.F
Chd|====================================================================
      SUBROUTINE SDLEN_SMS(DELTAX,VOLG,IXS,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   IPARTS,TAGPRT_SMS,TAGELEM_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IXS(NIXS,*), IPARTS(*), TAGPRT_SMS(*), TAGELEM_SMS(*)
C     REAL
      my_real
     .   DELTAX(*),VOLG(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, IT, IPT, IDEGE(MVSIZ), NINDX, NJNDX, 
     .         INDEX(MVSIZ), JNDEX(MVSIZ)
C     REAL
      my_real
     .   AJ11, AJ12, AJ13, AJ21,
     .   AJ22, AJ23, AJ31, AJ32,
     .   AJ33, AI11, AI12, AI13,
     .   AI21, AI22, AI23, AI31,
     .   AI32, AI33
C     REAL
      my_real
     .   X12(MVSIZ), X34(MVSIZ), X56(MVSIZ),
     .   X78(MVSIZ), Y12(MVSIZ), Y34(MVSIZ), Y56(MVSIZ), Y78(MVSIZ),
     .   Z12(MVSIZ), Z34(MVSIZ), Z56(MVSIZ), Z78(MVSIZ), X14(MVSIZ),
     .   X23(MVSIZ), X58(MVSIZ), X67(MVSIZ), Y14(MVSIZ), Y23(MVSIZ),
     .   Y58(MVSIZ), Y67(MVSIZ), Z14(MVSIZ), Z23(MVSIZ), Z58(MVSIZ),
     .   Z67(MVSIZ), X15(MVSIZ), X26(MVSIZ), X37(MVSIZ), X48(MVSIZ),
     .   Y15(MVSIZ), Y26(MVSIZ), Y37(MVSIZ), Y48(MVSIZ), Z15(MVSIZ),
     .   Z26(MVSIZ), Z37(MVSIZ), Z48(MVSIZ), H(8), VLINV,
     .   XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3,SMAX(MVSIZ),VMIN,
     .   P1(8), P2(8), P3(8),VLINC(MVSIZ,8)
      my_real
     .   AREAM(MVSIZ), FAC, V_G
C-----------------------------------------------
C  
      NINDX=0    
      NJNDX=0
C
      IF (ISMS_SELEC==1) THEN
C-- Full AMS
        DO I=LFT,LLT
          NJNDX=NJNDX+1
          JNDEX(NJNDX)=I
        END DO
      ELSEIF (ISMS_SELEC==2) THEN
C-- AMS by parts    
        DO I=LFT,LLT
          IF(TAGPRT_SMS(IPARTS(I))==0)THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ELSE
            NJNDX=NJNDX+1
            JNDEX(NJNDX)=I
          END IF
        END DO
      ELSEIF (ISMS_SELEC==3) THEN
C-- AMS auto - defined by elements
        DO I=LFT,LLT
          IF(TAGELEM_SMS(I)==0)THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ELSE
            NJNDX=NJNDX+1
            JNDEX(NJNDX)=I
          END IF
        END DO
      ELSEIF (ISMS_SELEC==4) THEN
C-- AMS auto + parts
        DO I=LFT,LLT
          IF ((TAGELEM_SMS(I)==0).AND.(TAGPRT_SMS(IPARTS(I))==0)) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ELSE
            NJNDX=NJNDX+1
            JNDEX(NJNDX)=I
          END IF
        END DO
      ENDIF
C      
      IF(IDTS6 > 0)THEN
        CALL DEGENES8(IXS,IDEGE)
      ELSE
        IDEGE(LFT:LLT)=0
      END IF
C-----------------------------------------------------------
C     Non AMS elements ~ sdlen.F
C-----------------------------------------------------------
      DO J=1,NINDX
       I=INDEX(J)
C
       AREAM(I) =EM20
C
       CALL IDEGE8(X1(I),X2(I),X3(I),X4(I),Y1(I),Y2(I),Y3(I),Y4(I),
     .            Z1(I),Z2(I),Z3(I),Z4(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X5(I),X6(I),X7(I),X8(I),Y5(I),Y6(I),Y7(I),Y8(I),
     .            Z5(I),Z6(I),Z7(I),Z8(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X1(I),X2(I),X6(I),X5(I),Y1(I),Y2(I),Y6(I),Y5(I),
     .            Z1(I),Z2(I),Z6(I),Z5(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X2(I),X3(I),X7(I),X6(I),Y2(I),Y3(I),Y7(I),Y6(I),
     .            Z2(I),Z3(I),Z7(I),Z6(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X3(I),X4(I),X8(I),X7(I),Y3(I),Y4(I),Y8(I),Y7(I),
     .            Z3(I),Z4(I),Z8(I),Z7(I),AREAM(I),FAC,IT)
       CALL IDEGE8(X4(I),X1(I),X5(I),X8(I),Y4(I),Y1(I),Y5(I),Y8(I),
     .            Z4(I),Z1(I),Z5(I),Z8(I),AREAM(I),FAC,IT)
C----tetra 4  ,pyrami    
       IF(IDEGE(I) > 0)THEN
C----tetra 4  ,pyrami     
         IF (IDEGE(I) > 2) THEN
           FAC=ONE_OVER_9
         ELSEIF (IDEGE(I) > 1) THEN
           FAC=FOURTH
         ELSE
           FAC=ONE
         END IF
C--------suposse here V=0.5*A_max*L for penta	=0.333A_max*L for Pyram 
         IF (IT ==0  ) AREAM(I)=FAC*AREAM(I) 
C--------add special treat for tetra4, as V is not right values		
         IF (IDEGE(I) > 3 ) THEN
	   AREAM(I)=AREAM(I)*FAC
           CALL DEGES4V(V_G,
     .    X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .    Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .    Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
	 ELSE
	   V_G=VOLG(I)
	 END IF
       ENDIF
C------
       DELTAX(I)=FOUR*VOLG(I)/SQRT(AREAM(I))
      END DO
C-----------------------------------------------------------
C     AMS elements ~ sdlen8
C-----------------------------------------------------------
      DO J=1,NJNDX
       I=JNDEX(J)
       IF(IDEGE(I)/=0) THEN
C------- due to the fact that AREA_Max*L is far from V for Dege---
         AREAM(I) =EM20
C----tetra 4  ,pyrami     
         IF (IDEGE(I) > 2) THEN
           FAC=ONE_OVER_9
         ELSEIF (IDEGE(I) > 1) THEN
           FAC=FOURTH
         ELSE
           FAC=ONE
         END IF
	 IT = 0
         CALL IDEGE8(X1(I),X2(I),X3(I),X4(I),Y1(I),Y2(I),Y3(I),Y4(I),
     .              Z1(I),Z2(I),Z3(I),Z4(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X5(I),X6(I),X7(I),X8(I),Y5(I),Y6(I),Y7(I),Y8(I),
     .              Z5(I),Z6(I),Z7(I),Z8(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X1(I),X2(I),X6(I),X5(I),Y1(I),Y2(I),Y6(I),Y5(I),
     .              Z1(I),Z2(I),Z6(I),Z5(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X2(I),X3(I),X7(I),X6(I),Y2(I),Y3(I),Y7(I),Y6(I),
     .              Z2(I),Z3(I),Z7(I),Z6(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X3(I),X4(I),X8(I),X7(I),Y3(I),Y4(I),Y8(I),Y7(I),
     .              Z3(I),Z4(I),Z8(I),Z7(I),AREAM(I),FAC,IT)
         CALL IDEGE8(X4(I),X1(I),X5(I),X8(I),Y4(I),Y1(I),Y5(I),Y8(I),
     .              Z4(I),Z1(I),Z5(I),Z8(I),AREAM(I),FAC,IT)
C----tetra 4       
         IF (IT ==0  ) AREAM(I)=FAC*AREAM(I)
         IF (IDEGE(I) > 3 ) THEN
	   AREAM(I)=AREAM(I)*FAC
           CALL DEGES4V(V_G,
     .    X1(I), X2(I), X3(I), X4(I), X5(I), X6(I), X7(I), X8(I),
     .    Y1(I), Y2(I), Y3(I), Y4(I), Y5(I), Y6(I), Y7(I), Y8(I),
     .    Z1(I), Z2(I), Z3(I), Z4(I), Z5(I), Z6(I), Z7(I), Z8(I))
	 ELSE
	   V_G=VOLG(I)
	 END IF
         DELTAX(I) = FOUR*V_G/SQRT(AREAM(I))
       END IF
      END DO

      DO I=LFT,LLT
         X12(I)=X1(I)-X2(I)
         Y12(I)=Y1(I)-Y2(I)
         Z12(I)=Z1(I)-Z2(I)
         X34(I)=X3(I)-X4(I)
         Y34(I)=Y3(I)-Y4(I)
         Z34(I)=Z3(I)-Z4(I)
         X56(I)=X5(I)-X6(I)
         Y56(I)=Y5(I)-Y6(I)
         Z56(I)=Z5(I)-Z6(I)
         X78(I)=X7(I)-X8(I)
         Y78(I)=Y7(I)-Y8(I)
         Z78(I)=Z7(I)-Z8(I)
         X14(I)=X1(I)-X4(I)
         Y14(I)=Y1(I)-Y4(I)
         Z14(I)=Z1(I)-Z4(I)
         X23(I)=X2(I)-X3(I)
         Y23(I)=Y2(I)-Y3(I)
         Z23(I)=Z2(I)-Z3(I)
         X58(I)=X5(I)-X8(I)
         Y58(I)=Y5(I)-Y8(I)
         Z58(I)=Z5(I)-Z8(I)
         X67(I)=X6(I)-X7(I)
         Y67(I)=Y6(I)-Y7(I)
         Z67(I)=Z6(I)-Z7(I)
         X15(I)=X1(I)-X5(I)
         Y15(I)=Y1(I)-Y5(I)
         Z15(I)=Z1(I)-Z5(I)
         X26(I)=X2(I)-X6(I)
         Y26(I)=Y2(I)-Y6(I)
         Z26(I)=Z2(I)-Z6(I)
         X37(I)=X3(I)-X7(I)
         Y37(I)=Y3(I)-Y7(I)
         Z37(I)=Z3(I)-Z7(I)
         X48(I)=X4(I)-X8(I)
         Y48(I)=Y4(I)-Y8(I)
         Z48(I)=Z4(I)-Z8(I)
       ENDDO
C      
      DO IPT=1,8
       CALL BASISF (H,P1,P2,P3,IPT)
C
       DO I=LFT,LLT
          AJ11=P1(1)*X12(I)+P1(3)*X34(I)+P1(5)*X56(I)+P1(7)*X78(I)
          AJ12=P1(1)*Y12(I)+P1(3)*Y34(I)+P1(5)*Y56(I)+P1(7)*Y78(I)
          AJ13=P1(1)*Z12(I)+P1(3)*Z34(I)+P1(5)*Z56(I)+P1(7)*Z78(I)
          AJ21=P2(1)*X14(I)+P2(2)*X23(I)+P2(5)*X58(I)+P2(6)*X67(I)
          AJ22=P2(1)*Y14(I)+P2(2)*Y23(I)+P2(5)*Y58(I)+P2(6)*Y67(I)
          AJ23=P2(1)*Z14(I)+P2(2)*Z23(I)+P2(5)*Z58(I)+P2(6)*Z67(I)
          AJ31=P3(1)*X15(I)+P3(2)*X26(I)+P3(3)*X37(I)+P3(4)*X48(I)
          AJ32=P3(1)*Y15(I)+P3(2)*Y26(I)+P3(3)*Y37(I)+P3(4)*Y48(I)
          AJ33=P3(1)*Z15(I)+P3(2)*Z26(I)+P3(3)*Z37(I)+P3(4)*Z48(I)
          AI11= AJ22*AJ33-AJ23*AJ32
          AI21=-AJ21*AJ33+AJ23*AJ31
          AI31= AJ21*AJ32-AJ22*AJ31
          VLINC(I,IPT)=AJ11*AI11+AJ12*AI21+AJ13*AI31
       ENDDO
      END DO ! IPT=1,8
C      
      DO I=LFT,LLT
c mediane * 4
        XX1 = X1(I) + X2(I) + X3(I) + X4(I)
     .      - X5(I) - X6(I) - X7(I) - X8(I)
        YY1 = Y1(I) + Y2(I) + Y3(I) + Y4(I)
     .      - Y5(I) - Y6(I) - Y7(I) - Y8(I)
        ZZ1 = Z1(I) + Z2(I) + Z3(I) + Z4(I)
     .      - Z5(I) - Z6(I) - Z7(I) - Z8(I)
        XX2 = X1(I) + X2(I) + X5(I) + X6(I)
     .      - X3(I) - X4(I) - X7(I) - X8(I)
        YY2 = Y1(I) + Y2(I) + Y5(I) + Y6(I)
     .      - Y3(I) - Y4(I) - Y7(I) - Y8(I)
        ZZ2 = Z1(I) + Z2(I) + Z5(I) + Z6(I)
     .      - Z3(I) - Z4(I) - Z7(I) - Z8(I)
        XX3 = X1(I) + X4(I) + X5(I) + X8(I)
     .      - X3(I) - X2(I) - X7(I) - X6(I)
        YY3 = Y1(I) + Y4(I) + Y5(I) + Y8(I)
     .      - Y3(I) - Y2(I) - Y7(I) - Y6(I)
        ZZ3 = Z1(I) + Z4(I) + Z5(I) + Z8(I)
     .      - Z3(I) - Z2(I) - Z7(I) - Z6(I)
C surface * 16
                SMAX(I) =       (YY1 * ZZ2 - YY2 * ZZ1)**2
     .                + (ZZ1 * XX2 - ZZ2 * XX1)**2
     .                + (XX1 * YY2 - XX2 * YY1)**2
                SMAX(I) =    MAX(SMAX(I),(YY1 * ZZ3 - YY3 * ZZ1)**2
     .                + (ZZ1 * XX3 - ZZ3 * XX1)**2
     .                + (XX1 * YY3 - XX3 * YY1)**2)
                SMAX(I) =    MAX(SMAX(I),(YY3 * ZZ2 - YY2 * ZZ3)**2
     .                + (ZZ3 * XX2 - ZZ2 * XX3)**2
     .                + (XX3 * YY2 - XX2 * YY3)**2)
        ENDDO

C volume / 8
      IF (IDTS6>0) THEN
                DO I=LFT,LLT
                        IF(IDEGE(I)==0)THEN
                                VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .                                     VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
                                DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
                        ENDIF
                ENDDO
      ELSE
                DO I=LFT,LLT
        VMIN = MIN(VLINC(I,1),VLINC(I,2),VLINC(I,3),VLINC(I,4),
     .             VLINC(I,5),VLINC(I,6),VLINC(I,7),VLINC(I,8))
                        DELTAX(I)=HUNDRED28*VMIN/SQRT(SMAX(I))
      ENDDO
      ENDIF
C-----------
      RETURN
      END SUBROUTINE SDLEN_SMS
Chd|====================================================================
Chd|  SCOOR_CP2SP                   source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCOOR_CP2SP(X0,Y0,Z0,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
      DOUBLE PRECISION 
     .   X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, IPT
C-----------------------------------------------
        DO I=LFT,LLT
        X1(I)= X0(I,1) 
        Y1(I)= Y0(I,1) 
        Z1(I)= Z0(I,1)
        X2(I)= X0(I,2) 
        Y2(I)= Y0(I,2) 
        Z2(I)= Z0(I,2)
        X3(I)= X0(I,3) 
        Y3(I)= Y0(I,3) 
        Z3(I)= Z0(I,3)
        X4(I)= X0(I,4) 
        Y4(I)= Y0(I,4) 
        Z4(I)= Z0(I,4) 
        X5(I)= X0(I,5) 
        Y5(I)= Y0(I,5)
        Z5(I)= Z0(I,5) 
        X6(I)= X0(I,6) 
        Y6(I)= Y0(I,6) 
        Z6(I)= Z0(I,6) 
        X7(I)= X0(I,7) 
        Y7(I)= Y0(I,7) 
        Z7(I)= Z0(I,7) 
        X8(I)= X0(I,8) 
        Y8(I)= Y0(I,8) 
        Z8(I)= Z0(I,8) 
        ENDDO    
C-----------
      RETURN
      END SUBROUTINE SCOOR_CP2SP
Chd|====================================================================
Chd|  SZTORTH3                      source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZTORTH3(JFT,JLT,ISORTH,NEL,
     .                   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z ,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,GAMA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER JFT, JLT,ISORTH,NEL
C     REAL
      my_real
     .   E1X(*),E1Y(*),E1Z(*),
     .   E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*),
     .   G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),GAMA(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33
C=======================================================================
      IF (ISORTH > 0) THEN 
       DO I=JFT,JLT
         R11=GAMA(I,1)
         R12=GAMA(I,2)
         R13=GAMA(I,3)
         R21=GAMA(I,4)
         R22=GAMA(I,5)
         R23=GAMA(I,6)
         R31=R12*R23-R13*R22
         R32=R13*R21-R11*R23
         R33=R11*R22-R12*R21
         G1X(I)=R11*E1X(I)+R12*E2X(I)+R13*E3X(I)
         G1Y(I)=R11*E1Y(I)+R12*E2Y(I)+R13*E3Y(I)
         G1Z(I)=R11*E1Z(I)+R12*E2Z(I)+R13*E3Z(I)
         G2X(I)=R21*E1X(I)+R22*E2X(I)+R23*E3X(I)
         G2Y(I)=R21*E1Y(I)+R22*E2Y(I)+R23*E3Y(I)
         G2Z(I)=R21*E1Z(I)+R22*E2Z(I)+R23*E3Z(I)
         G3X(I)=R31*E1X(I)+R32*E2X(I)+R33*E3X(I)
         G3Y(I)=R31*E1Y(I)+R32*E2Y(I)+R33*E3Y(I)
         G3Z(I)=R31*E1Z(I)+R32*E2Z(I)+R33*E3Z(I)    
       ENDDO
      ELSE
       DO I=JFT,JLT
         G1X(I)=E1X(I)
         G1Y(I)=E1Y(I)
         G1Z(I)=E1Z(I)
         G2X(I)=E2X(I)
         G2Y(I)=E2Y(I)
         G2Z(I)=E2Z(I)
         G3X(I)=E3X(I)
         G3Y(I)=E3Y(I)
         G3Z(I)=E3Z(I)    
       ENDDO
      ENDIF 
C-----------
      RETURN
      END SUBROUTINE SZTORTH3
C                                                                     12
Chd|====================================================================
Chd|  SORDEFT12                     source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORDEFT12(JFT,JLT,MXX, MXY, MXZ,  
     .                     MYX, MYY, MYZ,MZX, MZY, MZZ,
     .                     G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   MXX(*), MXY(*), MXZ(*),MYX(*), MYY(*), MYZ(*),  
     .   MZX(*), MZY(*), MZZ(*),G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ,FXX,FXY,FXZ,FYX,FYY,FYZ,FZX,FZY,FZZ
C-----------------------------------------------
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
         SX = MXX(I)*G1X(I)+MYX(I)*G1Y(I)+MZX(I)*G1Z(I)
         SY = MXY(I)*G1X(I)+MYY(I)*G1Y(I)+MZY(I)*G1Z(I)
         SZ = MXZ(I)*G1X(I)+MYZ(I)*G1Y(I)+MZZ(I)*G1Z(I)
         FXX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FXY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FXZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G2X(I)+MYX(I)*G2Y(I)+MZX(I)*G2Z(I)
         SY = MXY(I)*G2X(I)+MYY(I)*G2Y(I)+MZY(I)*G2Z(I)
         SZ = MXZ(I)*G2X(I)+MYZ(I)*G2Y(I)+MZZ(I)*G2Z(I)
         FYX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FYY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FYZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G3X(I)+MYX(I)*G3Y(I)+MZX(I)*G3Z(I)
         SY = MXY(I)*G3X(I)+MYY(I)*G3Y(I)+MZY(I)*G3Z(I)
         SZ = MXZ(I)*G3X(I)+MYZ(I)*G3Y(I)+MZZ(I)*G3Z(I)
         FZX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FZY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FZZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         MXX(I)=FXX
         MXY(I)=FXY
         MXZ(I)=FXZ
         MYX(I)=FYX
         MYY(I)=FYY
         MYZ(I)=FYZ
         MZX(I)=FZX
         MZY(I)=FZY
         MZZ(I)=FZZ
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SORDEFT12
Chd|====================================================================
Chd|  SROTO12                       source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        SORDEF12                      source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTO12(JFT,JLT,SIG,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   SIG(MVSIZ,6), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ,SIGN(MVSIZ,6)
C-----------------------------------------------
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
         SX = SIG(I,1)*G1X(I)+SIG(I,4)*G1Y(I)+SIG(I,6)*G1Z(I)
         SY = SIG(I,4)*G1X(I)+SIG(I,2)*G1Y(I)+SIG(I,5)*G1Z(I)
         SZ = SIG(I,6)*G1X(I)+SIG(I,5)*G1Y(I)+SIG(I,3)*G1Z(I)
         SIGN(I,1) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIGN(I,4) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,6) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G2X(I)+SIG(I,4)*G2Y(I)+SIG(I,6)*G2Z(I)
         SY = SIG(I,4)*G2X(I)+SIG(I,2)*G2Y(I)+SIG(I,5)*G2Z(I)
         SZ = SIG(I,6)*G2X(I)+SIG(I,5)*G2Y(I)+SIG(I,3)*G2Z(I)
         SIGN(I,2) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,5) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G3X(I)+SIG(I,4)*G3Y(I)+SIG(I,6)*G3Z(I)
         SY = SIG(I,4)*G3X(I)+SIG(I,2)*G3Y(I)+SIG(I,5)*G3Z(I)
         SZ = SIG(I,6)*G3X(I)+SIG(I,5)*G3Y(I)+SIG(I,3)*G3Z(I)
         SIGN(I,3) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SIG(I,1) = SIGN(I,1) 
         SIG(I,2) = SIGN(I,2) 
         SIG(I,3) = SIGN(I,3) 	 
         SIG(I,4) = SIGN(I,4) 
         SIG(I,5) = SIGN(I,5) 
         SIG(I,6) = SIGN(I,6) 	 
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SROTO12
Chd|====================================================================
Chd|  SORDEF12                      source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SROTO12                       source/elements/solid/solidez/szforc3.F
Chd|====================================================================
      SUBROUTINE SORDEF12(JFT,JLT,DXX,DYY,DZZ,D4,D5,D6,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   DXX(*),DYY(*),DZZ(*),D4(*),D5(*),D6(*), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DEF(MVSIZ,6)
C=======================================================================
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
        DEF(I,1) = DXX(I)
        DEF(I,2) = DYY(I)
        DEF(I,3) = DZZ(I)
        DEF(I,4) = HALF*D4(I)
        DEF(I,5) = HALF*D5(I)
        DEF(I,6) = HALF*D6(I)
      ENDDO
      CALL SROTO12(JFT,JLT,DEF,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
        DXX(I) = DEF(I,1)
        DYY(I) = DEF(I,2)
        DZZ(I) = DEF(I,3)
        D4(I) = TWO*DEF(I,4)
        D5(I) = TWO*DEF(I,5)
        D6(I) = TWO*DEF(I,6)
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SORDEF12
Chd|====================================================================
Chd|  SROTO12_SIG                   source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTO12_SIG(JFT,JLT,SIG,NEL,
     .                       G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
C     REAL
      my_real
     .   SIG(NEL,6), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ,SIGN(NEL,6)
C-----------------------------------------------
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
         SX = SIG(I,1)*G1X(I)+SIG(I,4)*G1Y(I)+SIG(I,6)*G1Z(I)
         SY = SIG(I,4)*G1X(I)+SIG(I,2)*G1Y(I)+SIG(I,5)*G1Z(I)
         SZ = SIG(I,6)*G1X(I)+SIG(I,5)*G1Y(I)+SIG(I,3)*G1Z(I)
         SIGN(I,1) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIGN(I,4) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,6) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G2X(I)+SIG(I,4)*G2Y(I)+SIG(I,6)*G2Z(I)
         SY = SIG(I,4)*G2X(I)+SIG(I,2)*G2Y(I)+SIG(I,5)*G2Z(I)
         SZ = SIG(I,6)*G2X(I)+SIG(I,5)*G2Y(I)+SIG(I,3)*G2Z(I)
         SIGN(I,2) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,5) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G3X(I)+SIG(I,4)*G3Y(I)+SIG(I,6)*G3Z(I)
         SY = SIG(I,4)*G3X(I)+SIG(I,2)*G3Y(I)+SIG(I,5)*G3Z(I)
         SZ = SIG(I,6)*G3X(I)+SIG(I,5)*G3Y(I)+SIG(I,3)*G3Z(I)
         SIGN(I,3) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SIG(I,1) = SIGN(I,1) 
         SIG(I,2) = SIGN(I,2) 
         SIG(I,3) = SIGN(I,3)	 
         SIG(I,4) = SIGN(I,4) 
         SIG(I,5) = SIGN(I,5) 
         SIG(I,6) = SIGN(I,6)	 
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SZORSTR3                      source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SROTO3V                       source/elements/solid/solidez/szforc3.F
Chd|====================================================================
      SUBROUTINE SZORSTR3(JFT,JLT,STRAIN,NEL,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
C     REAL
      my_real
     .   STRAIN(NEL,6), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DEF(MVSIZ,6),DEFN(MVSIZ,6)
C=======================================================================
      DO I=JFT,JLT
        DEF(I,1) = STRAIN(I,1)
        DEF(I,2) = STRAIN(I,2)
        DEF(I,3) = STRAIN(I,3)
        DEF(I,4) = HALF*STRAIN(I,4)
        DEF(I,5) = HALF*STRAIN(I,5)
        DEF(I,6) = HALF*STRAIN(I,6)
      ENDDO
      CALL SROTO3V(JFT,JLT,DEF,DEFN,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
      DO I=JFT,JLT
        STRAIN(I,1) = DEFN(I,1)
        STRAIN(I,2) = DEFN(I,2)
        STRAIN(I,3) = DEFN(I,3)
        STRAIN(I,4) = TWO*DEFN(I,4)
        STRAIN(I,5) = TWO*DEFN(I,5)
        STRAIN(I,6) = TWO*DEFN(I,6)
      ENDDO
C-----------
      RETURN
      END SUBROUTINE SZORSTR3
Chd|====================================================================
Chd|  VISC_ET                       source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VISC_ET(
     .           NEL     ,MXT     ,IPM     ,UPARAM  ,CXX     ,PM      ,
     .           ET      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MXT(*)
      INTEGER IPM(NPROPMI,*)
      my_real
     .   UPARAM(*),CXX(*),ET(*),PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J ,IADBUF, NPRONY,IMAT
      my_real
     .   GG,GV,RHO0,NU,GV_MAX,NU1
C=======================================================================        
      IMAT = MXT(1)
      IADBUF = IPM(223,IMAT)  
      NPRONY  = NINT(UPARAM(IADBUF + 1))
C
      GV  = ZERO
      DO J=1,NPRONY                                  
       GV   = GV  +  UPARAM(IADBUF + 1 + J)            
      ENDDO
C
      RHO0=PM(1,IMAT)
      NU=PM(21,IMAT)
      GV_MAX = THIRD*(ONE+NU)/(ONE -TWO*NU)
      NU1= RHO0*(ONE -TWO*NU)/(ONE-NU)
C
      DO I=1,NEL
       GG = CXX(I)*CXX(I)*NU1    
       ET(I)= ET(I) + MIN(GV_MAX,GV/GG)
      ENDDO
C------------
      RETURN
      END
C                                                                     12
