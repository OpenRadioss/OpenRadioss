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
Chd|        AMASS3P                       source/elements/solid/solide/amass3p.F
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
Chd|        S8SAV12                       source/elements/solid/solide/s8sav12.F
Chd|        S8SAV3                        source/elements/solid/solide/s8sav3.F
Chd|        S8UPD11T12                    source/elements/solid/solide/s8upd11t12.F
Chd|        SCOOR_CP2SP                   source/elements/solid/solidez/scoor_cp2sp.F
Chd|        SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3p.F
Chd|        SDEFO3                        source/elements/solid/solide/sdefo3.F
Chd|        SDEFOT3                       source/elements/solid/solide/sdefot3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SDLEN8                        source/elements/solid/solidez/sdlen8.F
Chd|        SDLENMAX                      source/elements/solid/solide/sdlenmax.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/sdlen_sms.F
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SFINT3                        source/elements/solid/solide/sfint3.F
Chd|        SFINT_REG                     source/elements/solid/solide/sfint_reg.F
Chd|        SGCOOR3                       source/elements/solid/solide/sgcoor3.F
Chd|        SGEODEL3                      source/elements/solid/solide/sgeodel3.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SMALLGEO3                     source/elements/solid/solide/smallgeo3.F
Chd|        SORDEFT3                      source/elements/solid/solidez/sordeft3.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SRMALLA11                     source/elements/solid/solide/srmalla11.F
Chd|        SRMALLA3                      source/elements/solid/solide/srmall3.F
Chd|        SROTO3                        source/elements/solid/solidez/sroto3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        SRROTADP                      source/elements/solid/solide/srrotadp.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STHERM                        source/elements/solid/solide/stherm.F
Chd|        STOPTIME                      source/system/timer.F         
Chd|        STORTH3                       source/elements/solid/solidez/storth3.F
Chd|        SZDERI3                       source/elements/solid/solidez/szderi3.F
Chd|        SZDERIT3                      source/elements/solid/solidez/szderi3.F
Chd|        SZDERITO3                     source/elements/solid/solidez/szderi3.F
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3_or.F
Chd|        SZORDEF3                      source/elements/solid/solidez/szordef3.F
Chd|        SZTORTH3                      source/elements/solid/solidez/sztorth3.F
Chd|        VISC_ET                       source/elements/solid/solidez/visc_et.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
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
     H                   D        ,TAGPRT_SMS,SENSORS,IOUTPRT  ,
     I                   NALE     ,NLOC_DMG  ,MAT_ELEM,H3D_STRAIN,
     J                   DT       ,OUTPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE SENSOR_MOD
      USE ALE_MOD
      USE DT_MOD
      USE OUTPUT_MOD
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
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (SENSORS_) , INTENT(IN) :: SENSORS
      TYPE(DT_),     INTENT(INOUT) :: DT
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
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
     .  XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ),CNS2,RHO0_1,NU,L_MAX(MVSIZ)
      my_real, 
     .  DIMENSION(:), POINTER :: EINT
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----
C----- 
C     IBOLTP : flag bolt preloading
      INTEGER IBOLTP,NBPRELD,IMAT,ISM12_11,NN_DEL,PID
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
      TEMPEL(1:MVSIZ) = ZERO
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
      NN_DEL = 0
      PID  = NGEO(1)
      IMAT = MXT(1)  

      IF (GEO(190,PID)+GEO(191,PID)+GEO(192,PID)+GEO(192,PID)>ZERO)
     .        NN_DEL=8
      IF (NN_DEL ==0 .AND. DT%IDEL_BRICK>0) NN_DEL=8
      CNS2 = ZERO
      IF (ICP==1.AND.MTN==1.AND.ISMSTR==12) THEN
         RHO0_1 =PM( 1,IMAT)
         CNS2 = ZEP02
         IF (IGEO(35,NGEO(1))>0) CNS2=CNS2-ABS(GEO(17,NGEO(1)))
      END IF
C Total strain Pij are in global system for ISMSTR=10,12 and in local system for ISMSTR=11
      IF ((ISMSTR >= 10.AND.ISMSTR <= 12).AND.JLAG > 0) THEN
        CALL SGCOOR3(
     1   TT,        8,         X,         IXS(1,NF1),
     2   X0,        Y0,        Z0,        VX0,
     3   VY0,       VZ0,       GBUF%SMSTR,D,
     4   GBUF%OFF,  OFFG0,     NEL,       XDP,
     5   MTN,       ISMSTR)
        IF (ISMSTR == 11) THEN
        CALL SRROTADP(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     X0(1,1), X0(1,2), X0(1,3),
     4   X0(1,4), X0(1,5), X0(1,6), X0(1,7),
     5   X0(1,8), Y0(1,1), Y0(1,2), Y0(1,3),
     6   Y0(1,4), Y0(1,5), Y0(1,6), Y0(1,7),
     7   Y0(1,8), Z0(1,1), Z0(1,2), Z0(1,3),
     8   Z0(1,4), Z0(1,5), Z0(1,6), Z0(1,7),
     9   Z0(1,8), NEL)
        CALL SRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX0(1,1),VX0(1,2),VX0(1,3),
     4   VX0(1,4),VX0(1,5),VX0(1,6),VX0(1,7),
     5   VX0(1,8),VY0(1,1),VY0(1,2),VY0(1,3),
     6   VY0(1,4),VY0(1,5),VY0(1,6),VY0(1,7),
     7   VY0(1,8),VZ0(1,1),VZ0(1,2),VZ0(1,3),
     8   VZ0(1,4),VZ0(1,5),VZ0(1,6),VZ0(1,7),
     9   VZ0(1,8),NEL)
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
     .    JAC4,JAC5,JAC6,JAC9,JLAG)
        ELSE
          CALL SZDERITO3(
     1   OFF,       VOLN,      PX1,       PX2,
     2   PX3,       PX4,       PY1,       PY2,
     3   PY3,       PY4,       PZ1,       PZ2,
     4   PZ3,       PZ4,       GBUF%JAC_I,NEL,
     5   JLAG)
        END IF
         CALL SDEFOT3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   VX0(1,1),VX0(1,2),VX0(1,3),VX0(1,4),
     5   VX0(1,5),VX0(1,6),VX0(1,7),VX0(1,8),
     6   VY0(1,1),VY0(1,2),VY0(1,3),VY0(1,4),
     7   VY0(1,5),VY0(1,6),VY0(1,7),VY0(1,8),
     8   VZ0(1,1),VZ0(1,2),VZ0(1,3),VZ0(1,4),
     9   VZ0(1,5),VZ0(1,6),VZ0(1,7),VZ0(1,8),
     A   MFXX,    MFXY,    MFXZ,    MFYX,
     B   MFYY,    MFYZ,    MFZX,    MFZY,
     C   MFZZ,    NEL)
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
         CALL SCOOR_CP2SP(
     1   X0,      Y0,      Z0,      X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      NEL)
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
     .        GBUF%SMSTR,GBUF%OFF,NEL,VOLDP,JLAG)
        END IF
C        
        IF (IDT1SOL == 1) THEN
          CALL SDLEN8(
     1   DELTAX,    VOLN,      IXS(1,NF1),X1,
     2   X2,        X3,        X4,        X5,
     3   X6,        X7,        X8,        Y1,
     4   Y2,        Y3,        Y4,        Y5,
     5   Y6,        Y7,        Y8,        Z1,
     6   Z2,        Z3,        Z4,        Z5,
     7   Z6,        Z7,        Z8,        NEL)
        ELSEIF(IDTMINS==2)THEN
C
C         Possible mix of elts computing DELTAX like in sdlen3 & sdlen8
          CALL SDLEN_SMS(
     1   DELTAX,    VOLN,      IXS(1,NF1),X1,
     2   X2,        X3,        X4,        X5,
     3   X6,        X7,        X8,        Y1,
     4   Y2,        Y3,        Y4,        Y5,
     5   Y6,        Y7,        Y8,        Z1,
     6   Z2,        Z3,        Z4,        Z5,
     7   Z6,        Z7,        Z8,        IPARTS,
     8   TAGPRT_SMS,GBUF%ISMS, NEL)
        ELSE
          CALL SDLEN3(
     1   VOLN,    DELTAX,  X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      N1X,     N2X,
     8   N3X,     N4X,     N5X,     N6X,
     9   N1Y,     N2Y,     N3Y,     N4Y,
     A   N5Y,     N6Y,     N1Z,     N2Z,
     B   N3Z,     N4Z,     N5Z,     N6Z,
     C   NEL,     MTN,     JALE,    JEUL)
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
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   PX5,     PX6,     PX7,     PX8,
     5   PY5,     PY6,     PY7,     PY8,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   VX1,     VX2,     VX3,     VX4,
     8   VX5,     VX6,     VX7,     VX8,
     9   VY1,     VY2,     VY3,     VY4,
     A   VY5,     VY6,     VY7,     VY8,
     B   VZ1,     VZ2,     VZ3,     VZ4,
     C   VZ5,     VZ6,     VZ7,     VZ8,
     D   DXX,     DXY,     DXZ,     DYX,
     E   DYY,     DYZ,     DZX,     DZY,
     F   DZZ,     D4,      D5,      D6,
     G   WXX,     WYY,     WZZ,     BID,
     H   BID,     BID,     IXS,     NEL,
     I   NFT,     ISMSTR,  JEUL,    JHBE,
     J   JCVT,    ISROT)
      ENDIF
C
      IF (JCVT == 2) THEN
       CALL STORTH3(
     1   LFT,      LLT,      NEL,      G1X,
     2   G1Y,      G1Z,      G2X,      G2Y,
     3   G2Z,      G3X,      G3Y,      G3Z,
     4   GBUF%GAMA,ISORTH)
       CALL SZORDEF3(LFT,LLT,DXX,DYY,DZZ,D4,D5,D6,
     .              G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z)
      ENDIF
      DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))  
      CALL SRHO3(
     1   PM,         GBUF%VOL,   GBUF%RHO,   GBUF%EINT,
     2   DIVDE,      FLUX(1,NF1),FLU1(NF1),  VOLN,
     3   DVOL,       NGL,        MXT,        OFF,
     4   IBID,       GBUF%TAG22, VOLDP,      LBUF%VOL0DP,
     5   AMU,        GBUF%OFF,   NEL,        MTN,
     6   JALE,       ISMSTR,     JEUL,       JLAG)
C
C Extract stresses + small strain
       CALL SRMALLA3(GBUF%SIG,S1,S2,S3,S4,S5,S6,
     .             GBUF%OFF,OFF,NEL)
       CALL SRMALLA11(
     1   GBUF%SMSTR,GBUF%OFF,  WXX,       WYY,
     2   WZZ,       R11,       R12,       R13,
     3   R21,       R22,       R23,       R31,
     4   R32,       R33,       NEL,       ISMSTR)
C
C  Update reference configuration (possible future change to small strain option)
C  Total strain option doesn't change the reference configuration
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
       CALL S8SAV3(
     1   GBUF%OFF,  GBUF%SMSTR,XD1,       XD2,
     2   XD3,       XD4,       XD5,       XD6,
     3   XD7,       XD8,       YD1,       YD2,
     4   YD3,       YD4,       YD5,       YD6,
     5   YD7,       YD8,       ZD1,       ZD2,
     6   ZD3,       ZD4,       ZD5,       ZD6,
     7   ZD7,       ZD8,       NEL)
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
      IF(IBOLTP /= 0) CALL BOLTST(IP    ,BPRELD    ,LBUF%SIG  ,TT     ,NEL   ,
     .                            NPT   ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB)
C
C Compute stresses according to the material law
      CALL MMAIN(
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2   FV,          ALE_CONNECT, IXS,         IPARG,
     3   V,           TF,          NPF,         BUFMAT,
     4   STI,         X,           DT2T,        NELTST,
     5   ITYPTST,     OFFSET,      NEL,         W,
     6   OFF,         NGEO,        MXT,         NGL,
     7   VOLN,        VD2,         DVOL,        DELTAX,
     8   VIS,         QVIS,        CXX,         S1,
     9   S2,          S3,          S4,          S5,
     A   S6,          DXX,         DYY,         DZZ,
     B   D4,          D5,          D6,          WXX,
     C   WYY,         WZZ,         JAC1,        JAC2,
     D   JAC3,        JAC4,        JAC5,        JAC6,
     E   VDX,         VDY,         VDZ,         MUVOID,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   R1_FREE,     LBUF%PLA,    R3_DAM,      AMU,
     H   MFXX,        MFXY,        MFXZ,        MFYX,
     I   MFYY,        MFYZ,        MFZX,        MFZY,
     J   MFZZ,        IPM,         GAMA,        BID,
     K   DXY,         DYX,         DYZ,         DZY,
     L   DZX,         DXZ,         ISTRAIN,     TEMPEL,
     M   DIE,         IEXPAN,      ILAY,        MSSA,
     N   DMELS,       IPTR,        IPTS,        IPTT,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         IPARG(1,NG), IGEO,        CONDE,
     Q   ITASK,       NLOC_DMG,    VAR_REG,     MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH)
      IF (ISTRAIN == 1)THEN 
        CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
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
      IF (NN_DEL >0) THEN
        CALL SDLENMAX(L_MAX, 
     1   X1,      X2,      X3,      X4,      
     2   X5,      X6,      X7,      X8,      
     3   Y1,      Y2,      Y3,      Y4,      
     4   Y5,      Y6,      Y7,      Y8,      
     5   Z1,      Z2,      Z3,      Z4,      
     6   Z5,      Z6,      Z7,      Z8,      
     7   NEL)
       CALL SGEODEL3(NGL,GBUF%OFF,VOLN,DELTAX,GBUF%VOL,GEO(1,NGEO(1)),L_MAX,DT,NEL,IDEL7NOK )
      END IF
C
C Small strain formulation is activated:
C           a) by minimum time step in MMAIN
C           b) by small volume or bad aspect ratio in SMALLGEO3
C If ISMSTR=2 GBUF%SMSTR was updated by S8SAV3 (before MMAIN, each cycle) fixed when off=2
C If ISMSTR=12 fixed GBUF%SMSTR before, updated only once by S8SAV12 
      CALL SMALLB3(GBUF%OFF,OFF,NEL,ISMSTR)
      CALL SMALLGEO3(NGL, GBUF%OFF ,VOLN ,DELTAX, GBUF%VOL ,ITET,NEL,ISMSTR,DT)
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        CALL S8SAV12(
     1   GBUF%OFF,  OFFG0,     GBUF%SMSTR,X,
     2   XDP,       NC1,       NC2,       NC3,
     3   NC4,       NC5,       NC6,       NC7,
     4   NC8,       NEL,       JCVT)
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
          CALL AMASS3(
     1   MS,                GBUF%RHO,          VEUL(LVEUL*NFT+44),GBUF%TAG22,
     2   VOLN,              NC1,               NC2,               NC3,
     3   NC4,               NC5,               NC6,               NC7,
     4   NC8,               MSNF,              NVC,               OFF,
     5   IXS,               NEL,               JEUL)
        ELSE
          CALL AMASS3P(
     1   FSKYM,             GBUF%RHO,          VEUL(LVEUL*NFT+44),GBUF%TAG22,
     2   VOLN,              IADS,              OFF,               IXS,
     3   NEL,               NFT,               JEUL)
        ENDIF
      ENDIF
C
C Update ET for /VISC/PRONY
      IF (IMATVIS > 2) THEN
        IVISC = ELBUF_TAB(NG)%BUFLY(ILAY)%IVISC
        IF (IVISC == 1) THEN    ! /VISC/PRONY only
          RHO0_1 = PM(1 ,IMAT)
          NU     = PM(21,IMAT)
          CALL VISC_ET(NEL     ,MAT_ELEM%MAT_PARAM(IMAT)%VISC,
     .                 RHO0_1  ,NU     ,CXX     ,ET      )

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
       CALL SZHOUR3_OR(
     1   ELBUF_TAB(NG), PM,            GBUF%RHO,      OFF,
     2   VX1,           VX2,           VX3,           VX4,
     3   VX5,           VX6,           VX7,           VX8,
     4   VY1,           VY2,           VY3,           VY4,
     5   VY5,           VY6,           VY7,           VY8,
     6   VZ1,           VZ2,           VZ3,           VZ4,
     7   VZ5,           VZ6,           VZ7,           VZ8,
     8   F11,           F21,           F31,           F12,
     9   F22,           F32,           F13,           F23,
     A   F33,           F14,           F24,           F34,
     B   F15,           F25,           F35,           F16,
     C   F26,           F36,           F17,           F27,
     D   F37,           F18,           F28,           F38,
     E   PX1H1,         PX1H2,         PX1H3,         PX1H4,
     F   PX2H1,         PX2H2,         PX2H3,         PX2H4,
     G   PX3H1,         PX3H2,         PX3H3,         PX3H4,
     H   PX4H1,         PX4H2,         PX4H3,         PX4H4,
     I   VOLN,          MXT,           CXX,           VIS,
     J   VD2,           DELTAX,        NGEO,          GEO,
     K   PARTSAV,       IPARTS,        DXX,           DYY,
     L   DZZ,           D4,            D5,            D6,
     M   GBUF%HOURG,    JAC1,          JAC5,          JAC9,
     N   GBUF%EINT,     GBUF%VOL,      SIGY,          SIGN,
     O   SIGO,          ICP,           LBUF%PLA,      IMATVIS,
     P   ET,            R3_DAM,        NEL,           GAMA,
     Q   BUFMAT(IADBUF),GBUF%STRHG,    LBUF%STRA,     ISTRAIN,
     R   MTN,           ISMSTR,        JLAG,          IINT)
C
      ELSE
      CALL SZHOUR3(
     1   ELBUF_TAB(NG),PM,           GBUF%RHO,     OFF,
     2   VX1,          VX2,          VX3,          VX4,
     3   VX5,          VX6,          VX7,          VX8,
     4   VY1,          VY2,          VY3,          VY4,
     5   VY5,          VY6,          VY7,          VY8,
     6   VZ1,          VZ2,          VZ3,          VZ4,
     7   VZ5,          VZ6,          VZ7,          VZ8,
     8   F11,          F21,          F31,          F12,
     9   F22,          F32,          F13,          F23,
     A   F33,          F14,          F24,          F34,
     B   F15,          F25,          F35,          F16,
     C   F26,          F36,          F17,          F27,
     D   F37,          F18,          F28,          F38,
     E   PX1H1,        PX1H2,        PX1H3,        PX1H4,
     F   PX2H1,        PX2H2,        PX2H3,        PX2H4,
     G   PX3H1,        PX3H2,        PX3H3,        PX3H4,
     H   PX4H1,        PX4H2,        PX4H3,        PX4H4,
     I   VOLN,         MXT,          CXX,          VIS,
     J   VD2,          DELTAX,       NGEO,         GEO,
     K   PARTSAV,      IPARTS,       DXX,          DYY,
     L   DZZ,          D4,           D5,           D6,
     M   GBUF%HOURG,   JAC1,         JAC5,         JAC9,
     N   GBUF%EINT,    GBUF%VOL,     SIGY,         SIGN,
     O   SIGO,         ICP,          LBUF%PLA,     IMATVIS,
     P   ET,           R3_DAM,       NEL,          GBUF%STRHG,
     Q   ISTRAIN,      MTN,          ISMSTR,       JLAG,
     R   IINT)
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
     1   NEL,        NFT,        MTN)
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
      IF(IFLAG == 0.OR.TT >= OUTPUT%TH%THIS.OR.MDESS /= 0.                             
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS).                  
     .  OR.TT >= OUTPUT%TH%THIS1(1).OR.TT >= OUTPUT%TH%THIS1(2).                     
     .  OR.TT >= OUTPUT%TH%THIS1(3).OR.TT >= OUTPUT%TH%THIS1(4).OR.TT >= OUTPUT%TH%THIS1(5).   
     .  OR.TT >= OUTPUT%TH%THIS1(6).OR.TT >= OUTPUT%TH%THIS1(7).OR.TT >= OUTPUT%TH%THIS1(8).   
     .  OR.TT >= OUTPUT%TH%THIS1(9).OR.NTH /= 0.OR.NANIM /= 0.             
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(1)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(2)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(3)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(4)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(5)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(6)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(7)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(8)).              
     .  OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S >= OUTPUT%TH%THIS1(9)))THEN          
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
      CALL SFINT3(
     1   SIGN,    PX1,     PX2,     PX3,
     2   PX4,     PY1,     PY2,     PY3,
     3   PY4,     PZ1,     PZ2,     PZ3,
     4   PZ4,     PX5,     PX6,     PX7,
     5   PX8,     PY5,     PY6,     PY7,
     6   PY8,     PZ5,     PZ6,     PZ7,
     7   PZ8,     F11,     F21,     F31,
     8   F12,     F22,     F32,     F13,
     9   F23,     F33,     F14,     F24,
     A   F34,     F15,     F25,     F35,
     B   F16,     F26,     F36,     F17,
     C   F27,     F37,     F18,     F28,
     D   F38,     VOLN,    QVIS,    N1X,
     E   N2X,     N3X,     N4X,     N5X,
     F   N6X,     N1Y,     N2Y,     N3Y,
     G   N4Y,     N5Y,     N6Y,     N1Z,
     H   N2Z,     N3Z,     N4Z,     N5Z,
     I   N6Z,     BID,     P,       IXS,
     J   NEL,     NFT,     JALE,    JEUL)
C
C Virtual internal forces of regularized non local dof 
      IF (INLOC > 0) THEN  
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
     1   R11,     R21,     R31,     R12,
     2   R22,     R32,     R13,     R23,
     3   R33,     F11,     F12,     F13,
     4   F14,     F15,     F16,     F17,
     5   F18,     F21,     F22,     F23,
     6   F24,     F25,     F26,     F27,
     7   F28,     F31,     F32,     F33,
     8   F34,     F35,     F36,     F37,
     9   F38,     NEL)
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
          CALL SCUMU3(
     1   GBUF%OFF,A,       NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   NC7,     NC8,     STIFN,   STI,
     4   F11,     F21,     F31,     F12,
     5   F22,     F32,     F13,     F23,
     6   F33,     F14,     F24,     F34,
     7   F15,     F25,     F35,     F16,
     8   F26,     F36,     F17,     F27,
     9   F37,     F18,     F28,     F38,
     A   NVC,     BID,     BID,     BID,
     B   BID,     BID,     BID,     BID,
     C   BID,     BID,     BID,     BID,
     D   BID,     BID,     BID,     BID,
     E   BID,     BID,     BID,     BID,
     F   BID,     BID,     BID,     BID,
     G   BID,     BID,     BID,     BID,
     H   THEM,    FTHE,    CONDN,   CONDE,
     I   NEL,     JTHE,    ISROT,   IPARTSPH)
      ELSE
          CALL SCUMU3P(
     1   GBUF%OFF,STI,     FSKY,    FSKY,
     2   IADS,    F11,     F21,     F31,
     3   F12,     F22,     F32,     F13,
     4   F23,     F33,     F14,     F24,
     5   F34,     F15,     F25,     F35,
     6   F16,     F26,     F36,     F17,
     7   F27,     F37,     F18,     F28,
     8   F38,     NC1,     NC2,     NC3,
     9   NC4,     NC5,     NC6,     NC7,
     A   NC8,     BID,     BID,     BID,
     B   BID,     BID,     BID,     BID,
     C   BID,     BID,     BID,     BID,
     D   BID,     BID,     BID,     BID,
     E   BID,     BID,     BID,     BID,
     F   BID,     BID,     BID,     BID,
     G   BID,     BID,     BID,     BID,
     H   THEM,    FTHESKY, CONDNSKY,CONDE,
     I   NEL,     NFT,     JTHE,    ISROT,
     J   IPARTSPH)
      ENDIF
      IF (ALLOCATED(VAR_REG)) DEALLOCATE(VAR_REG)
      RETURN
      END
