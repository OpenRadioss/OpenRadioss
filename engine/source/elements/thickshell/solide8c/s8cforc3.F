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
Chd|  S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        BOLTST                        source/elements/solid/solide/boltst.F
Chd|        CSMALL3                       source/elements/solid/solide/csmall3.F
Chd|        DEGENES8                      source/elements/solid/solide/sdlen3.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S8CSIGP3                      source/elements/thickshell/solide8c/s8csigp3.F
Chd|        S8ETHERM                      source/elements/solid/solide8e/s8etherm.F
Chd|        S8SAV3                        source/elements/solid/solide/srcoor3.F
Chd|        S8ZDEFO3                      source/elements/solid/solide8z/s8zdefo3.F
Chd|        S8ZDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZFINT3                      source/elements/solid/solide8z/s8zfint3.F
Chd|        S8ZFINTP3                     source/elements/solid/solide8z/s8zfint3.F
Chd|        S8ZTEMPEL                     source/elements/solid/solide8z/s8zforc3.F
Chd|        S8ZZERO3                      source/elements/solid/solide8z/s8zfint3.F
Chd|        SBILAN                        source/elements/solid/solide/sbilan.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SCTORTH3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3.F
Chd|        SCUMUALPHA                    source/elements/thickshell/solidec/scdtdc3.F
Chd|        SDLENSH                       source/elements/thickshell/solidec/scdtdc3.F
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen3.F
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SGPARAV3                      source/elements/solid/solide/sgcoor3.F
Chd|        SMALLA3                       source/elements/solid/solide/small3.F
Chd|        SMALLB3                       source/elements/solid/solide/small3.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SROTO3                        source/elements/solid/solidez/szforc3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        STORTH3                       source/elements/solid/solidez/szforc3.F
Chd|        SZORDEF3                      source/elements/solid/solidez/szforc3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S8CFORC3(ELBUF_TAB,NG     ,
     1                    PM       ,GEO    ,IXS    ,X      ,
     2                    A        ,V      ,MS     ,W      ,FLUX    ,
     3                    FLU1     ,VEUL   ,FV     ,ALE_CONNECT  ,IPARG   ,
     4                    TF       ,NPF    ,BUFMAT ,PARTSAV,NLOC_DMG,
     5                    DT2T     ,NELTST ,ITYPTST,STIFN  ,FSKY    ,
     6                    IADS     ,OFFSET ,EANI   ,IPARTS  ,
     7                    F11      ,F21    ,F31    ,F12    ,F22     ,
     8                    F32      ,F13    ,F23    ,F33    ,F14     ,
     9                    F24      ,F34    ,F15    ,F25    ,F35     ,
     A                    F16      ,F26    ,F36    ,F17    ,F27     ,
     B                    F37      ,F18    ,F28    ,F38    ,NEL     ,
     C                    ICP      ,
     F                    ICSIG    ,SMR    ,SMS    ,SMT    ,MFXX    ,
     G                    MFXY     ,MFXZ   ,MFYX   ,MFYY   ,MFYZ    ,
     H                    MFZX     ,MFZY   ,MFZZ   ,NVC    ,IPM     ,
     I                    ITASK  ,ISTRAIN,TEMP   ,FTHE    ,
     J                    FTHESKY  ,IEXPAN ,IGEO   ,NNPT   ,GRESAV  ,
     K                    GRTH     ,IGRTH  ,MSSA   ,DMELS  ,TABLE   ,
     L                    XDP      ,VOLN   ,CONDN  ,CONDNSKY,SENSOR_TAB,
     M                    IOUTPRT,MATPARAM_TAB,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE SENSOR_MOD
      USE MATPARAM_DEF_MOD
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
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG,NGROUP),NPF(*),
     .        IPARTS(*), IPM(NPROPMI,*),IGEO(NPROPGI,*),GRTH(*),
     .        IGRTH(*),IADS(8,*),IOUTPRT,H3D_STRAIN
C
      INTEGER NELTST,ITYPTST,OFFSET,NEL,ICP, 
     .        ICSIG, NVC,ITASK, ISTRAIN, IEXPAN ,NNPT,NG
     
      DOUBLE PRECISION 
     .        XDP(3,*)     
     
      my_real
     .   DT2T
C
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),X(*),A(*),V(*),MS(*),W(*), 
     .   FLUX(6,*),FLU1(*),VEUL(*),FV(*),TF(*),BUFMAT(*),
     .   PARTSAV(*),STIFN(*), FSKY(*),EANI(*),
     .   F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),
     .   F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .   F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),
     .   F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),
     .   F15(MVSIZ),F25(MVSIZ),F35(MVSIZ),
     .   F16(MVSIZ),F26(MVSIZ),F36(MVSIZ),
     .   F17(MVSIZ),F27(MVSIZ),F37(MVSIZ),
     .   F18(MVSIZ),F28(MVSIZ),F38(MVSIZ)
      my_real
     .    SMR(MVSIZ,729),SMS(MVSIZ,729),SMT(MVSIZ,729),
     .    MFXX(MVSIZ,729),MFXY(MVSIZ,729),MFYX(MVSIZ,729),
     .    MFYY(MVSIZ,729),MFYZ(MVSIZ,729),MFZY(MVSIZ,729),
     .    MFZZ(MVSIZ,729),MFZX(MVSIZ,729),MFXZ(MVSIZ,729),
     .    TEMP(*), FTHE(*), FTHESKY(*),GRESAV(*), MSSA(*), 
     .    DMELS(*), VOLN(MVSIZ),CONDN(*),CONDNSKY(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,N,LCO,NF1,IFLAG,IAD0,NLAY,L_PLA,L_EPSD,
     .   IP,IR,IS,IT,IL,NPTR,NPTS,NPTT,ICR,ICS,ICT,PID,MTN0,IPTHK,
     .   IPPOS,IPMAT,NLYMAX,MID,IPANG,IOR_TSH,NPTR0,NPTS0,NPTT0,IBID,MX,
     .   II(6)
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),MXT0(MVSIZ),IBIDON(1),
     .  IDEG(MVSIZ)
     
      DOUBLE PRECISION 
     .   X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),VOLDP(MVSIZ)
          
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . AJC1(MVSIZ) , AJC2(MVSIZ) , AJC3(MVSIZ) ,
     . AJC4(MVSIZ) , AJC5(MVSIZ) , AJC6(MVSIZ) ,
     . AJC7(MVSIZ) , AJC8(MVSIZ) , AJC9(MVSIZ) ,
     . AJ1(MVSIZ) , AJ2(MVSIZ) , AJ3(MVSIZ) ,
     . AJ4(MVSIZ) , AJ5(MVSIZ) , AJ6(MVSIZ) ,
     . VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . E0(MVSIZ),C1,FAC(MVSIZ),THEM(MVSIZ,8),TEMPEL(MVSIZ),NI(8),
     . DIE(MVSIZ),CONDE(MVSIZ),CONDEN(MVSIZ),AMU(MVSIZ)
C-----
C Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ),STIN(MVSIZ),GAMA(MVSIZ,6),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ)
C Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)
      INTEGER IOFFS,IFVM22,NN_DEL
      my_real
     .   OFFS(MVSIZ),DSV(MVSIZ)
C-----
C Variables utilisees dans les routines solides uniquement (en arguments).
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      my_real
     .   OFF(MVSIZ) ,OFFL(MVSIZ) , RHOO(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .  VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .  VX5(MVSIZ),VX6(MVSIZ),VX7(MVSIZ),VX8(MVSIZ),
     .  VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .  VY5(MVSIZ),VY6(MVSIZ),VY7(MVSIZ),VY8(MVSIZ),
     .  VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .  VZ5(MVSIZ),VZ6(MVSIZ),VZ7(MVSIZ),VZ8(MVSIZ),
     .  HX1(MVSIZ),HX2(MVSIZ),HX3(MVSIZ),HX4(MVSIZ),
     .  HY1(MVSIZ),HY2(MVSIZ),HY3(MVSIZ),HY4(MVSIZ),
     .  HZ1(MVSIZ),HZ2(MVSIZ),HZ3(MVSIZ),HZ4(MVSIZ),
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),
     .  PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .  PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .  PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .  PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .  PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .  PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .  PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .  PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .  PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .  PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .  PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .  PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
     .  PXC1(MVSIZ),PXC2(MVSIZ),PXC3(MVSIZ),PXC4(MVSIZ),
     .  PYC1(MVSIZ),PYC2(MVSIZ),PYC3(MVSIZ),PYC4(MVSIZ),
     .  PZC1(MVSIZ),PZC2(MVSIZ),PZC3(MVSIZ),PZC4(MVSIZ),
     .  BXY1(MVSIZ),BXY2(MVSIZ),BXY3(MVSIZ),BXY4(MVSIZ),
     .  BXY5(MVSIZ),BXY6(MVSIZ),BXY7(MVSIZ),BXY8(MVSIZ),
     .  BYX1(MVSIZ),BYX2(MVSIZ),BYX3(MVSIZ),BYX4(MVSIZ),
     .  BYX5(MVSIZ),BYX6(MVSIZ),BYX7(MVSIZ),BYX8(MVSIZ),
     .  BXZ1(MVSIZ),BXZ2(MVSIZ),BXZ3(MVSIZ),BXZ4(MVSIZ),
     .  BXZ5(MVSIZ),BXZ6(MVSIZ),BXZ7(MVSIZ),BXZ8(MVSIZ),
     .  BZX1(MVSIZ),BZX2(MVSIZ),BZX3(MVSIZ),BZX4(MVSIZ),
     .  BZX5(MVSIZ),BZX6(MVSIZ),BZX7(MVSIZ),BZX8(MVSIZ),
     .  BYZ1(MVSIZ),BYZ2(MVSIZ),BYZ3(MVSIZ),BYZ4(MVSIZ),
     .  BYZ5(MVSIZ),BYZ6(MVSIZ),BYZ7(MVSIZ),BYZ8(MVSIZ),
     .  BZY1(MVSIZ),BZY2(MVSIZ),BZY3(MVSIZ),BZY4(MVSIZ),
     .  BZY5(MVSIZ),BZY6(MVSIZ),BZY7(MVSIZ),BZY8(MVSIZ),
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
     .   HX(MVSIZ,4), HY(MVSIZ,4),  HZ(MVSIZ,4),
     .  XGXA(MVSIZ),XGYA(MVSIZ),XGZA(MVSIZ),
     .  XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),
     .  XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ)
      my_real
     .  DXY(MVSIZ),DYX(MVSIZ),
     .  DYZ(MVSIZ),DZY(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),DIVDE(MVSIZ)
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   G1X(MVSIZ),G2X(MVSIZ),G3X(MVSIZ),
     .   G1Y(MVSIZ),G2Y(MVSIZ),G3Y(MVSIZ),
     .   G1Z(MVSIZ),G2Z(MVSIZ),G3Z(MVSIZ)
      my_real
     .   WI,SMAX(MVSIZ),VOLG(MVSIZ),NU(MVSIZ),PP(MVSIZ),USB(MVSIZ),
     .   VOLM(MVSIZ),SIGM(MVSIZ),DTI,BID(MVSIZ)
      my_real
     .   SIGY(MVSIZ), SIGN(NEL,6),ET(MVSIZ), NU1(MVSIZ),
     . R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      my_real
     .   VX0(MVSIZ,8),VY0(MVSIZ,8),VZ0(MVSIZ,8),
     .   ALPHA_E(MVSIZ),LLSH(MVSIZ),LLSMIN(MVSIZ),AREA(MVSIZ)
      DOUBLE PRECISION 
     .   FACDP
      my_real
     .   SHF(MVSIZ),ZR,ZS,ZT,WT,ZZ,
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),
     .   TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ)
      my_real, DIMENSION(MVSIZ,NNPT) :: SMTT
      my_real, 
     .  DIMENSION(:), POINTER :: EINT
C-----
      my_real VARNL(NEL)
      my_real
     .   EV(mvsiz),evd(mvsiz)    !!!!   BIDON !!!!!
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
c-----------------------------------------------------
      PARAMETER (NLYMAX = 200,IPMAT = 100,IPANG = 200)
c-----------------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
c-----------------------------------------------------
c     Flag Bolt Preloading
      INTEGER IBOLTP,NBPRELD
      my_real, 
     .  DIMENSION(:), POINTER :: BPRELD
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF   => ELBUF_TAB(NG)%GBUF
!
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
!
      IBOLTP = IPARG(72,NG)
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
C-----
      IBID = 0
      IBIDON(1) = 0
      BID       = ZERO
      IF (JCVT==1 .AND. ISORTH==1) JCVT=2
      IAD0 = 1
      IF (IGTYP /= 22) THEN
        IF (ISORTH > 0) IAD0 = 1 + 6*NEL
        ISORTHG = 0
      END IF 
      IOR_TSH = 0
      IF (IGTYP == 21.OR.IGTYP == 22) IOR_TSH = 1
C
C-----------
      NF1=NFT+1
C--------------------------
       IF (IOR_TSH  >  0) THEN
         CALL SGPARAV3(
     .   8 ,X,IXS(1,NF1),
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
       ENDIF
C-----------
C GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
C-----------
      IF (JCVT == 0) THEN
C       GATHER NODAL VARIABLES
        CALL SCOOR3(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     2   X1, X2, X3, X4, X5, X6, X7, X8,
     3   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     4   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     5   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     6   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     7   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     8   VDX1, VDX2, VDX3, VDX4, VDX5, VDX6, VDX7, VDX8,
     9   VDY1, VDY2, VDY3, VDY4, VDY5, VDY6, VDY7, VDY8,
     A   VDZ1, VDZ2, VDZ3, VDZ4, VDZ5, VDZ6, VDZ7, VDZ8,
     B   VDX,VDY,VDZ,VD2,VIS,GBUF%OFF,OFFL,GBUF%SMSTR,
     C   GBUF%RHO,RHOO,NC1,NC2,NC3,NC4,NC5,NC6,
     C   NC7,NC8,NGL,MXT,NGEO,
     E   BID,BID,BID,BID,BID,BID,
     F   XD1, XD2  , XD3, XD4, XD5, XD6, XD7, XD8,
     G   YD1, YD2  , YD3, YD4, YD5, YD6, YD7, YD8,
     H   ZD1, ZD2  , ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     I   XDP, IPARG, NG , NEL)
      ELSE
C       GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
        CALL SRCOOR3(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     .    X1, X2, X3, X4, X5, X6, X7, X8,
     .    Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .    Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    VD2,VIS,GBUF%OFF,OFFL,GBUF%SMSTR,GBUF%RHO,RHOO,
     .    R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .    NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     .    NGL,MXT,NGEO,IOUTPRT, VGXA, VGYA, VGZA, VGA2,
     .    XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .    YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .    ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,     
     .    XDP, X0 , Y0 , Z0 , NEL, XGXA, XGYA, XGZA,
     .    XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,IPARG(1,NG),
     .    GBUF%GAMA_R) 
      ENDIF
      NN_DEL = 0
      PID = NGEO(1)
      IF (GEO(190,PID)+GEO(191,PID)+GEO(192,PID)+GEO(192,PID)>ZERO)
     .        NN_DEL=6
c
      IF (JCVT == 2 .AND. IOR_TSH  == 0) THEN
        CALL STORTH3(LFT,LLT,NEL,
     .       G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z, GBUF%GAMA)
      ENDIF
c
      DO I=LFT,LLT
        NU(I)=MIN(HALF,PM(21,MXT(I)))
        DELTAX(I)=EP30
        C1 =PM(32,MXT(I))
        E0(I) =THREE*(ONE-TWO*NU(I))*C1
        USB(I)=EM01/C1
        IDEG(I)=0
      ENDDO
c      
C---------R,S,T->S,T,R-------------
      ICS=ICSIG/100
      ICT=MOD(ICSIG/10,10)
      ICR=MOD(ICSIG,10)
c      NPTS=NPT/100
c      NPTT=MOD(NPT/10,10)
c      NPTR=MOD(NPT,10)

      NLAY=ELBUF_TAB(NG)%NLAY
      NPTR=ELBUF_TAB(NG)%NPTR
      NPTS=ELBUF_TAB(NG)%NPTS
      NPTT=ELBUF_TAB(NG)%NPTT
      IT = 1
c---------------------------      
      CALL S8ZDERIC3(OFFL,VOLG,NGL,
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .   PXC1, PXC2, PXC3, PXC4,
     .   PYC1, PYC2, PYC3, PYC4,
     .   PZC1, PZC2, PZC3, PZC4,
     .   PX1H1, PX1H2, PX1H3, PX1H4,
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4,
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   HX,   HY,   HZ,   
     .   AJC1,AJC2,AJC3,
     .   AJC4,AJC5,AJC6,
     .   AJC7,AJC8,AJC9,SMAX,GBUF%SMSTR,GBUF%OFF ,NEL)
      IF (IDTS6>0) THEN                   
        CALL SDLEN_DEGE(VOLG,DELTAX,         
     .       X1, X2, X3, X4, X5, X6, X7, X8, 
     .       Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, 
     .       Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, 
     .       IXS(1,NF1),IDEG )  
      END IF                                 
      IF (NTSHEG > 0) THEN
         CALL SDLENSH(VOLN,LLSH,AREA ,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
         LLSMIN(LFT:LLT) = DELTAX(LFT:LLT)
      END IF        
C --------------------------
C  --- UPDATE REF CONFIGURATION (possible future change to small strain option)
C  --- ! Total strain option doesn't change the Ref CONF.
C --------------------------
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
       CALL S8SAV3(GBUF%OFF,GBUF%SMSTR,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             NEL)
      END IF !(ISMSTR == 2) THEN
C
      IF (ICP == 2) THEN
        CALL S8CSIGP3(GBUF%SIG,E0  ,GBUF%PLA,FAC,GBUF%G_PLA,NEL)
        DO I=LFT,LLT
          NU1(I)=NU(I)+(HALF-NU(I))*FAC(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          NU1(I)=NU(I)
        ENDDO
      ENDIF 
C-----don't do it w/ degenerated elm    
      IF (ICP >0 .AND. IDTS6==0)  CALL DEGENES8(IXS(1,NF1),IDEG)
C-----------
C INITIALIZATION---Before increment-----
C-----------
      CALL S8ZZERO3(
     .  F11, F21, F31, F12, F22, F32, F13, F23, F33, F14, F24, F34, 
     .  F15, F25, F35, F16, F26, F36, F17, F27, F37, F18, F28, F38,
     .  GBUF%SIG ,GBUF%EINT,GBUF%RHO,GBUF%QVIS, GBUF%PLA ,
     .  GBUF%EPSD,STIN     ,PP      ,GBUF%G_PLA,GBUF%G_EPSD, 
     .  IEXPAN,   GBUF%EINTTH,NEL   ,CONDEN)
C-------------------------------------------
C    COMPUTE AVERAGE TEMPERATURE IN ELEMENT
C-------------------------------------------
      IF(JTHE < 0) THEN 
         DO I=LFT,LLT
           IF(GBUF%OFF(I) == ZERO) CYCLE
           TEMPEL(I) = ONE_OVER_8 *(  TEMP(NC1(I)) + TEMP(NC2(I))  
     .                          + TEMP(NC3(I)) + TEMP(NC4(I)) 
     .                          + TEMP(NC5(I)) + TEMP(NC6(I)) 
     .                          + TEMP(NC7(I)) + TEMP(NC8(I)))
           GBUF%TEMP(I) = TEMPEL(I)
         ENDDO
      ENDIF
C
C-----------Constant SIG traitement-----
C-----------dans ce cas unique constant direction- EV(NB2) est dans ortho. sys------
      IF (IOR_TSH > 0) THEN    ! Thick shell property - ortho or composite
        PID=NGEO(LFT)
c
c        NPTS0=NPTS
c        NPTT0=NPTT
c        NPTR0=NPTR
c        IF (ICS == 1) THEN
c          NPTR=NPTT0
c          NPTS=NPTR0
c          NPTT=NPTS0
c        ELSEIF (ICR == 1) THEN
c          NPTR=NPTS0
c          NPTS=NPTT0
c          NPTT=NPTR0
c        ENDIF 
c        IF (IGTYP == 22 .AND. NLAY==0) NLAY=IGEO(15,PID)
c------------------   
        DO IR=1,NPTR
          DO IS=1,NPTS
            DO I=LFT,LLT
              SIGM(I) = ZERO
              VOLM(I) = ZERO
            ENDDO
            DO IL=1,NLAY                                    
              LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)         
              DO I=LFT,LLT                                         
               SIGM(I) = SIGM(I)+LBUF%SIG(II(3)+I)*LBUF%VOL(I)         
               VOLM(I) = VOLM(I)+LBUF%VOL(I)                       
              ENDDO                                                
            ENDDO                                                  
            DO IL=1,NLAY                                        
              LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)         
              IP = IR + ( (IS-1) + (IL-1)*NPTS )*NPTR
              DO I=LFT,LLT                                         
                SMTT(I,IP)=(LBUF%SIG(II(3)+I)-SIGM(I)/VOLM(I))*USB(I)  
              ENDDO                                                
            ENDDO 
          ENDDO 
        ENDDO
c------------------
C       
        IF (IGTYP == 21) THEN                                           
          CALL SCTORTH3(LFT,LLT,ICSIG,NEL,
     .                  RX ,RY ,RZ ,SX ,SY ,SZ ,TX ,TY ,TZ ,            
     .                  R11,R21,R31,R12,R22,R32,R13,R23,R33,            
     .                  G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,GBUF%GAMA) 
c     .                  G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,EV(NBGAMA)) 
        ELSEIF (IGTYP == 22) THEN                                       
          IPTHK = IPANG+NLYMAX                                          
          IPPOS = IPTHK+NLYMAX                                          
          MTN0=MTN                                                      
          DO I=LFT,LLT                                                  
            MXT0(I)=MXT(I)                                               
            SHF(I)=GEO(38,NGEO(I))                                       
          ENDDO                                                         
        ENDIF                                                           
c---------
      ELSE  ! IOR_TSH == 0 = Isotropic thick shell
C-------------R--direction------
        IF (ICR == 1) THEN     ! normal = local X direction
          DO IS=1,NPTS
            DO IL=1,NLAY
              DO I=LFT,LLT                                       
               SIGM(I) = ZERO                                    
               VOLM(I) = ZERO                                    
              ENDDO                                              
              DO IR=1,NPTR                                     
                LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)       
                DO I=LFT,LLT                                     
                  SIGM(I) = SIGM(I) + LBUF%SIG(II(1)+I)*LBUF%VOL(I)  
                  VOLM(I) = VOLM(I) + LBUF%VOL(I)                
                ENDDO                                            
              ENDDO                                              
              DO IR=1,NPTR                                     
                LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                IP = IR + ( (IS-1) + (IL-1)*NPTS )*NPTR               
                DO I=LFT,LLT                                          
                  SMR(I,IP) = (LBUF%SIG(II(1)+I)-SIGM(I)/VOLM(I))*USB(I)  
                ENDDO                                                 
              ENDDO                                                   
            ENDDO                                                     
          ENDDO
        ENDIF                                                     
C--------------S--direction--
        IF (ICS == 1) THEN     ! normal = local Y direction
          DO IL=1,NLAY                                       
            DO IR=1,NPTR                                           
              DO I=LFT,LLT                                       
               SIGM(I) = ZERO                                    
               VOLM(I) = ZERO                                    
              ENDDO                                              
              DO IS=1,NPTS                                         
                LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)       
                DO I=LFT,LLT                                     
                  SIGM(I) = SIGM(I) + LBUF%SIG(II(2)+I)*LBUF%VOL(I)  
                  VOLM(I) = VOLM(I) + LBUF%VOL(I)                
                ENDDO                                            
              ENDDO                                              
              DO IS=1,NPTS                                         
                LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                IP = IR + ( (IS-1) + (IL-1)*NPTS )*NPTR               
                DO I=LFT,LLT                                          
                  SMS(I,IP) = (LBUF%SIG(II(2)+I)-SIGM(I)/VOLM(I))*USB(I)  
                ENDDO                                                 
              ENDDO                                                   
            ENDDO                                                     
          ENDDO                                                       
        ENDIF                                                     
C------------T--direction--
        IF (ICT == 1) THEN     ! normal = local Z direction
          DO IR=1,NPTR                                           
            DO IS=1,NPTS                                         
              DO I=LFT,LLT                                       
               SIGM(I) = ZERO                                    
               VOLM(I) = ZERO                                    
              ENDDO                                              
              DO IL=1,NLAY                                       
                LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)       
                DO I=LFT,LLT                                     
                  SIGM(I) = SIGM(I) + LBUF%SIG(II(3)+I)*LBUF%VOL(I)  
                  VOLM(I) = VOLM(I) + LBUF%VOL(I)                
                ENDDO                                            
              ENDDO                                              
              DO IL=1,NLAY                                            
                LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)             
                IP = IR + ( (IS-1) + (IL-1)*NPTS )*NPTR               
                DO I=LFT,LLT                                          
                  SMT(I,IP) = (LBUF%SIG(II(3)+I)-SIGM(I)/VOLM(I))*USB(I)  
                ENDDO                                                 
              ENDDO                                                   
            ENDDO                                                     
          ENDDO                                                     
        ENDIF                                                     
C----------
      ENDIF ! IF (IOR_TSH  > 0)
C---------------------------------
      IF ((ICT+ICR+ICS) >= 1) THEN
        IF (DT1 ==  ZERO) THEN
          DTI =ZERO
        ELSE
          DTI = ONE/DT1
        ENDIF 
      ENDIF 
C
      IOFFS=0
      DO I=LFT,LLT
        OFFS(I)=EP20
C
        FAC(I) = ONE
      END DO
      IF(JTHE < 0) THEM(LFT:LLT,1:8) = ZERO
C-----------Matrix B----------
C-----------Begin integrating points-----
c
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IL=1,NLAY
c         IF (ICR == 1) ILAY = IR
c         IF (ICS == 1) ILAY = IS
c         IF (ICT == 1) ILAY = IT
          IP = IR + ( (IS-1) + (IL-1)*NPTS )*NPTR
          LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)
C-----avoid multi-print	  
          IF (IOFFS == 1)THEN
           DO I=LFT,LLT
            IF (OFFS(I)<=TWO) LBUF%OFF(I)=OFFS(I)
           ENDDO
          END IF
C-----------
c         IF (IFAILURE > 0)  ILAY = IP
C----------------
         IF (IOR_TSH > 0) THEN  !  thick shell property
           IF (IGTYP == 22) THEN
            WT = GEO(IPTHK+IL,PID)
            ZZ = GEO(IPPOS+IL,PID)
            MID=IGEO(IPMAT+IL,PID)
            MTN=NINT(PM(19,MID))
            DO I=LFT,LLT
             MXT(I)=MID
            ENDDO
           ELSE
            ZZ = A_GAUSS(IL,NLAY)
            WT = W_GAUSS(IL,NLAY)
	          ENDIF
           IF (ICT == 1) THEN
            ZR = A_GAUSS(IR,NPTR)
            ZS = A_GAUSS(IS,NPTS)
            ZT = ZZ
           ELSEIF (ICS == 1) THEN
            ZR = A_GAUSS(IR,NPTR)
            ZS = ZZ
            ZT = A_GAUSS(IS,NPTS)
           ELSEIF (ICR == 1) THEN
            ZR = ZZ
            ZS = A_GAUSS(IR,NPTR)
            ZT = A_GAUSS(IS,NPTS)
           ENDIF 
         ELSE  !  solid property
          ZR = A_GAUSS(IR,NPTR)
          ZS = A_GAUSS(IS,NPTS)
          ZT = A_GAUSS(IL,NLAY)
          WT = W_GAUSS(IL,NLAY)
         ENDIF ! IF (IOR_TSH)
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT    
c
         CALL S8ZDERI3(OFFL,OFF,VOLN,NGL,
     .    ZR,ZS,ZT,WI,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
     .    HX,   HY,   HZ,   
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    AJC1,AJC2,AJC3,
     .    AJC4,AJC5,AJC6,
     .    AJC7,AJC8,AJC9,
     .    AJ1,AJ2,AJ3,
     .    AJ4,AJ5,AJ6,
     .    SMAX,DELTAX,NU1,ICP,IDEG,VOLDP)
C
         IF (IGTYP == 22) THEN
           IF (ICT == 1) THEN
             CALL S8ZDEFO3(
     .       PXC1, PXC2, PXC3, PXC4,
     .       PYC1, PYC2, PYC3, PYC4,
     .       PZC1, PZC2, PZC3, PZC4,
     .       PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .       PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .       PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .       PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .       PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .       PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .       PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .       PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .       PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .       BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .       BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .       BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .       BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .       BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .       BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .       VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .       VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .       VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .       DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .       WXX, WYY, WZZ,LBUF%VOL,OFF,LBUF%EINT,GBUF%OFF,DSV,
     .       ICP,ICR,ICS,ICT,DTI,SMR(1,IP),SMS(1,IP),SMTT(1,IP),
     .       NGL,IDEG,LBUF%VOL0DP)
           ELSEIF (ICS == 1) THEN
             CALL S8ZDEFO3(
     .       PXC1, PXC2, PXC3, PXC4,
     .       PYC1, PYC2, PYC3, PYC4,
     .       PZC1, PZC2, PZC3, PZC4,
     .       PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .       PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .       PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .       PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .       PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .       PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .       PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .       PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .       PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .       BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .       BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .       BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .       BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .       BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .       BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .       VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .       VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .       VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .       DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .       WXX, WYY, WZZ,LBUF%VOL,OFF,LBUF%EINT,GBUF%OFF,DSV,
     .       ICP,ICR,ICS,ICT,DTI,SMR(1,IP),SMTT(1,IP),SMT(1,IP),
     .       NGL,IDEG,LBUF%VOL0DP)
           ELSEIF (ICR == 1) THEN
             CALL S8ZDEFO3(
     .       PXC1, PXC2, PXC3, PXC4,
     .       PYC1, PYC2, PYC3, PYC4,
     .       PZC1, PZC2, PZC3, PZC4,
     .       PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .       PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .       PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .       PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .       PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .       PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .       PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .       PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .       PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .       BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .       BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .       BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .       BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .       BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .       BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .       VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .       VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .       VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .       DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .       WXX, WYY, WZZ,LBUF%VOL,OFF,LBUF%EINT,GBUF%OFF,DSV,
     .       ICP,ICR,ICS,ICT,DTI,SMTT(1,IP),SMS(1,IP),SMT(1,IP),
     .       NGL,IDEG,LBUF%VOL0DP)
           ENDIF 
	     ELSE
           CALL S8ZDEFO3(
     .     PXC1, PXC2, PXC3, PXC4,
     .     PYC1, PYC2, PYC3, PYC4,
     .     PZC1, PZC2, PZC3, PZC4,
     .     PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .     PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .     PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .     PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .     PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .     PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .     PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .     PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .     PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .     BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .     BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .     BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .     BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .     BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .     BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .     VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .     VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .     VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .     DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .     WXX, WYY, WZZ,LBUF%VOL,OFF,LBUF%EINT,GBUF%OFF,DSV,
     .     ICP,ICR,ICS,ICT,DTI,SMR(1,IP),SMS(1,IP),SMT(1,IP),
     .     NGL,IDEG,LBUF%VOL0DP)
        ENDIF 
C
        MX = MXT(LFT)
        DO I=LFT,LLT
          RHOO(I) = LBUF%RHO(I)
          TEMPEL(I) = PM(79,MX)
        ENDDO
        IF (IGTYP == 22)  
     .     CALL SCTORTH3(LFT,LLT,ICSIG,NEL,
     .                    RX ,RY ,RZ ,SX ,SY ,SZ ,TX ,TY ,TZ ,
     .                    R11,R21,R31,R12,R22,R32,R13,R23,R33,
     .                    G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,LBUF%GAMA)
        IF (JCVT == 2) THEN
         CALL SZORDEF3(LFT,LLT,DXX,DYY,DZZ,D4,D5,D6,
     .                G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z)
        ENDIF
        IF (IGTYP == 22) THEN
          IF (ICS == 1) THEN
           DO I=LFT,LLT
            D4(I)=SHF(I)*D4(I)
            D5(I)=SHF(I)*D5(I)
           ENDDO
          ELSEIF (ICR == 1) THEN
           DO I=LFT,LLT
            D4(I)=SHF(I)*D4(I)
            D6(I)=SHF(I)*D6(I)
           ENDDO
          ELSEIF (ICT == 1) THEN 
           DO I=LFT,LLT
            D5(I)=SHF(I)*D5(I)
            D6(I)=SHF(I)*D6(I)
           ENDDO
          ENDIF 
        ENDIF 
C        
        IF (NTSHEG > 0) THEN
          DO I=LFT,LLT
            IF (GBUF%IDT_TSH(I)<=0) CYCLE
            LLSMIN(I) = MIN(LLSMIN(I),DELTAX(I))
            DELTAX(I) = MAX(LLSH(I),DELTAX(I))
          ENDDO
        END IF
        DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))+DSV(1:NEL)   
        CALL SRHO3(PM         , LBUF%VOL  , LBUF%RHO , LBUF%EINT, DIVDE,
     2             FLUX(1,NF1), FLU1(NF1) , VOLN     , DVOL     , NGL, MXT, OFF,
     3             0          , GBUF%TAG22 , VOLDP,LBUF%VOL0DP,AMU  ,GBUF%OFF)
C
        IF (JCVT == 0) THEN
          CALL SROTA3(
     1   LBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
C-----------------------------
C      SMALL STRAIN
C-----------------------------
          CALL SMALLA3(GBUF%SMSTR,GBUF%OFF,OFF,WXX,WYY,WZZ,NEL)
C
        ELSE
C-----------------------------
C      EXTRACT STRESSES + SMALL STRAIN
C-----------------------------
          CALL CSMALL3(LBUF%SIG,S1,S2,S3,S4,S5,S6,
     .                 GBUF%OFF,OFF,NEL)
        ENDIF
C
C       for heat transfert
C
        IF(JTHE < 0 ) THEN
         CALL S8ZTEMPEL(LFT,LLT,NI,
     1                 ZR,ZS,ZT,
     2                 NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     3                 TEMP,TEMPEL)
        ENDIF    
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C     TIME STEP EST CALCULE ICI DT2T
C------------------------------------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
c
      IF(IBOLTP /= 0) CALL BOLTST(
     1   IBOLTP,    IP,        BPRELD,    LBUF%SIG,
     2   TT,        NEL,       SENSOR_TAB,NPT)

        CALL MMAIN(ELBUF_TAB,NG    ,PM     ,GEO     ,FV      ,ALE_CONNECT   ,
     2           IXS     ,IPARG    ,V      ,TF      ,NPF     ,BUFMAT  ,
     3           STI      ,X      ,DT2T   ,NELTST   ,ITYPTST ,OFFSET  ,
     4           NEL      ,W      ,OFF    ,NGEO     ,
     6           MXT      ,NGL    ,VOLN   ,VD2      ,DVOL    ,DELTAX  ,
     7           VIS      ,QVIS   ,CXX    ,S1       ,S2      ,S3      ,
     8           S4       ,S5     ,S6     ,DXX      ,DYY     ,DZZ     ,
     9           D4       ,D5     ,D6     ,WXX      ,WYY     ,WZZ     ,
     A           AJ1      ,AJ2    ,AJ3    ,AJ4      ,AJ5     ,AJ6     ,
     B           VDX      ,VDY    ,VDZ    ,MUVOID   ,SSP_EQ  ,AIRE    ,
     C           SIGY     ,ET     ,R1_FREE,LBUF%PLA ,R3_FREE ,AMU   ,
     DMFXX(1,IP),MFXY(1,IP),MFXZ(1,IP),MFYX(1,IP)   ,MFYY(1,IP),MFYZ(1,IP), 
     EMFZX(1,IP),MFZY(1,IP),MFZZ(1,IP)  ,IPM        ,GAMA      ,BID    ,
     F           BID    ,BID    ,BID    ,BID        ,BID        ,BID     ,
     G           ISTRAIN,TEMPEL ,DIE    ,IEXPAN     ,IL         ,MSSA    ,
     H           DMELS  ,IR     ,IS     ,IT         ,TABLE      ,BID     ,
     I           BID    ,BID    ,BID    ,IPARG(1,NG),IGEO    ,CONDE  ,
     J           ITASK  ,NLOC_DMG,VARNL ,MATPARAM_TAB,H3D_STRAIN)
C    
        IF (ISTRAIN == 1) THEN 
          CALL SSTRA3(DXX,DYY,DZZ,D4,D5,
     .                D6, LBUF%STRA, WXX, WYY, WZZ,
     .                OFF,NEL)
        ENDIF
C
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
        L_PLA  = ELBUF_TAB(NG)%BUFLY(IL)%L_PLA
        L_EPSD = ELBUF_TAB(NG)%BUFLY(IL)%L_EPSD
C
        IF (JCVT == 2) THEN
          CALL SROTO3(LFT,LLT,LBUF%SIG,SIGN,
     .            G1X, G2X, G3X, G1Y, G2Y, G3Y, G1Z, G2Z, G3Z,NEL)
          CALL S8ZFINT3(SIGN,SIGN,
     .    PX1, PX2, PX3, PX4,
     .    PY1, PY2, PY3, PY4,
     .    PZ1, PZ2, PZ3, PZ4,
     .    PX5, PX6, PX7, PX8,
     .    PY5, PY6, PY7, PY8,
     .    PZ5, PZ6, PZ7, PZ8,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .    F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .    VOLN,QVIS,PP,
     .    LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     .    GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,
     .    VOLG,STI,STIN,ICP,OFF,LBUF%VOL,GBUF%VOL,
     .    GBUF%G_PLA*L_PLA,L_EPSD,FAC,
     .    LBUF%EINTTH,GBUF%EINTTH,IEXPAN,NEL,IDEG,CONDE,CONDEN) 
        ELSE
C----------------------------
C         INTERNAL FORCES
C----------------------------
          CALL S8ZFINT3(LBUF%SIG,LBUF%SIG,
     .    PX1, PX2, PX3, PX4,
     .    PY1, PY2, PY3, PY4,
     .    PZ1, PZ2, PZ3, PZ4,
     .    PX5, PX6, PX7, PX8,
     .    PY5, PY6, PY7, PY8,
     .    PZ5, PZ6, PZ7, PZ8,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .    F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .    VOLN,QVIS,PP,
     .    LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     .    GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,
     .    VOLG,STI,STIN,ICP,OFF,LBUF%VOL,GBUF%VOL,
     .    L_PLA,L_EPSD,FAC, 
     .    LBUF%EINTTH,GBUF%EINTTH,IEXPAN,NEL,IDEG,CONDE,CONDEN) 
        ENDIF
        DO I=LFT,LLT                                        
          OFFL(I)=MIN(OFFL(I),OFF(I))                        
          IF (LBUF%OFF(I) > ONE .AND. GBUF%OFF(I) == ONE) THEN  
            OFFS(I)=MIN(LBUF%OFF(I),OFFS(I))                 
            IOFFS  =1                                        
          END IF                                             
        ENDDO                                               
CC-------------------------
c    finite element heat transfert  
C--------------------------
          IF(JTHE < 0) THEN
            CALL S8ETHERM(
     1   PM,      MXT,     VOLN,    NI,
     2   NC1,     NC2,     NC3,     NC4,
     3   NC5,     NC6,     NC7,     NC8,
     4   PX1,     PX2,     PX3,     PX4,
     5   PY1,     PY2,     PY3,     PY4,
     6   PZ1,     PZ2,     PZ3,     PZ4,
     7   PX5,     PX6,     PX7,     PX8,
     8   PY5,     PY6,     PY7,     PY8,
     9   PZ5,     PZ6,     PZ7,     PZ8,
     A   DT1,     TEMP,    TEMPEL,  DIE,
     B   THEM,    GBUF%OFF,LBUF%OFF,PARTSAV,
     C   IPARTS,  LBUF%VOL,NEL)
          ENDIF 
        ENDDO  ! IL=1,NLAY
       ENDDO   ! IS=1,NPTS
      ENDDO    ! IR=1,NPTR
C-----------End integrating points-----
c
      IF (IOFFS == 1) THEN
        DO I=LFT,LLT
          IF (OFFS(I) <= TWO) GBUF%OFF(I)=OFFS(I)
        END DO
c
        DO IR=1,NPTR                                         
         DO IS=1,NPTS                                        
          DO IL=1,NLAY                                       
            LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)       
            DO I=LFT,LLT                                     
              IF (GBUF%OFF(I) > ONE) LBUF%OFF(I)=GBUF%OFF(I)  
            END DO                                           
          END DO                                             
         END DO                                              
        END DO                                               
      END IF
c----------------------      
      IF (ICP == 1) THEN
       CALL S8ZFINTP3(
     .   PXC1, PXC2, PXC3, PXC4,
     .   PYC1, PYC2, PYC3, PYC4,
     .   PZC1, PZC2, PZC3, PZC4,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   VOLG,PP,IDEG)
      ENDIF
      IF (IGTYP == 22) THEN
        MTN = MTN0
        DO I=LFT,LLT
          MXT(I)=MXT0(I)
        ENDDO
      ENDIF
      IF ( NN_DEL> 0) THEN
        IF (NTSHEG == 0)
     .    CALL SDLENSH(VOLG,LLSH,AREA ,
     .          X1, X2, X3, X4, X5, X6, X7, X8,
     .          Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .          Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
        CALL TSHGEODEL3(NGL,GBUF%OFF,VOLG,AREA,GBUF%VOL,
     .                  LLSH,GEO(1,PID),NN_DEL,NEL )
      ENDIF
C-----------------------------
C     SMALL STRAIN 
C-----------------------------
      CALL SMALLB3(GBUF%OFF,OFFL)
C--------------------------
C       BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
           IF (JCVT == 0) THEN
            IFVM22      = 0
            CALL SBILAN(PARTSAV,GBUF%EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .                  VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .                  VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .                  VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .                  VOLG,IPARTS  , GRESAV,GRTH,IGRTH,GBUF%OFF,
     .                  IEXPAN,GBUF%EINTTH,GBUF%FILL,GBUF%MOM,NEL,IFVM22,
     .                  X1, X2, X3, X4, X5, X6, X7, X8,
     .                  Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .                  Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .                  ITASK,IPARG(1,NG))
           ELSE
            CALL SRBILAN(PARTSAV,GBUF%EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .                   VGXA, VGYA, VGZA, VGA2, VOLG,IPARTS,
     .                   GRESAV,GRTH,IGRTH,GBUF%OFF,IEXPAN,GBUF%EINTTH,
     .                   GBUF%FILL,XGXA, XGYA, XGZA,
     .                   XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
           ENDIF
      ENDIF
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      IF (JCVT/=0) THEN
       CALL SRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F11, F12, F13, F14, F15, F16, F17, F18,
     .   F21, F22, F23, F24, F25, F26, F27, F28,
     .   F31, F32, F33, F34, F35, F36, F37, F38)
      ENDIF
C----------------------------
      IF(NFILSOL/=0) CALL SFILLOPT(
     1   GBUF%FILL,STIN,     F11,      F21,
     2   F31,      F12,      F22,      F32,
     3   F13,      F23,      F33,      F14,
     4   F24,      F34,      F15,      F25,
     5   F35,      F16,      F26,      F36,
     6   F17,      F27,      F37,      F18,
     7   F28,      F38,      NEL)
C----------------------------
      IF(IPARIT == 0)THEN
          CALL SCUMU3(GBUF%OFF,A,NC1, NC2, NC3, NC4,
     2             NC5, NC6, NC7, NC8,STIFN,STIN,
     3   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     4   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     5   NVC,BID,BID,BID,
     6   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     7   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     8   THEM, FTHE,CONDN,CONDEN)
      ELSE
          CALL SCUMU3P(GBUF%OFF,STIN,FSKY,FSKY,IADS,
     2   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     3   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     4   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     5   BID,BID,BID,
     6   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     7   BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,BID,
     8   THEM, FTHESKY,CONDNSKY,CONDEN)
      ENDIF
      IF (NTSHEG > 0) THEN
        ALPHA_E(LFT:LLT) = ONE  
        DO I=LFT,LLT
          IF (GBUF%IDT_TSH(I)<=0) CYCLE
          FACDP = 1.343*LLSH(I)/LLSMIN(I)
          ALPHA_E(I) = FACDP*FACDP  
        ENDDO
        CALL SCUMUALPHA(GBUF%OFF,ALPHA_E,NC1, NC2, NC3, NC4,
     +                               NC5, NC6, NC7, NC8)
      END IF        
C-----------
      RETURN
      END SUBROUTINE S8CFORC3
      
Chd|====================================================================
Chd|  SCTORTH3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCTORTH3(JFT,JLT,ICSTR,NEL,
     .                   RX ,RY ,RZ ,SX ,SY ,SZ ,TX ,TY ,TZ ,
     .                   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,GAMA)
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
      INTEGER JFT, JLT,ICSTR,NEL
C     REAL
      my_real
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*),
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
C     REAL
      my_real 
     .   V1,V2,V3,VR,VS,AA,BB,SUMA,CPN(MVSIZ),SPN(MVSIZ)
C-----------------------------------------------
        IF(IREP == 0) THEN
           CPN(JFT:JLT) = GAMA(JFT:JLT,1)
           SPN(JFT:JLT) = GAMA(JFT:JLT,2)
        ELSE
         SELECT CASE (ICSTR)                                             
          CASE (1)                                                        
          DO I=JFT,JLT
            AA = GAMA(I,1)
            BB = GAMA(I,2)
            V1 = AA*RX(I) + BB*SX(I)
            V2 = AA*RY(I) + BB*SY(I)
            V3 = AA*RZ(I) + BB*SZ(I)
            VR=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            VS=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            CPN(I) = VR*SUMA
            SPN(I) = VS*SUMA
          ENDDO
         CASE (100)                                                        
          DO I=JFT,JLT
            AA = GAMA(I,1)
            BB = GAMA(I,2)
            V1 = AA*SX(I) + BB*TX(I)
            V2 = AA*SY(I) + BB*TY(I)
            V3 = AA*SZ(I) + BB*TZ(I)
            VR=V1*E3X(I)+V2*E3Y(I)+V3*E3Z(I)
            VS=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            CPN(I) = VR*SUMA
            SPN(I) = VS*SUMA
          ENDDO
        CASE (10)                                                        
          DO I=JFT,JLT
            AA = GAMA(I,1)
            BB = GAMA(I,2)
            V1 = AA*TX(I) + BB*RX(I)
            V2 = AA*TY(I) + BB*RY(I)
            V3 = AA*TZ(I) + BB*RZ(I)
            VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
            VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
            SUMA=SQRT(VR*VR + VS*VS)
            SUMA=ONE/MAX(EM20,SUMA)
            CPN(I) = VR*SUMA
            SPN(I) = VS*SUMA
          ENDDO
         END SELECT                                                      
        ENDIF
C
        SELECT CASE (ICSTR)                                             
        CASE (1)                                                        
            G1X(JFT:JLT)=ZERO
            G1Y(JFT:JLT)=CPN(JFT:JLT)
            G1Z(JFT:JLT)=SPN(JFT:JLT)
            G2X(JFT:JLT)=ZERO                                                 
            G2Y(JFT:JLT)=-SPN(JFT:JLT)                                                 
            G2Z(JFT:JLT)=CPN(JFT:JLT)                                               
            G3X(JFT:JLT)=ONE
            G3Y(JFT:JLT)=ZERO
            G3Z(JFT:JLT)=ZERO
        CASE (100)                                                        
            G1X(JFT:JLT)=SPN(JFT:JLT)
            G1Y(JFT:JLT)=ZERO
            G1Z(JFT:JLT)=CPN(JFT:JLT)
            G2X(JFT:JLT)=CPN(JFT:JLT)                                                 
            G2Y(JFT:JLT)=ZERO                                                 
            G2Z(JFT:JLT)=-SPN(JFT:JLT)                                               
            G3X(JFT:JLT)=ZERO
            G3Y(JFT:JLT)=ONE
            G3Z(JFT:JLT)=ZERO
        CASE (10)                                                        
            G1X(JFT:JLT)=CPN(JFT:JLT)
            G1Y(JFT:JLT)=SPN(JFT:JLT)
            G1Z(JFT:JLT)=ZERO
            G2X(JFT:JLT)=-SPN(JFT:JLT)                                                 
            G2Y(JFT:JLT)=CPN(JFT:JLT)                                                 
            G2Z(JFT:JLT)= ZERO                                               
            G3X(JFT:JLT)= ZERO
            G3Y(JFT:JLT)= ZERO
            G3Z(JFT:JLT)= ONE
       END SELECT                                                      
C----------
      RETURN
      END SUBROUTINE SCTORTH3
      
