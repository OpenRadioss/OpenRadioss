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
Chd|  S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BASIS8                        source/elements/solid/solide8/basis8.F
Chd|        BOLTST                        source/elements/solid/solide/boltst.F
Chd|        CSMALL3                       source/elements/solid/solide/csmall3.F
Chd|        DEGENES8                      source/elements/solid/solide/degenes8.F
Chd|        JACOB_F0                      source/elements/solid/solide8e/jacob_f0.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S8EDEFW3                      source/elements/solid/solide8e/s8edefw3.F
Chd|        S8ETHERM                      source/elements/solid/solide8e/s8etherm.F
Chd|        S8FUPD11T12                   source/elements/solid/solide8e/s8fupd11t12.F
Chd|        S8SAV12                       source/elements/solid/solide/s8sav12.F
Chd|        S8SAV3                        source/elements/solid/solide/s8sav3.F
Chd|        S8ZDEFO3                      source/elements/solid/solide8z/s8zdefo3.F
Chd|        S8ZDEFOT3                     source/elements/solid/solide8z/s8zdefot3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderic3.F
Chd|        S8ZDERICM3                    source/elements/solid/solide8z/s8zdericm3.F
Chd|        S8ZDERICT3                    source/elements/solid/solide8z/s8zderict3.F
Chd|        S8ZDERICTO3                   source/elements/solid/solide8z/s8zdericto3.F
Chd|        S8ZDERIM3                     source/elements/solid/solide8z/s8zderim3.F
Chd|        S8ZDERIMS3                    source/elements/solid/solide8z/s8zderims3.F
Chd|        S8ZDERIPR3                    source/elements/solid/solide8z/s8zderipr3.F
Chd|        S8ZDERIT3                     source/elements/solid/solide8z/s8zderit3.F
Chd|        S8ZFINT3                      source/elements/solid/solide8z/s8zfint3.F
Chd|        S8ZFINTP3                     source/elements/solid/solide8z/s8zfintp3.F
Chd|        S8ZFINT_REG                   source/elements/solid/solide8z/s8zfint_reg.F
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zsigp3.F
Chd|        S8ZTEMPEL                     source/elements/solid/solide8z/s8ztempel.F
Chd|        S8ZZERO3                      source/elements/solid/solide8z/s8zzero3.F
Chd|        SBILAN                        source/elements/solid/solide/sbilan.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SCOOR_CP2SP                   source/elements/solid/solidez/scoor_cp2sp.F
Chd|        SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3p.F
Chd|        SDLENMAX                      source/elements/solid/solide/sdlenmax.F
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen_dege.F
Chd|        SDLEN_SM11                    source/elements/solid/solide/sdlen_sm11.F
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SGCOOR3                       source/elements/solid/solide/sgcoor3.F
Chd|        SGEODEL3                      source/elements/solid/solide/sgeodel3.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SMALLG3                       source/elements/solid/solide/smallg3.F
Chd|        SMALLGEO3                     source/elements/solid/solide/smallgeo3.F
Chd|        SORDEFT3                      source/elements/solid/solidez/sordeft3.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SROTO3                        source/elements/solid/solidez/sroto3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        SRROTADP                      source/elements/solid/solide/srrotadp.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        STORTH3                       source/elements/solid/solidez/storth3.F
Chd|        SZORDEF3                      source/elements/solid/solidez/szordef3.F
Chd|        SZTORTH3                      source/elements/solid/solidez/sztorth3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S8ZFORC3(ELBUF_TAB,NG     ,
     1                    PM       ,GEO    ,IXS    ,X       ,
     2                    A        ,V      ,MS     ,W       ,FLUX    ,
     3                    FLU1     ,VEUL   ,FV     ,ALE_CONNECT   ,IPARG   ,
     4                    TF       ,NPF    ,BUFMAT ,PARTSAV ,NLOC_DMG,
     5                    DT2T     ,NELTST ,ITYPTST,STIFN   ,FSKY    ,
     6                    IADS     ,OFFSET ,EANI   ,IPARTS  ,ICP     ,
     7                    F11      ,F21    ,F31    ,F12     ,F22     ,
     8                    F32      ,F13    ,F23    ,F33     ,F14     ,
     9                    F24      ,F34    ,F15    ,F25     ,F35     ,
     A                    F16      ,F26    ,F36    ,F17     ,F27     ,
     B                    F37      ,F18    ,F28    ,F38     ,NEL     ,
     F                    ICSIG    ,SMR    ,SMS    ,SMT     ,MFXX    ,
     G                    MFXY     ,MFXZ   ,MFYX   ,MFYY    ,MFYZ    ,
     H                    MFZX     ,MFZY   ,MFZZ   ,NVC     ,IPM     ,
     I                    ITASK    ,ISTRAIN,TEMP   ,FTHE    , 
     J                    FTHESKY  ,IEXPAN ,IGEO   ,NNPT    ,GRESAV  ,
     K                    GRTH     ,IGRTH  ,MSSA   ,DMELS   ,TABLE   ,
     L                    XDP      ,VOLN   ,CONDN  ,CONDNSKY  ,JFAC,
     M                    D        ,SENSORS,IOUTPRT,MAT_ELEM,H3D_STRAIN,
     N                    DT       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE SENSOR_MOD
      USE DT_MOD
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
#include      "scr07_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG,NGROUP),NPF(*),
     .        IPARTS(*), IPM(NPROPMI,*),IGEO(NPROPGI,*),GRTH(*),
     .        IGRTH(*),IADS(8,*),IOUTPRT
C
      INTEGER NELTST,ITYPTST,OFFSET,NEL,ICP,NG, 
     .        ICSIG, NVC, ITASK, ISTRAIN, IEXPAN ,NNPT,H3D_STRAIN
     
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
     .    JFAC(MVSIZ,729),
     .    TEMP(*), FTHE(*), FTHESKY(*),GRESAV(*), MSSA(*), DMELS(*),VOLN(MVSIZ),
     .    CONDN(*),CONDNSKY(*),D(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (SENSORS_) , INTENT(IN) :: SENSORS
      TYPE(DT_),     INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      INTEGER  I,J,N,LCO,NF1,IFLAG,IAD0,ILAY,NLAY,
     .   IP,IR,IS,IT,NPTR,NPTS,NPTT,ICR,ICS,ICT,PID,MTN0,IPTHK,
     .   IPPOS,IPMAT,NLYMAX,MID,IPANG,NPTR0,NPTS0,NPTT0,IBID,MX

      DOUBLE PRECISION 
     .   X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),VOLP(MVSIZ,NNPT)
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),MXT0(MVSIZ),IPARBID(1)
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
     . E0(MVSIZ),C1,FAC(MVSIZ) ,THEM(MVSIZ,8),TEMPEL(MVSIZ),NI(8),
     . DIE(MVSIZ),CONDE(MVSIZ) ,CONDEN(MVSIZ),AMU(MVSIZ)
C-----
C   Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ),STIN(MVSIZ),GAMA(MVSIZ,6),
     .   WXX0(MVSIZ) , WYY0(MVSIZ) , WZZ0(MVSIZ),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ)
C   Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)
      INTEGER IOFFS,ITET,IFVM22
      my_real
     .   OFFS(MVSIZ),DSV(MVSIZ)
C-----
C   Variables utilisees dans les routines solides uniquement (en arguments).
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      INTEGER IDEG(MVSIZ)
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
     .   HX(MVSIZ,4), HY(MVSIZ,4),  HZ(MVSIZ,4),OFFG0(MVSIZ),
     .  XGXA(MVSIZ),XGYA(MVSIZ),XGZA(MVSIZ),
     .  XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),
     .  XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ)
      my_real
     .  DXY(MVSIZ),DYX(MVSIZ),
     .  DYZ(MVSIZ),DZY(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),DETF0(MVSIZ),DIVDE(MVSIZ)
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
     .   VX0(MVSIZ,8),VY0(MVSIZ,8),VZ0(MVSIZ,8)
      my_real
     .   SHF(MVSIZ),ZR,ZS,ZT,WT,ZZ,
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),
     .   TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ)
      my_real, DIMENSION(MVSIZ,NNPT) :: AJI1,AJI2,AJI3
      my_real, DIMENSION(MVSIZ,NNPT) :: AJI4,AJI5,AJI6
      my_real, DIMENSION(MVSIZ,NNPT) :: AJI7,AJI8,AJI9
      INTEGER NNEGA,INDEX(MVSIZ)
      my_real
     .   AJ7(MVSIZ) , AJ8(MVSIZ) , AJ9(MVSIZ),L_MAX(MVSIZ),DIV0(MVSIZ) 
C----- 

c
C-----
C     Variables utilis  es pour le non-local
      my_real,
     .  DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      INTEGER :: INLOC, L_NLOC, INOD(8), IPOS(8), IMAT,NN_DEL
      my_real,
     .  DIMENSION(:), POINTER :: DNL
      my_real 
     .  H(8),PS(8),PR(8),PT(8)

c     Flag Bolt Preloading
      INTEGER IBOLTP,NBPRELD,ISM12_11,II(6)
      my_real, 
     .  DIMENSION(:), POINTER :: BPRELD
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
c-----------------------------------------------------
      PARAMETER (NLYMAX = 200,IPMAT = 100,IPANG = 200)
c-----------------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
c---
     1 2.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 1.D0               ,1.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     3 0.555555555555556D0,0.888888888888889D0,0.555555555555556D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 0.347854845137454D0,0.652145154862546D0,0.652145154862546D0,
     4 0.347854845137454D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 0.236926885056189D0,0.478628670499366D0,0.568888888888889D0,
     5 0.478628670499366D0,0.236926885056189D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 0.171324492379170D0,0.360761573048139D0,0.467913934572691D0,
     6 0.467913934572691D0,0.360761573048139D0,0.171324492379170D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 0.129484966168870D0,0.279705391489277D0,0.381830050505119D0,
     7 0.417959183673469D0,0.381830050505119D0,0.279705391489277D0,
     7 0.129484966168870D0,0.D0               ,0.D0               ,
     8 0.101228536290376D0,0.222381034453374D0,0.313706645877887D0,
     8 0.362683783378362D0,0.362683783378362D0,0.313706645877887D0,
     8 0.222381034453374D0,0.101228536290376D0,0.D0               ,
     9 0.081274388361574D0,0.180648160694857D0,0.260610696402935D0,
     9 0.312347077040003D0,0.330239355001260D0,0.312347077040003D0,
     9 0.260610696402935D0,0.180648160694857D0,0.081274388361574D0/
c------------------------------------------------------------
      DATA A_GAUSS / 
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 -.577350269189625D0,0.577350269189625D0,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               , 
     3 -.774596669241483D0,0.D0               ,0.774596669241483D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 -.861136311594053D0,-.339981043584856D0,0.339981043584856D0,
     4 0.861136311594053D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 -.906179845938664D0,-.538469310105683D0,0.D0               ,
     5 0.538469310105683D0,0.906179845938664D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 -.932469514203152D0,-.661209386466265D0,-.238619186083197D0,
     6 0.238619186083197D0,0.661209386466265D0,0.932469514203152D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 -.949107912342759D0,-.741531185599394D0,-.405845151377397D0,
     7 0.D0               ,0.405845151377397D0,0.741531185599394D0,
     7 0.949107912342759D0,0.D0               ,0.D0               ,
     8 -.960289856497536D0,-.796666477413627D0,-.525532409916329D0,
     8 -.183434642495650D0,0.183434642495650D0,0.525532409916329D0,
     8 0.796666477413627D0,0.960289856497536D0,0.D0               ,
     9 -.968160239507626D0,-.836031107326636D0,-.613371432700590D0,
     9 -.324253423403809D0,0.D0               ,0.324253423403809D0,
     9 0.613371432700590D0,0.836031107326636D0,0.968160239507626D0/
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF   =>ELBUF_TAB(NG)%GBUF
      NPTR   = ELBUF_TAB(NG)%NPTR
      NPTS   = ELBUF_TAB(NG)%NPTS
      NPTT   = ELBUF_TAB(NG)%NPTT
      NLAY   = ELBUF_TAB(NG)%NLAY
      ILAY   = 1
      ICS    = 0
      ICT    = 0
      ICR    = 0
      DTI = ZERO
      IBOLTP  = IPARG(72,NG)
      INLOC   = IPARG(78,NG) 
      ALLOCATE(VAR_REG(NEL,NPTR*NPTS*NPTT))
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
      ISM12_11 = ELBUF_TAB(NG)%BUFLY(1)%L_SIGL
      TEMPEL(1:MVSIZ) = ZERO
C----
      NF1=NFT+1
C----
      IBID = 0
      IPARBID= 0
      IF (JCVT==1 .AND. ISORTH==1) JCVT=2
      IF (IGTYP /= 22) ISORTHG = 0
c      MPT = IABS(NPT)
c      NPTS=MPT/100
c      NPTT=MOD(MPT/10,10)
c      NPTR=MOD(MPT,10)
C--------------------------
C-----------
C GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
C-----------
      IF (JCVT == 0) THEN
C GATHER NODAL VARIABLES
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
     F   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     G   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     H   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     I   XDP,IPARG,NG , NEL)
      ELSE
C GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
        CALL SRCOOR3(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   VD2,VIS,GBUF%OFF,OFFL,GBUF%SMSTR,GBUF%RHO,RHOO,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     .   NGL,MXT,NGEO,IOUTPRT, VGXA, VGYA, VGZA, VGA2,
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,     
     .   XDP, X0 , Y0 , Z0 , NEL, XGXA, XGYA, XGZA,
     .   XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,IPARG(1,NG),
     .   GBUF%GAMA_R) 
      ENDIF
      NN_DEL = 0
      PID = NGEO(1)
      IF (GEO(190,PID)+GEO(191,PID)+GEO(192,PID)+GEO(192,PID)>ZERO)
     .        NN_DEL=8
      IF (NN_DEL ==0 .AND. DT%IDEL_BRICK>0) NN_DEL=8
C-----------
C GATHER NODAL VARIABLES FOR TOTAL STRAIN CASE.
C-----------
      IF (ISMSTR >= 10.AND.ISMSTR <= 12) THEN
        CALL SGCOOR3(
     1   TT,        8,         X,         IXS(1,NF1),
     2   X0,        Y0,        Z0,        VX0,
     3   VY0,       VZ0,       GBUF%SMSTR,D,
     4   GBUF%OFF,  OFFG0,     NEL,       XDP,
     5   MTN,       ISMSTR)
C-----------Pij is in global system w/ Ismstr10 and local system w/ 11
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
       END IF !(ISMSTR == 11) THEN
      ENDIF    ! ISMSTR == 10.OR.ISMSTR == 11
c
      MX = MXT(LFT)
      DO I=LFT,LLT
        NU(I)=MIN(HALF,PM(21,MX))
        DELTAX(I)=EP30
        C1 =PM(32,MX)
        E0(I) =THREE*(ONE-TWO*NU(I))*C1
        USB(I)=EM01/C1
        IDEG(I)=0
      ENDDO
c---------------------------      
      IF (ISMSTR >= 10.AND.ISMSTR <= 12) THEN
       IF (ISMSTR == 11) THEN
        CALL S8ZDERICT3(
     1   OFFL,    VOLG,    NGL,     X0(1,1),
     2   X0(1,2), X0(1,3), X0(1,4), X0(1,5),
     3   X0(1,6), X0(1,7), X0(1,8), Y0(1,1),
     4   Y0(1,2), Y0(1,3), Y0(1,4), Y0(1,5),
     5   Y0(1,6), Y0(1,7), Y0(1,8), Z0(1,1),
     6   Z0(1,2), Z0(1,3), Z0(1,4), Z0(1,5),
     7   Z0(1,6), Z0(1,7), Z0(1,8), PXC1,
     8   PXC2,    PXC3,    PXC4,    PYC1,
     9   PYC2,    PYC3,    PYC4,    PZC1,
     A   PZC2,    PZC3,    PZC4,    PX1H1,
     B   PX1H2,   PX1H3,   PX1H4,   PX2H1,
     C   PX2H2,   PX2H3,   PX2H4,   PX3H1,
     D   PX3H2,   PX3H3,   PX3H4,   PX4H1,
     E   PX4H2,   PX4H3,   PX4H4,   HX,
     F   HY,      HZ,      AJC1,    AJC2,
     G   AJC3,    AJC4,    AJC5,    AJC6,
     H   AJC7,    AJC8,    AJC9,    SMAX,
     I   NEL)
        CALL SCOOR_CP2SP(
     1   X0,      Y0,      Z0,      X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      NEL)
        IF (IDTS6>0) THEN
         CALL SDLEN_DEGE(
     1   VOLG,      DELTAX,    X1,        X2,
     2   X3,        X4,        X5,        X6,
     3   X7,        X8,        Y1,        Y2,
     4   Y3,        Y4,        Y5,        Y6,
     5   Y7,        Y8,        Z1,        Z2,
     6   Z3,        Z4,        Z5,        Z6,
     7   Z7,        Z8,        IXS(1,NF1),IDEG,
     8   NEL)
        END IF
        CALL SDLEN_SM11(
     1   VOLG,    DELTAX,  SMAX,    IDEG,
     2   NEL)
        IF (JCVT == 2) THEN
         CALL STORTH3(
     1   LFT,      LLT,      NEL,      G1X,
     2   G1Y,      G1Z,      G2X,      G2Y,
     3   G2Z,      G3X,      G3Y,      G3Z,
     4   GBUF%GAMA,ISORTH)
        ENDIF
C------Ismstr 10,12       
       ELSE
        CALL S8ZDERICTO3(
     1   GBUF%OFF,  VOLG,      NGL,       X0(1,1),
     2   X0(1,2),   X0(1,3),   X0(1,4),   X0(1,5),
     3   X0(1,6),   X0(1,7),   X0(1,8),   Y0(1,1),
     4   Y0(1,2),   Y0(1,3),   Y0(1,4),   Y0(1,5),
     5   Y0(1,6),   Y0(1,7),   Y0(1,8),   Z0(1,1),
     6   Z0(1,2),   Z0(1,3),   Z0(1,4),   Z0(1,5),
     7   Z0(1,6),   Z0(1,7),   Z0(1,8),   PXC1,
     8   PXC2,      PXC3,      PXC4,      PYC1,
     9   PYC2,      PYC3,      PYC4,      PZC1,
     A   PZC2,      PZC3,      PZC4,      PX1H1,
     B   PX1H2,     PX1H3,     PX1H4,     PX2H1,
     C   PX2H2,     PX2H3,     PX2H4,     PX3H1,
     D   PX3H2,     PX3H3,     PX3H4,     PX4H1,
     E   PX4H2,     PX4H3,     PX4H4,     HX,
     F   HY,        HZ,        AJC1,      AJC2,
     G   AJC3,      AJC4,      AJC5,      AJC6,
     H   AJC7,      AJC8,      AJC9,      SMAX,
     I   GBUF%JAC_I,NEL,       ISMSTR)
        CALL SZTORTH3(LFT,LLT,ISORTH,NEL,
     .         R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .         G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z, GBUF%GAMA)
       END IF !(ISMSTR==11) THEN
       IF (ICP ==1) THEN
        CALL JACOB_F0(
     1   DETF0,   PXC1,    PXC2,    PXC3,
     2   PXC4,    PYC1,    PYC2,    PYC3,
     3   PYC4,    PZC1,    PZC2,    PZC3,
     4   PZC4,    VX0(1,1),VX0(1,2),VX0(1,3),
     5   VX0(1,4),VX0(1,5),VX0(1,6),VX0(1,7),
     6   VX0(1,8),VY0(1,1),VY0(1,2),VY0(1,3),
     7   VY0(1,4),VY0(1,5),VY0(1,6),VY0(1,7),
     8   VY0(1,8),VZ0(1,1),VZ0(1,2),VZ0(1,3),
     9   VZ0(1,4),VZ0(1,5),VZ0(1,6),VZ0(1,7),
     A   VZ0(1,8),DIV0,    NEL)
        IF (IDTS6==0)  CALL DEGENES8(
     1   IXS(1,NF1),IDEG,      NEL)
         DO I=LFT,LLT
          IF (IDEG(I)>0) IDEG(I) = IDEG(I) + 10
         ENDDO
       END IF
C-----------Begin integrating points-----
C
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
             LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
         CALL S8ZDERIT3(
     1   OFFL,            OFF,             VOLN,            NGL,
     2   A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,
     3   PXC1,            PXC2,            PXC3,            PXC4,
     4   PYC1,            PYC2,            PYC3,            PYC4,
     5   PZC1,            PZC2,            PZC3,            PZC4,
     6   PX1H1,           PX1H2,           PX1H3,           PX1H4,
     7   PX2H1,           PX2H2,           PX2H3,           PX2H4,
     8   PX3H1,           PX3H2,           PX3H3,           PX3H4,
     9   PX4H1,           PX4H2,           PX4H3,           PX4H4,
     A   HX,              HY,              HZ,              PX1,
     B   PX2,             PX3,             PX4,             PX5,
     C   PX6,             PX7,             PX8,             PY1,
     D   PY2,             PY3,             PY4,             PY5,
     E   PY6,             PY7,             PY8,             PZ1,
     F   PZ2,             PZ3,             PZ4,             PZ5,
     G   PZ6,             PZ7,             PZ8,             PXY1,
     H   PXY2,            PXY3,            PXY4,            PXY5,
     I   PXY6,            PXY7,            PXY8,            PYX1,
     J   PYX2,            PYX3,            PYX4,            PYX5,
     K   PYX6,            PYX7,            PYX8,            PXZ1,
     L   PXZ2,            PXZ3,            PXZ4,            PXZ5,
     M   PXZ6,            PXZ7,            PXZ8,            PZX1,
     N   PZX2,            PZX3,            PZX4,            PZX5,
     O   PZX6,            PZX7,            PZX8,            PYZ1,
     P   PYZ2,            PYZ3,            PYZ4,            PYZ5,
     Q   PYZ6,            PYZ7,            PYZ8,            PZY1,
     R   PZY2,            PZY3,            PZY4,            PZY5,
     S   PZY6,            PZY7,            PZY8,            BXY1,
     T   BXY2,            BXY3,            BXY4,            BXY5,
     U   BXY6,            BXY7,            BXY8,            BYX1,
     V   BYX2,            BYX3,            BYX4,            BYX5,
     W   BYX6,            BYX7,            BYX8,            BXZ1,
     X   BXZ2,            BXZ3,            BXZ4,            BXZ5,
     Y   BXZ6,            BXZ7,            BXZ8,            BZX1,
     Z   BZX2,            BZX3,            BZX4,            BZX5,
     1   BZX6,            BZX7,            BZX8,            BYZ1,
     2   BYZ2,            BYZ3,            BYZ4,            BYZ5,
     3   BYZ6,            BYZ7,            BYZ8,            BZY1,
     4   BZY2,            BZY3,            BZY4,            BZY5,
     5   BZY6,            BZY7,            BZY8,            AJC1,
     6   AJC2,            AJC3,            AJC4,            AJC5,
     7   AJC6,            AJC7,            AJC8,            AJC9,
     8   AJ1,             AJ2,             AJ3,             AJ4,
     9   AJ5,             AJ6,             SMAX,            DELTAX,
     A   NU,              ICP,             IDEG,            LBUF%JAC_I,
     B   NEL,             MTN,             ISMSTR,          JHBE)
         CALL S8ZDEFOT3(
     1   PX1,       PX2,       PX3,       PX4,
     2   PX5,       PX6,       PX7,       PX8,
     3   PY1,       PY2,       PY3,       PY4,
     4   PY5,       PY6,       PY7,       PY8,
     5   PZ1,       PZ2,       PZ3,       PZ4,
     6   PZ5,       PZ6,       PZ7,       PZ8,
     7   PXY1,      PXY2,      PXY3,      PXY4,
     8   PXY5,      PXY6,      PXY7,      PXY8,
     9   PYX1,      PYX2,      PYX3,      PYX4,
     A   PYX5,      PYX6,      PYX7,      PYX8,
     B   PXZ1,      PXZ2,      PXZ3,      PXZ4,
     C   PXZ5,      PXZ6,      PXZ7,      PXZ8,
     D   PZX1,      PZX2,      PZX3,      PZX4,
     E   PZX5,      PZX6,      PZX7,      PZX8,
     F   PYZ1,      PYZ2,      PYZ3,      PYZ4,
     G   PYZ5,      PYZ6,      PYZ7,      PYZ8,
     H   PZY1,      PZY2,      PZY3,      PZY4,
     I   PZY5,      PZY6,      PZY7,      PZY8,
     J   BXY1,      BXY2,      BXY3,      BXY4,
     K   BXY5,      BXY6,      BXY7,      BXY8,
     L   BYX1,      BYX2,      BYX3,      BYX4,
     M   BYX5,      BYX6,      BYX7,      BYX8,
     N   BXZ1,      BXZ2,      BXZ3,      BXZ4,
     O   BXZ5,      BXZ6,      BXZ7,      BXZ8,
     P   BZX1,      BZX2,      BZX3,      BZX4,
     Q   BZX5,      BZX6,      BZX7,      BZX8,
     R   BYZ1,      BYZ2,      BYZ3,      BYZ4,
     S   BYZ5,      BYZ6,      BYZ7,      BYZ8,
     T   BZY1,      BZY2,      BZY3,      BZY4,
     U   BZY5,      BZY6,      BZY7,      BZY8,
     V   VX0(1,1),  VX0(1,2),  VX0(1,3),  VX0(1,4),
     W   VX0(1,5),  VX0(1,6),  VX0(1,7),  VX0(1,8),
     X   VY0(1,1),  VY0(1,2),  VY0(1,3),  VY0(1,4),
     Y   VY0(1,5),  VY0(1,6),  VY0(1,7),  VY0(1,8),
     Z   VZ0(1,1),  VZ0(1,2),  VZ0(1,3),  VZ0(1,4),
     1   VZ0(1,5),  VZ0(1,6),  VZ0(1,7),  VZ0(1,8),
     2   MFXX(1,IP),MFXY(1,IP),MFXZ(1,IP),MFYX(1,IP),
     3   MFYY(1,IP),MFYZ(1,IP),MFZX(1,IP),MFZY(1,IP),
     4   MFZZ(1,IP),ICP,       DETF0,     JFAC(1,IP),
     5   NU,        IDEG,      NEL)
C    
         IF (JCVT == 2.OR.ISMSTR==10.OR.ISMSTR==12) THEN
          CALL SORDEFT3(LFT,LLT,MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP),
     .         MFYX(1,IP), MFYY(1,IP), MFYZ(1,IP),
     .         MFZX(1,IP), MFZY(1,IP), MFZZ(1,IP),
     .         G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z)
         ENDIF
           ENDDO  ! IR=1,NPTR
          ENDDO   ! IS=1,NPTS
         ENDDO    ! IT=1,NPTT
      ENDIF       ! ISMSTR == 10
c---------------------------      
      IF (JCVT == 2) THEN
       CALL STORTH3(
     1   LFT,      LLT,      NEL,      G1X,
     2   G1Y,      G1Z,      G2X,      G2Y,
     3   G2Z,      G3X,      G3Y,      G3Z,
     4   GBUF%GAMA,ISORTH)
      ENDIF
      IF (ISMSTR/=11) THEN
       CALL S8ZDERIC3(
     1   OFFL,      VOLG,      NGL,       XD1,
     2   XD2,       XD3,       XD4,       XD5,
     3   XD6,       XD7,       XD8,       YD1,
     4   YD2,       YD3,       YD4,       YD5,
     5   YD6,       YD7,       YD8,       ZD1,
     6   ZD2,       ZD3,       ZD4,       ZD5,
     7   ZD6,       ZD7,       ZD8,       PXC1,
     8   PXC2,      PXC3,      PXC4,      PYC1,
     9   PYC2,      PYC3,      PYC4,      PZC1,
     A   PZC2,      PZC3,      PZC4,      PX1H1,
     B   PX1H2,     PX1H3,     PX1H4,     PX2H1,
     C   PX2H2,     PX2H3,     PX2H4,     PX3H1,
     D   PX3H2,     PX3H3,     PX3H4,     PX4H1,
     E   PX4H2,     PX4H3,     PX4H4,     HX,
     F   HY,        HZ,        AJC1,      AJC2,
     G   AJC3,      AJC4,      AJC5,      AJC6,
     H   AJC7,      AJC8,      AJC9,      SMAX,
     I   GBUF%SMSTR,GBUF%OFF,  NEL,       ISMSTR,
     J   JLAG)
       IF (IDTS6>0) THEN
        CALL SDLEN_DEGE(
     1   VOLG,      DELTAX,    X1,        X2,
     2   X3,        X4,        X5,        X6,
     3   X7,        X8,        Y1,        Y2,
     4   Y3,        Y4,        Y5,        Y6,
     5   Y7,        Y8,        Z1,        Z2,
     6   Z3,        Z4,        Z5,        Z6,
     7   Z7,        Z8,        IXS(1,NF1),IDEG,
     8   NEL)
       END IF
      END IF !(ISMSTR/=11) THEN
C----------JACOBIEN-inverse- in case of Vol<=0-------
      NNEGA = 0
C-----------Begin integrating points-----
c
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
          IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C----------------
          ZR = A_GAUSS(IR,NPTR)
          ZS = A_GAUSS(IS,NPTS)
          ZT = A_GAUSS(IT,NPTT)
          WT = W_GAUSS(IT,NPTT)
          WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C--------[MF] is used as work array for AJI (inversed Jacobian)
          CALL S8ZDERIPR3(
     1   GBUF%OFF,  VOLP(1,IP),NGL,       ZR,
     2   ZS,        ZT,        WI,        AJC1,
     3   AJC2,      AJC3,      AJC4,      AJC5,
     4   AJC6,      AJC7,      AJC8,      AJC9,
     5   HX,        HY,        HZ,        AJ1,
     6   AJ2,       AJ3,       AJ4,       AJ5,
     7   AJ6,       AJI1(1,IP),AJI2(1,IP),AJI3(1,IP),
     8   AJI4(1,IP),AJI5(1,IP),AJI6(1,IP),AJI7(1,IP),
     9   AJI8(1,IP),AJI9(1,IP),NNEGA,     INDEX,
     A   IP,        NEL)
        ENDDO  ! IT=1,NPTT
       ENDDO   ! IS=1,NPTS
      ENDDO    ! IR=1,NPTR
C
      IF (NNEGA>0) THEN
       CALL S8ZDERICM3(
     1   VOLG,      NGL,       XD1,       XD2,
     2   XD3,       XD4,       XD5,       XD6,
     3   XD7,       XD8,       YD1,       YD2,
     4   YD3,       YD4,       YD5,       YD6,
     5   YD7,       YD8,       ZD1,       ZD2,
     6   ZD3,       ZD4,       ZD5,       ZD6,
     7   ZD7,       ZD8,       PXC1,      PXC2,
     8   PXC3,      PXC4,      PYC1,      PYC2,
     9   PYC3,      PYC4,      PZC1,      PZC2,
     A   PZC3,      PZC4,      PX1H1,     PX1H2,
     B   PX1H3,     PX1H4,     PX2H1,     PX2H2,
     C   PX2H3,     PX2H4,     PX3H1,     PX3H2,
     D   PX3H3,     PX3H4,     PX4H1,     PX4H2,
     E   PX4H3,     PX4H4,     HX,        HY,
     F   HZ,        AJC1,      AJC2,      AJC3,
     G   AJC4,      AJC5,      AJC6,      AJC7,
     H   AJC8,      AJC9,      SMAX,      GBUF%SMSTR,
     I   GBUF%OFF,  NNEGA,     INDEX,     NEL,
     J   ISMSTR)
       DO IR=1,NPTR
        DO IS=1,NPTS
         DO IT=1,NPTT
          IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C----------------
          ZR = A_GAUSS(IR,NPTR)
          ZS = A_GAUSS(IS,NPTS)
          ZT = A_GAUSS(IT,NPTT)
          WT = W_GAUSS(IT,NPTT)
          WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C
          CALL S8ZDERIMS3(VOLP(1,IP),
     .    ZR,ZS,ZT,WI,
     .    HX,   HY,   HZ,   
     .    AJC1,AJC2,AJC3,
     .    AJC4,AJC5,AJC6,
     .    AJC7,AJC8,AJC9,
     .    AJ1,AJ2,AJ3,
     .    AJ4,AJ5,AJ6,
     .    AJ7,AJ8,AJ9,
     .    AJI1(1,IP),AJI2(1,IP),AJI3(1,IP),
     .    AJI4(1,IP),AJI5(1,IP),AJI6(1,IP),
     .    AJI7(1,IP),AJI8(1,IP),AJI9(1,IP),NNEGA,INDEX)
         ENDDO  ! IT=1,NPTT
        ENDDO   ! IS=1,NPTS
       ENDDO    ! IR=1,NPTR
       IF (INEG_V ==0) THEN
C---     /NEGAVOL/STOP       
           CALL ANCMSG(MSGID=280,ANMODE=ANINFO)
           MSTOP = 1
       END IF !(INEG_V /=0) THEN
      END IF
C --------------------------
C  --- UPDATE REF CONFIGURATION (possible future change to small strain option)
C  --- ! Total strain option doesn't change the Ref CONF.
C --------------------------
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
      IF (ICP == 2) THEN
        CALL S8ZSIGP3(LFT  ,LLT       ,GBUF%SIG,E0  ,GBUF%PLA,
     2                FAC  ,GBUF%G_PLA,NEL     )
        DO I=LFT,LLT
          NU1(I)=NU(I)+(HALF-NU(I))*FAC(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          NU1(I)=NU(I)
        ENDDO
      ENDIF 
C
C-----don't do it w/ degenerated elm    
       IF (ISMSTR <10.AND.ICP==1) THEN
        IF (IDTS6==0)  CALL DEGENES8(
     1   IXS(1,NF1),IDEG,      NEL)
         DO I=LFT,LLT
          IF (IDEG(I)>0) IDEG(I) = IDEG(I) + 10
         ENDDO
       END IF !(ICP==1) THEN
C---------------------------------------
C INITIALIZATION---Before increment-----
C---------------------------------------
      CALL S8ZZERO3(
     1   F11,        F21,        F31,        F12,
     2   F22,        F32,        F13,        F23,
     3   F33,        F14,        F24,        F34,
     4   F15,        F25,        F35,        F16,
     5   F26,        F36,        F17,        F27,
     6   F37,        F18,        F28,        F38,
     7   GBUF%SIG,   GBUF%EINT,  GBUF%RHO,   GBUF%QVIS,
     8   GBUF%PLA,   GBUF%EPSD,  STIN,       PP,
     9   GBUF%G_PLA, GBUF%G_EPSD,IEXPAN,     GBUF%EINTTH,
     A   NEL,        CONDEN)
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
      IOFFS=0
      OFFS(LFT:LLT)=EP20
      IF(JTHE<0) THEM(LFT:LLT,1:8) = ZERO
c-------------------------------------------
c    COMPUTE Regularized non local variable in Gauss point
c-------------------------------------------
      IF (INLOC > 0) THEN
        L_NLOC = NLOC_DMG%L_NLOC
        DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        DO IR=1,NPTR
          DO IS=1,NPTS
            DO IT=1,NPTT
              ZR = A_GAUSS(IR,NPTR)
              ZS = A_GAUSS(IS,NPTS) 
              ZT = A_GAUSS(IT,NPTT)
              IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
              CALL BASIS8 (ZR,ZS,ZT,H,PR,PS,PT)
              DO I=LFT,LLT
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
                VAR_REG(I,IP) = H(1)*DNL(IPOS(1)) + H(2)*DNL(IPOS(2)) + H(3)*DNL(IPOS(3))
     .                        + H(4)*DNL(IPOS(4)) + H(5)*DNL(IPOS(5)) + H(6)*DNL(IPOS(6))
     .                        + H(7)*DNL(IPOS(7)) + H(8)*DNL(IPOS(8))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
c-------------------------------------------
C-----------Matrix B----------
C-----------Begin integrating points-----
c
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
          IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)

C-----avoid multi-print    
          IF (IOFFS == 1)THEN
           DO I=LFT,LLT
            IF (OFFS(I)<=TWO) LBUF%OFF(I)=OFFS(I)
           ENDDO
          END IF
C-----------
c         IF (IFAILURE > 0)  ILAY = IP
C----------------
           ZR = A_GAUSS(IR,NPTR)
           ZS = A_GAUSS(IS,NPTS)
          ZT = A_GAUSS(IT,NPTT)
          WT = W_GAUSS(IT,NPTT)
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C--------[MF] is used as work array for AJI (inversed Jacobian)
         CALL S8ZDERIM3(
     1   OFFL,      OFF,       VOLN,      NGL,
     2   ZR,        ZS,        ZT,        WI,
     3   PXC1,      PXC2,      PXC3,      PXC4,
     4   PYC1,      PYC2,      PYC3,      PYC4,
     5   PZC1,      PZC2,      PZC3,      PZC4,
     6   PX1H1,     PX1H2,     PX1H3,     PX1H4,
     7   PX2H1,     PX2H2,     PX2H3,     PX2H4,
     8   PX3H1,     PX3H2,     PX3H3,     PX3H4,
     9   PX4H1,     PX4H2,     PX4H3,     PX4H4,
     A   PX1,       PX2,       PX3,       PX4,
     B   PX5,       PX6,       PX7,       PX8,
     C   PY1,       PY2,       PY3,       PY4,
     D   PY5,       PY6,       PY7,       PY8,
     E   PZ1,       PZ2,       PZ3,       PZ4,
     F   PZ5,       PZ6,       PZ7,       PZ8,
     G   PXY1,      PXY2,      PXY3,      PXY4,
     H   PXY5,      PXY6,      PXY7,      PXY8,
     I   PYX1,      PYX2,      PYX3,      PYX4,
     J   PYX5,      PYX6,      PYX7,      PYX8,
     K   PXZ1,      PXZ2,      PXZ3,      PXZ4,
     L   PXZ5,      PXZ6,      PXZ7,      PXZ8,
     M   PZX1,      PZX2,      PZX3,      PZX4,
     N   PZX5,      PZX6,      PZX7,      PZX8,
     O   PYZ1,      PYZ2,      PYZ3,      PYZ4,
     P   PYZ5,      PYZ6,      PYZ7,      PYZ8,
     Q   PZY1,      PZY2,      PZY3,      PZY4,
     R   PZY5,      PZY6,      PZY7,      PZY8,
     S   BXY1,      BXY2,      BXY3,      BXY4,
     T   BXY5,      BXY6,      BXY7,      BXY8,
     U   BYX1,      BYX2,      BYX3,      BYX4,
     V   BYX5,      BYX6,      BYX7,      BYX8,
     W   BXZ1,      BXZ2,      BXZ3,      BXZ4,
     X   BXZ5,      BXZ6,      BXZ7,      BXZ8,
     Y   BZX1,      BZX2,      BZX3,      BZX4,
     Z   BZX5,      BZX6,      BZX7,      BZX8,
     1   BYZ1,      BYZ2,      BYZ3,      BYZ4,
     2   BYZ5,      BYZ6,      BYZ7,      BYZ8,
     3   BZY1,      BZY2,      BZY3,      BZY4,
     4   BZY5,      BZY6,      BZY7,      BZY8,
     5   AJC1,      AJC2,      AJC3,      AJC4,
     6   AJC5,      AJC6,      AJC7,      AJC8,
     7   AJC9,      HX,        HY,        HZ,
     8   AJ1,       AJ2,       AJ3,       AJ4,
     9   AJ5,       AJ6,       AJI1(1,IP),AJI2(1,IP),
     A   AJI3(1,IP),AJI4(1,IP),AJI5(1,IP),AJI6(1,IP),
     B   AJI7(1,IP),AJI8(1,IP),AJI9(1,IP),SMAX,
     C   DELTAX,    NU1,       ICP,       IDEG,
     D   VOLP(1,IP),NEL,       MTN,       ISMSTR,
     E   JHBE)
C
           CALL S8ZDEFO3(
     1   PXC1,       PXC2,       PXC3,       PXC4,
     2   PYC1,       PYC2,       PYC3,       PYC4,
     3   PZC1,       PZC2,       PZC3,       PZC4,
     4   PX1,        PX2,        PX3,        PX4,
     5   PX5,        PX6,        PX7,        PX8,
     6   PY1,        PY2,        PY3,        PY4,
     7   PY5,        PY6,        PY7,        PY8,
     8   PZ1,        PZ2,        PZ3,        PZ4,
     9   PZ5,        PZ6,        PZ7,        PZ8,
     A   PXY1,       PXY2,       PXY3,       PXY4,
     B   PXY5,       PXY6,       PXY7,       PXY8,
     C   PYX1,       PYX2,       PYX3,       PYX4,
     D   PYX5,       PYX6,       PYX7,       PYX8,
     E   PXZ1,       PXZ2,       PXZ3,       PXZ4,
     F   PXZ5,       PXZ6,       PXZ7,       PXZ8,
     G   PZX1,       PZX2,       PZX3,       PZX4,
     H   PZX5,       PZX6,       PZX7,       PZX8,
     I   PYZ1,       PYZ2,       PYZ3,       PYZ4,
     J   PYZ5,       PYZ6,       PYZ7,       PYZ8,
     K   PZY1,       PZY2,       PZY3,       PZY4,
     L   PZY5,       PZY6,       PZY7,       PZY8,
     M   BXY1,       BXY2,       BXY3,       BXY4,
     N   BXY5,       BXY6,       BXY7,       BXY8,
     O   BYX1,       BYX2,       BYX3,       BYX4,
     P   BYX5,       BYX6,       BYX7,       BYX8,
     Q   BXZ1,       BXZ2,       BXZ3,       BXZ4,
     R   BXZ5,       BXZ6,       BXZ7,       BXZ8,
     S   BZX1,       BZX2,       BZX3,       BZX4,
     T   BZX5,       BZX6,       BZX7,       BZX8,
     U   BYZ1,       BYZ2,       BYZ3,       BYZ4,
     V   BYZ5,       BYZ6,       BYZ7,       BYZ8,
     W   BZY1,       BZY2,       BZY3,       BZY4,
     X   BZY5,       BZY6,       BZY7,       BZY8,
     Y   VX1,        VX2,        VX3,        VX4,
     Z   VX5,        VX6,        VX7,        VX8,
     1   VY1,        VY2,        VY3,        VY4,
     2   VY5,        VY6,        VY7,        VY8,
     3   VZ1,        VZ2,        VZ3,        VZ4,
     4   VZ5,        VZ6,        VZ7,        VZ8,
     5   DXX,        DXY,        DXZ,        DYX,
     6   DYY,        DYZ,        DZX,        DZY,
     7   DZZ,        D4,         D5,         D6,
     8   WXX,        WYY,        WZZ,        LBUF%VOL,
     9   OFF,        LBUF%EINT,  GBUF%OFF,   DSV,
     A   ICP,        ICR,        ICS,        ICT,
     B   DTI,        SMR(1,IP),  SMS(1,IP),  SMT(1,IP),
     C   NGL,        IDEG,       LBUF%VOL0DP,NEL,
     D   ISMSTR,     JCVT)
C
        MX = MXT(LFT)
        DO I=LFT,LLT
          RHOO(I) = LBUF%RHO(I)
          TEMPEL(I) = PM(79,MX)
        ENDDO
C
        IF (JCVT == 2) THEN
          CALL SZORDEF3(LFT,LLT,DXX,DYY,DZZ,D4,D5,D6,
     .                  G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z)
        ENDIF
         DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))+DSV(1:NEL)   
        CALL SRHO3(
     1   PM,         LBUF%VOL,   LBUF%RHO,   LBUF%EINT,
     2   DIVDE,      FLUX(1,NF1),FLU1(NF1),  VOLN,
     3   DVOL,       NGL,        MXT,        OFF,
     4   0,          GBUF%TAG22, VOLP(1,IP), LBUF%VOL0DP,
     5   AMU,        GBUF%OFF,   NEL,        MTN,
     6   JALE,       ISMSTR,     JEUL,       JLAG)
C
C-----------------------------
        IF (JCVT == 0) THEN
          CALL SROTA3(
     1   LBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
      ELSE
C-----
C      EXTRACT STRESSES + SMALL STRAIN
          CALL CSMALL3(LBUF%SIG,S1,S2,S3,S4,S5,S6,
     .                 GBUF%OFF,OFF,NEL)
      ENDIF
C-----------------------------
C
C    for heat transfert
C
      IF(JTHE < 0 ) THEN
       CALL S8ZTEMPEL(LFT,LLT,NI,
     1               ZR,ZS,ZT,
     2               NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     3               TEMP,TEMPEL)
      ENDIF    
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C     TIME STEP EST CALCULE ICI DT2T
C------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
C
      IF (IBOLTP /= 0) 
     .      CALL BOLTST(IP    ,BPRELD   ,LBUF%SIG ,TT  ,NEL   ,        
     .                  NPT   ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB)
C
      CALL MMAIN(
     1   ELBUF_TAB,    NG,           PM,           GEO,
     2   FV,           ALE_CONNECT,  IXS,          IPARG,
     3   V,            TF,           NPF,          BUFMAT,
     4   STI,          X,            DT2T,         NELTST,
     5   ITYPTST,      OFFSET,       NEL,          W,
     6   OFF,          NGEO,         MXT,          NGL,
     7   VOLN,         VD2,          DVOL,         DELTAX,
     8   VIS,          QVIS,         CXX,          S1,
     9   S2,           S3,           S4,           S5,
     A   S6,           DXX,          DYY,          DZZ,
     B   D4,           D5,           D6,           WXX,
     C   WYY,          WZZ,          AJ1,          AJ2,
     D   AJ3,          AJ4,          AJ5,          AJ6,
     E   VDX,          VDY,          VDZ,          MUVOID,
     F   SSP_EQ,       AIRE,         SIGY,         ET,
     G   R1_FREE,      LBUF%PLA,     R3_FREE,      AMU,
     H   MFXX(1,IP),   MFXY(1,IP),   MFXZ(1,IP),   MFYX(1,IP),
     I   MFYY(1,IP),   MFYZ(1,IP),   MFZX(1,IP),   MFZY(1,IP),
     J   MFZZ(1,IP),   IPM,          GAMA,         BID,
     K   BID,          BID,          BID,          BID,
     L   BID,          BID,          ISTRAIN,      TEMPEL,
     M   DIE,          IEXPAN,       ILAY,         MSSA,
     N   DMELS,        IR,           IS,           IT,
     O   TABLE,        BID,          BID,          BID,
     P   BID,          IPARG(1,NG),  IGEO,         CONDE,
     Q   ITASK,        NLOC_DMG,     VAR_REG(1,IP),MAT_ELEM,
     R   H3D_STRAIN,   JPLASOL,      JSPH)
C    
        IF (ISTRAIN == 1) THEN 
          CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
      ENDIF
C
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C
        IF (JCVT == 2) THEN
          CALL SROTO3(LFT,LLT,LBUF%SIG,SIGN,
     .            G1X, G2X, G3X, G1Y, G2Y, G3Y, G1Z, G2Z, G3Z,NEL)
        CALL S8ZFINT3(
     1   SIGN,       SIGN,       PX1,        PX2,
     2   PX3,        PX4,        PY1,        PY2,
     3   PY3,        PY4,        PZ1,        PZ2,
     4   PZ3,        PZ4,        PX5,        PX6,
     5   PX7,        PX8,        PY5,        PY6,
     6   PY7,        PY8,        PZ5,        PZ6,
     7   PZ7,        PZ8,        PXY1,       PXY2,
     8   PXY3,       PXY4,       PXY5,       PXY6,
     9   PXY7,       PXY8,       PYX1,       PYX2,
     A   PYX3,       PYX4,       PYX5,       PYX6,
     B   PYX7,       PYX8,       PXZ1,       PXZ2,
     C   PXZ3,       PXZ4,       PXZ5,       PXZ6,
     D   PXZ7,       PXZ8,       PZX1,       PZX2,
     E   PZX3,       PZX4,       PZX5,       PZX6,
     F   PZX7,       PZX8,       PYZ1,       PYZ2,
     G   PYZ3,       PYZ4,       PYZ5,       PYZ6,
     H   PYZ7,       PYZ8,       PZY1,       PZY2,
     I   PZY3,       PZY4,       PZY5,       PZY6,
     J   PZY7,       PZY8,       BXY1,       BXY2,
     K   BXY3,       BXY4,       BXY5,       BXY6,
     L   BXY7,       BXY8,       BYX1,       BYX2,
     M   BYX3,       BYX4,       BYX5,       BYX6,
     N   BYX7,       BYX8,       BXZ1,       BXZ2,
     O   BXZ3,       BXZ4,       BXZ5,       BXZ6,
     P   BXZ7,       BXZ8,       BZX1,       BZX2,
     Q   BZX3,       BZX4,       BZX5,       BZX6,
     R   BZX7,       BZX8,       BYZ1,       BYZ2,
     S   BYZ3,       BYZ4,       BYZ5,       BYZ6,
     T   BYZ7,       BYZ8,       BZY1,       BZY2,
     U   BZY3,       BZY4,       BZY5,       BZY6,
     V   BZY7,       BZY8,       F11,        F21,
     W   F31,        F12,        F22,        F32,
     X   F13,        F23,        F33,        F14,
     Y   F24,        F34,        F15,        F25,
     Z   F35,        F16,        F26,        F36,
     1   F17,        F27,        F37,        F18,
     2   F28,        F38,        VOLN,       QVIS,
     3   PP,         LBUF%EINT,  LBUF%RHO,   LBUF%QVIS,
     4   LBUF%PLA,   LBUF%EPSD,  GBUF%EPSD,  GBUF%SIG,
     5   GBUF%EINT,  GBUF%RHO,   GBUF%QVIS,  GBUF%PLA,
     6   VOLG,       STI,        STIN,       ICP,
     7   OFF,        LBUF%VOL,   GBUF%VOL,   GBUF%G_PLA,
     8   GBUF%G_EPSD,JFAC(1,IP), LBUF%EINTTH,GBUF%EINTTH,
     9   IEXPAN,     NEL,        IDEG,       CONDE,
     A   CONDEN,     MTN,        ISMSTR)
       ELSE
C----------------------------
C     INTERNAL FORCES
C----------------------------
          CALL S8ZFINT3(
     1   LBUF%SIG,   LBUF%SIG,   PX1,        PX2,
     2   PX3,        PX4,        PY1,        PY2,
     3   PY3,        PY4,        PZ1,        PZ2,
     4   PZ3,        PZ4,        PX5,        PX6,
     5   PX7,        PX8,        PY5,        PY6,
     6   PY7,        PY8,        PZ5,        PZ6,
     7   PZ7,        PZ8,        PXY1,       PXY2,
     8   PXY3,       PXY4,       PXY5,       PXY6,
     9   PXY7,       PXY8,       PYX1,       PYX2,
     A   PYX3,       PYX4,       PYX5,       PYX6,
     B   PYX7,       PYX8,       PXZ1,       PXZ2,
     C   PXZ3,       PXZ4,       PXZ5,       PXZ6,
     D   PXZ7,       PXZ8,       PZX1,       PZX2,
     E   PZX3,       PZX4,       PZX5,       PZX6,
     F   PZX7,       PZX8,       PYZ1,       PYZ2,
     G   PYZ3,       PYZ4,       PYZ5,       PYZ6,
     H   PYZ7,       PYZ8,       PZY1,       PZY2,
     I   PZY3,       PZY4,       PZY5,       PZY6,
     J   PZY7,       PZY8,       BXY1,       BXY2,
     K   BXY3,       BXY4,       BXY5,       BXY6,
     L   BXY7,       BXY8,       BYX1,       BYX2,
     M   BYX3,       BYX4,       BYX5,       BYX6,
     N   BYX7,       BYX8,       BXZ1,       BXZ2,
     O   BXZ3,       BXZ4,       BXZ5,       BXZ6,
     P   BXZ7,       BXZ8,       BZX1,       BZX2,
     Q   BZX3,       BZX4,       BZX5,       BZX6,
     R   BZX7,       BZX8,       BYZ1,       BYZ2,
     S   BYZ3,       BYZ4,       BYZ5,       BYZ6,
     T   BYZ7,       BYZ8,       BZY1,       BZY2,
     U   BZY3,       BZY4,       BZY5,       BZY6,
     V   BZY7,       BZY8,       F11,        F21,
     W   F31,        F12,        F22,        F32,
     X   F13,        F23,        F33,        F14,
     Y   F24,        F34,        F15,        F25,
     Z   F35,        F16,        F26,        F36,
     1   F17,        F27,        F37,        F18,
     2   F28,        F38,        VOLN,       QVIS,
     3   PP,         LBUF%EINT,  LBUF%RHO,   LBUF%QVIS,
     4   LBUF%PLA,   LBUF%EPSD,  GBUF%EPSD,  GBUF%SIG,
     5   GBUF%EINT,  GBUF%RHO,   GBUF%QVIS,  GBUF%PLA,
     6   VOLG,       STI,        STIN,       ICP,
     7   OFF,        LBUF%VOL,   GBUF%VOL,   GBUF%G_PLA,
     8   GBUF%G_EPSD,JFAC(1,IP), LBUF%EINTTH,GBUF%EINTTH,
     9   IEXPAN,     NEL,        IDEG,       CONDE,
     A   CONDEN,     MTN,        ISMSTR)
      ENDIF
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
      IF (INLOC > 0) THEN  
        IMAT = MXT(LFT)  
        CALL BASIS8 (ZR,ZS,ZT,H,PR,PS,PT)
        CALL S8ZFINT_REG(
     1   NLOC_DMG,     VAR_REG(1,IP),NEL,          GBUF%OFF,
     2   VOLN,         NC1,          NC2,          NC3,
     3   NC4,          NC5,          NC6,          NC7,
     4   NC8,          PX1,          PX2,          PX3,
     5   PX4,          PX5,          PX6,          PX7,
     6   PX8,          PY1,          PY2,          PY3,
     7   PY4,          PY5,          PY6,          PY7,
     8   PY8,          PZ1,          PZ2,          PZ3,
     9   PZ4,          PZ5,          PZ6,          PZ7,
     A   PZ8,          IMAT,         H,            WI,
     B   IP,           ITASK,        DT2T,         GBUF%VOL,
     C   NFT)
      ENDIF       
c
       DO I=LFT,LLT
        OFFL(I)=MIN(OFFL(I),OFF(I))
          IF (LBUF%OFF(I) > ONE .AND. GBUF%OFF(I) == ONE) THEN  
            OFFS(I)=MIN(LBUF%OFF(I),OFFS(I))                 
          IOFFS  =1
        END IF
       ENDDO
c-------------------------
c    finite element heat transfert  
c-------------------------
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
        ENDDO  ! IT=1,NPTT
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
         DO IT=1,NPTT
            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)     
          DO I=LFT,LLT
              IF (GBUF%OFF(I) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
          END DO
         END DO 
        END DO 
       END DO
      END IF
c----------------------      
      IF (ICP == 1.AND.ISMSTR/=10.AND.ISMSTR/=12) THEN
       CALL S8ZFINTP3(
     1   PXC1,    PXC2,    PXC3,    PXC4,
     2   PYC1,    PYC2,    PYC3,    PYC4,
     3   PZC1,    PZC2,    PZC3,    PZC4,
     4   F11,     F21,     F31,     F12,
     5   F22,     F32,     F13,     F23,
     6   F33,     F14,     F24,     F34,
     7   F15,     F25,     F35,     F16,
     8   F26,     F36,     F17,     F27,
     9   F37,     F18,     F28,     F38,
     A   VOLG,    PP,      IDEG,    NEL)
      ENDIF
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
       CALL SGEODEL3(NGL,GBUF%OFF,VOLG,DELTAX,GBUF%VOL,GEO(1,NGEO(1)),L_MAX,DT,NEL,IDEL7NOK)
      END IF
C-----------------------------
C     SMALL STRAIN 
C-----------------------------
      CALL SMALLB3(GBUF%OFF,OFFL,NEL,ISMSTR)
      CALL SMALLGEO3(NGL, GBUF%OFF ,VOLG ,DELTAX, GBUF%VOL ,ITET,NEL,ISMSTR,DT)
      IF (ISMSTR==11.OR.(ISMSTR == 12.AND.IDTMIN(1)==3)) THEN
         CALL S8EDEFW3(
     1   PXC1,    PXC2,    PXC3,    PXC4,
     2   PYC1,    PYC2,    PYC3,    PYC4,
     3   PZC1,    PZC2,    PZC3,    PZC4,
     4   VX1,     VX2,     VX3,     VX4,
     5   VX5,     VX6,     VX7,     VX8,
     6   VY1,     VY2,     VY3,     VY4,
     7   VY5,     VY6,     VY7,     VY8,
     8   VZ1,     VZ2,     VZ3,     VZ4,
     9   VZ5,     VZ6,     VZ7,     VZ8,
     A   WXX0,    WYY0,    WZZ0,    NEL)
         CALL SMALLG3(
     1   GBUF%SMSTR,GBUF%OFF,  WXX0,      WYY0,
     2   WZZ0,      R11,       R12,       R13,
     3   R21,       R22,       R23,       R31,
     4   R32,       R33,       NEL,       ISMSTR,
     5   JCVT)
      END IF
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
       IOFFS =0
       DO I=LFT,LLT
        IF(GBUF%OFF(I)/=OFFG0(I).AND.ABS(GBUF%OFF(I)) > ONE ) IOFFS=1
       ENDDO
       IF (IOFFS == 1) THEN
        CALL S8SAV12(
     1   GBUF%OFF,  OFFG0,     GBUF%SMSTR,X,
     2   XDP,       NC1,       NC2,       NC3,
     3   NC4,       NC5,       NC6,       NC7,
     4   NC8,       NEL,       JCVT)
        IF (ISM12_11>0 .AND. ISORTH == 0) THEN
             CALL S8FUPD11T12(ELBUF_TAB(NG),GBUF%OFF,OFFG0  ,X  ,XDP  ,
     .              NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8, NEL)
        END IF 
         DO IR=1,NPTR
          DO IS=1,NPTS
           DO IT=1,NPTT
            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
            DO I=LFT,LLT
               IF (ABS(GBUF%OFF(I)) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
            END DO
           END DO 
          END DO 
         END DO
       END IF !(IOFFS == 1) THEN
      ENDIF
C--------------------------
C       BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
           IF (JCVT == 0) THEN
            IFVM22 = 0
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
     .       VGXA, VGYA, VGZA, VGA2, VOLG,IPARTS,
     .       GRESAV,GRTH,IGRTH,GBUF%OFF,IEXPAN,GBUF%EINTTH,
     .       GBUF%FILL,XGXA, XGYA, XGZA,
     .       XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
           ENDIF
      ENDIF
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      IF (JCVT/=0) THEN
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
          CALL SCUMU3(
     1   GBUF%OFF,A,       NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   NC7,     NC8,     STIFN,   STIN,
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
     H   THEM,    FTHE,    CONDN,   CONDEN,
     I   NEL,     JTHE,    ISROT,   IPARTSPH)
      ELSE
          CALL SCUMU3P(
     1   GBUF%OFF,STIN,    FSKY,    FSKY,
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
     H   THEM,    FTHESKY, CONDNSKY,CONDEN,
     I   NEL,     NFT,     JTHE,    ISROT,
     J   IPARTSPH)
      ENDIF
C-----------
      IF (ALLOCATED(VAR_REG)) DEALLOCATE(VAR_REG)
      RETURN
      END
