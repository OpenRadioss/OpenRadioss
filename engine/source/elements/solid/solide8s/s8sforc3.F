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
Chd|  S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BASIS8                        source/elements/solid/solide8/basis8.F
Chd|        CSMALL3                       source/elements/solid/solide/csmall3.F
Chd|        DEGENES8                      source/elements/solid/solide/degenes8.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S8EDEFOC3                     source/elements/solid/solide8e/s8edefoc3.F
Chd|        S8EDEFW3                      source/elements/solid/solide8e/s8edefw3.F
Chd|        S8EDERIC3                     source/elements/solid/solide8e/s8ederic3.F
Chd|        S8EDERICM3                    source/elements/solid/solide8e/s8edericm3.F
Chd|        S8EDERIG3                     source/elements/solid/solide8e/s8ederig3.F
Chd|        S8EDERIPR3                    source/elements/solid/solide8e/s8ederipr3.F
Chd|        S8EDERI_2                     source/elements/solid/solide8e/s8ederi_2.F
Chd|        S8EFMOY3                      source/elements/solid/solide8e/s8efmoy3.F
Chd|        S8EJACIP3                     source/elements/solid/solide8e/s8ejacip3.F
Chd|        S8EPRST_INI                   source/elements/solid/solide8e/s8eprst_ini.F
Chd|        S8SBDEFO3                     source/elements/solid/solide8s/s8sbdefo3.F
Chd|        S8SDERI3                      source/elements/solid/solide8s/s8sderi3.F
Chd|        S8SFINT3                      source/elements/solid/solide8s/s8sfint3.F
Chd|        S8SFINT3_CRIMP                source/elements/solid/solide8s/s8sfint3_crimp.F
Chd|        S8XREF_IMP                    source/elements/solid/solide8s/s8xref_imp.F
Chd|        S8ZDERIMS3                    source/elements/solid/solide8z/s8zderims3.F
Chd|        S8ZFINTP3                     source/elements/solid/solide8z/s8zfintp3.F
Chd|        S8ZFINT_REG                   source/elements/solid/solide8z/s8zfint_reg.F
Chd|        S8ZZERO3                      source/elements/solid/solide8z/s8zzero3.F
Chd|        SBILAN                        source/elements/solid/solide/sbilan.F
Chd|        SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3p.F
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen_dege.F
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SMALLG3                       source/elements/solid/solide/smallg3.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S8SFORC3(ELBUF_TAB,NG     ,
     1                    PM       ,GEO    ,IXS     ,X       ,
     2                    A        ,V      ,MS      ,W       ,FLUX    ,
     3                    FLU1     ,VEUL   ,FV      ,ALE_CONNECT   ,IPARG   ,
     4                    TF       ,NPF    ,BUFMAT  ,PARTSAV ,NLOC_DMG,
     5                    DT2T     ,NELTST ,ITYPTST ,STIFN   ,FSKY    ,
     6                    IADS     ,OFFSET ,EANI    ,IPARTS  ,ICP     ,
     7                    F11      ,F21    ,F31     ,F12     ,F22     ,
     8                    F32      ,F13    ,F23     ,F33     ,F14     ,
     9                    F24      ,F34    ,F15     ,F25     ,F35     ,
     A                    F16      ,F26    ,F36     ,F17     ,F27     ,
     B                    F37      ,F18    ,F28     ,F38     ,NEL     ,
     C                    NVC      ,IPM    ,ITASK   ,ISTRAIN ,
     D                    TEMP     ,FTHE   ,FTHESKY ,IEXPAN  ,GRESAV  ,
     E                    GRTH     ,IGRTH  ,MSSA    ,DMELS   ,TABLE   ,
     F                    IGEO     ,XDP    ,VOLN    ,CONDN   ,CONDNSKY,
     G                    D        ,IOUTPRT,MAT_ELEM,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "scr07_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG,NGROUP),NPF(*),
     .    IADS(8,*),IPARTS(*), IPM(NPROPMI,*),GRTH(*),IGRTH(*),IGEO(*),
     .    IOUTPRT
C          
      INTEGER NELTST,ITYPTST,OFFSET,NEL,ICP, 
     .        ICSIG, NVC   ,ITASK, ISTRAIN,  IEXPAN,NG,H3D_STRAIN

      DOUBLE PRECISION
     .        XDP(3,*)
      my_real
     .   DT2T
C
      my_real
     .   PM(NPROPM,*), GEO(*), X(*), A(*), V(*), MS(*), W(*), 
     .   FLUX(6,*),FLU1(*), VEUL(*), FV(*), TF(*), 
     .   PARTSAV(*),STIFN(*), FSKY(*),EANI(*),BUFMAT(*),
     .   F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),
     .   F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .   F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),
     .   F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),
     .   F15(MVSIZ),F25(MVSIZ),F35(MVSIZ),
     .   F16(MVSIZ),F26(MVSIZ),F36(MVSIZ),
     .   F17(MVSIZ),F27(MVSIZ),F37(MVSIZ),
     .   F18(MVSIZ),F28(MVSIZ),F38(MVSIZ),
     .   TEMP(*),FTHE(*),FTHESKY(*),GRESAV(*), MSSA(*), DMELS(*), VOLN(MVSIZ),
     .   CONDN(*),CONDNSKY(*),D(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LCO, NF1, IFLAG, I, 
     .     ILAY,IP,IR,IS,IT,MPT,NPTR,NPTS,NPTT,ICR,ICS,ICT,j
      INTEGER IBID,IBIDV(1), MX, IFVM22
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ)
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
     . AJP1(MVSIZ,8) , AJP2(MVSIZ,8) , AJP3(MVSIZ,8) ,
     . AJP4(MVSIZ,8) , AJP5(MVSIZ,8) , AJP6(MVSIZ,8) ,
     . AJP7(MVSIZ,8) , AJP8(MVSIZ,8) , AJP9(MVSIZ,8) ,
     . VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . E0(MVSIZ),C1,FAC(MVSIZ) ,PR(8,8),PS(8,8),PT(8,8)
C-----
C   Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ),STIN(MVSIZ),GAMA(MVSIZ,6),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ),
     .   WXX0(MVSIZ) , WYY0(MVSIZ) , WZZ0(MVSIZ),
     .   CONDE(MVSIZ), CONDEN(MVSIZ),DIVDE(MVSIZ)
C   Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)
      INTEGER IOFFS, N,ITET
      my_real
     .   OFFS(MVSIZ),DSV(MVSIZ),SDV(MVSIZ)
C-----
C   Variables utilisees dans les routines solides uniquement (en arguments).
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ),
     .        MAT(MVSIZ)
     
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8)
          
      my_real
     .   OFF(MVSIZ) ,OFFG(MVSIZ) , RHOO(MVSIZ),
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
     .  PXC1(MVSIZ),PXC2(MVSIZ),PXC3(MVSIZ),PXC4(MVSIZ),
     .  PYC1(MVSIZ),PYC2(MVSIZ),PYC3(MVSIZ),PYC4(MVSIZ),
     .  PZC1(MVSIZ),PZC2(MVSIZ),PZC3(MVSIZ),PZC4(MVSIZ),
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
     .  DZX(MVSIZ),DXZ(MVSIZ)
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      my_real
     .   WI,SMAX(MVSIZ),VOLG(MVSIZ),PP(MVSIZ),BID(MVSIZ)
      my_real
     .   SIGY(MVSIZ), ET(MVSIZ), NU(MVSIZ),NU1(MVSIZ),
     . R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      my_real
     .   VX0(MVSIZ,8),VY0(MVSIZ,8),VZ0(MVSIZ,8),
     .   TEMPEL(MVSIZ),THEM(MVSIZ,8),DIE(MVSIZ)
      INTEGER NNPT,IDEG(MVSIZ)
      PARAMETER (NNPT = 8)
      my_real
     .    MFXX(MVSIZ,NNPT),MFXY(MVSIZ,NNPT),MFYX(MVSIZ,NNPT),
     .    MFYY(MVSIZ,NNPT),MFYZ(MVSIZ,NNPT),MFZY(MVSIZ,NNPT),
     .    MFZZ(MVSIZ,NNPT),MFZX(MVSIZ,NNPT),MFXZ(MVSIZ,NNPT),
     .    BXX(MVSIZ,NNPT),BYY(MVSIZ,NNPT),BZZ(MVSIZ,NNPT),
     .    BXY(MVSIZ,NNPT),BYZ(MVSIZ,NNPT),BXZ(MVSIZ,NNPT),
     .    NI(8),DETF0(MVSIZ),JFAC(MVSIZ,NNPT)
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
c-----------------------------------------------------
      my_real
     .   BXY1(MVSIZ),BXY2(MVSIZ),BXY3(MVSIZ),BXY4(MVSIZ),
     .   BXY5(MVSIZ),BXY6(MVSIZ),BXY7(MVSIZ),BXY8(MVSIZ),
     .   BYX1(MVSIZ),BYX2(MVSIZ),BYX3(MVSIZ),BYX4(MVSIZ),
     .   BYX5(MVSIZ),BYX6(MVSIZ),BYX7(MVSIZ),BYX8(MVSIZ),
     .   BXZ1(MVSIZ),BXZ2(MVSIZ),BXZ3(MVSIZ),BXZ4(MVSIZ),
     .   BXZ5(MVSIZ),BXZ6(MVSIZ),BXZ7(MVSIZ),BXZ8(MVSIZ),
     .   BZX1(MVSIZ),BZX2(MVSIZ),BZX3(MVSIZ),BZX4(MVSIZ),
     .   BZX5(MVSIZ),BZX6(MVSIZ),BZX7(MVSIZ),BZX8(MVSIZ),
     .   BYZ1(MVSIZ),BYZ2(MVSIZ),BYZ3(MVSIZ),BYZ4(MVSIZ),
     .   BYZ5(MVSIZ),BYZ6(MVSIZ),BYZ7(MVSIZ),BYZ8(MVSIZ),
     .   BZY1(MVSIZ),BZY2(MVSIZ),BZY3(MVSIZ),BZY4(MVSIZ),
     .   BZY5(MVSIZ),BZY6(MVSIZ),BZY7(MVSIZ),BZY8(MVSIZ),
     .   BXX1(MVSIZ),BXX2(MVSIZ),BXX3(MVSIZ),BXX4(MVSIZ),
     .   BXX5(MVSIZ),BXX6(MVSIZ),BXX7(MVSIZ),BXX8(MVSIZ),
     .   BYY1(MVSIZ),BYY2(MVSIZ),BYY3(MVSIZ),BYY4(MVSIZ),
     .   BYY5(MVSIZ),BYY6(MVSIZ),BYY7(MVSIZ),BYY8(MVSIZ),
     .   BZZ1(MVSIZ),BZZ2(MVSIZ),BZZ3(MVSIZ),BZZ4(MVSIZ),
     .   BZZ5(MVSIZ),BZZ6(MVSIZ),BZZ7(MVSIZ),BZZ8(MVSIZ)
C
      INTEGER NNEGA,INDEX(MVSIZ),ISELECT,iel,ipr
      my_real
     .   PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .   PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .   PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .   PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .   PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .   PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .   PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .   PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .   PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .   PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .   PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
     . AJI1(MVSIZ,NNPT) , AJI2(MVSIZ,NNPT) , AJI3(MVSIZ,NNPT) ,
     . AJI4(MVSIZ,NNPT) , AJI5(MVSIZ,NNPT) , AJI6(MVSIZ,NNPT) ,
     . AJI7(MVSIZ,NNPT) , AJI8(MVSIZ,NNPT) , AJI9(MVSIZ,NNPT),
     .  PX1(MVSIZ,NNPT),PX2(MVSIZ,NNPT),PX3(MVSIZ,NNPT),PX4(MVSIZ,NNPT),
     .  PX5(MVSIZ,NNPT),PX6(MVSIZ,NNPT),PX7(MVSIZ,NNPT),PX8(MVSIZ,NNPT),
     .  PY1(MVSIZ,NNPT),PY2(MVSIZ,NNPT),PY3(MVSIZ,NNPT),PY4(MVSIZ,NNPT),
     .  PY5(MVSIZ,NNPT),PY6(MVSIZ,NNPT),PY7(MVSIZ,NNPT),PY8(MVSIZ,NNPT),
     .  PZ1(MVSIZ,NNPT),PZ2(MVSIZ,NNPT),PZ3(MVSIZ,NNPT),PZ4(MVSIZ,NNPT),
     .  PZ5(MVSIZ,NNPT),PZ6(MVSIZ,NNPT),PZ7(MVSIZ,NNPT),PZ8(MVSIZ,NNPT),
     .   P0XY1(MVSIZ,2),P0XY2(MVSIZ,2),P0XY3(MVSIZ,2),P0XY4(MVSIZ,2),
     .   P0XY5(MVSIZ,2),P0XY6(MVSIZ,2),P0XY7(MVSIZ,2),P0XY8(MVSIZ,2),
     .   P0YX1(MVSIZ,2),P0YX2(MVSIZ,2),P0YX3(MVSIZ,2),P0YX4(MVSIZ,2),
     .   P0YX5(MVSIZ,2),P0YX6(MVSIZ,2),P0YX7(MVSIZ,2),P0YX8(MVSIZ,2),
     .   P0XZ1(MVSIZ,2),P0XZ2(MVSIZ,2),P0XZ3(MVSIZ,2),P0XZ4(MVSIZ,2),
     .   P0XZ5(MVSIZ,2),P0XZ6(MVSIZ,2),P0XZ7(MVSIZ,2),P0XZ8(MVSIZ,2),
     .   P0ZX1(MVSIZ,2),P0ZX2(MVSIZ,2),P0ZX3(MVSIZ,2),P0ZX4(MVSIZ,2),
     .   P0ZX5(MVSIZ,2),P0ZX6(MVSIZ,2),P0ZX7(MVSIZ,2),P0ZX8(MVSIZ,2),
     .   P0YZ1(MVSIZ,2),P0YZ2(MVSIZ,2),P0YZ3(MVSIZ,2),P0YZ4(MVSIZ,2),
     .   P0YZ5(MVSIZ,2),P0YZ6(MVSIZ,2),P0YZ7(MVSIZ,2),P0YZ8(MVSIZ,2),
     .   P0ZY1(MVSIZ,2),P0ZY2(MVSIZ,2),P0ZY3(MVSIZ,2),P0ZY4(MVSIZ,2),
     .   P0ZY5(MVSIZ,2),P0ZY6(MVSIZ,2),P0ZY7(MVSIZ,2),P0ZY8(MVSIZ,2),
     .   PX1H1(MVSIZ), PX1H2(MVSIZ), PX1H3(MVSIZ), PX1H4(MVSIZ),  
     .   PX2H1(MVSIZ), PX2H2(MVSIZ), PX2H3(MVSIZ), PX2H4(MVSIZ),  
     .   PX3H1(MVSIZ), PX3H2(MVSIZ), PX3H3(MVSIZ), PX3H4(MVSIZ),  
     .   PX4H1(MVSIZ), PX4H2(MVSIZ), PX4H3(MVSIZ), PX4H4(MVSIZ),
     .   JR_1(MVSIZ),JS_1(MVSIZ),JT_1(MVSIZ), 
     .   BB(6,24,MVSIZ),AMU(MVSIZ)
      DOUBLE PRECISION
     .   VOLP(MVSIZ,NNPT)
C----- Variables utilisees pour le calcul de puissance iteree
C-----
C     Variables utilis  es pour le non-local
      my_real,
     .  DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      INTEGER :: INLOC, L_NLOC, INOD(8), IPOS(8), IMAT
      my_real,
     .  DIMENSION(:), POINTER :: DNL
      my_real 
     .  H(8),PS2(8),PR2(8),PT2(8), ZR,ZS,ZT
C
      DOUBLE PRECISION
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ),
     .   DN_X(MVSIZ,8),DN_Y(MVSIZ,8),DN_Z(MVSIZ,8) ,
     .   TRM(NEL,24,24),DN_R(8),DN_S(8),DN_T(8),INVJ(9,MVSIZ)
      my_real
     .   A11(MVSIZ), A12(MVSIZ), A13(MVSIZ), 
     .   A21(MVSIZ), A22(MVSIZ), A23(MVSIZ), 
     .   A31(MVSIZ), A32(MVSIZ), A33(MVSIZ)      
C
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
C
c     Flag Bolt Preloading
      INTEGER IBOLTP,NBPRELD
      my_real, 
     .  DIMENSION(:), POINTER :: BPRELD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      NPTR   = ELBUF_TAB(NG)%NPTR
      NPTS   = ELBUF_TAB(NG)%NPTS
      NPTT   = ELBUF_TAB(NG)%NPTT
      GBUF   => ELBUF_TAB(NG)%GBUF
C
      IBOLTP = IPARG(72,NG)
      INLOC   = IPARG(78,NG)
      ALLOCATE(VAR_REG(NEL,NPTR*NPTS*NPTT))
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
      TEMPEL(1:MVSIZ) = ZERO
c
      ILAY   = 1
      IBID = 0
      IBIDV  = 0
      NF1=NFT+1
C------------------------shear locking is alleviated with ANS for shear
      ISELECT=0
C-----------
C GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
C-----------
C GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
       CALL SRCOOR3_IMP(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   VD2,VIS,GBUF%OFF,OFFG,GBUF%SMSTR,GBUF%RHO,RHOO, GBUF%COR_FR,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     .   NGL,MXT,NGEO,IOUTPRT, VGXA, VGYA, VGZA, VGA2,
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .   XDP, X0 , Y0 , Z0 , NEL, TRM, GBUF%COR_XR,
     .   ULX1  ,ULX2  ,ULX3  ,ULX4  ,ULX5  ,ULX6  ,ULX7  ,ULX8  ,
     .   ULY1  ,ULY2  ,ULY3  ,ULY4  ,ULY5  ,ULY6  ,ULY7  ,ULY8  ,
     .   ULZ1  ,ULZ2  ,ULZ3  ,ULZ4  ,ULZ5  ,ULZ6  ,ULZ7  ,ULZ8  ,
     .   XGXA  ,XGYA  ,XGZA, XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,IPARG(1,NG))
c---  
C-----------
C GATHER NODAL VARIABLES FOR TOTAL STRAIN CASE.
C-----------
C-----------
      IOFFS=0
      DO I=1,NEL
        OFFS(I)=EP20
        DELTAX(I)=EP30
        IDEG(I)=0
      ENDDO
      IF(JTHE < 0) THEM(1:NEL,1:8) =ZERO

      CALL S8EPRST_INI(PR ,PS ,PT )
C-------now [B](Pij) is always in global system w/ ISMSTR10
C-----------------------------
C----------JACOBIEN-inverse- in case of Vol<=0-------
      IF (ISMSTR/=11) THEN
       CALL S8EDERIC3(
     1   OFFG,      VOLG,      NGL,       XD1,
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
     F   HY,        HZ,        JR_1,      JS_1,
     G   JT_1,      AJC1,      AJC2,      AJC3,
     H   AJC4,      AJC5,      AJC6,      AJC7,
     I   AJC8,      AJC9,      SMAX,      GBUF%SMSTR,
     J   GBUF%OFF,  NEL,       ISMSTR,    JLAG)
         CALL S8EJACIP3(
     1   HX,      HY,      HZ,      AJC1,
     2   AJC2,    AJC3,    AJC4,    AJC5,
     3   AJC6,    AJC7,    AJC8,    AJC9,
     4   AJP1,    AJP2,    AJP3,    AJP4,
     5   AJP5,    AJP6,    AJP7,    AJP8,
     6   AJP9,    NEL)
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
          WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
          CALL S8EDERIPR3(
     1   GBUF%OFF,  VOLP(1,IP),NGL,       WI,
     2   AJP1(1,IP),AJP2(1,IP),AJP3(1,IP),AJP4(1,IP),
     3   AJP5(1,IP),AJP6(1,IP),AJP7(1,IP),AJP8(1,IP),
     4   AJP9(1,IP),AJI1(1,IP),AJI2(1,IP),AJI3(1,IP),
     5   AJI4(1,IP),AJI5(1,IP),AJI6(1,IP),AJI7(1,IP),
     6   AJI8(1,IP),AJI9(1,IP),NNEGA,     INDEX,
     7   IP,        NEL)
        ENDDO  ! IT=1,NPTT
       ENDDO   ! IS=1,NPTS
      ENDDO    ! IR=1,NPTR
C
      IF (NNEGA>0) THEN
       CALL S8EDERICM3(
     1   VOLG,      NGL,       XD1,       XD2,
     2   XD3,       XD4,       XD5,       XD6,
     3   XD7,       XD8,       YD1,       YD2,
     4   YD3,       YD4,       YD5,       YD6,
     5   YD7,       YD8,       ZD1,       ZD2,
     6   ZD3,       ZD4,       ZD5,       ZD6,
     7   ZD7,       ZD8,       PXC1,      PXC2,
     8   PXC3,      PXC4,      PYC1,      PYC2,
     9   PYC3,      PYC4,      PZC1,      PZC2,
     A   PZC3,      PZC4,      HX,        HY,
     B   HZ,        AJC1,      AJC2,      AJC3,
     C   AJC4,      AJC5,      AJC6,      AJC7,
     D   AJC8,      AJC9,      SMAX,      GBUF%SMSTR,
     E   GBUF%OFF,  NNEGA,     INDEX,     NEL,
     F   ISMSTR)
       DO IR=1,NPTR
        DO IS=1,NPTS
         DO IT=1,NPTT
          IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
          WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
          CALL S8ZDERIMS3(VOLP(1,IP),
     .    A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,
     .    HX,   HY,   HZ,   
     .    AJC1,AJC2,AJC3,
     .    AJC4,AJC5,AJC6,
     .    AJC7,AJC8,AJC9,
     .    AJP1(1,IP),AJP2(1,IP),AJP3(1,IP),
     .    AJP4(1,IP),AJP5(1,IP),AJP6(1,IP),
     .    AJP7(1,IP),AJP8(1,IP),AJP9(1,IP),
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
C      
      IF (ICP==1.OR.ICP==2) THEN
       CALL S8EDEFOC3(
     1   PXC1,    PXC2,    PXC3,    PXC4,
     2   PYC1,    PYC2,    PYC3,    PYC4,
     3   PZC1,    PZC2,    PZC3,    PZC4,
     4   VX1,     VX2,     VX3,     VX4,
     5   VX5,     VX6,     VX7,     VX8,
     6   VY1,     VY2,     VY3,     VY4,
     7   VY5,     VY6,     VY7,     VY8,
     8   VZ1,     VZ2,     VZ3,     VZ4,
     9   VZ5,     VZ6,     VZ7,     VZ8,
     A   DSV,     NEL)
C-----don't do it w/ degenerated elm, but limit to law28 after QA   
        IF (IDTS6==0)  CALL DEGENES8(
     1   IXS(1,NF1),IDEG,      NEL)
        IF (ICP==1.AND.MTN==28) THEN
         DO I=1,NEL
          IF (IDEG(I)>0) IDEG(I) = IDEG(I) + 10
         ENDDO
        END IF !(ICP==1.AND.MTN==28) THEN
       ENDIF 
C-----------
C INITIALIZATION---Before increment-----
C-----------
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
C    COMPUTE AVERAGE TEMPERATURE IN ELEMENT (for output)
C-------------------------------------------
      IF(JTHE < 0 ) THEN 
        DO I=1,NEL
           IF(GBUF%OFF(I) == ZERO) CYCLE
           TEMPEL(I) = ONE_OVER_8 *(  TEMP(NC1(I)) + TEMP(NC2(I))  
     .                          + TEMP(NC3(I)) + TEMP(NC4(I)) 
     .                          + TEMP(NC5(I)) + TEMP(NC6(I)) 
     .                          + TEMP(NC7(I)) + TEMP(NC8(I)))
           GBUF%TEMP(I) = TEMPEL(I)
        ENDDO
      ENDIF
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
              CALL BASIS8 (ZR,ZS,ZT,H,PR2,PS2,PT2)
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
                VAR_REG(I,IP) = H(1)*DNL(IPOS(1)) + H(2)*DNL(IPOS(2)) + H(3)*DNL(IPOS(3))
     .                        + H(4)*DNL(IPOS(4)) + H(5)*DNL(IPOS(5)) + H(6)*DNL(IPOS(6))
     .                        + H(7)*DNL(IPOS(7)) + H(8)*DNL(IPOS(8))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C---------[Bj] first---      
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
         CALL S8EDERIG3(
     1   PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     2   PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     3   PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     4   PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     5   PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     6   PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     7   AJI1(1,IP),AJI2(1,IP),AJI3(1,IP),AJI4(1,IP),
     8   AJI5(1,IP),AJI6(1,IP),AJI7(1,IP),AJI8(1,IP),
     9   AJI9(1,IP),PR(1,IP),  PS(1,IP),  PT(1,IP),
     A   NEL)
        ENDDO    !  IT=1,NPTT
       ENDDO     !  IS=1,NPTS
      ENDDO      !  IR=1,NPTR
C-----------Begin integrating points-----
      DO IS=1,NPTS
       DO IR=1,NPTR
        DO IT=1,NPTT
          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
C-----avoid multi-print    
          IF (IOFFS == 1)THEN
           DO I=1,NEL
            IF (OFFS(I)<=TWO) LBUF%OFF(I)=OFFS(I)
           ENDDO
          END IF
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
         CALL S8EDERI_2(
     1   OFFG,            OFF,             VOLN,            A_GAUSS(IR,NPTR),
     2   A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,              PX1(1,IP),
     3   PX2(1,IP),       PX3(1,IP),       PX4(1,IP),       PX5(1,IP),
     4   PX6(1,IP),       PX7(1,IP),       PX8(1,IP),       PY1(1,IP),
     5   PY2(1,IP),       PY3(1,IP),       PY4(1,IP),       PY5(1,IP),
     6   PY6(1,IP),       PY7(1,IP),       PY8(1,IP),       PZ1(1,IP),
     7   PZ2(1,IP),       PZ3(1,IP),       PZ4(1,IP),       PZ5(1,IP),
     8   PZ6(1,IP),       PZ7(1,IP),       PZ8(1,IP),       PXC1,
     9   PXC2,            PXC3,            PXC4,            PYC1,
     A   PYC2,            PYC3,            PYC4,            PZC1,
     B   PZC2,            PZC3,            PZC4,            BXY1,
     C   BXY2,            BXY3,            BXY4,            BXY5,
     D   BXY6,            BXY7,            BXY8,            BYX1,
     E   BYX2,            BYX3,            BYX4,            BYX5,
     F   BYX6,            BYX7,            BYX8,            BXZ1,
     G   BXZ2,            BXZ3,            BXZ4,            BXZ5,
     H   BXZ6,            BXZ7,            BXZ8,            BZX1,
     I   BZX2,            BZX3,            BZX4,            BZX5,
     J   BZX6,            BZX7,            BZX8,            BYZ1,
     K   BYZ2,            BYZ3,            BYZ4,            BYZ5,
     L   BYZ6,            BYZ7,            BYZ8,            BZY1,
     M   BZY2,            BZY3,            BZY4,            BZY5,
     N   BZY6,            BZY7,            BZY8,            BXX1,
     O   BXX2,            BXX3,            BXX4,            BXX5,
     P   BXX6,            BXX7,            BXX8,            BYY1,
     Q   BYY2,            BYY3,            BYY4,            BYY5,
     R   BYY6,            BYY7,            BYY8,            BZZ1,
     S   BZZ2,            BZZ3,            BZZ4,            BZZ5,
     T   BZZ6,            BZZ7,            BZZ8,            AJP4(1,IP),
     U   AJP5(1,IP),      AJP6(1,IP),      AJP7(1,IP),      AJP8(1,IP),
     V   AJP9(1,IP),      AJ1,             AJ2,             AJ3,
     W   AJ4,             AJ5,             AJ6,             SMAX,
     X   DELTAX,          ICP,             IDEG,            NU,
     Y   VOLP(1,IP),      NEL)
C
         CALL S8SDERI3(
     1   OFFG,            OFF,             VOLP(1,IP),      NGL,
     2   A_GAUSS(IT,NPTT),A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),WI,
     3   XD1,             XD2,             XD3,             XD4,
     4   XD5,             XD6,             XD7,             XD8,
     5   YD1,             YD2,             YD3,             YD4,
     6   YD5,             YD6,             YD7,             YD8,
     7   ZD1,             ZD2,             ZD3,             ZD4,
     8   ZD5,             ZD6,             ZD7,             ZD8,
     9   A11,             A12,             A13,             A21,
     A   A22,             A23,             A31,             A32,
     B   A33,             DN_R,            DN_S,            DN_T,
     C   INVJ,            DN_X,            DN_Y,            DN_Z,
     D   VOLN,            NEL)
        CALL S8SBDEFO3(
     1   ULX1,            ULX2,            ULX3,            ULX4,
     2   ULX5,            ULX6,            ULX7,            ULX8,
     3   ULY1,            ULY2,            ULY3,            ULY4,
     4   ULY5,            ULY6,            ULY7,            ULY8,
     5   ULZ1,            ULZ2,            ULZ3,            ULZ4,
     6   ULZ5,            ULZ6,            ULZ7,            ULZ8,
     7   XD1,             XD2,             XD3,             XD4,
     8   XD5,             XD6,             XD7,             XD8,
     9   YD1,             YD2,             YD3,             YD4,
     A   YD5,             YD6,             YD7,             YD8,
     B   ZD1,             ZD2,             ZD3,             ZD4,
     C   ZD5,             ZD6,             ZD7,             ZD8,
     D   INVJ,            A_GAUSS(IT,NPTT),A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),
     E   A11,             A12,             A13,             A21,
     F   A22,             A23,             A31,             A32,
     G   A33,             DN_R,            DN_S,            DN_T,
     H   BB,              DXX,             DXY,             DXZ,
     I   DYX,             DYY,             DYZ,             DZX,
     J   DZY,             DZZ,             D4,              D5,
     K   D6,              WXX,             WYY,             WZZ,
     L   LBUF%VOL,        OFF,             LBUF%EINT,       GBUF%OFF,
     M   DSV,             ICP,             FAC,             SDV,
     N   ISELECT,         IDEG,            NEL,             ISMSTR)
c
         DO I=1,NEL
           RHOO(I) = LBUF%RHO(I)
         ENDDO
          DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))+SDV(1:NEL)   
          CALL SRHO3(
     1   PM,         LBUF%VOL,   LBUF%RHO,   LBUF%EINT,
     2   DIVDE,      FLUX(1,NF1),FLU1(NF1),  VOLN,
     3   DVOL,       NGL,        MXT,        OFF,
     4   0,          GBUF%TAG22, VOLP(1,IP), LBUF%VOL0DP,
     5   AMU,        GBUF%OFF,   NEL,        MTN,
     6   JALE,       ISMSTR,     JEUL,       JLAG)
C                                       
C-----------------------------
C      EXTRACT STRESSES + SMALL STRAIN
C-----------------------------
            CALL CSMALL3(LBUF%SIG,S1,S2,S3,S4,S5,S6,
     .                   GBUF%OFF,OFF,NEL)
C
C    for heat transfert
C
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C     TIME STEP EST CALCULE ICI DT2T
C------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
c
c
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
       IF (ISTRAIN == 1)THEN 
        CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
       ENDIF
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C----------------------------
C     INTERNAL FORCES
C----------------------------
      CALL S8SFINT3(
     1   LBUF%SIG,  F11,       F21,       F31,
     2   F12,       F22,       F32,       F13,
     3   F23,       F33,       F14,       F24,
     4   F34,       F15,       F25,       F35,
     5   F16,       F26,       F36,       F17,
     6   F27,       F37,       F18,       F28,
     7   F38,       DN_X,      DN_Y,      DN_Z,
     8   BB,        VOLN,      QVIS,      ICP,
     9   JFAC(1,IP),NEL,       ISELECT,   IDEG,
     A   ISMSTR)
      CALL S8EFMOY3(
     1   LBUF%SIG,   VOLN,       QVIS,       PP,
     2   LBUF%EINT,  LBUF%RHO,   LBUF%QVIS,  LBUF%PLA,
     3   LBUF%EPSD,  GBUF%EPSD,  GBUF%SIG,   GBUF%EINT,
     4   GBUF%RHO,   GBUF%QVIS,  GBUF%PLA,   VOLG,
     5   STI,        STIN,       ICP,        OFF,
     6   LBUF%VOL,   GBUF%VOL,   GBUF%G_PLA, GBUF%G_EPSD,
     7   LBUF%EINTTH,GBUF%EINTTH,IEXPAN,     NEL,
     8   CONDE,      CONDEN)
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
      IF (INLOC > 0) THEN  
        IMAT = MXT(1)
        ZR = A_GAUSS(IR,NPTR)
        ZS = A_GAUSS(IS,NPTS) 
        ZT = A_GAUSS(IT,NPTT)
        CALL BASIS8 (ZR,ZS,ZT,H,PR2,PS2,PT2)
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
       DO I=1,NEL
        OFFG(I)=MIN(OFFG(I),OFF(I))
        IF (LBUF%OFF(I)>ONE .AND. GBUF%OFF(I) == ONE) THEN
          OFFS(I)=MIN(LBUF%OFF(I),OFFS(I))
          IOFFS  =1
        END IF
       ENDDO
C-------------------------
c    finite element heat transfert  
C--------------------------
C
        ENDDO    !  IT=1,NPTT
       ENDDO     !  IS=1,NPTS
      ENDDO      !  IR=1,NPTR
C-----------End integrating points-----
c
      IF (IOFFS == 1) THEN
       DO I=1,NEL
          IF (OFFS(I)<=TWO) GBUF%OFF(I)=OFFS(I)
       END DO
c-------------------
       DO IR=1,NPTR
        DO IS=1,NPTS
         DO IT=1,NPTT
            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
          IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
          DO I=1,NEL
              IF (GBUF%OFF(I) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
          END DO
         END DO 
        END DO 
       END DO
      END IF   ! IOFFS == 1
      IF(ICP==1.AND.ISMSTR/=10.AND.ISMSTR/=12) THEN
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
C-----------------------------
C     SMALL STRAIN 
C-----------------------------
      CALL SMALLB3(
     1   GBUF%OFF,OFFG,    NEL,     ISMSTR)
      IF (ISMSTR==11.OR.(JCVT == 0 .AND. ISMSTR>0)) THEN
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
     1   GBUF%SMSTR,OFFG,      WXX0,      WYY0,
     2   WZZ0,      R11,       R12,       R13,
     3   R21,       R22,       R23,       R31,
     4   R32,       R33,       NEL,       ISMSTR,
     5   JCVT)
      END IF
       IF (INCONV==1.AND.TT>ZERO ) THEN
         CALL S8XREF_IMP(
     1   GBUF%OFF,   GBUF%COR_XR,X1,         X2,
     2   X3,         X4,         X5,         X6,
     3   X7,         X8,         Y1,         Y2,
     4   Y3,         Y4,         Y5,         Y6,
     5   Y7,         Y8,         Z1,         Z2,
     6   Z3,         Z4,         Z5,         Z6,
     7   Z7,         Z8,         GBUF%COR_FR,NEL)
       END IF

C--------------------------
C       BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
           IF(JCVT == 0) THEN
            IFVM22 = 0
            CALL SBILAN(PARTSAV,GBUF%EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .                  VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .                  VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .                  VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .                  VOLG,IPARTS,GRESAV,GRTH,IGRTH,GBUF%OFF,
     .                  IEXPAN,GBUF%EINTTH,GBUF%FILL,GBUF%MOM,NEL,IFVM22,
     .                  X1, X2, X3, X4, X5, X6, X7, X8,
     .                  Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .                  Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .                  ITASK,IPARG(1,NG))
           ELSE
             CALL SRBILAN(PARTSAV,GBUF%EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .       VGXA, VGYA, VGZA, VGA2, VOLG,IPARTS,
     .       GRESAV,GRTH,IGRTH,GBUF%OFF,IEXPAN,GBUF%EINTTH,
     .       GBUF%FILL, XGXA, XGYA, XGZA,
     .       XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
           ENDIF
      ENDIF
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      CALL S8SFINT3_CRIMP(TRM,GBUF%COR_NF,GBUF%COR_FR,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   NEL   ) 
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
 
