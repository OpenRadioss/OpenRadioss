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
Chd|  S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
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
Chd|        NSVIS_SM12                    source/elements/solid/solide10/nsvis_sm12.F
Chd|        S8EDEFO3                      source/elements/solid/solide8e/s8edefo3.F
Chd|        S8EDEFOC3                     source/elements/solid/solide8e/s8edefoc3.F
Chd|        S8EDEFOT12                    source/elements/solid/solide8e/s8edefot12.F
Chd|        S8EDEFOT3                     source/elements/solid/solide8e/s8edefot3.F
Chd|        S8EDEFW3                      source/elements/solid/solide8e/s8edefw3.F
Chd|        S8EDERIC3                     source/elements/solid/solide8e/s8ederic3.F
Chd|        S8EDERICM3                    source/elements/solid/solide8e/s8edericm3.F
Chd|        S8EDERICT3                    source/elements/solid/solide8e/s8ederict3.F
Chd|        S8EDERICTO3                   source/elements/solid/solide8e/s8edericto3.F
Chd|        S8EDERIG3                     source/elements/solid/solide8e/s8ederig3.F
Chd|        S8EDERIPR3                    source/elements/solid/solide8e/s8ederipr3.F
Chd|        S8EDERISH2                    source/elements/solid/solide8e/s8ederish2.F
Chd|        S8EDERI_2                     source/elements/solid/solide8e/s8ederi_2.F
Chd|        S8EDERI_BIJ                   source/elements/solid/solide8e/s8ederi_bij.F
Chd|        S8EFINT3                      source/elements/solid/solide8e/s8efint3.F
Chd|        S8EFMOY3                      source/elements/solid/solide8e/s8efmoy3.F
Chd|        S8EGETBIJ                     source/elements/solid/solide8e/s8egetbij.F
Chd|        S8EGETPIJ                     source/elements/solid/solide8e/s8egetpij.F
Chd|        S8EJACIP3                     source/elements/solid/solide8e/s8ejacip3.F
Chd|        S8EJACIT3                     source/elements/solid/solide8e/s8ejacit3.F
Chd|        S8EPRST_INI                   source/elements/solid/solide8e/s8eprst_ini.F
Chd|        S8ESELECSH                    source/elements/solid/solide8e/s8eselecsh.F
Chd|        S8ESELECSHT                   source/elements/solid/solide8e/s8eselecsht.F
Chd|        S8ETHERM                      source/elements/solid/solide8e/s8etherm.F
Chd|        S8ETOTSH10                    source/elements/solid/solide8e/s8etotsh10.F
Chd|        S8EVIS3                       source/elements/solid/solide8e/s8evis3.F
Chd|        S8E_SIGP                      source/elements/solid/solide8e/s8e_sig.F
Chd|        S8E_SIG_A                     source/elements/solid/solide8e/s8e_sig_a.F
Chd|        S8FUPD11T12                   source/elements/solid/solide8e/s8fupd11t12.F
Chd|        S8SAV12                       source/elements/solid/solide/s8sav12.F
Chd|        S8SAV3                        source/elements/solid/solide/s8sav3.F
Chd|        S8SAVSR3                      source/elements/solid/solide/s8savsr3.F
Chd|        S8ZDERIMS3                    source/elements/solid/solide8z/s8zderims3.F
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
Chd|        SORDEF12                      source/elements/solid/solidez/sordef12.F
Chd|        SORDEFT12                     source/elements/solid/solidez/sordeft12.F
Chd|        SORDEFT3                      source/elements/solid/solidez/sordeft3.F
Chd|        SORDEFTB                      source/elements/solid/solide8e/sordeftb.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR12                      source/elements/solid/solide/srcoor12.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SROTO12_SIG                   source/elements/solid/solidez/sroto12_sig.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        SRROTADP                      source/elements/solid/solide/srrotadp.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S8EFORC3(
     1   ELBUF_TAB,  NG,         PM,         GEO,
     2   IXS,        X,          A,          V,
     3   MS,         W,          FLUX,       FLU1,
     4   VEUL,       FV,         ALE_CONNECT,IPARG,
     5   TF,         NPF,        BUFMAT,     PARTSAV,
     6   NLOC_DMG,   DT2T,       NELTST,     ITYPTST,
     7   STIFN,      FSKY,       IADS,       OFFSET,
     8   EANI,       IPARTS,     ICP,        F11,
     9   F21,        F31,        F12,        F22,
     A   F32,        F13,        F23,        F33,
     B   F14,        F24,        F34,        F15,
     C   F25,        F35,        F16,        F26,
     D   F36,        F17,        F27,        F37,
     E   F18,        F28,        F38,        NEL,
     F   NVC,        IPM,        ITASK,      ISTRAIN,
     G   TEMP,       FTHE,       FTHESKY,    IEXPAN,
     H   GRESAV,     GRTH,       IGRTH,      MSSA,
     I   DMELS,      TABLE,      IGEO,       XDP,
     J   VOLN,       CONDN,      CONDNSKY,   D,
     K   SENSORS,    IOUTPRT,    MAT_ELEM,   H3D_STRAIN,
     L   DT)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG,NGROUP),NPF(*),
     .    IADS(8,*),IPARTS(*), IPM(NPROPMI,*),GRTH(*),IGRTH(*),IGEO(NPROPGI,*),
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
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), A(*), V(*), MS(*), W(*), 
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
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE(DT_),    INTENT(INOUT) :: DT
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
C Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ),STIN(MVSIZ),GAMA(MVSIZ,6),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ),
     .   WXX0(MVSIZ) , WYY0(MVSIZ) , WZZ0(MVSIZ),
     .   CONDE(MVSIZ), CONDEN(MVSIZ)
C Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)
      INTEGER IOFFS, N,ITET
      my_real
     .   OFFS(MVSIZ),DSV(MVSIZ),SDV(MVSIZ),L_MAX(MVSIZ)
C-----
C Variables utilisees dans les routines solides uniquement (en arguments).
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
     .  DZX(MVSIZ),DXZ(MVSIZ),DIVDE(MVSIZ)
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
      INTEGER NNEGA,INDEX(MVSIZ),ISELECT,ipr,iel,ISEL_V,ISM12_11,ICPT
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
     .   PX1H1(MVSIZ), PX1H2(MVSIZ), PX1H3(MVSIZ),PX1H4(MVSIZ),  
     .   PX2H1(MVSIZ), PX2H2(MVSIZ), PX2H3(MVSIZ),PX2H4(MVSIZ),  
     .   PX3H1(MVSIZ), PX3H2(MVSIZ), PX3H3(MVSIZ),PX3H4(MVSIZ),  
     .   PX4H1(MVSIZ), PX4H2(MVSIZ), PX4H3(MVSIZ),PX4H4(MVSIZ),  
     .   SIGP(NEL,6),AMU(MVSIZ),JR_1(MVSIZ),JS_1(MVSIZ),JT_1(MVSIZ),
     .   FLD(MVSIZ),CAQ,SIG_A(MVSIZ,3),RHO0,NUT(MVSIZ),RHO0_1,CNS2,NU0
      DOUBLE PRECISION 
     .   VOLP(MVSIZ,NNPT)

C-----
C     Variables utilis  es pour le non-local
      my_real,
     .  DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      INTEGER :: INLOC, L_NLOC, INOD(8), IPOS(8), IMAT,II(6),NN_DEL,PID
      my_real,
     .  DIMENSION(:), POINTER :: DNL
      my_real 
     .  H(8),PS2(8),PR2(8),PT2(8),ZR,ZS,ZT
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
      INTEGER IBOLTP,NBPRELD,LLPIJ
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
      ISM12_11 = ELBUF_TAB(NG)%BUFLY(1)%L_SIGL
c
      ILAY   = 1
      IBID = 0
      IBIDV  = 0
      NF1=NFT+1
C--------------------------
      ISEL_V=0
C----- free shear locking by integ selective      
      IF (IINT==2.AND.JCVT/=0.AND.ICP/=1)THEN
       ISELECT=1
C------ use Bij (v termes) ----     
       IF ((ICP==0.OR.ICP==2).AND.ISMSTR/=10.AND.ISMSTR/=12) ISELECT=2
C-------v termes will be calculated like HEPH       
       IF (MTN==1.AND.(ISMSTR==10.OR.ISMSTR==12)) ISEL_V=1
      ELSE
       ISELECT=0
      ENDIF
      ICPT = ICP
      IF (IINT==2.AND.MTN==1.AND.ISMSTR>=11) ICPT=0
c      ipr=0
c      iel=1        
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
     B   VDX,VDY,VDZ,VD2,VIS,GBUF%OFF,OFFG,GBUF%SMSTR,
     C   GBUF%RHO,RHOO,NC1,NC2,NC3,NC4,NC5,NC6,
     D   NC7,NC8,NGL,MXT,NGEO,
     E   BID,BID,BID,BID,BID,BID,
     F   XD1, XD2  , XD3, XD4, XD5, XD6, XD7, XD8,
     G   YD1, YD2  , YD3, YD4, YD5, YD6, YD7, YD8,
     H   ZD1, ZD2  , ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     I   XDP, IPARG, NG , NEL)
      ELSE
C GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
       CALL SRCOOR3(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   VD2,VIS,GBUF%OFF,OFFG,GBUF%SMSTR,GBUF%RHO,RHOO,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     .   NGL,MXT,NGEO,IOUTPRT, VGXA, VGYA, VGZA, VGA2,
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .   XDP, X0 , Y0 , Z0 , NEL,XGXA, XGYA, XGZA,
     .   XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,IPARG(1,NG),
     .   GBUF%GAMA_R) 
      ENDIF
c---  
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
       IF (ISMSTR == 11.AND.JCVT>0) THEN
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
       ENDIF
      ENDIF
C-----------
      IOFFS=0
      DO I=LFT,LLT
        OFFS(I)=EP20
        DELTAX(I)=EP30
        IDEG(I)=0
      ENDDO
      IF(JTHE < 0) THEM(LFT:LLT,1:8) =ZERO

      MX = MXT(LFT)
      NU0=PM(21,MX)
      IF (ICP == 2) THEN
       C1 =PM(32,MX)
       DO I=LFT,LLT
        NU(I)=MIN(HALF,NU0)
        E0(I) =THREE*(ONE-TWO*NU(I))*C1
       ENDDO
c------ Sigy of GBUF%SIG is not good, take the ip of min(e_ps) 
       SIGP=ZERO   
       IF (GBUF%G_PLA>0) THEN
        CALL S8E_SIGP(
     1   ELBUF_TAB(NG),SIGP,         NEL)
       END IF !(GBUF%G_PLA>0) THEN
       CALL S8ZSIGP3(LFT  ,LLT       ,SIGP  ,E0  ,GBUF%PLA,
     2               FAC  ,GBUF%G_PLA,NEL     )
       IF (ISELECT >1) THEN
        DO I=LFT,LLT
         IF (FAC(I)>ZERO) THEN
         NU(I)=NU(I)+(HALF-NU(I))*FAC(I)
         END IF
        ENDDO
       END IF
      ELSEIF (ICP > 0 .OR.ISELECT >1.OR.ISEL_V>0) THEN
       DO I=LFT,LLT
        NU(I)=MIN(HALF,NU0)
       ENDDO
      ENDIF 
      IF (ISELECT >1.OR.ISEL_V>0) THEN
       DO I=LFT,LLT
        NU1(I) = NU(I)/(ONE - NU(I))
       ENDDO
      ENDIF 
      CNS2 = ZERO
      NUT(LFT:LLT) =NU0
      IF (IINT==2.AND.ICP==1.AND.MTN==1.AND.
     .               (ISMSTR==10.OR.ISMSTR==12)) THEN
        DO I=LFT,LLT
          NUT(I) = TWO_THIRD*NU0*NU0
        ENDDO
        IF (ISMSTR==12) THEN
          RHO0_1 =PM( 1,MX)
          CNS2 = ZEP02
          IF (IGEO(35,NGEO(1))>0) CNS2=CNS2-ABS(GEO(17,NGEO(1)))
        END IF
      END IF
      CALL S8EPRST_INI(PR ,PS ,PT )
C-------now [B](Pij) is always in global system w/ ISMSTR10
      IF (ISMSTR >= 10.AND.ISMSTR <= 12) THEN
       IF (ISMSTR == 11) THEN
        CALL S8EDERICT3(
     1   OFFG,    VOLG,    NGL,     X0(1,1),
     2   X0(1,2), X0(1,3), X0(1,4), X0(1,5),
     3   X0(1,6), X0(1,7), X0(1,8), Y0(1,1),
     4   Y0(1,2), Y0(1,3), Y0(1,4), Y0(1,5),
     5   Y0(1,6), Y0(1,7), Y0(1,8), Z0(1,1),
     6   Z0(1,2), Z0(1,3), Z0(1,4), Z0(1,5),
     7   Z0(1,6), Z0(1,7), Z0(1,8), PXC1,
     8   PXC2,    PXC3,    PXC4,    PYC1,
     9   PYC2,    PYC3,    PYC4,    PZC1,
     A   PZC2,    PZC3,    PZC4,    HX,
     B   HY,      HZ,      AJC1,    AJC2,
     C   AJC3,    AJC4,    AJC5,    AJC6,
     D   AJC7,    AJC8,    AJC9,    SMAX,
     E   NEL)
        CALL S8EJACIP3(
     1   HX,      HY,      HZ,      AJC1,
     2   AJC2,    AJC3,    AJC4,    AJC5,
     3   AJC6,    AJC7,    AJC8,    AJC9,
     4   AJP1,    AJP2,    AJP3,    AJP4,
     5   AJP5,    AJP6,    AJP7,    AJP8,
     6   AJP9,    NEL)
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
C----- !!!! seems no more necessary--- to remove later       
        CALL SDLEN_SM11(
     1   VOLG,    DELTAX,  SMAX,    IDEG,
     2   NEL)
       ELSE
        IF (ISMSTR == 12.AND.IDTMIN(1)==3.AND.JCVT == 0.AND.ISM12_11==0) THEN
C!!!!!!calcul local rep for ISMSTR 10 to 11 (offg>un) 
          CALL SRCOOR12(
     1   GBUF%OFF,X,       XDP,     NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     R11,
     4   R12,     R13,     R21,     R22,
     5   R23,     R31,     R32,     R33,
     6   NEL,     JHBE)
        END IF 
        CALL S8EDERICTO3(
     1   OFFG,      VOLG,      PXC1,      PXC2,
     2   PXC3,      PXC4,      PYC1,      PYC2,
     3   PYC3,      PYC4,      PZC1,      PZC2,
     4   PZC3,      PZC4,      GBUF%JAC_I,NEL)
C----------just for initialization     
         SMAX(LFT:LLT)=EP10
       END IF !(ISMSTR == 11) THEN
C     
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
     A   VZ0(1,8),NEL)
C-----don't do it w/ degenerated elm   
         IF (IDTS6==0)  CALL DEGENES8(
     1   IXS(1,NF1),IDEG,      NEL)
         DO I=LFT,LLT
          IF (IDEG(I)>0) IDEG(I) = IDEG(I) + 10
         ENDDO
       END IF
       IF (ISMSTR == 11) THEN
C-----------[B] first
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
         LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
C
         CALL S8EJACIT3(
     1   VOLP(1,IP),      A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),
     2   WI,              AJP1(1,IP),      AJP2(1,IP),      AJP3(1,IP),
     3   AJP4(1,IP),      AJP5(1,IP),      AJP6(1,IP),      AJP7(1,IP),
     4   AJP8(1,IP),      AJP9(1,IP),      AJI1(1,IP),      AJI2(1,IP),
     5   AJI3(1,IP),      AJI4(1,IP),      AJI5(1,IP),      AJI6(1,IP),
     6   AJI7(1,IP),      AJI8(1,IP),      AJI9(1,IP),      LBUF%JAC_I,
     7   NEL,             ISMSTR)
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C------ISMSTR=10,12-----: w/ fixed PIJ---
       ELSE
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
         LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
C
         CALL S8EGETPIJ(
     .    PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     .    PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     .    PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     .    PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     .    PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     .    PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     .    LBUF%PIJ , LLT  )
          VOLP(LFT:LLT,IP)=VOLG(LFT:LLT)
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C-----------Begin integrating points-----
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
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
         IF (ISELECT>1) THEN
          CALL S8EGETBIJ(
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    LBUF%PIJ,LLT)
         END IF !(ISELECT>1) THEN
         CALL S8EDEFOT3(
     1   PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     2   PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     3   PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     4   PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     5   PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     6   PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     7   BXY1,      BXY2,      BXY3,      BXY4,
     8   BXY5,      BXY6,      BXY7,      BXY8,
     9   BYX1,      BYX2,      BYX3,      BYX4,
     A   BYX5,      BYX6,      BYX7,      BYX8,
     B   BXZ1,      BXZ2,      BXZ3,      BXZ4,
     C   BXZ5,      BXZ6,      BXZ7,      BXZ8,
     D   BZX1,      BZX2,      BZX3,      BZX4,
     E   BZX5,      BZX6,      BZX7,      BZX8,
     F   BYZ1,      BYZ2,      BYZ3,      BYZ4,
     G   BYZ5,      BYZ6,      BYZ7,      BYZ8,
     H   BZY1,      BZY2,      BZY3,      BZY4,
     I   BZY5,      BZY6,      BZY7,      BZY8,
     J   BXX1,      BXX2,      BXX3,      BXX4,
     K   BXX5,      BXX6,      BXX7,      BXX8,
     L   BYY1,      BYY2,      BYY3,      BYY4,
     M   BYY5,      BYY6,      BYY7,      BYY8,
     N   BZZ1,      BZZ2,      BZZ3,      BZZ4,
     O   BZZ5,      BZZ6,      BZZ7,      BZZ8,
     P   PXY1,      PXY2,      PXY3,      PXY4,
     Q   PXY5,      PXY6,      PXY7,      PXY8,
     R   PYX1,      PYX2,      PYX3,      PYX4,
     S   PYX5,      PYX6,      PYX7,      PYX8,
     T   PXZ1,      PXZ2,      PXZ3,      PXZ4,
     U   PXZ5,      PXZ6,      PXZ7,      PXZ8,
     V   PZX1,      PZX2,      PZX3,      PZX4,
     W   PZX5,      PZX6,      PZX7,      PZX8,
     X   PYZ1,      PYZ2,      PYZ3,      PYZ4,
     Y   PYZ5,      PYZ6,      PYZ7,      PYZ8,
     Z   PZY1,      PZY2,      PZY3,      PZY4,
     1   PZY5,      PZY6,      PZY7,      PZY8,
     2   VX0(1,1),  VX0(1,2),  VX0(1,3),  VX0(1,4),
     3   VX0(1,5),  VX0(1,6),  VX0(1,7),  VX0(1,8),
     4   VY0(1,1),  VY0(1,2),  VY0(1,3),  VY0(1,4),
     5   VY0(1,5),  VY0(1,6),  VY0(1,7),  VY0(1,8),
     6   VZ0(1,1),  VZ0(1,2),  VZ0(1,3),  VZ0(1,4),
     7   VZ0(1,5),  VZ0(1,6),  VZ0(1,7),  VZ0(1,8),
     8   MFXX(1,IP),MFXY(1,IP),MFXZ(1,IP),MFYX(1,IP),
     9   MFYY(1,IP),MFYZ(1,IP),MFZX(1,IP),MFZY(1,IP),
     A   MFZZ(1,IP),ICP,       DETF0,     JFAC(1,IP),
     B   NUT,       ISELECT,   IDEG,      BXX(1,IP),
     C   BYY(1,IP), BZZ(1,IP), GBUF%OFF,  NEL,
     D   ISMSTR)
C     
         IF (JCVT > 0.AND.(ISMSTR==10.OR.ISMSTR==12)) THEN
          CALL SORDEFT3(LFT,LLT,MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP),
     .         MFYX(1,IP), MFYY(1,IP), MFYZ(1,IP),
     .         MFZX(1,IP), MFZY(1,IP), MFZZ(1,IP),
     .         R11, R21, R31, R12, R22, R32, R13, R23, R33)
         IF (ISELECT>1) 
     .    CALL SORDEFTB(LFT,LLT,BXX(1,IP),  BYY(1,IP), BZZ(1,IP),
     .         R11, R21, R31, R12, R22, R32, R13, R23, R33)
         ELSEIF (IDTMIN(1)==3.AND.ISMSTR == 12.AND.ISM12_11==0) THEN
          CALL SORDEFT12(LFT,LLT,MFXX(1,IP), MFXY(1,IP), MFXZ(1,IP),
     .         MFYX(1,IP), MFYY(1,IP), MFYZ(1,IP),
     .         MFZX(1,IP), MFZY(1,IP), MFZZ(1,IP),
     .         R11, R21, R31, R12, R22, R32, R13, R23, R33,GBUF%OFF)
         ENDIF
C
        ENDDO 
       ENDDO 
      ENDDO
C---shear treatment here directly on Cauchy Left [B] for Ismstr=10  
        IF (ISELECT>0) THEN
         CALL S8ESELECSHT(
     1   NPTR,    NPTS,    NPTT,    NNPT,
     2   MFXX,    MFXY,    MFXZ,    MFYX,
     3   MFYY,    MFYZ,    MFZX,    MFZY,
     4   MFZZ,    BXX,     BYY,     BZZ,
     5   BXY,     BYZ,     BXZ,     ISELECT,
     6   GBUF%OFF,NEL,     ISMSTR)
        ENDIF
       END IF !(ISMSTR == 11) THEN
      ENDIF     ! (ISMSTR >= 10.AND.ISMSTR <= 12)
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
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
       IF (JCVT /= 0.AND.ISORTH/=0 ) THEN
       CALL S8SAVSR3(
     1   GBUF%OFF,  GBUF%SMSTR,X0,        Y0,
     2   Z0,        NEL)
       ELSE
       CALL S8SAV3(
     1   GBUF%OFF,  GBUF%SMSTR,XD1,       XD2,
     2   XD3,       XD4,       XD5,       XD6,
     3   XD7,       XD8,       YD1,       YD2,
     4   YD3,       YD4,       YD5,       YD6,
     5   YD7,       YD8,       ZD1,       ZD2,
     6   ZD3,       ZD4,       ZD5,       ZD6,
     7   ZD7,       ZD8,       NEL)
       END IF 
      END IF !(ISMSTR <= 3) THEN
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
      ENDIF 
C-----don't do it w/ degenerated elm,    
       IF ((ICP==1.OR.ISELECT>1).AND.ISMSTR < 10) THEN
        IF (IDTS6==0)  CALL DEGENES8(
     1   IXS(1,NF1),IDEG,      NEL)
        IF (ISELECT>1) THEN
         DO I=LFT,LLT
          IF (IDEG(I)>2) IDEG(I) = IDEG(I) + 10
         ENDDO
        ELSE
         DO I=LFT,LLT
          IF (IDEG(I)>0) IDEG(I) = IDEG(I) + 10
         ENDDO
        END IF 
       END IF !(ICP==1.AND.ISMSTR < 10)
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
        DO I=LFT,LLT
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
C----------selective integration
      IF (ISELECT>0) THEN
        CALL S8ESELECSH(
     1   NPTR,    NPTS,    NPTT,    NNPT,
     2   PX1,     PX2,     PX3,     PX4,
     3   PX5,     PX6,     PX7,     PX8,
     4   PY1,     PY2,     PY3,     PY4,
     5   PY5,     PY6,     PY7,     PY8,
     6   PZ1,     PZ2,     PZ3,     PZ4,
     7   PZ5,     PZ6,     PZ7,     PZ8,
     8   P0XY1,   P0XY2,   P0XY3,   P0XY4,
     9   P0XY5,   P0XY6,   P0XY7,   P0XY8,
     A   P0YX1,   P0YX2,   P0YX3,   P0YX4,
     B   P0YX5,   P0YX6,   P0YX7,   P0YX8,
     C   P0XZ1,   P0XZ2,   P0XZ3,   P0XZ4,
     D   P0XZ5,   P0XZ6,   P0XZ7,   P0XZ8,
     E   P0ZX1,   P0ZX2,   P0ZX3,   P0ZX4,
     F   P0ZX5,   P0ZX6,   P0ZX7,   P0ZX8,
     G   P0YZ1,   P0YZ2,   P0YZ3,   P0YZ4,
     H   P0YZ5,   P0YZ6,   P0YZ7,   P0YZ8,
     I   P0ZY1,   P0ZY2,   P0ZY3,   P0ZY4,
     J   P0ZY5,   P0ZY6,   P0ZY7,   P0ZY8,
     K   NEL)
      END IF 
C-----------Begin integrating points-----
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
C-----avoid multi-print   
          IF (IOFFS == 1)THEN
           DO I=LFT,LLT
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
         IF (ISELECT>0) THEN
          CALL S8EDERISH2(
     1   PXY1,       PXY2,       PXY3,       PXY4,
     2   PXY5,       PXY6,       PXY7,       PXY8,
     3   PYX1,       PYX2,       PYX3,       PYX4,
     4   PYX5,       PYX6,       PYX7,       PYX8,
     5   PXZ1,       PXZ2,       PXZ3,       PXZ4,
     6   PXZ5,       PXZ6,       PXZ7,       PXZ8,
     7   PZX1,       PZX2,       PZX3,       PZX4,
     8   PZX5,       PZX6,       PZX7,       PZX8,
     9   PYZ1,       PYZ2,       PYZ3,       PYZ4,
     A   PYZ5,       PYZ6,       PYZ7,       PYZ8,
     B   PZY1,       PZY2,       PZY3,       PZY4,
     C   PZY5,       PZY6,       PZY7,       PZY8,
     D   P0XY1(1,IR),P0XY2(1,IR),P0XY3(1,IR),P0XY4(1,IR),
     E   P0XY5(1,IR),P0XY6(1,IR),P0XY7(1,IR),P0XY8(1,IR),
     F   P0YX1(1,IR),P0YX2(1,IR),P0YX3(1,IR),P0YX4(1,IR),
     G   P0YX5(1,IR),P0YX6(1,IR),P0YX7(1,IR),P0YX8(1,IR),
     H   P0XZ1(1,IT),P0XZ2(1,IT),P0XZ3(1,IT),P0XZ4(1,IT),
     I   P0XZ5(1,IT),P0XZ6(1,IT),P0XZ7(1,IT),P0XZ8(1,IT),
     J   P0ZX1(1,IT),P0ZX2(1,IT),P0ZX3(1,IT),P0ZX4(1,IT),
     K   P0ZX5(1,IT),P0ZX6(1,IT),P0ZX7(1,IT),P0ZX8(1,IT),
     L   P0YZ1(1,IS),P0YZ2(1,IS),P0YZ3(1,IS),P0YZ4(1,IS),
     M   P0YZ5(1,IS),P0YZ6(1,IS),P0YZ7(1,IS),P0YZ8(1,IS),
     N   P0ZY1(1,IS),P0ZY2(1,IS),P0ZY3(1,IS),P0ZY4(1,IS),
     O   P0ZY5(1,IS),P0ZY6(1,IS),P0ZY7(1,IS),P0ZY8(1,IS),
     P   NEL)
         IF (ISELECT>1.OR.ISEL_V>0)
     .    CALL S8EDERI_BIJ(
     1   PX1(1,IP),PX2(1,IP),PX3(1,IP),PX4(1,IP),
     2   PX5(1,IP),PX6(1,IP),PX7(1,IP),PX8(1,IP),
     3   PY1(1,IP),PY2(1,IP),PY3(1,IP),PY4(1,IP),
     4   PY5(1,IP),PY6(1,IP),PY7(1,IP),PY8(1,IP),
     5   PZ1(1,IP),PZ2(1,IP),PZ3(1,IP),PZ4(1,IP),
     6   PZ5(1,IP),PZ6(1,IP),PZ7(1,IP),PZ8(1,IP),
     7   PXC1,     PXC2,     PXC3,     PXC4,
     8   PYC1,     PYC2,     PYC3,     PYC4,
     9   PZC1,     PZC2,     PZC3,     PZC4,
     A   PXY1,     PXY2,     PXY3,     PXY4,
     B   PXY5,     PXY6,     PXY7,     PXY8,
     C   PYX1,     PYX2,     PYX3,     PYX4,
     D   PYX5,     PYX6,     PYX7,     PYX8,
     E   PXZ1,     PXZ2,     PXZ3,     PXZ4,
     F   PXZ5,     PXZ6,     PXZ7,     PXZ8,
     G   PZX1,     PZX2,     PZX3,     PZX4,
     H   PZX5,     PZX6,     PZX7,     PZX8,
     I   PYZ1,     PYZ2,     PYZ3,     PYZ4,
     J   PYZ5,     PYZ6,     PYZ7,     PYZ8,
     K   PZY1,     PZY2,     PZY3,     PZY4,
     L   PZY5,     PZY6,     PZY7,     PZY8,
     M   BXY1,     BXY2,     BXY3,     BXY4,
     N   BXY5,     BXY6,     BXY7,     BXY8,
     O   BYX1,     BYX2,     BYX3,     BYX4,
     P   BYX5,     BYX6,     BYX7,     BYX8,
     Q   BXZ1,     BXZ2,     BXZ3,     BXZ4,
     R   BXZ5,     BXZ6,     BXZ7,     BXZ8,
     S   BZX1,     BZX2,     BZX3,     BZX4,
     T   BZX5,     BZX6,     BZX7,     BZX8,
     U   BYZ1,     BYZ2,     BYZ3,     BYZ4,
     V   BYZ5,     BYZ6,     BYZ7,     BYZ8,
     W   BZY1,     BZY2,     BZY3,     BZY4,
     X   BZY5,     BZY6,     BZY7,     BZY8,
     Y   NU,       NU1,      NEL)
C--------- modified [B] is saved in GBUF%ETOTSH verify for Ismstr=11 !!!!!     
          IF (ISMSTR==10.OR.ISMSTR==12) THEN
            CALL S8ETOTSH10(
     1   GBUF%ETOTSH,BXX(1,IP),  BYY(1,IP),  BZZ(1,IP),
     2   BXY(1,IP),  BYZ(1,IP),  BXZ(1,IP),  NEL)
          END IF !(ISMSTR==10.OR.ISMSTR==12) THEN

         END IF !(ISELECT>0) THEN
         IF (ISMSTR==11) THEN
          CALL S8EDEFOT3(
     1   PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     2   PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     3   PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     4   PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     5   PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     6   PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     7   BXY1,      BXY2,      BXY3,      BXY4,
     8   BXY5,      BXY6,      BXY7,      BXY8,
     9   BYX1,      BYX2,      BYX3,      BYX4,
     A   BYX5,      BYX6,      BYX7,      BYX8,
     B   BXZ1,      BXZ2,      BXZ3,      BXZ4,
     C   BXZ5,      BXZ6,      BXZ7,      BXZ8,
     D   BZX1,      BZX2,      BZX3,      BZX4,
     E   BZX5,      BZX6,      BZX7,      BZX8,
     F   BYZ1,      BYZ2,      BYZ3,      BYZ4,
     G   BYZ5,      BYZ6,      BYZ7,      BYZ8,
     H   BZY1,      BZY2,      BZY3,      BZY4,
     I   BZY5,      BZY6,      BZY7,      BZY8,
     J   BXX1,      BXX2,      BXX3,      BXX4,
     K   BXX5,      BXX6,      BXX7,      BXX8,
     L   BYY1,      BYY2,      BYY3,      BYY4,
     M   BYY5,      BYY6,      BYY7,      BYY8,
     N   BZZ1,      BZZ2,      BZZ3,      BZZ4,
     O   BZZ5,      BZZ6,      BZZ7,      BZZ8,
     P   PXY1,      PXY2,      PXY3,      PXY4,
     Q   PXY5,      PXY6,      PXY7,      PXY8,
     R   PYX1,      PYX2,      PYX3,      PYX4,
     S   PYX5,      PYX6,      PYX7,      PYX8,
     T   PXZ1,      PXZ2,      PXZ3,      PXZ4,
     U   PXZ5,      PXZ6,      PXZ7,      PXZ8,
     V   PZX1,      PZX2,      PZX3,      PZX4,
     W   PZX5,      PZX6,      PZX7,      PZX8,
     X   PYZ1,      PYZ2,      PYZ3,      PYZ4,
     Y   PYZ5,      PYZ6,      PYZ7,      PYZ8,
     Z   PZY1,      PZY2,      PZY3,      PZY4,
     1   PZY5,      PZY6,      PZY7,      PZY8,
     2   VX0(1,1),  VX0(1,2),  VX0(1,3),  VX0(1,4),
     3   VX0(1,5),  VX0(1,6),  VX0(1,7),  VX0(1,8),
     4   VY0(1,1),  VY0(1,2),  VY0(1,3),  VY0(1,4),
     5   VY0(1,5),  VY0(1,6),  VY0(1,7),  VY0(1,8),
     6   VZ0(1,1),  VZ0(1,2),  VZ0(1,3),  VZ0(1,4),
     7   VZ0(1,5),  VZ0(1,6),  VZ0(1,7),  VZ0(1,8),
     8   MFXX(1,IP),MFXY(1,IP),MFXZ(1,IP),MFYX(1,IP),
     9   MFYY(1,IP),MFYZ(1,IP),MFZX(1,IP),MFZY(1,IP),
     A   MFZZ(1,IP),ICPT,      DETF0,     JFAC(1,IP),
     B   NU,        ISELECT,   IDEG,      BXX(1,IP),
     C   BYY(1,IP), BZZ(1,IP), GBUF%OFF,  NEL,
     D   ISMSTR)
         ELSEIF (ISMSTR==12) THEN
          CALL S8EDEFOT12(
     1   PX1(1,IP), PX2(1,IP), PX3(1,IP), PX4(1,IP),
     2   PX5(1,IP), PX6(1,IP), PX7(1,IP), PX8(1,IP),
     3   PY1(1,IP), PY2(1,IP), PY3(1,IP), PY4(1,IP),
     4   PY5(1,IP), PY6(1,IP), PY7(1,IP), PY8(1,IP),
     5   PZ1(1,IP), PZ2(1,IP), PZ3(1,IP), PZ4(1,IP),
     6   PZ5(1,IP), PZ6(1,IP), PZ7(1,IP), PZ8(1,IP),
     7   BXY1,      BXY2,      BXY3,      BXY4,
     8   BXY5,      BXY6,      BXY7,      BXY8,
     9   BYX1,      BYX2,      BYX3,      BYX4,
     A   BYX5,      BYX6,      BYX7,      BYX8,
     B   BXZ1,      BXZ2,      BXZ3,      BXZ4,
     C   BXZ5,      BXZ6,      BXZ7,      BXZ8,
     D   BZX1,      BZX2,      BZX3,      BZX4,
     E   BZX5,      BZX6,      BZX7,      BZX8,
     F   BYZ1,      BYZ2,      BYZ3,      BYZ4,
     G   BYZ5,      BYZ6,      BYZ7,      BYZ8,
     H   BZY1,      BZY2,      BZY3,      BZY4,
     I   BZY5,      BZY6,      BZY7,      BZY8,
     J   BXX1,      BXX2,      BXX3,      BXX4,
     K   BXX5,      BXX6,      BXX7,      BXX8,
     L   BYY1,      BYY2,      BYY3,      BYY4,
     M   BYY5,      BYY6,      BYY7,      BYY8,
     N   BZZ1,      BZZ2,      BZZ3,      BZZ4,
     O   BZZ5,      BZZ6,      BZZ7,      BZZ8,
     P   PXY1,      PXY2,      PXY3,      PXY4,
     Q   PXY5,      PXY6,      PXY7,      PXY8,
     R   PYX1,      PYX2,      PYX3,      PYX4,
     S   PYX5,      PYX6,      PYX7,      PYX8,
     T   PXZ1,      PXZ2,      PXZ3,      PXZ4,
     U   PXZ5,      PXZ6,      PXZ7,      PXZ8,
     V   PZX1,      PZX2,      PZX3,      PZX4,
     W   PZX5,      PZX6,      PZX7,      PZX8,
     X   PYZ1,      PYZ2,      PYZ3,      PYZ4,
     Y   PYZ5,      PYZ6,      PYZ7,      PYZ8,
     Z   PZY1,      PZY2,      PZY3,      PZY4,
     1   PZY5,      PZY6,      PZY7,      PZY8,
     2   VX0(1,1),  VX0(1,2),  VX0(1,3),  VX0(1,4),
     3   VX0(1,5),  VX0(1,6),  VX0(1,7),  VX0(1,8),
     4   VY0(1,1),  VY0(1,2),  VY0(1,3),  VY0(1,4),
     5   VY0(1,5),  VY0(1,6),  VY0(1,7),  VY0(1,8),
     6   VZ0(1,1),  VZ0(1,2),  VZ0(1,3),  VZ0(1,4),
     7   VZ0(1,5),  VZ0(1,6),  VZ0(1,7),  VZ0(1,8),
     8   R11,       R12,       R13,       R21,
     9   R22,       R23,       R31,       R32,
     A   R33,       MFXX(1,IP),MFXY(1,IP),MFXZ(1,IP),
     B   MFYX(1,IP),MFYY(1,IP),MFYZ(1,IP),MFZX(1,IP),
     C   MFZY(1,IP),MFZZ(1,IP),ICPT,      DETF0,
     D   JFAC(1,IP),NU,        IDEG,      GBUF%OFF,
     E   ISEL_V,    ISELECT,   NEL,       ISMSTR,
     F   JCVT)
         END IF !(ISMSTR==11) THEN
         CALL S8EDEFO3(
     1   PX1(1,IP),  PX2(1,IP),  PX3(1,IP),  PX4(1,IP),
     2   PX5(1,IP),  PX6(1,IP),  PX7(1,IP),  PX8(1,IP),
     3   PY1(1,IP),  PY2(1,IP),  PY3(1,IP),  PY4(1,IP),
     4   PY5(1,IP),  PY6(1,IP),  PY7(1,IP),  PY8(1,IP),
     5   PZ1(1,IP),  PZ2(1,IP),  PZ3(1,IP),  PZ4(1,IP),
     6   PZ5(1,IP),  PZ6(1,IP),  PZ7(1,IP),  PZ8(1,IP),
     7   BXY1,       BXY2,       BXY3,       BXY4,
     8   BXY5,       BXY6,       BXY7,       BXY8,
     9   BYX1,       BYX2,       BYX3,       BYX4,
     A   BYX5,       BYX6,       BYX7,       BYX8,
     B   BXZ1,       BXZ2,       BXZ3,       BXZ4,
     C   BXZ5,       BXZ6,       BXZ7,       BXZ8,
     D   BZX1,       BZX2,       BZX3,       BZX4,
     E   BZX5,       BZX6,       BZX7,       BZX8,
     F   BYZ1,       BYZ2,       BYZ3,       BYZ4,
     G   BYZ5,       BYZ6,       BYZ7,       BYZ8,
     H   BZY1,       BZY2,       BZY3,       BZY4,
     I   BZY5,       BZY6,       BZY7,       BZY8,
     J   BXX1,       BXX2,       BXX3,       BXX4,
     K   BXX5,       BXX6,       BXX7,       BXX8,
     L   BYY1,       BYY2,       BYY3,       BYY4,
     M   BYY5,       BYY6,       BYY7,       BYY8,
     N   BZZ1,       BZZ2,       BZZ3,       BZZ4,
     O   BZZ5,       BZZ6,       BZZ7,       BZZ8,
     P   PXY1,       PXY2,       PXY3,       PXY4,
     Q   PXY5,       PXY6,       PXY7,       PXY8,
     R   PYX1,       PYX2,       PYX3,       PYX4,
     S   PYX5,       PYX6,       PYX7,       PYX8,
     T   PXZ1,       PXZ2,       PXZ3,       PXZ4,
     U   PXZ5,       PXZ6,       PXZ7,       PXZ8,
     V   PZX1,       PZX2,       PZX3,       PZX4,
     W   PZX5,       PZX6,       PZX7,       PZX8,
     X   PYZ1,       PYZ2,       PYZ3,       PYZ4,
     Y   PYZ5,       PYZ6,       PYZ7,       PYZ8,
     Z   PZY1,       PZY2,       PZY3,       PZY4,
     1   PZY5,       PZY6,       PZY7,       PZY8,
     2   VX1,        VX2,        VX3,        VX4,
     3   VX5,        VX6,        VX7,        VX8,
     4   VY1,        VY2,        VY3,        VY4,
     5   VY5,        VY6,        VY7,        VY8,
     6   VZ1,        VZ2,        VZ3,        VZ4,
     7   VZ5,        VZ6,        VZ7,        VZ8,
     8   DXX,        DXY,        DXZ,        DYX,
     9   DYY,        DYZ,        DZX,        DZY,
     A   DZZ,        D4,         D5,         D6,
     B   WXX,        WYY,        WZZ,        LBUF%VOL,
     C   OFF,        LBUF%EINT,  GBUF%OFF,   DSV,
     D   ICP,        FAC,        SDV,        ISELECT,
     E   IDEG,       LBUF%VOL0DP,NEL,        ISMSTR,
     F   JCVT)
C
         IF (IDTMIN(1)==3.AND.ISMSTR == 12.AND.ISM12_11==0.AND.JCVT==0) THEN
          CALL SORDEF12(LFT,LLT,DXX, DYY, DZZ,
     .         D4, D5, D6,
     .         R11, R21, R31, R12, R22, R32, R13, R23, R33,OFFG0)
         ENDIF
         DO I=LFT,LLT
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
       IF (ISMSTR == 12.AND.IDTMIN(1)==3.AND.ISM12_11==0.AND.JCVT==0) THEN
         CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .           R11, R21, R31, R12, R22, R32, R13, R23, R33,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!         CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .           R11, R21, R31, R12, R22, R32, R13, R23, R33,OFFG0)
       ENDIF ! (ISMSTR == 10.AND.IDTMIN(1)==3)
         IF (JCVT == 0) THEN
            CALL SROTA3(
     1   LBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
C-----------------------------
C      SMALL STRAIN
C-----------------------------
c            CALL SMALLC3(GBUF%SMSTR,GBUF%OFF,OFF,WXX,WYY,
c     .              WZZ,DXX,DYY,DZZ,LBUF%RHO,RHOO,
c     .             DVOL,VOLN,SDV ,NEL)

         ELSE
C-----------------------------
C      EXTRACT STRESSES + SMALL STRAIN
C-----------------------------
            CALL CSMALL3(LBUF%SIG,S1,S2,S3,S4,S5,S6,
     .                   GBUF%OFF,OFF,NEL)
         ENDIF
C
C    for heat transfert
C
      IF(JTHE < 0 ) THEN
            CALL S8ZTEMPEL(
     .           LFT,LLT,NI,
     .           A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),
     .           NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     .           TEMP,TEMPEL)
      ENDIF 
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C     TIME STEP EST CALCULE ICI DT2T
C------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
c
      IF(IBOLTP /= 0) CALL BOLTST(
     .                 IP,        BPRELD,    LBUF%SIG,TT    ,   
     .                 NEL   ,NPT   ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB)
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
       IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        IF (ISM12_11==0.AND.JCVT==0) THEN
         CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .           R11, R12, R13, R21, R22, R23, R31, R32, R33,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!         CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .           R11, R12, R13, R21, R22, R23, R31, R32, R33,OFFG0)
         IF (ISTRAIN == 1) THEN 
          CALL SORDEF12(LFT,LLT,DXX, DXY, DXZ,
     .         D4, D5, D6,
     .         R11, R12, R13, R21, R22, R23, R31, R32, R33,OFFG0)
         ENDIF
        END IF 
       ENDIF ! (ISMSTR == 10.AND.IDTMIN(1)==3)
       IF (ISTRAIN == 1)THEN 
        CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
       ENDIF
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
      IF (ISEL_V>0) THEN
         MX = MXT(LFT)
        CALL S8E_SIG_A(
     1   PM(32,MX),PM(22,MX),OFF,      BXY1,
     2   BXY2,     BXY3,     BXY4,     BXY5,
     3   BXY6,     BXY7,     BXY8,     BYX1,
     4   BYX2,     BYX3,     BYX4,     BYX5,
     5   BYX6,     BYX7,     BYX8,     BXZ1,
     6   BXZ2,     BXZ3,     BXZ4,     BXZ5,
     7   BXZ6,     BXZ7,     BXZ8,     BZX1,
     8   BZX2,     BZX3,     BZX4,     BZX5,
     9   BZX6,     BZX7,     BZX8,     BYZ1,
     A   BYZ2,     BYZ3,     BYZ4,     BYZ5,
     B   BYZ6,     BYZ7,     BYZ8,     BZY1,
     C   BZY2,     BZY3,     BZY4,     BZY5,
     D   BZY6,     BZY7,     BZY8,     VX1,
     E   VX2,      VX3,      VX4,      VX5,
     F   VX6,      VX7,      VX8,      VY1,
     G   VY2,      VY3,      VY4,      VY5,
     H   VY6,      VY7,      VY8,      VZ1,
     I   VZ2,      VZ3,      VZ4,      VZ5,
     J   VZ6,      VZ7,      VZ8,      LBUF%PIJ,
     K   SIG_A,    LBUF%EINT,VOLN,     DXX,
     L   DYY,      DZZ,      LBUF%SIG, S1,
     M   S2,       S3,       LLT,      GBUF%OFF)
      END IF
      IF (CNS2 > ZERO) 
     .     CALL NSVIS_SM12(GBUF%OFF ,CNS2,CXX  ,VOLN ,DXX     ,
     .                      DYY     ,DZZ    ,D4    ,D5  ,D6   ,
     .                      LBUF%VOL,RHO0_1,STI   ,NEL   ) 
C----------------------------
C     INTERNAL FORCES
C----------------------------
          CALL S8EFINT3(
     1   LBUF%SIG,  PX1(1,IP), PX2(1,IP), PX3(1,IP),
     2   PX4(1,IP), PY1(1,IP), PY2(1,IP), PY3(1,IP),
     3   PY4(1,IP), PZ1(1,IP), PZ2(1,IP), PZ3(1,IP),
     4   PZ4(1,IP), PX5(1,IP), PX6(1,IP), PX7(1,IP),
     5   PX8(1,IP), PY5(1,IP), PY6(1,IP), PY7(1,IP),
     6   PY8(1,IP), PZ5(1,IP), PZ6(1,IP), PZ7(1,IP),
     7   PZ8(1,IP), BXY1,      BXY2,      BXY3,
     8   BXY4,      BXY5,      BXY6,      BXY7,
     9   BXY8,      BYX1,      BYX2,      BYX3,
     A   BYX4,      BYX5,      BYX6,      BYX7,
     B   BYX8,      BXZ1,      BXZ2,      BXZ3,
     C   BXZ4,      BXZ5,      BXZ6,      BXZ7,
     D   BXZ8,      BZX1,      BZX2,      BZX3,
     E   BZX4,      BZX5,      BZX6,      BZX7,
     F   BZX8,      BYZ1,      BYZ2,      BYZ3,
     G   BYZ4,      BYZ5,      BYZ6,      BYZ7,
     H   BYZ8,      BZY1,      BZY2,      BZY3,
     I   BZY4,      BZY5,      BZY6,      BZY7,
     J   BZY8,      BXX1,      BXX2,      BXX3,
     K   BXX4,      BXX5,      BXX6,      BXX7,
     L   BXX8,      BYY1,      BYY2,      BYY3,
     M   BYY4,      BYY5,      BYY6,      BYY7,
     N   BYY8,      BZZ1,      BZZ2,      BZZ3,
     O   BZZ4,      BZZ5,      BZZ6,      BZZ7,
     P   BZZ8,      PXY1,      PXY2,      PXY3,
     Q   PXY4,      PXY5,      PXY6,      PXY7,
     R   PXY8,      PYX1,      PYX2,      PYX3,
     S   PYX4,      PYX5,      PYX6,      PYX7,
     T   PYX8,      PXZ1,      PXZ2,      PXZ3,
     U   PXZ4,      PXZ5,      PXZ6,      PXZ7,
     V   PXZ8,      PZX1,      PZX2,      PZX3,
     W   PZX4,      PZX5,      PZX6,      PZX7,
     X   PZX8,      PYZ1,      PYZ2,      PYZ3,
     Y   PYZ4,      PYZ5,      PYZ6,      PYZ7,
     Z   PYZ8,      PZY1,      PZY2,      PZY3,
     1   PZY4,      PZY5,      PZY6,      PZY7,
     2   PZY8,      F11,       F21,       F31,
     3   F12,       F22,       F32,       F13,
     4   F23,       F33,       F14,       F24,
     5   F34,       F15,       F25,       F35,
     6   F16,       F26,       F36,       F17,
     7   F27,       F37,       F18,       F28,
     8   F38,       VOLN,      QVIS,      ICPT,
     9   JFAC(1,IP),NEL,       ISELECT,   IDEG,
     A   SIG_A,     ISEL_V,    ISMSTR)
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
        IMAT = MXT(LFT) 
        ZR   = A_GAUSS(IR,NPTR)
        ZS   = A_GAUSS(IS,NPTS) 
        ZT   = A_GAUSS(IT,NPTT)
        CALL BASIS8 (ZR,ZS,ZT,H,PR2,PS2,PT2)
        CALL S8ZFINT_REG(
     1   NLOC_DMG,     VAR_REG(1,IP),NEL,          GBUF%OFF,
     2   VOLN,         NC1,          NC2,          NC3,
     3   NC4,          NC5,          NC6,          NC7,
     4   NC8,          PX1(1,IP),    PX2(1,IP),    PX3(1,IP),
     5   PX4(1,IP),    PX5(1,IP),    PX6(1,IP),    PX7(1,IP),
     6   PX8(1,IP),    PY1(1,IP),    PY2(1,IP),    PY3(1,IP),
     7   PY4(1,IP),    PY5(1,IP),    PY6(1,IP),    PY7(1,IP),
     8   PY8(1,IP),    PZ1(1,IP),    PZ2(1,IP),    PZ3(1,IP),
     9   PZ4(1,IP),    PZ5(1,IP),    PZ6(1,IP),    PZ7(1,IP),
     A   PZ8(1,IP),    IMAT,         H,            WI,
     B   IP,           ITASK,        DT2T,         GBUF%VOL,
     C   NFT)
      ENDIF
c
       DO I=LFT,LLT
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
         IF(JTHE < 0) THEN
           CALL S8ETHERM(
     1   PM,       MXT,      VOLN,     NI,
     2   NC1,      NC2,      NC3,      NC4,
     3   NC5,      NC6,      NC7,      NC8,
     4   PX1(1,IP),PX2(1,IP),PX3(1,IP),PX4(1,IP),
     5   PY1(1,IP),PY2(1,IP),PY3(1,IP),PY4(1,IP),
     6   PZ1(1,IP),PZ2(1,IP),PZ3(1,IP),PZ4(1,IP),
     7   PX5(1,IP),PX6(1,IP),PX7(1,IP),PX8(1,IP),
     8   PY5(1,IP),PY6(1,IP),PY7(1,IP),PY8(1,IP),
     9   PZ5(1,IP),PZ6(1,IP),PZ7(1,IP),PZ8(1,IP),
     A   DT1,      TEMP,     TEMPEL,   DIE,
     B   THEM,     GBUF%OFF, LBUF%OFF, PARTSAV,
     C   IPARTS,   LBUF%VOL, NEL)
          ENDIF  
        ENDDO    !  IT=1,NPTT
       ENDDO     !  IS=1,NPTS
      ENDDO      !  IR=1,NPTR
C-----------End integrating points-----
c
      IF (IOFFS == 1) THEN
       DO I=LFT,LLT
          IF (OFFS(I)<=TWO) GBUF%OFF(I)=OFFS(I)
       END DO
c-------------------
       DO IR=1,NPTR
        DO IS=1,NPTS
         DO IT=1,NPTT
            LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
          IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
          DO I=LFT,LLT
              IF (GBUF%OFF(I) > ONE) LBUF%OFF(I)=GBUF%OFF(I)
          END DO
         END DO 
        END DO 
       END DO
      END IF   ! IOFFS == 1
      IF(ICPT==1.AND.ISMSTR/=10.AND.ISMSTR/=12) THEN
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
      CALL SMALLB3(GBUF%OFF,OFFG,NEL,ISMSTR)
      CALL SMALLGEO3(NGL, GBUF%OFF ,VOLG ,DELTAX, GBUF%VOL ,ITET, NEL, ISMSTR,DT )
      IF (ISMSTR==11.OR.ISMSTR==12.OR.(JCVT == 0 .AND. ISMSTR>0)) THEN
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
     .              NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8, NEL  )
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
      END IF
      IF(IINT==2.AND.MTN==1.AND.ISMSTR/=1.AND.ISMSTR/=11) THEN
         DO I=LFT,LLT
          CAQ=GBUF%RHO(I)*VOLG(I)**THIRD
          FLD(I)=FOURTH*EM02*CAQ*CXX(I)*OFFG(I)
          IF (ISMSTR==12.AND.GBUF%OFF(I) > ONE) FLD(I)=ZERO
         ENDDO
        CALL S8EVIS3(
     1   VZ1,     VZ2,     VZ3,     VZ4,
     2   VZ5,     VZ6,     VZ7,     VZ8,
     3   VX1,     VX2,     VX3,     VX4,
     4   VX5,     VX6,     VX7,     VX8,
     5   VY1,     VY2,     VY3,     VY4,
     6   VY5,     VY6,     VY7,     VY8,
     7   F31,     F11,     F21,     F32,
     8   F12,     F22,     F33,     F13,
     9   F23,     F34,     F14,     F24,
     A   F35,     F15,     F25,     F36,
     B   F16,     F26,     F37,     F17,
     C   F27,     F38,     F18,     F28,
     D   PX1H1,   PX1H2,   PX1H3,   PX1H4,
     E   PX2H1,   PX2H2,   PX2H3,   PX2H4,
     F   PX3H1,   PX3H2,   PX3H3,   PX3H4,
     G   PX4H1,   PX4H2,   PX4H3,   PX4H4,
     H   JT_1,    JR_1,    JS_1,    NU0,
     I   FLD,     NEL)
      ENDIF
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
     .       GBUF%FILL,XGXA, XGYA, XGZA,
     .       XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
           ENDIF
      ENDIF
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      IF (JCVT /= 0) THEN
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
