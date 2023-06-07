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
Chd|  SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ALEFLOW                       source/ale/porous/aleflow.F   
Chd|        ALEFVM_EPSDOT                 source/ale/alefvm/alefvm_epsdot.F
Chd|        ALEFVM_FRESET                 source/ale/alefvm/alefvm_freset.F
Chd|        ALEFVM_GRAVITY                source/ale/alefvm/alefvm_gravity.F
Chd|        ALEFVM_GRAVITY_INT22          source/ale/alefvm/alefvm_gravity_int22.F
Chd|        ALEFVM_STRESS                 source/ale/alefvm/alefvm_stress.F
Chd|        ALEFVM_STRESS_INT22           source/ale/alefvm/alefvm_stress_int22.F
Chd|        ALEFVM_TFEXT                  source/ale/alefvm/alefvm_tfext.F
Chd|        AMASS3                        source/elements/solid/solide/amass3.F
Chd|        AMASS3F                       source/elements/solid/solide/amass3f.F
Chd|        AMASS3P                       source/elements/solid/solide/amass3p.F
Chd|        AMASS3PF                      source/elements/solid/solide/amass3pf.F
Chd|        AMOMT3                        source/elements/solid/solide/amomt3.F
Chd|        BOLTST                        source/elements/solid/solide/boltst.F
Chd|        CHECK_OFF_ALE                 source/elements/solid/solide/check_off_ale.F
Chd|        DELTAX22                      source/interfaces/int22/deltax22.F
Chd|        EMOMT3B                       source/elements/solid/solide/emomt3b.F
Chd|        EPXLE3                        source/elements/solid/solide/epxle3.F
Chd|        FCUMU3                        source/elements/solid/solide/fcumu3.F
Chd|        FCUMU3P                       source/elements/solid/solide/fcumu3p.F
Chd|        FDEFO3                        source/elements/solid/solide/fdefo3.F
Chd|        FDERI3                        source/elements/solid/solide/fderi3.F
Chd|        FDLEN3                        source/elements/solid/solide/fdlen3.F
Chd|        FE_CLOSE                      source/elements/solid/solide/fe_close.F
Chd|        FFINT3                        source/elements/solid/solide/ffint3.F
Chd|        FHVIS3                        source/elements/solid/solide/fhvis3.F
Chd|        FRHO3                         source/elements/solid/solide/frho3.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MOD_CLOSE                     source/elements/solid/solide/mod_close.F
Chd|        S11DEFO3                      source/elements/solid/solide/s11defo3.F
Chd|        S11FX3                        source/elements/solid/solide/s11fx3.F
Chd|        S8SAV12                       source/elements/solid/solide/s8sav12.F
Chd|        S8SAV3                        source/elements/solid/solide/s8sav3.F
Chd|        S8SAVSR3                      source/elements/solid/solide/s8savsr3.F
Chd|        S8UPD11T12                    source/elements/solid/solide/s8upd11t12.F
Chd|        SBILAN                        source/elements/solid/solide/sbilan.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SCOOR3_FVM                    source/ale/alefvm/scoor3_fvm.F
Chd|        SCOOR_CP2SP                   source/elements/solid/solidez/scoor_cp2sp.F
Chd|        SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3p.F
Chd|        SDEFO3                        source/elements/solid/solide/sdefo3.F
Chd|        SDEFOT3                       source/elements/solid/solide/sdefot3.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        SDERIT3                       source/elements/solid/solide/sderit3.F
Chd|        SDERITO3                      source/elements/solid/solide/sderito3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SFINT3                        source/elements/solid/solide/sfint3.F
Chd|        SFINT3B                       source/elements/solid/solide/sfint3b.F
Chd|        SFINT_REG                     source/elements/solid/solide/sfint_reg.F
Chd|        SGCOOR3                       source/elements/solid/solide/sgcoor3.F
Chd|        SGEODEL3                      source/elements/solid/solide/sgeodel3.F
Chd|        SHVIS3                        source/elements/solid/solide/shvis3.F
Chd|        SMALLA3                       source/elements/solid/solide/smalla3.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SMALLGEO3                     source/elements/solid/solide/smallgeo3.F
Chd|        SORDEF12                      source/elements/solid/solidez/sordef12.F
Chd|        SORDEFT12                     source/elements/solid/solidez/sordeft12.F
Chd|        SORDEFT3                      source/elements/solid/solidez/sordeft3.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR12                      source/elements/solid/solide/srcoor12.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SRMALLA11                     source/elements/solid/solide/srmalla11.F
Chd|        SRMALLA3                      source/elements/solid/solide/srmall3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SROTO12_SIG                   source/elements/solid/solidez/sroto12_sig.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        SRROTADP                      source/elements/solid/solide/srrotadp.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STHERM                        source/elements/solid/solide/stherm.F
Chd|        STOPTIME                      source/system/timer.F         
Chd|        VOLN22                        source/interfaces/int22/voln22.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SFORC3(ELBUF_TAB,NG       ,
     2                  PM       ,GEO      ,IXS      ,X       ,NV46    ,
     3                  A        ,V        ,MS       ,W       ,FLUX    ,
     4                  FLU1     ,VEUL     ,FV       ,ALE_CONNECT   ,IPARG   ,
     5                  TF       ,NPF      ,BUFMAT   ,PARTSAV ,ITAB    ,
     6                  DT2T     ,NELTST   ,ITYPTST  ,STIFN   ,FSKY    ,
     7                  IADS     ,OFFSET   ,EANI     ,IPARTS   ,
     8                  F11      ,F21      ,F31      ,F12     ,F22     ,
     9                  F32      ,F13      ,F23      ,F33     ,F14     ,
     A                  F24      ,F34      ,F15      ,F25     ,F35     ,
     B                  F16      ,F26      ,F36      ,F17     ,F27     ,
     C                  F37      ,F18      ,F28      ,F38     ,NEL     ,
     D                  FSKYM    ,MSNF     ,ISKY     ,FSKYI   ,
     E                  NVC      ,IPM      ,IGEO     ,AR      ,VR      ,
     F                  IN       ,FR_WAVE  ,DR       ,BUFVOIS ,
     G                  ITASK    ,QMV      ,ISTRAIN  ,TEMP    ,FTHE    ,
     H                  FTHESKY  ,IEXPAN   ,GRESAV   ,GRTH    ,IGRTH   ,
     I                  MSSA     ,DMELS    ,TABLE    ,PHI1    ,PHI2    ,
     J                  VF       ,AF       ,DF       ,WF      ,FFSKY   ,
     K                  AFGLOB   ,MSF      ,IPARG1   ,XDP     ,
     M                  POR      ,ICONTACT ,IFOAM    ,VOLN    ,CONDN   ,
     N                  CONDNSKY ,AGRAV    ,IGRV     ,LGRAV   ,SENSORS ,
     O                  SKEW     ,NALE     ,D        ,IOUTPRT ,NLOC_DMG,
     Q                  MATPARAM_TAB,H3D_STRAIN,DT   )     
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
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "inter22.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARG,IPARTS,NPF,NALE
      INTEGER,INTENT(IN) :: NG,NEL
      INTEGER IXS(NIXS,*), IADS(8,*),IPM(NPROPMI,*),
     .        IGEO(NPROPGI,*),GRTH(*),IGRTH(*),IPARG1(*),IFOAM(*),
     .        ICONTACT(*),ISKY(*),ITAB(*)
      INTEGER NELTST,ITYPTST,OFFSET,NVC,ITASK,
     .        ISTRAIN, IEXPAN, NV46,IOUTPRT,H3D_STRAIN
C
      DOUBLE PRECISION :: XDP(3,*)
C          
      my_real :: DT2T
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), A(*), V(*), MS(*), W(*), 
     .   FLUX(6,*), FSKYM(*),
     .   FLU1(*), VEUL(*), FV(*), TF(*), BUFMAT(*),
     .   PARTSAV(*),STIFN(*), FSKY(*),EANI(*),
     .   F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),
     .   F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .   F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),
     .   F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),
     .   F15(MVSIZ),F25(MVSIZ),F35(MVSIZ),
     .   F16(MVSIZ),F26(MVSIZ),F36(MVSIZ),
     .   F17(MVSIZ),F27(MVSIZ),F37(MVSIZ),
     .   F18(MVSIZ),F28(MVSIZ),F38(MVSIZ),
     .   TEMP(*), FTHE(*),FTHESKY(*),GRESAV(*), MSSA(*), DMELS(*), 
     .   PHI1(*),PHI2(*),VF(3,*) ,D(3,*),
     .   AF(3,*), FFSKY(3,*),DF(3,*),WF(3,*),MSF(*),AFGLOB(3,*),
     .   POR(*),VOLN(MVSIZ), CONDN(*),CONDNSKY(*)
C
      my_real :: BUFVOIS(6,*) ! Neighbour element numbers (ALE/CFD)
      my_real :: MSNF(*),AR(*),VR(3,*),IN(*),FR_WAVE(*),DR(3,*),QMV(12,*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB 
C
      my_real :: P(MVSIZ),FSKYI(LSKYI,NFSKYI)
C
      INTEGER :: IGRV(*), LGRAV(*)
      my_real :: AGRAV(*),SKEW(LSKEW,*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE (DT_),   INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,LCO,NF1,IFLAG,NUVAR,IBID,IPTR,IPTS,IPTT,
     .        ILAY,ITET,IFVM_SKIP,NNOD
      INTEGER NBF1,NBF2,NBF3, IALEFVM_MAT,II(6)
C
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),INOD(8)
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . B1(MVSIZ)  , B2(MVSIZ)  , B3(MVSIZ)  ,
     . B4(MVSIZ)  , B5(MVSIZ)  , B6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ),
     . JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ),
     . VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ),SSP_EQ(MVSIZ),
     . AIRE(MVSIZ),FQVIS(MVSIZ), FCXX(MVSIZ)
C
      my_real :: STI(MVSIZ), WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ)
      my_real :: MUVOID(MVSIZ)  ! used in case of SPH 
      my_real :: SIGY(MVSIZ),ET(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
C
C Connectivities
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
C
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8),VOLDP(MVSIZ)
C
      my_real
     .   OFF(MVSIZ) , RHOO(MVSIZ),
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
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),
     .  PX1H1(MVSIZ),PX2H1(MVSIZ),PX3H1(MVSIZ),PX4H1(MVSIZ),
     .  PX1H2(MVSIZ),PX2H2(MVSIZ),PX3H2(MVSIZ),PX4H2(MVSIZ),
     .  PX1H3(MVSIZ),PX2H3(MVSIZ),PX3H3(MVSIZ),PX4H3(MVSIZ),
     .  VDX1(MVSIZ),VDX2(MVSIZ),VDX3(MVSIZ),VDX4(MVSIZ),
     .  VDX5(MVSIZ),VDX6(MVSIZ),VDX7(MVSIZ),VDX8(MVSIZ),
     .  VDY1(MVSIZ),VDY2(MVSIZ),VDY3(MVSIZ),VDY4(MVSIZ),
     .  VDY5(MVSIZ),VDY6(MVSIZ),VDY7(MVSIZ),VDY8(MVSIZ),
     .  VDZ1(MVSIZ),VDZ2(MVSIZ),VDZ3(MVSIZ),VDZ4(MVSIZ),
     .  VDZ5(MVSIZ),VDZ6(MVSIZ),VDZ7(MVSIZ),VDZ8(MVSIZ),
     .  VGXA(MVSIZ),VGYA(MVSIZ),VGZA(MVSIZ), VGA2(MVSIZ),
     .  DXY(MVSIZ),DYX(MVSIZ),DYZ(MVSIZ),
     .  DZY(MVSIZ),DZX(MVSIZ),DXZ(MVSIZ),
     .  G1X(MVSIZ),G2X(MVSIZ),G3X(MVSIZ),
     .  G1Y(MVSIZ),G2Y(MVSIZ),G3Y(MVSIZ),
     .  G1Z(MVSIZ),G2Z(MVSIZ),G3Z(MVSIZ),
     .  R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),                        
     .  R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),                        
     .  R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),HH(MVSIZ),GAMA(MVSIZ,6),
     .  VX0(MVSIZ,8),VY0(MVSIZ,8),VZ0(MVSIZ,8),
     .  MFXX(MVSIZ),MFXY(MVSIZ),MFYX(MVSIZ),
     .  MFYY(MVSIZ),MFYZ(MVSIZ),MFZY(MVSIZ),
     .  MFZZ(MVSIZ),MFZX(MVSIZ),MFXZ(MVSIZ),
     .  DFE(MVSIZ,3),SIGF(MVSIZ,6),OFFG0(MVSIZ),
     .  XGXA(MVSIZ),XGYA(MVSIZ),XGZA(MVSIZ),
     .  XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),
     .  XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ)
C
      my_real
     . VXR(MVSIZ),VYR(MVSIZ),VZR(MVSIZ),FR_WAV(MVSIZ),
     . SXY(MVSIZ),SYX(MVSIZ),SYZ(MVSIZ),SZY(MVSIZ),SZX(MVSIZ),
     . SXZ(MVSIZ),TEMPEL(MVSIZ),THEM(MVSIZ,8),DIE(MVSIZ)
C
      my_real
     .  MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .  MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .  MX3(MVSIZ),MY3(MVSIZ),MZ3(MVSIZ),
     .  MX4(MVSIZ),MY4(MVSIZ),MZ4(MVSIZ),
     .  MX5(MVSIZ),MY5(MVSIZ),MZ5(MVSIZ),
     .  MX6(MVSIZ),MY6(MVSIZ),MZ6(MVSIZ),
     .  MX7(MVSIZ),MY7(MVSIZ),MZ7(MVSIZ),
     .  MX8(MVSIZ),MY8(MVSIZ),MZ8(MVSIZ)
C
      my_real
     .   N1X(MVSIZ), N2X(MVSIZ), N3X(MVSIZ),
     .   N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ),
     .   N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ),
     .   N4X(MVSIZ), N5X(MVSIZ), N6X(MVSIZ),
     .   N4Y(MVSIZ), N5Y(MVSIZ), N6Y(MVSIZ),
     .   N4Z(MVSIZ), N5Z(MVSIZ), N6Z(MVSIZ)
C
      my_real
     .  FVX1(MVSIZ),FVX2(MVSIZ),FVX3(MVSIZ),FVX4(MVSIZ),
     .  FVX5(MVSIZ),FVX6(MVSIZ),FVX7(MVSIZ),FVX8(MVSIZ),
     .  FVY1(MVSIZ),FVY2(MVSIZ),FVY3(MVSIZ),FVY4(MVSIZ),
     .  FVY5(MVSIZ),FVY6(MVSIZ),FVY7(MVSIZ),FVY8(MVSIZ),
     .  FVZ1(MVSIZ),FVZ2(MVSIZ),FVZ3(MVSIZ),FVZ4(MVSIZ),
     .  FVZ5(MVSIZ),FVZ6(MVSIZ),FVZ7(MVSIZ),FVZ8(MVSIZ),
     .  FVDX1(MVSIZ),FVDX2(MVSIZ),FVDX3(MVSIZ),FVDX4(MVSIZ),
     .  FVDX5(MVSIZ),FVDX6(MVSIZ),FVDX7(MVSIZ),FVDX8(MVSIZ),
     .  FVDY1(MVSIZ),FVDY2(MVSIZ),FVDY3(MVSIZ),FVDY4(MVSIZ),
     .  FVDY5(MVSIZ),FVDY6(MVSIZ),FVDY7(MVSIZ),FVDY8(MVSIZ),
     .  FVDZ1(MVSIZ),FVDZ2(MVSIZ),FVDZ3(MVSIZ),FVDZ4(MVSIZ),
     .  FVDZ5(MVSIZ),FVDZ6(MVSIZ),FVDZ7(MVSIZ),FVDZ8(MVSIZ),
     .  FDXX(MVSIZ) ,FDXY(MVSIZ)  ,FDXZ(MVSIZ)  ,FDYX(MVSIZ),
     .  FDYY(MVSIZ) ,FDYZ(MVSIZ)  ,FDZX(MVSIZ)  ,FDZY(MVSIZ),
     .  FDZZ(MVSIZ) ,FD4(MVSIZ)   ,FD5(MVSIZ)   ,FD6(MVSIZ) ,
     .  FWXX(MVSIZ) ,FWYY(MVSIZ) ,FWZZ(MVSIZ)  ,FVXR(MVSIZ),
     .  FVYR(MVSIZ) ,FVZR(MVSIZ) ,FDVOL(MVSIZ),FVOLN(MVSIZ),
     .  FVD2(MVSIZ)
C
      my_real
     .   FF11(MVSIZ),FF21(MVSIZ),FF31(MVSIZ),
     .   FF12(MVSIZ),FF22(MVSIZ),FF32(MVSIZ),
     .   FF13(MVSIZ),FF23(MVSIZ),FF33(MVSIZ),
     .   FF14(MVSIZ),FF24(MVSIZ),FF34(MVSIZ),
     .   FF15(MVSIZ),FF25(MVSIZ),FF35(MVSIZ),
     .   FF16(MVSIZ),FF26(MVSIZ),FF36(MVSIZ),
     .   FF17(MVSIZ),FF27(MVSIZ),FF37(MVSIZ),
     .   FF18(MVSIZ),FF28(MVSIZ),FF38(MVSIZ),
     .   FVDX(MVSIZ),FVDY(MVSIZ),FVDZ(MVSIZ),
     .   FRHO0(MVSIZ),ALPHA(MVSIZ),
     .   FPX1H1(MVSIZ),FPX1H2(MVSIZ), FPX1H3(MVSIZ), 
     .   FPX2H1(MVSIZ),FPX2H2(MVSIZ), FPX2H3(MVSIZ),
     .   FPX3H1(MVSIZ),FPX3H2(MVSIZ), FPX3H3(MVSIZ), 
     .   FPX4H1(MVSIZ),FPX4H2(MVSIZ), FPX4H3(MVSIZ), 
     .   FN1X(MVSIZ), FN2X(MVSIZ), FN3X(MVSIZ),
     .   FN1Y(MVSIZ), FN2Y(MVSIZ), FN3Y(MVSIZ),
     .   FN1Z(MVSIZ), FN2Z(MVSIZ), FN3Z(MVSIZ),
     .   FN4X(MVSIZ), FN5X(MVSIZ), FN6X(MVSIZ),
     .   FN4Y(MVSIZ), FN5Y(MVSIZ), FN6Y(MVSIZ),
     .   FN4Z(MVSIZ), FN5Z(MVSIZ), FN6Z(MVSIZ),   
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FX5(MVSIZ), FX6(MVSIZ), FX7(MVSIZ), FX8(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FY5(MVSIZ), FY6(MVSIZ), FY7(MVSIZ), FY8(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   FZ5(MVSIZ), FZ6(MVSIZ), FZ7(MVSIZ), FZ8(MVSIZ)
C
      my_real
     .  FPX1(MVSIZ),FPX2(MVSIZ),FPX3(MVSIZ),FPX4(MVSIZ),
     .  FPY1(MVSIZ),FPY2(MVSIZ),FPY3(MVSIZ),FPY4(MVSIZ),
     .  FPZ1(MVSIZ),FPZ2(MVSIZ),FPZ3(MVSIZ),FPZ4(MVSIZ),
     .  FDELTAX(MVSIZ) ,FLUX0(MVSIZ,6),FLU01(MVSIZ)
      my_real, DIMENSION(:), ALLOCATABLE :: VAR_REG
      my_real :: DT2T_lay(2)
      my_real :: CONDE(MVSIZ),AMU(MVSIZ),DIVDE(MVSIZ)
      my_real, DIMENSION(:), POINTER :: EINT
C
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C
      INTEGER IALEFVM_FLG, IFVM22, IMAT, INLOC, L_NLOC, J, IPOS(8)
C
      INTEGER IBOLTP,NBPRELD,ISM12_11  ! IBOLTP Flag Bolt Preloading
      my_real, DIMENSION(:), POINTER :: BPRELD
      my_real, DIMENSION(:), POINTER :: DNL
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_TAB(NG)%GBUF
      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
      MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
      ISM12_11 = ELBUF_TAB(NG)%BUFLY(1)%L_SIGL
      INLOC = IPARG(78)
      ALLOCATE(VAR_REG(NEL))
      TEMPEL(1:MVSIZ) = ZERO
C
      IBOLTP = IPARG1(72) !IPARG(72,NG)
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
C
      IBID  = 0
      NF1=NFT+1
C
      IF (JCVT == 0) THEN
C Gather nodal variables (coordinates, velocities) updated lagrangian formulation
       IALEFVM_MAT = IPM(251,IXS(1,NF1))
       IF(IALEFVM_MAT<=1)THEN
C FEM velocity
         CALL SCOOR3(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     .     X1, X2, X3, X4, X5, X6, X7, X8,
     .     Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .     Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .     VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .     VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .     VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .     VDX1, VDX2, VDX3, VDX4, VDX5, VDX6, VDX7, VDX8,
     .     VDY1, VDY2, VDY3, VDY4, VDY5, VDY6, VDY7, VDY8,
     .     VDZ1, VDZ2, VDZ3, VDZ4, VDZ5, VDZ6, VDZ7, VDZ8,
     .     VDX,VDY,VDZ,VD2,VIS,GBUF%OFF,OFF,GBUF%SMSTR,GBUF%RHO,
     .     RHOO,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .     VR,VXR,VYR,VZR,FR_WAVE,FR_WAV,
     .     XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .     YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .     ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .     XDP,IPARG, NG , NEL)
       ELSE
C FVM velocity
         CALL SCOOR3_FVM(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     .     X1, X2, X3, X4, X5, X6, X7, X8,
     .     Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .     Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .     VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .     VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .     VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .     VDX1, VDX2, VDX3, VDX4, VDX5, VDX6, VDX7, VDX8,
     .     VDY1, VDY2, VDY3, VDY4, VDY5, VDY6, VDY7, VDY8,
     .     VDZ1, VDZ2, VDZ3, VDZ4, VDZ5, VDZ6, VDZ7, VDZ8,
     .     VDX,VDY,VDZ,VD2,VIS,GBUF%OFF,OFF,GBUF%SMSTR,GBUF%RHO,
     .     RHOO,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .     VR,VXR,VYR,VZR,FR_WAVE,FR_WAV,
     .     XD1, XD2 , XD3, XD4, XD5, XD6, XD7, XD8,
     .     YD1, YD2 , YD3, YD4, YD5, YD6, YD7, YD8,
     .     ZD1, ZD2 , ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .     XDP, IPARG, NG , NEL, GBUF%MOM,GBUF%TAG22,GBUF%VOL)
       ENDIF
      ELSE
C Gather nodal variables Belytschko co-rotational formulation
       CALL SRCOOR3(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
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
     .   XGXA2,XGYA2  ,XGZA2 ,XGXYA ,XGYZA ,XGZXA,IPARG1,
     .   GBUF%GAMA_R) 
      ENDIF  ! JCVT == 0
C
      IF ((ISMSTR >= 10.AND.ISMSTR <= 12).AND.JLAG > 0) THEN
C Gather initial coordinates total strain formulation
        CALL SGCOOR3(
     1   TT,        8,         X,         IXS(1,NF1),
     2   X0,        Y0,        Z0,        VX0,
     3   VY0,       VZ0,       GBUF%SMSTR,D,
     4   GBUF%OFF,  OFFG0,     NEL,       XDP,
     5   MTN,       ISMSTR)
C
       IF (ISMSTR == 11) THEN
        IF (JCVT /= 0) THEN
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
C Compute shape functions partial derivatives Pij total strain formulation
        CALL SDERIT3(OFF,VOLN,NGL,
     .   X0(1,1), X0(1,2), X0(1,3), X0(1,4), 
     .   X0(1,5), X0(1,6), X0(1,7), X0(1,8),
     .   Y0(1,1), Y0(1,2), Y0(1,3), Y0(1,4), 
     .   Y0(1,5), Y0(1,6), Y0(1,7), Y0(1,8),
     .   Z0(1,1), Z0(1,2), Z0(1,3), Z0(1,4), 
     .   Z0(1,5), Z0(1,6), Z0(1,7), Z0(1,8),
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,     
     .   PX1H1, PX1H2, PX1H3,
     .   PX2H1, PX2H2, PX2H3,
     .   PX3H1, PX3H2, PX3H3,
     .   PX4H1, PX4H2, PX4H3,
     .   JAC1,JAC2,JAC3,
     .   JAC4,JAC5,JAC6,
     .   NEL,JHBE)
       ELSE
        IF (IDTMIN(1)==3.AND.ISMSTR == 12.AND.JCVT == 0.AND.ISM12_11==0) THEN
C Compute Rij local reference frame for ISMSTR=12 (OFFG>1)
          CALL SRCOOR12(
     1   GBUF%OFF,X,       XDP,     NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     R11,
     4   R12,     R13,     R21,     R22,
     5   R23,     R31,     R32,     R33,
     6   NEL,     JHBE)
        END IF 
C Compute Pij in global system using inverse Jacobian matrix JAC_I for ISMSTR=10 
        CALL SDERITO3(OFF,VOLN,
     .    PX1, PX2, PX3, PX4,
     .    PY1, PY2, PY3, PY4,
     .    PZ1, PZ2, PZ3, PZ4,
     .    GBUF%JAC_I,NEL)
       END IF
C Compute strain rates MFij total strain formulation
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
C
       IF (JCVT /= 0 .AND.(ISMSTR == 10 .OR.ISMSTR == 12)) THEN
          CALL SORDEFT3(LFT,LLT,MFXX, MFXY, MFXZ, MFYX, MFYY, MFYZ,
     .         MFZX, MFZY, MFZZ,
     .         R11, R21, R31, R12, R22, R32, R13, R23, R33)
       ELSEIF (IDTMIN(1)==3.AND.ISMSTR == 12.AND.JCVT==0.AND.ISM12_11==0) THEN
          CALL SORDEFT12(LFT,LLT,MFXX, MFXY, MFXZ,
     .         MFYX, MFYY, MFYZ,
     .         MFZX, MFZY, MFZZ,
     .         R11, R21, R31, R12, R22, R32, R13, R23, R33,OFFG0)
       END IF
      ENDIF ! End total strain formulation
C
      IF(JALE+JLAG /= 0)THEN
C Lagrangian or Arbitrary Euler Lagrange simulation
       IF(JCLOSE /= 0) THEN
C ALE option /ALE/CLOSE
         CALL MOD_CLOSE(
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
       ENDIF
C
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
C Compute shape functions partial derivatives Pij
        CALL SDERI3(
     1   OFF,       VOLN,      NGL,       XD1,
     2   XD2,       XD3,       XD4,       XD5,
     3   XD6,       XD7,       XD8,       YD1,
     4   YD2,       YD3,       YD4,       YD5,
     5   YD6,       YD7,       YD8,       ZD1,
     6   ZD2,       ZD3,       ZD4,       ZD5,
     7   ZD6,       ZD7,       ZD8,       PX1,
     8   PX2,       PX3,       PX4,       PY1,
     9   PY2,       PY3,       PY4,       PZ1,
     A   PZ2,       PZ3,       PZ4,       PX1H1,
     B   PX1H2,     PX1H3,     PX2H1,     PX2H2,
     C   PX2H3,     PX3H1,     PX3H2,     PX3H3,
     D   PX4H1,     PX4H2,     PX4H3,     JAC1,
     E   JAC2,      JAC3,      JAC4,      JAC5,
     F   JAC6,      GBUF%SMSTR,GBUF%OFF,  NEL,
     G   VOLDP,     JHBE,      ISMSTR,    JLAG)
       END IF
C Compute characteristic length
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
C
      ELSEIF(JEUL /= 0)THEN
C Eulerian simulation
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
C
C SuperCell characteristic length for interface type 22   
       CALL DELTAX22(
     1   GBUF%VOL,  VEUL,      VOLN,      DELTAX,
     2   GBUF%TAG22,IXS,       LFT,       LLT,
     3   JALE,      JEUL)
      ENDIF
C
      IF(INT22>0)THEN
C SDERI3 is updating GBUF%VOL with uncut brick volume 
C One must have polyhedra maincell volume for interface type 22
        IF(I22_ALEUL == 1)THEN
          CALL VOLN22(
     1   VOLN,      GBUF%TAG22,IXS,       LFT,
     2   LLT,       JALE,      JEUL)
        ENDIF
      ENDIF
C
      IF(JALE+JEUL > 0 .AND. IPARG1(64)==1)THEN
C Compute strain rates Dij (silent boundary element)
        CALL S11DEFO3(
     1   PM,         V,          VEUL,       X,
     2   IXS,        ALE_CONNECT,DXX,        DXY,
     3   DXZ,        DYX,        DYY,        DYZ,
     4   DZX,        DZY,        DZZ,        D4,
     5   D5,         D6,         WXX,        WYY,
     6   WZZ,        BUFMAT,     NEL,        NFT)
      ELSE
C Compute strain rates Dij
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
     G   WXX,     WYY,     WZZ,     VXR,
     H   VYR,     VZR,     IXS,     NEL,
     I   NFT,     ISMSTR,  JEUL,    JHBE,
     J   JCVT,    ISROT)
C
        IF (IDTMIN(1)==3.AND.ISMSTR == 12.AND.ISM12_11==0.AND.JCVT==0) THEN
          CALL SORDEF12(LFT,LLT,DXX, DYY, DZZ,
     .         D4, D5, D6,
     .         R11, R21, R31, R12, R22, R32, R13, R23, R33,GBUF%OFF)
        ENDIF    
      ENDIF 
C
      CALL S11FX3(
     1   PM,         FLUX(1,NF1),ALE_CONNECT,IXS,
     2   IPM,        BUFMAT,     NEL,        NFT,
     3   JALE,       JEUL)
C
      DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))  
C Compute new density
      CALL SRHO3(
     1   PM,         GBUF%VOL,   GBUF%RHO,   GBUF%EINT,
     2   DIVDE,      FLUX(1,NF1),FLU1(NF1),  VOLN,
     3   DVOL,       NGL,        MXT,        OFF,
     4   IPARG1(64), GBUF%TAG22, VOLDP,      LBUF%VOL0DP,
     5   AMU,        GBUF%OFF,   NEL,        MTN,
     6   JALE,       ISMSTR,     JEUL,       JLAG)
C
      IF (ISMSTR == 12.AND.IDTMIN(1)==3.AND.ISM12_11==0.AND.JCVT==0) THEN
         CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .           R11, R21, R31, R12, R22, R32, R13, R23, R33,OFFG0)
      ENDIF
C
      IF (JCVT == 0) THEN
C Jaumann objective stress tensor
        CALL SROTA3(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
C Small Strain formulation
        CALL SMALLA3(
     1   GBUF%SMSTR,GBUF%OFF,  OFF,       WXX,
     2   WYY,       WZZ,       NEL,       ISMSTR,
     3   JLAG)
      ELSE  
C Stress tensor co-rotational formulation
        CALL SRMALLA3(GBUF%SIG,S1,S2,S3,S4,S5,S6,
     .                GBUF%OFF,OFF,NEL)
C Small strain formulation
        CALL SRMALLA11(
     1   GBUF%SMSTR,GBUF%OFF,  WXX,       WYY,
     2   WZZ,       R11,       R12,       R13,
     3   R21,       R22,       R23,       R31,
     4   R32,       R33,       NEL,       ISMSTR)
      ENDIF
C
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
C Update the reference configuration (possible future change to small strain option)
C Total strain option doesn't change the reference configuration
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
       ENDIF
      END IF
C
      IF(JTHE < 0) THEN 
C Element Temperature
        DO I=1,NEL
           TEMPEL(I) = ONE_OVER_8 *(  TEMP(NC1(I)) + TEMP(NC2(I))  
     .                              + TEMP(NC3(I)) + TEMP(NC4(I)) 
     .                              + TEMP(NC5(I)) + TEMP(NC6(I)) 
     .                              + TEMP(NC7(I)) + TEMP(NC8(I)))       
        ENDDO
      ENDIF
C
      IF (INLOC > 0) THEN
C Compute Regularized non local variable in Gauss point
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
      IF (JALE == 3 .AND. JLAG > 0) THEN
C Law 77 internal gas flow of open cell solved with ALE formulation
C AFGLOB ----> ZERO is not activated
        CALL ALEFLOW (
     .     PM     ,IPM   ,MXT    ,X      ,IPARG ,
     .     MBUF   ,NEL   ,NPF    ,TF     ,MBUF%VAR      ,
     .     DF     ,IXS   ,ALE_CONNECT    ,VF     ,AFGLOB,              
     .     WF     ,DFE   ,FRHO0  ,ALPHA  ,                  
     .     NC1    ,NC2   ,NC3    ,NC4    ,NC5    ,NC6   ,              
     .     NC7    ,NC8   ,FX1    ,FX2    ,FX3    ,FX4   ,              
     .     FX5    ,FX6   ,FX7    ,FX8    ,FY1    ,FY2   ,              
     .     FY3    ,FY4   ,FY5    ,FY6    ,FY7    ,FY8   ,              
     .     FZ1    ,FZ2   ,FZ3    ,FZ4    ,FZ5    ,FZ6   ,              
     .     FZ7    ,FZ8   ,PHI1   ,PHI2   ,FLUX0  ,FLU01 ,   
     .     FVX1   ,FVX2  ,FVX3   ,FVX4   ,FVX5   ,FVX6  ,              
     .     FVX7   ,FVX8  ,FVY1   ,FVY2   ,FVY3   ,FVY4  ,              
     .     FVY5   ,FVY6  ,FVY7   ,FVY8   ,FVZ1   ,FVZ2  ,              
     .     FVZ3   ,FVZ4  ,FVZ5   ,FVZ6   ,FVZ7   ,FVZ8  ,              
     .     FVDX1  ,FVDX2 ,FVDX3  ,FVDX4  ,FVDX5  ,FVDX6 ,              
     .     FVDX7  ,FVDX8 ,FVDY1  ,FVDY2  ,FVDY3  ,FVDY4 ,              
     .     FVDY5  ,FVDY6 ,FVDY7  ,FVDY8  ,FVDZ1  ,FVDZ2 ,              
     .     FVDZ3  ,FVDZ4 ,FVDZ5  ,FVDZ6  ,FVDZ7  ,FVDZ8 ,              
     .     FVDX   ,FVDY  ,FVDZ   ,FVD2   ,NGL    ,
     .     VX1    ,VX2   ,VX3    ,VX4    ,VX5    ,VX6   ,              
     .     VX7    ,VX8   ,VY1    ,VY2    ,VY3    ,VY4   ,              
     .     VY5    ,VY6   ,VY7    ,VY8    ,VZ1    ,VZ2   ,              
     .     VZ3    ,VZ4   ,VZ5    ,VZ6    ,VZ7    ,VZ8   ,
     .     POR   ,ICONTACT ,IFOAM)    
C
        CALL FDERI3(
     1   OFF,     FVOLN,   NGL,     FX1,
     2   FX2,     FX3,     FX4,     FX5,
     3   FX6,     FX7,     FX8,     FY1,
     4   FY2,     FY3,     FY4,     FY5,
     5   FY6,     FY7,     FY8,     FZ1,
     6   FZ2,     FZ3,     FZ4,     FZ5,
     7   FZ6,     FZ7,     FZ8,     FPX1,
     8   FPX2,    FPX3,    FPX4,    FPY1,
     9   FPY2,    FPY3,    FPY4,    FPZ1,
     A   FPZ2,    FPZ3,    FPZ4,    FPX1H1,
     B   FPX1H2,  FPX1H3,  FPX2H1,  FPX2H2,
     C   FPX2H3,  FPX3H1,  FPX3H2,  FPX3H3,
     D   FPX4H1,  FPX4H2,  FPX4H3,  NEL,
     E   JHBE)
C
        CALL FDLEN3(
     1   FVOLN,   FDELTAX, FX1,     FX2,
     2   FX3,     FX4,     FX5,     FX6,
     3   FX7,     FX8,     FY1,     FY2,
     4   FY3,     FY4,     FY5,     FY6,
     5   FY7,     FY8,     FZ1,     FZ2,
     6   FZ3,     FZ4,     FZ5,     FZ6,
     7   FZ7,     FZ8,     FN1X,    FN2X,
     8   FN3X,    FN4X,    FN5X,    FN6X,
     9   FN1Y,    FN2Y,    FN3Y,    FN4Y,
     A   FN5Y,    FN6Y,    FN1Z,    FN2Z,
     B   FN3Z,    FN4Z,    FN5Z,    FN6Z,
     C   NEL)
C
C Strain flow    
        CALL FDEFO3(
     1   FPX1,    FPX2,    FPX3,    FPX4,
     2   FPY1,    FPY2,    FPY3,    FPY4,
     3   FPZ1,    FPZ2,    FPZ3,    FPZ4,
     4   FVX1,    FVX2,    FVX3,    FVX4,
     5   FVX5,    FVX6,    FVX7,    FVX8,
     6   FVY1,    FVY2,    FVY3,    FVY4,
     7   FVY5,    FVY6,    FVY7,    FVY8,
     8   FVZ1,    FVZ2,    FVZ3,    FVZ4,
     9   FVZ5,    FVZ6,    FVZ7,    FVZ8,
     A   FDXX,    FDXY,    FDXZ,    FDYX,
     B   FDYY,    FDYZ,    FDZX,    FDZY,
     C   FDZZ,    FD4,     FD5,     FD6,
     D   FWXX,    FWYY,    FWZZ,    FVXR,
     E   FVYR,    FVZR,    FQVIS,   NEL,
     F   JHBE,    JCVT,    ISROT)
C
        DO I=1,NEL                                   
           FVOLN(I) = ALPHA(I)*FVOLN(I)                
           DFE(I,1) = DFE(I,1)*FVOLN(I)                
           DFE(I,2) = DFE(I,2)*FVOLN(I)                
           DFE(I,3) = DFE(I,3)*FVOLN(I)                                                                      
        ENDDO 
C
        CALL FRHO3(MBUF%VAR,FRHO0,FDXX, FDYY, FDZZ,    
     .             FLUX0,FLU01,FVOLN,NGL,MXT,OFF ,NEL) 
      ENDIF    ! JALE == 3 .AND. JLAG > 0
C
      IALEFVM_MAT = IPM(251,IXS(1,NF1))
      IF (IALEFVM_MAT  > 1) THEN
C ALEFVM strain rates
          CALL ALEFVM_EPSDOT(
     1   IXS,     NV46,    GBUF%SIG,QVIS,
     2   VOLN,    N1X,     N2X,     N3X,
     3   N4X,     N5X,     N6X,     N1Y,
     4   N2Y,     N3Y,     N4Y,     N5Y,
     5   N6Y,     N1Z,     N2Z,     N3Z,
     6   N4Z,     N5Z,     N6Z,     DXX,
     7   DYY,     DZZ,     X,       IPM,
     8   NEL)
      ENDIF
C
      IF ((IMON_MAT==1).AND. ITASK == 0) CALL STARTIME(35,1)
C
      ILAY = 1                                                
      IPTR = 1                                                
      IPTS = 1                                                
      IPTT = 1   
C
      IF(IBOLTP /= 0) THEN
        IF(JCVT==0)
     .          CALL SROTA3(
     1   BPRELD(3*NEL+1),B1,             B2,             B3,
     2   B4,             B5,             B6,             WXX,
     3   WYY,            WZZ,            NEL,            MTN,
     4   ISMSTR)

        CALL BOLTST(IPTR  ,BPRELD,LBUF%SIG       ,TT    ,NEL   ,        
     .              NPT   ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB)
      END IF
C
C Stress computation according to material laws 
C
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
     G   BUFVOIS,     LBUF%PLA,    R3_FREE,     AMU,
     H   MFXX,        MFXY,        MFXZ,        MFYX,
     I   MFYY,        MFYZ,        MFZX,        MFZY,
     J   MFZZ,        IPM,         GAMA,        FR_WAV,
     K   DXY,         DYX,         DYZ,         DZY,
     L   DZX,         DXZ,         ISTRAIN,     TEMPEL,
     M   DIE,         IEXPAN,      ILAY,        MSSA,
     N   DMELS,       IPTR,        IPTS,        IPTT,
     O   TABLE,       FVD2,        FDELTAX,     FCXX,
     P   FQVIS,       IPARG1,      IGEO,        CONDE,
     Q   ITASK,       NLOC_DMG,    VAR_REG,     MATPARAM_TAB,
     R   H3D_STRAIN,  JPLASOL,     JSPH)
C
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
        IF (ISM12_11==0.AND.JCVT==0) THEN
          CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .           R11, R12, R13, R21, R22, R23, R31, R32, R33,OFFG0)
C
          IF (ISTRAIN == 1) THEN 
            CALL SORDEF12(LFT,LLT,DXX, DXY, DXZ,
     .         D4, D5, D6,
     .         R11, R12, R13, R21, R22, R23, R31, R32, R33,OFFG0)
          ENDIF
        END IF 
      ENDIF ! ISMSTR == 12.AND.IDTMIN(1)==3
C
      IF (ISTRAIN == 1) THEN 
C Compute Strains for output
        CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
      ENDIF
      IF ((IMON_MAT==1).AND. ITASK == 0) CALL STOPTIME(35,1)
C
      ITET = 0
      CALL SGEODEL3(NGL,GBUF%OFF,VOLN,DELTAX,GBUF%VOL,GEO(1,NGEO(1)),ITET,DT,NEL )
C Small Strain
      CALL SMALLB3(
     1   GBUF%OFF,OFF,     NEL,     ISMSTR)
      ITET = 0
      CALL SMALLGEO3(
     1   NGL,     GBUF%OFF,VOLN,    DELTAX,
     2   GBUF%VOL,ITET,    NEL,     ISMSTR,DT )
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
C
      IF (JLAG+JALE+JEUL == 0) THEN
C Energy balance thermal material
        IF (IOUTPRT>0)THEN
          IF (IPARG1(64) == 1) THEN                                    
            EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)                   
          ELSE                                                   
            EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)                   
          ENDIF                                                  
C
          IF (JCVT == 0) THEN  
            IFVM22      = 0
            IMAT        = IXS(1,LFT+NFT)
            IALEFVM_FLG = IPM(251,IMAT)
            IF(IALEFVM_FLG>=2)IFVM22 = 1
            CALL SBILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,  
     .                  VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .                  VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .                  VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .                  VOLN,IPARTS,GRESAV,GRTH,IGRTH, GBUF%OFF,
     .                  IEXPAN,GBUF%EINTTH,GBUF%FILL,GBUF%MOM,NEL,IFVM22,
     .                  X1, X2, X3, X4, X5, X6, X7, X8,
     .                  Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .                  Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .                  ITASK,IPARG1)
          ELSE
            CALL SRBILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL, 
     .                   VGXA, VGYA, VGZA, VGA2, VOLN,IPARTS,
     .                   GRESAV,GRTH,IGRTH,GBUF%OFF,IEXPAN,GBUF%EINTTH,
     .                   GBUF%FILL,XGXA, XGYA, XGZA,
     .                   XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG1)
          ENDIF
        ENDIF
        RETURN
      ENDIF
C
C Mass update
      IF (JALE == 3 .AND. JLAG == 1 .AND. ALE%GLOBAL%INCOMP == 0) THEN  ! law 77 
       IF(IPARIT == 0)THEN         
          CALL AMASS3F(
     1   MSF,     MBUF%VAR,FVOLN,   NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     MSNF,
     4   NVC,     OFF,     NEL)
        ELSE
          CALL AMASS3PF(
     1   FSKYM,   MBUF%VAR,FVOLN,   IADS,
     2   OFF,     NEL,     NFT)
        ENDIF
      ELSEIF (JALE+JEUL > 0 .AND. ALE%GLOBAL%INCOMP == 0)THEN
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
      IALEFVM_MAT = IPM(251,IXS(1,NF1))
      IFVM_SKIP=0
      IF(IALEFVM_MAT > 1)IFVM_SKIP=1
C
C Anti hourglass forces
      CALL SHVIS3(
     1   PM,       GBUF%RHO, OFF,      VX1,
     2   VX2,      VX3,      VX4,      VX5,
     3   VX6,      VX7,      VX8,      VY1,
     4   VY2,      VY3,      VY4,      VY5,
     5   VY6,      VY7,      VY8,      VZ1,
     6   VZ2,      VZ3,      VZ4,      VZ5,
     7   VZ6,      VZ7,      VZ8,      F11,
     8   F21,      F31,      F12,      F22,
     9   F32,      F13,      F23,      F33,
     A   F14,      F24,      F34,      F15,
     B   F25,      F35,      F16,      F26,
     C   F36,      F17,      F27,      F37,
     D   F18,      F28,      F38,      PX1H1,
     E   PX1H2,    PX1H3,    PX2H1,    PX2H2,
     F   PX2H3,    PX3H1,    PX3H2,    PX3H3,
     G   PX4H1,    PX4H2,    PX4H3,    VOLN,
     H   MXT,      CXX,      VIS,      VD2,
     I   DELTAX,   EANI,     NGEO,     GEO,
     J   PARTSAV,  IPARTS,   GBUF%OFF, GBUF%VOL,
     K   IPARG1,   IFVM_SKIP,NEL,      NFT,
     L   MTN,      ISMSTR,   JLAG,     JHBE)
C
      IF (JALE == 3) THEN   ! law 77
        CALL FHVIS3(
     1   PM,      MBUF%VAR,OFF,     FVX1,
     2   FVX2,    FVX3,    FVX4,    FVX5,
     3   FVX6,    FVX7,    FVX8,    FVY1,
     4   FVY2,    FVY3,    FVY4,    FVY5,
     5   FVY6,    FVY7,    FVY8,    FVZ1,
     6   FVZ2,    FVZ3,    FVZ4,    FVZ5,
     7   FVZ6,    FVZ7,    FVZ8,    FF11,
     8   FF21,    FF31,    FF12,    FF22,
     9   FF32,    FF13,    FF23,    FF33,
     A   FF14,    FF24,    FF34,    FF15,
     B   FF25,    FF35,    FF16,    FF26,
     C   FF36,    FF17,    FF27,    FF37,
     D   FF18,    FF28,    FF38,    FPX1H1,
     E   FPX1H2,  FPX1H3,  FPX2H1,  FPX2H2,
     F   FPX2H3,  FPX3H1,  FPX3H2,  FPX3H3,
     G   FPX4H1,  FPX4H2,  FPX4H3,  FVOLN,
     H   MXT,     FCXX,    VIS,     FVD2,
     I   FDELTAX, EANI,    NGEO,    GEO,
     J   PARTSAV, IPARTS,  NEL,     NFT,
     K   JLAG,    JHBE)
C
      ENDIF 
C
      IF (JCLOSE /= 0) 
     .  CALL FE_CLOSE(
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
      IALEFVM_MAT = IPM(251,IXS(1,NF1))
      IF(IALEFVM_MAT <= 1)THEN
       IF (JALE == 1 .OR. (JEUL == 1.AND.INTEG8 == 0)) THEN
C ALE & Euler momentum transport forces 
        CALL AMOMT3(
     1   PM,        GBUF%RHO,  VOLN,      X1,
     2   X2,        X3,        X4,        X5,
     3   X6,        X7,        X8,        Y1,
     4   Y2,        Y3,        Y4,        Y5,
     5   Y6,        Y7,        Y8,        Z1,
     6   Z2,        Z3,        Z4,        Z5,
     7   Z6,        Z7,        Z8,        VX1,
     8   VX2,       VX3,       VX4,       VX5,
     9   VX6,       VX7,       VX8,       VY1,
     A   VY2,       VY3,       VY4,       VY5,
     B   VY6,       VY7,       VY8,       VZ1,
     C   VZ2,       VZ3,       VZ4,       VZ5,
     D   VZ6,       VZ7,       VZ8,       F11,
     E   F21,       F31,       F12,       F22,
     F   F32,       F13,       F23,       F33,
     G   F14,       F24,       F34,       F15,
     H   F25,       F35,       F16,       F26,
     I   F36,       F17,       F27,       F37,
     J   F18,       F28,       F38,       PX1,
     K   PX2,       PX3,       PX4,       PY1,
     L   PY2,       PY3,       PY4,       PZ1,
     M   PZ2,       PZ3,       PZ4,       DXX,
     N   DXY,       DXZ,       DYX,       DYY,
     O   DYZ,       DZX,       DZY,       DZZ,
     P   VDX1,      VDX2,      VDX3,      VDX4,
     Q   VDX5,      VDX6,      VDX7,      VDX8,
     R   VDY1,      VDY2,      VDY3,      VDY4,
     S   VDY5,      VDY6,      VDY7,      VDY8,
     T   VDZ1,      VDZ2,      VDZ3,      VDZ4,
     U   VDZ5,      VDZ6,      VDZ7,      VDZ8,
     V   VDX,       VDY,       VDZ,       DELTAX,
     W   VIS,       MXT,       QMV,       BUFMAT,
     X   IPARG1,    IXS,       GBUF%TAG22,NC1,
     Y   NC2,       NC3,       NC4,       NC5,
     Z   NC6,       NC7,       NC8,       NALE,
     1   NEL,       NFT,       MTN)
C
       ELSEIF (JEUL == 1 .AND. INTEG8 == 1) THEN
C SUPG and TG UPWIND not included 
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
     N   VZ7,               VZ8,               IPARG1,            NEL,
     O   MTN)
       ENDIF
C
        IF(JEUL+JALE/=0) CALL CHECK_OFF_ALE(F11,F21,F31,F12,F22,
     1                           F32,F13,F23,F33,F14,
     2                           F24,F34,F15,F25,F35,
     3                           F16,F26,F36,F17,F27,
     4                           F37,F18,F28,F38,GBUF%OFF,
     5                           LFT,LLT,NEL)
C
       IF (JALE  == 3 )THEN       
         CALL AMOMT3(
     1   PM,        MBUF%VAR,  FVOLN,     FX1,
     2   FX2,       FX3,       FX4,       FX5,
     3   FX6,       FX7,       FX8,       FY1,
     4   FY2,       FY3,       FY4,       FY5,
     5   FY6,       FY7,       FY8,       FZ1,
     6   FZ2,       FZ3,       FZ4,       FZ5,
     7   FZ6,       FZ7,       FZ8,       FVX1,
     8   FVX2,      FVX3,      FVX4,      FVX5,
     9   FVX6,      FVX7,      FVX8,      FVY1,
     A   FVY2,      FVY3,      FVY4,      FVY5,
     B   FVY6,      FVY7,      FVY8,      FVZ1,
     C   FVZ2,      FVZ3,      FVZ4,      FVZ5,
     D   FVZ6,      FVZ7,      FVZ8,      FF11,
     E   FF21,      FF31,      FF12,      FF22,
     F   FF32,      FF13,      FF23,      FF33,
     G   FF14,      FF24,      FF34,      FF15,
     H   FF25,      FF35,      FF16,      FF26,
     I   FF36,      FF17,      FF27,      FF37,
     J   FF18,      FF28,      FF38,      FPX1,
     K   FPX2,      FPX3,      FPX4,      FPY1,
     L   FPY2,      FPY3,      FPY4,      FPZ1,
     M   FPZ2,      FPZ3,      FPZ4,      FDXX,
     N   FDXY,      FDXZ,      FDYX,      FDYY,
     O   FDYZ,      FDZX,      FDZY,      FDZZ,
     P   FVDX1,     FVDX2,     FVDX3,     FVDX4,
     Q   FVDX5,     FVDX6,     FVDX7,     FVDX8,
     R   FVDY1,     FVDY2,     FVDY3,     FVDY4,
     S   FVDY5,     FVDY6,     FVDY7,     FVDY8,
     T   FVDZ1,     FVDZ2,     FVDZ3,     FVDZ4,
     U   FVDZ5,     FVDZ6,     FVDZ7,     FVDZ8,
     V   FVDX,      FVDY,      FVDZ,      FDELTAX,
     W   VIS,       MXT,       QMV,       BUFMAT,
     X   IPARG1,    IXS,       GBUF%TAG22,NC1,
     Y   NC2,       NC3,       NC4,       NC5,
     Z   NC6,       NC7,       NC8,       NALE,
     1   NEL,       NFT,       MTN)
       ENDIF     
      ENDIF !  IALEFVM_MAT <= 1
C
      IF (IOUTPRT>0)THEN
C Energy, momentum, mass balance by part in case of print-out
           EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)
           IF (JCVT == 0) THEN
            IFVM22      = 0
            IMAT        = IXS(1,LFT+NFT)
            IALEFVM_FLG = IPM(251,IMAT)
            IF(IALEFVM_FLG>=2)IFVM22 = 1
            CALL SBILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .                  VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .                  VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .                  VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .                  VOLN,IPARTS,GRESAV,GRTH,IGRTH,GBUF%OFF,
     .                  IEXPAN,GBUF%EINTTH,GBUF%FILL,GBUF%MOM,NEL,IFVM22,
     .                  X1, X2, X3, X4, X5, X6, X7, X8,
     .                  Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .                  Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .                  ITASK,IPARG1)
           ELSE
            CALL SRBILAN(PARTSAV,EINT    ,GBUF%RHO,GBUF%RK,GBUF%VOL ,
     .                   VGXA   ,VGYA    ,VGZA    ,VGA2    ,VOLN     ,
     .                   IPARTS ,GRESAV  ,GRTH    ,IGRTH   ,GBUF%OFF ,
     .                   IEXPAN ,GBUF%EINTTH,GBUF%FILL,XGXA, XGYA, XGZA,
     .                   XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG1)
           ENDIF
      ENDIF
C
C Compute nodal internal forces
      IF (ISROT == 0)THEN
        CALL SFINT3(
     1   GBUF%SIG,PX1,     PX2,     PX3,
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
     I   N6Z,     DFE,     P,       IXS,
     J   NEL,     NFT,     JALE,    JEUL)
      ELSE
       IISROT = 1
        CALL SFINT3B(
     1   GBUF%SIG, LBUF%SIGD,X,        NC1,
     2   NC2,      NC3,      NC4,      NC5,
     3   NC6,      NC7,      NC8,      PX1,
     4   PX2,      PX3,      PX4,      PY1,
     5   PY2,      PY3,      PY4,      PZ1,
     6   PZ2,      PZ3,      PZ4,      PX5,
     7   PX6,      PX7,      PX8,      PY5,
     8   PY6,      PY7,      PY8,      PZ5,
     9   PZ6,      PZ7,      PZ8,      F11,
     A   F21,      F31,      F12,      F22,
     B   F32,      F13,      F23,      F33,
     C   F14,      F24,      F34,      F15,
     D   F25,      F35,      F16,      F26,
     E   F36,      F17,      F27,      F37,
     F   F18,      F28,      F38,      VOLN,
     G   QVIS,     DR,       GBUF%RHO, CXX,
     H   MX1,      MY1,      MZ1,      MX2,
     I   MY2,      MZ2,      MX3,      MY3,
     J   MZ3,      MX4,      MY4,      MZ4,
     K   MX5,      MY5,      MZ5,      MX6,
     L   MY6,      MZ6,      MX7,      MY7,
     M   MZ7,      MX8,      MY8,      MZ8,
     N   IXS,      NEL,      ISMSTR)
      ENDIF
C
      IF (JALE  == 3) THEN
C Flow internal forces
          NBF1 = NEL*4
          NBF2 = NEL + NBF1
          NBF3 = NEL + NBF2
          DO I=1,NEL
             SIGF(I,1) = MBUF%VAR(NBF1 + I)                              
             SIGF(I,2) = MBUF%VAR(NBF2 + I)                              
             SIGF(I,3) = MBUF%VAR(NBF3 + I)                              
             SIGF(I,4) = ZERO                                            
             SIGF(I,5) = ZERO                                            
             SIGF(I,6) = ZERO                                            
          ENDDO 
          CALL FFINT3(
     1   SIGF,    DFE,     FPX1,    FPX2,
     2   FPX3,    FPX4,    FPY1,    FPY2,
     3   FPY3,    FPY4,    FPZ1,    FPZ2,
     4   FPZ3,    FPZ4,    FF11,    FF21,
     5   FF31,    FF12,    FF22,    FF32,
     6   FF13,    FF23,    FF33,    FF14,
     7   FF24,    FF34,    FF15,    FF25,
     8   FF35,    FF16,    FF26,    FF36,
     9   FF17,    FF27,    FF37,    FF18,
     A   FF28,    FF38,    FVOLN,   FQVIS,
     B   FN1X,    FN2X,    FN3X,    FN4X,
     C   FN5X,    FN6X,    FN1Y,    FN2Y,
     D   FN3Y,    FN4Y,    FN5Y,    FN6Y,
     E   FN1Z,    FN2Z,    FN3Z,    FN4Z,
     F   FN5Z,    FN6Z,    LLT)
      ENDIF
C
      IALEFVM_MAT = IPM(251,IXS(1,NF1))
      IF (IALEFVM_MAT > 1) THEN
C ALEFVM forces
C External forces work
          CALL ALEFVM_TFEXT(
     1   IXS,        NV46,       GBUF%SIG,   QVIS,
     2   ALE_CONNECT,VOLN,       N1X,        N2X,
     3   N3X,        N4X,        N5X,        N6X,
     4   N1Y,        N2Y,        N3Y,        N4Y,
     5   N5Y,        N6Y,        N1Z,        N2Z,
     6   N3Z,        N4Z,        N5Z,        N6Z,
     7   DXX,        DYY,        DZZ,        X,
     8   IPM,        NALE,       W,          P,
     9   VEUL,       NEL)
C
C Initial forces
          CALL ALEFVM_FRESET(
     1   ITASK,     GBUF%TAG22)
C
C Gravity forces
          CALL ALEFVM_GRAVITY(
     1   AGRAV,   IGRV,    LGRAV,   GBUF%VOL,
     2   IXS,     ITASK,   NPF,     TF,
     3   SKEW,    GBUF%RHO)
          IF(INT22/=0)THEN
            CALL ALEFVM_GRAVITY_INT22(
     1   AGRAV,     IGRV,      LGRAV,     GBUF%VOL,
     2   IXS,       ITASK,     NPF,       TF,
     3   SKEW,      GBUF%RHO,  GBUF%TAG22)
          ENDIF
C
C Internal forces
          CALL ALEFVM_STRESS(
     1   IXS,       NV46,      GBUF%SIG,  QVIS,
     2   N1X,       N2X,       N3X,       N4X,
     3   N5X,       N6X,       N1Y,       N2Y,
     4   N3Y,       N4Y,       N5Y,       N6Y,
     5   N1Z,       N2Z,       N3Z,       N4Z,
     6   N5Z,       N6Z,       X,         IPM,
     7   GBUF%RHO,  GBUF%VOL,  A,         GBUF%TAG22,
     8   NEL,       GBUF%MOM,  LBUF%SSP)
          IF(INT22/=0)THEN
            CALL ALEFVM_STRESS_INT22(
     1   IXS,       NV46,      GBUF%SIG,  QVIS,
     2   N1X,       N2X,       N3X,       N4X,
     3   N5X,       N6X,       N1Y,       N2Y,
     4   N3Y,       N4Y,       N5Y,       N6Y,
     5   N1Z,       N2Z,       N3Z,       N4Z,
     6   N5Z,       N6Z,       X,         IPM,
     7   GBUF%RHO,  GBUF%VOL,  A,         GBUF%TAG22,
     8   ELBUF_TAB, NEL,       GBUF%MOM,  LBUF%SSP,
     9   ITASK)
          ENDIF    
      ENDIF ! IALEFVM_MAT > 1 
C
      IF (INLOC > 0) THEN    
C Virtual internal forces of regularized non local dof
        CALL SFINT_REG(
     1   NLOC_DMG,VAR_REG, NEL,     OFF,
     2   VOLN,    NC1,     NC2,     NC3,
     3   NC4,     NC5,     NC6,     NC7,
     4   NC8,     PX1,     PX2,     PX3,
     5   PX4,     PY1,     PY2,     PY3,
     6   PY4,     PZ1,     PZ2,     PZ3,
     7   PZ4,     MXT(LFT),ITASK,   DT2T,
     8   GBUF%VOL,NFT)
      ENDIF 
C
      IF (JTHE < 0) THEN
C Finite element heat transfert forces
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
      IF (JCVT /= 0) THEN
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
        IF(JALE == 3) THEN
         CALL SRROTA3(
     1   R11,     R21,     R31,     R12,
     2   R22,     R32,     R13,     R23,
     3   R33,     FF11,    FF12,    FF13,
     4   FF14,    FF15,    FF16,    FF17,
     5   FF18,    FF21,    FF22,    FF23,
     6   FF24,    FF25,    FF26,    FF27,
     7   FF28,    FF31,    FF32,    FF33,
     8   FF34,    FF35,    FF36,    FF37,
     9   FF38,    NEL)
        ENDIF 
      ENDIF
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
     A   NVC,     AR,      FR_WAVE, FR_WAV,
     B   MX1,     MY1,     MZ1,     MX2,
     C   MY2,     MZ2,     MX3,     MY3,
     D   MZ3,     MX4,     MY4,     MZ4,
     E   MX5,     MY5,     MZ5,     MX6,
     F   MY6,     MZ6,     MX7,     MY7,
     G   MZ7,     MX8,     MY8,     MZ8,
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
     A   NC8,     AR,      FR_WAVE, FR_WAV,
     B   MX1,     MY1,     MZ1,     MX2,
     C   MY2,     MZ2,     MX3,     MY3,
     D   MZ3,     MX4,     MY4,     MZ4,
     E   MX5,     MY5,     MZ5,     MX6,
     F   MY6,     MZ6,     MX7,     MY7,
     G   MZ7,     MX8,     MY8,     MZ8,
     H   THEM,    FTHESKY, CONDNSKY,CONDE,
     I   NEL,     NFT,     JTHE,    ISROT,
     J   IPARTSPH)
      ENDIF
C
      IF(JALE  == 3) THEN   
       IF(IPARIT == 0 ) THEN
          CALL FCUMU3(
     1   GBUF%OFF,AF,      NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   NC7,     NC8,     STIFN,   STI,
     4   FF11,    FF21,    FF31,    FF12,
     5   FF22,    FF32,    FF13,    FF23,
     6   FF33,    FF14,    FF24,    FF34,
     7   FF15,    FF25,    FF35,    FF16,
     8   FF26,    FF36,    FF17,    FF27,
     9   FF37,    FF18,    FF28,    FF38,
     A   NVC,     NEL)
       
       ELSE  
          CALL FCUMU3P(
     1   GBUF%OFF,STI,     FFSKY,   IADS,
     2   FF11,    FF21,    FF31,    FF12,
     3   FF22,    FF32,    FF13,    FF23,
     4   FF33,    FF14,    FF24,    FF34,
     5   FF15,    FF25,    FF35,    FF16,
     6   FF26,    FF36,    FF17,    FF27,
     7   FF37,    FF18,    FF28,    FF38,
     8   NC1,     NC2,     NC3,     NC4,
     9   NC5,     NC6,     NC7,     NC8,
     A   NEL,     NFT)
       ENDIF   
      ENDIF
C
      IF (ALLOCATED(VAR_REG)) DEALLOCATE(VAR_REG)
      RETURN
      END
