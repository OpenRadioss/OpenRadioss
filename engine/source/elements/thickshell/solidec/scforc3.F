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
Chd|  SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        CSMALL3                       source/elements/solid/solide/csmall3.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S8CSIGP3                      source/elements/thickshell/solide8c/s8csigp3.F
Chd|        S8SAV3                        source/elements/solid/solide/s8sav3.F
Chd|        SCDEFC3                       source/elements/thickshell/solidec/scdefc3.F
Chd|        SCDEFO3                       source/elements/thickshell/solidec/scdefo3.F
Chd|        SCDERI3                       source/elements/thickshell/solidec/scderi3.F
Chd|        SCFINT3                       source/elements/thickshell/solidec/scfint3.F
Chd|        SCFINT_REG                    source/elements/thickshell/solidec/scfint_reg.F
Chd|        SCHOUR3_1                     source/elements/thickshell/solidec/schour3_1.F
Chd|        SCORDEF3                      source/elements/thickshell/solidec/scordef3.F
Chd|        SCROTO_SIG                    source/elements/thickshell/solidec/scroto_sig.F
Chd|        SCTHERM                       source/elements/thickshell/solidec/sctherm.F
Chd|        SCUMU3                        source/elements/solid/solide/scumu3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3p.F
Chd|        SCUMUALPHA                    source/elements/thickshell/solidec/scumualpha.F
Chd|        SCZERO3                       source/elements/thickshell/solidec/sczero3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SDLENSH                       source/elements/thickshell/solidec/sdlensh.F
Chd|        SDLENSH2                      source/elements/thickshell/solidec/sdlensh2.F
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SGETDIR3                      source/elements/thickshell/solidec/sgetdir3.F
Chd|        SGPARAV3                      source/elements/solid/solide/sgparav3.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SRBILAN                       source/elements/solid/solide/srbilan.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        TSHGEODEL3                    source/elements/thickshell/solidec/tshgeodel3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SCFORC3(ELBUF_TAB,NG      ,
     1                   PM       ,GEO     ,IXS      ,X       ,
     2                   A        ,V       ,MS       ,W       ,FLUX     ,
     3                   FLU1     ,VEUL    ,FV       ,ALE_CONNECT   ,IPARG    ,
     4                   TF       ,NPF     ,BUFMAT   ,PARTSAV ,NLOC_DMG ,
     5                   DT2T     ,NELTST  ,ITYPTST  ,STIFN   ,FSKY     ,
     6                   IADS     ,OFFSET  ,EANI     ,IPARTS  ,
     7                   F11      ,F21     ,F31      ,F12     ,F22      ,
     8                   F32      ,F13     ,F23      ,F33     ,F14      ,
     9                   F24      ,F34     ,F15      ,F25     ,F35      ,
     A                   F16      ,F26     ,F36      ,F17     ,F27      ,
     B                   F37      ,F18     ,F28      ,F38     ,NEL      ,
     C                   ICP      ,ICSIG   ,NVC      ,
     D                   IPM      ,ISTRAIN ,TEMP     ,FTHE    ,FTHESKY  ,
     E                   IEXPAN   ,IGEO    ,GRESAV   ,GRTH    ,IGRTH    ,
     F                   MSSA     ,DMELS   ,TABLE    ,XDP     ,VOLN     ,
     G                   CONDN    ,CONDNSKY,ITASK    ,IOUTPRT ,MAT_ELEM ,
     H                   H3D_STRAIN,DT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP),NPF(*),IADS(8,*),
     .        IPARTS(*), IPM(NPROPMI,*),IGEO(NPROPGI,*),GRTH(*),
     .        IGRTH(*),ITASK,IOUTPRT
      INTEGER NELTST,ITYPTST,OFFSET,ICP,ICSIG,NVC,NEL,ISTRAIN,IEXPAN,NG,H3D_STRAIN
      DOUBLE PRECISION 
     .        XDP(3,*)
      my_real
     .   DT2T
      my_real
     .   PM(NPROPM,*),  X(*), A(*), V(*), MS(*), W(*),
     .   FLUX(6,*),GEO(NPROPG,*),
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
     .   TEMP(*),FTHE(*), FTHESKY(*),GRESAV(*), MSSA(*), DMELS(*), VOLN(MVSIZ),
     .   CONDN(*),CONDNSKY(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE(DT_), INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ILAY,IR,IS,IT,IP,NF1,IFLAG,L_PLA,L_EPSD,
     .   PID,MTN0,IPTHK,IPPOS,IPMAT,NLYMAX,MID,IPANG,IBID,NLAY,
     .   JJ(6)
      INTEGER MXT0(MVSIZ),MX
C-----           
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),IBIDV(1)

      DOUBLE PRECISION 
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
     . JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ),
     . JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ),
     . VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ),
     . CONDE(MVSIZ)
C-----
C
      my_real
     .   STI(MVSIZ) ,GAMA(MVSIZ,6),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ)
C
      my_real
     .   MUVOID(MVSIZ) ! used for SPH
C-----
C
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      INTEGER IOFFS,G_PLA,G_EPSD,NN_DEL,IPRES
      my_real
     .   OFFS(MVSIZ)
      my_real
     .   OFF(MVSIZ) , RHOO(MVSIZ),RHOM(MVSIZ),OFFG(MVSIZ) ,
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
     .  PX1H4(MVSIZ),PX2H4(MVSIZ),PX3H4(MVSIZ),PX4H4(MVSIZ),
     .  HGX1(MVSIZ),HGY2(MVSIZ),HGZ1(MVSIZ),HGZ2(MVSIZ),
     .  VDX1(MVSIZ),VDX2(MVSIZ),VDX3(MVSIZ),VDX4(MVSIZ),
     .  VDX5(MVSIZ),VDX6(MVSIZ),VDX7(MVSIZ),VDX8(MVSIZ),
     .  VDY1(MVSIZ),VDY2(MVSIZ),VDY3(MVSIZ),VDY4(MVSIZ),
     .  VDY5(MVSIZ),VDY6(MVSIZ),VDY7(MVSIZ),VDY8(MVSIZ),
     .  VDZ1(MVSIZ),VDZ2(MVSIZ),VDZ3(MVSIZ),VDZ4(MVSIZ),
     .  VDZ5(MVSIZ),VDZ6(MVSIZ),VDZ7(MVSIZ),VDZ8(MVSIZ),
     .  VGXA(MVSIZ),VGYA(MVSIZ),VGZA(MVSIZ), VGA2(MVSIZ),
     .  XGXA(MVSIZ),XGYA(MVSIZ),XGZA(MVSIZ),
     .  XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),
     .  XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ)
      my_real
     .  DXY(MVSIZ),DYX(MVSIZ),
     .  DYZ(MVSIZ),DZY(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),
     .  R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .  R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .  R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),HH(MVSIZ),
     .   N1X(MVSIZ), N2X(MVSIZ), N3X(MVSIZ),
     .   N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ),
     .   N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ),
     .   N4X(MVSIZ), N5X(MVSIZ), N6X(MVSIZ),
     .   N4Y(MVSIZ), N5Y(MVSIZ), N6Y(MVSIZ),
     .   N4Z(MVSIZ), N5Z(MVSIZ), N6Z(MVSIZ),
     .   SIGYM(MVSIZ),NU(MVSIZ),VOLG(MVSIZ),SIGY(MVSIZ),
     .   RX0(MVSIZ),RY0(MVSIZ),SX0(MVSIZ),SY0(MVSIZ),
     .   DCXX(MVSIZ),DCXY(MVSIZ),DCXZ(MVSIZ),DCYX(MVSIZ),DCYY(MVSIZ),
     .   DCYZ(MVSIZ),DCZX(MVSIZ),DCZY(MVSIZ),DCZZ(MVSIZ),DC4(MVSIZ),
     .   DC5(MVSIZ),DC6(MVSIZ),VZL(MVSIZ),
     .   DHXX(MVSIZ),DHXY(MVSIZ),DHXZ(MVSIZ),DHYX(MVSIZ),DHYY(MVSIZ),
     .   DHYZ(MVSIZ),DHZX(MVSIZ),DHZY(MVSIZ),DHZZ(MVSIZ),DH4(MVSIZ),
     .   DH5(MVSIZ),DH6(MVSIZ),EINTM(MVSIZ),DDHV(MVSIZ),EINTO(MVSIZ),
     .   SIGZM(MVSIZ),VOLM(MVSIZ),MM(MVSIZ,2),USB(MVSIZ),ET(MVSIZ),
     .   R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ),
     .   TEMPEL(MVSIZ),THEM(MVSIZ,8),DIE(MVSIZ),
     .   STIN(MVSIZ),DSV(MVSIZ),BID(MVSIZ),CONDEN(MVSIZ),DTI
       my_real
     .       NU1(MVSIZ),FAC(MVSIZ),DIVDE(MVSIZ),FOR(MVSIZ,6),
     .       ALPHA_E(MVSIZ),LLSH(MVSIZ),C1,E0(MVSIZ),AREA(MVSIZ)
      DOUBLE PRECISION 
     .   FACDP
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----
      PARAMETER (NLYMAX = 200,IPMAT = 100,IPANG = 200)
      my_real
     .   DIR(MVSIZ,2),SIGN(NEL,6),SHF(MVSIZ),ZT,WT,
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),
     .   TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ),AMU(MVSIZ)
      INTEGER INLOC,L_NLOC,IPOS(8),INOD(8)
      my_real, DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: DNL
C-----
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9),W_NEWTON(9,9),A_NEWTON(9,9)
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
C-----
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
      DATA W_NEWTON / 
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.5              ,1.               ,0.5              ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.166666666666667,0.833333333333333,0.833333333333333,
     4 0.166666666666667,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.25             ,0.5              ,0.5              ,
     5 0.5              ,0.25             ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.066666666666667,0.37847496       ,0.55485838       ,
     6 0.55485838       ,0.37847496       ,0.066666666666667,
     6 0.               ,0.               ,0.               ,
     7 0.04761904       ,0.27682604       ,0.43174538       ,
     7 0.48761904       ,0.43174538       ,0.27682604       ,
     7 0.04761904       ,0.               ,0.               ,
     8 0.03571428       ,0.21070422       ,0.34112270       ,
     8 0.41245880       ,0.41245880       ,0.34112270       ,
     8 0.21070422       ,0.03571428       ,0.               ,
     9 0.027777777777778,0.1654953616     ,0.2745387126     ,
     9 0.3464285110     ,0.3715192744     ,0.3464285110     ,
     9 0.2745387126     ,0.1654953616     ,0.027777777777778/
      DATA A_NEWTON / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -1.              ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -1.              ,0.               ,1.               ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -1.              ,-.44721360       ,0.44721360       ,
     4  1.              ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -1.              ,-.5              ,0.               ,
     5 0.5              , 1.              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -1.              ,-.76505532       ,-.28523152       ,
     6 0.28523152       ,0.76505532       , 1.              ,
     6 0.               ,0.               ,0.               ,
     7 -1.              ,-.83022390       ,-.46884879       ,
     7 0.               ,0.46884879       ,0.83022390       ,
     7  1.              ,0.               ,0.               ,
     8 -1.              ,-.87174015       ,-.59170018       ,
     8 -.20929922       ,0.20929922       ,0.59170018       ,
     8 0.87174015       , 1.              ,0.               ,
     9 -1.              ,-.8997579954     ,-.6771862795     ,
     9 -.3631174638     ,0.               ,0.3631174638     ,
     9 0.6771862795     ,0.8997579954     , 1.              /
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_TAB(NG)%GBUF
      NLAY = ELBUF_TAB(NG)%NLAY
      IR = 1
      IS = 1
      IT = 1
      TEMPEL(1:MVSIZ) = ZERO
      INLOC = IPARG(78,NG)
      ALLOCATE(VAR_REG(NEL,NLAY))
!
      DO J=1,6
        JJ(J) = NEL*(J-1)
      ENDDO
!
C-----------
      NF1=NFT+1
C-----
      IF (IGTYP /= 22) THEN
        ISORTHG = 0
      END IF 
C-----------
      IBID = 0
      IBIDV= 0
C-----------
       IF (ISORTH > 0) THEN
         CALL SGPARAV3(
     1   8,         X,         IXS(1,NF1),RX,
     2   RY,        RZ,        SX,        SY,
     3   SZ,        TX,        TY,        TZ,
     4   NEL)
       ENDIF
C
C Gather nodal variables and compute intrinsic rotations
       CALL SRCOOR3(X,IXS(1,NF1),V ,W  ,GBUF%GAMA,GAMA  ,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   VD2,VIS,GBUF%OFF,OFFG,GBUF%SMSTR,GBUF%RHO,RHOO,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33, 
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .   IOUTPRT, VGXA, VGYA, VGZA, VGA2,
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .   XDP, BID, BID, BID, NEL, XGXA, XGYA, XGZA,
     .   XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,IPARG(1,NG),
     .   GBUF%GAMA_R) 
C
      NN_DEL = 0
      PID = NGEO(1)
      IF (GEO(190,PID)+GEO(191,PID)+GEO(192,PID)+GEO(192,PID)>ZERO)
     .        NN_DEL=8
      IF (NN_DEL ==0 .AND. DT%IDEL_BRICK>0) NN_DEL=8
      MX = MXT(1)
      IPRES = MAT_ELEM%MAT_PARAM(MX)%IPRES
      DO I=1,NEL
        SIGY(I) = EP30
        SIGYM(I) = EP30
        SIGZM(I) = ZERO
        MM(I,1) = ZERO
        MM(I,2) = SIGY(I)
        VOLM(I) = ZERO
        NU(I)  = MIN(HALF,PM(21,MX))
        USB(I) = 0.1/PM(32,MX)
        STIN(I)= ZERO
        DHXZ(I)= ZERO
        DHYZ(I)= ZERO
        CONDEN(I)= ZERO
        C1 =PM(32,MXT(I))
        E0(I) =THREE*(ONE-TWO*NU(I))*C1
      ENDDO
        IF (ICP == 1) THEN
          DO I=1,NEL
           NU1(I)=HALF
          ENDDO
        ELSEIF (ICP == 2) THEN
          CALL S8CSIGP3(GBUF%SIG,E0 ,GBUF%PLA,FAC,GBUF%G_PLA,NEL)
          DO I=1,NEL
            NU1(I)=NU(I)+(HALF-NU(I))*FAC(I)
          ENDDO
        ELSE
          DO I=1,NEL
           NU1(I) =NU(I)
          ENDDO
        ENDIF
      CALL SCDERI3(
     1   OFFG,      VOLN,      NGL,       XD1,
     2   XD2,       XD3,       XD4,       XD5,
     3   XD6,       XD7,       XD8,       YD1,
     4   YD2,       YD3,       YD4,       YD5,
     5   YD6,       YD7,       YD8,       ZD1,
     6   ZD2,       ZD3,       ZD4,       ZD5,
     7   ZD6,       ZD7,       ZD8,       PX1,
     8   PX2,       PX3,       PX4,       PY1,
     9   PY2,       PY3,       PY4,       PZ1,
     A   PZ2,       PZ3,       PZ4,       PX1H1,
     B   PX1H2,     PX1H3,     PX1H4,     PX2H1,
     C   PX2H2,     PX2H3,     PX2H4,     PX3H1,
     D   PX3H2,     PX3H3,     PX3H4,     PX4H1,
     E   PX4H2,     PX4H3,     PX4H4,     JAC1,
     F   JAC2,      JAC3,      JAC4,      JAC5,
     G   JAC6,      RX0,       RY0,       SX0,
     H   SY0,       VZL,       VOLG,      GBUF%SMSTR,
     I   GBUF%OFF,  NEL,       ISMSTR)
      CALL SDLEN3(
     1   VOLG,    DELTAX,  X1,      X2,
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
      IF (NTSHEG > 0) THEN
         CALL SDLENSH(VOLN,LLSH,AREA ,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, NEL)
        ALPHA_E(1:NEL) = ONE  
        DO I=1,NEL
          IF (GBUF%IDT_TSH(I)<=0) CYCLE
          FACDP = 1.343*LLSH(I)/DELTAX(I)
          ALPHA_E(I) = FACDP*FACDP  
          DELTAX(I)=MAX(LLSH(I),DELTAX(I))
        ENDDO
      END IF        
      CALL SCDEFC3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   VX1,     VX2,     VX3,     VX4,
     5   VX5,     VX6,     VX7,     VX8,
     6   VY1,     VY2,     VY3,     VY4,
     7   VY5,     VY6,     VY7,     VY8,
     8   VZ1,     VZ2,     VZ3,     VZ4,
     9   VZ5,     VZ6,     VZ7,     VZ8,
     A   DCXX,    DCXY,    DCXZ,    DCYX,
     B   DCYY,    DCYZ,    DCZX,    DCZY,
     C   DCZZ,    DC4,     DC5,     DC6,
     D   WXX,     WYY,     WZZ,     DHXX,
     E   DHXY,    DHYX,    DHYY,    DHZX,
     F   DHZY,    DHZZ,    DH4,     DH5,
     G   DH6,     PX1H1,   PX1H2,   PX2H1,
     H   PX2H2,   PX3H1,   PX3H2,   PX4H1,
     I   PX4H2,   HGX1,    HGY2,    HGZ1,
     J   HGZ2,    RX0,     RY0,     SX0,
     K   SY0,     NU1,     DDHV,    NEL)
         G_PLA  = GBUF%G_PLA
         G_EPSD = GBUF%G_EPSD
      CALL SCZERO3(
     .   F11, F21, F31, F12, F22, F32, F13, F23,
     .   F33, F14, F24, F34, F15, F25, F35, F16, 
     .   F26, F36, F17, F27, F37, F18, F28, F38,
     .   GBUF%SIG,EINTM,RHOM,GBUF%QVIS,GBUF%PLA,
     .   GBUF%EPSD,G_PLA,G_EPSD,NEL)
C ------------------------------------------------------------------------------
C Update reference configuration (possible future change to small strain option)
C Total strain option doesn't change the reference configuration
C ------------------------------------------------------------------------------
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
       CALL S8SAV3(
     1   GBUF%OFF,  GBUF%SMSTR,XD1,       XD2,
     2   XD3,       XD4,       XD5,       XD6,
     3   XD7,       XD8,       YD1,       YD2,
     4   YD3,       YD4,       YD5,       YD6,
     5   YD7,       YD8,       ZD1,       ZD2,
     6   ZD3,       ZD4,       ZD5,       ZD6,
     7   ZD7,       ZD8,       NEL)
      END IF
c
       IF (ISORTH > 0) THEN
         PID=NGEO(1)
         IF (IGTYP == 21) THEN
          CALL SGETDIR3(NEL, RX,RY,RZ,TX,TY,TZ,
     .           R11,R21,R31,R12,R22,R32,GBUF%GAMA,DIR,IREP)
         ENDIF
         IF (IGTYP == 22) THEN
           IPTHK = IPANG+NLYMAX
           IPPOS = IPTHK+NLYMAX
           MTN0=MTN
           DO I=1,NEL
             MXT0(I)=MXT(I)
             SHF(I)=GEO(38,NGEO(I))
           ENDDO
         ENDIF
       ENDIF 
C-------------------------------------
C Uniform stress through the thickness
C-------------------------------------
      DO ILAY=1,NLAY
        LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
        IF (IGTYP == 22) THEN
          MID=IGEO(IPMAT+ILAY,PID)
          MTN=NINT(PM(19,MID))
        ENDIF
        DO I=1,NEL
          SIGZM(I) = SIGZM(I) + LBUF%VOL(I)*LBUF%SIG(JJ(3)+I)
          VOLM(I)  = VOLM(I)  + LBUF%VOL(I)
        ENDDO
      ENDDO
C-------------------------------------------
      IF (DT1 == ZERO) THEN
        DTI =ZERO
      ELSE
        DTI = ONE/DT1
      ENDIF 
C-------------------------------------------
C Element temperature
C-------------------------------------------
      IF(JTHE < 0) THEN       
        DO I=1,NEL
          TEMPEL(I) = ONE_OVER_8 *(  TEMP(NC1(I)) + TEMP(NC2(I))  
     .                         + TEMP(NC3(I)) + TEMP(NC4(I)) 
     .                         + TEMP(NC5(I)) + TEMP(NC6(I)) 
     .                         + TEMP(NC7(I)) + TEMP(NC8(I)))
          GBUF%TEMP(I) = TEMPEL(I)
        ENDDO
      ENDIF
      IOFFS=0
      DO I=1,NEL
        OFFS(I)  = EP20
      ENDDO
      IF(JTHE<0) THEM(1:NEL,1:8) =ZERO
c
C---------------------------------------------------------
C Compute non-local variable increment at each Gauss point 
C---------------------------------------------------------
      IF (INLOC > 0) THEN  
        L_NLOC = NLOC_DMG%L_NLOC
        DNL => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        DO ILAY=1,NLAY
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
              IPOS(J) = NLOC_DMG%POSI(INOD(J))+ILAY-1
            ENDDO
            VAR_REG(I,ILAY) = DNL(IPOS(1)) + DNL(IPOS(2)) + DNL(IPOS(3)) + 
     .                        DNL(IPOS(4)) + DNL(IPOS(5)) + DNL(IPOS(6)) +
     .                        DNL(IPOS(7)) + DNL(IPOS(8))
            VAR_REG(I,ILAY) = VAR_REG(I,ILAY)*ONE_OVER_8
          ENDDO
        ENDDO     
      ENDIF 
C---------------------------------------------------------
c
C-------------------------------------------------
C Loop on integration points through the thickness
C-------------------------------------------------
      DO ILAY=1,NLAY
        LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
C
        IF (IGTYP == 22) THEN
          ZT = GEO(IPPOS+ILAY,PID)
          WT = GEO(IPTHK+ILAY,PID)
          MID=IGEO(IPMAT+ILAY,PID)
          MTN=NINT(PM(19,MID))
          DO I=1,NEL
            MXT(I)=MID
          ENDDO
        ELSE
          ZT = A_GAUSS(ILAY,NLAY)
          WT = W_GAUSS(ILAY,NLAY)
        ENDIF
        CALL SCDEFO3(
     1   DXX,        DXY,        DXZ,        DYX,
     2   DYY,        DYZ,        DZX,        DZY,
     3   DZZ,        D4,         D5,         D6,
     4   DCXX,       DCXY,       DCXZ,       DCYX,
     5   DCYY,       DCYZ,       DCZX,       DCZY,
     6   DCZZ,       DC4,        DC5,        DC6,
     7   DHXX,       DHXY,       DHXZ,       DHYX,
     8   DHYY,       DHYZ,       DHZX,       DHZY,
     9   DHZZ,       DH4,        DH5,        DH6,
     A   ZT,         WT,         VZL,        VOLN,
     B   VOLG,       LBUF%VOL,   DDHV,       LBUF%SIG,
     C   SIGZM,      VOLM,       USB,        LBUF%EINT,
     D   OFF,        OFFG,       DTI,        GBUF%OFF,
     E   DSV,        LBUF%VOL0DP,VOLDP,      IPRES,
     F   NEL    )
        DO I=1,NEL
          EINTO(I) = LBUF%EINT(I)
          RHOO(I)  = LBUF%RHO(I)
        ENDDO
        IF (ISORTH > 0) THEN
          IF (IGTYP == 22)  
     .      CALL SGETDIR3(NEL,RX,RY,RZ,TX,TY,TZ,
     .                    R11,R21,R31,R12,R22,R32,
     .                    LBUF%GAMA,DIR,IREP)
          CALL SCORDEF3(NEL,DXX,DYY,DZZ,D4,D5,D6,DIR)
          IF (IGTYP == 22) THEN
            DO I=1,NEL
              D5(I)=SHF(I)*D5(I)
              D6(I)=SHF(I)*D6(I) 
            ENDDO
          ENDIF
        ENDIF

        DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))+DSV(1:NEL)   
        CALL SRHO3(
     1   PM,         LBUF%VOL,   LBUF%RHO,   LBUF%EINT,
     2   DIVDE,      FLUX(1,NF1),FLU1(NF1),  VOLN,
     3   DVOL,       NGL,        MXT,        OFF,
     4   0,          GBUF%TAG22, VOLDP,      LBUF%VOL0DP,
     5   AMU,        GBUF%OFF,   NEL,        MTN,
     6   JALE,       ISMSTR,     JEUL,       JLAG)
     
C-----------------------------
C Gather stresses
C-----------------------------
      CALL CSMALL3(LBUF%SIG,S1,S2,S3,S4,S5,S6,
     .             GBUF%OFF,OFF,NEL)
C------------------------------------------------------
C Compute new stresses according to constitutive laws
C Compute time step DT2T
C------------------------------------------------------
        CALL MMAIN(
     1   ELBUF_TAB,    NG,          PM,        GEO,
     2   FV,           ALE_CONNECT, IXS,       IPARG,
     3   V,            TF,          NPF,       BUFMAT,
     4   STI,          X,           DT2T,      NELTST,
     5   ITYPTST,      OFFSET,      NEL,       W,
     6   OFF,          NGEO,        MXT,       NGL,
     7   VOLN,         VD2,         DVOL,      DELTAX,
     8   VIS,          QVIS,        CXX,       S1,
     9   S2,           S3,          S4,        S5,
     A   S6,           DXX,         DYY,       DZZ,
     B   D4,           D5,          D6,        WXX,
     C   WYY,          WZZ,         JAC1,      JAC2,
     D   JAC3,         JAC4,        JAC5,      JAC6,
     E   VDX,          VDY,         VDZ,       MUVOID,
     F   SSP_EQ,       AIRE,        SIGY,      ET,
     G   R1_FREE,      LBUF%PLA,    R3_FREE,   AMU,
     H   DXX,          DXY,         DXZ,       DYX,
     I   DYY,          DYZ,         DZX,       DZY,
     J   DZZ,          IPM,         GAMA,      BID,
     K   BID,          BID,         BID,       BID,
     L   BID,          BID,         ISTRAIN,   TEMPEL,
     M   DIE,          IEXPAN,      ILAY,      MSSA,
     N   DMELS,        IR,          IS,        IT,
     O   TABLE,        BID,         BID,       BID,
     P   BID,          IPARG(1,NG), IGEO,      CONDE,
     Q   ITASK,        NLOC_DMG,    VAR_REG(1,ILAY),   MAT_ELEM,
     R   H3D_STRAIN,   JPLASOL,     JSPH,      OPT_MTN=MTN,
     S   OPT_JCVT=JCVT,OPT_ISORTH=ISORTH,      OPT_ISORTHG=ISORTHG)

C
        DO I=1,NEL
          SIGYM(I) = MIN(SIGYM(I),SIGY(I))
          STIN(I) = STIN(I)+STI(I)
        ENDDO
C
        IF(NODADT_THERM == 1) THEN
          DO I=1,NEL
             CONDEN(I)= CONDEN(I)+ CONDE(I)
          ENDDO
        ENDIF
        IF (ISTRAIN == 1) THEN 
          CALL SSTRA3(
     1   DXX,      DYY,      DZZ,      D4,
     2   D5,       D6,       LBUF%STRA,WXX,
     3   WYY,      WZZ,      OFF,      NEL,
     4   JCVT)
        ENDIF 
C----------------------------
C Internal forces
C----------------------------
        L_PLA  = ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA
        L_EPSD = ELBUF_TAB(NG)%BUFLY(ILAY)%L_EPSD
        IF (ISORTH > 0) THEN
         CALL SCROTO_SIG(NEL,LBUF%SIG,SIGN,DIR)
!! SCROTO() temporary replaced by (the same) SCROTO_SIG() in order not to affect
!! the other multidimensional buffer ARRAYS which are still not modified
         CALL SCFINT3(SIGN,
     .        PX1, PX2, PX3, PX4,
     .        PY1, PY2, PY3, PY4,
     .        PZ1, PZ2, PZ3, PZ4,
     .        PX5, PX6, PX7, PX8,
     .        PY5, PY6, PY7, PY8,
     .        PZ5, PZ6, PZ7, PZ8,
     .        F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .        F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .        VOLN,QVIS,
     .        PX1H1, PX1H2, PX2H1, PX2H2, 
     .        PX3H1, PX3H2, PX4H1, PX4H2, 
     .        RX0, RY0, SX0, SY0,
     .        LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     .        GBUF%SIG,EINTM,EINTO,RHOM,GBUF%QVIS,GBUF%PLA,
     .        NU1,ZT  ,WT   ,VOLG,MM,OFF,
     .        LBUF%VOL,GBUF%VOL,L_PLA,L_EPSD,NEL)
        ELSE
          CALL SCFINT3(LBUF%SIG,
     .        PX1, PX2, PX3, PX4,
     .        PY1, PY2, PY3, PY4,
     .        PZ1, PZ2, PZ3, PZ4,
     .        PX5, PX6, PX7, PX8,
     .        PY5, PY6, PY7, PY8,
     .        PZ5, PZ6, PZ7, PZ8,
     .        F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .        F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .        VOLN,QVIS,
     .        PX1H1, PX1H2, PX2H1, PX2H2, 
     .        PX3H1, PX3H2, PX4H1, PX4H2, 
     .        RX0, RY0, SX0, SY0,
     .        LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     .        GBUF%SIG,EINTM,EINTO,RHOM,GBUF%QVIS,GBUF%PLA,
     .        NU1,A_GAUSS(ILAY,NLAY),W_GAUSS(ILAY,NLAY),VOLG,MM,OFF,
     .        LBUF%VOL,GBUF%VOL,L_PLA,L_EPSD,NEL)
        ENDIF
C-------------------------
C Finite element heat transfert  
C--------------------------
        IF (JTHE < 0) THEN
          CALL SCTHERM(
     1   PM,      MXT,     VOLN,    NC1,
     2   NC2,     NC3,     NC4,     NC5,
     3   NC6,     NC7,     NC8,     PX1,
     4   PX2,     PX3,     PX4,     PY1,
     5   PY2,     PY3,     PY4,     PZ1,
     6   PZ2,     PZ3,     PZ4,     DT1,
     7   TEMP,    TEMPEL,  DIE,     THEM,
     8   GBUF%OFF,LBUF%OFF,NEL)
        ENDIF 
        DO I=1,NEL
          OFFG(I)=MIN(OFFG(I),OFF(I))
         IF (LBUF%OFF(I) > ONE .AND. GBUF%OFF(I) == ONE) THEN
           OFFS(I) = MIN(LBUF%OFF(I),OFFS(I))
           IOFFS   = 1
         ENDIF
        ENDDO
C-----------------------------
      ENDDO  !  ILAY=1,NLAY
C-----------------------------
c
C-------------------------------
C Non-local specific computation
C-------------------------------
      IF (INLOC > 0) THEN 
        ! Computation of thickshell area
        CALL SDLENSH(VOLN,LLSH,AREA ,
     .     X1, X2, X3, X4, X5, X6, X7, X8,
     .     Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .     Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, NEL)  
        ! Non-local internal forces 
        CALL SCFINT_REG(
     1      NLOC_DMG ,VAR_REG  ,NEL     ,OFF     ,
     2      VOLG     ,NC1      ,NC2     ,NC3     ,
     3      NC4      ,NC5      ,NC6     ,NC7     ,
     4      NC8      ,PX1      ,PX2     ,PX3     ,
     5      PX4      ,PY1      ,PY2     ,PY3     ,
     6      PY4      ,PZ1      ,PZ2     ,PZ3     ,
     7      PZ4      ,MXT(LFT) ,ITASK   ,DT2T    ,
     8      GBUF%VOL ,NFT      ,NLAY    ,W_GAUSS ,
     9      A_GAUSS  ,AREA     ,ELBUF_TAB(NG)%NLOCTS(1,1))
      ENDIF
C--------------------------
c
      IF (IOFFS == 1)THEN
        DO I=1,NEL
          IF (OFFS(I)<=TWO) GBUF%OFF(I)=OFFS(I)
        END DO
        DO ILAY=1,NLAY
          LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
          IF (IGTYP == 22) THEN
            MID=IGEO(IPMAT+ILAY,PID)
            MTN=NINT(PM(19,MID))
          ENDIF
          DO I=1,NEL
            IF (GBUF%OFF(I) > ONE) LBUF%OFF(I) = GBUF%OFF(I)
          ENDDO
        ENDDO  !  ILAY=1,NLAY
      ENDIF
      DO I=1,NEL
        GBUF%RHO(I) = RHOM(I)
      END DO
C-----------------------------
      IF (IGTYP == 22) THEN
        MTN = MTN0
        DO I=1,NEL
          MXT(I)=MXT0(I)
        ENDDO
      ENDIF
      IF ( NN_DEL> 0) THEN
         CALL SDLENSH2(VOLG,LLSH,AREA ,
     .          X1, X2, X3, X4, X5, X6, X7, X8,
     .          Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .          Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,NEL)
        CALL TSHGEODEL3(NGL,GBUF%OFF,VOLG,AREA,GBUF%VOL,
     .                  LLSH,GEO(1,PID),NN_DEL,DT,NEL )
      ENDIF
C-----------------------------
C Small strain
C-----------------------------
      CALL SMALLB3(
     1   GBUF%OFF,OFFG,    NEL,     ISMSTR)
C-----------------------------
C Anti hourglass forces
C-----------------------------
      CALL SCHOUR3_1(
     1   PM,        GBUF%RHO,  OFFG,      VX1,
     2   VX2,       VX3,       VX4,       VX5,
     3   VX6,       VX7,       VX8,       VY1,
     4   VY2,       VY3,       VY4,       VY5,
     5   VY6,       VY7,       VY8,       VZ1,
     6   VZ2,       VZ3,       VZ4,       VZ5,
     7   VZ6,       VZ7,       VZ8,       F11,
     8   F21,       F31,       F12,       F22,
     9   F32,       F13,       F23,       F33,
     A   F14,       F24,       F34,       F15,
     B   F25,       F35,       F16,       F26,
     C   F36,       F17,       F27,       F37,
     D   F18,       F28,       F38,       PX1H1,
     E   PX1H2,     PX1H3,     PX1H4,     PX2H1,
     F   PX2H2,     PX2H3,     PX2H4,     PX3H1,
     G   PX3H2,     PX3H3,     PX3H4,     PX4H1,
     H   PX4H2,     PX4H3,     PX4H4,     HGX1,
     I   HGY2,      HGZ1,      HGZ2,      VOLG,
     J   MXT,       CXX,       NGEO,      GEO,
     K   GBUF%HOURG,RX0,       RY0,       SX0,
     L   SY0,       JAC5,      GBUF%EINT, EINTM,
     M   GBUF%VOL,  SIGYM,     GBUF%SIG,  MM,
     N   NU,        GBUF%PLA,  ICP,       NEL,
     O   MTN,       NLAY   )
C--------------------------------------
C Balance per part in case of print out
C--------------------------------------
      IFLAG = MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
           CALL SRBILAN(PARTSAV,GBUF%EINT,GBUF%RHO,GBUF%RK ,GBUF%VOL,
     .                  VGXA   ,VGYA     ,VGZA    ,VGA2    ,VOLG    ,
     .                  IPARTS ,GRESAV   ,GRTH    ,IGRTH   ,GBUF%OFF,
     .                  IEXPAN ,GBUF%EINTTH,GBUF%FILL, XGXA, XGYA, XGZA,
     .                  XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
      ENDIF
C
C----------------------------
C Covected frame to global frame
C----------------------------
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
C----------------------------
      IF(NFILSOL/=0) CALL SFILLOPT(
     1   GBUF%FILL,STI,      F11,      F21,
     2   F31,      F12,      F22,      F32,
     3   F13,      F23,      F33,      F14,
     4   F24,      F34,      F15,      F25,
     5   F35,      F16,      F26,      F36,
     6   F17,      F27,      F37,      F18,
     7   F28,      F38,      NEL)
C----------------------------
C Assemble internal forces
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
      IF (NTSHEG > 0)
     +  CALL SCUMUALPHA(
     1   GBUF%OFF,ALPHA_E, NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   NC7,     NC8,     NEL)
C-----------
      IF (ALLOCATED(VAR_REG)) DEALLOCATE(VAR_REG)
      RETURN
      END
