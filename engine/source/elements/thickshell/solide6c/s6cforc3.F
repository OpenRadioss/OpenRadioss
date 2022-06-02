Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        CSMALL3                       source/elements/solid/solide/csmall3.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S6CBILAN                      source/elements/thickshell/solide6c/s6cbilan.F
Chd|        S6CDEFC3                      source/elements/thickshell/solide6c/s6cdefo3.F
Chd|        S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|        S6CFINT3                      source/elements/thickshell/solide6c/s6cfint3.F
Chd|        S6CUMU3                       source/elements/thickshell/solide6c/s6cumu3.F
Chd|        S6CUMU3P                      source/elements/thickshell/solide6c/s6cumu3.F
Chd|        S6CZERO3                      source/elements/thickshell/solide6c/s6cfint3.F
Chd|        S6FILLOPT                     source/elements/thickshell/solide6c/s6fillopt.F
Chd|        S6PROJ3                       source/elements/thickshell/solide6c/s6rcoor3.F
Chd|        S6RCOOR3                      source/elements/thickshell/solide6c/s6rcoor3.F
Chd|        S6SAV3                        source/elements/thickshell/solide6c/s6rcoor3.F
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCDEFO3                       source/elements/thickshell/solidec/scdefo3.F
Chd|        SCORDEF3                      source/elements/thickshell/solidec/scforc3.F
Chd|        SCROTO_SIG                    source/elements/thickshell/solidec/scforc3.F
Chd|        SCUMUALPHA6                   source/elements/thickshell/solidec/scdtdc3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SDLENSH3N                     source/elements/thickshell/solidec/scdtdc3.F
Chd|        SGETDIR3                      source/elements/thickshell/solidec/scforc3.F
Chd|        SGPARAV3                      source/elements/solid/solide/sgcoor3.F
Chd|        SMALLB3                       source/elements/solid/solide/small3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        VRROTA3                       source/elements/thickshell/solide6c/vrrota3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S6CFORC3(ELBUF_TAB,NG     , 
     1                   PM       ,GEO     ,IXS     ,X      ,
     2                   A        ,V       ,MS      ,W      ,FLUX   ,
     3                   FLU1     ,VEUL    ,FV      ,ALE_CONNECT  ,IPARG  ,
     4                   TF       ,NPF     ,BUFMAT  ,PARTSAV,
     5                   DT2T     ,NELTST  ,ITYPTST ,STIFN  ,FSKY   ,
     6                   IADS     ,OFFSET  ,EANI    ,IPARTS ,
     7                   F11      ,F21     ,F31     ,F12    ,F22    ,
     8                   F32      ,F13     ,F23     ,F33    ,F14    ,
     9                   F24      ,F34     ,F15     ,F25    ,F35    ,
     A                   F16      ,F26     ,F36     ,NEL    ,
     B                   ICP      ,ICSIG   ,NLOC_DMG,
     C                   IPM      ,ISTRAIN ,IGEO    ,GRESAV ,GRTH   ,
     D                   IGRTH    ,TABLE   ,MSSA    ,DMELS  ,VOLN   ,
     E                   ITASK    ,IOUTPRT ,MATPARAM_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP),NPF(*),IADS(8,*),GRTH(*),
     .  IPARTS(*),IPM(NPROPMI,*),IGEO(NPROPGI,*),IGRTH(*), ITASK,IOUTPRT
      INTEGER NELTST,ITYPTST,OFFSET,NG,NEL,ICP,ICSIG,ISTRAIN
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
     .   F16(MVSIZ),F26(MVSIZ),F36(MVSIZ),GRESAV(*),
     .   MSSA(*), DMELS(*), VOLN(MVSIZ)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,LCO,NF1,IFLAG,NUVAR,
     .   ILAY,NLAY,IR,IS,IT,IP,IBID,MX
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),IBIDON(1)
c     Variables utilisees dans les routines solides uniquement (en arguments).
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
C----------
      my_real
     .   C1,DTI, MBID(1)
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . AJ1(MVSIZ) , AJ2(MVSIZ) , AJ3(MVSIZ) ,
     . AJ4(MVSIZ) , AJ5(MVSIZ) , AJ6(MVSIZ) ,
     . VDX(MVSIZ) , VDY(MVSIZ) , VDZ(MVSIZ),SSP_EQ(MVSIZ),AIRE(MVSIZ)
C-----
c      Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ),WXX(MVSIZ),WYY(MVSIZ),WZZ(MVSIZ),CONDE(MVSIZ)
C     Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .   MUVOID(MVSIZ)
      my_real
     .   OFF(MVSIZ) , RHOO(MVSIZ), OFFG(MVSIZ) ,
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), 
     .  VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .  VX5(MVSIZ),VX6(MVSIZ),
     .  VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .  VY5(MVSIZ),VY6(MVSIZ),
     .  VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .  VZ5(MVSIZ),VZ6(MVSIZ),
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),
     .  PX1H(MVSIZ),PX2H(MVSIZ),PX3H(MVSIZ),
     .  PY1H(MVSIZ),PY2H(MVSIZ),PY3H(MVSIZ),
     .  PZ1H(MVSIZ),PZ2H(MVSIZ),PZ3H(MVSIZ),
     .  VGXA(MVSIZ),VGYA(MVSIZ),VGZA(MVSIZ), VGA2(MVSIZ),
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
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),GAMA(MVSIZ,6)
C                                                                     12
      my_real
     .   SIGYM(MVSIZ),G(MVSIZ),NU(MVSIZ),VOLG(MVSIZ),SIGY(MVSIZ),
     .   B1122(MVSIZ),B1221(MVSIZ),B2212(MVSIZ),B1121(MVSIZ),
     .   B1122H(MVSIZ),B1221H(MVSIZ),B2212H(MVSIZ),B1121H(MVSIZ),
     .   B1X(MVSIZ,2),B1Y(MVSIZ,2),B2X(MVSIZ,2),B2Y(MVSIZ,2),
     .   B1XH(MVSIZ,2),B1YH(MVSIZ,2),B2XH(MVSIZ,2),B2YH(MVSIZ,2),
     .   DCXX(MVSIZ),DCXY(MVSIZ),DCXZ(MVSIZ),DCYX(MVSIZ),DCYY(MVSIZ),
     .   DCYZ(MVSIZ),DCZX(MVSIZ),DCZY(MVSIZ),DCZZ(MVSIZ),DC4(MVSIZ),
     .   DC5(MVSIZ),DC6(MVSIZ),VZL(MVSIZ),JI33(MVSIZ),
     .   DHXX(MVSIZ),DHXY(MVSIZ),DHYX(MVSIZ),DHYY(MVSIZ),DHYZ(MVSIZ),
     .   DHZX(MVSIZ),DHZY(MVSIZ),DHZZ(MVSIZ),DH4(MVSIZ),DHXZ(MVSIZ),
     .   DH5(MVSIZ),DH6(MVSIZ),EINTM(MVSIZ),DDHV(MVSIZ),DD(MVSIZ,6),
     .   SIGZM(MVSIZ),VOLM(MVSIZ),USB(MVSIZ),ET(MVSIZ),
     .   R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ),
     .   STIN(MVSIZ),BID(MVSIZ),DSV(MVSIZ),ALPHA_E(MVSIZ),LLSH(MVSIZ)
C     
      INTEGER PID,MTN0,IPTHK,IPPOS,IPMAT,NLYMAX,MID,IPANG,IOFFS
      INTEGER MXT0(MVSIZ)
      my_real
     .   DIR(MVSIZ,2),SIGN(NEL,6),SHF(MVSIZ),ZT,WT,OFFS(MVSIZ),
     .   RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),NU1(MVSIZ),FAC(MVSIZ),
     .   SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),
     .   TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ),E0(MVSIZ),
     .   N1X(MVSIZ), N2X(MVSIZ), N3X(MVSIZ),
     .   N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ),
     .   N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ),
     .   N4X(MVSIZ), N5X(MVSIZ), N6X(MVSIZ),
     .   N4Y(MVSIZ), N5Y(MVSIZ), N6Y(MVSIZ),
     .   N4Z(MVSIZ), N5Z(MVSIZ), N6Z(MVSIZ),AMU(MVSIZ)
      my_real VARNL(NEL)
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ),FACDP
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----------------------------------------------
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
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_TAB(NG)%GBUF
      NLAY = ELBUF_TAB(NG)%NLAY
      IR = 1
      IS = 1
      IT = 1
C-----------
      IBID = 0
      IBIDON(1) = 0
c      MPT =IABS(NPT)
      IF (IGTYP /= 22) THEN
        ISORTHG = 0
      END IF 
C-----------
      NF1=NFT+1
C--------------------------
C-----------
       IF (ISORTH > 0) THEN
         CALL SGPARAV3(
     .   6 ,X,IXS(1,NF1),
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
       ENDIF
C      GATHER NODAL VARIABLES AND COMPUTE INTRINSIC ROTATION.
       CALL S6RCOOR3(X,IXS(1,NF1),V,W,GBUF%GAMA,GAMA,
     .   X1, X2, X3, X4, X5, X6,
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, 
     .   VX1, VX2, VX3, VX4, VX5, VX6, 
     .   VY1, VY2, VY3, VY4, VY5, VY6, 
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, 
     .   VD2,VIS,GBUF%OFF,OFFG,GBUF%SMSTR,GBUF%RHO,RHOO,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33, 
     .   NC1,NC2,NC3,NC4,NC5,NC6,NGL,MXT,NGEO,
     .   IOUTPRT, VGXA, VGYA, VGZA, VGA2,DD,
     .   NEL, XGXA, XGYA, XGZA,XGXA2,XGYA2,XGZA2,
     .   XGXYA,XGYZA,XGZXA,IPARG(1,NG),GBUF%GAMA_R) 
C
      MX = MXT(LFT)
      C1 =PM(32,MX)
      DO I=LFT,LLT
        SIGZM(I) = ZERO
        VOLM(I) = ZERO
        NU(I)=MIN(UNDEMI,PM(21,MX))
        E0(I) =TROIS*(UN-DEUX*NU(I))*C1
        USB(I)=EM01/C1
        STIN(I)=ZERO
      ENDDO
C
      IF (ICP.EQ.1) THEN                                
        DO I=LFT,LLT                                    
         NU1(I)=UNDEMI                                  
        ENDDO                                           
      ELSEIF (ICP.EQ.2) THEN                            
        CALL S8ZSIGP3(LFT  ,LLT       ,GBUF%SIG,E0  ,GBUF%PLA,
     2                FAC  ,GBUF%G_PLA,NEL     )
        DO I=LFT,LLT                                    
          NU1(I)=NU(I)+(UNDEMI-NU(I))*FAC(I)             
        ENDDO                                           
      ELSE                                             
         DO I=LFT,LLT                                    
          NU1(I) =NU(I)                                  
         ENDDO                                           
      ENDIF                                             
C
      CALL S6CDERI3(OFFG,VOLN,NGL,
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, 
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX1H, PX2H, PX3H, 
     .   PY1H, PY2H, PY3H, 
     .   PZ1H, PZ2H, PZ3H, 
     .   AJ1,AJ2,AJ3,
     .   AJ4,AJ5,AJ6,
     .   JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .   B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .   VZL,VOLG,GBUF%SMSTR,GBUF%OFF,NEL)
      CALL SDLEN3(VOLG,DELTAX,
     .   X1, X2, X5, X4, X3, X3, X6, X6,
     .   Y1, Y2, Y5, Y4, Y3, Y3, Y6, Y6,
     .   Z1, Z2, Z5, Z4, Z3, Z3, Z6, Z6,
     .   N1X, N2X, N3X, N4X, N5X, N6X,
     .   N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .   N1Z, N2Z, N3Z, N4Z, N5Z, N6Z)
      IF (NTSHEG > 0) THEN
         CALL SDLENSH3N(VOLG,LLSH,
     .                  X1, X2, X3, X4, X5, X6,
     .                  Y1, Y2, Y3, Y4, Y5, Y6,
     .                  Z1, Z2, Z3, Z4, Z5, Z6)
        ALPHA_E(LFT:LLT) = UN  
        DO I=LFT,LLT
          IF (GBUF%IDT_TSH(I)<=0) CYCLE
          FACDP = 1.343*LLSH(I)/DELTAX(I)
          ALPHA_E(I) = FACDP*FACDP  
          DELTAX(I)=MAX(LLSH(I),DELTAX(I))
        ENDDO
      END IF        
      CALL S6CDEFC3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, 
     .   VY1, VY2, VY3, VY4, VY5, VY6,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, 
     .   DCXX,DCXY,DCXZ,DCYX,DCYY,DCYZ,DCZX,DCZY,DCZZ,DC4,DC5,DC6,
     .   WXX, WYY, WZZ, 
     .   DHXX, DHXY, DHXZ,DHYX, DHYY, DHYZ,DHZX, DHZY, DHZZ,
     .   DH4, DH5, DH6,
     .   PX1H, PX2H, PX3H, 
     .   PY1H, PY2H, PY3H, 
     .   PZ1H, PZ2H, PZ3H, 
     .   JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .   B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .   DDHV,NU1) 
      CALL S6CZERO3(
     .   F11, F21, F31, F12, F22, F32, F13, F23,
     .   F33, F14, F24, F34, F15, F25, F35, F16, F26, F36, 
     .   GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,
     .   GBUF%PLA,GBUF%EPSD,GBUF%G_PLA,GBUF%G_EPSD,NEL)
C --------------------------
C  --- UPDATE REF CONFIGURATION (possible future change to small strain option)
C  --- ! Total strain option doesn't change the Ref CONF.
C --------------------------
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
       CALL S6SAV3(GBUF%OFF,GBUF%SMSTR,
     .   X1, X2, X3, X4, X5, X6,
     .   Y1, Y2, Y3, Y4, Y5, Y6,
     .   Z1, Z2, Z3, Z4, Z5, Z6,
     .   NEL)
      END IF !(ISMSTR <= 3) THEN
c
      IF (ISORTH > 0) THEN                         
        PID = NGEO(LFT)                              
        IF (IGTYP == 21) THEN                      
         CALL SGETDIR3(LFT,LLT, RX,RY,RZ,TX,TY,TZ, 
     .                 R11,R21,R31,R12,R22,R32,    
     .                 GBUF%GAMA,DIR,IREP,NEL)         
        ENDIF                                      
        IF (IGTYP == 22) THEN                      
          NLYMAX= 200                              
          IPANG = 200                              
          IPTHK = IPANG+NLYMAX                     
          IPPOS = IPTHK+NLYMAX                     
	         IPMAT = 100                              
          MTN0=MTN                                 
          DO I=LFT,LLT                             
            MXT0(I)=MXT(I)                         
            SHF(I)=GEO(38,NGEO(I))                 
          ENDDO                                    
        ENDIF                                      
      ENDIF                                        
C-----------------------------
C     TRAITEMENT ASSURER SIGZ CONSTANTE
C-----------------------------
      DO ILAY=1,NLAY
        LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
        IF (IGTYP == 22) THEN
          MID=IGEO(IPMAT+ILAY,PID)
          MTN=NINT(PM(19,MID))
        ENDIF
        DO I=LFT,LLT
          SIGZM(I) = SIGZM(I)+LBUF%VOL(I)*LBUF%SIG(I+2*NEL)
          VOLM(I)  = VOLM(I) +LBUF%VOL(I)
        ENDDO
      ENDDO
      IF (DT1 == ZERO) THEN
        DTI =ZERO
      ELSE
        DTI = UN/DT1
      ENDIF 
C-----------------------------
C     POINTS D' INTEGRATION DANS EPAISSEUR
C-----------------------------
      DO ILAY=1,NLAY
        LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
        IF (IGTYP == 22) THEN
          ZT = GEO(IPPOS+ILAY,PID)
          WT = GEO(IPTHK+ILAY,PID)
          MID=IGEO(IPMAT+ILAY,PID)
          MTN=NINT(PM(19,MID))
          DO I=LFT,LLT
            MXT(I)=MID
          ENDDO
        ELSE
          ZT = A_GAUSS(ILAY,NLAY)
          WT = W_GAUSS(ILAY,NLAY)
        ENDIF
C
        CALL SCDEFO3(
     .    DXX, DXY, DXZ, DYX,DYY,DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .    DCXX, DCXY, DCXZ,DCYX, DCYY,DCYZ, DCZX, DCZY, DCZZ,
     .    DC4, DC5,DC6,DHXX, DHXY,DHXZ, 
     .    DHYX, DHYY,DHYZ, DHZX, DHZY, DHZZ,DH4, DH5,DH6,
     .    ZT      ,WT   ,VZL     ,VOLN  ,VOLG   ,
     .    LBUF%VOL,DDHV ,LBUF%SIG,SIGZM ,VOLM   ,
     .    USB     ,LBUF%EINT,OFF ,OFFG,DTI,
     .    GBUF%OFF,DSV,NEL,LBUF%VOL0DP,VOLDP)
        DO I=LFT,LLT
          RHOO(I)= LBUF%RHO(I)
        ENDDO
        IF (ISORTH > 0) THEN
          IF (IGTYP == 22)  
     .      CALL SGETDIR3(LFT,LLT, RX,RY,RZ,TX,TY,TZ,
     .                   R11,R21,R31,R12,R22,R32,
     .                   LBUF%GAMA,DIR,IREP,NEL)
          CALL SCORDEF3(LFT,LLT,DXX,DYY,DZZ,D4,D5,D6,DIR)
          IF (IGTYP == 22) THEN
            DO I=LFT,LLT
              D5(I)=SHF(I)*D5(I)
              D6(I)=SHF(I)*D6(I)
            ENDDO
          ENDIF
        ENDIF
C 
         DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))+DSV(1:NEL)  
        CALL SRHO3( PM         , LBUF%VOL , LBUF%RHO, LBUF%EINT, DIVDE,
     .              FLUX(1,NF1), FLU1(NF1), VOLN    , DVOL     , NGL, MXT, OFF,
     .              0          , GBUF%TAG22 , VOLDP    ,LBUF%VOL0DP,AMU  ,GBUF%OFF)
C
C-----------------------------
C      EXTRACT STRESSES + SMALL STRAIN
C-----------------------------
        CALL CSMALL3(LBUF%SIG,S1,S2,S3,S4,S5,S6,
     .              GBUF%OFF,OFF,NEL)
C------------------------------------------------------
C       CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C       TIME STEP EST CALCULE ICI DT2T
C------------------------------------------------------
        CALL MMAIN(
     1           ELBUF_TAB,NG     ,PM     ,GEO        ,FV	  ,ALE_CONNECT   ,
     2           IXS      ,IPARG  ,V      ,TF         ,NPF	  ,BUFMAT  ,
     3           STI      ,X      ,DT2T   ,NELTST     ,ITYPTST    ,OFFSET  ,
     4           NEL      ,W      ,OFF    ,NGEO       ,
     6           MXT      ,NGL    ,VOLN   ,VD2        ,DVOL	  ,DELTAX  ,
     7           VIS      ,QVIS   ,CXX    ,S1         ,S2	  ,S3	   ,
     8           S4       ,S5     ,S6     ,DXX        ,DYY	  ,DZZ     ,
     9           D4       ,D5     ,D6     ,WXX        ,WYY	  ,WZZ     ,
     A           AJ1      ,AJ2    ,AJ3    ,AJ4        ,AJ5	  ,AJ6     ,
     B           VDX      ,VDY    ,VDZ    ,MUVOID     ,SSP_EQ	  ,AIRE    ,
     C           SIGY     ,ET     ,R1_FREE,LBUF%PLA   ,R3_FREE    ,AMU     ,
     D           DXX      ,DXY    ,DXZ    ,DYX        ,DYY	  ,DYZ     ,
     E           DZX      ,DZY    ,DZZ    ,IPM        ,GAMA	  ,BID     ,
     F           BID      ,BID    ,BID    ,BID        ,BID	  ,BID     ,
     G           ISTRAIN  ,BID    ,BID    ,IBID       ,ILAY	  ,MSSA    ,
     H           DMELS    ,IR     ,IS     ,IT         ,TABLE	  ,BID     ,
     I           BID      ,BID    ,BID    ,IPARG(1,NG),IGEO    ,BID      ,
     J           ITASK    ,NLOC_DMG,VARNL ,MATPARAM_TAB)
C
        DO I=LFT,LLT
          STIN(I) = STIN(I)+STI(I)
        ENDDO
        IF (ISTRAIN == 1) THEN 
          CALL SSTRA3(DXX,DYY,DZZ,D4,D5,
     .                D6, LBUF%STRA, WXX, WYY, WZZ,
     .                OFF,NEL)
        ENDIF
C----------------------------
C     INTERNAL FORCES
C----------------------------
        IF (ISORTH > 0) THEN
         CALL SCROTO_SIG(LFT,LLT,LBUF%SIG,SIGN,DIR,NEL)
!! temporary replaced by (the same) SCROTO_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!         CALL SCROTO(LFT,LLT,LBUF%SIG,SIGN,DIR)
         CALL S6CFINT3(SIGN,
     .     PX1, PX2, PX3, PX4,
     .     PY1, PY2, PY3, PY4,
     .     PZ1, PZ2, PZ3, PZ4,
     .     PX1H, PX2H, PX3H, 
     .     PY1H, PY2H, PY3H, 
     .     PZ1H, PZ2H, PZ3H, 
     .     JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .     B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .     F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .     F15,F25,F35,F16,F26,F36,
     .     VOLN,QVIS,
     .     LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     .     GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,
     .     ZT,WT,VOLG,OFF,NU1,
     .     LBUF%VOL,GBUF%VOL,GBUF%G_PLA,GBUF%G_EPSD,NEL)
        ELSE
         CALL S6CFINT3(LBUF%SIG,
     .     PX1, PX2, PX3, PX4,
     .     PY1, PY2, PY3, PY4,
     .     PZ1, PZ2, PZ3, PZ4,
     .     PX1H, PX2H, PX3H, 
     .     PY1H, PY2H, PY3H, 
     .     PZ1H, PZ2H, PZ3H, 
     .     JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .     B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .     F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .     F15,F25,F35,F16,F26,F36,
     .     VOLN,QVIS,
     .     LBUF%EINT,LBUF%RHO,LBUF%QVIS,LBUF%PLA,LBUF%EPSD,GBUF%EPSD,
     .     GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,
     .     A_GAUSS(ILAY,NLAY),W_GAUSS(ILAY,NLAY),VOLG,OFF,NU1,
     .     LBUF%VOL,GBUF%VOL,GBUF%G_PLA,GBUF%G_EPSD,NEL)
          DO I=LFT,LLT
            OFFS(I)=EP20
          ENDDO
        ENDIF ! IF (ISORTH > 0)
        IOFFS=0
        DO I=LFT,LLT                                        
          OFFG(I)=MIN(OFFG(I),OFF(I))                        
         IF (LBUF%OFF(I) > UN .AND. GBUF%OFF(I) == UN) THEN
           OFFS(I) = MIN(LBUF%OFF(I),OFFS(I))
           IOFFS   = 1                                         
          END IF                                             
        ENDDO                                               
C-----------------------------
      ENDDO  !  ILAY=1,NLAY
C-----------------------------
      IF (IOFFS == 1) THEN
       DO I=LFT,LLT
         IF (OFFS(I).LE.DEUX)GBUF%OFF(I) = OFFS(I)
       END DO
       DO ILAY=1,NLAY
         LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IR,IS,IT)
         IF (IGTYP == 22) THEN
           MID=IGEO(IPMAT+ILAY,PID)
           MTN=NINT(PM(19,MID))
         ENDIF
         DO I=LFT,LLT
           IF (GBUF%OFF(I) > UN) LBUF%OFF(I)=GBUF%OFF(I)
         END DO
       END DO
      END IF
      IF (IGTYP == 22) THEN
        MTN = MTN0
        DO I=LFT,LLT
          MXT(I)=MXT0(I)
        ENDDO
      ENDIF
C-----------------------------
C     SMALL STRAIN 
C-----------------------------
      CALL SMALLB3(GBUF%OFF,OFFG)
C--------------------------
C       BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF (IOUTPRT>0) THEN         
           CALL S6CBILAN(PARTSAV,GBUF%EINT,GBUF%RHO,GBUF%RK ,GBUF%VOL,
     .       VGXA, VGYA, VGZA, VGA2, VOLG,IPARTS,
     .       GRESAV,GRTH,IGRTH,GBUF%OFF,IBID,GBUF%EINTTH,
     .       GBUF%FILL, XGXA, XGYA, XGZA,XGXA2,XGYA2,XGZA2,
     .       XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
      ENDIF
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      CALL S6PROJ3(
     .   X1, X2, X3, X4, X5, X6,
     .   Y1, Y2, Y3, Y4, Y5, Y6,
     .   Z1, Z2, Z3, Z4, Z5, Z6,
     .   F11, F12, F13, F14, F15, F16, 
     .   F21, F22, F23, F24, F25, F26,
     .   F31, F32, F33, F34, F35, F36,
     .   DD)
      CALL VRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F11, F21, F31 )
      CALL VRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F12, F22, F32 )
      CALL VRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F13, F23, F33 )
      CALL VRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F14, F24, F34 )
      CALL VRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F15, F25, F35 )
      CALL VRROTA3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   F16, F26, F36 )
C----------------------------
      IF(NFILSOL/=0) CALL S6FILLOPT(GBUF%FILL,STI,
     1   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     2   F15,F25,F35,F16,F26,F36)    
C----------------------------
      IF (IPARIT == 0) THEN
        CALL S6CUMU3(GBUF%OFF,A,NC1, NC2, NC3, NC4,
     .             NC5, NC6, STIFN,STIN,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36)
      ELSE
        CALL S6CUMU3P(GBUF%OFF,STIN,FSKY,FSKY,IADS,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36)
      ENDIF
      IF (NTSHEG > 0)
     +  CALL SCUMUALPHA6(GBUF%OFF,ALPHA_E,NC1, NC2, NC3, NC4,
     +                       NC5, NC6)
C-----------
      RETURN
      END SUBROUTINE S6CFORC3

