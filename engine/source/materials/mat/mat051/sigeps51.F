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
Chd|  SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        COMPUTE_BFRAC                 source/materials/mat/mat051/bfrac.F
Chd|        DPRAG51                       source/materials/mat/mat051/dprag51.F
Chd|        JCOOK51                       source/materials/mat/mat051/jcook51.F
Chd|        JWL51                         source/materials/mat/mat051/jwl51.F
Chd|        JWLUN51                       source/materials/mat/mat051/jwl51.F
Chd|        M51VOIS2                      source/materials/mat/mat051/m51vois2.F
Chd|        M51VOIS3                      source/materials/mat/mat051/m51vois3.F
Chd|        POLY51                        source/materials/mat/mat051/polynomial51.F
Chd|        POLYUN51                      source/materials/mat/mat051/polynomial51.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS51 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC    ,
     2     NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM   ,NUMEL  ,
     3     RHO0   ,RHO    ,VOLUME  ,EINT    ,SIGY     ,VEL_O  ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NFT      ,V      ,
     B     W      ,X      ,IX      ,N48     ,NIX      ,JTHE   ,
     C     GEO    ,PID    ,ILAY    ,NG      ,ELBUF_TAB,PM     ,
     D     IPARG  ,ALE_CONNECT  ,BUFVOIS ,IPM     ,BUFMAT   ,STIFN  ,
     E     VD2    ,VDX    ,VDY     ,VDZ     ,MAT      ,TAG22  ,
     F     QVIS   , DDVOL ,QQOLD   ,NV46)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
      USE ALE_CONNECTIVITY_MOD
! ARTIFICIAL VISCO :  EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
! NON UTILISE      :  EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX
! PLASTICITY       :  DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
!EINT    : BUFLY%LBUF
!VOLUME  : BUFLY%LBUF
!SIGO    : BUFLY%LBUF
!SIGN    : USERBUF
!SIGV    : USERBUF
!SOUNDSP : local sforc3.F : CXX (boucle sur ILAY)
!VISCMAX : local sforc3.F : SSP_EQ  (boucle sur ILAY)
!UVAR    : BUFLY%MAT%VAR
!OFF     : unused

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX
C NPF     |  *      | I | R | FUNCTION ARRAY
C TF      |  *      | F | R | FUNCTION ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UVAR    | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY (see below)
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C phase 0  dim=N0PHAS
C          UVAR(1) = TEMP  !post
C          UVAR(2) = PLAS  !post
C          UVAR(3) = BFRAC !post
C phase k  dim=NVPHAS
C          IAD = N0PHAS+(k-1)*NVPHAS
c          UVAR(1 + IAD) = AVk                 (EV(NB10) +12k)
C          UVAR(2 + IAD) = SIGxk
C          UVAR(3 + IAD) = SIGyk
C          UVAR(4 + IAD) = SIGzk
C          UVAR(5 + IAD) = SIGxyk
C          UVAR(6 + IAD) = SIGyzk
C          UVAR(7 + IAD) = SIGzxk
C          UVAR(8 + IAD) = EINTk              ! energie IN rho e OUT
C          UVAR(9 + IAD) = RHOk0 * VOLUMk     ! masse IN rho OUT
C          UVAR(10+ IAD) = Qk
C          UVAR(11+ IAD) = VOLUMEk
C          UVAR(12+ IAD) = RHO
C          UVAR(13+ IAD) = DDVOL
C          UVAR(14+ IAD) = SSP
C          UVAR(15+ IAD) = PLAS
C          UVAR(16+ IAD) = Temp
C          UVAR(17+ IAD) = Edif/V
C          UVAR(18+ IAD) = P
C          UVAR(19+ IAD) = EPSEQ_k
C          UVAR(20+ IAD) = rho0 ( /= rho(t=0) in UVAR(12) ) due to inigrav
C          UVAR(21+ IAD) = E0 = rho0.e0 (/= rho.e(t) at t=0 in UVAR 8) due to inigrav
C          UVAR(22+ IAD) = SSP0
C          UVAR(23+ IAD) = AV0
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com06_c.inc"
#include      "com01_c.inc"
#include      "mvsiz_p.inc"
#include      "inter22.inc"
#include      "alefvm.inc"
#include      "tabsiz_c.inc"
#include      "com04_c.inc"
C
      INTEGER NEL, NUPARAM, NUVAR,NFT,N48,NIX,JTHE,NUMEL,
     .        IX(NIX,NUMEL), PID(NEL), ILAY, NG,IPARG(SIPARG),MAT(NEL),
     .        IPM(NPROPMI,NUMMAT),NV46
      my_real
     .   TIME,TIMESTEP,UPARAM(NUPARAM),SIGY(NEL),VK(NEL),PM(NPROPM,NUMMAT),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),BUFVOIS(*),QVIS(NEL),DDVOL(NEL),QQOLD(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   V(3,NUMNOD),W(3,NUMNOD),X(3,NUMNOD),GEO(NPROPG,NUMGEO), BUFMAT(*),
     .   STIFN(MVSIZ),VD2(NEL),VDX(NEL),VDY(NEL),VDZ(NEL)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, PARAMETER :: IDBG = 0
      INTEGER I,J,K,KK,II,ITER,NITER,ibug,UNEPHASE
      my_real
     .   P,PEXT,TFEXTT,P0_NRF,P0_NRFv(MVSIZ),DP0,
     .   GG1,GG2,GG3,TC31,TC32,TC33,
     .   C11,C12,C13,C21,C22,C23,C31,C32,C33,C41,C42,C43,C51,C52,C53,
     .   AV1(MVSIZ),AV2(MVSIZ),AV3(MVSIZ),AV4(MVSIZ),RHO10,RHO20,RHO30,RHO40,RHO1,RHO2,RHO3,RHO4,
     .   RHO0E1,RHO0E2,RHO0E3,RHO0E4,RHO1OLD,RHO2OLD,RHO3OLD,RHO4OLD,
     .   MAS1,MAS2,MAS3,MAS4,EINT1,EINT2,EINT3,EINT4,EINT_NEW,EINT_k10,
     .   E1_INF, E2_INF, E3_INF, E4_INF,
     .   DPDV1,DPDV2,DPDV3,DPDV4,
     .   P1,P2,P3,P4,P1I,P2I,P3I,P4I,VQ0,VQ1,VQ2,VQ3,VQ4,
     .   Q0, Q1,Q2,Q3,Q4,Q1OLD,Q2OLD,Q3OLD,Q4OLD,SSP1,SSP2,SSP3,SSP4,SSP(0:4),BETA,
     .   MU1,MU2,MU3,MU4,
     .   MU1P1, MU2P1, MU3P1, MU4P1,
     .   DVDP1,DVDP2,DVDP3,DVDP4,DDVOL1,DDVOL2,DDVOL3,DDVOL4,
     .   V10,V20,V30,V40,V1,V2,V3,V4,V1OLD,V2OLD,V3OLD,V4OLD,
     .   V1I,V2I,V3I,V4I,E01,E02,E03,E04,PM1,PM2,PM3,PM4,
     .   ECOLD1,ECOLD2,ECOLD3,ECOLD4,SPH1,SPH2,SPH3,SPH4,
     .   T10,T20,T30,T40,H1,H2,H3,H4,TEMP1,TEMP2,TEMP3,TEMP4,
     .   XL,QAL,QBL,Q,POLD,QOLD,MASS,
     .   VISA1,VISB1,AA,DD,QA,QB,UNDT,
     .   VOLD,DVOL,
     .   C01,C02,C03,C04,ABCS,DYDX,EDIF1,EDIF2,EDIF3,EDIF4,
     .   V0,DE(NEL),
     .   AA1,AA2,AA3,
     .   EEE,AAA,BBB,CC1,CC2,CC3,U2,
     .   TBURN,ECOLD,T,
     .   VCRT1,VCRT2,VCRT3,U21,U22,U23,GV1,GV2,GV3,RV1,RV2,RV3,
     .   VN,X0,Y0,Z0,VX,VY,VZ,NX,NY,NZ,RHO1A,P1A,E01A,RHO2A,P2A,E02A,
     .   RHO3A,P3A,E03A,FAC
      my_real
     .   DEPS(6,MVSIZ),EPD(MVSIZ),
     .   P1OLD(MVSIZ),P2OLD(MVSIZ),P3OLD(MVSIZ),P4OLD(MVSIZ),
     .   SIGD(6,MVSIZ), EINT0(MVSIZ),PLAS1(MVSIZ),PLAS2(MVSIZ),PLAS3(MVSIZ),
     .   VOL(MVSIZ), TEMP(MVSIZ),
     .   EPSEQ1(MVSIZ), EPSEQ2(MVSIZ), EPSEQ3(MVSIZ),
     .   PM5, BFRAC(MVSIZ),
     .   VEL_N(MVSIZ),VEL_O(MVSIZ), VEL(MVSIZ),
     .   EIV(0:4,MVSIZ), RHOV(0:4,MVSIZ), PV(0:4,MVSIZ), TV(0:4,MVSIZ),RHO0V(0:4,MVSIZ),
     .   AVV(0:4,MVSIZ), SSPv(0:4,MVSIZ),EPSPv(0:4,MVSIZ),
     .   RHOC2(4),RHOC20,ROC,
     .   alpha, myVAR, PP(MVSIZ), PP0(MVSIZ), PFAR, TAG22(MVSIZ), dbVOLD(4), dbVOLD_f(4), Tol51,
     .   XL1,XL2,XL3,XL4,QAL1,QAL2,QAL3,QAL4,QBL1,QBL2,QBL3,QBL4,DD2,
     .   TOL, TOL2, ERROR, ERROR2, COEF1, COEF2, COEF3, COEF4,
     .   STEP, DV1, DV2, DV3, DV4,
     .   GRUN1, GRUN2, GRUN3, GRUN4,
     .   EINT1_INI, EINT2_INI, EINT3_INI, EINT4_INI,
     .   ALPHA1OLD, ALPHA2OLD, ALPHA3OLD, ALPHA4OLD,
     .   RHOOLD, SSP1_INI, SSP2_INI, SSP3_INI, SSP4_INI, VFRAC(MVSIZ), MACH,
     .   E01f,E02f,E03f,E04f,RHO1f,RHO2f,RHO3f,RHO4f
      my_real :: VEL_IN, MOM
      my_real :: VISC1, VISC2, VISC3,VISC4
      INTEGER :: IVEL
      INTEGER :: CONT
      INTEGER IFLG,IAV1,IAV2,IAV3,IRHO1,IRHO2,IRHO3,IE1,IE2,IE3,IEXP,
     .        IOPT, IPLA, IPLA1, IPLA2, IPLA3,
     .        K1,K2,K3,K4, ISUPERSONIC, IVOI,ML,IFORM,IADBUF
      INTEGER ::
     .   IX1,IX2,IX3,IX4
      my_real ::
     .   X13, Y13, Z13, X24, Y24, Z24, XN, YN, ZN
      INTEGER IBUG_ID(2),ICF3D(4,6), ICF2D(2,4), IAD2
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY
C
      DATA ICF3D/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
      DATA ICF2D/1,2,2,3,3,4,4,1/
C
      IF(TIMESTEP>ZERO)THEN
        UNDT = ONE/TIMESTEP
      ELSE
        UNDT = ZERO
      ENDIF
C
      IBUG_ID(1:2) =  (/0, 0/)

      IF(INT22==0)THEN
        tol51   = EM10
      ELSE
        tol51   = EM20
      ENDIF
      TOL51 = EM10
C
      TFEXTT=ZERO
      MU1P1  = ONE
      MU2P1  = ONE
      MU3P1  = ONE
      MU4P1  = ONE   
      
      GBUF   => ELBUF_TAB(NG)%GBUF           
      LBUF   => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
      BUFLY  => ELBUF_TAB(NG)%BUFLY(ILAY)
C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===============================================================================C
C                                                                               C
C     LAW51 : TRI MATERIAL LAW & OPTIONAL HIGH EXPLOSIVE MATERIAL               C
C                                                                               C
C      + SOLID, LIQUID, GAS, HE (IFLG==0.or.IFLG==1).and.(IEXP==0.or.IEXP==1)   C
C      + INLET CONDITIONS (IFLG == 2)                                           C
C      + OUTLET CONDITIONS (IFLG == 3)                                          C
C      + INLET CONDITIONSFLUID STAGNATION PRESSURE   (IFLG == 4.or.IFLG == 5)   C
C                                                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===============================================================================C
C
C     |===========================================!
C     | Reading Material Flags Through UPARAM()   !
C     |===========================================!

      VISA1  = UPARAM(1)  !1st global Viscosity coefficient : mu
      VISB1  = UPARAM(3)  !2nd vglobal iscosity coefficient : lambda    !VISB1 =(VISV1-2.VISA1)/3.

      VISC1 = UPARAM(81) !law 6 parameter when law51 is defined from law6 (iform=12)
      VISC2 = UPARAM(82)
      VISC3 = UPARAM(83)
      VISC4 = UPARAM(84)

      !INITIAL volume fraction (in case of inivol, it was updated in Starter)
      K=N0PHAS
      DO I=1,NEL
        AV1(I)=UVAR(I,K+23)
      ENDDO
      K=N0PHAS+NVPHAS
      DO I=1,NEL
        AV2(I)=UVAR(I,K+23)
      ENDDO
      K=N0PHAS+2*NVPHAS
      DO I=1,NEL
        AV3(I)=UVAR(I,K+23)
      ENDDO
       K=N0PHAS+3*NVPHAS
      DO I=1,NEL
        AV4(I)=UVAR(I,K+23)
      ENDDO

      PFAR   = UPARAM(7)
      PEXT   = UPARAM(8)

      RHO10  = UPARAM(9)
      RHO20  = UPARAM(10)
      RHO30  = UPARAM(11)
      RHO40  = UPARAM(47)
      !This avoid to divide by zero for empty phase
      IF(RHO10 == ZERO)RHO10=EM20
      IF(RHO20 == ZERO)RHO20=EM20
      IF(RHO30 == ZERO)RHO30=EM20
      IF(RHO40 == ZERO)RHO40=EM20

      RHO1   = RHO10
      RHO2   = RHO20
      RHO3   = RHO30
      RHO4   = RHO40

      C11    = UPARAM(12)
      C12    = UPARAM(13)
      C13    = UPARAM(14)

      C21    = UPARAM(15)
      C22    = UPARAM(16)
      C23    = UPARAM(17)

      C31    = UPARAM(18)
      C32    = UPARAM(20)
      C33    = UPARAM(21)

      C41    = UPARAM(22)
      C42    = UPARAM(23)
      C43    = UPARAM(24)

      C51    = UPARAM(25)
      C52    = UPARAM(26)
      C53    = UPARAM(27)

      GG1    = UPARAM(28)
      GG2    = UPARAM(29)
      GG3    = UPARAM(30)

      IFLG   = NINT(UPARAM(31))
      IOPT   = NINT(UPARAM(61))
      IEXP   = NINT(UPARAM(55))
      ALPHA  = UPARAM(62)

      IPLA   = NINT(UPARAM(63))
      IPLA1  = NINT(UPARAM(64))
      IPLA2  = NINT(UPARAM(65))
      IPLA3  = NINT(UPARAM(66))

      E01    = UPARAM(32)
      E02    = UPARAM(33)
      E03    = UPARAM(34)
      E04    = UPARAM(48)

      C01    = UPARAM(35)
      C02    = UPARAM(36)
      C03    = UPARAM(37)
      C04    = UPARAM(49)

      PM1    = UPARAM(39)
      PM2    = UPARAM(40)
      PM3    = UPARAM(41)
      PM4    = UPARAM(56)

      E1_INF = UPARAM(57) !E1_INF (t=0) Eint1_INF (t>0)
      E2_INF = UPARAM(58) !E2_INF (t=0) Eint2_INF (t>0)
      E3_INF = UPARAM(59) !E3_INF (t=0) Eint3_INF (t>0)
      E4_INF = UPARAM(60) !E4_INF (t=0) Eint4_INF (t>0)

      SPH1   = UPARAM(112)
      SPH2   = UPARAM(162)
      SPH3   = UPARAM(212)
      SPH4   = UPARAM(262)

      T10    = UPARAM(113)
      T20    = UPARAM(163)
      T30    = UPARAM(213)
      T40    = UPARAM(263)

      TC31   = THREE*C31
      TC32   = THREE*C32
      TC33   = THREE*C33

      VEL_IN = UPARAM(75)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===========================================================================C
C                                                                           C
C     LAW51 : INLET CONDITIONS (IFLG == 2)                                  C
C                                                                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===========================================================================C

      IF(IFLG == 2)THEN
        ABCS = TIME / UPARAM(38)

        IAV1  = IFUNC(1)
        IRHO1 = IFUNC(2)
        IE1   = IFUNC(3)
        IAV2  = IFUNC(4)
        IRHO2 = IFUNC(5)
        IE2   = IFUNC(6)
        IAV3  = IFUNC(7)
        IRHO3 = IFUNC(8)
        IE3   = IFUNC(9)
        IVEL  = IFUNC(10)


        IF(IAV1 /= 0)THEN
          myVAR = FINTER(IAV1,ABCS,NPF,TF,DYDX)
          AV1(1:NEL) = myVAR * AV1(1:NEL)
        ENDIF
        IF(IAV2 /= 0)THEN
          myVAR = FINTER(IAV2,ABCS,NPF,TF,DYDX)
          AV2(1:NEL) = myVAR * AV2(1:NEL)
        ENDIF
        IF(IAV3 /= 0)THEN
          myVAR = FINTER(IAV3,ABCS,NPF,TF,DYDX)
          AV3(1:NEL) = myVAR * AV3(1:NEL)
        ENDIF

        !rho0,E0,P0,SSP0 in UVAR(20,21,22,24)

        !scale factors
        RHO1 = ONE
        RHO2 = ONE
        RHO3 = ONE
        RHO4 = ONE
        E01  = ONE
        E02  = ONE
        E03  = ONE
        E04  = ONE
        IF(IRHO1/=0)RHO1 = FINTER(IRHO1,ABCS,NPF,TF,DYDX)
        IF(IRHO2/=0)RHO2 = FINTER(IRHO2,ABCS,NPF,TF,DYDX)
        IF(IRHO3/=0)RHO3 = FINTER(IRHO3,ABCS,NPF,TF,DYDX)
        IF(IE1/=0)E01 = FINTER(IE1,ABCS,NPF,TF,DYDX)
        IF(IE2/=0)E02 = FINTER(IE2,ABCS,NPF,TF,DYDX)
        IF(IE3/=0)E03 = FINTER(IE3,ABCS,NPF,TF,DYDX)
        IF(IVEL/=0)VEL_IN = VEL_IN*FINTER(IVEL,ABCS,NPF,TF,DYDX)


        K1 = N0PHAS
        K2 = N0PHAS+NVPHAS
        K3 = N0PHAS+2*NVPHAS
        K4 = N0PHAS+3*NVPHAS

        DO I=1,NEL
          P1=UVAR(I,4)
          P2=UVAR(I,4)
          P3=UVAR(I,4)
          MU1=UVAR(I,K1+20)*RHO1/RHO10 - ONE
          IF(UVAR(I,K1+20)/=ZERO)THEN
            P1 = ( C01 + C11*MU1
     .      	   + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1)
     .      	   + (C41 + C51*MU1)*E01*UVAR(I,K1+21)*RHO10/RHO1/UVAR(I,K1+20) )
          ENDIF
          P1 = MAX(P1,PM1)
          MU2=UVAR(I,K2+20)*RHO2/RHO20 - ONE
          IF(UVAR(I,K2+20)/=ZERO)THEN
            P2 = ( C02 + C12*MU2
     .    	   + MAX(MU2,ZERO)*(C22*MU2 + C32*MU2*MU2)
     .    	   + (C42 + C52*MU2)*E02*UVAR(I,K2+21)*RHO20/RHO2/UVAR(I,K2+20) )
          ENDIF
          P2 = MAX(P2,PM2)
          MU3=UVAR(I,K3+20)*RHO3/RHO30 - ONE
          IF(UVAR(I,K3+20)/=ZERO)THEN
            P3 = ( C03 + C13*MU3
     .    	   + MAX(MU3,ZERO)*(C23*MU3 + C33*MU3*MU3)
     .    	   + (C43 + C53*MU3)*E03*UVAR(I,K3+21)*RHO30/RHO3/UVAR(I,K3+20) )
          ENDIF
          P3 = MAX(P3,PM3)
          PP(I) = P1 * AV1(I) + P2 * AV2(I) + P3 * AV3(I)
        ENDDO

        DO I=1,NEL

          UVAR(I,1)  =  ZERO
          UVAR(I,2)  =  ZERO
          UVAR(I,3)  =  ZERO !BFRAC

C         |====================!
C         |     material 1     !
C         |====================!
          KK = N0PHAS
          UVAR(I,1+KK)  = AV1(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E01*UVAR(I,K1+21)  !E0*AV1
          UVAR(I,9+KK)  = RHO1*UVAR(I,K1+20) !RHO1*V1
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV1(I)
          UVAR(I,12+KK) = RHO1*UVAR(I,K1+20)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 2     !
C         |====================!
          KK = N0PHAS + NVPHAS
          UVAR(I,1+KK)  = AV2(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E02*UVAR(I,K2+21)
          UVAR(I,9+KK)  = RHO2*UVAR(I,K2+20)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV2(I)
          UVAR(I,12+KK) = RHO2*UVAR(I,K2+20)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 3     !
C         |====================!
          KK = N0PHAS + 2*NVPHAS
          UVAR(I,1+KK)  = AV3(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E03*UVAR(I,K3+21)
          UVAR(I,9+KK)  = RHO3*UVAR(I,K3+20)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV3(I)
          UVAR(I,12+KK) = RHO3*UVAR(I,K3+20)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 4     !
C         |====================!
          IF(TRIMAT == 4) THEN
            KK = N0PHAS + 3*NVPHAS
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = ZERO
            !------------------!
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = ZERO
            UVAR(I,19+KK) = ZERO
          ENDIF

C         |==================================!
C         |     ELEMENT  OUTPUT  (IFLG == 2) !
C         |==================================!
          RHO(I) = RHO1*UVAR(I,K1+20) * AV1(I) + RHO2*UVAR(I,K2+20) * AV2(I) + RHO3*UVAR(I,K3+20) * AV3(I)
          SOUNDSP(I) = EM20
          VISCMAX(I) = ZERO
          EEE        = VOLUME(I) * (E01*UVAR(I,K1+21) * AV1(I) + E02*UVAR(I,K2+21) * AV2(I) + E03*UVAR(I,K3+21) * AV3(I))
          TFEXTT      = ZERO !TFEXTT + EEE - EINT(I)
          EINT(I)    = EEE
          DD = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)
          SIGNXX(I) = -PP(I)
          SIGNYY(I) = -PP(I)
          SIGNZZ(I) = -PP(I)

          IF(VEL_IN /= ZERO .OR. IVEL /= 0)THEN
            !normal face
            IF(N2D==0)THEN
               II     = I+NFT
               IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
               DO  J=1,NV46
                IVOI  = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
                ML    = 51
                IFORM = 1000
                IF(IVOI /= 0)            ML     = NINT(PM(19,IX(1,IVOI)))
                IF(IVOI /= 0)            IADBUF = IPM(7,IX(1,IVOI))
                IF(IVOI /= 0.AND.ML==51) IFORM  = BUFMAT(IADBUF+31-1)
                IF(ML==51.AND.IFORM<=1)  EXIT
               ENDDO
               XN = ZERO
               YN = ZERO
               ZN = ZERO
               IF(ML==51 .AND. IFORM<=1)THEN
                IX1 = IX(ICF3D(1,J)+1,II)
                IX2 = IX(ICF3D(2,J)+1,II)
                IX3 = IX(ICF3D(3,J)+1,II)
                IX4 = IX(ICF3D(4,J)+1,II)
                X13 = X(1,IX3)-X(1,IX1)
                Y13 = X(2,IX3)-X(2,IX1)
                Z13 = X(3,IX3)-X(3,IX1)
                X24 = X(1,IX4)-X(1,IX2)
                Y24 = X(2,IX4)-X(2,IX2)
                Z24 = X(3,IX4)-X(3,IX2)
                XN  = -Y13*Z24+Z13*Y24
                YN  = -Z13*X24+X13*Z24
                ZN  = -X13*Y24+Y13*X24
                FAC = ONE/SQRT(XN**2+YN**2+ZN**2)
                XN  = XN*FAC
                YN  = YN*FAC
                ZN  = ZN*FAC
              ENDIF
            ELSE !IF(N2D==0)
              II     = I+NFT
              IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
              DO  J=1,NV46
               IVOI  =  ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
               ML    = 51
               IFORM = 1000
               IF(IVOI /= 0)            ML     = NINT(PM(19,IX(1,IVOI)))
               IF(IVOI /= 0)            IADBUF = IPM(7,IX(1,IVOI))
               IF(IVOI /= 0.AND.ML==51) IFORM  = BUFMAT(IADBUF+31-1)     !si voisin est materiau 51 alors on recupere UPARAM(31)=IFLG (IFLG=0,1 pour IFROM=0,1ou10)
               IF(ML==51.AND.IFORM<=1)  EXIT                             ! si materiau voisin est loi 51 Iform=1 ou 10 alors on a trouve
              ENDDO
              XN = ZERO
              YN = ZERO
              ZN = ZERO
              IF(ML==51 .AND. IFORM<=1)THEN
                IX1 = IX(ICF2D(1,J)+1,II)
                IX2 = IX(ICF2D(2,J)+1,II)
                XN   = ZERO
                YN   = (X(2,IX2)-X(2,IX1))
                ZN   = (X(3,IX2)-X(3,IX1))
                FAC  = ONE/SQRT(YN**2+ZN**2)
                YN  = YN*FAC
                ZN  = ZN*FAC
              ENDIF
            ENDIF
            ! sauvegarde de la quantite de mouvement imposee
            IF(IALEFVM  /=  0)THEN
              MOM = RHO(I) * VEL_IN * VOLUME(I)
              GBUF%MOM(NEL*(1-1)+I) = -MOM * XN
              GBUF%MOM(NEL*(2-1)+I) = -MOM * YN
              GBUF%MOM(NEL*(3-1)+I) = -MOM * ZN
            ELSE
              IF(N2D==0)THEN
                V(1:3,IX1) = VEL_IN * (/XN,YN,ZN/)
                V(1:3,IX2) = VEL_IN * (/XN,YN,ZN/)
                V(1:3,IX3) = VEL_IN * (/XN,YN,ZN/)
                V(1:3,IX4) = VEL_IN * (/XN,YN,ZN/)
              ELSE
                V(1:3,IX1) = VEL_IN * (/XN,YN,ZN/)
                V(1:3,IX2) = VEL_IN * (/XN,YN,ZN/)
              ENDIF
            ENDIF
          ENDIF

          VDX(I) = ZERO
          VDY(I) = ZERO
          VDZ(I) = ZERO
C
        ENDDO
C
#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        TFEXT = TFEXT + TFEXTT
C
        RETURN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===========================================================================C
C                                                                           C
C     LAW51 : OUTLET CONDITIONS (IFLG == 3)                                 C
C                                                                           C
C===========================================================================C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ELSEIF(IFLG == 3)THEN

        DO I=1,NEL
          PP0(I) = C01 * AV1(I) + C02 * AV2(I) + C03 * AV3(I)
        ENDDO

        IF(TIME == 0.0)THEN
          DO I=1,NEL
            SIGOXX(I) = -PP0(I)
            SIGOYY(I) = -PP0(I)
            SIGOZZ(I) = -PP0(I)
            SIGOXY(I) = ZERO
            SIGOYZ(I) = ZERO
            SIGOZX(I) = ZERO
            RHO(I) = RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I)
          ENDDO
        ENDIF


        DO I=1,NEL
          DD = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)
          BBB = ONE - ALPHA          
          IF(DD > ZERO)THEN
            SIGNXX(I) = SIGOXX(I)
            SIGNYY(I) = SIGOYY(I)
            SIGNZZ(I) = SIGOZZ(I)
            SIGNXY(I) = SIGOXY(I)
            SIGNYZ(I) = SIGOYZ(I)
            SIGNZX(I) = SIGOZX(I)
            IF(IOPT==1)RHO(I) =
     .        ALPHA*(RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I))+ BBB*RHO(I)
          ELSE
            AAA = -ALPHA * PP0(I)
            SIGNXX(I) = BBB * SIGOXX(I) + AAA
            SIGNYY(I) = BBB * SIGOYY(I) + AAA
            SIGNZZ(I) = BBB * SIGOZZ(I) + AAA
            !IF(IOPT==0)BBB=ZERO
            SIGNXY(I) = BBB * SIGOXY(I)
            SIGNYZ(I) = BBB * SIGOYZ(I)
            SIGNZX(I) = BBB * SIGOZX(I)
          ENDIF
          UVAR(I,1)  =  ZERO
          UVAR(I,2)  =  ZERO
          UVAR(I,3)  =  ZERO !BFRAC

C         |====================!
C         |     material 1     !
C         |====================!
          KK = N0PHAS
          UVAR(I,1+KK)  = AV1(I)
          UVAR(I,8+KK)  = E01
          UVAR(I,9+KK)  = RHO1
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV1(I)
          UVAR(I,12+KK) = RHO1
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 2     !
C         |====================!
          KK = N0PHAS + NVPHAS
          UVAR(I,1+KK)  = AV2(I)
          UVAR(I,8+KK)  = E02
          UVAR(I,9+KK)  = RHO2
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV2(I)
          UVAR(I,12+KK) = RHO2
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 3     !
C         |====================!
          KK = N0PHAS + 2*NVPHAS
          UVAR(I,1+KK)  = AV3(I)
          UVAR(I,8+KK)  = E03
          UVAR(I,9+KK)  = RHO3
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV3(I)
          UVAR(I,12+KK) = RHO3
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 4     !
C         |====================!
          IF(TRIMAT == 4) THEN
            KK = N0PHAS + 3*NVPHAS
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = ZERO
            !------------------!
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = ZERO
            UVAR(I,19+KK) = ZERO
          ENDIF

C         |==================================!
C         |     ELEMENT  OUTPUT  (IFLG == 3) !
C         |==================================!
          IF(IOPT==0)RHO(I) = RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I)
          EEE        = VOLUME(I) * (E01 * AV1(I) + E02 * AV2(I) + E03 * AV3(I))
          TFEXTT     = ZERO ! TFEXTT + EEE - EINT(I)
          EINT(I)    = EEE
          SOUNDSP(I) = EM20
          VISCMAX(I) = ZERO

          VDX(I) = ZERO
          VDY(I) = ZERO
          VDZ(I) = ZERO

        ENDDO
C
#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        TFEXT = TFEXT + TFEXTT
C
        RETURN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===========================================================================C
C                                                                           C
C     LAW51 : GAS OR LIQUID STAGNATION PRESSURE                             C
C      INLET CONDITIONS (IFLG == 4.or.IFLG == 5)                            C
C                                                                           C
C===========================================================================C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ELSEIF(IFLG == 4.or.IFLG == 5)THEN
        ! Conditions d'arret pour gaz parfait ou liquide selon la phase
        ABCS  = TIME / UPARAM(38)
        IAV1  = IFUNC(1)
        IRHO1 = IFUNC(2)
        IE1   = IFUNC(3)
        IAV2  = IFUNC(4)
        IRHO2 = IFUNC(5)
        IE2   = IFUNC(6)
        IAV3  = IFUNC(7)
        IRHO3 = IFUNC(8)
        IE3   = IFUNC(9)
        IF(IAV1 /= 0)THEN
          myVAR = FINTER(IAV1,ABCS,NPF,TF,DYDX)
          AV1(1:NEL) = myVAR * AV1(1:NEL)
        ENDIF
        IF(IAV2 /= 0)THEN
          myVAR = FINTER(IAV2,ABCS,NPF,TF,DYDX)
          AV2(1:NEL) = myVAR * AV2(1:NEL)
        ENDIF
        IF(IAV3 /= 0)THEN
          myVAR = FINTER(IAV3,ABCS,NPF,TF,DYDX)
          AV3(1:NEL) = myVAR * AV3(1:NEL)
        ENDIF
        K1 = N0PHAS
        K2 = N0PHAS+NVPHAS
        K3 = N0PHAS+2*NVPHAS
        K4 = N0PHAS+3*NVPHAS
        RHO1f = ONE
        RHO2f = ONE
        RHO3f = ONE
	E01f  = ONE
	E02f  = ONE
	E03f  = ONE
        IF(IRHO1/=0)RHO1f = FINTER(IRHO1,ABCS,NPF,TF,DYDX)
        IF(IRHO2/=0)RHO2f = FINTER(IRHO2,ABCS,NPF,TF,DYDX)
        IF(IRHO3/=0)RHO3f = FINTER(IRHO3,ABCS,NPF,TF,DYDX)
        IF(IE1/=0)E01f = FINTER(IE1,ABCS,NPF,TF,DYDX)
        IF(IE2/=0)E02f = FINTER(IE2,ABCS,NPF,TF,DYDX)
        IF(IE3/=0)E03f = FINTER(IE3,ABCS,NPF,TF,DYDX)
        DO I=1,NEL
          E01=E01f
          E02=E02f
          E03=E03f
          RHO1=RHO1f
          RHO2=RHO2f
          RHO3=RHO3f                    
          MU1=RHO1*UVAR(I,20+K1)/RHO10 - ONE
          P1 = ( C01 + C11*MU1+ MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1) + (C41 + C51*MU1)*E01*UVAR(I,21+K1)*RHO10/RHO1/UVAR(I,20+K1) )
          P1 = MAX(P1,PM1)
          MU2=RHO2*UVAR(I,20+K2)/RHO20 - ONE
          P2 = ( C02 + C12*MU2+ MAX(MU2,ZERO)*(C22*MU2 + C32*MU2*MU2) + (C42 + C52*MU2)*E02*UVAR(I,21+K2)*RHO20/RHO2/UVAR(I,20+K2) )
          P2 = MAX(P2,PM2)
          MU3=RHO3*UVAR(I,20+K3)/RHO30 - ONE
          P3 = ( C03 + C13*MU3 + MAX(MU3,ZERO)*(C23*MU3 + C33*MU3*MU3)+ (C43 + C53*MU3)*E03*UVAR(I,21+K3)*RHO30/RHO3/UVAR(I,20+K3) )
          P3 = MAX(P3,PM3)
          !switching to total pressure formulation since following lines are based on it.
          P1 = PEXT + P1
          P2 = PEXT + P2
          P3 = PEXT + P3
          !conditions d'arret
          RHO1A = RHO1*UVAR(I,20+K1)
          P1A = P1
          E01A = E01*UVAR(I,21+K1)
          RHO2A = RHO2*UVAR(I,20+K2)
          P2A = P2
          E02A = E02*UVAR(I,21+K2)
          RHO3A = RHO3*UVAR(I,20+K3)
          P3A = P3
          E03A = E03*UVAR(I,21+K3)
          CC1 = (C41+ONE)*P1/RHO1/UVAR(I,20+K1)
          CC2 = (C42+ONE)*P2/RHO2/UVAR(I,20+K2)
          CC3 = (C43+ONE)*P3/RHO3/UVAR(I,20+K3)
          AA1 = TWO / (C41+TWO)
          AA2 = TWO / (C42+TWO)
          AA3 = TWO / (C43+TWO)
          ! vitesse critique calculee dans LECM11, ce qui est discutable (yann)
          VCRT1 = AA1*CC1
          VCRT2 = AA2*CC2
          VCRT3 = AA3*CC3
          !------------
          UVAR(I,1:N0PHAS)  =  ZERO
          II = I + NFT
          VN = ZERO
          U2 = ZERO
          X0 = ZERO
          Y0 = ZERO
          Z0 = ZERO
          DO KK=1,N48
            K  = IX(1+KK,II)
            X0 = X0 + X(1,K)
            Y0 = Y0 + X(2,K)
            Z0 = Z0 + X(3,K)
          ENDDO
          X0 = X0 / N48
          Y0 = Y0 / N48
          Z0 = Z0 / N48
          DO KK=1,N48
            K  = IX(1+KK,II)
            VX = V(1,K)-W(1,K)
            VY = V(2,K)-W(2,K)
            VZ = V(3,K)-W(3,K)
            U2 = U2 + VX*VX + VY*VY + VZ*VZ
            NX = X(1,K)-X0
            NY = X(2,K)-Y0
            NZ = X(3,K)-Z0
            VN = VN + VX*NX + VY*NY + VZ*NZ
          ENDDO
          U2 = U2 / (N48/2)
          IF(VN <= ZERO) U2 = ZERO

          !===============================!
          !     material 1 computation    !
          !===============================!
          IF(AV1(I) > ZERO)THEN
            !---GAZ
            IF((IFLG == 4.and.C41 /= ZERO).or. (IFLG == 5.and.C41 /= ZERO.and.C11 == ZERO))THEN
              U21 = MIN(U2,VCRT1)
              GV1 = C41/2/(C41+1)*RHO1A*U21/P1A
              IF(GV1 > EM4)THEN
                AAA  = ONE - GV1
                RHO1 = RHO1A * AAA**(ONE/C41)
                BBB  = AAA**((C41+ONE)/C41)
                E01  = E01A * BBB
                P1   = P1A * BBB
                !yann     test sur 0.0001 pour cas incompressible (Bernouilli classique)
              ELSE
                RV1  = HALF*RHO1A*U21
                RHO1 = RHO1A *(ONE- RV1/(C41+ONE)/P1A)
                P1   = P1A - RV1
                E01  = P1/C41
              ENDIF
            !---LIQUID
            ELSE
              FAC = (C11 + HALF*RHO10*U2)
              IF(FAC/=ZERO)THEN
                RHO1 = RHO1A*C11/FAC
                P1   = P1A - HALF*RHO1*U2
                E01  = E01A + (ONE - RHO1/RHO1A)*P1
              ENDIF
            ENDIF
          ELSE
           !unused submaterial
           RHO1 = RHO10
          ENDIF! (AV1(I) > ZERO)
          !================!
          !     output     !
          !================!
          KK = N0PHAS
          UVAR(I,1+KK)  = AV1(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E01
          UVAR(I,9+KK)  = RHO1
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV1(I)
          UVAR(I,12+KK) = RHO1
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !===============================!
          !     material 2 computation    !
          !===============================!
          IF(AV2(I) > ZERO)THEN
            IF((IFLG == 4.and.C42 /= ZERO).or. (IFLG == 5.and.C42 /= ZERO.and.C12 == ZERO))THEN
              U22 = MIN(U2,VCRT2)
              GV2 = C42/2/(C42+1)*RHO2A*U22/P2A
              IF(GV2 > EM4)THEN
                AAA  = ONE - GV2
                RHO2 = RHO2A * AAA**(ONE/C42)
                BBB  = AAA**((C42+ONE)/C42)
                E02  = E02A * BBB
                P2   = P2A * BBB
              ELSE
                RV2  = HALF*RHO2A*U22
                RHO2 = RHO2A *(ONE- RV2/(C42+ONE)/P2A)
                P2   = P2A - RV2
                E02  = P2/C42
              ENDIF
            ELSE
              FAC = (C12 + HALF*RHO20*U2)
              IF(FAC/=ZERO)THEN
                RHO2 = RHO2A*C12/FAC
                P2   = P2A - HALF*RHO2*U2
                E02  = E02A + (ONE - RHO2/RHO2A)*P2
              ENDIF
            ENDIF
          ELSE
           !unused submaterial
           RHO2 = RHO20
          ENDIF! (AV2(I) > ZERO)
          !================!
          !     output     !
          !================!
          KK = N0PHAS + NVPHAS
          UVAR(I,1+KK)  = AV2(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E02
          UVAR(I,9+KK)  = RHO2
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV2(I)
          UVAR(I,12+KK) = RHO2
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO

          !===============================!
          !     material 3 computation    !
          !===============================!
          IF(AV3(I) > ZERO)THEN
            IF((IFLG == 4.and.C43 /= ZERO).or. (IFLG == 5.and.C43 /= ZERO.and.C13 == ZERO))THEN
              U23 = MIN(U2,VCRT3)
              GV3 = C43/2/(C43+1)*RHO3A*U23/P3A
              IF(GV3 > EM4)THEN
                AAA  = ONE - GV3
                RHO3 = RHO3A * AAA**(ONE/C43)
                BBB  = AAA**((C43+ONE)/C43)
                E03  = E03A * BBB
                P3   = P3A * BBB
              ELSE
                RV3  = HALF*RHO3A*U23
                RHO3 = RHO3A *(ONE- RV3/(C43+ONE)/P3A)
                P3   = P3A - RV3
                E03  = P3/C43
              ENDIF
            ELSE
              FAC = (C13 + HALF*RHO30*U2)
              IF(FAC/=ZERO)THEN
                RHO3 = RHO3A*C13/FAC
                P3   = P3A - HALF*RHO3*U2
                E03  = E03A + (ONE - RHO3/RHO3A)*P3
              ENDIF
            ENDIF
          ELSE
            !unused submaterial
            RHO3 = RHO30
          ENDIF! (AV3(I) > ZERO)
          !================!
          !     output     !
          !================!
          KK = N0PHAS + 2*NVPHAS
          UVAR(I,1+KK)  = AV3(I)
          UVAR(I,2+KK)  = ZERO
          UVAR(I,3+KK)  = ZERO
          UVAR(I,4+KK)  = ZERO
          UVAR(I,5+KK)  = ZERO
          UVAR(I,6+KK)  = ZERO
          UVAR(I,7+KK)  = ZERO
          UVAR(I,8+KK)  = E03
          UVAR(I,9+KK)  = RHO3
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AV3(I)
          UVAR(I,12+KK) = RHO3

          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = ZERO
          UVAR(I,15+KK) = ZERO
          UVAR(I,16+KK) = ZERO
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = ZERO
          UVAR(I,19+KK) = ZERO
          !===============================!
          !     material 4 output         !
          !===============================!
          IF(TRIMAT == 4) THEN
            KK = N0PHAS + 3*NVPHAS
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = ZERO
            !------------------!
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = ZERO
            UVAR(I,19+KK) = ZERO
          ENDIF

          !================================================!
          !     ELEMENT  OUTPUT  (IFLG == 4.or.IFLG == 5)  !
          !================================================!
          RHO(I) = RHO1 * AV1(I) + RHO2 * AV2(I) + RHO3 * AV3(I)
          SOUNDSP(I) = EM20
          VISCMAX(I) = ZERO
          EEE        = VOLUME(I) * (E01 * AV1(I) + E02 * AV2(I) + E03 * AV3(I))
          TFEXTT     = ZERO !TFEXTT + EEE - EINT(I)
          EINT(I)    = EEE
          P1 = P1 - PEXT
          P2 = P2 - PEXT
          P3 = P3 - PEXT
          P  = P1 * AV1(I) + P2 * AV2(I) + P3 * AV3(I)
          SIGNXX(I) = -P
          SIGNYY(I) = -P
          SIGNZZ(I) = -P

          VDX(I) = ZERO
          VDY(I) = ZERO
          VDZ(I) = ZERO

        ENDDO  !DO I=1,NEL

#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        TFEXT = TFEXT + TFEXTT

        RETURN

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===========================================================================C
C                                                                           C
C     LAW51 : GENERALIZED OUTLET CONDITIONS (IFLG == 6)                     C
C                                                                           C
C===========================================================================C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ELSEIF(IFLG == 6)THEN

        IF(TIME == ZERO) THEN
          DO I=1,NEL
            P0_NRF = UVAR(I,4)
            !
            KK     = N0PHAS + 0*NVPHAS
            RHO1   = UVAR(I,KK+20)
            E01    = UVAR(I,KK+21)
            SSP1   = UVAR(I,KK+22)
            !
            KK     = N0PHAS + 1*NVPHAS
            RHO2   = UVAR(I,KK+20)
            E02    = UVAR(I,KK+21)
            SSP2   = UVAR(I,KK+22)
            !
            KK     = N0PHAS + 2*NVPHAS
            RHO3   = UVAR(I,KK+20)
            E03    = UVAR(I,KK+21)
            SSP3   = UVAR(I,KK+22)
            !
            KK     = N0PHAS + 3*NVPHAS
            RHO4   = UVAR(I,KK+20)
            E04    = UVAR(I,KK+21)
            SSP4   = UVAR(I,KK+22)
            !----------------------!
            SIGNXX(I)     = -P0_NRF
            SIGNYY(I)     = -P0_NRF
            SIGNZZ(I)     = -P0_NRF
            SIGNXY(I)     = ZERO
            SIGNYZ(I)     = ZERO
            SIGNZX(I)     = ZERO
            !----------------------!
            SIGOXX(I)     = -P0_NRF
            SIGOYY(I)     = -P0_NRF
            SIGOZZ(I)     = -P0_NRF
            SIGOXY(I)     = ZERO
            SIGOYZ(I)     = ZERO
            SIGOZX(I)     = ZERO
            !----------------------!
            RHO(I)        = AV1(I)*RHO1+ AV2(I)*RHO2+ AV3(I)*RHO3                     !not relevant to add explosive here
            EINT(I)       = VOLUME(I) * (E01 * AV1(I) + E02 * AV2(I) + E03 * AV3(I))  !not relevant to add explosive here

            KK = N0PHAS
            UVAR(I,1+KK)  = AV1(I)
            UVAR(I,8+KK)  = E01
            UVAR(I,9+KK)  = RHO1
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) * AV1(I)
            UVAR(I,12+KK) = RHO1
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP1
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

            KK = N0PHAS + NVPHAS
            UVAR(I,1+KK)  = AV2(I)
            UVAR(I,8+KK)  = E02
            UVAR(I,9+KK)  = RHO2
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) * AV2(I)
            UVAR(I,12+KK) = RHO2
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP2
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

            KK = N0PHAS + 2*NVPHAS
            UVAR(I,1+KK)  = AV3(I)
            UVAR(I,8+KK)  = E03
            UVAR(I,9+KK)  = RHO3
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) *  AV3(I)
            UVAR(I,12+KK) = RHO3
            !------------------!
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP3
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

            KK = N0PHAS + 3*NVPHAS
            UVAR(I,1+KK)  = AV4(I)
            UVAR(I,8+KK)  = E04
            UVAR(I,9+KK)  = RHO4
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = VOLUME(I) *  AV4(I)
            UVAR(I,12+KK) = RHO4
            !------------------!
            UVAR(I,13+KK) = ZERO
            UVAR(I,14+KK) = SSP4
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = ZERO
            UVAR(I,17+KK) = ZERO
            UVAR(I,18+KK) = P0_NRF
            UVAR(I,19+KK) = ZERO

          ENDDO
        ENDIF

        IF(TIMESTEP > EM20)THEN
          BETA   = TIMESTEP/MAX(UPARAM(71),TIMESTEP)
          IF(UPARAM(71)>=EP20)BETA=ZERO  ! default : Tcalpha=EP20 => BETA = 0
          ALPHA  = TIMESTEP/UPARAM(70)
        ELSE
          BETA   = ONE
          ALPHA  = ONE
        ENDIF  
       
        !RHOC2(1)  = MAX(RHO1*SSP1*SSP1,EM20)
        !RHOC2(2)  = MAX(RHO2*SSP2*SSP2,EM20)
        !RHOC2(3)  = MAX(RHO3*SSP3*SSP3,EM20)
        !RHOC2(4)  = MAX(RHO4*SSP4*SSP4,EM20)

        STIFN(1:NEL) = ZERO

        !     Recherche des quantits voisines
        !     et calcul de la vitesse normale  la face.
        IF(N2D==0)THEN
          CALL M51VOIS3(PM   ,  IPARG  ,IX      ,ALE_CONNECT   ,ELBUF_TAB ,V    ,
     2                  X    ,  VEL_N  ,W       ,VEL     ,VD2  ,
     3                  MAT  ,  RHOV   ,PV      ,VDX     ,VDY       ,VDZ  ,
     4                  EIV  ,  TV     ,BUFVOIS ,IFLG    ,AVV       ,RHO0V,
     5                  IPM    ,BUFMAT  ,UVAR    ,NEL       ,NUVAR,
     6                  NV46 ,  SSPv   ,EPSPv   ,P0_NRFv)
        ELSE
          CALL M51VOIS2(PM   ,  IPARG  ,IX      ,ALE_CONNECT   ,ELBUF_TAB ,V    ,
     2                  X    ,  VEL_N  ,W       ,VEL     ,VD2  ,
     3                  MAT  ,  RHOV   ,PV      ,VDX     ,VDY       ,VDZ  ,
     4                  EIV  ,  TV     ,BUFVOIS ,IFLG    ,AVV       ,RHO0V,
     5                  IPM    ,BUFMAT  ,UVAR    ,NEL       ,NUVAR,
     6                  NV46 ,  SSPv   ,EPSPv   ,P0_NRFv)
        ENDIF

        IF(TIME == ZERO) THEN
         DO I=1,NEL
           VEL_O(I)=VEL_N(I)
           P0_NRF = UVAR(I,4)
           KK           = N0PHAS
           UVAR(I,1+KK) = AVV(1,I)
           AV1(I)       = AVV(1,I)
           KK           = N0PHAS + NVPHAS
           UVAR(I,1+KK) = AVV(2,I)
           AV2(I)       = AVV(2,I)
           KK           = N0PHAS + 2*NVPHAS
           UVAR(I,1+KK) = AVV(3,I)
           AV3(I)       = AVV(3,I)
           KK           = N0PHAS + 3*NVPHAS
           UVAR(I,1+KK) = AVV(4,I)
           AV4(I)       = AVV(4,I)
           SIGOXX(I)    = -P0_NRF
           SIGOYY(I)    = -P0_NRF
           SIGOZZ(I)    = -P0_NRF
           SIGOXY(I)    = ZERO
           SIGOYZ(I)    = ZERO
           SIGOZX(I)    = ZERO
         ENDDO
        ENDIF
        DO I=1,NEL
          P0_NRF = UVAR(I,4)
          !
          KK     = N0PHAS + 0*NVPHAS
          RHO1   = UVAR(I,KK+20)
          E01    = UVAR(I,KK+21)
          SSP1   = UVAR(I,KK+22)
          !
          KK     = N0PHAS + 1*NVPHAS
          RHO2   = UVAR(I,KK+20)
          E02    = UVAR(I,KK+21)
          SSP2   = UVAR(I,KK+22)
          !
          KK     = N0PHAS + 2*NVPHAS
          RHO3   = UVAR(I,KK+20)
          E03    = UVAR(I,KK+21)
          SSP3   = UVAR(I,KK+22)
          !
          KK     = N0PHAS + 3*NVPHAS
          RHO4   = UVAR(I,KK+20)
          E04    = UVAR(I,KK+21)
          SSP4   = UVAR(I,KK+22)

          IF( SUM(AVV(1:4,I)) == ZERO) THEN !corner element
            RHO(I)     = EM20
            SOUNDSP(I) = EM20
            EINT(I)    = ZERO
            P          = ZERO
            EPSPv(0,I) = ZERO
            EEE        = ZERO
          ELSE
            RHOC2(1)  = MAX(EM20,RHOV(1,I)*SSPV(1,I)*SSPV(1,I))
            RHOC2(2)  = MAX(EM20,RHOV(2,I)*SSPV(2,I)*SSPV(2,I))
            RHOC2(3)  = MAX(EM20,RHOV(3,I)*SSPV(3,I)*SSPV(3,I))
            RHOC2(4)  = MAX(EM20,RHOV(4,I)*SSPV(4,I)*SSPV(4,I))
            RHOC20    = ZERO
            IF(AVV(1,I)>EM06)RHOC20=RHOC20 + AVV(1,I)/RHOC2(1)
            IF(AVV(2,I)>EM06)RHOC20=RHOC20 + AVV(2,I)/RHOC2(2)
            IF(AVV(3,I)>EM06)RHOC20=RHOC20 + AVV(3,I)/RHOC2(3)
            IF(AVV(4,I)>EM06)RHOC20=RHOC20 + AVV(4,I)/RHOC2(4)
            RHOC20=ONE/RHOC20 !average stiffness
            RHO(I) = AVV(1,I)*RHOV(1,I)+ AVV(2,I)*RHOV(2,I)+ AVV(3,I)*RHOV(3,I) + AVV(4,I)*RHOV(4,I)  
            !RHOC2(1)  = MAX(EM20,RHOV(1,I)*SSPV(1,I)*SSPV(1,I))
            !RHOC2(2)  = MAX(EM20,RHOV(2,I)*SSPV(2,I)*SSPV(2,I))
            !RHOC2(3)  = MAX(EM20,RHOV(3,I)*SSPV(3,I)*SSPV(3,I))
            !RHOC2(4)  = MAX(EM20,RHOV(4,I)*SSPV(4,I)*SSPV(4,I)) 
            !RHOC2_    = ONE/(AVV(1,I)/RHOC2(1)+AVV(2,I)/RHOC2(2)+AVV(3,I)/RHOC2(3)  + AVV(4,I)/RHOC2(4))
            !SOUNDSP(I) = RHOC2_/RHO(I)                                                                                     
            SOUNDSP(I)  = SSPV(0,I)
            MACH        = VEL_N(I) / SOUNDSP(I)
            ISUPERSONIC = 0
            IF(MACH>=ONE .AND. VEL_N(I)>ZERO)THEN
             !vitesse sortante supersonique : etat = etat voisin
              ISUPERSONIC = 1
              P           = PV(0,I)
              EINT(I)     = EIV(0,I)
              RHO(I)      = RHOV(0,I)
            ELSE
              !IF(VEL_N(I)<=ZERO)P=P0_NRF-HALF*RHO(I)*VEL_N(I)**2 !Bernoulli
              ROC   =SQRT(RHO(I)*RHOC20)
              POLD  = -THIRD*(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))
              DP0   = (P0_NRF-P0_NRFv(I)) ! assuming rho0*g(t)*(z-zvois) = cst ; otherwise complicated: get g(t) and compute z-zvois
              P     = ONE/(ONE+ALPHA)*(POLD+ROC*(VEL_N(I)-VEL_O(I)))+ALPHA*(PV(0,I)+DP0)/(ALPHA + ONE)
            ENDIF
            
            IF(VEL_N(I)<ZERO)THEN     !flux rentrant relaxation du pourcentage volumique  
               KK = N0PHAS 
               AVV(1,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV1(I)                                                                        
               KK = N0PHAS + NVPHAS
               AVV(2,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV2(I)                                                                        
               KK = N0PHAS + 2*NVPHAS
               AVV(3,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV3(I)                                                                        
               KK = N0PHAS + 3*NVPHAS
               AVV(4,I)  = (ONE-BETA)*UVAR(I,1+KK) + BETA* AV4(I)
            ELSE  
                        
            ENDIF   
          ENDIF 
          EEE       = EIV(0,I) * VOLUME(I)
          RHO(I)    = RHOV(0,I)
          SIGNXX(I) = -P
          SIGNYY(I) = SIGNXX(I)
          SIGNZZ(I) = SIGNXX(I)
          SIGNXY(I) = ZERO
          SIGNYZ(I) = ZERO
          SIGNZX(I) = ZERO
          VEL_O(I)  = VEL_N(I)
          SIGOXX(I) = SIGNXX(I)
          SIGOYY(I) = SIGNYY(I)
          SIGOZZ(I) = SIGNZZ(I)
          SIGOXY(I) = SIGNXY(I)
          SIGOYZ(I) = SIGNYZ(I)
          SIGOZX(I) = SIGNZX(I)

          IF(BUFLY%L_PLA>0)THEN
            LBUF%PLA(I) = EPSPv(0,I)
            UVAR(I,1)  =  EPSPv(0,I)
          ENDIF
          UVAR(I,2)  =  ZERO
          UVAR(I,3)  =  ZERO

C         |====================!
C         |     material 1     !
C         |====================!
          KK = N0PHAS
          UVAR(I,1+KK)  = AVV(1,I)
          UVAR(I,8+KK)  = EIV(1,I)
          UVAR(I,9+KK)  = RHOV(1,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(1,I)
          UVAR(I,12+KK) = RHOV(1,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(1,I)
          UVAR(I,15+KK) = EPSPv(1,I)
          UVAR(I,16+KK) = TV(1,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 2     !
C         |====================!
          KK = N0PHAS + NVPHAS
          UVAR(I,1+KK)  = AVV(2,I)
          UVAR(I,8+KK)  = EIV(2,I)
          UVAR(I,9+KK)  = RHOV(2,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(2,I)
          UVAR(I,12+KK) = RHOV(2,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(2,I)
          UVAR(I,15+KK) = EPSPv(2,I)
          UVAR(I,16+KK) = TV(2,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 3     !
C         |====================!
          KK = N0PHAS + 2*NVPHAS
          UVAR(I,1+KK)  = AVV(3,I)
          UVAR(I,8+KK)  = EIV(3,I)
          UVAR(I,9+KK)  = RHOV(3,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(3,I)
          UVAR(I,12+KK) = RHOV(3,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(3,I)
          UVAR(I,15+KK) = EPSPv(3,I)
          UVAR(I,16+KK) = TV(3,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

C         |====================!
C         |     material 4     !
C         |====================!
          KK = N0PHAS + 3*NVPHAS
          UVAR(I,1+KK)  = AVV(4,I)
          UVAR(I,8+KK)  = EIV(4,I)
          UVAR(I,9+KK)  = RHOV(4,I)
          UVAR(I,10+KK) = ZERO
          UVAR(I,11+KK) = VOLUME(I) * AVV(4,I)
          UVAR(I,12+KK) = RHOV(4,I)
          !------------------!
          UVAR(I,13+KK) = ZERO
          UVAR(I,14+KK) = SSPV(4,I)
          UVAR(I,15+KK) = ZERO          !no plasticity with detonation products
          UVAR(I,16+KK) = TV(4,I)
          UVAR(I,17+KK) = ZERO
          UVAR(I,18+KK) = P0_NRF
          UVAR(I,19+KK) = ZERO

C         |==================================!

          TFEXTT     = ZERO !TFEXTT + EEE - EINT(I)
          EINT(I)    = EEE
          VISCMAX(I) = ZERO
        ENDDO
C
#if defined(_OPENMP)
!$OMP ATOMIC
#endif
        TFEXT = TFEXT + TFEXTT
C

C
        RETURN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ENDIF ! IFLG


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C===========================================================================C
C     LAW51 : MULTI MATERIAL LAW                                            C
C     (IFLG == 0.or.IFLG == 1)                                              C
C     (IEXP == 0.or.IEXP == 1)                                              C
C===========================================================================C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (TIME  ==  ZERO) THEN
         DO I = 1, NEL
            KK = N0PHAS + 0 * NVPHAS
            V1OLD = AV1(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V1OLD
            UVAR(I, 9 + KK) = V1OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V1OLD * UVAR(I, 21 + KK)

            KK = N0PHAS + 1 * NVPHAS
            V2OLD = AV2(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V2OLD
            UVAR(I, 9 + KK) = V2OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V2OLD * UVAR(I, 21 + KK)

            KK = N0PHAS + 2 * NVPHAS
            V3OLD = AV3(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V3OLD
            UVAR(I, 9 + KK) = V3OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V3OLD * UVAR(I, 21 + KK)

            KK = N0PHAS + 3 * NVPHAS
            V4OLD = AV4(I) * VOLUME(I)
            UVAR(I, 11 + KK) = V4OLD
            UVAR(I, 9 + KK) = V4OLD * UVAR(I, 20 + KK)
            UVAR(I, 8 + KK) = V4OLD * UVAR(I, 21 + KK)
         ENDDO
      ENDIF

C=======================================================================
C     TEMPERATURES
C     T > 0
C=======================================================================
      DO I=1,NEL
           !---submaterial_1---!
           KK = N0PHAS + 0*NVPHAS
           EINT1   = UVAR(I,8 +KK)
           V1OLD   = UVAR(I,11+KK)
           RHO1OLD = UVAR(I,12+KK)
           ECOLD = -T10 * SPH1
           MU1 = (RHO1OLD/RHO10 - ONE)
           H1 = SPH1*V1OLD*(ONE+MU1)
           IF(MU1 > ZERO) ECOLD = ECOLD * (ONE+C51*MU1*(ONE-MU1)) + HALF*C21*MU1*MU1
           ECOLD1 = ECOLD*V1OLD*(ONE+MU1)
           TEMP1 = (EINT1 - ECOLD1)/MAX(EM20,H1)
           UVAR(I,16+KK) = TEMP1
           P1OLD(I) = UVAR(I,18+KK)

           !---submaterial_2---!
           KK = N0PHAS + 1*NVPHAS
           EINT2   = UVAR(I,8 +KK)
           V2OLD   = UVAR(I,11+KK)
           RHO2OLD = UVAR(I,12+KK)
           ECOLD = -T20 * SPH2
           MU2 = (RHO2OLD/RHO20 - ONE)
           H2 = SPH2*V2OLD*(ONE+MU2)
           IF(MU2 > ZERO) ECOLD = ECOLD * (ONE+C52*MU2*(ONE-MU2)) + HALF*C22*MU2*MU2
           ECOLD2 = ECOLD*V2OLD*(ONE+MU2)
           TEMP2 = (EINT2 - ECOLD2)/MAX(EM20,H2)
           UVAR(I,16+KK) = TEMP2
           P2OLD(I) = UVAR(I,18+KK)

           !---submaterial_3---!
           KK = N0PHAS + 2*NVPHAS
           EINT3   = UVAR(I,8 +KK)
           V3OLD   = UVAR(I,11+KK)
           RHO3OLD = UVAR(I,12+KK)
           ECOLD = -T30 * SPH3
           MU3 = (RHO3OLD/RHO30 - ONE)
           H3 = SPH3*V3OLD*(ONE+MU3)
           IF(MU3 > ZERO) ECOLD = ECOLD * (ONE+C53*MU3*(ONE-MU3)) + HALF*C23*MU3*MU3
           ECOLD3 = ECOLD*V3OLD*(ONE+MU3)
           TEMP3 = (EINT3 - ECOLD3)/MAX(EM20,H3)
           UVAR(I,16+KK) = TEMP3
           P3OLD(I) = UVAR(I,18+KK)

           !---submaterial_4---!
           IF(IEXP == 1)THEN
             KK = N0PHAS + 3*NVPHAS
             EINT4   = UVAR(I,8 +KK)
             V4OLD   = UVAR(I,11+KK)
             RHO4OLD = UVAR(I,12+KK)
             ECOLD = -T40 * SPH4
             MU4 = (RHO4OLD/RHO40 - ONE)
             H4 = SPH4*V4OLD*(ONE+MU4)
             ECOLD4 = ECOLD*V4OLD*(ONE+MU4)
             TEMP4 = (EINT4 - ECOLD4)/MAX(EM20,H4)
             P4OLD(I) = UVAR(I,18+KK)
           ELSE
             V4OLD    = ZERO
             EINT4    = ZERO
             RHO4OLD  = EM20
             MU4      = ZERO
             TEMP4    = ZERO
             P4OLD(I) = ZERO
             ECOLD4   = ZERO
             H4       = ZERO
           ENDIF
      ENDDO

C=======================================================================
C     PLASTICITY
C     T > 0
C=======================================================================
      ! If at least, one material has plasticity data
      IF(IPLA > ZERO)THEN
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
          !
          DE(I) = THIRD*(DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))
          DEPS(1,I) = DEPSXX(I) !- DE
          DEPS(2,I) = DEPSYY(I) !- DE
          DEPS(3,I) = DEPSZZ(I) !- DE
          DEPS(4,I) = DEPSXY(I)
          DEPS(5,I) = DEPSYZ(I)
          DEPS(6,I) = DEPSZX(I)
          EPD(I)=OFF(I)*MAX(   ABS(EPSPXX(I)),   ABS(EPSPYY(I)),   ABS(EPSPZZ(I)),
     .         HALF*ABS(EPSPXY(I)),HALF*ABS(EPSPYZ(I)),HALF*ABS(EPSPZX(I))) 
        ENDDO
        IF(BUFLY%L_EPSD > 0)THEN
          DO I=1,NEL
           LBUF%EPSD(I) = EPD(I)
          ENDDO
        ENDIF
      ENDIF

      !===================================
      !       material 1 : PLASTICITY
      !===================================
      IF(IPLA1 /= ZERO)THEN
        KK =  N0PHAS
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
           DO J=1,6
             SIGD(J,I)  = UVAR(I,KK + J + 1)
           ENDDO

           VFRAC(I)     = UVAR(I,1+KK)
           PP(I)        = UVAR(I,18+KK)
           PLAS1(I)     = UVAR(I,15+KK)
           TEMP(I)      = UVAR(I,16+KK)
           VOL(I)       = UVAR(I,11+KK)
           EINT0(I)     = UVAR(I,8 +KK)
           EPSEQ1(I)    = UVAR(I,19+KK)   ! Dprag

        ENDDO
        SELECT CASE (IPLA1)
           CASE (1)
             CALL JCOOK51 (NEL  ,SIGD     ,PLAS1       ,TEMP   ,VOL  ,
     .                     DEPS ,EPD      ,UPARAM(101) ,VOLUME ,EINT0,
     .                     DE   ,TIMESTEP ,OFF         ,IX     ,NFT  ,
     .                     VFRAC)
           CASE (2)
              CALL DPRAG51
     .            (NEL    ,SIGD   ,TEMP       ,VOL     ,EPSEQ1  ,
     .             DEPS   ,EPD    ,UPARAM(101),VOLUME  ,EINT0   ,PLAS1 ,
     .             UVAR   ,NUVAR  ,KK         ,RHO10   ,
     .             C01    ,C11    ,C21        ,C31     ,PM1     ,PP    ,
     .             SIGOXX ,SIGOYY ,SIGOZZ     ,SIGOXY  ,SIGOYZ  ,SIGOZX,
     .             SIGNXX ,SIGNYY ,SIGNZZ     ,SIGNXY  ,SIGNYZ  ,SIGNZX,
     .             OFF    ,PEXT   ,TIMESTEP   ,DE)
        END SELECT
        DO I=1,NEL

          !inter22
          IF(VOLUME(I) == ZERO)CYCLE

             UVAR(I,KK + 1 + 1)= SIGD(1,I)
             UVAR(I,KK + 2 + 1)= SIGD(2,I)
             UVAR(I,KK + 3 + 1)= SIGD(3,I)
             UVAR(I,KK + 4 + 1)= SIGD(4,I)
             UVAR(I,KK + 5 + 1)= SIGD(5,I)
             UVAR(I,KK + 6 + 1)= SIGD(6,I)

             UVAR(I,15+KK) = PLAS1(I)
             UVAR(I,16+KK) = TEMP(I)
             UVAR(I,11+KK) = VOL(I)
             UVAR(I,8 +KK) = EINT0(I)
             UVAR(I,19+KK) = EPSEQ1(I)   ! Dprag

             !total stress is set before  returning from sigeps51.F

        ENDDO
      ENDIF

      !===================================
      !        material 2 : PLASTICITY
      !===================================
      IF(IPLA2 /= ZERO)THEN
        KK = N0PHAS + NVPHAS
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
          DO J=1,6
            SIGD(J,I)   = UVAR(I,KK + J + 1)
          ENDDO
           VFRAC(I)     = UVAR(I,1+KK)
           PP(I)        = UVAR(I,18+KK)
           PLAS2(I)     = UVAR(I,15+KK)
           TEMP(I)      = UVAR(I,16+KK)
           VOL(I)       = UVAR(I,11+KK)
           EINT0(I)     = UVAR(I,8 +KK)
           EPSEQ2(I)    = UVAR(I,19+KK)   ! Dprag
        ENDDO
        SELECT CASE (IPLA2)
           CASE (1)
             CALL JCOOK51 (NEL  ,SIGD     ,PLAS2       ,TEMP   ,VOL  ,
     .                     DEPS ,EPD      ,UPARAM(151) ,VOLUME ,EINT0,
     .                     DE   ,TIMESTEP ,OFF         ,IX     ,NFT  ,
     .                     VFRAC)
           CASE (2)
             CALL DPRAG51
     .            (NEL    ,SIGD   ,TEMP       ,VOL     ,EPSEQ2  ,
     .             DEPS   ,EPD    ,UPARAM(151),VOLUME  ,EINT0   ,PLAS2 ,
     .             UVAR   ,NUVAR  ,KK         ,RHO20   ,
     .             C02    ,C12    ,C22        ,C32     ,PM2     ,PP    ,
     .             SIGOXX ,SIGOYY ,SIGOZZ     ,SIGOXY  ,SIGOYZ  ,SIGOZX,
     .             SIGNXX ,SIGNYY ,SIGNZZ     ,SIGNXY  ,SIGNYZ  ,SIGNZX,
     .             OFF    ,PEXT   ,TIMESTEP   ,DE)
        END SELECT
        DO I=1,NEL
         !inter22
         IF(VOLUME(I) == ZERO)CYCLE

           UVAR(I,KK + 1 + 1) = SIGD(1,I)
           UVAR(I,KK + 2 + 1) = SIGD(2,I)
           UVAR(I,KK + 3 + 1) = SIGD(3,I)
           UVAR(I,KK + 4 + 1) = SIGD(4,I)
           UVAR(I,KK + 5 + 1) = SIGD(5,I)
           UVAR(I,KK + 6 + 1) = SIGD(6,I)

           UVAR(I,15+KK)      = PLAS2(I)
           UVAR(I,16+KK)      = TEMP(I)
           UVAR(I,11+KK)      = VOL(I)
           UVAR(I,8+KK)       = EINT0(I)
           UVAR(I,19+KK)      = EPSEQ2(I)   ! Dprag

        ENDDO
      ENDIF

      !===================================
      !        material 3 : PLASTICITY
      !===================================
      IF(IPLA3 /= ZERO)THEN
        KK = N0PHAS + 2*NVPHAS
        DO I=1,NEL
          !inter22
          IF(VOLUME(I) == ZERO)CYCLE
           DO J=1,6
             SIGD(J,I)  = UVAR(I,KK + J + 1)
           ENDDO
           VFRAC(I)     = UVAR(I,1+KK)
           PP(I)        = UVAR(I,18+KK)
           PLAS3(I)     = UVAR(I,15+KK)
           TEMP(I)      = UVAR(I,16+KK)
           VOL(I)       = UVAR(I,11+KK)
           EINT0(I)     = UVAR(I,8 +KK)
           EPSEQ3(I)    = UVAR(I,19+KK)   ! Dprag
        ENDDO
        SELECT CASE (IPLA3)
           CASE (1)
             CALL JCOOK51 (NEL  ,SIGD    ,PLAS3       ,TEMP   ,VOL  ,
     .                     DEPS ,EPD     ,UPARAM(201) ,VOLUME ,EINT0,
     .                     DE   ,TIMESTEP,OFF         ,IX     ,NFT  ,
     .                     VFRAC)
           CASE (2)
             CALL DPRAG51
     .            (NEL    ,SIGD   ,TEMP       ,VOL     ,EPSEQ3  ,
     .             DEPS   ,EPD    ,UPARAM(201),VOLUME  ,EINT0   ,PLAS3 ,
     .             UVAR   ,NUVAR  ,KK         ,RHO30   ,
     .             C03    ,C13    ,C23        ,C33     ,PM3     ,PP    ,
     .             SIGOXX ,SIGOYY ,SIGOZZ     ,SIGOXY  ,SIGOYZ  ,SIGOZX,
     .             SIGNXX ,SIGNYY ,SIGNZZ     ,SIGNXY  ,SIGNYZ  ,SIGNZX,
     .             OFF    ,PEXT   ,TIMESTEP   ,DE)
        END SELECT
        DO I=1,NEL
         !inter22
         IF(VOLUME(I) == ZERO)CYCLE

            UVAR(I,KK + 1 + 1)= SIGD(1,I)
            UVAR(I,KK + 2 + 1)= SIGD(2,I)
            UVAR(I,KK + 3 + 1)= SIGD(3,I)
            UVAR(I,KK + 4 + 1)= SIGD(4,I)
            UVAR(I,KK + 5 + 1)= SIGD(5,I)
            UVAR(I,KK + 6 + 1)= SIGD(6,I)

            UVAR(I,15+KK) = PLAS3(I)
            UVAR(I,16+KK) = TEMP(I)
            UVAR(I,11+KK) = VOL(I)
            UVAR(I,8+KK)  = EINT0(I)
            UVAR(I,19+KK) = EPSEQ3(I)   ! Dprag
            !total stress is set before  returning from sigeps51.F

        ENDDO
      ENDIF



C=======================================================================
C     THERMODYNAMICAL STATE
C=======================================================================
      DO I=1,NEL

          !inter22
          IF(VOLUME(I) == ZERO)CYCLE

        !===================================
        ! material 1 : THERMODYNAMICAL STATE
        !===================================
        KK = N0PHAS
        EINT1   = UVAR(I,8+KK)    ! energie IN rho e OUT
        MAS1    = UVAR(I,9+KK)    ! masse IN rho OUT
        Q1OLD   = UVAR(I,10+KK)
        V1OLD   = UVAR(I,11+KK)
        RHO1OLD = UVAR(I,12+KK)   ! rho_old IN rho OUT
        DDVOL1  = UVAR(I,13+KK)
        SSP1    = UVAR(I,14+KK)   ! sound speed
        PLAS1(I)= UVAR(I,15+KK)   ! Eps plastique
        TEMP1   = UVAR(I,16+KK)   ! temperature
        EDIF1   = UVAR(I,17+KK)   ! chaleur diffusee
        P1OLD(I)= UVAR(I,18+KK)   ! Pressure
!        EPSEQ1(I)= UVAR(I,19+KK)   ! Dprag
        ECOLD = -T10 * SPH1
        MU1 = (RHO1OLD/RHO10 - ONE)
        H1 = SPH1*V1OLD*(ONE+MU1)
          IF(MU1 > ZERO) ECOLD = ECOLD *
     1       (ONE+C51*MU1*(ONE-MU1)) + HALF*C21*MU1*MU1
        ECOLD1 = ECOLD*V1OLD*(ONE+MU1)
        TEMP1 = (EINT1 - ECOLD1)/MAX(EM20,H1)


        !===================================
        ! material 2 : THERMODYNAMICAL STATE
        !===================================
        KK = N0PHAS + NVPHAS
        EINT2   = UVAR(I,8+KK)    ! energie IN rho e OUT
        MAS2    = UVAR(I,9+KK)    ! masse IN rho OUT
        Q2OLD   = UVAR(I,10+KK)
        V2OLD   = UVAR(I,11+KK)
        RHO2OLD = UVAR(I,12+KK)   ! rho_old IN rho OUT
        DDVOL2  = UVAR(I,13+KK)
        SSP2    = UVAR(I,14+KK)   ! sound speed
        PLAS2(I)= UVAR(I,15+KK)   ! Eps plastique
        TEMP2   = UVAR(I,16+KK)   ! temperature
        EDIF2   = UVAR(I,17+KK)   ! chaleur diffusee
        P2OLD(I)= UVAR(I,18+KK)   ! Pressure
!        EPSEQ2(I)= UVAR(I,19+KK)   ! Dprag
        ECOLD = -T20 * SPH2
        MU2 = (RHO2OLD/RHO20 - ONE)
        H2 = SPH2*V2OLD*(ONE+MU2)
           IF(MU2 > ZERO) ECOLD = ECOLD *
     1        (ONE+C52*MU2*(ONE-MU2)) + HALF*C22*MU2*MU2
        ECOLD2 = ECOLD*V2OLD*(ONE+MU2)
        TEMP2 = (EINT2 - ECOLD2)/MAX(H2,EM20)


        !===================================
        ! material 3 : THERMODYNAMICAL STATE
        !===================================
        KK = N0PHAS + 2*NVPHAS
        EINT3   = UVAR(I,8+KK)    ! energie IN rho e OUT
        MAS3    = UVAR(I,9+KK)    ! masse IN rho OUT
        Q3OLD   = UVAR(I,10+KK)
        V3OLD   = UVAR(I,11+KK)
        RHO3OLD = UVAR(I,12+KK)   ! rho_old IN rho OUT
        DDVOL3  = UVAR(I,13+KK)
        SSP3    = UVAR(I,14+KK)   ! sound speed
        PLAS3(I)= UVAR(I,15+KK)   ! Eps plastique
        TEMP3   = UVAR(I,16+KK)   ! temperature
        EDIF3   = UVAR(I,17+KK)   ! chaleur diffusee
        P3OLD(I)= UVAR(I,18+KK)   ! Pressure
!        EPSEQ3(I)= UVAR(I,19+KK)   ! Dprag
        ECOLD   = -T30 * SPH3
        MU3     = (RHO3OLD/RHO30 - ONE)
        H3      = SPH3*V3OLD*(ONE+MU3)
          IF(MU3 > ZERO) ECOLD = ECOLD *
     1       (ONE+C53*MU3*(ONE-MU3)) + HALF*C23*MU3*MU3
        ECOLD3  = ECOLD*V3OLD*(ONE+MU3)
        TEMP3   = (EINT3 - ECOLD3)/MAX(H3,EM20)


        !===================================
        ! H.Explosive : THERMODYNAMICAL STATE
        !===================================
        IF(IEXP == 0)THEN
          EINT4   = ZERO
          MAS4    = ZERO
          Q4OLD   = ZERO
          V4OLD   = ZERO
          RHO4OLD = ZERO
          DDVOL4  = ZERO
          SSP4    = ZERO
          TEMP4   = ZERO
          EDIF4   = ZERO
          MU4     = ZERO
          H4      = ZERO
          ECOLD4  = ZERO
          TEMP4   = ZERO
          P4OLD(I)= ZERO
        ELSE
          KK = N0PHAS + 3*NVPHAS
          EINT4   = UVAR(I,8+KK)
          MAS4    = UVAR(I,9+KK)
          Q4OLD   = UVAR(I,10+KK)
          V4OLD   = UVAR(I,11+KK)
          RHO4OLD = UVAR(I,12+KK)
          DDVOL4  = UVAR(I,13+KK)
          SSP4    = UVAR(I,14+KK)   ! sound speed
          TEMP4   = UVAR(I,16+KK)
          TBURN =  UVAR(I, 15+KK)
          EDIF4   = UVAR(I,17+KK)   ! chaleur diffusee
          P4OLD(I)= UVAR(I,KK + 18) ! Pressure
          ECOLD = -T40 * SPH4
          MU4 = (RHO4OLD/RHO40 - ONE)
          H4 = SPH4*V4OLD*(ONE+MU4)
          ECOLD4 = ECOLD*V4OLD*(ONE+MU4)
          TEMP4 = (EINT4 - ECOLD4)/MAX(H4,EM20)
          BFRAC(I) = GBUF%BFRAC(I)

        ENDIF

        !===================================
        ! GLOBAL PRESSURE IN ELEMENT
        !===================================
        POLD = P1OLD(I)*V1OLD + P2OLD(I)*V2OLD+ P3OLD(I)*V3OLD + P4OLD(I)*V4OLD
        POLD = POLD / (V1OLD + V2OLD + V3OLD + V4OLD)

!         if(int22>0)then
!           if (ibug22_dvol==-1 .or . ibug22_dvol==ix(11,I+nft))then
!                   write (*,FMT='(A,I10,5(A,F24.16))' )
!     .             "  *inter22, cell_id=", ix(11,I+nft), "   SUMdv =",TIMESTEP*(DDVOL1+DDVOL2+DDVOL3+DDVOL4),
!     .             "   DDVOL1=",TIMESTEP*DDVOL1,
!     .             "   DDVOL2=",TIMESTEP*DDVOL2,
!     .             "   DDVOL3=",TIMESTEP*DDVOL3,
!     .             "   DDVOL4=",TIMESTEP*DDVOL4
!           endif
!         endif

Cmtheta.f
c              SPH     =PM(69,MAT(I))
c              EINT0   =PM(23,MAT(I))
c              THETA(I)=PM(79,MAT(I))
c              PLIM    =PC(I)-C0(I)-PSH(I)
c              IF(C1(I) > ABS(PLIM/1.E36))THEN
c              DMU = MIN(AMU(I)-PLIM/C1(I),ZERO)
c              ELSE
c              DMU =0.
c              ENDIF
c              ECOLD   =(C0(I)+PSH(I)+0.5*C1(I)*AMU(I))*AMU(I)
c                   +0.5*DMU*(C0(I)+PSH(I)+C1(I)*AMU(I)-PC(I))
c                   +EINT0
c              THETA(I)=THETA(I) + (EINT(I)*DF(I)-ECOLD)/SPH
Cmtemp.fc               ECOLD=-300.*SPH(I)
c              IF(AMU(I) > 0.) ECOLD=ECOLD*(1.+C4(I)*AMU(I)*(1-AMU(I)))
c             1                         +0.5*C1(I)*AMU2(I)
c              THETA(I)=(EINT(I)-ECOLD)/SPH(I)
C
C=======================================================================
c  THERMAL CONDUCTION BETWEEN THE MATERIALS
c     --> Energy is here corrected with TdH
c                    (dE = dW+dQ = -PdV+TdH)
C=======================================================================
        IF(JTHE == 1)THEN
         IF(V1OLD+V2OLD+V3OLD > EM03)THEN
         ! chaleur in
          T = ( EINT1 - ECOLD1 + EINT2 - ECOLD2 + EINT3 - ECOLD3 + UVAR(I,2)) / (H1 + H2 + H3)
          !--------material_1---------!
          AAA   = (T-TEMP1)*H1        ! 'TdH'
          EDIF1   = EDIF1 + AAA       ! -> Heat 'Q'
          EINT1   = EINT1 + AAA       ! -> E(k)=E(k)+ TdH (non adiabatique => energy correction)
          !--------material_2---------!
          AAA   = (T-TEMP2)*H2
          EDIF2   = EDIF2 + AAA
          EINT2   = EINT2 + AAA
          !--------material_3---------!
          AAA   = (T-TEMP3)*H3
          EDIF3   = EDIF3 + AAA
          EINT3   = EINT3 + AAA
          !--------output-------------!
         !================================================!
         !Internal Energy is here bounded (Inferior limit)!
         !for stability reasons                           !
         !================================================!
          EINT1=max(EINT1,MAS1/MAX(RHO10,EM20)*E1_INF)
          EINT2=max(EINT2,MAS2/MAX(RHO20,EM20)*E2_INF)
          EINT3=max(EINT3,MAS3/MAX(RHO30,EM20)*E3_INF)
          EINT4=max(EINT4,MAS4/MAX(RHO40,EM20)*E4_INF)
         END IF
        ECOLD4 = ZERO
        END IF
        IF (TIME  ==  ZERO) THEN
           EINT(I)= EINT1 + EINT2 + EINT3 + EINT4
        ENDIF
C=======================================================================
c  NUMERICAL VISCOSITY
C=======================================================================
        QOLD = Q1OLD*V1OLD + Q2OLD*V2OLD + Q3OLD*V3OLD + Q4OLD*V4OLD
        QOLD = QOLD /(V1OLD + V2OLD + V3OLD + V4OLD)

        DD   = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)

        dbVOLD(1) = V1OLD
        dbVOLD(2) = V2OLD
        dbVOLD(3) = V3OLD
        dbVOLD(4) = V4OLD

        !inter22 : Polyhedra strain rate trace estimation
!        IF(INT22>0)THEN
!          IF(TAG22(I) > ZERO)THEN
!            VOLD = BRICK_LIST(1,NINT(TAG22(I)))%Vold_SCell
!            VOLD = SUM(dbVOLD(:))
!            DD   = VOLUME(I)**THIRD
!            DD   = (DD - VOLD**THIRD) / DD
!            DD   = -THREE*DD*UNDT
!          ENDIF
!        ENDIF

        IF(INT22 > 0)DD=ZERO

        QA   = GEO(14,PID(I)) !1.225D00
        QB   = GEO(15,PID(I)) !0.06D00
        XL   = VOLUME(I)**THIRD
        QAL  = (QA*XL)**2
        QBL  = QB*XL



        VQ0  = RHO(I)*QAL*MAX(ZERO,DD)
        VQ0  = VQ0 + (RHO1OLD*SSP1*V1OLD+RHO2OLD*SSP2*V2OLD+RHO3OLD*SSP3*V3OLD+RHO4OLD*SSP4*V4OLD)*QBL/(V1OLD+V2OLD+V3OLD+V4OLD)
        VQ1  = VQ0
        VQ2  = VQ0
        VQ3  = VQ0
        VQ4  = VQ0

        Q0   = VQ0*MAX(ZERO,DD)
        Q1   = Q0
        Q2   = Q0
        Q3   = Q0
        Q4   = Q0

        XL1  = V1OLD**THIRD
        XL2  = V2OLD**THIRD
        XL3  = V3OLD**THIRD
        XL4  = V4OLD**THIRD

        QAL1 = (QA*XL1)**2
        QAL2 = (QA*XL2)**2
        QAL3 = (QA*XL3)**2
        QAL4 = (QA*XL4)**2

        QBL1 = QB*XL1
        QBL2 = QB*XL2
        QBL3 = QB*XL3
        QBL4 = QB*XL4

        IF(DD > ZERO)THEN
          DD2  = DD * DD
          VQ1  = RHO1OLD*(QAL1*DD2 + SSP1*QBL1*DD)
          VQ2  = RHO2OLD*(QAL2*DD2 + SSP2*QBL2*DD)
          VQ3  = RHO3OLD*(QAL3*DD2 + SSP3*QBL3*DD)
          VQ4  = RHO4OLD*(QAL4*DD2 + SSP4*QBL4*DD)
        ELSE
          VQ1  = ZERO
          VQ2  = ZERO
          VQ3  = ZERO
          VQ4  = ZERO
        ENDIF

        Q1   = VQ1
        Q2   = VQ2
        Q3   = VQ3
        Q4   = VQ4

Cts        if     (ix(11,I+NFT) == ibug_id(1))then
Cts          write (*,FMT='(A,5E30.16)')"Q1,Q2,Q3,Q4, Q0 =", Q1,Q2,Q3,Q4,Q0
Cts        endif

        SSP1 = ZERO
        SSP2 = ZERO
        SSP3 = ZERO
        SSP4 = ZERO

C------------------------------------------------------------C
Cts                 INTEGER UNEPHASE                         C
C                   ----------------                         C
C                                                            C
C    ___Description_____________________________________     C
C   Describe the presence/absence of differtent material     C
C   possible values 1,2,3,...,15                             C
C   this integer can be described with 4 bits                C
C                                                            C
C                                                            C
C               mat1   mat2  mat3  mat4                      C
C              +-----+-----+-----+-----+                     C
C              |  0  |  1  |  1  |  0  |                     C
C              +-----+-----+-----+-----+                     C
C                 0     1     2     3                        C
C                2     2     2     2                         C
C                                                            C
C    ___bit values_____________________________________      C
C    mas3 <  1E-8 % => bit3 = 0 => UNEPHASE+= 0*2^2 = 0      C
C    mas3 >= 1E-8 % => bit3 = 1 => UNEPHASE+= 1*2^2 = 4      C
C    ...etc...                                               C
C                                                            C
C    ___result___________________________________________    C
C    UNEPHASE = bit1*2^0 + bit2*2^1 + bit3*2^2 + bit4*2^3    C
C    UNEPHASE = bit1* 1  + bit2* 2  + bit3* 4  + bit4* 8     C
C                                                            C
C------------------------------------------------------------C
C=======================================================================
c   MATERIAL PRESENCE (INTEGER UNEPHASE)
C=======================================================================
        UNEPHASE=0
        MASS = MAS1 + MAS2 + MAS3 + MAS4
        VOLD = V1OLD + V2OLD + V3OLD + V4OLD

        IF (MAS1 / MASS  >  TOL51 .AND. MAS1/RHO10/VOLD > Tol51) THEN
           UNEPHASE=UNEPHASE + 1
           V10 = MAS1 / RHO10
           RHO0E1 = EINT1/V10
           ALPHA1OLD = V1OLD / VOLD
           !   PRINT*, "ALPHA1OLD",ALPHA1OLD
           !ENDIF
           V1 = V1OLD - TIMESTEP*DDVOL1 + ALPHA1OLD * DDVOL(I)
           V1 = MAX(ZERO,V1)
        ELSE
           MAS1  = ZERO
           V1 = ZERO
           V10   = ZERO
           ALPHA1OLD = ZERO
        ENDIF
        IF (MAS2 / MASS  >  TOL51 .AND. MAS2/RHO20/VOLD > Tol51) THEN
           UNEPHASE=UNEPHASE + 2
           V20 = MAS2 / RHO20
           RHO0E2 = EINT2/V20
           ALPHA2OLD = V2OLD / VOLD
           !   PRINT*, "ALPHA2OLD",ALPHA2OLD
           !ENDIF
           V2 = V2OLD - TIMESTEP*DDVOL2 + ALPHA2OLD * DDVOL(I)
           V2 = MAX(ZERO,V2)
        ELSE
           MAS2  = ZERO
           V2 = ZERO
           V20   = ZERO
           ALPHA2OLD = ZERO
        ENDIF

        IF (MAS3 / MASS  >  TOL51 .AND. MAS3/RHO30/VOLD > Tol51) THEN
           UNEPHASE=UNEPHASE + 4
           V30 = MAS3 / RHO30
           RHO0E3 = EINT3/V30
           ALPHA3OLD = V3OLD / VOLD
           !   PRINT*, "ALPHA3OLD",ALPHA3OLD
           !ENDIF
           V3 = V3OLD - TIMESTEP*DDVOL3 + ALPHA3OLD * DDVOL(I)
           V3 = MAX(ZERO,V3)
        ELSE
          MAS3  = ZERO
          V3 = ZERO
          V30   = ZERO
          ALPHA3OLD = ZERO
        ENDIF

        IF (MAS4 / MASS  >  TOL51 .AND. MAS4/RHO40/VOLD > Tol51) THEN
           UNEPHASE=UNEPHASE + 8
           V40 = MAS4 / RHO40
           ALPHA4OLD = V4OLD / VOLD
           !   PRINT*, "ALPHA4OLD",ALPHA4OLD
           !ENDIF
           V4 = V4OLD - TIMESTEP*DDVOL4 + ALPHA4OLD * DDVOL(I)
           V4 = MAX(ZERO,V4)
        ELSE
           MAS4  = ZERO
           V4 = ZERO
           V40   = ZERO
           ALPHA4OLD = ZERO
        ENDIF
        ! SANITY CHECK
        AA = (V1 + V2 + V3 + V4) / VOLUME(I)
        IF(AA > EM06)THEN
         V1 = V1 / AA
         V2 = V2 / AA
         V3 = V3 / AA
         V4 = V4 / AA
        ELSE
         V1 = ZERO
         V2 = ZERO
         V3 = ZERO
         V4 = ZERO
        ENDIF
        !Mass may have changed and then needs to be update.
        MASS = MAS1 + MAS2 + MAS3 + MAS4
        RHO(I) = MASS / VOLUME(I)

        RHOOLD = ALPHA1OLD * RHO1OLD + ALPHA2OLD * RHO2OLD +
     .       ALPHA3OLD * RHO3OLD + ALPHA4OLD * RHO4OLD

        dbVOLD_f(1) = V1OLD
        dbVOLD_f(2) = V2OLD
        dbVOLD_f(3) = V3OLD
        dbVOLD_f(4) = V4OLD

C=======================================================================
C IF ONLY ONE MATERIAL IN ELEMENT
C      MAT1 : (UNEPHASE=1) => Polynomial EOS
C   or MAT2 : (UNEPHASE=2) => Polynomial EOS
C   or MAT3 : (UNEPHASE=4) => Polynomial EOS
C   or MAT4 : (UNEPHASE=8) => JWL EOS
C=======================================================================
        IF(UNEPHASE == 1 .OR. UNEPHASE == 2 .OR. UNEPHASE == 4 .OR. UNEPHASE == 8) THEN

          P1    = ZERO
          P2    = ZERO
          P3    = ZERO
          P4    = ZERO
          Q1    = ZERO
          Q2    = ZERO
          Q3    = ZERO
          Q4    = ZERO
          V1    = ZERO
          V2    = ZERO
          V3    = ZERO
          V4    = ZERO

         !==========================================
         ! The only material is MAT1 (sol, liq, gas)
         !==========================================
          IF(UNEPHASE == 1)THEN
             EINT1 = EINT1  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
             EINT(I) = EINT1
             V1 = VOLUME(I)
             RHO1 = MASS / V1
             CALL POLYUN51 (
     .            C01,C11,C21,C31,C41,C51,GG1,
     .            VOLUME(I),DVOL,V1OLD,V1,
     .            RHO(I),MAS1 ,RHO10,RHO1,DD,MU1,MU1P1,
     .            POLD,PEXT,P1,PM1,P,Q1,QOLD,Q,
     .            RHO0E1,EINT1 ,SOUNDSP(I),VISCMAX(I),XL ,SSP1,
     .            QA,QB, E1_INF,IPLA1,UPARAM(101),PLAS1(I))


         !==========================================
         ! The only material is MAT2 (sol, liq, gas)
         !==========================================
         ELSEIF(UNEPHASE == 2)THEN
            EINT2 = EINT2  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
            EINT(I) = EINT2
            V2 = VOLUME(I)
            RHO2 = MASS / V2
            CALL POLYUN51 (
     .           C02,C12,C22,C32,C42,C52,GG2,
     .           VOLUME(I),DVOL,V2OLD,V2,
     .           RHO(I),MAS2 ,RHO20,RHO2,DD,MU2,MU2P1,
     .           POLD,PEXT,P2,PM2,P,Q2,QOLD,Q,
     .           RHO0E2,EINT2 ,SOUNDSP(I),VISCMAX(I),XL ,SSP2,
     .           QA,QB, E2_INF,IPLA2,UPARAM(151),PLAS2(I))
         !==========================================
         ! The only material is MAT3 (sol, liq, gas)
         !==========================================
         ELSEIF(UNEPHASE == 4)THEN
            EINT3 = EINT3  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
            EINT(I) = EINT3
            V3 = VOLUME(I)
            RHO3 = MASS / V3
            CALL POLYUN51 (
     .           C03,C13,C23,C33,C43,C53,GG3,
     .           VOLUME(I),DVOL,V3OLD,V3,
     .           RHO(I),MAS3 ,RHO30,RHO3,DD,MU3,MU3P1,
     .           POLD,PEXT,P3,PM3,P,Q3,QOLD,Q,
     .           RHO0E3,EINT3 ,SOUNDSP(I),VISCMAX(I),XL ,SSP3,
     .           QA,QB, E3_INF,IPLA3,UPARAM(201),PLAS3(I))
         !===========================================
         ! The only material is MAT4 (High Explosive)
         !===========================================
         ELSEIF(UNEPHASE == 8)THEN
            EINT4 = EINT4  - (PEXT + POLD + QQOLD(I)) * DDVOL(I)
            EINT(I) = EINT4
            V4 = VOLUME(I)
            RHO4 = MASS / V4
C---------------------------------------------------
C     Computation of burnt fraction
C---------------------------------------------------
            CALL JWLUN51 (TIME,XL,TBURN,UPARAM,DD,MU4,MU4P1,
     .           VOLUME(I),DVOL, V4OLD,EINT4,VISCMAX(I),
     .           P4OLD(I),Q4,Q4OLD,PEXT,P4,PM4,
     .           RHO(I),RHO40,MAS4,SOUNDSP(I),
     .           P,Q,RHO4,V4,SSP4,
     .           QA,QB,BFRAC(I))
           ENDIF

C=======================================================================
C MORE THAN ONE MATERIAL IN ELEMENT
C    UNEPHASE /= 1,2,4,8
C    UNEPHASE /= 0
C=======================================================================
        ELSEIF(UNEPHASE /= 0) THEN
C=======================================================================
C=======================================================================
C     New solver
C=======================================================================
C=======================================================================
C---------------------------------------------------
C     Submaterial energies evolve according to
C     dEi / dt + div(Ei u) + alpha_i p_i div(u) = 0
C     with a fraction of the global pseudo-viscosity
C     coefficient
C---------------------------------------------------
C     Pseudo viscosity pondarated by old massic fractions
          Q1OLD = RHO1OLD / RHOOLD * QQOLD(I)
          Q2OLD = RHO2OLD / RHOOLD * QQOLD(I)
          Q3OLD = RHO3OLD / RHOOLD * QQOLD(I)
          Q4OLD = RHO4OLD / RHOOLD * QQOLD(I)

          EINT1 = EINT1 - ALPHA1OLD * (PEXT + P1OLD(I) + Q1OLD) * DDVOL(I)
          EINT2 = EINT2 - ALPHA2OLD * (PEXT + P2OLD(I) + Q2OLD) * DDVOL(I)
          EINT3 = EINT3 - ALPHA3OLD * (PEXT + P3OLD(I) + Q3OLD) * DDVOL(I)
          EINT4 = EINT4 - ALPHA4OLD * (PEXT + P4OLD(I) + Q4OLD) * DDVOL(I)
C---------------------------------------------------
C     Global energy evolution
C     d(E) / dt + div(Eu) +  P div(u) = 0
C     with contribution of pseudo viscosity and
C     pressure at time t^n.
C     However, since EINT1 ... EINT4 have received
C     plasticity contributions, we directly set
C     EINT(I) = EINT1 + EINT2 + EINT3 + EINT4
C---------------------------------------------------
          EINT(I) = EINT1 + EINT2 + EINT3 + EINT4

C---------------------------------------------------
C     COMPUTE MASS DENSITIES FOR EACH FLUID
C---------------------------------------------------
          P1    = ZERO
          P2    = ZERO
          P3    = ZERO
          P4    = ZERO
          DVDP1 = ZERO
          DVDP2 = ZERO
          DVDP3 = ZERO
          DVDP4 = ZERO
          V1I   = V1
          V2I   = V2
          V3I   = V3
          V4I   = V4
          IF (V1  >  ZERO) RHO1 = MAS1 / V1
          IF (V2  >  ZERO) RHO2 = MAS2 / V2
          IF (V3  >  ZERO) RHO3 = MAS3 / V3
          IF (V4  >  ZERO) RHO4 = MAS4 / V4
C---------------------------------------------------
C     COMPUTE PRESSURE FOR EACH SUBMATERIAL
C---------------------------------------------------

C     Gruneisen coefficient : GRUN = 1 / RHO * Dp / De (cst rho)
          GRUN1 = ZERO
          GRUN2 = ZERO
          GRUN3 = ZERO
          GRUN4 = ZERO
C     MAT1
          IF (V1  >  ZERO) THEN
             CALL POLY51 (C01,C11,C21,C31,C41,C51,GG1,
     .            VOLUME(I),V10,V1,V1I,MU1,MU1P1,EINT1,
     .            P1OLD(I),Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .            RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, E1_INF,
     .            IPLA1,UPARAM(101),PLAS1(I), 0, GRUN1)
          ENDIF
C     MAT2
          IF (V2  >  ZERO) THEN
             CALL POLY51 (C02,C12,C22,C32,C42,C52,GG2,
     .            VOLUME(I),V20,V2,V2I,MU2,MU2P1,EINT2,
     .            P2OLD(I),Q2,Q2OLD,PEXT,P2,PM2,P2I,
     .            RHO2,RHO20,MAS2,SSP2,DVDP2,DPDV2, E2_INF,
     .            IPLA2,UPARAM(151),PLAS2(I), 0, GRUN2)
          ENDIF
C     MAT3
          IF (V3  >  ZERO) THEN
             CALL POLY51 (C03,C13,C23,C33,C43,C53,GG3,
     .            VOLUME(I),V30,V3,V3I,MU3,MU3P1,EINT3,
     .            P3OLD(I),Q3,Q3OLD,PEXT,P3,PM3,P3I,
     .            RHO3,RHO30,MAS3,SSP3,DVDP3,DPDV3, E3_INF,
     .            IPLA3,UPARAM(201),PLAS3(I), 0, GRUN3)
          ENDIF
C     MAT4 : High Explosive
          IF (V4  >  ZERO) THEN
C---------------------------------------------------
C     Computation of burnt fraction
C---------------------------------------------------
             CALL COMPUTE_BFRAC(TIME, XL, TBURN, UPARAM, V4, RHO4, RHO40, MAS4, BFRAC(I))

             CALL JWL51 (TIME,XL,TBURN,UPARAM,
     .            VOLUME(I),V4,V4I,MU4,MU4P1,EINT4,
     .            P4OLD(I),Q4,Q4OLD,PEXT,P4,PM4,P4I,
     .            RHO4,RHO40,MAS4,SSP4,DVDP4,DPDV4,
     .            BFRAC(I),V40, 0, GRUN4)
          ENDIF
C---------------------------------------------------
C     Save initial state in case of convergence failure
C---------------------------------------------------

          V1I = V1
          V2I = V2
          V3I = V3
          V4I = V4
          P1I = P1
          P2I = P2
          P3I = P3
          P4I = P4
          EINT1_INI = EINT1
          EINT2_INI = EINT2
          EINT3_INI = EINT3
          EINT4_INI = EINT4
          SSP1_INI = SSP1
          SSP2_INI = SSP2
          SSP3_INI = SSP3
          SSP4_INI = SSP4
C---------------------------------------------------
C     ITERATIVE SOLVER PARAMETERS
C---------------------------------------------------
          ITER = 0
          NITER = 50
          TOL = EM4
          TOL2 = EM12
          ERROR = EP10
          ERROR2 = ERROR
          STEP = ONE
          CONT = 1
          DO WHILE(ITER <= NITER .AND. CONT  ==  1)
             ITER = ITER + 1

             COEF1 = ZERO
             IF (V1  >  ZERO .AND. SSP1  >  ZERO) THEN
                COEF1 = V1 / RHO1 / SSP1 / SSP1
             ENDIF
             COEF2 = ZERO
             IF (V2  >  ZERO .AND. SSP2  >  ZERO) THEN
                COEF2 = V2 / RHO2 / SSP2 / SSP2
             ENDIF
             COEF3 = ZERO
             IF (V3  >  ZERO .AND. SSP3  >  ZERO) THEN
                COEF3 = V3 / RHO3 / SSP3 / SSP3
             ENDIF
             COEF4 = ZERO
             IF (V4  >  ZERO .AND. SSP4  >  ZERO) THEN
                COEF4 = V4 / RHO4 / SSP4 / SSP4
             ENDIF
             !!! Relaxation pressure
             P = (COEF1 * P1 + COEF2 * P2 + COEF3 * P3 + COEF4 * P4)
             P = P / (COEF1 + COEF2 + COEF3 + COEF4)

             !!! Compute corresponding volume variations
             DV1 = COEF1 * (P1 - P)
             DV2 = COEF2 * (P2 - P)
             DV3 = COEF3 * (P3 - P)
             DV4 = COEF4 * (P4 - P)

             !!! Compute a step less or equal that one to ensure
             !!! that volume does not decrease of more than half
             !!! of its previous value

             STEP = ONE
             IF (DV1 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V1 / DV1)
             ENDIF
             IF (DV2 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V2 / DV2)
             ENDIF
             IF (DV3 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V3 / DV3)
             ENDIF
             IF (DV4 < ZERO) THEN
                STEP = MIN(STEP, -HALF * V4 / DV4)
             ENDIF

             DV1 = STEP * DV1
             DV2 = STEP * DV2
             DV3 = STEP * DV3
             DV4 = STEP * DV4

             !!! Note that SUM (DV_i) = ZERO so that volume conservation is achieved

             !!! Update submaterial internal energies in a thermodynamically consistent way
             !!! only if this has a sense (GRUN /= 0)
             IF (V1  >  ZERO) THEN
                EINT1 = EINT1 - (P + PEXT) * DV1
             ENDIF
             IF (V2  >  ZERO) THEN
                EINT2 = EINT2 - (P + PEXT) * DV2
             ENDIF
             IF (V3  >  ZERO) THEN
                EINT3 = EINT3 - (P + PEXT) * DV3
             ENDIF
             IF (V4  >  ZERO) THEN
                EINT4 = EINT4 - (P + PEXT) * DV4
             ENDIF

             !!! Update volumes
             V1 = V1 + DV1
             V2 = V2 + DV2
             V3 = V3 + DV3
             V4 = V4 + DV4

             AA = (V1 + V2 + V3 + V4) / VOLUME(I)
             V1 = V1 / AA
             V2 = V2 / AA
             V3 = V3 / AA
             V4 = V4 / AA

             !!! Update densities
             IF (V1  >  ZERO) RHO1 = MAS1 / V1
             IF (V2  >  ZERO) RHO2 = MAS2 / V2
             IF (V3  >  ZERO) RHO3 = MAS3 / V3
             IF (V4  >  ZERO) RHO4 = MAS4 / V4

             !!! Update submaterial pressures
             PM5 = -EP20
             IF (V1  >  ZERO) PM5 = max(PM5,PM1)
             IF (V2  >  ZERO) PM5 = max(PM5,PM2)
             IF (V3  >  ZERO) PM5 = max(PM5,PM3)
             IF (V4  >  ZERO) PM5 = max(PM5,PM4)

            !===========================================
            !       material 1 - Polynomial EOS
            !===========================================
            IF (V1  >  ZERO) THEN
                CALL POLY51 (C01,C11,C21,C31,C41,C51,GG1,
     .                        VOLUME(I),V10,V1,V1I,MU1,MU1P1,EINT1,
     .                        P1OLD(I),Q1,Q1OLD,PEXT,P1,PM5,P1I,
     .                        RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, E1_INF,
     .                        IPLA1,UPARAM(101),PLAS1(I), 0, GRUN1)
            ENDIF
            !===========================================
            !       material 2 - Polynomial EOS
            !===========================================
            IF (V2  >  ZERO) THEN
                CALL POLY51 (C02,C12,C22,C32,C42,C52,GG2,
     .                        VOLUME(I),V20,V2,V2I,MU2,MU2P1,EINT2,
     .                        P2OLD(I),Q2,Q2OLD,PEXT,P2,PM5,P2I,
     .                        RHO2,RHO20,MAS2,SSP2,DVDP2,DPDV2, E2_INF,
     .                        IPLA2,UPARAM(151),PLAS2(I), 0, GRUN2)
            ENDIF
            !===========================================
            !       material 3 - Polynomial EOS
            !===========================================
            IF (V3  >  ZERO) THEN
                CALL POLY51 (C03,C13,C23,C33,C43,C53,GG3,
     .                        VOLUME(I),V30,V3,V3I,MU3,MU3P1,EINT3,
     .                        P3OLD(I),Q3,Q3OLD,PEXT,P3,PM5,P3I,
     .                        RHO3,RHO30,MAS3,SSP3,DVDP3,DPDV3, E3_INF,
     .                        IPLA3,UPARAM(201),PLAS3(I), 0, GRUN3)
            ENDIF
            !===========================================
            !       material 4 - Polynomial EOS
            !===========================================
            IF (V4  >  ZERO)THEN
                CALL JWL51 (TIME,XL,TBURN,UPARAM,
     .                      VOLUME(I),V4,V4I,MU4,MU4P1,EINT4,
     .                      P4OLD(I),Q4,Q4OLD,PEXT,P4,PM5,P4I,
     .                      RHO4,RHO40,MAS4,SSP4,DVDP4,DPDV4,
     .                      BFRAC(I),V40, 0, GRUN4)
            ENDIF

            !!! Check convergence
            ERROR = ZERO
            IF (V1  >  ZERO) ERROR = ERROR + ABS(DV1) / V1
            IF (V2  >  ZERO) ERROR = ERROR + ABS(DV2) / V2
            IF (V3  >  ZERO) ERROR = ERROR + ABS(DV3) / V3
            IF (V4  >  ZERO) ERROR = ERROR + ABS(DV4) / V4

            IF (ERROR  <  TOL) THEN
               CONT = 0
            ENDIF

          ENDDO

C---------------------------------------------------
C     CONVERGENCE TEST
C---------------------------------------------------
          IF (CONT  ==  1) THEN
             IF (IDBG  ==  1) THEN
                PRINT*, "*** Non convergence of LAW51 EOS in elem", IX(NIX, I + NFT), ERROR, ERROR2
             ENDIF
c$$$             P1 = P1I
c$$$             P2 = P2I
c$$$             P3 = P3I
c$$$             P4 = P4I
c$$$             EINT1 = EINT1_INI
c$$$             EINT2 = EINT2_INI
c$$$             EINT3 = EINT3_INI
c$$$             EINT4 = EINT4_INI
c$$$             V1 = V1I
c$$$             V2 = V2I
c$$$             V3 = V3I
c$$$             V4 = V4I
c$$$             SSP1 = SSP1_INI
c$$$             SSP2 = SSP2_INI
c$$$             SSP3 = SSP3_INI
c$$$             SSP4 = SSP4_INI
c$$$             P = V1 * P1 + V2 * P2 + V3 * P3 + V4 * P4
c$$$             P = P / VOLUME(I)
          ENDIF
          AA = (V1 + V2 + V3 + V4) / VOLUME(I)
          IF (IDBG==1 .AND. ABS(AA - ONE) > EM14) THEN
             PRINT*, "**WARNING : CONVERGENCE ISSUE", AA
          ENDIF
C     Sanity check
          ERROR2 = ABS(EINT(I) - EINT1 - EINT2 - EINT3 - EINT4)
          IF (IDBG==1 .AND. ERROR2 > EM13 * (ONE + ABS(EINT(I)))) THEN
             PRINT*, "**Warning : Conversgence Issue", ERROR2
          ENDIF
          Q1 = Q1OLD
          Q2 = Q2OLD
          Q3 = Q3OLD
          Q4 = Q4OLD
          P = P1 * V1 + P2 * V2 + P3 * V3 + P4 * V4
          P = P / VOLUME(I)
C=======================================================================
C=======================================================================
C     END New solver
C=======================================================================
C=======================================================================


        ELSEIF(UNEPHASE == 0) THEN
          !! This case should not occur.It means that MASS<=ZERO
          !print *, "Warning, empty element"
           SOUNDSP(I)=EM20

        ENDIF
        RHO(I) = MASS / VOLUME(I)

        SOUNDSP(I) = ZERO
        VISA1 = ZERO

        IF (MAS1 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS1 / MASS * SSP1 * SSP1
           VISA1 = VISA1 + V1*RHO1 * VISC1
        ENDIF
        IF (MAS2 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS2 / MASS * SSP2 * SSP2
           VISA1 = VISA1 + V2*RHO2 * VISC2
        ENDIF
        IF (MAS3 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS3 / MASS * SSP3 * SSP3
           VISA1 = VISA1 + V3*RHO3 * VISC3
        ENDIF
        IF (MAS4 / MASS  >  TOL51) THEN
           SOUNDSP(I) = SOUNDSP(I) + MAS4 / MASS * SSP4 * SSP4
           VISA1 = VISA1 + V4*RHO4 * VISC4
        ENDIF

        VISA1 = VISA1 / VOLUME(I) / RHO(I)

        SOUNDSP(I) = SQRT(SOUNDSP(I))
        VISCMAX(I)=VQ0
        VISCMAX(I) = VISCMAX(I)/TWO
        VISCMAX(I) = VISCMAX(I) + RHO(I)*(TWO*VISA1 + VISB1)/THREE

!
C=======================================================================
C CALCULTATION OF CONSISTENT DENSITY WITH PRESSURE
C IF ONE MATERIAL IS NOT PRESENT
C=======================================================================
C----------------------------
C       phases vide calcul de rho consistant avec p
c
c     dP/drho = C1/rho0 + max(mu,0)(2 C2 + 3 C3 mu)/rho0
c            + (C4 + C5 mu)  (P+Q) /(1+mu)^2 /rho0
c            + C5 e
C----------------------------
c 1-  mu=0 e = E0/rho0
c     dP/drho = (C1 + C4 (P+Q) + C5 E0 ) /rho0
c     drho/dP = rho0/(C1 + C4 (P+Q) + C5 E0 )
c     drho = dP rho0/(C1 + C4 (P+Q) + C5 E0 )
C----------------------------
c 2-  mu=rho/rho0 - 1
c     e = E /rho0
c     d(rho0e)V0 = -P DV
c     d(rho0e) = -P DV/V0 = -P (V/VO - V_/V0) = -P(rho0/rho-rho0/rho_)
c     d(rho0e) = -P rho0/rho(1-rho/rho_)
c     rho0e = rho0e + P rho0/rho a      a = rho/rho_ -1
c     a = drho/rho
c     dP/drho = C1/rho0 + max(mu,0)(2 C2 + 3 C3 mu)/rho0
c            + (C4 + C5 mu)  (P+Q) /(1+mu)^2 /rho0
c            + C5 rho0e/rho0
c     drho/dP = rho0 / [C1 + max(mu,0)(2 C2 + 3 C3 mu)
c            + (C4 + C5 mu)  (P+Q) /(1+mu)^2
c            + C5 rho0e]
c     drho = rho0 dP/ [C1 + max(mu,0)(2 C2 + 3 C3 mu)
c            + (C4 + C5 mu)  (P+Q) /(1+mu)^2
c            + C5 rho0e]
c     drho/rho = rho0/rho dP/ [C1 + max(mu,0)(2 C2 + 3 C3 mu)
c            + (C4 + C5 mu)  (P+Q) /(1+mu)^2
c            + C5 rho0e]
c     a = rho0/rho dP/ [C1 + max(mu,0)(2 C2 + 3 C3 mu)
c            + (C4 + C5 mu)  (P+Q) /(1+mu)^2
c            + C5 rho0e]
c     rho0e = rho0e + P rho0/rho a
c     rho   = rho  (1+a)
C----------------------------
!
C=======================================================================
C  OUTPUT : STRESS TENSOR
C           VISCOUS STRESS
C=======================================================================

        !----------------------------------------------------------------------------------!
        !SUBATERIAL INTERNAL ENERGY HAS TO BE BOUNDED : EINT1+EINT2+EINT3+EINT4 = EINT(new)
        ! THIS IS NOT ENSURED AT THE END OF NEWTON ITERATIONS
        !----------------------------------------------------------------------------------!
        !EINT_NEW = EINT(I) -(POLD+P+PEXT+PEXT+QOLD+Q)*DVOL*HALF
        !EINT_k10 = EINT1+EINT2+EINT3+EINT4
        !IF(ABS(EINT_k10) >= EM06)THEN
        !  ALP   = EINT_NEW / EINT_k10
        !  EINT1 = ALP * EINT1
        !  EINT2 = ALP * EINT2
        !  EINT3 = ALP * EINT3
        !  EINT4 = ALP * EINT4
        !ENDIF

        !---------------------------------------------------------------------------------------------!
        !ENERGY INTEGRATION WITH PRESSURE CONTRIBUTION IS DONE AT ENT OF MULAW SUBROUTINE             !
        !---------------------------------------------------------------------------------------------!        
        !EINT(I) = EINT(I) - (PEXT+PEXT+QOLD+Q)*DVOL*HALF !Attention : Q est retranche ici car stocke dans Svis pour mulaw et sfint
     
        !---------------------------!
        !   TOTAL STRESS TENSOR     !
        !---------------------------!
        K1 = N0PHAS
        K2 = N0PHAS+NVPHAS
        K3 = N0PHAS+2*NVPHAS
        K4 = N0PHAS+3*NVPHAS

        SIGNXX(I) = UVAR(I,2+K1) * V1 + UVAR(I,2+K2) * V2 + UVAR(I,2+K3) * V3
        SIGNYY(I) = UVAR(I,3+K1) * V1 + UVAR(I,3+K2) * V2 + UVAR(I,3+K3) * V3
        SIGNZZ(I) = UVAR(I,4+K1) * V1 + UVAR(I,4+K2) * V2 + UVAR(I,4+K3) * V3
        SIGNXY(I) = UVAR(I,5+K1) * V1 + UVAR(I,5+K2) * V2 + UVAR(I,5+K3) * V3
        SIGNYZ(I) = UVAR(I,6+K1) * V1 + UVAR(I,6+K2) * V2 + UVAR(I,6+K3) * V3
        SIGNZX(I) = UVAR(I,7+K1) * V1 + UVAR(I,7+K2) * V2 + UVAR(I,7+K3) * V3
        IF(IEXP/=0)THEN
         SIGNXX(I) = SIGNXX(I) + UVAR(I,2+K4) * V4
         SIGNYY(I) = SIGNYY(I) + UVAR(I,3+K4) * V4
         SIGNZZ(I) = SIGNZZ(I) + UVAR(I,4+K4) * V4
         SIGNXY(I) = SIGNXY(I) + UVAR(I,5+K4) * V4
         SIGNYZ(I) = SIGNYZ(I) + UVAR(I,6+K4) * V4
         SIGNZX(I) = SIGNZX(I) + UVAR(I,7+K4) * V4
        ENDIF

        SIGNXX(I) = SIGNXX(I) / VOLUME(I)
        SIGNYY(I) = SIGNYY(I) / VOLUME(I)
        SIGNZZ(I) = SIGNZZ(I) / VOLUME(I)
        SIGNXY(I) = SIGNXY(I) / VOLUME(I)
        SIGNYZ(I) = SIGNYZ(I) / VOLUME(I)
        SIGNZX(I) = SIGNZX(I) / VOLUME(I)

        SIGNXX(I) = SIGNXX(I) - P
        SIGNYY(I) = SIGNYY(I) - P
        SIGNZZ(I) = SIGNZZ(I) - P

        !---------------------------!
        !   VISCOUS STRESS TENSOR   !
        !---------------------------!
        SIGVXX(I) = TWO*RHO(I)*VISA1*EPSPXX(I) + RHO(I)*VISB1*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))      
        SIGVYY(I) = TWO*RHO(I)*VISA1*EPSPYY(I) + RHO(I)*VISB1*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))              
        SIGVZZ(I) = TWO*RHO(I)*VISA1*EPSPZZ(I) + RHO(I)*VISB1*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))              
        SIGVXY(I) = RHO(I)* VISA1 * EPSPXY(I)
        SIGVYZ(I) = RHO(I)* VISA1 * EPSPYZ(I)
        SIGVZX(I) = RHO(I)* VISA1 * EPSPZX(I)

C===========================================================C
C  OUTPUT : UVAR(,) SUBMATERIAL OUTPUTS                     C
C===========================================================C
C     DC : suppress this. Really dangerous, since modification of
C     volume should me followed by modification of density,
C     energy and pressure ...
        !IF(V1 == ZERO.AND.MAS1 > ZERO)V1 = Tol51*VOLUME(I)
        !IF(V2 == ZERO.AND.MAS2 > ZERO)V2 = Tol51*VOLUME(I)
        !IF(V3 == ZERO.AND.MAS3 > ZERO)V3 = Tol51*VOLUME(I)
        !IF(V4 == ZERO.AND.MAS4 > ZERO)V4 = Tol51*VOLUME(I)
C     DC : same comment
        !IF (MAS1 > ZERO .AND. V1/VOLUME(I) < EM20)V1 = EM20*VOLUME(I)
        !IF (MAS2 > ZERO .AND. V2/VOLUME(I) < EM20)V2 = EM20*VOLUME(I)
        !IF (MAS3 > ZERO .AND. V3/VOLUME(I) < EM20)V3 = EM20*VOLUME(I)
        !IF (MAS4 > ZERO .AND. V4/VOLUME(I) < EM20)V4 = EM20*VOLUME(I)
        !===================================================C
        !        material 1 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        KK = N0PHAS
        IF(V1  >  ZERO)THEN
            UVAR(I,1+KK)  = V1 / VOLUME(I)
            !deviator stress already updated in plasticity subroutines
            UVAR(I,18+KK) =   P1
            UVAR(I,14+KK) = SSP1
            UVAR(I,15+KK) = PLAS1(I)     ! Eps plastique
            ECOLD         = -T10 * SPH1
            IF(MU1 > ZERO) ECOLD = ECOLD * (ONE+C51*MU1*(ONE-MU1)) + HALF*C21*MU1*MU1
            TEMP1         = (EINT1/V1/MU1P1 - ECOLD) / SPH1
            UVAR(I,16+KK) = TEMP1
            UVAR(I,8+KK)  = EINT1 / V1 ! energie IN rho e OUT
            UVAR(I,9+KK)  = RHO1       ! masse IN rho OUT
            UVAR(I,10+KK) = Q1
            UVAR(I,11+KK) = V1
            UVAR(I,12+KK) = RHO1       ! rho_old IN rho OUT
            UVAR(I,17+KK) = EDIF1 / V1 ! energie diffusee IN spec OUT
!           UVAR(I,19+KK) = EPSEQ1(I)
        ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO10
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = T10
            TEMP1         = T10
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            UVAR(I,18+KK) =  P
            UVAR(I,19+KK) = ZERO
            EPSEQ1(I)     = ZERO
        ENDIF
C        IF(V1/VOLUME(I)  <  EM03)UVAR(I,14+KK)=ZERO

        !===================================================C
        !        material 2 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        KK = N0PHAS + NVPHAS
        IF(V2  >  ZERO)THEN
            UVAR(I,1+KK)  = V2  / VOLUME(I)
            !deviator stress already updated in plasticity subroutines
            UVAR(I,18+KK) = P2
            UVAR(I,14+KK) = SSP2
            UVAR(I,15+KK) = PLAS2(I)   ! Eps plastique
            ECOLD         = -T20 * SPH2
            IF(MU2 > ZERO) ECOLD = ECOLD * (ONE+C52*MU2*(ONE-MU2)) + HALF*C22*MU2*MU2
            TEMP2         = (EINT2/V2/(MU2P1) - ECOLD) / SPH2
            UVAR(I,16+KK) = TEMP2
            UVAR(I,8+KK)  = EINT2 / V2
            UVAR(I,9+KK)  = RHO2
            UVAR(I,10+KK) = Q2
            UVAR(I,11+KK) = V2
            UVAR(I,12+KK) = RHO2
            UVAR(I,17+KK) = EDIF2 / V2 ! energie diffusee IN spec OUT
!           UVAR(I,19+KK) = EPSEQ2(I)
        ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO20
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = T20
            TEMP2         = T20
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            UVAR(I,18+KK) =   P
            UVAR(I,19+KK) = ZERO
            EPSEQ2(I)     = ZERO
        ENDIF
C        IF(V2/VOLUME(I)  <  EM03)UVAR(I,14+KK)=ZERO

        !===================================================C
        !        material 3 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        KK = N0PHAS + 2*NVPHAS
        IF(V3  >  ZERO)THEN
            UVAR(I,1+KK)  = V3  / VOLUME(I)
            !deviator stress already updated in plasticity subroutines
            UVAR(I,18+KK) =   P3
            UVAR(I,14+KK) = SSP3
            UVAR(I,15+KK) = PLAS3(I)   ! Eps plastique
            ECOLD         = -T30 * SPH3
            IF(MU3 > ZERO) ECOLD = ECOLD * (ONE+C53*MU3*(ONE-MU3)) + HALF*C23*MU3*MU3
            TEMP3         = (EINT3/V3/MU3P1 - ECOLD) / SPH3
            UVAR(I,16+KK) = TEMP3
            UVAR(I,8+KK)  = EINT3 / V3
            UVAR(I,9+KK)  = RHO3
            UVAR(I,10+KK) = Q3
            UVAR(I,11+KK) = V3
            UVAR(I,12+KK) = RHO3
            UVAR(I,17+KK) = EDIF3 / V3 ! energie diffusee IN spec OUT
!           UVAR(I,19+KK) = EPSEQ3(I)
        ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO30
            UVAR(I,14+KK) = ZERO
            UVAR(I,15+KK) = ZERO
            UVAR(I,16+KK) = T30
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            UVAR(I,18+KK) =   P
            TEMP3         = T30
            UVAR(I,19+KK) = ZERO
            EPSEQ3(I)     = ZERO
        ENDIF
C        IF(V3/VOLUME(I)  <  EM03)UVAR(I,14+KK)=ZERO

        !===================================================C
        !        material 4 : THERMODYNAMICAL STATE OUTPUT  C
        !===================================================C
        IF(IEXP/=0)THEN
        KK = N0PHAS + 3*NVPHAS
         IF(V4  >  ZERO)THEN
            UVAR(I,1+KK)  = V4 / VOLUME(I)
            UVAR(I,18+KK) = P4
            UVAR(I,14+KK) = SSP4
            UVAR(I,15+KK) = ZERO      ! Eps plastique
            UVAR(I,8+KK)  = EINT4 / V4 ! energie IN rho e OUT
            UVAR(I,9+KK)  = RHO4       ! masse IN rho OUT
            UVAR(I,10+KK) = Q4
            UVAR(I,11+KK) = V4
            UVAR(I,12+KK) = RHO4       ! rho_old IN rho OUT
            UVAR(I,15+KK) = TBURN   ! -burn time ou burn fraction
            UVAR(I,17+KK) = EDIF4 / V4 ! energie diffusee IN spec OUT
            TEMP4 = T40
            UVAR(I,19+KK) = ZERO
         ELSE
            UVAR(I,1+KK)  = ZERO
            UVAR(I,2+KK)  = ZERO
            UVAR(I,3+KK)  = ZERO
            UVAR(I,4+KK)  = ZERO
            UVAR(I,18+KK) = P
            UVAR(I,5+KK)  = ZERO
            UVAR(I,6+KK)  = ZERO
            UVAR(I,7+KK)  = ZERO
            UVAR(I,8+KK)  = ZERO
            UVAR(I,9+KK)  = ZERO
            UVAR(I,10+KK) = ZERO
            UVAR(I,11+KK) = ZERO
            UVAR(I,12+KK) = RHO40
            UVAR(I,14+KK) = ZERO
            UVAR(I,16+KK) = T40
            UVAR(I,17+KK) = ZERO ! energie diffusee IN spec OUT
            TEMP4 = T40
            UVAR(I,19+KK) = ZERO
         ENDIF
C         IF(V4/VOLUME(I)  <  EM03)UVAR(I,14+KK)=ZERO
        END IF
        !===================================================
        !               UVAR(I,1) : PLAS
        !===================================================
        IF(IPLA1 == 2)PLAS1(I)=EPSEQ1(I)
        IF(IPLA2 == 2)PLAS2(I)=EPSEQ2(I)
        IF(IPLA3 == 2)PLAS3(I)=EPSEQ3(I)
        UVAR(I,1) = (PLAS1(I) * V1 + PLAS2(I) * V2 + PLAS3(I) * V3) / VOLUME(I)
        IF(BUFLY%L_PLA>0)LBUF%PLA(I) = UVAR(I,1)

        !===================================================
        !               UVAR(I,2)    : TEMP
        !===================================================
        H1 = SPH1*V1*(MU1P1)
        H2 = SPH2*V2*(MU2P1)
        H3 = SPH3*V3*(MU3P1)
        H4 = SPH4*V4*(MU4P1)
        ! temperature out ! chaleur     in
        UVAR(I,2) = (TEMP1*H1 + TEMP2*H2 + TEMP3*H3 + TEMP4*H4)  / (H1 + H2 + H3 + H4)
        IF(BUFLY%L_TEMP>0)LBUF%TEMP(I) = UVAR(I,2)
        !===================================================
        !               UVAR(I,3) : BFRAC
        !===================================================
        IF(V4 > ZERO)THEN
          UVAR(I,3) = BFRAC(I)
        ELSE
          UVAR(I,3) = ZERO
        END IF
        IF(BUFLY%L_BFRAC>0)LBUF%BFRAC(I) = UVAR(I,3)
        !===================================================
        !               EPSEQ (DPrag)
        !===================================================
        IF(BUFLY%L_EPSQ>0)THEN
          LBUF%EPSQ(I) = ZERO
          IF(IPLA1==2)LBUF%EPSQ(I) = LBUF%EPSQ(I) + V1*EPSEQ1(I)
          IF(IPLA2==2)LBUF%EPSQ(I) = LBUF%EPSQ(I) + V2*EPSEQ2(I)
          IF(IPLA3==2)LBUF%EPSQ(I) = LBUF%EPSQ(I) + V3*EPSEQ3(I)
          LBUF%EPSQ(I) = LBUF%EPSQ(I) / VOLUME(I)
        ENDIF

       IF(JTHE == 1 ) THEN
         GBUF%TEMP(I) = THREE100 + EM03
       ELSE
         GBUF%TEMP(I) = UVAR(I,2)
       ENDIF


       !provisoire : tracking mat 1 | mat B in animation files
        !IF(INT22>0)THEN
        !  GBUF  => ELBUF_TAB(NG)%GBUF
        !  UVAR(I,1)=GBUF%TAG22(I)
        !ENDIF

        QVIS(I) = QQOLD(I)

        !===================================================
        !               POST-TREATMENT (DEBUG)
        !     must be commented in official releases
        !===================================================
Cts        IF(IX(11,I+NFT)==ibug_ID(1) .OR. IX(11,I+NFT)==ibug_ID(2)) THEN
Cts          CALL WRITE_BUF_LAW51(IX    , NFT     , NUVAR    , NEL      , UVAR     ,
Cts     .                         I     , UNEPHASE, DD       , dbVOLD   , dbVOLD_f ,
Cts     .                         VOLUME, VOLD    , EPSPXX   , EPSPYY   , EPSPZZ   ,
Cts     .                         TAG22 ,BFRAC(I) , RHO10    , RHO20    , RHO30    ,
Cts     .                         RHO40)
Cts        ENDIF


      ENDDO  !DO I=1,NEL

      RETURN
      END
