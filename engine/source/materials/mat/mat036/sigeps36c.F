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
Chd|  SIGEPS36C                     source/materials/mat/mat036/sigeps36c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS36C(
     1     NEL    ,NUPARAM,NUVAR   ,NVARTMP ,MFUNC  ,
     2     KFUNC  ,NPF    ,NPT0    ,IPT     ,IPLAS  ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     AREA   ,EINT   ,THKLY   ,ASRATE  ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX   ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     9     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR    ,
     A     VARTMP ,OFF    ,NGL     ,IPM     ,IMAT    ,
     B     ETSE   ,GS     ,YLD     ,EPSP    ,ISRATE  ,
     C     DPLA_I ,GAMA_IMP,SIGNOR ,SHF     ,HARDM  ,
     D     FACYLDI,SIGB   ,INLOC   ,DPLANL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C NPT0    |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C IPLAS   |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL    | F | R | INITIAL DENSITY
C AREA    | NEL    | F | R | AREA
C EINT    | 2*NEL  | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C THKLY   | NEL    | F | R | LAYER THICKNESS
C EPSPXX  | NEL    | F | R | STRAIN RATE XX
C EPSPYY  | NEL    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL    | F | R | STRAIN XX
C EPSYY   | NEL    | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL    | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C THK     | NEL    | F |R/W| THICKNESS
C PLA     | NEL    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR, NVARTMP,NPT0,IMAT,IPT,ISRATE,INLOC
      INTEGER IPLAS(*),NGL(NEL),IPM(NPROPMI,*)
      my_real :: TIME,TIMESTEP,ASRATE
      my_real :: UPARAM(*),SIGB(*),AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THKLY(NEL),PLA(NEL),FACYLDI(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL),EPSXY(NEL),EPSYZ(NEL),EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   GS(NEL),EPSP(NEL),SHF(NEL),HARDM(NEL),DPLANL(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL),
     .    DPLA_I(NEL),GAMA_IMP(NEL),SIGNOR(MVSIZ,5)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .    UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
      INTEGER, INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER :: NPF(*), MFUNC, KFUNC(MFUNC)
      my_real :: TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ::  I,J,K,N,II,J1,J2,NINDX,VP,IFAIL,OPTE,NINDEX_PLA,
     .        NITER,NRATE,IPFUN,NFUNC,PFUN,IFUNCE,FUN1,FUN2,YLDCHECK
      INTEGER IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .        JJ(MVSIZ),INDEX(MVSIZ),IPOSP(MVSIZ),IPOSPE(MVSIZ),
     .        IFUNC(100),IADP(MVSIZ),ILENP(MVSIZ)
      INTEGER, DIMENSION(MVSIZ) :: INDEX_PLA
      my_real :: A,B,C,P,P2,R,CC,FAC,DEZZ,SVM,
     .        HKIN,DTINV,AAA,SOUNDSP1,ALPHA,CE,EINF,
     .        ERR,F,DF,PLA_I,Q2,YLD_I,SIGPXX,SIGPYY,SIGPXY,
     .        E1,A11,A21,G1,G31,NU11,NU21,NU_MNU,U_MNU,T_PNU,NUX,NU3,
     .        EPSMAX,EPSR1,EPSR2,EPSF,FISOKIN,S1,S2,S3,DPLA,VM2,YLD0
      my_real ::  FACT(NEL),E(MVSIZ),A1(MVSIZ),A2(MVSIZ),G(MVSIZ),
     .        DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ),
     .        Y1(MVSIZ),Y2(MVSIZ),DR(MVSIZ),YFAC(MVSIZ,2),ESCALE(MVSIZ),
     .        AA(MVSIZ),BB(MVSIZ),DPLA_J(MVSIZ),
     .        PP(MVSIZ),QQ(MVSIZ),FAIL(MVSIZ),H(MVSIZ),HK(MVSIZ),
     .        SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEXY(MVSIZ),
     .        SVM2(MVSIZ),YLD2(MVSIZ),HI(MVSIZ),G3(MVSIZ),EPST(MVSIZ),
     .        PFAC(MVSIZ),P0(MVSIZ),DFDP(MVSIZ),PSCALE(MVSIZ),
     .        DYDXE(MVSIZ),E0(MVSIZ),PLAP(MVSIZ)
      my_real, DIMENSION(MVSIZ) :: TAB_LOC,VECNUL
      my_real, DIMENSION(:) ,POINTER  :: SIGBXX,SIGBYY,SIGBXY,SIGBYZ,SIGBZX
      TARGET  :: SIGB ,VECNUL
c------------------
      DATA NITER/3/
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      NFUNC = IPM(10,IMAT)
      DO J=1,NFUNC
        IFUNC(J) = IPM(10+J,IMAT)
      ENDDO
      IPFUN = IFUNC(NFUNC-1)
C         
      E1   = UPARAM(2)
      A11  = UPARAM(3)
      A21  = UPARAM(4)
      G1   = UPARAM(5)
      NUX  = UPARAM(6)
      NRATE  = NINT(UPARAM(1))
      EPSMAX = UPARAM(2*NRATE + 7)
      EPSR1  = UPARAM(2*NRATE + 8)
      EPSR2  = UPARAM(2*NRATE + 9)
      G31    = UPARAM(2*NRATE + 11)
      FISOKIN= UPARAM(2*NRATE + 14)
      EPSF   = UPARAM(2*NRATE + 15)
      PFUN   = NINT(UPARAM(2*NRATE + 16))
      SOUNDSP1 = UPARAM(2*NRATE + 18)
      NU_MNU = UPARAM(2*NRATE + 19)   !  NU / (1 - NU)
      T_PNU  = UPARAM(2*NRATE + 20)   !  3  / (1 + NU)
      U_MNU  = UPARAM(2*NRATE + 21)   !  1  / (1 - NU)
      OPTE   = UPARAM(2*NRATE + 23)
      EINF   = UPARAM(2*NRATE + 24)
      CE     = UPARAM(2*NRATE + 25)
      VP       = NINT(UPARAM(2*NRATE + 26)) ! flag for plastic strain dependency
      IFAIL    = NINT(UPARAM(2*NRATE + 27)) ! flag for failure
      YLDCHECK = NINT(UPARAM(2*NRATE + 28))
c-----------------------------
      IF (FISOKIN > 0) THEN
        SIGBXX => SIGB(1       :  NEL)
        SIGBYY => SIGB(NEL+1  :2*NEL)
        SIGBXY => SIGB(2*NEL+1:3*NEL)
        SIGBYZ => SIGB(3*NEL+1:4*NEL)
        SIGBZX => SIGB(4*NEL+1:5*NEL)
      ELSE
        VECNUL(1:NEL) = ZERO
        SIGBXX => VECNUL(1:NEL)
        SIGBYY => VECNUL(1:NEL)
        SIGBXY => VECNUL(1:NEL)
        SIGBYZ => VECNUL(1:NEL)
        SIGBZX => VECNUL(1:NEL)
      ENDIF
c
      NINDEX_PLA = 0
      VISCMAX(1:NEL) = ZERO
      ETSE(1:NEL) = ONE
      E(1:NEL)  = E1
      A1(1:NEL) = A11
      A2(1:NEL) = A21
      G(1:NEL)  = G1
      G3(1:NEL) = G31
      SOUNDSP(1:NEL) = SOUNDSP1
c-----------------------
      IF (OPTE == 1) THEN    ! Variable Young modulus defined by tabulated function
        IFUNCE = UPARAM(2*NRATE + 22)
        IF (IFUNCE > 0) THEN
          DO I=1,NEL
            IF(PLA(I) > ZERO)THEN
              NINDEX_PLA = NINDEX_PLA + 1
              INDEX_PLA(NINDEX_PLA) = I
              IPOSPE(NINDEX_PLA) = VARTMP(I,1)
              IADP(NINDEX_PLA)  = NPF(KFUNC(IFUNCE)) / 2 + 1
              ILENP(NINDEX_PLA) = NPF(KFUNC(IFUNCE)+1) / 2 - IADP(NINDEX_PLA) - IPOSPE(NINDEX_PLA)
              TAB_LOC(NINDEX_PLA) = PLA(I)
            ENDIF
          ENDDO
          CALL VINTER2(TF,IADP,IPOSPE,ILENP,NINDEX_PLA,TAB_LOC,DYDXE,ESCALE)
          VARTMP(INDEX_PLA(1:NINDEX_PLA),1) = IPOSPE(1:NINDEX_PLA)
        ENDIF

#include "vectorize.inc"
        DO II=1,NINDEX_PLA
          I = INDEX_PLA(II)
          E(I) =  ESCALE(II)* E1
          A1(I) = E(I)/(ONE - NUX*NUX)
          A2(I) = NUX*A1(I)
          G(I) =  HALF*E(I)/(ONE+NUX)
          G3(I) = THREE*G(I)
          GS(I) = G(I) * SHF(I)
          SOUNDSP(I) = SQRT(E(I)/(ONE - NUX*NUX)/RHO0(I))
        ENDDO
      ELSEIF (CE /= ZERO) THEN       ! Variable Young modulus defined analytically
        DO I=1,NEL
          IF (PLA(I) > ZERO) THEN
            E(I) = E1-(E1-EINF)*(ONE-EXP(-CE*PLA(I)))
            A1(I) = E(I)/(ONE - NUX*NUX)
            A2(I) = NUX*A1(I)
            G(I) =  HALF*E(I)/(ONE+NUX)
            G3(I) = THREE*G(I)
            GS(I) = G(I) * SHF(I)
            SOUNDSP(I) = SQRT(E(I)/(ONE - NUX*NUX)/RHO0(I))
          ENDIF
        ENDDO
      ENDIF  ! OPTE
c-------------------
c     damage factor based on equivalent strain
c-------------------
      IF (IFAIL == 2) THEN
        EPST(1:NEL) = HALF*( EPSXX(1:NEL)+EPSYY(1:NEL)
     .   + SQRT( (EPSXX(1:NEL)-EPSYY(1:NEL))*(EPSXX(1:NEL)-EPSYY(1:NEL))
     .   + EPSXY(1:NEL)*EPSXY(1:NEL) ) )
        FAIL(1:NEL) = MAX(EM20,MIN(ONE,(EPSR2-EPST(1:NEL))/(EPSR2-EPSR1)))
      ELSE
        FAIL(1:NEL) = ONE
      ENDIF
C=======================================================================
c     Split code in 2 independent part :
c              VP = 1 dependent on plastic strain rate
c              VP = 0 dependent on total strain rate
C=======================================================================
      IF (VP == 0) THEN
c------------------------------------------
        SIGOXX(1:NEL) = SIGOXX(1:NEL) - SIGBXX(1:NEL)
        SIGOYY(1:NEL) = SIGOYY(1:NEL) - SIGBYY(1:NEL)
        SIGOXY(1:NEL) = SIGOXY(1:NEL) - SIGBXY(1:NEL)
C
        P0(1:NEL) = -(SIGOXX(1:NEL) + SIGOYY(1:NEL))*THIRD
        SIGNXX(1:NEL) = SIGOXX(1:NEL)+A1(1:NEL)*DEPSXX(1:NEL)+A2(1:NEL)*DEPSYY(1:NEL)
        SIGNYY(1:NEL) = SIGOYY(1:NEL)+A2(1:NEL)*DEPSXX(1:NEL)+A1(1:NEL)*DEPSYY(1:NEL)
        SIGNXY(1:NEL) = SIGOXY(1:NEL)+G(1:NEL) *DEPSXY(1:NEL)
        SIGNYZ(1:NEL) = SIGOYZ(1:NEL)+GS(1:NEL) *DEPSYZ(1:NEL)
        SIGNZX(1:NEL) = SIGOZX(1:NEL)+GS(1:NEL) *DEPSZX(1:NEL)
        SIGEXX(1:NEL) = SIGNXX(1:NEL)
        SIGEYY(1:NEL) = SIGNYY(1:NEL)
        SIGEXY(1:NEL) = SIGNXY(1:NEL)
C-------------------
C       STRAIN RATE
C-------------------
        IF (ISRATE == 0) THEN
          DO I=1,NEL
            EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .              + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .              + EPSPXY(I)*EPSPXY(I) ) )
          ENDDO
        ENDIF
c-----------------------------------------------
c       PRESSURE DEPENDENT YIELD FUNCTION FACTOR
c-----------------------------------------------
        IF (PFUN > 0) THEN
          PSCALE(1:NEL) = UPARAM(17 + 2*NRATE )*P0(1:NEL)
          DO I=1,NEL
            IPOSP(I) = VARTMP(I,2)
            IADP(I)  = NPF(IPFUN) / 2 + 1
            ILENP(I) = NPF(IPFUN) / 2 - IADP(I) - IPOSP(I)
          ENDDO
          CALL VINTER2(TF,IADP,IPOSP,ILENP,NEL,PSCALE,DFDP,PFAC)
          PFAC(1:NEL) = MAX(ZERO, PFAC(1:NEL))
          VARTMP(1:NEL,2) = IPOSP(1:NEL)
        ELSE
          PFAC(1:NEL) = ONE
        ENDIF
C-------------------
C       CRITERE
C-------------------
        IF (NRATE == 1) THEN   ! only static curve => no strain rate interpolation
          IPOS1(1:NEL) = VARTMP(1:NEL,3)
          IAD1(1:NEL)  = NPF(IFUNC(1))   / 2 + 1
          ILEN1(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD1(1:NEL) - IPOS1(1:NEL)
c
          CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
c
          YFAC(1:NEL,1)   = UPARAM(6+NRATE+1)*FACYLDI(1:NEL)
          VARTMP(1:NEL,3) = IPOS1(1:NEL)
          FACT(1:NEL) = FAIL(1:NEL) * PFAC(1:NEL) * YFAC(1:NEL,1)
          H(1:NEL)    = DYDX1(1:NEL) * FACT(1:NEL)
c
          IF (FISOKIN == ZERO) THEN
            YLD(1:NEL) = Y1(1:NEL) * FACT(1:NEL)
          ELSE IF (FISOKIN == ONE) THEN
            YLD0 = TF(NPF(IFUNC(1))+1)
            YLD(1:NEL) =  YLD0 * FACT(1:NEL)
          ELSE IF (FISOKIN > ZERO) THEN
            YLD0 = TF(NPF(IFUNC(1))+1)
            YLD(1:NEL) = ((ONE-FISOKIN)*Y1(1:NEL) + FISOKIN *YLD0)*FACT(1:NEL)
          END IF
c
        ELSE  ! strain rate interpolation
          JJ(1:NEL) = 1
          DO J=2,NRATE-1
            DO I=1,NEL
              IF (EPSP(I) >= UPARAM(6+J)) JJ(I) = J
            ENDDO
          ENDDO
#include "vectorize.inc"
          DO I=1,NEL
            FAC = UPARAM(6+JJ(I))
            RATE(I) = (EPSP(I) - FAC)/(UPARAM(7+JJ(I)) - FAC)
            YFAC(I,1)=UPARAM(6+NRATE+JJ(I))*FACYLDI(I)
            YFAC(I,2)=UPARAM(6+NRATE+JJ(I)+1)*FACYLDI(I)
          ENDDO

#include  "vectorize.inc"
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            FUN1 = IFUNC(J1)
            FUN2 = IFUNC(J2)
            IPOS1(I) = VARTMP(I,2+J1)
            IPOS2(I) = VARTMP(I,2+J2)
            IAD1(I)  = NPF(FUN1) / 2 + 1
            ILEN1(I) = NPF(FUN1+1) / 2 - IAD1(I)-IPOS1(I)
            IAD2(I)  = NPF(FUN2) / 2 + 1
            ILEN2(I) = NPF(FUN2+1) / 2 - IAD2(I)-IPOS2(I)
          END DO
C
          CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
          CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
c
          IF (FISOKIN == ZERO) THEN
#include   "vectorize.inc"
            DO I=1,NEL
             J1 = JJ(I)
             J2 = J1+1
             Y1(I)=Y1(I)*YFAC(I,1)
             Y2(I)=Y2(I)*YFAC(I,2)
             FAC   = RATE(I)
             YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
             YLD(I) = MAX(YLD(I),EM20)
             DYDX1(I)=DYDX1(I)*YFAC(I,1)
             DYDX2(I)=DYDX2(I)*YFAC(I,2)
             H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
             YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
             H(I)   = H(I)*MAX(ZERO,PFAC(I))
            ENDDO
            DO I=1,NEL
             J1 = JJ(I)
             J2 = J1+1
             VARTMP(I,2+J1) = IPOS1(I)
             VARTMP(I,2+J2) = IPOS2(I)
            ENDDO
          ELSEIF (FISOKIN == ONE) THEN
#include   "vectorize.inc"
            DO I=1,NEL
             J1 = JJ(I)
             J2 = J1+1
             FUN1 = IFUNC(J1)
             FUN2 = IFUNC(J2)
             FAC   = RATE(I)
             DYDX1(I)=DYDX1(I)*YFAC(I,1)
             DYDX2(I)=DYDX2(I)*YFAC(I,2)
             H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
C           ECROUISSAGE CINEMATIQUE
             Y1(I)=TF(NPF(FUN1)+1)
             Y2(I)=TF(NPF(FUN2)+1)
             Y1(I)=Y1(I)*YFAC(I,1)
             Y2(I)=Y2(I)*YFAC(I,2)
             YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
             YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
             H(I)   = H(I)*MAX(ZERO,PFAC(I))
            ENDDO
            DO I=1,NEL
             J1 = JJ(I)
             J2 = J1+1
             VARTMP(I,2+J1) = IPOS1(I)
             VARTMP(I,2+J2) = IPOS2(I)
            ENDDO
          ELSE  ! Mixed hardening
#include "vectorize.inc"
            DO I=1,NEL
              J1 = JJ(I)
              J2 = J1+1
              FUN1 = IFUNC(J1)
              FUN2 = IFUNC(J2)
              Y1(I)=Y1(I)*YFAC(I,1)
              Y2(I)=Y2(I)*YFAC(I,2)
              FAC   = RATE(I)
              YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
              YLD(I) = MAX(YLD(I),EM20)
              DYDX1(I)=DYDX1(I)*YFAC(I,1)
              DYDX2(I)=DYDX2(I)*YFAC(I,2)
              H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
              Y1(I)=TF(NPF(FUN1)+1)
              Y2(I)=TF(NPF(FUN2)+1)
              Y1(I)=Y1(I)*YFAC(I,1)
              Y2(I)=Y2(I)*YFAC(I,2)
              YLD(I) = (ONE - FISOKIN) * YLD(I) +
     .             FISOKIN * (FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I))))
              YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
              H(I)   = H(I)*MAX(ZERO,PFAC(I))
            ENDDO
            DO I=1,NEL
              J1 = JJ(I)
              J2 = J1+1
              VARTMP(I,2+J1) = IPOS1(I)
              VARTMP(I,2+J2) = IPOS2(I)
            ENDDO
          ENDIF  ! FISOKIN
        END IF  ! NRATE > 0
c
        IF (YLDCHECK == 1) THEN
          DO I=1,NEL
            YLD(I) = MAX(YLD(I), EM20)
          END DO
        END IF
c------------------------------
C       PROJECTION
C-------------------
        IF (IPLAS(1) == 0) THEN
c         projection radiale
          NU3 = ONE-NU_MNU
          DO I=1,NEL
            SVM2(I)= SIGNXX(I)*SIGNXX(I)
     .             + SIGNYY(I)*SIGNYY(I)
     .             - SIGNXX(I)*SIGNYY(I)
     .             + THREE*SIGNXY(I)*SIGNXY(I)
            IF (SVM2(I)>YLD(I)*YLD(I)) THEN
              SVM =SQRT(SVM2(I))
              R  = YLD(I)/SVM
              SIGNXX(I)=SIGNXX(I)*R
              SIGNYY(I)=SIGNYY(I)*R
              SIGNXY(I)=SIGNXY(I)*R
              DPLA_I(I) = OFF(I)*SVM*(ONE-R)/(G3(I)+H(I))
              PLA(I) = PLA(I) + DPLA_I(I)
              IF (INLOC == 0) THEN
                DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
                DEZZ=-(DEPSXX(I)+DEPSYY(I))*NU_MNU-NU3*DEZZ
                THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
              ENDIF
              ETSE(I)= H(I)/(H(I)+E(I)) !
            ENDIF
          ENDDO
C
        ELSEIF (IPLAS(1) == 1) THEN    ! implicit scheme, 3 Newton iterations
C-------------------------
C         CRITERE DE VON MISES
C-------------------------
          DO I=1,NEL
            H(I) = MAX(ZERO,H(I))
            S1=SIGNXX(I)+SIGNYY(I)
            S2=SIGNXX(I)-SIGNYY(I)
            S3=SIGNXY(I)
            AA(I)= FOURTH*S1*S1
            BB(I)= THREE_OVER_4*S2*S2 + THREE*S3*S3
            SVM2(I)= AA(I)+BB(I)
            IF (INLOC == 0) THEN
              DEZZ = -(DEPSXX(I)+DEPSYY(I))*NU_MNU
              THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
            ENDIF
          ENDDO
C-------------------------
C         GATHER PLASTIC FLOW
C-------------------------
          NINDX=0
          DO I=1,NEL
            IF (SVM2(I) > YLD(I)*YLD(I) .AND. OFF(I) == ONE) THEN
              NINDX=NINDX+1
              INDEX(NINDX)=I
            ENDIF
          ENDDO
C
          IF (NINDX > 0) THEN
C---------------------------
C           DEP EN CONTRAINTE PLANE
C---------------------------
#include "vectorize.inc"
            DO J=1,NINDX
             I=INDEX(J)
             SVM=SQRT(SVM2(I))
             DPLA_J(I)=(SVM-YLD(I))/(G3(I)+H(I))
             ETSE(I)= H(I)/(H(I)+E(I))
             HI(I) = H(I)*(ONE-FISOKIN)
             HK(I) = TWO_THIRD*H(I)*FISOKIN
            ENDDO
C
            NU3 = ONE-NU_MNU
            DO N=1,NITER
#include "vectorize.inc"
              DO J=1,NINDX
                I=INDEX(J)
                DPLA_I(I)=DPLA_J(I)
                YLD_I = YLD(I) + HI(I)*DPLA_I(I)
                DR(I) = HALF*E(I)*DPLA_I(I)/YLD_I
                AAA   = THREE*HK(I)/E(I)
                NU11  = U_MNU+AAA
                NU21  = T_PNU+AAA
                PP(I) = ONE/(ONE+DR(I)*NU11)
                QQ(I) = ONE/(ONE+DR(I)*NU21)
                P2    = PP(I)*PP(I)
                Q2    = QQ(I)*QQ(I)
                F     = AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
                DF    =-(AA(I)*NU11*P2*PP(I)+BB(I)*NU21*Q2*QQ(I))
     .            *(E(I)-TWO*DR(I)*HI(I))/YLD_I
     .            -TWO*HI(I)*YLD_I
                DF = SIGN(MAX(ABS(DF),EM20),DF)
                IF(DPLA_I(I) > ZERO) THEN
                  DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
                ELSE
                  DPLA_J(I)=ZERO
                ENDIF
              ENDDO
            ENDDO
C------------------------------------------
C           CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc"
            DO J=1,NINDX
             I=INDEX(J)
             PLA(I) = PLA(I) + DPLA_I(I)
             S1=(SIGNXX(I)+SIGNYY(I))*PP(I)
             S2=(SIGNXX(I)-SIGNYY(I))*QQ(I)
             SIGNXX(I)=HALF*(S1+S2)
             SIGNYY(I)=HALF*(S1-S2)
             SIGNXY(I)=SIGNXY(I)*QQ(I)
             IF (INLOC == 0) THEN
               DEZZ = - NU3*DR(I)*S1/E(I)
               THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
             ENDIF
C-----       for kin hardening	   
             YLD(I) =YLD(I)+HI(I)*DPLA_I(I)
            ENDDO
          ENDIF
c-------------------------------------------
        ELSEIF (IPLAS(1) == 2) THEN
c-------------------------------------------
C         CRITERE DE VON MISES
C-------------------------
          DO I=1,NEL
            H(I) = MAX(ZERO,H(I))
            SVM2(I) = SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)
     .              - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I)
            IF (INLOC == 0) THEN
              DEZZ = -(DEPSXX(I)+DEPSYY(I))*NU_MNU
              THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
            ENDIF
          ENDDO
C-------------------------
C         GATHER PLASTIC FLOW
C-------------------------
          NINDX=0
          DO I=1,NEL
            YLD2(I)=YLD(I)*YLD(I)
            IF (SVM2(I) > YLD2(I) .AND. OFF(I) == ONE) THEN
              NINDX=NINDX+1
              INDEX(NINDX)=I
            ENDIF
          ENDDO
C
          IF (NINDX > 0) THEN
C-------------
C           PROJ NORMALE AU CRITERE AVEC CALCUL APPROCHE DE LA NORMALE + RETOUR RADIAL
C-------------
            NU3 = ONE-NU_MNU
#include "vectorize.inc"
            DO J=1,NINDX
              I=INDEX(J)
              A = (SVM2(I)-YLD2(I))
     .          / (FIVE*SVM2(I)+THREE*(-SIGNXX(I)*SIGNYY(I)+SIGNXY(I)*SIGNXY(I)))
              S1= (ONE-TWO*A)*SIGNXX(I)+ A*SIGNYY(I)
              S2= A*SIGNXX(I)+(ONE-TWO*A)*SIGNYY(I)
              S3=(ONE-THREE*A)*SIGNXY(I)
              SIGNXX(I)=S1
              SIGNYY(I)=S2
              SIGNXY(I)=S3
              SVM=SQRT(SVM2(I))
              DPLA_I(I) = OFF(I)*(SVM-YLD(I))/(G3(I)+H(I))
C
              HK(I) = H(I)*(ONE-FISOKIN)
              YLD(I) =YLD(I)+HK(I)*DPLA_I(I)
            END DO
C
            DO J=1,NINDX
              I=INDEX(J)
              SVM = SQRT( SIGNXX(I)*SIGNXX(I)
     .                    +SIGNYY(I)*SIGNYY(I)
     .                    -SIGNXX(I)*SIGNYY(I)
     .                    +THREE*SIGNXY(I)*SIGNXY(I))
              R  = MIN(ONE,YLD(I)/MAX(EM20,SVM))
              SIGNXX(I)=SIGNXX(I)*R
              SIGNYY(I)=SIGNYY(I)*R
              SIGNXY(I)=SIGNXY(I)*R
              PLA(I) = PLA(I) + DPLA_I(I)
              IF (INLOC == 0) THEN
                DEZZ = DPLA_I(I) * HALF*(SIGNXX(I)+SIGNYY(I)) / YLD(I)
                DEZZ = -NU3*DEZZ
                THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
              ENDIF
              ETSE(I)= H(I)/(H(I)+E(I))
            END DO
          END IF
C=======================================================================
        ENDIF !  IPLAS
C=======================================================================
      ELSE    ! VP = 1  : plastic strain rate dependency
C=======================================================================
c
        SIGOXX(1:NEL) = SIGOXX(1:NEL) - SIGBXX(1:NEL)
        SIGOYY(1:NEL) = SIGOYY(1:NEL) - SIGBYY(1:NEL)
        SIGOXY(1:NEL) = SIGOXY(1:NEL) - SIGBXY(1:NEL)
C
        P0(1:NEL) = -(SIGOXX(1:NEL) + SIGOYY(1:NEL))*THIRD
        SIGNXX(1:NEL) = SIGOXX(1:NEL)+A1(1:NEL)*DEPSXX(1:NEL)+A2(1:NEL)*DEPSYY(1:NEL)
        SIGNYY(1:NEL) = SIGOYY(1:NEL)+A2(1:NEL)*DEPSXX(1:NEL)+A1(1:NEL)*DEPSYY(1:NEL)
        SIGNXY(1:NEL) = SIGOXY(1:NEL)+G(1:NEL) *DEPSXY(1:NEL)
        SIGNYZ(1:NEL) = SIGOYZ(1:NEL)+GS(1:NEL) *DEPSYZ(1:NEL)
        SIGNZX(1:NEL) = SIGOZX(1:NEL)+GS(1:NEL) *DEPSZX(1:NEL)
        SIGEXX(1:NEL) = SIGNXX(1:NEL)
        SIGEYY(1:NEL) = SIGNYY(1:NEL)
        SIGEXY(1:NEL) = SIGNXY(1:NEL)
c-----------------------------------------------
c       PRESSURE DEPENDENT YIELD FUNCTION FACTOR
c-----------------------------------------------
        IF (PFUN > 0) THEN
          PSCALE(1:NEL) = UPARAM(17 + 2*NRATE )*P0(1:NEL)
          DO I=1,NEL
            IPOSP(I) = VARTMP(I,2)
            IADP(I)  = NPF(IPFUN) / 2 + 1
            ILENP(I) = NPF(IPFUN) / 2 - IADP(I) - IPOSP(I)
          ENDDO
          CALL VINTER2(TF,IADP,IPOSP,ILENP,NEL,PSCALE,DFDP,PFAC)
          PFAC(1:NEL) = MAX(ZERO, PFAC(1:NEL))
          VARTMP(1:NEL,2) = IPOSP(1:NEL)
        ELSE
          PFAC(1:NEL) = ONE
        ENDIF
C-------------------
C       PLASTIC STRAIN RATE
C-------------------
        PLAP(1:NEL) = UVAR(1:NEL,2)
C-------------------
C       CRITERE
C-------------------
        JJ(1:NEL) = 1
        DO J=2,NRATE-1
          DO I=1,NEL
            IF (PLAP(I) >= UPARAM(6+J)) JJ(I) = J
          ENDDO
        ENDDO
#include  "vectorize.inc"
        DO I=1,NEL
          FAC = UPARAM(6+JJ(I))
          RATE(I) = (PLAP(I) - FAC)/(UPARAM(7+JJ(I)) - FAC)
          YFAC(I,1) = UPARAM(6+NRATE+JJ(I))  *FACYLDI(I)
          YFAC(I,2) = UPARAM(6+NRATE+JJ(I)+1)*FACYLDI(I)
        ENDDO

#include"vectorize.inc"
        DO I=1,NEL
          J1 = JJ(I)
          J2 = J1+1
          FUN1 = IFUNC(J1)
          FUN2 = IFUNC(J2)
          IPOS1(I) = VARTMP(I,2+J1)
          IPOS2(I) = VARTMP(I,2+J2)
          IAD1(I)  = NPF(FUN1) / 2 + 1
          ILEN1(I) = NPF(FUN1+1) / 2 - IAD1(I)-IPOS1(I)
          IAD2(I)  = NPF(FUN2) / 2 + 1
          ILEN2(I) = NPF(FUN2+1) / 2 - IAD2(I)-IPOS2(I)
        END DO
C
        CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
        CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
c-------------------------------
        IF (FISOKIN == ZERO) THEN
c-------------------------------
#include "vectorize.inc"
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            Y1(I)=Y1(I)*YFAC(I,1)
            Y2(I)=Y2(I)*YFAC(I,2)
            FAC   = RATE(I)
            YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
            YLD(I) = MAX(YLD(I),EM20)
            DYDX1(I)=DYDX1(I)*YFAC(I,1)
            DYDX2(I)=DYDX2(I)*YFAC(I,2)
            H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
            YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
            H(I)   = H(I)*MAX(ZERO,PFAC(I))
          ENDDO
          DO I=1,NEL
           J1 = JJ(I)
           J2 = J1+1
           VARTMP(I,2+J1) = IPOS1(I)
           VARTMP(I,2+J2) = IPOS2(I)
          ENDDO
c-------------------------------
        ELSEIF (FISOKIN == ONE) THEN  ! ecrouissage cinematique
c-------------------------------
#include "vectorize.inc"
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            FUN1 = IFUNC(J1)
            FUN2 = IFUNC(J2)
            FAC   = RATE(I)
            DYDX1(I)=DYDX1(I)*YFAC(I,1)
            DYDX2(I)=DYDX2(I)*YFAC(I,2)
            H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
            Y1(I)=TF(NPF(FUN1)+1)
            Y2(I)=TF(NPF(FUN2)+1)
            Y1(I)=Y1(I)*YFAC(I,1)
            Y2(I)=Y2(I)*YFAC(I,2)
            YLD(I) = FAIL(I)*(Y1(I) + FAC*(Y2(I)-Y1(I)))
            YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
            H(I)   = H(I)*MAX(ZERO,PFAC(I))
          ENDDO
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            VARTMP(I,2+J1) = IPOS1(I)
            VARTMP(I,2+J2) = IPOS2(I)
          ENDDO
c-------------------------------
        ELSE   ! ecrouissage mixte
c-------------------------------
#include "vectorize.inc"
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            FUN1 = IFUNC(J1)
            FUN2 = IFUNC(J2)
            Y1(I)=Y1(I)*YFAC(I,1)
            Y2(I)=Y2(I)*YFAC(I,2)
            FAC   = RATE(I)
            YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
            YLD(I) = MAX(YLD(I),EM20)
            DYDX1(I)=DYDX1(I)*YFAC(I,1)
            DYDX2(I)=DYDX2(I)*YFAC(I,2)
            H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
            Y1(I)=TF(NPF(FUN1)+1)
            Y2(I)=TF(NPF(FUN2)+1)
            Y1(I)=Y1(I)*YFAC(I,1)
            Y2(I)=Y2(I)*YFAC(I,2)
            YLD(I) = (ONE - FISOKIN) * YLD(I) +
     .           FISOKIN * (FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I))))
            YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
            H(I)   = H(I)*MAX(ZERO,PFAC(I))
          ENDDO
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            VARTMP(I,2+J1) = IPOS1(I)
            VARTMP(I,2+J2) = IPOS2(I)
          ENDDO
        ENDIF  ! ISOKIN
C-------------------
C       PROJECTION
C-------------------
C       CRITERE DE VON MISES
C-------------------------
        DO I=1,NEL
          H(I) = MAX(ZERO,H(I))
          S1=SIGNXX(I)+SIGNYY(I)
          S2=SIGNXX(I)-SIGNYY(I)
          S3=SIGNXY(I)
          AA(I)=FOURTH*S1*S1
          BB(I)=THREE_OVER_4*S2*S2+3.*S3*S3
          SVM2(I)= AA(I)+BB(I)
          IF (INLOC == 0) THEN
            DEZZ = -(DEPSXX(I)+DEPSYY(I))*NU_MNU
            THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
          ENDIF
        ENDDO
C-------------------------
C       GATHER PLASTIC FLOW
C-------------------------
        NINDX=0
        DO I=1,NEL
          IF(SVM2(I) > YLD(I)*YLD(I) .AND. OFF(I) == ONE) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ENDIF
        ENDDO
C
        IF (NINDX > 0) THEN
          !---------------------------
          !DEP EN CONTRAINTE PLANE
          !---------------------------
#include "vectorize.inc"
          DO J=1,NINDX
           I=INDEX(J)
           SVM = SQRT(SVM2(I))
           DPLA_J(I)=(SVM-YLD(I))/(G3(I)+H(I))
           ETSE(I)= H(I)/(H(I)+E(I))
           HI(I) = H(I)*(ONE-FISOKIN)
           HK(I) = TWO_THIRD*H(I)*FISOKIN
          ENDDO
C
          NU3 = ONE-NU_MNU
          DO N=1,NITER
#include "vectorize.inc"
            DO J=1,NINDX
              I=INDEX(J)
              DPLA_I(I)=DPLA_J(I)
              YLD_I =YLD(I)+HI(I)*DPLA_I(I)
              DR(I) =HALF*E(I)*DPLA_I(I)/YLD_I
              AAA = THREE*HK(I)/E(I)
              NU11  = U_MNU+AAA
              NU21  = T_PNU+AAA
              PP(I) = ONE/(ONE+DR(I)*NU11)
              QQ(I) = ONE/(ONE+DR(I)*NU21)
              P2    = PP(I)*PP(I)
              Q2    = QQ(I)*QQ(I)
              F     = AA(I)*P2+BB(I)*Q2-YLD_I*YLD_I
              DF    =-(AA(I)*NU11*P2*PP(I)+BB(I)*NU21*Q2*QQ(I))
     .          *(E(I)-TWO*DR(I)*HI(I))/YLD_I
     .          -TWO*HI(I)*YLD_I
              DF = SIGN(MAX(ABS(DF),EM20),DF)
              IF(DPLA_I(I) > ZERO) THEN
                DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
              ELSE
                DPLA_J(I)=ZERO
              ENDIF
            ENDDO ! NINDX
          ENDDO   ! NITER
C------------------------------------------
C         CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc"
          DO J=1,NINDX
             I=INDEX(J)
             PLA(I) = PLA(I) + DPLA_I(I)
             S1=(SIGNXX(I)+SIGNYY(I))*PP(I)
             S2=(SIGNXX(I)-SIGNYY(I))*QQ(I)
             SIGNXX(I)=HALF*(S1+S2)
             SIGNYY(I)=HALF*(S1-S2)
             SIGNXY(I)=SIGNXY(I)*QQ(I)
             IF (INLOC == 0) THEN
               DEZZ = - NU3*DR(I)*S1/E(I)
               THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
             ENDIF
             !-----for kin hardening	
             YLD(I) =YLD(I)+HI(I)*DPLA_I(I)
          ENDDO
        ENDIF
C=======================================================================
      ENDIF  ! VP flag
C=======================================================================
c----------------------------------------------------------------------
c     Element failure
c----------------------------------------------------------------------
      IF (IFAIL == 1) THEN
        DO I=1,NEL
          IF (OFF(I) == ONE .and. PLA(I) > EPSMAX) OFF(I) = FOUR_OVER_5
        ENDDO
      ELSEIF (IFAIL == 2) THEN
        DO I=1,NEL
          IF (OFF(I) == ONE .and. (PLA(I) > EPSMAX .or. EPST(I) > EPSF)) OFF(I) = FOUR_OVER_5
        ENDDO
      ENDIF
c----------------------------------------------------------------------
      IF (IMPL_S > 0) THEN
       IF (IKT > 0 ) THEN
       DO I = 1,NEL
        IF (DPLA_I(I) > ZERO) THEN
c ...... Parameter d(gama)
         GAMA_IMP(I)= THREE_HALF*DPLA_I(I)/YLD(I)
c ...... HK,HH---
         SIGNOR(I,4)=FISOKIN*H(I)
         SIGNOR(I,5)=(ONE-FISOKIN)*H(I)
c ...... Deviatoric stresses shifted by midified back stress ->ksi
         SIGNOR(I,1)=THIRD*(TWO*SIGNXX(I)-SIGNYY(I))
         SIGNOR(I,2)=THIRD*(TWO*SIGNYY(I)-SIGNXX(I))
         SIGNOR(I,3)=TWO*SIGNXY(I)
        ELSE
         GAMA_IMP(I) = ZERO
        END IF
       END DO
       END IF !(IKT > 0) THEN
      END IF
C------------------------------------------
C     PLASTIC STRAIN FILTERING
C------------------------------------------
      IF (VP == 1) THEN
         DTINV = ONE/MAX(TIMESTEP,EM20)
         DO I=1,NEL
            PLAP(I)   = ASRATE*DPLA_I(I)*DTINV + (ONE - ASRATE)*UVAR(I,2)
            UVAR(I,2) = PLAP(I)
          ENDDO
      ENDIF
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IF (FISOKIN > ZERO) THEN
        DO I=1,NEL
	         HKIN  = FISOKIN*H(I)
          ALPHA = HKIN*DPLA_I(I)/YLD(I)
          SIGPXX = ALPHA*SIGNXX(I)
          SIGPYY = ALPHA*SIGNYY(I)
          SIGPXY = ALPHA*SIGNXY(I)

          SIGBXX(I) = SIGBXX(I) + SIGPXX
          SIGBYY(I) = SIGBYY(I) + SIGPYY
          SIGBXY(I) = SIGBXY(I) + SIGPXY
C
          SIGNXX(I) = SIGNXX(I) + SIGBXX(I)
          SIGNYY(I) = SIGNYY(I) + SIGBYY(I)
          SIGNXY(I) = SIGNXY(I) + SIGBXY(I)
        ENDDO
      ENDIF
C--------------------------------
C     HARDENING MODULUS
C--------------------------------
      DO I=1,NEL
        HARDM(I) = H(I)
      ENDDO
C--------------------------------
C     NON-LOCAL THICKNESS VARIATION
C--------------------------------
      IF (INLOC > 0) THEN
        DO I = 1,NEL
          SVM    = SQRT(SIGNXX(I)*SIGNXX(I)
     .           + SIGNYY(I)*SIGNYY(I)
     .           - SIGNXX(I)*SIGNYY(I)
     .           + THREE*SIGNXY(I)*SIGNXY(I))
          DEZZ   = MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(SVM,EM20)
          DEZZ   = -NUX*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E(I)) - DEZZ
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
        ENDDO
      ENDIF
c-----------
      RETURN
      END
