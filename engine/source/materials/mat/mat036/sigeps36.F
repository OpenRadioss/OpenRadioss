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
Chd|  SIGEPS36                      source/materials/mat/mat036/sigeps36.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        M36ITER_IMP                   source/materials/mat/mat036/m36iter_imp.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS36(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     6     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPT    ,
     9     IPM    ,MAT    ,EPSP    ,IPLA    ,YLD     ,PLA    ,
     A     DPLA1  ,ETSE   ,AL_IMP  ,SIGNOR  ,AMU,
     B     FACYLDI,NVARTMP,VARTMP  ,SIGB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
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
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C=======================================================================
      INTEGER NEL, NUPARAM, NUVAR,IPT,IPLA,NVARTMP,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,UPARAM(*),SIGB(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSXX(NEL),EPSYY(NEL),EPSZZ(NEL),
     .   EPSXY(NEL),EPSYZ(NEL),EPSZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   EPSP(NEL),ETSE(NEL),AMU(NEL),FACYLDI(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),DPLA1(NEL),
     .    AL_IMP(NEL),SIGNOR(MVSIZ,6)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL),  YLD(NEL),
     .        PLA(NEL)
      INTEGER, INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real :: TF(*),FINTER
      EXTERNAL FINTER
C-----------------------------------------------
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
      INTEGER I,J,K,J1,J2,NFUNC,VP,II,NITER,IFAIL,
     .        NRATE,PFUN,IPFUN,IPFLAG,YLDCHECK,
     .        OPTE,IFUNCE,NINDX,
     .        ILEN1(MVSIZ),IAD2(MVSIZ),ILEN2(MVSIZ),INDEX(MVSIZ),
     .        IFUNC(100),IAD1(MVSIZ), IADP(MVSIZ),ILENP(MVSIZ),
     .        INDX(MVSIZ),JJ(MVSIZ), IADP2(MVSIZ),ILENP2(MVSIZ)
      INTEGER, DIMENSION(MVSIZ) :: IPOS1,IPOS2,IPOSPE,IPOSP,IPOSP2
      my_real
     .        E11,NU,DAV,VM,R(MVSIZ),FAC,EPST,P,
     .        E1,E2,E3,E4,E5,E6,C,CC,D,Y,YP,E42,E52,E62,EPST2,
     .        C11,G11,G21,G31,EPSR1,DPLA,EPSF,HKIN,FISOKIN,
     .        DSXX,DSYY,DSZZ,DSXY,DSYZ,
     .        DSZX,SIGPXX,SIGPYY,SIGPXY,SIGPYZ,SIGPZX,SIGPZZ,ALPHA,
     .        EPSR1DAV,EPSR1F,VM_1,G3H,NORM_1,EPSMAX,SSP,
     .        EINF,CE1,EPSR2,SVM,DF,F,DFSR,COEF
      my_real :: FACT(NEL),
     .        C1(MVSIZ),G(MVSIZ),G2(MVSIZ),G3(MVSIZ),
     .        Y1(MVSIZ),Y2(MVSIZ),H(MVSIZ),DYDX1(MVSIZ),
     .        DYDX2(MVSIZ),FAIL(MVSIZ),E(MVSIZ),
     .        P0(MVSIZ),PFAC(MVSIZ),PSCALE(MVSIZ),
     .        PSCALE1(MVSIZ),PLA0(MVSIZ), PLAM(MVSIZ),
     .        DFDP(MVSIZ),EPSTT(MVSIZ),
     .        SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEZZ(MVSIZ),
     .        SIGEXY(MVSIZ),SIGEYZ(MVSIZ),SIGEZX(MVSIZ),
     .        DYDXE(MVSIZ),PLAOLD(MVSIZ),
     .        RATE(MVSIZ,2),YFAC(MVSIZ,2)
      my_real
     .       PLAP(MVSIZ),SVM2(MVSIZ),DPLA_J(MVSIZ),DPLA_I(MVSIZ),
     .       SRATE(MVSIZ),HI(MVSIZ),ESCALE12(MVSIZ),
     .       SVM_TAB(MVSIZ)
      INTEGER :: NINDEX_PLA,NINDEX_VINTER
      INTEGER, DIMENSION(MVSIZ) :: INDEX_PLA,INDEX_VINTER
      my_real, DIMENSION(MVSIZ) :: TAB_LOC,Y1_LOC,DYDX1_LOC, Y2_LOC,DYDX2_LOC ,VECNUL 
      my_real, DIMENSION(:) ,POINTER  :: SIGBXX,SIGBYY,SIGBZZ,SIGBXY,SIGBYZ,SIGBZX
      TARGET  SIGB, VECNUL      
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      NITER = 5
      NINDEX_PLA = 0                         
      NFUNC  = IPM(10,MAT(1))
      DO J=1,NFUNC
        IFUNC(J)= IPM(10+J,MAT(1))
      ENDDO
      IPFUN = IFUNC(NFUNC-1)
C         
      E11      = UPARAM(2)
      G11      = UPARAM(5)
      NU       = UPARAM(6)
      NRATE    = NINT(UPARAM(1))
      EPSMAX   = UPARAM(6+2*NRATE+1)      
      EPSR1    = UPARAM(6+2*NRATE+2)
      EPSR2    = UPARAM(6+2*NRATE+3)
      G21      = UPARAM(6+2*NRATE+4)
      G31      = UPARAM(6+2*NRATE+5)
      C11      = UPARAM(6+2*NRATE+6) ! ..bulk modulus
      SSP      = UPARAM(6+2*NRATE+7)
      FISOKIN  = UPARAM(6+2*NRATE+8)
      EPSF     = UPARAM(6+2*NRATE+9)
      PFUN     = NINT(UPARAM(16+2*NRATE))
      OPTE     = UPARAM(2*NRATE+23)
      EINF     = UPARAM(2*NRATE+24)
      CE1      = UPARAM(2*NRATE+25)        
      VP       = NINT(UPARAM(2*NRATE + 26))  ! flag for plastic strain dependency
      IFAIL    = NINT(UPARAM(2*NRATE + 27))  ! flag rupture 
      YLDCHECK = NINT(UPARAM(2*NRATE + 28))
C
#include "vectorize.inc"
      DO I=1,NEL
        PFAC(I)   = ONE
        PSCALE(I) = UPARAM(17+2*NRATE)
        E(I)  = E11   
        G(I)  = G11 
        G2(I) = G21                                  
        G3(I) = G31 
        C1(I) = C11            
        SOUNDSP(I) = SSP
      ENDDO            
c-----------------------------
      IF (FISOKIN > 0) THEN
          SIGBXX => SIGB(1      :  NEL) 
          SIGBYY => SIGB(NEL+1  :2*NEL)
          SIGBZZ => SIGB(2*NEL+1:3*NEL)
          SIGBXY => SIGB(3*NEL+1:4*NEL)
          SIGBYZ => SIGB(4*NEL+1:5*NEL)
          SIGBZX => SIGB(5*NEL+1:6*NEL)
      ELSE IF (IMPL_S > 0) THEN
          VECNUL(1:NEL) = ZERO
          SIGBXX => VECNUL(1:NEL)
          SIGBYY => VECNUL(1:NEL)
          SIGBZZ => VECNUL(1:NEL)
          SIGBXY => VECNUL(1:NEL)
          SIGBYZ => VECNUL(1:NEL)
          SIGBZX => VECNUL(1:NEL)     
      ENDIF
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IF (FISOKIN > ZERO) THEN
c        ..subtracts back stresses for kinmatic or mixed hardening
c          from the old stresses, thus, SIGOxx is a shifted old stress
#include "vectorize.inc"
        DO I=1,NEL
            SIGOXX(I) = SIGOXX(I) - SIGBXX(I)
            SIGOYY(I) = SIGOYY(I) - SIGBYY(I)
            SIGOZZ(I) = SIGOZZ(I) - SIGBZZ(I)
            SIGOXY(I) = SIGOXY(I) - SIGBXY(I)
            SIGOYZ(I) = SIGOYZ(I) - SIGBYZ(I)
            SIGOZX(I) = SIGOZX(I) - SIGBZX(I)       
        ENDDO            
      ENDIF 
c------------------------------------------
c     element lenght scale function
c------------------------------------------
      IF (OPTE == 1) THEN
        IFUNCE = UPARAM( 2*NRATE + 22)
        DO I=1,NEL
          IF(PLA(I) > ZERO)THEN
            NINDEX_PLA = NINDEX_PLA + 1
            INDEX_PLA(NINDEX_PLA) = I
            IPOSPE(NINDEX_PLA) = VARTMP(I,1)
            IADP(NINDEX_PLA)   = NPF(KFUNC(IFUNCE)) / 2 + 1
            ILENP(NINDEX_PLA)  = NPF(KFUNC(IFUNCE)+1) / 2 - IADP(NINDEX_PLA) - IPOSPE(NINDEX_PLA)
            TAB_LOC(NINDEX_PLA) = PLA(I)
          ENDIF
        ENDDO
         CALL VINTER2(TF,IADP,IPOSPE,ILENP,NINDEX_PLA,TAB_LOC,DYDXE,ESCALE12)
         VARTMP(INDEX_PLA(1:NINDEX_PLA),1) = IPOSPE(1:NINDEX_PLA)
#include "vectorize.inc"
         DO II=1,NINDEX_PLA 
           I = INDEX_PLA(II)                
           E(I) =  ESCALE12(II)* E(I)   
           G(I) =  HALF*E(I)/(ONE+NU) 
           G2(I) = TWO*G(I)                                  
           G3(I) = THREE*G(I) 
           C1(I) = E(I)/THREE/(ONE - TWO*NU)            
           SOUNDSP(I) = SQRT((C1(I) + FOUR_OVER_3*G(I))/RHO0(I))                 
        ENDDO   
      ELSEIF ( CE1 /= ZERO) THEN 
#include "vectorize.inc"
        DO I=1,NEL                                           
          IF(PLA(I) > ZERO)THEN                                                        
            E(I) = E11-(E11-EINF)*(ONE-EXP(-CE1*PLA(I)))              
            G(I) =  HALF*E(I)/(ONE+NU)   
            G2(I) = TWO*G(I)                                  
            G3(I) = THREE*G(I)                                               
            C1(I) =E(I)/THREE/(ONE - TWO*NU)               
            SOUNDSP(I) = SQRT((C1(I) + FOUR_OVER_3*G(I))/RHO0(I))
          ENDIF    
        ENDDO         
      ENDIF
c------------------------------------------
c     PRESSURE DEPENDENT YIELD FUNCTION FACTOR
c------------------------------------------
      IF (PFUN > 0) THEN
        DO I=1,NEL
          IADP(I)  = NPF(IPFUN) / 2 + 1
          ILENP(I) = NPF(IPFUN  + 1) / 2 - IADP(I) - VARTMP(I,2)
        ENDDO
        CALL VINTER2(TF,IADP,VARTMP(1:NEL,2),ILENP,NEL,PSCALE,DFDP,PFAC)
        PFAC(1:NEL) = MAX(ZERO, PFAC(1:NEL))
      ENDIF 
c
c     ..pressure + delta_pressure (scaled) .. 
      IF (IMPL_S > 0) PSCALE1(1:NEL)=PSCALE(1:NEL) * ( C1(1:NEL) * AMU(1:NEL) )
c
c------------------------------------------
c     PRESSURE DEPENDENT YIELD FUNCTION FACTOR
c------------------------------------------
#include "vectorize.inc"
      DO I=1,NEL
C
c     ..strain increment axiator ..
        DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
c     ..pressure..        
        P0(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
C
c     ..pressure (scaled, typically the scale factor = 1)
        PSCALE(I) = PSCALE(I)*P0(I)
c        
c     ..deviatoric elastic predictor ..
        SIGNXX(I)=SIGOXX(I)+P0(I)+G2(I)*(DEPSXX(I)-DAV)
        SIGNYY(I)=SIGOYY(I)+P0(I)+G2(I)*(DEPSYY(I)-DAV)
        SIGNZZ(I)=SIGOZZ(I)+P0(I)+G2(I)*(DEPSZZ(I)-DAV)
      ENDDO
C
      SIGNXY(1:NEL)=SIGOXY(1:NEL)+G(1:NEL) *DEPSXY(1:NEL)
      SIGNYZ(1:NEL)=SIGOYZ(1:NEL)+G(1:NEL) *DEPSYZ(1:NEL)
      SIGNZX(1:NEL)=SIGOZX(1:NEL)+G(1:NEL) *DEPSZX(1:NEL)
      SIGEXX(1:NEL) = SIGNXX(1:NEL)
      SIGEYY(1:NEL) = SIGNYY(1:NEL)
      SIGEZZ(1:NEL) = SIGNZZ(1:NEL)               
      SIGEXY(1:NEL) = SIGNXY(1:NEL)
      SIGEYZ(1:NEL) = SIGNYZ(1:NEL)
      SIGEZX(1:NEL) = SIGNZX(1:NEL)
      VISCMAX(1:NEL) = ZERO
      DPLA1(1:NEL) =ZERO
      EPSTT(1:NEL) = ZERO
      FAIL(1:NEL)  = ONE
C-------------------
C     STRAIN principal 1, 4 newton iterations (only when rupture)
C-------------------
      IF (IFAIL > 1) THEN
        EPSR1F = MIN(EPSR1,EPSF)
        DO I=1,NEL
c          IF(EPSR1F == EP30)CYCLE
          DAV = (EPSXX(I)+EPSYY(I)+EPSZZ(I)) * THIRD
          E1 = EPSXX(I) - DAV
          E2 = EPSYY(I) - DAV
          E3 = EPSZZ(I) - DAV
          E4 = HALF*EPSXY(I)
          E5 = HALF*EPSYZ(I)
          E6 = HALF*EPSZX(I)
C          -y = (e1-x)(e2-x)(e3-x)
C             - e5^2(e1-x) - e6^2(e2-x) - e4^2(e3-x)
C             + 2e4 e5 e6
C           e1 + e2 + e3 = 0 => terme en x^2 = 0
C           y = x^3 + c x + d
c           yp= 3 x^2 + c
          E42 = E4*E4  
          E52 = E5*E5
          E62 = E6*E6
          C = - HALF *(E1*E1 + E2*E2 + E3*E3) - E42 - E52 - E62
          EPST = SQRT(-C*THIRD)
c         2*EPST est un majorant de la solution
          EPSR1DAV = EPSR1F - DAV
          IF(EPST+EPST < EPSR1DAV)CYCLE
          D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42
     &        - TWO*E4*E5*E6
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          IF(ABS(Y) > EM8)THEN
            EPST = ONEP75 * EPST
            EPST2 = EPST * EPST
            Y = (EPST2 + C)* EPST + D
            YP = THREE*EPST2 + C
            EPST = EPST - Y/YP
c           EPST est un majorant de la solution
            IF(EPST < EPSR1DAV)CYCLE
            EPST2 = EPST * EPST
            Y = (EPST2 + C)* EPST + D
            YP = THREE*EPST2 + C
            EPST = EPST - Y/YP
            IF(EPST < EPSR1DAV)CYCLE
            EPST2 = EPST * EPST
            Y = (EPST2 + C)* EPST + D
            YP = THREE*EPST2 + C
            EPST = EPST - Y/YP
            IF(EPST < EPSR1DAV)CYCLE
            EPST2 = EPST * EPST
            Y = (EPST2 + C)* EPST + D
            YP = THREE*EPST2 + C
            EPST = EPST - Y/YP
          ENDIF
          EPST = EPST + DAV
          EPSTT(I)= EPST
C-------------------
C     tension failure 
C-------------------
          FAIL(I) = MAX(EM20, MIN(ONE, (EPSR2-EPST)/(EPSR2-EPSR1) ))
        ENDDO
      ENDIF
C=======================================================================
c     Split code in 2 independent part :
c              VP = 1 dependent on plastic strain rate 
c        VP = 0 dependent on total strain rate 
C=======================================================================
      IF (VP == 0) THEN
C=======================================================================
        IF (NRATE == 1) THEN   ! only static curve => no strain rate interpolation
          IAD1(1:NEL)  = NPF(IFUNC(1))   / 2 + 1
          ILEN1(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD1(1:NEL) - VARTMP(1:NEL,3)
c
          CALL VINTER(TF,IAD1,VARTMP(1:NEL,3),ILEN1,NEL,PLA,DYDX1,Y1)
c
          YFAC(1:NEL,1)   = UPARAM(6+NRATE+1)*FACYLDI(1:NEL)
          FACT(1:NEL) = FAIL(1:NEL) * PFAC(1:NEL) * YFAC(1:NEL,1)
          H(1:NEL)    = DYDX1(1:NEL) * FACT(1:NEL)
c
          IF (FISOKIN == ZERO) THEN
            YLD(1:NEL) = Y1(1:NEL) * FACT(1:NEL)
          ELSE IF (FISOKIN == ONE) THEN
            YLD(1:NEL) =  TF(NPF(IFUNC(1))+1) * FACT(1:NEL)  
          ELSE IF (FISOKIN > ZERO) THEN
            YLD(1:NEL) = ((ONE-FISOKIN)*Y1(1:NEL) + FISOKIN *TF(NPF(IFUNC(1))+1))*FACT(1:NEL) 
          END IF
c-----------        
        ELSE  ! strain rate interpolation
c-----------
          JJ(1:NEL) = 1
          DO J=2,NRATE-1
#include "vectorize.inc"
            DO I=1,NEL
              IF (EPSP(I) >= UPARAM(6+J)) JJ(I) = J
            ENDDO
          ENDDO
C
#include "vectorize.inc"
          DO I=1,NEL
            RATE(I,1)=UPARAM(6+JJ(I))
            RATE(I,2)=UPARAM(6+JJ(I)+1)
            YFAC(I,1)=UPARAM(6+NRATE+JJ(I))*FACYLDI(I)
            YFAC(I,2)=UPARAM(6+NRATE+JJ(I)+1)*FACYLDI(I)
          ENDDO
C
#include "vectorize.inc"
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            IPOS1(I) = VARTMP(I,J1+2)
            IPOS2(I) = VARTMP(I,J2+2)
          ENDDO
          IAD1(1:NEL)  = NPF(IFUNC(JJ(1:NEL))) / 2 + 1
          ILEN1(1:NEL) = NPF(IFUNC(JJ(1:NEL))+1) / 2 - IAD1(1:NEL) - IPOS1(1:NEL)
          IAD2(1:NEL)  = NPF(IFUNC(JJ(1:NEL)+1)) / 2 + 1
          ILEN2(1:NEL) = NPF(IFUNC(JJ(1:NEL)+1)+1) / 2 - IAD2(1:NEL) - IPOS2(1:NEL)
C
          CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
          CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
c
          DO I=1,NEL
            J1 = JJ(I)
            J2 = J1+1
            VARTMP(I,J1+2) = IPOS1(I)
            VARTMP(I,J2+2) = IPOS2(I)
          ENDDO
C
          IF (FISOKIN == ZERO) THEN   ! ecrouissage isotrope
#include "vectorize.inc"
            DO I=1,NEL
              J1 = JJ(I)
              J2 = J1+1
              Y1(I)=Y1(I)*YFAC(I,1)
              Y2(I)=Y2(I)*YFAC(I,2)
              FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
              CC = FAIL(I)* PFAC(I)
              YLD(I)  = (Y1(I)    + FAC*(Y2(I)-Y1(I))) * CC
              DYDX1(I)=DYDX1(I)*YFAC(I,1)
              DYDX2(I)=DYDX2(I)*YFAC(I,2)
              H(I) = (DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I))) * CC
            ENDDO
c
          ELSEIF (FISOKIN == ONE) THEN   ! ecrouissage cinematique
c
#include "vectorize.inc"
            DO I=1,NEL
              J1 = JJ(I)
              J2 = J1+1        
              FAC = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
              CC  = FAIL(I) * PFAC(I)
              DYDX1(I)=DYDX1(I)*YFAC(I,1)
              DYDX2(I)=DYDX2(I)*YFAC(I,2) 
              H(I) = CC*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
              Y1(I)= TF(NPF(IFUNC(J1))+1)
              Y2(I)= TF(NPF(IFUNC(J2))+1)
              Y1(I)= Y1(I)*YFAC(I,1)
              Y2(I)= Y2(I)*YFAC(I,2)
              YLD(I) = CC*(Y1(I)    + FAC*(Y2(I)-Y1(I)))            
            ENDDO
c
          ELSE      ! 0 < FISOKIN < 1  ecrouissage mixte
c
#include "vectorize.inc"
            DO I=1,NEL
              J1 = JJ(I)
              J2 = J1+1
              Y1(I) = Y1(I)*YFAC(I,1)
              Y2(I) = Y2(I)*YFAC(I,2)
              FAC     = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
              YLD(I)  = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
              YLD(I)  = MAX(YLD(I),EM20)
              DYDX1(I)= DYDX1(I)*YFAC(I,1)
              DYDX2(I)= DYDX2(I)*YFAC(I,2)
              H(I)    = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
              H(I)    = H(I) * PFAC(I)

              Y1(I)=TF(NPF(IFUNC(J1))+1)
              Y2(I)=TF(NPF(IFUNC(J2))+1)
              Y1(I)=Y1(I)*YFAC(I,1)
              Y2(I)=Y2(I)*YFAC(I,2)   
              YLD(I) = (ONE - FISOKIN) * YLD(I)
     .               + FISOKIN * (FAIL(I)*(Y1(I)  + FAC*(Y2(I)-Y1(I))))      
              YLD(I) = YLD(I) * PFAC(I)
            ENDDO
          ENDIF     ! FISOKIN
        END IF      ! NRATE > 0
c
        IF (YLDCHECK == 1) THEN
          DO I=1,NEL
            YLD(I) = MAX(YLD(I), EM20)
          END DO
        END IF
C-------------------
C       PROJECTION
C-------------------
        IF (IPLA == 0) THEN
          DO I=1,NEL
            VM =THREE*(HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .           + SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2)
            IF(VM > YLD(I)*YLD(I))THEN
              VM = SQRT(VM)
              R(I) = YLD(I)/ MAX(VM,EM20)
              SIGNXX(I)=SIGNXX(I)*R(I)
              SIGNYY(I)=SIGNYY(I)*R(I)
              SIGNZZ(I)=SIGNZZ(I)*R(I)
              SIGNXY(I)=SIGNXY(I)*R(I)
              SIGNYZ(I)=SIGNYZ(I)*R(I)
              SIGNZX(I)=SIGNZX(I)*R(I)
              PLA(I) = PLA(I) + (ONE - R(I))*VM/MAX(G3(I)+H(I),EM20)
              DPLA1(I) = (ONE - R(I))*VM/MAX(G3(I)+H(I),EM20)         
            ENDIF
          ENDDO
c
        ELSEIF(IPLA == 2)THEN
c
         DO I=1,NEL
           VM = THREE*(HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .        + SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2)
           IF (VM > YLD(I)*YLD(I)) THEN
             VM = SQRT(VM)
             R(I) = YLD(I)/ MAX(VM,EM20)
             SIGNXX(I)=SIGNXX(I)*R(I)
             SIGNYY(I)=SIGNYY(I)*R(I)
             SIGNZZ(I)=SIGNZZ(I)*R(I)
             SIGNXY(I)=SIGNXY(I)*R(I)
             SIGNYZ(I)=SIGNYZ(I)*R(I)
             SIGNZX(I)=SIGNZX(I)*R(I)
             PLA(I)=PLA(I) + (ONE-R(I))*VM/MAX(G3(I),EM20)
             DPLA1(I) =  (ONE-R(I))*VM/MAX(G3(I),EM20)     
           ENDIF 
         ENDDO
c
        ELSEIF(IPLA == 1)THEN
c
          IF (IMPL_S == 0) THEN
            DO I=1,NEL
              VM = THREE*(HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .           + SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2)
              IF (VM > YLD(I)*YLD(I)) THEN
                VM = SQRT(VM)
                R(I) = YLD(I)/ MAX(VM,EM20)
C               plastic strain increment.
                DPLA =(ONE - R(I)) * VM/MAX(G3(I)+H(I),EM20)
C               updated yield stress.
                YLD(I) = MAX(YLD(I)+(ONE - FISOKIN)*DPLA*H(I),ZERO)
                R(I)   = MIN(ONE,YLD(I) / MAX(VM,EM20))
C
c     .     .   here the updated stresses are shifted stresses
c               (for kinematic or mixed hardening)
                SIGNXX(I)=SIGNXX(I)*R(I)
                SIGNYY(I)=SIGNYY(I)*R(I)
                SIGNZZ(I)=SIGNZZ(I)*R(I)
                SIGNXY(I)=SIGNXY(I)*R(I)
                SIGNYZ(I)=SIGNYZ(I)*R(I)
                SIGNZX(I)=SIGNZX(I)*R(I)
                PLA(I)=PLA(I) + DPLA     
                DPLA1(I) = DPLA      
              ENDIF  
            ENDDO
C
          ELSE ! -- IMPL_S > 0
C
c ---       nonlinear hardening requires iterations in radial return --
            IPFLAG = 0
            JJ(1:NEL) = 1
            IF (PFUN > 0) IPFLAG = 1
              DO I=1,NEL
C               inside M36ITER_IMP, IPOS1=0        
                J1 = JJ(I)
                IAD1(I)  = HALF*NPF(IFUNC(J1)) + 1
                ILEN1(I) = HALF*NPF(IFUNC(J1)+1) - IAD1(I) 
              ENDDO
              CALL M36ITER_IMP( SIGNXX,  SIGNYY,  SIGNZZ,
     .                          SIGNXY,  SIGNYZ,  SIGNZX,
     .                          PLA,        YLD,      G3,                   
     .                          YFAC,     DPLA1,       H,   
     .                          TF,        IAD1, 
     .                          ILEN1,      NEL,
     .                          FISOKIN,  VARTMP,PLA0, 
     .                          PLAM,Y1,  DYDX1,
     .                          IPFLAG, PFUN, IPFUN, IPOSP, 
     .                          NRATE,  NPF,  IADP,  ILENP,
     .                          PFAC, PSCALE1, DFDP, FAIL,
     .                          NVARTMP ,
     .                          SIGBXX,SIGBYY,SIGBZZ,SIGBXY,SIGBYZ,SIGBZX)
C
          ENDIF ! -- IMPL_S<=0
c
        ENDIF ! -- IPLA --      
C=======================================================================
C=======================================================================
      ELSE  ! VP=1
C=======================================================================
C=======================================================================
        IF (ISIGI == 0) THEN
          IF(TIME == ZERO)THEN
           DO I=1,NEL
                YFAC(I,1) = UPARAM(6+NRATE+1)*FACYLDI(I)
                YLD(I) = YFAC(I,1)*FINTER(IFUNC(1)  ,ZERO,NPF,TF,DYDX1(I)) 
            H(I)   = DYDX1(I) 
            YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
            H(I)   = H(I)  *MAX(ZERO,PFAC(I))          
            UVAR(I,3) = YLD(I)     
           ENDDO
          ENDIF
        ENDIF
C
        DO I=1,NEL
         PLAOLD(I) = PLA(I)                                          
         PLAP(I) = UVAR(I,2)                              
         YLD(I)  = UVAR(I,3)    
         JJ(I) = 1                                                 
        ENDDO
C-------------------------
C       CRITERE DE VON MISES
C-------------------------
        DO I=1,NEL
          SVM2(I) = THREE*(HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .            + SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2)
        ENDDO
        NINDX=0
        DO I=1,NEL         
          IF(SVM2(I)>YLD(I)*YLD(I).AND.OFF(I)== ONE) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ENDIF
        ENDDO
C=======================================================================
c       PLASTICITY
C=======================================================================
        IF (NINDX > 0 ) THEN
c
          IF (FISOKIN == ZERO) THEN  !===No kinematic hardening        
            !------------------- 
            !Plastic increment : 
            !-------------------
#include "vectorize.inc" 
            DO II=1,NINDX                                             
                  I = INDEX(II) 
                  SVM_TAB(I)       = SQRT(SVM2(I))  
                  DPLA_J(I) =  UVAR(I,1) + EM09 
                  !-------------- 
                  !update yield :
                  !-------------- 
                  JJ(I) = 1                                                                                            
                  !Y1 and Y2 for Pl SR dependency
            ENDDO
            DO J=2,NRATE-1 
              DO II=1,NINDX                                             
                I = INDEX(II)                                       
                IF (PLAP(I) >= UPARAM(6+J)) JJ(I) = J               
              ENDDO
            ENDDO  
#include "vectorize.inc" 
            DO II=1,NINDX                                             
                  I = INDEX(II) 
                FAC=UPARAM(6+JJ(I))
                SRATE(I)=(PLAP(I) - FAC)/(UPARAM(7+JJ(I)) - FAC)
                YFAC(I,1)=UPARAM(6+NRATE+JJ(I))*FACYLDI(I)
                YFAC(I,2)=UPARAM(6+NRATE+JJ(I)+1)*FACYLDI(I)
            ENDDO
! ------------------------
            NINDEX_VINTER = 0
#include "vectorize.inc"
            DO II=1,NINDX                                             
              I = INDEX(II)
              NINDEX_VINTER = NINDEX_VINTER + 1
              INDEX_VINTER(NINDEX_VINTER) = I 
              J1=JJ(I)
              J2=JJ(I)+1
              IPOSP(NINDEX_VINTER) = VARTMP(I,J1+2)
              IADP(NINDEX_VINTER)  = NPF(IFUNC(JJ(I))     ) / 2 + 1
              ILENP(NINDEX_VINTER) = NPF(IFUNC(JJ(I))  + 1) / 2 - IADP(NINDEX_VINTER) - IPOSP(NINDEX_VINTER)
              IPOSP2(NINDEX_VINTER) = VARTMP(I,J2+2)
              IADP2(NINDEX_VINTER)  = NPF(IFUNC(JJ(I)+1)) / 2 + 1
              ILENP2(NINDEX_VINTER) = NPF(IFUNC(JJ(I)+1)  + 1) / 2 - IADP2(NINDEX_VINTER) - IPOSP2(NINDEX_VINTER)
              TAB_LOC(NINDEX_VINTER) = PLA(I)
            ENDDO

            CALL VINTER2(TF,IADP,IPOSP,ILENP,NINDEX_VINTER,TAB_LOC,DYDX1_LOC,Y1_LOC)
            CALL VINTER2(TF,IADP2,IPOSP2,ILENP2,NINDEX_VINTER,TAB_LOC,DYDX2_LOC,Y2_LOC)

            DO II=1,NINDEX_VINTER  
              I = INDEX_VINTER(II)       
              J1 = JJ(I)
              J2 = J1+1                                 
              VARTMP(I,J1+2) = IPOSP(II)
              VARTMP(I,J2+2) = IPOSP2(II)
            ENDDO
#include "vectorize.inc"
            DO II=1,NINDEX_VINTER  
                  I = INDEX_VINTER(II)
                  Y1(I) = Y1_LOC(II)
                  Y2(I) = Y2_LOC(II)
                  DYDX1(I)=DYDX1_LOC(II)
                  DYDX2(I)=DYDX2_LOC(II)
            ENDDO
! ------------------------
#include "vectorize.inc" 
            DO II=1,NINDX                                             
                I = INDEX(II)
                Y1(I) = Y1(I) * YFAC(I,1)
                Y2(I) = Y2(I) * YFAC(I,2)        
                FAC     = SRATE(I)        
                YLD(I)  = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
                YLD(I)  = MAX(YLD(I),EM20)
                DYDX1(I)=DYDX1(I)*YFAC(I,1)
                DYDX2(I)=DYDX2(I)*YFAC(I,2)
                H(I)    = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
                YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
                H(I)   = H(I)  *MAX(ZERO,PFAC(I))          
                DFSR   = H(I)  + MAX(ZERO,PFAC(I))*FAIL(I)*(Y2(I)-Y1(I))/
     .         (UPARAM(7+JJ(I))-UPARAM(6+JJ(I))) /TIMESTEP
            ENDDO
c------------------------
            DO K=1,NITER 
              !update svm
#include "vectorize.inc" 
              DO II=1,NINDX                                                
                    I = INDEX(II)                                        
                    DPLA_I(I) = DPLA_J(I)                                
                    PLA(I)    = PLAOLD(I) + DPLA_I(I)                    
                    PLAP(I)   = DPLA_I(I) / TIMESTEP                     
                    R(I)      = YLD(I)/(YLD(I) +G3(I)*DPLA_I(I))         
                    SVM       = R(I)* SVM_TAB(I)                         

                    F = SVM -  YLD(I) - G3(I) *DPLA_I(I)                 
                    DF = - G3(I) -DFSR                                   
                    DF = SIGN(MAX(ABS(DF),EM20),DF)                      
                    IF(DPLA_I(I) > ZERO) THEN                            
                      DPLA_J(I)=MAX( EM10 ,DPLA_I(I)-F/DF)               
                    ELSE                                                 
                      DPLA_J(I)=EM10                                     
                    ENDIF                                                
                    !--------------                                      
                    !update yield :                                      
                    !--------------                                      
                    PLA(I)    = PLAOLD(I) + DPLA_J(I)                    
                    PLAP(I)   = DPLA_J(I) / TIMESTEP                     
                    JJ(I) = 1                                            
              ENDDO                                                        
              DO J=2,NRATE-1
                DO II=1,NINDX                                             
                  I = INDEX(II)
                  IF(PLAP(I) >= UPARAM(6+J)) JJ(I) = J       
                ENDDO       
              ENDDO
#include "vectorize.inc" 
              DO II=1,NINDX                                             
                I = INDEX(II)  
                FAC=UPARAM(6+JJ(I))
                SRATE(I)=(PLAP(I) - FAC)/(UPARAM(7+JJ(I)) - FAC)
                YFAC(I,1)=UPARAM(6+NRATE+JJ(I))*FACYLDI(I)
                YFAC(I,2)=UPARAM(6+NRATE+JJ(I)+1)*FACYLDI(I)
              ENDDO
! ------------------------
              NINDEX_VINTER = 0
#include "vectorize.inc"
              DO II=1,NINDX                                             
                I = INDEX(II)
                NINDEX_VINTER = NINDEX_VINTER + 1
                INDEX_VINTER(NINDEX_VINTER) = I  
                J1=JJ(I)
                J2=JJ(I)+1
                IPOSP(NINDEX_VINTER) = VARTMP(I,J1+2)
                IADP(NINDEX_VINTER)  = NPF(IFUNC(JJ(I))     ) / 2 + 1
                ILENP(NINDEX_VINTER) = NPF(IFUNC(JJ(I))  + 1) / 2 - IADP(NINDEX_VINTER) - IPOSP(NINDEX_VINTER)
                IPOSP2(NINDEX_VINTER) = VARTMP(I,J2+2)
                IADP2(NINDEX_VINTER)  = NPF(IFUNC(JJ(I)+1)) / 2 + 1
                ILENP2(NINDEX_VINTER) = NPF(IFUNC(JJ(I)+1)  + 1) / 2 - IADP2(NINDEX_VINTER) - IPOSP2(NINDEX_VINTER)
                TAB_LOC(NINDEX_VINTER) = PLA(I)
              ENDDO

              CALL VINTER2(TF,IADP,IPOSP,ILENP,NINDEX_VINTER,TAB_LOC,DYDX1_LOC,Y1_LOC)
              CALL VINTER2(TF,IADP2,IPOSP2,ILENP2,NINDEX_VINTER,TAB_LOC,DYDX2_LOC,Y2_LOC)

              DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)       
                J1 = JJ(I)
                J2 = J1+1                                 
                VARTMP(I,J1+2) = IPOSP(II)
                VARTMP(I,J2+2) = IPOSP2(II)
              ENDDO
#include "vectorize.inc"
              DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)
                Y1(I) = Y1_LOC(II)
                Y2(I) = Y2_LOC(II)
                DYDX1(I)=DYDX1_LOC(II)
                DYDX2(I)=DYDX2_LOC(II)
              ENDDO
! ------------------------
#include "vectorize.inc" 
              DO II=1,NINDX                                             
                I = INDEX(II)  
                !Y1 and Y2 for Pl SR dependency
                Y1(I) = YFAC(I,1) * Y1(I)
                Y2(I) = YFAC(I,2) * Y2(I)
                FAC   = SRATE(I)         
                YLD(I)= FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
                YLD(I) = MAX(YLD(I),EM20)
                YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
                DYDX1(I)=DYDX1(I)*YFAC(I,1)
                DYDX2(I)=DYDX2(I)*YFAC(I,2)
                H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
                H(I)   = H(I)  *MAX(ZERO,PFAC(I))          
                DFSR   = H(I)+ MAX(ZERO,PFAC(I))*FAIL(I)*(Y2(I)-Y1(I))
     .                 /(UPARAM(7+JJ(I))-UPARAM(6+JJ(I))) /TIMESTEP
              ENDDO
            ENDDO  !iter
! ------------------------  
#include "vectorize.inc" 
            DO II=1,NINDX                                             
              I = INDEX(II) 
              PLA(I)    = PLAOLD(I) + DPLA_I(I)
              PLAP(I)   = DPLA_I(I) / TIMESTEP    
              SIGNXX(I) = SIGNXX(I)*R(I)
              SIGNYY(I) = SIGNYY(I)*R(I)
              SIGNZZ(I) = SIGNZZ(I)*R(I)
              SIGNXY(I) = SIGNXY(I)*R(I)
              SIGNYZ(I) = SIGNYZ(I)*R(I)
              SIGNZX(I) = SIGNZX(I)*R(I)
              DPLA1(I)  = DPLA_I(I)      
              UVAR(I,1) = DPLA_I(I) 
              UVAR(I,2) = PLAP(I)     
              UVAR(I,3) = YLD(I)     
            ENDDO  !NINDX      
c------------------------------------------------  
          ELSEIF (FISOKIN == ONE ) THEN  ! PURE kinematic hardening 
c------------------------------------------------  
            !Plastic increment : 
            !------------------- 
#include "vectorize.inc" 
            DO II=1,NINDX                                             
                I = INDEX(II) 
                SVM_TAB(I)       = SQRT(SVM2(I))  
                DPLA_J(I) =  UVAR(I,1) + EM09 
            !-------------- 
            !update yield :
            !-------------- 
                    JJ(I) = 1  
            ENDDO
            DO J=2,NRATE-1
#include "vectorize.inc"   
              DO II=1,NINDX                                             
                I = INDEX(II)                                      
                IF (PLAP(I) >= UPARAM(6+J)) JJ(I) = J               
              ENDDO
            ENDDO    
#include "vectorize.inc" 
            DO II=1,NINDX                                             
              I = INDEX(II) 
              J1 = JJ(I)
              J2 = JJ(I)+1
              FAC=UPARAM(6+J1)
              SRATE(I)=(PLAP(I) - FAC)/(UPARAM(7+J1) - FAC)
              YFAC(I,1)=UPARAM(6+NRATE+J1)*FACYLDI(I)
              YFAC(I,2)=UPARAM(6+NRATE+J2)*FACYLDI(I)
            ENDDO
! ------------------------
            NINDEX_VINTER = 0
#include "vectorize.inc"
            DO II=1,NINDX                                             
              I = INDEX(II)
              NINDEX_VINTER = NINDEX_VINTER + 1
              INDEX_VINTER(NINDEX_VINTER) = I 
              J1=JJ(I)
              J2=JJ(I)+1
              IPOSP(NINDEX_VINTER) = VARTMP(I,J1+2)
              IADP(NINDEX_VINTER)  = NPF(IFUNC(JJ(I))     ) / 2 + 1
              ILENP(NINDEX_VINTER) = NPF(IFUNC(JJ(I))  + 1) / 2 - IADP(NINDEX_VINTER) - IPOSP(NINDEX_VINTER)
              IPOSP2(NINDEX_VINTER) = VARTMP(I,J2+2)
              IADP2(NINDEX_VINTER)  = NPF(IFUNC(JJ(I)+1)) / 2 + 1
              ILENP2(NINDEX_VINTER) = NPF(IFUNC(JJ(I)+1)  + 1) / 2 - IADP2(NINDEX_VINTER) - IPOSP2(NINDEX_VINTER)
              TAB_LOC(NINDEX_VINTER) = PLA(I)
            ENDDO

            CALL VINTER2(TF,IADP,IPOSP,ILENP,NINDEX_VINTER,TAB_LOC,DYDX1_LOC,Y1_LOC)
            CALL VINTER2(TF,IADP2,IPOSP2,ILENP2,NINDEX_VINTER,TAB_LOC,DYDX2_LOC,Y2_LOC)

            DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)       
                J1 = JJ(I)
                J2 = JJ(I)+1                                 
                VARTMP(I,J1+2) = IPOSP(II)
                VARTMP(I,J2+2) = IPOSP2(II)
            ENDDO
#include "vectorize.inc"
            DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)
                Y1(I) = Y1_LOC(II)
                Y2(I) = Y2_LOC(II)
                DYDX1(I)=DYDX1_LOC(II)
                DYDX2(I)=DYDX2_LOC(II)
            ENDDO
! ------------------------
#include "vectorize.inc" 
            DO II=1,NINDX                                             
              I = INDEX(II) 
              !Y1 and Y2 for Pl SR dependency 
              J1 = JJ(I)
              J2 = JJ(I)+1
              Y1(I) = YFAC(I,1)*Y1(I)
              Y2(I) = YFAC(I,2)*Y2(I)                        
              FAC   = SRATE(I)        
              YLD(I)= FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
        
              DYDX1(I)=DYDX1(I)*YFAC(I,1)
              DYDX2(I)=DYDX2(I)*YFAC(I,2)
              H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
              H(I)   = H(I)  *MAX(ZERO,PFAC(I))          
              HI(I)   = H(I)*(ONE-FISOKIN)

              COEF    = (Y2(I)-Y1(I))/(UPARAM(6+J2)-UPARAM(6+J1)) /TIMESTEP   
              DFSR   = H(I)+ COEF
              DFSR   = DFSR * (ONE-FISOKIN)*MAX(ZERO,PFAC(I))  
              !Hardening
              Y1(I)=TF(NPF(IFUNC(J1))+1)
              Y2(I)=TF(NPF(IFUNC(J2))+1)
              Y1(I)= Y1(I)*YFAC(I,1)
              Y2(I)= Y2(I)*YFAC(I,2)
              COEF = MAX(ZERO,PFAC(I))*FISOKIN * FAIL(I)*(Y2(I)-Y1(I))
     .               /(UPARAM(6+J2)-UPARAM(6+J1)) /TIMESTEP   
              YLD(I) = (ONE-FISOKIN) * YLD(I) + 
     .              FISOKIN * (FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I))))
              YLD(I) = MAX(YLD(I),EM20)
              YLD(I)   = YLD(I)  *MAX(ZERO,PFAC(I))
            ENDDO          
            !----------------------
            !-- NEWTON ITERATIONS :
            !----------------------
            DO K=1,NITER
#include "vectorize.inc" 
              DO II=1,NINDX                                             
                I = INDEX(II) 
                !update svm       
                DPLA_I(I) = DPLA_J(I)
                PLA(I)    = PLAOLD(I) + DPLA_I(I)
                PLAP(I)   = DPLA_I(I) / TIMESTEP    
                R(I)      = YLD(I)/(YLD(I) +(G3(I)+FISOKIN*H(I))*DPLA_I(I))
                SVM       = R(I)* SVM_TAB(I)
                F         = SVM -  YLD(I) - (G3(I)+FISOKIN*H(I)) *DPLA_I(I)
                DF        =-(G3(I)+FISOKIN*H(I)) - (DFSR + COEF )
                DF = SIGN(MAX(ABS(DF),EM20),DF)
                IF(DPLA_I(I) > ZERO) THEN
                  DPLA_J(I)=MAX( EM10 ,DPLA_I(I)-F/DF)
                ELSE
                  DPLA_J(I)=EM10
                ENDIF 
                !--------------
                !update yield :
                !-------------- 
                PLA(I)    = PLAOLD(I) + DPLA_J(I)
                PLAP(I)   = DPLA_J(I) / TIMESTEP    
                JJ(I) = 1      
              ENDDO
              DO J=2,NRATE-1
#include "vectorize.inc" 
                DO II=1,NINDX                                             
                         I = INDEX(II) 
                           IF(PLAP(I) >= UPARAM(6+J)) JJ(I) = J
                ENDDO              
              ENDDO
#include "vectorize.inc" 
              DO II=1,NINDX                                             
                I = INDEX(II) 
                J1 = JJ(I)
                J2 = J1+1
                FAC=UPARAM(6+J1)
                SRATE(I)=(PLAP(I) - FAC)/(UPARAM(7+J1) - FAC)
                YFAC(I,1)=UPARAM(6+NRATE+J1)*FACYLDI(I)
                YFAC(I,2)=UPARAM(6+NRATE+J2)*FACYLDI(I)
              ENDDO
! ------------------------
              NINDEX_VINTER = 0
#include "vectorize.inc"
              DO II=1,NINDX                                             
                I = INDEX(II)
                NINDEX_VINTER = NINDEX_VINTER + 1
                INDEX_VINTER(NINDEX_VINTER) = I 
                J1=JJ(I)
                J2=JJ(I)+1
                IPOSP(NINDEX_VINTER) = VARTMP(I,J1+2)
                IADP(NINDEX_VINTER)  = NPF(IFUNC(JJ(I))     ) / 2 + 1
                ILENP(NINDEX_VINTER) = NPF(IFUNC(JJ(I))  + 1) / 2 - IADP(NINDEX_VINTER) - IPOSP(NINDEX_VINTER)
                IPOSP2(NINDEX_VINTER) = VARTMP(I,J2+2)
                IADP2(NINDEX_VINTER)  = NPF(IFUNC(JJ(I)+1)) / 2 + 1
                ILENP2(NINDEX_VINTER) = NPF(IFUNC(JJ(I)+1)  + 1) / 2 - IADP2(NINDEX_VINTER) - IPOSP2(NINDEX_VINTER)
                TAB_LOC(NINDEX_VINTER) = PLA(I)
              ENDDO

              CALL VINTER2(TF,IADP,IPOSP,ILENP,NINDEX_VINTER,TAB_LOC,DYDX1_LOC,Y1_LOC)
              CALL VINTER2(TF,IADP2,IPOSP2,ILENP2,NINDEX_VINTER,TAB_LOC,DYDX2_LOC,Y2_LOC)

              DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)       
                J1 = JJ(I)
                J2 = J1+1                                 
                VARTMP(I,J1+2) = IPOSP(II)
                VARTMP(I,J2+2) = IPOSP2(II)
              ENDDO
#include      "vectorize.inc"
              DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)
                Y1(I) = Y1_LOC(II)
                Y2(I) = Y2_LOC(II)
                DYDX1(I)=DYDX1_LOC(II)
                DYDX2(I)=DYDX2_LOC(II)
              ENDDO
! ------------------------
#include "vectorize.inc" 
              DO II=1,NINDX                                             
                I = INDEX(II) 
                !Y1 and Y2 for Pl SR dependency
                J1 = JJ(I)
                J2 = JJ(I)+1
                Y1(I) = YFAC(I,1) * Y1(I) 
                Y2(I) = YFAC(I,2) * Y2(I) 
                FAC   = SRATE(I)        
                        YLD(I)= FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
                DYDX1(I)=DYDX1(I)*YFAC(I,1)
                DYDX2(I)=DYDX2(I)*YFAC(I,2)
                H(I)  = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
                H(I)  = H(I)  *MAX(ZERO,PFAC(I))          
                HI(I) = H(I)*(ONE-FISOKIN)
                DFSR  = HI(I)+ MAX(ZERO,PFAC(I))* (ONE-FISOKIN)*(Y2(I)-Y1(I))
     .                / (UPARAM(7+JJ(I))-UPARAM(6+JJ(I))) /TIMESTEP
                Y1(I)=TF(NPF(IFUNC(J1))+1)
                Y2(I)=TF(NPF(IFUNC(J2))+1)
                Y1(I)=Y1(I)*YFAC(I,1)
                Y2(I)=Y2(I)*YFAC(I,2)
                COEF = MAX(ZERO,PFAC(I))*FISOKIN * FAIL(I)*(Y2(I)-Y1(I))
     .               / (UPARAM(6+J2)-UPARAM(6+J1)) /TIMESTEP   
                YLD(I) = (ONE-FISOKIN) * YLD(I)
     .                 + FISOKIN * (FAIL(I)*(Y1(I) + FAC*(Y2(I)-Y1(I))))
                YLD(I) = MAX(YLD(I),EM20)
                YLD(I) = YLD(I) * MAX(ZERO,PFAC(I))
              ENDDO          
            ENDDO  !iter
! ------------------------
#include "vectorize.inc" 
            DO II=1,NINDX                                             
              I = INDEX(II)  
              PLA(I)    = PLAOLD(I) + DPLA_I(I)
              PLAP(I)   = DPLA_I(I) / TIMESTEP    
              SIGNXX(I) = SIGNXX(I)*R(I)
              SIGNYY(I) = SIGNYY(I)*R(I)
              SIGNZZ(I) = SIGNZZ(I)*R(I)
              SIGNXY(I) = SIGNXY(I)*R(I)
              SIGNYZ(I) = SIGNYZ(I)*R(I)
              SIGNZX(I) = SIGNZX(I)*R(I)
              DPLA1(I)  = DPLA_I(I)      
              UVAR(I,1) = DPLA_I(I) 
              UVAR(I,2) = PLAP(I)     
              UVAR(I,3) = YLD(I)     
            ENDDO  !NINDX   
c------------------------------------------------  
          ELSE  !  (1 > FISOKIN > 0) : Mixed ISO-KINE HARDENING
c------------------------------------------------  
#include "vectorize.inc" 
            DO II=1,NINDX                                             
              I = INDEX(II) 
              SVM_TAB(I)       = SQRT(SVM2(I))  
              DPLA_J(I) =  UVAR(I,1) + EM09 
              !DPLA_J(I) = (SVM-YLD(I))/(G3(I)+H(I))!converge moins bien avec estimation radial
              !-------------- 
              !update yield :
              !-------------- 
              JJ(I) = 1                                                                                               
            ENDDO
            DO J=2,NRATE-1  
#include "vectorize.inc" 
              DO II=1,NINDX                                             
                I = INDEX(II)                                      
                IF(PLAP(I) >= UPARAM(6+J)) JJ(I) = J               
              ENDDO 
            ENDDO
#include "vectorize.inc" 
            DO II=1,NINDX                                             
              I = INDEX(II)
              J1 = JJ(I)
              J2 = J1+1
              FAC=UPARAM(6+J1)
              SRATE(I)=(PLAP(I) - FAC)/(UPARAM(7+J1) - FAC)
              YFAC(I,1)=UPARAM(6+NRATE+J1)*FACYLDI(I)
              YFAC(I,2)=UPARAM(6+NRATE+J2)*FACYLDI(I) 
            ENDDO
! ------------------------
            NINDEX_VINTER = 0
#include "vectorize.inc"
            DO II=1,NINDX                                             
              I = INDEX(II)
              NINDEX_VINTER = NINDEX_VINTER + 1
              INDEX_VINTER(NINDEX_VINTER) = I 
              J1=JJ(I)
              J2=JJ(I)+1
              IPOSP(NINDEX_VINTER) = VARTMP(I,J1+2)
              IADP(NINDEX_VINTER)  = NPF(IFUNC(JJ(I))     ) / 2 + 1
              ILENP(NINDEX_VINTER) = NPF(IFUNC(JJ(I))  + 1) / 2 - IADP(NINDEX_VINTER) - IPOSP(NINDEX_VINTER)
              IPOSP2(NINDEX_VINTER) = VARTMP(I,J2+2)
              IADP2(NINDEX_VINTER)  = NPF(IFUNC(JJ(I)+1)) / 2 + 1
              ILENP2(NINDEX_VINTER) = NPF(IFUNC(JJ(I)+1)  + 1) / 2 - IADP2(NINDEX_VINTER) - IPOSP2(NINDEX_VINTER)
              TAB_LOC(NINDEX_VINTER) = PLA(I)
            ENDDO

            CALL VINTER2(TF,IADP,IPOSP,ILENP,NINDEX_VINTER,TAB_LOC,DYDX1_LOC,Y1_LOC)
            CALL VINTER2(TF,IADP2,IPOSP2,ILENP2,NINDEX_VINTER,TAB_LOC,DYDX2_LOC,Y2_LOC)

            DO II=1,NINDEX_VINTER  
              I = INDEX_VINTER(II)       
              J1 = JJ(I)
              J2 = J1+1                                 
              VARTMP(I,J1+2) = IPOSP(II)
              VARTMP(I,J2+2) = IPOSP2(II)
            ENDDO
#include    "vectorize.inc"
            DO II=1,NINDEX_VINTER  
              I = INDEX_VINTER(II)
              Y1(I) = Y1_LOC(II)
              Y2(I) = Y2_LOC(II)
              DYDX1(I)=DYDX1_LOC(II)
              DYDX2(I)=DYDX2_LOC(II)
            ENDDO
! ------------------------
#include "vectorize.inc"
            DO II=1,NINDX                                             
              I = INDEX(II) 
              !Y1 and Y2 for Pl SR dependency
              J1 = JJ(I)
              J2 = JJ(I)+1
              Y1(I) = YFAC(I,1) * Y1(I)
              Y2(I) = YFAC(I,2) * Y2(I)
              FAC   = SRATE(I)        
              YLD(I)= FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
        
              DYDX1(I)=DYDX1(I)*YFAC(I,1)
              DYDX2(I)=DYDX2(I)*YFAC(I,2)
              H(I)  = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
              H(I)  = H(I)  *MAX(ZERO,PFAC(I))          
              HI(I) = H(I)*(ONE-FISOKIN)
        
              COEF = (Y2(I)-Y1(I))/(UPARAM(6+J2)-UPARAM(6+J1)) /TIMESTEP   
              DFSR = H(I)+ COEF
              DFSR = DFSR * (ONE-FISOKIN)*MAX(ZERO,PFAC(I))  
              !Hardening
              Y1(I)=TF(NPF(IFUNC(J1))+1)
              Y2(I)=TF(NPF(IFUNC(J2))+1)
              Y1(I)= Y1(I)*YFAC(I,1)
              Y2(I)= Y2(I)*YFAC(I,2)
              COEF = MAX(ZERO,PFAC(I))*FISOKIN * FAIL(I)*(Y2(I)-Y1(I))
     .               /(UPARAM(6+J2)-UPARAM(6+J1)) /TIMESTEP   
              YLD(I) = (ONE-FISOKIN) * YLD(I) + 
     .              FISOKIN * (FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I))))
              YLD(I) = MAX(YLD(I),EM20)
              YLD(I) = YLD(I)*MAX(ZERO,PFAC(I))
            ENDDO          
            !----------------------
            !-- NEWTON ITERATIONS :
            !----------------------
            DO K=1,NITER
#include     "vectorize.inc"
              DO II=1,NINDX                                             
                I = INDEX(II) 
                !update svm       
                DPLA_I(I) = DPLA_J(I)
                PLA(I)    = PLAOLD(I) + DPLA_I(I)
                PLAP(I)   = DPLA_I(I) / TIMESTEP    
                R(I)      = YLD(I)/(YLD(I) +(G3(I)+FISOKIN*H(I))*DPLA_I(I))
                SVM       = R(I)* SVM_TAB(I)
                F         = SVM -  YLD(I) - (G3(I)+FISOKIN*H(I)) *DPLA_I(I)
                DF        =-(G3(I)+FISOKIN*H(I)) - (DFSR + COEF )
                DF = SIGN(MAX(ABS(DF),EM20),DF)
                IF(DPLA_I(I) > ZERO) THEN
                  DPLA_J(I)=MAX( EM10 ,DPLA_I(I)-F/DF)
                ELSE
                DPLA_J(I)=EM10
                ENDIF 
                !--------------
                !update yield :
                !-------------- 
                PLA(I)    = PLAOLD(I) + DPLA_J(I)
                PLAP(I)   = DPLA_J(I) / TIMESTEP    
                JJ(I) = 1      
              ENDDO
              DO J=2,NRATE-1
#include "vectorize.inc"
                DO II=1,NINDX                                             
                  I = INDEX(II) 
                  IF(PLAP(I) >= UPARAM(6+J)) JJ(I) = J              
                ENDDO
              ENDDO
#include "vectorize.inc"
              DO II=1,NINDX                                             
                I = INDEX(II)
                J1 = JJ(I)
                J2 = J1+1
                FAC=UPARAM(6+J1)
                SRATE(I)=(PLAP(I) - FAC)/(UPARAM(7+J1) - FAC)
                YFAC(I,1)=UPARAM(6+NRATE+J1)*FACYLDI(I)
                YFAC(I,2)=UPARAM(6+NRATE+J2)*FACYLDI(I)
              ENDDO
! ------------------------
              NINDEX_VINTER = 0
#include "vectorize.inc"
              DO II=1,NINDX                                             
                I = INDEX(II)
                NINDEX_VINTER = NINDEX_VINTER + 1
                INDEX_VINTER(NINDEX_VINTER) = I 
                J1=JJ(I)
                J2=JJ(I)+1
                IPOSP(NINDEX_VINTER) = VARTMP(I,J1+2)
                IADP(NINDEX_VINTER)  = NPF(IFUNC(JJ(I))     ) / 2 + 1
                ILENP(NINDEX_VINTER) = NPF(IFUNC(JJ(I))  + 1) / 2 - IADP(NINDEX_VINTER) - IPOSP(NINDEX_VINTER)
                IPOSP2(NINDEX_VINTER) = VARTMP(I,J2+2)
                IADP2(NINDEX_VINTER)  = NPF(IFUNC(JJ(I)+1)) / 2 + 1
                ILENP2(NINDEX_VINTER) = NPF(IFUNC(JJ(I)+1)  + 1) / 2 - IADP2(NINDEX_VINTER) - IPOSP2(NINDEX_VINTER)
                TAB_LOC(NINDEX_VINTER) = PLA(I)
              ENDDO

              CALL VINTER2(TF,IADP,IPOSP,ILENP,NINDEX_VINTER,TAB_LOC,DYDX1_LOC,Y1_LOC)
              CALL VINTER2(TF,IADP2,IPOSP2,ILENP2,NINDEX_VINTER,TAB_LOC,DYDX2_LOC,Y2_LOC)

              DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)       
                J1 = JJ(I)
                J2 = J1+1                                 
                VARTMP(I,J1+2) = IPOSP(II)
                VARTMP(I,J2+2) = IPOSP2(II)
              ENDDO
#include "vectorize.inc"
              DO II=1,NINDEX_VINTER  
                I = INDEX_VINTER(II)
                Y1(I) = Y1_LOC(II)
                Y2(I) = Y2_LOC(II)
                DYDX1(I)=DYDX1_LOC(II)
                DYDX2(I)=DYDX2_LOC(II)
              ENDDO
! ------------------------
#include "vectorize.inc"
              DO II=1,NINDX                                             
                I = INDEX(II)                 
                !Y1 and Y2 for Pl SR dependency
                J1 = JJ(I)
                J2 = JJ(I)+1
                Y1(I) = YFAC(I,1) * Y1(I)
                Y2(I) = YFAC(I,2) * Y2(I) 
                FAC   = SRATE(I)        
                YLD(I)= FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
                DYDX1(I)=DYDX1(I)*YFAC(I,1)
                DYDX2(I)=DYDX2(I)*YFAC(I,2)
                H(I) = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
                H(I) = H(I)  *MAX(ZERO,PFAC(I))          
                HI(I)= H(I)*(ONE-FISOKIN)
                DFSR = HI(I)+ MAX(ZERO,PFAC(I))* (ONE-FISOKIN)*(Y2(I)-Y1(I))
     .                   /(UPARAM(7+JJ(I))-UPARAM(6+JJ(I))) /TIMESTEP
                Y1(I)=TF(NPF(IFUNC(J1))+1)
                Y2(I)=TF(NPF(IFUNC(J2))+1)
                Y1(I)=Y1(I)*YFAC(I,1)
                Y2(I)=Y2(I)*YFAC(I,2)
                COEF = MAX(ZERO,PFAC(I))*FISOKIN * FAIL(I)*(Y2(I)-Y1(I))
     .                /(UPARAM(6+J2)-UPARAM(6+J1)) /TIMESTEP   
                YLD(I) = (ONE-FISOKIN) * YLD(I) + 
     .               FISOKIN * (FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I))))
                YLD(I) = MAX(YLD(I),EM20)
                YLD(I) = YLD(I)  *MAX(ZERO,PFAC(I))
              ENDDO          
            ENDDO  !iter  
#include "vectorize.inc"
            DO II=1,NINDX                                             
                I = INDEX(II) 
                PLA(I)    = PLAOLD(I) + DPLA_I(I)
                PLAP(I)   = DPLA_I(I) / TIMESTEP    
                SIGNXX(I) = SIGNXX(I)*R(I)
                SIGNYY(I) = SIGNYY(I)*R(I)
                SIGNZZ(I) = SIGNZZ(I)*R(I)
                SIGNXY(I) = SIGNXY(I)*R(I)
                SIGNYZ(I) = SIGNYZ(I)*R(I)
                SIGNZX(I) = SIGNZX(I)*R(I)
                DPLA1(I)  = DPLA_I(I)      
                UVAR(I,1) = DPLA_I(I) 
                UVAR(I,2) = PLAP(I)     
                UVAR(I,3) = YLD(I)     
            ENDDO 
          ENDIF  !   FISOKIN
        ENDIF    !   NINDX > 0  (plasticity)
c------------------------------------------
      ENDIF ! VP flag
C=======================================================================
      IF (IPLA /= 1 .OR. IMPL_S <= 0) THEN
C------------------------------------------
        IF (FISOKIN == ONE ) THEN  ! ECROUISSAGE CINE
#include "vectorize.inc"
          DO I=1,NEL
             DSXX = SIGEXX(I) - SIGNXX(I) 
             DSYY = SIGEYY(I) - SIGNYY(I)
             DSZZ = SIGEZZ(I) - SIGNZZ(I)
             DSXY = SIGEXY(I) - SIGNXY(I)
             DSYZ = SIGEYZ(I) - SIGNYZ(I)
             DSZX = SIGEZX(I) - SIGNZX(I)
 
             HKIN = TWO_THIRD*FISOKIN*H(I)
             ALPHA = HKIN/(G2(I)+HKIN)  
             SIGPXX = ALPHA*DSXX 
             SIGPYY = ALPHA*DSYY
             SIGPZZ = ALPHA*DSZZ 
             SIGPXY = ALPHA*DSXY
             SIGPYZ = ALPHA*DSYZ
             SIGPZX = ALPHA*DSZX

c          ..updates back stresses
             SIGBXX(I) = SIGBXX(I)  + SIGPXX 
             SIGBYY(I) = SIGBYY(I)  + SIGPYY
             SIGBZZ(I) = SIGBZZ(I)  + SIGPZZ
             SIGBXY(I) = SIGBXY(I)  + SIGPXY
             SIGBYZ(I) = SIGBYZ(I)  + SIGPYZ
             SIGBZX(I) = SIGBZX(I)  + SIGPZX         

c          ..gets stresses from shifted stresses and back stresses
             SIGNXX(I) = SIGNXX(I) + SIGBXX(I) 
             SIGNYY(I) = SIGNYY(I) + SIGBYY(I) 
             SIGNZZ(I) = SIGNZZ(I) + SIGBZZ(I) 
             SIGNXY(I) = SIGNXY(I) + SIGBXY(I)
             SIGNYZ(I) = SIGNYZ(I) + SIGBYZ(I) 
             SIGNZX(I) = SIGNZX(I) + SIGBZX(I) 
          ENDDO

        ELSEIF (FISOKIN > ZERO) THEN   ! ecrouissage mixte
#include "vectorize.inc"
          DO I=1,NEL
             DSXX = SIGEXX(I) - SIGNXX(I) 
             DSYY = SIGEYY(I) - SIGNYY(I)
             DSZZ = SIGEZZ(I) - SIGNZZ(I)
             DSXY = SIGEXY(I) - SIGNXY(I)
             DSYZ = SIGEYZ(I) - SIGNYZ(I)
             DSZX = SIGEZX(I) - SIGNZX(I)
 
                HKIN = TWO_THIRD*FISOKIN*H(I)
             ALPHA = HKIN/(G2(I)+HKIN)  
             SIGPXX = ALPHA*DSXX 
             SIGPYY = ALPHA*DSYY
             SIGPZZ = ALPHA*DSZZ 
             SIGPXY = ALPHA*DSXY
             SIGPYZ = ALPHA*DSYZ
             SIGPZX = ALPHA*DSZX

c          ..updates back stresses
             SIGBXX(I) = SIGBXX(I)  + SIGPXX 
             SIGBYY(I) = SIGBYY(I)  + SIGPYY
             SIGBZZ(I) = SIGBZZ(I)  + SIGPZZ
             SIGBXY(I) = SIGBXY(I)  + SIGPXY
             SIGBYZ(I) = SIGBYZ(I)  + SIGPYZ
             SIGBZX(I) = SIGBZX(I)  + SIGPZX         

c          ..gets stresses from shifted stresses and back stresses
             SIGNXX(I) = SIGNXX(I) + SIGBXX(I) 
             SIGNYY(I) = SIGNYY(I) + SIGBYY(I) 
             SIGNZZ(I) = SIGNZZ(I) + SIGBZZ(I) 
             SIGNXY(I) = SIGNXY(I) + SIGBXY(I)
             SIGNYZ(I) = SIGNYZ(I) + SIGBYZ(I) 
             SIGNZX(I) = SIGNZX(I) + SIGBZX(I) 
          ENDDO
        ENDIF ! ..FISOKIN 
      END IF !(IPLA/=1.OR.IMPL_S<=0 ) THEN

c   ..adds pressure to the deviatoric stress
      DO I=1,NEL
c        P = C1(I) * (RHO(I)/RHO0(I)-ONE)
        P = C1(I) * AMU(I)
        SIGNXX(I)=SIGNXX(I)-P
        SIGNYY(I)=SIGNYY(I)-P
        SIGNZZ(I)=SIGNZZ(I)-P
      ENDDO
c
c-------------------------
      IF (IMPL_S > 0) THEN
c    ..saves the shifted elastic predictors, plastic strain multiplier,
c      the Kirchhoff's modulus and the plastic hardening modulus
c      for the sake of computation of elasto-plastic stiffness matrix
c
        DO I=1,NEL
          IF(DPLA1(I) > 0) ETSE(I)= H(I)/G2(I)
        ENDDO
        DO I = 1,NEL
          IF (DPLA1(I) > ZERO) THEN

c ......    Von Mises stress at the elastic predictor (point B)
c ......    SIGEXX, etc. are deviatoric stresses
            VM =HALF*(SIGEXX(I)**2+SIGEYY(I)**2+SIGEZZ(I)**2)
     .             +SIGEXY(I)**2+SIGEYZ(I)**2+SIGEZX(I)**2
            VM_1 =ONE/SQRT(THREE*VM)
            G3H = G3(I)+H(I)
            R(I) = MAX(ZERO,ONE-G3H*DPLA1(I)*VM_1)
c ......    NORM_1 normalizes deviatoric stresses, includes consistent
c ......    stiffness matrix parameter beta, von Mises at B, and two_pmi
            NORM_1=G3(I)*VM_1*SQRT(R(I)/G3H)
c ......    Deviatoric stresses "normalized"
            SIGNOR(I,1)=SIGEXX(I)*NORM_1
            SIGNOR(I,2)=SIGEYY(I)*NORM_1
            SIGNOR(I,3)=SIGEZZ(I)*NORM_1
            SIGNOR(I,4)=SIGEXY(I)*NORM_1
            SIGNOR(I,5)=SIGEYZ(I)*NORM_1
            SIGNOR(I,6)=SIGEZX(I)*NORM_1

c ......    Parameter alpha of consistent matrix
            AL_IMP(I)= ONE - G3(I)*DPLA1(I)*VM_1
          ELSE
            AL_IMP(I)=ONE
          ENDIF
        ENDDO
      ENDIF  ! IMPL_S > 0
c-------------------------
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < ONE)   OFF(I) = OFF(I)*FOUR_OVER_5
      ENDDO
c
      IF (IFAIL > 0) THEN
        NINDX = 0
        IF (IFAIL == 2) THEN
          DO I=1,NEL
            IF ((PLA(I) > EPSMAX .OR. EPSTT(I) > EPSF) .AND. OFF(I) == ONE) THEN
              OFF(I)= FOUR_OVER_5
              NINDX = NINDX+1
              INDX(NINDX) = I
            ENDIF
          ENDDO
        ELSE
          DO I=1,NEL
            IF (PLA(I) > EPSMAX .AND. OFF(I) == ONE) THEN
              OFF(I)= FOUR_OVER_5
              NINDX = NINDX+1
              INDX(NINDX) = I
            ENDIF
          ENDDO
        END IF
        IF (NINDX > 0 .AND. IMCONV == 1) THEN
          IDEL7NOK = 1
          DO J=1,NINDX
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(INDX(J))
            WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF  ! IFAIL
C-----------
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10, ' AT TIME :',G11.4)
C-----------
      RETURN
      END
