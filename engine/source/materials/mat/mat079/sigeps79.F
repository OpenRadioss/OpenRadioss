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
C|======================================================================
C|  SIGEPS79                                /mate79/sigeps79.F
C|----------------------------------------------------------------------
C|-- appelee par -----------
C|         MULAW                            /matera/mulaw.F
C|-- appelle ---------------
C|         
C|======================================================================
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  SIGEPS79                      source/materials/mat/mat079/sigeps79.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SIGEPS79(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,                                                                                                               
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VNEW   , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , DVOL  , VOL0  ,
     B      IPM    , MAT    , EPSD    , SIGY  , DPLA  , DEFP  ,
     C      AMU    )
C
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
C VNEW    | NEL     | F | R | VOLUME
C VOL0    | NEL     | F | R | VOLUME INITIAL (LAGRANGE)
C         |         | F | R | VOLUME (ALE)
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
C EPSD    | NEL     | F | R | STRAIN RATE
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SIGY    | NEL     | F | W | YIELD STRESS
C DPLA    | NEL     | F | W | INCREMENT OF PLASTIC STRAIN
C DEFP    | NEL     | F | W | PLASTIC STRAIN
C ...     |         |   |   |    
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "com08_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "units_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR, IPM(NPROPMI,*), MAT(NEL)
C
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VNEW(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      DVOL(NEL)  , VOL0(NEL)  , EPSD(NEL)  ,
     .      AMU(NEL)      
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SIGY(NEL)   , DPLA(NEL)   , DEFP(NEL)  ,
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I A B L E S
C----------------------------------------------------------------
      INTEGER I
      my_real
     .   G     , G2   , AA   , BB     , MM   ,
     .   NN    , CC   , EPS0 , SIGFMAX,
     .   TSTAR , PHEL , SHEL , BETA   ,
     .   D1    , D2   , K1   , K2     , K3
      my_real
     .   MU(MVSIZ),MU2(MVSIZ), POLD(MVSIZ), DAMAGE(MVSIZ), VM(MVSIZ),
     .   DELTAP(MVSIZ), PNEW(MVSIZ), PSTAR(MVSIZ), SCALE(MVSIZ), 
     .   SIGYI(MVSIZ),  SIGYF(MVSIZ), SIGYOLD(MVSIZ)
      my_real
     .   DAV, CE, SIGSTAR, EPFAIL, P1, YIELD, DELTAU, DPDMU,
     .   PMIN, RATIO, J2
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      G       = UPARAM(1)
      G2      = UPARAM(2)
      AA      = UPARAM(3)
      BB      = UPARAM(4)
      MM      = UPARAM(5)
      NN      = UPARAM(6)
      CC      = UPARAM(7)
      EPS0    = UPARAM(8)
      SIGFMAX = UPARAM(9)
      TSTAR   = UPARAM(10)
      PHEL    = UPARAM(11)
      SHEL    = UPARAM(12)
      D1      = UPARAM(13)
      D2      = UPARAM(14)
      K1      = UPARAM(15)
      K2      = UPARAM(16)
      K3      = UPARAM(17)
      BETA    = UPARAM(18)
C
      DO I=1,NEL
         DEFP(I)   = UVAR(I,1)
         DAMAGE(I) = UVAR(I,3)
         DELTAP(I) = UVAR(I,4)
         SIGYOLD(I)= UVAR(I,5)/SHEL
         POLD(I)   =-(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
c         MU(I)     = RHO(I)/RHO0(I)-ONE
         MU(I)     = AMU(I)
         MU2(I)    = MU(I)*MU(I)
      ENDDO
C--------------------------------
C     ELASTIC DEVIATORIC STRESSES
C--------------------------------
      DO I=1,NEL
         DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
         SIGNXX(I)=SIGOXX(I)+POLD(I)+G2*(DEPSXX(I)-DAV)
         SIGNYY(I)=SIGOYY(I)+POLD(I)+G2*(DEPSYY(I)-DAV)
         SIGNZZ(I)=SIGOZZ(I)+POLD(I)+G2*(DEPSZZ(I)-DAV)
         SIGNXY(I)=SIGOXY(I)+G*DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)+G*DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I)+G*DEPSZX(I)
      ENDDO
C-------------------------------
C     EQUIVALENT STRESS
C-------------------------------
      DO I=1,NEL
         J2   =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     .                +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
         VM(I)=SQRT(THREE*J2)
      ENDDO
C-------------------------------
C     PRESSURE
C-------------------------------
      DO I=1,NEL
         PNEW(I)=K1*MU(I)+DELTAP(I)
         IF(MU(I).GT.ZERO) THEN
           PNEW(I)=PNEW(I)+K2*MU2(I)+K3*MU2(I)*MU(I)
         ELSE
           PMIN=-TSTAR*PHEL*(ONE-DAMAGE(I))
           PNEW(I)=MAX(PNEW(I),PMIN)
         ENDIF
         PSTAR(I)=PNEW(I)/PHEL
      ENDDO
C-------------------------------
C     CRITERE
C-------------------------------
      DO I=1,NEL
         IF(NN == ZERO) THEN
           SIGYI(I) = AA
         ELSEIF((PSTAR(I)+TSTAR) > ZERO) THEN
           SIGYI(I) = AA*(PSTAR(I)+TSTAR)**NN
         ELSE
           SIGYI(I) = ZERO
         ENDIF
C
         IF(MM == ZERO) THEN
           SIGYF(I) = BB
         ELSEIF(PSTAR(I) > ZERO) THEN
           SIGYF(I) = BB*(PSTAR(I))**MM
         ELSE
           SIGYF(I) = ZERO
         ENDIF
C
         IF(EPSD(I).LE.EPS0) THEN
           CE = ONE
         ELSE
           CE = ONE + CC*LOG(EPSD(I)/EPS0)
         ENDIF
C
         SIGYI(I)  = CE*SIGYI(I)
         SIGYF(I)  = MIN(CE*SIGYF(I),SIGFMAX)
         SIGY(I)   =(ONE-DAMAGE(I))*SIGYI(I)+DAMAGE(I)*SIGYF(I)
      ENDDO
C-------------------------------
C     PROJECTION
C-------------------------------
      DO I=1,NEL
         SIGSTAR=VM(I)/SHEL
         IF(SIGSTAR < SIGY(I)) THEN
           SCALE(I)=ONE
         ELSEIF(VM(I) > ZERO) THEN
           SCALE(I)=SIGY(I)/SIGSTAR
         ELSE
           SCALE(I)=ZERO
         ENDIF
      ENDDO
C
      DO I=1,NEL
         SIGNXX(I)=SCALE(I)*SIGNXX(I)
         SIGNYY(I)=SCALE(I)*SIGNYY(I)
         SIGNZZ(I)=SCALE(I)*SIGNZZ(I)
         SIGNXY(I)=SCALE(I)*SIGNXY(I)
         SIGNYZ(I)=SCALE(I)*SIGNYZ(I)
         SIGNZX(I)=SCALE(I)*SIGNZX(I)
      ENDDO
C-----------------------------
C NEW DAMAGE - PLASTIC STRAIN
C-----------------------------
      DO I=1,NEL
         IF(D2 == ZERO) THEN
           EPFAIL = D1
         ELSEIF((PSTAR(I)+TSTAR) >= ZERO) THEN
           EPFAIL = D1*(PSTAR(I)+TSTAR)**D2
         ELSE
           EPFAIL = ZERO
         ENDIF
C
         IF(EPFAIL > ZERO) THEN
           DPLA(I) = (ONE -SCALE(I))*VM(I)/(THREE*SQRT(THREE)*G)
           DEFP(I) = DEFP(I)+DPLA(I)   
           DAMAGE(I) = MIN(ONE,DAMAGE(I)+DPLA(I)/EPFAIL)
         ELSEIF(SCALE(I) < ONE) THEN
           DAMAGE(I) = ONE
         ENDIF
      ENDDO      
C------------
C NEW DELTAP
C------------
      DO I=1,NEL
        IF(MU(I) <= ZERO) CYCLE
        IF(DAMAGE(I) > UVAR(I,3)) THEN
         P1=K1*MU(I)
         YIELD  =(ONE-DAMAGE(I))*SIGYI(I)+DAMAGE(I)*SIGYF(I)
         DELTAU =(SIGYOLD(I)*SIGYOLD(I)-YIELD*YIELD)/(SIX*G)
         IF(DELTAU > ZERO) THEN
           DELTAU=DELTAU*SHEL*SHEL
           DELTAP(I)=-P1+
     .               SQRT((DELTAP(I)+P1)**2+TWO*BETA*K1*DELTAU)
         ENDIF
        ENDIF
      ENDDO
C
      DO I=1,NEL
         UVAR(I,1)=DEFP(I)
         UVAR(I,2)=ZERO
         UVAR(I,3)=DAMAGE(I)
         UVAR(I,4)=DELTAP(I)
         UVAR(I,5)=SIGY(I)*SHEL
         SIGNXX(I)=SIGNXX(I)-PNEW(I)
         SIGNYY(I)=SIGNYY(I)-PNEW(I)
         SIGNZZ(I)=SIGNZZ(I)-PNEW(I)
C----------------
C SET SOUND SPEED
C----------------
         DPDMU=K1+TWO*K2*MAX(ZERO,MU(I))+THREE*K3*MU2(I)
         SOUNDSP(I)=SQRT((DPDMU+FOUR_OVER_3*G)/RHO0(I))
C----------------------
C SET MAXIMUM VISCOSITY
C----------------------
         VISCMAX(I)=ZERO
      ENDDO
C
      RETURN
      END
