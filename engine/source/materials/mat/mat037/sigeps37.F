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
Chd|  SIGEPS37                      source/materials/mat/mat037/sigeps37.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS37 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,IX      ,NIX    ,
     B     NFT )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Biphasic material law : air / liquid.
C   air is modeled with ideal gas EOS : PV**gamma = constant
C   liquid is modeled with linear EOS : P= P0+C1.mu     where C1 is bulk modulus and P0 is initial pressure
C   massic percentage determines the ratio AIR/LIQUID in the cell
C Purpose is to compute equilibrium Pair=Pwater, this leads to mu_air and mu_water (iterative method, niter=2 is used)
C
C   ISOLVER = 1 (default) is the legacy solver. Sound Speed is computed from water whatever is the mixture. There is only 2 iterations.
C   ISOLVER = 2 is an alternative solver using a Newton Algorithm with (max 20 iterations). Convergence criteria introduced and sound speed computed from mixture.
C               This sound speed formulation may leads to global value potentially lower than each submaterial value.
C               recommended : Courant number <= 0.5 (dt_scale)
C
C Output
C    !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multipling by element volume in aleconve.F)
C    !UVAR(I,2) : density of gas
C    !UVAR(I,3) : density of liquid
C    !UVAR(I,4) : volumetric fraction of liquid
C    !UVAR(I,5) : volumetric fraction of gas
C
C        !---------+---------+---+---+--------------------------------------------
C        ! VAR     | SIZE    |TYP| RW| DEFINITION
C        !---------+---------+---+---+--------------------------------------------
C        ! NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C        ! NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C        ! NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C        !---------+---------+---+---+--------------------------------------------
C        ! NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C        ! IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C        ! NPF     |  *      | I | R | FUNCTION ARRAY   
C        ! TF      |  *      | F | R | FUNCTION ARRAY 
C        !---------+---------+---+---+--------------------------------------------
C        ! TIME    |  1      | F | R | CURRENT TIME
C        ! TIMESTEP|  1      | F | R | CURRENT TIME STEP
C        ! UVAR    | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C        ! RHO0    | NEL     | F | R | INITIAL DENSITY
C        ! RHO     | NEL     | F | R | DENSITY
C        ! VOLUME  | NEL     | F | R | VOLUME
C        ! EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C        ! EPSPXX  | NEL     | F | R | STRAIN RATE XX
C        ! EPSPYY  | NEL     | F | R | STRAIN RATE YY
C        ! ...     |         |   |   |
C        ! DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C        ! DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C        ! ...     |         |   |   |
C        ! EPSXX   | NEL     | F | R | STRAIN XX
C        ! EPSYY   | NEL     | F | R | STRAIN YY
C        ! ...     |         |   |   |
C        ! SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C        ! SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C        ! ...     |         |   |   |    
C        !---------+---------+---+---+--------------------------------------------
C        ! SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C        ! SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C        ! ...     |         |   |   |
C        ! SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C        ! SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C        ! ...     |         |   |   |
C        ! SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C        ! VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C        !---------+---------+---+---+--------------------------------------------
C        ! UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C        ! OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C        !---------+---------+---+---+--------------------------------------------
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I n c l u d  e s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR
      my_real TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) ::
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real,INTENT(INOUT) :: UVAR(NEL,NUVAR), OFF(NEL)
      INTEGER, INTENT(IN)   :: NPF(*), NFUNC, IFUNC(NFUNC), NIX, IX(NIX,*), NFT
      my_real, INTENT(IN)   :: TF(*)
      my_real, EXTERNAL     :: FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, ISOLVER, NITER, ITER
      my_real 
     .   SSP,VIS,VIS2,VIS3,VV,C1,C2,C12,R1,R2,PMIN,A2,RHO10,RHO20,
     .   RHO1,RHO2,A1,A,B,C, B1,B2,P,GAM,P0,GPR,POLD,
     .   PN2,RHN2,VISA1,VISB1,VISA2,VISB2,DYDX,RHOSCALE,TOL, VOL, MAS, MAS1, MAS2,
     .   RHO1T,RHO2T, ERROR, P1,P2,F1,F2,DF11,DF12,DF21,DF22,DET,DRHO1,DRHO2,
     .   MU1P1, MU2P1, PSH, SSP1, SSP2
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------

      !------------------------------------!
      !      USER PARAMETERS               !
      !------------------------------------!
      VISA1    = UPARAM(1)
      VISB1    = UPARAM(3)
      VISA2    = UPARAM(13)
      VISB2    = UPARAM(15)
      C1       = UPARAM(4)
      GAM      = UPARAM(5)
      R1       = UPARAM(6)
      GPR      = UPARAM(7)
      PMIN     = UPARAM(8)
      P0       = UPARAM(9)
      RHO10    = UPARAM(11)
      RHO20    = UPARAM(12)
      PSH      = UPARAM(16)
      ISOLVER  = NINT(UPARAM(17))
      RHOSCALE = ONE
      IF(IFUNC(1)>0)RHOSCALE=FINTER(IFUNC(1),TIME,NPF,TF,DYDX)

      !------------------------------------!
      !      INITIALIZATION TIME==ZERO     !
      !------------------------------------!
      IF(TIME==ZERO)THEN
       DO I=1,NEL
         P = MAX(EM30,(-SIGOXX(I)-SIGOYY(I)-SIGOZZ(I))*THIRD)-PSH         
         IF(GAM*C1>=EM30)THEN !if Liquid and gas correctly defined
          MU1P1     = ((P-P0)/C1+ONE)
          MU2P1     =( P/P0)**(ONE/GAM)
          RHO1      = RHO10*MU1P1
          RHO2      = RHO20*MU2P1
          A         = (RHO(I)-RHO2)/(RHO1-RHO2)
          UVAR(I,1) = A*RHO1
          UVAR(I,2) = RHO2
          UVAR(I,3) = RHO1
          UVAR(I,4) = A
          IF(UVAR(I,4)<EM20)UVAR(I,4)=ZERO
          UVAR(I,5) = ONE-UVAR(I,4)
         ELSE !boundary element
           UVAR(I,3)=RHO(I)
         ENDIF
       ENDDO
      ELSE
        DO I=1,NEL
          UVAR(I,1) = UVAR(I,1)/volume(I)      !updated in aleconve.F 
        ENDDO
      ENDIF

      !------------------------------------!
      ! CASE OF BOUNDARY ELEMENT INPUT     !
      ! C1=0:not a fluid ; gam=0:not a gas !
      !------------------------------------!
      IF(GAM*C1<EM30)THEN
       DO I=1,NEL
        IF(UVAR(I,3)/RHO10<HALF)THEN
         UVAR(I,1) = ZERO !massic percentage of liquid
         RHO(I)    = UVAR(I,3)*RHOSCALE
         UVAR(I,2) = RHO(I)
         UVAR(I,4) = ZERO  ! no mass of liquid then no volume
         UVAR(I,5) = ONE
        ELSE
         UVAR(I,1) = UVAR(I,3)
         UVAR(I,2) = RHO20
         RHO(I)    = UVAR(I,3)
         UVAR(I,4) = ONE
         UVAR(I,5) = ZERO
        ENDIF                                   
        SOUNDSP(I) = EM30
        SIGNXX(I)  = SIGOXX(I)
        SIGNYY(I)  = SIGOYY(I)
        SIGNZZ(I)  = SIGOZZ(I)
        SIGNXY(I)  = SIGOXY(I)
        SIGNYZ(I)  = SIGOYZ(I)
        SIGNZX(I)  = SIGOZX(I)
       ENDDO
       RETURN
      ENDIF

      IF(ISOLVER==2)THEN
        !------------------------------------!
        !     ALTERNATIVE SOLVER (Newton)    !
        !------------------------------------!
        TOL = EM10
        NITER = 20
        DO I=1,NEL
          VOL = VOLUME(I)
          MAS = RHO(I) * VOL
          MAS1 = UVAR(I, 1) * VOL
          MAS2 = MAS - MAS1
          RHO2 = UVAR(I, 2)
          RHO1 = UVAR(I, 3)
          RHO1T = RHO1 / RHO10
          RHO2T = RHO2 / RHO20
          POLD = P0 * RHO2T**GAM
          IF (MAS1 / MAS < EM10) THEN
             !! Phase 2 only is present
             UVAR(I, 1) = ZERO
             UVAR(I, 4) = ZERO
             UVAR(I, 5) = ONE
             RHO2 = MAS / VOL
             UVAR(I, 2) = RHO2
             P = P0 * (RHO2/RHO20)**GAM
          ELSEIF (MAS2 / MAS < EM10) THEN
             !! Phase 1 only is present
             RHO1 = MAS / VOL
             UVAR(I, 1) = RHO1
             UVAR(I, 3) = RHO1
             UVAR(I, 4) = ONE
             UVAR(I, 5) = ZERO
             P = R1 * RHO1 - C1 + P0
          ELSE
             ERROR = EP30
             ITER = 1
             DO WHILE(ITER < NITER .AND. ERROR > TOL)
                P1 = R1 * RHO1 - C1 + P0
                P2 = P0 * (RHO2/RHO20)**GAM
                F1 = MAS1 / RHO1 + MAS2 / RHO2 - VOL
                F2 = P1 - P2
                DF11 = - MAS1 / (RHO1 * RHO1)
                DF12 = - MAS2 / (RHO2 * RHO2)
                DF21 = R1
                DF22 = - GAM * P0 / (RHO20**GAM) * RHO2**(GAM - ONE)
                DET = DF11 * DF22 - DF12 * DF21
                DRHO1 = (-DF22 * F1 + DF12 * F2) / DET
                DRHO2 = (DF21 * F1 - DF11 * F2) / DET
                DRHO1 = MIN(THREE * RHO1, MAX(DRHO1, - HALF * RHO1))
                DRHO2 = MIN(THREE * RHO2, MAX(DRHO2, - HALF * RHO2))
                RHO1 = RHO1 + DRHO1
                RHO2 = RHO2 + DRHO2
                ERROR = ABS(DRHO1 / RHO1) + ABS(DRHO2 / RHO2)
                ITER = ITER + 1
             ENDDO
             IF (ERROR > TOL) THEN
                PRINT*, "*** WARNING LAW37, convergence tolerance ", ERROR, TOL
             ENDIF
             P = R1 * RHO1 - C1 + P0
          ENDIF
          SSP1 = R1 * RHO1
          SSP2 = GAM * P0 * (RHO2/RHO20)**GAM 
          B1 = UVAR(I, 1)
          B2 = RHO(I) - B1
          !---output
          UVAR(I,2)  = RHO2
          UVAR(I,3)  = RHO1
          UVAR(I,4)  = UVAR(I,1)/RHO1 
          IF(UVAR(I,4)<EM20)UVAR(I,4)=ZERO
          UVAR(I,5)  = ONE-UVAR(I,4)
          IF (SSP1 > ZERO) THEN
             SSP1 = UVAR(I,4) / SSP1
          ELSE
             SSP1 = ZERO
          ENDIF
          IF (SSP2 > ZERO) THEN
             SSP2 = UVAR(I,5) / SSP2
          ELSE
             SSP2 = ZERO
          ENDIF
          SSP = SSP1 + SSP2
          SSP = SQRT(ONE / SSP / RHO(I))
          P          = MAX(PMIN, P) + PSH
          SIGNXX(I)  = -P
          SIGNYY(I)  = -P
          SIGNZZ(I)  = -P
          VIS        = (B1*RHO1*VISA1 + B2*RHO2*VISA2)/RHO(I)
          VIS2       = TWO*VIS
          VIS3       = (B1*RHO1*VISB1 + B2*RHO2*VISB2)/RHO(I)
          VV         = VIS3*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
          SIGVXX(I)  = VIS2*EPSPXX(I)+VV
          SIGVYY(I)  = VIS2*EPSPYY(I)+VV
          SIGVZZ(I)  = VIS2*EPSPZZ(I)+VV
          SIGVXY(I)  = VIS *EPSPXY(I)
          SIGVYZ(I)  = VIS *EPSPYZ(I)
          SIGVZX(I)  = VIS *EPSPZX(I)
          SOUNDSP(I) = SSP
          VISCMAX(I) = VIS2 + VIS3 
          UVAR(I,1)  = MAX(ZERO, UVAR(I,1))
          UVAR(I,2)  = MAX(ZERO, UVAR(I,2))
          UVAR(I,3)  = MAX(ZERO, UVAR(I,3))
          UVAR(I,4)  = MAX(ZERO, UVAR(I,4))
          UVAR(I,5)  = MAX(ZERO, UVAR(I,5))
        ENDDO!next I
        
      ELSE !ISOLVER 
        !------------------------------------!
        !    LEGACY SOLVER (default)         ! 
        !------------------------------------!
        DO I=1,NEL
          RHO2       = UVAR(I,2)
          !---iter 1
          POLD       = P0 * (RHO2/RHO20)**GAM
          R2         = GAM * POLD / RHO2
          C2         = - (ONE-GAM)*POLD + P0
          C12        = C1 - C2
          B1         = UVAR(I,1)
          B2         = RHO(I) - B1
          A          = R1
          B          = HALF*(B1*R1+B2*R2+C12)
          C          = B1*C12
          RHO1       = ( B + SQRT(MAX(ZERO,B*B - A*C)) ) / A
          P          = R1*RHO1 - C1
          RHN2       = MAX(EM30,(P + C2) / R2)       
          !---iter 2
          PN2        = (POLD + P0 * (RHN2/RHO20)**GAM)
          R2         = GAM * PN2 / (RHO2+RHN2)
          B          = HALF*(B1*R1+B2*R2+C12)
          RHO1       = ( B + SQRT(MAX(ZERO,B*B - A*C)) ) / A
          P          = R1*RHO1 - C1
          RHO2 = MAX(EM30,(P + C2) / R2)         
          !---output
          UVAR(I,2)  = RHO2
          UVAR(I,3)  = RHO1
          UVAR(I,4)  = UVAR(I,1)/RHO1 
          IF(UVAR(I,4)<EM20)UVAR(I,4)=ZERO
          UVAR(I,5)  = ONE-UVAR(I,4)
          P          = MAX(PMIN, P) +P0+PSH !P is here a relative pressure
          SIGNXX(I)  = -P
          SIGNYY(I)  = -P
          SIGNZZ(I)  = -P
          VIS        = (B1*RHO1*VISA1 + B2*RHO2*VISA2)/RHO(I)
          VIS2       = TWO*VIS
          VIS3       = (B1*RHO1*VISB1 + B2*RHO2*VISB2)/RHO(I)
          VV         = VIS3*(EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))
          SIGVXX(I)  = VIS2*EPSPXX(I)+VV
          SIGVYY(I)  = VIS2*EPSPYY(I)+VV
          SIGVZZ(I)  = VIS2*EPSPZZ(I)+VV
          SIGVXY(I)  = VIS *EPSPXY(I)
          SIGVYZ(I)  = VIS *EPSPYZ(I)
          SIGVZX(I)  = VIS *EPSPZX(I)
          SOUNDSP(I) = SQRT(C1/RHO1)
          VISCMAX(I) = VIS2 + VIS3 
          UVAR(I,1)  = MAX(ZERO, UVAR(I,1))
          UVAR(I,2)  = MAX(ZERO, UVAR(I,2))
          UVAR(I,3)  = MAX(ZERO, UVAR(I,3))
          UVAR(I,4)  = MAX(ZERO, UVAR(I,4))
          UVAR(I,5)  = MAX(ZERO, UVAR(I,5))
        ENDDO!next I
      ENDIF

C-----------------------------------------------
      RETURN
      END
