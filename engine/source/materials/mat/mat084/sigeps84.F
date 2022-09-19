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
Chd|  SIGEPS84                      source/materials/mat/mat084/sigeps84.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS84 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,TEMPEL, NGL ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,YLD     ,PLA    ,
     B     DEP    ,ETSE   ,IPM     ,MAT    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
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
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,
     .        IPM(NPROPMI,*),MAT(NEL), NGL(NEL)
      my_real
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   TEMPEL(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .   SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .   SOUNDSP(NEL),VISCMAX(NEL),YLD(NEL) ,
     .   PLA(NEL),DEP(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   UVAR(NEL,NUVAR), OFF(NEL)
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .   FINTER ,TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,NITER,NINDX
      INTEGER INDX(NEL)
      my_real
     .   TOL,LAMDA,RES,DRES,SQR2INV,SEQ,YLDP,DAV,NU,YOUNG,SHEAR,G2,
     .   BULK,QVOCE,BVOCE,KSWIFT,KVOCE,KSWIFTP,KVOCEP,EXPV,
     .   K0,ALPHA,AN,EPS0,NN,DEPS0,EPSP0,CEPSP,CEPSPN,
     .   ETA,CP,TINI,TREF,TMELT,MTEMP,DEPSAD,SEFF,GEFF,F_EPS,GSS,
     .   GGSIG,G2GSIG,P12,P22,P33,G12,G22,G33,OMEGA,PLAP0
      my_real
     .   GSIG(6),PSIG(6),F_EPSP(NEL),F_TEMP(NEL),SVM(NEL),
     .   TEMP(NEL),DLAM(NEL),DLAMIN(NEL),PLAP(NEL),
     .   SIGTRXX(NEL),SIGTRYY(NEL),SIGTRZZ(NEL),
     .   SIGTRXY(NEL),SIGTRYZ(NEL),SIGTRZX(NEL)
c=========================================================================
c     State Variables
c     ---------------
c      UVAR(1)   = PLA  ----- equiv. plastic strain
c      UVAR(2)   = YLD  ----- Yield
c      UVAR(3)   = PLAP ---   Plastic Strain rate
c      UVAR(4)   = DLAM ----- Plastic Multiplier
c      UVAR(5)   = TEMP ----- Temperature
c      UVAR(6)   = H    ----- Tangent module (Yield prime)
C-----------------------------------------------
c     Plasticity model :
c      1- Quadratic Yield surface
c      2- Quadratic non associated flow rule
c      3- Isotropic strain hardening
c         a. Swift/Voce
c            k=alpha*k1(eps,p)+(1-alpha)*k2(eps,p)
c            k1=A*(eps,p+e0)**n
c            k2=Q*(1-exp(-b*eps,p))+sig0
c
c          b. Temperature Softening Johnson/Cook :
c            Temp_factor = 1-((T-Ttrans)/(Tmelt-Ttrans))**m
c
c          c. Strain Rate Hardening Johnson/Cook
c           Epsp_factor = 1 + C * ln(dEps/dEps0)
c
c     Material Properties
c     --------------------------
c     Equivalent stress
c           f(sig)=SQRT((P.sig).sig)
c     Flow potential
c           g(sig)=SQRT((G.sig).sig)
c     Hardening parameters
c
c     Calibration of P and G
c         for planar isotropic,
c             R = (R0 + 2*R45 + R90) / 4
c             P12 = -R / (1 + R)
c             P22 = 1
c             P33 = 2 * (1 + 2*R) / (1 + R)
c
c         for orthotropic
c             P12 = -R0 / (1 + R0)
c             P22 = (R0 / R90) * (1 + R90)/(1 + R0)
c             P33 = (1 + 2*R45) / R90 * (R0 + R90)/(1 + R0)
c=========================================================================
C
C     Initialize material parameters
C     --------------
      YOUNG = UPARAM(1)
	 NU    = UPARAM(2)
      SHEAR = YOUNG*HALF/(ONE+NU)
      G2    = TWO*SHEAR
      LAMDA = YOUNG*NU/(ONE+NU)/(ONE-TWO*NU)
      BULK  = YOUNG*THIRD/(ONE-TWO*NU)

      P12    = UPARAM(3)
      P22    = UPARAM(4)
      P33    = UPARAM(5)
      G12    = UPARAM(6)
      G22    = UPARAM(7)
      G33    = UPARAM(8)
      QVOCE  = UPARAM(9)
      BVOCE  = UPARAM(10)
      K0     = UPARAM(11)
      ALPHA  = UPARAM(12)

      AN     = UPARAM(13)
      EPS0   = UPARAM(14)
      NN     = UPARAM(15)
      CEPSP  = UPARAM(16)
      DEPS0  = UPARAM(17)
      CEPSPN = CEPSP

      ETA    = UPARAM(18)
      CP     = UPARAM(19)
      TINI   = UPARAM(20)
      TREF   = UPARAM(21)
      TMELT  = UPARAM(22)
      MTEMP  = UPARAM(23)
      DEPSAD = UPARAM(24)

c     Initialize state variables
C     ------------------------
      IF (ISIGI == 0 .and. TIME == ZERO) THEN
        DO I=1,NEL
          F_TEMP(I)= MIN(ONE,MAX(ZERO,((TINI-TREF)/(TMELT-TREF))**MTEMP))
          F_TEMP(I)= ONE - F_TEMP(I)
          F_EPS    = ALPHA*(AN*EPS0**NN) + (ONE-ALPHA)*K0
          YLD(I)   = F_EPS * F_TEMP(I)
c
          UVAR(I,2) = YLD(I)
          UVAR(I,5) = TINI
        ENDDO
      ENDIF
C     ------------------------
      DO I=1,NEL
        PLA(I)  = UVAR(I,1)
        YLD(I)  = UVAR(I,2)
        PLAP(I) = ZERO
        DLAM(I) = ZERO
        DEP(I)  = ZERO
      ENDDO
C
      IF (JTHE > 0 ) THEN
        DO I=1,NEL
          TEMP(I) = TEMPEL(I)
        ENDDO
      ELSE
        DO I=1,NEL
          TEMP(I) = UVAR(I,5)
        ENDDO
      ENDIF
c
C     Trial stress
C     ------------
      DO I=1,NEL
        DAV = DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)
        SIGNXX(I)  = SIGOXX(I) + DEPSXX(I)*G2 + LAMDA*DAV
        SIGNYY(I)  = SIGOYY(I) + DEPSYY(I)*G2 + LAMDA*DAV
        SIGNZZ(I)  = SIGOZZ(I) + DEPSZZ(I)*G2 + LAMDA*DAV
        SIGNXY(I)  = SIGOXY(I) + DEPSXY(I)*SHEAR
        SIGNYZ(I)  = SIGOYZ(I) + DEPSYZ(I)*SHEAR
        SIGNZX(I)  = SIGOZX(I) + DEPSZX(I)*SHEAR
        SIGTRXX(I) = SIGNXX(I)
        SIGTRYY(I) = SIGNYY(I)
        SIGTRZZ(I) = SIGNZZ(I)
        SIGTRXY(I) = SIGNXY(I)
        SIGTRYZ(I) = SIGNYZ(I)
        SIGTRZX(I) = SIGNZX(I)

        SVM(I) = SQRT(SIGNXX(I)*SIGNXX(I)
     .              + SIGNYY(I)*SIGNYY(I)*P22
     .              + SIGNZZ(I)*SIGNZZ(I)*(ONE+TWO*P12+P22)
     .              + SIGNXY(I)*SIGNXY(I)*P33
     .              + SIGNYZ(I)*SIGNYZ(I)*THREE
     .              + SIGNZX(I)*SIGNZX(I)*THREE
     .              + SIGNXX(I)*SIGNYY(I)*TWO*P12
     .              - SIGNZZ(I)*SIGNYY(I)*TWO*(P12+P22)
     .              - SIGNXX(I)*SIGNZZ(I)*TWO*(ONE+P12))
      ENDDO
c
C     Check Yield Condition  
C    ----------------------------------------------------
      NINDX  = 0
      DO I=1,NEL
        IF (SVM(I) > YLD(I)) THEN  ! Plastic Loading
          NINDX = NINDX + 1
          INDX(NINDX)  = I
        ENDIF
      ENDDO
c
C     Plastic increment
C     ----------------------------------------------------
      DO II=1,NINDX
        I = INDX(II)

C-----  Initialisation ----------------------------------------
C                                                               
        TOL     = 0.00001
        SQR2INV = ONE/SQR2
        NITER   = 5
        CEPSPN    = CEPSP
        IF( CEPSPN   == ZERO )THEN
           DLAMIN(I) = UVAR(I,4) + EM10
        ELSE
           DLAMIN(I) = FIVE*EM4*TIMESTEP*EXP((EM4)/CEPSPN)
        ENDIF
        !DLAM(I) = (SVM(I) - YLD(I)) / (SHEAR*THREE + UVAR(I,6))
        DLAM(I)   = UVAR(I,4) + EM9
        DLAM(I)   = MAX(DLAM(I), DLAMIN(I))
c
        PLAP(I)   = DLAM(I) / TIMESTEP
        F_TEMP(I) = ((TEMP(I)-TREF)/(TMELT-TREF))**MTEMP
        F_TEMP(I) = MIN(ONE,MAX(ZERO, F_TEMP(I)))
        F_TEMP(I) = ONE - F_TEMP(I)
C------------------------------------------------------------
C------------------------------------------------------------
        PSIG(1) = SIGNXX(I)         + SIGNYY(I)*P12-SIGNZZ(I)*(ONE+P12)
        PSIG(2) = SIGNXX(I)*P12     + SIGNYY(I)*P22-SIGNZZ(I)*(P12+P22)
        PSIG(3) =-SIGNXX(I)*(ONE+P12)- SIGNYY(I)*(P12+P22)
     .                               + SIGNZZ(I)*(ONE+TWO*P12+P22)
        PSIG(4) = SIGNXY(I)*P33
        PSIG(5) = SIGNYZ(I)*THREE
        PSIG(6) = SIGNZX(I)*THREE

        GGSIG   = DLAM(I)*SHEAR/SVM(I)
        G2GSIG  = GGSIG*TWO
        SIGNXX(I) = SIGTRXX(I) - PSIG(1)*G2GSIG
        SIGNYY(I) = SIGTRYY(I) - PSIG(2)*G2GSIG
        SIGNZZ(I) = SIGTRZZ(I) - PSIG(3)*G2GSIG
        SIGNXY(I) = SIGTRXY(I) - PSIG(4)*GGSIG
        SIGNYZ(I) = SIGTRYZ(I) - PSIG(5)*GGSIG
        SIGNZX(I) = SIGTRZX(I) - PSIG(6)*GGSIG
C------------------------------------------------------------
c-------- NEWTON ITERATIONS ------
C------------------------------------------------------------
        DO K=1,NITER
          !HILL
          SEFF = SQRT(SIGNXX(I)*SIGNXX(I)
     .            + SIGNYY(I)*SIGNYY(I)*P22
     .            + SIGNZZ(I)*SIGNZZ(I)*(ONE+TWO*P12+P22)
     .            + SIGNXY(I)*SIGNXY(I)*P33
     .            + SIGNYZ(I)*SIGNYZ(I)*THREE
     .            + SIGNZX(I)*SIGNZX(I)*THREE
     .            + SIGNXX(I)*SIGNYY(I)*TWO*P12
     .            - SIGNZZ(I)*SIGNYY(I)*TWO*(P12+P22)
     .            - SIGNXX(I)*SIGNZZ(I)*TWO*(ONE+P12))
          SEFF = MAX(EM20,SEFF)
          !FLOW FUNCTION G
          GEFF   = SQRT(SIGNXX(I)*SIGNXX(I)
     .            + SIGNYY(I)*SIGNYY(I)*G22
     .            + SIGNZZ(I)*SIGNZZ(I)*(ONE+TWO*G12+G22)
     .            + SIGNXY(I)*SIGNXY(I)*G33
     .            + SIGNYZ(I)*SIGNYZ(I)*THREE
     .            + SIGNZX(I)*SIGNZX(I)*THREE
     .            + SIGNXX(I)*SIGNYY(I)*TWO*G12
     .            - SIGNZZ(I)*SIGNYY(I)*TWO*(G12+G22)
     .            - SIGNXX(I)*SIGNZZ(I)*TWO*(ONE+G12))
          GEFF = MAX(EM20,GEFF)
c
          GSS = GEFF/SEFF
c
          ! P * SIGMA
          PSIG(1) = SIGNXX(I)          + SIGNYY(I)*P12 - SIGNZZ(I)*(ONE+P12)
          PSIG(2) = SIGNXX(I)*P12      + SIGNYY(I)*P22 - SIGNZZ(I)*(P12+P22)
          PSIG(3) =-SIGNXX(I)*(ONE+P12) - SIGNYY(I)*(P12+P22)
     .                                 + SIGNZZ(I)*(ONE+TWO*P12+P22)
          PSIG(4) = SIGNXY(I)*P33
          PSIG(5) = SIGNYZ(I)*THREE
          PSIG(6) = SIGNZX(I)*THREE
          ! G * SIGMA
          GSIG(1) = SIGNXX(I)          + SIGNYY(I)*G12 - SIGNZZ(I)*(ONE+G12)
          GSIG(2) = SIGNXX(I)*G12      + SIGNYY(I)*G22 - SIGNZZ(I)*(G12+G22)
          GSIG(3) =-SIGNXX(I)*(ONE+G12) - SIGNYY(I)*(G12+G22)
     .                                 + SIGNZZ(I)*(ONE+TWO*G12+G22)
          GSIG(4) = SIGNXY(I)*G33
          GSIG(5) = SIGNYZ(I)*THREE
          GSIG(6) = SIGNZX(I)*THREE
c
c-----    Yield, Yield prime
          EXPV   = EXP(-BVOCE*PLA(I))
          KSWIFT = AN*(PLA(I) + EPS0)**NN
          KVOCE  = K0 + QVOCE*(ONE - EXPV)
          F_EPS  = ALPHA*KSWIFT + (ONE-ALPHA)*KVOCE
          UVAR(I,2) = F_EPS*F_TEMP(I)
          YLD(I)    = UVAR(I,2)
c-----
          F_EPSP(I) = ONE + CEPSPN*LOG(PLAP(I)/DEPS0)
          KSWIFTP = KSWIFT*NN / (PLA(I) + EPS0)
          KVOCEP  = QVOCE*BVOCE*EXPV
          YLDP    = ALPHA*KSWIFTP + (ONE - ALPHA)*KVOCEP
          YLDP    = YLDP * F_TEMP(I) * F_EPSP(I)
          YLDP    = YLDP + YLD(I) * CEPSPN / DLAM(I)
          !UVAR(I,6)=  YLDP
          YLD(I)  = UVAR(I,2) * F_EPSP(I)
c         f(sig)-yield
          RES  = SEFF - YLD(I)
c
          ! Check for convergence
          IF ( (ABS(RES) < TOL*UVAR(I,2))     .and.
     +         (YLD(I) > UVAR(I,2)*(ONE-TOL))  .and.
     +         ( PLAP(I) > DEPS0  )           .and.
     +         (ABS(PLAP(I)-PLAP0) <  PLAP0 *TOL)
     +     ) EXIT
c
          ! Update d_lambda & plastic strain
c
          DRES = PSIG(1)*GSIG(1) + PSIG(2)*GSIG(2) + PSIG(3)*GSIG(3)
     .     + HALF*(PSIG(4)*GSIG(4)+PSIG(5)*GSIG(5)+PSIG(6)*GSIG(6))
          DRES = DRES / (SEFF*GEFF)
          DRES = -G2 * DRES  - YLDP  * GSS
          DLAM(I)   = DLAM(I) - RES / DRES
          UVAR(I,4) = DLAM(I)
          ! Check for convergence and Strain Rate > Reference Strain Rate
          IF ( (ABS(RES) < TOL*UVAR(I,2)) .and.
     +         (GSS*DLAM(I) / TIMESTEP < DEPS0) ) THEN
            CEPSPN    = ZERO
            DLAM(I)   = UVAR(I,4) + EM10
            DLAMIN(I) = EM20
          END IF
c
          PLAP0     = PLAP(I)
          DLAM(I)   = MAX(DLAM(I), DLAMIN(I))
          DEP(I)    = DLAM(I) * GSS
          PLA(I)    = UVAR(I,1) + DEP(I)
          PLAP(I)   = DEP(I) / TIMESTEP
          UVAR(I,4) = DLAM(I)
C 
          GGSIG   = DLAM(I)*SHEAR/GEFF
          G2GSIG  = GGSIG*TWO
          SIGNXX(I) = SIGTRXX(I) - GSIG(1)*G2GSIG
          SIGNYY(I) = SIGTRYY(I) - GSIG(2)*G2GSIG
          SIGNZZ(I) = SIGTRZZ(I) - GSIG(3)*G2GSIG
          SIGNXY(I) = SIGTRXY(I) - GSIG(4)*GGSIG
          SIGNYZ(I) = SIGTRYZ(I) - GSIG(5)*GGSIG
          SIGNZX(I) = SIGTRZX(I) - GSIG(6)*GGSIG

c
        END DO  ! K=1,NITER
c-----------    end Newton iterations
c
      ENDDO     ! plastic increment loop
c
c----------
C
C     Calculate Plastic Work / Temperature depending on omega
C     ---------------------------------
      IF (JTHE == 0 ) THEN
        DO II=1,NINDX
          I = INDX(II)
          IF (PLAP(I) <= DEPS0)THEN
            OMEGA = ZERO
          ELSEIF (PLAP(I) > DEPSAD) THEN
            OMEGA = ONE
          ELSE
            OMEGA = ((PLAP(I)-DEPS0)**2 )*
     .              (THREE*DEPSAD - TWO*PLAP(I) - DEPS0)
     .             /((DEPSAD-DEPS0)**3)
          ENDIF
          TEMP(I) = UVAR(I,5) + OMEGA*YLD(I)*DEP(I)*ETA/(RHO0(I)*CP)
        ENDDO
      ENDIF
C-----------
      DO I=1,NEL
        UVAR(I,1) = PLA(I)
        UVAR(I,2) = YLD(I)
        UVAR(I,3) = PLAP(I)
        UVAR(I,4) = DLAM(I)
        UVAR(I,5) = TEMP(I)
        ETSE(I)   = ONE
        SOUNDSP(I)= SQRT((BULK+FOUR_OVER_3*SHEAR)/RHO0(I))
        VISCMAX(I)= ZERO
      ENDDO
c-----------
      RETURN
      END
