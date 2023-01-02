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
Chd|  MAT104_NODAM_NICE             source/materials/mat/mat104/mat104_nodam_nice.F
Chd|-- called by -----------
Chd|        SIGEPS104                     source/materials/mat/mat104/sigeps104.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAT104_NODAM_NICE(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,TEMPEL  ,TEMP    ,SEQ     ,INLOC   ,
     8     DPLANL  )
      !=======================================================================
      !      Implicit types
      !=======================================================================
#include      "implicit_f.inc"
      !=======================================================================
      !      Common
      !=======================================================================
#include      "param_c.inc" 
#include      "com01_c.inc"
#include      "scr17_c.inc"
      !=======================================================================
      !      Dummy arguments
      !=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,JTHE,INLOC
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO0,RHO,TEMPEL,DPLANL,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,EPSD,OFF,TEMP,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
c
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,K,II,NSP,NINDX,INDEX(NEL),NICE
c
      my_real 
     .   YOUNG,BULK,LAM,G,G2,NU,CDR,KDR,HARD,YLD0,QVOCE,BVOCE,JCC,
     .   EPSP0,MTEMP,TINI,TREF,ETA,CP,DPIS,DPAD,ASRATE,AFILTR
      my_real 
     .   H,LDAV,TRDEP,TRDFDS,SIGVM,SIGDR2,YLD2I,OMEGA,
     .   DTEMP,FCOSH,FSINH,DPDT,DLAM,DDEP
      my_real 
     .   DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX,DSDRDJ2,DSDRDJ3,
     .   DJ3DSXX,DJ3DSYY,DJ3DSZZ,DJ3DSXY,DJ3DSYZ,DJ3DSZX,
     .   DJ2DSXX,DJ2DSYY,DJ2DSZZ,DJ2DSXY,DJ2DSYZ,DJ2DSZX,
     .   DFDSXX,DFDSYY,DFDSZZ,DFDSXY,DFDSYZ,DFDSZX,
     .   NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,
     .   DENOM,DPHI,DPHI_DTRSIG,SIG_DFDSIG,DFDSIG2,
     .   DPHI_DSIG,DPHI_DYLD,DPHI_DFDR,DPDT_NL,
     .   DYLD_DPLA,DYLD_DTEMP,DTEMP_DLAM,NORMSIG
c
      my_real, DIMENSION(NEL) ::
     .   DSIGXX,DSIGYY,DSIGZZ,DSIGXY,DSIGYZ,DSIGZX,TRSIG,
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,SIGM,J2,J3,SIGDR,YLD,WEITEMP,
     .   HARDP,FHARD,FRATE,FTHERM,FDR,PHI0,PHI,DPLA_DLAM
      !=======================================================================
      !       DRUCKER MATERIAL LAW WITH NO DAMAGE
      !=======================================================================
      !UVAR(1)   PHI    YIELD FUNCTION VALUE
      !-  
      !DEPIJ     PLASTIC STRAIN TENSOR COMPONENT
      !DEPSIJ    TOTAL   STRAIN TENSOR COMPONENT (EL+PL)
      !=======================================================================
c
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! Elastic parameters      
      YOUNG   = UPARAM(1)  ! Young modulus
      BULK    = UPARAM(2)  ! Bulk modulus
      G       = UPARAM(3)  ! Shear modulus
      G2      = UPARAM(4)  ! 2*Shear modulus
      LAM     = UPARAM(5)  ! Lambda Hooke parameter
      NU      = UPARAM(6)  ! Poisson ration
      ! Plastic criterion and hardening parameters [Drucker, 1948]
      NICE    = NINT(UPARAM(11)) ! Choice of the Nice method
      CDR     = UPARAM(12) ! Drucker coefficient
      KDR     = UPARAM(13) ! Drucker 1/K coefficient
      TINI    = UPARAM(14) ! Initial temperature
      HARD    = UPARAM(15) ! Linear hardening
      YLD0    = UPARAM(16) ! Initial yield stress
      QVOCE   = UPARAM(17) ! 1st Voce parameter
      BVOCE   = UPARAM(18) ! 2nd Voce parameter
      ! Strain-rate dependence parameters
      JCC     = UPARAM(20) ! Johnson-Cook strain rate coefficient
      EPSP0   = UPARAM(21) ! Johnson-Cook reference strain rate
      ! Thermal softening and self-heating parameters
      MTEMP   = UPARAM(22) ! Thermal softening slope
      TREF    = UPARAM(23) ! Reference temperature
      ETA     = UPARAM(24) ! Taylor-Quinney coefficient
      CP      = UPARAM(25) ! Thermal mass capacity
      DPIS    = UPARAM(26) ! Isothermal plastic strain rate
      DPAD    = UPARAM(27) ! Adiabatic plastic strain rate
      ! Plastic strain-rate filtering parameters
      ASRATE  = UPARAM(28) ! Plastic strain rate filtering frequency
      AFILTR  = ASRATE*TIMESTEP/(ASRATE*TIMESTEP + ONE)
c
      ! Recovering internal variables
      DO I=1,NEL
        IF (OFF(I) < 0.1) OFF(I) = ZERO
        IF (OFF(I) < ONE)  OFF(I) = OFF(I)*FOUR_OVER_5
        ! User inputs
        PHI0(I)  = UVAR(I,1) ! Previous yield function value
        ! Standard inputs
        DPLA(I)  = ZERO      ! Initialization of the plastic strain increment
        ET(I)    = ONE        ! Initialization of hourglass coefficient
        HARDP(I) = ZERO      ! Initialization of hourglass coefficient
      ENDDO    
c      
      ! Initialization of temperature and self-heating weight factor
      IF (TIME == ZERO) THEN   
        TEMP(1:NEL)    = TINI  
      ENDIF 
      IF (CP > ZERO) THEN     
        IF (JTHE == 0) THEN  
          IF (INLOC == 0) THEN   
            DO I=1,NEL
              IF (EPSD(I) < DPIS) THEN
                WEITEMP(I) = ZERO
              ELSEIF (EPSD(I) > DPAD) THEN
                WEITEMP(I) = ONE
              ELSE
                WEITEMP(I) = ((EPSD(I)-DPIS)**2 )
     .                  * (THREE*DPAD - TWO*EPSD(I) - DPIS)
     .                  / ((DPAD-DPIS)**3)
              ENDIF
            ENDDO
          ENDIF
        ELSE
          TEMP(1:NEL) = TEMPEL(1:NEL)
        ENDIF
      ENDIF
c      
      ! Computation of the initial yield stress
      DO I = 1,NEL
        ! a) - Hardening law
        FHARD(I) = YLD0 + HARD*PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
        ! b) - Correction factor for strain-rate dependence (Johnson-Cook)
        FRATE(I)  = ONE
        IF (EPSD(I) > EPSP0) FRATE(I) = FRATE(I) + JCC*LOG(EPSD(I)/EPSP0)   
        ! c) - Correction factor for thermal softening      
        FTHERM(I) = ONE
        IF (CP > ZERO) FTHERM(I) = ONE - MTEMP * (TEMP(I) - TREF)
        ! d) - Computation of the yield function and value check
        YLD(I) = FHARD(I)*FRATE(I)*FTHERM(I)
        ! e) - Checking values
        YLD(I) = MAX(EM10, YLD(I))
      ENDDO     
c      
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
        ! Computation of the trial stress tensor
        LDAV = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAM
        SIGNXX(I) = SIGOXX(I) + DEPSXX(I)*G2 + LDAV
        SIGNYY(I) = SIGOYY(I) + DEPSYY(I)*G2 + LDAV
        SIGNZZ(I) = SIGOZZ(I) + DEPSZZ(I)*G2 + LDAV
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*G
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*G
        ! Computation of the trace of the trial stress tensor
        TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
        SIGM(I)   = -TRSIG(I) * THIRD
        ! Computation of the deviatoric trial stress tensor
        SXX(I) = SIGNXX(I) + SIGM(I)
        SYY(I) = SIGNYY(I) + SIGM(I)
        SZZ(I) = SIGNZZ(I) + SIGM(I)
        SXY(I) = SIGNXY(I)
        SYZ(I) = SIGNYZ(I)
        SZX(I) = SIGNZX(I)
        ! Second deviatoric invariant
        J2(I) = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .          +       SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        ! Third deviatoric invariant
        J3(I) = SXX(I) * SYY(I) * SZZ(I)  
     .        + SXY(I) * SYZ(I) * SZX(I) * TWO
     .        - SXX(I) * SYZ(I)**2
     .        - SYY(I) * SZX(I)**2
     .        - SZZ(I) * SXY(I)**2 
        ! Drucker equivalent stress
        FDR(I) = J2(I)**3 - CDR*(J3(I)**2)
        ! Checking equivalent stress values
        IF (FDR(I) > ZERO) THEN
          SIGDR(I) = KDR * EXP((ONE/SIX)*LOG(FDR(I)))  ! FDR(I)**(1/6)
        ELSE
          SIGDR(I) = ZERO
        ENDIF
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !========================================================================
      PHI(1:NEL) = (SIGDR(1:NEL) / YLD(1:NEL))**2 - ONE
c
      ! Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL         
        IF ((PHI(I)>ZERO).AND.(OFF(I) == ONE)) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO
c      
      !====================================================================
      ! - PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !==================================================================== 
#include "vectorize.inc" 
      DO II = 1, NINDX  
c      
        ! Number of the element with plastic behaviour                                          
        I = INDEX(II)    
c        
        ! Computation of the trial stress increment
        LDAV = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAM
        DSIGXX(I) = DEPSXX(I)*G2 + LDAV
        DSIGYY(I) = DEPSYY(I)*G2 + LDAV
        DSIGZZ(I) = DEPSZZ(I)*G2 + LDAV
        DSIGXY(I) = DEPSXY(I)*G   
        DSIGYZ(I) = DEPSYZ(I)*G     
        DSIGZX(I) = DEPSZX(I)*G   
        DLAM      = ZERO        
c
        ! Computation of Drucker equivalent stress of the previous stress tensor
        TRSIG(I)  = SIGOXX(I) + SIGOYY(I) + SIGOZZ(I)
        SIGM(I)   = -TRSIG(I) * THIRD
        SXX(I)    = SIGOXX(I) + SIGM(I)
        SYY(I)    = SIGOYY(I) + SIGM(I)
        SZZ(I)    = SIGOZZ(I) + SIGM(I)
        SXY(I)    = SIGOXY(I)
        SYZ(I)    = SIGOYZ(I)
        SZX(I)    = SIGOZX(I)
        J2(I)     = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .          +         SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        J3(I)     = SXX(I)*SYY(I)*SZZ(I) + SXY(I)*SYZ(I)*SZX(I) * TWO
     .          - SXX(I)*SYZ(I)**2 - SYY(I)*SZX(I)**2 - SZZ(I)*SXY(I)**2          
        FDR(I)    = J2(I)**3 - CDR*(J3(I)**2)
        SIGDR(I)  = KDR * EXP((ONE/SIX)*LOG(FDR(I)))        
c        
        ! Note     : in this part, the purpose is to compute for each iteration
        ! a plastic multiplier allowing to update internal variables to satisfy
        ! the consistency condition. 
        ! Its expression is : DLAMBDA = - (PHI + DPHI) / DPHI_DLAMBDA
	! -> PHI  : current value of yield function (known)
        ! -> DPHI : yield function prediction (to compute)
        ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
        !                   into account of internal variables kinetic : 
        !                   plasticity, temperature and damage (to compute)
c        
	! 1 - Computation of DPHI_DSIG the normal to the yield surface
        !-------------------------------------------------------------
c        
        ! Computation of the normal to the yield criterion NORM
        ! flow direction (derivative dPhi / Dsig) 
        ! HERE WE COMPUTE DPHI/DS with phi = (sigbar/sigy)**2 - 1       
        ! on note dphi/dS=dF/dS                                        
        ! Fdr = J2^3 - c*J3^2                                     
        ! dPhi/dSig = dPhi/dFdr * (dFdr/dJ2 * dJ2/dSig + dFdr/dJ3 * dJ3/dSig)
        ! dJ2/dSig = S          
c        
        ! Derivative with respect to the equivalent stress
        YLD2I       = ONE/(YLD(I)**2)
        DPHI_DSIG   = TWO*SIGDR(I)*YLD2I 
c        
        ! Computation of the Eulerian norm of the stress tensor
        NORMSIG = SQRT(SIGOXX(I)*SIGOXX(I)
     .          + SIGOYY(I)*SIGOYY(I)
     .          + SIGOZZ(I)*SIGOZZ(I)  
     .          + TWO*SIGOXY(I)*SIGOXY(I)
     .          + TWO*SIGOYZ(I)*SIGOYZ(I)
     .          + TWO*SIGOZX(I)*SIGOZX(I))       
c     
        ! Derivative with respect to Fdr 
        FDR(I)     =  (J2(I)/(NORMSIG**2))**3 - CDR*((J3(I)/(NORMSIG**3))**2)       
        DPHI_DFDR  =  DPHI_DSIG*KDR*(ONE/SIX)*EXP(-(FIVE/SIX)*LOG(FDR(I)))             
        DSDRDJ2    =  DPHI_DFDR*THREE*(J2(I)/(NORMSIG**2))**2                             
        DSDRDJ3    = -DPHI_DFDR*TWO*CDR*(J3(I)/(NORMSIG**3))                    
        ! dJ3/dSig                                                        
        DJ3DSXX =  TWO_THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .           -  THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .           -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)                 
        DJ3DSYY = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .           + TWO_THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .           -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)             
        DJ3DSZZ = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .            - THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .           + TWO_THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2) 
        DJ3DSXY = TWO*(SXX(I)*SXY(I) + SXY(I)*SYY(I) + SZX(I)*SYZ(I))/(NORMSIG**2)                   
        DJ3DSYZ = TWO*(SXY(I)*SZX(I) + SYY(I)*SYZ(I) + SYZ(I)*SZZ(I))/(NORMSIG**2)                     
        DJ3DSZX = TWO*(SXX(I)*SZX(I) + SXY(I)*SYZ(I) + SZX(I)*SZZ(I))/(NORMSIG**2)                      
        ! dPhi/dSig
        NORMXX  = DSDRDJ2*SXX(I)/NORMSIG + DSDRDJ3*DJ3DSXX
        NORMYY  = DSDRDJ2*SYY(I)/NORMSIG + DSDRDJ3*DJ3DSYY
        NORMZZ  = DSDRDJ2*SZZ(I)/NORMSIG + DSDRDJ3*DJ3DSZZ
        NORMXY  = TWO*DSDRDJ2*SXY(I)/NORMSIG + DSDRDJ3*DJ3DSXY
        NORMYZ  = TWO*DSDRDJ2*SYZ(I)/NORMSIG + DSDRDJ3*DJ3DSYZ
        NORMZX  = TWO*DSDRDJ2*SZX(I)/NORMSIG + DSDRDJ3*DJ3DSZX 
c    
        ! Restoring previous value of the yield function
        PHI(I) = PHI0(I)
c
        ! Computation of yield surface trial increment DPHI       
        DPHI = NORMXX * DSIGXX(I)  
     .       + NORMYY * DSIGYY(I)  
     .       + NORMZZ * DSIGZZ(I)  
     .       + NORMXY * DSIGXY(I)
     .       + NORMYZ * DSIGYZ(I)
     .       + NORMZX * DSIGZX(I)
c        
        ! 2 - Computation of DPHI_DLAMBDA
        !---------------------------------------------------------
c        
        !   a) Derivative with respect stress increments tensor DSIG			
        !   --------------------------------------------------------
        DFDSIG2 = NORMXX * NORMXX * G2
     .          + NORMYY * NORMYY * G2
     .          + NORMZZ * NORMZZ * G2
     .          + NORMXY * NORMXY * G
     .          + NORMYZ * NORMYZ * G
     .          + NORMZX * NORMZX * G
c
        !   b) Derivatives with respect to plastic strain P 			
        !   ------------------------------------------------
c        
        !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
        !     ----------------------------------------------------------------------------
        HARDP(I)   = HARD + QVOCE*BVOCE*EXP(-BVOCE*PLA(I))      
        DYLD_DPLA  = HARDP(I)*FRATE(I)*FTHERM(I)
c        
        !     ii) Derivative of dPLA with respect to DLAM
        !     -------------------------------------------
        SIG_DFDSIG = SIGNXX(I) * NORMXX
     .             + SIGNYY(I) * NORMYY
     .             + SIGNZZ(I) * NORMZZ
     .             + SIGNXY(I) * NORMXY
     .             + SIGNYZ(I) * NORMYZ
     .             + SIGNZX(I) * NORMZX         
        DPLA_DLAM(I) = SIG_DFDSIG / YLD(I)   
c        
        !  c) Derivatives with respect to the temperature TEMP 
        !  ---------------------------------------------------
        IF (JTHE == 0 .AND. CP > ZERO .AND. INLOC == 0) THEN
        !    i)  Derivative of the yield stress with respect to temperature dYLD / dTEMP
        !    ---------------------------------------------------------------------------          
          DYLD_DTEMP = -FHARD(I)*FRATE(I)*MTEMP
        !    ii) Derivative of the temperature TEMP with respect to DLAM
        !    -----------------------------------------------------------
          DTEMP_DLAM = WEITEMP(I)*(ETA/(RHO0(I)*CP))*SIG_DFDSIG
        ELSE
          DYLD_DTEMP = ZERO
          DTEMP_DLAM = ZERO
        ENDIF
c        
        !  d) Derivative with respect to the yield stress
        !  ----------------------------------------------
        SIGDR2    =  SIGDR(I)**2                        
        DPHI_DYLD = -TWO*SIGDR2/(YLD(I)**3)
c        
        !  e) Derivative of PHI with respect to DLAM ( = -DENOM)
        DENOM = DFDSIG2 - (DPHI_DYLD * DYLD_DPLA * DPLA_DLAM(I))
        IF (JTHE == 0 .AND. CP > ZERO .AND. INLOC == 0) THEN
          DENOM = DENOM - (DPHI_DYLD * DYLD_DTEMP * DTEMP_DLAM)
        ENDIF
        DENOM = SIGN(MAX(ABS(DENOM),EM20) ,DENOM)   
c        
        ! 3 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------
c
        ! Computation of the plastic multiplier
        DLAM    = (DPHI + PHI(I)) / DENOM
        DLAM    = MAX(DLAM, ZERO)
c        
	! Plastic strains tensor update
        DPXX = DLAM * NORMXX
        DPYY = DLAM * NORMYY
        DPZZ = DLAM * NORMZZ
        DPXY = DLAM * NORMXY
        DPYZ = DLAM * NORMYZ
        DPZX = DLAM * NORMZX  
c        
        ! Elasto-plastic stresses update   
        SIGNXX(I) = SIGOXX(I) + DSIGXX(I) - DPXX*G2
        SIGNYY(I) = SIGOYY(I) + DSIGYY(I) - DPYY*G2
        SIGNZZ(I) = SIGOZZ(I) + DSIGZZ(I) - DPZZ*G2
        SIGNXY(I) = SIGOXY(I) + DSIGXY(I) - DPXY*G
        SIGNYZ(I) = SIGOYZ(I) + DSIGYZ(I) - DPYZ*G
        SIGNZX(I) = SIGOZX(I) + DSIGZX(I) - DPZX*G
        TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
        SIGM(I)   = -TRSIG(I) * THIRD
        SXX(I)    = SIGNXX(I) + SIGM(I)
        SYY(I)    = SIGNYY(I) + SIGM(I)
        SZZ(I)    = SIGNZZ(I) + SIGM(I)
        SXY(I)    = SIGNXY(I)
        SYZ(I)    = SIGNYZ(I)
        SZX(I)    = SIGNZX(I)
c        
        ! Cumulated plastic strain and strain rate update
        DDEP    = (DLAM/YLD(I))*SIG_DFDSIG
        DPLA(I) = DPLA(I) + DDEP 
        PLA(I)  = PLA(I)  + DDEP
c
        ! Drucker equivalent stress update
        J2(I)    = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .             +         SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        J3(I)    = SXX(I) * SYY(I) * SZZ(I) + TWO   * SXY(I) * SYZ(I) * SZX(I)
     .             - SXX(I) * SYZ(I)**2 - SYY(I) * SZX(I)**2 - SZZ(I) * SXY(I)**2 
        FDR(I)   = J2(I)**3 - CDR*(J3(I)**2)
        SIGDR(I) = KDR * EXP((ONE/SIX)*LOG(FDR(I)))
c
        ! Temperature update
        IF (JTHE == 0 .AND. CP > ZERO .AND. INLOC == 0) THEN 
          DTEMP     = WEITEMP(I)*YLD(I)*DDEP*ETA/(RHO0(I)*CP)
          TEMP(I)   = TEMP(I) + DTEMP
          FTHERM(I) = ONE - MTEMP* (TEMP(I) - TREF)
        ENDIF
c
        ! Hardening law update
        FHARD(I) = YLD0 + HARD * PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
c        
        ! Yield stress update
        YLD(I) = FHARD(I) * FRATE(I) * FTHERM(I)
        YLD(I) = MAX(YLD(I), EM10)
c
        ! Yield function value update
        SIGDR2 = SIGDR(I)**2
        YLD2I  = ONE / YLD(I)**2
        PHI(I) = SIGDR2 * YLD2I - ONE       
      ENDDO 
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !===================================================================
c
      ! Storing new values
      DO I=1,NEL
        ! USR Outputs
        UVAR(I,1)  = PHI(I)   ! Yield function value
        SEQ(I)     = SIGDR(I) ! SIGEQ
        DPDT       = DPLA(I) / MAX(EM20,TIMESTEP)
        ! Plastic strain-rate (filtered)
        EPSD(I)    = AFILTR * DPDT + (ONE - AFILTR) * EPSD(I)
        ! Coefficient for hourglass
        ET(I)      = HARDP(I)*FRATE(I) / (HARDP(I)*FRATE(I) + YOUNG)
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT((BULK + FOUR_OVER_3*G)/RHO0(I))
        ! Storing the yield stress
        SIGY(I)    = YLD(I)
        ! Non-local temperature
        IF (INLOC > 0) THEN 
          IF (CP > ZERO .AND. JTHE == 0) THEN     
            ! Computation of the weight factor
            DPDT_NL = MAX(DPLANL(I),ZERO)/MAX(TIMESTEP,EM20)
            IF (DPDT_NL < DPIS) THEN
              WEITEMP(I) = ZERO
            ELSEIF (DPDT_NL > DPAD) THEN
              WEITEMP(I) = ONE
            ELSE
              WEITEMP(I) = ((DPDT_NL-DPIS)**2 )
     .                * (THREE*DPAD - TWO*DPDT_NL - DPIS)
     .                / ((DPAD-DPIS)**3)
            ENDIF
            DTEMP   = WEITEMP(I)*DPLANL(I)*YLD(I)*ETA/(RHO0(I)*CP)
            TEMP(I) = TEMP(I) + DTEMP 
          ENDIF
        ENDIF
      ENDDO
c
      END
