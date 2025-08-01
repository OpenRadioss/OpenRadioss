Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    mat104_nodam_newton   ../engine/source/materials/mat/mat104/mat104_nodam_newton.F
!||--- called by ------------------------------------------------------
!||    sigeps104             ../engine/source/materials/mat/mat104/sigeps104.F
!||====================================================================
      SUBROUTINE MAT104_NODAM_NEWTON(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,VOLUME  ,FHEAT   , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,TEMP    ,SEQ     ,INLOC   ,DPLANL  ,
     8     JLAG    )
      !=======================================================================
      !      Implicit types
      !=======================================================================
#include      "implicit_f.inc"
      !=======================================================================
      !      Common
      !=======================================================================
      !=======================================================================
      !      Dummy arguments
      !=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,JTHE,INLOC
      INTEGER ,INTENT(IN) :: JLAG
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real ,DIMENSION(NEL), INTENT(IN)    :: VOLUME
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: FHEAT
      my_real ,DIMENSION(NEL), INTENT(IN)    :: RHO0,RHO,DPLANL,
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
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,II,ITER,NITER,NINDX,INDEX(NEL)
c
      my_real 
     .   YOUNG,BULK,LAM,G,G2,NU,CDR,KDR,HARD,YLD0,QVOCE,BVOCE,JCC,
     .   EPSP0,MTEMP,TINI,TREF,ETA,CP,DPIS,DPAD,ASRATE,AFILTR
      my_real 
     .   H,LDAV,TRDEP,SIGVM,OMEGA,
     .   DTEMP,FCOSH,FSINH,DPDT,DLAM,DDEP
      my_real 
     .   DSDRDJ2,DSDRDJ3,
     .   DJ3DSXX,DJ3DSYY,DJ3DSZZ,DJ3DSXY,DJ3DSYZ,DJ3DSZX,
     .   DJ2DSXX,DJ2DSYY,DJ2DSZZ,DJ2DSXY,DJ2DSYZ,DJ2DSZX,
     .   DFDSXX,DFDSYY,DFDSZZ,DFDSXY,DFDSYZ,DFDSZX,
     .   NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,
     .   SDPLA,DPHI_DTRSIG,SIG_DFDSIG,DFDSIG2,
     .   DPHI_DSIG,DPHI_DYLD,DPHI_DFDR,DF_DFS,DFS_DFT,DPHI_DFT,
     .   DPHI_DFS,DFN_DLAM,DFSH_DLAM,DFG_DLAM,DFT_DLAM,DPDT_NL,
     .   DFN,DFSH,DFG,DFT,DYLD_DPLA,DYLD_DTEMP,DTEMP_DLAM,NORMSIG
c
      my_real, DIMENSION(NEL) ::
     .   DSIGXX,DSIGYY,DSIGZZ,DSIGXY,DSIGYZ,DSIGZX,TRSIG,
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,SIGM,J2,J3,SIGDR,YLD,WEITEMP,
     .   HARDP,FHARD,FRATE,FTHERM,DTHERM,FDR,DEPXX,DEPYY,DEPZZ,DEPXY,DEPYZ,DEPZX,
     .   PHI,FT,FS,FG,FN,FSH,DPLA_DLAM,DPHI_DLAM,PHI0,
     .   DPXX,DPYY,DPXY,DPYZ,DPZX,DPZZ,SIGDR2,YLD2I
      !=======================================================================
      !             DRUCKER - VOCE - JOHNSON-COOK MATERIAL LAW
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
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) <  ONE) OFF(I) = OFF(I)*FOUR_OVER_5
        ! Standard inputs
        DPLA(I)  = ZERO    ! Initialization of the plastic strain increment
        ET(I)    = ONE     ! Initialization of hourglass coefficient
        HARDP(I) = ZERO    ! Initialization of hardening modulus
      ENDDO
c      
      ! Initialization of temperature and self-heating weight factor
      IF (TIME == ZERO .and. JTHE == 0) THEN   
        TEMP(1:NEL) = TINI 
      ENDIF 
      IF (CP > ZERO .OR. JTHE /= 0) THEN     
        IF (INLOC == 0) THEN       
          DO I=1,NEL
            IF (EPSD(I) < DPIS) THEN
              WEITEMP(I) = ZERO
            ELSEIF (EPSD(I) > DPAD) THEN
              WEITEMP(I) = ONE
            ELSE
              WEITEMP(I) = ((EPSD(I)-DPIS)**2 )
     .                * (THREE*DPAD - TWO*EPSD(I) - DPIS)
     .                / ((DPAD-DPIS)**3)
            ENDIF
          ENDDO
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
        FTHERM(I) = ONE - MTEMP * (TEMP(I) - TREF)
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
        SIGM(I)   =-TRSIG(I) * THIRD
        ! Computation of the deviatoric trial stress tensor
        SXX(I) = SIGNXX(I) + SIGM(I)
        SYY(I) = SIGNYY(I) + SIGM(I)
        SZZ(I) = SIGNZZ(I) + SIGM(I)
        SXY(I) = SIGNXY(I)
        SYZ(I) = SIGNYZ(I)
        SZX(I) = SIGNZX(I)
        ! Second deviatoric invariant
        J2(I) = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .        +       SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
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
      ! Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL         
        IF ((PHI(I) >= ZERO).AND.(OFF(I) == ONE)) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO
c      
      !====================================================================
      ! - PLASTIC CORRECTION WITH CUTTING PLANE METHOD (SEMI-IMPLICIT)
      !====================================================================         
c      
      ! Number of iterations
      NITER = 3
c
      ! Loop over yielding elements
#include "vectorize.inc" 
      DO II=1,NINDX  
c        
        ! Number of the element with plastic behaviour                                          
        I = INDEX(II) 
c        
        ! Initialization of the iterative Newton procedure
        SIGDR2(I) = SIGDR(I)**2
        YLD2I(I)  = ONE / YLD(I)**2
        DPXX(I)   = ZERO
        DPYY(I)   = ZERO
        DPZZ(I)   = ZERO
        DPXY(I)   = ZERO
        DPYZ(I)   = ZERO
        DPZX(I)   = ZERO
      ENDDO
c
      ! Loop over the iterations     
      DO ITER = 1, NITER
#include "vectorize.inc" 
        ! Loop over yielding elements
        DO II=1,NINDX 
          I = INDEX(II)    
c        
          ! Note: in this part, the purpose is to compute for each iteration
          ! a plastic multiplier allowing to update internal variables to satisfy
          ! the consistency condition using the cutting plane semi-implicit 
          ! iterative procedure.
          ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
          ! -> PHI          : current value of yield function (known)
          ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
          !                   into account of internal variables kinetic : 
          !                   plasticity, temperature and damage (to compute)
c        
          ! 1 - Computation of DPHI_DSIG the normal to the yield surface
          !-------------------------------------------------------------
c        
          ! Derivative with respect to the equivalent stress
          YLD2I(I)  = ONE/(YLD(I)**2) 
          DPHI_DSIG = TWO*SIGDR(I)*YLD2I(I)
c          
          ! Computation of the Eulerian norm of the stress tensor
          NORMSIG = SQRT(SIGNXX(I)*SIGNXX(I)
     .            + SIGNYY(I)*SIGNYY(I)
     .            + SIGNZZ(I)*SIGNZZ(I)  
     .            + TWO*SIGNXY(I)*SIGNXY(I)
     .            + TWO*SIGNYZ(I)*SIGNYZ(I)
     .            + TWO*SIGNZX(I)*SIGNZX(I))   
          NORMSIG = MAX(NORMSIG,ONE)
c        
          ! Derivative with respect to Fdr 
          FDR(I)     =  (J2(I)/(NORMSIG**2))**3 - CDR*((J3(I)/(NORMSIG**3))**2)       
          DPHI_DFDR  =  DPHI_DSIG*KDR*(ONE/SIX)*EXP(-(FIVE/SIX)*LOG(FDR(I)))             
          DSDRDJ2    =  DPHI_DFDR*THREE*(J2(I)/(NORMSIG**2))**2                             
          DSDRDJ3    = -DPHI_DFDR*TWO*CDR*(J3(I)/(NORMSIG**3))       
          ! dJ3/dSig                                                        
          DJ3DSXX =  TWO_THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .             -  THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)                 
          DJ3DSYY = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .             + TWO_THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)             
          DJ3DSZZ = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .              - THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             + TWO_THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)
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
          ! 2 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c        
          !   a) Derivative with respect stress increments tensor DSIG
          !   --------------------------------------------------------
          DFDSIG2 = NORMXX * NORMXX * G2
     .            + NORMYY * NORMYY * G2
     .            + NORMZZ * NORMZZ * G2
     .            + NORMXY * NORMXY * G
     .            + NORMYZ * NORMYZ * G
     .            + NORMZX * NORMZX * G   
c     
          !   b) Derivatives with respect to plastic strain P 
          !   ------------------------------------------------  
c          
          !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
          !     ----------------------------------------------------------------------------
          HARDP(I) = HARD + QVOCE*BVOCE*EXP(-BVOCE*PLA(I))
          DYLD_DPLA  = HARDP(I)*FRATE(I)*FTHERM(I)
c          
          !     ii) Derivative of dPLA with respect to DLAM
          !     -------------------------------------------                    
          SIG_DFDSIG = SIGNXX(I) * NORMXX
     .               + SIGNYY(I) * NORMYY
     .               + SIGNZZ(I) * NORMZZ
     .               + SIGNXY(I) * NORMXY
     .               + SIGNYZ(I) * NORMYZ
     .               + SIGNZX(I) * NORMZX  
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
            DTEMP_DLAM = WEITEMP(I)*ETA/(RHO0(I)*CP)*SIG_DFDSIG
          ELSE
            DYLD_DTEMP = ZERO
            DTEMP_DLAM = ZERO
          ENDIF
c          
          !  d) Derivative with respect to the yield stress
          !  ----------------------------------------------
          DPHI_DYLD = -TWO*SIGDR2(I)/(YLD(I)**3)
c          
          ! 3 - Computation of plastic multiplier and variables update
          !----------------------------------------------------------
c          
          ! Derivative of PHI with respect to DLAM
          DPHI_DLAM(I) = - DFDSIG2 + (DPHI_DYLD*DYLD_DPLA*DPLA_DLAM(I))
          IF (JTHE == 0 .AND. CP > ZERO .AND. INLOC == 0) THEN
            DPHI_DLAM(I) = DPHI_DLAM(I) + DPHI_DYLD*DYLD_DTEMP*DTEMP_DLAM
          ENDIF
          DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20) ,DPHI_DLAM(I))
c          
          ! Computation of the plastic multiplier
          DLAM = -PHI(I)/DPHI_DLAM(I)
c          
          ! Plastic strains tensor update
          DPXX(I) = DLAM * NORMXX
          DPYY(I) = DLAM * NORMYY
          DPZZ(I) = DLAM * NORMZZ
          DPXY(I) = DLAM * NORMXY
          DPYZ(I) = DLAM * NORMYZ
          DPZX(I) = DLAM * NORMZX  
c          
          ! Elasto-plastic stresses update
          SIGNXX(I) = SIGNXX(I) - DPXX(I)*G2
          SIGNYY(I) = SIGNYY(I) - DPYY(I)*G2
          SIGNZZ(I) = SIGNZZ(I) - DPZZ(I)*G2
          SIGNXY(I) = SIGNXY(I) - DPXY(I)*G
          SIGNYZ(I) = SIGNYZ(I) - DPYZ(I)*G
          SIGNZX(I) = SIGNZX(I) - DPZX(I)*G
          TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
          SIGM(I)   =-TRSIG(I)  * THIRD
          SXX(I)    = SIGNXX(I) + SIGM(I)
          SYY(I)    = SIGNYY(I) + SIGM(I)
          SZZ(I)    = SIGNZZ(I) + SIGM(I)
          SXY(I)    = SIGNXY(I)
          SYZ(I)    = SIGNYZ(I)
          SZX(I)    = SIGNZX(I)          
c          
          ! Cumulated plastic strain and strain rate update           
          DDEP    = (DLAM/YLD(I))*SIG_DFDSIG
          DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
          PLA(I)  = PLA(I) + DDEP
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
          ENDIF
          FTHERM(I) = ONE - MTEMP*(TEMP(I) - TREF)
c
          ! Hardening law update
          FHARD(I) = YLD0 + HARD*PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
c
          ! Yield stress update
          YLD(I) = FHARD(I)*FRATE(I)*FTHERM(I)
          YLD(I) = MAX(YLD(I), EM10)
c        
          ! Yield function value update
          SIGDR2(I) = SIGDR(I)**2
          YLD2I(I)  = ONE / YLD(I)**2
          PHI(I)    = SIGDR2(I) * YLD2I(I) - ONE
c
        ENDDO
        ! End of the loop over the iterations  
      ENDDO 
      ! End of the loop over the iterations 
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE ITERATIVE METHOD
      !===================================================================
c      
      ! Storing new values
      DO I=1,NEL  
        ! USR Outputs
        SEQ(I)     = SIGDR(I)
        ! Plastic strain-rate (filtered)
        DPDT       = DPLA(I) / MAX(EM20,TIMESTEP)
        EPSD(I)    = AFILTR * DPDT + (ONE - AFILTR) * EPSD(I)
        ! Coefficient for hourglass
        IF (DPLA(I) > ZERO) THEN 
          ET(I)    = HARDP(I)*FRATE(I) / (HARDP(I)*FRATE(I) + YOUNG)
        ELSE
          ET(I)    = ONE
        ENDIF
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT((BULK + FOUR_OVER_3*G)/RHO0(I))
        ! Storing the yield stress
        SIGY(I)    = YLD(I)
        ! Non-local temperature
        IF (INLOC > 0 .AND. OFF(I) == ONE) THEN 
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
C-----------------------------------------------
C     plastic work dissipated as heat - in case of /heat/mat
C-----------------------------------------------
      IF (JTHE < 0 .AND. JLAG /= 0) THEN
        DO I=1,NEL
          FHEAT(I) = FHEAT(I) + ETA*(ONE-FT(I))*WEITEMP(I)*YLD(I)*DPLA(I)*VOLUME(I)
        ENDDO
      END IF
!-----------
      RETURN      
      END
