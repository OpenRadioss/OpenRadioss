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
!||    mat104c_nldam_nice   ../engine/source/materials/mat/mat104/mat104c_nldam_nice.F
!||--- called by ------------------------------------------------------
!||    sigeps104c           ../engine/source/materials/mat/mat104/sigeps104c.F
!||====================================================================
      SUBROUTINE MAT104C_NLDAM_NICE(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7     THK     ,SIGY    ,ET      ,TEMPEL  ,DPLA_NL ,DMG     ,
     8     TEMP    ,SEQ     ,PLA_NL  ,PLAP_NL )
      !=======================================================================
      !      Implicit types
      !=======================================================================
#include      "implicit_f.inc"
      !=======================================================================
      !      Common
      !=======================================================================
#include      "com01_c.inc"
      !=======================================================================
      !      Dummy arguments
      !=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,JTHE
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     ::
     .   RHO,TEMPEL,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   GS,THKLY,PLA_NL,PLAP_NL,DPLA_NL
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: 
     .   PLA,DPLA,EPSD,OFF,THK,TEMP,SEQ
      my_real ,DIMENSION(NEL,6), INTENT(INOUT) :: 
     .   DMG
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
c      
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,K,II,IGURSON,NINDX,INDEX(NEL),NICE
c
      my_real ::
     .   YOUNG,BULK,LAM,G,G2,NU,CDR,KDR,HARD,YLD0,QVOCE,BVOCE,JCC,
     .   EPSP0,MTEMP,TINI,TREF,ETA,CP,DPIS,DPAD,ASRATE,AFILTR,HKHI,
     .   Q1,Q2,ED,AN,EPN,KW,FR,FC,F0,A11,A12,NNU
      my_real ::
     .   DTI,H,LDAV,SIGVM,SIGDR2,YLD2I,OMEGA,
     .   DTEMP,FCOSH,FSINH,DPDT,DLAM,DDEP
      my_real :: 
     .   DPXX,DPYY,DPXY,DPZZ,DSDRDJ2,DSDRDJ3,
     .   DJ3DSXX,DJ3DSYY,DJ3DSXY,DJ3DSZZ,
     .   DJ2DSXX,DJ2DSYY,DJ2DSXY,
     .   DFDSXX,DFDSYY,DFDSXY,
     .   DENOM,DPHI,SDPLA,SDV_DFDSIG,DFDSIG2,
     .   DPHI_DSIG,DPHI_DYLD,DPHI_DFDR,DPHI_DFS,DFS_DFT,
     .   DFN_DLAM,DFSH_DLAM,DFG_DLAM,DFT_DLAM,DFDPLA,NORMSIG,
     .   DFN,DFSH,DFG,DFT,DYLD_DPLA,DYLD_DTEMP,DTEMP_DLAM
c
      my_real, DIMENSION(NEL) ::
     .   DSIGXX,DSIGYY,DSIGXY,TRSIG,TRDEP,
     .   SXX,SYY,SXY,SZZ,SIGM,J2,J3,SIGDR,YLD,WEITEMP,TRDFDS,
     .   HARDP,FHARD,FRATE,FTHERM,FDR,TRIAX,ETAT,SIGDR_OLD,
     .   FDAM,PHI0,PHI,FT,FS,FG,FN,FSH,DPLA_DLAM,DEZZ,DPHI_DTRSIG,
     .   NORMXX,NORMYY,NORMXY,NORMZZ,SIG_DFDSIG,DLAM_NL
c
      !=======================================================================
      !       DRUCKER - VOCE - JOHNSON-COOK MATERIAL LAW WITH GURSON DAMAGE
      !                   USING NON LOCAL PEERLINGS METHOD
      !=======================================================================
      !UVAR(1)   PHI     YIELD FUNCTION VALUE
      !UVAR(2)   YLD     YIELD STRESS
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
      NNU     = UPARAM(7)  ! NU/(1-NU)
      A11     = UPARAM(9)  ! Diagonal term, elastic matrix in plane stress 
      A12     = UPARAM(10) ! Non-diagonal term, elastic matrix in plane stress 
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
      DTI     = ONE / MAX(TIMESTEP, EM20)
c      
      ! Gurson damage model parameters parameters
      IGURSON = NINT(UPARAM(30)) ! Gurson switch flag: 
                                 !  = 0 => no damage model
                                 !  = 1 => local damage model
                                 !  = 2 => non local (Forest - micromorphic) damage model
                                 !  = 3 => non local (Peerlings) damage model
      Q1      = UPARAM(31) ! Gurson yield criterion 1st parameter
      Q2      = UPARAM(32) ! Gurson yield criterion 2nd parameter
      ED      = UPARAM(34) ! Plastic strain trigger for nucleation
      AN      = UPARAM(35) ! Nucleation rate
      KW      = UPARAM(36) ! Shear coefficient (Nahshon & Hutchinson)
      FR      = UPARAM(37) ! Failure void volume fraction
      FC      = UPARAM(38) ! Critical void volume fraction
      F0      = UPARAM(39) ! Initial void volume fraction
      HKHI    = UPARAM(40) ! Micromorphic penalty parameter
c
      ! Recovering internal variables
      DO I=1,NEL
        ! If the element is failing
        IF (OFF(I) < EM03) OFF(I) = ZERO
        IF (OFF(I) < ONE)  OFF(I) = OFF(I)*FOUR_OVER_5
        ! User inputs
        PHI0(I)    = UVAR(I,1)  ! Previous yield function value
        YLD(I)     = UVAR(I,2)  ! Previous yield stress
        ! Damage variables
        FG(I)      = DMG(I,2)   ! Growth damage
        FN(I)      = DMG(I,3)   ! Nucleation damage
        FSH(I)     = DMG(I,4)   ! Shear damage
        FT(I)      = DMG(I,5)   ! Total damage
        FS(I)      = DMG(I,6)   ! Effective damage
        ! Standard inputs
        DPLA(I)    = ZERO       ! Initialization of the plastic strain increment
        ET(I)      = ONE        ! Initialization of hourglass coefficient
        HARDP(I)   = ZERO       ! Initialization of hardening modulus
        DEZZ(I)    = ZERO       ! Initialization of the transverse strain increment
        DLAM_NL(I) = ZERO       ! Initialization of the non-local plastic multiplier
        SIG_DFDSIG(I) = ZERO    ! Initialization of the stress - normal scalar product
        NORMXX(I)  = ZERO       ! Initialization of the x component of the normal tensor
        NORMYY(I)  = ZERO       ! Initialization of the y component of the normal tensor
        NORMZZ(I)  = ZERO       ! Initialization of the z component of the normal tensor
      ENDDO   
c
      ! Initialization of damage, temperature and self-heating weight factor
      IF (TIME == ZERO) THEN   
        TEMP(1:NEL) = TINI 
        IF (ISIGI == 0) THEN 
          DMG(1:NEL,5) = F0
          FT(1:NEL)    = F0
          DMG(1:NEL,1) = F0/FR
          IF (F0<FC) THEN 
            DMG(1:NEL,6) = F0
          ELSE
            DMG(1:NEL,6) = FC + (ONE/Q1-FC)*(F0-FC)/(FR-FC)
          ENDIF
          FS(1:NEL) = DMG(1:NEL,6)
        ENDIF
      ENDIF 
      IF (CP > ZERO) THEN     
        IF (JTHE == 0) THEN     
          DO I=1,NEL
            ! Computation of the weight factor
            IF (PLAP_NL(I) < DPIS) THEN
              WEITEMP(I) = ZERO
            ELSEIF (PLAP_NL(I) > DPAD) THEN
              WEITEMP(I) = ONE
            ELSE
              WEITEMP(I) = ((PLAP_NL(I)-DPIS)**2 )
     .                * (THREE*DPAD - TWO*PLAP_NL(I) - DPIS)
     .                / ((DPAD-DPIS)**3)
            ENDIF
          ENDDO
        ELSE
          TEMP(1:NEL) = TEMPEL(1:NEL)
        ENDIF
      ENDIF
c      
      !========================================================================
      ! NON-LOCAL VARIABLES UPDATE
      !========================================================================
      DO I=1,NEL  
c
        ! Previous value of Drucker equivalent stress
        TRSIG(I) = SIGOXX(I) + SIGOYY(I)
        SIGM(I)  = -TRSIG(I) * THIRD
        SXX(I)   = SIGOXX(I) + SIGM(I)
        SYY(I)   = SIGOYY(I) + SIGM(I)
        SZZ(I)   = SIGM(I)
        SXY(I)   = SIGOXY(I)
        J2(I)    = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + SXY(I)**2 
        J3(I)    = SXX(I) * SYY(I) * SZZ(I) - SZZ(I) * SXY(I)**2 
        FDR(I)   = J2(I)**3 - CDR*(J3(I)**2)
        IF (FDR(I) > ZERO) THEN
          SIGDR(I) = KDR * EXP((ONE/SIX)*LOG(FDR(I)))
        ELSE
          SIGDR(I) = ZERO
        ENDIF
        SIGDR_OLD(I) = SIGDR(I)
c
        ! Computation of the stress triaxiality and the etaT factor
        IF (SIGDR(I)>ZERO) THEN 
          TRIAX(I) = (TRSIG(I)*THIRD)/SIGDR(I)
        ELSE
          TRIAX(I) = ZERO
        ENDIF
        IF (TRSIG(I)<ZERO) THEN
          ETAT(I) = ZERO
        ELSE
          ETAT(I) = ONE
        ENDIF
c
        ! Normal to the previous yield surface
        IF (YLD(I)>ZERO) THEN 
          YLD2I          = ONE / YLD(I)**2
          DPHI_DSIG      = TWO * SIGDR(I) * YLD2I
          FSINH          = SINH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO))
          DPHI_DTRSIG(I) = Q1*Q2*ETAT(I)*FS(I)*FSINH/YLD(I)   
        ELSE
          YLD2I          = ZERO
          DPHI_DSIG      = ZERO
          FSINH          = ZERO
          DPHI_DTRSIG(I) = ZERO   
        ENDIF
c        
        ! Computation of the Eulerian norm of the stress tensor
        NORMSIG = SQRT(SIGOXX(I)*SIGOXX(I)
     .               + SIGOYY(I)*SIGOYY(I) 
     .           + TWO*SIGOXY(I)*SIGOXY(I)) 
        NORMSIG = MAX(NORMSIG,ONE)     
c     
        ! Computation of the normal to the yield surface
        FDR(I) = (J2(I)/(NORMSIG**2))**3 - CDR*((J3(I)/(NORMSIG**3))**2) 
        IF (FDR(I) > ZERO) THEN             
          DPHI_DFDR = DPHI_DSIG*KDR*(ONE/SIX)*EXP(-(FIVE/SIX)*LOG(FDR(I)))  
        ELSE
          DPHI_DFDR = ZERO 
        ENDIF                 
        DSDRDJ2 = DPHI_DFDR*THREE*(J2(I)/(NORMSIG**2))**2                             
        DSDRDJ3 = -DPHI_DFDR*TWO*CDR*(J3(I)/(NORMSIG**3))                    
        ! dJ3/dS
        DJ3DSXX =  TWO_THIRD*(SYY(I)*SZZ(I))/(NORMSIG**2)
     .              -  THIRD*(SXX(I)*SZZ(I))/(NORMSIG**2)
     .              -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)                 
        DJ3DSYY =   -  THIRD*(SYY(I)*SZZ(I))/(NORMSIG**2)
     .           + TWO_THIRD*(SXX(I)*SZZ(I))/(NORMSIG**2)
     .              -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)             
        DJ3DSZZ =   -  THIRD*(SYY(I)*SZZ(I))/(NORMSIG**2)
     .              -  THIRD*(SXX(I)*SZZ(I))/(NORMSIG**2)
     .           + TWO_THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)       
        DJ3DSXY =  TWO*(SXX(I)*SXY(I) + SXY(I)*SYY(I))/(NORMSIG**2)   
        ! dPhi/dSig
        NORMXX(I) = DSDRDJ2*SXX(I)/NORMSIG + DSDRDJ3*DJ3DSXX + DPHI_DTRSIG(I)
        NORMYY(I) = DSDRDJ2*SYY(I)/NORMSIG + DSDRDJ3*DJ3DSYY + DPHI_DTRSIG(I)
        NORMZZ(I) = DSDRDJ2*SZZ(I)/NORMSIG + DSDRDJ3*DJ3DSZZ + DPHI_DTRSIG(I)
        NORMXY(I) = TWO*DSDRDJ2*SXY(I)/NORMSIG + DSDRDJ3*DJ3DSXY
        TRDFDS(I) = NORMXX(I) + NORMYY(I) + NORMZZ(I)    
        SIG_DFDSIG(I) = SIGOXX(I)*NORMXX(I) + SIGOYY(I)*NORMYY(I)
     .                + SIGOXY(I)*NORMXY(I) 
c         
        ! Computation of the non-local plastic multiplier
        IF (SIG_DFDSIG(I) > ZERO) THEN 
          DLAM_NL(I) = ((ONE - FT(I))*YLD(I)*DPLA_NL(I))/MAX(SIG_DFDSIG(I),EM20)
        ELSE 
          DLAM_NL(I) = ZERO
        ENDIF
c
        ! Damage growth update
        IF ((TRDFDS(I)>ZERO).AND.(FT(I)>ZERO).AND.(FT(I)<FR)) THEN 
          FG(I) = FG(I) + (ONE-FT(I))*DLAM_NL(I)*TRDFDS(I)
        ENDIF
        FG(I) = MAX(FG(I),ZERO)
c 
        ! Nucleation damage update
        IF ((PLA_NL(I) >= ED).AND.(FT(I)<FR)) THEN 
          ! Case for positive stress triaxiality
          IF (TRIAX(I)>=ZERO) THEN 
            FN(I) = FN(I) + AN*DPLA_NL(I)
          ! Case for negative stress triaxiality
          ELSEIF ((TRIAX(I)<ZERO).AND.(TRIAX(I)>=-THIRD)) THEN
            FN(I) = FN(I) + AN*MAX(ONE + THREE*TRIAX(I),ZERO)*DPLA_NL(I)
          ENDIF
        ENDIF
        FN(I) = MAX(FN(I),ZERO)
c        
        ! Shear damage update
        IF ((SIGDR(I) > ZERO).AND.(FT(I)>ZERO).AND.(FT(I)<FR)) THEN 
          SIGVM  = SQRT(MAX(EM20,THREE*(J2(I)/(NORMSIG**2))))
          OMEGA  = ONE - ((TWENTY7 *(J3(I)/(NORMSIG**3)))/(TWO*(SIGVM**3)))**2
          OMEGA  = MAX(OMEGA,ZERO)
          OMEGA  = MIN(OMEGA,ONE)
          SDPLA  = (SXX(I)*NORMXX(I)+SYY(I)*NORMYY(I)+ SZZ(I)*NORMZZ(I)
     .            + SXY(I)*NORMXY(I))*DLAM_NL(I)  
          FSH(I) = FSH(I) + KW*OMEGA*FT(I)*(SDPLA/SIGDR(I))
        ENDIF
        FSH(I) = MAX(FSH(I),ZERO)
c        
        ! Total damage update
        FT(I) = F0 + FG(I) + FN(I) + FSH(I) 
        FT(I) = MIN(FT(I), FR)
        IF (FT(I) >= FR) THEN 
          IF (OFF(I)==ONE) OFF(I) = FOUR_OVER_5
        ENDIF
c        
        ! Effective update
        IF (FT(I) < FC)THEN
          FS(I) = FT(I)
        ELSE
          FS(I) = FC + (ONE/Q1 - FC) * (FT(I)-FC)/(FR-FC)
        ENDIF
        FS(I) = MIN(FS(I),ONE/Q1)
c
        ! Temperature update
        IF (CP > ZERO) THEN                   
          IF (JTHE == 0) THEN                   
            DTEMP   = WEITEMP(I)*(ONE-FT(I))*YLD(I)*DPLA_NL(I)*ETA/(RHO(I)*CP)
            TEMP(I) = TEMP(I) + DTEMP
          ENDIF
        ENDIF
      ENDDO      
c      
      ! Computation of the initial yield stress
      DO I=1,NEL
        ! a) - Hardening law
        FHARD(I) = YLD0 + HARD*PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
        ! b) - Correction factor for strain-rate dependence (Johnson-Cook)
        FRATE(I) = ONE
        IF (EPSD(I) > EPSP0) FRATE(I) = FRATE(I) + JCC*LOG(EPSD(I)/EPSP0)
        ! c) - Correction factor for thermal effects
        FTHERM(I) = ONE
        IF (CP > ZERO) FTHERM(I) = ONE - MTEMP*(TEMP(I) - TREF)
        ! d) - Computation of the yield function
        YLD(I) = FHARD(I)*FRATE(I)*FTHERM(I)
        ! e) - Checking values
        YLD(I) = MAX(EM10, YLD(I))
      ENDDO    
c      
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
c
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + (A11*DEPSXX(I) + A12*DEPSYY(I))
        SIGNYY(I) = SIGOYY(I) + (A11*DEPSYY(I) + A12*DEPSXX(I))
        SIGNXY(I) = SIGOXY(I) + (DEPSXY(I)*G)
        SIGNYZ(I) = SIGOYZ(I) + (DEPSYZ(I)*GS(I))
        SIGNZX(I) = SIGOZX(I) + (DEPSZX(I)*GS(I))
        ! Computation of the trace of the trial stress tensor
        TRSIG(I) = SIGNXX(I) + SIGNYY(I) 
        SIGM(I)  = -TRSIG(I) * THIRD
        ! Computation of the deviatoric trial stress tensor
        SXX(I) = SIGNXX(I) + SIGM(I)
        SYY(I) = SIGNYY(I) + SIGM(I)
        SZZ(I) = SIGM(I)
        SXY(I) = SIGNXY(I)
        ! Second deviatoric invariant
        J2(I) = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + SXY(I)**2 
        ! Third deviatoric invariant
        J3(I)  = SXX(I)*SYY(I)*SZZ(I) - SZZ(I)*SXY(I)**2 
        ! Drucker equivalent stress
        FDR(I) = J2(I)**3 - CDR*(J3(I)**2)
        ! Checking equivalent stress values
        IF (FDR(I) > ZERO) THEN
          SIGDR(I) = KDR * EXP((ONE/SIX)*LOG(FDR(I)))  ! FDR(I)**(1/6)
        ELSE
          SIGDR(I) = ZERO
        ENDIF
        ! Computation of the stress triaxiality and the etaT factor
        IF (SIGDR(I)>ZERO) THEN 
          TRIAX(I) = (TRSIG(I)*THIRD)/SIGDR(I)
        ELSE
          TRIAX(I) = ZERO
        ENDIF
        IF (TRSIG(I)<ZERO) THEN
          ETAT(I) = ZERO
        ELSE
          ETAT(I) = ONE
        ENDIF
c        
        ! Update out of plane strain increment
        IF (OFF(I) == ONE) THEN 
          DEZZ(I) = -NNU*DEPSXX(I)-NNU*DEPSYY(I)
          IF (SIG_DFDSIG(I) > EM01) THEN 
            DEZZ(I) = DEZZ(I) + NNU*(DLAM_NL(I)*NORMXX(I))
     .                        + NNU*(DLAM_NL(I)*NORMYY(I))
     .                        + DLAM_NL(I)*NORMZZ(I)
          ENDIF
        ENDIF
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !========================================================================
      DO I=1,NEL
        FDAM(I) = TWO*Q1*FS(I)*COSH(Q2*ETAT(I)*TRSIG(I)/YLD(I)/TWO) - (Q1*FS(I))**2
        PHI(I)  = (SIGDR(I) / YLD(I))**2 - ONE + FDAM(I) 
      ENDDO
c      
      ! Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL         
        IF ((PHI(I) >= ZERO).AND.(OFF(I) == ONE).AND.(FT(I)<FR)) THEN
          NINDX = NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO
c
      !====================================================================
      ! - PLASTIC CORRECTION WITH NICE - EXPLICIT METHOD
      !====================================================================
#include "vectorize.inc" 
      DO II=1,NINDX   
c      
        ! Number of the element with plastic behaviour                                             
        I = INDEX(II)
c        
        ! Computation of the trial stress increment
        DSIGXX(I) = A11*DEPSXX(I) + A12*DEPSYY(I)
        DSIGYY(I) = A11*DEPSYY(I) + A12*DEPSXX(I)
        DSIGXY(I) = DEPSXY(I)*G
        DLAM      = ZERO      
c        
        ! Note     : in this part, the purpose is to compute in one iteration
        ! a plastic multiplier allowing to update internal variables to satisfy
        ! the consistency condition.
        ! Its expression is : DLAMBDA = - (PHI_OLD + DPHI) / DPHI_DLAMBDA
        ! -> PHI_OLD : old value of yield function (known)
        ! -> DPHI : yield function prediction (to compute)
        ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
        !                   into account of internal variables kinetic : 
        !                   plasticity, temperature and damage (to compute)
c        
        ! 1 - Computation of DPHI_DSIG the normal to the yield surface
        !-------------------------------------------------------------  
c
        ! DPHI_DSIG already computed above
c        
        ! Restoring previous value of the yield function
        PHI(I) = PHI0(I)
c           
        ! Computation of yield surface trial increment DPHI       
        DPHI = NORMXX(I) * DSIGXX(I)  
     .       + NORMYY(I) * DSIGYY(I)   
     .       + NORMXY(I) * DSIGXY(I)
c  
        ! 2 - Computation of DPHI_DLAMBDA
        !---------------------------------------------------------
c        
        !   a) Derivative with respect stress increments tensor DSIG
        !   --------------------------------------------------------
        DFDSIG2   = NORMXX(I) * (A11*NORMXX(I) + A12*NORMYY(I))
     .            + NORMYY(I) * (A11*NORMYY(I) + A12*NORMXX(I))
     .            + NORMXY(I) * NORMXY(I) * G  
c  
        !   b) Derivatives with respect to plastic strain P 
        !   ------------------------------------------------
c        
        !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
        !     ----------------------------------------------------------------------------
        HARDP(I)  = HARD + QVOCE*BVOCE*EXP(-BVOCE*PLA(I)) 
        DYLD_DPLA = HARDP(I)*FRATE(I)*FTHERM(I)  
c        
        !     ii) Derivative of dPLA with respect to DLAM
        !     -------------------------------------------            
        DPLA_DLAM(I) = SIG_DFDSIG(I) / ((ONE - FT(I))*YLD(I)) 
c                
        !  c) Derivative with respect to the yield stress
        !  ----------------------------------------------
        SIGDR2    =  SIGDR_OLD(I)**2 
        TRSIG(I)  =  SIGOXX(I) + SIGOYY(I)
        DPHI_DYLD = -TWO*SIGDR2/YLD(I)**3-DPHI_DTRSIG(I)*TRSIG(I)/YLD(I)  
c        
        !  d) Derivative of PHI with respect to DLAM ( = -DENOM)
        DENOM = DFDSIG2 - (DPHI_DYLD * DYLD_DPLA * DPLA_DLAM(I))
        DENOM = SIGN(MAX(ABS(DENOM),EM20) ,DENOM)
c  
        ! 3 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------
c
        ! Computation of the plastic multiplier
        DLAM  = (DPHI + PHI(I)) / DENOM
        DLAM  = MAX(DLAM, ZERO)
c        
        ! Plastic strains tensor update
        DPXX     = DLAM * NORMXX(I)
        DPYY     = DLAM * NORMYY(I)
        DPZZ     = DLAM * NORMZZ(I)
        DPXY     = DLAM * NORMXY(I)
        TRDEP(I) = DPXX + DPYY + DPZZ          
c        
        ! Elasto-plastic stresses update   
        SIGNXX(I) = SIGOXX(I) + DSIGXX(I) - (A11*DPXX + A12*DPYY)
        SIGNYY(I) = SIGOYY(I) + DSIGYY(I) - (A11*DPYY + A12*DPXX) 
        SIGNXY(I) = SIGOXY(I) + DSIGXY(I) - DPXY*G
        TRSIG(I)  = SIGNXX(I) + SIGNYY(I)
        SIGM(I)   = -TRSIG(I) * THIRD
        SXX(I)    = SIGNXX(I) + SIGM(I)
        SYY(I)    = SIGNYY(I) + SIGM(I)
        SZZ(I)    = SIGM(I)
        SXY(I)    = SIGNXY(I)
c        
        ! Cumulated plastic strain and strain rate update
        DDEP     = (DLAM/((ONE - FT(I))*YLD(I)))*SIG_DFDSIG(I)
        DPLA(I)  = DPLA(I) + DDEP 
        PLA(I)   = PLA(I)  + DDEP  
c        
        ! Drucker equivalent stress update
        J2(I)    = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + SXY(I)**2
        J3(I)    = SXX(I) * SYY(I) * SZZ(I) - SZZ(I) * SXY(I)**2 
        FDR(I)   = J2(I)**3 - CDR*(J3(I)**2)
        SIGDR(I) = KDR * EXP((ONE/SIX)*LOG(FDR(I)))  
        ! Computation of the stress triaxiality and the etaT factor
        TRIAX(I) = (TRSIG(I)*THIRD)/SIGDR(I)
        IF (TRSIG(I)<ZERO) THEN
          ETAT(I) = ZERO
        ELSE
          ETAT(I) = ONE
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
        SIGDR2  = SIGDR(I)**2
        YLD2I   = ONE / YLD(I)**2
        FCOSH   = COSH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO)) 
        FDAM(I) = TWO*Q1*FS(I)*FCOSH - (Q1*FS(I))**2
        PHI(I)  = SIGDR2 * YLD2I - ONE + FDAM(I)    
c          
      ENDDO
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH NICE - EXPLICIT METHOD
      !===================================================================  
c      
      ! Storing new values
      DO I=1,NEL
        ! USR Outputs & coefficient for hourglass
        IF (DPLA(I) > ZERO) THEN 
          UVAR(I,1) = PHI(I)           ! Yield function value
          ET(I)     = HARDP(I)*FRATE(I) / (HARDP(I)*FRATE(I) + YOUNG)  
        ELSE
          UVAR(I,1) = ZERO
          ET(I)     = ONE
        ENDIF
        UVAR(I,2)  = YLD(I)            ! Yield stress
        ! Standard outputs
        DMG(I,1)   = FT(I)/FR          ! Normalized total damage
        DMG(I,2)   = FG(I)             ! Growth damage
        DMG(I,3)   = FN(I)             ! Nucleation damage 
        DMG(I,4)   = FSH(I)            ! Shear damage
        DMG(I,5)   = MIN(FT(I),FR)     ! Total damage
        DMG(I,6)   = MIN(FS(I),ONE/Q1) ! Effective damage
        SEQ(I)     = SIGDR(I)          ! Equivalent stress
        ! If element is broken
        IF (FT(I) >= FR) THEN
          DPLA(I)   = ZERO
          SIGNXX(I) = ZERO
          SIGNYY(I) = ZERO
          SIGNXY(I) = ZERO
          SIGNYZ(I) = ZERO
          SIGNZX(I) = ZERO
          SEQ(I)    = ZERO
        ENDIF
        ! Plastic strain-rate (filtered)
        DPDT       = DPLA(I) / MAX(EM20,TIMESTEP)
        EPSD(I)    = AFILTR * DPDT + (ONE - AFILTR) * EPSD(I)
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT((A11)/RHO(I))
        ! Storing the yield stress
        SIGY(I)    = YLD(I)
        ! Computation of the thickness variation 
        THK(I)     = THK(I) + DEZZ(I)*THKLY(I)*OFF(I) 
      ENDDO
c
      END