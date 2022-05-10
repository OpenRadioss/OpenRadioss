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
Chd|  MAT104_NLDAM_NEWTON           source/materials/mat/mat104/mat104_nldam_newton.F
Chd|-- called by -----------
Chd|        SIGEPS104                     source/materials/mat/mat104/sigeps104.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAT104_NLDAM_NEWTON(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,TEMP    ,
     8     SEQ     )
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
      INTEGER NEL,NUPARAM,NUVAR,JTHE
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO0,RHO,TEMPEL,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,VARNL,DPLA,EPSD,OFF,TEMP,DMG,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,II,IGURSON,ITER,NITER,NINDX,INDEX(NEL)
c
      DOUBLE PRECISION :: 
     .   YOUNG,BULK,LAM,G,G2,NU,CDR,KDR,HARD,YLD0,QVOCE,BVOCE,JCC,
     .   EPSP0,MTEMP,TINI,TREF,ETA,CP,DPIS,DPAD,ASRATE,AFILTR,HKHI,
     .   Q1,Q2,ED,AN,EPN,KW,FR,FC,F0
      DOUBLE PRECISION :: 
     .   DTI,H,LDAV,TRDEP,SIGVM,OMEGA,
     .   DTEMP,FCOSH,FSINH,DPDT,DLAM,DDEP
      DOUBLE PRECISION ::
     .   DSDRDJ2,DSDRDJ3,
     .   DJ3DSXX,DJ3DSYY,DJ3DSZZ,DJ3DSXY,DJ3DSYZ,DJ3DSZX,
     .   DJ2DSXX,DJ2DSYY,DJ2DSZZ,DJ2DSXY,DJ2DSYZ,DJ2DSZX,
     .   DFDSXX,DFDSYY,DFDSZZ,DFDSXY,DFDSYZ,DFDSZX,
     .   NORMSIG,SDPLA,DPHI_DTRSIG,DFDSIG2,SDV_DFDSIG,
     .   DPHI_DSIG,DPHI_DYLD,DPHI_DFDR,DF_DFS,DFS_DFT,DPHI_DFT,
     .   DPHI_DFS,DFN_DLAM,DFSH_DLAM,DFG_DLAM,DFT_DLAM,
     .   DFN,DFSH,DFG,DFT,DYLD_DPLA,DYLD_DTEMP,DTEMP_DLAM
c
      DOUBLE PRECISION, DIMENSION(NEL) :: 
     .   DSIGXX,DSIGYY,DSIGZZ,DSIGXY,DSIGYZ,DSIGZX,TRSIG,
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,SIGM,J2,J3,SIGDR,YLD,WEITEMP,
     .   HARDP,FHARD,FRATE,FTHERM,DTHERM,FDR,PHI0,TRDFDS,TRIAX,D,
     .   FDAM,PHI,FT,FS,FG,FN,FSH,DPLA_DLAM,DPHI_DLAM,ETAT,
     .   NORMXX,NORMYY,NORMXY,NORMZZ,NORMYZ,NORMZX,SIG_DFDSIG,
     .   DPXX,DPYY,DPXY,DPYZ,DPZX,DPZZ,SIGDR2,YLD2I
c
      DOUBLE PRECISION, DIMENSION(NEL) :: 
     .   DLAM_NL,PLAP_NL,DPLA_NL,PLA_NL
      !=======================================================================
      !       DRUCKER MATERIAL LAW WITH GURSON DAMAGE
      !           USING NON LOCAL PEERLINGS METHOD
      !=======================================================================
      !UVAR(1)   FG       DAMAGE OF CAVITIES GROWTH
      !UVAR(2)   FN       DAMAGE OF CAVITIES NUCLEATION
      !UVAR(3)   FSH      DAMAGE OF SHEAR
      !UVAR(4)   FT       TOTAL DAMAGE (FG + FN + FSH)
      !UVAR(5)   FS       EFFECTIVE DAMAGE FS = F(FT)
      !UVAR(6)   PLA_NL   NON-LOCAL PLASTIC STRAIN
      !UVAR(7)   PLAP_NL  NON-LOCAL PLASTIC STRAIN-RATE
      !UVAR(8)   YLD      YIELD STRESS
      !-----------------------------------------------------------------------  
      !Warning VARNL(NEL) :
      !input  => VARNL = non-local plastic strain increment
      !output => VARNL = cumulated local plastic strain
      !-----------------------------------------------------------------------  
      !DEPIJ = PLASTIC STRAIN TENSOR COMPONENT
      !DEPSIJ = TOTAL STRAIN  TENSOR COMPONENT (EL+PL)
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
      AFILTR  = ASRATE*TIMESTEP/(ASRATE*TIMESTEP + UN)
      DTI     = UN / MAX(TIMESTEP, EM20)        
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
      KW      = UPARAM(36) ! Shear coefficient (Nahshon-Hutchinson)
      FR      = UPARAM(37) ! Void volume fracture at failure
      FC      = UPARAM(38) ! Critical void volume fraction
      F0      = UPARAM(39) ! Initial void volume fraction
      HKHI    = UPARAM(40) ! Micromorphic penalty parameter
c      
      ! Recovering internal variables
      DO I=1,NEL
        ! If the element is failing
        IF (OFF(I) < 0.001)   OFF(I) = ZERO
        IF (OFF(I) < UN)      OFF(I) = OFF(I)*QCINQ
        ! User inputs
        FG(I)     = UVAR(I,1)  ! Growth damage
        FN(I)     = UVAR(I,2)  ! Nucleation damage
        FSH(I)    = UVAR(I,3)  ! Shear damage
        FT(I)     = UVAR(I,4)  ! Total damage
        FS(I)     = UVAR(I,5)  ! Effective damage
        PLA_NL(I) = UVAR(I,6)  ! Non-local plastic strain
        YLD(I)    = UVAR(I,8)  ! Previous yield stress
        ! Standard inputs
        D(I)      = Q1*FS(I)   ! Effective normalized damage
        DPLA(I)   = ZERO       ! Initialization of the plastic strain increment
        ET(I)     = UN         ! Initialization of hourglass coefficient
        HARDP(I)  = ZERO       ! Initialization of hourglass coefficient
        IF (OFF(I) == UN) THEN 
          DPLA_NL(I) = MAX(VARNL(I),ZERO)     ! Non-local plastic strain increment
          PLA_NL(I)  = PLA_NL(I) + DPLA_NL(I) ! Non-local cumulated plastic strain 
          PLAP_NL(I) = DPLA_NL(I) * DTI       ! Non-local plastic strain-rate
        ELSE 
          DPLA_NL(I) = ZERO
          PLAP_NL(I) = ZERO
        ENDIF
      ENDDO
c      
      ! Initialization of damage, temperature and self-heating weight factor
      IF (TIME == ZERO) THEN   
        TEMP(1:NEL) = TINI 
        IF (ISIGI == 0) THEN 
          UVAR(1:NEL,4) = F0
          FT(1:NEL)     = F0
          DMG(1:NEL)    = F0/FR
          IF (F0<FC) THEN 
            UVAR(1:NEL,5) = F0
          ELSE
            UVAR(1:NEL,5) = FC + (UN/Q1-FC)*(F0-FC)/(FR-FC)
          ENDIF
          FS(1:NEL) = UVAR(1:NEL,5)
        ENDIF
      ENDIF 
      IF (CP > ZERO) THEN     
        IF (JTHE == 0) THEN
          DO I=1,NEL
            ! Computation of the weight factor
            IF (PLAP_NL(I) < DPIS) THEN
              WEITEMP(I) = ZERO
            ELSEIF (PLAP_NL(I) > DPAD) THEN
              WEITEMP(I) = UN
            ELSE
              WEITEMP(I) = ((PLAP_NL(I)-DPIS)**2 )
     .        * (TROIS*DPAD - DEUX*PLAP_NL(I) - DPIS)
     .         / ((DPAD-DPIS)**3)
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
        TRSIG(I)= SIGOXX(I) + SIGOYY(I) + SIGOZZ(I)
        SIGM(I) =-TRSIG(I) * THIRD
        SXX(I)  = SIGOXX(I) + SIGM(I)
        SYY(I)  = SIGOYY(I) + SIGM(I)
        SZZ(I)  = SIGOZZ(I) + SIGM(I)
        SXY(I)  = SIGOXY(I)
        SYZ(I)  = SIGOYZ(I)
        SZX(I)  = SIGOZX(I)
        J2(I)   = UNDEMI*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .          +       SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        J3(I)   = SXX(I) * SYY(I) * SZZ(I)  
     .          + SXY(I) * SYZ(I) * SZX(I) * DEUX
     .          - SXX(I) * SYZ(I)**2
     .          - SYY(I) * SZX(I)**2
     .          - SZZ(I) * SXY(I)**2 
c
        FDR(I) = J2(I)**3 - CDR*(J3(I)**2)
        IF (FDR(I) > ZERO) THEN
          SIGDR(I) = KDR * EXP((UN/SIX)*LOG(FDR(I)))
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
          ETAT(I) = UN
        ENDIF
c
        ! Normal to the previous yield surface
        IF (YLD(I)>ZERO) THEN 
          YLD2I(I)    = UN / YLD(I)**2
          DPHI_DSIG   = DEUX * SIGDR(I) * YLD2I(I)
          FSINH       = SINH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*DEUX))
          DPHI_DTRSIG = Q1*Q2*ETAT(I)*FS(I)*FSINH/YLD(I) 
        ELSE
          YLD2I(I)    = ZERO
          DPHI_DSIG   = ZERO
          FSINH       = ZERO
          DPHI_DTRSIG = ZERO
        ENDIF
c        
        ! Computation of the Eulerian norm of the stress tensor
        NORMSIG = SQRT(SIGOXX(I)*SIGOXX(I)
     .          + SIGOYY(I)*SIGOYY(I)
     .          + SIGOZZ(I)*SIGOZZ(I)  
     .          + DEUX*SIGOXY(I)*SIGOXY(I)
     .          + DEUX*SIGOYZ(I)*SIGOYZ(I)
     .          + DEUX*SIGOZX(I)*SIGOZX(I))
c
        ! Computation of the norm to the yield surface
        IF (NORMSIG>ZERO) THEN
c
          FDR(I)      = (J2(I)/(NORMSIG**2))**3 - CDR*((J3(I)/(NORMSIG**3))**2)             
          DPHI_DFDR   = DPHI_DSIG*KDR*(UN/SIX)*EXP(-(CINQ/SIX)*LOG(FDR(I)))                    
          DSDRDJ2     = DPHI_DFDR*TROIS*(J2(I)/(NORMSIG**2))**2                             
          DSDRDJ3     = -DPHI_DFDR*DEUX*CDR*(J3(I)/(NORMSIG**3))                     
c       dJ3/dS
          DJ3DSXX =  DTIERS*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .             -  TIERS*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             -  TIERS*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)                 
          DJ3DSYY = - TIERS*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .             + DTIERS*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             -  TIERS*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)             
          DJ3DSZZ = - TIERS*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .              - TIERS*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             + DTIERS*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)
          DJ3DSXY = DEUX*(SXX(I)*SXY(I) + SXY(I)*SYY(I) + SZX(I)*SYZ(I))/(NORMSIG**2)                  
          DJ3DSYZ = DEUX*(SXY(I)*SZX(I) + SYY(I)*SYZ(I) + SYZ(I)*SZZ(I))/(NORMSIG**2)                    
          DJ3DSZX = DEUX*(SXX(I)*SZX(I) + SXY(I)*SYZ(I) + SZX(I)*SZZ(I))/(NORMSIG**2)   
        ! dPhi/dSig
          NORMXX(I) = DSDRDJ2*SXX(I)/NORMSIG + DSDRDJ3*DJ3DSXX + DPHI_DTRSIG
          NORMYY(I) = DSDRDJ2*SYY(I)/NORMSIG + DSDRDJ3*DJ3DSYY + DPHI_DTRSIG
          NORMZZ(I) = DSDRDJ2*SZZ(I)/NORMSIG + DSDRDJ3*DJ3DSZZ + DPHI_DTRSIG
          NORMXY(I) = DEUX*DSDRDJ2*SXY(I)/NORMSIG + DSDRDJ3*DJ3DSXY
          NORMYZ(I) = DEUX*DSDRDJ2*SYZ(I)/NORMSIG + DSDRDJ3*DJ3DSYZ
          NORMZX(I) = DEUX*DSDRDJ2*SZX(I)/NORMSIG + DSDRDJ3*DJ3DSZX  
          TRDFDS(I) = NORMXX(I) + NORMYY(I) + NORMZZ(I)   
          SIG_DFDSIG(I) = SIGOXX(I)*NORMXX(I) + SIGOYY(I)*NORMYY(I) + SIGOZZ(I)*NORMZZ(I)
     .                  + SIGOXY(I)*NORMXY(I) + SIGOYZ(I)*NORMYZ(I) + SIGOZX(I)*NORMZX(I)
        ELSE
          NORMXX(I)     = ZERO
          NORMYY(I)     = ZERO
          NORMZZ(I)     = ZERO
          NORMXY(I)     = ZERO
          NORMYZ(I)     = ZERO
          NORMZX(I)     = ZERO
          TRDFDS(I)     = ZERO
          SIG_DFDSIG(I) = ZERO
        ENDIF
c
        ! Computation of the non-local plastic multiplier
        IF (SIG_DFDSIG(I)>ZERO) THEN
          DLAM_NL(I) = ((UN - FT(I))*YLD(I)*DPLA_NL(I))/SIG_DFDSIG(I)
        ELSE
          DLAM_NL(I) = ZERO
        ENDIF
c
        ! Damage growth update
        IF ((FT(I)>ZERO).AND.(FT(I)<FR).AND.(TRDFDS(I)>ZERO)) THEN 
          FG(I) = FG(I) + (UN-FT(I))*DLAM_NL(I)*TRDFDS(I)
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
            FN(I) = FN(I) + AN*MAX(UN + TROIS*TRIAX(I),ZERO)*DPLA_NL(I)
          ENDIF
        ENDIF
        FN(I) = MAX(FN(I),ZERO)
c        
        ! Shear damage update
        IF ((SIGDR(I) > ZERO).AND.(FT(I)>ZERO).AND.(FT(I)<FR)) THEN 
          SIGVM  = SQRT(MAX(EM20,TROIS*(J2(I)/(NORMSIG**2))))
          OMEGA  = UN - ((VINGT7 *(J3(I)/(NORMSIG**3)))/(DEUX*(SIGVM**3)))**2
          OMEGA  = MAX(OMEGA,ZERO)
          OMEGA  = MIN(OMEGA,UN)
          SDPLA  = (SXX(I)*NORMXX(I)+ SYY(I)*NORMYY(I)+ SZZ(I)*NORMZZ(I)
     .           +  SXY(I)*NORMXY(I)+ SYZ(I)*NORMYZ(I)+ SZX(I)*NORMZX(I))
     .           * DLAM_NL(I)
          FSH(I) = FSH(I) + KW*OMEGA*FT(I)*(SDPLA/SIGDR(I))
        ENDIF
        FSH(I) = MAX(FSH(I),ZERO)
c        
        ! Total damage update
        FT(I) = F0 + FG(I) + FN(I) + FSH(I) 
        FT(I) = MIN(FT(I), FR)
        IF (FT(I) >= FR) THEN 
          IF (OFF(I)==UN) OFF(I) = QCINQ
        ENDIF
c        
        ! Effective update
        IF (FT(I) < FC)THEN
          FS(I) = FT(I)
        ELSE
          FS(I) = FC + (UN/Q1 - FC) * (FT(I)-FC)/(FR-FC)
        ENDIF
        FS(I)   = MIN (FS(I),UN/Q1)
        D(I)    = Q1*FS(I)
c
        ! Temperature update
        IF (CP > ZERO ) THEN                   
          IF (JTHE == 0) THEN                   
            DTEMP   = WEITEMP(I)*(UN-FT(I))*YLD(I)*DPLA_NL(I)*ETA/(RHO0(I)*CP)
            TEMP(I) = TEMP(I) + DTEMP
          ENDIF
        ENDIF
      ENDDO      
c      
      ! Computation of the initial yield stress
      DO I=1,NEL
        ! a) - Hardening law
        FHARD(I) = YLD0 + HARD*PLA(I) + QVOCE*(UN-EXP(-BVOCE*PLA(I)))
        ! b) - Correction factor for strain-rate dependence (Johnson-Cook)
        FRATE(I) = UN
        IF (EPSD(I) > EPSP0) FRATE(I) = FRATE(I) + JCC*LOG(EPSD(I)/EPSP0)
        ! c) - Correction factor for thermal effects
        FTHERM(I) = UN
        IF (CP > ZERO) FTHERM(I) = UN - MTEMP*(TEMP(I) - TREF)
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
        LDAV      = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAM
        SIGNXX(I) = SIGOXX(I) + (UN-D(I))*(DEPSXX(I)*G2 + LDAV)
        SIGNYY(I) = SIGOYY(I) + (UN-D(I))*(DEPSYY(I)*G2 + LDAV)
        SIGNZZ(I) = SIGOZZ(I) + (UN-D(I))*(DEPSZZ(I)*G2 + LDAV)
        SIGNXY(I) = SIGOXY(I) + (UN-D(I))*DEPSXY(I)*G
        SIGNYZ(I) = SIGOYZ(I) + (UN-D(I))*DEPSYZ(I)*G
        SIGNZX(I) = SIGOZX(I) + (UN-D(I))*DEPSZX(I)*G
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
        J2(I) = UNDEMI*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .          +       SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        ! Third deviatoric invariant
        J3(I) = SXX(I) * SYY(I) * SZZ(I)  
     .        + SXY(I) * SYZ(I) * SZX(I) * DEUX
     .        - SXX(I) * SYZ(I)**2
     .        - SYY(I) * SZX(I)**2
     .        - SZZ(I) * SXY(I)**2 
        ! Drucker equivalent stress
        FDR(I) = J2(I)**3 - CDR*(J3(I)**2)
        ! Checking equivalent stress values
        IF (FDR(I) > ZERO) THEN
          SIGDR(I) = KDR * EXP((UN/SIX)*LOG(FDR(I)))  ! FDR(I)**(1/6)
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
          ETAT(I) = UN
        ENDIF
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !=======================================================================
      DO I=1,NEL
        ! If the element is not broken
        IF (OFF(I)==UN) THEN
          FDAM(I) = DEUX*Q1*FS(I)*COSH(Q2*ETAT(I)*TRSIG(I)/YLD(I)/DEUX) - (Q1*FS(I))**2
          PHI(I)  = (SIGDR(I) / YLD(I))**2 - UN + FDAM(I)
        ! If the element is broken  
        ELSE
          PHI(I) = ZERO
        ENDIF 
      ENDDO
c
      ! Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL         
        IF ((PHI(I)>ZERO).AND.(OFF(I) == UN).AND.(FT(I)<FR)) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO
c
      !====================================================================
      ! - PLASTIC CORRECTION WITH BACKWARD EULER METHOD (NEWTON RESOLUTION)
      !==================================================================== 
c      
      ! Number of maximum Newton iterations
      NITER = 3
c      
      ! Loop over plastifying elements
#include "vectorize.inc"
      DO II=1,NINDX 
c      
        ! Number of the element with plastic behaviour                                              
        I = INDEX(II) 
c        
        ! Initialization of the iterative Newton procedure
        SIGDR2(I) = SIGDR(I)**2
        YLD2I(I)  = UN / YLD(I)**2
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
        DO II=1,NINDX 
          I = INDEX(II)
c        
          ! Note     : in this part, the purpose is to compute for each iteration
          ! a plastic multiplier allowing to update internal variables to satisfy
          ! the consistency condition using the backward Euler implicit method
          ! with a Newton-Raphson iterative procedure
          ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
	  ! -> PHI          : current value of yield function (known)
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
          YLD2I(I)    = UN/(YLD(I)**2)
          DPHI_DSIG   = DEUX*SIGDR(I)*YLD2I(I)    
          FSINH       = SINH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*DEUX))
          DPHI_DTRSIG = Q1*Q2*ETAT(I)*FS(I)*FSINH/YLD(I)      
c          
          ! Computation of the Eulerian norm of the stress tensor
          NORMSIG = SQRT(SIGNXX(I)*SIGNXX(I)
     .            + SIGNYY(I)*SIGNYY(I)
     .            + SIGNZZ(I)*SIGNZZ(I)  
     .            + DEUX*SIGNXY(I)*SIGNXY(I)
     .            + DEUX*SIGNYZ(I)*SIGNYZ(I)
     .            + DEUX*SIGNZX(I)*SIGNZX(I))            
c        
          ! Derivative with respect to Fdr 
          FDR(I)     =  (J2(I)/(NORMSIG**2))**3 - CDR*((J3(I)/(NORMSIG**3))**2)       
          DPHI_DFDR  =  DPHI_DSIG*KDR*(UN/SIX)*EXP(-(CINQ/SIX)*LOG(FDR(I)))             
          DSDRDJ2    =  DPHI_DFDR*TROIS*(J2(I)/(NORMSIG**2))**2                             
          DSDRDJ3    = -DPHI_DFDR*DEUX*CDR*(J3(I)/(NORMSIG**3))         
          ! dJ3/dSig
          DJ3DSXX =  DTIERS*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .             -  TIERS*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             -  TIERS*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)                 
          DJ3DSYY = - TIERS*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .             + DTIERS*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             -  TIERS*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)             
          DJ3DSZZ = - TIERS*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .              - TIERS*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .             + DTIERS*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)
          DJ3DSXY = DEUX*(SXX(I)*SXY(I) + SXY(I)*SYY(I) + SZX(I)*SYZ(I))/(NORMSIG**2)                   
          DJ3DSYZ = DEUX*(SXY(I)*SZX(I) + SYY(I)*SYZ(I) + SYZ(I)*SZZ(I))/(NORMSIG**2)                     
          DJ3DSZX = DEUX*(SXX(I)*SZX(I) + SXY(I)*SYZ(I) + SZX(I)*SZZ(I))/(NORMSIG**2)              
          ! dPhi/dSig
          NORMXX(I) = DSDRDJ2*SXX(I)/NORMSIG + DSDRDJ3*DJ3DSXX + DPHI_DTRSIG
          NORMYY(I) = DSDRDJ2*SYY(I)/NORMSIG + DSDRDJ3*DJ3DSYY + DPHI_DTRSIG
          NORMZZ(I) = DSDRDJ2*SZZ(I)/NORMSIG + DSDRDJ3*DJ3DSZZ + DPHI_DTRSIG
          NORMXY(I) = DEUX*DSDRDJ2*SXY(I)/NORMSIG + DSDRDJ3*DJ3DSXY
          NORMYZ(I) = DEUX*DSDRDJ2*SYZ(I)/NORMSIG + DSDRDJ3*DJ3DSYZ
          NORMZX(I) = DEUX*DSDRDJ2*SZX(I)/NORMSIG + DSDRDJ3*DJ3DSZX              
c          
          ! 2 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c        
          !   a) Derivative with respect stress increments tensor DSIG			
          !   --------------------------------------------------------
          TRDFDS(I) = NORMXX(I) + NORMYY(I) + NORMZZ(I)  
          DFDSIG2   = NORMXX(I) * (UN-D(I))*(NORMXX(I)*G2 + LAM*TRDFDS(I))
     .              + NORMYY(I) * (UN-D(I))*(NORMYY(I)*G2 + LAM*TRDFDS(I))
     .              + NORMZZ(I) * (UN-D(I))*(NORMZZ(I)*G2 + LAM*TRDFDS(I)) 
     .              + NORMXY(I) * (UN-D(I))*NORMXY(I) * G
     .              + NORMYZ(I) * (UN-D(I))*NORMYZ(I) * G
     .              + NORMZX(I) * (UN-D(I))*NORMZX(I) * G              
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
          SIG_DFDSIG(I) = SIGNXX(I) * NORMXX(I)
     .                  + SIGNYY(I) * NORMYY(I)
     .                  + SIGNZZ(I) * NORMZZ(I)
     .                  + SIGNXY(I) * NORMXY(I)
     .                  + SIGNYZ(I) * NORMYZ(I)
     .                  + SIGNZX(I) * NORMZX(I) 
          DPLA_DLAM(I) = SIG_DFDSIG(I) / ((UN - FT(I))*YLD(I))           
c          
          !  c) Derivative with respect to the yield stress
          !  ----------------------------------------------
          SIGDR2(I) =  SIGDR(I)**2                        
          DPHI_DYLD = -DEUX*SIGDR2(I)/YLD(I)**3-DPHI_DTRSIG*TRSIG(I)/YLD(I)                 
c       
          !  d) Derivative of PHI with respect to DLAM
          DPHI_DLAM(I) = - DFDSIG2 + DPHI_DYLD*DYLD_DPLA*DPLA_DLAM(I)
          DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20) ,DPHI_DLAM(I))  
c          
          ! 3 - Computation of plastic multiplier and variables update
          !----------------------------------------------------------
c          
          ! Computation of the plastic multiplier
          DLAM = -PHI(I)/DPHI_DLAM(I)  
c          
	  ! Plastic strains tensor update
          DPXX(I)  = DLAM * NORMXX(I)
          DPYY(I)  = DLAM * NORMYY(I)
          DPZZ(I)  = DLAM * NORMZZ(I)
          DPXY(I)  = DLAM * NORMXY(I)
          DPYZ(I)  = DLAM * NORMYZ(I)
          DPZX(I)  = DLAM * NORMZX(I)
          TRDEP    = DPXX(I) + DPYY(I) + DPZZ(I)  
c          
          ! Elasto-plastic stresses update   
          LDAV      = TRDEP * LAM
          SIGNXX(I) = SIGNXX(I) - (UN-D(I))*(DPXX(I)*G2 + LDAV)
          SIGNYY(I) = SIGNYY(I) - (UN-D(I))*(DPYY(I)*G2 + LDAV)
          SIGNZZ(I) = SIGNZZ(I) - (UN-D(I))*(DPZZ(I)*G2 + LDAV)
          SIGNXY(I) = SIGNXY(I) - (UN-D(I))*DPXY(I)*G
          SIGNYZ(I) = SIGNYZ(I) - (UN-D(I))*DPYZ(I)*G
          SIGNZX(I) = SIGNZX(I) - (UN-D(I))*DPZX(I)*G        
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
          DDEP     = (DLAM/((UN - FT(I))*YLD(I)))*SIG_DFDSIG(I)
          DPLA(I)  = DPLA(I) + DDEP 
          PLA(I)   = PLA(I)  + DDEP   
c          
          ! Drucker equivalent stress update
          J2(I)    = UNDEMI*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .             +         SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
          J3(I)    = SXX(I) * SYY(I) * SZZ(I) + DEUX   * SXY(I) * SYZ(I) * SZX(I)
     .             - SXX(I) * SYZ(I)**2 - SYY(I) * SZX(I)**2 - SZZ(I) * SXY(I)**2 
          FDR(I)   = J2(I)**3 - CDR*(J3(I)**2)
          SIGDR(I) = KDR * EXP((UN/SIX)*LOG(FDR(I)))
          ! Computation of the stress triaxiality and the etaT factor
          TRIAX(I) = (TRSIG(I)*THIRD)/SIGDR(I)
          IF (TRSIG(I)<ZERO) THEN
            ETAT(I) = ZERO
          ELSE
            ETAT(I) = UN
          ENDIF
c          
          ! Hardening law update
          FHARD(I) = YLD0 + HARD * PLA(I) + QVOCE*(UN-EXP(-BVOCE*PLA(I)))
c        
          ! Yield stress update
          YLD(I) = FHARD(I) * FRATE(I) * FTHERM(I)
          YLD(I) = MAX(YLD(I), EM10)
c
          ! Yield function value update
          SIGDR2(I) = SIGDR(I)**2
          YLD2I(I)  = UN / YLD(I)**2
          FCOSH     = COSH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*DEUX)) 
          FDAM(I)   = DEUX*Q1*FS(I)*FCOSH - (Q1*FS(I))**2
          PHI(I)    = SIGDR2(I) * YLD2I(I) - UN + FDAM(I)
c          
        ENDDO
        ! End of the loop over the iterations  
      ENDDO 
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH NEWTON IMPLICIT ITERATIVE METHOD
      !===================================================================
c          
      ! Storing new values
      DO I=1,NEL
        ! If element is broken
        IF (FT(I) >= FR) THEN
          DPLA(I)    = ZERO
          SIGNXX(I)  = ZERO
          SIGNYY(I)  = ZERO
          SIGNZZ(I)  = ZERO
          SIGNXY(I)  = ZERO
          SIGNYZ(I)  = ZERO
          SIGNZX(I)  = ZERO
          SIGDR(I)   = ZERO
        ENDIF
        ! USR Outputs
        UVAR(I,1)  = FG(I)            ! Growth damage
        UVAR(I,2)  = FN(I)            ! Nucleation damage
        UVAR(I,3)  = FSH(I)           ! Shear damage
        UVAR(I,4)  = MIN(FT(I),FR)    ! Total damage
        UVAR(I,5)  = MIN(FS(I),UN/Q1) ! Effective damage
        UVAR(I,6)  = PLA_NL(I)        ! Non-local cumulated plastic strain
        UVAR(I,7)  = PLAP_NL(I)       ! Non-local plastic strain-rate   
        UVAR(I,8)  = YLD(I)           ! Yield stress
        ! Standard outputs        
        DMG(I)     = FT(I)/FR         ! Normalized total damage
        SEQ(I)     = SIGDR(I)         ! SIGEQ
        ! Plastic strain-rate (filtered)
        DPDT       = DPLA(I) / MAX(EM20,TIMESTEP)
        EPSD(I)    = AFILTR * DPDT + (UN - AFILTR) * EPSD(I)
        ! Coefficient for hourglass
        ET(I)      = HARDP(I)*FRATE(I) / (HARDP(I)*FRATE(I) + YOUNG)
        ! Variable to regularize (cumulated plastic strain)
        VARNL(I)   = PLA(I)
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT((BULK + QTIERS*G)/RHO0(I))
        ! Storing the yield stress
        SIGY(I)    = YLD(I)
      ENDDO 
c
      END
