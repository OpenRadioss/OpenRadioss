Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  MAT104_LDAM_NICE              source/materials/mat/mat104/mat104_ldam_nice.F
Chd|-- called by -----------
Chd|        SIGEPS104                     source/materials/mat/mat104/sigeps104.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAT104_LDAM_NICE(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     RHO0    ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,TEMPEL  ,VARNL   ,DMG     ,TEMP    ,
     8     SEQ     ,PLA_NL  ,L_PLANL ,PLAP_NL ,L_EPSDNL)
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
      INTEGER ,INTENT(IN) :: L_PLANL,L_EPSDNL 
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO0,RHO,TEMPEL,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real, DIMENSION(NEL*L_PLANL), INTENT(IN) :: 
     .   PLA_NL
      my_real, DIMENSION(NEL*L_EPSDNL), INTENT(IN) :: 
     .   PLAP_NL
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT) :: 
     .   PLA,DPLA,EPSD,OFF,VARNL,TEMP,SEQ
      my_real ,DIMENSION(NEL,6), INTENT(INOUT)     :: 
     .   DMG
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,K,II,IGURSON,NINDX,INDEX(NEL),NICE
c
      DOUBLE PRECISION ::
     .   YOUNG,BULK,LAM,G,G2,NU,CDR,KDR,HARD,YLD0,QVOCE,BVOCE,JCC,
     .   EPSP0,MTEMP,TINI,TREF,ETA,CP,DPIS,DPAD,ASRATE,AFILTR,HKHI,
     .   Q1,Q2,ED,AN,EPN,KW,FR,FC,F0,NORMSIG,DTI
      DOUBLE PRECISION ::
     .   H,LDAV,TRDEP,TRDFDS,SIGVM,SIGDR2,YLD2I,OMEGA,
     .   DTEMP,FCOSH,FSINH,DPDT,DLAM,DDEP
      DOUBLE PRECISION ::
     .   DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX,DSDRDJ2,DSDRDJ3,
     .   DJ3DSXX,DJ3DSYY,DJ3DSZZ,DJ3DSXY,DJ3DSYZ,DJ3DSZX,
     .   DJ2DSXX,DJ2DSYY,DJ2DSZZ,DJ2DSXY,DJ2DSYZ,DJ2DSZX,
     .   DFDSXX,DFDSYY,DFDSZZ,DFDSXY,DFDSYZ,DFDSZX,
     .   NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,
     .   DENOM,DPHI,SDPLA,DPHI_DTRSIG,SDV_DFDSIG,SIG_DFDSIG,DFDSIG2,
     .   DPHI_DSIG,DPHI_DYLD,DPHI_DFDR,DPHI_DFS,DFS_DFT,
     .   DFN_DLAM,DFSH_DLAM,DFG_DLAM,DFT_DLAM,DFDPLA,DYLD_DPLA_NL,
     .   DFN,DFSH,DFG,DFT,DYLD_DPLA,DYLD_DTEMP,DTEMP_DLAM
c
      DOUBLE PRECISION, DIMENSION(NEL) :: 
     .   DSIGXX,DSIGYY,DSIGZZ,DSIGXY,DSIGYZ,DSIGZX,TRSIG,
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,SIGM,J2,J3,SIGDR,YLD,WEITEMP,
     .   HARDP,FHARD,FRATE,FTHERM,FDR,DEPXX,DEPYY,DEPZZ,DEPXY,DEPYZ,DEPZX,
     .   FDAM,PHI0,PHI,FT,FS,FG,FN,FSH,DPLA_DLAM,D,TRIAX,ETAT
c
      DOUBLE PRECISION, DIMENSION(NEL) :: 
     .   DLAM_NL,DDPNL,DPLA_NL
      !=======================================================================
      !    DRUCKER - VOCE - JOHNSON-COOK MATERIAL LAW WITH GURSON DAMAGE
      !        USING LOCAL DAMAGE OR NON-LOCAL MICROMORPHIC METHOD
      !=======================================================================
      !UVAR(1)   PHI     YIELD FUNCTION VALUE
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
      DTI     = ONE / MAX(TIMESTEP, EM20)
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
        IF (OFF(I) < EM03) OFF(I) = ZERO
        IF (OFF(I) < ONE)  OFF(I) = OFF(I)*FOUR_OVER_5
        ! User inputs
        PHI0(I)  = UVAR(I,1)  ! Previous yield function value
        ! Damage variables
        FG(I)    = DMG(I,2)   ! Growth damage
        FN(I)    = DMG(I,3)   ! Nucleation damage
        FSH(I)   = DMG(I,4)   ! Shear damage
        FT(I)    = DMG(I,5)   ! Total damage
        FS(I)    = DMG(I,6)   ! Effective damage
        ! Standard inputs
        D(I)     = Q1*FS(I)   ! Effective normalized damage
        DPLA(I)  = ZERO       ! Initialization of the plastic strain increment
        ET(I)    = ONE        ! Initialization of hourglass coefficient
        HARDP(I) = ZERO       ! Initialization of hourglass coefficient
        IF (IGURSON == 2) THEN
          IF (OFF(I) == ONE) THEN 
            DPLA_NL(I) = MAX(VARNL(I),ZERO)     ! Non-local plastic strain increment
          ELSE 
            DPLA_NL(I) = ZERO
          ENDIF
        ENDIF    
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
        ELSE
          TEMP(1:NEL) = TEMPEL(1:NEL)
        ENDIF
      ENDIF
c      
      ! Computation of the initial yield stress 
      DO I=1,NEL
        ! a) - Hardening law
        FHARD(I) = YLD0 + HARD*PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
        IF (IGURSON == 2) THEN 
          IF (PLA_NL(I) - PLA(I) < ZERO) THEN
            FHARD(I) = FHARD(I) - HKHI*(PLA_NL(I) - PLA(I))     
          ENDIF
        ENDIF
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
        LDAV      = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAM
        SIGNXX(I) =  SIGOXX(I) + (ONE-D(I))*(DEPSXX(I)*G2 + LDAV)
        SIGNYY(I) =  SIGOYY(I) + (ONE-D(I))*(DEPSYY(I)*G2 + LDAV)
        SIGNZZ(I) =  SIGOZZ(I) + (ONE-D(I))*(DEPSZZ(I)*G2 + LDAV)
        SIGNXY(I) =  SIGOXY(I) + (ONE-D(I))*DEPSXY(I)*G
        SIGNYZ(I) =  SIGOYZ(I) + (ONE-D(I))*DEPSYZ(I)*G
        SIGNZX(I) =  SIGOZX(I) + (ONE-D(I))*DEPSZX(I)*G
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
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !========================================================================
      DO I=1,NEL
        ! If the element is not broken
        IF (OFF(I)==ONE) THEN
          FDAM(I) = TWO*Q1*FS(I)*COSH(Q2*ETAT(I)*TRSIG(I)/YLD(I)/TWO) - (Q1*FS(I))**2
          PHI(I)  = (SIGDR(I) / YLD(I))**2 - ONE + FDAM(I)
        ! If the element is broken  
        ELSE
          PHI(I) = ZERO
        ENDIF  
      ENDDO
c      
      ! Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL         
        IF ((PHI(I)>ZERO).AND.(OFF(I) == ONE).AND.(FT(I)<FR)) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO
c      
      !====================================================================
      ! - PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !====================================================================
#include "vectorize.inc" 
      DO II=1,NINDX   
c                                               
        ! Number of the element with plastic behaviour                                          
        I = INDEX(II) 
c        
        ! Computation of the trial stress increment
        LDAV      = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAM
        DSIGXX(I) = (ONE-D(I))*(DEPSXX(I)*G2 + LDAV)
        DSIGYY(I) = (ONE-D(I))*(DEPSYY(I)*G2 + LDAV)
        DSIGZZ(I) = (ONE-D(I))*(DEPSZZ(I)*G2 + LDAV)
        DSIGXY(I) = (ONE-D(I))*DEPSXY(I)*G   
        DSIGYZ(I) = (ONE-D(I))*DEPSYZ(I)*G     
        DSIGZX(I) = (ONE-D(I))*DEPSZX(I)*G   
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
        ! Computation of the stress triaxiality and the etaT factor
        TRIAX(I) = (TRSIG(I)*THIRD)/SIGDR(I)
        IF (TRSIG(I)<ZERO) THEN
          ETAT(I) = ZERO
        ELSE
          ETAT(I) = ONE
        ENDIF
c        
        ! Note : in this part, the purpose is to compute for each iteration
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
        ! Derivative with respect to the equivalent stress and trace
        YLD2I       = ONE/(YLD(I)**2)
        DPHI_DSIG   = TWO*SIGDR(I)*YLD2I    
        FSINH       = SINH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO))
        DPHI_DTRSIG = Q1*Q2*ETAT(I)*FS(I)*FSINH/YLD(I) 
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
        NORMXX  = DSDRDJ2*SXX(I)/NORMSIG + DSDRDJ3*DJ3DSXX + DPHI_DTRSIG
        NORMYY  = DSDRDJ2*SYY(I)/NORMSIG + DSDRDJ3*DJ3DSYY + DPHI_DTRSIG
        NORMZZ  = DSDRDJ2*SZZ(I)/NORMSIG + DSDRDJ3*DJ3DSZZ + DPHI_DTRSIG
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
        !--------------------------------------------------------   
        !   a) Derivative with respect stress increments tensor DSIG
        !   --------------------------------------------------------
        TRDFDS  = NORMXX + NORMYY + NORMZZ  
        DFDSIG2 = NORMXX*(ONE-D(I))*(NORMXX*G2 + LAM*TRDFDS)
     .          + NORMYY*(ONE-D(I))*(NORMYY*G2 + LAM*TRDFDS) 
     .          + NORMZZ*(ONE-D(I))*(NORMZZ*G2 + LAM*TRDFDS) 
     .          + NORMXY*(ONE-D(I))*NORMXY*G
     .          + NORMYZ*(ONE-D(I))*NORMYZ*G
     .          + NORMZX*(ONE-D(I))*NORMZX*G 
c     
        !   b) Derivatives with respect to plastic strain P 
        !   ------------------------------------------------
c        
        !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
        !     ----------------------------------------------------------------------------
        HARDP(I)  = HARD + QVOCE*BVOCE*EXP(-BVOCE*PLA(I))
        IF (IGURSON == 2) THEN 
          IF (PLA_NL(I) - PLA(I) < ZERO) THEN
            HARDP(I) = HARDP(I) + HKHI   
          ENDIF
        ENDIF      
        DYLD_DPLA = HARDP(I)*FRATE(I)*FTHERM(I)  
c        
        IF (IGURSON == 2) THEN 
          IF (PLA_NL(I) - PLA(I) < ZERO) THEN
            DYLD_DPLA_NL = -HKHI*FRATE(I)*FTHERM(I) 
          ELSE
            DYLD_DPLA_NL = ZERO
          ENDIF  
        ENDIF  
c        
        !     ii) Derivative of dPLA with respect to DLAM
        !     -------------------------------------------
        SDV_DFDSIG = SXX(I) * NORMXX
     .             + SYY(I) * NORMYY
     .             + SZZ(I) * NORMZZ
     .             + SXY(I) * NORMXY
     .             + SYZ(I) * NORMYZ
     .             + SZX(I) * NORMZX        
        SIG_DFDSIG = SIGOXX(I) * NORMXX
     .             + SIGOYY(I) * NORMYY
     .             + SIGOZZ(I) * NORMZZ
     .             + SIGOXY(I) * NORMXY
     .             + SIGOYZ(I) * NORMYZ
     .             + SIGOZX(I) * NORMZX         
        DPLA_DLAM(I) = SIG_DFDSIG / ((ONE - FT(I))*YLD(I))    
c     
        !  c) Derivatives with respect to the temperature TEMP 
        !  ---------------------------------------------------
        IF (JTHE == 0 .and. CP > ZERO) THEN
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
        DPHI_DYLD = -TWO*SIGDR2/YLD(I)**3-DPHI_DTRSIG*TRSIG(I)/YLD(I)
c        
        !  e) Derivatives with respect to the damage variables: FG,FN,FSH
        !  -------------------------------------------------------------- 
c              
        !    i)  Derivative of PHI with respect to the damage FS        
        FCOSH    = COSH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO)) 
        DPHI_DFS = TWO*Q1*FCOSH - TWO*Q1*Q1*FS(I)
c        
        !    ii) Derivative of FS with respect to FT
        !    -----------------------------------------------------------        
        IF (FT(I) >= FC) THEN 
          DFS_DFT = ((ONE/Q1)-FC)*(FR-FC)
        ELSE
          DFS_DFT = ONE
        ENDIF
c
        !    iii) Derivative of FN with respect to LAM
        !    -----------------------------------------------------------   
        IF ((PLA(I)>=ED).AND.(FT(I)<FR)) THEN
          ! Case for positive stress triaxiality
          IF (TRIAX(I)>=ZERO) THEN 
            DFN_DLAM = AN*DPLA_DLAM(I)
          ! Case for negative stress triaxiality
          ELSEIF ((TRIAX(I)<ZERO).AND.(TRIAX(I)>=-THIRD)) THEN
            DFN_DLAM =  AN*MAX(ONE + THREE*TRIAX(I),ZERO)*DPLA_DLAM(I)
          ! Other cases
          ELSE
            DFN_DLAM = ZERO          
          ENDIF
        ELSE
          DFN_DLAM = ZERO
        ENDIF
c        
        !    iv) Derivative of FSH with respect to LAM
        !    -----------------------------------------------------------         
        IF ((SIGDR(I) > ZERO).AND.(FT(I)>ZERO).AND.(FT(I)<FR)) THEN
          SIGVM = SQRT(MAX(EM20, THREE*(J2(I)/(NORMSIG**2))))
          OMEGA = ONE - ((TWENTY7 *(J3(I)/(NORMSIG**3)))/(TWO*SIGVM**3))**2
          OMEGA = MAX(ZERO,OMEGA)
          OMEGA = MIN(ONE,OMEGA)
          DFSH_DLAM = KW*OMEGA*FT(I)*SDV_DFDSIG/SIGDR(I)
        ELSE
          DFSH_DLAM = ZERO
        ENDIF
c        
        !    v) Derivative of FG with respect to LAM
        !    -----------------------------------------------------------  
        IF ((FT(I)>ZERO).AND.(FT(I)<FR).AND.(TRDFDS>ZERO)) THEN       
          DFG_DLAM = (ONE-FT(I))*TRDFDS
        ELSE
          DFG_DLAM = ZERO
        ENDIF
c        
        !    vi) Derivative of FT with respect to LAM
        !    -----------------------------------------------------------    
        DFT_DLAM = DFN_DLAM + DFG_DLAM + DFSH_DLAM
c
        !  e) Derivative of PHI with respect to DLAM ( = -DENOM)
        DENOM = DFDSIG2 - (DPHI_DYLD * DYLD_DPLA * DPLA_DLAM(I))
     .        - DPHI_DFS * DFS_DFT * DFT_DLAM
        IF (JTHE == 0 .and. CP > ZERO) THEN
          DENOM = DENOM - DPHI_DYLD * DYLD_DTEMP * DTEMP_DLAM
        ENDIF
        DENOM = SIGN(MAX(ABS(DENOM),EM20) ,DENOM)
c        
        ! 3 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------
c
        ! Computation of the plastic multiplier
        IF (IGURSON == 2) THEN
          DPHI = DPHI + DPHI_DYLD*DYLD_DPLA_NL*DPLA_NL(I)       
        ENDIF
        DLAM  = (DPHI + PHI(I)) / DENOM
        DLAM  = MAX(DLAM, ZERO)
c        
        ! Plastic strains tensor update
        DPXX  = DLAM * NORMXX
        DPYY  = DLAM * NORMYY
        DPZZ  = DLAM * NORMZZ
        DPXY  = DLAM * NORMXY
        DPYZ  = DLAM * NORMYZ
        DPZX  = DLAM * NORMZX 
        TRDEP = DPXX + DPYY + DPZZ  
c        
        ! Cumulated plastic strain and strain rate update
        DDEP     = (DLAM/((ONE - FT(I))*YLD(I)))*SIG_DFDSIG
        DPLA(I)  = DPLA(I) + DDEP 
        PLA(I)   = PLA(I)  + DDEP  
c
        ! Damage variables update
        ! Growth damage
        IF ((FT(I)>ZERO).AND.(FT(I)<FR).AND.(TRDEP>ZERO)) THEN 
          FG(I) = FG(I) + (ONE-FT(I))*TRDEP
        ENDIF
        FG(I) = MAX(FG(I),ZERO)
        ! Nucleation damage
        IF ((PLA(I) >= ED).AND.(FT(I)<FR)) THEN
          ! Case for positive stress triaxiality
          IF (TRIAX(I)>=ZERO) THEN 
            FN(I) = FN(I) + AN*DDEP
          ! Case for negative stress triaxiality
          ELSEIF ((TRIAX(I)<ZERO).AND.(TRIAX(I)>=-THIRD)) THEN
            FN(I) = FN(I) + AN*MAX(ONE + THREE*TRIAX(I),ZERO)*DDEP
          ENDIF
        ENDIF
        FN(I) = MAX(FN(I),ZERO)
        ! Shear damage
        IF ((SIGDR(I) > ZERO).AND.(FT(I)>ZERO).AND.(FT(I)<FR)) THEN 
          SIGVM   = SQRT(MAX(EM20, THREE*(J2(I)/(NORMSIG**2))))
          OMEGA   = ONE - ((TWENTY7 *(J3(I)/(NORMSIG**3)))/(TWO*SIGVM**3))**2
          OMEGA   = MAX(ZERO,OMEGA)
          OMEGA   = MIN(ONE,OMEGA)
          SDPLA   = SXX(I)*DPXX + SYY(I)*DPYY + SZZ(I)*DPZZ
     .            + SXY(I)*DPXY + SYZ(I)*DPYZ + SZX(I)*DPZX
          FSH(I)  = FSH(I) + KW*OMEGA*FT(I)*(SDPLA/SIGDR(I))
        ENDIF
        FSH(I) = MAX(FSH(I),ZERO)
        ! Total damage
        FT(I) = F0 + FG(I) + FN(I) + FSH(I) 
        FT(I) = MIN(FT(I),FR)
        ! Effective damage
        IF (FT(I) < FC)THEN
          FS(I) = FT(I)
        ELSE
          FS(I) = FC + (ONE/Q1 - FC) * (FT(I)-FC)/(FR-FC)
        ENDIF
        FS(I) = MIN(FS(I),ONE/Q1)
        D(I)  = Q1*FS(I)
c        
        ! Temperature update
        IF (JTHE == 0 .AND. CP > ZERO ) THEN 
          DTEMP     = WEITEMP(I)*YLD(I)*(ONE-FT(I))*DDEP*ETA/(RHO0(I)*CP)
          TEMP(I)   = TEMP(I) + DTEMP
          FTHERM(I) = ONE - MTEMP* (TEMP(I) - TREF)
        ENDIF        
c             
        ! Hardening law update
        FHARD(I) = YLD0 + HARD * PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
        IF (IGURSON == 2) THEN 
          IF (PLA_NL(I) - PLA(I) < ZERO) THEN
            FHARD(I) = FHARD(I) - HKHI*(PLA_NL(I) - PLA(I)) 
          ENDIF    
        ENDIF
c        
        ! Yield stress update
        YLD(I) = FHARD(I) * FRATE(I) * FTHERM(I)
        YLD(I) = MAX(YLD(I), EM10)
c        
        ! Elasto-plastic stresses update   
        LDAV      = TRDEP * LAM
        SIGNXX(I) = SIGOXX(I) + DSIGXX(I) - (ONE-D(I))*(DPXX*G2 + LDAV)
        SIGNYY(I) = SIGOYY(I) + DSIGYY(I) - (ONE-D(I))*(DPYY*G2 + LDAV)
        SIGNZZ(I) = SIGOZZ(I) + DSIGZZ(I) - (ONE-D(I))*(DPZZ*G2 + LDAV)
        SIGNXY(I) = SIGOXY(I) + DSIGXY(I) - (ONE-D(I))*DPXY*G
        SIGNYZ(I) = SIGOYZ(I) + DSIGYZ(I) - (ONE-D(I))*DPYZ*G
        SIGNZX(I) = SIGOZX(I) + DSIGZX(I) - (ONE-D(I))*DPZX*G        
        TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
        SIGM(I)   = -TRSIG(I) * THIRD
        SXX(I)    = SIGNXX(I) + SIGM(I)
        SYY(I)    = SIGNYY(I) + SIGM(I)
        SZZ(I)    = SIGNZZ(I) + SIGM(I)
        SXY(I)    = SIGNXY(I)
        SYZ(I)    = SIGNYZ(I)
        SZX(I)    = SIGNZX(I)        
c               
        ! Drucker equivalent stress update
        J2(I)    = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .             +         SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        J3(I)    = SXX(I) * SYY(I) * SZZ(I) + TWO   * SXY(I) * SYZ(I) * SZX(I)
     .             - SXX(I) * SYZ(I)**2 - SYY(I) * SZX(I)**2 - SZZ(I) * SXY(I)**2 
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
        ! Yield function value update
        SIGDR2  = SIGDR(I)**2
        YLD2I   = ONE / YLD(I)**2
        FCOSH   = COSH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO))
        FDAM(I) = TWO*Q1*FS(I)*FCOSH - (Q1*FS(I))**2
        PHI(I)  = SIGDR2 * YLD2I - ONE + FDAM(I)           
      ENDDO
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !===================================================================
c      
      ! Storing new values
      DO I=1,NEL
        ! If element is broken
        IF (FT(I) >= FR) THEN 
          IF (OFF(I) == ONE) OFF(I) = FOUR_OVER_5
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
        IF (DPLA(I) > ZERO) THEN 
          UVAR(I,1) = PHI(I)           ! Yield function value
        ELSE
          UVAR(I,1) = ZERO
        ENDIF
        ! Standard outputs
        DMG(I,1)   = FT(I)/FR          ! Normalized total damage
        DMG(I,2)   = FG(I)             ! Growth damage
        DMG(I,3)   = FN(I)             ! Nucleation damage 
        DMG(I,4)   = FSH(I)            ! Shear damage
        DMG(I,5)   = MIN(FT(I),FR)     ! Total damage
        DMG(I,6)   = MIN(FS(I),ONE/Q1) ! Effective damage
        SEQ(I)     = SIGDR(I)          ! Equivalent stress
        DPDT       = DPLA(I) / MAX(EM20,TIMESTEP)
        ! Plastic strain-rate (filtered)
        EPSD(I)    = AFILTR * DPDT + (ONE - AFILTR) * EPSD(I)
        ! Coefficient for hourglass
        ET(I)      = HARDP(I)*FRATE(I) / (HARDP(I)*FRATE(I) + YOUNG)
        ! Variable to regularize (cumulated plastic strain)
        IF (IGURSON==2) THEN
          VARNL(I)  = PLA(I)         
        ENDIF
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT((BULK + FOUR_OVER_3*G)/RHO0(I))
        ! Storing the yield stress
        SIGY(I)    = YLD(I)
      ENDDO 
c
      END
