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
!||    mat104c_ldam_newton   ../engine/source/materials/mat/mat104/mat104c_ldam_newton.F
!||--- called by ------------------------------------------------------
!||    sigeps104c            ../engine/source/materials/mat/mat104/sigeps104c.F
!||====================================================================
      SUBROUTINE MAT104C_LDAM_NEWTON(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   , 
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     GS      ,RHO     ,PLA     ,DPLA    ,EPSD    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     7     THK     ,SIGY    ,ET      ,TEMPEL  ,DPLA_NL ,DMG     , 
     8     TEMP    ,SEQ     ,PLA_NL  ,L_PLANL ,PLAP_NL ,L_EPSDNL)
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
      INTEGER, INTENT(IN) :: L_PLANL,L_EPSDNL 
      my_real 
     .   TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO,TEMPEL,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   GS,THKLY,DPLA_NL
      my_real, DIMENSION(NEL*L_PLANL), INTENT(IN) :: 
     .   PLA_NL
      my_real, DIMENSION(NEL*L_EPSDNL), INTENT(IN) :: 
     .   PLAP_NL
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,EPSD,OFF,THK,TEMP,SEQ
      my_real ,DIMENSION(NEL,6), INTENT(INOUT) :: 
     .   DMG 
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,II,IGURSON,ITER,NITER,NINDX,INDEX(NEL)
c
      my_real :: 
     .   YOUNG,BULK,LAM,G,G2,NU,CDR,KDR,HARD,YLD0,QVOCE,BVOCE,JCC,
     .   EPSP0,MTEMP,TINI,TREF,ETA,CP,DPIS,DPAD,ASRATE,AFILTR,HKHI,
     .   Q1,Q2,ED,AN,EPN,KW,FR,FC,F0,A11,A12,NNU,DTI
      my_real ::  
     .   H,LDAV,TRDFDS,SIGVM,OMEGA,
     .   DTEMP,FCOSH,FSINH,DPDT,DLAM,DDEP
      my_real :: 
     .   DSDRDJ2,DSDRDJ3,
     .   DJ3DSXX,DJ3DSYY,DJ3DSXY,DJ3DSZZ,
     .   DJ2DSXX,DJ2DSYY,DJ2DSXY,NORMSIG,
     .   DFDSXX,DFDSYY,DFDSXY,DYLD_DPLA_NL,
     .   NORMXX,NORMYY,NORMXY,NORMZZ,
     .   SDPLA,DPHI_DTRSIG,SIG_DFDSIG,DFDSIG2,SDV_DFDSIG,
     .   DPHI_DSIG,DPHI_DYLD,DPHI_DFDR,DF_DFS,DFS_DFT,DPHI_DFT,
     .   DPHI_DFS,DFN_DLAM,DFSH_DLAM,DFG_DLAM,DFT_DLAM,
     .   DFN,DFSH,DFG,DFT,DYLD_DPLA,DYLD_DTEMP,DTEMP_DLAM
C
      my_real, DIMENSION(NEL) ::
     .   DSIGXX,DSIGYY,DSIGXY,TRSIG,TRDEP,
     .   SXX,SYY,SXY,SZZ,SIGM,J2,J3,SIGDR,YLD,WEITEMP,
     .   HARDP,FHARD,FRATE,FTHERM,DTHERM,FDR,PHI0,TRIAX,
     .   FDAM,PHI,FT,FS,FG,FN,FSH,DPLA_DLAM,DPHI_DLAM,DEZZ,ETAT,
     .   SIGDR2,YLD2I,DPXX,DPYY,DPZZ,DPXY,DLAM_NL
     .   
      !=======================================================================
      !     DRUCKER - VOCE - JOHNSON-COOK MATERIAL LAW WITH GURSON DAMAGE
      !         USING LOCAL DAMAGE OR NON-LOCAL MICROMORPHIC METHOD
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
        ! Damage variables
        FG(I)    = DMG(I,2)   ! Growth damage
        FN(I)    = DMG(I,3)   ! Nucleation damage
        FSH(I)   = DMG(I,4)   ! Shear damage
        FT(I)    = DMG(I,5)   ! Total damage
        FS(I)    = DMG(I,6)   ! Effective damage
        ! Standard inputs
        DPLA(I)  = ZERO       ! Initialization of the plastic strain increment
        ET(I)    = ONE        ! Initialization of hourglass coefficient
        HARDP(I) = ZERO       ! Initialization of hardening modulus           
        DEZZ(I)  = ZERO       ! Initialization of the transverse strain
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
      DO I = 1,NEL
        ! a) - Hardening law
        FHARD(I) = YLD0 + HARD*PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
        IF (IGURSON == 2) THEN 
          IF (PLA_NL(I) - PLA(I) < ZERO) THEN
            FHARD(I) = FHARD(I) - HKHI*(PLA_NL(I) - PLA(I))     
          ENDIF
        ENDIF
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
c
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + (A11*DEPSXX(I) + A12*DEPSYY(I))
        SIGNYY(I) = SIGOYY(I) + (A11*DEPSYY(I) + A12*DEPSXX(I))
        SIGNXY(I) = SIGOXY(I) + (DEPSXY(I)*G)
        SIGNYZ(I) = SIGOYZ(I) + (DEPSYZ(I)*GS(I))
        SIGNZX(I) = SIGOZX(I) + (DEPSZX(I)*GS(I))
        ! Computation of the trace of the trial stress tensor
        TRSIG(I)  = SIGNXX(I) + SIGNYY(I) 
        SIGM(I)   = -TRSIG(I) * THIRD
        ! Computation of the deviatoric trial stress tensor
        SXX(I)    = SIGNXX(I) + SIGM(I)
        SYY(I)    = SIGNYY(I) + SIGM(I)
        SZZ(I)    = SIGM(I)
        SXY(I)    = SIGNXY(I)
        DEZZ(I)   = -NNU*DEPSXX(I) - NNU*DEPSYY(I)
        ! Second deviatoric invariant
        J2(I) = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 ) + SXY(I)**2 
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
      ENDDO
c        
      ! Loop over the iterations     
      DO ITER = 1, NITER
#include "vectorize.inc" 
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
          ! Derivative with respect to the equivalent stress and trace
          YLD2I(I)    = ONE/(YLD(I)**2)
          DPHI_DSIG   = TWO*SIGDR(I)*YLD2I(I)    
          FSINH       = SINH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO))
          DPHI_DTRSIG = Q1*Q2*ETAT(I)*FS(I)*FSINH/YLD(I)           
c        
          ! Computation of the Eulerian norm of the stress tensor
          NORMSIG = SQRT(SIGNXX(I)*SIGNXX(I)
     .                 + SIGNYY(I)*SIGNYY(I)
     .             + TWO*SIGNXY(I)*SIGNXY(I))
          NORMSIG = MAX(NORMSIG,ONE)
c 
          ! Derivative with respect to Fdr 
          FDR(I)     =  (J2(I)/(NORMSIG**2))**3 - CDR*((J3(I)/(NORMSIG**3))**2)       
          DPHI_DFDR  =  DPHI_DSIG*KDR*(ONE/SIX)*EXP(-(FIVE/SIX)*LOG(FDR(I)))             
          DSDRDJ2    =  DPHI_DFDR*THREE*(J2(I)/(NORMSIG**2))**2                             
          DSDRDJ3    = -DPHI_DFDR*TWO*CDR*(J3(I)/(NORMSIG**3))                   
          ! dJ3/dSig                                                        
          DJ3DSXX =  TWO_THIRD*(SYY(I)*SZZ(I))/(NORMSIG**2)
     .                -  THIRD*(SXX(I)*SZZ(I))/(NORMSIG**2)
     .                -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)                 
          DJ3DSYY =    - THIRD*(SYY(I)*SZZ(I))/(NORMSIG**2)
     .             + TWO_THIRD*(SXX(I)*SZZ(I))/(NORMSIG**2)
     .                -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)             
          DJ3DSZZ =    - THIRD*(SYY(I)*SZZ(I))/(NORMSIG**2)
     .                 - THIRD*(SXX(I)*SZZ(I))/(NORMSIG**2)
     .             + TWO_THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)
          DJ3DSXY = TWO*(SXX(I)*SXY(I) + SXY(I)*SYY(I))/(NORMSIG**2)                   
          ! dPhi/dSig
          NORMXX  = DSDRDJ2*SXX(I)/NORMSIG + DSDRDJ3*DJ3DSXX + DPHI_DTRSIG
          NORMYY  = DSDRDJ2*SYY(I)/NORMSIG + DSDRDJ3*DJ3DSYY + DPHI_DTRSIG
          NORMZZ  = DSDRDJ2*SZZ(I)/NORMSIG + DSDRDJ3*DJ3DSZZ + DPHI_DTRSIG
          NORMXY  = TWO*DSDRDJ2*SXY(I)/NORMSIG + DSDRDJ3*DJ3DSXY
c          
          ! 2 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c        
          !   a) Derivative with respect stress increments tensor DSIG
          !   --------------------------------------------------------     
          TRDFDS  = NORMXX + NORMYY + NORMZZ  
          DFDSIG2 = NORMXX * (A11*NORMXX + A12*NORMYY)
     .            + NORMYY * (A11*NORMYY + A12*NORMXX)
     .            + NORMXY * NORMXY * G
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
          !     ii) Derivative of dPLA with respect to DLAM
          !     -------------------------------------------
          SDV_DFDSIG = SXX(I) * NORMXX
     .               + SYY(I) * NORMYY
     .               + SZZ(I) * NORMZZ
     .               + SXY(I) * NORMXY        
          SIG_DFDSIG = SIGNXX(I) * NORMXX
     .               + SIGNYY(I) * NORMYY
     .               + SIGNXY(I) * NORMXY       
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
            DTEMP_DLAM = WEITEMP(I)*(ETA/(RHO(I)*CP))*SIG_DFDSIG
          ELSE
            DYLD_DTEMP = ZERO
            DTEMP_DLAM = ZERO
          ENDIF
c          
          !  d) Derivative with respect to the yield stress
          !  ----------------------------------------------
          SIGDR2(I) =  SIGDR(I)**2                        
          DPHI_DYLD = -TWO*SIGDR2(I)/YLD(I)**3-DPHI_DTRSIG*TRSIG(I)/YLD(I)
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
              DFN_DLAM = AN*MAX(ONE + THREE*TRIAX(I),ZERO)*DPLA_DLAM(I)
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
            OMEGA = MAX(OMEGA,ZERO)
            OMEGA = MIN(OMEGA,ONE)
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
          !  e) Derivative of PHI with respect to DLAM   
          DPHI_DLAM(I) = - DFDSIG2 + (DPHI_DYLD*DYLD_DPLA*DPLA_DLAM(I))
     .                   + DPHI_DFS*DFS_DFT*DFT_DLAM
          IF (JTHE == 0 .and. CP > ZERO) THEN
            DPHI_DLAM(I) = DPHI_DLAM(I) + DPHI_DYLD*DYLD_DTEMP*DTEMP_DLAM
          ENDIF
          DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20) ,DPHI_DLAM(I)) 
c                    
          ! 3 - Computation of plastic multiplier and variables update
          !----------------------------------------------------------
c          
          ! Computation of the plastic multiplier
          DLAM = -PHI(I)/DPHI_DLAM(I)    
c          
          ! Plastic strains tensor update
          DPXX(I)  = DLAM * NORMXX
          DPYY(I)  = DLAM * NORMYY
          DPZZ(I)  = DLAM * NORMZZ
          DPXY(I)  = DLAM * NORMXY
          TRDEP(I) = DPXX(I) + DPYY(I) + DPZZ(I)   
c          
          ! Cumulated plastic strain and strain rate update         
          DDEP    = (DLAM/((ONE - FT(I))*YLD(I)))*SIG_DFDSIG
          DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
          PLA(I)  = PLA(I) + DDEP 
c          
          ! Damage variables update
          ! Growth damage
          IF ((FT(I)>ZERO).AND.(FT(I)<FR).AND.(TRDEP(I)>ZERO)) THEN 
            FG(I) = FG(I) + (ONE-FT(I))*TRDEP(I)
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
            SDPLA   = SXX(I)*DPXX(I) + SYY(I)*DPYY(I) + SZZ(I)*DPZZ(I)
     .              + SXY(I)*DPXY(I)
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
c          
          ! Temperature update
          IF (JTHE == 0 .AND. CP > ZERO) THEN   
            DTEMP     = WEITEMP(I)*YLD(I)*(ONE-FT(I))*DDEP*ETA/(RHO(I)*CP)
            TEMP(I)   = TEMP(I) + DTEMP
            FTHERM(I) = ONE - MTEMP*(TEMP(I) - TREF)
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
          SIGNXX(I) = SIGNXX(I) - (A11*DPXX(I) + A12*DPYY(I))
          SIGNYY(I) = SIGNYY(I) - (A11*DPYY(I) + A12*DPXX(I)) 
          SIGNXY(I) = SIGNXY(I) - DPXY(I)*G
          TRSIG(I)  = SIGNXX(I) + SIGNYY(I)
          SIGM(I)   = -TRSIG(I) * THIRD
          SXX(I)    = SIGNXX(I) + SIGM(I)
          SYY(I)    = SIGNYY(I) + SIGM(I)
          SZZ(I)    = SIGM(I)
          SXY(I)    = SIGNXY(I)
c          
          ! Drucker equivalent stress update
          J2(I)    = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 ) + SXY(I)**2
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
          ! Yield function value update
          SIGDR2(I)  = SIGDR(I)**2
          YLD2I(I)   = ONE / YLD(I)**2
          FCOSH   = COSH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO))
          FDAM(I) = TWO*Q1*FS(I)*FCOSH - (Q1*FS(I))**2
          PHI(I)  = SIGDR2(I) * YLD2I(I) - ONE + FDAM(I)  
c          
          ! Transverse strain update
          DEZZ(I) = DEZZ(I) + NNU*DPXX(I) + NNU*DPYY(I) + DPZZ(I)   
c        
        ENDDO
        ! End of the loop over yielding elements 
      ENDDO
      ! End of the loop over the iterations -
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE ITERATIVE METHOD
      !===================================================================
c      
      ! Storing new values
      DO I=1,NEL   
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
          IF (OFF(I) == ONE) OFF(I) = FOUR_OVER_5
          DPLA(I)    = ZERO
          SIGNXX(I)  = ZERO
          SIGNYY(I)  = ZERO
          SIGNXY(I)  = ZERO
          SIGNYZ(I)  = ZERO
          SIGNZX(I)  = ZERO
          SEQ(I)     = ZERO
        ENDIF   
        ! Plastic strain-rate (filtered)
        DPDT       = DPLA(I) / MAX(EM20,TIMESTEP)  
        EPSD(I)    = AFILTR * DPDT + (ONE - AFILTR) * EPSD(I)        
        ! Coefficient for hourglass
        IF (DPLA(I) > ZERO) THEN 
          ET(I) = HARDP(I)*FRATE(I) / (HARDP(I)*FRATE(I) + YOUNG)  
        ELSE
          ET(I) = ONE
        ENDIF
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT((A11)/RHO(I))
        ! Storing the yield stress
        SIGY(I)    = YLD(I)
        ! Computation of the thickness variation 
        THK(I)     = THK(I) + DEZZ(I)*THKLY(I)*OFF(I) 
      ENDDO
c      
      END
