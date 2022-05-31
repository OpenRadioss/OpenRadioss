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
Chd|  MAT107C_NEWTON                source/materials/mat/mat107/mat107c_newton.F
Chd|-- called by -----------
Chd|        SIGEPS107C                    source/materials/mat/mat107/sigeps107c.F
Chd|-- calls ---------------
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MAT107C_NEWTON(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2     UPARAM  ,UVAR    ,JTHE    ,OFF     ,RHO     ,
     3     PLA     ,DPLA    ,EPSD    ,SOUNDSP ,SHF     ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,
     8     NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  )
      !=======================================================================
      !      Modules
      !=======================================================================
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
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
#include      "com04_c.inc"
      !=======================================================================
      !      Dummy arguments
      !=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,JTHE,NUMTABL,ITABLE(NUMTABL),NVARTMP
      INTEGER, DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real 
     .   TIME,TIMESTEP,VARTMP(NEL,NVARTMP)
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO,SHF,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   DPLA,OFF
      my_real ,DIMENSION(NEL,6),INTENT(INOUT)      :: 
     .   PLA,EPSD
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
c
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE  
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,K,II,NINDX,INDEX(NEL),NITER,ITER,ITAB,ISMOOTH,IPOS(NEL,2)
c
      my_real 
     .   YOUNG1,YOUNG2,NU12,NU21,G12,A11,A12,A21,A22,C1,
     .   XI1,XI2,K1,K2,K3,K4,K5,K6,G1C,D1,D2,SIGY1,
     .   CINI1,S1,SIGY2,CINI2,S2,SIGY1C,CINI1C,S1C,
     .   SIGY2C,CINI2C,S2C,SIGYT,CINIT,ST,G23,G31
      my_real 
     .   NORMSIG,
     .   DPDT,DLAM,DDEP
      my_real 
     .   NORMXX,NORMYY,NORMXY,
     .   NORMPXX,NORMPYY,NORMPXY,
     .   DPHI_DLAM,DPHI,DFDSIG2,DPHI_DPLA,DPXX,DPYY,DPXY,
     .   DPHI_DR1,DPHI_DR2,DPHI_DR1C,DPHI_DR2C,DPHI_DRT
      my_real
     .   XSCALE1,YSCALE1,XSCALE1C,YSCALE1C,XSCALE2,YSCALE2,XSCALE2C,YSCALE2C,
     .   XSCALET,YSCALET,XVEC(NEL,2),ASRATE
c
      my_real, DIMENSION(NEL) ::
     .   ALPHA1,ALPHA2,ALPHA3,ALPHA4,ALPHA5,R1,R2,R1C,R2C,RT,
     .   DSIGXX,DSIGYY,DSIGXY,DSIGYZ,DSIGZX,HARDP,PHI,ETA1,ETA2,
     .   DR1_DP,DR2_DP,DR1C_DP,DR2C_DP,DRT_DP,HARDR,DPLA2,DPLA3,
     .   DPLA4,DPLA5,DPLA6,BETA1,BETA2
c      
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! Elastic parameters      
      YOUNG1  = UPARAM(1)   ! Young modulus in direction 1 (MD)
      YOUNG2  = UPARAM(2)   ! Young modulus in direction 2 (CD)
      NU12    = UPARAM(4)   ! Poisson's ratio in 12 
      NU21    = UPARAM(5)   ! Poisson's ratio in 21
      A11     = UPARAM(6)   ! Component 11 of orthotropic shell elasticity matrix 
      A12     = UPARAM(7)   ! Component 12 of orthotropic shell elasticity matrix 
      A21     = UPARAM(8)   ! Component 21 of orthotropic shell elasticity matrix 
      A22     = UPARAM(9)   ! Component 22 of orthotropic shell elasticity matrix 
      G12     = UPARAM(10)  ! Shear modulus in 12
      G23     = UPARAM(11)  ! Shear modulus in 23
      G31     = UPARAM(12)  ! Shear modulus in 31
      ITAB    = INT(UPARAM(14))  ! Tabulated yield stress flag
      XI1     = UPARAM(15)  ! 1st coupling parameter
      XI2     = UPARAM(16)  ! 2nd coupling parameter
      K1      = UPARAM(17)  ! 1st plastic potential parameter
      K2      = UPARAM(18)  ! 2nd plastic potential parameter    
      K3      = UPARAM(19)  ! 3rd plastic potential parameter
      K4      = UPARAM(20)  ! 4th plastic potential parameter
      K5      = UPARAM(21)  ! 5th plastic potential parameter
      K6      = UPARAM(22)  ! 6th plastic potential parameter    
      G1C     = UPARAM(23)  ! Correction factor for R1C    
      IF (ITAB == 0) THEN
        D1      = UPARAM(24)  ! 1st Shear yield stress parameter
        D2      = UPARAM(25)  ! 2nd Shear yield stress parameter
        SIGY1   = UPARAM(26)  ! Initial yield stress in tension in direction 1 (MD)
        CINI1   = UPARAM(27)  ! Yield stress intersection with ordinate axis in tension in direction 1 (MD)
        S1      = UPARAM(28)  ! Yield stress slope in tension in direction 1 (MD)  
        SIGY2   = UPARAM(29)  ! Initial yield stress in tension in direction 2 (CD)
        CINI2   = UPARAM(30)  ! Yield stress intersection with ordinate axis in tension in direction 2 (CD)
        S2      = UPARAM(31)  ! Yield stress slope in tension in direction 2 (CD)
        SIGY1C  = UPARAM(32)  ! Initial yield stress in compression in direction 1 (MD)    
        CINI1C  = UPARAM(33)  ! Yield stress intersection with ordinate axis in compression in direction 1 (MD)    
        S1C     = UPARAM(34)  ! Yield stress slope in compression in direction 1 (MD)
        SIGY2C  = UPARAM(35)  ! Initial yield stress in compression in direction 2 (CD)
        CINI2C  = UPARAM(36)  ! Yield stress intersection with ordinate axis in compression in direction 2 (CD)
        S2C     = UPARAM(37)  ! Yield stress slope in compression in direction 2 (CD)  
        SIGYT   = UPARAM(38)  ! Initial yield stress in shear  
        CINIT   = UPARAM(39)  ! Yield stress intersection with ordinate axis in shear
        ST      = UPARAM(40)  ! Yield stress slope in shear
      ELSE
        XSCALE1 = UPARAM(24)
        YSCALE1 = UPARAM(25)
        XSCALE2 = UPARAM(26)
        YSCALE2 = UPARAM(27)
        XSCALE1C= UPARAM(28) 
        YSCALE1C= UPARAM(29)
        XSCALE2C= UPARAM(30)
        YSCALE2C= UPARAM(31)
        XSCALET = UPARAM(32) 
        YSCALET = UPARAM(33)
        ASRATE  = UPARAM(34)
        ASRATE  = (TWO*PI*ASRATE*TIMESTEP)/(TWO*PI*ASRATE*TIMESTEP + ONE)
        ISMOOTH = INT(UPARAM(35))      
      ENDIF
c      
      ! Recovering internal variables
      DO I=1,NEL
        ! OFF parameter for element deletion
        IF (OFF(I) < 0.1) OFF(I) = ZERO
        IF (OFF(I) < ONE)  OFF(I) = OFF(I)*FOUR_OVER_5
        ! Standard inputs
        DPLA(I)  = ZERO      ! Initialization of the "global" plastic strain increment
        DPLA2(I) = ZERO      ! Initialization of the in-plane plastic strain increment
        DPLA3(I) = ZERO      ! Initialization of the transverse shear plastic strain increment       
        DPLA4(I) = ZERO      ! Initialization of the in-plane plastic strain increment
        DPLA5(I) = ZERO      ! Initialization of the transverse shear plastic strain increment  
        DPLA6(I) = ZERO      ! Initialization of the in-plane plastic strain increment
        ET(I)    = ONE        ! Initialization of hourglass coefficient
        HARDP(I) = ZERO      ! Initialization of hourglass coefficient
      ENDDO
c
      ! Computation of the initial yield stress
      IF (ITAB > 0) THEN
        !   -> Tensile yield stress in direction 1 (MD)
        XVEC(1:NEL,1) = PLA(1:NEL,2)
        XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE1
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,1))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(1)),ISMOOTH,NEL,IPOS,XVEC,R1,DR1_DP,HARDR)
        R1(1:NEL)     = R1(1:NEL) * YSCALE1
        DR1_DP(1:NEL) = DR1_DP(1:NEL) * YSCALE1
        VARTMP(1:NEL,1) = IPOS(1:NEL,1)
        !   -> Compressive yield stress in direction 1 (MD)
        XVEC(1:NEL,1) = PLA(1:NEL,3)
        XVEC(1:NEL,2) = EPSD(1:NEL,3) * XSCALE2
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,2))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(2)),ISMOOTH,NEL,IPOS,XVEC,R2,DR2_DP,HARDR)
        R2(1:NEL)     = R2(1:NEL) * YSCALE2
        DR2_DP(1:NEL) = DR2_DP(1:NEL) * YSCALE2
        VARTMP(1:NEL,2) = IPOS(1:NEL,1)  
        !   -> Positive shear yield stress
        XVEC(1:NEL,1) = PLA(1:NEL,4)
        XVEC(1:NEL,2) = EPSD(1:NEL,4) * XSCALE1C
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,3))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(3)),ISMOOTH,NEL,IPOS,XVEC,R1C,DR1C_DP,HARDR)
        R1C(1:NEL)    = R1C(1:NEL) * YSCALE1C
        DR1C_DP(1:NEL)= DR1C_DP(1:NEL) * YSCALE1C
        VARTMP(1:NEL,3) = IPOS(1:NEL,1)
        !   -> Tensile yield stress in direction 2 (CD)
        XVEC(1:NEL,1) = PLA(1:NEL,5)
        XVEC(1:NEL,2) = EPSD(1:NEL,5) * XSCALE2C
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,4))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(4)),ISMOOTH,NEL,IPOS,XVEC,R2C,DR2C_DP,HARDR)
        R2C(1:NEL)    = R2C(1:NEL) * YSCALE2C
        DR2C_DP(1:NEL)= DR2C_DP(1:NEL) * YSCALE2C
        VARTMP(1:NEL,4) = IPOS(1:NEL,1) 
        ! Compressive yield stress in direction 2 (CD)
        XVEC(1:NEL,1) = PLA(1:NEL,6)
        XVEC(1:NEL,2) = EPSD(1:NEL,6) * XSCALET
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,5))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(5)),ISMOOTH,NEL,IPOS,XVEC,RT,DRT_DP,HARDR)
        RT(1:NEL)     = RT(1:NEL) * YSCALET
        DRT_DP(1:NEL) = DRT_DP(1:NEL) * YSCALET
        VARTMP(1:NEL,5) = IPOS(1:NEL,1)
      ENDIF
c
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
c
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A21*DEPSXX(I) + A22*DEPSYY(I) 
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G12
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*G23*SHF(I)
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*G31*SHF(I)
c        
        ! Computation of trial alpha coefficients
        ALPHA1(I) = ZERO
        ALPHA2(I) = ZERO
        ALPHA3(I) = ZERO
        ALPHA4(I) = ZERO
        ALPHA5(I) = ZERO
        IF ((SIGNXX(I) - XI1*SIGNYY(I)) > ZERO)   ALPHA1(I) = ONE
        IF ((SIGNYY(I) - XI2*SIGNXX(I)) > ZERO)   ALPHA2(I) = ONE
        IF (-SIGNXX(I) > ZERO)                    ALPHA3(I) = ONE        
        IF (-SIGNYY(I) > ZERO)                    ALPHA4(I) = ONE
        IF (ABS(SIGNXY(I)) > ZERO)                ALPHA5(I) = ONE
c
        IF (ITAB == 0) THEN 
          ! Computation of yield stresses
          ! Tensile yield stresses
          !   - in direction 1 (MD)
          R1(I)  = SIGY1  + (ONE/(CINI1  + S1*PLA(I,2)))*PLA(I,2)
          !   - in direction 2 (CD)
          R2(I)  = SIGY2  + (ONE/(CINI2  + S2*PLA(I,3)))*PLA(I,3)
          ! Compressive yield stresses
          !    - in direction 1 (MD) (including correction)
          R1C(I) = SIGY1C + (ONE/(CINI1C + S1C*PLA(I,4)))*PLA(I,4)
          R1C(I) = R1C(I)/SQRT(ONE - G1C)
          !    - in direction 2 (CD)
          R2C(I) = SIGY2C + (ONE/(CINI2C + S2C*PLA(I,5)))*PLA(I,5)
          ! Shear yield stress
          ETA1(I) = ONE + ALPHA3(I)*ALPHA4(I)*D1
          ETA2(I) = ONE + ALPHA3(I)*ALPHA4(I)*D2
          RT(I)   = ETA1(I)*SIGYT + ETA2(I)*(ONE/(CINIT + ST*PLA(I,6)))*PLA(I,6)
        ENDIF
c
        ! Computation of BETA switching coefficient
        NORMSIG = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I) + TWO*SIGNXY(I)*SIGNXY(I))
        NORMSIG = MAX(NORMSIG,EM20)
        BETA1(I) = ZERO
        BETA2(I) = ZERO
        IF ((SIGNXX(I)/NORMSIG >  EM01).OR.(SIGNYY(I)/NORMSIG >  EM01)) BETA1(I) = ONE
        IF ((SIGNXX(I)/NORMSIG < -EM01).OR.(SIGNYY(I)/NORMSIG < -EM01)) BETA2(I) = ONE
        IF (((ABS(SIGNXX(I))/NORMSIG)<EM01).AND.((ABS(SIGNYY(I))/NORMSIG)<EM01)) THEN
          BETA1(I) = ONE
          BETA2(I) = ONE
        ENDIF
c
        ! Computation of the yield function
        PHI(I) = ALPHA1(I)*((SIGNXX(I) - XI1*SIGNYY(I))/R1(I))**2 + 
     .           ALPHA2(I)*((SIGNYY(I) - XI2*SIGNXX(I))/R2(I))**2 +  
     .           ALPHA3(I)*((- SIGNXX(I))/R1C(I))**2 + 
     .           ALPHA4(I)*((- SIGNYY(I))/R2C(I))**2 + 
     .           ALPHA5(I)*(ABS(SIGNXY(I))/RT(I))**2 - ONE
      ENDDO
c
      !========================================================================
      ! - CHECKING THE PLASTIC BEHAVIOR
      !========================================================================
c
      ! Checking plastic behavior for all elements
      NINDX = 0
      DO I=1,NEL         
        IF (PHI(I) > ZERO .AND. OFF(I) == ONE) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO   
c      
      !============================================================
      ! - PLASTIC CORRECTION WITH CUTTING PLANE (NEWTON RESOLUTION)
      !============================================================    
c      
      ! Number of Newton iterations
      NITER = 3
c     
      ! Loop over the iterations     
      DO ITER = 1, NITER
#include "vectorize.inc" 
        ! Loop over yielding elements
        DO II=1,NINDX 
          I = INDEX(II)
c        
          ! Note     : in this part, the purpose is to compute for each iteration
          ! a plastic multiplier allowing to update internal variables to satisfy
          ! the consistency condition using the cutting plane method
          ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
	  ! -> PHI          : current value of yield function (known)
          ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
          !                   into account of internal variables kinetic : 
          !                   hardening parameters
c        
	  ! 1 - Computation of DPHI_DSIG the normal to the yield criterion
          !-------------------------------------------------------------          
c    
          NORMXX  = TWO*(ALPHA1(I)/(R1(I)**2))*(SIGNXX(I)-XI1*SIGNYY(I))
     .            - TWO*(ALPHA2(I)*XI2/(R2(I)**2))*(SIGNYY(I)-XI2*SIGNXX(I))
     .            + TWO*(ALPHA3(I)/(R1C(I)**2))*SIGNXX(I) 
          NORMYY  = -TWO*(ALPHA1(I)*XI1/(R1(I)**2))*(SIGNXX(I)-XI1*SIGNYY(I)) 
     .            + TWO*(ALPHA2(I)/(R2(I)**2))*(SIGNYY(I)-XI2*SIGNXX(I))
     .            + TWO*(ALPHA4(I)/(R2C(I)**2))*SIGNYY(I)
          NORMXY  = (ALPHA5(I)/(RT(I)**2))*ABS(SIGNXY(I))*SIGN(ONE,SIGNXY(I))
c
	  ! 2 - Computation of DFP_DSIG the normal to the non-associated plastic potential
          !-------------------------------------------------------------------------------
          !  a) Computation of derivatives
          NORMPXX  = BETA1(I)*(TWO*SIGNXX(I)    + K2*SIGNYY(I)) + BETA2(I)*(TWO*SIGNXX(I)    + K4*SIGNYY(I))
          NORMPYY  = BETA1(I)*(TWO*K1*SIGNYY(I) + K2*SIGNXX(I)) + BETA2(I)*(TWO*K3*SIGNYY(I) + K4*SIGNXX(I))
          NORMPXY  = (BETA1(I)*K5 + BETA2(I)*K6)*SIGNXY(I)
          !  b) Computation of the norm of the derivative
          NORMSIG  = SQRT(NORMPXX*NORMPXX + NORMPYY*NORMPYY + TWO*NORMPXY*NORMPXY)
          NORMSIG  = MAX(NORMSIG,EM20)
          !  c) Computation of the normal to the plastic potential
          NORMPXX  = NORMPXX/NORMSIG
          NORMPYY  = NORMPYY/NORMSIG
          NORMPXY  = NORMPXY/NORMSIG
c             
          ! 3 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c        
          !   a) Derivative with respect stress increments tensor DSIG
          !   --------------------------------------------------------
          DFDSIG2 = NORMXX * (A11*NORMPXX + A12*NORMPYY)
     .            + NORMYY * (A21*NORMPXX + A22*NORMPYY)
     .            + TWO*NORMXY * TWO*NORMPXY * G12
c     
          !   b) Derivatives with respect to hardening parameters 
          !   ---------------------------------------------------
c        
          !      i) Derivatives of PHI with respect to the yield stresses
          DPHI_DR1  = -TWO*ALPHA1(I)*(((SIGNXX(I)-XI1*SIGNYY(I))**2)/(R1(I)**3))
          DPHI_DR2  = -TWO*ALPHA2(I)*(((SIGNYY(I)-XI2*SIGNXX(I))**2)/(R2(I)**3))
          DPHI_DR1C = -TWO*ALPHA3(I)*(SIGNXX(I)**2)/(R1C(I)**3)
          DPHI_DR2C = -TWO*ALPHA4(I)*(SIGNYY(I)**2)/(R2C(I)**3)
          DPHI_DRT  = -TWO*ALPHA5(I)*(SIGNXY(I)**2)/(RT(I)**3)
          !     ii) Derivatives of yield stresses with respect to hardening parameters
          IF (ITAB == 0) THEN
            DR1_DP(I)  = (ONE/(CINI1  + S1*PLA(I,2)))   * (ONE - (S1*PLA(I,2) /(CINI1  + S1*PLA(I,2))))
            DR2_DP(I)  = (ONE/(CINI2  + S2*PLA(I,3)))   * (ONE - (S2*PLA(I,3) /(CINI2  + S2*PLA(I,3))))
            DR1C_DP(I) = (ONE/(CINI1C + S1C*PLA(I,4)))  * (ONE - (S1C*PLA(I,4)/(CINI1C + S1C*PLA(I,4))))
            DR1C_DP(I) = DR1C_DP(I)/SQRT(ONE - G1C)
            DR2C_DP(I) = (ONE/(CINI2C + S2C*PLA(I,5)))  * (ONE - (S2C*PLA(I,5)/(CINI2C + S2C*PLA(I,5))))
            DRT_DP(I)  = ETA2(I)*(ONE/(CINIT  + ST*PLA(I,6))) * (ONE - (ST*PLA(I,6) /(CINIT  + ST*PLA(I,6))))
          ENDIF
          !     iii) Assembling derivatives of PHI with respect to hardening parameter
          HARDP(I)  = SQRT(ALPHA1(I)*DR1_DP(I)**2  + ALPHA2(I)*DR2_DP(I)**2 + ALPHA3(I)*DR1C_DP(I)**2 
     .                   + ALPHA4(I)*DR2C_DP(I)**2 + TWO*ALPHA5(I)*DRT_DP(I)**2)
          DPHI_DPLA = DPHI_DR1*DR1_DP(I)*ALPHA1(I)   + DPHI_DR2*DR2_DP(I)*ALPHA2(I)   + 
     .                DPHI_DR1C*DR1C_DP(I)*ALPHA3(I) + DPHI_DR2C*DR2C_DP(I)*ALPHA4(I) +
     .                DPHI_DRT*DRT_DP(I)*ALPHA5(I)                           
c        
          !     iv) Derivative of PHI with respect to DLAM ( = -DENOM)
          DPHI_DLAM = -DFDSIG2 + DPHI_DPLA
          DPHI_DLAM = SIGN(MAX(ABS(DPHI_DLAM),EM20) ,DPHI_DLAM)   
c        
          ! 4 - Computation of plastic multiplier and variables update
          !----------------------------------------------------------
c                              
          !   a) Computation of the plastic multiplier
          DLAM = -PHI(I) / DPHI_DLAM
c        
	  !   b) Plastic strains tensor update
          DPXX = DLAM * NORMPXX
          DPYY = DLAM * NORMPYY
          DPXY = TWO * DLAM * NORMPXY 
c        
          !   c) Elasto-plastic stresses update   
          SIGNXX(I) = SIGNXX(I) - (A11*DPXX + A12*DPYY)
          SIGNYY(I) = SIGNYY(I) - (A21*DPXX + A22*DPYY)      
          SIGNXY(I) = SIGNXY(I) - G12*DPXY
c  
          !   d) Cumulated plastic strain and hardening parameter update
          DDEP     = DLAM
          DPLA(I)  = MAX(ZERO, DPLA(I) + DDEP)
          DPLA2(I) = MAX(ZERO, DPLA2(I) + ALPHA1(I)*DDEP)
          DPLA3(I) = MAX(ZERO, DPLA3(I) + ALPHA2(I)*DDEP)
          DPLA4(I) = MAX(ZERO, DPLA4(I) + ALPHA3(I)*DDEP)
          DPLA5(I) = MAX(ZERO, DPLA5(I) + ALPHA4(I)*DDEP)
          DPLA6(I) = MAX(ZERO, DPLA6(I) + ALPHA5(I)*DDEP)
          PLA(I,1) = PLA(I,1) + DDEP
          PLA(I,2) = PLA(I,2) + ALPHA1(I)*DDEP
          PLA(I,3) = PLA(I,3) + ALPHA2(I)*DDEP
          PLA(I,4) = PLA(I,4) + ALPHA3(I)*DDEP
          PLA(I,5) = PLA(I,5) + ALPHA4(I)*DDEP
          PLA(I,6) = PLA(I,6) + ALPHA5(I)*DDEP
c        
          !  e) Update of Alpha coefficient
          ALPHA1(I) = ZERO
          ALPHA2(I) = ZERO
          ALPHA3(I) = ZERO
          ALPHA4(I) = ZERO
          ALPHA5(I) = ZERO
          IF ((SIGNXX(I) - XI1*SIGNYY(I)) > ZERO)   ALPHA1(I) = ONE
          IF ((SIGNYY(I) - XI2*SIGNXX(I)) > ZERO)   ALPHA2(I) = ONE
          IF (-SIGNXX(I) > ZERO)                    ALPHA3(I) = ONE        
          IF (-SIGNYY(I) > ZERO)                    ALPHA4(I) = ONE
          IF (ABS(SIGNXY(I)) > ZERO)                ALPHA5(I) = ONE   
c
          !  f) Yield stresses update
          IF (ITAB == 0) THEN
            !      i)  Tensile yield stresses update
            !      - in direction 1 (MD)
            R1(I)  = SIGY1  + (ONE/(CINI1  + S1*PLA(I,2)))*PLA(I,2)
            !      - in direction 2 (CD)
            R2(I)  = SIGY2  + (ONE/(CINI2  + S2*PLA(I,3)))*PLA(I,3)
            !      ii) Compressive yield stresses update
            !      - in direction 1 (MD)
            R1C(I) = SIGY1C + (ONE/(CINI1C + S1C*PLA(I,4)))*PLA(I,4)
            R1C(I) = R1C(I)/SQRT(ONE - G1C)
            !      - in direction 2 (CD)
            R2C(I) = SIGY2C + (ONE/(CINI2C + S2C*PLA(I,5)))*PLA(I,5)
            !      iii) Shear yield stress update
            ETA1(I) = ONE + ALPHA3(I)*ALPHA4(I)*D1
            ETA2(I) = ONE + ALPHA3(I)*ALPHA4(I)*D2
            RT(I)   = ETA1(I)*SIGYT + ETA2(I)*(ONE/(CINIT + ST*PLA(I,6)))*PLA(I,6)
c        
            !  i) Yield function value update
            PHI(I) = ALPHA1(I)*((SIGNXX(I) - XI1*SIGNYY(I))/R1(I))**2 + 
     .               ALPHA2(I)*((SIGNYY(I) - XI2*SIGNXX(I))/R2(I))**2 +  
     .               ALPHA3(I)*((- SIGNXX(I))/R1C(I))**2 + 
     .               ALPHA4(I)*((- SIGNYY(I))/R2C(I))**2 + 
     .               ALPHA5(I)*(ABS(SIGNXY(I))/RT(I))**2 - ONE
          ENDIF
c             
        ENDDO
        ! End of the loop over yielding elements 
c
        ! If tabulated yield function, update of the yield stress for all element
        IF (ITAB > 0) THEN
          IF (NINDX > 0) THEN
            !   -> Tensile yield stress in direction 1 (MD)
            XVEC(1:NEL,1) = PLA(1:NEL,2)
            XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE1
            IPOS(1:NEL,1) = INT(VARTMP(1:NEL,1))
            IPOS(1:NEL,2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(1)),ISMOOTH,NEL,IPOS,XVEC,R1,DR1_DP,HARDR)
            R1(1:NEL)     = R1(1:NEL) * YSCALE1
            DR1_DP(1:NEL) = DR1_DP(1:NEL) * YSCALE1
            VARTMP(1:NEL,1) = IPOS(1:NEL,1)
            !   -> Compressive yield stress in direction 1 (MD)
            XVEC(1:NEL,1) = PLA(1:NEL,3)
            XVEC(1:NEL,2) = EPSD(1:NEL,3) * XSCALE2
            IPOS(1:NEL,1) = INT(VARTMP(1:NEL,2))
            IPOS(1:NEL,2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(2)),ISMOOTH,NEL,IPOS,XVEC,R2,DR2_DP,HARDR)
            R2(1:NEL)     = R2(1:NEL) * YSCALE2
            DR2_DP(1:NEL) = DR2_DP(1:NEL) * YSCALE2
            VARTMP(1:NEL,2) = IPOS(1:NEL,1)  
            !   -> Positive shear yield stress
            XVEC(1:NEL,1) = PLA(1:NEL,4)
            XVEC(1:NEL,2) = EPSD(1:NEL,4) * XSCALE1C
            IPOS(1:NEL,1) = INT(VARTMP(1:NEL,3))
            IPOS(1:NEL,2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(3)),ISMOOTH,NEL,IPOS,XVEC,R1C,DR1C_DP,HARDR)
            R1C(1:NEL)    = R1C(1:NEL) * YSCALE1C
            DR1C_DP(1:NEL)= DR1C_DP(1:NEL) * YSCALE1C
            VARTMP(1:NEL,3) = IPOS(1:NEL,1)
            !   -> Tensile yield stress in direction 2 (CD)
            XVEC(1:NEL,1) = PLA(1:NEL,5)
            XVEC(1:NEL,2) = EPSD(1:NEL,5) * XSCALE2C
            IPOS(1:NEL,1) = INT(VARTMP(1:NEL,4))
            IPOS(1:NEL,2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(4)),ISMOOTH,NEL,IPOS,XVEC,R2C,DR2C_DP,HARDR)
            R2C(1:NEL)    = R2C(1:NEL) * YSCALE2C
            DR2C_DP(1:NEL)= DR2C_DP(1:NEL) * YSCALE2C
            VARTMP(1:NEL,4) = IPOS(1:NEL,1) 
            ! Compressive yield stress in direction 2 (CD)
            XVEC(1:NEL,1) = PLA(1:NEL,6)
            XVEC(1:NEL,2) = EPSD(1:NEL,6) * XSCALET
            IPOS(1:NEL,1) = INT(VARTMP(1:NEL,5))
            IPOS(1:NEL,2) = 1
            CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(5)),ISMOOTH,NEL,IPOS,XVEC,RT,DRT_DP,HARDR)
            RT(1:NEL)     = RT(1:NEL) * YSCALET
            DRT_DP(1:NEL) = DRT_DP(1:NEL) * YSCALET
            VARTMP(1:NEL,5) = IPOS(1:NEL,1)   
            !  Yield function value update
            DO I = 1, NEL
              PHI(I) = ALPHA1(I)*((SIGNXX(I) - XI1*SIGNYY(I))/R1(I))**2 + 
     .                 ALPHA2(I)*((SIGNYY(I) - XI2*SIGNXX(I))/R2(I))**2 +  
     .                 ALPHA3(I)*((- SIGNXX(I))/R1C(I))**2 + 
     .                 ALPHA4(I)*((- SIGNYY(I))/R2C(I))**2 + 
     .                 ALPHA5(I)*(ABS(SIGNXY(I))/RT(I))**2 - ONE
            ENDDO
          ENDIF
        ENDIF
      ENDDO 
      ! End of the loop over the iterations 
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH NEWTON IMPLICIT ITERATIVE METHOD
      !===================================================================
c
      ! Storing new values
      DO I=1,NEL
        ! Plastic strain-rate
        IF (ITAB == 0) THEN 
          EPSD(I,1) = DPLA(I)  / MAX(EM20,TIMESTEP)
          EPSD(I,2) = DPLA2(I) / MAX(EM20,TIMESTEP)
          EPSD(I,3) = DPLA3(I) / MAX(EM20,TIMESTEP)
          EPSD(I,4) = DPLA4(I) / MAX(EM20,TIMESTEP)
          EPSD(I,5) = DPLA5(I) / MAX(EM20,TIMESTEP)
          EPSD(I,6) = DPLA6(I) / MAX(EM20,TIMESTEP)
        ELSE 
          DPDT      = DPLA(I)/MAX(EM20,TIMESTEP)
          EPSD(I,1) = ASRATE * DPDT + (ONE - ASRATE) * EPSD(I,1)
          DPDT      = DPLA2(I)/MAX(EM20,TIMESTEP)
          EPSD(I,2) = ASRATE * DPDT + (ONE - ASRATE) * EPSD(I,2)          
          DPDT      = DPLA3(I)/MAX(EM20,TIMESTEP)
          EPSD(I,3) = ASRATE * DPDT + (ONE - ASRATE) * EPSD(I,3)
          DPDT      = DPLA4(I)/MAX(EM20,TIMESTEP)
          EPSD(I,4) = ASRATE * DPDT + (ONE - ASRATE) * EPSD(I,4)
          DPDT      = DPLA5(I)/MAX(EM20,TIMESTEP)
          EPSD(I,5) = ASRATE * DPDT + (ONE - ASRATE) * EPSD(I,5)
          DPDT      = DPLA6(I)/MAX(EM20,TIMESTEP)
          EPSD(I,6) = ASRATE * DPDT + (ONE - ASRATE) * EPSD(I,6)
        ENDIF        
        ! Coefficient for hourglass
        ET(I)      = HARDP(I) / (HARDP(I) + MAX(YOUNG1,YOUNG2))  
        ! Computation of the sound speed   
        SOUNDSP(I) = SQRT(MAX(A11,A12,A21,A22,G12,G23,G31)/ RHO(I))
        ! Storing the yield stress
        SIGY(I)    = SQRT(R1(I)**2 + R2(I)**2 + R1C(I)**2 + 
     .                    R2C(I)**2 + TWO*RT(I)**2)
      ENDDO
c
      END