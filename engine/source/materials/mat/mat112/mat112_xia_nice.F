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
Chd|  MAT112_XIA_NICE               source/materials/mat/mat112/mat112_xia_nice.F
Chd|-- called by -----------
Chd|        SIGEPS112                     source/materials/mat/mat112/sigeps112.F
Chd|-- calls ---------------
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MAT112_XIA_NICE(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIME    ,TIMESTEP,
     2     UPARAM  ,UVAR    ,JTHE    ,OFF     ,RHO0    ,RHO     ,
     3     PLA     ,DPLA    ,EPSD    ,SOUNDSP ,EPSZZ   ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,
     8     NVARTMP ,NUMTABL ,VARTMP  ,ITABLE  ,TABLE   )
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
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real 
     .   TIME,TIMESTEP,VARTMP(NEL,NVARTMP)
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO,RHO0,EPSZZ,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   DPLA,OFF
      my_real ,DIMENSION(NEL,4),INTENT(INOUT)      :: 
     .   PLA,EPSD
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
c
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE  
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,K,II,NINDX,INDEX(NEL),NINDX2,INDEX2(NEL),NINDX3,INDEX3(NEL),
     .        ITAB,ISMOOTH,IPOS(NEL,2)
c
      my_real 
     .   YOUNG1,YOUNG2,YOUNG3,NU12,NU21,
     .   A11,A12,A21,A22,
     .   G12,G23,G31,DEUXK,E3C,CC,C1,
     .   NU1P,NU2P,NU4P,NU5P,S01,A01,B01,C01,
     .   S02,A02,B02,C02,S03,A03,B03,C03,
     .   S04,A04,B04,C04,S05,A05,B05,C05,
     .   ASIG,BSIG,CSIG,TAU0,ATAU,BTAU,N3,N6,
     .   BULK,NU13,NU31,NU23,NU32
      my_real 
     .   NORMSIG,H,DPDT,DLAM,DDEP,DEPXX,DEPYY
      my_real 
     .   NORMXX,NORMYY,NORMXY,NORMPXX,NORMPYY,NORMPXY,
     .   DPHI_DLAM,DPHI,DFDSIG2,DPHI_DPLA,DPXX,DPYY,DPXY,
     .   DPHI_DSIGY1,DPHI_DSIGY2,DPHI_DSIGY3,DPHI_DSIGY4,
     .   DPHI_DSIGY5
      my_real
     .   NORMZZ,DTHETA_DLAM,DTHETA,DTHETA_DPLA,
     .   DTHETA_DSIGYO
      my_real
     .   NORMYZ,NORMZX,DPSI_DLAM,DPSI,DPSI_DPLA,
     .   DPYZ,DPZX,DPSI_DSIGYS
      my_real
     .   XSCALE1,YSCALE1,XSCALE2,YSCALE2,XSCALE3,YSCALE3,XSCALE4,YSCALE4,
     .   XSCALE5,YSCALE5,XSCALEC,YSCALEC,XSCALES,YSCALES,XVEC(NEL,2),ASRATE
c
      my_real, DIMENSION(2) ::
     .   N1,N2,N4,N5
c
      my_real, DIMENSION(NEL) ::
     .   KHI1,KHI2,KHI3,KHI4,KHI5,KHI6,SIGY1,SIGY2,SIGY3,SIGY4,SIGY5,DPLA2,DPLA3,
     .   DPLA4,SIGYS,SIGYO,DSIGXX,DSIGYY,DSIGXY,DSIGYZ,DSIGZX,HARDP,PHI,PHI0,PSI,PSI0,
     .   THETA,THETA0,DSIGZZ,EEZZ,GAM1,GAM2,GAM3,GAM4,GAM5,GAM6,DSIGY1_DP,DSIGY2_DP,
     .   DSIGY3_DP,DSIGY4_DP,DSIGY5_DP,DSIGYO_DP,DSIGYS_DP,HARDR
c     
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! Elastic parameters      
      YOUNG1 = UPARAM(1)   ! Young modulus in direction 1 (MD)
      YOUNG2 = UPARAM(2)   ! Young modulus in direction 2 (CD)
      YOUNG3 = UPARAM(3)   ! Young modulus in direction 3 (Out of plane) 
      NU12   = UPARAM(4)   ! Poisson's ratio in 12 
      NU21   = UPARAM(5)   ! Poisson's ratio in 21
      A11    = UPARAM(6)   ! Component 11 of orthotropic shell elasticity matrix 
      A12    = UPARAM(7)   ! Component 12 of orthotropic shell elasticity matrix 
      A21    = UPARAM(8)   ! Component 21 of orthotropic shell elasticity matrix 
      A22    = UPARAM(9)   ! Component 22 of orthotropic shell elasticity matrix 
      G12    = UPARAM(10)  ! Shear modulus in 12
      G23    = UPARAM(11)  ! Shear modulus in 23
      G31    = UPARAM(12)  ! Shear modulus in 31
      ITAB   = INT(UPARAM(14))  ! Tabulated yield stress flag
      DEUXK  = UPARAM(15)  ! Yield criterion exponent
      E3C    = UPARAM(16)  ! Compressive elasticity parameter
      CC     = UPARAM(17)  ! Exponent compressive elasticity parameter
      NU1P   = UPARAM(18)  ! Tensile plastic Poisson ratio in direction 1 (MD)
      NU2P   = UPARAM(19)  ! Tensile plastic Poisson ratio in direction 2 (CD)
      NU4P   = UPARAM(20)  ! Compressive plastic Poisson ratio in direction 1 (MD)
      NU5P   = UPARAM(21)  ! Compressive plastic Poisson ratio in direction 2 (CD)
      IF (ITAB == 0) THEN
        S01     = UPARAM(22)  ! 1st Tensile plasticity parameter direction 1 (MD)
        A01     = UPARAM(23)  ! 2nd Tensile plasticity parameter direction 1 (MD)
        B01     = UPARAM(24)  ! 3rd Tensile plasticity parameter direction 1 (MD)
        C01     = UPARAM(25)  ! 4th Tensile plasticity parameter direction 1 (MD)
        S02     = UPARAM(26)  ! 1st Tensile plasticity parameter direction 2 (CD)
        A02     = UPARAM(27)  ! 2nd Tensile plasticity parameter direction 2 (CD)
        B02     = UPARAM(28)  ! 3rd Tensile plasticity parameter direction 2 (CD)
        C02     = UPARAM(29)  ! 4th Tensile plasticity parameter direction 2 (CD)
        S03     = UPARAM(30)  ! 1st Positive shear plasticity parameter
        A03     = UPARAM(31)  ! 2nd Positive shear plasticity parameter
        B03     = UPARAM(32)  ! 3rd Positive shear plasticity parameter
        C03     = UPARAM(33)  ! 4th Positive shear plasticity parameter
        S04     = UPARAM(34)  ! 1st Compressive plasticity parameter direction 1 (MD)
        A04     = UPARAM(35)  ! 2nd Compressive plasticity parameter direction 1 (MD)
        B04     = UPARAM(36)  ! 3rd Compressive plasticity parameter direction 1 (MD)
        C04     = UPARAM(37)  ! 4th Compressive plasticity parameter direction 1 (MD)
        S05     = UPARAM(38)  ! 1st Compressive plasticity parameter direction 2 (CD)
        A05     = UPARAM(39)  ! 2nd Compressive plasticity parameter direction 2 (CD)
        B05     = UPARAM(40)  ! 3rd Compressive plasticity parameter direction 2 (CD)
        C05     = UPARAM(41)  ! 4th Compressive plasticity parameter direction 2 (CD)
        ASIG    = UPARAM(42)  ! 1st Out-of-plane plasticity parameter
        BSIG    = UPARAM(43)  ! 2nd Out-of-plane plasticity parameter
        CSIG    = UPARAM(44)  ! 3rd Out-of-plane plasticity parameter
        TAU0    = UPARAM(45)  ! 1st Transverse shear plasticity parameter
        ATAU    = UPARAM(46)  ! 2nd Transverse shear plasticity parameter
        BTAU    = UPARAM(47)  ! 3rd Transverse shear plasticity parameter
      ELSE
        XSCALE1 = UPARAM(22)
        YSCALE1 = UPARAM(23)
        XSCALE2 = UPARAM(24)
        YSCALE2 = UPARAM(25)
        XSCALE3 = UPARAM(26) 
        YSCALE3 = UPARAM(27)
        XSCALE4 = UPARAM(28)
        YSCALE4 = UPARAM(29)
        XSCALE5 = UPARAM(30) 
        YSCALE5 = UPARAM(31)
        XSCALEC = UPARAM(32)
        YSCALEC = UPARAM(33)
        XSCALES = UPARAM(34)
        YSCALES = UPARAM(35)
        ASRATE  = UPARAM(36)
        ASRATE  = (DEUX*PI*ASRATE*TIMESTEP)/(DEUX*PI*ASRATE*TIMESTEP + UN)
        ISMOOTH = INT(UPARAM(37))
      ENDIF
c
      ! Yield planes normal
      N1(1)   = UN/(SQRT(UN+NU1P**2))
      N1(2)   = -NU1P/(SQRT(UN+NU1P**2))
      N2(1)   = -NU2P/(SQRT(UN+NU2P**2))
      N2(2)   = UN/(SQRT(UN+NU2P**2))
      N3      = UN
      N4(1)   = -UN/(SQRT(UN+NU4P**2))
      N4(2)   = NU4P/(SQRT(UN+NU4P**2))
      N5(1)   = NU5P/(SQRT(UN+NU5P**2))
      N5(2)   = -UN/(SQRT(UN+NU5P**2))
      N6      = -UN
c
      ! Recovering internal variables
      DO I=1,NEL
        ! OFF parameter for element deletion
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < UN)   OFF(I) = OFF(I)*QCINQ
        ! User inputs
        PHI0(I)   = UVAR(I,1)            ! Previous in-plane yield function value
        THETA0(I) = UVAR(I,2)            ! Previous out-of-plane yield function value
        PSI0(I)   = UVAR(I,3)            ! Previous transverse shear yield function value
        EEZZ(I)   = EPSZZ(I) + PLA(I,3)  ! Out-of-plane elastic strain
        ! Standard inputs
        DPLA(I)   = ZERO      ! Initialization of the "global" plastic strain increment
        DPLA2(I)  = ZERO      ! Initialization of the in-plane plastic strain increment
        DPLA3(I)  = ZERO      ! Initialization of the out-of-plane plastic strain increment
        DPLA4(I)  = ZERO      ! Initialization of the transverse shear plastic strain increment
        ET(I)     = UN        ! Initialization of hourglass coefficient
        HARDP(I)  = ZERO      ! Initialization of hourglass coefficient
      ENDDO
c      
      ! Computation of the initial yield stress
      IF (ITAB > 0) THEN
        ! In-plane tabulation with strain-rate
        XVEC(1:NEL,1) = PLA(1:NEL,2)
        XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE1
        !   -> Tensile yield stress in direction 1 (MD)
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,1))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(1)),ISMOOTH,NEL,IPOS,XVEC,SIGY1,DSIGY1_DP,HARDR)
        SIGY1(1:NEL)    = SIGY1(1:NEL) * YSCALE1
        DSIGY1_DP(1:NEL)= DSIGY1_DP(1:NEL) * YSCALE1
        VARTMP(1:NEL,1) = IPOS(1:NEL,1)
        !   -> Tensile yield stress in direction 2 (CD)
        XVEC(1:NEL,2)   = EPSD(1:NEL,2) * XSCALE2
        IPOS(1:NEL,1)   = INT(VARTMP(1:NEL,2))
        IPOS(1:NEL,2)   = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(2)),ISMOOTH,NEL,IPOS,XVEC,SIGY2,DSIGY2_DP,HARDR)
        SIGY2(1:NEL)    = SIGY2(1:NEL) * YSCALE2
        DSIGY2_DP(1:NEL)= DSIGY2_DP(1:NEL) * YSCALE2
        VARTMP(1:NEL,2) = IPOS(1:NEL,1)  
        !   -> Positive shear yield stress
        XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE3
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,3))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(3)),ISMOOTH,NEL,IPOS,XVEC,SIGY3,DSIGY3_DP,HARDR)
        SIGY3(1:NEL)    = SIGY3(1:NEL) * YSCALE3
        DSIGY3_DP(1:NEL)= DSIGY3_DP(1:NEL) * YSCALE3
        VARTMP(1:NEL,3) = IPOS(1:NEL,1)
        !   -> Compressive yield stress in direction 1 (MD)
        XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE4
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,4))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(4)),ISMOOTH,NEL,IPOS,XVEC,SIGY4,DSIGY4_DP,HARDR)
        SIGY4(1:NEL)    = SIGY4(1:NEL) * YSCALE4
        DSIGY4_DP(1:NEL)= DSIGY4_DP(1:NEL) * YSCALE4
        VARTMP(1:NEL,4) = IPOS(1:NEL,1) 
        !   -> Compressive yield stress in direction 2 (CD)
        XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE5
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,5))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(5)),ISMOOTH,NEL,IPOS,XVEC,SIGY5,DSIGY5_DP,HARDR)
        SIGY5(1:NEL)    = SIGY5(1:NEL) * YSCALE5
        DSIGY5_DP(1:NEL)= DSIGY5_DP(1:NEL) * YSCALE5
        VARTMP(1:NEL,5) = IPOS(1:NEL,1)
        !   -> Out-of-plane tabulation with strain-rate
        XVEC(1:NEL,1) = PLA(1:NEL,3)
        XVEC(1:NEL,2) = EPSD(1:NEL,3) * XSCALEC
        !   -> Transverse shear yield stress
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,6))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(6)),ISMOOTH,NEL,IPOS,XVEC,SIGYO,DSIGYO_DP,HARDR)
        SIGYO(1:NEL)    = SIGYO(1:NEL) * YSCALEC
        DSIGYO_DP(1:NEL)= DSIGYO_DP(1:NEL) * YSCALEC
        VARTMP(1:NEL,6) = IPOS(1:NEL,1)
        !   -> Transverse shear tabulation with strain-rate
        XVEC(1:NEL,1) = PLA(1:NEL,4)
        XVEC(1:NEL,2) = EPSD(1:NEL,4) * XSCALES
        !   -> Transverse shear yield stress
        IPOS(1:NEL,1) = INT(VARTMP(1:NEL,7))
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(7)),ISMOOTH,NEL,IPOS,XVEC,SIGYS,DSIGYS_DP,HARDR)
        SIGYS(1:NEL)    = SIGYS(1:NEL) * YSCALES
        DSIGYS_DP(1:NEL)= DSIGYS_DP(1:NEL) * YSCALES
        VARTMP(1:NEL,7) = IPOS(1:NEL,1)
      ELSE
        DO I = 1,NEL
          ! Tensile yield stress in direction 1 (MD)
          SIGY1(I) = S01  + A01*TANH(B01*PLA(I,2)) + C01*PLA(I,2)
          ! Tensile yield stress in direction 2 (CD)
          SIGY2(I) = S02  + A02*TANH(B02*PLA(I,2)) + C02*PLA(I,2)
          ! Positive shear yield stress
          SIGY3(I) = S03  + A03*TANH(B03*PLA(I,2)) + C03*PLA(I,2)
          ! Compressive yield stress in direction 1 (MD)
          SIGY4(I) = S04  + A04*TANH(B04*PLA(I,2)) + C04*PLA(I,2)
          ! Compressive yield stress in direction 2 (CD)
          SIGY5(I) = S05  + A05*TANH(B05*PLA(I,2)) + C05*PLA(I,2)
          ! Out-of-plane yield stress
          SIGYO(I) = ASIG + BSIG*EXP(CSIG*PLA(I,3))
          ! Transverse shear yield stress
          SIGYS(I) = TAU0 + (ATAU - MIN(ZERO,SIGOZZ(I))*BTAU)*PLA(I,4)
        ENDDO
      ENDIF
c
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
c
        ! Computation of the trial stress tensor
        !  - in-plane stresses
        SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A21*DEPSXX(I) + A22*DEPSYY(I)
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G12
        !  - out-of-plane stress
        IF (EEZZ(I) >= ZERO) THEN 
          SIGNZZ(I) = SIGOZZ(I) + YOUNG3*DEPSZZ(I)
        ELSE
          SIGNZZ(I) = SIGOZZ(I) + E3C*CC*EXP(-CC*EEZZ(I))*DEPSZZ(I)
        ENDIF
        !  - transverse shear stresses
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*G23
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*G31  
c        
        ! Computation of trial khi coefficients
        KHI1(I) = ZERO
        KHI2(I) = ZERO
        KHI3(I) = ZERO
        KHI4(I) = ZERO
        KHI5(I) = ZERO
        KHI6(I) = ZERO
        IF ((N1(1)*SIGNXX(I)+N1(2)*SIGNYY(I)) > ZERO)  KHI1(I) = UN
        IF ((N2(1)*SIGNXX(I)+N2(2)*SIGNYY(I)) > ZERO)  KHI2(I) = UN       
        IF (N3*SIGNXY(I) > ZERO)                       KHI3(I) = UN         
        IF ((N4(1)*SIGNXX(I)+N4(2)*SIGNYY(I)) > ZERO)  KHI4(I) = UN
        IF ((N5(1)*SIGNXX(I)+N5(2)*SIGNYY(I)) > ZERO)  KHI5(I) = UN
        IF (N6*SIGNXY(I) > ZERO)                       KHI6(I) = UN
c
        ! Computation of the in-plane yield function
        GAM1(I) = (N1(1)*SIGNXX(I)+N1(2)*SIGNYY(I))/SIGY1(I)
        GAM2(I) = (N2(1)*SIGNXX(I)+N2(2)*SIGNYY(I))/SIGY2(I)
        GAM3(I) = N3*SIGNXY(I)/SIGY3(I)
        GAM4(I) = (N4(1)*SIGNXX(I)+N4(2)*SIGNYY(I))/SIGY4(I)
        GAM5(I) = (N5(1)*SIGNXX(I)+N5(2)*SIGNYY(I))/SIGY5(I)
        GAM6(I) = N6*SIGNXY(I)/SIGY3(I)
        PHI(I)  = KHI1(I)*GAM1(I)**DEUXK + KHI2(I)*GAM2(I)**DEUXK +  
     .            KHI3(I)*GAM3(I)**DEUXK + KHI4(I)*GAM4(I)**DEUXK + 
     .            KHI5(I)*GAM5(I)**DEUXK + KHI6(I)*GAM6(I)**DEUXK - UN
c
        ! Computation of the out-of-plane shear yield function
        THETA(I) = - SIGNZZ(I) - SIGYO(I)
c
        ! Computation of the transverse shear yield function
        PSI(I)   = (SQRT(SIGNYZ(I)**2 + SIGNZX(I)**2)/(SIGYS(I))) - UN
c
      ENDDO
c
      !========================================================================
      ! - CHECKING THE PLASTIC BEHAVIOR
      !========================================================================
c      
      ! Checking plastic behavior
      NINDX  = 0
      NINDX2 = 0
      NINDX3 = 0
      DO I=1,NEL   
        ! In plane
        IF (PHI(I) > ZERO .AND. OFF(I) == UN) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
        ! Out-of-plane
        IF (THETA(I) > ZERO .AND. OFF(I) == UN) THEN
          NINDX2=NINDX2+1
          INDEX2(NINDX2)=I
        ENDIF
        ! Transverse shear
        IF (PSI(I) > ZERO .AND. OFF(I) == UN) THEN
          NINDX3=NINDX3+1
          INDEX3(NINDX3)=I
        ENDIF
      ENDDO
c      
      !====================================================================
      ! - I IN-PLANE PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !====================================================================
#include "vectorize.inc" 
      DO II=1,NINDX   
c      
        ! Number of the element with plastic behaviour  	                                           
        I = INDEX(II)
c        
        ! Note     : in this part, the purpose is to compute for each iteration
        ! a plastic multiplier allowing to update internal variables to satisfy
        ! the consistency condition. 
        ! Its expression is : DLAMBDA = - (PHI + DPHI) / DPHI_DLAMBDA
	! -> PHI_OLD : old value of yield function (known)
        ! -> DPHI    : yield function prediction (to compute)
        ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
        !                   into account of internal variables kinetic : 
        !                   plasticity, temperature and damage (to compute) 
c        
	! 1 - Computation of DPHI_DSIG the normal to the yield criterion
        !-------------------------------------------------------------          
        ! Computation of derivatives to the yield criterion
        ! - in-plane normal
        GAM1(I) = (N1(1)*SIGOXX(I)+N1(2)*SIGOYY(I))/SIGY1(I)
        GAM2(I) = (N2(1)*SIGOXX(I)+N2(2)*SIGOYY(I))/SIGY2(I)
        GAM3(I) = N3*SIGOXY(I)/SIGY3(I)
        GAM4(I) = (N4(1)*SIGOXX(I)+N4(2)*SIGOYY(I))/SIGY4(I)
        GAM5(I) = (N5(1)*SIGOXX(I)+N5(2)*SIGOYY(I))/SIGY5(I)
        GAM6(I) = N6*SIGOXY(I)/SIGY3(I)
        NORMXX  = DEUXK*KHI1(I)*(GAM1(I)**(DEUXK-1))*(N1(1)/SIGY1(I)) +
     .            DEUXK*KHI2(I)*(GAM2(I)**(DEUXK-1))*(N2(1)/SIGY2(I)) +     
     .            DEUXK*KHI4(I)*(GAM4(I)**(DEUXK-1))*(N4(1)/SIGY4(I)) + 
     .            DEUXK*KHI5(I)*(GAM5(I)**(DEUXK-1))*(N5(1)/SIGY5(I))
        NORMYY  = DEUXK*KHI1(I)*(GAM1(I)**(DEUXK-1))*(N1(2)/SIGY1(I)) +
     .            DEUXK*KHI2(I)*(GAM2(I)**(DEUXK-1))*(N2(2)/SIGY2(I)) +     
     .            DEUXK*KHI4(I)*(GAM4(I)**(DEUXK-1))*(N4(2)/SIGY4(I)) + 
     .            DEUXK*KHI5(I)*(GAM5(I)**(DEUXK-1))*(N5(2)/SIGY5(I))
        NORMXY  = DEUXK/DEUX*KHI3(I)*(GAM3(I)**(DEUXK-1))*(N3/SIGY3(I)) +
     .            DEUXK/DEUX*KHI6(I)*(GAM6(I)**(DEUXK-1))*(N6/SIGY3(I))
c
        ! Normalization of the derivatives
        !  - in-plane normal
        NORMSIG = SQRT(NORMXX**2 + NORMYY**2 + DEUX*NORMXY**2)
        NORMSIG = MAX(NORMSIG,EM20)
        NORMPXX = NORMXX/NORMSIG
        NORMPYY = NORMYY/NORMSIG
        NORMPXY = NORMXY/NORMSIG
c             
        ! 3 - Computation of DPHI_DLAMBDA
        !---------------------------------------------------------
c        
        !   a) Derivative with respect stress increments tensor DSIG
        !   --------------------------------------------------------
        DFDSIG2 = NORMXX * (A11*NORMPXX + A12*NORMPYY)
     .          + NORMYY * (A21*NORMPXX + A22*NORMPYY)
     .          + DEUX*NORMXY * DEUX*NORMPXY * G12
c     
        !   b) Derivatives with respect to hardening parameters 
        !   ---------------------------------------------------
c        
        !      i) Derivatives of PHI with respect to the yield stresses
        DPHI_DSIGY1 = DEUXK*KHI1(I)*(GAM1(I)**(DEUXK-1))*(-(N1(1)*SIGOXX(I)+N1(2)*SIGOYY(I))/(SIGY1(I)**2))
        DPHI_DSIGY2 = DEUXK*KHI2(I)*(GAM2(I)**(DEUXK-1))*(-(N2(1)*SIGOXX(I)+N2(2)*SIGOYY(I))/(SIGY2(I)**2))
        DPHI_DSIGY3 = DEUXK*KHI3(I)*(GAM3(I)**(DEUXK-1))*(-N3*SIGOXY(I)/(SIGY3(I)**2)) + 
     .                DEUXK*KHI6(I)*(GAM6(I)**(DEUXK-1))*(-N6*SIGOXY(I)/(SIGY3(I)**2))
        DPHI_DSIGY4 = DEUXK*KHI4(I)*(GAM4(I)**(DEUXK-1))*(-(N4(1)*SIGOXX(I)+N4(2)*SIGOYY(I))/(SIGY4(I)**2))
        DPHI_DSIGY5 = DEUXK*KHI5(I)*(GAM5(I)**(DEUXK-1))*(-(N5(1)*SIGOXX(I)+N5(2)*SIGOYY(I))/(SIGY5(I)**2))
        !     ii) Derivatives of yield stresses with respect to hardening parameters
        IF (ITAB == 0) THEN 
          DSIGY1_DP(I) = A01*B01*(UN-(TANH(B01*PLA(I,2)))**2) + C01
          DSIGY2_DP(I) = A02*B02*(UN-(TANH(B02*PLA(I,2)))**2) + C02
          DSIGY3_DP(I) = A03*B03*(UN-(TANH(B03*PLA(I,2)))**2) + C03
          DSIGY4_DP(I) = A04*B04*(UN-(TANH(B04*PLA(I,2)))**2) + C04
          DSIGY5_DP(I) = A05*B05*(UN-(TANH(B05*PLA(I,2)))**2) + C05
        ENDIF
        !     iii) Assembling derivatives of PHI with respect to hardening parameter
        HARDP(I)    = SQRT(KHI1(I)*DSIGY1_DP(I)**2      + KHI2(I)*DSIGY2_DP(I)**2 + 
     .                     KHI3(I)*DEUX*DSIGY3_DP(I)**2 + KHI4(I)*DSIGY4_DP(I)**2 + 
     .                     KHI5(I)*DSIGY5_DP(I)**2)
        DPHI_DPLA   = DPHI_DSIGY1*DSIGY1_DP(I) + DPHI_DSIGY2*DSIGY2_DP(I) + 
     .                DPHI_DSIGY3*DSIGY3_DP(I) + DPHI_DSIGY4*DSIGY4_DP(I) +
     .                DPHI_DSIGY5*DSIGY5_DP(I)
c        
        !     iv) Derivative of PHI with respect to DLAM ( = -DENOM)
        DPHI_DLAM = -DFDSIG2 + DPHI_DPLA
        DPHI_DLAM = SIGN(MAX(ABS(DPHI_DLAM),EM20) ,DPHI_DLAM)     
c        
        ! 4 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------     
c     
        !  a) Restoring previous value of the yield function
        PHI(I) = PHI0(I)
c
        !  b) Computation of the trial stress increment
        DSIGXX(I) = A11*DEPSXX(I) + A12*DEPSYY(I)
        DSIGYY(I) = A21*DEPSXX(I) + A22*DEPSYY(I)
        DSIGXY(I) = DEPSXY(I)*G12
c
        !  c) Computation of yield surface trial increment DPHI       
        DPHI = NORMXX * DSIGXX(I)  
     .       + NORMYY * DSIGYY(I)
     .       + DEUX * NORMXY * DSIGXY(I) 
c
        !  d) Computation of the plastic multiplier
        DLAM = -(PHI(I) + DPHI) / DPHI_DLAM     
        DLAM = MAX(DLAM, ZERO)
c        
        !  e) Plastic strains tensor update
        DPXX = DLAM * NORMPXX
        DPYY = DLAM * NORMPYY
        DPXY = DEUX * DLAM * NORMPXY 
c        
        !  f) Elasto-plastic stresses update   
        SIGNXX(I) = SIGOXX(I) + DSIGXX(I) - (A11*DPXX + A12*DPYY)
        SIGNYY(I) = SIGOYY(I) + DSIGYY(I) - (A21*DPXX + A22*DPYY)      
        SIGNXY(I) = SIGOXY(I) + DSIGXY(I) - G12*DPXY
c  
        !  g) Cumulated plastic strain and hardening parameter update
        DDEP     = DLAM
        DPLA2(I) = MAX(ZERO, DPLA2(I) + DDEP)
        PLA(I,2) = PLA(I,2) + DDEP       
c        
        !  h) Yield stresses update
        IF (ITAB == 0) THEN
          ! Tensile yield stress in direction 1 (MD)
          SIGY1(I) = S01 + A01*TANH(B01*PLA(I,2)) + C01*PLA(I,2)
          ! Tensile yield stress in direction 2 (CD)
          SIGY2(I) = S02 + A02*TANH(B02*PLA(I,2)) + C02*PLA(I,2)
          ! Positive shear yield stress
          SIGY3(I) = S03 + A03*TANH(B03*PLA(I,2)) + C03*PLA(I,2)
          ! Compressive yield stress in direction 1 (MD)
          SIGY4(I) = S04 + A04*TANH(B04*PLA(I,2)) + C04*PLA(I,2)
          ! Compressive yield stress in direction 2 (CD)
          SIGY5(I) = S05 + A05*TANH(B05*PLA(I,2)) + C05*PLA(I,2)
        ENDIF
c
        ! i) Update of trial alpha coefficients
        KHI1(I) = ZERO
        KHI2(I) = ZERO
        KHI3(I) = ZERO
        KHI4(I) = ZERO
        KHI5(I) = ZERO
        KHI6(I) = ZERO
        IF ((N1(1)*SIGNXX(I)+N1(2)*SIGNYY(I)) > ZERO)  KHI1(I) = UN
        IF ((N2(1)*SIGNXX(I)+N2(2)*SIGNYY(I)) > ZERO)  KHI2(I) = UN       
        IF (N3*SIGNXY(I) > ZERO)                       KHI3(I) = UN         
        IF ((N4(1)*SIGNXX(I)+N4(2)*SIGNYY(I)) > ZERO)  KHI4(I) = UN
        IF ((N5(1)*SIGNXX(I)+N5(2)*SIGNYY(I)) > ZERO)  KHI5(I) = UN
        IF (N6*SIGNXY(I) > ZERO)                       KHI6(I) = UN
c        
        !  j) Yield function value update
        IF (ITAB == 0) THEN 
          GAM1(I) = (N1(1)*SIGNXX(I)+N1(2)*SIGNYY(I))/SIGY1(I)
          GAM2(I) = (N2(1)*SIGNXX(I)+N2(2)*SIGNYY(I))/SIGY2(I)
          GAM3(I) = N3*SIGNXY(I)/SIGY3(I)
          GAM4(I) = (N4(1)*SIGNXX(I)+N4(2)*SIGNYY(I))/SIGY4(I)
          GAM5(I) = (N5(1)*SIGNXX(I)+N5(2)*SIGNYY(I))/SIGY5(I)
          GAM6(I) = N6*SIGNXY(I)/SIGY3(I)
          PHI(I)  = KHI1(I)*GAM1(I)**DEUXK + KHI2(I)*GAM2(I)**DEUXK +  
     .              KHI3(I)*GAM3(I)**DEUXK + KHI4(I)*GAM4(I)**DEUXK + 
     .              KHI5(I)*GAM5(I)**DEUXK + KHI6(I)*GAM6(I)**DEUXK - UN
        ENDIF
c                                      
      ENDDO 
      !===================================================================
      ! - END OF IN-PLANE PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !===================================================================
c
      !====================================================================
      ! - II OUT-OF-PLANE PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !====================================================================
#include "vectorize.inc" 
      DO II=1,NINDX2   
c      
        ! Number of the element with plastic behaviour  	                                           
        I = INDEX2(II)
c        
        ! Note     : in this part, the purpose is to compute for each iteration
        ! a plastic multiplier allowing to update internal variables to satisfy
        ! the consistency condition. 
        ! Its expression is : DLAMBDA = - (THETA + DTHETA) / DPHI_DLAMBDA
	! -> THETA  : current value of yield function (known)
        ! -> DTHETA : yield function prediction (to compute)
        ! -> DTHETA_DLAMBDA : derivative of THETA with respect to DLAMBDA by taking
        !                   into account of internal variables kinetic : 
        !                   plasticity, temperature and damage (to compute) 
c        
	! 1 - Computation of DTHETA_DSIG the normal to the yield criterion
        !-------------------------------------------------------------          
        ! Computation of derivatives to the yield criterion
        ! - in-plane normal
        NORMZZ = -UN
c             
        ! 3 - Computation of DTHETA_DLAMBDA
        !---------------------------------------------------------
c        
        !   a) Derivative with respect stress increments tensor DSIG
        !   --------------------------------------------------------
        IF (EEZZ(I) >= ZERO) THEN 
          DFDSIG2 = NORMZZ * YOUNG3 * NORMZZ
        ELSE
          DFDSIG2 = NORMZZ * E3C*CC*EXP(-CC*EEZZ(I)) * NORMZZ
        ENDIF
c     
        !   b) Derivatives with respect to hardening parameters 
        !   ---------------------------------------------------
c        
        !      i) Derivatives of THETA with respect to the yield stresses
        DTHETA_DSIGYO = -UN
        !     ii) Derivatives of yield stresses with respect to hardening parameters
        IF (ITAB == 0) DSIGYO_DP(I) = CSIG*BSIG*EXP(CSIG*PLA(I,3))
        !     iii) Assembling derivatives of THETA with respect to hardening parameter
        DTHETA_DPLA   = DTHETA_DSIGYO*DSIGYO_DP(I)
c        
        !     iv) Derivative of THETA with respect to DLAM ( = -DENOM)
        DTHETA_DLAM = -DFDSIG2 + DTHETA_DPLA
        DTHETA_DLAM = SIGN(MAX(ABS(DTHETA_DLAM),EM20) ,DTHETA_DLAM)     
c        
        ! 4 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------     
c     
        !  a) Restoring previous value of the yield function
        THETA(I) = THETA0(I)
c
        !  b) Computation of the trial stress increment
        IF (EEZZ(I) >= ZERO) THEN
          DSIGZZ(I) = YOUNG3*DEPSZZ(I)
        ELSE
          DSIGZZ(I) = E3C*CC*EXP(-CC*EEZZ(I))*DEPSZZ(I)
        ENDIF
c
        !  c) Computation of yield surface trial increment DTHETA       
        DTHETA = NORMZZ * DSIGZZ(I)   
c
        !  d) Computation of the plastic multiplier
        DLAM = -(THETA(I) + DTHETA) / DTHETA_DLAM
        DLAM = MAX(DLAM, ZERO)
c        
        !  e) Elasto-plastic stresses update   
        IF (EEZZ(I)>=ZERO) THEN
          SIGNZZ(I) = SIGOZZ(I) + DSIGZZ(I) - YOUNG3*DLAM*NORMZZ
        ELSE
          SIGNZZ(I) = SIGOZZ(I) + DSIGZZ(I) - E3C*CC*EXP(-CC*EEZZ(I))*DLAM*NORMZZ        
        ENDIF
c  
        !  f) Cumulated plastic strain and hardening parameter update
        DDEP     = DLAM
        DPLA3(I) = MAX(ZERO,DPLA3(I) + DDEP)
        PLA(I,3) = PLA(I,3) + DDEP
        EEZZ(I)  = EPSZZ(I) + PLA(I,3)       
c        
        !  g) Yield stresses update
        IF (ITAB == 0) THEN
          ! Out-of-plane yield stress
          SIGYO(I) = ASIG + BSIG*EXP(CSIG*PLA(I,3))
          ! Yield function value update
          THETA(I) = - SIGNZZ(I) - SIGYO(I)
        ENDIF
c                                      
      ENDDO 
      !=======================================================================
      ! - END OF OUT-OF-PLANE PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !=======================================================================    
c
      !========================================================================
      ! - III TRANSVERSE SHEAR PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !========================================================================
#include "vectorize.inc" 
      DO II=1,NINDX3   
c      
        ! Number of the element with plastic behaviour  	                                           
        I = INDEX3(II)
c        
        ! Note     : in this part, the purpose is to compute for each iteration
        ! a plastic multiplier allowing to update internal variables to satisfy
        ! the consistency condition. 
        ! Its expression is : DLAMBDA = - (PSI + DPSI) / DPSI_DLAMBDA
	! -> PSI  : current value of yield function (known)
        ! -> DPSI : yield function prediction (to compute)
        ! -> DPSI_DLAMBDA : derivative of PSI with respect to DLAMBDA by taking
        !                   into account of internal variables kinetic : 
        !                   plasticity, temperature and damage (to compute) 
c        
	! 1 - Computation of DPSI_DSIG the normal to the yield criterion
        !-------------------------------------------------------------          
c    
        ! Computation of derivatives to the yield criterion
        ! - transverse shear normal
        NORMYZ  = SIGOYZ(I)/MAX((SQRT(SIGOYZ(I)**2 + SIGOZX(I)**2)*SIGYS(I)),EM20)
        NORMZX  = SIGOZX(I)/MAX((SQRT(SIGOYZ(I)**2 + SIGOZX(I)**2)*SIGYS(I)),EM20)
c             
        ! 2 - Computation of DPSI_DLAMBDA
        !---------------------------------------------------------
c        
        !   a) Derivative with respect stress increments tensor DSIG
        !   --------------------------------------------------------
        DFDSIG2 = DEUX*NORMYZ * G23 * DEUX*NORMYZ +
     .            DEUX*NORMZX * G31 * DEUX*NORMZX
c     
        !   b) Derivatives with respect to hardening parameters 
        !   ---------------------------------------------------
c        
        !      i) Derivatives of PSI with respect to the yield stresses
        DPSI_DSIGYS = -SQRT(SIGOYZ(I)**2 + SIGOZX(I)**2)/(SIGYS(I)**2)
        !     ii) Derivatives of yield stresses with respect to hardening parameters
        IF (ITAB == 0) DSIGYS_DP(I) = ATAU - MIN(ZERO,SIGOZZ(I))*BTAU
        !     iii) Assembling derivatives of PSI with respect to hardening parameter
        DPSI_DPLA   = DPSI_DSIGYS*DSIGYS_DP(I)
c        
        !     iv) Derivative of PSI with respect to DLAM ( = -DENOM)
        DPSI_DLAM = -DFDSIG2 + DPSI_DPLA
        DPSI_DLAM = SIGN(MAX(ABS(DPSI_DLAM),EM20),DPSI_DLAM)      
c        
        ! 4 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------     
c     
        !  a) Restoring previous value of the yield function
        PSI(I) = PSI0(I)
c
        !  b) Computation of the trial stress increment
        DSIGYZ(I) = DEPSYZ(I)*G23
        DSIGZX(I) = DEPSZX(I)*G31
c
        !  c) Computation of yield surface trial increment DPSI       
        DPSI = DEUX*NORMYZ * DSIGYZ(I)
     .       + DEUX*NORMZX * DSIGZX(I)
c
        !  d) Computation of the plastic multiplier
        DLAM = -(PSI(I) + DPSI) / DPSI_DLAM     
        DLAM = MAX(DLAM, ZERO)
c        
        !  e) Plastic strains tensor update
        DPYZ = DEUX*DLAM * NORMYZ
        DPZX = DEUX*DLAM * NORMZX
c        
        !  f) Elasto-plastic stresses update     
        SIGNYZ(I) = SIGOYZ(I) + DSIGYZ(I) - G23*DPYZ
        SIGNZX(I) = SIGOZX(I) + DSIGZX(I) - G31*DPZX
c  
        !  g) Cumulated plastic strain and hardening parameter update
        DDEP     = DLAM
        DPLA4(I) = MAX(ZERO, DPLA4(I) + DDEP)
        PLA(I,4) = PLA(I,4) + DDEP     
c        
        !  h) Yield stresses update
        IF (ITAB == 0) THEN
          ! Transverse shear yield stress
          SIGYS(I) = TAU0 + (ATAU - MIN(ZERO,SIGOZZ(I))*BTAU)*PLA(I,4)  
          !  i) Yield function value update
          PSI(I) = (SQRT(SIGNYZ(I)**2 + SIGNZX(I)**2)/SIGYS(I)) - UN
        ENDIF
c                                      
      ENDDO
      !===========================================================================
      ! - END OF TRANSVERSE SHEAR PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
      !===========================================================================    
c
      ! If tabulated yield function, update of the yield stress for all element
      IF (ITAB > 0) THEN
        IF (NINDX > 0) THEN
          ! In-plane tabulation with strain-rate
          XVEC(1:NEL,1)   = PLA(1:NEL,2)
          XVEC(1:NEL,2)   = EPSD(1:NEL,2) * XSCALE1
          !   -> Tensile yield stress in direction 1 (MD)
          IPOS(1:NEL,1)   = INT(VARTMP(1:NEL,1))
          IPOS(1:NEL,2)   = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(1)),ISMOOTH,NEL,IPOS,XVEC,SIGY1,DSIGY1_DP,HARDR)
          SIGY1(1:NEL)    = SIGY1(1:NEL) * YSCALE1
          DSIGY1_DP(1:NEL)= DSIGY1_DP(1:NEL) * YSCALE1
          VARTMP(1:NEL,1) = IPOS(1:NEL,1)
          !   -> Tensile yield stress in direction 2 (CD)
          XVEC(1:NEL,2)   = EPSD(1:NEL,2) * XSCALE2
          IPOS(1:NEL,1)   = INT(VARTMP(1:NEL,2))
          IPOS(1:NEL,2)   = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(2)),ISMOOTH,NEL,IPOS,XVEC,SIGY2,DSIGY2_DP,HARDR)
          SIGY2(1:NEL)    = SIGY2(1:NEL) * YSCALE2
          DSIGY2_DP(1:NEL)= DSIGY2_DP(1:NEL) * YSCALE2
          VARTMP(1:NEL,2) = IPOS(1:NEL,1)  
          !   -> Positive shear yield stress
          XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE3
          IPOS(1:NEL,1) = INT(VARTMP(1:NEL,3))
          IPOS(1:NEL,2) = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(3)),ISMOOTH,NEL,IPOS,XVEC,SIGY3,DSIGY3_DP,HARDR)
          SIGY3(1:NEL)    = SIGY3(1:NEL) * YSCALE3
          DSIGY3_DP(1:NEL)= DSIGY3_DP(1:NEL) * YSCALE3
          VARTMP(1:NEL,3) = IPOS(1:NEL,1)
          !   -> Compressive yield stress in direction 1 (MD)
          XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE4
          IPOS(1:NEL,1) = INT(VARTMP(1:NEL,4))
          IPOS(1:NEL,2) = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(4)),ISMOOTH,NEL,IPOS,XVEC,SIGY4,DSIGY4_DP,HARDR)
          SIGY4(1:NEL)    = SIGY4(1:NEL) * YSCALE4
          DSIGY4_DP(1:NEL)= DSIGY4_DP(1:NEL) * YSCALE4
          VARTMP(1:NEL,4) = IPOS(1:NEL,1) 
          !   -> Compressive yield stress in direction 2 (CD)
          XVEC(1:NEL,2) = EPSD(1:NEL,2) * XSCALE5
          IPOS(1:NEL,1) = INT(VARTMP(1:NEL,5))
          IPOS(1:NEL,2) = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(5)),ISMOOTH,NEL,IPOS,XVEC,SIGY5,DSIGY5_DP,HARDR)
          SIGY5(1:NEL)    = SIGY5(1:NEL) * YSCALE5
          DSIGY5_DP(1:NEL)= DSIGY5_DP(1:NEL) * YSCALE5
          VARTMP(1:NEL,5) = IPOS(1:NEL,1)          
          ! Yield function value update
          DO I = 1,NEL
            GAM1(I) = (N1(1)*SIGNXX(I)+N1(2)*SIGNYY(I))/SIGY1(I)
            GAM2(I) = (N2(1)*SIGNXX(I)+N2(2)*SIGNYY(I))/SIGY2(I)
            GAM3(I) = N3*SIGNXY(I)/SIGY3(I)
            GAM4(I) = (N4(1)*SIGNXX(I)+N4(2)*SIGNYY(I))/SIGY4(I)
            GAM5(I) = (N5(1)*SIGNXX(I)+N5(2)*SIGNYY(I))/SIGY5(I)
            GAM6(I) = N6*SIGNXY(I)/SIGY3(I)
            PHI(I)  = KHI1(I)*GAM1(I)**DEUXK + KHI2(I)*GAM2(I)**DEUXK +  
     .                KHI3(I)*GAM3(I)**DEUXK + KHI4(I)*GAM4(I)**DEUXK + 
     .                KHI5(I)*GAM5(I)**DEUXK + KHI6(I)*GAM6(I)**DEUXK - UN 
          ENDDO
        ENDIF
        IF (NINDX2 > 0) THEN
          !   -> Transverse shear tabulation with strain-rate
          XVEC(1:NEL,1) = PLA(1:NEL,3)
          XVEC(1:NEL,2) = EPSD(1:NEL,3) * XSCALEC
          !   -> Transverse shear yield stress
          IPOS(1:NEL,1) = INT(VARTMP(1:NEL,6))
          IPOS(1:NEL,2) = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(6)),ISMOOTH,NEL,IPOS,XVEC,SIGYO,DSIGYO_DP,HARDR)
          SIGYO(1:NEL)    = SIGYO(1:NEL) * YSCALEC
          DSIGYO_DP(1:NEL)= DSIGYO_DP(1:NEL) * YSCALEC
          VARTMP(1:NEL,6) = IPOS(1:NEL,1)
          ! Yield function value update
          DO I = 1,NEL
            THETA(I) = - SIGNZZ(I) - SIGYO(I)
          ENDDO        
        ENDIF
        IF (NINDX3 > 0) THEN
          ! Transverse shear tabulation with strain-rate
          XVEC(1:NEL,1) = PLA(1:NEL,4)
          XVEC(1:NEL,2) = EPSD(1:NEL,4) * XSCALES
          ! Transverse shear yield stress
          IPOS(1:NEL,1) = INT(VARTMP(1:NEL,7))
          IPOS(1:NEL,2) = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(7)),ISMOOTH,NEL,IPOS,XVEC,SIGYS,DSIGYS_DP,HARDR)
          SIGYS(1:NEL)    = SIGYS(1:NEL) * YSCALES
          DSIGYS_DP(1:NEL)= DSIGYS_DP(1:NEL) * YSCALES
          VARTMP(1:NEL,7) = IPOS(1:NEL,1)
          ! Yield function value update
          DO I = 1,NEL
            PSI(I) = (SQRT(SIGNYZ(I)**2 + SIGNZX(I)**2)/SIGYS(I)) - UN
          ENDDO
        ENDIF
      ENDIF
c
      ! Storing new values
      DO I=1,NEL
        ! USR Outputs
        UVAR(I,1)  = PHI(I)              ! In-plane yield function
        UVAR(I,2)  = THETA(I)            ! Out-of-plane yield function
        UVAR(I,3)  = PSI(I)              ! Transverse shear yield function
        ! Plastic strain
        PLA(I,1)   = SQRT(PLA(I,2)**2 + PLA(I,3)**2 + PLA(I,4)**2)        
        DPLA(I)    = (PLA(I,2)/(MAX(SQRT(PLA(I,2)**2 + PLA(I,3)**2 + PLA(I,4)**2),EM20)))*DPLA2(I) +
     .               (PLA(I,3)/(MAX(SQRT(PLA(I,2)**2 + PLA(I,3)**2 + PLA(I,4)**2),EM20)))*DPLA3(I) +
     .               (PLA(I,4)/(MAX(SQRT(PLA(I,2)**2 + PLA(I,3)**2 + PLA(I,4)**2),EM20)))*DPLA4(I)
        ! Plastic strain-rate
        IF (ITAB == 0) THEN 
          EPSD(I,1) = DPLA(I)  / MAX(EM20,TIMESTEP)
          EPSD(I,2) = DPLA2(I) / MAX(EM20,TIMESTEP)
          EPSD(I,3) = DPLA3(I) / MAX(EM20,TIMESTEP)
          EPSD(I,4) = DPLA4(I) / MAX(EM20,TIMESTEP)
        ELSE 
          DPDT      = DPLA(I)/MAX(EM20,TIMESTEP)
          EPSD(I,1) = ASRATE * DPDT + (UN - ASRATE) * EPSD(I,1)
          DPDT      = DPLA2(I)/MAX(EM20,TIMESTEP)
          EPSD(I,2) = ASRATE * DPDT   + (UN - ASRATE) * EPSD(I,2)
          DPDT      = DPLA3(I)/MAX(EM20,TIMESTEP)
          EPSD(I,3) = ASRATE * DPDT   + (UN - ASRATE) * EPSD(I,3)
          DPDT      = DPLA4(I)/MAX(EM20,TIMESTEP)
          EPSD(I,4) = ASRATE * DPDT   + (UN - ASRATE) * EPSD(I,4)
        ENDIF
        ! Coefficient for hourglass and soundspeed
        IF (EEZZ(I) >= ZERO) THEN 
          ET(I)      = HARDP(I) / (HARDP(I) + MAX(YOUNG1,YOUNG2,YOUNG3))
          SOUNDSP(I) = SQRT(MAX(A11,A12,A21,A22,YOUNG3,G12,G23,G31)/ RHO(I))
        ELSE
          ET(I)      = HARDP(I) / (HARDP(I) + MAX(YOUNG1,YOUNG2,E3C*CC*EXP(-CC*EEZZ(I))))
          SOUNDSP(I) = SQRT(MAX(A11,A12,A21,A22,E3C*CC*EXP(-CC*EEZZ(I)),G12,G23,G31)/ RHO(I))        
        ENDIF
        ! Storing the yield stress
        SIGY(I)    = SQRT(KHI1(I)*SIGY1(I)**2 + KHI2(I)*SIGY2(I)**2 + KHI4(I)*SIGY4(I)**2 + 
     .                    KHI5(I)*SIGY5(I)**2 + DEUX*KHI3(I)*SIGY3(I)**2 + SIGYO(I)**2
     .                    + DEUX*SIGYS(I)**2)
      ENDDO
c
      END