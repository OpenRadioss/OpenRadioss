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
Chd|  SIGEPS110C_LITE_NICE          source/materials/mat/mat110/sigeps110c_lite_nice.F
Chd|-- called by -----------
Chd|        SIGEPS110C                    source/materials/mat/mat110/sigeps110c.F
Chd|-- calls ---------------
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS110C_LITE_NICE(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NPF     ,
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,JTHE    ,OFF     ,
     3     GS      ,RHO     ,PLA     ,DPLA    ,EPSP    ,SOUNDSP ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,ASRATE  ,
     5     EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     6     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,THKLY   ,
     8     THK     ,SIGY    ,ET      ,TEMPEL  ,TEMP    ,SEQ     ,
     9     TF      ,NUMTABL ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP  ,
     A     SIGA    ,INLOC   ,DPLANL  )
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
      !=======================================================================
      !      Dummy arguments
      !=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,JTHE,NUMTABL,ITABLE(NUMTABL),NVARTMP,NPF(*),INLOC
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real
     .   TIME,TIMESTEP,ASRATE,TF(*)
      INTEGER :: VARTMP(NEL,NVARTMP)
      my_real,DIMENSION(NUPARAM), INTENT(IN) ::
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     ::
     .   RHO,TEMPEL,DPLANL,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSPXX,EPSPYY,EPSPXY,EPSPYZ,EPSPZX ,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX,
     .   GS , THKLY
c
      my_real ,DIMENSION(NEL), INTENT(OUT)   ::
     .   SOUNDSP,SIGY,ET,EPSP,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
c
      my_real ,DIMENSION(NEL), INTENT(INOUT)       ::
     .   PLA,DPLA,OFF,THK,TEMP,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) ::
     .   UVAR
      my_real ,DIMENSION(NEL,3)     ,INTENT(INOUT) ::
     .   SIGA
c
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,J,II,K,ITER,NITER,NINDX,INDEX(NEL),VFLAG,IPOS(NEL,2),NANGLE,
     .        IPOS0(NEL,2),ISMOOTH
c
      my_real
     .   YOUNG,G,G2,NU,NNU,A11,A12,YLD0,DSIGM,BETA,OMEGA,NEXP,EPS0,SIGST,
     .   DG0,DEPS0,MEXP,fBI(2),KBOLTZ,FISOKIN,TINI,XSCALE,YSCALE
      my_real
     .   MOHR_RADIUS,MOHR_CENTER,NORMSIG,TMP,SIG_RATIO,VAR_A,VAR_B,VAR_C,
     .   A(2),B(2),C(2),H,DPDT,DLAM,DDEP,DAV,DEVE1,DEVE2,DEVE3,DEVE4,
     .   XVEC(NEL,4)
      my_real
     .   fUN_THETA(NEL,2),HIPS_THETA(NEL,2),HISH_THETA(NEL,2)
      my_real
     .   F1,F2,DF1_DMU,DF2_DMU,NORMXX,NORMYY,NORMXY,
     .   DENOM,SIG_DFDSIG,DFDSIG2,DPHI_DSIG1,DPHI_DSIG2,
     .   DSXX,DSYY,DSXY,DEXX,DEYY,DEXY,ALPHA,DA_DCOS2(2),
     .   DB_DCOS2(2),DC_DCOS2(2),DF1_DCOS2,DF2_DCOS2,DPHI_DCOS2,
     .   DWEIGHT_DCOS2,U,UPRIM,V,VPRIM,COS2(10,10),DPHI
c
      my_real, DIMENSION(NEL) ::
     .   DSIGXX,DSIGYY,DSIGXY,DSIGYZ,DSIGZX,SIGVG,YLD,HARDP,SIGHARD,SIGRATE,
     .   PHI,DPLA_DLAM,DEZZ,DPHI_DLAM,DPXX,DPYY,DPXY,DPYZ,DPZX,DPZZ,
     .   SIG1,SIG2,COS2THETA,SIN2THETA,DEELZZ,MU,DYLD_DPLA,YL0,SIGEXX,SIGEYY,
     .   SIGEXY,WEIGHT,HARDR,YLD_TREF,DYDX,YLD_TEMP,TFAC,PHI0
c
      my_real, DIMENSION(:,:), ALLOCATABLE ::
     .   HIPS,HISH,Q_HIPS,Q_HISH
c
      my_real, DIMENSION(:), ALLOCATABLE ::
     .   Q_WPS,Q_WSH,Q_fUN
c
      my_real, PARAMETER :: TOL = 1.0D-6
c
      LOGICAL :: SIGN_CHANGED(NEL)
c
      DATA  COS2/
     1 1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,
     2 0.   ,1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,
     3 -1.  ,0.   ,2.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,
     4 0.   ,-3.  ,0.   ,4.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,
     5 1.   ,0.   ,-8.  ,0.   ,8.   ,0.   ,0.   ,0.   ,0.   ,0.   ,
     6 0.   ,5.   ,0.   ,-20. ,0.   ,16.  ,0.   ,0.   ,0.   ,0.   ,
     7 -1.  ,0.   ,18.  ,0.   ,-48. ,0.   ,32.  ,0.   ,0.   ,0.   ,
     8 0.   ,-7.  ,0.   ,56.  ,0.   ,-112.,0.   ,64.  ,0.   ,0.   ,
     9 1.   ,0.   ,-32. ,0.   ,160. ,0.   ,-256.,0.   ,128. ,0.   ,
     A 0.   ,9.   ,0.   ,-120.,0.   ,432. ,0.   ,-576 ,0.   ,256. /
      !=======================================================================
      !       DRUCKER MATERIAL LAW WITH NO DAMAGE
      !=======================================================================
      !UVAR(1)
      !UVAR(2)   YLD    YIELD STRESS
      !UVAR(3)   H      HARDENING RATE
      !UVAR(4)   PHI    YIELD FUNCTION VALUE
      !-
      !DEPIJ     PLASTIC STRAIN TENSOR COMPONENT
      !DEPSIJ    TOTAL   STRAIN TENSOR COMPONENT (EL+PL)
      !=======================================================================
c
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      YOUNG   = UPARAM(1)
      NU      = UPARAM(2)
      G       = UPARAM(3)
      G2      = UPARAM(4)
      NNU     = UPARAM(5)
      A11     = UPARAM(6)
      A12     = UPARAM(7)
      YLD0    = UPARAM(8)
      DSIGM   = UPARAM(9)
      BETA    = UPARAM(10)
      OMEGA   = UPARAM(11)
      NEXP    = UPARAM(12)
      EPS0    = UPARAM(13)
      SIGST   = UPARAM(14)
      DG0     = UPARAM(15)
      DEPS0   = UPARAM(16)
      MEXP    = UPARAM(17)
      KBOLTZ  = UPARAM(18)
      XSCALE  = UPARAM(19)
      YSCALE  = UPARAM(20)
      FISOKIN = UPARAM(21)
      VFLAG   = NINT(UPARAM(23))
      TINI    = UPARAM(24)
      fBI(1)  = UPARAM(25)
      fBI(2)  = UPARAM(25)
      NANGLE  = NINT(UPARAM(26))
      ISMOOTH = NINT(UPARAM(28))
c
      ALLOCATE(HIPS(NANGLE,2),HISH(NANGLE,2),
     .         Q_fUN(NANGLE),Q_HISH(NANGLE,2),Q_HIPS(NANGLE,2),
     .         Q_WPS(NANGLE),Q_WSH(NANGLE))
c
      DO I = 1, NANGLE
        ! Hinge points
        HIPS(I,1)   = UPARAM(30+11*(I-1))
        HIPS(I,2)   = UPARAM(31+11*(I-1))
        HISH(I,1)   = UPARAM(32+11*(I-1))
        HISH(I,2)   = UPARAM(33+11*(I-1))
        ! Interpolation factors
        Q_fUN(I)    = UPARAM(34+11*(I-1))
        Q_HISH(I,1) = UPARAM(35+11*(I-1))
        Q_HISH(I,2) = UPARAM(36+11*(I-1))
        Q_HIPS(I,1) = UPARAM(37+11*(I-1))
        Q_HIPS(I,2) = UPARAM(38+11*(I-1))
        Q_WSH(I)    = UPARAM(39+11*(I-1))
        Q_WPS(I)    = UPARAM(40+11*(I-1))
      ENDDO
c
      ! Initial variable
      IF (TIME == ZERO) THEN
        IF (JTHE == 0) THEN
          TEMP(1:NEL) = TINI
        ENDIF
        IF (EPS0 > ZERO) THEN
          PLA(1:NEL) = EPS0
        ENDIF
      ENDIF
c
      ! Recovering internal variables
      DO I=1,NEL
        IF (OFF(I) < 0.1) OFF(I) = ZERO
        IF (OFF(I) < ONE)  OFF(I) = OFF(I)*FOUR_OVER_5
        ! User inputs
        PHI0(I)  = UVAR(I,2) ! Previous yield function value
        ! Standard inputs
        DPLA(I)  = ZERO      ! Initialization of the plastic strain increment
        ET(I)    = ONE        ! Initialization of hourglass coefficient
        HARDP(I) = ZERO      ! Initialization of hourglass coefficient
        SIGN_CHANGED(I) = .FALSE.
        DEZZ(I)  = ZERO
      ENDDO
c
      ! /HEAT/MAT temperature
      IF (JTHE > 0) THEN
        TEMP(1:NEL) = TEMPEL(1:NEL)
      ENDIF
c
      ! Computation of the strain-rate depending on the flags
      IF ((VFLAG == 1) .OR. (VFLAG == 3)) THEN
        DO I = 1, NEL
          IF (VFLAG == 1) THEN
            EPSP(I)   = UVAR(I,1)
          ELSEIF (VFLAG == 3) THEN
            DAV       = (EPSPXX(I)+EPSPYY(I))*THIRD
            DEVE1     = EPSPXX(I) - DAV
            DEVE2     = EPSPYY(I) - DAV
            DEVE3     = - DAV
            DEVE4     = HALF*EPSPXY(I)
            EPSP(I)   = HALF*(DEVE1**2 + DEVE2**2 + DEVE3**2) + DEVE4**2
            EPSP(I)   = SQRT(THREE*EPSP(I))/THREE_HALF
            EPSP(I)   = ASRATE*EPSP(I) + (ONE - ASRATE)*UVAR(I,1)
            UVAR(I,1) = EPSP(I)
          ENDIF
        ENDDO
      ENDIF
c
      ! Initial yield stress for kinematic hardening
      IF (FISOKIN > ZERO) THEN
        IF (NUMTABL > 0) THEN
          XVEC(1:NEL,1)  = ZERO
          XVEC(1:NEL,2)  = EPSP(1:NEL) * XSCALE
          IPOS0(1:NEL,1) = 1
          IPOS0(1:NEL,2) = 1
          CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(1)),ISMOOTH,NEL,IPOS0,XVEC  ,YL0  ,HARDP,HARDR)
          YL0(1:NEL) = YL0(1:NEL)   * YSCALE
          ! Tabulation with Temperature
          IF (NUMTABL == 2) THEN
            ! Reference temperature factor
            XVEC(1:NEL,2)  = TINI
            IPOS0(1:NEL,1) = 1
            IPOS0(1:NEL,2) = 1
            CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS0,XVEC,YLD_TREF,DYDX)
            ! Current temperature factor
            XVEC(1:NEL,2) = TEMP(1:NEL)
            CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS0,XVEC,YLD_TEMP,DYDX)
            TFAC(1:NEL)  = YLD_TEMP(1:NEL) / YLD_TREF(1:NEL)
            YL0(1:NEL)   = YL0(1:NEL) * TFAC(1:NEL)
          ENDIF
        ELSE
          YL0(1:NEL) = YLD0
        ENDIF
      ELSE
        YL0(1:NEL) = ZERO
      ENDIF
c
      ! Computation of the yield stress
      IF (NUMTABL > 0) THEN
        ! Tabulation with strain-rate
        XVEC(1:NEL,1) = PLA(1:NEL)
        XVEC(1:NEL,2) = EPSP(1:NEL) * XSCALE
        IPOS(1:NEL,1) = VARTMP(1:NEL,1)
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(1)),ISMOOTH,NEL,IPOS,XVEC,YLD,HARDP,HARDR)
        YLD(1:NEL)   = YLD(1:NEL)   * YSCALE
        HARDP(1:NEL) = HARDP(1:NEL) * YSCALE
        VARTMP(1:NEL,1) = IPOS(1:NEL,1)
        ! Tabulation with Temperature
        IF (NUMTABL == 2) THEN
          ! Reference temperature factor
          XVEC(1:NEL,2) = TINI
          IPOS(1:NEL,1) = VARTMP(1:NEL,2)
          IPOS(1:NEL,2) = VARTMP(1:NEL,3)
          CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS,XVEC,YLD_TREF,DYDX)
          VARTMP(1:NEL,2) = IPOS(1:NEL,1)
          VARTMP(1:NEL,3) = IPOS(1:NEL,2)
          ! Current temperature factor
          XVEC(1:NEL,2) = TEMP(1:NEL)
          CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS,XVEC,YLD_TEMP,DYDX)
          TFAC(1:NEL)  = YLD_TEMP(1:NEL) / YLD_TREF(1:NEL)
          YLD(1:NEL)   = YLD(1:NEL)   * TFAC(1:NEL)
          HARDP(1:NEL) = HARDP(1:NEL) * TFAC(1:NEL)
        ELSE
          TFAC(1:NEL) = ONE
        ENDIF
        ! Including kinematic hardening effect
        YLD(1:NEL)   = (ONE-FISOKIN)*YLD(1:NEL)+FISOKIN*YL0(1:NEL)
      ELSE
        DO I = 1,NEL
          ! a) - Hardening law
          !   Continuous law
          SIGHARD(I) = YLD0 + DSIGM*(BETA*(PLA(I))+(ONE-EXP(-OMEGA*(PLA(I))))**NEXP)
          ! b) - Strain-rate dependent hardening stress
          SIGRATE(I) = SIGST*(ONE + (KBOLTZ*TEMP(I)/DG0)*LOG(ONE + (EPSP(I)/DEPS0)))**MEXP
          ! c) - Computation of the yield function and value check
          YLD(I) = SIGHARD(I) + SIGRATE(I)
          ! d) - Including kinematic hardening
          IF (FISOKIN > ZERO) THEN
            YL0(I) = YL0(I) + SIGRATE(I)
          ENDIF
          YLD(I) = (ONE-FISOKIN)*YLD(I)+FISOKIN*YL0(I)
          ! d) - Checking values
          YLD(I) = MAX(EM10, YLD(I))
        ENDDO
      ENDIF
c
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
c
        ! Computation of the trial stress tensor
        IF (FISOKIN > ZERO) THEN
          SIGNXX(I) = SIGOXX(I) - SIGA(I,1) + A11*DEPSXX(I) + A12*DEPSYY(I)
          SIGNYY(I) = SIGOYY(I) - SIGA(I,2) + A12*DEPSXX(I) + A11*DEPSYY(I)
          SIGNXY(I) = SIGOXY(I) - SIGA(I,3) + G*DEPSXY(I)
          SIGEXX(I) = SIGNXX(I)
          SIGEYY(I) = SIGNYY(I)
          SIGEXY(I) = SIGNXY(I)
        ELSE
          SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
          SIGNYY(I) = SIGOYY(I) + A11*DEPSYY(I) + A12*DEPSXX(I)
          SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G
        ENDIF
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*GS(I)
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*GS(I)
c
        ! Computation of the equivalent stress
        NORMSIG = SQRT(SIGNXX(I)*SIGNXX(I)
     .               + SIGNYY(I)*SIGNYY(I)
     .               + TWO*SIGNXY(I)*SIGNXY(I))
        IF (NORMSIG < EM20) THEN
          SIGVG(I) = ZERO
        ELSE
c
          ! Computation of the principal stresses
          MOHR_RADIUS = SQRT(((SIGNXX(I)-SIGNYY(I))/TWO)**2 + SIGNXY(I)**2)
          MOHR_CENTER = (SIGNXX(I)+SIGNYY(I))/TWO
          SIG1(I)     = MOHR_CENTER + MOHR_RADIUS
          SIG2(I)     = MOHR_CENTER - MOHR_RADIUS
          IF (MOHR_RADIUS>EM20) THEN
            COS2THETA(I) = ((SIGNXX(I)-SIGNYY(I))/TWO)/MOHR_RADIUS
            SIN2THETA(I) = SIGNXY(I)/MOHR_RADIUS
          ELSE
            COS2THETA(I) = ONE
            SIN2THETA(I) = ZERO
          ENDIF
c
          ! Computation of scale factors for shear
          HISH_THETA(I,1:2)  = ZERO
          DO J = 1,NANGLE
            DO K = 1,J
              HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
            ENDDO
          ENDDO
c
          ! Computation of the equivalent stress of Vegter
          IF (SIG1(I)<ZERO .OR. ((SIG2(I)<ZERO) .AND. (SIG2(I)*HISH_THETA(I,1)<SIG1(I)*HISH_THETA(I,2)))) THEN
            COS2THETA(I) = -COS2THETA(I)
            SIN2THETA(I) = -SIN2THETA(I)
            TMP     = SIG1(I)
            SIG1(I) = -SIG2(I)
            SIG2(I) = -TMP
            SIGN_CHANGED(I) = .TRUE.
          ELSE
            SIGN_CHANGED(I) = .FALSE.
          ENDIF
          !   Between tension and compression uniaxial points
          IF (SIG2(I)<ZERO) THEN
            ! Interpolation of factors
            fUN_THETA(I,1:2)  = ZERO
            HISH_THETA(I,1:2) = ZERO
            WEIGHT(I)         = ZERO
            DO J = 1,NANGLE
              DO K = 1,J
                fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
                HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
                WEIGHT(I)         = WEIGHT(I)         + Q_WSH(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              ENDDO
            ENDDO
            ! Filling the table
            A(1)      = fUN_THETA(I,2)
            A(2)      = -fUN_THETA(I,1)
            B(1:2)    = HISH_THETA(I,1:2)
            C(1:2)    = fUN_THETA(I,1:2)
          !   Between uniaxial and equibiaxial point
          ELSE
            ! Interpolation of factors
            fUN_THETA(I,1:2)  = ZERO
            HIPS_THETA(I,1:2) = ZERO
            WEIGHT(I)         = ZERO
            DO J = 1,NANGLE
              DO K = 1,J
                fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
                HIPS_THETA(I,1:2) = HIPS_THETA(I,1:2) + Q_HIPS(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
                WEIGHT(I)         = WEIGHT(I)         + Q_WPS(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              ENDDO
            ENDDO
            ! Filling the table
            A(1:2)    = fUN_THETA(I,1:2)
            B(1:2)    = HIPS_THETA(I,1:2)
            C(1:2)    = fBI(1:2)
          ENDIF
          !   Determine MU and SIGVG
          IF (SIG1(I)<EM20) THEN
            MU(I)    = ZERO
            SIGVG(I) = ZERO
          ELSE
            SIG_RATIO = SIG2(I)/SIG1(I)
            VAR_A = (C(2)+A(2)-TWO*WEIGHT(I)*B(2)) - SIG_RATIO*(C(1)+A(1)-TWO*WEIGHT(I)*B(1))
            VAR_B = TWO*((WEIGHT(I)*B(2)-A(2)) - SIG_RATIO*(WEIGHT(I)*B(1)-A(1)))
            VAR_C = A(2) - SIG_RATIO*A(1)
            IF (ABS(VAR_A)<EM08) THEN
              MU(I) = -VAR_C/VAR_B
            ELSE
              MU(I) = (-VAR_B+SQRT(VAR_B*VAR_B - FOUR*VAR_A*VAR_C))/(TWO*VAR_A)
            ENDIF
            SIGVG(I) = SIG1(I)*(((ONE-MU(I))**2) + TWO*MU(I)*WEIGHT(I)*(ONE-MU(I)) + (MU(I)**2))/
     .            (A(1)*((ONE-MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2))
          END IF
        ENDIF
c
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !========================================================================
      PHI(1:NEL) = SIGVG(1:NEL) - YLD(1:NEL)
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
      !=======================================================
      ! - PLASTIC CORRECTION WITH NICE EXPLICIT RETURN MAPPING
      !=======================================================
c
      ! Loop over yielding elements
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
        DSIGYZ(I) = DEPSYZ(I)*GS(I)
        DSIGZX(I) = DEPSZX(I)*GS(I)
        DLAM      = ZERO
c
        ! Computation of old principal stresses
        MOHR_RADIUS = SQRT(((SIGOXX(I)-SIGOYY(I))/TWO)**2 + SIGOXY(I)**2)
        MOHR_CENTER = (SIGOXX(I)+SIGOYY(I))/TWO
        SIG1(I)     = MOHR_CENTER + MOHR_RADIUS
        SIG2(I)     = MOHR_CENTER - MOHR_RADIUS
        IF (MOHR_RADIUS>EM20) THEN
          COS2THETA(I) = ((SIGOXX(I)-SIGOYY(I))/TWO)/MOHR_RADIUS
          SIN2THETA(I) = SIGOXY(I)/MOHR_RADIUS
        ELSE
          COS2THETA(I) = ONE
          SIN2THETA(I) = ZERO
        ENDIF
c
        ! Computation of old scale factors for shear
        HISH_THETA(I,1:2)  = ZERO
        DO J = 1,NANGLE
          DO K = 1,J
            HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
          ENDDO
        ENDDO
c
        ! Computation of the old equivalent stress of Vegter
        IF (SIG1(I)<ZERO .OR. ((SIG2(I)<ZERO) .AND. (SIG2(I)*HISH_THETA(I,1)<SIG1(I)*HISH_THETA(I,2)))) THEN
          COS2THETA(I) = -COS2THETA(I)
          SIN2THETA(I) = -SIN2THETA(I)
          TMP     = SIG1(I)
          SIG1(I) = -SIG2(I)
          SIG2(I) = -TMP
          SIGN_CHANGED(I) = .TRUE.
        ELSE
          SIGN_CHANGED(I) = .FALSE.
        ENDIF
        !   Between tension and compression uniaxial points
        IF (SIG2(I)<ZERO) THEN
          ! Interpolation of factors
          fUN_THETA(I,1:2)  = ZERO
          HISH_THETA(I,1:2) = ZERO
          WEIGHT(I)         = ZERO
          DO J = 1,NANGLE
            DO K = 1,J
              fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
              WEIGHT(I)         = WEIGHT(I)         + Q_WSH(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
            ENDDO
          ENDDO
          ! Filling the table
          A(1)      = fUN_THETA(I,2)
          A(2)      = -fUN_THETA(I,1)
          B(1:2)    = HISH_THETA(I,1:2)
          C(1:2)    = fUN_THETA(I,1:2)
          ! Derivatives of PHI with respect to THETA
          DA_DCOS2(1:2) = ZERO
          DB_DCOS2(1:2) = ZERO
          DC_DCOS2(1:2) = ZERO
          DWEIGHT_DCOS2 = ZERO
          IF (NANGLE > 1) THEN
            ! Computation of their first derivative with respect to COS2THET
            DO J = 2, NANGLE
              DO K = 2, J
                DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HISH(J,1:2)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                DC_DCOS2(1)   = DC_DCOS2(1)   + Q_fUN(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                DWEIGHT_DCOS2 = DWEIGHT_DCOS2 + Q_WSH(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
              ENDDO
            ENDDO
            DA_DCOS2(2) = -DC_DCOS2(1)
          ENDIF
        !   Between uniaxial and equibiaxial point
        ELSE
          ! Interpolation of factors
          fUN_THETA(I,1:2)  = ZERO
          HIPS_THETA(I,1:2) = ZERO
          WEIGHT(I)         = ZERO
          DO J = 1,NANGLE
            DO K = 1,J
              fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              HIPS_THETA(I,1:2) = HIPS_THETA(I,1:2) + Q_HIPS(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
              WEIGHT(I)         = WEIGHT(I)         + Q_WPS(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
            ENDDO
          ENDDO
          ! Filling the table
          A(1:2)    = fUN_THETA(I,1:2)
          B(1:2)    = HIPS_THETA(I,1:2)
          C(1:2)    = fBI(1:2)
          ! Derivatives of PHI with respect to THETA
          DA_DCOS2(1:2) = ZERO
          DB_DCOS2(1:2) = ZERO
          DC_DCOS2(1:2) = ZERO
          DWEIGHT_DCOS2 = ZERO
          ! If anisotropic, compute derivatives with respect to COS2THET
          IF (NANGLE > 1) THEN
            ! Computation of their first derivative with respect to COS2THET
            DO J = 2, NANGLE
              DO K = 2, J
                DA_DCOS2(1)   = DA_DCOS2(1)   + Q_fUN(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HIPS(J,1:2)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                DWEIGHT_DCOS2 = DWEIGHT_DCOS2 + Q_WPS(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
        !   Determine MU and SIGVG
        IF (SIG1(I)<EM20) THEN
          MU(I)    = ZERO
          SIGVG(I) = ZERO
        ELSE
          SIG_RATIO = SIG2(I)/SIG1(I)
          VAR_A = (C(2)+A(2)-TWO*WEIGHT(I)*B(2)) - SIG_RATIO*(C(1)+A(1)-TWO*WEIGHT(I)*B(1))
          VAR_B = TWO*((WEIGHT(I)*B(2)-A(2)) - SIG_RATIO*(WEIGHT(I)*B(1)-A(1)))
          VAR_C = A(2) - SIG_RATIO*A(1)
          IF (ABS(VAR_A)<EM08) THEN
            MU(I) = -VAR_C/VAR_B
          ELSE
            MU(I) = (-VAR_B+SQRT(VAR_B*VAR_B - FOUR*VAR_A*VAR_C))/(TWO*VAR_A)
          ENDIF
          SIGVG(I) = SIG1(I)*(((ONE-MU(I))**2) + TWO*MU(I)*WEIGHT(I)*(ONE-MU(I)) + (MU(I)**2))/
     .          (A(1)*((ONE-MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2))
        END IF
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
        !   Derivatives of Fi with respect to COS2THET
        U         = A(1)*((ONE - MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2)
        UPRIM     = DA_DCOS2(1)*((ONE - MU(I))**2) + TWO*MU(I)*(ONE-MU(I))*(DWEIGHT_DCOS2*B(1)+
     .                     WEIGHT(I)*DB_DCOS2(1)) + DC_DCOS2(1)*(MU(I)**2)
        V         = (ONE-MU(I))**2 + TWO*WEIGHT(I)*MU(I)*(ONE-MU(I)) + MU(I)**2
        VPRIM     = TWO*MU(I)*(ONE-MU(I))*DWEIGHT_DCOS2
        F1        = U/MAX(V,EM20)
        DF1_DCOS2 = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
        U         = A(2)*((ONE - MU(I))**2) + TWO*B(2)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(2)*(MU(I)**2)
        UPRIM     = DA_DCOS2(2)*((ONE - MU(I))**2) + TWO*MU(I)*(ONE-MU(I))*(DWEIGHT_DCOS2*B(2)+
     .                     WEIGHT(I)*DB_DCOS2(2)) + DC_DCOS2(2)*(MU(I)**2)
        F2        = U/MAX(V,EM20)
        DF2_DCOS2 = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
c
        !   Derivatives of Fi with respect to MU
        U       = A(1)*((ONE - MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2)
        UPRIM   = -TWO*(ONE-MU(I))*A(1)  + TWO*WEIGHT(I)*B(1)*(ONE-TWO*MU(I))  + TWO*MU(I)*C(1)
        V       = (ONE-MU(I))**2 + TWO*WEIGHT(I)*MU(I)*(ONE-MU(I)) + MU(I)**2
        VPRIM   = -TWO*(ONE-MU(I)) + TWO*WEIGHT(I)*(ONE-TWO*MU(I)) + TWO*MU(I)
        DF1_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
        U       = A(2)*((ONE - MU(I))**2) + TWO*B(2)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(2)*(MU(I)**2)
        UPRIM   = -TWO*(ONE-MU(I))*A(2)  + TWO*WEIGHT(I)*B(2)*(ONE-TWO*MU(I))  + TWO*MU(I)*C(2)
        DF2_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
c
        !   Derivatives of PHI with respect to SIG1, SIG2 and MU
        IF ((F1*DF2_DMU - F2*DF1_DMU)/=ZERO) THEN
          DPHI_DSIG1 =  DF2_DMU/(F1*DF2_DMU - F2*DF1_DMU)
          DPHI_DSIG2 = -DF1_DMU/(F1*DF2_DMU - F2*DF1_DMU)
          IF (ABS(SIG1(I)-SIG2(I))>TOL) THEN
            DPHI_DCOS2 = (SIGVG(I)/(SIG1(I) - SIG2(I)))*(DF2_DCOS2*DF1_DMU -
     .                              DF1_DCOS2*DF2_DMU)/(F1*DF2_DMU - F2*DF1_DMU)
          ELSE
            DPHI_DCOS2 = (TWO*((ONE-MU(I))*DA_DCOS2(2)+TWO*WEIGHT(I)*MU(I)*DB_DCOS2(2))*DF1_DMU -
     .                    TWO*((ONE-MU(I))*DA_DCOS2(1)+TWO*WEIGHT(I)*MU(I)*DB_DCOS2(1))*DF2_DMU)/
     .                   ((ONE-MU(I))*(A(1)-A(2)) + TWO*WEIGHT(I)*MU(I)*(B(1)-B(2)))
          ENDIF
        ELSE
          DPHI_DSIG1 =  ZERO
          DPHI_DSIG2 =  ZERO
          DPHI_DCOS2 =  ZERO
        ENDIF
        NORMXX = HALF*(ONE + COS2THETA(I))*DPHI_DSIG1 + HALF*(ONE-COS2THETA(I))*DPHI_DSIG2 +
     .           (SIN2THETA(I)**2)*DPHI_DCOS2
        NORMYY = HALF*(ONE - COS2THETA(I))*DPHI_DSIG1 + HALF*(ONE+COS2THETA(I))*DPHI_DSIG2 -
     .           (SIN2THETA(I)**2)*DPHI_DCOS2
        NORMXY = SIN2THETA(I)*DPHI_DSIG1 - SIN2THETA(I)*DPHI_DSIG2 -
     .           (TWO*SIN2THETA(I)*COS2THETA(I))*DPHI_DCOS2
        IF (SIGN_CHANGED(I)) THEN
          NORMXX = -NORMXX
          NORMYY = -NORMYY
          NORMXY = -NORMXY
        ENDIF
c
        ! Restoring previous value of the yield function
        PHI(I) = PHI0(I)
c
        ! Computation of yield surface trial increment DPHI
        DPHI = NORMXX * DSIGXX(I)
     .       + NORMYY * DSIGYY(I)
     .       + NORMXY * DSIGXY(I)
c
        ! 2 - Computation of DPHI_DLAMBDA
        !---------------------------------------------------------
c
        !   a) Derivative with respect stress increments tensor DSIG
        !   --------------------------------------------------------
        DFDSIG2 = NORMXX * (A11*NORMXX + A12*NORMYY)
     .          + NORMYY * (A11*NORMYY + A12*NORMXX)
     .          + NORMXY * NORMXY * G
c
        !   b) Derivatives with respect to plastic strain P
        !   ------------------------------------------------
c
        !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
        !     ----------------------------------------------------------------------------
        IF (NUMTABL == 0) THEN
          ! Continuous hardening law
          HARDP(I)     = DSIGM*BETA
          IF (PLA(I)>ZERO) THEN
            HARDP(I)   = HARDP(I) + DSIGM*(NEXP*(ONE-EXP(-OMEGA*(PLA(I))))**(NEXP-ONE))*
     .                         (OMEGA*EXP(-OMEGA*(PLA(I))))
          ENDIF
        ENDIF
        ! Accounting for kinematic hardening
        DYLD_DPLA(I) = (ONE-FISOKIN)*HARDP(I)
c
        !     ii) Derivative of dPLA with respect to DLAM
        !     -------------------------------------------
        SIG_DFDSIG = SIGOXX(I) * NORMXX
     .             + SIGOYY(I) * NORMYY
     .             + SIGOXY(I) * NORMXY
        DPLA_DLAM(I) = SIG_DFDSIG/YLD(I)
c
        ! 3 - Computation of plastic multiplier and variables update
        !----------------------------------------------------------
c
        ! Derivative of PHI with respect to DLAM
        DPHI_DLAM(I) = - DFDSIG2 - DYLD_DPLA(I)*DPLA_DLAM(I)
        DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20) ,DPHI_DLAM(I))
c
        ! Computation of the plastic multiplier
        DLAM = - (DPHI + PHI(I)) / DPHI_DLAM(I)
        DLAM = MAX(DLAM, ZERO)
c
        ! Plastic strains tensor update
        DPXX(I) = DLAM * NORMXX
        DPYY(I) = DLAM * NORMYY
        DPXY(I) = DLAM * NORMXY
c
        ! Elasto-plastic stresses update
        SIGNXX(I) = SIGNXX(I) - (A11*DPXX(I) + A12*DPYY(I))
        SIGNYY(I) = SIGNYY(I) - (A11*DPYY(I) + A12*DPXX(I))
        SIGNXY(I) = SIGNXY(I) - DPXY(I)*G
c
        ! Cumulated plastic strain and strain rate update
        DDEP    = DLAM*SIG_DFDSIG/YLD(I)
        DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
        PLA(I)  = PLA(I) + DDEP
c
        ! Computation of the equivalent stress
        NORMSIG = SQRT(SIGNXX(I)*SIGNXX(I)
     .               + SIGNYY(I)*SIGNYY(I)
     .               + TWO*SIGNXY(I)*SIGNXY(I))
        IF (NORMSIG < EM20) THEN
          SIGVG(I) = ZERO
        ELSE
          ! Computation of the principal stresses
          MOHR_RADIUS = SQRT(((SIGNXX(I)-SIGNYY(I))/TWO)**2 + SIGNXY(I)**2)
          MOHR_CENTER = (SIGNXX(I)+SIGNYY(I))/TWO
          SIG1(I)     = MOHR_CENTER + MOHR_RADIUS
          SIG2(I)     = MOHR_CENTER - MOHR_RADIUS
          IF (MOHR_RADIUS>EM20) THEN
            COS2THETA(I) = ((SIGNXX(I)-SIGNYY(I))/TWO)/MOHR_RADIUS
            SIN2THETA(I) = SIGNXY(I)/MOHR_RADIUS
          ELSE
            COS2THETA(I) = ONE
            SIN2THETA(I) = ZERO
          ENDIF
          ! Computation of scale factors for shear
          HISH_THETA(I,1:2)  = ZERO
          DO J = 1,NANGLE
            DO K = 1,J
              HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
            ENDDO
          ENDDO
          ! Computation of the equivalent stress of Vegter
          IF (SIG1(I)<ZERO .OR. (SIG2(I)<ZERO .AND. SIG2(I)*HISH_THETA(I,1)<SIG1(I)*HISH_THETA(I,2))) THEN
            COS2THETA(I) = -COS2THETA(I)
            SIN2THETA(I) = -SIN2THETA(I)
            TMP     = SIG1(I)
            SIG1(I) = -SIG2(I)
            SIG2(I) = -TMP
            SIGN_CHANGED(I)  = .TRUE.
          ELSE
            SIGN_CHANGED(I) = .FALSE.
          ENDIF
          !   Between tension and compression uniaxial points
          IF (SIG2(I)<ZERO) THEN
            ! Interpolation of factors
            fUN_THETA(I,1:2)  = ZERO
            HISH_THETA(I,1:2) = ZERO
            WEIGHT(I)         = ZERO
            DO J = 1,NANGLE
              DO K = 1,J
                fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
                HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
                WEIGHT(I)         = WEIGHT(I)         + Q_WSH(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              ENDDO
            ENDDO
            ! Filling the table
            A(1)      = fUN_THETA(I,2)
            A(2)      = -fUN_THETA(I,1)
            B(1:2)    = HISH_THETA(I,1:2)
            C(1:2)    = fUN_THETA(I,1:2)
          !   Between uniaxial and equibiaxial point
          ELSE
            ! Interpolation of factors
            fUN_THETA(I,1:2)  = ZERO
            HIPS_THETA(I,1:2) = ZERO
            WEIGHT(I)         = ZERO
            DO J = 1,NANGLE
              DO K = 1,J
                fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
                HIPS_THETA(I,1:2) = HIPS_THETA(I,1:2) + Q_HIPS(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
                WEIGHT(I)         = WEIGHT(I)         + Q_WPS(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              ENDDO
            ENDDO
            ! Filling the table
            A(1:2)    = fUN_THETA(I,1:2)
            B(1:2)    = HIPS_THETA(I,1:2)
            C(1:2)    = fBI(1:2)
          ENDIF
          !   Determine MU and SIGVG
          IF (SIG1(I)<EM20) THEN
            MU(I)    = ZERO
            SIGVG(I) = ZERO
          ELSE
            SIG_RATIO = SIG2(I)/SIG1(I)
            VAR_A = (C(2)+A(2)-TWO*WEIGHT(I)*B(2)) - SIG_RATIO*(C(1)+A(1)-TWO*WEIGHT(I)*B(1))
            VAR_B = TWO*((WEIGHT(I)*B(2)-A(2)) - SIG_RATIO*(WEIGHT(I)*B(1)-A(1)))
            VAR_C = A(2) - SIG_RATIO*A(1)
            IF (ABS(VAR_A)<EM08) THEN
              MU(I) = -VAR_C/VAR_B
            ELSE
              MU(I) = (-VAR_B+SQRT(VAR_B*VAR_B - FOUR*VAR_A*VAR_C))/(TWO*VAR_A)
            ENDIF
            SIGVG(I) = SIG1(I)*(((ONE-MU(I))**2) + TWO*MU(I)*WEIGHT(I)*(ONE-MU(I)) + (MU(I)**2))/
     .            (A(1)*((ONE-MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2))
          END IF
        ENDIF
c
        IF (NUMTABL == 0) THEN
          ! Continuous hardening law update
          SIGHARD(I) = YLD0 + DSIGM*(BETA*(PLA(I))+(ONE-EXP(-OMEGA*(PLA(I))))**NEXP)
          ! Yield stress update
          YLD(I) = SIGHARD(I) + SIGRATE(I)
          YLD(I) = (ONE-FISOKIN)*YLD(I)+FISOKIN*YL0(I)
          YLD(I) = MAX(YLD(I), EM10)
          ! Yield function value update
          PHI(I) = SIGVG(I) - YLD(I)
        ENDIF
c
        ! Transverse strain update
        IF (INLOC == 0) THEN
          DEZZ(I) = DEZZ(I) - (DPXX(I)+DPYY(I))
        ENDIF
c
      ENDDO
      ! End of the loop over yielding elements
      !==============================================================
      ! - END OF PLASTIC CORRECTION WITH NICE EXPLICIT RETURN MAPPING
      !==============================================================
c
      ! If tabulated yield function, update of the yield stress for all element
      IF ((NUMTABL > 0).AND.(NINDX > 0)) THEN
        XVEC(1:NEL,1) = PLA(1:NEL)
        XVEC(1:NEL,2) = EPSP(1:NEL) * XSCALE
        IPOS(1:NEL,1) = VARTMP(1:NEL,1)
        IPOS(1:NEL,2) = 1
        CALL TABLE2D_VINTERP_LOG(TABLE(ITABLE(1)),ISMOOTH,NEL,IPOS,XVEC  ,YLD  ,HARDP,HARDR)
        YLD(1:NEL)      = YLD(1:NEL) * YSCALE
        HARDP(1:NEL)    = HARDP(1:NEL) * YSCALE
        VARTMP(1:NEL,1) = IPOS(1:NEL,1)
        ! Tabulation with Temperature
        IF (NUMTABL == 2) THEN
          ! Reference temperature factor
          XVEC(1:NEL,2) = TINI
          IPOS(1:NEL,1) = VARTMP(1:NEL,2)
          IPOS(1:NEL,2) = VARTMP(1:NEL,3)
          CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS,XVEC,YLD_TREF,DYDX)
          VARTMP(1:NEL,2) = IPOS(1:NEL,1)
          VARTMP(1:NEL,3) = IPOS(1:NEL,2)
          ! Current temperature factor
          XVEC(1:NEL,2) = TEMP(1:NEL)
          CALL TABLE_VINTERP(TABLE(ITABLE(2)),NEL,IPOS,XVEC,YLD_TEMP,DYDX)
          TFAC(1:NEL)  = YLD_TEMP(1:NEL) / YLD_TREF(1:NEL)
          YLD(1:NEL)   = YLD(1:NEL)   * TFAC(1:NEL)
          HARDP(1:NEL) = HARDP(1:NEL) * TFAC(1:NEL)
        ELSE
          TFAC(1:NEL) = ONE
        ENDIF
        ! Including kinematic hardening effect
        YLD(1:NEL) = (ONE-FISOKIN)*YLD(1:NEL)+FISOKIN*YL0(1:NEL)
        ! Yield function value update
        PHI(1:NEL) = SIGVG(1:NEL) - YLD(1:NEL)
      ENDIF
c
      ! Kinematic hardening
      IF (FISOKIN > ZERO) THEN
        DO I=1,NEL
          DSXX  = SIGEXX(I) - SIGNXX(I)
          DSYY  = SIGEYY(I) - SIGNYY(I)
          DSXY  = SIGEXY(I) - SIGNXY(I)
          DEXX  = (DSXX - NU*DSYY)
          DEYY  = (DSYY - NU*DSXX)
          DEXY  = TWO*(ONE+NU)*DSXY
          ALPHA = FISOKIN*HARDP(I)/(YOUNG+HARDP(I))*THIRD
          SIGNXX(I) = SIGNXX(I) + SIGA(I,1)
          SIGNYY(I) = SIGNYY(I) + SIGA(I,2)
          SIGNXY(I) = SIGNXY(I) + SIGA(I,3)
          SIGA(I,1) = SIGA(I,1) + ALPHA*(FOUR*DEXX+TWO*DEYY)
          SIGA(I,2) = SIGA(I,2) + ALPHA*(FOUR*DEYY+TWO*DEXX)
          SIGA(I,3) = SIGA(I,3) + ALPHA*DEXY
        ENDDO
      ENDIF
c
      ! Storing new values
      DO I=1,NEL
        ! Computation of the plastic strain rate
        IF (VFLAG == 1) THEN
          DPDT      = DPLA(I)/MAX(EM20,TIMESTEP)
          UVAR(I,1) = ASRATE * DPDT + (ONE - ASRATE) * UVAR(I,1)
          EPSP(I)   = UVAR(I,1)
        ENDIF
        ! USR Outputs
        UVAR(I,2)  = PHI(I)   ! Yield function value
        SEQ(I)     = SIGVG(I) ! SIGEQ
        ! Coefficient for hourglass
        ET(I)      = HARDP(I) / (HARDP(I) + YOUNG)
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT(A11/RHO(I))
        ! Storing the yield stress
        SIGY(I)    = YLD(I)
        ! Computation of the thickness variation
        DEELZZ(I) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG
        ! Computation of the non-local thickness variation
        IF (INLOC > 0) THEN
          ! Computation of old principal stresses
          MOHR_RADIUS = SQRT(((SIGNXX(I)-SIGNYY(I))/TWO)**2 + SIGNXY(I)**2)
          MOHR_CENTER = (SIGNXX(I)+SIGNYY(I))/TWO
          SIG1(I)     = MOHR_CENTER + MOHR_RADIUS
          SIG2(I)     = MOHR_CENTER - MOHR_RADIUS
          IF (MOHR_RADIUS>EM20) THEN
            COS2THETA(I) = ((SIGOXX(I)-SIGOYY(I))/TWO)/MOHR_RADIUS
            SIN2THETA(I) = SIGOXY(I)/MOHR_RADIUS
          ELSE
            COS2THETA(I) = ONE
            SIN2THETA(I) = ZERO
          ENDIF
          ! Computation of old scale factors for shear
          HISH_THETA(I,1:2)  = ZERO
          DO J = 1,NANGLE
            DO K = 1,J
              HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
            ENDDO
          ENDDO
          ! Computation of the old equivalent stress of Vegter
          IF (SIG1(I)<ZERO .OR. ((SIG2(I)<ZERO) .AND. (SIG2(I)*HISH_THETA(I,1)<SIG1(I)*HISH_THETA(I,2)))) THEN
            COS2THETA(I) = -COS2THETA(I)
            SIN2THETA(I) = -SIN2THETA(I)
            TMP     = SIG1(I)
            SIG1(I) = -SIG2(I)
            SIG2(I) = -TMP
            SIGN_CHANGED(I) = .TRUE.
          ELSE
            SIGN_CHANGED(I) = .FALSE.
          ENDIF
          !   Between tension and compression uniaxial points
          IF (SIG2(I)<ZERO) THEN
            ! Interpolation of factors
            fUN_THETA(I,1:2)  = ZERO
            HISH_THETA(I,1:2) = ZERO
            WEIGHT(I)         = ZERO
            DO J = 1,NANGLE
              DO K = 1,J
                fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
                HISH_THETA(I,1:2) = HISH_THETA(I,1:2) + Q_HISH(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
                WEIGHT(I)         = WEIGHT(I)         + Q_WSH(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              ENDDO
            ENDDO
            ! Filling the table
            A(1)      = fUN_THETA(I,2)
            A(2)      = -fUN_THETA(I,1)
            B(1:2)    = HISH_THETA(I,1:2)
            C(1:2)    = fUN_THETA(I,1:2)
            ! Derivatives of PHI with respect to THETA
            DA_DCOS2(1:2) = ZERO
            DB_DCOS2(1:2) = ZERO
            DC_DCOS2(1:2) = ZERO
            DWEIGHT_DCOS2 = ZERO
            IF (NANGLE > 1) THEN
              ! Computation of their first derivative with respect to COS2THET
              DO J = 2, NANGLE
                DO K = 2, J
                  DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HISH(J,1:2)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                  DC_DCOS2(1)   = DC_DCOS2(1)   + Q_fUN(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                  DWEIGHT_DCOS2 = DWEIGHT_DCOS2 + Q_WSH(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                ENDDO
              ENDDO
              DA_DCOS2(2) = -DC_DCOS2(1)
            ENDIF
          !   Between uniaxial and equibiaxial point
          ELSE
            ! Interpolation of factors
            fUN_THETA(I,1:2)  = ZERO
            HIPS_THETA(I,1:2) = ZERO
            WEIGHT(I)         = ZERO
            DO J = 1,NANGLE
              DO K = 1,J
                fUN_THETA(I,1)    = fUN_THETA(I,1)    + Q_fUN(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
                HIPS_THETA(I,1:2) = HIPS_THETA(I,1:2) + Q_HIPS(J,1:2)*COS2(K,J)*(COS2THETA(I))**(K-1)
                WEIGHT(I)         = WEIGHT(I)         + Q_WPS(J)*COS2(K,J)*(COS2THETA(I))**(K-1)
              ENDDO
            ENDDO
            ! Filling the table
            A(1:2)    = fUN_THETA(I,1:2)
            B(1:2)    = HIPS_THETA(I,1:2)
            C(1:2)    = fBI(1:2)
            ! Derivatives of PHI with respect to THETA
            DA_DCOS2(1:2) = ZERO
            DB_DCOS2(1:2) = ZERO
            DC_DCOS2(1:2) = ZERO
            DWEIGHT_DCOS2 = ZERO
            ! If anisotropic, compute derivatives with respect to COS2THET
            IF (NANGLE > 1) THEN
              ! Computation of their first derivative with respect to COS2THET
              DO J = 2, NANGLE
                DO K = 2, J
                  DA_DCOS2(1)   = DA_DCOS2(1)   + Q_fUN(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                  DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HIPS(J,1:2)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                  DWEIGHT_DCOS2 = DWEIGHT_DCOS2 + Q_WPS(J)*(K-1)*COS2(K,J)*(COS2THETA(I))**(K-2)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
          !   Determine MU and SIGVG
          IF (SIG1(I)<EM20) THEN
            MU(I)    = ZERO
            SIGVG(I) = ZERO
          ELSE
            SIG_RATIO = SIG2(I)/SIG1(I)
            VAR_A = (C(2)+A(2)-TWO*WEIGHT(I)*B(2)) - SIG_RATIO*(C(1)+A(1)-TWO*WEIGHT(I)*B(1))
            VAR_B = TWO*((WEIGHT(I)*B(2)-A(2)) - SIG_RATIO*(WEIGHT(I)*B(1)-A(1)))
            VAR_C = A(2) - SIG_RATIO*A(1)
            IF (ABS(VAR_A)<EM08) THEN
              MU(I) = -VAR_C/VAR_B
            ELSE
              MU(I) = (-VAR_B+SQRT(VAR_B*VAR_B - FOUR*VAR_A*VAR_C))/(TWO*VAR_A)
            ENDIF
            SIGVG(I) = SIG1(I)*(((ONE-MU(I))**2) + TWO*MU(I)*WEIGHT(I)*(ONE-MU(I)) + (MU(I)**2))/
     .            (A(1)*((ONE-MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2))
          END IF
          U         = A(1)*((ONE - MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2)
          UPRIM     = DA_DCOS2(1)*((ONE - MU(I))**2) + TWO*MU(I)*(ONE-MU(I))*(DWEIGHT_DCOS2*B(1)+
     .                     WEIGHT(I)*DB_DCOS2(1)) + DC_DCOS2(1)*(MU(I)**2)
          V         = (ONE-MU(I))**2 + TWO*WEIGHT(I)*MU(I)*(ONE-MU(I)) + MU(I)**2
          VPRIM     = TWO*MU(I)*(ONE-MU(I))*DWEIGHT_DCOS2
          F1        = U/MAX(V,EM20)
          DF1_DCOS2 = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
          U         = A(2)*((ONE - MU(I))**2) + TWO*B(2)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(2)*(MU(I)**2)
          UPRIM     = DA_DCOS2(2)*((ONE - MU(I))**2) + TWO*MU(I)*(ONE-MU(I))*(DWEIGHT_DCOS2*B(2)+
     .                     WEIGHT(I)*DB_DCOS2(2)) + DC_DCOS2(2)*(MU(I)**2)
          F2        = U/MAX(V,EM20)
          DF2_DCOS2 = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
          !   Derivatives of Fi with respect to MU
          U       = A(1)*((ONE - MU(I))**2) + TWO*B(1)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(1)*(MU(I)**2)
          UPRIM   = -TWO*(ONE-MU(I))*A(1)  + TWO*WEIGHT(I)*B(1)*(ONE-TWO*MU(I))  + TWO*MU(I)*C(1)
          V       = (ONE-MU(I))**2 + TWO*WEIGHT(I)*MU(I)*(ONE-MU(I)) + MU(I)**2
          VPRIM   = -TWO*(ONE-MU(I)) + TWO*WEIGHT(I)*(ONE-TWO*MU(I)) + TWO*MU(I)
          DF1_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
          U       = A(2)*((ONE - MU(I))**2) + TWO*B(2)*MU(I)*WEIGHT(I)*(ONE-MU(I)) + C(2)*(MU(I)**2)
          UPRIM   = -TWO*(ONE-MU(I))*A(2)  + TWO*WEIGHT(I)*B(2)*(ONE-TWO*MU(I))  + TWO*MU(I)*C(2)
          DF2_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
          IF ((F1*DF2_DMU - F2*DF1_DMU)/=ZERO) THEN
            DPHI_DSIG1 =  DF2_DMU/(F1*DF2_DMU - F2*DF1_DMU)
            DPHI_DSIG2 = -DF1_DMU/(F1*DF2_DMU - F2*DF1_DMU)
            IF (ABS(SIG1(I)-SIG2(I))>TOL) THEN
              DPHI_DCOS2 = (SIGVG(I)/(SIG1(I) - SIG2(I)))*(DF2_DCOS2*DF1_DMU -
     .                                DF1_DCOS2*DF2_DMU)/(F1*DF2_DMU - F2*DF1_DMU)
            ELSE
              DPHI_DCOS2 = (TWO*((ONE-MU(I))*DA_DCOS2(2)+TWO*WEIGHT(I)*MU(I)*DB_DCOS2(2))*DF1_DMU -
     .                      TWO*((ONE-MU(I))*DA_DCOS2(1)+TWO*WEIGHT(I)*MU(I)*DB_DCOS2(1))*DF2_DMU)/
     .                     ((ONE-MU(I))*(A(1)-A(2)) + TWO*WEIGHT(I)*MU(I)*(B(1)-B(2)))
            ENDIF
          ELSE
            DPHI_DSIG1 =  ZERO
            DPHI_DSIG2 =  ZERO
            DPHI_DCOS2 =  ZERO
          ENDIF
          NORMXX = HALF*(ONE + COS2THETA(I))*DPHI_DSIG1 + HALF*(ONE-COS2THETA(I))*DPHI_DSIG2 +
     .             (SIN2THETA(I)**2)*DPHI_DCOS2
          NORMYY = HALF*(ONE - COS2THETA(I))*DPHI_DSIG1 + HALF*(ONE+COS2THETA(I))*DPHI_DSIG2 -
     .             (SIN2THETA(I)**2)*DPHI_DCOS2
          NORMXY = SIN2THETA(I)*DPHI_DSIG1 - SIN2THETA(I)*DPHI_DSIG2 -
     .             (TWO*SIN2THETA(I)*COS2THETA(I))*DPHI_DCOS2
          IF (SIGN_CHANGED(I)) THEN
            NORMXX = -NORMXX
            NORMYY = -NORMYY
            NORMXY = -NORMXY
          ENDIF
          SIG_DFDSIG = SIGNXX(I) * NORMXX
     .               + SIGNYY(I) * NORMYY
     .               + SIGNXY(I) * NORMXY
          IF (SIG_DFDSIG > ZERO) THEN
            DEZZ(I) = -MAX(DPLANL(I),ZERO)*(YLD(I)/SIG_DFDSIG)*(NORMXX+NORMYY)
          ELSE
            DEZZ(I) = ZERO
          ENDIF
        ENDIF
        DEZZ(I)   = DEELZZ(I) + DEZZ(I)
        THK(I)    = THK(I) + DEZZ(I)*THKLY(I)*OFF(I)
      ENDDO
c
      END
