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
Chd|  MAT115_NICE                   source/materials/mat/mat115/mat115_nice.F
Chd|-- called by -----------
Chd|        SIGEPS115                     source/materials/mat/mat115/sigeps115.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MAT115_NICE(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,GRHO    ,
     2     TIME    ,TIMESTEP,UPARAM  ,UVAR    ,OFF     ,SIGY    ,
     3     RHO0    ,PLA     ,DPLA    ,SOUNDSP ,ET      ,SEQ     ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  )
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
        INTEGER NEL,NUPARAM,NUVAR
        INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
        my_real
     .     TIME,TIMESTEP
        my_real,DIMENSION(NUPARAM), INTENT(IN) ::
     .     UPARAM
        my_real,DIMENSION(2*NEL), INTENT(IN)     ::
     .     GRHO
        my_real,DIMENSION(NEL), INTENT(IN)     ::
     .     RHO0,
     .     DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .     SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
c
        my_real ,DIMENSION(NEL), INTENT(OUT)   ::
     .     SOUNDSP,SIGY,ET,
     .     SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
c
        my_real ,DIMENSION(NEL), INTENT(INOUT)       ::
     .     PLA,DPLA,OFF,SEQ
        my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) ::
     .     UVAR
c
        !=======================================================================
        !      Local Variables
        !=======================================================================
        INTEGER I,K,II,NINDX,INDEX(NEL),Istat
c
        my_real
     .     YOUNG,BULK,LAMHOOK,G,G2,NU,ALPHA,CFAIL,PFAIL,RHOF0
c
        my_real, DIMENSION(NEL) ::
     .     GAMMA,EPSD,ALPHA2,BETA,SIGP
c
        my_real
     .     H,LDAV,TRDEP,DLAM,DDEP,DPHI_DSIGVM,DPHI_DSIGM,
     .     DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX,TRDFDS,
     .     NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,
     .     DPHI,SIG_DFDSIG,DFDSIG2,DPHI_DPLA,
     .     I1,I2,I3,Q,R,R_INTER,PSI,S11,S22,S33
c
        my_real, DIMENSION(NEL) ::
     .     SIGVM,SIGEQ,DSIGXX,DSIGYY,DSIGZZ,DSIGXY,DSIGYZ,DSIGZX,TRSIG,
     .     SXX,SYY,SZZ,SXY,SYZ,SZX,SIGM,J2,YLD,HARDP,PHI0,PHI,DPLA_DLAM,
     .     DEFVP,DPHI_DLAM
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
        LAMHOOK = UPARAM(5)  ! Lambda Hooke parameter
        NU      = UPARAM(6)  ! Poisson ration
        ! Statistic variation flag
        Istat   = UPARAM(12)
        ! Plastic criterion and hardening parameters
        ALPHA   = UPARAM(13) ! Yield function shape parameter
        CFAIL   = UPARAM(14) ! Tensile volumic strain at failure
        PFAIL   = UPARAM(15) ! Principal stress at failure
        ! Density of base material
        IF (Istat == 1) RHOF0 = UPARAM(16)
c
        ! Recovering internal variables
        DO I=1,NEL
          IF (OFF(I) < 0.1) OFF(I) = ZERO
          IF (OFF(I) < ONE)  OFF(I) = OFF(I)*FOUR_OVER_5
          ! User inputs
          PHI0(I)  = UVAR(I,1) ! Previous yield function value
          ! Standard inputs
          DEFVP(I) = ZERO      ! Initialization of the volumic plastic strain increment
          DPLA(I)  = ZERO      ! Initialization of the plastic strain increment
          ET(I)    = ONE        ! Initialization of hourglass coefficient
          HARDP(I) = ZERO      ! Initialization of hourglass coefficient
          IF (Istat == 0) THEN
            GAMMA(I)   = UPARAM(16) ! Yield stress parameter
            EPSD(I)    = UPARAM(17) ! Densification strain
            ALPHA2(I)  = UPARAM(18) ! Yield stress parameter
            BETA(I)    = UPARAM(19) ! Yield stress parameter
            SIGP(I)    = UPARAM(20) ! Initial yield stress
          ELSE
            SIGP(I)    = UPARAM(17) + UPARAM(18)*((GRHO(I+NEL)/RHOF0)**UPARAM(19))
            ALPHA2(I)  = UPARAM(20) + UPARAM(21)*((GRHO(I+NEL)/RHOF0)**UPARAM(22))
            GAMMA(I)   = UPARAM(23) + UPARAM(24)*((GRHO(I+NEL)/RHOF0)**UPARAM(25))
            BETA(I)    = ONE/(UPARAM(26) + UPARAM(27)*((GRHO(I+NEL)/RHOF0)**UPARAM(28)))
            EPSD(I)    = (-(NINE + (ALPHA**2))/(THREE*(ALPHA**2)))*LOG(GRHO(I+NEL)/RHOF0)
          ENDIF
        ENDDO
c
        ! Computation of the initial yield stress
        DO I = 1,NEL
          ! a) - Hardening law
          YLD(I) = SIGP(I) + GAMMA(I)*(PLA(I)/EPSD(I)) +
     .                  ALPHA2(I)*LOG(ONE/MAX((ONE - (PLA(I)/EPSD(I))**BETA(I)),EM20))
          ! b) - Checking values
          YLD(I) = MAX(EM10,YLD(I))
        ENDDO
c
        !========================================================================
        ! - COMPUTATION OF TRIAL VALUES
        !========================================================================
        DO I=1,NEL
          ! Computation of the trial stress tensor
          LDAV = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAMHOOK
          SIGNXX(I) = SIGOXX(I) + DEPSXX(I)*G2 + LDAV
          SIGNYY(I) = SIGOYY(I) + DEPSYY(I)*G2 + LDAV
          SIGNZZ(I) = SIGOZZ(I) + DEPSZZ(I)*G2 + LDAV
          SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G
          SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*G
          SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*G
          ! Computation of the trace of the trial stress tensor
          TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
          SIGM(I)   = TRSIG(I) * THIRD
          ! Computation of the deviatoric trial stress tensor
          SXX(I) = SIGNXX(I) - SIGM(I)
          SYY(I) = SIGNYY(I) - SIGM(I)
          SZZ(I) = SIGNZZ(I) - SIGM(I)
          SXY(I) = SIGNXY(I)
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)
          ! Second deviatoric invariant and Mises equivalent stress
          J2(I) = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .            +       SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
          SIGVM(I) = SQRT(THREE*J2(I))
          ! Deshpande - Fleck equivalent stress
          SIGEQ(I) = SQRT((SIGVM(I)**2 + (ALPHA*SIGM(I))**2)/(ONE + (ALPHA/THREE)**2))
        ENDDO
c
        !========================================================================
        ! - COMPUTATION OF YIELD FONCTION
        !========================================================================
        PHI(1:NEL) = SIGEQ(1:NEL) - YLD(1:NEL)
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
          LDAV = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAMHOOK
          DSIGXX(I) = DEPSXX(I)*G2 + LDAV
          DSIGYY(I) = DEPSYY(I)*G2 + LDAV
          DSIGZZ(I) = DEPSZZ(I)*G2 + LDAV
          DSIGXY(I) = DEPSXY(I)*G
          DSIGYZ(I) = DEPSYZ(I)*G
          DSIGZX(I) = DEPSZX(I)*G
          DLAM      = ZERO
c
          ! Computation of Von Mises equivalent stress of the previous stress tensor
          TRSIG(I)  = SIGOXX(I) + SIGOYY(I) + SIGOZZ(I)
          SIGM(I)   = TRSIG(I) * THIRD
          SXX(I)    = SIGOXX(I) - SIGM(I)
          SYY(I)    = SIGOYY(I) - SIGM(I)
          SZZ(I)    = SIGOZZ(I) - SIGM(I)
          SXY(I)    = SIGOXY(I)
          SYZ(I)    = SIGOYZ(I)
          SZX(I)    = SIGOZX(I)
          J2(I)     = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .              +         SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
          SIGVM(I)  = SQRT(THREE*J2(I))
          ! Deshpande - Fleck equivalent stress
          SIGEQ(I)  = SQRT((SIGVM(I)**2 + (ALPHA*SIGM(I))**2)/(ONE + (ALPHA/THREE)**2))

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
          ! Derivative with respect to the Von Mises equivalent stress
          DPHI_DSIGVM = SIGVM(I)/MAX((SIGEQ(I)*(ONE + (ALPHA/THREE)**2)),EM20)
c
          ! Derivative with respect to the pressure stress
          DPHI_DSIGM  = (ALPHA**2)*SIGM(I)/MAX((SIGEQ(I)*(ONE + (ALPHA/THREE)**2)),EM20)
c
          ! dPhi/dSig
          NORMXX  = DPHI_DSIGVM*THREE_HALF*SXX(I)/(MAX(SIGVM(I),EM20)) + DPHI_DSIGM*THIRD
          NORMYY  = DPHI_DSIGVM*THREE_HALF*SYY(I)/(MAX(SIGVM(I),EM20)) + DPHI_DSIGM*THIRD
          NORMZZ  = DPHI_DSIGVM*THREE_HALF*SZZ(I)/(MAX(SIGVM(I),EM20)) + DPHI_DSIGM*THIRD
          NORMXY  = TWO*DPHI_DSIGVM*THREE_HALF*SXY(I)/(MAX(SIGVM(I),EM20))
          NORMYZ  = TWO*DPHI_DSIGVM*THREE_HALF*SYZ(I)/(MAX(SIGVM(I),EM20))
          NORMZX  = TWO*DPHI_DSIGVM*THREE_HALF*SZX(I)/(MAX(SIGVM(I),EM20))
c
          ! Restoring previous value of the yield function
          PHI(I) = PHI0(I)
c
          ! Computation of yield surface trial increment DPHI
          DPHI = NORMXX * DSIGXX(I)
     .         + NORMYY * DSIGYY(I)
     .         + NORMZZ * DSIGZZ(I)
     .         + NORMXY * DSIGXY(I)
     .         + NORMYZ * DSIGYZ(I)
     .         + NORMZX * DSIGZX(I)
c
          ! 2 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c
          !   a) Derivative with respect stress increments tensor DSIG
          !   --------------------------------------------------------

          TRDFDS  = NORMXX + NORMYY + NORMZZ
          DFDSIG2 = NORMXX * (NORMXX*G2 + LAMHOOK*TRDFDS)
     .            + NORMYY * (NORMYY*G2 + LAMHOOK*TRDFDS)
     .            + NORMZZ * (NORMZZ*G2 + LAMHOOK*TRDFDS)
     .            + NORMXY * NORMXY * G
     .            + NORMYZ * NORMYZ * G
     .            + NORMZX * NORMZX * G
c
          !   b) Derivatives with respect to plastic strain P
          !   ------------------------------------------------
c
          !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
          !     ----------------------------------------------------------------------------
          HARDP(I)   = (GAMMA(I)/EPSD(I)) +
     .        ALPHA2(I)*((ONE-(BETA(I)/EPSD(I))*(PLA(I)/EPSD(I))**(BETA(I)-1))/
     .        MAX((ONE - (PLA(I)/EPSD(I))**BETA(I)),EM20))
          DPHI_DPLA  = HARDP(I)
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
          !  d) Derivative with respect to the yield stress
          !  ----------------------------------------------
c
          !  e) Derivative of PHI with respect to DLAM ( = -DENOM)
          DPHI_DLAM(I) = - DFDSIG2 - DPHI_DPLA*DPLA_DLAM(I)
          DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20) ,DPHI_DLAM(I))
c
          ! 3 - Computation of plastic multiplier and variables update
          !----------------------------------------------------------
c
          ! Computation of the plastic multiplier
          DLAM  = -(DPHI + PHI(I)) / DPHI_DLAM(I)
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
          DDEP     = (DLAM/YLD(I))*SIG_DFDSIG
          DPLA(I)  = DPLA(I) + DDEP
          PLA(I)   = PLA(I)  + DDEP
          DEFVP(I) = DEFVP(I) + TRDEP
c
          ! Elasto-plastic stresses update
          LDAV      = TRDEP * LAMHOOK
          SIGNXX(I) = SIGOXX(I) + DSIGXX(I) - (DPXX*G2 + LDAV)
          SIGNYY(I) = SIGOYY(I) + DSIGYY(I) - (DPYY*G2 + LDAV)
          SIGNZZ(I) = SIGOZZ(I) + DSIGZZ(I) - (DPZZ*G2 + LDAV)
          SIGNXY(I) = SIGOXY(I) + DSIGXY(I) - DPXY*G
          SIGNYZ(I) = SIGOYZ(I) + DSIGYZ(I) - DPYZ*G
          SIGNZX(I) = SIGOZX(I) + DSIGZX(I) - DPZX*G
          TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
          SIGM(I)   = TRSIG(I) * THIRD
          SXX(I)    = SIGNXX(I) - SIGM(I)
          SYY(I)    = SIGNYY(I) - SIGM(I)
          SZZ(I)    = SIGNZZ(I) - SIGM(I)
          SXY(I)    = SIGNXY(I)
          SYZ(I)    = SIGNYZ(I)
          SZX(I)    = SIGNZX(I)
          J2(I)     = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .              +         SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
          SIGVM(I)  = SQRT(THREE*J2(I))
          ! Deshpande - Fleck equivalent stress
          SIGEQ(I)  = SQRT((SIGVM(I)**2 + (ALPHA*SIGM(I))**2)/(ONE + (ALPHA/THREE)**2))
c
          ! Yield stress update
          YLD(I) = SIGP(I) + GAMMA(I)*(PLA(I)/EPSD(I)) +
     .                  ALPHA2(I)*LOG(ONE/MAX((ONE - (PLA(I)/EPSD(I))**BETA(I)),EM20))
          YLD(I) = MAX(YLD(I),EM10)
c
          ! Yield function value update
          PHI(I) = SIGEQ(I) - YLD(I)
        ENDDO
        !===================================================================
        ! - END OF PLASTIC CORRECTION WITH NICE - EXPLICIT 1 METHOD
        !===================================================================
c
        ! Storing new values
        DO I=1,NEL
          ! USR Outputs
          IF (DPLA(I) > ZERO) THEN
            UVAR(I,1) = PHI(I)   ! Yield function value
          ELSE
            UVAR(I,1) = ZERO
          ENDIF
          IF (CFAIL > ZERO) THEN
            UVAR(I,2) = UVAR(I,2) + DEFVP(I)
            IF (UVAR(I,2) >= CFAIL) THEN
              IF (OFF(I) == ONE) OFF(I) = FOUR_OVER_5
            ENDIF
          ENDIF
          IF (PFAIL > ZERO) THEN
            ! Computing the principal stresses
            I1 = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
            I2 = SIGNXX(I)*SIGNYY(I)+SIGNYY(I)*SIGNZZ(I)+SIGNZZ(I)*SIGNXX(I)-
     .           SIGNXY(I)*SIGNXY(I)-SIGNZX(I)*SIGNZX(I)-SIGNYZ(I)*SIGNYZ(I)
            I3 = SIGNXX(I)*SIGNYY(I)*SIGNZZ(I)-SIGNXX(I)*SIGNYZ(I)*SIGNYZ(I)-
     .           SIGNYY(I)*SIGNZX(I)*SIGNZX(I)-SIGNZZ(I)*SIGNXY(I)*SIGNXY(I)+
     .           TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
            Q  = (THREE*I2 - I1*I1)/NINE
            R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54
            R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
            PSI = ACOS(MAX(R_INTER,-ONE))
            S11 = TWO*SQRT(-Q)*COS(PSI/THREE)+THIRD*I1
            S22 = TWO*SQRT(-Q)*COS((PSI+TWO*PI)/THREE)+THIRD*I1
            S33 = TWO*SQRT(-Q)*COS((PSI+FOUR*PI)/THREE)+THIRD*I1
            S11 = MAX(S11,S22,S33)
            ! Failure criterion
            IF (S11 >= PFAIL) THEN
              IF (OFF(I) == ONE) OFF(I) = FOUR_OVER_5
            ENDIF
          ENDIF
          SEQ(I)     = SIGEQ(I) ! SIGEQ
          ! Coefficient for hourglass
          ET(I)      = HARDP(I) / (HARDP(I) + YOUNG)
          ! Computation of the sound speed
          SOUNDSP(I) = SQRT((BULK + FOUR_OVER_3*G)/GRHO(NEL+I))
          ! Storing the yield stress
          SIGY(I)    = YLD(I)
        ENDDO
c
      END
