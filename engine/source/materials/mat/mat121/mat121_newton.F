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
Chd|  MAT121_NEWTON                 source/materials/mat/mat121/mat121_newton.F
Chd|-- called by -----------
Chd|        SIGEPS121                     source/materials/mat/mat121/sigeps121.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE MAT121_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA     ,
     3         DPLA    ,SOUNDSP ,EPSD    ,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     6         SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7         SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8         SIGY    ,ET      ,SEQ     )
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
      INTEGER NEL,NUPARAM,NUVAR,NPF(*),NFUNC,IFUNC(NFUNC)
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real
     .   TIME,TIMESTEP,TF(*)
      my_real,DIMENSION(NUPARAM), INTENT(IN) ::
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     ::
     .   RHO,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL), INTENT(OUT)   ::
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL), INTENT(INOUT)       ::
     .   PLA,DPLA,EPSD,OFF,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) ::
     .   UVAR
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,II,Ivisc,ITER,NITER,NINDX,INDEX(NEL),IPOS(NEL),
     .        IAD(NEL),ILEN(NEL)
      my_real
     .   YOUNG(NEL),BULK(NEL),G(NEL),NU,NNU,TANG(NEL),LAM(NEL),G2(NEL),
     .   AFILTR,Xscale_SIG0,Yscale_SIG0,Xscale_YOUN,Yscale_YOUN,
     .   Xscale_TANG,Yscale_TANG
      my_real
     .   DPDT,DLAM,DDEP,DEPXX,DEPYY,DEVEPSPXX,DEVEPSPYY,DEVEPSPZZ,TREPSP,LDAV,
     .   NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,DENOM,DFDSIG2,DPDT_NL,DEPSDT,
     .   DTINV
      my_real, DIMENSION(NEL) ::
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,SIGVM,YLD,HARDP,PHI,DEZZ,YLD0,DYLD0DEPSD,
     .   DYOUNDEPSD,DTANGDEPSD,TRSIG,DPHI_DLAM,TEST,DPXX,DPYY,DPXY,
     .   DPZZ,DPYZ,DPZX
c
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! Elastic parameters
      YOUNG(1:NEL) = UPARAM(1)  ! Young modulus
      BULK(1:NEL)  = UPARAM(2)  ! Bulk modulus
      G(1:NEL)     = UPARAM(3)  ! Shear modulus
      G2(1:NEL)    = UPARAM(4)  ! 2*Shear modulus
      LAM(1:NEL)   = UPARAM(5)  ! Lame coefficient
      NU           = UPARAM(6)  ! Poisson ration
      NNU          = UPARAM(7)  ! NU/(1-NU)
      ! Flags for computation
      Ivisc        = NINT(UPARAM(12)) ! Viscosity formulation
      ! Strain-rate filtering (if Ivisc = 0)
      AFILTR       = MIN(ONE, UPARAM(14)*TIMESTEP)
      ! Initial yield stress vs strain-rate curve
      Xscale_SIG0  = UPARAM(16) ! Strain-rate scale factor
      Yscale_SIG0  = UPARAM(17) ! Initial yield stress scale factor
      ! Young modulus vs strain-rate curve
      Xscale_YOUN  = UPARAM(18) ! Strain-rate scale factor
      Yscale_YOUN  = UPARAM(19) ! Young modulus scale factor
      ! Tangent modulus vs strain-rate curve
      IF (IFUNC(3) > 0) THEN
        Xscale_TANG  = UPARAM(20) ! Strain-rate scale factor
        Yscale_TANG  = UPARAM(21) ! Tangent modulus scale factor
        TANG(1:NEL)  = ZERO
      ELSE
        TANG(1:NEL)  = UPARAM(21) ! Constant tangent modulus
      ENDIF
      DTINV          = ONE/MAX(TIMESTEP,EM20) ! Inverse of timestep
c
      ! Recovering internal variables
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < ONE)   OFF(I) = OFF(I)*FOUR_OVER_5
        ! Standard inputs
        DPLA(I)       = ZERO ! Initialization of the plastic strain increment
        ET(I)         = ONE   ! Initialization of hourglass coefficient
        HARDP(I)      = ZERO ! Initialization of hourglass coefficient
        DPXX(I)       = ZERO ! Initialization of the XX plastic strain increment
        DPYY(I)       = ZERO ! Initialization of the YY plastic strain increment
        DPZZ(I)       = ZERO ! Initialization of the ZZ plastic strain increment
        DPXY(I)       = ZERO ! Initialization of the XY plastic strain increment
        DPYZ(I)       = ZERO ! Initialization of the YZ plastic strain increment
        DPZX(I)       = ZERO ! Initialization of the ZX plastic strain increment
        DYLD0DEPSD(I) = ZERO ! Initialization of the derivative of SIG0
        DYOUNDEPSD(I) = ZERO ! Initialization of the derivative of YOUN
        DTANGDEPSD(I) = ZERO ! Initialization of the derivative of TANG
      ENDDO
c
      ! Filling the strain-rate vector
      IF (Ivisc == 0) THEN
        ! Compute effective strain-rate
        DO I = 1,NEL
          TREPSP    = THIRD*(EPSPXX(I) + EPSPYY(I) + EPSPZZ(I))
          DEVEPSPXX = EPSPXX(I) - TREPSP
          DEVEPSPYY = EPSPYY(I) - TREPSP
          DEVEPSPZZ = EPSPZZ(I) - TREPSP
          DEPSDT    = TWO_THIRD*(DEVEPSPXX**2 + DEVEPSPYY**2 + DEVEPSPZZ**2 +
     .                        TWO*(EPSPXY(I)**2) + TWO*(EPSPYZ(I)**2) +
     .                        TWO*(EPSPZX(I)**2))
          DEPSDT    = SQRT(MAX(DEPSDT,ZERO))
          EPSD(I)   = AFILTR * DEPSDT + (ONE - AFILTR) * EPSD(I)
        ENDDO
      ELSE
        ! Reset plastic strain-rate
        EPSD(1:NEL) = ZERO
      ENDIF
c
      ! Compute the initial yield stress
      IPOS(1:NEL) = 1
      IAD (1:NEL) = NPF(IFUNC(1)) / 2 + 1
      ILEN(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
      CALL VINTER2(TF,IAD,IPOS,ILEN,NEL,EPSD/Xscale_SIG0,DYLD0DEPSD,YLD0)
      YLD0(1:NEL) = Yscale_SIG0*YLD0(1:NEL)
      DYLD0DEPSD(1:NEL) = Yscale_SIG0*DYLD0DEPSD(1:NEL)
      ! Compute the Young modulus
      IF (IFUNC(2) > 0) THEN
        IPOS(1:NEL)  = 1
        IAD (1:NEL)  = NPF(IFUNC(2)) / 2 + 1
        ILEN(1:NEL)  = NPF(IFUNC(2)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        CALL VINTER2(TF,IAD,IPOS,ILEN,NEL,EPSD/Xscale_YOUN,DYOUNDEPSD,YOUNG)
        YOUNG(1:NEL) = Yscale_YOUN*YOUNG(1:NEL)
        G(1:NEL)     = HALF * YOUNG(1:NEL) / (ONE + NU)
        G2(1:NEL)    = YOUNG(1:NEL) / (ONE + NU)
        BULK(1:NEL)  = THIRD * YOUNG(1:NEL) / (ONE - NU*TWO)
        LAM(1:NEL)   = G2(1:NEL) * NU /(ONE - TWO*NU)
      ENDIF
      ! Compute the Tangent modulus
      IF (IFUNC(3) > 0) THEN
        IPOS(1:NEL) = 1
        IAD (1:NEL) = NPF(IFUNC(3)) / 2 + 1
        ILEN(1:NEL) = NPF(IFUNC(3)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        CALL VINTER2(TF,IAD,IPOS,ILEN,NEL,EPSD/Xscale_TANG,DTANGDEPSD,TANG)
        TANG(1:NEL) = Yscale_TANG*TANG(1:NEL)
        DTANGDEPSD(1:NEL) = Yscale_TANG*DTANGDEPSD(1:NEL)
      ENDIF
      ! Check tangent modulus value + Assembling the yield stress
      DO I = 1,NEL
        IF (TANG(I) >= 0.99D0*YOUNG(I)) THEN
          TANG(I) = 0.99D0*YOUNG(I)
          DTANGDEPSD(I) = ZERO
        ENDIF
        YLD(I) = YLD0(I) + (YOUNG(I)*TANG(I)/(YOUNG(I)-TANG(I)))*PLA(I)
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
        ! Computation of the trial stress tensor
        LDAV = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAM(I)
        SIGNXX(I) = SIGOXX(I) + DEPSXX(I)*G2(I) + LDAV
        SIGNYY(I) = SIGOYY(I) + DEPSYY(I)*G2(I) + LDAV
        SIGNZZ(I) = SIGOZZ(I) + DEPSZZ(I)*G2(I) + LDAV
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G(I)
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*G(I)
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*G(I)
        ! Computation of the trace of the trial stress tensor
        TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
        ! Computation of the deviatoric trial stress tensor
        SXX(I)    = SIGNXX(I) - TRSIG(I) * THIRD
        SYY(I)    = SIGNYY(I) - TRSIG(I) * THIRD
        SZZ(I)    = SIGNZZ(I) - TRSIG(I) * THIRD
        SXY(I)    = SIGNXY(I)
        SYZ(I)    = SIGNYZ(I)
        SZX(I)    = SIGNZX(I)
        ! Von Mises equivalent stress
        SIGVM(I)  = THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SXY(I)**2
     .              + THREE*SYZ(I)**2 + THREE*SZX(I)**2
        SIGVM(I)  = SQRT(SIGVM(I))
      ENDDO
c
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !========================================================================
      PHI(1:NEL) = SIGVM(1:NEL) - YLD(1:NEL)
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
      !====================================================================
      ! - PLASTIC CORRECTION WITH CUTTING PLANE (NEWTON-ITERATION) METHOD
      !====================================================================
      IF (NINDX > 0) THEN
c
        ! Number of Newton iterations
        IF (Ivisc == 0) THEN
          NITER = 3
        ELSE
          NITER = 5
        ENDIF
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
            NORMXX = THREE_HALF*SXX(I)/SIGVM(I)
            NORMYY = THREE_HALF*SYY(I)/SIGVM(I)
            NORMZZ = THREE_HALF*SZZ(I)/SIGVM(I)
            NORMXY = THREE*SXY(I)/SIGVM(I)
            NORMYZ = THREE*SYZ(I)/SIGVM(I)
            NORMZX = THREE*SZX(I)/SIGVM(I)
c
            ! 2 - Computation of DPHI_DLAMBDA
            !---------------------------------------------------------
c
            !   a) Derivative with respect stress increments tensor DSIG
            !   --------------------------------------------------------
            DFDSIG2 = NORMXX * NORMXX * G2(I)
     .              + NORMYY * NORMYY * G2(I)
     .              + NORMZZ * NORMZZ * G2(I)
     .              + NORMXY * NORMXY * G(I)
     .              + NORMYZ * NORMYZ * G(I)
     .              + NORMZX * NORMZX * G(I)
c
            !   b) Derivatives with respect to plastic strain P
            !   ------------------------------------------------
            HARDP(I) = (YOUNG(I)*TANG(I)/(YOUNG(I)-TANG(I)))
            IF (Ivisc == 1) THEN
              HARDP(I) = HARDP(I) + DYLD0DEPSD(I)*DTINV +
     .                   ((YOUNG(I)*DTANGDEPSD(I)*(YOUNG(I) - TANG(I))
     .                   + YOUNG(I)*TANG(I)*DTANGDEPSD(I))/
     .                   ((YOUNG(I) - TANG(I))**2))*DTINV*PLA(I)
            ENDIF
c
            ! 3 - Computation of plastic multiplier and variables update
            !----------------------------------------------------------
c
            ! Derivative of PHI with respect to DLAM
            DPHI_DLAM(I) = - DFDSIG2 - HARDP(I)
            DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20),DPHI_DLAM(I))
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
            SIGNXX(I) = SIGNXX(I) - DPXX(I)*G2(I)
            SIGNYY(I) = SIGNYY(I) - DPYY(I)*G2(I)
            SIGNZZ(I) = SIGNZZ(I) - DPZZ(I)*G2(I)
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G(I)
            SIGNYZ(I) = SIGNYZ(I) - DPYZ(I)*G(I)
            SIGNZX(I) = SIGNZX(I) - DPZX(I)*G(I)
c
            ! Computation of the trace of the new stress tensor
            TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
            ! Computation of the new deviatoric stress tensor
            SXX(I)    = SIGNXX(I) - TRSIG(I) * THIRD
            SYY(I)    = SIGNYY(I) - TRSIG(I) * THIRD
            SZZ(I)    = SIGNZZ(I) - TRSIG(I) * THIRD
            SXY(I)    = SIGNXY(I)
            SYZ(I)    = SIGNYZ(I)
            SZX(I)    = SIGNZX(I)
c
            ! Cumulated plastic strain and strain rate update
            DPLA(I) = MAX(ZERO,DPLA(I) + DLAM)
            PLA(I)  = MAX(ZERO,PLA(I) + DLAM)
            IF (Ivisc == 1) THEN
              EPSD(I) = DPLA(I)*DTINV
            ENDIF
c
            ! Von Mises equivalent stress update
            SIGVM(I)  = THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SXY(I)**2
     .                  + THREE*SYZ(I)**2 + THREE*SZX(I)**2
            SIGVM(I)  = SQRT(SIGVM(I))
c
            IF (Ivisc == 0) THEN
              ! Yield stress update
              YLD(I) = YLD0(I) + (YOUNG(I)*TANG(I)/(YOUNG(I)-TANG(I)))*PLA(I)
              ! Yield function value update
              PHI(I) = SIGVM(I) - YLD(I)
            ENDIF
c
          ENDDO
          ! End of the loop over yielding elements
c
          ! Update variable for full viscoplastic formulation
          IF (Ivisc == 1) THEN
            ! Compute the initial yield stress
            IPOS(1:NEL)     = 1
            IAD (1:NEL)     = NPF(IFUNC(1)) / 2 + 1
            ILEN(1:NEL)     = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
            CALL VINTER2(TF,IAD,IPOS,ILEN,NEL,EPSD/Xscale_SIG0,DYLD0DEPSD,YLD0)
            YLD0(1:NEL)       = Yscale_SIG0*YLD0(1:NEL)
            DYLD0DEPSD(1:NEL) = Yscale_SIG0*DYLD0DEPSD(1:NEL)
            ! Compute the Tangent modulus
            IF (IFUNC(3) > 0) THEN
              IPOS(1:NEL) = 1
              IAD (1:NEL) = NPF(IFUNC(3)) / 2 + 1
              ILEN(1:NEL) = NPF(IFUNC(3)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
              CALL VINTER2(TF,IAD,IPOS,ILEN,NEL,EPSD/Xscale_TANG,DTANGDEPSD,TANG)
              TANG(1:NEL) = Yscale_TANG*TANG(1:NEL)
              DTANGDEPSD(1:NEL) = Yscale_TANG*DTANGDEPSD(1:NEL)
            ENDIF
            ! Updating values
            DO II=1,NINDX
              I = INDEX(II)
              ! Check tangent modulus value
              IF (TANG(I) >= 0.99D0*YOUNG(I)) THEN
                TANG(I) = 0.99D0*YOUNG(I)
                DTANGDEPSD(I) = ZERO
              ENDIF
              ! Yield stress update
              YLD(I) = YLD0(I) + (YOUNG(I)*TANG(I)/(YOUNG(I)-TANG(I)))*PLA(I)
              ! Yield function value update
              PHI(I) = SIGVM(I) - YLD(I)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      ! End of the loop over the iterations
      !=========================================================================
      ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE (NEWTON-ITERATION) METHOD
      !=========================================================================
c
      ! Storing new values
      DO I=1,NEL
        ! USR Outputs
        SEQ(I) = SIGVM(I) ! SIGEQ
        ! Coefficient for hourglass
        ET(I) = HARDP(I) / (HARDP(I) + YOUNG(I))
        ! Computation of the sound speed
        SOUNDSP(I) = SQRT((BULK(I) + FOUR_OVER_3*G(I))/RHO(I))
        ! Storing the yield stress
        SIGY(I) = YLD(I)
      ENDDO
c
      END
