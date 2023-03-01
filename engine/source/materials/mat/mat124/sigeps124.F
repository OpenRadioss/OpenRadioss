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
Chd|  SIGEPS124                     source/materials/mat/mat124/sigeps124.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
      SUBROUTINE SIGEPS124(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,TIMESTEP,TIME    ,
     2     UPARAM  ,UVAR    ,RHO     ,PLA     ,DPLA    ,UELR    ,
     3     SOUNDSP ,EPSD    ,OFF     ,LOFF    ,IPG     ,NPG     ,
     4     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     EPSXX   ,EPSYY   ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX   ,
     6     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SIGY    ,ET      ,DPLANL  ,INLOC   ,DMG     ,LE      )     
      !=======================================================================
      !      Implicit types
      !=======================================================================
#include      "implicit_f.inc"
      !=======================================================================
      !      Common
      !=======================================================================
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "mvsiz_p.inc"
      !=======================================================================
      !      Dummy arguments
      !=======================================================================
      INTEGER NEL,NUPARAM,NUVAR,INLOC,IPG,NPG,
     .        NGL(NEL)
      my_real 
     .   TIME,TIMESTEP,UPARAM(NUPARAM)
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO,LE,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSXX ,EPSYY ,EPSZZ ,EPSXY ,EPSYZ ,EPSZX 
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NEL) ::
     .   SIGY,ET
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,EPSD,DPLANL,LOFF,OFF,DMG,UELR
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,J,II,ITER,NITER,NINDX,INDX(NEL),NINDX2,INDX2(NEL),
     .        IRATE,DTYPE,DFLAG,IREG,INDX3(NEL),NINDX3,FAILIP
      my_real 
     .   YOUNG,BULK,G,NU,LAM,G2,AFILTR,AH,BH,CH,DH,HP,AS,QH0,
     .   M0,ECC,DF,BS,EPSI,EPST0,EPSTMAX,DELTAS,BETAS,EPSC0,
     .   EPSCMAX,ALPHAS,GAMMAS
      my_real
     .   DLAM,AG,BG,DFP_DKAP,DFP_DQH1,DFP_DQH2,DFP_DRHOB,DFP_DSIGM,
     .   NORMXX,NORMYY,NORMZZ,NORMXY,NORMYZ,NORMZX,DENOM,DFPDSIG2,
     .   DFP_DTHETA,DGP_DRHOB,DGP_DSIGM,DJ3DSXX,DJ3DSXY,DJ3DSYY,
     .   DJ3DSYZ,DJ3DSZX,DJ3DSZZ,DKAP_DLAM,DMG_DSIGM,DQH1_DKAP,
     .   DQH2_DKAP,DRCOS_DTHETA,DTHETA_DJ2,DTHETA_DJ3,EH,FH,
     .   NORMDP,NORMGP,NORMPXX,NORMPXY,NORMPYY,NORMPYZ,NORMPZX,
     .   NORMPZZ,RH,TRDEP,TRDGPDS,XSIH,NORMFP,LDAV,DFAPEX_DKAP,DFP,
     .   RS,U,UPRIM,V,VPRIM,DFP_DTH,DR_DCOSTH,DTH_DJ2,DTH_DJ3,
     .   WF,WF1,EFC,BETAC,SIGTENS(MVSIZ,6),EVV(MVSIZ,3),DIRPRV(MVSIZ,3,3),
     .   SIGPT(NEL,3),SIGPC(NEL,3)
      my_real, DIMENSION(NEL) ::
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,J2,J3,THETA,QH1,QH2,HARDP,FP,TRSIG,
     .   DPXX,DPYY,DPXY,DPZZ,DPYZ,DPZX,RCOS,KAP,SIGM,RHOB,DFP_DLAM,
     .   COSTH,SINTH,COS3TH,AL,BL,CL,PLAXX,PLAYY,PLAZZ,PLAXY,PLAYZ,PLAZX,
     .   APEX,FAPEX,EPS_EQ,KAPDT,KAPDT1,KAPDT2,FST,OMEGAT,EPS_INEL,H,FC,FT,
     .   ALPHART,ALPHARC,ARATE,EPS0,OMEGAC,KAPDC,KAPDC1,XSIS,NORMSIGP,
     .   KAPDC2,FSC,ALPHAC,SIGTXX,SIGTYY,SIGTZZ,SIGTXY,SIGTYZ,SIGTZX,
     .   SIGCXX,SIGCYY,SIGCZZ,SIGCXY,SIGCYZ,SIGCZX,ELXX,ELYY,ELZZ,ELXY,
     .   ELYZ,ELZX,SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX,FT1
      !=======================================================================
c  
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      !  -> Elastic parameters   
      YOUNG      = UPARAM(1)        ! Young modulus
      NU         = UPARAM(2)        ! Poisson ratio
      G          = UPARAM(3)        ! Shear modulus 
      G2         = UPARAM(4)        ! 2*Shear modulus
      LAM        = UPARAM(5)        ! Lame coefficient
      BULK       = UPARAM(6)        ! Bulk modulus 
      !  -> Plastic parameters
      FT(1:NEL)  = UPARAM(7)        ! Uniaxial tensile strength
      FC(1:NEL)  = UPARAM(8)        ! Uniaxial compressive strength
      ECC        = UPARAM(9)        ! Eccentricity
      M0         = UPARAM(10)       ! Friction parameter
      QH0        = UPARAM(11)       ! Initial hardening
      HP         = UPARAM(12)       ! Hardening modulus
      AH         = UPARAM(13)       ! Hardening ductility parameter 1
      BH         = UPARAM(14)       ! Hardening ductility parameter 2
      CH         = UPARAM(15)       ! Hardening ductility parameter 3
      DH         = UPARAM(16)       ! Hardening ductility parameter 4
      !  -> Damage parameters
      AS         = UPARAM(17)       ! Damage ductility measure
      BS         = UPARAM(18)       ! Damage ductility parameter
      DF         = UPARAM(19)       ! Dilation parameter
      DFLAG      = NINT(UPARAM(20)) ! Damage flag 
      DTYPE      = NINT(UPARAM(21)) ! Tensile damage type
      IREG       = NINT(UPARAM(22)) ! Regularization flag
      WF         = UPARAM(23)       ! First displacement threshold
      WF1        = UPARAM(24)       ! Second displacement threshold
      FT1(1:NEL) = UPARAM(25)       ! Second uniaxial tensile strength
      EFC        = UPARAM(26)       ! Compressive inelastic strain threshold
      !  -> Strain rate effect parameters
      IRATE      = NINT(UPARAM(27)) ! Strain rate effect flag
      EPST0      = UPARAM(28)       ! Reference tensile strain rate
      EPSTMAX    = UPARAM(29)       ! Maximum tensile strain rate threshold
      DELTAS     = UPARAM(30)       ! Tensile strain rate effect parameter 1
      BETAS      = UPARAM(31)       ! Tensile strain rate effect parameter 2
      EPSC0      = UPARAM(32)       ! Reference tensile strain rate
      EPSCMAX    = UPARAM(33)       ! Maximum compressive strain rate threshold
      ALPHAS     = UPARAM(34)       ! Compressive strain rate effect parameter 1
      GAMMAS     = UPARAM(35)       ! Compressive strain rate effect parameter 2           
      !  -> Flag for element deletion
      FAILIP     = NINT(UPARAM(36)) ! Number of failed integration points
      FAILIP     = MIN(FAILIP,NPG)
c
      !--------------------------------------------------------------------
      ! Recovering internal variables
      !--------------------------------------------------------------------   
      DO I=1,NEL
        ! Elastic strain tensor
        ELXX(I) = (EPSXX(I)-DEPSXX(I)) - UVAR(I,2)
        ELYY(I) = (EPSYY(I)-DEPSYY(I)) - UVAR(I,3)
        ELZZ(I) = (EPSZZ(I)-DEPSZZ(I)) - UVAR(I,4)
        ELXY(I) = (EPSXY(I)-DEPSXY(I)) - UVAR(I,5)
        ELYZ(I) = (EPSYZ(I)-DEPSYZ(I)) - UVAR(I,6)
        ELZX(I) = (EPSZX(I)-DEPSZX(I)) - UVAR(I,7)
        LDAV    = (ELXX(I) + ELYY(I) + ELZZ(I)) * LAM
        ! Old undamaged stress tensor
        SIGOXX(I) = ELXX(I)*G2 + LDAV
        SIGOYY(I) = ELYY(I)*G2 + LDAV
        SIGOZZ(I) = ELZZ(I)*G2 + LDAV
        SIGOXY(I) = ELXY(I)*G
        SIGOYZ(I) = ELYZ(I)*G
        SIGOZX(I) = ELZX(I)*G
        ! User variables
        KAP(I)    = UVAR(I,1)  ! Kappa hardening parameter
        PLAXX(I)  = UVAR(I,2)  ! Plastic strain tensor component X
        PLAYY(I)  = UVAR(I,3)  ! Plastic strain tensor component Y
        PLAZZ(I)  = UVAR(I,4)  ! Plastic strain tensor component Z
        PLAXY(I)  = UVAR(I,5)  ! 2*Plastic strain tensor component XY
        PLAYZ(I)  = UVAR(I,6)  ! 2*Plastic strain tensor component YZ
        PLAZX(I)  = UVAR(I,7)  ! 2*Plastic strain tensor component ZX
        KAPDT(I)  = UVAR(I,9)  ! Tensile damage strain threshold
        KAPDT1(I) = UVAR(I,10) ! First tensile damage variable
        KAPDT2(I) = UVAR(I,11) ! Second tensile damage variable
        OMEGAT(I) = UVAR(I,12) ! Tensile damage
        KAPDC(I)  = UVAR(I,13) ! Compressive damage strain threshold
        KAPDC1(I) = UVAR(I,14) ! First compressive damage variable
        KAPDC2(I) = UVAR(I,15) ! Second compressive damage variable
        OMEGAC(I) = UVAR(I,16) ! Compressive damage
        ARATE(I)  = UVAR(I,17) ! Rate dependency factor
        ! Standard inputs
        DPLA(I)   = ZERO ! Initialization of the plastic strain increment
        ET(I)     = ONE  ! Initialization of hourglass coefficient
        HARDP(I)  = ZERO ! Initialization of hardening rate
        DPXX(I)   = ZERO ! Initialization of the XX plastic strain increment
        DPYY(I)   = ZERO ! Initialization of the YY plastic strain increment
        DPZZ(I)   = ZERO ! Initialization of the ZZ plastic strain increment
        DPXY(I)   = ZERO ! Initialization of the XY plastic strain increment
        DPYZ(I)   = ZERO ! Initialization of the YZ plastic strain increment
        DPZX(I)   = ZERO ! Initialization of the ZX plastic strain increment
        ! Regularization factor
        IF (IREG > 0) THEN 
          IF (UVAR(I,18) == ZERO) UVAR(I,18) = LE(I)
          H(I) = UVAR(I,18)
        ELSE
          H(I) = ONE
        ENDIF
      ENDDO       
c      
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES + YIELD FONCTION
      !========================================================================     
      ! Computation of the trial stress tensor 
      DO I=1,NEL
        LDAV      = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAM
        SIGNXX(I) = SIGOXX(I) + DEPSXX(I)*G2 + LDAV
        SIGNYY(I) = SIGOYY(I) + DEPSYY(I)*G2 + LDAV
        SIGNZZ(I) = SIGOZZ(I) + DEPSZZ(I)*G2 + LDAV
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*G
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*G
      ENDDO
c
      ! Strain rate effects
      IF (IRATE > 0) THEN 
        ! Computation of principal stresses and vectors
        DO I = 1,NEL
          SIGTENS(I,1) = SIGNXX(I)
          SIGTENS(I,2) = SIGNYY(I)
          SIGTENS(I,3) = SIGNZZ(I)
          SIGTENS(I,4) = SIGNXY(I)
          SIGTENS(I,5) = SIGNYZ(I)
          SIGTENS(I,6) = SIGNZX(I)
        ENDDO
        EVV(1:MVSIZ,1:3) = ZERO
        DIRPRV(1:MVSIZ,1:3,1:3) = ZERO
        SIGPT(1:NEL,1:3) = ZERO
        SIGPC(1:NEL,1:3) = ZERO
        IF (IRESP == 1) THEN
          CALL VALPVECDP_V(SIGTENS,EVV,DIRPRV,NEL)
        ELSE
          CALL VALPVEC_V(SIGTENS,EVV,DIRPRV,NEL)
        ENDIF
c
        ! Computation of ALPHAC compression factor
        DO I = 1,NEL 
          NORMSIGP(I) = EVV(I,1)**2 + EVV(I,2)**2 + EVV(I,3)**2
        ENDDO
        ALPHAC(1:NEL) = ZERO
        DO J = 1,3
          DO I = 1,NEL  
            IF (EVV(I,J) > ZERO) THEN 
              SIGPT(I,J) = EVV(I,J)
            ELSE
              SIGPC(I,J) = EVV(I,J)
            ENDIF
            ALPHAC(I) = ALPHAC(I) + SIGPC(I,J)*(SIGPT(I,J) + SIGPC(I,J))/MAX(NORMSIGP(I),EM20)
          ENDDO
        ENDDO
c 
        ! Computation of strain rate factor
        DO I = 1,NEL
          ! Tension strain rate factor
          IF (EPSD(I) <= EPST0) THEN  
            ALPHART(I) = ONE
          ELSEIF (EPSD(I) > EPST0 .AND. EPSD(I) <= EPSTMAX) THEN 
            ALPHART(I) = (EPSD(I)/EPST0)**(DELTAS)
          ELSE 
            ALPHART(I) = BETAS*(EPSD(I)/EPST0)**THIRD
          ENDIF
c
          ! Compression strain rate factor
          IF (EPSD(I) <= EPSC0) THEN
            ALPHARC(I) = ONE
          ELSEIF (EPSD(I) > EPSC0 .AND. EPSD(I) <= EPSCMAX) THEN 
            ALPHARC(I) = (EPSD(I)/EPSC0)**(1.026D0*ALPHAS)
          ELSE
            ALPHARC(I) = GAMMAS*(EPSD(I)/EPSC0)**THIRD
          ENDIF
c
          ! Rate factor
          IF (ARATE(I) == ZERO) ARATE(I) = (ONE - ALPHAC(I))*ALPHART(I) + ALPHAC(I)*ALPHARC(I)
c
          ! Compute strain rate dependent parameters
          FT(I)  = FT(I)*ARATE(I)
          FT1(I) = FT1(I)*ARATE(I)
          FC(I)  = FC(I)*ARATE(I)
        ENDDO
      ENDIF
c
      ! Computation of the yield functions
      DO I = 1,NEL 
        ! Computation of the trace of stress tensor and hydrostatic stress
        TRSIG(I) = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
        SIGM(I)  = TRSIG(I)*THIRD
        ! Computation of the deviatoric trial stress tensor
        SXX(I) = SIGNXX(I) - SIGM(I)
        SYY(I) = SIGNYY(I) - SIGM(I)
        SZZ(I) = SIGNZZ(I) - SIGM(I)
        SXY(I) = SIGNXY(I)
        SYZ(I) = SIGNYZ(I)
        SZX(I) = SIGNZX(I)
        ! Second deviatoric invariant
        J2(I)  = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) 
     .               + SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        J2(I)  = MAX(J2(I),EM20)
        ! Radius, Third deviatoric invariant and Lode angle
        RHOB(I) = SQRT(TWO*J2(I))
        J3(I)   = SXX(I)*SYY(I)*SZZ(I) + TWO*SXY(I)*SZX(I)*SYZ(I) - 
     .            SXX(I)*(SYZ(I)**2) - SZZ(I)*(SXY(I)**2) - SYY(I)*(SZX(I)**2)
        COS3TH(I) = (THREE*SQR3/TWO)*J3(I)/((J2(I)**(THREE/TWO)))
        IF (COS3TH(I) > ONE) THEN
          COS3TH(I) = ONE
        ELSE IF (COS3TH(I) < -ONE) THEN
          COS3TH(I) = -ONE
        ENDIF
        THETA(I) = THIRD*ACOS(COS3TH(I))
        ! Save cosinus and sinus values to keep good performances
        COSTH(I) = COS(THETA(I))
        SINTH(I) = SIN(THETA(I))
c
        ! Compute the R function value for Lode Angle
        RCOS(I) = (FOUR*(ONE - ECC**2)*(COSTH(I)**2) + (TWO*ECC - ONE)**2)/          
     .            MAX(((TWO*(ONE - ECC**2)*COSTH(I)) +
     .             (TWO*ECC  - ONE)*SQRT(FOUR*(ONE - ECC**2)*(COSTH(I)**2) + 
     .             FIVE*(ECC**2) - FOUR*ECC)),EM20) 
c
        ! Compute hardening variables QH1 and QH2
        IF (KAP(I) < ZERO) THEN
          QH1(I) = QH0
          QH2(I) = ONE
        ELSEIF (KAP(I) >= ZERO .AND. KAP(I) < ONE) THEN
          QH1(I) = QH0 + 
     .             (ONE - QH0)*((KAP(I)**3) - THREE*(KAP(I)**2) + THREE*KAP(I)) -
     .                      HP*((KAP(I)**3) - THREE*(KAP(I)**2) +   TWO*KAP(I))
          QH2(I) = ONE
        ELSE 
          QH1(I) = ONE
          QH2(I) = ONE + (KAP(I)-ONE)*HP
        ENDIF
c
        ! Apex stress
        APEX(I) = QH2(I)*FC(I)/M0
c
        ! Compute the regular yield function value
        CL(I) = (RHOB(I)*RCOS(I)/(SQR6*FC(I))) + SIGM(I)/FC(I)
        BL(I) = (SIGM(I)/FC(I)) + RHOB(I)/(SQR6*FC(I))
        AL(I) = (ONE - QH1(I))*(BL(I)**2) + SQRT(THREE/TWO)*(RHOB(I)/FC(I))
        FP(I) = AL(I)**2 + M0*(QH1(I)**2)*QH2(I)*CL(I) - (QH1(I)**2)*(QH2(I)**2) 
c
        ! Compute the apex yield function value
        FAPEX(I) = SIGM(I) - APEX(I)
      ENDDO  
c     
      ! Checking plastic behavior for all elements
      NINDX = 0
      INDX(1:NEL)  = 0
      NINDX2 = 0
      INDX2(1:NEL) = 0
      DO I=1,NEL         
        ! Apex return mapping
        IF ((FAPEX(I) > ZERO) .AND. (OFF(I) == ONE)) THEN
          NINDX = NINDX+1
          INDX(NINDX) = I
        ! Regular return mapping
        ELSEIF (FP(I) > ZERO  .AND. OFF(I) == ONE)   THEN
          NINDX2 = NINDX2+1
          INDX2(NINDX2) = I
        ENDIF
      ENDDO
c
      !====================================================================
      ! - APEX RETURN MAPPING WITH CUTTING PLANE (NEWTON-ITERATION) METHOD
      !==================================================================== 
      IF (NINDX > 0) THEN   
c      
        ! Number of Newton iterations
        NITER = 6
c     
        ! Loop over the iterations   
        DO ITER = 1, NITER
#include "vectorize.inc" 
          ! Loop over yielding elements
          DO II=1,NINDX 
            I = INDX(II)
            ! -> Derivative of QH2 with respect to KAP
            IF (KAP(I) < ONE) THEN
              DQH2_DKAP = ZERO
            ELSE 
              DQH2_DKAP = HP
            ENDIF
            HARDP(I) = DQH2_DKAP
            ! Compute the ductility measure
            RH = -(SIGM(I)/FC(I)) - THIRD
            IF (RH >= ZERO) THEN 
              XSIH = AH-(AH-BH)*EXP(-RH/CH)
            ELSE
              EH = BH - DH
              FH = (BH-DH)*CH/(AH-BH)
              XSIH = EH*EXP(RH/FH) + DH 
            ENDIF
            ! Derivative of FAPEX with respect to kappa
            DFAPEX_DKAP = DQH2_DKAP*(FC(I)/M0) + THREE*BULK*XSIH
            ! Update Kappa parameter
            KAP(I)  = KAP(I) + FAPEX(I)/DFAPEX_DKAP
            ! Update hydrostatic stress
            SIGM(I) = SIGM(I) - THREE*BULK*XSIH*FAPEX(I)/DFAPEX_DKAP
            ! Update APEX stress
            IF (KAP(I) < ONE) THEN
              QH2(I) = ONE
            ELSE 
              QH2(I) = ONE + (KAP(I)-ONE)*HP
            ENDIF
            APEX(I) = QH2(I)*FC(I)/M0
            ! Update the APEX yield function
            FAPEX(I) = SIGM(I) - APEX(I)
          ENDDO
        ENDDO
c
        ! Update the plastic strain and the stress tensors
#include "vectorize.inc" 
        DO II=1,NINDX 
          I = INDX(II)
c
          ! Plastic strain
          PLAXX(I) = PLAXX(I) + (ONE/YOUNG)*(
     &                                       (SIGNXX(I)-SIGM(I)) 
     &                                  - NU*(SIGNYY(I)-SIGM(I)) 
     &                                  - NU*(SIGNZZ(I)-SIGM(I))) 
          PLAYY(I) = PLAYY(I) + (ONE/YOUNG)*(
     &                                  - NU*(SIGNXX(I)-SIGM(I)) 
     &                                  +    (SIGNYY(I)-SIGM(I)) 
     &                                  - NU*(SIGNZZ(I)-SIGM(I))) 
          PLAZZ(I) = PLAZZ(I) + (ONE/YOUNG)*(
     &                                  - NU*(SIGNXX(I)-SIGM(I)) 
     &                                  - NU*(SIGNYY(I)-SIGM(I)) 
     &                                  +    (SIGNZZ(I)-SIGM(I)))
          PLAXY(I) = PLAXY(I) + (ONE/G)*(SIGNXY(I))
          PLAYZ(I) = PLAYZ(I) + (ONE/G)*(SIGNYZ(I))
          PLAZX(I) = PLAZX(I) + (ONE/G)*(SIGNZX(I))
c
          ! Stress tensor
          SIGNXX(I) = SIGM(I)
          SIGNYY(I) = SIGM(I)
          SIGNZZ(I) = SIGM(I)
          SIGNXY(I) = ZERO
          SIGNYZ(I) = ZERO
          SIGNZX(I) = ZERO
          J2(I)     = EM20
          RHOB(I)   = SQRT(TWO*J2(I))
          CL(I)     = SIGM(I)/FC(I)
        ENDDO 
c
      ENDIF
c      
      !====================================================================
      ! - REGULAR RETURN MAPPING WITH CUTTING PLANE (NEWTON-ITERATION) METHOD
      !==================================================================== 
      IF (NINDX2 > 0) THEN   
c      
        ! Number of Newton iterations
        NITER = 6
c     
        ! Loop over the iterations   
        DO ITER = 1, NITER
#include "vectorize.inc" 
          ! Loop over yielding elements
          DO II=1,NINDX2 
            I = INDX2(II)
c        
            ! Note     : in this part, the purpose is to compute for each iteration
            ! a plastic multiplier allowing to update internal variables to satisfy
            ! the consistency condition using the cutting plane semi-implicit 
            ! method
            ! Its expression at each iteration is : DLAMBDA = - FP/DFP_DLAMBDA
            ! -> FP          : current value of yield function (known)
            ! -> DFP_DLAMBDA : derivative of FP with respect to DLAMBDA by taking
            !                  into account of internal variables kinetic : 
            !                  hardening parameter ...
c        
            ! 1 a) - Computation of DFP_DSIG the normal to the yield surface
            !-------------------------------------------------------------
            !  -> Computation of derivative of FP with respect to RHOB
            DFP_DRHOB = (TWO*AL(I)/FC(I))*(TWO*((ONE-QH1(I))/SQR6)*BL(I) + SQRT(THREE/TWO)) + 
     .                M0*(QH1(I)**2)*QH2(I)*RCOS(I)/(SQR6*FC(I))
            !  -> Computation of derivative of FP with respect to SIGM
            DFP_DSIGM = (FOUR*AL(I)*BL(I)*(ONE-QH1(I))/FC(I)) + M0*(QH1(I)**2)*QH2(I)/FC(I)
            !  -> Computation of derivative of FP with respect to COSTH
            U     = FOUR*(ONE - ECC**2)*(COSTH(I)**2) + (TWO*ECC - ONE)**2
            UPRIM = EIGHT*(ONE - ECC**2)*COSTH(I)
            V     = TWO*(ONE - ECC**2)*COSTH(I) + 
     .                         (TWO*ECC - ONE)*SQRT(FOUR*(ONE-ECC**2)*(COSTH(I)**2)
     .                         + FIVE*(ECC**2) - FOUR*ECC)
            VPRIM = TWO*(ONE-ECC**2) + (TWO*ECC - ONE)*((EIGHT*(ONE-ECC**2)*COSTH(I))/
     .                         (TWO*SQRT(FOUR*(ONE-ECC**2)*(COSTH(I)**2) 
     .                         + FIVE*(ECC**2) - FOUR*ECC)))
            DR_DCOSTH = (UPRIM*V - U*VPRIM)/(V**2)
            DFP_DTH = M0*(QH1(I)**2)*(QH2(I))*(RHOB(I)/(SQR6*FC(I)))*DR_DCOSTH*(-SINTH(I))
            DTH_DJ2 = THREE*SQR3*J3(I)/(FOUR*(J2(I)**2)*SQRT(J2(I))*MAX(SQRT(ONE - (COS3TH(I))**2),EM20))
            DTH_DJ3 = -SQR3/(TWO*J2(I)*SQRT(J2(I))*MAX(SQRT(ONE - (COS3TH(I))**2),EM20))
c
            ! Derivative of third deviatoric invariant with respect to stresses
            DJ3DSXX =  TWO_THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)
     .              -  THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)
     .              -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)               
            DJ3DSYY = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)
     .              +  TWO_THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)
     .              -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)            
            DJ3DSZZ = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)
     .              - THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)
     .              + TWO_THIRD*(SXX(I)*SYY(I)-SXY(I)**2) 
            DJ3DSXY = TWO*(SXX(I)*SXY(I) + SXY(I)*SYY(I) + SZX(I)*SYZ(I))         
            DJ3DSYZ = TWO*(SXY(I)*SZX(I) + SYY(I)*SYZ(I) + SYZ(I)*SZZ(I))                     
            DJ3DSZX = TWO*(SXX(I)*SZX(I) + SXY(I)*SYZ(I) + SZX(I)*SZZ(I))
c
            ! -> Assembling derivatives
            NORMXX  = DFP_DRHOB*(SXX(I)/RHOB(I)) + DFP_DSIGM*THIRD + 
     .                  DFP_DTH*(DTH_DJ2*SXX(I) + DTH_DJ3*DJ3DSXX)
            NORMYY  = DFP_DRHOB*(SYY(I)/RHOB(I)) + DFP_DSIGM*THIRD + 
     .                  DFP_DTH*(DTH_DJ2*SYY(I) + DTH_DJ3*DJ3DSYY)
            NORMZZ  = DFP_DRHOB*(SZZ(I)/RHOB(I)) + DFP_DSIGM*THIRD + 
     .                  DFP_DTH*(DTH_DJ2*SZZ(I) + DTH_DJ3*DJ3DSZZ)
            NORMXY  = TWO*DFP_DRHOB*(SXY(I)/RHOB(I)) + 
     .                  DFP_DTH*(DTH_DJ2*TWO*SXY(I) + DTH_DJ3*DJ3DSXY)
            NORMYZ  = TWO*DFP_DRHOB*(SYZ(I)/RHOB(I)) + 
     .                  DFP_DTH*(DTH_DJ2*TWO*SYZ(I) + DTH_DJ3*DJ3DSYZ)
            NORMZX  = TWO*DFP_DRHOB*(SZX(I)/RHOB(I)) + 
     .                  DFP_DTH*(DTH_DJ2*TWO*SZX(I) + DTH_DJ3*DJ3DSZX)  
c
            ! 1 b) - Computation of DGP_DSIG the normal to the plastic potential
            !-------------------------------------------------------------
            !  -> Computation of derivative of GP with respect to RHOB
            DGP_DRHOB = (TWO*AL(I)/FC(I))*(TWO*((ONE-QH1(I))/SQR6)*BL(I) + SQRT(THREE/TWO)) + 
     .                  (QH1(I)**2)*M0/(SQR6*FC(I))
            !  -> Computation of AG and BG parameters
            AG = (THREE*FT(I)*QH2(I)/FC(I)) + M0*HALF
            BG = ((QH2(I)*THIRD)*(ONE + (FT(I)/FC(I))))/MAX((
     .            LOG(AG) - LOG(TWO*DF - ONE) - LOG(THREE*QH2(I) + M0*HALF) + 
     .            LOG(DF + ONE)),EM20)
            !  -> Computation of derivative of MG with respect to SIGM
            DMG_DSIGM = AG*EXP((SIGM(I) - QH2(I)*FT(I)*THIRD)/(BG*FC(I)))
            !  -> Computation of derivative of GP with respect to SIGM
            DGP_DSIGM = (FOUR*AL(I)*BL(I)*(ONE-QH1(I))/FC(I)) + ((QH1(I)**2)/FC(I))*DMG_DSIGM
            ! -> Assembling derivatives
            NORMPXX = DGP_DRHOB*(SXX(I)/RHOB(I)) + DGP_DSIGM*THIRD
            NORMPYY = DGP_DRHOB*(SYY(I)/RHOB(I)) + DGP_DSIGM*THIRD
            NORMPZZ = DGP_DRHOB*(SZZ(I)/RHOB(I)) + DGP_DSIGM*THIRD
            NORMPXY = TWO*DGP_DRHOB*(SXY(I)/RHOB(I)) 
            NORMPYZ = TWO*DGP_DRHOB*(SYZ(I)/RHOB(I)) 
            NORMPZX = TWO*DGP_DRHOB*(SZX(I)/RHOB(I))
c          
            ! 2 - Computation of DPHI_DLAMBDA
            !---------------------------------------------------------
c        
            !   a) Derivative with respect stress increments tensor DSIG
            !   --------------------------------------------------------
            TRDGPDS   = NORMPXX + NORMPYY + NORMPZZ  
            DFPDSIG2  = NORMXX * (NORMPXX*G2 + LAM*TRDGPDS)
     .                + NORMYY * (NORMPYY*G2 + LAM*TRDGPDS)
     .                + NORMZZ * (NORMPZZ*G2 + LAM*TRDGPDS) 
     .                + NORMXY *  NORMPXY*G
     .                + NORMYZ *  NORMPYZ*G
     .                + NORMZX *  NORMPZX*G     
c         
            !   b) Derivatives with respect to hardening Kap
            !   ------------------------------------------------
            !     -> Derivative of FP with respect to QH1
            DFP_DQH1 = -TWO*AL(I)*(BL(I)**2) + TWO*QH1(I)*QH2(I)*(M0*CL(I) - QH2(I))
            !     -> Derivative of FP with respect to QH2
            DFP_DQH2 = (QH1(I)**2)*(M0*CL(I) - TWO*QH2(I))
            !     -> Derivative of QH1 and QH2 with respect to KAP
            IF (KAP(I) < ZERO) THEN
              DQH1_DKAP = (ONE - QH0)*THREE - HP*TWO
              DQH2_DKAP = ZERO
            ELSEIF (KAP(I) >= ZERO .AND. KAP(I) < ONE) THEN
              DQH1_DKAP = (ONE - QH0)*(THREE*(KAP(I)**2) - SIX*KAP(I) + THREE) - 
     .                             HP*(THREE*(KAP(I)**2) - SIX*KAP(I) + TWO)
              DQH2_DKAP = ZERO
            ELSE 
              DQH1_DKAP = ZERO
              DQH2_DKAP = HP
            ENDIF
            !     -> Derivative of FP with respect to KAP
            DFP_DKAP = DFP_DQH1*DQH1_DKAP + DFP_DQH2*DQH2_DKAP
            DFP_DKAP = MIN(DFP_DKAP,ZERO)
            !     -> Computation of RH and XSIH parameters + Euclidian norm of derivative 
            RH = -(SIGM(I)/FC(I)) - THIRD
            IF (RH >= ZERO) THEN 
              XSIH = AH-(AH-BH)*EXP(-RH/CH)
            ELSE
              EH   = BH-DH
              FH   = (BH-DH)*CH/(AH-BH)
              XSIH = EH*EXP(RH/FH) + DH 
            ENDIF
            NORMGP = SQRT(THIRD*(DGP_DSIGM)**2 + DGP_DRHOB**2)
            !     -> Derivative of KAP with respect to DLAM
            DKAP_DLAM = (NORMGP/XSIH)*(TWO*COSTH(I))**2
c              
            ! 3 - Computation of plastic multiplier and variables update
            !----------------------------------------------------------
c          
            ! Derivative of PHI with respect to DLAM
            DFP_DLAM(I) = - DFPDSIG2 + DFP_DKAP
            DFP_DLAM(I) = SIGN(MAX(ABS(DFP_DLAM(I)),EM20),DFP_DLAM(I))      
c          
            ! Computation of the plastic multiplier
            DLAM = -FP(I)/DFP_DLAM(I)   
c          
            ! Plastic strains tensor update
            DPXX(I)  = DLAM * NORMPXX
            DPYY(I)  = DLAM * NORMPYY
            DPZZ(I)  = DLAM * NORMPZZ
            DPXY(I)  = DLAM * NORMPXY
            DPYZ(I)  = DLAM * NORMPYZ
            DPZX(I)  = DLAM * NORMPZX
            TRDEP    = DPXX(I) + DPYY(I) + DPZZ(I)  
            PLAXX(I) = PLAXX(I) + DPXX(I)
            PLAYY(I) = PLAYY(I) + DPYY(I)
            PLAZZ(I) = PLAZZ(I) + DPZZ(I)
            PLAXY(I) = PLAXY(I) + DPXY(I)
            PLAYZ(I) = PLAYZ(I) + DPYZ(I)
            PLAZX(I) = PLAZX(I) + DPZX(I)
c          
            ! Update the kap hardening parameter KAP 
            KAP(I) = KAP(I) + DKAP_DLAM*DLAM
            KAP(I) = MAX(KAP(I),ZERO)
c
            ! Update the kap hardening parameters
!            IF (KAP(I) < ZERO) THEN
!              QH1(I) = QH0
!              QH2(I) = ONE
!            ELSEIF (KAP(I) >= ZERO .AND. KAP(I) < ONE) THEN
!              QH1(I) = QH0 + 
!     .                 (ONE - QH0)*((KAP(I)**3) - THREE*(KAP(I)**2) + THREE*KAP(I)) -
!     .                          HP*((KAP(I)**3) - THREE*(KAP(I)**2) +   TWO*KAP(I))
!              QH2(I) = ONE
!            ELSE 
!              QH1(I) = ONE
!              QH2(I) = ONE + (KAP(I)-ONE)*HP
!            ENDIF 
c          
            ! Elasto-plastic stresses update   
            LDAV      = TRDEP * LAM
            SIGNXX(I) = SIGNXX(I) - (DPXX(I)*G2 + LDAV)
            SIGNYY(I) = SIGNYY(I) - (DPYY(I)*G2 + LDAV)
            SIGNZZ(I) = SIGNZZ(I) - (DPZZ(I)*G2 + LDAV)
            SIGNXY(I) = SIGNXY(I) -  DPXY(I)*G
            SIGNYZ(I) = SIGNYZ(I) -  DPYZ(I)*G
            SIGNZX(I) = SIGNZX(I) -  DPZX(I)*G    
c
            ! Computation of the trace and hydrostatic stress the new stress tensor           
            TRSIG(I)  = SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)
            SIGM(I)   = TRSIG(I)*THIRD
            ! Computation of the new deviatoric stress tensor
            SXX(I)    = SIGNXX(I) - SIGM(I)
            SYY(I)    = SIGNYY(I) - SIGM(I)
            SZZ(I)    = SIGNZZ(I) - SIGM(I)
            SXY(I)    = SIGNXY(I)
            SYZ(I)    = SIGNYZ(I)
            SZX(I)    = SIGNZX(I)
            ! Second deviatoric invariant
            J2(I)     = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) 
     .                      + SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
            J2(I)     = MAX(J2(I),EM20)
            ! Radius, Third deviatoric invariant and Lode angle
            RHOB(I) = SQRT(TWO*J2(I))
            J3(I)   = SXX(I)*SYY(I)*SZZ(I) + TWO*SXY(I)*SZX(I)*SYZ(I) - 
     .                SXX(I)*(SYZ(I)**2) - SZZ(I)*(SXY(I)**2) - SYY(I)*(SZX(I)**2)
            COS3TH(I) = (THREE*SQR3/TWO)*J3(I)/((J2(I)**(THREE/TWO)))
            IF (COS3TH(I) > ONE) THEN
              COS3TH(I) = ONE
            ELSE IF (COS3TH(I) < -ONE) THEN
              COS3TH(I) = -ONE
            ENDIF
            THETA(I) = THIRD*ACOS(COS3TH(I))
            ! Save cosinus and sinus values to keep good performances
            COSTH(I) = COS(THETA(I))
            SINTH(I) = SIN(THETA(I))
c
            ! Compute the R function value for Lode Angle
            RCOS(I) = (FOUR*(ONE - ECC**2)*(COSTH(I)**2) + (TWO*ECC - ONE)**2)/          
     .                MAX(((TWO*(ONE - ECC**2)*COSTH(I)) +
     .                (TWO*ECC - ONE)*SQRT(FOUR*(ONE - ECC**2)*(COSTH(I)**2) + 
     .                FIVE*(ECC**2) - FOUR*ECC)),EM20)
c
            ! Update the yield function value
            CL(I) = (RHOB(I)*RCOS(I)/(SQR6*FC(I))) + SIGM(I)/FC(I)
            BL(I) = (SIGM(I)/FC(I)) + RHOB(I)/(SQR6*FC(I))
            AL(I) = (ONE - QH1(I))*(BL(I)**2) + SQRT(THREE/TWO)*(RHOB(I)/FC(I))
            FP(I) = AL(I)**2 + M0*(QH1(I)**2)*QH2(I)*CL(I) - (QH1(I)**2)*(QH2(I)**2)
c
          ENDDO
          ! End of the loop over yielding elements
        ENDDO
        !End of the loop over the Newton iterations 
      ENDIF 
      !=========================================================================
      ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE (NEWTON-ITERATION) METHOD
      !=========================================================================
c 
      !====================================================================
      ! - UPDATE EQUIVALENT PLASTIC STRAIN
      !==================================================================== 
      DO I=1,NEL  
        ! Update cumulated plastic strain for damage and output
        DPLA(I) = SQRT((PLAXX(I)-UVAR(I,2))**2 + (PLAYY(I)-UVAR(I,3))**2 +
     .                 (PLAZZ(I)-UVAR(I,4))**2 + HALF*((PLAXY(I)-UVAR(I,5))**2 +
     .                 (PLAYZ(I)-UVAR(I,6))**2 + (PLAZX(I)-UVAR(I,7))**2))
        PLA(I)  = PLA(I) + DPLA(I)
        ! Hardening rate
        IF (DPLA(I) > ZERO) THEN 
          HARDP(I) = SQRT((SIGNXX(I)-SIGOXX(I))**2 + 
     .                    (SIGNYY(I)-SIGOYY(I))**2 +
     .                    (SIGNZZ(I)-SIGOZZ(I))**2 +
     .                TWO*((SIGNXY(I)-SIGOXY(I))**2 +
     .                    (SIGNYZ(I)-SIGOYZ(I))**2 +
     .                    (SIGNZX(I)-SIGOZX(I))**2))
          HARDP(I) = HARDP(I)/DPLA(I)
        ELSE
          HARDP(I) = ZERO
        ENDIF
      ENDDO
c
      !====================================================================
      ! - COMPUTE DAMAGE VARIABLES EVOLUTION
      !==================================================================== 
      NINDX3 = 0
      INDX3(1:NEL) = 0
      IF (DFLAG < 4) THEN 
c
        ! Compute eigen value and principal stress tensor
        DO I = 1,NEL
          SIGTENS(I,1) = SIGNXX(I)
          SIGTENS(I,2) = SIGNYY(I)
          SIGTENS(I,3) = SIGNZZ(I)
          SIGTENS(I,4) = SIGNXY(I)
          SIGTENS(I,5) = SIGNYZ(I)
          SIGTENS(I,6) = SIGNZX(I)
        ENDDO
        EVV(1:MVSIZ,1:3) = ZERO
        DIRPRV(1:MVSIZ,1:3,1:3) = ZERO
        SIGPT(1:NEL,1:3) = ZERO
        SIGPC(1:NEL,1:3) = ZERO
        IF (IRESP == 1) THEN
          CALL VALPVECDP_V(SIGTENS,EVV,DIRPRV,NEL)
        ELSE
          CALL VALPVEC_V(SIGTENS,EVV,DIRPRV,NEL)
        ENDIF
c 
        ! Loop over elements
        DO I = 1,NEL
c
          ! Equivalent strain damage threshold
          EPS0(I) = FT(I)/YOUNG
c
          ! Compute equivalent strain
          EPS_EQ(I) = EPS0(I)*M0*HALF*CL(I)
          EPS_EQ(I) = EPS_EQ(I) + SQRT((EPS_EQ(I))**2 + 
     .                            THREE*HALF*(EPS0(I)*RHOB(I)/FC(I))**2)
c
          ! Compute ductility measure 
          IF (SIGM(I) <= ZERO) THEN 
            RS = -SQR6*SIGM(I)/MAX(RHOB(I),EM20)
          ELSE
            RS = ZERO
          ENDIF
          XSIS(I) = ONE + (AS - ONE)*(RS**BS)     
c
          ! Computation of norm of principal stress tensor
          NORMSIGP(I) = EVV(I,1)**2 + EVV(I,2)**2 + EVV(I,3)**2
        ENDDO
c
        ! Computation of ALPHAC compression factor
        ALPHAC(1:NEL) = ZERO
        DO J = 1,3
          DO I = 1,NEL 
            IF (EVV(I,J) > ZERO) THEN 
              SIGPT(I,J) = EVV(I,J)
            ELSE
              SIGPC(I,J) = EVV(I,J)
            ENDIF
            ALPHAC(I) = ALPHAC(I) + SIGPC(I,J)*(SIGPT(I,J) + SIGPC(I,J))/MAX(NORMSIGP(I),EM20)
          ENDDO
        ENDDO
c
        ! Computation of damage variables
        DO I = 1,NEL
c
          ! Damage limit is reached
          IF (EPS_EQ(I)>=(EPS0(I)-EM08)) THEN 
            ! Compute tension damage
            KAPDT2(I) = KAPDT2(I) + (MAX(EPS_EQ(I) - KAPDT(I),ZERO))/XSIS(I)
            IF (INLOC > 0) THEN 
              KAPDT1(I) = KAPDT1(I) + MAX(DPLANL(I),ZERO)/XSIS(I)
            ELSE
              KAPDT1(I) = KAPDT1(I) + DPLA(I)/XSIS(I)
            ENDIF
            KAPDT(I) = MAX(EPS_EQ(I),KAPDT(I))
            EPS_INEL(I) = KAPDT1(I) + OMEGAT(I)*KAPDT2(I)
            ! -> Linear type
            IF (DTYPE == 1) THEN 
              OMEGAT(I) = (YOUNG*KAPDT(I)*WF - FT(I)*WF + 
     .               FT(I)*KAPDT1(I)*H(I))/(YOUNG*KAPDT(I)*WF - FT(I)*H(I)*KAPDT2(I))
            ! -> Bilinear type
            ELSEIF (DTYPE == 2) THEN 
              IF (H(I)*EPS_INEL(I) >= ZERO .AND. H(I)*EPS_INEL(I) < WF1) THEN 
                OMEGAT(I) = (YOUNG*KAPDT(I)*WF1 - FT(I)*WF1 - (FT1(I) - 
     .                 FT(I))*KAPDT1(I)*H(I))/(YOUNG*KAPDT(I)*WF1 + 
     .                 (FT1(I) - FT(I))*H(I)*KAPDT2(I))
              ELSEIF (H(I)*EPS_INEL(I) >= WF1 .AND. H(I)*EPS_INEL(I) < WF) THEN
                OMEGAT(I) = (YOUNG*KAPDT(I)*(WF - WF1) - 
     .            FT1(I)*(WF - WF1) + FT1(I)*H(I)*KAPDT1(I) - FT1(I)*WF1)/
     .            (YOUNG*KAPDT(I)*(WF - WF1) - FT1(I)*H(I)*KAPDT2(I))
              ELSE
                OMEGAT(I) = ONE
              ENDIF
            ! -> Exponential type
            ELSEIF (DTYPE == 3) THEN 
              OMEGAT(I) = ONE - EXP(-(EPS_INEL(I)/WF))
              IF (OMEGAT(I) > 0.999D0) OMEGAT(I) = ONE
            ENDIF
            OMEGAT(I) = MAX(OMEGAT(I),UVAR(I,12))
            OMEGAT(I) = MIN(MAX(OMEGAT(I),ZERO),ONE)
            ! Compute compression damage
            KAPDC2(I) = KAPDC2(I) + ALPHAC(I)*(MAX(EPS_EQ(I) - KAPDC(I),ZERO))/XSIS(I)
            BETAC = FT(I)*QH2(I)*SQRT(TWO_THIRD)/
     .              (RHOB(I)*SQRT(ONE + TWO*(DF**2)))
            IF (INLOC > 0) THEN
              KAPDC1(I) = KAPDC1(I) + ALPHAC(I)*BETAC*MAX(DPLANL(I),ZERO)/XSIS(I)
            ELSE
              KAPDC1(I) = KAPDC1(I) + ALPHAC(I)*BETAC*DPLA(I)/XSIS(I)
            ENDIF 
            KAPDC(I)  = MAX(ALPHAC(I)*EPS_EQ(I),KAPDC(I))
            EPS_INEL(I) = KAPDC1(I) + OMEGAC(I)*KAPDC2(I)
            OMEGAC(I) = ONE - EXP(-(EPS_INEL(I)/EFC))
            IF (OMEGAC(I) > 0.999D0) OMEGAC(I) = ONE
            OMEGAC(I) = MAX(OMEGAC(I),UVAR(I,16))
            OMEGAC(I) = MIN(MAX(OMEGAC(I),ZERO),ONE)      
          ENDIF
c
          ! Apply damage on stress computation
          !  -> Standard model with two damage variables
          IF (DFLAG == 1) THEN 
            DMG(I) = MIN(MAX(OMEGAT(I),OMEGAC(I)),ONE)
            ! Tensile stress tensor
            SIGTXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPT(I,1)
     .                + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPT(I,2)
     .                + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPT(I,3)
            SIGTYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPT(I,2)
     .                + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPT(I,3)
     .                + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPT(I,1)
            SIGTZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPT(I,3)
     .                + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPT(I,1)
     .                + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPT(I,2)
            SIGTXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPT(I,1)
     .                + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPT(I,2)
     .                + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPT(I,3)
            SIGTYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPT(I,2)
     .                + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPT(I,3)
     .                + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPT(I,1)
            SIGTZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPT(I,3)
     .                + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPT(I,1)
     .                + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPT(I,2)
            ! Compressive stress tensor
            SIGCXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPC(I,1)
     .                + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPC(I,2)
     .                + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPC(I,3)
            SIGCYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPC(I,2)
     .                + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPC(I,3)
     .                + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPC(I,1)
            SIGCZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPC(I,3)
     .                + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPC(I,1)
     .                + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPC(I,2)
            SIGCXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPC(I,1)
     .                + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPC(I,2)
     .                + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPC(I,3)
            SIGCYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPC(I,2)
     .                + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPC(I,3)
     .                + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPC(I,1)
            SIGCZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPC(I,3)
     .                + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPC(I,1)
     .                + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPC(I,2)
            ! New stress tensor
            SIGNXX(I) = (ONE-OMEGAT(I))*SIGTXX(I) + (ONE-OMEGAC(I))*SIGCXX(I)
            SIGNYY(I) = (ONE-OMEGAT(I))*SIGTYY(I) + (ONE-OMEGAC(I))*SIGCYY(I)
            SIGNZZ(I) = (ONE-OMEGAT(I))*SIGTZZ(I) + (ONE-OMEGAC(I))*SIGCZZ(I)
            SIGNXY(I) = (ONE-OMEGAT(I))*SIGTXY(I) + (ONE-OMEGAC(I))*SIGCXY(I)
            SIGNYZ(I) = (ONE-OMEGAT(I))*SIGTYZ(I) + (ONE-OMEGAC(I))*SIGCYZ(I)
            SIGNZX(I) = (ONE-OMEGAT(I))*SIGTZX(I) + (ONE-OMEGAC(I))*SIGCZX(I)
          !  -> Isotropic damage model with one damage variable
          ELSEIF (DFLAG == 2) THEN 
            DMG(I) = MIN(OMEGAT(I),ONE)
            SIGNXX(I) = (ONE-DMG(I))*SIGNXX(I)
            SIGNYY(I) = (ONE-DMG(I))*SIGNYY(I)
            SIGNZZ(I) = (ONE-DMG(I))*SIGNZZ(I)
            SIGNXY(I) = (ONE-DMG(I))*SIGNXY(I)
            SIGNYZ(I) = (ONE-DMG(I))*SIGNYZ(I)
            SIGNZX(I) = (ONE-DMG(I))*SIGNZX(I)
          !  -> Multiplicative damage model
          ELSEIF (DFLAG == 3) THEN 
            DMG(I) = ONE - MIN((ONE-OMEGAT(I))*(ONE-OMEGAC(I)),ONE)
            SIGNXX(I) = (ONE-DMG(I))*SIGNXX(I)
            SIGNYY(I) = (ONE-DMG(I))*SIGNYY(I)
            SIGNZZ(I) = (ONE-DMG(I))*SIGNZZ(I)
            SIGNXY(I) = (ONE-DMG(I))*SIGNXY(I)
            SIGNYZ(I) = (ONE-DMG(I))*SIGNYZ(I)
            SIGNZX(I) = (ONE-DMG(I))*SIGNZX(I)
          ENDIF
c
          ! Element deletion check    
          IF ((DMG(I) == ONE).AND.(LOFF(I) == ONE).AND.(OFF(I) > ZERO)) THEN 
            LOFF(I) = ZERO
            UELR(I) = UELR(I) + ONE
            IF (NINT(UELR(I)) == FAILIP)THEN 
              NINDX3 = NINDX3 + 1
              INDX3(NINDX3) = I
              IDEL7NOK = 1
              OFF(I) = ZERO   
            ENDIF
          ENDIF
c
        ENDDO
      ENDIF
c
      ! Storing new values
      DO I=1,NEL  
        ! Update user variable
        UVAR(I,1)  = KAP(I)
        UVAR(I,2)  = PLAXX(I)
        UVAR(I,3)  = PLAYY(I)
        UVAR(I,4)  = PLAZZ(I)
        UVAR(I,5)  = PLAXY(I)
        UVAR(I,6)  = PLAYZ(I)
        UVAR(I,7)  = PLAZX(I)
        UVAR(I,8)  = EPS_EQ(I)
        UVAR(I,9)  = KAPDT(I)
        UVAR(I,10) = KAPDT1(I)
        UVAR(I,11) = KAPDT2(I) 
        UVAR(I,12) = OMEGAT(I)
        UVAR(I,13) = KAPDC(I)
        UVAR(I,14) = KAPDC1(I)
        UVAR(I,15) = KAPDC2(I)
        UVAR(I,16) = OMEGAC(I)   
        ! Coefficient for hourglass
        ET(I)      = HARDP(I) / (HARDP(I) + YOUNG) 
        ! Computation of the sound speed   
        SOUNDSP(I) = SQRT((BULK + FOUR_OVER_3*G)/RHO(I))
      ENDDO
c
      IF (IRATE > 0) THEN 
#include "vectorize.inc" 
        DO II = 1,NINDX
          I = INDX(II)
          IF (UVAR(I,17) == ZERO) UVAR(I,17) = ARATE(I)
        ENDDO
#include "vectorize.inc" 
        DO II = 1,NINDX2
          I = INDX2(II)
          IF (UVAR(I,17) == ZERO) UVAR(I,17) = ARATE(I)
        ENDDO
      ENDIF

      ! Printout failure
      IF (NINDX3>0) THEN
        DO I=1,NINDX3
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(INDX3(I))
          WRITE(ISTDO,2100) NGL(INDX3(I)),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
c
 2000 FORMAT(1X,'FAILURE (CDPM2) OF SOLID ELEMENT ',I10)
 2100 FORMAT(1X,'FAILURE (CDPM2) OF SOLID ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
c      
      END
