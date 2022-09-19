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
Chd|  CNLOC_MAT104_INI              source/materials/mat/mat104/cnloc_mat104_ini.F
Chd|-- called by -----------
Chd|        CNLOC_MATINI                  source/materials/mat_share/cnloc_matini.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CNLOC_MAT104_INI(
     .          NEL      ,IPG      ,IPT      ,NUPARAM  ,NUVAR    ,UPARAM   ,
     .          UVAR     ,PLA      ,OFF      ,THKLY    ,OFFG     ,
     .          SIGOXX   ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     .          THK      ,DMG      ,NPTR     ,NPTS     ,NPTT     ,BUFLY    ,
     .          TIME     ,VARNL    ,FAILURE  )
      USE ELBUFDEF_MOD
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
      INTEGER NEL,IPG,IPT,NUPARAM,NUVAR,NPTR,NPTS,NPTT,IR,IS,IT
      my_real
     .   TIME
      my_real,DIMENSION(NUPARAM), INTENT(IN) ::
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     ::
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL), INTENT(INOUT) ::
     .   PLA,OFF,OFFG,THK,VARNL,DMG,THKLY
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) ::
     .   UVAR
      TYPE(BUF_LAY_) :: BUFLY
      LOGICAL :: FAILURE
c
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,K,II,IGURSON,ITER,NITER,NINDX,INDEX(NEL),NICE
c
      my_real
     .   CDR,KDR,HARD,YLD0,QVOCE,BVOCE,
     .   Q1,Q2,ED,AN,EPN,KW,FR,FC,F0,NNU
      my_real
     .   SIGVM,YLD2I,OMEGA,FCOSH,FSINH,DSDRDJ2,DSDRDJ3,NORMSIG,
     .   DJ3DSXX,DJ3DSYY,DJ3DSXY,DJ3DSZZ,DJ3DSYZ,DJ3DSZX,
     .   NORMXX,NORMYY,NORMXY,NORMZZ,NORMYZ,NORMZX,
     .   SDPLA,DPHI_DTRSIG,SIG_DFDSIG,DPHI_DSIG,DPHI_DFDR
      my_real, DIMENSION(NEL) ::
     .   TRSIG,SXX,SYY,SXY,SZZ,SYZ,SZX,SIGM,J2,J3,SIGDR,
     .   YLD,TRDFDS,FDR,D,TRIAX,FT,FS,FG,FN,FSH,DEZZ,DLAM,
     .   DLAM_NL,PLAP_NL,DPLA_NL,PLA_NL,ETAT
c
      !=======================================================================
      !                  INITIALIZATION OF
      !       DRUCKER MATERIAL LAW WITH GURSON DAMAGE
      !          USING NON LOCAL PEERLINGS METHOD
      !=======================================================================
      !UVAR(5)   FG      DAMAGE OF CAVITIES GROWTH
      !UVAR(6)   FN      DAMAGE OF CAVITIES NUCLEATION
      !UVAR(7)   FSH     DAMAGE OF SHEAR
      !UVAR(8)   FT      TOTAL DAMAGE (FG + FN + FSH)
      !UVAR(9)   FS      EFFECTIVE DAMAGE FS = F(FT)
      !UVAR(10)  PLA_NL  NON-LOCAL PLASTIC STRAIN
      !-----------------------------------------------------------------------
      !Warning VARNL(NEL) :
      !input  => VARNL = non-local plastic strain increment
      !output => VARNL = cumulated local plastic strain
      !=======================================================================
c
      ! Recovering model parameters
      ! Elastic parameters
      NNU     = UPARAM(7)
      ! Plastic criterion and hardening parameters [Drucker, 1948]
      NICE    = NINT(UPARAM(11)) ! Choice of the Nice method
      CDR     = UPARAM(12) ! Drucker coefficient
      KDR     = UPARAM(13) ! Drucker 1/K coefficient
      HARD    = UPARAM(15) ! Linear hardening
      YLD0    = UPARAM(16) ! Initial yield stress
      QVOCE   = UPARAM(17) ! 1st Voce parameter
      BVOCE   = UPARAM(18) ! 2nd Voce parameter
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
c
      ! Non-local micromorphic method
      IF (IGURSON == 2) THEN
        ! Recovering internal variables
        DO I=1,NEL
          ! User inputs
          IF (NICE == 1) THEN
            PLA_NL(I) = UVAR(I,7) ! Non-local plastic strain
          ELSE
            PLA_NL(I) = UVAR(I,6) ! Non-local plastic strain
          ENDIF
          IF (OFF(I) == ONE) THEN
            DPLA_NL(I) = MAX(VARNL(I),ZERO)     ! Non-local plastic strain increment
            PLA_NL(I)  = PLA_NL(I) + DPLA_NL(I) ! Non-local cumulated plastic strain
          ELSE
            DPLA_NL(I) = ZERO
            PLAP_NL(I) = ZERO
          ENDIF
          IF (NICE == 1) THEN
            UVAR(I,7) = PLA_NL(I)
          ELSE
            UVAR(I,6) = PLA_NL(I)
          ENDIF
          ! Variable to regularize (cumulated plastic strain)
          IF ((NPTR==1).AND.(NPTS==1)) THEN
            VARNL(I) = PLA(I)
          ELSE
            IF (IPG == NPTR*NPTS) THEN
              VARNL(I) = ZERO
              DO IR = 1,NPTR
                DO IS = 1,NPTS
                  VARNL(I) = VARNL(I) + (BUFLY%LBUF(IR,IS,IPT)%PLA(I))/(NPTR*NPTS)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ! Non-local Peerlings method
      ELSEIF (IGURSON == 3) THEN
        ! Recovering internal variables
        DO I=1,NEL
          ! User inputs
          IF (NICE == 1) THEN
            FG(I)     = UVAR(I,2)  ! Growth damage
            FN(I)     = UVAR(I,3)  ! Nucleation damage
            FSH(I)    = UVAR(I,4)  ! Shear damage
            FT(I)     = UVAR(I,5)  ! Total damage
            FS(I)     = UVAR(I,6)  ! Effective damage
            PLA_NL(I) = UVAR(I,7)  ! Non-local plastic strain
          ELSE
            FG(I)     = UVAR(I,1)  ! Growth damage
            FN(I)     = UVAR(I,2)  ! Nucleation damage
            FSH(I)    = UVAR(I,3)  ! Shear damage
            FT(I)     = UVAR(I,4)  ! Total damage
            FS(I)     = UVAR(I,5)  ! Effective damage
            PLA_NL(I) = UVAR(I,6)  ! Non-local plastic strain
          ENDIF
          ! Standard inputs
          D(I)      = Q1*FS(I)   ! Effective normalized damage
          DEZZ(I)   = ZERO       ! Initialization of the transverse strain increment
          IF (OFF(I) == ONE) THEN
            DPLA_NL(I) = MAX(VARNL(I),ZERO)     ! Non-local plastic strain increment
            PLA_NL(I)  = PLA_NL(I) + DPLA_NL(I) ! Non-local cumulated plastic strain
          ELSE
            DPLA_NL(I) = ZERO
            PLAP_NL(I) = ZERO
          ENDIF
          ! Computation of the yield stress
          YLD(I) = YLD0 + HARD*PLA(I) + QVOCE*(ONE-EXP(-BVOCE*PLA(I)))
c
          !========================================================================
          ! NON-LOCAL VARIABLES INITIALIZATION
          !========================================================================
c
          ! Previous value of Drucker equivalent stress
          TRSIG(I)= SIGOXX(I) + SIGOYY(I)
          SIGM(I) = -TRSIG(I) * THIRD
          SXX(I)  = SIGOXX(I) + SIGM(I)
          SYY(I)  = SIGOYY(I) + SIGM(I)
          SZZ(I)  = SIGM(I)
          SXY(I)  = SIGOXY(I)
          SYZ(I)  = SIGOYZ(I)
          SZX(I)  = SIGOZX(I)
          J2(I)   = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 )
     .            +       SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
          J3(I)   = SXX(I) * SYY(I) * SZZ(I)
     .            + SXY(I) * SYZ(I) * SZX(I) * TWO
     .            - SXX(I) * SYZ(I)**2
     .            - SYY(I) * SZX(I)**2
     .            - SZZ(I) * SXY(I)**2
c
          FDR(I) = J2(I)**3 - CDR*(J3(I)**2)
          IF (FDR(I) > ZERO) THEN
            SIGDR(I) = KDR * EXP((ONE/SIX)*LOG(FDR(I)))
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
          ! Normal to the previous yield surface
          IF (YLD(I)>ZERO) THEN
            YLD2I       = ONE / YLD(I)**2
            DPHI_DSIG   = TWO * SIGDR(I) * YLD2I
            FSINH       = SINH(Q2*ETAT(I)*TRSIG(I)/(YLD(I)*TWO))
            DPHI_DTRSIG = Q1*Q2*ETAT(I)*FS(I)*FSINH/YLD(I)
          ELSE
            YLD2I       = ZERO
            DPHI_DSIG   = ZERO
            FSINH       = ZERO
            DPHI_DTRSIG = ZERO
          ENDIF
c
          ! Computation of the Eulerian norm of the stress tensor
          NORMSIG = SQRT(SIGOXX(I)*SIGOXX(I)
     .                 + SIGOYY(I)*SIGOYY(I)
     .            + TWO*SIGOXY(I)*SIGOXY(I)
     .            + TWO*SIGOYZ(I)*SIGOYZ(I)
     .            + TWO*SIGOZX(I)*SIGOZX(I))
c
          ! Computation of the norm to the yield surface
          IF (NORMSIG>ZERO) THEN
            FDR(I)      = (J2(I)/(NORMSIG**2))**3 - CDR*((J3(I)/(NORMSIG**3))**2)
            DPHI_DFDR   = DPHI_DSIG*KDR*(ONE/SIX)*EXP(-(FIVE/SIX)*LOG(FDR(I)))
            DSDRDJ2     = DPHI_DFDR*THREE*(J2(I)/(NORMSIG**2))**2
            DSDRDJ3     = -DPHI_DFDR*TWO*CDR*(J3(I)/(NORMSIG**3))
            ! dJ3/dS
            DJ3DSXX =  TWO_THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .               -  THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .               -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)
            DJ3DSYY = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .               + TWO_THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .               -  THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)
            DJ3DSZZ = - THIRD*(SYY(I)*SZZ(I)-SYZ(I)**2)/(NORMSIG**2)
     .               - THIRD*(SXX(I)*SZZ(I)-SZX(I)**2)/(NORMSIG**2)
     .               + TWO_THIRD*(SXX(I)*SYY(I)-SXY(I)**2)/(NORMSIG**2)
            DJ3DSXY = TWO*(SXX(I)*SXY(I) + SXY(I)*SYY(I) + SZX(I)*SYZ(I))/(NORMSIG**2)
            DJ3DSYZ = TWO*(SXY(I)*SZX(I) + SYY(I)*SYZ(I) + SYZ(I)*SZZ(I))/(NORMSIG**2)
            DJ3DSZX = TWO*(SXX(I)*SZX(I) + SXY(I)*SYZ(I) + SZX(I)*SZZ(I))/(NORMSIG**2)
            ! dPhi/dSig
            NORMXX     = DSDRDJ2*SXX(I)/NORMSIG + DSDRDJ3*DJ3DSXX + DPHI_DTRSIG
            NORMYY     = DSDRDJ2*SYY(I)/NORMSIG + DSDRDJ3*DJ3DSYY + DPHI_DTRSIG
            NORMZZ     = DSDRDJ2*SZZ(I)/NORMSIG + DSDRDJ3*DJ3DSZZ + DPHI_DTRSIG
            NORMXY     = TWO*DSDRDJ2*SXY(I)/NORMSIG + DSDRDJ3*DJ3DSXY
            NORMYZ     = TWO*DSDRDJ2*SYZ(I)/NORMSIG + DSDRDJ3*DJ3DSYZ
            NORMZX     = TWO*DSDRDJ2*SZX(I)/NORMSIG + DSDRDJ3*DJ3DSZX
            TRDFDS(I)  = NORMXX + NORMYY + NORMZZ
            SIG_DFDSIG = SIGOXX(I)*NORMXX + SIGOYY(I)*NORMYY
     .                 + SIGOXY(I)*NORMXY + SIGOYZ(I)*NORMYZ + SIGOZX(I)*NORMZX
          ELSE
            NORMXX     = ZERO
            NORMYY     = ZERO
            NORMZZ     = ZERO
            NORMXY     = ZERO
            NORMYZ     = ZERO
            NORMZX     = ZERO
            TRDFDS(I)  = ZERO
            SIG_DFDSIG = ZERO
          ENDIF
c
          ! Computation of the non-local plastic multiplier
          IF (SIG_DFDSIG>ZERO) THEN
            DLAM_NL(I) = ((ONE - FT(I))*YLD(I)*DPLA_NL(I))/SIG_DFDSIG
          ELSE
            DLAM_NL(I) = ZERO
          ENDIF
          IF (TIME == ZERO) THEN
            IF (SIG_DFDSIG>EM01) THEN
              DLAM(I) = (YLD(I)*PLA(I))/SIG_DFDSIG
              DEZZ(I) = -NNU*DLAM(I)*(NORMXX + NORMYY) - DLAM(I)*NORMZZ
            ELSE
              DLAM(I) = ZERO
            ENDIF
          ENDIF
          IF ((SIG_DFDSIG>EM01).AND.(DLAM_NL(I)>ZERO)) THEN
            DEZZ(I) = DEZZ(I) + NNU*DLAM_NL(I)*(NORMXX + NORMYY) + DLAM_NL(I)*NORMZZ
          ENDIF
c
          ! Damage growth update
          IF ((FT(I)>ZERO).AND.(FT(I)<FR).AND.(TRDFDS(I)>ZERO)) THEN
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
            SDPLA  = (SXX(I)*NORMXX+SYY(I)*NORMYY+ SZZ(I)*NORMZZ
     .              + SXY(I)*NORMXY+SYZ(I)*NORMYZ+ SZX(I)*NORMZX)
     .             * DLAM_NL(I)
            FSH(I) = FSH(I) + KW*OMEGA*FT(I)*(SDPLA/SIGDR(I))
          ENDIF
          FSH(I) = MAX(FSH(I),ZERO)
c
          ! Total damage update
          FT(I) = F0 + FG(I) + FN(I) + FSH(I)
          FT(I) = MIN(FT(I),FR)
          IF (FT(I) >= FR) THEN
            IF (OFF(I)==ONE) OFF(I) = ZERO
            IF (.NOT.FAILURE) FAILURE = .TRUE.
          ENDIF
c
          ! Effective update
          IF (FT(I) < FC)THEN
            FS(I) = FT(I)
          ELSE
            FS(I) = FC + (ONE/Q1 - FC) * (FT(I)-FC)/(FR-FC)
          ENDIF
          FS(I) = MIN(FS(I),ONE/Q1)
          D(I)  = Q1*FS(I)
c
          ! Storing new values
          ! USR Outputs
          IF (NICE == 1) THEN
            UVAR(I,2) = FG(I)            ! Growth damage
            UVAR(I,3) = FN(I)            ! Nucleation damage
            UVAR(I,4) = FSH(I)           ! Shear damage
            UVAR(I,5) = MIN(FT(I),FR)    ! Total damage
            UVAR(I,6) = MIN(FS(I),ONE/Q1) ! Effective damage
            UVAR(I,7) = PLA_NL(I)        ! Non-local cumulated plastic strain
          ELSE
            UVAR(I,1) = FG(I)            ! Growth damage
            UVAR(I,2) = FN(I)            ! Nucleation damage
            UVAR(I,3) = FSH(I)           ! Shear damage
            UVAR(I,4) = MIN(FT(I),FR)    ! Total damage
            UVAR(I,5) = MIN(FS(I),ONE/Q1) ! Effective damage
            UVAR(I,6) = PLA_NL(I)        ! Non-local cumulated plastic strain
          ENDIF
          ! Standard outputs
          DMG(I)     = FT(I)/FR         ! Normalized total damage
          ! Variable to regularize (cumulated plastic strain)
          IF ((NPTR==1).AND.(NPTS==1)) THEN
            VARNL(I) = PLA(I)
            ! Computation of the thickness variation
            THK(I)   = THK(I) + DEZZ(I)*THK(I)*OFF(I)
          ELSE
            IF (IPG == 1) THEN
              DO IR = 1,NPTR
                DO IS = 1,NPTS
                  BUFLY%LBUF(IR,IS,IPT)%THK(I) = THK(I)
                ENDDO
              ENDDO
            ENDIF
            ! Computation of the thickness variation
            THKLY(I) = THKLY(I) + DEZZ(I)*THKLY(I)*OFF(I)
            IF (IPG == NPTR*NPTS) THEN
              VARNL(I) = ZERO
              THK(I)   = ZERO
              DO IR = 1,NPTR
                DO IS = 1,NPTS
                  VARNL(I) = VARNL(I) + (BUFLY%LBUF(IR,IS,IPT)%PLA(I))/(NPTR*NPTS)
                  THK(I)   = THK(I)   + (BUFLY%LBUF(IR,IS,IPT)%THK(I))/(NPTR*NPTS)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDDO
c
        DO I = 1,NEL
          ! Checking element deletion
          !   Case of under-integrated shells
          IF ((NPTR == 1).AND.(NPTS == 1)) THEN
             IF (NPTT>1) THEN
               !Initialization for checking complete failure of the shell (all integration points)
               IF (IPT == 1) THEN
                 OFFG(I) = ZERO
               ENDIF
               !If one integration points is not fully broken, the shell remains
               IF (OFF(I)>ZERO) OFFG(I) = ONE
            ENDIF
          !   Case of fully integrated shells
          ELSE
            IF ((IPG == 1).AND.(IPT == 1)) THEN
              !Initialization for checking complete failure of the shell (all integration points)
              OFFG(I) = ZERO
              ! Loop over all Gauss points (thickness + surface)
              DO IR = 1,NPTR
                DO IS = 1,NPTS
                  DO IT = 1,NPTT
                    !If one integration points is not fully broken, the shell remains
                    IF (BUFLY%LBUF(IR,IS,IT)%OFF(I)>ZERO) OFFG(I) = ONE
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
c-----------
      END
