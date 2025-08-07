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
!||    sigeps109             ../engine/source/materials/mat/mat109/sigeps109.F
!||--- called by ------------------------------------------------------
!||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
!||--- calls      -----------------------------------------------------
!||    table2d_vinterp_log   ../engine/source/tools/curve/table2d_vinterp_log.F
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE SIGEPS109(
     1     NEL     ,NGL     ,NUPARAM ,NUVAR   ,NVARTMP ,NUMTABL ,
     2     UPARAM  ,UVAR    ,VARTMP  ,ITABLE  ,TABLE   ,JTHE    ,
     3     TIME    ,TIMESTEP,OFF     ,RHO     ,PLA     ,DPLA    ,
     4     SOUNDSP ,SIGY    ,ET      ,TEMP    ,EPSD    ,DPDM    ,
     5     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8     INLOC   ,DPLANL  ,IEOS    ,JLAG    ,FHEAT   ,VOLUME  ,
     9     SEQ     ,LPLANL  ,PLA_NL  ,LEPSDNL ,DPDT_NL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IEOS
      INTEGER ,INTENT(IN) :: JTHE
      INTEGER ,INTENT(IN) :: JLAG
      INTEGER NEL,NUPARAM,NUVAR,NVARTMP,NUMTABL,INLOC
      INTEGER ,DIMENSION(NUMTABL),INTENT(IN)  :: ITABLE
      INTEGER ,DIMENSION(NEL)    ,INTENT(IN)  :: NGL
c
      my_real  :: TIME,TIMESTEP
      my_real,DIMENSION(NUPARAM) ,INTENT(IN)  :: UPARAM
      my_real,DIMENSION(NEL)     ,INTENT(IN)  :: VOLUME
      my_real,DIMENSION(NEL)     ,INTENT(IN)  :: RHO,DPDM,OFF,
     .   DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .   SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX,DPLANL
      my_real ,DIMENSION(NEL)    ,INTENT(OUT) ::  SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: PLA,DPLA,EPSD
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: TEMP
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: FHEAT
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: SEQ
      INTEGER, INTENT(IN) :: LPLANL, LEPSDNL
      my_real, DIMENSION(NEL*LPLANL) :: PLA_NL
      my_real, DIMENSION(NEL*LEPSDNL) :: DPDT_NL
c
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,NINDX,ITER,NITER,ISMOOTH,
     .   FUNC_YLD,FUNC_TEMP,FUNC_ETA,NDIM_YLD,NDIM_TEMP,NDIM_ETA
      INTEGER ,DIMENSION(NEL) :: INDEX
c
      my_real  :: YOUNG,BULK,LAME,G,G2,G3,NU,TREF,TINI,ETA,LDAV,CP,
     .   XRATE,XSCALE,YSCALE,DTINV,J2,Q2,DPHI_DLAM,R,DLAMIN,ALPHA,ALPHI
      my_real  :: ASRATE,DDEP,DFDSIG2,SIG_DFDSIG
c
      my_real, DIMENSION(NEL) ::SVM,SVMT,YLD,YLD_TREF,YLD_TEMP,
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,SIGM,
     .   FACT_ETA,DYDX,HARDP,HARDR,YLD_I,HARDP_I,HARDR_I,DXDYV,DLAM,PHI,
     .   FTHERM,TFAC,PLA0,NORMXX,NORMYY,NORMZZ,NORMXY,
     .   NORMYZ,NORMZX,DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX,DPLA_DLAM
      my_real, DIMENSION(NEL,3) :: XVEC_ETA
      my_real, DIMENSION(NEL,4) :: XVEC
      INTEGER, DIMENSION(NEL,3) :: IPOS_ETA
      INTEGER, DIMENSION(NEL,2) :: IPOS
C-----------------------------------------------
      ! VARTMP(1)   latest position of PLAS in TAB_YLD function 
      ! VARTMP(2)   latest position of PLAS in TAB_TEMP function 
      ! VARTMP(3)   latest position of TEMP in TAB_TEMP function 
      ! VARTMP(4)   latest position of TEMP in TAB_ETA function 
      ! VARTMP(5)   latest position of PLAS in TAB_ETA function 
C=======================================================================
!
      !=========================================================================
      !< - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Recovering model parameters   
      YOUNG   =  UPARAM(1)       !< Young modulus
      NU      =  UPARAM(2)       !< Poisson's ratio
      ETA     =  UPARAM(3)       !< Thermal work coefficient
      TREF    =  UPARAM(4)       !< Reference temperature
      TINI    =  UPARAM(5)       !< Initial tempareture
      ISMOOTH =  NINT(UPARAM(6)) !< Function interpolation flag
      XRATE   =  UPARAM(7)       !< Strain rate abscissa factor for eta function
      XSCALE  =  UPARAM(8)       !< Strain rate abscissa factor for yld function
      YSCALE  =  UPARAM(9)       !< Yield function scale factor
      G       =  UPARAM(11)      !< Shear modulus
      G2      =  UPARAM(12)      !< Shear modulus * 2
      G3      =  UPARAM(13)      !< Shear modulus * 3
      BULK    =  UPARAM(14)      !< Bulk modulus
      LAME    =  UPARAM(15)      !< Lame parameter
      CP      =  UPARAM(20)      !< Specific heat coefficient
      ASRATE  =  UPARAM(21)      !< Filtering pulsation for plastic strain rate
      !< Plastic strain rate filtering parameters
      DTINV  = ONE / MAX(EM20, TIMESTEP)
      ALPHA  = ASRATE*TIMESTEP
      ALPHI  = ONE-ALPHA
      !< No temperature calculation inside material
      IF (JTHE /= 0) ETA = ZERO 
!
      !< Recovering tables and functions
      FUNC_YLD  = ITABLE(1)
      FUNC_TEMP = ITABLE(2)
      FUNC_ETA  = ITABLE(3)
      NDIM_YLD  = TABLE(FUNC_YLD)%NDIM
      IF (FUNC_TEMP > 0) THEN
        NDIM_TEMP  = TABLE(FUNC_TEMP)%NDIM
      ENDIF
      IF (FUNC_ETA  > 0) THEN
        NDIM_ETA   = TABLE(FUNC_ETA)%NDIM
      ENDIF
!
      !< Recovering internal variables and initializations of local variables
      DO I = 1,NEL
        PLA0(I)  = PLA(I) !< Initial plastic strain
        DPLA(I)  = ZERO   !< Plastic strain increment initialization
        ET(I)    = ONE    !< Hourglass stabilization variable initialization
        HARDP(I) = ZERO   !< Hardening modulus initialization
      ENDDO
!      
      !< Temperature & thermal softening factor initialization
      IF (JTHE == 0) THEN
        !< Temperature initialization
        IF (TIME == ZERO) TEMP(1:NEL) = TINI
        !< Thermal softening factor initialization
        IF (ETA > ZERO) THEN
          !< Taylor-Quinney factor
          IF (FUNC_ETA > 0) THEN
            IF (INLOC == 0) THEN 
              XVEC_ETA(1:NEL,1) = EPSD(1:NEL) * XRATE 
            ELSE
              XVEC_ETA(1:NEL,1) = DPDT_NL(1:NEL) * XRATE 
            ENDIF
            IPOS_ETA(1:NEL,1) = 1
            IF (NDIM_ETA > 1) THEN
              XVEC_ETA(1:NEL,2) = TEMP(1:NEL)
              IPOS_ETA(1:NEL,2) = VARTMP(1:NEL,4)
            END IF
            IF (NDIM_ETA > 2) THEN
              IF (INLOC == 0) THEN 
                XVEC_ETA(1:NEL,3) = PLA(1:NEL)
              ELSE
                XVEC_ETA(1:NEL,3) = PLA_NL(1:NEL)
              ENDIF
              IPOS_ETA(1:NEL,3) = VARTMP(1:NEL,5)
            END IF
            CALL TABLE_VINTERP(TABLE(FUNC_ETA),NEL,NEL,IPOS_ETA,XVEC_ETA,
     .                         FACT_ETA,DXDYV)          
            IF (NDIM_ETA > 1) VARTMP(1:NEL,4) = IPOS_ETA(1:NEL,2)
            IF (NDIM_ETA > 2) VARTMP(1:NEL,5) = IPOS_ETA(1:NEL,3)
            DO I=1,NEL
              FTHERM(I) = MIN(ETA*FACT_ETA(I), ONE)
            END DO
          ELSE
            FTHERM(1:NEL) = MIN(ETA, ONE)
          END IF 
        END IF
      ENDIF
!      
      !=========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !=========================================================================  
      DO I=1,NEL
        !< Computation of the trial stress tensor
        LDAV = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAME
        SIGNXX(I) = SIGOXX(I) + DEPSXX(I)*G2 + LDAV
        SIGNYY(I) = SIGOYY(I) + DEPSYY(I)*G2 + LDAV
        SIGNZZ(I) = SIGOZZ(I) + DEPSZZ(I)*G2 + LDAV
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*G
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*G
        !< Computation of the trace of the mean spherical stress 
        SIGM(I) = (SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)) * THIRD
        !< Computation of the trial deviatoric stress tensor
        SXX(I) = SIGNXX(I) - SIGM(I)
        SYY(I) = SIGNYY(I) - SIGM(I)
        SZZ(I) = SIGNZZ(I) - SIGM(I)
        SXY(I) = SIGNXY(I)
        SYZ(I) = SIGNYZ(I)
        SZX(I) = SIGNZX(I)
        !< Assembling Von Mises equivalent stress
        J2 = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + 
     .             SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
        SVM(I) = SQRT(THREE*J2)
      ENDDO
!
      !< Computation of the initial yield stress      
      XVEC(1:NEL,1) = PLA(1:NEL)
      XVEC(1:NEL,2) = EPSD(1:NEL) * XSCALE
      IPOS(1:NEL,1) = VARTMP(1:NEL,1)
      IPOS(1:NEL,2) = 1
      CALL TABLE2D_VINTERP_LOG(TABLE(FUNC_YLD),ISMOOTH,NEL,NEL,IPOS,
     .                         XVEC,YLD,HARDP,HARDR)               
      YLD(1:NEL) = YLD(1:NEL)*YSCALE
      HARDP(1:NEL) = HARDP(1:NEL)*YSCALE
      VARTMP(1:NEL,1) = IPOS(1:NEL,1)
      !< Adding temperature dependence to yield stress   
      IF (FUNC_TEMP > 0) THEN
        XVEC(1:NEL,2) = TREF
        IPOS(1:NEL,1) = VARTMP(1:NEL,2)
        IPOS(1:NEL,2) = VARTMP(1:NEL,3)
        CALL TABLE_VINTERP(TABLE(FUNC_TEMP),NEL,NEL,IPOS,XVEC,YLD_TREF,DYDX)  
        VARTMP(1:NEL,2) = IPOS(1:NEL,1)     
        VARTMP(1:NEL,3) = IPOS(1:NEL,2)     
        XVEC(1:NEL,2) = TEMP(1:NEL)
        CALL TABLE_VINTERP(TABLE(FUNC_TEMP),NEL,NEL,IPOS,XVEC,YLD_TEMP,DYDX)
        TFAC(1:NEL)  = YLD_TEMP(1:NEL) / YLD_TREF(1:NEL)      
        YLD(1:NEL)   = YLD(1:NEL)   * TFAC(1:NEL)      
        HARDP(1:NEL) = HARDP(1:NEL) * TFAC(1:NEL) 
      ELSE
        TFAC(1:NEL) = ONE
      END IF
!     
      !=========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !=========================================================================
      PHI(1:NEL) = SVM(1:NEL) - YLD(1:NEL)
      NINDX = 0
      DO I=1,NEL
        IF (PHI(I) >= ZERO .AND. OFF(I) == ONE) THEN
          NINDX = NINDX + 1 
          INDEX(NINDX) = I
        ENDIF
      ENDDO       
!  
      !=========================================================================
      ! - PLASTIC CORRECTION WITH CUTTING PLANE METHOD (SEMI-IMPLICIT)
      !=========================================================================       
!      
      !< Number of iterations
      NITER = 3
!
      IF (NINDX > 0) THEN
!
        !< Loop over the iterations  
        DO ITER = 1,NITER 
#include "vectorize.inc" 
          !< Loop over yielding elements
          DO II = 1, NINDX      
            I = INDEX(II)
!
            ! Note: in this part, the purpose is to compute for each iteration
            ! a plastic multiplier allowing to update internal variables to satisfy
            ! the consistency condition using the cutting plane semi-implicit 
            ! iterative procedure.
            ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
            ! -> PHI          : current value of yield function (known)
            ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
            !                   into account of internal variables kinetic : 
            !                   plasticity, temperature and damage (to compute)
!
            !< 1 - Computation of the normal to the yield surface
            !-------------------------------------------------------------------
            NORMXX(I) = THREE_HALF*SXX(I)/(MAX(SVM(I),EM20))
            NORMYY(I) = THREE_HALF*SYY(I)/(MAX(SVM(I),EM20))
            NORMZZ(I) = THREE_HALF*SZZ(I)/(MAX(SVM(I),EM20))
            NORMXY(I) = THREE*SXY(I)/(MAX(SVM(I),EM20))
            NORMYZ(I) = THREE*SYZ(I)/(MAX(SVM(I),EM20))
            NORMZX(I) = THREE*SZX(I)/(MAX(SVM(I),EM20))
!
            !< 2 - Computation of DPHI_DLAMBDA
            !-------------------------------------------------------------------
!        
            !   a) Derivative with respect stress increments tensor DSIG
            !   ----------------------------------------------------------------
            DFDSIG2 = NORMXX(I) * NORMXX(I) * G2
     .              + NORMYY(I) * NORMYY(I) * G2
     .              + NORMZZ(I) * NORMZZ(I) * G2
     .              + NORMXY(I) * NORMXY(I) * G
     .              + NORMYZ(I) * NORMYZ(I) * G
     .              + NORMZX(I) * NORMZX(I) * G           
!          
            !   b) Derivative of dPLA with respect to DLAM
            !   ----------------------------------------------------------------   
            SIG_DFDSIG = SIGNXX(I) * NORMXX(I)
     .                 + SIGNYY(I) * NORMYY(I)
     .                 + SIGNZZ(I) * NORMZZ(I)
     .                 + SIGNXY(I) * NORMXY(I) 
     .                 + SIGNYZ(I) * NORMYZ(I)
     .                 + SIGNZX(I) * NORMZX(I)   
            DPLA_DLAM(I) = SIG_DFDSIG / YLD(I)       
!
            !   c) Assemble the derivation of the yield function w.r.t. lambda
            !   ----------------------------------------------------------------
            DPHI_DLAM = - DFDSIG2 + HARDP(I)*DPLA_DLAM(I)
            DPHI_DLAM = SIGN(MAX(ABS(DPHI_DLAM),EM20),DPHI_DLAM)  
!
            !< 3 - Computation of the plastic multiplier
            !-------------------------------------------------------------------
            DLAM(I) = - PHI(I) / DPHI_DLAM
!
            !< 4 - Update the plastic strain related variables
            !-------------------------------------------------------------------
            !< Plastic strain increment on the iteration
            DDEP    = DPLA_DLAM(I)*DLAM(I)
            !< Plastic strain increment on the time step
            DPLA(I) = MAX(DPLA(I) + DDEP,ZERO)
            !< Update the plastic strain
            PLA(I)  = PLA0(I) + DPLA(I)
            !< Plastic strain tensor increment on the iteration
            DPXX(I) = DLAM(I)*NORMXX(I)
            DPYY(I) = DLAM(I)*NORMYY(I)
            DPZZ(I) = DLAM(I)*NORMZZ(I)
            DPXY(I) = DLAM(I)*NORMXY(I)
            DPYZ(I) = DLAM(I)*NORMYZ(I)
            DPZX(I) = DLAM(I)*NORMZX(I)
!
          ENDDO
!
          !< 5 - Update the yield stress and its derivative
          !---------------------------------------------------------------------
          XVEC(1:NEL,1:2) = ZERO
          IPOS(1:NEL,1:2) = 0
          DO II = 1, NINDX 
            I = INDEX(II)
            XVEC(II,1) = PLA(I)
            XVEC(II,2) = EPSD(I)
            IPOS(II,1) = VARTMP(I,1)
            IPOS(II,2) = 1
          ENDDO
          CALL TABLE2D_VINTERP_LOG(TABLE(FUNC_YLD),ISMOOTH,NEL,NINDX,IPOS,XVEC,YLD_I,HARDP_I,HARDR_I)
          DO II = 1, NINDX 
            I = INDEX(II)
            VARTMP(I,1) = IPOS(II,1)
            HARDP(I) = HARDP_I(II)*YSCALE*TFAC(I)
            YLD(I) = YLD_I(II)*YSCALE*TFAC(I)
          ENDDO
!                    
          !< 6 - Update the stress tensor and the yield function
          !---------------------------------------------------------------------
#include "vectorize.inc" 
          DO II = 1, NINDX 
            I = INDEX(II)
!
            !< Update the Cauchy stress tensor
            SIGNXX(I) = SIGNXX(I) - DPXX(I)*G2
            SIGNYY(I) = SIGNYY(I) - DPYY(I)*G2
            SIGNZZ(I) = SIGNZZ(I) - DPZZ(I)*G2
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G
            SIGNYZ(I) = SIGNYZ(I) - DPYZ(I)*G
            SIGNZX(I) = SIGNZX(I) - DPZX(I)*G
!
            !< Computation of the new trace of the mean spherical stress 
            SIGM(I) = (SIGNXX(I) + SIGNYY(I) + SIGNZZ(I)) * THIRD
!
            !< Computation of the new deviatoric stress tensor
            SXX(I) = SIGNXX(I) - SIGM(I)
            SYY(I) = SIGNYY(I) - SIGM(I)
            SZZ(I) = SIGNZZ(I) - SIGM(I)
            SXY(I) = SIGNXY(I)
            SYZ(I) = SIGNYZ(I)
            SZX(I) = SIGNZX(I)
!
            !< Assembling the new Von Mises equivalent stress
            J2 = HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + 
     .                 SXY(I)**2 + SYZ(I)**2 + SZX(I)**2
            SVM(I) = SQRT(THREE*J2)
!
            !< Update the yield function
            PHI(I) = SVM(I) - YLD(I)
!
          ENDDO          
          !< End of the loop over yielding elements            
c            
        ENDDO
        ! End of the loop over the iterations 
        !=======================================================================
        ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE ITERATIVE METHOD
        !=======================================================================         
!        
        !< Update the hourglass stabilization variable
        DO II = 1, NINDX 
          I = INDEX(II)
          ET(I)  = HARDP(I) / (HARDP(I) + YOUNG)
        ENDDO  
!
        ! Update the temperature or heat energy due to plastic work
        IF (ETA > ZERO .AND. INLOC == 0) THEN 
          IF (JTHE /= 0 .AND. JLAG /= 0) THEN
            DO II = 1, NINDX 
              I = INDEX(II)
              FHEAT(I) = FHEAT(I) + FTHERM(I)*YLD(I)*DPLA(I) * VOLUME(I)
            ENDDO
          ELSE IF (CP > ZERO) THEN
            DO II = 1, NINDX 
              I = INDEX(II)
              TEMP(I) = TEMP(I) + FTHERM(I)*YLD(I)*DPLA(I) / (CP*RHO(I))
            ENDDO
          ENDIF
        ENDIF
!
      ENDIF
!
      !< Non-local thickness variation and temperature update
      !-------------------------------------------------------------------------
      IF (INLOC > 0 .AND. ETA > ZERO) THEN 
        IF (JTHE /= 0 .AND. JLAG /= 0) THEN
          DO I = 1, NEL
            IF (OFF(I) == ONE) THEN 
              FHEAT(I) = FHEAT(I) + 
     .               FTHERM(I)*YLD(I)*MAX(DPLANL(I),ZERO)*VOLUME(I)
            ENDIF
          ENDDO
        ELSE IF (CP > ZERO) THEN
          DO I = 1,NEL   
            IF (OFF(I) == ONE) THEN 
              TEMP(I) = TEMP(I) + FTHERM(I)*YLD(I)*MAX(DPLANL(I),ZERO)/(CP*RHO(I))
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
      !< Equation Of State (EOS) specific treatment
      !-------------------------------------------------------------------------
      ! if EOS is used, material law calculates only deviatoric stress tensor
      !                 sound speed depends on pressure derivative over volume change
      !                 calculated in EOS
      IF (IEOS > 0) THEN
        SIGNXX(1:NEL) = SIGNXX(1:NEL) - SIGM(1:NEL)
        SIGNYY(1:NEL) = SIGNYY(1:NEL) - SIGM(1:NEL)
        SIGNZZ(1:NEL) = SIGNZZ(1:NEL) - SIGM(1:NEL)
        SIGNXY(1:NEL) = SIGNXY(1:NEL)
        SIGNYZ(1:NEL) = SIGNYZ(1:NEL)
        SIGNZX(1:NEL) = SIGNZX(1:NEL)
        SOUNDSP(1:NEL)= SQRT((DPDM(1:NEL) + FOUR_OVER_3*G) / RHO(1:NEL))
      ELSE 
        SOUNDSP(1:NEL)= SQRT((BULK + FOUR_OVER_3*G) / RHO(1:NEL))
      END IF                
!      
      !< Storing output values
      !-------------------------------------------------------------------------
      DO I=1,NEL 
        !< Equivalent stress
        SEQ(I) = SVM(I)
        !< Yield stress
        SIGY(I) = YLD(I)
        !< Plastic strain rate
        EPSD(I) = ALPHA*DPLA(I)*DTINV + ALPHI*EPSD(I)
      ENDDO
!
      END SUBROUTINE SIGEPS109
