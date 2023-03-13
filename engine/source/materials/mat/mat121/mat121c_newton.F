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
Chd|  MAT121C_NEWTON                source/materials/mat/mat121/mat121c_newton.F
Chd|-- called by -----------
Chd|        SIGEPS121C                    source/materials/mat/mat121/sigeps121c.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE MAT121C_NEWTON(
     1         NEL     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,TIMESTEP,TIME    ,UPARAM  ,UVAR    ,RHO     ,PLA     ,
     3         DPLA    ,SOUNDSP ,EPSD    ,GS      ,THK     ,THKLY   ,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5         EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     6         SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8         SIGY    ,ET      ,DPLANL  ,SEQ     ,INLOC   )
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
      INTEGER NEL,NUPARAM,NUVAR,INLOC,NPF(*),NFUNC,IFUNC(NFUNC)
      INTEGER ,DIMENSION(NEL), INTENT(IN)    :: NGL
      my_real 
     .   TIME,TIMESTEP,TF(*)
      my_real,DIMENSION(NUPARAM), INTENT(IN) :: 
     .   UPARAM
      my_real,DIMENSION(NEL), INTENT(IN)     :: 
     .   RHO,DPLANL,GS,THKLY,
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .   EPSPXX,EPSPYY,EPSPXY,EPSPYZ,EPSPZX,
     .   SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL), INTENT(OUT)   :: 
     .   SOUNDSP,SIGY,ET,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL), INTENT(INOUT)       :: 
     .   PLA,DPLA,EPSD,OFF,THK,SEQ
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: 
     .   UVAR
      !=======================================================================
      !      Local Variables
      !=======================================================================
      INTEGER I,II,Ivisc,ITER,NITER,NINDX,INDEX(NEL),IPOS(NEL),
     .        IAD(NEL),ILEN(NEL)
      my_real 
     .   YOUNG(NEL),BULK(NEL),G(NEL),NU,A11(NEL),A12(NEL),NNU,TANG(NEL),
     .   AFILTR,Xscale_SIG0,Yscale_SIG0,Xscale_YOUN,Yscale_YOUN,
     .   Xscale_TANG,Yscale_TANG
      my_real
     .   DPDT,DLAM,DDEP,DEPXX,DEPYY,DEVEPSPXX,DEVEPSPYY,DEVEPSPZZ,TREPSP,
     .   NORMXX,NORMYY,NORMXY,DENOM,DFDSIG2,DPDT_NL,DEPSDT,DTINV
      my_real, DIMENSION(NEL) ::
     .   SXX,SYY,SZZ,SXY,SIGVM,YLD,HARDP,PHI,DEZZ,YLD0,DYLD0DEPSD,
     .   DYOUNDEPSD,DTANGDEPSD,TRSIG,DPHI_DLAM,TEST,DPXX,DPYY,DPXY
c
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      ! Elastic parameters     
      YOUNG(1:NEL) = UPARAM(1)  ! Young modulus
      BULK(1:NEL)  = UPARAM(2)  ! Bulk modulus 
      G(1:NEL)     = UPARAM(3)  ! Shear modulus 
      NU           = UPARAM(6)  ! Poisson ration 
      NNU          = UPARAM(7)  ! NU/(1-NU)
      A11(1:NEL)   = UPARAM(9)  ! Diagonal term, elastic matrix in plane stress
      A12(1:NEL)   = UPARAM(10) ! Non-diagonal term, elastic matrix in plane stress  
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
        DEZZ(I)       = ZERO ! Initialization of the strain increment in Z direction
        DPXX(I)       = ZERO ! Initialization of the XX plastic strain increment
        DPYY(I)       = ZERO ! Initialization of the YY plastic strain increment
        DPXY(I)       = ZERO ! Initialization of the ZZ plastic strain increment
        DYLD0DEPSD(I) = ZERO ! Initialization of the derivative of SIG0
        DYOUNDEPSD(I) = ZERO ! Initialization of the derivative of YOUN
        DTANGDEPSD(I) = ZERO ! Initialization of the derivative of TANG
      ENDDO
c      
      ! Filling the strain rate vector
      IF (Ivisc == 0) THEN 
        ! Compute effective strain-rate
        DO I = 1,NEL
          TREPSP    = THIRD*(EPSPXX(I) + EPSPYY(I))
          DEVEPSPXX = EPSPXX(I) - TREPSP
          DEVEPSPYY = EPSPYY(I) - TREPSP
          DEVEPSPZZ = -TREPSP
          DEPSDT    = TWO_THIRD*(DEVEPSPXX**2 + DEVEPSPYY**2 + DEVEPSPZZ**2 +
     .                        TWO*(EPSPXY(I)**2))
          DEPSDT    = SQRT(DEPSDT)
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
        BULK(1:NEL)  = THIRD * YOUNG(1:NEL) / (ONE - NU*TWO)
        A11(1:NEL)   = YOUNG(1:NEL) / (ONE - NU*NU)
        A12(1:NEL)   = A11(1:NEL) * NU
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
        SIGNXX(I) = SIGOXX(I) + A11(I)*DEPSXX(I) + A12(I)*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A11(I)*DEPSYY(I) + A12(I)*DEPSXX(I)
        SIGNXY(I) = SIGOXY(I) + DEPSXY(I)*G(I)
        SIGNYZ(I) = SIGOYZ(I) + DEPSYZ(I)*GS(I)
        SIGNZX(I) = SIGOZX(I) + DEPSZX(I)*GS(I)
        ! Computation of the trace of the trial stress tensor
        TRSIG(I)  = SIGNXX(I) + SIGNYY(I) 
        ! Computation of the deviatoric trial stress tensor
        SXX(I)    = SIGNXX(I) - TRSIG(I) * THIRD
        SYY(I)    = SIGNYY(I) - TRSIG(I) * THIRD
        SZZ(I)    = -TRSIG(I) * THIRD
        SXY(I)    = SIGNXY(I)
        ! Von Mises equivalent stress
        SIGVM(I)  = THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SXY(I)**2
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
c      
            ! Number of the element with plastic behaviour  	
            I = INDEX(II)
c        
            ! Note     : in this part, the purpose is to compute for each iteration
            ! a plastic multiplier allowing to update internal variables to satisfy
            ! the consistency condition using the backward Euler implicit method
            ! with a cutting plane iterative procedure
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
            NORMXY = THREE*SXY(I)/SIGVM(I)
c          
            ! 2 - Computation of DPHI_DLAMBDA
            !---------------------------------------------------------
c        
            !   a) Derivative with respect stress increments tensor DSIG
            !   --------------------------------------------------------
            DFDSIG2 = NORMXX * (A11(I)*NORMXX + A12(I)*NORMYY)
     .              + NORMYY * (A11(I)*NORMYY + A12(I)*NORMXX)
     .              + NORMXY * NORMXY * G(I)  
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
            DPXY(I) = DLAM * NORMXY
c          
            ! Elasto-plastic stresses update   
            SIGNXX(I) = SIGNXX(I) - (A11(I)*DPXX(I) + A12(I)*DPYY(I))
            SIGNYY(I) = SIGNYY(I) - (A11(I)*DPYY(I) + A12(I)*DPXX(I))
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G(I)
            TRSIG(I)  = SIGNXX(I) + SIGNYY(I)
            SXX(I)    = SIGNXX(I) - TRSIG(I) * THIRD
            SYY(I)    = SIGNYY(I) - TRSIG(I) * THIRD
            SZZ(I)    = - TRSIG(I) * THIRD
            SXY(I)    = SIGNXY(I)
c          
            ! Cumulated plastic strain and strain rate update
            DPLA(I) = MAX(ZERO,DPLA(I) + DLAM)
            PLA(I)  = MAX(ZERO,PLA(I) + DLAM)
            IF (Ivisc == 1) THEN 
              EPSD(I) = DPLA(I)*DTINV
            ENDIF
c          
            ! Von Mises equivalent stress update
            SIGVM(I) = THREE_HALF*(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2) + THREE*SXY(I)**2
            SIGVM(I) = SQRT(SIGVM(I)) 
c
            IF (Ivisc == 0) THEN 
              ! Yield stress update
              YLD(I) = YLD0(I) + (YOUNG(I)*TANG(I)/(YOUNG(I)-TANG(I)))*PLA(I)
              ! Yield function value update
              PHI(I) = SIGVM(I) - YLD(I)
            ENDIF
c          
            ! Transverse strain update
            IF (INLOC == 0) THEN
              DEZZ(I) = DEZZ(I) - DPXX(I) - DPYY(I)
            ENDIF   
c             
          ENDDO
          ! End of the loop over yielding elements 
c
          ! Update variable for full viscoplastic formulation
          IF (Ivisc == 1) THEN 
            ! Compute the initial yield stress
            IPOS(1:NEL) = 1
            IAD (1:NEL) = NPF(IFUNC(1)) / 2 + 1
            ILEN(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
            CALL VINTER2(TF,IAD,IPOS,ILEN,NEL,EPSD/Xscale_SIG0,DYLD0DEPSD,YLD0) 
            YLD0(1:NEL) = Yscale_SIG0*YLD0(1:NEL)
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
        ! End of the loop over the iterations 
      ENDIF
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
        SOUNDSP(I) = SQRT(A11(I)/RHO(I))
        ! Storing the yield stress
        SIGY(I) = YLD(I)  
        ! Thickness variation
        IF (INLOC > 0) THEN
          DEZZ(I) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG(I)
          DEZZ(I) = DEZZ(I) - MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(YLD(I),EM20)
        ELSE
          DEZZ(I) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG(I) + DEZZ(I)
        ENDIF
        ! Computation of the thickness variation  
        THK(I) = THK(I) + DEZZ(I)*THKLY(I)*OFF(I)  
      ENDDO
c      
      END