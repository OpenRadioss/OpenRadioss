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
Chd|  SIGEPS72C                     source/materials/mat/mat072/sigeps72c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS72C(
     1     NEL      ,NUPARAM  ,NUVAR    ,
     2     TIME     ,TIMESTEP ,UPARAM   ,RHO0     ,THKLY    ,
     3     DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     4     SIGOXX   ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     5     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6     SOUNDSP  ,THK      ,PLA      ,UVAR     ,OFF      ,
     7     ETSE     ,GS       ,YLD      ,HARDM    ,SEQ      ,
     8     DPLA     ,DMG      ,INLOC    ,DPLANL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL,NUPARAM,NUVAR,INLOC
      my_real
     .   TIME,TIMESTEP(NEL),UPARAM(NUPARAM),
     .   RHO0(NEL),THKLY(NEL),PLA(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   GS(*),HARDM(NEL),SEQ(NEL),DMG(NEL),
     .   DPLANL(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),ETSE(NEL),DPLA(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .    UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,NMAX,NINDX,INDEX(MVSIZ),NITER,ITER
      my_real
     . E,NU,G,A11,A12,
     . SIGY,EPS0,NEXP,
     . FF,GG,HH,NN,
     . C1,C2,C3,MEXP,DC
      my_real
     . ETA,COS3THETA,THETA,F1,F2,F3,CC(MVSIZ),
     . BETA(MVSIZ),DAM(MVSIZ),DEZZ(MVSIZ),
     . EPSF,P,SIGHL(MVSIZ),H(MVSIZ),SVM(MVSIZ),
     . NORMXX,NORMYY,NORMXY,DPXX(MVSIZ),DPYY(MVSIZ),
     . DPZZ(MVSIZ),DPXY(MVSIZ),DEELZZ(MVSIZ),
     . DPLA_DLAM(MVSIZ),DLAM,SIG_DFDSIG,DFDSIG2,
     . PHI(MVSIZ),DPHI_DLAM(MVSIZ),DDEP
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C             
      ! Elastic parameters
      E    = UPARAM(1)   ! Young modulus
      NU   = UPARAM(2)   ! Poisson's ratio
      G    = UPARAM(3)   ! Shear modulus
      A11  = UPARAM(5)   ! Diag. component of plane stress elastic matrix
      A12  = UPARAM(6)   ! Non diag component of plane stress elastic matrix
      ! Hardening parameters
      SIGY = UPARAM(11)  ! Initial yield stress 
      EPS0 = UPARAM(12)  ! Initial plastic strain (> 0)
      NEXP = UPARAM(13)  ! Hardening exponent
      ! Hill yield criterion parameters
      FF   = UPARAM(14)  ! First  Hill coefficient
      GG   = UPARAM(15)  ! Second Hill coefficient
      HH   = UPARAM(16)  ! Third  Hill coefficient
      NN   = UPARAM(17)  ! Fourth Hill coefficient
      ! Modified Mohr-Coulomb yield criterion parameters
      C1   = UPARAM(20)  ! First failure parameter
      C2   = UPARAM(21)  ! Second failure parameter
      C3   = UPARAM(22)  ! Third failure parameter
      MEXP = UPARAM(23)  ! Damage exponent
      DC   = UPARAM(24)  ! Critical damage value
c 
      ! Initial value
      IF (ISIGI == 0) THEN
        IF (TIME == ZERO) THEN
          DO I=1,NEL
            UVAR(I,1) = ZERO                              
          ENDDO
        ENDIF
      ENDIF
c
      ! Recovering internal variables
      DO I=1,NEL
        ! Checking deletion flag value
        IF (OFF(I) < ONE)  OFF(I) = FOUR_OVER_5*OFF(I)
        IF (OFF(I) < EM01) OFF(I) = ZERO
        ! Hourglass coefficient
        ETSE(I) = ONE
        H(I)    = ZERO
        ! Plastic strain increment
        DPLA(I) = ZERO
        DPXX(I) = ZERO
        DPYY(I) = ZERO
        DPZZ(I) = ZERO
        DPXY(I) = ZERO
        ! Damage variable
        DAM(I)  = UVAR(I,1)
      ENDDO       
c
      ! Computing yield stress and checking damage criteria
      DO I = 1,NEL
        ! Computation of the BETA factor
        BETA(I) = ONE
        IF (OFF(I) == ONE) THEN
          IF ((DAM(I) <= DC) .AND. (DAM(I) >= ONE)) THEN
            BETA(I) = ONE/(MAX(DC - ONE,EM20))
            BETA(I) = (DC - DAM(I))*BETA(I)
            BETA(I) = BETA(I)**MEXP
          ENDIF
        ENDIF       
        ! Computation of the yield stress
        CC(I)  = PLA(I) + EPS0
        YLD(I) = BETA(I)*SIGY*EXP(NEXP*LOG(CC(I)))
        YLD(I) = MAX(YLD(I), EM10)
      ENDDO      
c      
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !======================================================================== 
      DO I = 1,NEL
c    
        ! Computation of the trial stress tensor
        SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I)+A12*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A12*DEPSXX(I)+A11*DEPSYY(I)
        SIGNXY(I) = SIGOXY(I) + G*DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + GS(I)*DEPSZX(I)
C
        ! Hill equivalent stress
        SIGHL(I) = (FF+HH)*SIGNYY(I)**2 + (GG+HH)*SIGNXX(I)**2
     .           - TWO*HH*SIGNXX(I)*SIGNYY(I) + TWO*NN*SIGNXY(I)**2    
        SIGHL(I) = SQRT(MAX(ZERO,SIGHL(I)))
C        
      ENDDO 
c
      !========================================================================
      ! - COMPUTATION OF YIELD FONCTION
      !========================================================================
      PHI(1:NEL) = SIGHL(1:NEL) - YLD(1:NEL)
      ! Checking plastic behavior for all elements
      NINDX = 0
      INDEX = 0
      DO I=1,NEL         
        IF ((PHI(I)>ZERO).AND.(OFF(I) == ONE)) THEN
          NINDX = NINDX+1
          INDEX(NINDX) = I
        ENDIF
      ENDDO
c      
      !====================================================================
      ! - PLASTIC CORRECTION WITH CUTTING PLANE ALGORITHM (NEWTON ITERATION)
      !====================================================================       
c      
      ! Number of maximum Newton iterations
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
          NORMXX = (GG*SIGNXX(I) + HH*(SIGNXX(I)-SIGNYY(I)))/(MAX(SIGHL(I),EM20))
          NORMYY = (FF*SIGNYY(I) + HH*(SIGNYY(I)-SIGNXX(I)))/(MAX(SIGHL(I),EM20))
          NORMXY = TWO*NN*SIGNXY(I)/(MAX(SIGHL(I),EM20))
c          
          ! 2 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c        
          !   a) Derivative with respect stress increments tensor DSIG			
          !   --------------------------------------------------------
          DFDSIG2 = NORMXX * (A11*NORMXX + A12*NORMYY)
     .            + NORMYY * (A11*NORMYY + A12*NORMXX)
     .            + NORMXY * NORMXY * G         
c
          !   b) Derivatives with respect to plastic strain P 			
          !   ------------------------------------------------  
c          
          !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
          !     ----------------------------------------------------------------------------
          H(I) = BETA(I)*NEXP*SIGY*EXP((NEXP-1)*LOG(CC(I)))
c
          !     ii) Derivative of dPLA with respect to DLAM
          !     -------------------------------------------   
          SIG_DFDSIG = SIGNXX(I) * NORMXX
     .               + SIGNYY(I) * NORMYY
     .               + SIGNXY(I) * NORMXY          
          DPLA_DLAM(I) = SIG_DFDSIG / MAX(YLD(I),EM20)
c
          ! 3 - Computation of plastic multiplier and variables update
          !----------------------------------------------------------
c          
          ! Derivative of PHI with respect to DLAM
          DPHI_DLAM(I) = - DFDSIG2 - H(I)*DPLA_DLAM(I)
          DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20) ,DPHI_DLAM(I))
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
          SIGNXX(I) = SIGNXX(I) - (A11*DPXX(I) + A12*DPYY(I))
          SIGNYY(I) = SIGNYY(I) - (A11*DPYY(I) + A12*DPXX(I))
          SIGNXY(I) = SIGNXY(I) - DPXY(I)*G
c          
          ! Cumulated plastic strain and strain rate update           
          DDEP    = DLAM*DPLA_DLAM(I)
          DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
          PLA(I)  = MAX(PLA(I) + DDEP,ZERO) 
c
          ! Update the hardening yield stress
          CC(I)  = PLA(I) + EPS0
          YLD(I) = BETA(I)*SIGY*EXP(NEXP*LOG(CC(I)))
          YLD(I) = MAX(YLD(I),EM10)
c
          ! Update Hill equivalent stress          
          SIGHL(I) = (FF+HH)*SIGNYY(I)**2 + (GG+HH)*SIGNXX(I)**2
     .             - TWO*HH*SIGNXX(I)*SIGNYY(I) + TWO*NN*SIGNXY(I)**2    
          SIGHL(I) = SQRT(MAX(ZERO,SIGHL(I)))          
c
          ! Update yield function value
          PHI(I)   = SIGHL(I) - YLD(I)
c      
          ! Transverse strain update
          DPZZ(I) = DPZZ(I) - (DPXX(I)+DPYY(I)) 
c
        ENDDO
        ! End of the loop over the yielding elements
      ENDDO
      ! End of the loop over the iterations 
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE ALGORITHM
      !===================================================================          
C    
      ! Update and store new internal variables
      DO I=1,NEL
        ! Modified Mohr Criteria Failure Model
        IF (OFF(I) == ONE) THEN
          ! Pressure
          P = THIRD*(SIGNXX(I) + SIGNYY(I))
          ! Von Mises equivalent stress
          SVM(I) = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I) +
     .             THREE*SIGNXY(I)**2
          SVM(I) = SQRT(SVM(I))
C
          ! Computation of triaxiality
          ETA = P/MAX(SVM(I),EM20)
          IF (ETA < -TWO_THIRD) ETA = -TWO_THIRD
          IF (ETA > TWO_THIRD)  ETA =  TWO_THIRD
          ! Computation of Lode angle
          COS3THETA = -HALF*TWENTY7*ETA*(ETA**2 - THIRD)
          IF (COS3THETA < -ONE ) COS3THETA = -ONE
          IF (COS3THETA > ONE )  COS3THETA =  ONE
          THETA = ONE - TWO*ACOS(COS3THETA)/PI
C             
          ! Computation of the failure factor
          F1 = COS(THETA*PI/SIX)
          F2 = SIN(THETA*PI/SIX)
          F3 = C3 + (SQRT(THREE)/(TWO - SQRT(THREE)))*(ONE - C3)*(ONE/MAX(F1,EM20) - ONE)
C
          ! Computation of the failure plastic strain
          EPSF = (SIGY/MAX(C2,EM20))*F3*(F1*SQRT(THIRD*(ONE + C1**2)) + C1*(ETA + F2*THIRD))
          EPSF = MAX(EPSF,EM20)**(-ONE/NEXP)
C               
          ! Computation of the damage variable
          IF (INLOC > 0) THEN 
            DAM(I) = DAM(I) + MAX(DPLANL(I),ZERO)/MAX(EM20,EPSF)
          ELSE
            DAM(I) = DAM(I) + DPLA(I)/MAX(EM20,EPSF)
          ENDIF
          IF (DAM(I) >= DC) THEN 
            DAM(I) = DC
            OFF(I) = FOUR_OVER_5
          ENDIF
C
          ! Store the new value of damage
          UVAR(I,1) = DAM(I) 
        ENDIF
        ! Equivalent stress
        SEQ(I)     = SIGHL(I)
        ! Normalized damage
        DMG(I)     = DAM(I)/DC
        ! Sound-speed
        SOUNDSP(I) = SQRT(A11/RHO0(I)) 
        ! Coefficient for hourglass
        ETSE(I)    = H(I) / (H(I) + E)
        HARDM(I)   = H(I)
        ! Computation of the thickness variation 
        DEELZZ(I)  = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E
        IF (INLOC > 0) THEN 
          NORMXX   = (GG*SIGNXX(I) + HH*(SIGNXX(I)-SIGNYY(I)))/(MAX(SIGHL(I),EM20))
          NORMYY   = (FF*SIGNYY(I) + HH*(SIGNYY(I)-SIGNXX(I)))/(MAX(SIGHL(I),EM20))
          DEZZ(I)  = DEELZZ(I) - MAX(DPLANL(I),ZERO)*(NORMXX+NORMYY)
        ELSE
          DEZZ(I)  = DEELZZ(I) + DPZZ(I)
        ENDIF
        THK(I)     = THK(I) + DEZZ(I)*THKLY(I)*OFF(I)  
      ENDDO     
C
      END
C