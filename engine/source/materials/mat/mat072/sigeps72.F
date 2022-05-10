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
C  --New-- Hill model + MMC failure model
Chd|====================================================================
Chd|  SIGEPS72                      source/materials/mat/mat072/sigeps72.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS72(
     1     NEL      ,NUPARAM  ,NUVAR    ,
     2     TIME     ,TIMESTEP ,UPARAM   ,RHO0     ,RHO      ,
     3     DEPSXX   ,DEPSYY   ,DEPSZZ   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     4     SIG0XX   ,SIG0YY   ,SIG0ZZ   ,SIG0XY   ,SIG0YZ   ,SIG0ZX   ,
     5     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6     SOUNDSP  ,UVAR     ,OFF      ,NGL      ,YLD      ,PLA      ,
     7     DPLA     ,ETSE     ,SEQ      ,DMG      ,INLOC    ,DPLANL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C
      INTEGER NEL,NUPARAM,NUVAR,IPT,NGL(NEL),INLOC
      my_real
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   SIG0XX(NEL),SIG0YY(NEL),SIG0ZZ(NEL),
     .   SIG0XY(NEL),SIG0YZ(NEL),SIG0ZX(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),YLD(NEL),ETSE(NEL),DPLA(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .    UVAR(NEL,NUVAR),OFF(NEL),PLA(NEL),SEQ(NEL),
     .    DMG(NEL),DPLANL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,NINDX,INDX(MVSIZ),NITER,ITER
      my_real
     .    E,NU,G,G2,A1,A2,BULK,LAMHOOK,
     .    SIGY,EPS0,NEXPO,
     .    FF,GG,HH,NN,LL,MM,
     .    C1,C2,C3,DC,MEXP
      my_real
     .    CC(MVSIZ),DAM(MVSIZ),BETA(MVSIZ),
     .    P,SD11,SD22,SD33,
     .    F1,F2,F3,EPSF,DET,SVM,THETA,COS3THETA,ETA,
     .    SIGHL(MVSIZ),H(MVSIZ),DAV,
     .    PHI(MVSIZ),NORMXX,NORMYY,NORMZZ,
     .    NORMXY,NORMZX,NORMYZ,DPXX(MVSIZ),DPYY(MVSIZ),
     .    DPZZ(MVSIZ),DPXY(MVSIZ),DPZX(MVSIZ),DPYZ(MVSIZ),
     .    SIG_DFDSIG,DFDSIG2,DPHI_DLAM(MVSIZ),DPLA_DLAM(MVSIZ),
     .    DLAM,DDEP
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      ! Elastic parameters
      E       = UPARAM(1)   ! Young modulus
      NU      = UPARAM(2)   ! Poisson's ratio
      G       = UPARAM(3)   ! Shear modulus
      G2      = UPARAM(4)   ! 2x Shear modulus
      A1      = UPARAM(7)   ! Diag. component of 3D elastic matrix
      A2      = UPARAM(8)   ! Non-diag. component of 3D elastic matrix
      BULK    = UPARAM(9)   ! Bulk modulus
      LAMHOOK = UPARAM(10)  ! Hooke's lamba coefficient
      ! Hardening parameter
      SIGY    = UPARAM(11)  ! Initial yield stress 
      EPS0    = UPARAM(12)  ! Initial plastic strain
      NEXPO   = UPARAM(13)  ! Hardening exponent
      ! Hill yield criterion parameters
      FF      = UPARAM(14)  ! First  Hill coefficient
      GG      = UPARAM(15)  ! Second Hill coefficient
      HH      = UPARAM(16)  ! Third  Hill coefficient
      NN      = UPARAM(17)  ! Fourth Hill coefficient
      LL      = UPARAM(18)  ! Fifth  Hill coefficient
      MM      = UPARAM(19)  ! Sixth  Hill coefficient
      ! Modified Mohr-Coulomb yield criterion parameters
      C1      = UPARAM(20)  ! First failure parameter
      C2      = UPARAM(21)  ! Second failure parameter
      C3      = UPARAM(22)  ! Third failure parameter
      MEXP    = UPARAM(23)  ! Damage exponent
      DC      = UPARAM(24)  ! Critical damage value
c 
      ! Initial value
      IF (ISIGI == 0) THEN
        IF (TIME == ZERO) THEN
          DO I=1,NEL
            PLA(I)    = EPS0
            UVAR(I,1) = ZERO                              
          ENDDO
        ENDIF
      ENDIF
c
      ! Recovering internal variables
      DO I=1,NEL
        ! Checking deletion flag value
        IF (OFF(I) < UN)   OFF(I) = QCINQ*OFF(I)
        IF (OFF(I) < EM01) OFF(I) = ZERO
        ! Hourglass coefficient
        ETSE(I) = UN
        H(I)    = ZERO
        ! Plastic strain increment
        DPLA(I) = ZERO
        DPXX(I) = ZERO
        DPYY(I) = ZERO
        DPZZ(I) = ZERO
        DPXY(I) = ZERO
        DPYZ(I) = ZERO
        DPZX(I) = ZERO
        ! Damage variable
        DAM(I)  = UVAR(I,1)
      ENDDO
c
      ! Computing yield stress and checking damage criteria
      DO I = 1,NEL
        ! Computation of the BETA factor
        BETA(I) = UN
        IF (OFF(I) == UN) THEN
          IF ((DAM(I) <= DC) .AND. (DAM(I) >= UN)) THEN
            BETA(I) = UN/(MAX(DC - UN,EM20))
            BETA(I) = (DC - DAM(I))*BETA(I)
            BETA(I) = BETA(I)**MEXP
          ENDIF
        ENDIF       
        ! Computation of the yield stress
        CC(I)  = MAX(EM20,PLA(I))
        YLD(I) = BETA(I)*SIGY*EXP(NEXPO*LOG(CC(I)))
        YLD(I) = MAX(YLD(I), EM10)
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !======================================================================== 
      DO I = 1,NEL
c    
        ! Computation of the trial stress tensor
        DAV = (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * LAMHOOK
        SIGNXX(I) = SIG0XX(I) + DEPSXX(I)*G2 + DAV
        SIGNYY(I) = SIG0YY(I) + DEPSYY(I)*G2 + DAV
        SIGNZZ(I) = SIG0ZZ(I) + DEPSZZ(I)*G2 + DAV
        SIGNXY(I) = SIG0XY(I) + DEPSXY(I)*G
        SIGNYZ(I) = SIG0YZ(I) + DEPSYZ(I)*G
        SIGNZX(I) = SIG0ZX(I) + DEPSZX(I)*G
C
        ! Hill yield stress
        SIGHL(I) = FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2 +
     .             HH*(SIGNXX(I) - SIGNYY(I))**2 + DEUX*LL*SIGNYZ(I)**2 +
     .             DEUX*MM*SIGNZX(I)**2 + DEUX*NN*SIGNXY(I)**2
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
      INDX  = 0
      DO I=1,NEL         
        IF ((PHI(I)>ZERO).AND.(OFF(I) == UN)) THEN
          NINDX = NINDX+1
          INDX(NINDX) = I
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
          I = INDX(II)
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
          
          NORMXX = (GG*(SIGNXX(I)-SIGNZZ(I)) + HH*(SIGNXX(I)-SIGNYY(I)))/(MAX(SIGHL(I),EM20))
          NORMYY = (FF*(SIGNYY(I)-SIGNZZ(I)) + HH*(SIGNYY(I)-SIGNXX(I)))/(MAX(SIGHL(I),EM20))
          NORMZZ = (FF*(SIGNZZ(I)-SIGNYY(I)) + GG*(SIGNZZ(I)-SIGNXX(I)))/(MAX(SIGHL(I),EM20))
          NORMXY = DEUX*NN*SIGNXY(I)/(MAX(SIGHL(I),EM20))
          NORMYZ = DEUX*LL*SIGNYZ(I)/(MAX(SIGHL(I),EM20))
          NORMZX = DEUX*MM*SIGNZX(I)/(MAX(SIGHL(I),EM20))
c          
          ! 2 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c        
          !   a) Derivative with respect stress increments tensor DSIG			
          !   --------------------------------------------------------
          DFDSIG2 = NORMXX * NORMXX * G2
     .            + NORMYY * NORMYY * G2
     .            + NORMZZ * NORMZZ * G2
     .            + NORMXY * NORMXY * G
     .            + NORMYZ * NORMYZ * G
     .            + NORMZX * NORMZX * G  
c
          !   b) Derivatives with respect to plastic strain P 			
          !   ------------------------------------------------  
c          
          !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
          !     ----------------------------------------------------------------------------
          H(I) = BETA(I)*NEXPO*SIGY*EXP((NEXPO-1)*LOG(CC(I)))
c          
          !     ii) Derivative of dPLA with respect to DLAM
          !     -------------------------------------------  
          SIG_DFDSIG = SIGNXX(I) * NORMXX
     .               + SIGNYY(I) * NORMYY
     .               + SIGNZZ(I) * NORMZZ
     .               + SIGNXY(I) * NORMXY
     .               + SIGNYZ(I) * NORMYZ
     .               + SIGNZX(I) * NORMZX  
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
          DPZZ(I) = DLAM * NORMZZ
          DPXY(I) = DLAM * NORMXY
          DPYZ(I) = DLAM * NORMYZ
          DPZX(I) = DLAM * NORMZX  
c          
          ! Elasto-plastic stresses update
          SIGNXX(I) = SIGNXX(I) - DPXX(I)*G2
          SIGNYY(I) = SIGNYY(I) - DPYY(I)*G2
          SIGNZZ(I) = SIGNZZ(I) - DPZZ(I)*G2
          SIGNXY(I) = SIGNXY(I) - DPXY(I)*G
          SIGNYZ(I) = SIGNYZ(I) - DPYZ(I)*G
          SIGNZX(I) = SIGNZX(I) - DPZX(I)*G
c          
          ! Cumulated plastic strain and strain rate update           
          DDEP    = DLAM*DPLA_DLAM(I)
          DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
          PLA(I)  = PLA(I) + DDEP
c
          ! Update the hardening yield stress
          CC(I)  = MAX(EM20,PLA(I))
          YLD(I) = BETA(I)*SIGY*EXP(NEXPO*LOG(CC(I)))
          YLD(I) = MAX(YLD(I),EM10)
c
          ! Update Hill equivalent stress
          SIGHL(I) = FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2 +
     .               HH*(SIGNXX(I) - SIGNYY(I))**2 + DEUX*LL*SIGNYZ(I)**2 +
     .               DEUX*MM*SIGNZX(I)**2 + DEUX*NN*SIGNXY(I)**2
          SIGHL(I) = SQRT(MAX(SIGHL(I),ZERO))
c
          ! Update yield function value
          PHI(I)   = SIGHL(I) - YLD(I)
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
      NINDX = 0
      INDX  = 0
      DO I=1,NEL
        ! Modified Mohr Criteria Failure Model
        IF ((OFF(I) == UN).AND.(DPLA(I) > ZERO)) THEN
          ! Pressure
          P    = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          ! Von Mises equivalent stress
          SD11 = SIGNXX(I) - P
          SD22 = SIGNYY(I) - P
          SD33 = SIGNZZ(I) - P
          SVM  = UNDEMI*(SD11**2 + SD22**2 + SD33**2) + 
     +                   SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
          SVM  = SQRT(MAX(TROIS*SVM,ZERO))
          ! Determinant of the deviatoric stress tensor
          DET  = SD11*SD22*SD33 + DEUX*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
     .         - SD11*SIGNYZ(I)**2-SD33*SIGNXY(I)**2 - SD22*SIGNZX(I)**2
          ! Computation of triaxiality
          ETA = P/MAX(SVM,EM20)
          IF (ETA < -UN) ETA = -UN
          IF (ETA > UN ) ETA = UN
          ! Computation of Lode angle and Lode parameter
          COS3THETA = UNDEMI*VINGT7*DET/MAX(EM20,SVM**3)
          IF (COS3THETA < -UN) COS3THETA = -UN
          IF (COS3THETA > UN)  COS3THETA = UN
          THETA = UN - DEUX*ACOS(COS3THETA)/PI
C 
          ! Computation of failure coefficient
          F1 = COS(THETA*PI/SIX)
          F2 = SIN(THETA*PI/SIX) 
          F3 = C3 + (SQRT(TROIS)/(DEUX - SQRT(TROIS)))*(UN - C3)*(UN/MAX(F1,EM20) - UN)
C
          ! Computation of the failure plastic strain
          EPSF = (SIGY/MAX(C2,EM20))*F3*(F1*SQRT(THIRD*(UN + C1**2)) + C1*(ETA + F2*THIRD))
          EPSF = MAX(EPSF,EM20)**(-UN/NEXPO)
C           
          ! Computation of the damage variable
          IF (INLOC > 0) THEN 
            DAM(I) = DAM(I) + MAX(DPLANL(I),ZERO)/MAX(EPSF,EM20)
          ELSE
            DAM(I) = DAM(I) + DPLA(I)/MAX(EPSF,EM20)
          ENDIF
          IF (DAM(I) >= DC) THEN
            DAM(I) = DC
            OFF(I) = QCINQ
            IDEL7NOK = 1
            NINDX = NINDX+1
            INDX(NINDX)=I
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
        SOUNDSP(I) = SQRT((BULK + QTIERS*G)/RHO0(I)) 
        ! Coefficient for hourglass
        ETSE(I)    = H(I) / (H(I) + E)
      ENDDO 
C
      ! Printout element deletion
      IF(NINDX.GT.0)THEN
        DO J=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(J))
          WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
!---
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',G11.4)                          
!---
      END
C