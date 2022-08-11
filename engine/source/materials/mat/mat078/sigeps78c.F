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
Chd|  SIGEPS78C                     source/materials/mat/mat078/sigeps78c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS78C(
     1     NEL     ,NUPARAM ,NUVAR   ,NVARTMP ,TIME    ,
     2     NFUNC   ,IFUNC   ,NPF     ,TF      ,UPARAM  ,
     3     THKLY   ,THK     ,GS      ,ETSE    ,SIGY    ,
     4     DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     6     SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7     SOUNDSP ,UVAR    ,SIGA    ,SIGB    ,SIGC    ,
     8     RHO     ,OFF     ,PLA     ,DEP     ,VARTMP  ,
     9     INLOC   ,DPLANL  ,ETIMP   ,SEQ     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "scr17_c.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include "param_c.inc"
#include "com01_c.inc"
#include "vect01_c.inc"
#include "units_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,
     .                       NVARTMP,INLOC
      my_real ,INTENT(IN) :: 
     . TIME
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN) :: 
     . UPARAM
      my_real ,DIMENSION(NEL)     ,INTENT(IN) :: 
     .         RHO,THKLY,OFF,GS,DPLANL,
     .         DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .         SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      INTEGER ,INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real ,DIMENSION(NEL,3)     ,INTENT(INOUT) :: 
     . SIGA,SIGB
      my_real ,DIMENSION(NEL,4)     ,INTENT(INOUT) ::
     . SIGC 
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: 
     . UVAR
      my_real ,DIMENSION(NEL)       ,INTENT(INOUT) :: 
     . PLA,THK
      my_real ,DIMENSION(NEL)       ,INTENT(OUT)   :: 
     .   SOUNDSP,DEP,ETSE,SIGY,SEQ,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,ETIMP
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real 
     .     TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,OPTE,OPTR,NITER,NINDX,IPLAS,ITER
      INTEGER ,DIMENSION(NEL) :: INDEX,IPOS,ILEN,IAD2
      my_real :: 
     .        NU,BSAT,EINF,COE,ASTA,FCT,GSIGMA,DGSIGMA,RDOT,
     .        EINI,YIELD,BYU,MYU,HYU,CYU,RSAT,P1,P2,P3,P4,N3,CST,
     .        CSTT,DF_DK1,DF_DK2,DK1_DSIGXX,DK1_DSIGYY,DK2_DSIGXX,
     .        DK2_DSIGYY,DK2_DSIGXY,NORMXX,NORMYY,NORMXY,DFDSIG_C_2,
     .        DFDSIG_A,DFDSIG_ALPHA,DFDSIG_BETA,DPHI_DLAM,DPLA_DLAM,
     .        DRNIH,DMU,C1_KH,C2_KH,NORM_SP
      REAL(KIND=8) :: DLAM,DEP_DP(NEL)
      my_real ,DIMENSION(NEL) :: 
     .        YOUNG,SHEAR,AYU,SCALEE,H,A1,A2,AXX,AYY,AXY,K1,K2,
     .        SEFF,R,RNIH,DEPSZZ,DEELZZ,DEVBOXX,DEVBOYY,DEVBOZZ,DEVBOXY,DYDXE,
     .        DEPLXX,DEPLYY,DEPLXY,DEPLZZ,DEVBXX,DEVBYY,DEVBZZ,DEVBXY,PHI,
     .        DBXX,DBYY,DBZZ,DBXY,BQXX,BQYY,BQZZ,BQXY,YLD,MAX_ASTA
C=======================================================================
C     SIGA  (alpha) = CENTER OF YIELD SURFACE ALPHA (Backstress)
C     SIGB  (beta)  = CENTER OF BOUNDING SURFACE
C     SIGC  (q)     = CENTER OF NON-IH SURFACE G_SIGMA
c
C     UVAR(I,1)     = R : ISOTROPIC HARDENING OF BOUNDING SURFACE
C     UVAR(I,2)     = r : RADIUS OF G_SIGMA (RNIH)
C     UVAR(I,3)     = a = B + R - YIELD     (AYU) 
C     UVAR(I,4)     = Yld Stress
C=======================================================================
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      EINI  = UPARAM(1)             ! Initial Young Modulus
      NU    = UPARAM(2)             ! Poisson's ratio
      YIELD = UPARAM(3)             ! Yield stress
      BYU   = UPARAM(4)             ! Center of the bounding surface
      C2_KH = UPARAM(5)             ! Parameter for kinematic hardening rule of yield surface (Lower case if C1_KH is defined)
      HYU   = UPARAM(6)             ! Material parameter for controlling work hardening stagnation
      BSAT  = UPARAM(7)             ! Initial size of the bounding surface
      MYU   = UPARAM(8)             ! Parameter for isotropic and kinematic hardening of the bounding surface
      RSAT  = UPARAM(9)             ! Saturated value of the isotropic hardening stress
      EINF  = UPARAM(10)            ! Asymptotic value of Young's modulus
      COE   = UPARAM(11)            ! Parameter controlling the dependency of Young's modulus on the effective plastic strain
      OPTE  = UPARAM(12)            ! Modified Young's modulus
      OPTR  = UPARAM(13)            ! Modified isotropic hardening rule flag
      P1    = UPARAM(14)            ! First yield criterion parameter
      P2    = UPARAM(15)            ! First yield criterion parameter
      P3    = UPARAM(16)            ! First yield criterion parameter
      P4    = UPARAM(17)            ! First yield criterion parameter
      N3    = UPARAM(18)            ! Barlat 1989 exponent
      CST   = UPARAM(19)            ! Constant used in the modified formulation of the isotropic hardening of bounding surface
      CSTT  = UPARAM(20)            !
      IPLAS = NINT(UPARAM(21))      ! Flag for choosing yield criterion (Hill 1948 - Barlat 1989)
      C1_KH = UPARAM(22)            ! Upper case for controlling work hardening stagnation
c      
      IF (IPLAS == 1) THEN 
        NORM_SP = ONE
      ELSEIF(IPLAS == 2) THEN
        NORM_SP = EP03 / EINI
      ENDIF
      ! Initial value
      IF (ISIGI == 0 .and. TIME == ZERO) THEN
        DO I=1,NEL
          ! Initial AYU value
          UVAR(I,3) = BSAT - YIELD
          ! Initial yield stress
          UVAR(I,4) = YIELD
        ENDDO
      ENDIF   
C
      ! Update of Young modulus
      !  -> Constant young modulus
      IF (COE == ZERO .and. IFUNC(1) == 0) THEN
        YOUNG(1:NEL)    = EINI
      !  -> Tabulated young modulus
      ELSE IF (OPTE == 1) THEN
        IPOS(1:NEL)     = VARTMP(1:NEL,1)
        IAD2(1:NEL)     = NPF(IFUNC(1)) / 2 + 1
        ILEN(1:NEL)     = NPF(IFUNC(1)+1) / 2 - IAD2(1:NEL) - IPOS(1:NEL)
        CALL VINTER(TF,IAD2,IPOS,ILEN,NEL,PLA,DYDXE,SCALEE) 
        YOUNG(1:NEL)    = EINI * SCALEE(1:NEL)    
        VARTMP(1:NEL,1) = IPOS(1:NEL)
      !  -> Non-linear young modulus
      ELSE 
        DO I=1,NEL
          IF (PLA(I) > ZERO)THEN
            YOUNG(I) = EINI-(EINI-EINF)*(ONE-EXP(-COE*PLA(I))) 
          ELSE          
            YOUNG(I) = EINI
          ENDIF
        ENDDO
      ENDIF
C
      ! Recovering elastic parameters and hardening variables
      DO I=1,NEL 
        A1(I)       = YOUNG(I)/(ONE-NU*NU)     ! Plane stress elastic matrix diagonal component
        A2(I)       = NU*YOUNG(I)/(ONE-NU*NU)  ! Plane stress elastic matrix non-diagonal component
        SHEAR(I)    = YOUNG(I)*HALF/(ONE+NU) ! Shear modulus
        R(I)        = UVAR(I,1)               ! Isotropic hardening
        RNIH(I)     = UVAR(I,2)               ! Non-IH hardening
        AYU(I)      = UVAR(I,3)               ! AYU variable
        MAX_ASTA(I) = UVAR(I,6)               ! Maximal value of NORM(ALPHA_STAR)
        DEP_DP(I)   = ZERO
        DEPLXX(I)   = ZERO
        DEPLYY(I)   = ZERO
        DEPLXY(I)   = ZERO
        DEPLZZ(I)   = ZERO
        ETIMP(I)    = YOUNG(I)/EINI        ! For implicit
      ENDDO
c      
      !========================================================================
      ! - COMPUTATION OF TRIAL VALUES
      !========================================================================
      DO I=1,NEL
        ! Trial stress tensor
        SIGNXX(I) = SIGOXX(I) + A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I) 
        SIGNYY(I) = SIGOYY(I) + A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
        SIGNXY(I) = SIGOXY(I) + SHEAR(I) * DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + GS(I) * DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + GS(I) * DEPSZX(I)
        ! Update with the back stress (Backstress ALPHA = SIGA + SIGB)
        AXX(I)    = SIGNXX(I) - SIGA(I,1) - SIGB(I,1)
        AYY(I)    = SIGNYY(I) - SIGA(I,2) - SIGB(I,2)
        AXY(I)    = SIGNXY(I) - SIGA(I,3) - SIGB(I,3)
        ! Choice of yield criterion
        !   -> Hill 1948
        IF (IPLAS == 1) THEN 
          SEFF(I) = SQRT(AXX(I)**2 - TWO*P2*AXX(I)*AYY(I) + P1*AYY(I)**2 + P3*AXY(I)**2)
        !   -> Barlat 1989
        ELSEIF (IPLAS == 2) THEN
          K1(I)   = (AXX(I) + P3*AYY(I))*NORM_SP/TWO
          K2(I)   = (SQRT(((AXX(I) - P3*AYY(I))/TWO)**TWO + (P4*AXY(I))**TWO))*NORM_SP
          SEFF(I) = P1*ABS(K1(I)+K2(I))**N3 + P1*ABS(K1(I)-K2(I))**N3 + P2*ABS(TWO*K2(I))**N3
          SEFF(I) = (HALF*MAX(SEFF(I),EM20))**(ONE/N3)
        ENDIF
        ! Yield function value
        PHI(I)    = SEFF(I) - YIELD*NORM_SP
        ! Initialization of variable
        DEP_DP(I) = ZERO      ! Plastic strain increment
        ETSE(I)   = ONE        ! For hourglassing
        DEPLZZ(I) = ZERO      ! Plastic strain increment for thickness variation
      ENDDO
c
      !========================================================================
      ! - CHECKING PLASTIC BEHAVIOR
      !========================================================================
      NINDX = 0
      DO I=1,NEL
        IF (PHI(I) >= ZERO .and. OFF(I) == ONE) THEN
          NINDX = NINDX+1
          INDEX(NINDX) = I
        ENDIF
      ENDDO
c
      !====================================================================
      ! - PLASTIC CORRECTION WITH CUTTING PLANE ALGORITHM
      !====================================================================    
c      
      ! Number of Newton iterations
      NITER = 3
c
      ! Loop over the iterations
      DO ITER = 1,NITER
#include "vectorize.inc"
        ! Loop over yielding elements 
        DO J=1,NINDX
          ! Number of the element with plastic behaviour   
          I = INDEX(J)         
c        
          ! Note     : in this part, the purpose is to compute for each iteration
          ! a plastic multiplier allowing to update internal variables to satisfy
          ! the consistency condition using the cutting plane method
          ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
	  ! -> PHI          : current value of yield function (known)
          ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
          !                   into account of internal variables kinetic : 
          !                   hardening parameters
C
          ! 1 - Computation of the normal to the yield surface
          !---------------------------------------------------------
          !   -> Hill 1948
          IF (IPLAS == 1) THEN 
            NORMXX = (AXX(I)-P2*AYY(I))/(MAX(SEFF(I),EM20))
            NORMYY = (P1*AYY(I)-P2*AXX(I))/(MAX(SEFF(I),EM20))
            NORMXY = (P3*AXY(I))/(MAX(SEFF(I),EM20))
          !   -> Barlat 1989
          ELSEIF (IPLAS == 2) THEN
            !  Computation of the derivatives
            DF_DK1 = (SEFF(I)**(1-N3))*(P1/TWO)*(
     .                        +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(N3-1)) 
     .                        +  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(N3-1)))
            DF_DK2 = (SEFF(I)**(1-N3))*((P1/TWO)*(
     .                        +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(N3-1)) 
     .                        -  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(N3-1))) 
     .                        +  P2*(ABS(TWO*K2(I))**(N3-1)))
            DK1_DSIGXX = HALF
            DK1_DSIGYY = P3/TWO
            DK2_DSIGXX = (AXX(I)-P3*AYY(I))*NORM_SP/(MAX(FOUR*K2(I),EM20))
            DK2_DSIGYY = -P3*(AXX(I)-P3*AYY(I))*NORM_SP/(MAX(FOUR*K2(I),EM20))
            DK2_DSIGXY = (P4**TWO)*AXY(I)*NORM_SP/MAX(K2(I),EM20)
            !  Assembling the normal
            NORMXX     = DF_DK1*DK1_DSIGXX + DF_DK2*DK2_DSIGXX
            NORMYY     = DF_DK1*DK1_DSIGYY + DF_DK2*DK2_DSIGYY
            NORMXY     = DF_DK2*DK2_DSIGXY
          ENDIF
c          
          ! 2 - Computation of DPHI_DLAMBDA
          !---------------------------------------------------------
c        
          ! Computation of the tensor product Norm * Celas * Norm
          DFDSIG_C_2 = NORMXX * (A1(I)*NORMXX + A2(I)*NORMYY)
     .               + NORMYY * (A1(I)*NORMYY + A2(I)*NORMXX)
     .               + NORMXY * NORMXY * SHEAR(I)
c
          ! Computation of the tensor product Norm * (SigN - Alpha)
          DFDSIG_A   = NORMXX * AXX(I)
     .               + NORMYY * AYY(I)
     .               + NORMXY * AXY(I)
c
          ! Computation of the tensor product Norm * Alpha*     
          DFDSIG_ALPHA = NORMXX * SIGA(I,1)
     .                 + NORMYY * SIGA(I,2)
     .                 + NORMXY * SIGA(I,3)
c
          ! Computation of the tensor product Norm * Beta   
          DFDSIG_BETA  = NORMXX * SIGB(I,1)
     .                 + NORMYY * SIGB(I,2)
     .                 + NORMXY * SIGB(I,3)
C     
          ! Computation of the norm of Alpha* backstress
          !   -> Hill 1948
          IF (IPLAS == 1) THEN 
            ASTA = SQRT(SIGA(I,1)**2 - TWO*P2*SIGA(I,1)*SIGA(I,2) + P1*SIGA(I,2)**2 + P3*SIGA(I,3)**2)
          !   -> Barlat 1989
          ELSEIF (IPLAS == 2) THEN            
            K1(I)= (SIGA(I,1) + P3*SIGA(I,2))/TWO
            K2(I)= SQRT(((SIGA(I,1) - P3*SIGA(I,2))/TWO)**TWO + (P4*SIGA(I,3))**TWO)
            ASTA = P1*ABS(K1(I)+K2(I))**N3 + P1*ABS(K1(I)-K2(I))**N3 + P2*ABS(TWO*K2(I))**N3
            ASTA = (HALF*MAX(EM20,ASTA))**(ONE/N3)
          ENDIF
          ASTA = MAX(ASTA,EM20)
          MAX_ASTA(I) = MAX(MAX_ASTA(I),ASTA)
          IF (MAX_ASTA(I) < BSAT - YIELD) THEN
            CYU = C1_KH
          ELSE
            CYU = C2_KH
          ENDIF
C
          ! Assembling the derivative
          DPLA_DLAM = DFDSIG_A/YIELD
          DPHI_DLAM = (- DFDSIG_C_2  
     .                - (CYU*AYU(I)/YIELD)*DFDSIG_A*DPLA_DLAM + (CYU*SQRT(AYU(I)/ASTA))*DFDSIG_ALPHA*DPLA_DLAM
     .                - (MYU*BYU/YIELD)*DFDSIG_A*DPLA_DLAM    + MYU*DFDSIG_BETA*DPLA_DLAM)* NORM_SP
          !DPHI_DLAM = DPHI_DLAM * NORM_SP
          DPHI_DLAM = SIGN(MAX(ABS(DPHI_DLAM),EM20) ,DPHI_DLAM)

          ! 3 - Computation of plastic multiplier and variables update
          !----------------------------------------------------------
c             
          ! Plastic multiplier          
          DLAM = - PHI(I)/DPHI_DLAM 
c          
          ! Plastic strains tensor update              
          DEPLXX(I) = DLAM*NORMXX
          DEPLYY(I) = DLAM*NORMYY
          DEPLXY(I) = DLAM*NORMXY
c
          ! Cumulated plastic strain update
          PLA(I)    = PLA(I) + DLAM*DPLA_DLAM
c
          ! Total plastic strain increment on the time step
          DEP_DP(I) = DEP_DP(I) + DLAM*DPLA_DLAM
          DEP_DP(I) = MAX(DEP_DP(I),ZERO)
c
          ! Cauchy stress tensor update              
          SIGNXX(I) = SIGNXX(I) - A1(I)*DEPLXX(I) - A2(I)*DEPLYY(I)                
          SIGNYY(I) = SIGNYY(I) - A2(I)*DEPLXX(I) - A1(I)*DEPLYY(I)               
          SIGNXY(I) = SIGNXY(I) - SHEAR(I)*DEPLXY(I)
c
          ! Alpha* back stress tensor update              
          SIGA(I,1) = SIGA(I,1) + ((CYU*AYU(I)/YIELD)*AXX(I) - CYU*SQRT(AYU(I)/ASTA)*SIGA(I,1))*DLAM*DPLA_DLAM
          SIGA(I,2) = SIGA(I,2) + ((CYU*AYU(I)/YIELD)*AYY(I) - CYU*SQRT(AYU(I)/ASTA)*SIGA(I,2))*DLAM*DPLA_DLAM
          SIGA(I,3) = SIGA(I,3) + ((CYU*AYU(I)/YIELD)*AXY(I) - CYU*SQRT(AYU(I)/ASTA)*SIGA(I,3))*DLAM*DPLA_DLAM 
c
          ! Old beta deviator
          DEVBOXX(I) = TWO_THIRD*SIGB(I,1) - THIRD*SIGB(I,2)
          DEVBOYY(I) = TWO_THIRD*SIGB(I,2) - THIRD*SIGB(I,1)
          DEVBOZZ(I) = -THIRD*SIGB(I,1) - THIRD*SIGB(I,2)
          DEVBOXY(I) = SIGB(I,3)
c
          ! Beta back stress tensor update
          SIGB(I,1) = SIGB(I,1) + ((MYU*BYU/YIELD)*AXX(I) - MYU*SIGB(I,1))*DLAM*DPLA_DLAM
          SIGB(I,2) = SIGB(I,2) + ((MYU*BYU/YIELD)*AYY(I) - MYU*SIGB(I,2))*DLAM*DPLA_DLAM     
          SIGB(I,3) = SIGB(I,3) + ((MYU*BYU/YIELD)*AXY(I) - MYU*SIGB(I,3))*DLAM*DPLA_DLAM 
c
          ! New beta deviator
          DEVBXX(I) = TWO_THIRD*SIGB(I,1) - THIRD*SIGB(I,2)
          DEVBYY(I) = TWO_THIRD*SIGB(I,2) - THIRD*SIGB(I,1)
          DEVBZZ(I) = -THIRD*SIGB(I,1) - THIRD*SIGB(I,2)
          DEVBXY(I) = SIGB(I,3)   
c          
          !-------------------------------------------------------------------------------------
          ! Updating the hardening
          !-------------------------------------------------------------------------------------
          ! -> Computation of the equivalent stress for the bounding surface Gsigma(DevBeta - q_n)
          BQXX(I) = DEVBXX(I) - SIGC(I,1)
          BQYY(I) = DEVBYY(I) - SIGC(I,2)
          BQZZ(I) = DEVBZZ(I) - SIGC(I,3)
          BQXY(I) = DEVBXY(I) - SIGC(I,4)
          ! -> Computation of Beta increment
          DBXX(I) = DEVBXX(I) - DEVBOXX(I)
          DBYY(I) = DEVBYY(I) - DEVBOYY(I)
          DBZZ(I) = DEVBZZ(I) - DEVBOZZ(I)
          DBXY(I) = DEVBXY(I) - DEVBOXY(I)  
          ! Computation of GSIGMA J2-type yield function
          GSIGMA  = THREE_HALF*(BQXX(I)*BQXX(I) + BQYY(I)*BQYY(I) + BQZZ(I)*BQZZ(I) + TWO*BQXY(I)*BQXY(I)) 
     .                                     - RNIH(I)*RNIH(I)  
          ! Computation of (Beta - q)*DBeta 
          DGSIGMA = BQXX(I)*DBXX(I) + BQYY(I)*DBYY(I) + BQZZ(I)*DBZZ(I) + TWO*BQXY(I)*DBXY(I)
          ! If the GSIGMA yield function is greater that 0 -> SIGC must be updated
          IF ((GSIGMA >= ZERO).AND.(DGSIGMA > ZERO)) THEN          
            ! Isotropic hardening
            IF (OPTR == ONE) THEN
              RDOT = RSAT*((PLA(I)+CST)**CSTT-CST**CSTT) - R(I)
              R(I) = RSAT*((PLA(I)+CST)**CSTT-CST**CSTT)
            ELSE 
              RDOT = MYU*(RSAT - R(I))*DLAM*DPLA_DLAM 
              R(I) = R(I) + RDOT
            ENDIF               
            AYU(I) = BSAT + R(I) - YIELD
            ! Update of q back stress tensor in case of work-hardening stagnation
            IF (HYU > ZERO) THEN
              ! Computation of DMU 
              IF (RNIH(I) == ZERO) THEN
                DMU = (GSIGMA/(THREE*HYU*DGSIGMA)) - ONE
              ELSE
                DMU = (ONE-HYU)*THREE_HALF*DGSIGMA/(RNIH(I)**2)
              ENDIF
              ! q back stress update
              SIGC(I,1) = DEVBXX(I) - BQXX(I)/(ONE+DMU)
              SIGC(I,2) = DEVBYY(I) - BQYY(I)/(ONE+DMU)
              SIGC(I,3) = DEVBZZ(I) - BQZZ(I)/(ONE+DMU)
              SIGC(I,4) = DEVBXY(I) - BQXY(I)/(ONE+DMU)
              ! -> Update of the equivalent stress for the bounding surface Gsigma(DevBeta - q_n)
              BQXX(I) = DEVBXX(I) - SIGC(I,1)
              BQYY(I) = DEVBYY(I) - SIGC(I,2)
              BQZZ(I) = DEVBZZ(I) - SIGC(I,3)
              BQXY(I) = DEVBXY(I) - SIGC(I,4)
              ! Update of (Beta - q)*DBeta 
              DGSIGMA = BQXX(I)*DBXX(I) + BQYY(I)*DBYY(I) + BQZZ(I)*DBZZ(I) + TWO*BQXY(I)*DBXY(I)
              ! Computation of RNIH evolution 
              IF (RDOT > ZERO) THEN
                IF (RNIH(I) == ZERO) THEN
                  ! Radius initial value
                  RNIH(I) = THREE*HYU*DGSIGMA
                ELSE
                  ! Radius increment
                  DRNIH = HYU*THREE_HALF*DGSIGMA/RNIH(I)
                  ! Radius update
                  RNIH(I) = RNIH(I) + DRNIH
                ENDIF
              ENDIF
            ENDIF
          ENDIF 
          !-------------------------------------------------------------------------------------
c              
          ! New equivalent stress
          AXX(I) = SIGNXX(I) - SIGA(I,1) - SIGB(I,1)
          AYY(I) = SIGNYY(I) - SIGA(I,2) - SIGB(I,2)
          AXY(I) = SIGNXY(I) - SIGA(I,3) - SIGB(I,3)
          !   -> Hill 1948
          IF (IPLAS == 1) THEN 
            SEFF(I) = SQRT(AXX(I)**2 - TWO*P2*AXX(I)*AYY(I) + P1*AYY(I)**2 + P3*AXY(I)**2)
          !   -> Barlat 1989
          ELSEIF (IPLAS == 2) THEN
            K1(I)   = (AXX(I) + P3*AYY(I))*NORM_SP/TWO
            K2(I)   = SQRT(((AXX(I) - P3*AYY(I))/TWO)**TWO + (P4*AXY(I))**TWO)*NORM_SP
            SEFF(I) = P1*ABS(K1(I)+K2(I))**N3 + P1*ABS(K1(I)-K2(I))**N3 + P2*ABS(TWO*K2(I))**N3
            SEFF(I) = (HALF*SEFF(I))**(ONE/N3)
          ENDIF            
c
          ! New yield function value
          PHI(I) = SEFF(I) - YIELD*NORM_SP
c
          ! Thickness plastic strain update
          IF (INLOC == 0) DEPLZZ(I) = DEPLZZ(I) - (DEPLXX(I)+DEPLYY(I)) 
        ENDDO
      ENDDO
      !===================================================================
      ! - END OF PLASTIC CORRECTION WITH NEWTON IMPLICIT ITERATIVE METHOD
      !===================================================================
C
      !============================================================
      ! - STORING NEW VALUES
      !============================================================
#include "vectorize.inc" 
      DO J = 1,NINDX
        ! Number of the yielding element
        I = INDEX(J)
        ! Equivalent stress
        !   -> Hill 1948
        IF (IPLAS == 1) THEN   
          YLD(I) = SQRT(SIGNXX(I)**2 - TWO*P2*SIGNXX(I)*SIGNYY(I) + P1*SIGNYY(I)**2 + P3*SIGNXY(I)**2)
        !   -> Barlat 1989
        ELSEIF (IPLAS == 2) THEN
          K1(I)  = (SIGNXX(I) + P3*SIGNYY(I))/TWO
          K2(I)  = SQRT(((SIGNXX(I) - P3*SIGNYY(I))/TWO)**TWO + (P4*SIGNXY(I))**TWO)            
          YLD(I) = P1*ABS(K1(I)+K2(I))**N3 + P1*ABS(K1(I)-K2(I))**N3 + P2*ABS(TWO*K2(I))**N3    
          YLD(I) = (HALF*MAX(YLD(I),EM20))**(ONE/N3) 
        ENDIF
        ! Hardening rate
        IF (DEP_DP(I) > ZERO) THEN
          H(I) = (YLD(I)-UVAR(I,4))/DEP_DP(I)
          H(I) = MAX(EM20,H(I))
        ENDIF
        ! Storing new values
        UVAR(I,1) = R(I)
        UVAR(I,2) = RNIH(I)  
        UVAR(I,3) = AYU(I) 
        UVAR(I,4) = YLD(I)
        UVAR(I,6) = MAX_ASTA(I)
        ETSE(I)   = H(I) / (H(I)+YOUNG(I))
      END DO         
      ! Equivalent stress output, thickness update and soundspeed
      DO I=1,NEL
         ! New equivalent stress
        AXX(I) = SIGNXX(I) - SIGA(I,1) - SIGB(I,1)
        AYY(I) = SIGNYY(I) - SIGA(I,2) - SIGB(I,2)
        AXY(I) = SIGNXY(I) - SIGA(I,3) - SIGB(I,3)
        !   -> Hill 1948
        IF (IPLAS == 1) THEN 
          SEFF(I) = SQRT(AXX(I)**2 - TWO*P2*AXX(I)*AYY(I) + P1*AYY(I)**2 + P3*AXY(I)**2)
        !   -> Barlat 1989
        ELSEIF (IPLAS == 2) THEN
          K1(I)   = (AXX(I) + P3*AYY(I))/TWO
          K2(I)   = SQRT(((AXX(I) - P3*AYY(I))/TWO)**TWO + (P4*AXY(I))**TWO)
          SEFF(I) = P1*ABS(K1(I)+K2(I))**N3 + P1*ABS(K1(I)-K2(I))**N3 + P2*ABS(TWO*K2(I))**N3
          SEFF(I) = (HALF*SEFF(I))**(ONE/N3)
        ENDIF   
        SEQ(I)    = SEFF(I)
        DEELZZ(I) = -NU*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG(I)
        IF (INLOC > 0) THEN 
          !   -> Hill 1948
          IF (IPLAS == 1) THEN 
            SEFF(I) = SQRT(AXX(I)**2 - TWO*P2*AXX(I)*AYY(I) + P1*AYY(I)**2 + P3*AXY(I)**2)
            NORMXX  = (AXX(I)-P2*AYY(I))/(MAX(SEFF(I),EM20))
            NORMYY  = (P1*AYY(I)-P2*AXX(I))/(MAX(SEFF(I),EM20))
            NORMXY  = (P3*AXY(I))/(MAX(SEFF(I),EM20))
          !   -> Barlat 1989
          ELSEIF (IPLAS == 2) THEN
            K1(I)   = (AXX(I) + P3*AYY(I))*NORM_SP/TWO
            K2(I)   = (SQRT(((AXX(I) - P3*AYY(I))/TWO)**TWO + (P4*AXY(I))**TWO))*NORM_SP
            SEFF(I) = P1*ABS(K1(I)+K2(I))**N3 + P1*ABS(K1(I)-K2(I))**N3 + P2*ABS(TWO*K2(I))**N3
            SEFF(I) = (HALF*MAX(SEFF(I),EM20))**(ONE/N3)
            DF_DK1 = (SEFF(I)**(1-N3))*(P1/TWO)*(
     .                        +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(N3-1)) 
     .                        +  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(N3-1)))
            DF_DK2 = (SEFF(I)**(1-N3))*((P1/TWO)*(
     .                        +  SIGN(ONE,K1(I)+K2(I))*(ABS(K1(I)+K2(I))**(N3-1)) 
     .                        -  SIGN(ONE,K1(I)-K2(I))*(ABS(K1(I)-K2(I))**(N3-1))) 
     .                        +  P2*(ABS(TWO*K2(I))**(N3-1)))
            DK1_DSIGXX = HALF
            DK1_DSIGYY = P3/TWO
            DK2_DSIGXX = (AXX(I)-P3*AYY(I))*NORM_SP/(MAX(FOUR*K2(I),EM20))
            DK2_DSIGYY = -P3*(AXX(I)-P3*AYY(I))*NORM_SP/(MAX(FOUR*K2(I),EM20))
            DK2_DSIGXY = (P4**TWO)*AXY(I)*NORM_SP/MAX(K2(I),EM20)
            NORMXX     = DF_DK1*DK1_DSIGXX + DF_DK2*DK2_DSIGXX
            NORMYY     = DF_DK1*DK1_DSIGYY + DF_DK2*DK2_DSIGYY
            NORMXY     = DF_DK2*DK2_DSIGXY
          ENDIF
          DFDSIG_A  = NORMXX * AXX(I)
     .              + NORMYY * AYY(I)
     .              + NORMXY * AXY(I)
          IF (DFDSIG_A /= ZERO) THEN
            DEPLZZ(I) = - MAX(DPLANL(I),ZERO)*(YIELD/DFDSIG_A)*(NORMXX + NORMYY)
          ELSE
            DEPLZZ(I) = ZERO
          ENDIF
        ENDIF
        DEPSZZ(I)  = DEELZZ(I) + DEPLZZ(I)
        THK(I)     = THK(I) + DEPSZZ(I)*THKLY(I)*OFF(I)
        SOUNDSP(I) = SQRT(A1(I)/RHO(I))
        SIGY(I)    = UVAR(I,4)
        DEP(I)     = DEP_DP(I)
      ENDDO
c-----------      
      RETURN
      END
