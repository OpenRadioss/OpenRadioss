Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    sigeps42c   ../engine/source/materials/mat/mat042/sigeps42c.F
      !||--- called by ------------------------------------------------------
      !||    mulawc      ../engine/source/materials/mat_share/mulawc.F
      !||====================================================================
      SUBROUTINE SIGEPS42C(
     1      NEL    , NUPARAM ,NIPARAM, NUVAR  , ISMSTR ,
     2      TIME   , TIMESTEP,UPARAM , IPARAM , RHO0   ,  
     3      DEPSXX , DEPSYY , DEPSXY , DEPSYZ , DEPSZX ,
     4      EPSXX  , EPSYY  , EPSXY  , THKN   , THKLYL ,
     5      SIGNXX , SIGNYY , SIGNXY , SIGNYZ , SIGNZX ,
     6      SIGOYZ , SIGOZX , SOUNDSP, GS     , UVAR   ,
     7      OFF    )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include "param_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER :: NEL
      INTEGER :: NUPARAM
      INTEGER :: NIPARAM
      INTEGER :: NUVAR
      INTEGER :: ISMSTR
      INTEGER :: IPARAM(NIPARAM)
      my_real :: UPARAM(NUPARAM)
      my_real :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) :: THKN,THKLYL,RHO0,GS,
     .  DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,EPSXX,EPSYY,EPSXY
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: SIGOYZ,SIGOZX
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real ,DIMENSION(NEL) :: SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,SOUNDSP
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real :: UVAR(NEL,NUVAR), OFF(NEL)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER :: I,J,II,NPRONY,NORDER,ITER,JNV
      my_real :: SUM,FAC,FSCAL,TENSCUT,SUMDWDL,SUMDDWDDL,PARTP
      my_real :: E11,E22,E12,EMAX,GMAX,GVMAX,NU,RBULK,A11,PUI11,PUI22,ALMA1,ALMA2,ALMA3
      my_real ,DIMENSION(3)       :: DDWDDL,DWDL
      my_real ,DIMENSION(10)      :: MU,AL    
      my_real ,DIMENSION(100)     :: GI,TAUX,H1,H2,H12,H10,H20,H120
      my_real ,DIMENSION(NEL,100) :: H30,H31
      my_real ,DIMENSION(NEL)     :: RVT,C30,C31,DC3EV3,CD10,CD20,CD120
      my_real ,DIMENSION(NEL)     :: CP1,CP2,CD1,CD2,CD12,EA,INVRV,INVV3,DEZZ,RV,TRAV,ROOTV
      my_real ,DIMENSION(NEL)     :: S2,CS,KT3,EPSZZ,RHO
      my_real ,DIMENSION(NEL,10)  :: EVMA1,EVMA2,EVMA3,C11,C22,C12
      my_real ,DIMENSION(NEL,3)   :: T,SV,SIGPRV,EVV,EV,EVM        
      my_real ,DIMENSION(NEL,3,2) :: EIGV(NEL,3,2)      
C=======================================================================
C SET INITIAL MATERIAL CONSTANTS
      
      NORDER = IPARAM(1)
      NPRONY = IPARAM(2)
!
      DO I=1,NORDER
        MU(I) = UPARAM(I)
        AL(I) = UPARAM(I+10)
      ENDDO                               
      RBULK  = UPARAM(21)
      NU     = UPARAM(22)
      TENSCUT= UPARAM(23)
      FSCAL  = UPARAM(24)
!     ------------------      
      GMAX  = ZERO
      GVMAX = ZERO
      DO I=1,NPRONY                       
        GI(I)   = UPARAM(24 + I)          
        TAUX(I) = UPARAM(24 + NPRONY + I) 
        GVMAX = GVMAX +  GI(I)
      ENDDO                               
      DO I=1,NORDER
        GMAX = GMAX + MU(I)*AL(I)
      ENDDO                               
      GMAX = GMAX + GVMAX
C
C     User variables initialisation
      IF (TIME == ZERO .AND. ISIGI == 0) THEN 
        DO I=1,NEL                          
          DO J=1,NUVAR                          
            UVAR(I,J) = ZERO                  
          ENDDO                               
          UVAR(I,3) = ONE                      
        ENDDO 
      ELSEIF (TIME == ZERO ) THEN 
        DO I=1,NEL                                  
          UVAR(I,3) = ONE           
        ENDDO 
      ENDIF                                   
C     principal stretch (def gradient eigenvalues)
      DO I=1,NEL
        TRAV(I)  = EPSXX(I)+EPSYY(I)
        ROOTV(I) = SQRT((EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .           + EPSXY(I)*EPSXY(I))
        EVV(I,1) = HALF*(TRAV(I)+ROOTV(I))
        EVV(I,2) = HALF*(TRAV(I)-ROOTV(I))
        EVV(I,3) = ZERO
        EA(I) = ZERO
      ENDDO
C-- avoid NaN---------        
      IF (ISMSTR == 10) THEN
        DO I=1,NEL
          IF (MIN(EVV(I,1),EVV(I,2)) <= -ONE) THEN
           EVV(I,1) = ZERO
           EVV(I,2) = ZERO
           OFF(I) = FOUR_OVER_5
          END IF
        ENDDO
      END IF  
C     rot matrix (eigenvectors)
      DO I=1,NEL
        IF (ABS(EVV(I,2)-EVV(I,1)) < EM10) THEN
          EIGV(I,1,1)=ONE
          EIGV(I,2,1)=ONE
          EIGV(I,3,1)=ZERO
          EIGV(I,1,2)=ZERO
          EIGV(I,2,2)=ZERO
          EIGV(I,3,2)=ZERO
        ELSE
          INVRV(I) = ONE / ROOTV(I)                                 
          EIGV(I,1,1) = (EPSXX(I)-EVV(I,2)) * INVRV(I)
          EIGV(I,2,1) = (EPSYY(I)-EVV(I,2)) * INVRV(I)
          EIGV(I,3,1) = (HALF*EPSXY(I))     * INVRV(I)
          EIGV(I,1,2) = (EVV(I,1)-EPSXX(I)) * INVRV(I) 
          EIGV(I,2,2) = (EVV(I,1)-EPSYY(I)) * INVRV(I)
          EIGV(I,3,2) =-(HALF*EPSXY(I))     * INVRV(I)  
        ENDIF                          
      ENDDO
C     Strain definition
      IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN  ! engineering strain
        DO I=1,NEL
          EV(I,1)=EVV(I,1)+ ONE
          EV(I,2)=EVV(I,2)+ ONE
          EV(I,3)=ONE/EV(I,1)/EV(I,2)
        ENDDO
      ELSEIF(ISMSTR == 10) THEN
        DO I=1,NEL
          EV(I,1)=SQRT(EVV(I,1)+ ONE)
          EV(I,2)=SQRT(EVV(I,2)+ ONE)
          EV(I,3)=ONE/EV(I,1)/EV(I,2)
        ENDDO
      ELSE  ! true strain
        DO I=1,NEL
          EV(I,1)=EXP(EVV(I,1))
          EV(I,2)=EXP(EVV(I,2))
          EV(I,3)=ONE/EV(I,1)/EV(I,2)
        ENDDO
      ENDIF
      IF (NPRONY >  0) EV(1:NEL,3) = UVAR(1:NEL,3)
      DO I=1,NEL
        IF (OFF(I)==ZERO.OR.OFF(I)==FOUR_OVER_5) EV(I,1:3)=ONE
      ENDDO 
C--------------------------------------
C     Newton method =>  Find EV(3) : T3(EV(3)) = 0
C--------------------------------------
      IF (NPRONY  > 0) THEN
C--------------------------------------
C       Newton method =>  Find EV(3) : T3(EV(3)) = 0
C--------------------------------------                                                             
        DO ITER = 1,5
!       ----------------------- 
          DO I=1,NEL 
            RV(I) = EV(I,1)*EV(I,2)*EV(I,3)                                    
c----       normalized stretch => unified compressible/uncompressible formulation   
            ! RVT = RV(I)**(-THIRD)
            ! RV = 0 --> RVT = 0
            ! else   --> RVT = exp( -third * ln( RV)) 
            IF (RV(I)/= ZERO) THEN
              RVT(I) = EXP( (-THIRD)* LOG(RV(I)) )
              INVRV(I) = ONE / RV(I)
            ELSE
              RVT(I)   = ZERO
              INVRV(I) = ZERO
            ENDIF             
            EVM(I,1) = EV(I,1)*RVT(I)                                      
            EVM(I,2) = EV(I,2)*RVT(I)                                      
            EVM(I,3) = EV(I,3)*RVT(I)
          ENDDO  ! 1,NEL    
!       -----------------------                                   
          ! partial derivatives of strain energy
          DO J = 1,NORDER
            DO I=1,NEL 
              ! EVMA(I) = MU * EVM(I) ** AL(I) :
              ! EVM = 0 --> EVMA(I) = 0
              ! AL  = 0 --> EVMA(I) = MU
              ! else    --> EVMA(I) = MU * exp(AL(I) * ln( EVM(I)))
              IF(EVM(I,1)==ZERO) THEN
                EVMA1(I,J) = ZERO
              ELSE
               IF (AL(J) == ZERO) THEN
                 EVMA1(I,J) = MU(J)
               ELSE
                 EVMA1(I,J) = MU(J) * EXP(AL(J)* LOG(EVM(I,1)) )
               ENDIF
              ENDIF
!
              IF(EVM(I,2)==ZERO) THEN
                EVMA2(I,J) = ZERO
              ELSE
               IF (AL(J) == ZERO) THEN
                 EVMA2(I,J) = MU(J)
               ELSE
                 EVMA2(I,J) = MU(J) * EXP(AL(J)* LOG(EVM(I,2)) )
               ENDIF
              ENDIF
!              
              IF(EVM(I,3)==ZERO) THEN
                EVMA3(I,J) = ZERO
              ELSE
                IF (AL(J) == ZERO) THEN
                  EVMA3(I,J) = MU(J)
                ELSE
                  EVMA3(I,J) = MU(J) * EXP(AL(J)* LOG(EVM(I,3)) )
               ENDIF
              ENDIF    
            ENDDO       ! 1,NEL    
          ENDDO         ! J=1,NORDER
!       -----------------------                   
          DO I=1,NEL
            DWDL(1) = ZERO          
            DWDL(2) = ZERO          
            DWDL(3) = ZERO          
            DO J=1,NORDER                                                
              DWDL(1) = DWDL(1) + EVMA1(I,J)        
              DWDL(2) = DWDL(2) + EVMA2(I,J)        
              DWDL(3) = DWDL(3) + EVMA3(I,J)
            END DO        
            SUMDWDL = (DWDL(1) + DWDL(2) + DWDL(3)) * THIRD                            
            PARTP   = RBULK*(RV(I)- ONE)                                           
c---------
c           principal cauchy stress
            IF (EV(I,3) == ZERO) THEN
              INVV3(I) = ZERO
            ELSE
              INVV3(I) = ONE / EV(I,3)
            ENDIF
            T(I,1)  = (DWDL(1) - SUMDWDL) *INVRV(I)  + PARTP                         
            T(I,2)  = (DWDL(2) - SUMDWDL) *INVRV(I)  + PARTP                         
            T(I,3)  = (DWDL(3) - SUMDWDL) *INVRV(I)  + PARTP                         
c---------
            KT3(I) = -THIRD*(DWDL(1) + DWDL(2)) + TWO_THIRD*(DWDL(3))          
            KT3(I) = -EV(I,1)*EV(I,2)*KT3(I)*INVRV(I)*INVRV(I)  + RBULK*EV(I,1)*EV(I,2)
!
            ALMA1 = ZERO
            ALMA2 = ZERO
            ALMA3 = ZERO
            DO J=1,NORDER
              ALMA1 = ALMA1 + AL(J)*EVMA1(I,J)                                             
              ALMA2 = ALMA2 + AL(J)*EVMA2(I,J)                                             
              ALMA3 = ALMA3 + AL(J)*EVMA3(I,J)                                             
            END DO        
            KT3(I) = KT3(I) + (ONE_OVER_9*(ALMA1 + ALMA2 + FOUR*(ALMA3))) * INVRV(I)*INVV3(I)                                            
C           viscosty traitement            
            C30(I) = UVAR(I,5) 
C
            SUM = THIRD*(EVM(I,1)**2 +  EVM(I,2)**2 + EVM(I,3)**2)
            C31(I)   =  EVM(I,3)**2 - SUM 
            ! SV3 = ZERO
            DC3EV3(I) = FOUR_OVER_3*RVT(I)*EVM(I,3)-TWO_THIRD*INVV3(I)* 
     .                 (TWO_THIRD*EVM(I,3)**2 - THIRD*EVM(I,1)**2 - THIRD*EVM(I,2)**2)
          ENDDO  ! 1,NEL
!       -----------------------  
          JNV = 8
          DO II= 1,NPRONY
            FAC= -TIMESTEP/TAUX(II)                          
            H30(1:NEL,II) = UVAR(1:NEL,JNV + II)                
            H31(1:NEL,II) = EXP(FAC)*H30(1:NEL,II)+ EXP(HALF*FAC)*(C31(1:NEL) - C30(1:NEL)) 
          ENDDO
C         Kirchoff visco stress --->
C         PK2 stress, PK2 = F**(-1)*Taux* F**(-T)n 
C         cauchy =Taux/RV is used here
          DO II = 1,NPRONY
                  FAC= -TIMESTEP/TAUX(II) 
                  T(1:NEL,3) = T(1:NEL,3) + GI(II)*H31(1:NEL,II)*INVRV(1:NEL)
                  KT3(1:NEL) = KT3(1:NEL) - GI(II)*H31(1:NEL,II)*INVV3(1:NEL)*INVRV(1:NEL)
     .                 + DC3EV3(1:NEL)*GI(II)*EXP(HALF*FAC)*INVRV(1:NEL)
          ENDDO
          DO I=1,NEL                                                 
            IF (OFF(I)==ZERO.OR.OFF(I)==FOUR_OVER_5) CYCLE
            IF (ABS(KT3(I))>EM20) EV(I,3) = EV(I,3)  - T(I,3)/KT3(I)
          ENDDO
        ENDDO  ! iteration
!       -----------------------
C       stored converged solution
        JNV = 8
        DO II= 1,NPRONY
           UVAR(1:NEL,JNV  + II) =  H31(1:NEL,II)
        ENDDO
        DO I=1,NEL
           UVAR(I,5) = C31(I) 
C           
           RV(I)   = EV(I,1)*EV(I,2)*EV(I,3)
! compute viscos stress
           ! RVT = RV(I) ** (-THIRD) :
           ! RV(I) = 0 --> RVT = 0
           ! else      --> RVT = exp((-THIRD) * ln(RV(I)) 
           IF (RV(I) /= ZERO) THEN
             RVT(I) = EXP( (-THIRD)* LOG(RV(I)) )
           ELSE
             RVT(I)   = ZERO
           ENDIF            
           EVM(I,1) = EV(I,1)*RVT(I)
           EVM(I,2) = EV(I,2)*RVT(I)
           EVM(I,3) = EV(I,3)*RVT(I)
C           
           CD10(I) = UVAR(I,6) 
           CD20(I) = UVAR(I,7) 
           CD120(I) = UVAR(I,8) 
C
           SUM  = THIRD*(EVM(I,1)**2 +  EVM(I,2)**2 + EVM(I,3)**2) 
           CP1(I)   =  EVM(I,1)**2 - SUM                          
           CP2(I)   =  EVM(I,2)**2 - SUM
           CD1(I)  = EIGV(I,1,1)*CP1(I) + EIGV(I,1,2)*CP2(I)
           CD2(I)  = EIGV(I,2,1)*CP1(I) + EIGV(I,2,2)*CP2(I)
           CD12(I) = EIGV(I,3,1)*CP1(I) + EIGV(I,3,2)*CP2(I)                        
           UVAR(I,6) = CD1(I)
           UVAR(I,7) = CD2(I)
           UVAR(I,8) = CD12(I) 
           SV(I,1) = ZERO
           SV(I,2) = ZERO
           SV(I,3) = ZERO
        ENDDO 
        JNV = 8 + NPRONY    
        DO II= 1,NPRONY
          DO I=1,NEL
              FAC= -TIMESTEP/TAUX(II)                          
              H10(II)   =  UVAR(I,JNV + II )                        
              H20(II)   =  UVAR(I,JNV + NPRONY + II )                        
              H120(II)  =  UVAR(I,JNV + 2*NPRONY + II )                
              H1(II)  = EXP(FAC)*H10(II)+ EXP(HALF*FAC)*(CD1(I) - CD10(I))                
              H2(II)  = EXP(FAC)*H20(II)+ EXP(HALF*FAC)*(CD2(I) - CD20(I))              
              H12(II)  = EXP(FAC)*H120(II)+ EXP(HALF*FAC)*(CD12(I) - CD120(I)) 
              UVAR(I,JNV +            II )= H1(II)              
              UVAR(I,JNV + NPRONY   + II )= H2(II)   
              UVAR(I,JNV + 2*NPRONY + II )= H12(II)          
C           Kirchoff visco stress
              SV(I,1) = SV(I,1) + GI(II)*H1(II)
              SV(I,2) = SV(I,2) + GI(II)*H2(II)
              SV(I,3) = SV(I,3) + GI(II)*H12(II)
            ENDDO       ! 1,NEL                                                                   
        ENDDO  !  NPRONY
                             
!     -----------------------
      ELSE ! lam3=1/lam1/lam2 (incompressible formulation) with out viscosity
!     -----------------------
        DO J = 1,NORDER                                                                
c----     normalized stretch => unified compressible/uncompressible formulation   
C----     partial derivatives of strain energy
          ! EVMA(I) = MU * EV(I) ** AL(I) :
          ! EV  = 0 --> EVMA(I) = 0
          ! AL  = 0 --> EVMA(I) = MU
          ! else    --> EVMA(I) = MU * PUI
          ! with PUI = exp(AL(I) * ln( EV(I)))
          ! EVMA3 = MU * ( EV(1) * EV(2) )** (-AL(I))
          ! --> EVMA3 = MU/ (PUI11 * PUI22)
          DO I=1,NEL 
            IF(EV(I,1)==ZERO) THEN
              EVMA1(I,J) = ZERO
            ELSE
              IF (AL(J) == ZERO) THEN
                EVMA1(I,J) = MU(J)
                PUI11 = ONE
              ELSE
                PUI11 = EXP(AL(J)* LOG(EV(I,1)) )
                EVMA1(I,J) = MU(J) * PUI11
              ENDIF
            ENDIF
!            
            IF(EV(I,2)==ZERO) THEN
              EVMA2(I,J) = ZERO
            ELSE
              IF (AL(J) == ZERO) THEN
                EVMA2(I,J) = MU(J)
                PUI22 = ONE
              ELSE
                PUI22 = EXP(AL(J)* LOG(EV(I,2)) )
                EVMA2(I,J) = MU(J) * PUI22
              ENDIF
            ENDIF
!            
            IF((EV(I,1)*EV(I,2))==ZERO) THEN
              EVMA3(I,J) = ZERO
            ELSE
              IF (AL(J) == ZERO) THEN
                EVMA3(I,J) = MU(J)
              ELSE
                EVMA3(I,J) = MU(J)/(PUI11*PUI22)
              ENDIF
            ENDIF
          ENDDO    ! 1,NEL 
        ENDDO      ! 1,NORDER
!       -----------------------
        DO I=1,NEL
          DWDL(1) = ZERO          
          DWDL(2) = ZERO          
          DWDL(3) = ZERO          
          DO J=1,NORDER                                                
            DWDL(1) = DWDL(1) + EVMA1(I,J)        
            DWDL(2) = DWDL(2) + EVMA2(I,J)        
            DWDL(3) = DWDL(3) + EVMA3(I,J)
          END DO        
c---------
c         principal cauchy stress
          T(I,1) = DWDL(1) - DWDL(3)                       
          T(I,2) = DWDL(2) - DWDL(3)                                     
          T(I,3) = ZERO            
        ENDDO                                                     
!       -----------------------
!       compute rigidity
        DO J = 1,NORDER
           ! CXX = 1/2 * MU * [1/2 * AL - 1] * EVX ** (AL - 4)
           ! CXY = 1/2 * MU * [1/2 * AL + 1] * (EVX * EVY) **(-AL - 4)
           ! AL - 4 = 0 --> CXX =  1/2 * MU * [1/2 * AL - 1]
           !            --> CXY = 1/2 * MU * [1/2 * AL + 1] * (EVX * EVY) **(-8)
           ! AL = 0     --> CXX =  1/2 * MU * [1/2 * AL - 1]* EVX ** (- 4)
           !            --> CXY = 1/2 * MU * [1/2 * AL + 1] * (EVX * EVY) **(-4)
           ! else       --> CXX =  1/2 * MU * [1/2 * AL - 1]* PUIXX / EVX**(4)
           !                 with PUIXX = exp( AL * ln(EVX) )
           !            --> CXY = 1/2 * MU * [1/2 * AL + 1] / ( PUIXX*PUIYY*(EVX**(4)*EVY**(4) )
          DO I=1,NEL
            IF (AL(J) == FOUR) THEN
              C11(I,J) = HALF*MU(J)*(HALF*AL(J)-ONE)
              C22(I,J) = HALF*MU(J)*(HALF*AL(J)-ONE)
              C12(I,J) = HALF*MU(J)*(HALF*AL(J) + ONE) / (EV(I,1)*EV(I,2))**(8)
            ELSEIF (AL(J) == ZERO) THEN
              C11(I,J) = HALF*MU(J)*(HALF*AL(J)-ONE)   /  EV(I,1)**(4)
              C22(I,J) = HALF*MU(J)*(HALF*AL(J)-ONE)   /  EV(I,2)**(4)
              C12(I,J) = HALF*MU(J)*(HALF*AL(J) + ONE) / (EV(I,1)*EV(I,2))**(4)
            ELSE
              IF(EV(I,1) /= ZERO) THEN
                PUI11 = EXP((AL(J) )* LOG(EV(I,1)) )
                C11(I,J) = HALF*MU(J)*(HALF*AL(J) - ONE)*PUI11 / EV(I,1)**(4)
              ELSE
                C11(I,J) = ZERO
                PUI11 = ONE
              ENDIF
              IF(EV(I,2) /= ZERO) THEN
                PUI22 = EXP((AL(J))* LOG(EV(I,2)) )
                C22(I,J) = HALF*MU(J)*(HALF*AL(J) - ONE)*PUI22 / EV(I,2)**(4)
              ELSE
                C22(I,J) = ZERO
                PUI22 = ONE
              ENDIF
              IF(EV(I,1)*EV(I,2) /= ZERO) THEN
                C12(I,J) = HALF*MU(J)*(HALF*AL(J) + ONE)       /
     .                   ( (PUI11 * PUI22)*(EV(I,1)**4 * EV(I,2)**4) )
              ELSE
                C12(I,J) = ZERO
              ENDIF
            ENDIF     
          ENDDO  ! 1,NEL
        ENDDO    ! 1,NORDER
!       -----------------------             
        DO I=1,NEL
          E11 = ZERO
          E22 = ZERO
          E12 = ZERO
          DO J=1,NORDER
            E11 = E11 + C11(I,J)
            E22 = E22 + C22(I,J)
            E12 = E12 + C12(I,J)
          END DO
          E11 = E11 + E12 * EV(I,2)**4
          E22 = E22 + E12 * EV(I,1)**4
          EA(I) = MAX(E11,E22)
          SV(I,1) = ZERO
          SV(I,2) = ZERO
          SV(I,3) = ZERO
        ENDDO  ! 1,NEL
!       -----------------------
      ENDIF ! PRONY                                                                             
C-------------------------------------------------------------
c     tension cut                                                            
      DO I=1,NEL                                                             
        IF (OFF(I) /= ZERO .AND.                                             
     .   (T(I,1) > ABS(TENSCUT) .OR. T(I,2) > ABS(TENSCUT))) THEN        
          T(I,1) = ZERO                                                  
          T(I,2) = ZERO                                                  
          T(I,3) = ZERO                                                  
          OFF(I) = FOUR_OVER_5                                 
        ENDIF                                                                
      ENDDO                                                                  
C-------------------------------------------------------------
C     transform principal Cauchy stress to global directions
C-------------------------------------------------------------      
      IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN  ! engineering strain
        DO I=1,NEL
          EPSZZ(I)  = EV(I,3) - ONE
          UVAR(I,3) = EV(I,3)
        ENDDO
      ELSEIF (ISMSTR == 10) THEN  ! left gauchy-green strain
        DO I=1,NEL
          EPSZZ(I)  = EV(I,3) - ONE
          UVAR(I,3) = EV(I,3)
        ENDDO
      ELSE  ! true strain
        DO I=1,NEL
          EPSZZ(I)  = LOG(EV(I,3))
          UVAR(I,3) = EV(I,3)
        ENDDO
      ENDIF
c
      DO I=1,NEL
        RV(I)   = EV(I,1)*EV(I,2)*EV(I,3)
        IF (RV(I) /= ZERO) THEN
          INVRV(I) = ONE / RV(I)
        ELSE
          INVRV(I) = ZERO
        ENDIF
c
        DEZZ(I) = -NU/(ONE-NU)*(DEPSXX(I)+DEPSYY(I))
!!        DEZZ(I) = EPSZZ(I) - UVAR(I,4)
        SIGNXX(I) = EIGV(I,1,1)*T(I,1) + EIGV(I,1,2)*T(I,2) + SV(I,1)*INVRV(I)
        SIGNYY(I) = EIGV(I,2,1)*T(I,1) + EIGV(I,2,2)*T(I,2) + SV(I,2)*INVRV(I)
        SIGNXY(I) = EIGV(I,3,1)*T(I,1) + EIGV(I,3,2)*T(I,2) + SV(I,3)*INVRV(I)
        SIGNYZ(I) = SIGOYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I)+GS(I)*DEPSZX(I)
        RHO(I)    = RHO0(I)*INVRV(I)
        THKN(I)   = THKN(I) + DEZZ(I)*THKLYL(I)*OFF(I)
        UVAR(I,4) = EPSZZ(I)  
C         
        EMAX = GMAX*(ONE + NU)
        EMAX = MAX(EMAX,EA(I))
        A11  = EMAX/(ONE - NU**2)
        SOUNDSP(I)= SQRT(A11/RHO0(I))
      ENDDO
C-----------
      RETURN
      END
