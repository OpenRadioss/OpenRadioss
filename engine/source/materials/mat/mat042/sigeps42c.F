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
!||    sigeps42c   ../engine/source/materials/mat/mat042/sigeps42c.F
!||--- called by ------------------------------------------------------
!||    mulawc      ../engine/source/materials/mat_share/mulawc.F90
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
      INTEGER :: I,J,II,NPRONY,NORDER,ITER,JNV,NITER
      my_real :: SUM,FAC,FSCAL,TENSCUT,SUMDWDL,SUMDDWDDL,PARTP,AMAX
      my_real :: E11,E22,E12,EMAX,GMAX,GVMAX,NU,RBULK,A11,PUI11,PUI22,ALMA1,ALMA2,ALMA3
      my_real ,DIMENSION(3)       :: DDWDDL,DWDL,LAM_AL
      my_real ,DIMENSION(10)      :: MU,AL    
      my_real ,DIMENSION(NEL)     :: RVT,GTMAX,DLAM3
      my_real ,DIMENSION(NEL)     :: INVRV,INVV3,DEZZ,RV,TRAV,ROOTV
      my_real ,DIMENSION(NEL)     :: S2,CS,KT3,RHO,KIR3
      my_real ,DIMENSION(NEL,3)   :: T,SV,SIGPRV,EVV,EV,EVM,CII,S_LDWDL      
      my_real ,DIMENSION(NEL,3,2) :: EIGV(NEL,3,2)      
      my_real :: DAV,H0(4),DEPSZZ,TAUX
      my_real ,DIMENSION(NEL)     :: A1,A2
      my_real ,DIMENSION(NEL,4)   :: DEVS
      my_real, DIMENSION(:) , ALLOCATABLE :: GI,BETA
      my_real, DIMENSION(:,:) , ALLOCATABLE :: AA,BB,H11
C=======================================================================
C SET INITIAL MATERIAL CONSTANTS
      
      NORDER = IPARAM(1)
      NPRONY = IPARAM(2)
      IF (NPRONY>0) THEN 
        ALLOCATE(AA(NEL,NPRONY)) 
        ALLOCATE(BB(NEL,NPRONY)) 
        ALLOCATE(H11(6,NPRONY)) 
        ALLOCATE(GI(NPRONY)) 
        ALLOCATE(BETA(NPRONY)) 
      ENDIF
      SV(1:NEL,1:3) = ZERO
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
        TAUX    = UPARAM(24 + NPRONY + I) 
        GVMAX = GVMAX +  GI(I)
        BETA(I) = ONE/TAUX
      ENDDO                               
      DO I=1,NORDER
        GMAX = GMAX + MU(I)*AL(I)
      ENDDO                               
      GMAX = GMAX + GVMAX
C
      NITER = 4
C     principal stretch (def gradient eigenvalues)
      DO I=1,NEL
        TRAV(I)  = EPSXX(I)+EPSYY(I)
        ROOTV(I) = SQRT((EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .           + EPSXY(I)*EPSXY(I))
        EVV(I,1) = HALF*(TRAV(I)+ROOTV(I))
        EVV(I,2) = HALF*(TRAV(I)-ROOTV(I))
        EVV(I,3) = ZERO
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
      DO I=1,NEL
        IF (OFF(I)==ZERO.OR.OFF(I)==FOUR_OVER_5) EV(I,1:3)=ONE
      ENDDO 
!    old iterartive Pnony removed, new formulation uncoupling w/ Prony 
!--------------------------------------
!       Newton method =>  Find EV(3) : Kirchoff J*T3(EV(3)) = 0
!--------------------------------------
      DLAM3(1:NEL) =ZERO
      DO ITER = 1,NITER
        EV(1:NEL,3) = UVAR(1:NEL,3)+DLAM3(1:NEL) ! initial value takes lamda3(t)
        DO I=1,NEL 
          RV(I) = EV(I,1)*EV(I,2)*EV(I,3)                                    
          RVT(I) = EXP( (-THIRD)* LOG(RV(I)) )
          EVM(I,1:3) = EV(I,1:3)*RVT(I)                                      
        ENDDO  ! 1,NEL    
        KIR3(1:NEL) = ZERO
        KT3(1:NEL) = ZERO
        DO II = 1,NORDER
          IF (MU(II)*AL(II) /= ZERO) THEN
            DO I=1,NEL
              IF (OFF(I)==ZERO.OR.OFF(I)==FOUR_OVER_5) CYCLE
              LAM_AL(1:3) = EXP(AL(II)*LOG(EVM(I,1:3)))
              SUMDWDL = THIRD*(LAM_AL(1)+LAM_AL(2)+LAM_AL(3))
              SUM = MU(II)*(LAM_AL(3)-SUMDWDL) 
              KIR3(I) = KIR3(I) + SUM 
              KT3(I)  = KT3(I) + AL(II)*SUM
            ENDDO
          ENDIF
        ENDDO
        DO I=1,NEL 
          IF (OFF(I)==ZERO.OR.OFF(I)==FOUR_OVER_5) CYCLE
          PARTP = RBULK*(RV(I)- ONE)                                           
          T(I,3)= KIR3(I)  + PARTP*RV(I)    !Kirchoff                     
          KT3(I)= TWO_THIRD*KT3(I)/EV(I,3)+ RBULK*(TWO*RV(I)-ONE)*EV(I,1)*EV(I,2)          
          IF (KT3(I)>EM20) DLAM3(I) = DLAM3(I) -T(I,3)/KT3(I)
        ENDDO
      END DO ! ITER = 1,NITER
      EV(1:NEL,3) = UVAR(1:NEL,3)+DLAM3(1:NEL) ! initial value takes lamda3(t): looking for (t+dt)
      DEZZ(1:NEL) = LOG(ONE+DLAM3(1:NEL)/UVAR(1:NEL,3))
      UVAR(1:NEL,3) =  EV(1:NEL,3) 
! compute T1,T2 Cauchy stress
      DO I=1,NEL
        RV(I) = EV(I,1)*EV(I,2)*EV(I,3)                                    
        RVT(I) = EXP((-THIRD)*LOG(RV(I))) ! -> J^(-1/3)
        EVM(I,1:3) = EV(I,1:3)*RVT(I) 
        INVRV(I) = ONE / RV(I)
      END DO
      S_LDWDL(1:NEL,1:3) = ZERO
      DO II = 1,NORDER
         IF (MU(II)*AL(II) /= ZERO) THEN
           DO I=1,NEL
             LAM_AL(1:3) = EXP(AL(II)*LOG(EVM(I,1:3)))
             S_LDWDL(I,1:3)  = S_LDWDL(I,1:3) + MU(II)*LAM_AL(1:3)
           ENDDO
         ENDIF
      ENDDO
      DO I=1,NEL
        SUMDWDL = (S_LDWDL(I,1) + S_LDWDL(I,2) + S_LDWDL(I,3)) * THIRD                            
        PARTP   = RBULK*(RV(I)- ONE)                                           
        T(I,1)  = (S_LDWDL(I,1) - SUMDWDL) *INVRV(I)  + PARTP                         
        T(I,2)  = (S_LDWDL(I,2) - SUMDWDL) *INVRV(I)  + PARTP                         
      ENDDO                                                     
      IF (NPRONY  > 0 ) THEN ! Prony same as /VISC/PRONY
        A1(1:NEL) = ZERO
        A2(1:NEL) = ZERO
        JNV = 12
        DO II=1,NPRONY
          DO I=1,NEL
            AA(I,II) =  EXP(-BETA(II)*TIMESTEP) 
            BB(I,II) =  GI(II)*EXP(-HALF*BETA(II)*TIMESTEP)  !bb/dt, and use directly despsij
!  for computing DEPSZZ from (sigzz=0)
            H0(3) = UVAR(I,JNV + (II - 1)*4 + 3)
            A1(I)  = A1(I) +  AA(I,II)*H0(3) 
            A2(I)  = A2(I) +  BB(I,II)
          END DO
        END DO  
        DO I=1,NEL
           FAC = ONE/MAX(EM20,TWO_THIRD*A2(I))
           DEPSZZ = -A1(I)*FAC + HALF*(DEPSXX(I) + DEPSYY(I))
           DAV = THIRD*(DEPSXX(I) + DEPSYY(I) + DEPSZZ)
           DEVS(I,1) = DEPSXX(I) - DAV
           DEVS(I,2) = DEPSYY(I) - DAV
           DEVS(I,3) = DEPSZZ    - DAV         
           DEVS(I,4) = HALF*DEPSXY(I)          
           DEZZ(I)   = DEZZ(I) + DEPSZZ
        END DO
!              
        DO II= 1,NPRONY
          DO I=1,NEL              
            H0(1:4) = UVAR(I,JNV + (II - 1)*4 +1:4)
            H11(1:4,II) = AA(I,II)*H0(1:4) + BB(I,II)*DEVS(I,1:4)
            SV(I,1:2) = SV(I,1:2) + H11(1:2,II)
            SV(I,3) = SV(I,3) + H11(4,II)
            UVAR(I,JNV + (II - 1)*4 +1:4) = H11(1:4,II)
          END DO
        END DO 
      END IF ! PRONY 
!-------------------------------------------------------------
!     tension cut                                                            
      DO I=1,NEL                                                             
        IF (OFF(I) /= ZERO .AND.                                             
     .   (T(I,1) > ABS(TENSCUT) .OR. T(I,2) > ABS(TENSCUT))) THEN        
          T(I,1) = ZERO                                                  
          T(I,2) = ZERO                                                  
          T(I,3) = ZERO                                                  
          OFF(I) = FOUR_OVER_5                                 
        ENDIF                                                                
      ENDDO                                                                  
! new gt      
      CII(1:NEL,1:3) = ZERO
      DO II = 1,NORDER
        IF (MU(II)*AL(II) /= ZERO) THEN
          DO I=1,NEL
            LAM_AL(1:3) = EXP(AL(II)*LOG(EVM(I,1:3)))
            AMAX = THIRD*(LAM_AL(1)+LAM_AL(2)+LAM_AL(3))
            CII(I,1:3) = CII(I,1:3) + MU(II)*AL(II)*(LAM_AL(1:3)+AMAX) 
          ENDDO
        ENDIF
      ENDDO
      DO I = 1,NEL
        GTMAX(I) = GVMAX+HALF*MAX(CII(I,1),CII(I,2),CII(I,3)) 
      ENDDO
!
      DO I=1,NEL
        SIGNXX(I) = EIGV(I,1,1)*T(I,1) + EIGV(I,1,2)*T(I,2) + SV(I,1)
        SIGNYY(I) = EIGV(I,2,1)*T(I,1) + EIGV(I,2,2)*T(I,2) + SV(I,2)
        SIGNXY(I) = EIGV(I,3,1)*T(I,1) + EIGV(I,3,2)*T(I,2) + SV(I,3)
! transverse shear can't use visco(Prony) as /VISC due to incremental formulation
        SIGNYZ(I) = SIGOYZ(I)+GS(I)*DEPSYZ(I) 
        SIGNZX(I) = SIGOZX(I)+GS(I)*DEPSZX(I) 
        RHO(I)    = RHO0(I)*INVRV(I)
        THKN(I)   = THKN(I) + DEZZ(I)*THKLYL(I)*OFF(I)
         
! 
        EMAX = GTMAX(I)*(ONE + NU)
        A11  = EMAX/(ONE - NU**2)
        SOUNDSP(I)= SQRT(A11/RHO0(I))
      ENDDO
      IF (NPRONY>0) THEN 
        DEALLOCATE(AA) 
        DEALLOCATE(BB) 
        DEALLOCATE(H11) 
        DEALLOCATE(GI) 
        DEALLOCATE(BETA) 
      ENDIF
C-----------
      RETURN
      END
