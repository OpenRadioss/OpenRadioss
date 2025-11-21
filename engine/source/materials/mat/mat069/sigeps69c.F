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
!||    sigeps69c   ../engine/source/materials/mat/mat069/sigeps69c.F
!||--- called by ------------------------------------------------------
!||    mulawc      ../engine/source/materials/mat_share/mulawc.F90
!||====================================================================
              SUBROUTINE SIGEPS69C(
     1      NEL    , NUPARAM, NUVAR   ,NPT0   , ILAYER ,
     2      TIME   , TIMESTEP, UPARAM, RHO0  ,
     3      AREA   , EINT   , THKLYL,
     4      EPSPXX , EPSPYY , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, THKN  , UVAR  , NGL   ,
     B      OFF    , ISMSTR , GS    )
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
      INTEGER NEL,NUPARAM,NUVAR,ISMSTR,NPT0,ILAYER
      INTEGER NGL(NEL)
      my_real :: TIME,TIMESTEP
      my_real
     .  UPARAM(*),THKN(NEL),THKLYL(NEL),
     .  RHO0(NEL),AREA(NEL),EINT(NEL,2),GS(NEL),
     .  EPSPXX(NEL),EPSPYY(NEL),EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .  DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .  EPSXX (NEL),EPSYY (NEL),EPSXY (NEL),EPSYZ (NEL),EPSZX (NEL),
     .  SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .  SIGNXX (NEL),SIGNYY (NEL),SIGNXY (NEL),SIGNYZ (NEL),SIGNZX(NEL),
     .  SIGVXX (NEL),SIGVYY (NEL),SIGVXY (NEL),SIGVYZ (NEL),SIGVZX(NEL),
     .  SOUNDSP(NEL),VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real :: UVAR(NEL,NUVAR), OFF(NEL)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER :: I,ITER,NORDER,NITER,II
      my_real :: NU,RBULK,TENSCUT,GMAX,SUM,SUMDWDL,PARTP,EMAX,A11         
      my_real :: AMAX
      my_real ,DIMENSION(3)       :: LAM_AL
      my_real ,DIMENSION(5)       :: MU,AL    
      my_real ,DIMENSION(NEL)     :: RVT,GTMAX,DLAM3,KT3,RHO,KIR3
      my_real ,DIMENSION(NEL)     :: INVRV,DEZZ,RV,TRAV,ROOTV
      my_real ,DIMENSION(NEL,3)   :: T,EVV,EV,EVM,CII,S_LDWDL      
      my_real ,DIMENSION(NEL,3,2) :: EIGV(NEL,3,2)      
C=======================================================================
      MU(1)  = UPARAM(1)
      MU(2)  = UPARAM(2)
      MU(3)  = UPARAM(3)
      MU(4)  = UPARAM(4)
      MU(5)  = UPARAM(5)
      AL(1)  = UPARAM(6)
      AL(2)  = UPARAM(7)
      AL(3)  = UPARAM(8)
      AL(4)  = UPARAM(9)
      AL(5)  = UPARAM(10)
      RBULK  = UPARAM(11)
      TENSCUT= UPARAM(12)
      NU     = UPARAM(14)
      NORDER = NINT(UPARAM(18))
C
      GMAX = ZERO
      DO I=1,NORDER
        GMAX = GMAX + MU(I)*AL(I)
      ENDDO                               
! initialization of uvar(,3) in Starter
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
        IF(ABS(EVV(I,2)-EVV(I,1))<EM10) THEN
          EIGV(I,1,1) = ONE
          EIGV(I,2,1) = ONE
          EIGV(I,3,1) = ZERO
          EIGV(I,1,2) = ZERO
          EIGV(I,2,2) = ZERO
          EIGV(I,3,2) = ZERO
        ELSE
          EIGV(I,1,1) = (EPSXX(I)-EVV(I,2)) /ROOTV(I)
          EIGV(I,2,1) = (EPSYY(I)-EVV(I,2)) /ROOTV(I)
          EIGV(I,1,2) = (EVV(I,1)-EPSXX(I)) /ROOTV(I)
          EIGV(I,2,2) = (EVV(I,1)-EPSYY(I)) /ROOTV(I)
          EIGV(I,3,1) = (HALF*EPSXY(I))   /ROOTV(I)
          EIGV(I,3,2) =-(HALF*EPSXY(I))   /ROOTV(I)
        ENDIF
      ENDDO
C     Strain definition
      IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN  ! engineering strain
        DO I=1,NEL
          EV(I,1)=EVV(I,1)+ ONE
          EV(I,2)=EVV(I,2)+ ONE
          EV(I,3)=UVAR(I,3)
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
          EV(I,3)=UVAR(I,3)
        ENDDO
      ENDIF
      NITER = 4
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
      EV(1:NEL,3) = UVAR(1:NEL,3)+DLAM3(1:NEL) 
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
C--------------------------------------
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
        GTMAX(I) = HALF*MAX(CII(I,1),CII(I,2),CII(I,3)) 
      ENDDO
C-------------------------------------------------------------
      DO I=1,NEL
        SIGNXX(I) = EIGV(I,1,1)*T(I,1) + EIGV(I,1,2)*T(I,2) 
        SIGNYY(I) = EIGV(I,2,1)*T(I,1) + EIGV(I,2,2)*T(I,2) 
        SIGNXY(I) = EIGV(I,3,1)*T(I,1) + EIGV(I,3,2)*T(I,2) 
! 
        SIGNYZ(I) = SIGOYZ(I)+GS(I)*DEPSYZ(I) 
        SIGNZX(I) = SIGOZX(I)+GS(I)*DEPSZX(I) 
        RHO(I)    = RHO0(I)*INVRV(I)
        THKN(I)   = THKN(I) + DEZZ(I)*THKLYL(I)*OFF(I)
        VISCMAX(I)= ZERO         
! 
        EMAX = MAX(GMAX,GTMAX(I))*(ONE + NU)
        A11  = EMAX/(ONE - NU**2)
        SOUNDSP(I)= SQRT(A11/RHO0(I))
      ENDDO
C-----------
      RETURN
      END
