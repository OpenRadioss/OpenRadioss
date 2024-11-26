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
      !||    sigeps69c   ../engine/source/materials/mat/mat069/sigeps69c.F
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
      INTEGER :: I,J,K,ITER,NORDER
      my_real :: NU,RBULK,TENSCUT,GMAX,RVT,SUM,SUMDWDL,PARTP,EMAX,A11         
      my_real
     .   MU(5),AL(5),EVMA1(NEL,5),EVMA2(NEL,5),EVMA3(NEL,5),EVM(NEL,3),
     .   EIGV(NEL,3,2),TRAV(NEL),ROOTV(NEL),EVV(NEL,3),EV(NEL,3),
     .   RHO(NEL),DEZZ(NEL),DWDL(3),DDWDDL(3),RV(NEL),T(NEL,3),
     .   EPSZZ(NEL),SV(NEL,3),
     .   H30(100),H31(NEL,100),H1(100),H10(100),
     .   H2(100),H20(100),H12(100),H120(100),
     .   CD1(NEL),CD2(NEL),CD12(NEL),CD10(NEL),
     .   CD20(NEL),CD120(NEL), DC3EV3(NEL),C31(NEL),C30(NEL),KT3(NEL)
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
C     User variables initialisation
      IF (TIME == ZERO .AND. ISIGI == 0) THEN 
        DO I=1,NEL                            
          DO J=1,NUVAR                         
            UVAR(I,J) = ZERO                   
          ENDDO                                
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
      ENDDO
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
C--------------------------------------
C     Newton method =>  Find EV(3) : T3(EV(3)) = 0
C--------------------------------------
         DO ITER = 1,5
!       ----------------
          DO I=1,NEL  
            RV(I) = EV(I,1)*EV(I,2)*EV(I,3)  
c----       normalized stretch => unified compressible/uncompressible formution                   
!            RVT    = RV(I)**(-THIRD)
            IF(RV(I)> ZERO) THEN
             RVT    = EXP((-THIRD)*LOG(RV(I)))
            ELSE
             RVT = ZERO
            ENDIF                                               
            EVM(I,1) = EV(I,1)*RVT                                        
            EVM(I,2) = EV(I,2)*RVT                                        
            EVM(I,3) = EV(I,3)*RVT
         ENDDO  ! 1,NEL  
!       ----------------                                      
C----       partial derivatives of strain energy
!       ----------------
         DO J=1,5
          DO I=1,NEL
             IF(EVM(I,1)>ZERO) THEN
              IF(AL(J)/=ZERO) THEN
               EVMA1(I,J) = MU(J) * EXP(AL(J)*LOG(EVM(I,1)))
              ELSE 
               EVMA1(I,J) = MU(J)
              ENDIF
             ELSE
              EVMA1(I,J) = ZERO
             ENDIF
             IF(EVM(I,2)>ZERO) THEN
              IF(AL(J)/=ZERO) THEN
               EVMA2(I,J) = MU(J) * EXP(AL(J)*LOG(EVM(I,2)))
              ELSE
               EVMA2(I,J) = MU(J)
              ENDIF
             ELSE
              EVMA2(I,J) = ZERO
             ENDIF
             IF(EVM(I,3)>ZERO) THEN
              IF(AL(J)/=ZERO) THEN
               EVMA3(I,J) = MU(J) * EXP(AL(J)*LOG(EVM(I,3))) 
              ELSE
               EVMA3(I,J) = MU(J)
              ENDIF
             ELSE
              EVMA3(I,J) = ZERO
             ENDIF
           ENDDO        ! 1,NEL
          ENDDO   ! j=1,5             
!       ----------------  
          DO I=1,NEL                                               
            DWDL(1) = EVMA1(I,1)+EVMA1(I,2)+EVMA1(I,3)+EVMA1(I,4)+EVMA1(I,5)                              
            DWDL(2) = EVMA2(I,1)+EVMA2(I,2)+EVMA2(I,3)+EVMA2(I,4)+EVMA2(I,5)                                  
            DWDL(3) = EVMA3(I,1)+EVMA3(I,2)+EVMA3(I,3)+EVMA3(I,4)+EVMA3(I,5)                                                
            SUMDWDL = (DWDL(1)+DWDL(2)+DWDL(3))* THIRD                                
            PARTP   = RBULK*(RV(I)- ONE)                                                     
c------  ---
c           principal cauchy stress
            T(I,1)  = (DWDL(1) - SUMDWDL) / RV(I) + PARTP 
            T(I,2)  = (DWDL(2) - SUMDWDL) / RV(I) + PARTP 
            T(I,3)  = (DWDL(3) - SUMDWDL) / RV(I) + PARTP 
c------  ---

            KT3(I) = -THIRD*(EVMA1(I,1)+EVMA1(I,2)+EVMA1(I,3)+EVMA1(I,4)+EVMA1(I,5))
     .            -THIRD*(EVMA2(I,1)+EVMA2(I,2)+EVMA2(I,3)+EVMA2(I,4)+EVMA2(I,5))    
     .            +TWO_THIRD*(EVMA3(I,1)+EVMA3(I,2)+EVMA3(I,3)+EVMA3(I,4)+EVMA3(I,5))
            KT3(I) =-EV(I,1)*EV(I,2)*KT3(I)/(RV(I)**2) + RBULK*EV(I,1)*EV(I,2)
            KT3(I) = KT3(I) 
     .            +(ONE_OVER_9*(AL(1)*EVMA1(I,1)+AL(2)*EVMA1(I,2)+AL(3)*EVMA1(I,3) 
     .            +  AL(4)*EVMA1(I,4)+AL(5)*EVMA1(I,5) 
     .            +  AL(1)*EVMA2(I,1)+AL(2)*EVMA2(I,2) + AL(3)*EVMA2(I,3) 
     .            +  AL(4)*EVMA2(I,4)+AL(5)*EVMA2(I,5) 
     .            +  FOUR*(AL(1)*EVMA3(I,1) + AL(2)*EVMA3(I,2)
     .            +  AL(3)*EVMA3(I,3)
     .            +  AL(4)*EVMA3(I,4)+AL(5)*EVMA3(I,5))))/EV(I,3)/RV(I)                                                    
C
            EV(I,3) = EV(I,3)  - T(I,3)/KT3(I) 
            RV(I)   = EV(I,1)*EV(I,2)*EV(I,3) 
          ENDDO ! 1,NEL
      ENDDO    ! ITER = 1,5   
C-----------------------
      SV(1:NEL,1) = ZERO
      SV(1:NEL,2) = ZERO
      SV(1:NEL,3) = ZERO
C-----------------------
      DO I=1,NEL
        UVAR(I,3) = EV(I,3)
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
C-------------------------------------------------------------
C     set sound speed & viscosity
    !  DO I=1,NEL
    !    DEZZ(I)    =-NU/(ONE-NU)*(DEPSXX(I)+DEPSYY(I))
    !    THKN(I)    = THKN(I) + DEZZ(I)*THKLYL(I)
    !    RHO(I)     = RHO0(I)/RV(I)
    !    SOUNDSP(I) = SQRT((TWO_THIRD*GMAX+RBULK)/RHO(I))
    !    VISCMAX(I) = ZERO
    !  ENDDO
      IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN  ! engineering strain
        DO I=1,NEL
          EPSZZ(I) =EV(I,3) - ONE
          UVAR(I,3) = EV(I,3)
        ENDDO
      ELSEIF (ISMSTR == 10) THEN  ! left gauchy-green strain
        DO I=1,NEL
          EPSZZ(I) =EV(I,3) - ONE
          UVAR(I,3) = EV(I,3)
        ENDDO
      ELSE  ! true strain
        DO I=1,NEL
          EPSZZ(I) =LOG(EV(I,3))
          UVAR(I,3) = EV(I,3)
        ENDDO
      ENDIF
      DO I=1,NEL
        RV(I)   = EV(I,1)*EV(I,2)*EV(I,3)  
        DEZZ(I) =-NU/(ONE-NU)*(DEPSXX(I)+DEPSYY(I))
        SIGNXX(I) =EIGV(I,1,1)*T(I,1)+EIGV(I,1,2)*T(I,2) + SV(I,1)/RV(I)
        SIGNYY(I) =EIGV(I,2,1)*T(I,1)+EIGV(I,2,2)*T(I,2) + SV(I,2)/RV(I)
        SIGNXY(I) =EIGV(I,3,1)*T(I,1)+EIGV(I,3,2)*T(I,2) + SV(I,3)/RV(I)
C
        SIGNYZ(I) = SIGOYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I)+GS(I)*DEPSZX(I)
        RHO(I)    = RHO0(I)/RV(I)
        THKN(I) = THKN(I) + DEZZ(I)*THKLYL(I)*OFF(I)
        VISCMAX(I)= ZERO
C                
        EMAX = GMAX*(ONE + NU)
        A11  = EMAX/(ONE - NU**2)
        SOUNDSP(I)= SQRT(A11/RHO0(I))
      ENDDO
C-----------
      RETURN
      END
