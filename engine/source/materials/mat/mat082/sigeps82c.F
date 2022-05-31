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
Chd|  SIGEPS82C                     source/materials/mat/mat082/sigeps82c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
              SUBROUTINE SIGEPS82C(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC ,
     2      NPF    , NPT0   , IPT , IFLAG  ,
     2      TF     , TIME   , TIMESTEP,
     3      UPARAM , RHO0   ,AREA   , EINT   , THKLYL,
     4      EPSPXX , EPSPYY , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, THKN  , UVAR  ,
     B      NGL    , OFF    , ISMSTR , IPM   , GS    )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include "param_c.inc"
#include "scr17_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL,NUPARAM, NUVAR,ISMSTR, IPM(NPROPMI,*),MAT(NEL) ,
     .         NPT0,IPT,IFLAG(*),NGL(NEL)
      my_real
     .   TIME,TIMESTEP,UPARAM(NUPARAM),THKN(NEL),THKLYL(NEL),
     .   RHO0(NEL),AREA(NEL),EINT(NEL,2),GS(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX (NEL),EPSYY (NEL),EPSXY (NEL),EPSYZ (NEL),EPSZX (NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   SIGVXX(NEL),SIGVYY(NEL),SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .   SOUNDSP(NEL),VISCMAX(NEL),ET(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER  I,K,ITER,NORDRE,I_K2
      my_real
     .   NU,TENSCUT,GMAX,RBULK,FFAC,RVT,PARTT(NEL),PARTP(NEL),SUM,DD,K2
      my_real
     .   EVV(NEL,3),EV(NEL,3),EVM(NEL,3),RV(NEL),RHO(NEL),DWDL(NEL,3),
     .   DEZZ(NEL),EIGV(NEL,3,2),TRAV(NEL),ROOTV(NEL),T(NEL,3)
      my_real     
     .   MU(100),AL(100),D(100),EVMA1(NEL,100),EVMA2(NEL,100),EVMA3(NEL,100)
C=======================================================================
C SET INITIAL MATERIAL CONSTANTS
      GMAX = ZERO
      NORDRE  = NINT(UPARAM(1))
      DO I= 1,NORDRE
        MU(I) = UPARAM(1 + I)
        AL(I) = UPARAM(1 + NORDRE + I)
        D(I)  = UPARAM(1 + 2*NORDRE + I)
        GMAX  = GMAX  + MU(I)
      ENDDO
      RBULK   = TWO/D(1)
      NU = (THREE*RBULK-TWO*GMAX)/(TWO*GMAX+SIX*RBULK)
      IF (NU == HALF) NU = 0.495
      IF (TIME == ZERO) UVAR(1:NEL,1)=ONE
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
        IF (ABS(EVV(I,2)-EVV(I,1)) < EM10) THEN
          EIGV(I,1,1) = ONE
          EIGV(I,2,1) = ONE
          EIGV(I,3,1) = ZERO
          EIGV(I,1,2) = ZERO
          EIGV(I,2,2) = ZERO
          EIGV(I,3,2) = ZERO
        ELSE
          EIGV(I,1,1) = ONE/ROOTV(I)*(EPSXX(I)-EVV(I,2))
          EIGV(I,2,1) = ONE/ROOTV(I)*(EPSYY(I)-EVV(I,2))
          EIGV(I,1,2) = ONE/ROOTV(I)*(EVV(I,1)-EPSXX(I))
          EIGV(I,2,2) = ONE/ROOTV(I)*(EVV(I,1)-EPSYY(I))
          EIGV(I,3,1) = ONE/ROOTV(I)*(HALF*EPSXY(I))
          EIGV(I,3,2) =-ONE/ROOTV(I)*(HALF*EPSXY(I))
        ENDIF
      ENDDO
C---  Strain definition
      IF (ISMSTR == 1 .OR. ISMSTR == 3) THEN  ! engineering strain
        DO I=1,NEL
          EV(I,1) = EVV(I,1) + ONE
          EV(I,2) = EVV(I,2) + ONE
          EV(I,3) = UVAR(I,1)
        ENDDO
      ELSE  ! true strain
        DO I=1,NEL
          EV(I,1) = EXP(EVV(I,1))
          EV(I,2) = EXP(EVV(I,2))
          EV(I,3) = UVAR(I,1)
        ENDDO
      ENDIF
C--------------------------------------
C     Newton method =>  Find root EV(3) : T3(EV(3)) = 0
C--------------------------------------
      DO ITER = 1,3                  
          RV(1:NEL) = EV(1:NEL,1)*EV(1:NEL,2)*EV(1:NEL,3)
c----     normalized stretch => unified compressible/uncompressible formulation
          DO I=1,NEL
            IF(RV(I)/=ZERO) THEN                   
            ! RVT    = RV(I)**(-THIRD)
              RVT = RV(I)**(-THIRD) ! EXP((-THIRD)*LOG(RV(I)))
            ELSE
              RVT = ZERO
            ENDIF                                                 
            EVM(I,1) = EV(I,1)*RVT                                        
            EVM(I,2) = EV(I,2)*RVT                                        
            EVM(I,3) = EV(I,3)*RVT                                        
          ENDDO
          DO K = 1,NORDRE
            DO I=1,NEL 
              IF(EVM(I,1)/=ZERO) THEN
               EVMA1(I,K) = EVM(I,1)**AL(K)
              ELSE
               EVMA1(I,K) = ZERO
              ENDIF
              IF(EVM(I,2)/=ZERO) THEN
               EVMA2(I,K) = EVM(I,2)**AL(K)
              ELSE
               EVMA2(I,K) = ZERO
              ENDIF
              IF(EVM(I,3)/=ZERO) THEN 
               EVMA3(I,K) = EVM(I,3)**AL(K)
              ELSE
               EVMA3(I,K) = ZERO
              ENDIF
            ENDDO       ! 1,NEL
          ENDDO         ! Nordre

C----     partial derivatives of strain energy                                             
          PARTT(1:NEL) =ZERO
          DO K = 1,NORDRE  
            DD  = TWO*MU(K)/AL(K)
            DO I=1,NEL 
              SUM = THIRD*(EVMA1(I,K) + EVMA2(I,K) + EVMA3(I,K))                                      
              PARTT(I) = PARTT(I) + DD*(EVMA3(I,K) - SUM)
            ENDDO
          ENDDO
          PARTP(1:NEL) =ZERO
          DO K = 1,NORDRE
            IF (D(K) /= ZERO) THEN
              K2 = TWO*K
              I_K2 = 2*K
              DD = K2/D(K)
              DO I=1,NEL
                PARTP(I) = PARTP(I) + DD*(RV(I)-ONE)**(I_K2-1)
              ENDDO
            ENDIF
          ENDDO
C--------------------------------
          T(1:NEL,3) = PARTT(1:NEL)/RV(1:NEL) + PARTP(1:NEL)
C--------------------------------
c         second derivative (d_sigma/d_lambda3)
          PARTT(1:NEL) =ZERO
          DO K = 1,NORDRE
            PARTT(1:NEL) = PARTT(1:NEL) 
     .            + TWO*MU(K)*(EVMA1(1:NEL,K)+EVMA2(1:NEL,K)+FOUR*EVMA3(1:NEL,K))/NINE
          ENDDO
          PARTP(1:NEL) = ZERO
          DO K = 1,NORDRE
            IF (D(K) /= ZERO) THEN
              K2 = TWO*K
              I_K2 = 2*K
              DD = K2*(K2-ONE)/D(K)
!              PARTP = PARTP + DD*(RV(I)-ONE)**(K2-TWO)
              PARTP(1:NEL) = PARTP(1:NEL) + DD*(RV(1:NEL)-ONE)**(I_K2-2)
            ENDIF
          ENDDO
          PARTT(1:NEL)   = PARTT(1:NEL) / RV(1:NEL)+ PARTP(1:NEL) - T(1:NEL,3)
          EV(1:NEL,3) = EV(1:NEL,3)*(ONE - T(1:NEL,3)/PARTT(1:NEL)) 
      ENDDO   ! ITER
C----------------
c     Recalculate the principal stress
      DO I=1,NEL
        RV(I)  = EV(I,1)*EV(I,2)*EV(I,3)
!        RVT    = RV(I)**(-THIRD)
        IF(RV(I)/=ZERO) THEN                                                            
         RVT = RV(I)**(-THIRD) !EXP((-THIRD)*LOG(RV(I)))
        ELSE
         RVT = ZERO
        ENDIF
        EVM(I,1) = EV(I,1)*RVT                                                                
        EVM(I,2) = EV(I,2)*RVT                                                                
        EVM(I,3) = EV(I,3)*RVT   
      ENDDO                                                             
      DO K = 1,NORDRE
        DO I=1,NEL
          IF(EVM(I,1)/=ZERO) THEN
           EVMA1(I,K) = EVM(I,1)**AL(K)
          ELSE
           EVMA1(I,K) = ZERO
          ENDIF
          IF(EVM(I,2)/=ZERO) THEN
           EVMA2(I,K) = EVM(I,2)**AL(K)
          ELSE
           EVMA2(I,K) = ZERO
          ENDIF  
          IF(EVM(I,3)>ZERO) THEN
           EVMA3(I,K) = EVM(I,3)**AL(K)
          ELSE
           EVMA3(I,K) = ZERO
          ENDIF                                                                          
        ENDDO   ! 1:NEL                                                          
      ENDDO     ! Nordre                                                                               
      DWDL(1:NEL,1)=ZERO
      DWDL(1:NEL,2)=ZERO
      DWDL(1:NEL,3)=ZERO
      DO K = 1,NORDRE                                         
          DD  = MU(K)/AL(K)                               
          DO I=1,NEL
            SUM = THIRD*(EVMA1(I,K) + EVMA2(I,K) + EVMA3(I,K))                                        
            DWDL(I,1) = DWDL(I,1) + DD*(EVMA1(I,K) - SUM)   
            DWDL(I,2) = DWDL(I,2) + DD*(EVMA2(I,K) - SUM)   
            DWDL(I,3) = DWDL(I,3) + DD*(EVMA3(I,K) - SUM) 
          ENDDO  
      ENDDO                                                   
      PARTP(1:NEL) = ZERO                                          
      DO K = 1,NORDRE                                     
          IF (D(K) /= ZERO) THEN                            
            K2 = TWO*K
            I_K2 = 2*K
            DD = K2/D(K)   
            PARTP(1:NEL) = PARTP(1:NEL) + DD*(RV(1:NEL)-ONE)**(I_K2-1)
          ENDIF                                             
      ENDDO                                               
C
      T(1:NEL,1) = TWO*DWDL(1:NEL,1)/RV(1:NEL) + PARTP(1:NEL)
      T(1:NEL,2) = TWO*DWDL(1:NEL,2)/RV(1:NEL) + PARTP(1:NEL)
      T(1:NEL,3) = TWO*DWDL(1:NEL,3)/RV(1:NEL) + PARTP(1:NEL)
C-------------------------------------------------------------
      DO I=1,NEL
        UVAR(I,1) = EV(I,3)
      ENDDO
C-------------------------------------------------------------
C     transform principal Cauchy stress to global directions
      DO I=1,NEL
        SIGNXX(I) = EIGV(I,1,1)*T(I,1) + EIGV(I,1,2)*T(I,2)
        SIGNYY(I) = EIGV(I,2,1)*T(I,1) + EIGV(I,2,2)*T(I,2)
        SIGNXY(I) = EIGV(I,3,1)*T(I,1) + EIGV(I,3,2)*T(I,2)
        SIGNYZ(I) = SIGOYZ(I) + GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + GS(I)*DEPSZX(I)
      ENDDO
C-------------------------------------------------------------
C     set thickness, sound speed & viscosity
      DO I=1,NEL
        DEZZ(I)    =-NU/(ONE-NU)*(DEPSXX(I)+DEPSYY(I))
        THKN(I)    = THKN(I) + DEZZ(I)*THKLYL(I)
        RHO(I)     = RHO0(I)/RV(I)
        SOUNDSP(I) = SQRT((TWO_THIRD*GMAX+RBULK)/RHO(I))
        VISCMAX(I) = ZERO
      ENDDO
C-----------
      RETURN
      END
