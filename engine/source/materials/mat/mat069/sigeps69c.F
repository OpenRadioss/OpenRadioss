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
Chd|  SIGEPS69C                     source/materials/mat/mat069/sigeps69c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
              SUBROUTINE SIGEPS69C(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      NPT0   , ILAYER ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  ,
     3      AREA   , EINT   , THKLYL,
     4      EPSPXX , EPSPYY , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, THKN  , UVAR  , NGL   ,
     B      OFF    , ISMSTR, IPM     , GS   ,MAT    ,
     C      NUVARV ,UVARV )
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
      INTEGER NEL,NUPARAM,NUVAR,ISMSTR,NPT0,ILAYER,NUVARV
      INTEGER IPM(NPROPMI,*),MAT(NEL),NGL(NEL)
      my_real
     .   TIME,TIMESTEP
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
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL),UVARV(*)
!!     .      UVAR(NEL,NUVAR), OFF(NEL),UVARV(NEL*NUVARV)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER  I,J,K,ITER,NORDRE,NPRONY,IVISC,II,IADBUF,JNV
      my_real
     .   NU,RBULK,TENSCUT,GMAX,RVT,SUM,SUMDWDL,PARTP,KT3(NEL),
     .   EMAX,A11         
      my_real
     .   MU(5),AL(5),EVMA1(NEL,5),EVMA2(NEL,5),EVMA3(NEL,5),EVM(NEL,3),
     .   EIGV(NEL,3,2),TRAV(NEL),ROOTV(NEL),EVV(NEL,3),EV(NEL,3),
     .   RHO(NEL),DEZZ(NEL),DWDL(3),DDWDDL(3),RV(NEL),T(NEL,3),EA(NEL),
     .   EPSZZ(NEL),GI(100),BETA(100),SV(NEL,3),
     .   FAC,H30(100),H31(NEL,100),H1(100),H10(100),
     .   H2(100),H20(100),H12(100),H120(100),SV3,
     .   CD1(NEL),CD2(NEL),CD12(NEL),CD10(NEL),
     .   CD20(NEL),CD120(NEL), CP1,CP2,DC3EV3(NEL),C31(NEL),C30(NEL)
C=======================================================================
      IADBUF = IPM(7,MAT(1))
      MU(1)  = UPARAM(IADBUF)
      MU(2)  = UPARAM(IADBUF+1)
      MU(3)  = UPARAM(IADBUF+2)
      MU(4)  = UPARAM(IADBUF+3)
      MU(5)  = UPARAM(IADBUF+4)
      AL(1)  = UPARAM(IADBUF+5)
      AL(2)  = UPARAM(IADBUF+6)
      AL(3)  = UPARAM(IADBUF+7)
      AL(4)  = UPARAM(IADBUF+8)
      AL(5)  = UPARAM(IADBUF+9)
      RBULK  = UPARAM(IADBUF+10)
      TENSCUT= UPARAM(IADBUF+11)
      NU     = UPARAM(IADBUF+13)
      NORDRE = NINT(UPARAM(IADBUF+17))
      GMAX = ZERO
      IVISC = 0
      
C add viscosity using /visc/prony
      IF(IPM(222,MAT(1)) > 0) THEN 
         IADBUF = IPM(223,MAT(1))
         NPRONY =  INT(UPARAM(IADBUF +1 ))
         IVISC = 1
c             
         DO I=1,NPRONY
          GI(I)    = UPARAM(IADBUF + 1 + I)
          BETA(I)  = UPARAM(IADBUF + 1 + NPRONY + I)
          GMAX = GMAX + GI(I)
         ENDDO  
       ENDIF
C             
       DO I= 1,NORDRE
        GMAX  = GMAX  + MU(I)*AL(I)
       ENDDO           
C
C     User variables initialisation
      IF (TIME == ZERO .AND. ISIGI == 0) THEN 
        DO I=1,NEL                            
          DO J=1,NUVAR                         
            UVAR(I,J) = ZERO                   
          ENDDO                                
          UVAR(I,3) = ONE                      
        ENDDO                                 
      ENDIF                                   
C
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
C Like law42 
       IF(IVISC == 0)  THEN                                                       
         DO ITER = 1,5
!       ----------------
          DO I=1,NEL  
            RV(I) = EV(I,1)*EV(I,2)*EV(I,3)  
c----  la   normalized stretch => unified compressible/uncompressible formution                   
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
!       ----------------
         ENDDO    ! ITER = 1,5   
         SV(1:NEL,1) = ZERO    
         SV(1:NEL,2) = ZERO   
         SV(1:NEL,3) = ZERO          
      ELSE ! with viscosity                                                       
         DO ITER = 1,5
!       ---------------- 
          DO I=1,NEL  
            RV(I) = EV(I,1)*EV(I,2)*EV(I,3)  
c----  la   normalized stretch => unified compressible/uncompressible formution                                                    
            IF(RV(I)> ZERO) THEN
             RVT    = EXP((-THIRD)*LOG(RV(I)))
            ELSE
             RVT = ZERO
            ENDIF            
            EVM(I,1) = EV(I,1)*RVT                                        
            EVM(I,2) = EV(I,2)*RVT                                        
            EVM(I,3) = EV(I,3)*RVT
          ENDDO    
!       ----------------                                    
C----       partial derivatives of strain energy
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
           ENDDO
          ENDDO       ! j=1,5 
!       ----------------          
C  
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
                                                                  
C viscosty model the same as law42            
           C30(I) = UVAR(I,5) 
           SUM = THIRD*(EVM(I,1)**2 +  EVM(I,2)**2 + EVM(I,3)**2)
           C31(I)   =  EVM(I,3)**2 - SUM 
!
           DC3EV3(I) = FOUR_OVER_3*RVT*EVM(I,3)-TWO_THIRD*(TWO_THIRD*EVM(I,3)**2 - 
     .                                          THIRD* EVM(I,1)**2 - 
     .                                          THIRD* EVM(I,2)**2)/EV(I,3)
          ENDDO
!       ----------------
          DO II= 1,NPRONY
             FAC= -TIMESTEP*BETA(II)
             DO I=1,NEL 
                 H30(II)  =  UVARV(4*NEL*(II-1) + I)                
                 H31(I,II)  = EXP(FAC)*H30(II)+ EXP(HALF*FAC)*(C31(I) - C30(I))
C           Kirchoff visco stress --->
C   PK2 stress, PK2 = F**(-1)*Taux* F**(-T)n 
C   cauchy =Taux/RV is used here
                  FAC= -TIMESTEP*BETA(II)
                  T(I,3) = T(I,3) + GI(II)*H31(I,II)/RV(I)
                  KT3(I)    = KT3(I) - GI(II)*H31(I,II)/EV(I,3)/RV(I)  
     .                 + DC3EV3(I)*GI(II)*EXP(HALF*FAC)/RV(I)
              ENDDO ! 1,NEL
          ENDDO  ! NPRONY
          EV(1:NEL,3) = EV(1:NEL,3)  - T(1:NEL,3)/KT3(1:NEL)                                        
          RV(1:NEL)   = EV(1:NEL,1)*EV(1:NEL,2)*EV(1:NEL,3)   
!       ----------------        
         ENDDO  ! iteration 
         UVAR(1:NEL,5) = C31(1:NEL)
!       ----------------
         DO II= 1,NPRONY
              DO I=1,NEL
               UVARV(4*NEL*(II-1) + I)   =  H31(I,II)
              ENDDO
         ENDDO
!       ----------------
! compute viscos stress
         DO I=1,NEL                                               
           IF(RV(I)> ZERO) THEN
             RVT    = EXP((-THIRD)*LOG(RV(I)))
           ELSE
             RVT = ZERO
           ENDIF 
           EVM(I,1) = EV(I,1)*RVT
           EVM(I,2) = EV(I,2)*RVT
           EVM(I,3) = EV(I,3)*RVT
C           
           CD10(I) =  UVAR(I,6) 
           CD20(I) =  UVAR(I,7) 
           CD120(I) = UVAR(I,8) 
C
           SUM  = THIRD*(EVM(I,1)**2 +  EVM(I,2)**2 + EVM(I,3)**2) 
           CP1   =  EVM(I,1)**2 - SUM                          
           CP2   =  EVM(I,2)**2 - SUM
           CD1(I)  = EIGV(I,1,1)*CP1 + EIGV(I,1,2)*CP2
           CD2(I)  = EIGV(I,2,1)*CP1 + EIGV(I,2,2)*CP2
           CD12(I) = EIGV(I,3,1)*CP1 + EIGV(I,3,2)*CP2                        
           UVAR(I,6) = CD1(I)
           UVAR(I,7) = CD2(I)
           UVAR(I,8) = CD12(I)
         ENDDO  ! 1,NEL
!       ----------------
         SV(1:NEL,1) = ZERO
         SV(1:NEL,2) = ZERO
         SV(1:NEL,3) = ZERO
!       ---------------- 
         DO II= 1,NPRONY
           DO I=1,NEL 
              FAC= -TIMESTEP*BETA(II)                          
              H10(II)   =  UVARV(4*NEL*(II-1) +   NEL + I)           
              H20(II)   =  UVARV(4*NEL*(II-1) + 2*NEL + I)  
              H120(II)  =  UVARV(4*NEL*(II-1) + 3*NEL + I)
              H1(II)  = EXP(FAC)*H10(II)+ EXP(HALF*FAC)*(CD1(I) - CD10(I))                
              H2(II)  = EXP(FAC)*H20(II)+ EXP(HALF*FAC)*(CD2(I) - CD20(I))              
              H12(II)  = EXP(FAC)*H120(II)+ EXP(HALF*FAC)*(CD12(I) - CD120(I)) 
              UVARV(4*NEL*(II-1) +   NEL + I)= H1(II)              
              UVARV(4*NEL*(II-1) + 2*NEL + I)= H2(II)   
              UVARV(4*NEL*(II-1) + 3*NEL + I)= H12(II)        
C         Kirchoff visco stress
              SV(I,1) = SV(I,1) + GI(II)*H1(II)
              SV(I,2) = SV(I,2) + GI(II)*H2(II)
              SV(I,3) = SV(I,3) + GI(II)*H12(II)
           ENDDO       ! 1,NPRONY                                        
         ENDDO          ! 1,NEL  
!       ----------------
      ENDIF                                      
C-------------------------------------------------------------
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
!!        DEZZ(I) = EPSZZ(I) - UVAR(I,4)
        SIGNXX(I) =EIGV(I,1,1)*T(I,1)+EIGV(I,1,2)*T(I,2) + SV(I,1)/RV(I)
        SIGNYY(I) =EIGV(I,2,1)*T(I,1)+EIGV(I,2,2)*T(I,2) + SV(I,2)/RV(I)
        SIGNXY(I) =EIGV(I,3,1)*T(I,1)+EIGV(I,3,2)*T(I,2) + SV(I,3)/RV(I)
C
        SIGNYZ(I) = SIGOYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I)+GS(I)*DEPSZX(I)
        RHO(I)    = RHO0(I)/RV(I)
        THKN(I) = THKN(I) + DEZZ(I)*THKLYL(I)*OFF(I)
         VISCMAX(I)= ZERO
!!          UVAR(I,4) = EPSZZ(I)  
C         
           
          EMAX = GMAX*(ONE + NU)
!C           EMAX = MAX(EMAX,EA(I)) 
          A11  = EMAX/(ONE - NU**2)
          SOUNDSP(I)= SQRT(A11/RHO0(I))
      ENDDO
C-----------
      RETURN
      END
