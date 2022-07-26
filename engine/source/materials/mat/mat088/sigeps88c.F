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
Chd|  SIGEPS88C                     source/materials/mat/mat088/sigeps88c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
              SUBROUTINE SIGEPS88C(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      NPT0   , ILAYER , NGL   , OFF   , ISMSTR, GS   , 
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  ,
     3      AREA   , EINT   , THKLYL,
     4      EPSPXX , EPSPYY , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNXY, SIGNYZ, SIGNZX,
     A      SOUNDSP, VISCMAX, THKN  , UVAR     )
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
#include "tabsiz_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,ISMSTR,NPT0,ILAYER
      INTEGER, DIMENSION(NEL) :: NGL
      my_real 
     .     UPARAM(NUPARAM)
      my_real , DIMENSION(NEL,2) :: EINT
      my_real 
     .   TIME,TIMESTEP
      my_real, DIMENSION(NEL) :: 
     .  THKN,THKLYL, RHO0,AREA,GS,
     .  EPSPXX,EPSPYY,EPSPXY,EPSPYZ,EPSPZX,
     .  DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,
     .  EPSXX ,EPSYY ,EPSXY ,EPSYZ ,EPSZX ,
     .  SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL) ::
     .  SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX,SOUNDSP,VISCMAX
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION
C----------------------------------------------------------------
      INTEGER NPF(SNPC), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(STF),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER  IUNL_FOR,ICASE,I,ITER,NLOAD,NN,J
      my_real
     .   RBULK, NU,HYS, SHAPE,KT,Y3,A11,DD2,DD1,DD3,DF1,DF2,DF3,
     .   DFLAM3, EMAX, FAC,Y33,DF, P,INVR,YFAC(NFUNC),RATE(NFUNC),YFAC_UNL,
     .   SCALE,G_INI
      my_real, DIMENSION(NEL) :: T1,T2,T3,EPSZ,TRAV,ROOTV,F1,F2,F3,GMAX,
     .                           RV,DEZZ,EPSZZ
     .            
      my_real :: EE(NEL,3),EVV(NEL,3),EV(NEl,3), EIGV(NEL,3,3)
C=======================================================================
C material parameters
       RBULK     = UPARAM(1)
       NU        = UPARAM(2)
       GS        = UPARAM(3)
       NLOAD     = INT(UPARAM(4))
       IUNL_FOR  = INT(UPARAM(5))
       HYS       = UPARAM(6) 
       SHAPE     = UPARAM(7)
       ICASE      = NINT(UPARAM(9))
       DO J=1,NLOAD
         RATE(J) = UPARAM(9 + 2*J - 1 )  
         YFAC(J) = UPARAM(9 + 2*J )
       ENDDO
       YFAC_UNL = UPARAM(9 + 2*NLOAD + 2 )
C initialization 
      IF(TIME == ZERO)THEN
         UVAR(1:NEL,1:NUVAR) = ZERO 
         UVAR(1:NEL,1) = ONE 
      ENDIF      
      G_INI = THREE_HALF*RBULK*(ONE - TWO*NU)/(ONE + NU)
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
        IF(ABS(EVV(I,2)-EVV(I,1)).lt.EM10) THEN
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
          EV(I,3)=UVAR(I,1)
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
          EV(I,3)=UVAR(I,1)
        ENDDO
      ENDIF
      EE(1:NEL, 1) = EV(1:NEL, 1) - ONE
      EE(1:NEL, 2) = EV(1:NEL, 2) - ONE
      GMAX(1:NEL) = G_INI      
      DO I=1,NEL                                                         
        F1(I) = EV(I,1)*YFAC(1)*FINTER(IFUNC(1),EE(I,1),NPF,TF,DF1)      
        F2(I) = EV(I,2)*YFAC(1)*FINTER(IFUNC(1),EE(I,2),NPF,TF,DF2)      
        !!                                                               
        Gmax(I) = MAX(Gmax(I),    YFAC(1)*DF1, YFAC(1)*DF2 )             
        FAC = -HALF   
        DD1 = EV(I,1)**FAC - ONE 
        NN = 1                                                
        DO WHILE (ABS(DD1) >= EM03 .AND. NN <= 20)                                       
         F1(I) = F1(I) +  
     .         (DD1 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD1,NPF,TF,DF)
         FAC = FAC*(-HALF)                                               
         DD1 = EV(I,1)**FAC - ONE  
         NN  = NN  + 1
        ENDDO
        FAC = -HALF   
        DD2 = EV(I,2)**FAC - ONE
        NN = 1                                                     
        DO WHILE (ABS(DD2) >= EM03 .AND. NN <= 20 )                                     
         F2(I) = F2(I) +  
     .         (DD2 + ONE)*YFAC(1)*FINTER(IFUNC(1),DD2,NPF,TF,DF)        
         FAC = FAC*(-HALF)                                               
         DD2 = EV(I,2)**FAC - ONE  
         NN = NN + 1
        ENDDO                                                            
      ENDDO                                                              
! 
C--------------------------------------
C     Newton method =>  Find EV(3) : T3(EV(3)) = 0
C--------------------------------------            
       DO ITER = 1,5  
           DO I=1,NEL
                RV(I) = EV(I,1)*EV(I,2)*EV(I,3)
                EE(I,3) = EV(I,3) - ONE
                Y3  = YFAC(1)*FINTER(IFUNC(1),EE(I,3),NPF,TF,DF3)
                F3(I) = EV(I,3)*Y3
                Gmax(I) = MAX(Gmax(I),    YFAC(1)*DF3 ) 
                DFLAM3 = ZERO
                FAC = -HALF   
                SCALE = EV(I,3)**FAC 
                DD3 = SCALE - ONE 
                NN = 1                             
                DO WHILE (ABS(DD3) >= EM03 .AND. NN <= 20 ) 
                 Y33 = YFAC(1)*FINTER(IFUNC(1),DD3,NPF,TF,DF)
                 F3(I) = F3(I) +  SCALE*Y33
                 DFLAM3 = DFLAM3 + Y33*FAC * SCALE/EV(I,3) + YFAC(1)*FAC*SCALE**2*DF/EV(I,3)
                 FAC = FAC*(-HALF) 
                 SCALE = EV(I,3)**FAC
                 DD3 = SCALE - ONE
                 NN = NN + 1
                END DO
              
               INVR = ONE/MAX(EM20,RV(I))
               P = INVR*RBULK*(RV(I) - ONE)
               T1(I) = INVR*(TWO_THIRD*F1(I) - THIRD*(F2(I) + F3(I))) + P 
               T2(I) = INVR*(TWO_THIRD*F2(I) - THIRD*(F1(I) + F3(I))) + P 
               T3(I) = INVR*(TWO_THIRD*F3(I) - THIRD*(F1(I) + F2(I))) + P 
C
               KT = Y3 + EV(I,3)*YFAC(1)*DF3 + DFLAM3 
               KT = TWO_THIRD*KT*INVR + RBULK*INVR/EV(I,3) -
     .            INVR*(TWO_THIRD*F3(I) - THIRD*(F1(I)+F2(I)))/EV(I,3)
               IF(KT /= ZERO) EV(I,3) = EV(I,3) - T3(I)/KT  
           ENDDO ! NEL  
      ENDDO !  ITER 
C-------------------------------------------------------------                                  
C-------------------------------------------------------------
      IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN  ! engineering strain
        DO I=1,NEL
          EPSZZ(I) =EV(I,3) - ONE
          UVAR(I,1) = EV(I,3)
        ENDDO
      ELSEIF (ISMSTR == 10) THEN  ! left gauchy-green strain
        DO I=1,NEL
          EPSZZ(I) =EV(I,3) - ONE
          UVAR(I,1) = EV(I,3)
        ENDDO
      ELSE  ! true strain
        DO I=1,NEL
          EPSZZ(I) =LOG(EV(I,3))
          UVAR(I,1) = EV(I,3)
        ENDDO
      ENDIF
      DO I=1,NEL
        RV(I)   = EV(I,1)*EV(I,2)*EV(I,3)  
        DEZZ(I) =-NU/(ONE-NU)*(DEPSXX(I)+DEPSYY(I))
        SIGNXX(I) =EIGV(I,1,1)*T1(I)+EIGV(I,1,2)*T2(I)
        SIGNYY(I) =EIGV(I,2,1)*T1(I)+EIGV(I,2,2)*T2(I)
        SIGNXY(I) =EIGV(I,3,1)*T1(I)+EIGV(I,3,2)*T2(I)
C
        SIGNYZ(I) = SIGOYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I)+GS(I)*DEPSZX(I)
        THKN(I) = THKN(I) + DEZZ(I)*THKLYL(I)*OFF(I)
         VISCMAX(I)= ZERO
          EMAX = TWO*G_INI*(ONE + NU)
          A11  = EMAX/(ONE - NU**2)
          SOUNDSP(I)= SQRT(A11/RHO0(I))
      ENDDO
C-----------
      RETURN
      END
