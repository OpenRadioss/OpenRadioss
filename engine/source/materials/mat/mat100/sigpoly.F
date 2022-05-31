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
C   Hyperelastic model: polynomial strain energy potential
C   compute stresses using MATB which is the 
C   left cauchy green tensor [b]=[F][FT]
Chd|====================================================================
Chd|  POLYSTRESS2                   source/materials/mat/mat100/sigpoly.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|        SIGEPS95                      source/materials/mat/mat095/sigeps95.F
Chd|-- calls ---------------
Chd|        MULLINS_OR                    source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|====================================================================
       SUBROUTINE POLYSTRESS2(
     1                NEL , MATB, C10, C01, C20, 
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3,  SIG,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "scr17_c.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "impl1_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER,  INTENT(IN) ::       NEL,FLAG_MUL,NVARF
      my_real,  INTENT(IN) ::  C10,C01,C20,C11,C02,C30,C21,C12,C03,
     .                        D1,D2,D3,
     .                        COEFR, BETAF,COEFM        

      my_real, DIMENSION(NEL, 3,3), INTENT(IN) :: MATB(NEL,3,3)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL), INTENT(OUT) ::  BI1,BI2,JDET 
      my_real, DIMENSION(NEL, 3,3), INTENT(OUT)   :: SIG
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL,NVARF), INTENT(INOUT) :: UVARF
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I

      my_real     
     .   Wmax,LPCHAIN(NEL), TRACE(NEL),TRACEB(NEL),J2THIRD(NEL),
     .   SB1(NEL), SB2(NEL),SB3(NEL),TBNORM(NEL),DGAMMA(NEL),I1(NEL),
     .   AA,BB,CC,TRB2,TRB22,
     .   I2(NEL),JTHIRD(NEL),J4THIRD(NEL),ETA(NEL),WW(NEL) ,
     .   DPHIDI1(NEL) ,DPHIDI2(NEL) , DPHIDJ(NEL) ,INV2J(NEL) 
C----------------------------------------------------------------
      ETA(1:NEL) = ONE !MULLINS DAMAGE FACTOR
 
      DO I = 1,NEL
         !J = RHO0/RHO = RELATIVE VOLUME = DET F (F = GRADIENT OF DEFORMATION)
         JDET(I)=MATB(I,1,1)*MATB(I,2,2)*MATB(I,3,3) -MATB(I,1,1)*MATB(I,2,3)*MATB(I,3,2) -
     .       MATB(I,3,3)*MATB(I,1,2)*MATB(I,2,1) +MATB(I,1,2)*MATB(I,2,3)*MATB(I,3,1) +
     .       MATB(I,2,1)*MATB(I,3,2)*MATB(I,1,3) -MATB(I,2,2)*MATB(I,3,1)*MATB(I,1,3)
         JDET(I)= SQRT(MAX(EM20, JDET(I)))    
         !SECOND INVARIANT 
         I1(I) = MATB(I,1,1)+MATB(I,2,2)+MATB(I,3,3)

         TRB2 =  I1(I)**2
         TRB22= MATB(I,1,1)**2+MATB(I,2,2)**2+MATB(I,3,3)**2
     .      +  TWO*( MATB(I,1,2)**2+MATB(I,1,3)**2+MATB(I,2,3)**2) 
         !I2 = ((TrB)**2 - Tr (B**2) )/2
         I2(I)= (TRB2 - TRB22)/TWO

         IF(JDET(I)>ZERO) THEN
          JTHIRD(I)  = EXP((-THIRD )*LOG(JDET(I)))
          J2THIRD(I) = JTHIRD(I)**2       
          J4THIRD(I) = JTHIRD(I)**4  
         ELSE
          JTHIRD(I)  = ZERO
          J2THIRD(I) = ZERO
          J4THIRD(I) = ZERO  
         ENDIF
      ENDDO
      DO I = 1,NEL
        !INVARIANT BAR
        !first invariant deviator BI1:
         BI1(I) = I1(I) * J2THIRD(I) 
        !SECOND INVARIANT  BAR
         BI2(I) = I2(I)*J4THIRD(I)    
      ENDDO
      Wmax = zero

       DO I = 1,NEL ! helmotz free energy for polynomial
         WW(I) =  C10 *(BI1(I)-THREE) + C20 *(BI1(I)-THREE)**2 + C30 *(BI1(I)-THREE)**3
     .   +        C01 *(BI2(I)-THREE) + C02 *(BI2(I)-THREE)**2 + C03 *(BI2(I)-THREE)**3
     .   +        C11 *(BI1(I)-THREE)*(BI2(I)-THREE) 
     .   +        C12 *(BI1(I)-THREE)*(BI2(I)-THREE)**2 
     .   +        C21 *(BI2(I)-THREE)*(BI1(I)-THREE)**2   
         Wmax = max (wmax, ww(i)) ! need deviatoric part for Mullins
       ENDDO
       !print*, '******* WMAX********** ' , WMAX
      !====================================
      !dammge by mullins effect
      !====================================
      IF(FLAG_MUL == 1)THEN
       CALL MULLINS_OR(
     1                NEL ,NVARF,  COEFR,BETAF ,
     2                COEFM, WW  , UVARF,ETA )
      ENDIF
      !====================================

      DO I = 1,NEL
        ETA(I) = MAX(MIN(ETA(I),ONE),EM20)
        ! derivatives of energy function 
        DPHIDI1(I) = ETA(I)*(C10 + TWO *C20 *(BI1(I)-THREE)+ THREE*C30 *(BI1(I)-THREE)**2
     .   +                               C11 *(BI2(I)-THREE)+      C12 *(BI2(I)-THREE)**2 
     .   +                         TWO *C21 *(BI1(I)-THREE)*(BI2(I)-THREE))

        DPHIDI2(I) = ETA(I)*(C01  + TWO*C02*(BI2(I)-THREE) +THREE*C03*(BI2(I)-THREE)**2
     .   +                               C11*(BI1(I)-THREE) +      C21*(BI1(I)-THREE)**2        
     .   +                          TWO*C12*(BI1(I)-THREE)*(BI2(I)-THREE))
        DPHIDJ(I)  = TWO*D1* (JDET(I)-ONE) + FOUR * D2 * (JDET(I)-ONE)**3 + SIX*D3*(JDET(I)-ONE)**5


        INV2J(I)=TWO/MAX(EM20,JDET(I))
      ENDDO

      DO I=1,NEL   
         !CAUCHY STRESS
         AA =  (DPHIDI1(I) +  DPHIDI2(I) * BI1(I))*INV2J(I)*J2THIRD(I)
         BB =  DPHIDI2(I) *INV2J(I)*J4THIRD(I)
         CC =  THIRD* INV2J(I)* ( BI1(I)* DPHIDI1(I)+TWO* BI2(I)*DPHIDI2(I))  

         SIG(I,1,1) =  AA*MATB(I,1,1)
     .       -   BB*MATB(I,1,1)**2  
     .       -   CC               
     .       +   DPHIDJ(I)        
         SIG(I,2,2) =  AA*MATB(I,2,2)
     .       -   BB*MATB(I,2,2)**2  
     .       -   CC               
     .       +   DPHIDJ(I)        
         SIG(I,3,3) =  AA*MATB(I,3,3)
     .       -   BB*MATB(I,3,3)**2  
     .       -   CC               
     .       +   DPHIDJ(I)        
         SIG(I,1,2) =  AA*MATB(I,1,2)
     .       -   BB*MATB(I,1,2)**2  
         SIG(I,2,3) =  AA*MATB(I,2,3)
     .       -   BB*MATB(I,2,3)**2  
         SIG(I,3,1) =  AA*MATB(I,3,1)
     .       -   BB*MATB(I,3,1)**2  
         SIG(I,2,1)=SIG(I,1,2)   
         SIG(I,3,2)=SIG(I,2,3)   
         SIG(I,1,3)=SIG(I,3,1)   
      ENDDO
C   
      RETURN
      END
C   Hyperelastic model: polynomial strain energy potential
C   compute stresses using EV(NEL,3) which are the 
C   stretches (using principal direction)
Chd|====================================================================
Chd|  POLYSTRESS                    source/materials/mat/mat100/sigpoly.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE POLYSTRESS(
     1                NEL ,EV,NUPARAM, UPARAM,T1,T2,T3,STIFF,
     2                BI1,BI2,JDET,EVB)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "scr17_c.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "impl1_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL , NUPARAM
      my_real
     .      EV(NEL,3),UPARAM(NUPARAM)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .        T1(NEL), T2(NEL),T3(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I

      my_real     
     .   LPCHAIN(NEL), TRACE(NEL),TRACEB(NEL),JDET(NEL),STIFF(NEL),
     .   SB1(NEL), SB2(NEL),SB3(NEL),TBNORM(NEL),DGAMMA(NEL),
     .   EVPN(NEL,3),EVEL(NEL,3),EVB(NEL,3),
     .   AA,BB,CC,C10,C01,C20,C11,C02,C30,C21,C12,C03,D1,D2,D3,
     .   I2(NEL),JTHIRD(NEL),J4THIRD(NEL),BI1(NEL),
     .   BI2(NEL),DPHIDI1(NEL) ,DPHIDI2(NEL) , DPHIDJ(NEL) ,INV2J(NEL) 
C----------------------------------------------------------------
      C10  =  UPARAM(1)   
      C01  =  UPARAM(2)   
      C20  =  UPARAM(3)   
      C11  =  UPARAM(4)   
      C02  =  UPARAM(5)   
      C30  =  UPARAM(6)   
      C21  =  UPARAM(7)   
      C12  =  UPARAM(8)   
      C03  =  UPARAM(9)   
      D1   =  UPARAM(11)  !ONE/D1
      !IF(D1 /= ZERO) D1 = ONE/D1
      D2   =  UPARAM(12)  !ONE/D2
      D3   =  UPARAM(13)  !ONE/D3

      DO I = 1,NEL
         !J = RHO0/RHO = RELATIVE VOLUME = DET F (F = GRADIENT OF DEFORMATION)
         JDET(I) = EV(I,1)*EV(I,2)*EV(I,3)
         !SECOND INVARIANT  
         I2(I)= EV(I,1)*EV(I,1)*EV(I,2)*EV(I,2)
     .        +EV(I,3)*EV(I,3)*EV(I,2)*EV(I,2)
     .        +EV(I,3)*EV(I,3)*EV(I,1)*EV(I,1)
         IF(JDET(I)>ZERO) THEN
          JTHIRD(I)  = EXP((-THIRD )*LOG(JDET(I)))
          J4THIRD(I) = EXP((-FOUR_OVER_3)*LOG(JDET(I)))
         ELSE
          JTHIRD(I) = ZERO
          J4THIRD(I)= ZERO  
         ENDIF
      ENDDO
      !-------------------------
      !chain A: compute stress : 
      !-------------------------
      DO I = 1,NEL
        !LAMBDA BAR
         EVB(I,1)=EV(I,1)*JTHIRD(I)
         EVB(I,2)=EV(I,2)*JTHIRD(I)
         EVB(I,3)=EV(I,3)*JTHIRD(I)      
        !first invariant deviator BI1:
         BI1(I) = EVB(I,1)*EVB(I,1)+EVB(I,2)*EVB(I,2)+EVB(I,3)*EVB(I,3)
        !SECOND INVARIANT  BAR
         BI2(I)= EVB(I,1)*EVB(I,1)*EVB(I,2)*EVB(I,2)
     .        + EVB(I,3)*EVB(I,3)*EVB(I,2)*EVB(I,2)
     .        + EVB(I,3)*EVB(I,3)*EVB(I,1)*EVB(I,1)
      ENDDO
      DO I = 1,NEL

        DPHIDI1(I) = C10 + TWO *C20 *(BI1(I)-THREE)+ THREE*C30 *(BI1(I)-THREE)**2
     .   +                       C11 *(BI2(I)-THREE)+      C12 *(BI2(I)-THREE)**2 
     .   +                 TWO *C21 *(BI1(I)-THREE)*(BI2(I)-THREE)

        DPHIDI2(I) = C01  + TWO*C02*(BI2(I)-THREE) +THREE*C03*(BI2(I)-THREE)**2
     .   +                       C11*(BI1(I)-THREE) +      C21*(BI1(I)-THREE)**2        
     .   +                  TWO*C12*(BI1(I)-THREE)*(BI2(I)-THREE)
        DPHIDJ(I)  = TWO*D1* (JDET(I)-ONE) + FOUR * D2 * (JDET(I)-ONE)**3 + SIX*D3*(JDET(I)-ONE)**5

        INV2J(I)=TWO/MAX(EM20,JDET(I))
      ENDDO

      DO I=1,NEL   
         !CAUCHY STRESS
         AA =  (DPHIDI1(I) +  DPHIDI2(I) * BI1(I))*INV2J(I)
         BB =  DPHIDI2(I) *INV2J(I)
         CC =  THIRD* INV2J(I)* ( BI1(I)* DPHIDI1(I)+TWO* BI2(I)*DPHIDI2(I))  
         T1(I) =  AA*EVB(I,1)*EVB(I,1)
     .       -   BB*EVB(I,1)**4   
     .       -   CC               
     .       +   DPHIDJ(I)        
         T2(I) =  AA*EVB(I,2)*EVB(I,2)
     .       -   BB*EVB(I,2)**4   
     .       -   CC               
     .       +   DPHIDJ(I)        
         T3(I) =  AA*EVB(I,3)*EVB(I,3)
     .       -   BB*EVB(I,3)**4   
     .        -   CC               
     .        +   DPHIDJ(I)        

!         STIFF(I) =( TWO *C20  + THREE*C30 *(BI1(I)-THREE) ) !IF YEOH
!     .           * ((TWO * EVB(I,1)**2 - EVB(I,2)**2 -  EVB(I,3)**2)**2         )*TWO/NINE
!     .           + (C10 + TWO *C20 *(BI1(I)-THREE)+ THREE*C30 *(BI1(I)-THREE)**2)
!     .           * (FOUR * EVB(I,1)**2 + EVB(I,2)**2 +  EVB(I,3)**2            )*FOUR/NINE
!     .           + TWO*D1*(  JDET(I)*(JDET(I)-ONE) + JDET(I)**2  )

      ENDDO
C   
      RETURN
      END
      
C-----------------------------------------------
Chd|====================================================================
Chd|  POLYSTREST2                   source/materials/mat/mat100/sigpoly.F
Chd|-- called by -----------
Chd|        SIGEPS95                      source/materials/mat/mat095/sigeps95.F
Chd|-- calls ---------------
Chd|        MULLINS_OR                    source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|====================================================================
       SUBROUTINE POLYSTREST2(
     1                NEL , MATB, C10, C01, C20, 
     2                C11 ,C02, C30, C21, C12,
     3                C03 ,D1 ,D2  ,  D3,  SIG,
     4                BI1,BI2,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF,
     6                CII )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "scr17_c.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "impl1_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER,  INTENT(IN) ::       NEL,FLAG_MUL,NVARF
      my_real,  INTENT(IN) ::  C10,C01,C20,C11,C02,C30,C21,C12,C03,
     .                        D1,D2,D3,
     .                        COEFR, BETAF,COEFM        

      my_real, DIMENSION(NEL, 3,3), INTENT(IN) :: MATB(NEL,3,3)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL), INTENT(OUT) ::  BI1,BI2,JDET 
      my_real, DIMENSION(NEL, 3,3), INTENT(OUT)   :: SIG
      my_real, DIMENSION(NEL, 3), INTENT(OUT)   :: CII
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL,NVARF), INTENT(INOUT) :: UVARF
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I

      my_real     
     .   Wmax,LPCHAIN(NEL), TRACE(NEL),TRACEB(NEL),J2THIRD(NEL),
     .   SB1(NEL), SB2(NEL),SB3(NEL),TBNORM(NEL),DGAMMA(NEL),I1(NEL),
     .   AA,BB,CC,TRB2,TRB22,
     .   I2(NEL),JTHIRD(NEL),J4THIRD(NEL),ETA(NEL),WW(NEL) ,
     .   DPHIDI1(NEL) ,DPHIDI2(NEL) , DPHIDJ(NEL) ,INV2J(NEL), 
     .   DPHI2DI1(NEL) ,DPHI2DI2(NEL) , DPHI2DJ(NEL),LAM_B(3),LAM_B_1(3),
     .   BI1_3,BI2_3	 
C----------------------------------------------------------------
      ETA(1:NEL) = ONE !MULLINS DAMAGE FACTOR
 
      DO I = 1,NEL
         !J = RHO0/RHO = RELATIVE VOLUME = DET F (F = GRADIENT OF DEFORMATION)
         JDET(I)=MATB(I,1,1)*MATB(I,2,2)*MATB(I,3,3) -MATB(I,1,1)*MATB(I,2,3)*MATB(I,3,2) -
     .       MATB(I,3,3)*MATB(I,1,2)*MATB(I,2,1) +MATB(I,1,2)*MATB(I,2,3)*MATB(I,3,1) +
     .       MATB(I,2,1)*MATB(I,3,2)*MATB(I,1,3) -MATB(I,2,2)*MATB(I,3,1)*MATB(I,1,3)
         JDET(I)= SQRT(MAX(EM20, JDET(I)))    
         !SECOND INVARIANT 
         I1(I) = MATB(I,1,1)+MATB(I,2,2)+MATB(I,3,3)

         TRB2 =  I1(I)**2
         TRB22= MATB(I,1,1)**2+MATB(I,2,2)**2+MATB(I,3,3)**2
     .      +  TWO*( MATB(I,1,2)**2+MATB(I,1,3)**2+MATB(I,2,3)**2) 
         !I2 = ((TrB)**2 - Tr (B**2) )/2
         I2(I)= (TRB2 - TRB22)/TWO

         IF(JDET(I)>ZERO) THEN
          JTHIRD(I)  = EXP((-THIRD )*LOG(JDET(I)))
          J2THIRD(I) = JTHIRD(I)**2       
          J4THIRD(I) = JTHIRD(I)**4  
         ELSE
          JTHIRD(I)  = ZERO
          J2THIRD(I) = ZERO
          J4THIRD(I) = ZERO  
         ENDIF
      ENDDO
      DO I = 1,NEL
        !INVARIANT BAR
        !first invariant deviator BI1:
         BI1(I) = I1(I) * J2THIRD(I) 
        !SECOND INVARIANT  BAR
         BI2(I) = I2(I)*J4THIRD(I)    
      ENDDO
      Wmax = zero

       DO I = 1,NEL ! helmotz free energy for polynomial
         WW(I) =  C10 *(BI1(I)-THREE) + C20 *(BI1(I)-THREE)**2 + C30 *(BI1(I)-THREE)**3
     .   +        C01 *(BI2(I)-THREE) + C02 *(BI2(I)-THREE)**2 + C03 *(BI2(I)-THREE)**3
     .   +        C11 *(BI1(I)-THREE)*(BI2(I)-THREE) 
     .   +        C12 *(BI1(I)-THREE)*(BI2(I)-THREE)**2 
     .   +        C21 *(BI2(I)-THREE)*(BI1(I)-THREE)**2   
         Wmax = max (wmax, ww(i)) ! need deviatoric part for Mullins
       ENDDO
       !print*, '******* WMAX********** ' , WMAX
      !====================================
      !dammge by mullins effect
      !====================================
      IF(FLAG_MUL == 1)THEN
       CALL MULLINS_OR(
     1                NEL ,NVARF,  COEFR,BETAF ,
     2                COEFM, WW  , UVARF,ETA )
      ENDIF
      !====================================

      DO I = 1,NEL
        ETA(I) = MAX(MIN(ETA(I),ONE),EM20)
        ! derivatives of energy function 
        DPHIDI1(I) = ETA(I)*(C10 + TWO *C20 *(BI1(I)-THREE)+ THREE*C30 *(BI1(I)-THREE)**2
     .   +                               C11 *(BI2(I)-THREE)+      C12 *(BI2(I)-THREE)**2 
     .   +                         TWO *C21 *(BI1(I)-THREE)*(BI2(I)-THREE))

        DPHIDI2(I) = ETA(I)*(C01  + TWO*C02*(BI2(I)-THREE) +THREE*C03*(BI2(I)-THREE)**2
     .   +                               C11*(BI1(I)-THREE) +      C21*(BI1(I)-THREE)**2        
     .   +                          TWO*C12*(BI1(I)-THREE)*(BI2(I)-THREE))
        DPHIDJ(I)  = TWO*D1* (JDET(I)-ONE) + FOUR * D2 * (JDET(I)-ONE)**3 + SIX*D3*(JDET(I)-ONE)**5


        INV2J(I)=TWO/MAX(EM20,JDET(I))
      ENDDO

      DO I=1,NEL   
         !CAUCHY STRESS
         AA =  (DPHIDI1(I) +  DPHIDI2(I) * BI1(I))*INV2J(I)*J2THIRD(I)
         BB =  DPHIDI2(I) *INV2J(I)*J4THIRD(I)
         CC =  THIRD* INV2J(I)* ( BI1(I)* DPHIDI1(I)+TWO* BI2(I)*DPHIDI2(I))  

         SIG(I,1,1) =  AA*MATB(I,1,1)
     .       -   BB*MATB(I,1,1)**2  
     .       -   CC               
     .       +   DPHIDJ(I)        
         SIG(I,2,2) =  AA*MATB(I,2,2)
     .       -   BB*MATB(I,2,2)**2  
     .       -   CC               
     .       +   DPHIDJ(I)        
         SIG(I,3,3) =  AA*MATB(I,3,3)
     .       -   BB*MATB(I,3,3)**2  
     .       -   CC               
     .       +   DPHIDJ(I)        
         SIG(I,1,2) =  AA*MATB(I,1,2)
     .       -   BB*MATB(I,1,2)**2  
         SIG(I,2,3) =  AA*MATB(I,2,3)
     .       -   BB*MATB(I,2,3)**2  
         SIG(I,3,1) =  AA*MATB(I,3,1)
     .       -   BB*MATB(I,3,1)**2  
         SIG(I,2,1)=SIG(I,1,2)   
         SIG(I,3,2)=SIG(I,2,3)   
         SIG(I,1,3)=SIG(I,3,1)   
      ENDDO
	  
      DO I = 1,NEL
        DPHI2DI1(I) = TWO*ETA(I)*(C20 + THREE*C30*(BI1(I)-THREE)+ C21*(BI2(I)-THREE)) 

        DPHI2DI2(I) = TWO*ETA(I)*(C02 +THREE*C03*(BI2(I)-THREE) + C12*(BI1(I)-THREE))        
        DPHI2DJ(I)  = TWO*D1 + TWELVE * D2 * (JDET(I)-ONE)**2 + THIRTY*D3*(JDET(I)-ONE)**4
        LAM_B(1) = MATB(I,1,1)*J2THIRD(I)
        LAM_B(2) = MATB(I,2,2)*J2THIRD(I)
        LAM_B(3) = MATB(I,3,3)*J2THIRD(I)
        LAM_B_1(1:3) = ONE/LAM_B(1:3)
		BI1_3 = THIRD*BI1(I)
		BI2_3 = THIRD*BI2(I)
        CII(I,1:3) = TWO*(TWO_THIRD*DPHIDI1(I)*(LAM_B(1:3)+BI1_3)+DPHI2DI1(I)*(LAM_B(1:3)-BI1_3)+
     .                     TWO_THIRD*DPHIDI2(I)*(LAM_B_1(1:3)+BI2_3)+DPHI2DI2(I)*(LAM_B_1(1:3)-BI2_3))  
     .               + DPHI2DJ(I)  
      ENDDO
C   
      RETURN
      END SUBROUTINE POLYSTREST2
