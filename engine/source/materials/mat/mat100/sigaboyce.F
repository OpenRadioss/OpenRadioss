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
C   Hyperelastic model: ARRUDA BOYCE strain energy potential
C   compute stresses using MATB which is the 
C   left cauchy green tensor [b]=[F][FT]
Chd|====================================================================
Chd|  SIGABOYCE                     source/materials/mat/mat100/sigaboyce.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|-- calls ---------------
Chd|        MULLINS_OR                    source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|====================================================================
       SUBROUTINE SIGABOYCE(
     1                NEL , MATB ,C1,C2,C3,
     3                C4  ,C5   ,MU,BETA,D ,
     4                SIG ,BI1  ,JDET ,FLAG_MUL,
     5                NVARF,COEFR, BETAF,COEFM  ,UVARF)
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER,  INTENT(IN) ::       NEL,FLAG_MUL,NVARF
      my_real,  INTENT(IN) :: C1,C2,C3,C4,C5,MU,BETA,D ,
     .                        COEFR, BETAF,COEFM          
      my_real, DIMENSION(NEL, 3,3) , INTENT(IN) ::  MATB 
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL), INTENT(OUT) :: BI1 ,JDET 
      my_real, DIMENSION(NEL, 3,3)  :: SIG  
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL,NVARF), INTENT(INOUT) :: UVARF
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I

      my_real     
     .   J2THIRD(NEL),I1(NEL),JTHIRD(NEL),WW (NEL),
     .   DPHIDI1(NEL) ,DPHIDJ(NEL) ,ETA(NEL)
C----------------------------------------------------------------
      ETA(1:NEL) = ONE !MULLINS DAMAGE FACTOR
      DO I = 1,NEL
         JDET(I)=MATB(I,1,1)*MATB(I,2,2)*MATB(I,3,3) -MATB(I,1,1)*MATB(I,2,3)*MATB(I,3,2) -
     .       MATB(I,3,3)*MATB(I,1,2)*MATB(I,2,1) +MATB(I,1,2)*MATB(I,2,3)*MATB(I,3,1) +
     .       MATB(I,2,1)*MATB(I,3,2)*MATB(I,1,3) -MATB(I,2,2)*MATB(I,3,1)*MATB(I,1,3)
         JDET(I)= SQRT(MAX(EM20, JDET(I)))    
        !INVARIANT BAR         
         I1(I) = MATB(I,1,1)+MATB(I,2,2)+MATB(I,3,3)
         IF(JDET(I)>ZERO) THEN
          JTHIRD(I)  = EXP((-THIRD )*LOG(JDET(I)))
          J2THIRD(I) = JTHIRD(I)**2       
         ELSE
          JTHIRD(I)  = ZERO
          J2THIRD(I) = ZERO
         ENDIF
        !first invariant deviator BI1:
         BI1(I) = I1(I) * J2THIRD(I) 
      ENDDO
      !====================================
      !dammge by mullins effect
      !====================================
      IF(FLAG_MUL == 1)THEN
       DO I = 1,NEL
         WW(I) =  MU *( C1 * (BI1(I)- THREE)+C2 * BETA *   (BI1(I)**2- NINE)
     .   +                                   C3 * BETA**2 *(BI1(I)**3- THREE**3)    
     .   +                                   C4 * BETA**3 *(BI1(I)**4- THREE**4)
     .   +                                   C5 * BETA**4 *(BI1(I)**5- THREE**5) )

       ENDDO
       CALL MULLINS_OR(
     1                NEL ,NVARF, COEFR,BETAF ,
     2                COEFM, WW  , UVARF,ETA )

      ENDIF
      !====================================

      DO I = 1,NEL
        ETA(I) = MAX(MIN(ETA(I),ONE),EM20)
        DPHIDI1(I) = ETA(I)* TWO*MU *( C1 + TWO * C2 * BETA * BI1(I)
     .   +                           THREE* C3 *(BETA *BI1(I))**2    
     .   +                          FOUR* C4 *(BETA *BI1(I))**3 
     .   +                          FIVE  * C5 *(BETA *BI1(I))**4)/MAX(EM20,JDET(I))

        DPHIDJ(I)  = D * (  JDET(I) - ONE /MAX(EM20,JDET(I)) ) !d=1/d
      ENDDO
      DO I = 1,NEL
         SIG(I,1,1) =  DPHIDI1(I)* (MATB(I,1,1)-THIRD*BI1(I))
     .       +   DPHIDJ(I)        
         SIG(I,2,2) =  DPHIDI1(I)* (MATB(I,2,2)-THIRD*BI1(I))
     .       +   DPHIDJ(I)        
         SIG(I,3,3) =  DPHIDI1(I)* (MATB(I,3,3)-THIRD*BI1(I))
     .       +   DPHIDJ(I)        
         SIG(I,1,2) =  DPHIDI1(I)*MATB(I,1,2)
         SIG(I,2,3) =  DPHIDI1(I)*MATB(I,2,3)
         SIG(I,3,1) =  DPHIDI1(I)*MATB(I,3,1)
         SIG(I,2,1)=SIG(I,1,2)   
         SIG(I,3,2)=SIG(I,2,3)   
         SIG(I,1,3)=SIG(I,3,1)   
      ENDDO

      RETURN
      END
C   Hyperelastic model: ARRUDA BOYCE strain  energy potential
