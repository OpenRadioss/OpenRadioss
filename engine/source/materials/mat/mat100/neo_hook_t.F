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
C   Hyperelastic model: THERMAL DEPENDANT NEO-HOOK
C   compute stresses using MATB which is the 
C   left cauchy green tensor [b]=[F][FT]
Chd|====================================================================
Chd|  NEO_HOOK_T                    source/materials/mat/mat100/neo_hook_t.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|-- calls ---------------
Chd|        MULLINS_OR                    source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|====================================================================
       SUBROUTINE NEO_HOOK_T(
     1                NEL , MATB,SIG,
     4                BI1,JDET ,FLAG_MUL,MU,D,
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
      my_real,  INTENT(IN) ::  COEFR, BETAF,COEFM

      my_real, DIMENSION(NEL),  INTENT(IN) ::  MU,D
      my_real, DIMENSION(NEL, 3,3), INTENT(IN) :: MATB(NEL,3,3)
C      my_real, DIMENSION(NEL), INTENT(IN) :: MU
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL), INTENT(OUT) ::  BI1,JDET
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
     .   LPCHAIN(NEL), TRACE(NEL),TRACEB(NEL),J2THIRD(NEL),
     .   SB1(NEL), SB2(NEL),SB3(NEL),TBNORM(NEL),DGAMMA(NEL),I1(NEL),
     .   AA,BB,CC,TRB2,TRB22,INVJ(NEL),
     .   JTHIRD(NEL),J4THIRD(NEL),ETA(NEL),WW(NEL) ,
     .   DPHIDI1(NEL)  , DPHIDJ(NEL)
C----------------------------------------------------------------
      ETA(1:NEL) = ONE !MULLINS DAMAGE FACTOR

      DO I = 1,NEL
         !J = RHO0/RHO = RELATIVE VOLUME = DET F (F = GRADIENT OF DEFORMATION)
         JDET(I)=MATB(I,1,1)*MATB(I,2,2)*MATB(I,3,3) -MATB(I,1,1)*MATB(I,2,3)*MATB(I,3,2) -
     .       MATB(I,3,3)*MATB(I,1,2)*MATB(I,2,1) +MATB(I,1,2)*MATB(I,2,3)*MATB(I,3,1) +
     .       MATB(I,2,1)*MATB(I,3,2)*MATB(I,1,3) -MATB(I,2,2)*MATB(I,3,1)*MATB(I,1,3)
         JDET(I)= SQRT(MAX(EM20, JDET(I)))

         !FIRST INVARIANT
         I1(I) = MATB(I,1,1)+MATB(I,2,2)+MATB(I,3,3)

         IF(JDET(I)>ZERO) THEN
          J2THIRD(I)  = EXP((-TWO_THIRD )*LOG(JDET(I)))
         ELSE
          J2THIRD(I)  = ZERO
         ENDIF
      ENDDO
      DO I = 1,NEL
        !INVARIANT BAR
        !first invariant deviator BI1:
         BI1(I) = I1(I) * J2THIRD(I)
      ENDDO


      !====================================
      !dammge by mullins effect
      !====================================
      IF(FLAG_MUL == 1)THEN
       DO I = 1,NEL
         WW(I) =  MU(I) *(BI1(I)-THREE) /TWO

       ENDDO
       CALL MULLINS_OR(
     1                NEL ,NVARF,  COEFR,BETAF ,
     2                COEFM, WW  , UVARF,ETA )
      ENDIF
      !====================================

      DO I = 1,NEL
        ETA(I) = MAX(MIN(ETA(I),ONE),EM20)
        DPHIDI1(I) = ETA(I)*MU(I)/TWO
        DPHIDJ(I)  = D(I)* (JDET(I)-ONE)
        INVJ(I)=ONE/MAX(EM20,JDET(I))
      ENDDO

      DO I=1,NEL
         !CAUCHY STRESS
         AA =  ETA(I)*MU(I)/MAX(EM20,JDET(I))

         SIG(I,1,1) =  AA*(MATB(I,1,1)-THIRD*I1(I))
     .       +   DPHIDJ(I)
         SIG(I,2,2) =  AA*(MATB(I,2,2)-THIRD*I1(I))
     .       +   DPHIDJ(I)
         SIG(I,3,3) =  AA*(MATB(I,3,3)-THIRD*I1(I))
     .       +   DPHIDJ(I)
         SIG(I,1,2) =  AA*MATB(I,1,2)
         SIG(I,2,3) =  AA*MATB(I,2,3)
         SIG(I,3,1) =  AA*MATB(I,3,1)
         SIG(I,2,1)=SIG(I,1,2)
         SIG(I,3,2)=SIG(I,2,3)
         SIG(I,1,3)=SIG(I,3,1)
      ENDDO
C   
      RETURN
      END
