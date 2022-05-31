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
C Non linear viscous model POWER LAW
C output: the creep strain increment DGAMMA
C-----------------------------------------------
Chd|====================================================================
Chd|  VISCPOWER                     source/materials/mat/mat100/viscpower.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE VISCPOWER ( NEL, TBNORM,  A1,EXPM ,EXPN,GAMMAOLD, DGAMMA ) !POWER LAW
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
      INTEGER, INTENT(IN) ::  NEL  
C
      my_real,  INTENT(IN) :: A1, EXPN ,EXPM  
C
      my_real, DIMENSION(NEL), INTENT(IN) ::  TBNORM,GAMMAOLD
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL), INTENT(OUT) ::  DGAMMA
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I

      my_real     
     .   TEMP1 ,TEMP2 , TEMP3  
C----------------------------------------------------------------
        DO I=1,NEL 
          
          TEMP1 = (EXPM +ONE ) * GAMMAOLD(I)
          TEMP2 =  EXP(EXPN*LOG(TBNORM(I))) 
          TEMP3 =  EXP( EXPM*LOG(TEMP1) )     
          DGAMMA(I) = A1* EXP(  (ONE/(ONE +EXPM)) *LOG(TEMP2 * TEMP3 )) 

        ENDDO
      RETURN
      END
C----------------------------------------------------------------

