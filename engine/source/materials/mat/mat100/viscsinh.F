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
C Non linear viscous model based on HYPERBOLIC SINE
C output: the creep strain increment DGAMMA
C-----------------------------------------------
Chd|====================================================================
Chd|  VISCSINH                      source/materials/mat/mat100/viscsinh.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE VISCSINH ( NEL, TBNORM,  A1, B0 ,EXPN, DGAMMA ) !HYPERBOLIC SINE
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER,  INTENT(IN) ::    NEL  
C
      my_real,  INTENT(IN) :: A1, EXPN ,B0  
C
      my_real, DIMENSION(NEL), INTENT(IN) ::  TBNORM
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
     .   TEMP,  IP1(NEL)
C----------------------------------------------------------------


        DO I=1,NEL 

          TEMP =( SINH (B0 *  TBNORM(I) ) )**EXPN  
          DGAMMA(I) = A1* TEMP 
        ENDDO
      RETURN
      END
C----------------------------------------------------------------

