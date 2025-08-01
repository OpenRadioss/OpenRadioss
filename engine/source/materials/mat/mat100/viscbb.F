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
C Non linear viscous model based on Bergstrom Boyce
C output: the creep strain increment DGAMMA
C-----------------------------------------------
!||====================================================================
!||    viscbb      ../engine/source/materials/mat/mat100/viscbb.F
!||--- called by ------------------------------------------------------
!||    sigeps100   ../engine/source/materials/mat/mat100/sigeps100.F90
!||    sigeps95    ../engine/source/materials/mat/mat095/sigeps95.F
!||====================================================================
        SUBROUTINE VISCBB ( NEL, FP, TBNORM, A1,EXPC, EXPM , KSI , TAUREF, DGAMMA ) !BERGSTROM BOYCE MODEL
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER,  INTENT(IN) ::       NEL
C  
      my_real,  INTENT(IN) :: A1,EXPC, EXPM , KSI ,TAUREF
C
      my_real, DIMENSION(NEL), INTENT(IN) ::  TBNORM 
C
      my_real, DIMENSION(NEL, 3,3) , INTENT(IN) ::  FP 
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
     .   TEMP, LPCHAIN(NEL), IP1(NEL)
C----------------------------------------------------------------
C----------------------------------------------------------------
         DO I=1,NEL         
          IP1(I) = FP(I,1,1)**2 + 
     .             FP(I,2,2)**2 + 
     .             FP(I,3,3)**2           
          LPCHAIN (I) = SQRT(MAX(ZERO,THIRD*IP1(I)))          
          !MAXL = MAX(MAXL, LPCHAIN (I))
          TEMP = MAX(EM20,LPCHAIN(I) - ONE +KSI)       
          DGAMMA(I) = A1* EXP(EXPC*LOG(TEMP))* ((TBNORM(I)**EXPM)/MAX(EM20, TAUREF**EXPM))   
         ENDDO

      RETURN
      END
C----------------------------------------------------------------
