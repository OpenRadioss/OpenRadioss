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
C----------------------------------------------------------------
C compute the left cauchy green MATB in 3 steps:
C INVFPO = INVERSE (FP)
C FE = F * INVFPO
C MATB = FE * FET 
Chd|====================================================================
Chd|  CALCMATB                      source/materials/mat/mat100/calcmatb.F
Chd|-- called by -----------
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|        SIGEPS95                      source/materials/mat/mat095/sigeps95.F
Chd|-- calls ---------------
Chd|        KMATINV3                      source/materials/tools/kmatinv.F
Chd|        PRODAAT                       source/materials/tools/prodAAT.F
Chd|        PRODMAT                       source/materials/tools/prodmat.F
Chd|====================================================================
       SUBROUTINE CALCMATB (NEL, F, FP, MATB)    
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL  
      my_real
     .    F (NEL, 3,3),    FP (NEL, 3,3)   
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .       MATB(NEL, 3,3) 
C----------------------------------------------------------------
      INTEGER    I
      my_real
     .       INVFPO(NEL, 3,3) , FE(NEL, 3,3), FET(NEL, 3,3)
C----------------------------------------------------------------
      
       !COMPUTE FP viscous GRADIENT OF DEF
       !------------------------------------   
       !F=FE*FP
       CALL KMATINV3(FP , INVFPO, NEL) !INVFPO = INVERSE (FP)  
       CALL PRODMAT  (F , INVFPO, FE, NEL) ! FE = F * INVFPO      

       CALL PRODAAT(FE , MATB, NEL) ! 
      RETURN
      END
