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
Chd|  M95INIT                       source/materials/mat/mat095/m95init.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M95INIT(NEL    , NUVAR   ,UVAR   ) 
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER                        , INTENT(IN)     ::NEL, NUVAR
      my_real,  DIMENSION(NEL,NUVAR) , INTENT(INOUT)  :: UVAR
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER         I
C=======================================================================
       DO I=1,NEL                    
         UVAR(I,1) = ONE
         UVAR(I,2) = ONE
         UVAR(I,3) = ONE
         UVAR(I,4) = ZERO
         UVAR(I,5) = ZERO
         UVAR(I,6) = ZERO
         UVAR(I,7) = ZERO
         UVAR(I,8) = ZERO
         UVAR(I,9) = ZERO
         UVAR(I,10) = ZERO
       ENDDO

      RETURN
      END
