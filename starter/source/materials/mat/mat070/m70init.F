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
!||====================================================================
!||    m70init   ../starter/source/materials/mat/mat070/m70init.F
!||--- called by ------------------------------------------------------
!||    matini    ../starter/source/materials/mat_share/matini.F
!||    sigin3b   ../starter/source/elements/solid/solid8p/sigin3b.F
!||====================================================================
      SUBROUTINE M70INIT(NEL ,NUPARAM ,NUVAR ,UPARAM ,UVAR) 
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C-----------------------------------------------
C  I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER :: NEL, NUPARAM, NUVAR
      my_real :: UPARAM(NUPARAM)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real :: UVAR(NEL,NUVAR)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER :: I, J
      my_real :: E0
C=======================================================================
      E0 = UPARAM(2)
      DO I=1,NEL                    
        UVAR(I,3) =E0
        UVAR(I,9) =ONE
        UVAR(I,10)=ONE
      ENDDO
c-----------
      RETURN
      END
