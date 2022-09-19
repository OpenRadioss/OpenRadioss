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
Chd|  TIME_MOD                      share/modules/time_mod.F      
Chd|-- called by -----------
Chd|        ADD_ELAPSED_TIME_MON_OFF      source/system/timer.F         
Chd|        PRINTIME                      source/system/timer.F         
Chd|        PRINT_RUNTIME                 source/system/timer.F         
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      MODULE TIME_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE TIME_TYPE
!--------------------------------------------------------------------
!	RST_NAME --> name of the restart
!       RUN_NBR --> number of engine run
!       STARTER_TIME --> elapsed time of the starter
!       ENGINE_TIME --> elapsed time of the engine
!--------------------------------------------------------------------
	CHARACTER(len=1000) :: RST_NAME
        INTEGER :: RUN_NBR
        REAL(kind=8) :: STARTER_TIME
        REAL(kind=8), DIMENSION(:), ALLOCATABLE :: ENGINE_TIME

      END TYPE TIME_TYPE

      TYPE(TIME_TYPE) :: GLOBAL_COMP_TIME
C
      END MODULE TIME_MOD
