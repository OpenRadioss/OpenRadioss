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
Chd|  MID_PID_MOD                   share/modules/mid_pid_mod.F   
Chd|-- called by -----------
Chd|        SORT_MID_PID                  source/system/sort_mid_pid.F  
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MID_PID_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE MID_PID_TYPE
!--------------------------------------------------------------------
!       MID_PID_SHELL(MID)%PID1D( : ) --> (mid ; pid) pair
!       MID_PID_SHELL(MID)%GROUP1D --> number of Group for each (mid,pid)
!--------------------------------------------------------------------
        INTEGER, DIMENSION(:), ALLOCATABLE :: PID1D, GROUP1D 
           
      END TYPE MID_PID_TYPE
C
      END MODULE MID_PID_MOD
