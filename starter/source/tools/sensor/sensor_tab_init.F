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
Chd|  SENSOR_TAB_INIT               source/tools/sensor/sensor_tab_init.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      SUBROUTINE SENSOR_TAB_INIT(SENSOR_TAB,NSENSOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSENSOR
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), TARGET :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NVAR
      TYPE (SENSOR_STR_) ,POINTER :: SENSOR_PTR
C=======================================================================
      DO I=1,NSENSOR
        SENSOR_PTR => SENSOR_TAB(I)
        SENSOR_PTR%TYPE    = -1
        SENSOR_PTR%SENS_ID = 0  
        SENSOR_PTR%STATUS  = 0     
        SENSOR_PTR%TCRIT   = 0     
        SENSOR_PTR%TMIN    = 0     
        SENSOR_PTR%TDELAY  = 0    
        SENSOR_PTR%TSTART  = 0   
        SENSOR_PTR%VALUE   = 0   
        SENSOR_PTR%NPARI   = 0   
        SENSOR_PTR%NPARR   = 0   
        SENSOR_PTR%NVAR    = 0   
      END DO
C------------
      RETURN
      END 
