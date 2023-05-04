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
Chd|  SENSOR_TAB_INIT               source/tools/sensor/sensor_tab_init.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      SUBROUTINE SENSOR_TAB_INIT(SENSORS)
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
      TYPE (SENSORS_) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
      DO I=1,SENSORS%NSENSOR
        SENSORS%SENSOR_TAB(I)%TYPE    = -1
        SENSORS%SENSOR_TAB(I)%SENS_ID = 0  
        SENSORS%SENSOR_TAB(I)%STATUS  = 0     
        SENSORS%SENSOR_TAB(I)%TCRIT   = 0     
        SENSORS%SENSOR_TAB(I)%TMIN    = 0     
        SENSORS%SENSOR_TAB(I)%TDELAY  = 0    
        SENSORS%SENSOR_TAB(I)%TSTART  = 0   
        SENSORS%SENSOR_TAB(I)%VALUE   = 0   
        SENSORS%SENSOR_TAB(I)%NPARI   = 0   
        SENSORS%SENSOR_TAB(I)%NPARR   = 0   
        SENSORS%SENSOR_TAB(I)%NVAR    = 0   
      END DO
C------------
      RETURN
      END 
