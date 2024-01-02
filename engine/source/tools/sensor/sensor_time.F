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
C=======================================================================
Chd|====================================================================
Chd|  SENSOR_TIME                   source/tools/sensor/sensor_time.F
Chd|-- called by -----------
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|        SENSOR_INIT                   source/tools/sensor/sensor_init.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_TIME(SENSOR ,TIME   ,TIMESTEP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "comlock.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real            ,INTENT(IN)    :: TIME,TIMESTEP
      TYPE (SENSOR_STR_) ,INTENT(INOUT) :: SENSOR
C----------------------------------------------------------
C   Local Variables
C----------------------------------------------------------
      INTEGER :: NSENS
      my_real :: TDELAY,TSTOP,INFINITY
      DOUBLE PRECISION :: TDELAY_DP,TSTOP_DP,NEXT_CYCLE
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      NSENS  = SENSOR%SENS_ID
      TDELAY = SENSOR%TDELAY
      TSTOP  = SENSOR%VALUE
      NEXT_CYCLE = TIME + TIMESTEP
      TDELAY_DP  = TDELAY
      TSTOP_DP   = TSTOP
c
      IF (NEXT_CYCLE >= TDELAY_DP .AND. SENSOR%STATUS == 0) THEN
        SENSOR%STATUS = 1
        SENSOR%TSTART = TDELAY
        IF (ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (IOUT ,1000) SENSOR%SENS_ID,TDELAY
          WRITE (ISTDO,1000) SENSOR%SENS_ID,TDELAY
#include "lockoff.inc"
        ENDIF
      ELSE IF (NEXT_CYCLE >= TSTOP_DP .AND. SENSOR%STATUS == 1) THEN
        SENSOR%STATUS = 0
        SENSOR%TSTART = INFINITY
        SENSOR%TDELAY = INFINITY
        IF (ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (IOUT ,2000) SENSOR%SENS_ID,TSTOP
          WRITE (ISTDO,2000) SENSOR%SENS_ID,TSTOP
#include "lockoff.inc"
        ENDIF
      ENDIF
c-----------------------------------------------------------------------      
1000  FORMAT(' SENSOR (TIME) NUMBER ',I10,' ACTIVATED AT TIME ',1PE12.5)
2000  FORMAT(' SENSOR (TIME) NUMBER ',I10,' DESACTIVATED AT TIME ',1PE12.5)
c-----------   
      RETURN
      END SUBROUTINE SENSOR_TIME
