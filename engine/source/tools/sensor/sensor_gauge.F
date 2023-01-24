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
C=======================================================================
Chd|====================================================================
Chd|  SENSOR_GAUGE                  source/tools/sensor/sensor_gauge.F
Chd|-- called by -----------
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_GAUGE(SENSOR ,GAUGE)
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(LLGAUGE,*) :: GAUGE
      TYPE (SENSOR_STR_) :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,J,NGAUGE,IGAU,ICRIT
c
      my_real :: PMIN,TMIN,TSTART,TDELAY,INFINITY
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated
c
      TDELAY = SENSOR%TDELAY
      NGAUGE = SENSOR%IPARAM(1)

      J = 0
      DO I = 1,NGAUGE
        ICRIT  = 0
        IGAU   = SENSOR%IPARAM(I+1)
        PMIN   = SENSOR%RPARAM(J+1)
        TMIN   = SENSOR%RPARAM(J+2)
        TSTART = SENSOR%RPARAM(J+3)
        IF (ABS(GAUGE(30,IGAU)) > PMIN) ICRIT  = 1
c----------------------------------------------------------------
c       check activation status
c----------------------------------------------------------------
        IF (SENSOR%TCRIT + TMIN > TT) THEN
          IF (ICRIT == 0) THEN
            SENSOR%TCRIT = INFINITY
          ELSE IF (SENSOR%TCRIT == INFINITY) THEN
            SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
          END IF
        ELSE IF (SENSOR%RPARAM(J+3) == INFINITY) THEN
          SENSOR%RPARAM(J+3) = SENSOR%TCRIT + TMIN + TDELAY
        END IF
        IF (SENSOR%RPARAM(J+3) <= TT) THEN   ! sensor activation
          SENSOR%STATUS = 1
          SENSOR%TSTART = SENSOR%RPARAM(J+3)
        END IF
c  
        IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (ISTDO,1100) SENSOR%SENS_ID,SENSOR%TSTART
          WRITE (IOUT ,1100) SENSOR%SENS_ID,SENSOR%TSTART
          WRITE (IOUT ,1200) PMIN,ABS(GAUGE(30,IGAU))
#include "lockoff.inc"
        ENDIF
        J = J + 3
      END DO
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      TARGET PRESSURE = ',1PE12.5,/
     .       '      CURRENT PRESSURE AFTER TMIN AND TDELAY = ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
