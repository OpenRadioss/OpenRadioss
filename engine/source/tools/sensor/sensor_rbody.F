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
Chd|  SENSOR_RBODY                  source/tools/sensor/sensor_rbody.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_RBODY(SENSOR ,DIMFB   ,STABS   ,TABS   ,FBSAV6)
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
      INTEGER :: DIMFB,STABS
      INTEGER :: TABS(STABS)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE (SENSOR_STR_) ,TARGET   :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,RBOD_ID,IDIR,ICRIT,ISECT,IFLAG,IAD
c
      my_real :: FMIN,FMAX,FX,FY,FZ,FF,TSTART,TMIN,TDELAY,INFINITY
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated
c
      TSTART  = SENSOR%TSTART
      TMIN    = SENSOR%TMIN
      TDELAY  = SENSOR%TDELAY
c
      RBOD_ID = SENSOR%IPARAM(1)
      IDIR    = SENSOR%IPARAM(2)
      FMIN    = SENSOR%RPARAM(1)
      FMAX    = SENSOR%RPARAM(2)
      ICRIT   = 0
      IFLAG   = 0
c
      IAD   = NINTER + NRWALL + RBOD_ID
      ISECT = TABS(RBOD_ID + NSECT + NINTER + NINTSUB+NRWALL+1)
     .      - TABS(RBOD_ID + NSECT + NINTER + NINTSUB+NRWALL)
      IF (IDIR == 1) THEN
        FX = FBSAV6(1,1,ISECT) + FBSAV6(1,2,ISECT)
     .     + FBSAV6(1,3,ISECT) + FBSAV6(1,4,ISECT)
     .     + FBSAV6(1,5,ISECT) + FBSAV6(1,6,ISECT)
        FY = FBSAV6(2,1,ISECT) + FBSAV6(2,2,ISECT)
     .     + FBSAV6(2,3,ISECT) + FBSAV6(2,4,ISECT)
     .     + FBSAV6(2,5,ISECT) + FBSAV6(2,6,ISECT)
        FZ = FBSAV6(3,1,ISECT) + FBSAV6(3,2,ISECT)
     .     + FBSAV6(3,3,ISECT) + FBSAV6(3,4,ISECT)
     .     + FBSAV6(3,5,ISECT) + FBSAV6(3,6,ISECT)
        FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
      ELSEIF (IDIR == 2) THEN
        FX = FBSAV6(4,1,ISECT) + FBSAV6(4,2,ISECT)
     .     + FBSAV6(4,3,ISECT) + FBSAV6(4,4,ISECT)
     .     + FBSAV6(4,5,ISECT) + FBSAV6(4,6,ISECT)
        FY = FBSAV6(5,1,ISECT) + FBSAV6(5,2,ISECT)
     .     + FBSAV6(5,3,ISECT) + FBSAV6(5,4,ISECT)
     .     + FBSAV6(5,5,ISECT) + FBSAV6(5,6,ISECT)
        FZ = FBSAV6(6,1,ISECT) + FBSAV6(6,2,ISECT)
     .     + FBSAV6(6,3,ISECT) + FBSAV6(6,4,ISECT)
     .     + FBSAV6(6,5,ISECT) + FBSAV6(6,6,ISECT)
        FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
      ENDIF           
c
      IF (FF < FMIN) THEN
        ICRIT = 1
        IFLAG = 1
      ELSE IF (FF > FMAX) THEN
        ICRIT = 1
        IFLAG = 2
      END IF
c----------------------------------------------------------------
c     check activation status
c----------------------------------------------------------------
      IF (SENSOR%TCRIT + TMIN > TT) THEN
        IF (ICRIT == 0) THEN
          SENSOR%TCRIT = INFINITY
        ELSE IF (SENSOR%TCRIT == INFINITY) THEN
          SENSOR%TCRIT = TT
        END IF
      END IF
      IF (SENSOR%TCRIT < INFINITY) TSTART = SENSOR%TCRIT + TMIN + TDELAY
      IF (TSTART <= TT) THEN   ! sensor activation
        SENSOR%STATUS = 1
        SENSOR%TSTART = TSTART
      END IF
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100) SENSOR%SENS_ID,SENSOR%TSTART
        IF (IFLAG == 1) THEN
          WRITE (IOUT ,1200) FMIN,FF
        ELSE IF (IFLAG == 2) THEN
          WRITE (IOUT ,1300) FMAX,FF
        END IF
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR (RBODY) NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      TARGET MIN FORCE = ',1PE12.5,/
     .       '      CURRENT FORCE AFTER TMIN and TDELAY = ',1PE12.5)
1300  FORMAT('      TARGET MAX FORCE = ',1PE12.5,/
     .       '      CURRENT FORCE AFTER TMIN and TDELAY = ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
