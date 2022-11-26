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
Copyright>        software under a commercial license.  ConTSTART Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
C=======================================================================
Chd|====================================================================
Chd|  SENSOR_DIST                   source/tools/sensor/sensor_dist.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_DIST(SENSOR ,X      ,XSENS   )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(3,NUMNOD) :: X
      my_real, DIMENSION(12)       :: XSENS
      TYPE (SENSOR_STR_) ,TARGET   :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: N1,N2,ICRIT,DFLAG,IFLAG,ACTI
      my_real :: DX,DY,DZ,DD,DMIN,DMAX,TMIN,TDELAY,INFINITY
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      TDELAY = SENSOR%TDELAY
      TMIN   = SENSOR%TMIN
c      
      N1     = SENSOR%IPARAM(1)
      N2     = SENSOR%IPARAM(2)
      DFLAG  = SENSOR%IPARAM(3)
      DMIN   = SENSOR%RPARAM(1)
      DMAX   = SENSOR%RPARAM(2)
      ICRIT  = 0    ! activation
      IFLAG  = 0
      ACTI   = 0
c
      IF (NSPMD == 1)THEN
        DX = X(1,N1)-X(1,N2)
        DY = X(2,N1)-X(2,N2)
        DZ = X(3,N1)-X(3,N2)
      ELSE
        DX = XSENS(1)-XSENS(4)
        DY = XSENS(2)-XSENS(5)
        DZ = XSENS(3)-XSENS(6)
      END IF
      DD = SQRT(DX*DX+DY*DY+DZ*DZ)
      IF (DD < DMIN) THEN
        ICRIT = 1
        IFLAG = 1      
      ELSE IF (DD > DMAX) THEN
        ICRIT = 1
        IFLAG = 2
      END IF
c----------------------------------------------------------------
c     check activation status
c----------------------------------------------------------------
      IF (SENSOR%STATUS == 0) THEN
        IF (SENSOR%TCRIT + TMIN > TT) THEN
          IF (ICRIT == 0) THEN
            SENSOR%TCRIT = INFINITY
          ELSE IF (ICRIT == 1 .and. SENSOR%TCRIT == INFINITY) THEN
            SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
          END IF
        END IF
        IF (SENSOR%TSTART == INFINITY .and. SENSOR%TCRIT < INFINITY) THEN
          SENSOR%TSTART = SENSOR%TCRIT + TMIN + TDELAY      
        END IF
        IF (SENSOR%TSTART <= TT) THEN   
          ! sensor activation
          ACTI = 1
          SENSOR%STATUS = 1
          SENSOR%VALUE = INFINITY
        END IF 
      END IF 
c
      IF (DFLAG == 1 .and. SENSOR%STATUS == 1) THEN
          IF (SENSOR%VALUE + TMIN > TT) THEN
            IF (ICRIT == 0 .and. SENSOR%VALUE == INFINITY) THEN
              SENSOR%VALUE = MIN(SENSOR%VALUE, TT)
            ELSE IF (ICRIT > 0) THEN
              SENSOR%VALUE = INFINITY
            END IF
          END IF
          IF (SENSOR%VALUE + TMIN + TDELAY <= TT) THEN 
            ! sensor deactivation
            ACTI = 2
            SENSOR%STATUS = 0
            SENSOR%TCRIT  = INFINITY
            SENSOR%TSTART = INFINITY
          END IF 
      END IF
c-----------------------------------------------------------------------      
      IF (ACTI == 1) THEN
        IF (ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (ISTDO,1000) SENSOR%SENS_ID,SENSOR%TSTART
          WRITE (IOUT ,1000) SENSOR%SENS_ID,SENSOR%TSTART
          IF (IFLAG == 1) THEN
            WRITE (IOUT ,1200) DMIN
          ELSE IF (IFLAG == 2) THEN
            WRITE (IOUT ,1300) DMAX
          END IF
          WRITE (IOUT ,1400) DD
#include "lockoff.inc"
        ENDIF
      ELSE IF (ACTI == 2) THEN
        IF (ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (ISTDO,1100) SENSOR%SENS_ID,TT
          WRITE (IOUT ,1100) SENSOR%SENS_ID,TT
          WRITE (IOUT ,1400) DD
#include "lockoff.inc"
        ENDIF
      ENDIF
c-----------------------------------------------------------------------      
1000  FORMAT(' SENSOR NUMBER ',I10,' ACTIVATED AT TIME ',1PE12.5)
1100  FORMAT(' SENSOR NUMBER ',I10,' DEACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      TARGET MIN DISTANCE= ',1PE12.5)
1300  FORMAT('      TARGET MAX DISTANCE= ',1PE12.5)
1400  FORMAT('      CURRENT DISTANCE AFTER TMIN AND TDELAY = ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
