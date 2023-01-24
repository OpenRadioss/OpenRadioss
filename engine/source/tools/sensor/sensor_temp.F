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
Chd|  SENSOR_TEMP                   source/tools/sensor/sensor_temp.F
Chd|-- called by -----------
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_TEMP(SENSOR   ,ISENS     ,IGRNOD   ) 
c-----------------------------------------------------------------------
!$COMMENT
!       SENSOR_TEMP description
!       SENSOR_TEMP organization :
!       - sensor state modification
!$ENDCOMMENT
c-----------------------------------------------
C   M o d u l e s
c-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: ISENS
      TYPE(GROUP_)  ,DIMENSION(NGRNOD) ,INTENT(IN) :: IGRNOD
      TYPE (SENSOR_STR_) :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: I,IGN,ICRIT,TFLAG
      my_real :: TSTART,TDELAY,TMIN,TEMP_MAX,TEMP_MIN,TEMP_AVG,INFINITY
      PARAMETER (INFINITY = 1.0E20)
c----------------------------------------------------------
c     SENSOR%VAR(1)   = TEMP_MAX
c     SENSOR%VAR(2)   = TEMP_MIN 
c     SENSOR%VAR(3)   = TEMP_AVG 
c
c     sensor status : ICRIT
c            = 0   : inactive, criterion not met
c            = 1   : criterium reached, testing duration time Tmin 
c            = 2   : ready to activate after Tdelay
c            = 3   : activated at Time = TSTART
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated
c
      IF (TT == ZERO) SENSOR%VAR(2) = EP20   ! initialize Temp min
      TSTART   = SENSOR%TSTART
      TDELAY   = SENSOR%TDELAY
      TMIN     = SENSOR%TMIN
      IGN      = SENSOR%IPARAM(1)
      TEMP_MAX = SENSOR%VAR(1)
      TEMP_MIN = SENSOR%VAR(2)    
      TEMP_AVG = SENSOR%VAR(3)    
      ICRIT = 0
      TFLAG = 0

      IF (TEMP_AVG > SENSOR%RPARAM(3)) THEN
        ICRIT = 1
        TFLAG = 3      
      ELSE IF (TEMP_MAX > SENSOR%RPARAM(1)) THEN
        ICRIT = 1
        TFLAG = 1      
      ELSE IF (TEMP_MIN < SENSOR%RPARAM(2)) THEN
        ICRIT = 1
        TFLAG = 2      
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
      TSTART = SENSOR%TCRIT + TMIN + TDELAY      
      IF (TSTART <= TT) THEN   ! sensor activation
        SENSOR%STATUS = 1
        SENSOR%TSTART = TSTART
      END IF
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
        IF (TFLAG == 1) THEN
          WRITE (IOUT ,1200)  SENSOR%RPARAM(1),TEMP_MAX
        ELSE IF (TFLAG == 2) THEN
          WRITE (IOUT ,1300)  SENSOR%RPARAM(2),TEMP_MIN
        ELSE IF (TFLAG == 3) THEN
          WRITE (IOUT ,1400)  SENSOR%RPARAM(3),TEMP_AVG
        END IF
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR TEMPERATURE NUMBER ',I10,' ACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      TARGET MAX TEMPERATURE = ',1PE12.5,/
     .       '      CURRENT TEMPERATURE AFTER TMIN and TDELAY = ',1PE12.5)
1300  FORMAT('      TARGET MIN TEMPERATURE = ',1PE12.5,/
     .       '      CURRENT TEMPERATURE AFTER TMIN and TDELAY = ',1PE12.5)
1400  FORMAT('      TARGET AVG TEMPERATURE = ',1PE12.5,/
     .       '      CURRENT TEMPERATURE AFTER TMIN and TDELAY = ',1PE12.5)
c----------------------------------------------------------
      RETURN
      END SUBROUTINE SENSOR_TEMP
