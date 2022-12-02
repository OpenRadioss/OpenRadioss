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
Chd|  sensor_not                       source/tools/sensor/sensor1.F 
Chd|-- called by -----------
Chd|        sensor_logical.F            source/tools/sensor/sensor_logical.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SENSOR_AND(SENSOR_TAB,SENS)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   AND sensor : IS1 && IS2
C                IS1 and IS2 are the sensors given as definition
C----------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C    SENSOR_TAB     Sensor Structure
C    SENS           SENSOR ID to treat
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  SENS
      TYPE (SENSOR_STR_), INTENT(INOUT) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IS1,STATUS_IS1
      INTEGER IS2,STATUS_IS2

      INTEGER ACTI
      my_real TSTART, INFINITY
C-----------------------------------------------
      INFINITY = EP20
      ACTI = 0
c
      IS1        = SENSOR_TAB(SENS)%IPARAM(1)
      STATUS_IS1 = SENSOR_TAB(IS1)%STATUS

      IS2        = SENSOR_TAB(SENS)%IPARAM(2)
      STATUS_IS2 = SENSOR_TAB(IS2)%STATUS
c
      IF ( STATUS_IS1 == 1 .AND. STATUS_IS2 == 1 .AND. SENSOR_TAB(SENS)%STATUS == 0 ) THEN     ! Context change - AND goes ON
          SENSOR_TAB(SENS)%STATUS = 1
          SENSOR_TAB(SENS)%TSTART = TT
          ACTI = 1
      ENDIF

      IF ( (STATUS_IS1 == 0 .OR. STATUS_IS2 == 0) .AND. SENSOR_TAB(SENS)%STATUS == 1 ) THEN     ! Context change - AND goes OFF
          SENSOR_TAB(SENS)%STATUS = 0
          SENSOR_TAB(SENS)%TSTART = INFINITY
          ACTI = 2
      ENDIF
     
      IF (ACTI == 1) THEN

        IF (ISPMD == 0) THEN
          WRITE (ISTDO,1000) SENSOR_TAB(SENS)%SENS_ID,SENSOR_TAB(SENS)%TSTART
          WRITE (IOUT ,1000) SENSOR_TAB(SENS)%SENS_ID,SENSOR_TAB(SENS)%TSTART
        ENDIF

      ELSE IF (ACTI == 2) THEN

        IF (ISPMD == 0) THEN
          WRITE (ISTDO,1100) SENSOR_TAB(SENS)%SENS_ID,TT
          WRITE (IOUT ,1100) SENSOR_TAB(SENS)%SENS_ID,TT
        ENDIF

      ENDIF
c-----------------------------------------------------------------------      
1000  FORMAT(' SENSOR NUMBER ',I10,' ACTIVATED AT TIME ',1PE12.5)
1100  FORMAT(' SENSOR NUMBER ',I10,' DEACTIVATED AT TIME ',1PE12.5)
C-----------------------------------------------
      END


