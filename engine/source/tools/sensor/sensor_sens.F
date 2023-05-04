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
Chd|  SENSOR_SENS                   source/tools/sensor/sensor_sens.F
Chd|-- called by -----------
Chd|        SENSOR_LOGICAL                source/tools/sensor/sensor_logical.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_SENS(NSENSOR,SENSOR_TAB,SENS)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   SENS sensor : Starts when IS1 Starts,
C                 Remains active for at least TDELAY
C                 IF IS2 is defined : Stops when IS2 Starts
C                 ELSE Stops after TDELAY.
C                 IS1 and IS2 are the sensors given as definition
C----------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C    SENSOR_TAB     Sensor Structure
C    SENS           SENSOR ID to treat
C----------------------------------------------------------------
C    Sensor activation flag:
C 
C    Sensor inactive & cannot be reactivated afterwards
C    SENSOR_TAB(SENS)%TSART = INFINITY (1.E+20)
C    SENSOR_TAB(SENS)%STATUS = -1
C
C    Sensor inactive :
C    SENSOR_TAB(SENS)%TSTART = INFINITY (1.E+20)
C    SENSOR_TAB(SENS)%STATUS = 0
C
C    Sensor active :
C    SENSOR_TAB(SENS)%TSTART = TT (or activation time : TT+DT2)
C    SENSOR_TAB(SENS)%STATUS = 1
C
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
      INTEGER, INTENT(IN) :: NSENSOR,SENS
      TYPE (SENSOR_STR_), INTENT(INOUT) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IS1,STATUS_IS1
      INTEGER IS2,STATUS_IS2

      INTEGER ACTI,SENS_ID
      my_real TSTART, TDELAY,TSTART1, TSTOPS, INFINITY
C-----------------------------------------------
      INFINITY = EP20
      ACTI = 0
c
      SENS_ID = SENSOR_TAB(SENS)%SENS_ID
      TSTART  = SENSOR_TAB(SENS)%TSTART
      TDELAY  = SENSOR_TAB(SENS)%TDELAY
      TSTOPS  = INFINITY
c
      IS1        = SENSOR_TAB(SENS)%IPARAM(1)
      STATUS_IS1 = SENSOR_TAB(IS1)%STATUS
      TSTART1    = SENSOR_TAB(IS1)%TSTART
c
      IS2        = SENSOR_TAB(SENS)%IPARAM(2)
      IF (IS2 > 0) THEN
          STATUS_IS2 = SENSOR_TAB(IS2)%STATUS
      ENDIF

      IF (SENSOR_TAB(SENS)%STATUS == -1)THEN         ! Status= -1 : Sensor will never wake up again
         RETURN
      ENDIF


      IF (STATUS_IS1 <= 0) THEN
          TSTART1 = INFINITY
      END IF

      IF (STATUS_IS1 == 1) THEN                      ! check status of sensor1
         IF (SENSOR_TAB(SENS)%STATUS == 0) THEN
              ACTI = 1
              SENSOR_TAB(SENS)%STATUS = 1
              SENSOR_TAB(SENS)%TSTART = TSTART1     
         ENDIF
      ENDIF

      IF (TT > (TSTART + TDELAY) ) THEN              ! We reached TDELAY

         IF (IS2 == 0) THEN                          ! If sensor2 not defined : STOP
            TSTOPS = TT
         ELSE                                        ! sensor2 defined : stop when sens2 starts
            TSTOPS = INFINITY
            IF (SENSOR_TAB(IS2)%STATUS == 1) THEN
              TSTOPS = TT
            ENDIF
         ENDIF

         IF (TSTOPS <= TT) THEN
              SENSOR_TAB(SENS)%TSTART = INFINITY
              SENSOR_TAB(SENS)%STATUS = -1           ! Status= -1 : Sensor will never wake up again
              ACTI = ACTI + 2
         ENDIF
      ENDIF

     
      IF (ACTI == 1) THEN

        IF (ISPMD == 0) THEN
          WRITE (ISTDO,1000) SENSOR_TAB(SENS)%SENS_ID,TSTART1
          WRITE (IOUT ,1000) SENSOR_TAB(SENS)%SENS_ID,TSTART1
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


