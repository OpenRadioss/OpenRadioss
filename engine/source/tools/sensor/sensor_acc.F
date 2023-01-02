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
Chd|  SENSOR_ACC                    source/tools/sensor/sensor_acc.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_ACC(SENSOR ,ACC)
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
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(LLACCELM,*) :: ACC
      TYPE (SENSOR_STR_) ,TARGET :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,J,K,NACC,IACC,IDIR,ICRIT
c
      my_real :: ACC_X,ACC_Y,ACC_Z,ACCMIN,TSTART,TMIN,TDELAY,INFINITY
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated
c
      TMIN   = SENSOR%TMIN
      TDELAY = SENSOR%TDELAY
      NACC   = SENSOR%IPARAM(1)
c      
      DO I = 1,NACC
        J = (I-1)*2 + 1
        K = (I-1)*3
        IACC   = SENSOR%IPARAM(J+1)
        IDIR   = SENSOR%IPARAM(J+2)
        ACCMIN = SENSOR%RPARAM(K+1)
        TMIN   = SENSOR%RPARAM(K+2)
        TSTART = SENSOR%RPARAM(K+3)
        ICRIT  = 0
        IF (IDIR == 1) THEN
C         X
          IF (ABS(ACC(20,IACC)) > ACCMIN) ICRIT = 1
        ELSEIF (IDIR == 2) THEN
C         Y
          IF (ABS(ACC(21,IACC)) > ACCMIN) ICRIT = 1
        ELSEIF (IDIR == 4) THEN
C         Z
          IF (ABS(ACC(22,IACC)) > ACCMIN) ICRIT = 1
        ELSEIF (IDIR == 3) THEN
C         XY
          IF (SQRT(ACC(20,IACC)*ACC(20,IACC)
     .           + ACC(21,IACC)*ACC(21,IACC)) > ACCMIN) ICRIT = 1
        ELSEIF (IDIR == 5) THEN
C         XZ
          IF (SQRT(ACC(20,IACC)*ACC(20,IACC)
     .           + ACC(22,IACC)*ACC(22,IACC)) > ACCMIN) ICRIT = 1
        ELSEIF (IDIR == 6) THEN
C         YZ
          IF (SQRT(ACC(21,IACC)*ACC(21,IACC)
     .           + ACC(22,IACC)*ACC(22,IACC)) > ACCMIN) ICRIT = 1
        ELSEIF (IDIR == 7) THEN
C         XYZ
          IF (SQRT(ACC(20,IACC)*ACC(20,IACC)
     .           + ACC(21,IACC)*ACC(21,IACC)
     .           + ACC(22,IACC)*ACC(22,IACC)) > ACCMIN) ICRIT = 1
        ENDIF
c----------------------------------------------------------------
c       check activation status
c----------------------------------------------------------------
        IF (SENSOR%TCRIT + TMIN > TT) THEN
          IF (ICRIT == 0) THEN
            SENSOR%TCRIT = INFINITY
          ELSE IF (SENSOR%TCRIT == INFINITY) THEN
            SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
          END IF
        END IF
c
        IF (SENSOR%TSTART == INFINITY .and. SENSOR%TCRIT < INFINITY) THEN
          SENSOR%RPARAM(K+3) = SENSOR%TCRIT + TMIN + TDELAY      
        END IF
        IF (SENSOR%RPARAM(K+3) <= TT) THEN   ! sensor activation
          SENSOR%STATUS = 1
          SENSOR%TSTART = TT
        END IF
        
      END DO
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100) SENSOR%SENS_ID,SENSOR%TSTART
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
