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
Chd|  SENSOR_RWALL                    source/tools/sensor/sensor_rwall.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_RWALL(SENSOR ,
     .           NPRW    ,DIMFB   ,STABS   ,TABS   ,FBSAV6)
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
      INTEGER :: DIMFB,STABS
      INTEGER :: TABS(STABS),NPRW(*)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE (SENSOR_STR_) ,TARGET   :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: RW_ID,IDIR,ICRIT,ISECT,N5
      my_real :: FMIN,FMAX,FF,FX,FY,FZ,TMIN,TDELAY,INFINITY
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      TMIN   = SENSOR%TMIN
      TDELAY = SENSOR%TDELAY
c      
      RW_ID = SENSOR%IPARAM(1) 
      IDIR  = SENSOR%IPARAM(2) 
      FMIN  = SENSOR%RPARAM(1) 
      FMAX  = SENSOR%RPARAM(2) 
c
      ISECT = TABS(NSECT+RW_ID+1)-TABS(NSECT+RW_ID)
      ICRIT = 0
      N5 = RW_ID + 4*NRWALL
c
      IF (NPRW(N5) == 1) THEN
        ISECT = TABS(RW_ID+NSECT+NINTER+NINTSUB+1)-TABS(RW_ID+NSECT+NINTER+NINTSUB)
        IF (IDIR == 1) THEN
          FX = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     .         FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     .         FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
          FF = FX
        ELSEIF (IDIR == 2) THEN
          FY = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     .         FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     .         FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
          FF = FY
        ELSEIF (IDIR == 4) THEN
          FZ = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     .         FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     .         FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
          FF = FZ
        ELSEIF (IDIR == 5) THEN
          FX = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)+
     .         FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)+
     .         FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
          FY = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)+
     .         FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)+
     .         FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
          FZ = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)+
     .         FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)+
     .         FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
          FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
        ELSEIF (IDIR == 6) THEN
          FX = FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)+
     .         FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)+
     .         FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)
          FY = FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)+
     .         FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)+
     .         FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)
          FZ = FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)+
     .         FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)+
     .         FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)
          FF = SQRT(FX*FX + FY*FY + FZ*FZ) 
        ENDIF
        IF (FF < FMIN .OR. FF > FMAX) ICRIT = 1
c
        IF (SENSOR%TCRIT + TMIN > TT) THEN
          IF (ICRIT == 0) THEN
            SENSOR%TCRIT = INFINITY
          ELSE IF (SENSOR%TCRIT == INFINITY) THEN
            SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
          END IF
        ELSE IF (SENSOR%TSTART == INFINITY) THEN
          SENSOR%TSTART = SENSOR%TCRIT + TMIN + TDELAY
        END IF
c
        IF (SENSOR%TSTART <= TT) THEN   ! sensor activation
          SENSOR%STATUS = 1
        END IF
c
        IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
            WRITE (ISTDO,1100) SENSOR%SENS_ID,SENSOR%TSTART
            IF (FF < FMIN) THEN
              WRITE (IOUT ,1200) SENSOR%SENS_ID,SENSOR%TSTART,FMIN
            ELSE IF (FF > FMAX) THEN
              WRITE (IOUT ,1200) SENSOR%SENS_ID,SENSOR%TSTART,FMAX
            END IF
#include "lockoff.inc"
        ENDIF
c
      ELSE IF (TT > SENSOR%TSTART .AND. SENSOR%STATUS == 1) THEN  ! deactivation
        SENSOR%STATUS = 0
        SENSOR%TSTART = INFINITY
        SENSOR%TCRIT  = INFINITY
#include "lockon.inc"
        IF (ISPMD == 0) THEN
          WRITE (ISTDO,1300) SENSOR%SENS_ID,SENSOR%TSTART
          WRITE (IOUT ,1300) SENSOR%SENS_ID,SENSOR%TSTART
#include "lockoff.inc"
        END IF
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR (RWALL) NUMBER ',I10,' ,ACTIVATED AT TIME '  ,1PE12.5)
1200  FORMAT(' SENSOR NUMBER ',I10,' ,DEACTIVATED AT TIME ',1PE12.5,      
     .       ' TARGET VALUE =',1PE12.5,' CURRENT VALUE AT TMIN + TDELAY =',1PE12.5)
1300  FORMAT(' SENSOR NUMBER ',I10,' ,DEACTIVATED AT TIME ',1PE12.5)     
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
