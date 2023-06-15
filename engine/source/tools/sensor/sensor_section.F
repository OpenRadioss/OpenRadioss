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
Chd|  SENSOR_SECTION                source/tools/sensor/sensor_section.F
Chd|-- called by -----------
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_SECTION(SENSOR  ,DIMFB   ,STABS   ,TABS    ,
     .                          FSAV    ,FBSAV6  )
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
      DOUBLE PRECISION ,DIMENSION(12,6,DIMFB) :: FBSAV6
      my_real ,DIMENSION(NTHVKI,*) :: FSAV
      TYPE (SENSOR_STR_) ,TARGET   :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,SECT_ID,IDIR,ICRIT,ISECT,IAD,IFLAG
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
      SECT_ID = SENSOR%IPARAM(1)
      IDIR    = SENSOR%IPARAM(2)
      FMIN    = SENSOR%RPARAM(1)
      FMAX    = SENSOR%RPARAM(2)
      ICRIT   = 0
      IFLAG   = 0
c
      IAD   = NINTER + NRWALL + NRBODY + SECT_ID        
      ISECT = TABS(SECT_ID+1) - TABS(SECT_ID)
c
      IF (IDIR == 1) THEN      ! normal force                    
        IF (STABS == 0) THEN   ! Parith/Off
          FX = FSAV(1,IAD)              
          FY = FSAV(2,IAD)                          
          FZ = FSAV(3,IAD)                          
        ELSE               !         Parith/on
          FX = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)
     .       + FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)
     .       + FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
          FY = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)
     .       + FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)
     .       + FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
          FZ = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)
     .       + FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)
     .       + FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
        ENDIF
      ELSEIF (IDIR == 2) THEN  ! tangent force
        IF (STABS == 0)  THEN  ! Parith/Of
         FX = FSAV(4,IAD)                          
         FY = FSAV(5,IAD)                          
         FZ = FSAV(6,IAD)                          
        ELSE               !         Parith/on
          FX = FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)
     .       + FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)
     .       + FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)
          FY = FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)
     .       + FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)
     .       + FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)
          FZ = FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)
     .       + FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)
     .       + FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)
        ENDIF
      ELSEIF (IDIR == 3) THEN  ! Total force
        IF (STABS == 0) THEN  ! Parith/Off  
         FX = FSAV(34,IAD)                         
         FY = FSAV(35,IAD)                         
         FZ = FSAV(36,IAD)
        ELSE               !         Parith/on
          FX = FBSAV6(10,1,ISECT)+FBSAV6(10,2,ISECT)
     .       + FBSAV6(10,3,ISECT)+FBSAV6(10,4,ISECT)
     .       + FBSAV6(10,5,ISECT)+FBSAV6(10,6,ISECT)
          FY = FBSAV6(11,1,ISECT)+FBSAV6(11,2,ISECT)
     .       + FBSAV6(11,3,ISECT)+FBSAV6(11,4,ISECT)
     .       + FBSAV6(11,5,ISECT)+FBSAV6(11,6,ISECT)
          FZ = FBSAV6(12,1,ISECT)+FBSAV6(12,2,ISECT)
     .       + FBSAV6(12,3,ISECT)+FBSAV6(12,4,ISECT)
     .       + FBSAV6(12,5,ISECT)+FBSAV6(12,6,ISECT)
        ENDIF  
      ELSEIF (IDIR == 4) THEN  !  Total moment
        IF (STABS == 0) THEN  !  Parith/Off  
         FX = FSAV(31,IAD)                         
         FY = FSAV(32,IAD)                         
         FZ = FSAV(33,IAD)
        ELSE               !         Parith/on
         FX = FBSAV6(7,1,ISECT)+FBSAV6(7,2,ISECT)
     .      + FBSAV6(7,3,ISECT)+FBSAV6(7,4,ISECT)
     .      + FBSAV6(7,5,ISECT)+FBSAV6(7,6,ISECT)
         FY = FBSAV6(8,1,ISECT)+FBSAV6(8,2,ISECT)
     .      + FBSAV6(8,3,ISECT)+FBSAV6(8,4,ISECT)
     .      + FBSAV6(8,5,ISECT)+FBSAV6(8,6,ISECT)
         FZ = FBSAV6(9,1,ISECT)+FBSAV6(9,2,ISECT)
     .      + FBSAV6(9,3,ISECT)+FBSAV6(9,4,ISECT)
     .      + FBSAV6(9,5,ISECT)+FBSAV6(9,6,ISECT)
        ENDIF  
      ELSEIF (IDIR == 5) THEN  !  FX
        IF (STABS == 0) THEN !         Parith/Off
          FF = FSAV(1,IAD) + FSAV(4,IAD)
        ELSE                  !         Parith/on
          FF = FBSAV6(1,1,ISECT)+FBSAV6(1,2,ISECT)
     .       + FBSAV6(1,3,ISECT)+FBSAV6(1,4,ISECT)
     .       + FBSAV6(1,5,ISECT)+FBSAV6(1,6,ISECT)
     .       + FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)
     .       + FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)
     .       + FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT)
        ENDIF
      ELSEIF (IDIR == 6) THEN  !  FY
        IF (STABS == 0) THEN!         Parith/Off
          FF = FSAV(2,IAD) + FSAV(5,IAD)
        ELSE                  !         Parith/on
          FF = FBSAV6(2,1,ISECT)+FBSAV6(2,2,ISECT)
     .       + FBSAV6(2,3,ISECT)+FBSAV6(2,4,ISECT)
     .       + FBSAV6(2,5,ISECT)+FBSAV6(2,6,ISECT)
     .       + FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)
     .       + FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)
     .       + FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT)
        ENDIF                         
      ELSEIF (IDIR == 7) THEN  !  FZ
        IF (STABS == 0) THEN  !         Parith/Off
          FF = FSAV(3,IAD) + FSAV(6,IAD)
        ELSE                  !         Parith/on
          FF = FBSAV6(3,1,ISECT)+FBSAV6(3,2,ISECT)
     .       + FBSAV6(3,3,ISECT)+FBSAV6(3,4,ISECT)
     .       + FBSAV6(3,5,ISECT)+FBSAV6(3,6,ISECT)
     .       + FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)
     .       + FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)
     .       + FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT)
        ENDIF                          
      ELSEIF (IDIR == 8) THEN  !  MX
        IF (STABS == 0) THEN!         Parith/Off
          FF = FSAV(31,IAD)
        ELSE                  !         Parith/on
          FF = FBSAV6(7,1,ISECT)+FBSAV6(7,2,ISECT)
     .       + FBSAV6(7,3,ISECT)+FBSAV6(7,4,ISECT)
     .       + FBSAV6(7,5,ISECT)+FBSAV6(7,6,ISECT)
        ENDIF                          
      ELSEIF (IDIR == 9) THEN  !  MY
        IF (STABS == 0) THEN!         Parith/Off
          FF = FSAV(32,IAD)
        ELSE                  !         Parith/on
          FF = FBSAV6(8,1,ISECT)+FBSAV6(8,2,ISECT)
     .       + FBSAV6(8,3,ISECT)+FBSAV6(8,4,ISECT)
     .       + FBSAV6(8,5,ISECT)+FBSAV6(8,6,ISECT)
        ENDIF                           
      ELSEIF (IDIR == 10) THEN  !  MZ
        IF (STABS == 0) THEN        ! Parith/Off
          FF = FSAV(33,IAD)
        ELSE                        ! Parith/on
          FF = FBSAV6(9,1,ISECT)+FBSAV6(9,2,ISECT)
     .       + FBSAV6(9,3,ISECT)+FBSAV6(9,4,ISECT)
     .       + FBSAV6(9,5,ISECT)+FBSAV6(9,6,ISECT)
         ENDIF                          
      ENDIF 
c
      IF (IDIR < 5) FF = SQRT(FX*FX + FY*FY + FZ*FZ)  
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
          WRITE (IOUT ,1300) FMIN,FF
        ELSE IF (IFLAG == 2) THEN
          WRITE (IOUT ,1400) FMAX,FF
        END IF
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR (SECTION) NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
1300  FORMAT('      TARGET MIN FORCE = ',1PE12.5,/
     .       '      CURRENT FORCE AFTER TMIN and TDELAY = ',1PE12.5)
1400  FORMAT('      TARGET MAX FORCE = ',1PE12.5,/
     .       '      CURRENT FORCE AFTER TMIN and TDELAY = ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
