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
Chd|  SENSOR_WORK                   source/tools/sensor/sensor_work.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_WORK(SENSOR  ,X       ,XSENS   ,DIMFB   ,STABS   ,
     .                       TABS    ,FBSAV6  )
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
      INTEGER ,DIMENSION(STABS)    :: TABS
      my_real, DIMENSION(3,NUMNOD) :: X
      my_real, DIMENSION(12)       :: XSENS
      TYPE (SENSOR_STR_) ,TARGET   :: SENSOR
      DOUBLE PRECISION ,DIMENSION(12,6,DIMFB) :: FBSAV6
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: N1,N2,ICRIT,SECT_ID,INT_ID,SUB_ID,RWAL_ID,RBOD_ID,
     .           IN,ISECT,INTER,IRWAL,IRBOD
      my_real :: DX,DY,DZ,DD,FX,FY,FZ,WORK,WMAX,TMIN,TDELAY,
     .           INFINITY
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      IF (SENSOR%STATUS == 1) RETURN   ! already activated
c
      TMIN    = SENSOR%TMIN
      TDELAY  = SENSOR%TDELAY
c      
      N1      = SENSOR%IPARAM(1)
      N2      = SENSOR%IPARAM(2)
      SECT_ID = SENSOR%IPARAM(3) 
      INT_ID  = SENSOR%IPARAM(4) 
      SUB_ID  = SENSOR%IPARAM(5) 
      RWAL_ID = SENSOR%IPARAM(6) 
      RBOD_ID = SENSOR%IPARAM(7) 

      WMAX    = SENSOR%RPARAM(1)
      FX      = ZERO
      FY      = ZERO
      FZ      = ZERO
      ICRIT   = 0
c
      IF (NSPMD == 1) THEN
        XSENS(1) = X(1,N1)
        XSENS(2) = X(2,N1)
        XSENS(3) = X(3,N1)
        IF (N2 > 0) THEN
          XSENS(4) = X(1,N2)
          XSENS(5) = X(2,N2)
          XSENS(6) = X(3,N2)
        ELSE
          XSENS(4) = ZERO
          XSENS(5) = ZERO
          XSENS(6) = ZERO
        ENDIF
        IF (TT == ZERO) THEN
          XSENS(7)  = XSENS(1)
          XSENS(8)  = XSENS(2)
          XSENS(9)  = XSENS(3)
          XSENS(10) = XSENS(4)
          XSENS(11) = XSENS(5)
          XSENS(12) = XSENS(6)
        ENDIF
      ENDIF
c
      DX = (XSENS(1) - XSENS(7)) - (XSENS(4) - XSENS(10))
      DY = (XSENS(2) - XSENS(8)) - (XSENS(5) - XSENS(11))
      DZ = (XSENS(3) - XSENS(9)) - (XSENS(6) - XSENS(12))
c
      XSENS(7)  = XSENS(1)
      XSENS(8)  = XSENS(2)
      XSENS(9)  = XSENS(3)
      XSENS(10) = XSENS(4)
      XSENS(11) = XSENS(5)
      XSENS(12) = XSENS(6)
c
c-----------------------
c     Force in Section
c
      IN = SECT_ID
      IF (IN > 0) THEN
        ISECT = TABS(IN+1) - TABS(IN)
        FX = FX + FBSAV6(10,1,ISECT) + FBSAV6(10,2,ISECT)
     .          + FBSAV6(10,3,ISECT) + FBSAV6(10,4,ISECT)
     .          + FBSAV6(10,5,ISECT) + FBSAV6(10,6,ISECT)
        FY = FY + FBSAV6(11,1,ISECT) + FBSAV6(11,2,ISECT)
     .          + FBSAV6(11,3,ISECT) + FBSAV6(11,4,ISECT)
     .          + FBSAV6(11,5,ISECT) + FBSAV6(11,6,ISECT)
        FZ = FZ + FBSAV6(12,1,ISECT) + FBSAV6(12,2,ISECT)
     .          + FBSAV6(12,3,ISECT) + FBSAV6(12,4,ISECT)
     .          + FBSAV6(12,5,ISECT) + FBSAV6(12,6,ISECT)
        SENSOR%VAR(1) = SENSOR%VAR(1) + ABS(FX*DX) + ABS(FY*DY) + ABS(FZ*DZ)
      ENDIF
c
c     Force in Interface
c
      IN = INT_ID
      INTER = IN
      IF (IN > NINTER) INTER = SUB_ID
      IF (INTER > 0) THEN
        INTER = TABS(NSECT+IN+1) - TABS(NSECT+IN)
        FX = FX + FBSAV6(1,1,INTER) + FBSAV6(1,2,INTER)
     .          + FBSAV6(1,3,INTER) + FBSAV6(1,4,INTER)
     .          + FBSAV6(1,5,INTER) + FBSAV6(1,6,INTER)
        FY = FY + FBSAV6(2,1,INTER) + FBSAV6(2,2,INTER)
     .          + FBSAV6(2,3,INTER) + FBSAV6(2,4,INTER)
     .          + FBSAV6(2,5,INTER) + FBSAV6(2,6,INTER)
        FZ = FZ + FBSAV6(3,1,INTER) + FBSAV6(3,2,INTER)
     .          + FBSAV6(3,3,INTER) + FBSAV6(3,4,INTER)
     .          + FBSAV6(3,5,INTER) + FBSAV6(3,6,INTER)
        SENSOR%VAR(2) = SENSOR%VAR(2) + ABS(FX*DX) + ABS(FY*DY) + ABS(FZ*DZ)
      ENDIF
c
c     Force in Rwall
c
      IN = RWAL_ID
      IF (IN > 0) THEN
        IRWAL = TABS(IN+NSECT+NINTER+NINTSUB+1)
     .        - TABS(IN+NSECT+NINTER+NINTSUB)
        FX = FX + FBSAV6(1,1,IRWAL) + FBSAV6(1,2,IRWAL)
     .          + FBSAV6(1,3,IRWAL) + FBSAV6(1,4,IRWAL)
     .          + FBSAV6(1,5,IRWAL) + FBSAV6(1,6,IRWAL)
        FY = FY + FBSAV6(2,1,IRWAL) + FBSAV6(2,2,IRWAL)
     .          + FBSAV6(2,3,IRWAL) + FBSAV6(2,4,IRWAL)
     .          + FBSAV6(2,5,IRWAL) + FBSAV6(2,6,IRWAL)
        FZ = FZ + FBSAV6(3,1,IRWAL) + FBSAV6(3,2,IRWAL)
     .          + FBSAV6(3,3,IRWAL) + FBSAV6(3,4,IRWAL)
     .          + FBSAV6(3,5,IRWAL) + FBSAV6(3,6,IRWAL)
        SENSOR%VAR(3) = SENSOR%VAR(3) + ABS(FX*DX) + ABS(FY*DY) + ABS(FZ*DZ)
      ENDIF
c
c     Force in Rbody
c
      IN  = RBOD_ID
      IF (IN > 0) THEN
        IRBOD = TABS(IN+NSECT+NINTER+NINTSUB+NRWALL+1)
     .        - TABS(IN+NSECT+NINTER+NINTSUB+NRWALL)
        FX = FX + FBSAV6(1,1,IRBOD) + FBSAV6(1,2,IRBOD)
     .          + FBSAV6(1,3,IRBOD) + FBSAV6(1,4,IRBOD)
     .          + FBSAV6(1,5,IRBOD) + FBSAV6(1,6,IRBOD)
        FY = FY + FBSAV6(2,1,IRBOD) + FBSAV6(2,2,IRBOD)
     .          + FBSAV6(2,3,IRBOD) + FBSAV6(2,4,IRBOD)
     .          + FBSAV6(2,5,IRBOD) + FBSAV6(2,6,IRBOD)
        FZ = FZ + FBSAV6(3,1,IRBOD) + FBSAV6(3,2,IRBOD)
     .          + FBSAV6(3,3,IRBOD) + FBSAV6(3,4,IRBOD)
     .          + FBSAV6(3,5,IRBOD) + FBSAV6(3,6,IRBOD)
        SENSOR%VAR(4) = SENSOR%VAR(4) + ABS(FX*DX) + ABS(FY*DY) + ABS(FZ*DZ)
      ENDIF
c----------------------------------------------------------------
      WORK = SENSOR%VAR(1) + SENSOR%VAR(2) + SENSOR%VAR(3) + SENSOR%VAR(4)
      IF (WORK > WMAX) ICRIT = 1
c----------------------------------------------------------------
c     check activation status
c----------------------------------------------------------------
      IF (SENSOR%TCRIT + TMIN > TT) THEN
        IF (ICRIT == 0) THEN
          SENSOR%TCRIT = INFINITY
        ELSE IF (SENSOR%TCRIT == INFINITY) THEN
          SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
        END IF
      END IF
      IF (SENSOR%TSTART == INFINITY .and. SENSOR%TCRIT < INFINITY) THEN
        SENSOR%TSTART = SENSOR%TCRIT + TMIN + TDELAY      
      END IF
      IF (SENSOR%TSTART <= TT) THEN   ! sensor activation
        SENSOR%STATUS = 1
      END IF
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1200) WMAX,WORK
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR WORK NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      TARGET WORK = ',1PE12.5,/
     .       '      CURRENT WORK AFTER TMIN AND TDELAY = ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
