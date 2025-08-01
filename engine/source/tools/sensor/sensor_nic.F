Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    sensor_nic     ../engine/source/tools/sensor/sensor_nic.F
!||--- called by ------------------------------------------------------
!||    sensor_base    ../engine/source/tools/sensor/sensor_base.F
!||--- uses       -----------------------------------------------------
!||    groupdef_mod   ../common_source/modules/groupdef_mod.F
!||    sensor_mod     ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE SENSOR_NIC(SENSOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SENSOR_STR_) :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER NPT,NPH,IADFX,IADFY,IADMX,IADMY,ICRIT
      my_real :: FOR,MOM,DT,TIME,NIJMAX,NIC,ALPHA,BETA,CFC,TMIN,TDELAY,
     .   WD,WA,WA2,WA3,A0,A1,A2,B1,B2,X0,X1,X2,Y1,Y2,YF,YM,INFINITY
      PARAMETER (INFINITY = 1.0E20)
c-----------------------------------------------------------------------
c     Sensor state variables
c        SENSOR%VAR(1) :  Current non filtered spring force
c        SENSOR%VAR(2) :  Current non filtered spring moment
c        SENSOR%VAR(3) :  flag spmd : =0 => spring not present in the domain
c                                     =1 => spring is present in the domain
c        free
c        SENSOR%VAR(6 -10)  :  non filtered force values 
c        SENSOR%VAR(11-15)  :  filtered force values    
c        SENSOR%VAR(16-20   :  non filtered moment values 
c        SENSOR%VAR(21-25)  :  filtered moment values 
C=======================================================================
      IF (SENSOR%STATUS == 1)    RETURN   ! already activated
      IF (SENSOR%VAR(3) == ZERO) RETURN   ! spring is on different spmd domain
c
      TMIN   = SENSOR%TMIN
      TDELAY = SENSOR%TDELAY
      NIJMAX = SENSOR%RPARAM(1)
      ALPHA  = SENSOR%RPARAM(6)
      CFC    = SENSOR%RPARAM(7)
      TIME   = TT
      ICRIT  = 0
c
c     Parameters of SAE-J211 CFC filter
      DT   = DT2    
      WD   = TWO*PI*CFC*ALPHA
      BETA = HALF * DT * WD
      WA   = TAN(BETA)
      WA2  = WA**2
      WA3  = ONE + SQR2*WA + WA2
      A0   = WA2 / WA3
      A1   = A0 * TWO
      A2   = A0
      B1   = -TWO*(WA2 - ONE) / WA3
      B2   = (-ONE + SQR2*WA - WA2) / WA3
c
c     current non filtered normalized spring force and moment
      FOR  = SENSOR%VAR(1)
      MOM  = SENSOR%VAR(2)
c
      NPT = 5
      NPH = 3
      IADFX = 5
      IADFY = IADFX + NPT
      IADMX = IADFY + NPT
      IADMY = IADMX + NPT
c
      ! forward filter - forces
c
      X0 = FOR
      X1 = SENSOR%VAR(IADFX+5)      
      X2 = SENSOR%VAR(IADFX+4)    
      Y1 = SENSOR%VAR(IADFY+5)  
      Y2 = SENSOR%VAR(IADFY+4)
c      
      YF = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2
c              
      SENSOR%VAR(IADFX+1) = SENSOR%VAR(IADFX+2)
      SENSOR%VAR(IADFX+2) = SENSOR%VAR(IADFX+3)
      SENSOR%VAR(IADFX+3) = SENSOR%VAR(IADFX+4)
      SENSOR%VAR(IADFX+4) = SENSOR%VAR(IADFX+5)  
      SENSOR%VAR(IADFX+5) = FOR
c
      SENSOR%VAR(IADFY+1) = SENSOR%VAR(IADFY+2)
      SENSOR%VAR(IADFY+2) = SENSOR%VAR(IADFY+3)
      SENSOR%VAR(IADFY+3) = SENSOR%VAR(IADFY+4)
      SENSOR%VAR(IADFY+4) = SENSOR%VAR(IADFY+5)  
      SENSOR%VAR(IADFY+5) = YF
c
      ! backward filter - forces
c
      X0 = SENSOR%VAR(IADFX+NPH)
      X1 = SENSOR%VAR(IADFX+NPH+1)
      X2 = SENSOR%VAR(IADFX+NPH+2)
      Y1 = SENSOR%VAR(IADFY+NPH+1)
      Y2 = SENSOR%VAR(IADFY+NPH+2)
c      
      FOR = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2
c
      ! forward filter - moments
      X0 = MOM
      X1 = SENSOR%VAR(IADMX+5)      
      X2 = SENSOR%VAR(IADMX+4)
      Y1 = SENSOR%VAR(IADMY+5)
      Y2 = SENSOR%VAR(IADMY+4)
c      
      YM = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2
c              
      SENSOR%VAR(IADMX+1) = SENSOR%VAR(IADMX+2)
      SENSOR%VAR(IADMX+2) = SENSOR%VAR(IADMX+3)
      SENSOR%VAR(IADMX+3) = SENSOR%VAR(IADMX+4)
      SENSOR%VAR(IADMX+4) = SENSOR%VAR(IADMX+5)  
      SENSOR%VAR(IADMX+5) = MOM
      
      SENSOR%VAR(IADMY+1) = SENSOR%VAR(IADMY+2)
      SENSOR%VAR(IADMY+2) = SENSOR%VAR(IADMY+3)
      SENSOR%VAR(IADMY+3) = SENSOR%VAR(IADMY+4)
      SENSOR%VAR(IADMY+4) = SENSOR%VAR(IADMY+5)  
      SENSOR%VAR(IADMY+5) = YM
c
      ! backward filter - moments
c      
      X0  = SENSOR%VAR(IADMX+NPH)
      X1  = SENSOR%VAR(IADMX+NPH+1)
      X2  = SENSOR%VAR(IADMX+NPH+2)
      Y1  = SENSOR%VAR(IADMY+NPH+1)
      Y2  = SENSOR%VAR(IADMY+NPH+2)
      MOM = A0*X0 + A1*X1 + A2*X2 + B1*Y1 + B2*Y2               
c
      NIC = FOR + MOM
      IF (NIC >= NIJMAX) ICRIT = 1
c-----------------------------------------
c     Check Sensor State
c-----------------------------------------
      IF (SENSOR%TCRIT + TMIN > TT) THEN
        IF (ICRIT == 0) THEN
          SENSOR%TCRIT = INFINITY
        ELSE IF (SENSOR%TCRIT == INFINITY) THEN
          SENSOR%TCRIT = MIN(SENSOR%TCRIT, TT)
        END IF
      ELSE IF (SENSOR%TSTART == INFINITY) THEN
        SENSOR%TSTART = SENSOR%TCRIT + TMIN + TDELAY
      END IF
      IF (SENSOR%TSTART <= TT) THEN   ! sensor activation
        SENSOR%STATUS = 1
      END IF
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 1 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100 ) SENSOR%SENS_ID,SENSOR%TSTART,NIC
        WRITE (IOUT ,1100 ) SENSOR%SENS_ID,SENSOR%TSTART,NIC
        WRITE (IOUT ,1200) NIJMAX,NIC
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR (RBODY) NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5)
1200  FORMAT('      TARGET NIC VALUE = ',1PE12.5,/
     .       '      CURRENT NIC VALUE AFTER TMIN and TDELAY = ',1PE12.5)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
