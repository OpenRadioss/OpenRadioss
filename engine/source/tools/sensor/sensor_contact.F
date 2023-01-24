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
Chd|  SENSOR_CONTACT                source/tools/sensor/sensor_contact.F
Chd|-- called by -----------
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|-- calls ---------------
Chd|        BUTTERWORTH                   source/tools/univ/butterworth.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_CONTACT(SENSOR ,
     .           IPARI    ,DIMFB   ,STABS   ,TABS   ,FBSAV6)
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
      INTEGER :: TABS(STABS),IPARI(NPARI,NINTER)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE (SENSOR_STR_) ,TARGET   :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: INT_ID,INTSUB,ID_INTER,IDIR,ICRIT,ISECT,IFLAG
      my_real :: FMIN,FMAX,FF,FV,FV1,FV2,VV1,VV2,FREQ,
     .           TMIN,TDELAY,TCRIT,TSTART,TSTOPS,INFINITY
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
      TMIN   = SENSOR%TMIN
      TDELAY = SENSOR%TDELAY
      TCRIT  = SENSOR%TCRIT
      TSTOPS = SENSOR%VALUE
c      
      INT_ID = SENSOR%IPARAM(1) 
      INTSUB = SENSOR%IPARAM(2) 
      IDIR   = SENSOR%IPARAM(3) 
      FMIN   = SENSOR%RPARAM(1) 
      FMAX   = SENSOR%RPARAM(2) 
      FREQ   = SENSOR%RPARAM(3) 
c
      ISECT = TABS(NSECT+INT_ID+1)-TABS(NSECT+INT_ID)
      ICRIT = 0
      IFLAG = 0
      FF    = ZERO
c
      IF (IDIR == 1) THEN
        FF = SQRT((FBSAV6(1,1,ISECT) + FBSAV6(1,2,ISECT)
     .   	 +  FBSAV6(1,3,ISECT) + FBSAV6(1,4,ISECT)
     .   	 +  FBSAV6(1,5,ISECT) + FBSAV6(1,6,ISECT))**2  
     .     + (FBSAV6(2,1,ISECT) + FBSAV6(2,2,ISECT)
     .   	 +  FBSAV6(2,3,ISECT) + FBSAV6(2,4,ISECT)
     .   	 +  FBSAV6(2,5,ISECT) + FBSAV6(2,6,ISECT))**2 
     .     + (FBSAV6(3,1,ISECT) + FBSAV6(3,2,ISECT)
     .   	 +  FBSAV6(3,3,ISECT) + FBSAV6(3,4,ISECT)
     .   	 +  FBSAV6(3,5,ISECT) + FBSAV6(3,6,ISECT))**2) 
      ELSEIF (IDIR == 2) THEN
        FF = SQRT((FBSAV6(4,1,ISECT)+FBSAV6(4,2,ISECT)
     .     +  FBSAV6(4,3,ISECT)+FBSAV6(4,4,ISECT)
     .     +  FBSAV6(4,5,ISECT)+FBSAV6(4,6,ISECT))**2 
     .     + (FBSAV6(5,1,ISECT)+FBSAV6(5,2,ISECT)
     .     +  FBSAV6(5,3,ISECT)+FBSAV6(5,4,ISECT)
     .     +  FBSAV6(5,5,ISECT)+FBSAV6(5,6,ISECT))**2
     .     + (FBSAV6(6,1,ISECT)+FBSAV6(6,2,ISECT)
     .     +  FBSAV6(6,3,ISECT)+FBSAV6(6,4,ISECT)
     .     +  FBSAV6(6,5,ISECT)+FBSAV6(6,6,ISECT))**2) 
      END IF
c
      IF (FREQ > ZERO) THEN   ! Filter contact forces (Butterworth) 
    	   IF (TT == ZERO) THEN                           
    	     VV1 = FF                                     
    	     VV2 = FF                                     
    	     FV1 = FF                                     
    	     FV2 = FF                                     
    	     SENSOR%VAR(1) = VV1                             
    	     SENSOR%VAR(2) = VV2                            
    	     SENSOR%VAR(3) = FV1                            
    	     SENSOR%VAR(4) = FV2                            
    	   ENDIF                                          
c
    	   VV1 = SENSOR%VAR(1)                                 
    	   VV2 = SENSOR%VAR(2)                                
    	   FV1 = SENSOR%VAR(3)                                
    	   FV2 = SENSOR%VAR(4)                                
    	   CALL BUTTERWORTH(DT2,FREQ,VV2,VV1,FF,FV2,FV1,FV) 
    	   SENSOR%VAR(1) = FF                                  
    	   SENSOR%VAR(2) = VV1                                
    	   SENSOR%VAR(3) = FV                                 
    	   SENSOR%VAR(4) = FV1                                
    	   FF = FV
      END IF
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
      ID_INTER = INT_ID
      IF (INT_ID > NINTER) ID_INTER = INTSUB
c
      IF (IPARI(29,ID_INTER) == 1) THEN
c
        IF (ICRIT == 1) THEN
          TCRIT = MIN(TCRIT, TT)
        ELSE
          TCRIT = INFINITY
        ENDIF
        SENSOR%TCRIT = TCRIT

        IF (TCRIT + TMIN <= TT) THEN  ! activation
          TSTOPS = TT + TDELAY
          SENSOR%VALUE  = TSTOPS

          IF (SENSOR%STATUS == 0) THEN
            SENSOR%STATUS = 1
            TSTART = TT
            SENSOR%TSTART = TSTART
            IF (ISPMD == 0) THEN
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
          ENDIF
        ENDIF
c
      ELSE IF (TT > TSTOPS .AND. SENSOR%STATUS == 1) THEN  ! deactivation
c
        SENSOR%STATUS = 0
        SENSOR%TSTART = INFINITY
        SENSOR%TCRIT  = INFINITY
        SENSOR%VALUE  = INFINITY

        IF (ISPMD == 0) THEN
#include "lockon.inc"
          WRITE (ISTDO,1200) SENSOR%SENS_ID,TT
          WRITE (IOUT ,1200) SENSOR%SENS_ID,TT
#include "lockoff.inc"
        END IF
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR (CONTACT) NUMBER ',I10,' ACTIVATED AT TIME '  ,1PE12.5)
1200  FORMAT(' SENSOR (CONTACT) NUMBER ',I10,' DEACTIVATED AT TIME ',1PE12.5)
1300  FORMAT('    TARGET MIN FORCE = ',1PE12.5,/
     .       '    CURRENT FORCE AFTER TMIN and TDELAY = ',1PE12.5/)
1400  FORMAT('    TARGET MAX FORCE = ',1PE12.5,/
     .       '    CURRENT FORCE AFTER TMIN and TDELAY = ',1PE12.5/)
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE SENSOR_CONTACT
      
