Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  USENSOR_DUM                   source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE USENSOR_DUM(IERR)
C---------+---------+---+---+--------------------------------------------
#include "implicit_f.inc"
       INTEGER IERR
       IERR=0
      END
Chd|====================================================================
Chd|  GET_U_NUMSENS                 source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        CSENS3                        source/elements/shell/coque/csens3.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_NUMSENS(IDSENS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDSENS
      INTEGER J,ID,ITYP
C=======================================================================
C
      GET_U_NUMSENS = 0
      IF(IDSENS<=0)THEN
        RETURN
      ENDIF
C---
      DO J=1,SENSORS%NSENSOR

        ID   = SENSORS%SENSOR_TAB(J)%SENS_ID
        ITYP = SENSORS%SENSOR_TAB(J)%TYPE

        IF(IDSENS==ID)THEN
          IF (ITYP>=29 .AND. ITYP<=31)THEN
            GET_U_NUMSENS = J
            RETURN
          ENDIF

        ENDIF

      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_ID                 source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_ID(ISENS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISENS
C=======================================================================
      GET_U_SENS_ID = 0

      IF(ISENS<=0)THEN
        RETURN
      ENDIF

      GET_U_SENS_ID = SENSORS%SENSOR_TAB(ISENS)%SENS_ID
C---
      RETURN
      END
Chd|====================================================================
Chd|  SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_VALUE(NSENS,IVAR,VAR)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER K,L, SHIFT
      INTEGER NSENS,ITYP,IVAR 
      my_real 
     .      VAR
C=======================================================================
C
      SET_U_SENS_VALUE = -1

      IF(NSENS==0)THEN
        RETURN
      ENDIF

      IF(IVAR>LSENBUF-1)THEN
        WRITE(*,*)"ERROR WRITING USER SENSOR / TOO MANY VARIABLES"
        RETURN
      ENDIF

      ITYP = SENSORS%SENSOR_TAB(NSENS)%TYPE
C
      IF (ITYP>=29 .AND. ITYP<=31)THEN

        SENSORS%SENSOR_TAB(NSENS)%FLOAT_USERBUF(IVAR) = VAR
        SET_U_SENS_VALUE = 0

      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_VALUE(NSENS, IVAR, VAR)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER K,L, SHIFT,NSENS, ITYP, IVAR
      my_real VAR
C=======================================================================
C
      GET_U_SENS_VALUE = -1

      IF(NSENS==0)THEN
        VAR = 0.0
        RETURN
      ENDIF

      IF(IVAR>LSENBUF-1)THEN
        WRITE(*,*)"ERROR READING USER SENSOR / TOO MANY VARIABLES"
        RETURN
      ENDIF

      ITYP =  SENSORS%SENSOR_TAB(NSENS)%TYPE

      IF (ITYP>=29 .AND. ITYP<=31)THEN

        VAR = SENSORS%SENSOR_TAB(NSENS)%FLOAT_USERBUF(IVAR)
        GET_U_SENS_VALUE = 0

      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  SET_U_SENS_MAXVALUE           source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        CSENS3                        source/elements/shell/coque/csens3.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_MAXVALUE(NSENS,IVAR,VAR)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER K,L, SHIFT
      INTEGER NSENS,ITYP,IVAR 
      my_real VAR,MAXV
C=======================================================================
      SET_U_SENS_MAXVALUE = -1

      IF(NSENS==0)THEN
        RETURN
      ENDIF

      IF (IVAR>NSENPARR) THEN
        WRITE(*,*)"ERROR WRITING USER SENSOR / TOO MANY VARIABLES"
        RETURN
      ENDIF

      MAXV = SENSORS%SENSOR_TAB(NSENS)%FLOAT_USERBUF(IVAR)
      MAXV = MAX(MAXV,VAR)
      SENSORS%SENSOR_TAB(NSENS)%FLOAT_USERBUF(IVAR) = MAXV
      SET_U_SENS_MAXVALUE = 0    
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_FPAR               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_FPAR(NSENS,IVAR,VAR)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER NSENS,IVAR,ITYP
      my_real VAR
C=======================================================================
C

      GET_U_SENS_FPAR = -1

      IF(IVAR>NSENPARR)THEN

        WRITE(IOUT,*)  ' **ERROR NUMBER OF USER PARAMETER(',IVAR,') EXCEED MAXNUPARAM(',NSENPARR,')'
        WRITE(ISTDO,*) ' **ERROR NUMBER OF USER PARAMETER(',IVAR,') EXCEED MAXNUPARAM(',NSENPARR,')'
        GET_U_SENS_FPAR = NSENPARR

        RETURN
      ENDIF
C
      ITYP = SENSORS%SENSOR_TAB(NSENS)%TYPE

      IF ( ITYP>=29 .AND. ITYP<=31 )THEN

        VAR = SENSORS%SENSOR_TAB(NSENS)%FLOAT_USERPARAM(IVAR)
        GET_U_SENS_FPAR = 0

      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_IPAR               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_IPAR(NSENS,IVAR,VAR)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER NSENS,IVAR,VAR,ITYP
C=======================================================================
      GET_U_SENS_IPAR = -1

      IF(IVAR>NSENPARI)THEN

        WRITE(IOUT,*)  ' **ERROR NUMBER OF USER PARAMETER(',IVAR,') EXCEED MAXNUPARAM(',NSENPARI,')'
        WRITE(ISTDO,*) ' **ERROR NUMBER OF USER PARAMETER(',IVAR,') EXCEED MAXNUPARAM(',NSENPARI,')'

        GET_U_SENS_IPAR = NSENPARI

        RETURN

      ENDIF

      ITYP = SENSORS%SENSOR_TAB(NSENS)%TYPE

      IF (ITYP>=29 .AND. ITYP<=31)THEN

        VAR = SENSORS%SENSOR_TAB(NSENS)%INTEGER_USERPARAM(IVAR)
        GET_U_SENS_IPAR = 0
   
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  SET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_ACTI(NSENS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER NSENS,K,L,ITYP,LL
      my_real TSTART,SETVAR
c========================================================================
      SET_U_SENS_ACTI = -1


      IF (NSENS > 0 .OR. NSENS <= SENSORS%NSENSOR ) THEN
      ITYP = SENSORS%SENSOR_TAB(NSENS)%TYPE

        IF (ITYP >= 29 .AND. ITYP <= 31)THEN
  
          TSTART = SENSORS%SENSOR_TAB(NSENS)%TSTART
          SETVAR = MIN(TT,TSTART)

          SENSORS%SENSOR_TAB(NSENS)%TSTART = SETVAR
          SENSORS%SENSOR_TAB(NSENS)%STATUS = 1
          SET_U_SENS_ACTI = 0

        ENDIF

      ENDIF
C---
      RETURN
      END
c
Chd|====================================================================
Chd|  SET_U_SENS_DEACTI             source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_DEACTI(NSENS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER NSENS,K,L,ITYP,LL
      my_real TSTART,SETVAR
c========================================================================
      SET_U_SENS_DEACTI = -1

      IF (NSENS >= 1 .OR. NSENS <= SENSORS%NSENSOR) THEN

        ITYP = SENSORS%SENSOR_TAB(NSENS)%TYPE

        IF (ITYP >= 29 .AND. ITYP <= 31)THEN
          SENSORS%SENSOR_TAB(NSENS)%TSTART = EP30
          SET_U_SENS_DEACTI = 0
        ENDIF
      ENDIF
C---
      RETURN
      END
c
Chd|====================================================================
Chd|  GET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      my_real FUNCTION GET_U_SENS_ACTI(NSENS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER NSENS,L,ITYP
      my_real TSTART
C-----------------------------------------------
      GET_U_SENS_ACTI = ZERO


      
      IF (NSENS < 1 .OR. NSENS > SENSORS%NSENSOR) THEN

        ! Sensor not existing
        ! --------------------
        GET_U_SENS_ACTI = TT

      ELSE

        TSTART=SENSORS%SENSOR_TAB(NSENS)%TSTART

        IF (TT > TSTART) THEN
          GET_U_SENS_ACTI = TT-TSTART
        ENDIF

      ENDIF
C---
      RETURN
      END
c
Chd|====================================================================
Chd|  GET_U_SENS                    source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        RUSER32                       source/elements/spring/ruser32.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      my_real FUNCTION GET_U_SENS(USENS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER USENS,I,J,LI
      my_real LT
C-----------------------------------------------
C
      GET_U_SENS = ZERO

      ! -----------------
      ! CASE : No Sensor
      ! -----------------
      IF(USENS==0)THEN
        GET_U_SENS = TT
        RETURN
      ENDIF

      ! --------------------
      ! CASE : Find Sensor, 
      !        Do Treatment
      ! --------------------
      DO J=1,SENSORS%NSENSOR
        
          LI = SENSORS%SENSOR_TAB(J)%SENS_ID

          IF (USENS == LI) THEN 

            LT = SENSORS%SENSOR_TAB(J)%TSTART
            ! If sensor is activated - return the time Sensor is active
            IF (TT > LT) THEN
            GET_U_SENS = TT - LT
          ENDIF

          RETURN

        ENDIF
      ENDDO
C----
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_DELAY              source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      my_real FUNCTION GET_U_SENS_DELAY(NSENS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE SENSOR_MOD
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include   "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s  
C-----------------------------------------------
      INTEGER NSENS
      my_real TDELAY
C-----------------------------------------------
C
      GET_U_SENS_DELAY = ZERO

      ! CASE : No Sensor
      ! -----------------
      IF(NSENS==0)THEN
        GET_U_SENS_DELAY = TT
        RETURN
      ENDIF

      ! Sensor is not existing
      ! -----------------------
      IF ( NSENS < 1 .OR. NSENS > SENSORS%NSENSOR) THEN
         GET_U_SENS_DELAY = TT
        RETURN
      ENDIF

      ! TDELAY Value
      ! -------------
     
      TDELAY = SENSORS%SENSOR_TAB(NSENS)%TDELAY
      GET_U_SENS_DELAY = TDELAY
C---
      RETURN
      END
