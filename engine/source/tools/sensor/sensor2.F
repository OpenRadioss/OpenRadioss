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
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  SENSOR2                       source/tools/sensor/sensor2.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_USER_SENS         source/user_interface/dyn_userlib.c
Chd|        USENS_SHIFT_BA                source/user_interface/usensor.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR2(SENSOR_TAB,LSENSOR,SENSOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr_usens_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LSENSOR(NSENSI,*)
C     REAL
      my_real SENSOR(NSENSR,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), TARGET :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K,IS1,IS2,TYP,OLD_TYP,NSENS,ACTI,TYP1,TYP2,STATUS1,STATUS2
      my_real :: TDELAY,TSTART,TSTART1,TSTART2,TSTOPS,INFINITY
      CHARACTER OPTION*256
      INTEGER SIZE
      PARAMETER (INFINITY = 1.0E20)
C=======================================================================
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      DO K = 1, NSENSOR
       ACTI = 0
c      initialisation de TSTART a TT pour ecriture finale si ACTI=1
       TSTART = TT
       OLD_TYP = LSENSOR(2,K)
       TYP     = SENSOR_TAB(K)%TYPE
       IF (TYP <= 0 .and. OLD_TYP > 28) TYP = OLD_TYP
c      !----------------------------------------------!
       IF (TYP == 3) THEN
C--------------------------------
C         /SENSOR/SENS :  OFF/ON/OFF
C--------------------------------
          NSENS   = SENSOR_TAB(K)%SENS_ID
          IS1     = SENSOR_TAB(K)%IPARAM(1)
          IS2     = SENSOR_TAB(K)%IPARAM(2)
          TSTART  = SENSOR_TAB(K)%TSTART
          TDELAY  = SENSOR_TAB(K)%TDELAY
          TSTOPS  = INFINITY
c
          TYP1    = SENSOR_TAB(IS1)%TYPE
          TSTART1 = SENSOR_TAB(IS1)%TSTART
          STATUS1 = SENSOR_TAB(IS1)%STATUS
          IF (STATUS1 == 0) THEN
            TSTART1 = INFINITY
          END IF
          IF (TT >= TSTART1) THEN
            IF (TSTART /= TSTART1) ACTI = 1
            TSTART = TSTART1
            SENSOR_TAB(K)%TSTART = TSTART
          ENDIF
          IF (TSTART + TDELAY < TT) THEN
            IF (IS2 == 0) THEN
              TSTOPS = TT
            ELSE
              TSTOPS = SENSOR_TAB(IS2)%TSTART
              IF (SENSOR_TAB(IS2)%STATUS == 0) THEN
                TSTOPS = INFINITY
              ENDIF
            ENDIF
            IF (TSTOPS <= TT) THEN
              SENSOR_TAB(K)%TSTART = INFINITY
              SENSOR_TAB(K)%STATUS = 0
              ACTI = ACTI + 2
            ENDIF
          ENDIF
c
        ELSEIF (TYP == 4) THEN
C--------------------------------
C         SENSOR AND
C--------------------------------
          NSENS   = SENSOR_TAB(K)%SENS_ID
          IS1     = SENSOR_TAB(K)%IPARAM(1)
          IS2     = SENSOR_TAB(K)%IPARAM(2)
          TYP1    = SENSOR_TAB(IS1)%TYPE
          TYP2    = SENSOR_TAB(IS2)%TYPE
          TSTART1 = SENSOR_TAB(IS1)%TSTART
          TSTART2 = SENSOR_TAB(IS2)%TSTART
          STATUS1 = SENSOR_TAB(IS1)%STATUS
          STATUS2 = SENSOR_TAB(IS2)%STATUS
          IF (STATUS1 == 0) THEN
             TSTART1 = INFINITY
          ELSE
             TSTART1 = ZERO
          END IF
          IF (STATUS2 == 0) THEN
             TSTART2 = INFINITY
          ELSE
             TSTART2 = ZERO
          END IF
          IF (SENSOR_TAB(K)%STATUS == 0 .AND. TT >= MAX(TSTART1,TSTART2)) THEN
            ACTI = 1
            SENSOR_TAB(K)%TSTART = TT
            SENSOR_TAB(K)%STATUS = 1
          ELSEIF (SENSOR_TAB(K)%STATUS == 1 .AND. TT < MAX(TSTART1,TSTART2)) THEN
            ACTI = 2
            SENSOR_TAB(K)%TSTART = INFINITY
            SENSOR_TAB(K)%STATUS = 0
          ENDIF
c
        ELSEIF (TYP == 5) THEN
C--------------------------------
C         SENSOR OR
C--------------------------------
          NSENS   = SENSOR_TAB(K)%SENS_ID
          IS1     = SENSOR_TAB(K)%IPARAM(1)
          IS2     = SENSOR_TAB(K)%IPARAM(2)
          TYP1    = SENSOR_TAB(IS1)%TYPE
          TYP2    = SENSOR_TAB(IS2)%TYPE
          TSTART1 = SENSOR_TAB(IS1)%TSTART
          TSTART2 = SENSOR_TAB(IS2)%TSTART
          STATUS1 = SENSOR_TAB(IS1)%STATUS
          STATUS2 = SENSOR_TAB(IS2)%STATUS
          IF (SENSOR_TAB(K)%STATUS == 0) THEN
            IF (STATUS1 == 0) THEN
              TSTART1 = INFINITY
            END IF
            IF (STATUS2 == 0) THEN
               TSTART2 = INFINITY
            END IF
            IF (TT >= MIN(TSTART1,TSTART2)) THEN
                ACTI = 1
                SENSOR_TAB(K)%TSTART = TT
                SENSOR_TAB(K)%STATUS = 1
            ENDIF
          ELSE IF (SENSOR_TAB(K)%STATUS == 1 .and. TT < TSTART1 .and. TT < TSTART2) THEN
            ACTI = 2
            SENSOR_TAB(K)%TSTART = INFINITY          
            SENSOR_TAB(K)%STATUS = 0
          ENDIF
c
        ELSEIF (TYP == 8) THEN
C--------------------------------
C         SENSOR NOT
C--------------------------------
          NSENS   = SENSOR_TAB(K)%SENS_ID
          IS1     = SENSOR_TAB(K)%IPARAM(1)
          TYP1    = SENSOR_TAB(IS1)%TYPE
          IF (TYP1 == 0 .or. TYP1==3 .OR. TYP1==4 .OR.    
     .        TYP1 == 5 .OR. TYP1==8 .OR. TYP1==9) THEN                                          
            IF (SENSOR_TAB(IS1)%STATUS == 1) THEN
              IF (SENSOR_TAB(K)%STATUS == 1) THEN
                ACTI = 2
                SENSOR_TAB(K)%TSTART = INFINITY
                SENSOR_TAB(K)%STATUS = 0
              END IF
            ELSE
              IF (SENSOR_TAB(K)%STATUS == 0) THEN
                ACTI = 1
                SENSOR_TAB(K)%TSTART = TT
                SENSOR_TAB(K)%STATUS = 1
              END IF
            ENDIF

          ELSE
            TSTART1 = SENSOR_TAB(IS1)%TSTART
            IF (TT >= TSTART1) THEN
              IF (SENSOR_TAB(K)%STATUS == 1) ACTI = 2
              SENSOR_TAB(K)%TSTART = INFINITY
              SENSOR_TAB(K)%STATUS = 0
            ELSE
              IF (SENSOR_TAB(K)%STATUS == 0) THEN
                ACTI = 1
                SENSOR_TAB(K)%TSTART = TT
                SENSOR_TAB(K)%STATUS = 1
              ENDIF
            ENDIF
          ENDIF
C--------------------------------
C         USER SENSOR
C--------------------------------
        ELSEIF (TYP>=29 .AND. TYP<=31) THEN
          IF (TYP == 29) THEN
            NSENS  = LSENSOR(1,K)
            IF (USERL_AVAIL==1) THEN
              CALL ENG_USERLIB_USER_SENS(TYP,NSENS)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
              OPTION='USER SENSOR'
              SIZE=LEN_TRIM(OPTION)
              CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
              CALL ARRET(2)
             ! ----------------
            ENDIF
          ELSEIF (TYP == 30) THEN
            NSENS  = LSENSOR(1,K)
            IF (USERL_AVAIL==1) THEN
              CALL ENG_USERLIB_USER_SENS(TYP,NSENS)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
              OPTION='USER SENSOR'
              SIZE=LEN_TRIM(OPTION)
              CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
              CALL ARRET(2)
             ! ----------------
            ENDIF
          ELSEIF (TYP == 31) THEN
            NSENS  = LSENSOR(1,K)
            IF (USERL_AVAIL==1) THEN
              CALL ENG_USERLIB_USER_SENS(TYP,NSENS)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
              OPTION='USER SENSOR'
              SIZE=LEN_TRIM(OPTION)
              CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
              CALL ARRET(2)
             ! ----------------
            ENDIF
          ENDIF
          CALL USENS_SHIFT_BA(SENSOR(1,K))
        ENDIF
c-----------------------------------------------------------------------
        IF (ACTI == 1) THEN
#include "lockon.inc"
           IF (ISPMD == 0)
     .       WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .   ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
#include "lockoff.inc"
       ELSEIF (ACTI == 2) THEN
#include "lockon.inc"
           IF (ISPMD == 0)
     .       WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .   ' SENSOR NUMBER ',NSENS,' DEACTIVATED AT TIME ',TT
#include "lockoff.inc"
        ENDIF
c-----------------------------------------------------------------------
       ENDDO ! K = 1, NSENSOR
C-----------
      RETURN
      END
