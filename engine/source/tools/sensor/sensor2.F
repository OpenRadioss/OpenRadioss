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
      SUBROUTINE SENSOR2(LSENSOR,SENSOR,SENSOR_TAB)
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
      INTEGER TYP, I, J, IS1, IS2, NSENS, ACTI,TYP1,TYP2
      my_real
     . DURE, TDELAY, TSTART, TSTART1, TSTOPS, TSTART2
      INTEGER K
!
      CHARACTER OPTION*256
      INTEGER SIZE
C=======================================================================
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      DO K = 1, NSENSOR
       ACTI = 0
c      initialisation de TSTART a TT pour ecriture finale si ACTI=1
       TSTART = TT
       TYP    = LSENSOR(2,K)
       IF (TYP == 3) THEN
C--------------------------------
C         SENSOR OFF/ON/OFF
C--------------------------------
          NSENS  = LSENSOR(1,K)
          IS1    = LSENSOR(4,K)
          IS2    = LSENSOR(5,K)
          DURE    = SENSOR(1,K)
          TDELAY  = SENSOR(1,IS1)
          TSTART1 = SENSOR(2,IS1)
          TYP1    = LSENSOR(2,IS1)
          IF (TYP1 == 0 .or. TYP1==3 .OR. TYP1==4 .OR. TYP1==5 .OR. TYP1==8 .OR.    
     .        TYP1==9) THEN                                          
            IF (LSENSOR(3,IS1) == 0) THEN
              TSTART1=EP20
            END IF
          ENDIF
          IF (TT >= TSTART1) THEN
            IF (SENSOR(2,K) /= TSTART1) ACTI = 1
            TSTART = TSTART1
            SENSOR(2,K) = TSTART
          ENDIF
          IF (TSTART1+DURE < TT) THEN
            IF (IS2 == 0) THEN
              TSTOPS = TT
            ELSE
              TSTOPS = SENSOR(2,IS2)
              TYP2   = LSENSOR(2,IS2)
              IF (TYP2 == 0 .or. TYP2==3 .OR. TYP2==4 .OR. TYP2==5 .OR. TYP2==8 .OR.
     .            TYP2==9) THEN
                IF (LSENSOR(3,IS2) == 0) THEN
                  TSTOPS=EP20
                ENDIF
              ENDIF
            ENDIF
            IF (TSTOPS <= TT) THEN
              SENSOR(2,K) = EP20
              ACTI = ACTI + 2
            ENDIF
          ENDIF
c
        ELSEIF(TYP == 4)THEN
C--------------------------------
C         SENSOR ET
C--------------------------------
          NSENS = LSENSOR(1,K)
          IS1   = LSENSOR(4,K)
          IS2   = LSENSOR(5,K)
          TYP1  = LSENSOR(2,IS1)
          TSTART1 = SENSOR(2,IS1)
          IF(TYP1 == 3.OR.TYP1 == 4.OR.TYP1 == 5.OR.TYP1 == 8)THEN
            IF(LSENSOR(3,IS1) == 0)THEN
               TSTART1=EP20
            ELSE
               TSTART1=ZERO
            END IF
          ENDIF
          TSTART2 = SENSOR(2,IS2)
          TYP2   = LSENSOR(2,IS2)
          IF (TYP2==3 .OR. TYP2==4 .OR. TYP2==5 .OR. TYP2==8 .OR.
     .        TYP2==9) THEN
            IF(LSENSOR(3,IS2) == 0)THEN
               TSTART2=EP20
            ELSE
               TSTART2=ZERO
            END IF
          ENDIF
          IF(LSENSOR(3,K) == 0.AND.TT.GE.MAX(TSTART1,TSTART2))THEN
              ACTI = 1
              SENSOR(2,K) = TT
          ELSEIF(LSENSOR(3,K) == 1.AND.TT < MAX(TSTART1,TSTART2))THEN
              ACTI = 2
              SENSOR(2,K) = EP20
          ENDIF
        ELSEIF (TYP == 5) THEN
C--------------------------------
C         SENSOR OU
C--------------------------------
          NSENS = LSENSOR(1,K)
          IS1   = LSENSOR(4,K)
          IS2   = LSENSOR(5,K)
          TYP1  = LSENSOR(2,IS1)
          TYP2  = LSENSOR(2,IS2)
          TSTART1 = SENSOR(2,IS1)
          TSTART2 = SENSOR(2,IS2)
          IF (LSENSOR(3,K) == 0) THEN
            IF (TYP1==0 .OR. TYP1==3 .OR. TYP1==4 .OR. TYP1==5 .OR. TYP1==8 .OR.    
     .          TYP1==9) THEN                                          
              IF (LSENSOR(3,IS1) == 0) THEN
                TSTART1=EP20
              END IF
            ENDIF
            IF (TYP2==0 .OR. TYP2==3 .OR. TYP2==4 .OR. TYP2==5 .OR. TYP2==8 .OR.
     .          TYP2==9) THEN
              IF (LSENSOR(3,IS2) == 0) THEN
                 TSTART2=EP20
              END IF
            ENDIF
            IF (TT >= MIN(TSTART1,TSTART2)) THEN
                ACTI = 1
                SENSOR(2,K) = TT
            ENDIF
          ELSE IF (LSENSOR(3,K) == 1 .and. TT < TSTART1 .and. TT < TSTART2) THEN
              ACTI = 2
              SENSOR(2,K) = EP20          
          ENDIF
        ELSEIF(TYP == 8)THEN
C--------------------------------
C         SENSOR non
C--------------------------------
          NSENS = LSENSOR(1,K)
          IS1   = LSENSOR(4,K)
          TYP1  = LSENSOR(2,IS1)
          IF (TYP1 == 0 .or. TYP1==3 .OR. TYP1==4 .OR. TYP1==5 .OR. TYP1==8 .OR.    
     .        TYP1==9) THEN                                          
            IF (LSENSOR(3,IS1) == 1)THEN
              IF (LSENSOR(3,K) == 1) THEN
                ACTI = 2
                SENSOR(2,K) = EP20
              END IF
            ELSE
              IF (LSENSOR(3,K) == 0) THEN
                ACTI = 1
                SENSOR(2,K) = TT
              END IF
            ENDIF
          ELSEIF  (TYP1==13) THEN    
            TSTART1 = SENSOR(2,IS1)
            IF (TT >= TSTART1) THEN
              IF (LSENSOR(3,K) == 1) ACTI = 2
              SENSOR(2,K) = EP20
            ELSE
              IF (LSENSOR(3,K) == 0) THEN
                ACTI = 1
                SENSOR(2,K) = TT
              ENDIF
            ENDIF
          ELSE
            TSTART1 = SENSOR(2,IS1)
            IF (TT >= TSTART1) THEN
              IF (LSENSOR(3,K) == 1) ACTI = 2
              SENSOR(2,K) = EP20
            ELSE
              IF (LSENSOR(3,K) == 0) THEN
                ACTI = 1
                SENSOR(2,K) = TT
              ENDIF
            ENDIF
          ENDIF
C--------------------------------
        ELSEIF (TYP == 13) THEN    ! sensor WORK
C--------------------------------
C         USER SENSOR
C--------------------------------
        ELSEIF(TYP.GE.29 .AND. TYP.LE.31)THEN
          IF(TYP == 29)THEN
            NSENS  = LSENSOR(1,K)
            IF(USERL_AVAIL==1)THEN
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
          ELSEIF(TYP == 30)THEN
            NSENS  = LSENSOR(1,K)
            IF(USERL_AVAIL==1)THEN
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
          ELSEIF(TYP == 31)THEN
            NSENS  = LSENSOR(1,K)
            IF(USERL_AVAIL==1)THEN
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
C
        IF (ACTI == 1) THEN
C#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TSTART
C#include "lockoff.inc"
        ELSEIF(ACTI == 2)THEN
C#include "lockon.inc"
            IF(IMACH /= 3.OR.ISPMD == 0)
     .        WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .    ' SENSOR NUMBER ',NSENS,' DEACTIVATED AT TIME ',TT
C#include "lockoff.inc"
         ENDIF

       ENDDO ! K = 1, NSENSOR
C-----------
      RETURN
      END
