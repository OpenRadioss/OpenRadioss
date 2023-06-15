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
Chd|  INIPARSEN                     source/tools/sensor/iniparsen.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      SUBROUTINE INIPARSEN(SENSORS,NSECT,NINTER,NINTSUB,NRWALL,NRBODY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE MESSAGE_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
c#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NSECT,NINTER,NINTSUB,NRWALL,NRBODY
      TYPE (SENSORS_) ,INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NOSYS,ISECT,IINT,IRWAL,IRBOD,COMPTEUR,SENS_TYPE,
     .   SECT_ID,INT_ID,SUB_ID,RWAL_ID,RBOD_ID
      INTEGER, DIMENSION(0:NSECT+NINTER+NINTSUB+NRWALL+NRBODY) :: TABLOCAL
C-----------------------------------------------
C     Special treatment of sensors with Parith/on (type 6,11,12,13)
C     Tabsensor(i) pointe vers les interfaces/sections/rwalls/rbodies concernees 
C     par les sensors type force
c=======================================================================
      TABLOCAL(:) = 0
c
      DO I=1,SENSORS%NSENSOR
        SENS_TYPE = SENSORS%SENSOR_TAB(I)%TYPE
        IF (SENS_TYPE == 12 ) THEN         ! sensort type section
          ISECT = SENSORS%SENSOR_TAB(I)%IPARAM(1)
          TABLOCAL(ISECT) = I
        ELSEIF (SENS_TYPE == 6) THEN       ! sensort type interface
          INT_ID = SENSORS%SENSOR_TAB(I)%IPARAM(1)
          IINT   = INT_ID + NSECT
          TABLOCAL(IINT) = I
        ELSEIF (SENS_TYPE == 7) THEN       ! sensort type rwall
          RWAL_ID = SENSORS%SENSOR_TAB(I)%IPARAM(1)
          IRWAL   = RWAL_ID + NSECT + NINTER+ NINTSUB
          TABLOCAL(IRWAL) = I
        ELSEIF (SENS_TYPE == 11) THEN      ! sensort type rbody
          RBOD_ID = SENSORS%SENSOR_TAB(I)%IPARAM(1)
          IRBOD   = RBOD_ID + NSECT + NINTER+ NINTSUB + NRWALL
          TABLOCAL(IRBOD) = I
        ELSEIF (SENS_TYPE == 13) THEN      ! sensort type work
          SECT_ID = SENSORS%SENSOR_TAB(I)%IPARAM(3)
          INT_ID  = SENSORS%SENSOR_TAB(I)%IPARAM(4)
          SUB_ID  = SENSORS%SENSOR_TAB(I)%IPARAM(5)
          RWAL_ID = SENSORS%SENSOR_TAB(I)%IPARAM(6)
          RBOD_ID = SENSORS%SENSOR_TAB(I)%IPARAM(7)
        
          ISECT = SECT_ID
          IF (ISECT > 0) TABLOCAL(ISECT) = I
c
          IINT = INT_ID + NSECT
          IF (INT_ID > 0) TABLOCAL(IINT)=I     ! interface
          IINT = SUB_ID + NSECT
          IF (SUB_ID > 0) TABLOCAL(IINT)=I     ! sub-interface
c
          IRWAL = RWAL_ID + NSECT + NINTER+ NINTSUB
          IF (RWAL_ID > 0) TABLOCAL(IRWAL)=I 
c
          IRBOD = RBOD_ID + NSECT + NINTER+ NINTSUB + NRWALL
          IF (RBOD_ID > 0) TABLOCAL(IRBOD)=I 
        ENDIF
      ENDDO
c------------
      NOSYS=1      ! No systeme section,interface,rwall, rbody
      DO WHILE (TABLOCAL(NOSYS) == 0)
        NOSYS=NOSYS+1
      ENDDO
C
      DO I=1,NOSYS
        SENSORS%TABSENSOR(I) = TABLOCAL(NOSYS)
      ENDDO
C
      COMPTEUR=1
      DO I = NOSYS, NSECT+NINTER+NINTSUB+NRWALL+NRBODY
        IF (TABLOCAL(I)==0) THEN
          SENSORS%TABSENSOR(I+1) = SENSORS%TABSENSOR(I)
        ELSE
          SENSORS%TABSENSOR(I+1) = SENSORS%TABSENSOR(I) + COMPTEUR
          COMPTEUR=COMPTEUR+1
        ENDIF
      ENDDO
C------------
      RETURN
      END 
