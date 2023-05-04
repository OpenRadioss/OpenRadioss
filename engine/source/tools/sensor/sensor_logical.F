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
Chd|  SENSOR_LOGICAL                source/tools/sensor/sensor_logical.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|        SENSOR_INIT                   source/tools/sensor/sensor_init.F
Chd|-- calls ---------------
Chd|        SENSOR_AND                    source/tools/sensor/sensor_and.F
Chd|        SENSOR_NOT                    source/tools/sensor/sensor_not.F
Chd|        SENSOR_OR                     source/tools/sensor/sensor_or.F
Chd|        SENSOR_SENS                   source/tools/sensor/sensor_sens.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_LOGICAL(NSENSOR,SENSOR_TAB)
C-----------------------------------------------
C   M a c r o s 
C-----------------------------------------------
#define SENS_SENS 3
#define SENS_AND  4
#define SENS_OR   5 
#define SENS_NOT  8
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), TARGET :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,K,TYP
C=======================================================================
      DO I = 1, LOGICAL_SENSOR_COUNT

         K = LOGICAL_SENSORS_LIST(I)
         TYP     = SENSOR_TAB(K)%TYPE
C--------------------------------
         SELECT CASE(TYP)

         CASE (SENS_SENS) 

                         CALL SENSOR_SENS(NSENSOR,SENSOR_TAB,K)

         CASE (SENS_AND) 

                         CALL SENSOR_AND(NSENSOR,SENSOR_TAB,K)
c
         CASE (SENS_OR)

                         CALL SENSOR_OR(NSENSOR,SENSOR_TAB,K)
c
         CASE (SENS_NOT)

                         CALL SENSOR_NOT(NSENSOR,SENSOR_TAB,K)
         END SELECT
C-----------------------------------------------------------------------
       ENDDO
C-----------
      RETURN
      END
