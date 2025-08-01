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
!||====================================================================
!||    sort_logical_sensors   ../starter/source/tools/sensor/sort_logical_sensors.F
!||--- called by ------------------------------------------------------
!||    inisen                 ../starter/source/tools/sensor/inisen.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE SORT_LOGICAL_SENSORS(SENSORS)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Sort AND, OR NOT sensors with their dependencies
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C    SENSOR_TAB     Sensor Structure
C===========================================================================================
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
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SENSOR_MOD
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
      TYPE (SENSORS_),INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      INTEGER SENS_TYPE,SENS_COUNT,IERROR
      INTEGER DEP_LIST(2),NUM_ITEM
      INTEGER,DIMENSION(:),ALLOCATABLE :: SORTED_SENSORS
C-----------------------------------------------
      SENSORS%LOGICAL_SENSOR_COUNT = 0

      ALLOCATE(SORTED_SENSORS(SENSORS%NSENSOR))

      DO I=1,SENSORS%NSENSOR

         SELECT CASE(SENSORS%SENSOR_TAB(I)%TYPE)

            CASE ( SENS_SENS )
                       DEP_LIST(1) = SENSORS%SENSOR_TAB(I)%IPARAM(1)
                       DEP_LIST(2) = SENSORS%SENSOR_TAB(I)%IPARAM(2)

                       IF(DEP_LIST(2) == 0 )THEN               ! /SENSOR/SENS can have SENSOR2 non existent.
                           NUM_ITEM = 1 
                       ELSE
                           NUM_ITEM = 2 
                       ENDIF

                       CALL SET_GRAPH_ADD_SET(I, DEP_LIST,NUM_ITEM)
                       SENSORS%LOGICAL_SENSOR_COUNT = SENSORS%LOGICAL_SENSOR_COUNT +1
                       ! print*,I,'-- SENS --',SENSOR_TAB(I)%IPARAM(1),SENSOR_TAB(I)%IPARAM(2)

            CASE ( SENS_AND )
                       DEP_LIST(1) = SENSORS%SENSOR_TAB(I)%IPARAM(1)
                       DEP_LIST(2) = SENSORS%SENSOR_TAB(I)%IPARAM(2)
                       NUM_ITEM = 2 
                       CALL SET_GRAPH_ADD_SET(I, DEP_LIST,NUM_ITEM)
                       SENSORS%LOGICAL_SENSOR_COUNT = SENSORS%LOGICAL_SENSOR_COUNT +1
                       ! print*,I,'-- AND --',SENSOR_TAB(I)%IPARAM(1),SENSOR_TAB(I)%IPARAM(2)

            CASE ( SENS_OR )
                       DEP_LIST(1) = SENSORS%SENSOR_TAB(I)%IPARAM(1)
                       DEP_LIST(2) = SENSORS%SENSOR_TAB(I)%IPARAM(2)
                       NUM_ITEM = 2 
                       CALL SET_GRAPH_ADD_SET(I, DEP_LIST,NUM_ITEM)
                       SENSORS%LOGICAL_SENSOR_COUNT = SENSORS%LOGICAL_SENSOR_COUNT +1
                       ! print*,I,'--  OR --',SENSOR_TAB(I)%IPARAM(1),SENSOR_TAB(I)%IPARAM(2)

            CASE ( SENS_NOT )
                       DEP_LIST(1) = SENSORS%SENSOR_TAB(I)%IPARAM(1)
                       NUM_ITEM = 1
                       CALL SET_GRAPH_ADD_SET(I, DEP_LIST,NUM_ITEM)
                       SENSORS%LOGICAL_SENSOR_COUNT = SENSORS%LOGICAL_SENSOR_COUNT +1
                       ! print*,I,'-- NOT --',SENSOR_TAB(I)%IPARAM(1)

           CASE DEFAULT
                       DEP_LIST(1:2)=0
                       NUM_ITEM = 0
                       CALL SET_GRAPH_ADD_SET(I, DEP_LIST,NUM_ITEM)
                       ! print*,I,'OTHER'
         END SELECT     
      ENDDO

      CALL SET_GRAPH_SORT(SORTED_SENSORS, IERROR)
      CALL SET_GRAPH_CLEAN()

      ! When IERROR is negative, a Sensor has a circular dependency.
      ! IERROR is set to -ID of Sensor.
      ! -------------------------------------------------------------
      IF (IERROR < 0)THEN
         CALL ANCMSG(MSGID=2095,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=-IERROR)
      ENDIF

      ALLOCATE (SENSORS%LOGICAL_SENSORS_LIST(SENSORS%LOGICAL_SENSOR_COUNT))
      SENS_COUNT=0


      ! Create index table with logical sensors only
      ! ---------------------------------------------
      DO I=1,SENSORS%NSENSOR
        J = SORTED_SENSORS(I)
        SENS_TYPE = SENSORS%SENSOR_TAB(J)%TYPE

        IF( SENS_TYPE == SENS_SENS .OR. SENS_TYPE == SENS_AND .OR. 
     *      SENS_TYPE == SENS_OR   .OR. SENS_TYPE == SENS_NOT ) THEN

            SENS_COUNT = SENS_COUNT + 1
            SENSORS%LOGICAL_SENSORS_LIST(SENS_COUNT) = J
            ! print*,'>',LOGICAL_SENSORS_LIST(SENS_COUNT),'-',SENSOR_TAB(J)%SENS_ID
        ENDIF
      ENDDO


      DEALLOCATE(SORTED_SENSORS)

      END


