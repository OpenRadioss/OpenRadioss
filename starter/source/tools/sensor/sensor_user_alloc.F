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
Chd|  SENSOR_USER_ALLOC             source/tools/sensor/sensor_user_alloc.F
Chd|-- called by -----------
Chd|        READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
        SUBROUTINE SENSOR_USER_ALLOC(ALREADY_DONE)
!$COMMENT
!       SENSOR_USER_ALLOC description
!       allocation of array for user sensor
!       
!       SENSOR_USER_ALLOC organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        LOGICAL, INTENT(inout) :: ALREADY_DONE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: SIZE_LOCAL
!   -------------------------------

        ALREADY_DONE = .TRUE.
        SENSOR_USER_STRUCT%IS_USED = .TRUE.

        SIZE_LOCAL = NUMNOD
        ALLOCATE( SENSOR_USER_STRUCT%NODE_LIST(SIZE_LOCAL) )
        ALLOCATE( SENSOR_USER_STRUCT%NODE_PER_PART_LIST(SIZE_LOCAL) )

        SENSOR_USER_STRUCT%NUMBER_NODE = SIZE_LOCAL
        SENSOR_USER_STRUCT%NUMBER_NODE_PER_PART = SIZE_LOCAL

        SIZE_LOCAL = NPART
        ALLOCATE( SENSOR_USER_STRUCT%PART_LIST(SIZE_LOCAL) )
        SENSOR_USER_STRUCT%NUMBER_PART = SIZE_LOCAL


        RETURN
        END SUBROUTINE SENSOR_USER_ALLOC
