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
      !||    sensor_user_init   ../starter/source/tools/sensor/sensor_user_init.F
      !||--- called by ------------------------------------------------------
      !||    lectur             ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        SUBROUTINE SENSOR_USER_INIT(SENSOR_USER_STRUCT)
!$COMMENT
!       SENSOR_USER_INIT description
!       initialization of size of SENSOR_USER_STRUCT
!       
!       SENSOR_USER_INIT organization :
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
C   A r g u m e n t s
C-----------------------------------------------
      type(sensor_user_struct_), INTENT(inout) :: SENSOR_USER_STRUCT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        SENSOR_USER_STRUCT%IS_USED = .FALSE.

        SENSOR_USER_STRUCT%NUMBER_NODE = 0
        SENSOR_USER_STRUCT%NUMBER_PART = 0
        SENSOR_USER_STRUCT%NUMBER_NODE_PER_PART = 0

        SENSOR_USER_STRUCT%POINTER_NODE = 0
        SENSOR_USER_STRUCT%POINTER_PART = 0
        SENSOR_USER_STRUCT%POINTER_NODE_PER_PART = 0

        RETURN
        END SUBROUTINE SENSOR_USER_INIT
