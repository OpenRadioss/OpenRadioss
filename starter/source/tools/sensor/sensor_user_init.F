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
Chd|  SENSOR_USER_INIT              source/tools/sensor/sensor_user_init.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
        SUBROUTINE SENSOR_USER_INIT()
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
