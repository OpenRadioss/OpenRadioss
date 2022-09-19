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
Chd|  SET_U_SENS_SPMD_NODE_LIST     source/tools/sensor/set_u_sens_spmd_node_list.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
        INTEGER FUNCTION SET_U_SENS_SPMD_NODE_LIST(ARRAY,SIZE_ARRAY)
!$COMMENT
!       SET_U_SENS_SPMD_NODE_LIST description
!       user function : save in a Radioss buffer
!                       a list of node
!
!       SET_U_SENS_SPMD_NODE_LIST organization :
!           - check if the size of %NODE_LIST is sufficient
!             and reallocate the array if the size is not sufficient
!           - save the node in %NODE_LIST
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: SIZE_ARRAY
        INTEGER, DIMENSION(SIZE_ARRAY), INTENT(in) :: ARRAY
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J
        INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_ARRAY
!   -------------------------------
        J = SENSOR_USER_STRUCT%POINTER_NODE
        DO I=1,SIZE_ARRAY
            J = J + 1
            IF(J>SENSOR_USER_STRUCT%NUMBER_NODE) THEN
                SENSOR_USER_STRUCT%NUMBER_NODE = SENSOR_USER_STRUCT%NUMBER_NODE + NUMNOD
                ALLOCATE( LOCAL_ARRAY(SENSOR_USER_STRUCT%NUMBER_NODE) )

                LOCAL_ARRAY(1:J-1) = SENSOR_USER_STRUCT%NODE_LIST(J-1)

                DEALLOCATE( SENSOR_USER_STRUCT%NODE_LIST )
                ALLOCATE( SENSOR_USER_STRUCT%NODE_LIST( SENSOR_USER_STRUCT%NUMBER_NODE ) )

                SENSOR_USER_STRUCT%NODE_LIST(1:J-1) = LOCAL_ARRAY(1:J-1)
                SENSOR_USER_STRUCT%NODE_LIST(J:SENSOR_USER_STRUCT%NUMBER_NODE) = 0

                DEALLOCATE( LOCAL_ARRAY )
            ENDIF
            SENSOR_USER_STRUCT%NODE_LIST(J) = ARRAY(I)
        ENDDO

        SENSOR_USER_STRUCT%POINTER_NODE = J

        SET_U_SENS_SPMD_NODE_LIST = 0
        RETURN
        END FUNCTION SET_U_SENS_SPMD_NODE_LIST
