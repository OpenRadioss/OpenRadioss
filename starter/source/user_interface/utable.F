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
Chd|  ST_UTABLE_DUM                 source/user_interface/utable.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ST_UTABLE_DUM(IERR)
#include      "implicit_f.inc"
      INTEGER IERR
      IERR=0
      END      
Chd|====================================================================
Chd|  GET_U_NUMTABLE                source/user_interface/utable.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TABLE_GLOB_MOD                share/modules1/table_glob_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      INTEGER FUNCTION GET_U_NUMTABLE(TID) 
      USE TABLE_GLOB_MOD
      USE TABLE_MOD
#include      "implicit_f.inc"
#include      "com04_c.inc"         
      TYPE(TTABLE) CUR_TABLE
      INTEGER I,TID
C need NTABLE
C
      GET_U_NUMTABLE = 0
      DO I=1,NTABLE
        IF(TABLE(I)%NOTABLE==TID)THEN
          GET_U_NUMTABLE = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_TABLE                   source/user_interface/utable.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules1/table_mod.F    
Chd|        TABLE_GLOB_MOD                share/modules1/table_glob_mod.F
Chd|====================================================================
      SUBROUTINE GET_U_TABLE(ITABLE,XX,YY) 
      USE TABLE_GLOB_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      INTEGER ITABLE
      my_real, 
     .       DIMENSION(:) :: XX
      my_real
     .       YY
C
      CALL TABLE_INTERP(TABLE(ITABLE),XX,YY)
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_VTABLE                  source/user_interface/utable.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules1/table_mod.F    
Chd|        TABLE_GLOB_MOD                share/modules1/table_glob_mod.F
Chd|====================================================================
      SUBROUTINE GET_U_VTABLE(ITABLE,NEL0,IPOS,XX,YY,DYDX1) 
      USE TABLE_GLOB_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITABLE,NEL0
      INTEGER, DIMENSION(:,:) :: IPOS
      my_real, 
     .       DIMENSION(:,:) :: XX
      my_real
     .       YY(*), DYDX1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      CALL TABLE_VINTERP(TABLE(ITABLE),NEL0,NEL0,IPOS,XX,YY,DYDX1)
      RETURN
      END
