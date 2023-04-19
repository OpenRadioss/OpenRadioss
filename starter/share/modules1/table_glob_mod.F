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
Chd|  TABLE_GLOB_MOD                share/modules1/table_glob_mod.F
Chd|-- called by -----------
Chd|        GET_U_TABLE                   source/user_interface/utable.F
Chd|        GET_U_VTABLE                  source/user_interface/utable.F
Chd|        INIGUSER                      source/system/iniguser.F      
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        GET_U_NUMTABLE                source/user_interface/utable.F
Chd|-- calls ---------------
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      MODULE TABLE_GLOB_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE(TTABLE), DIMENSION(:), ALLOCATABLE :: TABLE
      END MODULE TABLE_GLOB_MOD
