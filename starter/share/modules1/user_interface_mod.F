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
Chd|  USER_INTERFACE_MOD            share/modules1/user_interface_mod.F
Chd|-- called by -----------
Chd|        SET_USER_WINDOW_NODES         source/user_interface/user_windows_tools.F
Chd|        STARTER0                      source/starter/starter0.F     
Chd|        SET_U_SENS_FPAR               source/user_interface/uaccess.F
Chd|        SET_U_SENS_IPAR               source/user_interface/uaccess.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      MODULE USER_INTERFACE_MOD
         USE USER_WINDOWS_MOD
         USE SENSOR_MOD
#include "my_real.inc"
C-----------------------------------------------------------------------------------
C     MODULE dedicated to pass arrays from RADIOSS to user routines.
C-----------------------------------------------------------------------------------
        TYPE (USER_WINDOWS_)::USER_WINDOWS
      END MODULE USER_INTERFACE_MOD


      ! Need to have sensor_tab in a separate module as long as the array is not embedded in a Type
      ! Otherwise issues with Allocation / Reallocation
Chd|====================================================================
Chd|  USER_SENSOR_MOD               share/modules1/user_interface_mod.F
Chd|-- called by -----------
Chd|        SET_U_SENS_FPAR               source/user_interface/uaccess.F
Chd|        SET_U_SENS_IPAR               source/user_interface/uaccess.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      MODULE USER_SENSOR_MOD
         USE SENSOR_MOD
#include "my_real.inc"
C-----------------------------------------------------------------------------------
C     MODULE dedicated to pass sensors arrays
C-----------------------------------------------------------------------------------
        TYPE (SENSOR_STR_),DIMENSION(:) , ALLOCATABLE :: SENSOR_TAB       ! NSENSOR
      END MODULE USER_SENSOR_MOD

