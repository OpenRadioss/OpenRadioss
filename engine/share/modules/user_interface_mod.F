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
Chd|  USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|-- called by -----------
Chd|        GET_USER_WINDOW_NODES         source/user_interface/userwindow_interface_routines.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        REPORT                        source/output/report/report.F 
Chd|        USERWINDOW_GET_A              source/user_interface/userwindow_interface_routines.F
Chd|        USERWINDOW_GET_AR             source/user_interface/userwindow_interface_routines.F
Chd|        GET_U_NUMSENS                 source/user_interface/usensor.F
Chd|        GET_U_SENS                    source/user_interface/usensor.F
Chd|        GET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|        GET_U_SENS_DELAY              source/user_interface/usensor.F
Chd|        GET_U_SENS_FPAR               source/user_interface/usensor.F
Chd|        GET_U_SENS_ID                 source/user_interface/usensor.F
Chd|        GET_U_SENS_IPAR               source/user_interface/usensor.F
Chd|        GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        SET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|        SET_U_SENS_DEACTI             source/user_interface/usensor.F
Chd|        SET_U_SENS_MAXVALUE           source/user_interface/usensor.F
Chd|        SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      MODULE USER_INTERFACE_MOD
         USE USER_WINDOWS_MOD
         USE SENSOR_MOD
#include "my_real.inc"
C-----------------------------------------------------------------------------------
C     MODULE dedicated to pass arrays from RADIOSS to user routines.
C-----------------------------------------------------------------------------------
        
        TYPE (USER_WINDOWS_) :: USER_WINDOWS
        TYPE (SENSORS_)      :: SENSORS

      END MODULE USER_INTERFACE_MOD
