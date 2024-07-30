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
      !||====================================================================
      !||    user_interface_mod      ../starter/share/modules1/user_interface_mod.F
      !||--- called by ------------------------------------------------------
      !||    set_u_sens_fpar         ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_ipar         ../starter/source/user_interface/uaccess.F
      !||    set_user_window_nodes   ../starter/source/user_interface/user_windows_tools.F
      !||    starter0                ../starter/source/starter/starter0.F
      !||--- uses       -----------------------------------------------------
      !||    sensor_mod              ../starter/share/modules1/sensor_mod.F
      !||====================================================================
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
      !||====================================================================
      !||    user_sensor_mod   ../starter/share/modules1/user_interface_mod.F
      !||--- called by ------------------------------------------------------
      !||    lectur            ../starter/source/starter/lectur.F
      !||    set_u_sens_fpar   ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_ipar   ../starter/source/user_interface/uaccess.F
      !||    starter0          ../starter/source/starter/starter0.F
      !||--- uses       -----------------------------------------------------
      !||    sensor_mod        ../starter/share/modules1/sensor_mod.F
      !||====================================================================
      MODULE USER_SENSOR_MOD
         USE SENSOR_MOD
#include "my_real.inc"
C-----------------------------------------------------------------------------------
C     MODULE dedicated to pass sensors arrays
C-----------------------------------------------------------------------------------
        TYPE (SENSORS_)  :: SENSORS
      END MODULE USER_SENSOR_MOD

