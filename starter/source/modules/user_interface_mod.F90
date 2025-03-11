!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    user_interface_mod          ../starter/source/modules/user_interface_mod.F90
      !||--- called by ------------------------------------------------------
      !||    read_sensor_user            ../starter/source/tools/sensor/read_sensor_user.F
      !||    set_u_sens_fpar             ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_ipar             ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_spmd_node_list   ../starter/source/tools/sensor/set_u_sens_spmd_node_list.F
      !||    set_user_window_nodes       ../starter/source/user_interface/user_windows_tools.F
      !||    starter0                    ../starter/source/starter/starter0.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module user_interface_mod
         use user_windows_mod
         use sensor_mod
#include "my_real.inc"
!-----------------------------------------------------------------------------------
!     module dedicated to pass arrays from radioss to user routines.
!-----------------------------------------------------------------------------------
        type (user_windows_) :: user_windows
        type(sensor_user_struct_) :: sensor_user_struct
        integer :: ksens_cur

      end module user_interface_mod


      ! Need to have sensor_tab in a separate module as long as the array is not embedded in a Type
      ! Otherwise issues with Allocation / Reallocation
      !||====================================================================
      !||    user_sensor_mod   ../starter/source/modules/user_interface_mod.F90
      !||--- called by ------------------------------------------------------
      !||    lectur            ../starter/source/starter/lectur.F
      !||    set_u_sens_fpar   ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_ipar   ../starter/source/user_interface/uaccess.F
      !||    starter0          ../starter/source/starter/starter0.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module user_sensor_mod
         use sensor_mod
#include "my_real.inc"
!-----------------------------------------------------------------------------------
!     MODULE dedicated to pass sensors arrays
!-----------------------------------------------------------------------------------
        type (sensors_)  :: sensors
      end module user_sensor_mod

