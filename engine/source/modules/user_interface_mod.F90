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
!! \brief  Module intent to provide common buffers for user interface callback

      !||====================================================================
      !||    user_interface_mod      ../engine/source/modules/user_interface_mod.F90
      !||--- called by ------------------------------------------------------
      !||    get_u_numsens           ../engine/source/user_interface/usensor.F
      !||    get_u_sens              ../engine/source/user_interface/usensor.F
      !||    get_u_sens_acti         ../engine/source/user_interface/usensor.F
      !||    get_u_sens_delay        ../engine/source/user_interface/usensor.F
      !||    get_u_sens_fpar         ../engine/source/user_interface/usensor.F
      !||    get_u_sens_id           ../engine/source/user_interface/usensor.F
      !||    get_u_sens_ipar         ../engine/source/user_interface/usensor.F
      !||    get_u_sens_value        ../engine/source/user_interface/usensor.F
      !||    get_user_window_nodes   ../engine/source/user_interface/userwindow_interface_routines.F
      !||    mat_solid_get_nod_v     ../engine/source/user_interface/uaccess.F
      !||    mat_solid_get_nod_x     ../engine/source/user_interface/uaccess.F
      !||    radioss2                ../engine/source/engine/radioss2.F
      !||    report                  ../engine/source/output/report/report.F
      !||    set_u_sens_acti         ../engine/source/user_interface/usensor.F
      !||    set_u_sens_deacti       ../engine/source/user_interface/usensor.F
      !||    set_u_sens_maxvalue     ../engine/source/user_interface/usensor.F
      !||    set_u_sens_value        ../engine/source/user_interface/usensor.F
      !||    userwindow_get_a        ../engine/source/user_interface/userwindow_interface_routines.F
      !||    userwindow_get_ar       ../engine/source/user_interface/userwindow_interface_routines.F
      !||--- uses       -----------------------------------------------------
      !||    nodal_arrays_mod        ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    sensor_mod              ../common_source/modules/sensor_mod.F90
      !||    user_windows_mod        ../common_source/modules/user_windows_mod.F
      !||====================================================================
      module user_interface_mod
         use user_windows_mod
         use sensor_mod
         USE nodal_arrays_mod
#include "my_real.inc"
!-----------------------------------------------------------------------------------s
!     module dedicated to pass arrays from radioss to user routines.
!-----------------------------------------------------------------------------------
        
        type (user_windows_) :: user_windows
        type (sensors_)      :: sensors
        TYPE(nodal_arrays_), pointer :: user_interface_nodes 
      end module user_interface_mod
