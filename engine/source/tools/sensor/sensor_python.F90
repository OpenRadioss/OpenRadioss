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
      !||    sensor_python_mod   ../engine/source/tools/sensor/sensor_python.F90
      !||--- called by ------------------------------------------------------
      !||    sensor_base         ../engine/source/tools/sensor/sensor_base.F
      !||    sensor_init         ../engine/source/tools/sensor/sensor_init.F
      !||====================================================================
      module sensor_python_mod 
      contains
      !||====================================================================
      !||    sensor_python                     ../engine/source/tools/sensor/sensor_python.F90
      !||--- called by ------------------------------------------------------
      !||    sensor_base                       ../engine/source/tools/sensor/sensor_base.F
      !||    sensor_init                       ../engine/source/tools/sensor/sensor_init.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                      ../common_source/modules/constant_mod.F
      !||    python_funct_mod                  ../common_source/modules/python_mod.F90
      !||    sensor_mod                        ../common_source/modules/sensor_mod.F90
      !||====================================================================
      subroutine sensor_python(sensor)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
      use python_funct_mod
      use sensor_mod
      use constant_mod              
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
      type (sensor_str_) :: sensor
! ----------------------------------------------------------------------------------------------------------------------
!                                                     local variables
! ----------------------------------------------------------------------------------------------------------------------
      double precision :: y
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
!$OMP CRITICAL
        call python_call_function_with_state(sensor%python_function%name,y)
!$OMP END CRITICAL
        if(y >= half) then
          sensor%status = 1
        else if(y < half) then
          sensor%status = 0
        end if
      return
      end subroutine sensor_python
      end module sensor_python_mod
