!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    inivel_mod       ../common_source/modules/inivel_mod.F90
      !||--- called by ------------------------------------------------------
      !||    c_inivell        ../starter/source/restart/ddsplit/c_inivell.F90
      !||    hm_read_inivel   ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    inivel_dt2       ../engine/source/loads/general/inivel/inivel_dt2.F90
      !||    inivel_init      ../engine/source/loads/general/inivel/inivel_init.F90
      !||    inivel_start     ../engine/source/loads/general/inivel/inivel_start.F90
      !||    read_inivel      ../engine/source/output/restart/read_inivel.F90
      !||    w_inivel_str     ../starter/source/restart/ddsplit/w_inivel_str.F90
      !||    write_inivel     ../engine/source/output/restart/write_inivel.F90
      !||====================================================================
      module inivel_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
  ! ======================================================================================================================
  !                                                   TYPES
  ! ======================================================================================================================
  !! \brief module for /INIVEL using T_start or sensor
  
        ! type for /INIVEL
        type general_inivel_
          integer ::  type       !< type 
          integer ::  id         !< user_id
          integer ::  skew_id    !< skew_id
          integer ::  grnd_id    !< grnd_id
          integer ::  sensor_id  !< sensor_id
          my_real ::  VX,VY,VZ   !< VX,VY,VZ
          my_real ::  tstart     !< t_start
        end type  general_inivel_

        ! type for /INIVEL/AXIS
        type axis_inivel_
          integer ::  dir        !< 1:3 x,y,z 
          integer ::  id         !< user_id
          integer ::  frame_id   !< frame_id
          integer ::  grnd_id    !< grnd_id
          integer ::  sensor_id  !< sensor_id
          my_real ::  VX,VY,VZ   !< VX,VY,VZ
          my_real ::  VR         !< VR rotational
          my_real ::  tstart     !< t_start
        end type  axis_inivel_

        ! type for /INIVEL/FVM
        type fvm_inivel_
          integer ::  id         !< user_id
          integer ::  skew_id    !< skew_id
          integer ::  grbric_id  !< grbric_id
          integer ::  grqd_id    !< grqd_id
          integer ::  grtria_id  !< grtria_id
          integer ::  sensor_id  !< sensor_id
          my_real ::  VX,VY,VZ   !< VX,VY,VZ
          my_real ::  tstart     !< t_start
        end type  fvm_inivel_

        ! not available for /INIVEL/NODE 

        ! /INIVEL using T_start or sensor will be inside type loads_
        type inivel_
          integer                    :: id            !< user's id
          integer                    :: itype         !< type 
          type (general_inivel_)     :: general       !< /INIVEL
          type (axis_inivel_)        :: axis          !< /INIVEL/AXIS
          type (fvm_inivel_)         :: fvm           !< /INIVEL/FVM
        end type inivel_
      end module inivel_mod
