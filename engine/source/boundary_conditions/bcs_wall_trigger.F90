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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Check if sliding wall boundary condition must be enlabled or disabled
!! \details  with law151 (collocated scheme) boundary faces have automatic wall BCs. This option manage
!! \details  internal faces inside the mesh (not on element closure)
!
      subroutine bcs_wall_trigger(time, ale_connectivity, nsensor, sensor_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use bcs_mod , only : bcs
      use ale_connectivity_mod , only : t_ale_connectivity
      use constant_mod , only : ep20
      use sensor_mod , only : sensor_str_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
#include "my_real.inc"
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nsensor                                      !< size for array definition
      type (sensor_str_) ,dimension(nsensor) ,intent(in) :: sensor_tab   !< data structure for sensors
      my_real, intent(in) :: time                                        !< simulation time
      type(t_ale_connectivity), intent(inout) :: ale_connectivity        !< data structure for ale connectivities
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: ilen,ii,id,jj
      my_real :: tstart, tstop
      logical :: is_enabled
      integer :: iad, nb_face, ielem, iface, sensor_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
      if(bcs%num_wall == 0)return
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------                   

      do ii=1,bcs%num_wall

        if(bcs%wall(ii)%is_depending_on_sensor)then
          sensor_id = bcs%wall(ii)%sensor_id
          tstart =  sensor_tab(sensor_id)%tstart
          tstop = sensor_tab(sensor_id)%value
        else
          !depending_on_time
          tstart = bcs%wall(ii)%tstart
          tstop = bcs%wall(ii)%tstop          
        end if

        is_enabled = bcs%wall(ii)%is_enabled
        id = bcs%wall(ii)%user_id

        if(time < tstop)then
          if(time >=tstart .and. .not. is_enabled) then
            !ENABLE BCS_WALL
            bcs%wall(ii)%is_enabled = .true.
            ilen = bcs%wall(ii)%list%size
            !set wall into all elems defined in the list
            do jj=1,ilen
              ielem = bcs%wall(ii)%list%elem(jj)
              iface = bcs%wall(ii)%list%face(jj)
              iad = ale_connectivity%ee_connect%iad_connect(ielem)
              !---set wall to related face
              bcs%wall(ii)%list%adjacent_elem(jj) = ale_connectivity%ee_connect%connected(IAD + iface - 1)
              ale_connectivity%ee_connect%connected(IAD + iface - 1) = 0
            end do
            write (iout ,1000) bcs%wall(ii)%user_id, time
            write (istdo,1000) bcs%wall(ii)%user_id, time
          endif

        else

          if(time >= tstop .and. is_enabled)then
            !DISABLE BCS_WALL
            bcs%wall(ii)%is_enabled = .false.
            ilen = bcs%wall(ii)%list%size
            !remove wall from all elems defined in the list
            do jj=1,ilen
              ielem = bcs%wall(ii)%list%elem(jj)
              iface = bcs%wall(ii)%list%face(jj)
              iad = ale_connectivity%ee_connect%iad_connect(ielem)
              !---remove wall to related face
              ale_connectivity%ee_connect%connected(IAD + iface - 1) = bcs%wall(ii)%list%adjacent_elem(jj)
            end do
            write (iout ,2000) bcs%wall(ii)%user_id, time
            write (istdo,2000) bcs%wall(ii)%user_id, time
          endif
        endif !time

      enddo!next ii

! ----------------------------------------------------------------------------------------------------------------------
      return

1000  FORMAT(' BCS WALL NUMBER ',I10,' ACTIVATED AT TIME ',1PE12.5)
2000  FORMAT(' BCS WALL NUMBER ',I10,' DESACTIVATED AT TIME ',1PE12.5)


      end subroutine bcs_wall_trigger
