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
!! \brief Load buffer from restart file.
!! \details  necessary buffer specific to family of options /ALE/GRID/...
!
      subroutine read_ale_grid()
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use ale_mod , only : ale
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        my_real, dimension(16) :: rtmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

        ! NWALE=7 => /ALE/GRID/FLOW-TRACKING
        !   when starting from a restart file we need to read these value to get thet state from previous cycle
        if(ale%grid%nwale == 7)then
          call read_db(rtmp,16)
          ale%grid%flow_tracking_data%eigenvec(1:3,1) = rtmp(1:3)
          ale%grid%flow_tracking_data%eigenvec(1:3,2) = rtmp(4:6)
          ale%grid%flow_tracking_data%eigenvec(1:3,3) = rtmp(7:9)
          ale%grid%flow_tracking_data%beta0(1:6) = rtmp(10:15)
          ale%grid%flow_tracking_data%ms_elem_mean_0 = rtmp(16)
        endif

! ----------------------------------------------------------------------------------------------------------------------
        return
      end
