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
      module bcs_mod
        !use constant_mod , only : zero, ep20
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        ! specific to /BCS/WALL
        type bcs_wall_data_
          integer :: size
          integer, dimension(:), allocatable :: elem
          integer, dimension(:), allocatable :: face
          integer, dimension(:), allocatable :: adjacent_elem
        end type bcs_wall_data_

        ! specific to /BCS/WALL
        type bcs_wall_struct_
          logical :: is_enabled = .false.
          logical :: is_depending_on_time = .false.
          logical :: is_depending_on_sensor = .false.
          my_real :: tstart
          my_real :: tstop
          integer :: user_id = 0
          integer :: grnod_id = 0
          integer :: sensor_id = 0
          type (bcs_wall_data_) :: list
        end type bcs_wall_struct_

        !GENERAL DATA STRUCTURE /BCS
        type bcs_struct_
          integer :: num_wall
          type(bcs_wall_struct_),dimension(:),allocatable :: wall
          contains
            procedure :: deallocate
        end type bcs_struct_

! ----------------------------------------------------------------------------------------------------------------------

        type(bcs_struct_) :: bcs

! ----------------------------------------------------------------------------------------------------------------------
        contains
        
 ! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Deallocate related data structure if allocated
      subroutine deallocate(this)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      class(bcs_struct_),intent(inout) :: this
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      if(this%num_wall > 0)then
        do ii=1,this%num_wall
          if(this%wall(ii)%list%size > 0)then
            if(allocated(this%wall(ii)%list%elem)) deallocate(this%wall(ii)%list%elem)
            if(allocated(this%wall(ii)%list%face)) deallocate(this%wall(ii)%list%face)
            if(allocated(this%wall(ii)%list%adjacent_elem)) deallocate(this%wall(ii)%list%adjacent_elem)
          endif
        enddo
        if(allocated(this%wall))deallocate(this%wall)
      endif
! ----------------------------------------------------------------------------------------------------------------------
      return
      end subroutine deallocate
       
        
        
      end module bcs_mod
