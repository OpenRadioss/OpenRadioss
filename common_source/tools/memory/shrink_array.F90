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
      !||    shrink_array_mod              ../common_source/tools/memory/shrink_array.F90
      !||--- called by ------------------------------------------------------
      !||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
      !||    shrink_nodal_arrays           ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    fill_voxel_local              ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    fill_voxel_local_partial      ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    fill_voxel_remote             ../engine/source/interfaces/intsort/fill_voxel.F90
      !||--- calls      -----------------------------------------------------
      !||    build_error_message           ../common_source/tools/memory/shrink_array.F90
      !||====================================================================
      module shrink_array_mod
        implicit none
        integer, parameter :: len_error_message = 100
        private :: build_error_message
        private :: shrink_array_integer_1d
        private :: shrink_array_real_1d
        private :: shrink_array_double_1d
        private :: check_error_and_write
        public :: shrink_array

        !\shrink the array, copy the values
        interface shrink_array
          module procedure shrink_array_integer_1d
          module procedure shrink_array_real_1d
          module procedure shrink_array_double_1d
        end interface shrink_array

      contains

! ======================================================================================================================
!                                                     TOOLS
! ======================================================================================================================
        !||====================================================================
        !||    build_error_message   ../common_source/tools/memory/shrink_array.F90
        !||--- called by ------------------------------------------------------
        !||    shrink_array_mod      ../common_source/tools/memory/shrink_array.F90
        !||====================================================================
        function build_error_message(str) result(error_message)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=*), intent(in) :: str
          character(len=len_error_message) :: error_message
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(len_trim(str) > len_error_message) then
            error_message = str(1:len_error_message)
          else
            error_message = adjustl(str) // repeat(" ", len_error_message - len_trim(str))
          end if
        end function build_error_message

        !||====================================================================
        !||    check_error_and_write         ../common_source/tools/memory/shrink_array.F90
        !||--- called by ------------------------------------------------------
        !||====================================================================
        subroutine check_error_and_write(stat,msg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: stat
          character(len=len_error_message), optional,  intent(in) :: msg

! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if (stat /= 0) then
            write(6, "(a,i10,a)") 'Error in memory allocation'
            if(present(msg)) then
              write(6, "(a)") msg
            endif
            call arret(2)
          end if
        end subroutine check_error_and_write


!! \brief resize a 1D array of integer, copy the values
        !||====================================================================
        !||    shrink_array_integer_1d   ../common_source/tools/memory/shrink_array.F90
        !||--- calls      -----------------------------------------------------
        !||    check_error_and_write     ../common_source/tools/memory/shrink_array.F90
        !||====================================================================
        subroutine shrink_array_integer_1d(a,  newsize, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: newsize !< The new size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
          integer, allocatable :: temp(:)
          integer :: copy_size
          integer :: oldsize
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(allocated(a)) then
            oldsize = size(a)
          else
            oldsize = 0
          endif

          if(newsize < oldsize) then
            allocate(temp(newsize), stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
            if(present(stat)) stat = ierr
            copy_size = newsize
            if(copy_size >0) temp(1:copy_size) = a(1:copy_size)
            call move_alloc(temp, a)
          else if(newsize == oldsize .and. newsize == 0 .and. .not. allocated(a)) then
            allocate(a(1), stat=ierr)
            if(present(stat)) stat = ierr
          endif
        end subroutine shrink_array_integer_1d

        subroutine shrink_array_real_1d(a,  newsize, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: newsize !< The new size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
          real, allocatable :: temp(:)
          integer :: copy_size
          integer :: oldsize
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(allocated(a)) then
            oldsize = size(a)
          else
            oldsize = 0
          endif

          if(newsize < oldsize) then
            allocate(temp(newsize), stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
            if(present(stat)) stat = ierr
            copy_size = newsize
            if(copy_size >0) temp(1:copy_size) = a(1:copy_size)
            call move_alloc(temp, a)
          else if(newsize == oldsize .and. newsize == 0 .and. .not. allocated(a)) then
            allocate(a(1), stat=ierr)
            if(present(stat)) stat = ierr
          endif
        end subroutine shrink_array_real_1d
        subroutine shrink_array_double_1d(a, newsize, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: newsize !< The new size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
          double precision, allocatable :: temp(:)
          integer :: copy_size
          integer :: oldsize
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(allocated(a)) then
            oldsize = size(a)
          else
            oldsize = 0
          endif
          if(newsize < oldsize) then
            allocate(temp(newsize), stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
            if(present(stat)) stat = ierr
            copy_size = newsize
            if(copy_size >0) temp(1:copy_size) = a(1:copy_size)
            call move_alloc(temp, a)
          else if(newsize == oldsize .and. newsize == 0 .and. .not. allocated(a)) then
            allocate(a(1), stat=ierr)
            if(present(stat)) stat = ierr
          endif
        end subroutine shrink_array_double_1d
      end module shrink_array_mod
