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
      !||    extend_array_mod              ../common_source/tools/memory/extend_array.F90
      !||--- called by ------------------------------------------------------
      !||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
      !||    extend_nodal_arrays           ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    fill_voxel_local              ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    fill_voxel_local_partial      ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    fill_voxel_remote             ../engine/source/interfaces/intsort/fill_voxel.F90
      !||--- calls      -----------------------------------------------------
      !||    build_error_message           ../common_source/tools/memory/extend_array.F90
      !||====================================================================
      module extend_array_mod
        implicit none
        integer, parameter :: len_error_message = 100
        private :: build_error_message
        private :: extend_array_integer_1d
        private :: extend_array_integer_2d
        private :: extend_array_integer_3d
        private :: extend_array_double_1d
        private :: extend_array_double_2d
        private :: extend_array_double_3d
        private :: extend_array_real_1d
        private :: extend_array_real_2d
        private :: extend_array_real_3d
        private :: reallocate_array_integer_1d
        private :: check_error_and_write
        public :: extend_array

        !\extend the array, copy the values
        interface extend_array
          module procedure extend_array_integer_1d
          module procedure extend_array_integer_2d
          module procedure extend_array_integer_3d
          module procedure extend_array_double_1d
          module procedure extend_array_double_2d
          module procedure extend_array_double_3d
          module procedure extend_array_real_1d
          module procedure extend_array_real_2d
          module procedure extend_array_real_3d
        end interface extend_array
        !\reallocate the array to a larger size if necessary, fill with zeros
        interface reallocate_array
          module procedure reallocate_array_integer_1d 
        end interface reallocate_array

      contains

! ======================================================================================================================
!                                                     TOOLS
! ======================================================================================================================
      !||====================================================================
      !||    build_error_message   ../common_source/tools/memory/extend_array.F90
      !||--- called by ------------------------------------------------------
      !||    extend_array_mod      ../common_source/tools/memory/extend_array.F90
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
      !||    check_error_and_write         ../common_source/tools/memory/extend_array.F90
      !||--- called by ------------------------------------------------------
      !||    extend_array_double_1d        ../common_source/tools/memory/extend_array.F90
      !||    extend_array_double_2d        ../common_source/tools/memory/extend_array.F90
      !||    extend_array_double_3d        ../common_source/tools/memory/extend_array.F90
      !||    extend_array_integer_1d       ../common_source/tools/memory/extend_array.F90
      !||    extend_array_integer_2d       ../common_source/tools/memory/extend_array.F90
      !||    extend_array_integer_3d       ../common_source/tools/memory/extend_array.F90
      !||    extend_array_real_1d          ../common_source/tools/memory/extend_array.F90
      !||    extend_array_real_2d          ../common_source/tools/memory/extend_array.F90
      !||    extend_array_real_3d          ../common_source/tools/memory/extend_array.F90
      !||    my_alloc_8_double_1d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_double_2d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_double_3d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_integer_1d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_integer_2d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_integer_3d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_logical_1d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_logical_2d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_logical_3d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pdouble_1d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pdouble_2d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pdouble_3d         ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pinteger_1d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pinteger_2d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_pinteger_3d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_plogical_1d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_plogical_2d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_plogical_3d        ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_preal_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_preal_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_preal_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_real_1d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_real_2d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_8_real_3d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_double_1d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_double_2d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_double_3d            ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_integer_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_integer_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_integer_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_logical_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_logical_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_logical_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pdouble_1d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pdouble_2d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pdouble_3d           ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pinteger_1d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pinteger_2d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_pinteger_3d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_plogical_1d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_plogical_2d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_plogical_3d          ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_preal_1d             ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_preal_2d             ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_preal_3d             ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_real_1d              ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_real_2d              ../common_source/tools/memory/my_alloc.F90
      !||    my_alloc_real_3d              ../common_source/tools/memory/my_alloc.F90
      !||    reallocate_array_integer_1d   ../common_source/tools/memory/extend_array.F90
      !||    shrink_array_double_1d        ../common_source/tools/memory/shrink_array.F90
      !||    shrink_array_integer_1d       ../common_source/tools/memory/shrink_array.F90
      !||    shrink_array_real_1d          ../common_source/tools/memory/shrink_array.F90
      !||--- calls      -----------------------------------------------------
      !||    arret                         ../engine/source/system/arret.F
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
      !||    extend_array_integer_1d   ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write     ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_integer_1d(a, oldsize, newsize, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: oldsize !< The old size of the array
          integer, intent(in) :: newsize !< The new size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
          integer, allocatable :: temp(:)
          integer :: copy_size
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(newsize > oldsize) then  
            allocate(temp(newsize), stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
            if(present(stat)) stat = ierr
            copy_size = oldsize
            if(copy_size >0) temp(1:copy_size) = a(1:copy_size)
            call move_alloc(temp, a)
          else if(newsize == oldsize .and. newsize == 0 .and. .not. allocated(a)) then
            allocate(a(1), stat=ierr)
            if(present(stat)) stat = ierr
          endif
        end subroutine extend_array_integer_1d

      !||====================================================================
      !||    extend_array_integer_2d   ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write     ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_integer_2d(a, oldsize1, oldsize2, newsize1, newsize2, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, allocatable, dimension(:,:), intent(inout) :: a  !< The allocated 2D array
          integer, intent(in) :: oldsize1  !< The old size of the first dimension
          integer, intent(in) :: oldsize2  !< The old size of the second dimension
          integer, intent(in) :: newsize1  !< The new size of the first dimension
          integer, intent(in) :: newsize2  !< The new size of the second dimension
          character(len=*), optional, intent(in) :: msg  !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat  !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr, i, j
          integer, allocatable :: temp(:,:)
 ! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Check if the array needs to be extended
          if (newsize1 > oldsize1 .or. newsize2 > oldsize2) then
            if (newsize1 == oldsize1) then
              ! Extend only the second dimension (use move_alloc)
              allocate(temp(size(a, 1), newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(msg)) call check_error_and_write(ierr, msg=msg)
                if (present(stat)) stat = ierr
                return
              endif
              ! Copy existing data to the new array
              temp(:, 1:oldsize2) = a(:, 1:oldsize2)
              ! Use move_alloc for efficient reallocation
              call move_alloc(temp, a)
            else
              ! Extend in the first dimension or both dimensions (fallback to temporary array)
              allocate(temp(newsize1, newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(msg)) call check_error_and_write(ierr, msg=msg)
                if (present(stat)) stat = ierr
                return
              endif
              ! Initialize the new array
              temp = 0
              ! Copy existing data to the new array
              do i = 1, min(oldsize1, newsize1)
                do j = 1, min(oldsize2, newsize2)
                  temp(i, j) = a(i, j)
                end do
              end do
              ! Deallocate old array and assign the new array
              deallocate(a, stat=ierr)
              if (ierr /= 0) then
                if (present(stat)) stat = ierr
                return
              endif
              allocate(a(newsize1, newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(stat)) stat = ierr
                return
              endif
              a = temp
              deallocate(temp, stat=ierr)
            endif
            else if (newsize1 == oldsize1 .and. newsize2 == oldsize2 .and. &
                 newsize1 == 0 .and. newsize2 == 0 .and. .not. allocated(a)) then
            ! Special case for unallocated arrays
            allocate(a(1, 1), stat=ierr)
            if (present(stat)) stat = ierr
          endif
        
          ! Set the status to success if no errors occurred
          if (present(stat)) stat = 0
        
        end subroutine extend_array_integer_2d

      !||====================================================================
      !||    extend_array_integer_3d   ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write     ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_integer_3d(a, oldsize1, oldsize2, oldsize3, newsize1, newsize2, newsize3, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, allocatable, dimension(:,:,:) :: a  !< The allocated 3D array
        integer, intent(in) :: oldsize1  !< The old size of the first dimension
        integer, intent(in) :: oldsize2  !< The old size of the second dimension
        integer, intent(in) :: oldsize3  !< The old size of the third dimension
        integer, intent(in) :: newsize1  !< The new size of the first dimension
        integer, intent(in) :: newsize2  !< The new size of the second dimension
        integer, intent(in) :: newsize3  !< The new size of the third dimension
        character(len=*), optional, intent(in) :: msg  !< The error message to print if the allocation fails
        integer, optional, intent(out) :: stat  !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: ierr, i, j, k
        integer, allocatable :: temp(:,:,:)
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        ! Check if the array needs to be extended
        if (newsize1 > oldsize1 .or. newsize2 > oldsize2 .or. newsize3 > oldsize3) then
      
          if (newsize1 == oldsize1 .and. newsize2 == oldsize2) then
            ! Extend only the third dimension (use move_alloc)
            allocate(temp(size(a, 1), size(a, 2), newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(msg)) call check_error_and_write(ierr, msg=msg)
              if (present(stat)) stat = ierr
              return
            endif
            temp(:, :, 1:oldsize3) = a(:, :, 1:oldsize3)
            call move_alloc(temp, a)
          else
            allocate(temp(newsize1, newsize2, newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(msg)) call check_error_and_write(ierr, msg=msg)
              if (present(stat)) stat = ierr
              return
            endif
            temp = 0
            ! Copy existing data to the new array
            do i = 1, min(oldsize1, newsize1)
              do j = 1, min(oldsize2, newsize2)
                do k = 1, min(oldsize3, newsize3)
                  temp(i, j, k) = a(i, j, k)
                end do
              end do
            end do
            ! Deallocate old array and assign the new array
            deallocate(a, stat=ierr)
            if (ierr /= 0) then
              if (present(stat)) stat = ierr
              return
            endif
            allocate(a(newsize1, newsize2, newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(stat)) stat = ierr
              return
            endif
            a = temp
            deallocate(temp, stat=ierr)
          endif
        else if (newsize1 == oldsize1 .and. newsize2 == oldsize2 .and. newsize3 == oldsize3 .and. &
           newsize1 == 0 .and. newsize2 == 0 .and. newsize3 == 0 .and. .not. allocated(a)) then
          ! Special case for unallocated arrays
          allocate(a(1, 1, 1), stat=ierr)
          if (present(stat)) stat = ierr
        endif
        ! Set the status to success if no errors occurred
        if (present(stat)) stat = 0
      end subroutine extend_array_integer_3d

      !||====================================================================
      !||    extend_array_real_1d    ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_real_1d(a, oldsize, newsize, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: oldsize !< The old size of the array
          integer, intent(in) :: newsize !< The new size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
          real, allocatable :: temp(:)
          integer :: copy_size
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(newsize > oldsize) then  
            allocate(temp(newsize), stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
            if(present(stat)) stat = ierr
            copy_size = oldsize
            if(copy_size >0) temp(1:copy_size) = a(1:copy_size)
            call move_alloc(temp, a)
          else if(newsize == oldsize .and. newsize == 0 .and. .not. allocated(a)) then
            allocate(a(1), stat=ierr)
            if(present(stat)) stat = ierr
          endif
        end subroutine extend_array_real_1d

      !||====================================================================
      !||    extend_array_real_2d    ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_real_2d(a, oldsize1, oldsize2, newsize1, newsize2, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, allocatable, dimension(:,:), intent(inout) :: a  !< The allocated 2D array
          integer, intent(in) :: oldsize1  !< The old size of the first dimension
          integer, intent(in) :: oldsize2  !< The old size of the second dimension
          integer, intent(in) :: newsize1  !< The new size of the first dimension
          integer, intent(in) :: newsize2  !< The new size of the second dimension
          character(len=*), optional, intent(in) :: msg  !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat  !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr, i, j
          real, allocatable :: temp(:,:)
 ! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Check if the array needs to be extended
          if (newsize1 > oldsize1 .or. newsize2 > oldsize2) then
            if (newsize1 == oldsize1) then
              ! Extend only the second dimension (use move_alloc)
              allocate(temp(size(a, 1), newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(msg)) call check_error_and_write(ierr, msg=msg)
                if (present(stat)) stat = ierr
                return
              endif
              ! Copy existing data to the new array
              temp(:, 1:oldsize2) = a(:, 1:oldsize2)
              ! Use move_alloc for efficient reallocation
              call move_alloc(temp, a)
            else
              ! Extend in the first dimension or both dimensions (fallback to temporary array)
              allocate(temp(newsize1, newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(msg)) call check_error_and_write(ierr, msg=msg)
                if (present(stat)) stat = ierr
                return
              endif
              ! Initialize the new array
              temp = 0
              ! Copy existing data to the new array
              do i = 1, min(oldsize1, newsize1)
                do j = 1, min(oldsize2, newsize2)
                  temp(i, j) = a(i, j)
                end do
              end do
              ! Deallocate old array and assign the new array
              deallocate(a, stat=ierr)
              if (ierr /= 0) then
                if (present(stat)) stat = ierr
                return
              endif
              allocate(a(newsize1, newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(stat)) stat = ierr
                return
              endif
              a = temp
              deallocate(temp, stat=ierr)
            endif
            else if (newsize1 == oldsize1 .and. newsize2 == oldsize2 .and. &
                 newsize1 == 0 .and. newsize2 == 0 .and. .not. allocated(a)) then
            ! Special case for unallocated arrays
            allocate(a(1, 1), stat=ierr)
            if (present(stat)) stat = ierr
          endif
        
          ! Set the status to success if no errors occurred
          if (present(stat)) stat = 0
        
        end subroutine extend_array_real_2d

      !||====================================================================
      !||    extend_array_real_3d    ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_real_3d(a, oldsize1, oldsize2, oldsize3, newsize1, newsize2, newsize3, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        real, allocatable, dimension(:,:,:) :: a  !< The allocated 3D array
        integer, intent(in) :: oldsize1  !< The old size of the first dimension
        integer, intent(in) :: oldsize2  !< The old size of the second dimension
        integer, intent(in) :: oldsize3  !< The old size of the third dimension
        integer, intent(in) :: newsize1  !< The new size of the first dimension
        integer, intent(in) :: newsize2  !< The new size of the second dimension
        integer, intent(in) :: newsize3  !< The new size of the third dimension
        character(len=*), optional, intent(in) :: msg  !< The error message to print if the allocation fails
        integer, optional, intent(out) :: stat  !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: ierr, i, j, k
        real, allocatable :: temp(:,:,:)
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        ! Check if the array needs to be extended
        if (newsize1 > oldsize1 .or. newsize2 > oldsize2 .or. newsize3 > oldsize3) then
      
          if (newsize1 == oldsize1 .and. newsize2 == oldsize2) then
            ! Extend only the third dimension (use move_alloc)
            allocate(temp(size(a, 1), size(a, 2), newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(msg)) call check_error_and_write(ierr, msg=msg)
              if (present(stat)) stat = ierr
              return
            endif
            temp(:, :, 1:oldsize3) = a(:, :, 1:oldsize3)
            call move_alloc(temp, a)
          else
            allocate(temp(newsize1, newsize2, newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(msg)) call check_error_and_write(ierr, msg=msg)
              if (present(stat)) stat = ierr
              return
            endif
            temp = 0
            ! Copy existing data to the new array
            do i = 1, min(oldsize1, newsize1)
              do j = 1, min(oldsize2, newsize2)
                do k = 1, min(oldsize3, newsize3)
                  temp(i, j, k) = a(i, j, k)
                end do
              end do
            end do
            ! Deallocate old array and assign the new array
            deallocate(a, stat=ierr)
            if (ierr /= 0) then
              if (present(stat)) stat = ierr
              return
            endif
            allocate(a(newsize1, newsize2, newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(stat)) stat = ierr
              return
            endif
            a = temp
            deallocate(temp, stat=ierr)
          endif
        else if (newsize1 == oldsize1 .and. newsize2 == oldsize2 .and. newsize3 == oldsize3 .and. &
           newsize1 == 0 .and. newsize2 == 0 .and. newsize3 == 0 .and. .not. allocated(a)) then
          ! Special case for unallocated arrays
          allocate(a(1, 1, 1), stat=ierr)
          if (present(stat)) stat = ierr
        endif
        ! Set the status to success if no errors occurred
        if (present(stat)) stat = 0
      end subroutine extend_array_real_3d



      !||====================================================================
      !||    extend_array_double_1d   ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_double_1d(a, oldsize, newsize, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: oldsize !< The old size of the array
          integer, intent(in) :: newsize !< The new size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
          double precision, allocatable :: temp(:)
          integer :: copy_size
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(newsize > oldsize) then  
            allocate(temp(newsize), stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
            if(present(stat)) stat = ierr
            copy_size = oldsize
            if(copy_size >0) temp(1:copy_size) = a(1:copy_size)
            call move_alloc(temp, a)
          else if(newsize == oldsize .and. newsize == 0 .and. .not. allocated(a)) then
            allocate(a(1), stat=ierr)
            if(present(stat)) stat = ierr
          endif
        end subroutine extend_array_double_1d

      !||====================================================================
      !||    extend_array_double_2d   ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_double_2d(a, oldsize1, oldsize2, newsize1, newsize2, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, allocatable, dimension(:,:), intent(inout) :: a  !< The allocated 2D array
          integer, intent(in) :: oldsize1  !< The old size of the first dimension
          integer, intent(in) :: oldsize2  !< The old size of the second dimension
          integer, intent(in) :: newsize1  !< The new size of the first dimension
          integer, intent(in) :: newsize2  !< The new size of the second dimension
          character(len=*), optional, intent(in) :: msg  !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat  !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr, i, j
          double precision, allocatable :: temp(:,:)
 ! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          ! Check if the array needs to be extended
          if (newsize1 > oldsize1 .or. newsize2 > oldsize2) then
            if (newsize1 == oldsize1) then
              ! Extend only the second dimension (use move_alloc)
              allocate(temp(size(a, 1), newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(msg)) call check_error_and_write(ierr, msg=msg)
                if (present(stat)) stat = ierr
                return
              endif
              ! Copy existing data to the new array
              temp(:, 1:oldsize2) = a(:, 1:oldsize2)
              ! Use move_alloc for efficient reallocation
              call move_alloc(temp, a)
            else
              ! Extend in the first dimension or both dimensions (fallback to temporary array)
              allocate(temp(newsize1, newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(msg)) call check_error_and_write(ierr, msg=msg)
                if (present(stat)) stat = ierr
                return
              endif
              ! Initialize the new array
              temp = 0
              ! Copy existing data to the new array
              do i = 1, min(oldsize1, newsize1)
                do j = 1, min(oldsize2, newsize2)
                  temp(i, j) = a(i, j)
                end do
              end do
              ! Deallocate old array and assign the new array
              deallocate(a, stat=ierr)
              if (ierr /= 0) then
                if (present(stat)) stat = ierr
                return
              endif
              allocate(a(newsize1, newsize2), stat=ierr)
              if (ierr /= 0) then
                if (present(stat)) stat = ierr
                return
              endif
              a = temp
              deallocate(temp, stat=ierr)
            endif
            else if (newsize1 == oldsize1 .and. newsize2 == oldsize2 .and. &
                 newsize1 == 0 .and. newsize2 == 0 .and. .not. allocated(a)) then
            ! Special case for unallocated arrays
            allocate(a(1, 1), stat=ierr)
            if (present(stat)) stat = ierr
          endif
        
          ! Set the status to success if no errors occurred
          if (present(stat)) stat = 0
        
        end subroutine extend_array_double_2d

      !||====================================================================
      !||    extend_array_double_3d   ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine extend_array_double_3d(a, oldsize1, oldsize2, oldsize3, newsize1, newsize2, newsize3, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
        double precision, allocatable, dimension(:,:,:) :: a  !< The allocated 3D array
        integer, intent(in) :: oldsize1  !< The old size of the first dimension
        integer, intent(in) :: oldsize2  !< The old size of the second dimension
        integer, intent(in) :: oldsize3  !< The old size of the third dimension
        integer, intent(in) :: newsize1  !< The new size of the first dimension
        integer, intent(in) :: newsize2  !< The new size of the second dimension
        integer, intent(in) :: newsize3  !< The new size of the third dimension
        character(len=*), optional, intent(in) :: msg  !< The error message to print if the allocation fails
        integer, optional, intent(out) :: stat  !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: ierr, i, j, k
        double precision, allocatable :: temp(:,:,:)
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
        ! Check if the array needs to be extended
        if (newsize1 > oldsize1 .or. newsize2 > oldsize2 .or. newsize3 > oldsize3) then
      
          if (newsize1 == oldsize1 .and. newsize2 == oldsize2) then
            ! Extend only the third dimension (use move_alloc)
            allocate(temp(size(a, 1), size(a, 2), newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(msg)) call check_error_and_write(ierr, msg=msg)
              if (present(stat)) stat = ierr
              return
            endif
            temp(:, :, 1:oldsize3) = a(:, :, 1:oldsize3)
            call move_alloc(temp, a)
          else
            allocate(temp(newsize1, newsize2, newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(msg)) call check_error_and_write(ierr, msg=msg)
              if (present(stat)) stat = ierr
              return
            endif
            temp = 0
            ! Copy existing data to the new array
            do i = 1, min(oldsize1, newsize1)
              do j = 1, min(oldsize2, newsize2)
                do k = 1, min(oldsize3, newsize3)
                  temp(i, j, k) = a(i, j, k)
                end do
              end do
            end do
            ! Deallocate old array and assign the new array
            deallocate(a, stat=ierr)
            if (ierr /= 0) then
              if (present(stat)) stat = ierr
              return
            endif
            allocate(a(newsize1, newsize2, newsize3), stat=ierr)
            if (ierr /= 0) then
              if (present(stat)) stat = ierr
              return
            endif
            a = temp
            deallocate(temp, stat=ierr)
          endif
        else if (newsize1 == oldsize1 .and. newsize2 == oldsize2 .and. newsize3 == oldsize3 .and. &
           newsize1 == 0 .and. newsize2 == 0 .and. newsize3 == 0 .and. .not. allocated(a)) then
          ! Special case for unallocated arrays
          allocate(a(1, 1, 1), stat=ierr)
          if (present(stat)) stat = ierr
        endif
        ! Set the status to success if no errors occurred
        if (present(stat)) stat = 0
      end subroutine extend_array_double_3d


      !||====================================================================
      !||    reallocate_array_integer_1d   ../common_source/tools/memory/extend_array.F90
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write         ../common_source/tools/memory/extend_array.F90
      !||====================================================================
        subroutine reallocate_array_integer_1d(a, newsize, msg, stat)
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
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          ! if the newsize is smaller than the old size, we do nothing except filling with zeros
          if(newsize > size(a)) then
            ierr = 0
            if(allocated(a)) deallocate(a, stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
            allocate(a(newsize), stat=ierr)
            if(.not. present(stat)) then
              if(present(msg)) then
                call check_error_and_write(ierr, msg=msg)
              else
                call check_error_and_write(ierr)
              end if
            endif
          endif
          if(newsize > 0) a(1:newsize) = 0
        end subroutine reallocate_array_integer_1d 


      end module extend_array_mod
