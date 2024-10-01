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
      !||    extend_mod                       ../common_source/tools/memory/my_alloc.F
      !||--- called by ------------------------------------------------------
      !||====================================================================
      module extend_array_mod
        implicit none
        integer, parameter :: len_error_message = 100
        private :: build_error_message
        private :: extend_array_integer_1d
        private :: reallocate_array_integer_1d
        private :: check_error_and_write
        public :: extend_array

        !\extend the array, copy the values
        interface extend_array
          module procedure extend_array_integer_1d
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
      !||    build_error_message      ../common_source/tools/memory/extend.F
      !||--- called by ------------------------------------------------------
      !||    execargcheck   ../engine/source/engine/execargcheck.F
      !||    radioss2       ../engine/source/engine/radioss2.F
      !||    starter0       ../starter/source/starter/starter0.F
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
      !||    extend_integer_1d        ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/extend.F
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
