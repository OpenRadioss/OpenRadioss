Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      MODULE MY_ALLOC_MOD
        implicit none
        integer, parameter :: len_error_message = 100
        ! lengths : integer
        private :: my_alloc_real_1d
        private :: my_alloc_real_2d
        private :: my_alloc_real_3d
        private :: my_alloc_integer_1d
        private :: my_alloc_integer_2d
        private :: my_alloc_integer_3d
        private :: my_alloc_double_1d
        private :: my_alloc_double_2d
        private :: my_alloc_double_3d
        private :: my_alloc_preal_1d
        private :: my_alloc_preal_2d
        private :: my_alloc_preal_3d
        private :: my_alloc_pinteger_1d
        private :: my_alloc_pinteger_2d
        private :: my_alloc_pinteger_3d
        private :: my_alloc_pdouble_1d
        private :: my_alloc_pdouble_2d
        private :: my_alloc_pdouble_3d
        !lengths : integer(8)
        private :: my_alloc_8_real_1d
        private :: my_alloc_8_real_2d
        private :: my_alloc_8_real_3d
        private :: my_alloc_8_integer_1d
        private :: my_alloc_8_integer_2d
        private :: my_alloc_8_integer_3d
        private :: my_alloc_8_double_1d
        private :: my_alloc_8_double_2d
        private :: my_alloc_8_double_3d
        private :: my_alloc_8_preal_1d
        private :: my_alloc_8_preal_2d
        private :: my_alloc_8_preal_3d
        private :: my_alloc_8_pinteger_1d
        private :: my_alloc_8_pinteger_2d
        private :: my_alloc_8_pinteger_3d
        private :: my_alloc_8_pdouble_1d
        private :: my_alloc_8_pdouble_2d
        private :: my_alloc_8_pdouble_3d

        public :: my_alloc

        interface my_alloc
          module procedure my_alloc_real_1d
          module procedure my_alloc_real_2d
          module procedure my_alloc_real_3d
          module procedure my_alloc_integer_1d
          module procedure my_alloc_integer_2d
          module procedure my_alloc_integer_3d
          module procedure my_alloc_double_1d
          module procedure my_alloc_double_2d
          module procedure my_alloc_double_3d
          module procedure my_alloc_preal_1d
          module procedure my_alloc_preal_2d
          module procedure my_alloc_preal_3d
          module procedure my_alloc_pinteger_1d
          module procedure my_alloc_pinteger_2d
          module procedure my_alloc_pinteger_3d
          module procedure my_alloc_pdouble_1d
          module procedure my_alloc_pdouble_2d
          module procedure my_alloc_pdouble_3d

          module procedure my_alloc_8_real_1d
          module procedure my_alloc_8_real_2d
          module procedure my_alloc_8_real_3d
          module procedure my_alloc_8_integer_1d
          module procedure my_alloc_8_integer_2d
          module procedure my_alloc_8_integer_3d
          module procedure my_alloc_8_double_1d
          module procedure my_alloc_8_double_2d
          module procedure my_alloc_8_double_3d
          module procedure my_alloc_8_preal_1d
          module procedure my_alloc_8_preal_2d
          module procedure my_alloc_8_preal_3d
          module procedure my_alloc_8_pinteger_1d
          module procedure my_alloc_8_pinteger_2d
          module procedure my_alloc_8_pinteger_3d
          module procedure my_alloc_8_pdouble_1d
          module procedure my_alloc_8_pdouble_2d
          module procedure my_alloc_8_pdouble_3d
        end interface my_alloc

      contains

! ======================================================================================================================
!                                                     TOOLS
! ======================================================================================================================
        function build_msg(str) result(error_message)
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
        end function build_msg

!! \brief Check if the allocation was successful and print an error message if it was noti
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

! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
        subroutine my_alloc_real_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_1d

!! \brief Allocate a 2D array of real numbers
        subroutine my_alloc_real_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_2d

        subroutine my_alloc_real_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
        subroutine my_alloc_double_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_1d

!! \brief Allocate a 2D array of double numbers
        subroutine my_alloc_double_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_2d

        subroutine my_alloc_double_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
        subroutine my_alloc_integer_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_integer_1d

!! \brief Allocate a 2D array of integers
        subroutine my_alloc_integer_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_integer_2d

!! \brief Allocate a 3D array of integers
        subroutine my_alloc_integer_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_integer_3d
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
        subroutine my_alloc_preal_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_1d

!! \brief Allocate a 2D array of real numbers
        subroutine my_alloc_preal_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_2d

        subroutine my_alloc_preal_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
        subroutine my_alloc_pdouble_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_1d

!! \brief Allocate a 2D array of double numbers
        subroutine my_alloc_pdouble_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_2d

        subroutine my_alloc_pdouble_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
        subroutine my_alloc_pinteger_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pinteger_1d

!! \brief Allocate a 2D array of integers
        subroutine my_alloc_pinteger_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pinteger_2d

!! \brief Allocate a 3D array of integers
        subroutine my_alloc_pinteger_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_pinteger_3d

! ======================
! ====================== INTEGER*8
! ======================
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
        subroutine my_alloc_8_real_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_1d

!! \brief Allocate a 2D array of real numbers
        subroutine my_alloc_8_real_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_2d

        subroutine my_alloc_8_real_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
        subroutine my_alloc_8_double_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_1d

!! \brief Allocate a 2D array of double numbers
        subroutine my_alloc_8_double_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_2d

        subroutine my_alloc_8_double_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
        subroutine my_alloc_8_integer_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_integer_1d

!! \brief Allocate a 2D array of integers
        subroutine my_alloc_8_integer_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_integer_2d

!! \brief Allocate a 3D array of integers
        subroutine my_alloc_8_integer_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_8_integer_3d
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
        subroutine my_alloc_8_preal_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_1d

!! \brief Allocate a 2D array of real numbers
        subroutine my_alloc_8_preal_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_2d

        subroutine my_alloc_8_preal_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
        subroutine my_alloc_8_pdouble_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_1d

!! \brief Allocate a 2D array of double numbers
        subroutine my_alloc_8_pdouble_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_2d

        subroutine my_alloc_8_pdouble_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
        subroutine my_alloc_8_pinteger_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pinteger_1d

!! \brief Allocate a 2D array of integers
        subroutine my_alloc_8_pinteger_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pinteger_2d

!! \brief Allocate a 3D array of integers
        subroutine my_alloc_8_pinteger_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_8_pinteger_3d
      END MODULE MY_ALLOC_MOD

