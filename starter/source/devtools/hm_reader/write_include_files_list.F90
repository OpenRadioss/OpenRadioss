!||====================================================================
!||    write_include_files_list_mod
!||====================================================================

module write_include_files_list_mod

! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use NAMES_AND_TITLES_MOD, only : NCHARLINE
  use, intrinsic :: iso_c_binding, only : c_char, c_int

! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none

  private
  public :: write_include_files_list

  interface
    subroutine cpp_get_number_of_include_files(is_dyna, num_includes) &
      bind(C, name='cpp_get_number_of_include_files')
      import :: c_int
      integer(c_int), intent(in)  :: is_dyna
      integer(c_int), intent(out) :: num_includes
    end subroutine cpp_get_number_of_include_files

    subroutine cpp_get_include_file_by_index(is_dyna, include_index, file_name, bufsize) &
      bind(C, name='cpp_get_include_file_by_index')
      import :: c_char, c_int
      integer(c_int), intent(in)  :: is_dyna
      integer(c_int), intent(in)  :: include_index
      character(kind=c_char), intent(out) :: file_name(*)
      integer(c_int), intent(in)  :: bufsize
    end subroutine cpp_get_include_file_by_index
  end interface

contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Writes the input deck and its included files to the starter output.
subroutine write_include_files_list(fname, cwd, is_dyna, iout)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  character(len=2048), intent(in) :: fname
  character(len=NCHARLINE), intent(in) :: cwd
  integer(c_int),      intent(in) :: is_dyna
  integer,             intent(in) :: iout

! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
  integer(c_int)             :: bufsize, include_index, num_includes
  character(kind=c_char)     :: c_file_name(512)
  character(len=512)         :: file_name

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
  num_includes = 0
  call cpp_get_number_of_include_files(is_dyna, num_includes)

  if (num_includes <= 0) then
    return
  end if

  write(iout,'(A)') '* INPUT DECK'
  write(iout,'(A)') '************************************************************************'
  write(iout,'(1X)')
  write(iout,'(1X,A)') ' MAIN DECK  : '
  write(iout,'(1X)')
  if (fname(1:1) == '/') then
    write(iout,'(1X,A)') '     '//trim(fname)
  else
    write(iout,'(1X,A)') '     '//trim(cwd)//'/'//trim(fname)
  end if
  write(iout,'(1X)')
  write(iout,'(1X,A,I0,A)') ' INCLUDE FILES USED BY THE DECK (', num_includes, ' file(s)):'
  write(iout,'(1X)')

  bufsize = size(c_file_name, kind=c_int)
  do include_index = 1, num_includes
    c_file_name = ' '
    call cpp_get_include_file_by_index(is_dyna, include_index, c_file_name, bufsize)
    file_name = transfer(c_file_name, file_name)
    write(iout,'(3X,I0,A,A)') include_index, '. ', trim(file_name)
  end do
  write(iout,'(1X)')

end subroutine write_include_files_list

end module write_include_files_list_mod