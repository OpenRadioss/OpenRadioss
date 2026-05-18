!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    my_alloc_tools_mod   ../common_source/tools/memory/my_alloc_tools.F90
!||--- calls      -----------------------------------------------------
!||    arret                ../engine/source/system/arret.F
!||--- called by ------------------------------------------------------
!||    my_alloc_impl_idx4_mod  ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_impl_idx8_mod  ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||====================================================================

!! \brief C interfaces, error checking, and address-tracking utilities for my_alloc.
!! \details Separated from the generated allocation routines so that my_alloc_tools_mod
!!          can be compiled once and shared by all generated sub-modules, keeping each
!!          generated file within a manageable size for the compiler.
      module my_alloc_tools_mod
        use iso_c_binding, only : c_char, c_int, c_int64_t, c_ptr
        implicit none

        integer, parameter :: len_error_message = 100  !< Maximum length of allocation error messages

! ----------------------------------------------------------------------------------------------------------------------
!                                          C INTERFACE DECLARATIONS
! ----------------------------------------------------------------------------------------------------------------------
        interface
          subroutine cpp_record_alloc(msg, msg_len, nbytes) bind(C, name="cpp_record_alloc")
            import :: c_char, c_int, c_int64_t
            character(kind=c_char), intent(in) :: msg(*)
            integer(c_int),         intent(in) :: msg_len
            integer(c_int64_t),     intent(in) :: nbytes
          end subroutine cpp_record_alloc

          subroutine cpp_record_alloc_addr(addr, msg, msg_len, nbytes) bind(C, name="cpp_record_alloc_addr")
            import :: c_ptr, c_char, c_int, c_int64_t
            type(c_ptr),        value, intent(in) :: addr
            character(kind=c_char),    intent(in) :: msg(*)
            integer(c_int),            intent(in) :: msg_len
            integer(c_int64_t),        intent(in) :: nbytes
          end subroutine cpp_record_alloc_addr

          subroutine cpp_record_dealloc_addr(addr) bind(C, name="cpp_record_dealloc_addr")
            import :: c_ptr
            type(c_ptr), value, intent(in) :: addr
          end subroutine cpp_record_dealloc_addr

          subroutine cpp_print_alloc_report() bind(C, name="cpp_print_alloc_report")
          end subroutine cpp_print_alloc_report
        end interface

        public  :: my_alloc_check, record_alloc_addr, record_dealloc_addr, report_alloc
        private :: build_msg

      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!||====================================================================
!||    build_msg      ../common_source/tools/memory/my_alloc_tools.F90
!||--- called by ------------------------------------------------------
!||    execargcheck   ../engine/source/engine/execargcheck.F
!||    radioss2       ../engine/source/engine/radioss2.F
!||    starter0       ../starter/source/starter/starter0.F
!||====================================================================
        function build_msg(str) result(error_message)
          character(len=*), intent(in) :: str
          character(len=len_error_message) :: error_message
          if(len_trim(str) > len_error_message) then
            error_message = str(1:len_error_message)
          else
            error_message = adjustl(str) // repeat(" ", len_error_message - len_trim(str))
          end if
        end function build_msg

!||====================================================================
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||--- calls      -----------------------------------------------------
!||    arret            ../engine/source/system/arret.F
!||====================================================================
        subroutine my_alloc_check(stat, msg)
          integer,          intent(in)           :: stat
          character(len=*), intent(in), optional :: msg
          if (stat /= 0) then
            write(6, "(a,i10)") "Error in memory allocation, stat=", stat
            if(present(msg)) write(6, "(a)") msg
            call arret(2) ! the only line that calls an OpenRadioss routine, so that the error message is printed
          end if
        end subroutine my_alloc_check

        subroutine record_alloc_addr(addr, msg, nbytes)
          use iso_c_binding, only : c_ptr, c_char, c_int, c_int64_t
          type(c_ptr),      intent(in) :: addr
          character(len=*), intent(in) :: msg
          integer(kind=8),  intent(in) :: nbytes
          integer(c_int)    :: msg_len
          integer(c_int64_t) :: c_nbytes
          msg_len  = len_trim(msg)
          c_nbytes = nbytes
          call cpp_record_alloc_addr(addr, msg, msg_len, c_nbytes)
        end subroutine record_alloc_addr

        subroutine record_dealloc_addr(addr)
          use iso_c_binding, only : c_ptr
          type(c_ptr), intent(in) :: addr
          call cpp_record_dealloc_addr(addr)
        end subroutine record_dealloc_addr

        subroutine report_alloc()
          call cpp_print_alloc_report()
        end subroutine report_alloc

      end module my_alloc_tools_mod
