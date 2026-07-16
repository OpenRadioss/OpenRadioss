!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
! ======================================================================================================================
! fypp template — generates my_move_alloc.F90
! Do NOT edit the generated my_move_alloc.F90 directly; edit this file and re-run fypp.
!
! Generic wrapper around Fortran intrinsic move_alloc with memory-report bookkeeping:
!   - If destination already has a tracked allocation, remove it from the report.
!   - Perform move_alloc(from, to).
!   - If source allocation was already tracked, transfer tracking from old source
!     address to new destination address.
!   - If source was not tracked and msg is provided, record destination as a new
!     tracked allocation with that message.
!
! Axes of variation:
!   TYPES : (fortran_type, short_name)
!   RANKS : (rank, list_of_dim_variable_names)
!
! Subroutine naming convention:
!   my_move_alloc_<type_name>_<rank>d
!   e.g. my_move_alloc_double_2d
! ======================================================================================================================

#:set TYPES = [('real', 'real'), ('double precision', 'double'), ('integer', 'integer'), ('logical', 'logical')]
#:set RANKS = [(1, ['n']), (2, ['n', 'm']), (3, ['l', 'm', 'n'])]

#! Macro: emit one move_alloc wrapper subroutine.
#! Arguments:
#!   SUB_NAME - subroutine name string
#!   FTYPE    - Fortran type (e.g. 'real', 'double precision', 'integer', 'logical')
#!   RANK     - rank number (1,2,3)
#:def move_alloc_sub(SUB_NAME, FTYPE, RANK)
  #:set FIRST_ELEM = 'lbound(from,' + str(1) + ')'
  #:if RANK == 1
    #:set FIRST_FROM = 'from(lbound(from,1))'
    #:set FIRST_TO = 'to(lbound(to,1))'
  #:elif RANK == 2
    #:set FIRST_FROM = 'from(lbound(from,1), lbound(from,2))'
    #:set FIRST_TO = 'to(lbound(to,1), lbound(to,2))'
  #:else
    #:set FIRST_FROM = 'from(lbound(from,1), lbound(from,2), lbound(from,3))'
    #:set FIRST_TO = 'to(lbound(to,1), lbound(to,2), lbound(to,3))'
  #:endif
!||====================================================================
!||    ${SUB_NAME}$   ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    cpp_record_dealloc_addr     ../common_source/tools/memory/cpp_report_alloc.cpp
!||    cpp_record_move_alloc_addr  ../common_source/tools/memory/cpp_report_alloc.cpp
!||    cpp_record_alloc_addr       ../common_source/tools/memory/cpp_report_alloc.cpp
!||====================================================================
        subroutine ${SUB_NAME}$(from, to, msg)
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), allocatable, target, intent(inout) :: from
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(${FIRST_FROM}$)
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(${FIRST_TO}$)
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(${FIRST_TO}$)
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine ${SUB_NAME}$
#:enddef move_alloc_sub

      module my_move_alloc_mod
        use iso_c_binding, only : c_ptr, c_loc, c_char, c_int, c_int64_t
        implicit none

        interface
          subroutine cpp_record_dealloc_addr(addr) bind(C, name="cpp_record_dealloc_addr")
            import :: c_ptr
            type(c_ptr), value, intent(in) :: addr
          end subroutine cpp_record_dealloc_addr

          subroutine cpp_record_move_alloc_addr(src_addr, dst_addr) bind(C, name="cpp_record_move_alloc_addr")
            import :: c_ptr
            type(c_ptr), value, intent(in) :: src_addr
            type(c_ptr), value, intent(in) :: dst_addr
          end subroutine cpp_record_move_alloc_addr

          integer(c_int) function cpp_is_alloc_tracked_addr(addr) bind(C, name="cpp_is_alloc_tracked_addr")
            import :: c_int, c_ptr
            type(c_ptr), value, intent(in) :: addr
          end function cpp_is_alloc_tracked_addr

          subroutine cpp_record_alloc_addr(addr, msg, msg_len, nbytes) bind(C, name="cpp_record_alloc_addr")
            import :: c_ptr, c_char, c_int, c_int64_t
            type(c_ptr), value, intent(in) :: addr
            character(kind=c_char), intent(in) :: msg(*)
            integer(c_int), intent(in) :: msg_len
            integer(c_int64_t), intent(in) :: nbytes
          end subroutine cpp_record_alloc_addr
        end interface

#:for FTYPE, TNAME in TYPES
  #:for RANK, DIM_VARS in RANKS
        private :: my_move_alloc_${TNAME}$_${RANK}$d
  #:endfor
#:endfor

        public :: my_move_alloc

        interface my_move_alloc
#:for FTYPE, TNAME in TYPES
  #:for RANK, DIM_VARS in RANKS
          module procedure my_move_alloc_${TNAME}$_${RANK}$d
  #:endfor
#:endfor
        end interface my_move_alloc

      contains

        logical function is_addr_tracked(addr)
          type(c_ptr), intent(in) :: addr
          is_addr_tracked = (cpp_is_alloc_tracked_addr(addr) /= 0_c_int)
        end function is_addr_tracked

        subroutine record_alloc_addr(addr, msg, nbytes)
          type(c_ptr), intent(in) :: addr
          character(len=*), intent(in) :: msg
          integer(kind=8), intent(in) :: nbytes
          integer(c_int) :: msg_len
          integer(c_int64_t) :: c_nbytes
          msg_len = len_trim(msg)
          c_nbytes = nbytes
          call cpp_record_alloc_addr(addr, msg, msg_len, c_nbytes)
        end subroutine record_alloc_addr

! ======================================================================================================================
!                                     GENERATED MOVE_ALLOC ROUTINES
!   Loop order: TYPES x RANKS
!   TYPES      : real, double precision, integer, logical
!   RANKS      : 1d, 2d, 3d
! ======================================================================================================================

#:for FTYPE, TNAME in TYPES
  #:for RANK, DIM_VARS in RANKS
    #:set SUB_NAME = 'my_move_alloc_' + TNAME + '_' + str(RANK) + 'd'
!! \brief move_alloc wrapper for ${RANK}$D ${FTYPE}$ arrays with memory report update
$:move_alloc_sub(SUB_NAME, FTYPE, RANK)

  #:endfor
#:endfor

      end module my_move_alloc_mod